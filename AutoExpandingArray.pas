unit AutoExpandingArray;

{
  Version: 1.4
  - Fixed syntax errors
  - Improved capacity growth logic
  - Added TrimExcess method
  - Clarified documentation and method purposes
}

interface

uses
  System.Classes, System.SysUtils, Generics.Collections, Generics.Defaults, System.TypInfo;

type
  /// <summary>
  /// TAutoExpandingArray is a dynamic, auto-growing array structure that can automatically
  /// expand during read/write access and supports customizable hooks for item creation and finalization.
  /// </summary>
  TAutoExpandingArray<T> = record
  public type
    TArrayProc = reference to procedure(var aArray: TAutoExpandingArray<T>);
    TItemsProc = reference to procedure(var aItems: TArray<T>; aStartIndex, aCount: Integer);
    TItemFunc = reference to function: T;
  private
    fCount: Integer;
    fCapacity: Integer;
    fItems: TArray<T>;
    fOnFinalize: TArrayProc;
    fExpandOnRead: Boolean;
    fGrowthFactor: Single;
    fOnFinalizeItems: TItemsProc;
    fOnCreateEmptyItem: TItemFunc;
    fExpandOnWrite: Boolean;
    fOnCreateItems: TItemsProc;

    procedure SetCapacity(const Value: Integer); inline;
    procedure SetCount(const Value: Integer); inline;
    procedure SetItem(aIndex: Integer; const Value: T); inline;
    function GetItem(aIndex: Integer): T; inline;
  public
    class operator Initialize(out Dest: TAutoExpandingArray<T>);
    class operator Finalize(var Dest: TAutoExpandingArray<T>);

    /// <summary>
    /// Makes a shallow copy of the array and its metadata (note: T must be a value type or reference-safe).
    /// </summary>
    function Clone: TAutoExpandingArray<T>;

    /// <summary>
    /// Clears the array by setting Count to 0. Capacity is retained.
    /// </summary>
    procedure Clear;

    /// <summary>
    /// Reduces the capacity to match the current count to release unused memory.
    /// </summary>
    procedure TrimExcess;

    property Count: Integer read fCount write SetCount;
    property Capacity: Integer read fCapacity write SetCapacity;
    property Items[aIndex: Integer]: T read GetItem write SetItem; default;

    /// <summary>
    /// Invoked when growing the array; allows bulk initialization of new elements.
    /// </summary>
    property OnCreateItems: TItemsProc read fOnCreateItems write fOnCreateItems;

    /// <summary>
    /// Invoked when accessing out-of-range index during read, if ExpandOnRead = False.
    /// Returns a default or placeholder value.
    /// </summary>
    property OnCreateEmptyItem: TItemFunc read fOnCreateEmptyItem write fOnCreateEmptyItem;

    /// <summary>
    /// Invoked when reducing Count or finalizing the array. Allows cleanup or releasing of resources.
    /// </summary>
    property OnFinalizeItems: TItemsProc read fOnFinalizeItems write fOnFinalizeItems;

    /// <summary>
    /// Invoked when the record is finalized (e.g., goes out of scope).
    /// </summary>
    property OnFinalize: TArrayProc read fOnFinalize write fOnFinalize;

    /// <summary>
    /// Growth multiplier used when automatically expanding the capacity.
    /// </summary>
    property GrowthFactor: Single read fGrowthFactor write fGrowthFactor;

    /// <summary>
    /// If True, reading an out-of-range index will expand the array up to that index.
    /// </summary>
    property ExpandOnRead: Boolean read fExpandOnRead write fExpandOnRead;

    /// <summary>
    /// If True, writing to an out-of-range index will expand the array to fit it. Otherwise, write is ignored.
    /// </summary>
    property ExpandOnWrite: Boolean read fExpandOnWrite write fExpandOnWrite;
  end;

implementation

uses
  Math;

procedure TAutoExpandingArray<T>.Clear;
begin
  Count := 0;
end;

function TAutoExpandingArray<T>.Clone: TAutoExpandingArray<T>;
begin
  Result.fCount := Self.fCount;
  Result.fCapacity := Self.fCapacity;
  Result.fExpandOnRead := Self.fExpandOnRead;
  Result.fExpandOnWrite := Self.fExpandOnWrite;
  Result.fGrowthFactor := Self.fGrowthFactor;
  Result.fOnCreateItems := Self.fOnCreateItems;
  Result.fOnCreateEmptyItem := Self.fOnCreateEmptyItem;
  Result.fOnFinalizeItems := Self.fOnFinalizeItems;
  Result.fOnFinalize := Self.fOnFinalize;
  Result.fItems := Copy(Self.fItems);
end;

class operator TAutoExpandingArray<T>.Finalize(var Dest: TAutoExpandingArray<T>);
begin
  if Assigned(Dest.fOnFinalize) then
    Dest.fOnFinalize(Dest);
  Dest.Capacity := 0;
end;

function TAutoExpandingArray<T>.GetItem(aIndex: Integer): T;
begin
  if aIndex < 0 then
    raise EArgumentOutOfRangeException.Create('Index cannot be negative');

  if (aIndex >= fCount) and fExpandOnRead then
    Count := aIndex + 1;

  if InRange(aIndex, 0, fCount - 1) then
    Result := fItems[aIndex]
  else if Assigned(fOnCreateEmptyItem) then
    Result := fOnCreateEmptyItem()
  else
    Result := Default(T);
end;

class operator TAutoExpandingArray<T>.Initialize(out Dest: TAutoExpandingArray<T>);
begin
  Dest.fCount := 0;
  Dest.fCapacity := 0;
  Dest.fItems := nil;
  Dest.fOnFinalize := nil;
  Dest.fGrowthFactor := 2;
  Dest.fOnFinalizeItems := nil;
  Dest.fOnCreateEmptyItem := nil;
  Dest.fExpandOnWrite := True;
  Dest.fExpandOnRead := False;
  Dest.fOnCreateItems := nil;
end;

procedure TAutoExpandingArray<T>.SetCapacity(const Value: Integer);
begin
  if fCapacity = Value then
    Exit;

  if fCount > Value then
    SetCount(Value);

  SetLength(fItems, Value);
  fCapacity := Value;
end;

procedure TAutoExpandingArray<T>.SetCount(const Value: Integer);
begin
  if fCount = Value then
    Exit;

  if Value < 0 then
    raise EArgumentOutOfRangeException.Create('Count cannot be negative');

  if fCapacity < Value then
  begin
    if fCapacity = 0 then
      Capacity := Max(10, Value)
    else
      Capacity := Max(Value, Ceil(fCapacity * fGrowthFactor));
  end;

  if fCount > Value then
  begin
    if Assigned(fOnFinalizeItems) then
      fOnFinalizeItems(fItems, Value, fCount - Value);

    FinalizeArray(@fItems[Value], TypeInfo(T), fCount - Value);
  end
  else if (fCount < Value) and Assigned(fOnCreateItems) then
  begin
    fOnCreateItems(fItems, fCount, Value - fCount);
  end;

  fCount := Value;
end;

procedure TAutoExpandingArray<T>.SetItem(aIndex: Integer; const Value: T);
begin
  if aIndex < 0 then
    raise EArgumentOutOfRangeException.Create('Index cannot be negative');

  if (aIndex >= fCount) and fExpandOnWrite then
    Count := aIndex + 1
  else if aIndex >= fCount then
    Exit; // silently ignore if out-of-bounds and not set to expand

  fItems[aIndex] := Value;
end;

procedure TAutoExpandingArray<T>.TrimExcess;
begin
  if fCapacity > fCount then
    SetCapacity(fCount);
end;

end.

