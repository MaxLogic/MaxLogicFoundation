unit maxLogic.ringBuffer;

interface

uses
  sysUtils, classes, generics.collections;

type
TmaxRingBuffer < T >= class
  private type TBehavior = (bhAlwaysAdd, bhKeepExistingItems);
private
  fItems: array of T;
  fCount: integer;
  fCapacity: integer;
  fIndexOfFirstItem: integer;
  FBehavior: TBehavior;

  function ExternalIndexToInternalIndex(const i: integer): integer;
  inline;
  procedure ValidateIndex(const index: integer);
  inline;

  function GetFirst: T;
  function GetItem(index: integer): T;
  function GetLast: T;
  procedure SetCapacity(const Value: integer);
  procedure SetFirst(const Value: T);
  procedure SetItem(index: integer; const Value: T);
  procedure SetLast(const Value: T);
  procedure CleanItems(StartIndex, EndIndex: integer);
  procedure SetBehavior(const Value: TBehavior);
  // this enforces the first item to have the index of 0. this is important when changing the capacity
  procedure NormalizeBuffer;
public
  { Creates a ring buffer of a given size.

    Parameters:
    ACapacity: the number of elements (of type T) that the buffer will hold. }
  constructor Create(aCapacity: integer);
  destructor Destroy;
  override;

  { writes a single item to the buffer.
    NOTE: the behavior property is important as it defines what happens if the buffer is full..
    Parameters:
    Value: the item to be added
    Returns: true if the item was added otherwise false }
  Function add(const Value: T): boolean;
  overload;
  { as add, but you can add a array of items.
    Returns: the number of items added }
  Function add(const values: array of T): integer;
  overload;
  { as add, but you can add a array of items and specify the sub range giving the startIndex inside this array and the number of items to add.
    Returns: the number of items added }
  Function add(const values: array of T; StartIndex, ItemCount: integer): integer;
  overload;

  { retrives two last added items. They are not removed from the buffer }
  Function GetLastTwoItems: TArray<T>;
  Function TOArray: TArray<T>;
  procedure clear;
  Function IsBufferFull: boolean;

  { reads out the first item in the buffer and removes it from the buffer }
  Function read: T;
  overload;
  { reads out multiple items at once.
    NOTE: the returned number of items may be less tehn requested. }
  Function Read(aCount: integer): TArray<T>;
  overload;

  property Count: integer read fCount;
  { Sets the capacity. Please be aware that this may require a restructurisation of the buffer and therefore will take some time }
  property Capacity: integer read fCapacity write SetCapacity;
  property Items[index: integer]: T read GetItem write SetItem;
  default;
  property Last: T read GetLast write SetLast;
  property First: T read GetFirst write SetFirst;
  { determines the behavior when the buffer is full.
    bhAlwaysAdd: tells the ring buffer to add the new items and discard the first items in the buffer. This is good if you are storing the last vaalues,
    bhKeepExistingItems: no new items will be added until there is some space in the ring buffer. You need to use on of the read functions to retrive the first items from the buffer and make room for more. This is good when you are buffering some important data to use like in multimedia bufers.
    Default is bhAlwaysAdd }
  property Behavior: TBehavior read FBehavior write SetBehavior;
  end;

implementation

uses
  math;

{ TmaxRingBuffer<T> }

function TmaxRingBuffer<T>.GetFirst: T;
begin
  ValidateIndex(fIndexOfFirstItem);
  result := fItems[fIndexOfFirstItem];
end;

function TmaxRingBuffer<T>.GetItem(index: integer): T;
var
  i: integer;
begin
  i := ExternalIndexToInternalIndex(Index);
  result := fItems[i];
end;

function TmaxRingBuffer<T>.GetLast: T;
var
  i: integer;
begin
  i := ExternalIndexToInternalIndex(Count - 1);
  result := fItems[i];
end;

procedure TmaxRingBuffer<T>.SetCapacity(const Value: integer);
begin
  if fCapacity = Value then
    exit;

  if Value > fCapacity then
  begin
    // wea re enlarging the buffer

    { are all Items continously in the current range of 0 .. fCapacity - 1
      if so we can just extend the buffer
      if not we need first to normalize it }
    if (fCount <> 0) and ((fIndexOfFirstItem + fCount) > fCapacity) then
      NormalizeBuffer;

    SetLength(fItems, Value);
    fCapacity := Value;

  end else begin
    // we are shrinking the buffer

    // like before first check if we need to normalizeThe items first
    // if all the items are "left" of the new capacity, then we can just shrink the array otherweise we need to normalize first
    if (fCount <> 0) and ((fIndexOfFirstItem + fCount) > Value) then
      NormalizeBuffer;

    SetLength(fItems, Value);
    fCapacity := Value;

  end;

end;

procedure TmaxRingBuffer<T>.SetFirst(const Value: T);
begin
  ValidateIndex(fIndexOfFirstItem);
  fItems[fIndexOfFirstItem] := Value;
end;

procedure TmaxRingBuffer<T>.SetItem(index: integer; const Value: T);
var
  i: integer;
begin
  i := ExternalIndexToInternalIndex(index);
  fItems[i] := Value;
end;

procedure TmaxRingBuffer<T>.CleanItems(StartIndex, EndIndex: integer);
var
  x, i: integer;
begin
  for x := StartIndex to EndIndex do
  begin
    i := ExternalIndexToInternalIndex(x);
    fItems[i] := default (T);
  end;
end;

procedure TmaxRingBuffer<T>.SetLast(const Value: T);
var
  i: integer;
begin
  i := ExternalIndexToInternalIndex(fCount - 1);
  fItems[i] := Value;
end;

constructor TmaxRingBuffer<T>.Create(aCapacity: integer);
begin
  inherited Create;
  FBehavior := bhAlwaysAdd;
  fCapacity := aCapacity;
  SetLength(fItems, fCapacity);
end;

destructor TmaxRingBuffer<T>.Destroy;
begin
  fItems := nil;
  inherited;
end;

function TmaxRingBuffer<T>.GetLastTwoItems: TArray<T>;
var
  i1, i2: integer;
begin
  i1 := ExternalIndexToInternalIndex(fCount - 2);
  i2 := ExternalIndexToInternalIndex(fCount - 1);

  SetLength(result, 2);
  result[0] := fItems[i1];
  result[1] := fItems[i2];
end;

procedure TmaxRingBuffer<T>.clear;
begin
  CleanItems(0, fCount - 1);
  fCount := 0;
  fIndexOfFirstItem := 0;
end;

function TmaxRingBuffer<T>.TOArray: TArray<T>;
var
  x, i: integer;
begin
  SetLength(result, fCount);
  for x := 0 to fCount - 1 do
  begin
    i := ExternalIndexToInternalIndex(x);
    result[x] := fItems[i];
  end;
end;

function TmaxRingBuffer<T>.ExternalIndexToInternalIndex(const i: integer): integer;
begin
  ValidateIndex(i);

  result := i + fIndexOfFirstItem;
  if result >= fCapacity then
    dec(result, fCapacity);
end;

procedure TmaxRingBuffer<T>.SetBehavior(const Value: TBehavior);
begin
  FBehavior := Value;
end;

function TmaxRingBuffer<T>.add(const Value: T): boolean;
var
  i: integer;
begin
  if IsBufferFull then
    case FBehavior of
      bhKeepExistingItems:
        exit(false);
      bhAlwaysAdd:
        // by reading out the first item we remove it from the buffer
        read();
    end;

  inc(fCount);
  i := ExternalIndexToInternalIndex(fCount - 1);

  fItems[i] := Value;
  result := true;
end;

function TmaxRingBuffer<T>.read: T;
begin
  result := First;
  First := default (T);

  dec(fCount);

  if fCount = 0 then
    fIndexOfFirstItem := 0
  else
  begin
    inc(fIndexOfFirstItem);
    if fIndexOfFirstItem = fCapacity then
      fIndexOfFirstItem := 0;
  end;
end;

function TmaxRingBuffer<T>.IsBufferFull: boolean;
begin
  result := fCount = Capacity;
end;

procedure TmaxRingBuffer<T>.ValidateIndex(const index: integer);
begin
  if (index < 0) or (index >= fCount) then
    raise Exception.Create('Index out of bounds');
end;

procedure TmaxRingBuffer<T>.NormalizeBuffer;
var
  x, i: integer;
  newBuffer: TArray<T>;
begin
  newBuffer := self.TOArray;
  fItems := newBuffer;
end;

function TmaxRingBuffer<T>.add(const values: array of T; StartIndex, ItemCount: integer): integer;
var
  x: integer;
begin
  result := 0;
  for x := StartIndex to ItemCount - 1 do
    if add(values[x]) then
      inc(result)
    else
      break;
end;

function TmaxRingBuffer<T>.add(const values: array of T): integer;
begin
  result := add(values, 0, length(values));
end;

function TmaxRingBuffer<T>.read(aCount: integer): TArray<T>;
var
  x, len: integer;
begin
  len := min(fCount, aCount);
  SetLength(result, len);
  for x := 0 to len - 1 do
    result[x] := read;
end;

end.
