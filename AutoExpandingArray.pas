unit AutoExpandingArray;

{
  Version: 1.2
}
interface

uses
  windows, classes, sysUtils, generics.collections, generics.defaults;

type
TAutoExpandingArrayofRecord < T: Record >= class
  private type pT = ^T;
private
  fItems: array of pT;
  function Getitems(index: integer): pT;

  procedure EnsureElement(index: integer);
  function GetCount: integer;
public
  constructor Create;
  Destructor Destroy;
  override;

  Procedure Clear;

  procedure Setitem(index: integer; const Value: T);
  property items[index: integer]: pT read Getitems;
  default;
  property Count: integer read GetCount;
  end;

  // if you can not use the auto expanding array as a class - here is how you can use it as a record holding the class
  TAutoExpandingArrayofRecordHolder<T: record > = record
    private type
    TAr = TAutoExpandingArrayofRecord<T>;
  iHolder = interface
  function GetAr: TAutoExpandingArrayofRecord<T>;
  property ar: TAutoExpandingArrayofRecord<T> read GetAr;
  end;
  THolder = class(TInterfacedObject, iHolder)
    private
    fAr: TAr;
  function GetAr: TAr;
public
  property ar: TAr read GetAr;
  destructor Destroy;
  override;
  end;

strict private
  fItem: iHolder;
  function GetItem: TAr;
public
  property Item: TAr read GetItem;
  end;

  // takes ovnership of the items
  TAutoExpandingArrayofClass < T: Class, Constructor >= class
    private
    fItems: array of T;
  function Getitems(index: integer): T;
  procedure Setitem(index: integer; const Value: T);
  procedure EnsureElement(index: integer; CreateElement: boolean);
  function GetCount: integer;
public
  constructor Create;
  Destructor Destroy;
  override;

  Procedure Clear;

  property items[index: integer]: T read Getitems write Setitem;
  default;
  property Count: integer read GetCount;
  end;

  TAutoExpandingmatrixofClass < T: Class, Constructor >= class
    private
    fItems: TAutoExpandingArrayofClass<TAutoExpandingArrayofClass<T>>;
  function Getitems(i1, i2: integer): T;
  procedure Setitem(i1, i2: integer; const Value: T);
public
  constructor Create;
  Destructor Destroy;
  override;

  Function Count: integer;
  overload;
  Function Count(index1: integer): integer;
  overload;

  property items[i1, i2: integer]: T read Getitems write Setitem;
  default;
  end;

  TAutoExpandingmatrixofRecord < T: Record >= class
    private type pT = ^T;
private
  fItems: TAutoExpandingArrayofClass<TAutoExpandingArrayofRecord<T>>;
  function Getitems(i1, i2: integer): pT;

public
  constructor Create;
  Destructor Destroy;
  override;

  Function Count: integer;
  overload;
  Function Count(index1: integer): integer;
  overload;

  procedure Setitem(i1, i2: integer; const Value: T);
  property items[i1, i2: integer]: pT read Getitems;
  default;
  end;

  // non class non record. good for ordinary types
  TAutoExpandingArrayofOrdinary < T >= class
    private
    fItems: TList<T>;
  function Getitems(index: integer): T;
  procedure Setitem(index: integer; const Value: T);
  procedure EnsureElement(index: integer);
public
  constructor Create;
  Destructor Destroy;
  override;

  Procedure Clear;
  Function Count: integer;

  property items[index: integer]: T read Getitems write Setitem;
  default;
  end;

implementation

uses
  Rtti;

{ TAutoExpandingArrayofRecord<T:record> }

function TAutoExpandingArrayofRecord<T>.Getitems(index: integer): pT;
begin
  EnsureElement(index);
  result := fItems[index];
end;

procedure TAutoExpandingArrayofRecord<T>.Setitem(index: integer; const Value: T);
begin
  EnsureElement(index);
  fItems[index]^ := Value;
end;

constructor TAutoExpandingArrayofRecord<T>.Create;
begin
  inherited Create;
  fItems := NIL;

end;

destructor TAutoExpandingArrayofRecord<T>.Destroy;
begin
  Clear;
  fItems := NIL;
  inherited;
end;

procedure TAutoExpandingArrayofRecord<T>.Clear;
var
  x: integer;
begin
  for x := 0 to length(fItems) - 1 do
    if fItems[x] <> nil then
    begin
      // ensure references are freed correctly
      fItems[x]^ := default (T);
      dispose(fItems[x]);
    end;

  fItems := NIL;
end;

procedure TAutoExpandingArrayofRecord<T>.EnsureElement(index: integer);
var
  CurLen, reqLen: integer;
  x: integer;
begin
  reqLen := index + 1;
  CurLen := length(fItems);

  if reqLen > CurLen then
  begin
    // double the length to prevent small reallocation each time we require a new item
    reqLen := reqLen * 2;
    SetLength(fItems, reqLen);
    for x := CurLen to reqLen - 1 do
      fItems[x] := nil;
  end;

  if fItems[index] = nil then
  begin
    new(fItems[index]);
    fItems[index]^ := default (T);
  end;

end;

{ TAutoExpandingArrayofClass<T> }

constructor TAutoExpandingArrayofClass<T>.Create;
begin
  inherited Create;
  fItems := NIL;
end;

function TAutoExpandingArrayofClass<T>.Getitems(index: integer): T;
begin
  EnsureElement(index, True);
  result := fItems[index];
end;

procedure TAutoExpandingArrayofClass<T>.EnsureElement(index: integer; CreateElement: boolean);
var
  CurLen, reqLen: integer;
  x: integer;
begin
  reqLen := index + 1;
  CurLen := length(fItems);

  if reqLen > CurLen then
  begin
    // double the length to prevent small reallocation each time we require a new item
    reqLen := reqLen * 2;
    SetLength(fItems, reqLen);
    for x := CurLen to reqLen - 1 do
      fItems[x] := nil;
  end;

  if CreateElement then
    if not Assigned(fItems[index]) then
      fItems[index] := T.Create;
end;

destructor TAutoExpandingArrayofClass<T>.Destroy;
begin
  Clear;
  fItems := NIL;
  inherited;
end;

procedure TAutoExpandingArrayofClass<T>.Clear;
var
  x: integer;
begin
  for x := 0 to length(fItems) - 1 do
    if Assigned(fItems[x]) then
      FreeAndNIL(fItems[x]);

  fItems := nil;
end;

procedure TAutoExpandingArrayofClass<T>.Setitem(index: integer;
  const Value: T);
begin
  EnsureElement(index, false);
  if fItems[index] = Value then
    exit;

  if Assigned(fItems[index]) then
    FreeAndNIL(fItems[index]);

  fItems[index] := Value;
end;

function TAutoExpandingArrayofClass<T>.GetCount: integer;
begin
  result := length(fItems);
end;

{ TAutoExpandingArrayofOrdinary<T> }

constructor TAutoExpandingArrayofOrdinary<T>.Create;
begin
  inherited Create;
  fItems := TList<T>.Create;
end;

function TAutoExpandingArrayofOrdinary<T>.Getitems(index: integer): T;
begin
  EnsureElement(index);
  result := fItems[index];
end;

procedure TAutoExpandingArrayofOrdinary<T>.EnsureElement(index: integer);
var
  CurLen, reqLen: integer;
  x: integer;
begin
  reqLen := index + 1;
  CurLen := fItems.Count;

  if reqLen > CurLen then
  begin
    // double the length to prevent small reallocation each time we require a new item
    reqLen := reqLen * 2;
    fItems.Capacity := reqLen;
    for x := CurLen to reqLen - 1 do
      fItems.Add(Default (T));
  end;

end;

destructor TAutoExpandingArrayofOrdinary<T>.Destroy;
begin
  fItems.Free;
  inherited;
end;

procedure TAutoExpandingArrayofOrdinary<T>.Clear;
begin
  fItems.Clear;
end;

procedure TAutoExpandingArrayofOrdinary<T>.Setitem(index: integer;
  const Value: T);
begin
  EnsureElement(index);
  fItems[index] := Value;
end;

function TAutoExpandingArrayofOrdinary<T>.Count: integer;
begin
  result := fItems.Count;
end;

{ TAutoExpandingmatrixofClass<T> }

constructor TAutoExpandingmatrixofClass<T>.Create;
begin
  inherited Create;
  fItems := TAutoExpandingArrayofClass < TAutoExpandingArrayofClass < T >>.Create;
end;

function TAutoExpandingmatrixofClass<T>.Getitems(i1, i2: integer): T;
begin
  result := fItems[i1][i2];
end;

destructor TAutoExpandingmatrixofClass<T>.Destroy;
begin
  fItems.Free;
  inherited;
end;

procedure TAutoExpandingmatrixofClass<T>.Setitem(i1, i2: integer;
  const Value: T);
begin
  fItems[i1].Setitem(i2, Value);
end;

function TAutoExpandingmatrixofClass<T>.Count(index1: integer): integer;
begin
  result := fItems[index1].Count;
end;

function TAutoExpandingmatrixofClass<T>.Count: integer;
begin
  result := fItems.Count;;

end;

{ TAutoExpandingmatrixofRecord<T> }

constructor TAutoExpandingmatrixofRecord<T>.Create;
begin
  inherited Create;
  fItems := TAutoExpandingArrayofClass < TAutoExpandingArrayofRecord < T >>.Create;
end;

function TAutoExpandingmatrixofRecord<T>.Getitems(i1, i2: integer): pT;
var
  p: Pointer;
begin
  p := fItems[i1][i2];
  result := p;
end;

destructor TAutoExpandingmatrixofRecord<T>.Destroy;
begin
  fItems.Free;
  inherited;
end;

procedure TAutoExpandingmatrixofRecord<T>.Setitem(i1, i2: integer;
  const Value: T);
begin
  fItems[i1].Setitem(i2, Value);
end;

function TAutoExpandingArrayofRecord<T>.GetCount: integer;
begin
  result := length(fItems);
end;

function TAutoExpandingmatrixofRecord<T>.Count(index1: integer): integer;
begin
  result := fItems[index1].Count;
end;

function TAutoExpandingmatrixofRecord<T>.Count: integer;
begin
  result := fItems.Count;
end;

{ TAutoExpandingArrayofRecordHolder<T> }

function TAutoExpandingArrayofRecordHolder<T>.GetItem: TAr;
begin
  if not Assigned(fItem) then
    fItem := THolder.Create;

  result := fItem.ar;

end;

{ TAutoExpandingArrayofRecordHolder<T>.THolder }

function TAutoExpandingArrayofRecordHolder<T>.THolder.GetAr: TAr;
begin
  if not Assigned(far) then
    fAr := TAr.Create;
  result := fAr;
end;

destructor TAutoExpandingArrayofRecordHolder<T>.THolder.Destroy;
begin
  if Assigned(fAr) then
    FreeAndNIL(fAr);
  inherited;
end;

end.
