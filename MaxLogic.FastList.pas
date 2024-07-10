unit MaxLogic.FastList;

{ *******************************************************

  Digital Omerta 3D Engine

  Unit Author:   Pawel Piotrowski
  Copyright       Pawel Piotrowski

  STATUS:
  WORKING
  Version: 2.15
  History
  2019-12-24: GetRangeIndices implemented for TSimpleList and TSortedList
  2019-10-15: managed  types fully supported with TSimpleList and TSortedList
  2015-08-05: hotfix when using managed types with TSimpleList or TSortedList
  2015-07-23: compatible with delphi xe2
  2015-07-14: some optimizations, some generic support
  2007-12-22 Making it more flexible

  ******************************************************* }

interface

uses
  classes, types, sysUtils, SyncObjs, Contnrs, generics.collections, generics.defaults;

type
  {$IFDEF CLASSIMPORTER}
  Pointer = Integer;
  pInteger = Integer;
  {$ENDIF}

  { **
    This is just a small record, to help making real small lists, no larger then 255 elements long
    ** }
  TSmallList = packed record
    List: array of Pointer;
    Capacity,
      Count: byte;
  end;

TListAsRecord < T >= record
  private
  fCount: Integer;
function getCapacity: Integer;
procedure SetCapacity(const Value: Integer);
function getItem(index: Integer): T;
procedure setItem(index: Integer; const Value: T);
procedure setCount(const Value: Integer);
public
  // direct access
  Items: TArray<T>;

  property Capacity: Integer read getCapacity write SetCapacity;
  property Count: Integer read fCount write setCount;
  property item[index: Integer]: T read getItem write setItem;
  default;

  procedure add(const Value: T);
  procedure Clear;
  class
  function Create: TListAsRecord<T>;
  static;
  end;

  { *******************************************************
    Class: TIDSortedList
    Author:    pawelp
    Date:      2004-03-30 13:48

    DESCRIPTION:
    This list uses QuickSort Algo to sort its items by the given ID, which can be either
    an Integer or an Single.
    NOTE:
    It is not recomendet to mix integer and Single values... it is posible... byt the result are not tested
    ******************************************************* }

  TSortedList<TKey, TValue> = class
    private
    type
    TIDSortedListItem = packed record
    Data: TValue;
  IID: TKey;
  end;

private
  fComparer: IComparer<TKey>;
  fHasManagedType: boolean;

  fCount: Integer;
  FGrowthFactor: single;
  FDupIgnore: boolean;
  procedure SetCapacity(const Value: Integer);
  function getCapacity: Integer;
  function GetFirst: TValue;
  function GetLast: TValue;

  function getItem(index: Integer): TValue;
  procedure setItem(index: Integer; const Value: TValue);
  procedure setCount(const Value: Integer);
  procedure SetGrowthFactor(const Value: single);
  procedure SetDupIgnore(const Value: boolean);
  function DoFind(const id: TKey;
    var L, H: Integer;
    out index: Integer): boolean;
public
  // you have direct access to the underlying itens, but, do not change the iid, otherwise you will need to resort the array
  Items: array of TIDSortedListItem;

  constructor Create(aComparer: IComparer<TKey>);
  overload;
  constructor Create;
  overload;
  destructor Destroy;
  override;

  // I know... should be name/value, but that is like that to keep backwards copatibility with some legacy code
  // returns false if IgnoreDuplicates is true and this item already exists
  function add(const Value: TValue; const id: TKey): boolean;
  procedure AddOrSet(const Value: TValue; const id: TKey);
  // if you know exactly where it goes, then you can use this method
  procedure Insert(aIndex: Integer; const Value: TValue; const id: TKey);

  // note: if duplicates are allowed, then this will return an undefined item from the items with the same id. please refer to GetRangeIndices if you need to get the minIndex and maxIndex of the items with the same id
  function Find(const id: TKey; out index: Integer): boolean;
  virtual;
  // please note: it will return just the first item that matches
  // aOffset tells us from which item to start, in other words, how many items to skip
  function IndexOf(const Value: TValue; aOffset: Integer = 0;
    aComparer: IComparer<TValue> = nil): Integer;

  { Allows us to retrive the min and max index of items with the same id
    Note: aLeftIndex and aRightIndex will be the same if there is 1 or less items with the same id }
  function FindMinMaxIndex(const aId: TKey; out aLowIndex, aHighIndex: Integer): boolean;

  { will search of items that are between those values  : aLeftKey <= aItem.ID <= aRightKey
    will return false if aRightKey< aLeftKey
    will return false if none of the values are between aLeftKey and aRightKey
    NOTE: on result= false the indices are undefined }
  function FindValuesBetween(const aLeftKey, aRightKey: TKey;
    out aLowIndex, aHighIndex: Integer): boolean;

  procedure Delete(index: Integer);
  virtual;
  procedure Clear;
  virtual;
  procedure TrimExcess;
  virtual;

  // please note, this will work only if TKey and TValue are neither strings,pointers, classes or interfaces
  // that is here mostly for backwards compatibility
  procedure SaveToStream(Stream: TStream);
  function SaveToMemorySize: Integer;
  class function GetMinRequiredmemorySizeForSaving: Integer;
  class procedure SaveEmptyListToMemory(var pb: pByte);
  procedure SaveToMemory(var bytes: pByte);
  procedure LoadFromStream(Stream: TStream);
  procedure LoadFromMemory(var pb: pByte);

  property First: TValue read GetFirst;
  property Last: TValue read GetLast;
  property Capacity: Integer read getCapacity write SetCapacity;
  // Please note: the new items are of undefined value, they are not filled in with anything - except if they are managed types, in that case they are set to their default values
  property Count: Integer read fCount write setCount;
  property item[index: Integer]: TValue read getItem write setItem;
  default;
  property GrowthFactor: single read FGrowthFactor write SetGrowthFactor;
  // ATTENTION: Existing values are not checked to be unique. this applies only to adding values
  property DupIgnore: boolean read FDupIgnore write SetDupIgnore;
  end;

  TIdIntegerSortedList = TSortedList<Integer, Pointer>;
  TIdFloatSortedList = TSortedList<single, Pointer>;

  TIDSortedListItem2 = packed record
    Data1: Pointer;
  Data2: Pointer;
  end;

  TIDSortedList2 = class(TSortedList<Integer, TIDSortedListItem2>)
    private
  function GetFirst_Data1: Pointer;
  function GetFirst_Data2: Pointer;
  function GetLast_Data1: Pointer;
  function GetLast_Data2: Pointer;
  function GetItem_Data1(index: Integer): Pointer;
  function GetItem_Data2(index: Integer): Pointer;
  procedure SetItem_Data1(index: Integer; const Value: Pointer);
  procedure SetItem_Data2(index: Integer; const Value: Pointer);
public
  property First_Data1: Pointer read GetFirst_Data1;
  property First_Data2: Pointer read GetFirst_Data2;
  property Last_Data1: Pointer read GetLast_Data1;
  property Last_Data2: Pointer read GetLast_Data2;
  property Item_Data1[index: Integer]: Pointer read GetItem_Data1 write SetItem_Data1;
  property Item_Data2[index: Integer]: Pointer read GetItem_Data2 write SetItem_Data2;

  procedure add(p1, p2: Pointer; id: Integer);
  reintroduce;

  function IndexOf_data1(p: Pointer): Integer;
  function IndexOf_data2(p: Pointer): Integer;
  end;

  TIndexedIDSortedListItem = packed record
    Data: Pointer;
  pIndex: pInteger;
  case boolean of
    true:
    (IID: Integer;);
  False:
    (FID: single;);
  end;

  { *******************************************************
    Class: TIndexedIDSortedList
    Author:    pawelp
    Date:      2004-03-30 13:50

    DESCRIPTION:
    This is a mix between the FastList and the IDSortedList.
    Like the IDSortedList the IndexedIDSortedList sorts its items by an given ID,
    which can be an Integer or an Single value (please do not mix single and integers!!!).
    And Stores the Index of an Item in pIndex which is an pointer to an use given Integer Value
    ******************************************************* }

  TIndexedIDSortedList = class
    private
    FCapacity: Integer;
  fCount: Integer;
  procedure SetCapacity(const Value: Integer);
  function GetFirst: Pointer;
  function GetLast: Pointer;

  function getItem(index: Integer): Pointer;

protected

public
  Items: array of TIndexedIDSortedListItem;

  property First: Pointer read GetFirst;
  property Last: Pointer read GetLast;
  property Capacity: Integer read FCapacity write SetCapacity;
  property Count: Integer read fCount;
  property item[index: Integer]: Pointer read getItem;
  default;

  constructor Create;
  destructor Destroy;
  override;

  procedure add(p: Pointer; id: Integer; pIndex: pInteger);
  overload;
  procedure add(p: Pointer; id: single; pIndex: pInteger);
  overload;

  function Find(id: Integer; out index: Integer): boolean;
  overload;
  function Find(id: single; out index: Integer): boolean;
  overload;

  function IndexOf(p: Pointer): Integer;

  procedure Delete(i: Integer);
  procedure Clear;
  end;

  { *******************************************************
    Date:      2004-03-30 13:53
    DESCRIPTION:
    THis is an Array based List, like TList, but it does not change its Capacity during deleting or
    Clear Proc. So we have an much smaller memory allocation Rate.
    ******************************************************* }

  TSimpleList<T> = class
    private
    fCount: Integer;
  FGrowthFactor: single;
  fComparer: IComparer<T>;
  FSorted: boolean;
  fHasManagedType: boolean;
  FDupIgnore: boolean;

  procedure SetCapacity(const Value: Integer);
  function getItem(index: Integer): T;
  procedure setItem(index: Integer; const Value: T);
  procedure SetGrowthFactor(const Value: single);
  procedure setCount(const Value: Integer);
  procedure SetSorted(const Value: boolean);
  function getCapacity: Integer;
  procedure SetDupIgnore(const Value: boolean);
public
  // you have direct access to the array, but please be aware not to mess with it
  Items: array of T;

  function Slice(acount: Integer): TArray<T>;

  property Count: Integer read fCount write setCount;
  property Capacity: Integer read getCapacity write SetCapacity;
  property item[index: Integer]: T read getItem write setItem;
  default;
  // ATTENTION: Existing values are not checked to be unique. this applies only to adding values
  // ATTENTION: use only in combination with sorted!.
  // if dupIgnore = true this will set sorted to true  as well
  property DupIgnore: boolean read FDupIgnore write SetDupIgnore;

  // the multiplier when growing the list. default is 2.0
  property GrowthFactor: single read FGrowthFactor write SetGrowthFactor;
  // ATTENTION: seting this to false will set dupIgnore to false as well
  property Sorted: boolean read FSorted write SetSorted;

  constructor Create(aComparer: IComparer<T>);
  overload;
  constructor Create;
  overload;
  destructor Destroy;
  override;

  // if DupIgnore is true, then it will check first, if the value does exist.
  // returns true, if the value was added, false otherwise
  function add(const Value: T): boolean;
  // does not check for duplicates, ignores sorted. Be careful, you might end up with a miss sorted list
  procedure Insert(index: Integer; const Value: T);

  // if you want to add only a part of the array, then set aStartIndex and aCount
  // NOTE: doesn't check for duplicate values inside the avalues array
  procedure addSortedArray(const avalues: array of T; aStartIndex: Integer; acount: Integer);
  procedure addList(const avalues: TSimpleList<T>);

  // note: if sorted = true, the values will be sorted and addSortedArray will be called
  // otherwise the values are just appended at the end
  // NOTE: doesn't check for duplicate values inside the avalues array
  procedure append(const values: TArray<T>; aStartIndex: Integer; acount: Integer);

  procedure Clear;
  procedure TrimExcess;
  function IndexOf(const Value: T): Integer;
  // requires sorted to be true!
  function Find(const Value: T; out index: Integer): boolean;
  virtual;
  function Contains(const aValue: T): boolean;
  // requires sorted to be true!
  // returns false if the range is not contained in the list
  function GetRangeIndices(const LeftValue, RightValue: T; out LeftIndex, RightIndex: Integer): boolean;

  procedure Delete(i: Integer);
  overload;
  procedure Delete(aStartIndex, aNumOfItemsToDelete: Integer);
  overload;

  procedure Exchange(i1, i2: Integer);
  function toarray: TArray<T>;

  procedure SaveToStream(Stream: TStream);
  procedure LoadFromStream(Stream: TStream);
  end;

  pLLItem = ^TTLLItem;

  TTLLItem = record
    prev,
    Next: pLLItem;
  Data: Pointer;
  end;

  { *******************************************************
    Class: TLinkedList
    Author:    pawelp
    Date:      2004-03-30 13:52

    DESCRIPTION:
    An typical Linked List
    ******************************************************* }

  TLinkedList = class
    private
    fCount: Integer;
  fFirst, fLast: pLLItem;

  function GetLLItems(index: Integer): pLLItem;
  function getItem(index: Integer): Pointer;
  procedure setItem(index: Integer; const Value: Pointer);
  function GetNextLLItem(LLItem: pLLItem): pLLItem;
  function GetPrevLLItem(LLItem: pLLItem): pLLItem;
  function GetEmpty: boolean;
protected

public
  property LLItems[index: Integer]: pLLItem read GetLLItems;
  property First: pLLItem read fFirst;
  property Last: pLLItem read fLast;
  property Empty: boolean read GetEmpty; // tells if the list is empty or not

  property Next[LLItem: pLLItem]: pLLItem read GetNextLLItem;
  property prev[LLItem: pLLItem]: pLLItem read GetPrevLLItem;

  property Count: Integer read fCount;
  property Items[index: Integer]: Pointer read getItem write setItem;

  constructor Create;
  destructor Destroy;
  override;

  procedure add(Obj: Pointer);
  procedure AddAfter(item: pLLItem; Obj: Pointer);
  procedure AddBefore(item: pLLItem; Obj: Pointer);
  procedure Delete(index: Integer);
  overload;
  procedure Delete(p: pLLItem);
  overload;
  procedure DeleteFirst;
  procedure DeleteLast;

  procedure Exchange(i1, i2: Integer);
  procedure Clear;
  function IndexOf(p: Pointer; var LLItem: pLLItem): Integer;
  end;

  TFastListItem = record
    Obj: Pointer;
  pIndex: pInteger;
  end;

  { *******************************************************
    Class : TFastList
    Author:    pawelp
    Date:      2004-03-30 13:44

    DESCRIPTION:
    This is a fast List, where each Item have an Pointer to some user defined data
    and an Pointer where it updates its actual position.
    This is very good for often searchable Lists

    During deletion the last Item in the list is moved into the place
    of the deleted item, this way we have only a small memory movment, and the indexes of most ites are not affected
    ******************************************************* }

  TFastList = class
    private
    fItems: array of TFastListItem;
  FCapacity, fCount, fInitSize: Integer;
  function getItem(index: Integer): Pointer;
protected

public
  property item[index: Integer]: Pointer read getItem;
  default;
  property Count: Integer read fCount;

  constructor Create(InitSize: Integer);
  destructor Destroy;
  override;

  procedure Clear;
  procedure ClearHard;

  procedure Delete(index: Integer);
  function IndexOf(p: Pointer): Integer;
  procedure add(p: Pointer; pIndex: pInteger);
  end;

  TThreadSaveObjectList = class(TObject)
    private
    fSec: TCriticalSection;
  fList: TObjectList;
  function GetCount: Integer;
protected

public
  constructor Create;
  destructor Destroy;
  override;

  procedure add(aObject: TObject);
  procedure AddAsFirst(aObject: TObject);
  function GetFirstAndRemFromList(out aObject: TObject): boolean;
  function GetLastAndRemFromList(out aObject: TObject): boolean;
  procedure Clear;

  property Count: Integer read GetCount;

  end;

  // windows replacement
  procedure ZeroMemory(p: Pointer; Size: Integer);

implementation

uses
  System.RTLConsts, RTTI;
{ TFastList }

procedure ZeroMemory(p: Pointer; Size: Integer);
begin
  FillChar(p^, Size, 0);
end;

procedure TFastList.add(p: Pointer; pIndex: pInteger);
begin
  if fCount >= FCapacity then
  begin
    Inc(FCapacity, self.fInitSize);
    SetLength(fItems, FCapacity);
  end;
  fItems[fCount].Obj := p;
  fItems[fCount].pIndex := pIndex;
  pIndex^ := fCount;
  Inc(fCount);
end;

procedure TFastList.Clear;
var
  li: Integer;

  procedure C(st: Integer);
  var
    x: Integer;
  begin

    try
      for x := st to fCount - 1 do
      begin
        li := x;
        if fItems[x].pIndex <> nil then
          fItems[x].pIndex^ := -1;
      end;
    except
      C(li + 1);
    end;
  end;

begin
  C(0);
  fCount := 0;
end;

procedure TFastList.ClearHard;
begin
  fCount := 0;
end;

constructor TFastList.Create(InitSize: Integer);
begin
  inherited Create;

  FCapacity := InitSize;
  SetLength(fItems, FCapacity);
  fCount := 0;
  fInitSize := InitSize;
end;

procedure TFastList.Delete(index: Integer);
begin
  fItems[index].pIndex^ := -1;

  if (index <> fCount - 1) and (fCount <> 1) then
  begin
    fItems[index] := fItems[fCount - 1];
    fItems[index].pIndex^ := index;
  end;
  Dec(fCount);
end;

destructor TFastList.Destroy;
begin
  Clear;
  fItems := nil;

  inherited;
end;

function TFastList.getItem(index: Integer): Pointer;
begin
  Result := fItems[index].Obj;
end;

function TFastList.IndexOf(p: Pointer): Integer;
var
  x: Integer;
begin
  for x := 0 to fCount - 1 do
    if p = fItems[x].Obj then
    begin
      Result := x;
      Exit;
    end;
  Result := -1;
end;

{ TTLinkedList }

procedure TLinkedList.add(Obj: Pointer);
begin
  AddAfter(Last, Obj);
end;

procedure TLinkedList.AddAfter(item: pLLItem; Obj: Pointer);
var
  NewItem: pLLItem;
begin
  New(NewItem);
  NewItem.Next := nil;
  NewItem.prev := nil;
  NewItem^.Data := Obj;
  Inc(fCount);

  if (item = nil) and (fLast <> nil) then
    item := fLast;
  if item <> nil then
  begin
    NewItem.Next := item.Next;
    if assigned(item.Next) then
      item.Next.prev := NewItem;
    item.Next := NewItem;
    NewItem.prev := item;
    if item = fLast then
      fLast := NewItem;
  end
  else
  begin
    fLast := NewItem;
    fFirst := NewItem;
  end;
end;

procedure TLinkedList.AddBefore(item: pLLItem; Obj: Pointer);
var
  NewItem: pLLItem;
begin
  New(NewItem);
  NewItem.Next := nil;
  NewItem.prev := nil;
  NewItem^.Data := Obj;
  Inc(fCount);

  if item = nil then
    item := fFirst;
  if item = nil then
  begin
    fFirst := NewItem;
    fLast := NewItem;
  end
  else
  begin
    NewItem.prev := item.prev;
    if assigned(item.prev) then
      item.prev.Next := NewItem;
    item.prev := NewItem;
    NewItem.Next := item;
    if item = fFirst then
      fFirst := NewItem;

  end;
end;

procedure TLinkedList.Clear;
var
  p0, p1: pLLItem;
begin
  p0 := First;
  while p0 <> nil do
  begin
    p1 := p0;
    p0 := p0.Next;

    DISPOSE(p1);
  end;

  fFirst := nil;
  fLast := nil;
  fCount := 0;
end;

constructor TLinkedList.Create;
begin
  inherited;
  fFirst := nil;
  fLast := nil;
  fCount := 0;
end;

procedure TLinkedList.Delete(p: pLLItem);
begin
  if p = nil then
    Exit;

  if (p <> fLast) and (p <> fFirst) then
  begin
    if p.prev <> nil then
      p.prev.Next := p.Next;
    if p.Next <> nil then
      p.Next.prev := p.prev;
  end
  else
  begin
    if (p = fFirst) and (p = fLast) then
    begin
      fFirst := nil;
      fLast := nil;
    end
    else if p = fFirst then
    begin
      fFirst := p.Next;
      if fFirst <> nil then
        fFirst.prev := nil;
    end
    else if p = fLast then
    begin
      fLast := p.prev;
      if fLast <> nil then
        fLast.Next := nil;
    end;
  end;

  DISPOSE(p);
  Dec(fCount);
end;

procedure TLinkedList.DeleteLast;
begin
  if fLast <> nil then
    self.Delete(self.fLast);
end;

procedure TLinkedList.DeleteFirst;
begin
  if fFirst <> nil then
    Delete(fFirst);
end;

procedure TLinkedList.Delete(index: Integer);
var
  p: pLLItem;
begin
  p := LLItems[index];

  if p <> nil then
    Delete(p);
end;

destructor TLinkedList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TLinkedList.Exchange(i1, i2: Integer);
var
  p0, p1, p2: pLLItem;
begin
  p1 := LLItems[i1];
  p2 := LLItems[i2];

  if p1.prev <> nil then
    p1.prev.Next := p2;
  if p1.Next <> nil then
    p1.Next.prev := p2;

  if p2.prev <> nil then
    p2.prev.Next := p1;
  if p2.Next <> nil then
    p2.Next.prev := p1;

  p0 := p1.Next;
  p1.Next := p2.Next;
  p2.Next := p0;

  p0 := p1.prev;
  p1.prev := p2.prev;
  p2.prev := p0;

  p0 := p2.Next;
  p2.Next := p1.Next;
  p1.Next := p0;

  p0 := p2.prev;
  p2.prev := p1.prev;
  p1.prev := p0;

end;

function TLinkedList.GetEmpty: boolean;
begin
  Result := fFirst = nil;
end;

function TLinkedList.getItem(index: Integer): Pointer;
var
  p: pLLItem;
begin
  p := LLItems[index];
  if p <> nil then
    Result := p^.Data
  else
    Result := nil;
end;

function TLinkedList.GetLLItems(index: Integer): pLLItem;
var
  i: Integer;
begin
  if (fCount - index) > index then
  begin
    Result := fFirst;
    i := 0;
    while (Result <> nil) and (i <> index) do
    begin
      Result := Result.Next;
      Inc(i);
    end;
  end
  else
  begin
    Result := fLast;
    i := fCount - 1;
    while (Result <> nil) and (i <> index) do
    begin
      Result := Result.prev;
      Dec(i);
    end;
  end;
end;

function TLinkedList.GetNextLLItem(LLItem: pLLItem): pLLItem;
begin
  Result := LLItem^.Next;
end;

function TLinkedList.GetPrevLLItem(LLItem: pLLItem): pLLItem;
begin
  Result := LLItem.prev;
end;

function TLinkedList.IndexOf(p: Pointer; var LLItem: pLLItem): Integer;
var
  pI: pLLItem;
begin
  pI := First;
  Result := -1;
  while pI <> nil do
  begin
    Inc(Result);
    if pI = p then
    begin
      if LLItem <> nil then
        LLItem := pI;
      Exit;
    end
    else
      pI := pI.Next;
  end;
  Result := -1;
end;

procedure TLinkedList.setItem(index: Integer; const Value: Pointer);
var
  p: pLLItem;
begin
  p := LLItems[index];
  if p <> nil then
    p^.Data := Value;
end;

{ TSimpleList<T> }

function TSimpleList<T>.add(const Value: T): boolean;
var
  C, i: Integer;
begin

  if not FSorted then
  begin
    Result := true;
    if Count + 1 >= Capacity then
      Capacity := round((Capacity + 1) * GrowthFactor);

    Items[fCount] := Value;
    Inc(fCount);
  end
  else
  begin
    if self.Count = 0 then
    begin
      Count := 1;
      Items[0] := Value;
      Exit(true);
    end
    else
    begin
      { a small speedup:
        we know, that we mostly add the next bigger item,
        in that case, let us first check the last item, that way we can skip a lot of comparisions }
      i := Count - 1;
      C := fComparer.Compare(Items[i], Value);
      if FDupIgnore and (C = 0) then
        Exit(False)
      else if C < 0 then
        i := Count
      else if Find(Value, i) and FDupIgnore then
        Exit(False);
    end;
    Result := true;
    Insert(i, Value);
  end;
end;

procedure TSimpleList<T>.addList(const avalues: TSimpleList<T>);
begin
  if avalues.Count = 0 then
    Exit;

  if avalues.Sorted then
    self.addSortedArray(avalues.Items, 0, avalues.Count)
  else
    self.append(avalues.Items, 0, avalues.Count);
end;

procedure TSimpleList<T>.addSortedArray(const avalues: array of T; aStartIndex: Integer; acount: Integer);
var
  x, i1, i2: Integer;
  elementsToMove, NewPosToMove: Integer;
begin
  if acount = -1 then
    acount := Length(avalues) - aStartIndex;

  if fHasManagedType then
  begin
    // managed types must be properly assigned, not via move memory
    for x := aStartIndex to (aStartIndex + acount) - 1 do
      add(avalues[x]);
    Exit;
  end;

  if acount = 1 then
  begin
    add(avalues[0]);
    Exit;
  end;

  while acount <> 0 do
  begin

    // can we append all at the end?
    if (self.Count = 0)
      or (fComparer.Compare(Items[Count - 1], avalues[aStartIndex]) < 0) then
    begin
      i1 := Count;
      Count := Count + acount;
      move(avalues[aStartIndex], Items[i1], acount * sizeOf(avalues[0]));
      acount := 0;
    end
    else
      { // can we append the whole Array somewhere In the middle ? } if (not Find(avalues[aStartIndex], i1))
        and
        (not Find(avalues[(aStartIndex + acount) - 1], i2))
        and
        (i1 = i2) then
      begin
        elementsToMove := (Count - i1); // how many elements do we need to move to the end?
        Count := Count + acount; // inc the count and capacity
        // move the existing items to the end, so we have space for inserting the whole array
        move(Items[i1], Items[i1 + acount], elementsToMove * sizeOf(avalues[0]));
        move(avalues[aStartIndex], Items[i1], acount * sizeOf(avalues[0]));
        acount := 0;
      end
      else
      begin
        /// add the first item in the array, maybe we can append the whole array later, without this element
        add(avalues[aStartIndex]);
        Inc(aStartIndex);
        Dec(acount);
      end;
  end;
end;

procedure TSimpleList<T>.append(const values: TArray<T>; aStartIndex: Integer; acount: Integer);
var
  x, i: Integer;
  lValues: TArray<T>;
begin
  if acount = -1 then
    acount := Length(values) - aStartIndex;

  if acount = 0 then
    Exit;

  if self.Sorted and (not fHasManagedType) then
  begin
    lValues := copy(values, aStartIndex, acount);
    TArray.Sort<T>(lValues, fComparer, 0, Length(lValues));
    self.addSortedArray(lValues, 0, Length(lValues));

  end
  else if fHasManagedType then
  begin

    // we need to add it one by one....
    for x := aStartIndex to (aStartIndex + acount) - 1 do
      add(values[x]);

  end
  else
  begin

    i := Count;
    Count := Count + acount;
    move(values[aStartIndex], Items[i], acount * sizeOf(T));
  end;
end;

procedure TSimpleList<T>.Clear;
var
  x: Integer;
begin
  if fHasManagedType then
    for x := 0 to fCount - 1 do
      Items[x] := default (T);

  fCount := 0;
end;

function TSimpleList<T>.Contains(const aValue: T): boolean;
var
  i: Integer;
begin
  if FSorted then
    Result := Find(aValue, i)
  else
    Result := IndexOf(aValue) <> -1;
end;

constructor TSimpleList<T>.Create;
begin
  Create(nil);
end;

constructor TSimpleList<T>.Create(aComparer: IComparer<T>);
begin
  inherited Create;
  fHasManagedType := isManaged(TypeInfo(T));
  fComparer := aComparer;
  if fComparer = nil then
    fComparer := TComparer<T>.default;
  FGrowthFactor := 2.0;
  fCount := 0;
  Items := nil;
end;

procedure TSimpleList<T>.Delete(i: Integer);
begin
  if fHasManagedType then
    Items[i] := default (T);

  if i <> fCount - 1 then
  begin
    move(Items[i + 1], Items[i], (Count - (i + 1)) * sizeOf(Items[0]));

    // important to zero it here, because we moved the memory, if we would assign to a default(T) then we would reduce the reference count
    if fHasManagedType then
      ZeroMemory(@Items[fCount - 1], sizeOf(Items[0]));
  end;

  Dec(fCount);
end;

procedure TSimpleList<T>.Delete(aStartIndex, aNumOfItemsToDelete: Integer);
var
  x: Integer;
  TrailingItemsCount: Integer;
begin
  // check left bound
  if aStartIndex >= fCount then
    Exit;

  // check right bound
  if (aStartIndex + aNumOfItemsToDelete) > fCount then
    aNumOfItemsToDelete := fCount - aStartIndex;

  if fHasManagedType then
    for x := aStartIndex to aStartIndex + aNumOfItemsToDelete - 1 do
      Items[x] := default (T);

  TrailingItemsCount := Count - (aStartIndex + aNumOfItemsToDelete);
  if TrailingItemsCount > 0 then
  begin
    move(Items[aStartIndex + aNumOfItemsToDelete],
      Items[aStartIndex],
      TrailingItemsCount * sizeOf(T));

    // important to zero it here, because we moved the memory, if we would assign to a default(T) then we would reduce the reference count
    if fHasManagedType then
      ZeroMemory(@Items[fCount - TrailingItemsCount], TrailingItemsCount * sizeOf(T));
  end;

  Dec(fCount, aNumOfItemsToDelete);
end;

destructor TSimpleList<T>.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSimpleList<T>.Exchange(i1, i2: Integer);
var
  p: T;
begin
  p := Items[i1];
  Items[i1] := Items[i2];
  Items[i2] := p;
end;

function TSimpleList<T>.getCapacity: Integer;
begin
  Result := Length(Items);
end;

function TSimpleList<T>.getItem(index: Integer): T;
begin
  Result := Items[index];
end;

function TSimpleList<T>.GetRangeIndices(const LeftValue, RightValue: T;
  out LeftIndex, RightIndex: Integer): boolean;
var
  C: Integer;
begin
  Result := False;

  if not FSorted then
    raise Exception.Create(classname + '.GetRangeIndices requires sorted property to be true!');

  if self.Count = 0 then
    Exit(False);

  Find(LeftValue, LeftIndex);

  // left index outside bounds?
  if LeftIndex >= Count then
    Exit(False);

  C := fComparer.Compare(Items[LeftIndex], RightValue);
  // if they are equal, we do not need to search for the right index
  if C = 0 then
  begin
    RightIndex := LeftIndex;
    Exit(true);
  end;

  // but... if leftIndex points to a value greater then the RightValue, then.... we too need to exit
  if C > 0 then
    Exit(False);

  if not Find(RightValue, RightIndex) then
    if RightIndex >= self.Count then
      RightIndex := self.Count - 1;

  Result := true; // finally, if we are here, all is fine!
end;

function TSimpleList<T>.IndexOf(const Value: T): Integer;
var
  i, x: Integer;
begin
  Result := -1;

  if FSorted then
  begin
    if Find(Value, i) then
      Result := i;
    Exit;
  end;

  for x := 0 to Count - 1 do
    if self.fComparer.Compare(Items[x], Value) = 0 then
      Exit(x);
end;

procedure TSimpleList<T>.SetCapacity(const Value: Integer);
begin
  if Capacity <> Value then
  begin
    SetLength(Items, Value);
    if fCount > Capacity then
      fCount := Capacity;
  end;
end;

procedure TSimpleList<T>.setCount(const Value: Integer);
var
  oldCount: Integer;
  x: Integer;
begin
  oldCount := fCount;

  fCount := Value;

  // finalize managed types
  if fHasManagedType then
    if fCount < oldCount then
      for x := fCount to oldCount - 1 do
        Items[x] := default (T);

  if fCount > Capacity then
    Capacity := fCount;
end;

procedure TSimpleList<T>.SetDupIgnore(const Value: boolean);
begin
  FDupIgnore := Value;
  if Value then
    Sorted := true;
end;

procedure TSimpleList<T>.SetGrowthFactor(const Value: single);
begin
  if Value <= 1 then
    raise Exception.Create('GrowthFactor can not be 1 or less');

  FGrowthFactor := Value;
end;

{ TIDSortedListItem }

procedure TSortedList<TKey, TValue>.AddOrSet(const Value: TValue; const id: TKey);
var
  i: Integer;
begin
  if Find(id, i) then
    self.Items[i].Data := Value
  else
    self.Insert(i, Value, id);
end;

function TSortedList<TKey, TValue>.add(const Value: TValue; const id: TKey): boolean;
var
  C, i: Integer;
begin
  if self.Count = 0 then
    i := 0
  else begin
    { a small speedup:
      we know, that we mostly add the next bigger item,
      in that case, let us first check the last item, that way we can skip a lot of comparisions }
    i := Count - 1;
    C := fComparer.Compare(Items[i].IID, id);
    if FDupIgnore and (C = 0) then
      Exit(False)
    else if C < 0 then
      i := Count
    else if Find(id, i) and FDupIgnore then
      Exit(False);
  end;

  Result := true;

  Insert(i, Value, id);
end;

procedure TSortedList<TKey, TValue>.Clear;
var
  x: Integer;
begin
  if fHasManagedType then
    for x := 0 to fCount - 1 do
      Items[x] := default (TIDSortedListItem);

  fCount := 0;
end;

constructor TSortedList<TKey, TValue>.Create;
begin
  Create(nil);
end;

constructor TSortedList<TKey, TValue>.Create(aComparer: IComparer<TKey>);
begin
  inherited Create;
  fHasManagedType := isManaged(TypeInfo(TIDSortedListItem));
  if aComparer = nil then
    fComparer := TComparer<TKey>.default
  else
    fComparer := aComparer;

  FGrowthFactor := 2.0;
  fCount := 0;
  Capacity := 0;
end;

procedure TSortedList<TKey, TValue>.Delete(index: Integer);
begin
  if self.fHasManagedType then
    Items[index] := default (TIDSortedListItem);

  if index <> fCount - 1 then
  begin
    move(Items[index + 1], Items[index], (fCount - (index + 1)) * sizeOf(Items[0]));

    if self.fHasManagedType then
      ZeroMemory(@Items[fCount - 1], sizeOf(Items[0]));
  end;
  Dec(fCount);
end;

destructor TSortedList<TKey, TValue>.Destroy;
begin
  Clear;
  inherited;
end;

function TSortedList<TKey, TValue>.Find(const id: TKey;
  out index: Integer): boolean;
var
  L, H: Integer;
begin
  L := 0;
  H := Count - 1;
  Result := DoFind(id, L, H, index);
end;

function TSortedList<TKey, TValue>.DoFind(const id: TKey;
  var L, H: Integer;
  out index: Integer): boolean;
var
  cmp, C, i: Integer;
begin
  Result := False;

  while L <= H do
  begin
    i := (L + H) shr 1;

    C := fComparer.Compare(Items[i].IID, id);

    if C = 0 then
    begin
      index := i;
      Exit(true);
    end;

    if C < 0 then
      L := i + 1
    else
      H := i - 1;
  end;
  index := L;
end;

function TSortedList<TKey, TValue>.getCapacity: Integer;
begin
  Result := Length(Items);
end;

function TSortedList<TKey, TValue>.GetFirst: TValue;
begin
  if fCount <> 0 then
    Result := Items[0].Data
  else
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
end;

function TSortedList<TKey, TValue>.getItem(index: Integer): TValue;
begin
  if (index < fCount) and (index < Capacity) and (index >= 0) then
    Result := Items[index].Data
  else
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
end;

function TSortedList<TKey, TValue>.GetLast: TValue;
begin
  if fCount <> 0 then
    Result := Items[fCount - 1].Data
  else
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
end;

function TSortedList<TKey, TValue>.IndexOf(const Value: TValue; aOffset: Integer = 0;
  aComparer: IComparer<TValue> = nil): Integer;
var
  x: Integer;
  C: IComparer<TValue>;
begin
  if aComparer <> nil then
    C := aComparer
  else
    C := TComparer<TValue>.default;

  for x := aOffset to fCount - 1 do
    if C.Compare(Items[x].Data, Value) = 0 then
      Exit(x);

  Result := -1;
end;

procedure TSortedList<TKey, TValue>.Insert(aIndex: Integer;
  const Value: TValue; const id: TKey);
begin
  if Count + 1 >= Capacity then
    Capacity := round((Capacity + 1) * GrowthFactor);

  if aIndex <> fCount then
  begin
    move(Items[aIndex], Items[aIndex + 1],
      sizeOf(Items[0]) * (Count - aIndex));
    ZeroMemory(@Items[aIndex], sizeOf(Items[0]));
  end;

  Items[aIndex].Data := Value;
  Items[aIndex].IID := id;

  Inc(fCount);
end;

procedure TSortedList<TKey, TValue>.LoadFromMemory(
  var
    pb: pByte);
var
  C: Integer;
begin
  C := pInteger(pb)^;
  Inc(pb, 4);
  Capacity := C;
  Count := C;
  if C > 0 then
  begin
    move(pb^, Items[0], C * sizeOf(Items[0]));
    Inc(pb, C * sizeOf(Items[0]));
  end;
end;

procedure TSortedList<TKey, TValue>.LoadFromStream(Stream: TStream);
var
  C: Integer;
begin
  Stream.readBuffer(C, 4);
  Capacity := C;
  Count := C;
  if C > 0 then
    Stream.readBuffer(Items[0], C * sizeOf(Items[0]));
end;

procedure TSortedList<TKey, TValue>.SaveToMemory(var bytes: pByte);
var
  s: Integer;
begin
  s := Count * sizeOf(Items[0]);
  pInteger(bytes)^ := fCount;
  Inc(bytes, 4);
  if fCount > 0 then
  begin
    move(Items[0], bytes^, s);
    Inc(bytes, s);
  end;
end;

function TSortedList<TKey, TValue>.SaveToMemorySize: Integer;
begin
  Result := 4 + Count * sizeOf(Items[0]);
end;

procedure TSortedList<TKey, TValue>.SaveToStream(Stream: TStream);
begin
  Stream.writebuffer(fCount, 4);
  if Count > 0 then
    Stream.writebuffer(Items[0], Count * sizeOf(Items[0]));
end;

procedure TSortedList<TKey, TValue>.SetCapacity(const Value: Integer);
begin
  if Value <> Capacity then
    SetLength(Items, Value);

end;

procedure TSimpleList<T>.setItem(index: Integer; const Value: T);
begin
  Items[index] := Value;
end;

procedure TSimpleList<T>.SetSorted(const Value: boolean);
begin
  if FSorted = Value then
    Exit;
  if not Value then
    FDupIgnore := False;

  FSorted := Value;

  if FSorted then
    if fCount <> 0 then
      TArray.Sort<T>(Items, fComparer, 0, fCount);
end;

function TSimpleList<T>.Slice(acount: Integer): TArray<T>;
begin
  SetLength(Result, acount);
  if acount <> 0 then
    move(Items[0], Result[0], sizeOf(T) * acount);
end;

function TSimpleList<T>.toarray: TArray<T>;
begin
  Result := copy(Items, 0, Count);
end;

procedure TSimpleList<T>.TrimExcess;
begin
  Capacity := Count;
end;

function TSimpleList<T>.Find(const Value: T; out index: Integer): boolean;
var
  cmp, C, L, H, i: Integer;
begin
  Result := False;

  if not FSorted then
    raise Exception.Create(classname + '.Find requires sorted property to be true!');

  L := 0;
  H := fCount - 1;
  while L <= H do
  begin
    i := (L + H) shr 1;

    C := fComparer.Compare(Items[i], Value);

    if C = 0 then
    begin
      index := i;
      Exit(true);
    end;

    if C < 0 then
      L := i + 1
    else
      H := i - 1;

  end;
  index := L;
end;

procedure TSimpleList<T>.Insert(index: Integer; const Value: T);
begin
  if Count >= Capacity then
    Capacity := round((Capacity + 1) * GrowthFactor);

  if index < fCount then
  begin
    move(Items[index], Items[index + 1],
      sizeOf(T) * (Count - index));
    ZeroMemory(@Items[index], sizeOf(Items[0]));
  end;
  Items[index] := Value;

  Inc(fCount);
end;

procedure TSimpleList<T>.SaveToStream(Stream: TStream);
begin
  Stream.writebuffer(fCount, 4);
  if fCount <> 0 then
    Stream.writebuffer(Items[0], sizeOf(T) * fCount);
end;

procedure TSimpleList<T>.LoadFromStream(Stream: TStream);
var
  C: Integer;
begin
  Clear;

  Stream.readBuffer(C, 4);
  Capacity := C;
  fCount := C;
  if fCount <> 0 then
    Stream.readBuffer(Items[0], sizeOf(T) * fCount);
end;

{ TIndexedIDSortedList }

procedure TIndexedIDSortedList.add(p: Pointer;
  id:
    single;
  pIndex:
    pInteger);
var
  i, x: Integer;
begin
  Find(id, i);
  if Count >= Capacity then
    Capacity := Capacity + 1;

  if i < fCount then
  begin
    move(Items[i], Items[i + 1], sizeOf(TIndexedIDSortedListItem) * (Count - i));
    for x := i + 1 to fCount do
      Items[x].pIndex^ := x;
  end;

  Items[i].Data := p;
  Items[i].FID := id;
  Items[i].pIndex := pIndex;
  Items[i].pIndex^ := i;

  Inc(fCount);

end;

procedure TIndexedIDSortedList.add(p: Pointer;
  id:
    Integer;
  pIndex:
    pInteger);
var
  i, x: Integer;
begin
  Find(id, i);
  if Count >= Capacity then
    Capacity := Capacity + 1;

  if i < fCount then
  begin
    move(Items[i], Items[i + 1], sizeOf(TIndexedIDSortedListItem) * (Count - i));
    for x := i + 1 to fCount do
      Items[x].pIndex^ := x;
  end;

  Items[i].Data := p;
  Items[i].IID := id;
  Items[i].pIndex := pIndex;
  Items[i].pIndex^ := i;

  Inc(fCount);
end;

procedure TIndexedIDSortedList.Clear;
begin
  fCount := 0;
end;

constructor TIndexedIDSortedList.Create;
begin
  inherited;

  fCount := 0;
  Capacity := 0;
end;

procedure TIndexedIDSortedList.Delete(i: Integer);
var
  x: Integer;
begin
  if (fCount = 1) or (i = fCount - 1) then
  begin
    Dec(fCount);
    Exit;
  end;

  move(Items[i + 1], Items[i], (fCount - (i + 1)) * sizeOf(TIndexedIDSortedListItem));
  for x := i to fCount - 2 do
    Items[x].pIndex^ := x;
  Dec(fCount);
end;

destructor TIndexedIDSortedList.Destroy;
begin
  Capacity := 0;
  inherited;
end;

function TIndexedIDSortedList.Find(id: Integer;
  out index: Integer): boolean;
var
  C, L, H, i: Integer;
begin
  Result := False;
  L := 0;
  H := fCount - 1;
  while L <= H do
  begin
    i := (L + H) shr 1;

    C := Items[i].IID - id;

    if C = 0 then
    begin
      Result := true;
      index := i;
      Exit;
    end;

    if C < 0 then
      L := i + 1
    else
      H := i - 1;

  end;
  index := L;
end;

function TIndexedIDSortedList.Find(id: single;
  out index: Integer): boolean;
var
  C: single;
  L, H, i: Integer;
begin
  Result := False;
  L := 0;
  H := fCount - 1;
  while L <= H do
  begin
    i := (L + H) shr 1;

    C := Items[i].FID - id;

    if C = 0 then
    begin
      Result := true;
      index := i;
      Exit;
    end;

    if C < 0 then
      L := i + 1
    else
      H := i - 1;

  end;
  index := L;
end;

function TIndexedIDSortedList.GetFirst: Pointer;
begin
  if fCount > 0 then
    Result := Items[0].Data
  else
    Result := nil;
end;

function TIndexedIDSortedList.getItem(index: Integer): Pointer;
begin
  Result := Items[index].Data;
end;

function TIndexedIDSortedList.GetLast: Pointer;
begin
  if fCount > 0 then
    Result := Items[fCount - 1].Data
  else
    Result := nil;
end;

function TIndexedIDSortedList.IndexOf(p: Pointer): Integer;
var
  x: Integer;
begin
  Result := -1;
  for x := 0 to fCount - 1 do
  begin
    if Items[x].Data = p then
    begin
      Result := x;
      Exit;
    end;
  end;
end;

procedure TIndexedIDSortedList.SetCapacity(
  const
    Value:
    Integer);
begin
  if Value <> FCapacity then
  begin
    FCapacity := Value;
    SetLength(Items, FCapacity);
  end;
end;

procedure TSortedList<TKey, TValue>.setCount(const Value: Integer);
var
  oldCount: Integer;
  x: Integer;
begin
  if fCount <> Value then
  begin
    oldCount := fCount;
    fCount := Value;

    if fHasManagedType then
      for x := fCount to oldCount - 1 do
        Items[x] := default (TIDSortedListItem);

    if Capacity < Value then
      Capacity := Value;
  end;
end;

procedure TSortedList<TKey, TValue>.setItem(index: Integer;
  const
    Value:
    TValue);
begin
  if (index < fCount) and (index < Capacity) and (index >= 0) then
    self.Items[index].Data := Value
  else
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
end;

{ TIDSortedList2 }

procedure TIDSortedList2.add(p1, p2: Pointer; id: Integer);
var
  d: TIDSortedListItem2;
begin
  d.Data1 := p1;
  d.Data2 := p2;

  inherited add(d, id);
end;

function TIDSortedList2.GetFirst_Data1: Pointer;
begin
  Result := GetFirst.Data1;
end;

function TIDSortedList2.GetFirst_Data2: Pointer;
begin
  Result := GetFirst.Data2;
end;

function TIDSortedList2.GetItem_Data1(index: Integer): Pointer;
begin
  Result := Items[index].Data.Data1;
end;

function TIDSortedList2.GetItem_Data2(index: Integer): Pointer;
begin
  Result := Items[index].Data.Data2;
end;

function TIDSortedList2.GetLast_Data1: Pointer;
begin
  Result := GetLast.Data1;
end;

function TIDSortedList2.GetLast_Data2: Pointer;
begin
  Result := GetLast.Data2;
end;

function TIDSortedList2.IndexOf_data1(p: Pointer): Integer;
var
  x: Integer;
begin
  for x := 0 to fCount - 1 do
    if Items[x].Data.Data1 = p then
      Exit(x);

  Result := -1;

end;

function TIDSortedList2.IndexOf_data2(p: Pointer): Integer;
var
  x: Integer;
begin
  for x := 0 to fCount - 1 do
    if Items[x].Data.Data2 = p then
      Exit(x);

  Result := -1;
end;

procedure TIDSortedList2.SetItem_Data1(index: Integer;
  const
    Value:
    Pointer);
begin
  Items[index].Data.Data1 := Value;
end;

procedure TIDSortedList2.SetItem_Data2(index: Integer;
  const
    Value:
    Pointer);
begin

  Items[index].Data.Data2 := Value;
end;

{ TThreadSaveObjectList }

procedure TThreadSaveObjectList.add(aObject: TObject);
begin
  fSec.Enter;
  fList.add(aObject);
  fSec.Leave;
end;

procedure TThreadSaveObjectList.AddAsFirst(aObject: TObject);
begin
  fSec.Enter;
  fList.Insert(0, aObject);
  fSec.Leave;
end;

procedure TThreadSaveObjectList.Clear;
var
  x: Integer;
begin
  fSec.Enter;
  for x := 0 to fList.Count - 1 do
    fList[x].Free;
  fList.Clear;
  fSec.Leave;
end;

constructor TThreadSaveObjectList.Create;
begin
  inherited;
  fList := TObjectList.Create;
  fList.OwnsObjects := False;
  fSec := TCriticalSection.Create;
end;

destructor TThreadSaveObjectList.Destroy;
begin
  Clear;
  fList.Free;
  fSec.Free;
  inherited;
end;

function TThreadSaveObjectList.GetCount: Integer;
begin
  fSec.Enter;
  Result := fList.Count;
  fSec.Leave;
end;

function TThreadSaveObjectList.GetFirstAndRemFromList(out aObject: TObject): boolean;
begin
  fSec.Enter;
  if fList.Count > 0 then
  begin
    aObject := fList.First;
    Result := aObject <> nil;
    fList.Delete(0);
  end
  else
  begin
    Result := False;
    aObject := nil;
  end;
  fSec.Leave;
end;

function TThreadSaveObjectList.GetLastAndRemFromList(out aObject: TObject): boolean;
begin
  fSec.Enter;
  if fList.Count > 0 then
  begin
    aObject := fList.Last;
    Result := aObject <> nil;
    fList.Delete(fList.Count - 1);
  end
  else
  begin
    Result := False;
    aObject := nil;
  end;
  fSec.Leave;
end;

procedure TSortedList<TKey, TValue>.SetDupIgnore(const Value: boolean);
begin
  FDupIgnore := Value;
end;

procedure TSortedList<TKey, TValue>.SetGrowthFactor(
  const
    Value:
    single);
begin
  if Value > 1.0 then
    FGrowthFactor := Value;
end;

procedure TSortedList<TKey, TValue>.TrimExcess;
begin
  Capacity := Count;
end;

class
  function TSortedList<TKey, TValue>.GetMinRequiredmemorySizeForSaving: Integer;
begin
  Result := 4;
end;

function TSortedList<TKey, TValue>.FindMinMaxIndex(const aId: TKey; out aLowIndex, aHighIndex: Integer): boolean;
var
  i, C: Integer;
  lLow, lHigh: Integer;
begin
  Result := False;
  if DupIgnore then
  begin
    Result := Find(aId, aLowIndex);
    aHighIndex := aLowIndex;
  end else begin
    // we need to make sure we catch the most "left" of the items with the same id...
    lLow := 0;
    lHigh := Count - 1;
    aLowIndex := Count + 1; // will cause to exit of this method if we do not find anything
    // while searching, it will change the low and high values...
    while DoFind(aId, lLow, lHigh, i) do
    begin
      aLowIndex := i;
      lHigh := i - 1; // let search again, this time from the current low bound to the new high bound
      Result := true;
    end;

    // left index outside bounds? - this will be the case if find() did not find anything
    if not Result then
    begin
      aHighIndex := aLowIndex;
      Exit(False);
    end;

    // search for the right index
    lLow := aLowIndex + 1;
    lHigh := Count - 1;
    aHighIndex := aLowIndex;
    while DoFind(aId, lLow, lHigh, i) do
    begin
      aHighIndex := i;
      lLow := i + 1; // let search again, this time from the current low bound to the new high bound
    end;
  end;
end;

function TSortedList<TKey, TValue>.FindValuesBetween(const aLeftKey, aRightKey: TKey;
  out aLowIndex, aHighIndex: Integer): boolean;
var
  C: Integer;
  lLow, lHigh: Integer;
begin
  Result := False;
  if self.Count = 0 then
    Exit(False);

  if DupIgnore then
  begin
    Find(aLeftKey, aLowIndex);
    // note: find returns either the proper index or the index to insert the next item at
    if aLowIndex >= Count then
      Exit(False);
    if not Find(aRightKey, aHighIndex) then
      Dec(aHighIndex);
    Result := aHighIndex >= aLowIndex;
  end else begin
    FindMinMaxIndex(aLeftKey, aLowIndex, lHigh);
    // note: find returns either the proper index or the index to insert the next item at
    if aLowIndex >= Count then
      Exit(False);
    if not FindMinMaxIndex(aRightKey, lLow, aHighIndex) then
      Dec(aHighIndex);
    Result := aHighIndex >= aLowIndex;
  end;
end;

class
  procedure TSortedList<TKey, TValue>.SaveEmptyListToMemory(var pb: pByte);
begin
  pInteger(pb)^ := 0;
  Inc(pb, 4);
end;

{ TListAsRecord<T> }

procedure TListAsRecord<T>.add(const Value: T);
begin
  if fCount <= Capacity then
    Capacity := (fCount + 1) * 2;

  Items[fCount] := Value;
  Inc(fCount);
end;

procedure TListAsRecord<T>.Clear;
begin
  fCount := 0;
end;

class
  function TListAsRecord<T>.Create: TListAsRecord<T>;
begin
  Result := default (TListAsRecord<T>);
end;

function TListAsRecord<T>.getCapacity: Integer;
begin
  Result := Length(Items);
end;

function TListAsRecord<T>.getItem(index: Integer): T;
begin
  Result := Items[index];
end;

procedure TListAsRecord<T>.SetCapacity(const Value: Integer);
begin
  if Value <> Capacity then
  begin
    SetLength(Items, Value);

    if Capacity < fCount then
      fCount := Capacity;
  end;
end;

procedure TListAsRecord<T>.setCount(const Value: Integer);
begin
  fCount := Value;
  if Capacity < fCount then
    Capacity := fCount * 2;
end;

procedure TListAsRecord<T>.setItem(index: Integer; const Value: T);
begin
  Items[index] := Value;
end;

end.
