unit maxLogic.FastList;

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
  Classes, types, SysUtils, SyncObjs, Contnrs, generics.collections, generics.defaults;

type
  {$IFDEF CLASSIMPORTER}
  Pointer = integer;
  pInteger = integer;
  {$ENDIF}

  { **
    This is just a small record, to help making real small lists, no larger then 255 elements long
    ** }
  TSmallList = packed record
    List: array of Pointer;
    Capacity,
      Count: byte;
  end;

  TListAsRecord<t >= record
  private
    fCount: integer;
    function getCapacity: integer;
    procedure SetCapacity(const Value: integer);
    function GetItem(Index: integer): t;
    procedure Setitem(Index: integer; const Value: t);
    procedure setCount(const Value: integer);
  public
    // direct access
    Items: TArray<t>;

    property Capacity: integer read getCapacity write SetCapacity;
    property Count: integer read fCount write setCount;
    property Item[Index: integer]: t read GetItem write Setitem;
    default;

    procedure add(const Value: t);
    procedure Clear;
    class
      function Create: TListAsRecord<t>;
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

      fCount: integer;
      FGrowthFactor: Single;
      FDupIgnore: boolean;
      procedure SetCapacity(const Value: integer);
      function getCapacity: integer;
      function GetFirst: TValue;
      function GetLast: TValue;

      function GetItem(Index: integer): TValue;
      procedure Setitem(Index: integer; const Value: TValue);
      procedure setCount(const Value: integer);
      procedure SetGrowthFactor(const Value: Single);
      procedure SetDupIgnore(const Value: boolean);
      function DoFind(const Id: TKey;
        var l, H: integer;
        out Index: integer): boolean;
        inline;
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
      function add(const Value: TValue; const Id: TKey): boolean;
      procedure AddOrSet(const Value: TValue; const Id: TKey);
      // if you know exactly where it goes, then you can use this method
      procedure Insert(aIndex: integer; const Value: TValue; const Id: TKey);

      // note: if duplicates are allowed, then this will return an undefined item from the items with the same id. please refer to GetRangeIndices if you need to get the minIndex and maxIndex of the items with the same id
      function find(const Id: TKey; out Index: integer): boolean;
        virtual;
      // please note: it will return just the first item that matches
      // aOffset tells us from which item to start, in other words, how many items to skip
      function IndexOf(const Value: TValue; aOffset: integer = 0;
        aComparer: IComparer<TValue> = nil): integer;

      { Allows us to Retrieve the min and max index of items with the same id
        Note: aLeftIndex and aRightIndex will be the same if there is 1 or less items with the same id }
      function FindMinMaxIndex(const aId: TKey; out aLowIndex, aHighIndex: integer): boolean;

      { will search of items that are between those values  : aLeftKey <= aItem.ID <= aRightKey
        will return false if aRightKey< aLeftKey
        will return false if none of the values are between aLeftKey and aRightKey
        NOTE: on result= false the indices are undefined }
      function FindValuesBetween(const aLeftKey, aRightKey: TKey;
        out aLowIndex, aHighIndex: integer): boolean;

      procedure Delete(Index: integer);
        virtual;
      procedure Clear;
        virtual;
      procedure TrimExcess;
        virtual;

      // re-sort the underlying array
      // useful if you used the insert method to append values
      procedure sort;

      // please note, this will work only if TKey and TValue are neither strings,pointers, classes or interfaces
      // that is here mostly for backwards compatibility
      procedure SaveToStream(Stream: TStream);
      function SaveToMemorySize: integer;
      class function GetMinRequiredmemorySizeForSaving: integer;
      class procedure SaveEmptyListToMemory(var pb: pByte);
      procedure SaveToMemory(var bytes: pByte);
      procedure LoadFromStream(Stream: TStream);
      procedure LoadFromMemory(var pb: pByte);

      property First: TValue read GetFirst;
      property Last: TValue read GetLast;
      property Capacity: integer read getCapacity write SetCapacity;
      // Please note: the new items are of undefined value, they are not filled in with anything - except if they are managed types, in that case they are set to their default values
      property Count: integer read fCount write setCount;
      property Item[Index: integer]: TValue read GetItem write Setitem;
      default;
      property GrowthFactor: Single read FGrowthFactor write SetGrowthFactor;
      // ATTENTION: Existing values are not checked to be unique. this applies only to adding values
      property dupIgnore: boolean read FDupIgnore write SetDupIgnore;
      // ATTENTION: call sort() to re-sort the underlying array
      property Comparer: IComparer<TKey>read fComparer write fComparer;
    end;

    TIdIntegerSortedList = TSortedList<integer, Pointer>;
    TIdFloatSortedList = TSortedList<Single, Pointer>;

    TIDSortedListItem2 = packed record
      Data1: Pointer;
      Data2: Pointer;
    end;

    TIDSortedList2 = class(TSortedList<integer, TIDSortedListItem2>)
    private
      function GetFirst_Data1: Pointer;
      function GetFirst_Data2: Pointer;
      function GetLast_Data1: Pointer;
      function GetLast_Data2: Pointer;
      function GetItem_Data1(Index: integer): Pointer;
      function GetItem_Data2(Index: integer): Pointer;
      procedure SetItem_Data1(Index: integer; const Value: Pointer);
      procedure SetItem_Data2(Index: integer; const Value: Pointer);
    public
      property First_Data1: Pointer read GetFirst_Data1;
      property First_Data2: Pointer read GetFirst_Data2;
      property Last_Data1: Pointer read GetLast_Data1;
      property Last_Data2: Pointer read GetLast_Data2;
      property Item_Data1[Index: integer]: Pointer read GetItem_Data1 write SetItem_Data1;
      property Item_Data2[Index: integer]: Pointer read GetItem_Data2 write SetItem_Data2;

      procedure add(p1, p2: Pointer; Id: integer);
        reintroduce;

      function IndexOf_data1(p: Pointer): integer;
      function IndexOf_data2(p: Pointer): integer;
    end;

    TIndexedIDSortedListItem = packed record
      Data: Pointer;
      pIndex: pInteger;
      case boolean of
        True:
        (IID: integer; );
        False:
        (fid: Single; );
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
      FCapacity: integer;
      fCount: integer;
      procedure SetCapacity(const Value: integer);
      function GetFirst: Pointer;
      function GetLast: Pointer;

      function GetItem(Index: integer): Pointer;

    protected

    public
      Items: array of TIndexedIDSortedListItem;

      property First: Pointer read GetFirst;
      property Last: Pointer read GetLast;
      property Capacity: integer read FCapacity write SetCapacity;
      property Count: integer read fCount;
      property Item[Index: integer]: Pointer read GetItem;
      default;

      constructor Create;
      destructor Destroy;
        override;

      procedure add(p: Pointer; Id: integer; pIndex: pInteger);
        overload;
      procedure add(p: Pointer; Id: Single; pIndex: pInteger);
        overload;

      function find(Id: integer; out Index: integer): boolean;
        overload;
      function find(Id: Single; out Index: integer): boolean;
        overload;

      function IndexOf(p: Pointer): integer;

      procedure Delete(i: integer);
      procedure Clear;
    end;

    { *******************************************************
      Date:      2004-03-30 13:53
      DESCRIPTION:
      THis is an Array based List, like TList, but it does not change its Capacity during deleting or
      Clear Proc. So we have an much smaller memory allocation Rate.
      ******************************************************* }

    TSimpleList<t> = class
    private
      fCount: integer;
      FGrowthFactor: Single;
      fComparer: IComparer<t>;
      FSorted: boolean;
      fHasManagedType: boolean;
      FDupIgnore: boolean;

      procedure SetCapacity(const Value: integer);
      function GetItem(Index: integer): t;
      procedure Setitem(Index: integer; const Value: t);
      procedure SetGrowthFactor(const Value: Single);
      procedure setCount(const Value: integer);
      procedure SetSorted(const Value: boolean);
      function getCapacity: integer;
      procedure SetDupIgnore(const Value: boolean);
    public
      // you have direct access to the array, but please be aware not to mess with it
      Items: array of t;

      function Slice(acount: integer): TArray<t>;

      property Count: integer read fCount write setCount;
      property Capacity: integer read getCapacity write SetCapacity;
      property Item[Index: integer]: t read GetItem write Setitem;
      default;
      // ATTENTION: Existing values are not checked to be unique. this applies only to adding values
      // ATTENTION: use only in combination with sorted!.
      // if dupIgnore = true this will set sorted to true  as well
      property dupIgnore: boolean read FDupIgnore write SetDupIgnore;

      // the multiplier when growing the list. default is 2.0
      property GrowthFactor: Single read FGrowthFactor write SetGrowthFactor;
      // ATTENTION: seting this to false will set dupIgnore to false as well
      property Sorted: boolean read FSorted write SetSorted;

      constructor Create(aComparer: IComparer<t>);
        overload;
      constructor Create;
        overload;
      destructor Destroy;
        override;

      // if DupIgnore is true, then it will check first, if the value does exist.
      // returns true, if the value was added, false otherwise
      function add(const Value: t): boolean;
      // does not check for duplicates, ignores sorted. Be careful, you might end up with a miss sorted list
      procedure Insert(Index: integer; const Value: t);

      // if you want to add only a part of the array, then set aStartIndex and aCount
      // NOTE: doesn't check for duplicate values inside the avalues array
      procedure addSortedArray(const avalues: array of t; aStartIndex: integer; acount: integer);
      procedure addList(const avalues: TSimpleList<t>);

      // note: if sorted = true, the values will be sorted and addSortedArray will be called
      // otherwise the values are just appended at the end
      // NOTE: doesn't check for duplicate values inside the avalues array
      procedure append(const Values: TArray<t>; aStartIndex: integer; acount: integer);

      procedure Clear;
      procedure TrimExcess;
      function IndexOf(const Value: t): integer;
      // requires sorted to be true!
      function find(const Value: t; out Index: integer): boolean;
        virtual;
      function Contains(const aValue: t): boolean;
      // requires sorted to be true!
      // returns false if the range is not contained in the list
      function GetRangeIndices(const LeftValue, RightValue: t; out LeftIndex, RightIndex: integer): boolean;

      procedure Delete(i: integer);
        overload;
      procedure Delete(aStartIndex, aNumOfItemsToDelete: integer);
        overload;

      procedure Exchange(i1, i2: integer);
      function toarray: TArray<t>;

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
      fCount: integer;
      fFirst, fLast: pLLItem;

      function GetLLItems(Index: integer): pLLItem;
      function GetItem(Index: integer): Pointer;
      procedure Setitem(Index: integer; const Value: Pointer);
      function GetNextLLItem(LLItem: pLLItem): pLLItem;
      function GetPrevLLItem(LLItem: pLLItem): pLLItem;
      function GetEmpty: boolean;
    protected

    public
      property LLItems[Index: integer]: pLLItem read GetLLItems;
      property First: pLLItem read fFirst;
      property Last: pLLItem read fLast;
      property Empty: boolean read GetEmpty; // tells if the list is empty or not

      property Next[LLItem: pLLItem]: pLLItem read GetNextLLItem;
      property prev[LLItem: pLLItem]: pLLItem read GetPrevLLItem;

      property Count: integer read fCount;
      property Items[Index: integer]: Pointer read GetItem write Setitem;

      constructor Create;
      destructor Destroy;
        override;

      procedure add(Obj: Pointer);
      procedure AddAfter(Item: pLLItem; Obj: Pointer);
      procedure AddBefore(Item: pLLItem; Obj: Pointer);
      procedure Delete(Index: integer);
        overload;
      procedure Delete(p: pLLItem);
        overload;
      procedure DeleteFirst;
      procedure DeleteLast;

      procedure Exchange(i1, i2: integer);
      procedure Clear;
      function IndexOf(p: Pointer; var LLItem: pLLItem): integer;
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
      FCapacity, fCount, fInitSize: integer;
      function GetItem(Index: integer): Pointer;
    protected

    public
      property Item[Index: integer]: Pointer read GetItem;
      default;
      property Count: integer read fCount;

      constructor Create(InitSize: integer);
      destructor Destroy;
        override;

      procedure Clear;
      procedure ClearHard;

      procedure Delete(Index: integer);
      function IndexOf(p: Pointer): integer;
      procedure add(p: Pointer; pIndex: pInteger);
    end;

    TThreadSaveObjectList = class(TObject)
    private
      fSec: TCriticalSection;
      fList: TObjectList;
      function GetCount: integer;
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

      property Count: integer read GetCount;

    end;

    // windows replacement
    procedure ZeroMemory(p: Pointer; Size: integer);

implementation

uses
  System.RTLConsts, Rtti;
{ TFastList }

procedure ZeroMemory(p: Pointer; Size: integer);
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
  li: integer;

  procedure c(st: integer);
  var
    X: integer;
  begin

    try
      for X := st to fCount - 1 do
      begin
        li := X;
        if fItems[X].pIndex <> nil then
          fItems[X].pIndex^ := -1;
      end;
    except
      c(li + 1);
    end;
  end;

begin
  c(0);
  fCount := 0;
end;

procedure TFastList.ClearHard;
begin
  fCount := 0;
end;

constructor TFastList.Create(InitSize: integer);
begin
  inherited Create;

  FCapacity := InitSize;
  SetLength(fItems, FCapacity);
  fCount := 0;
  fInitSize := InitSize;
end;

procedure TFastList.Delete(Index: integer);
begin
  fItems[Index].pIndex^ := -1;

  if (Index <> fCount - 1) and (fCount <> 1) then
  begin
    fItems[Index] := fItems[fCount - 1];
    fItems[Index].pIndex^ := Index;
  end;
  Dec(fCount);
end;

destructor TFastList.Destroy;
begin
  Clear;
  fItems := nil;

  inherited;
end;

function TFastList.GetItem(Index: integer): Pointer;
begin
  Result := fItems[Index].Obj;
end;

function TFastList.IndexOf(p: Pointer): integer;
var
  X: integer;
begin
  for X := 0 to fCount - 1 do
    if p = fItems[X].Obj then
    begin
      Result := X;
      exit;
    end;
  Result := -1;
end;

{ TTLinkedList }

procedure TLinkedList.add(Obj: Pointer);
begin
  AddAfter(Last, Obj);
end;

procedure TLinkedList.AddAfter(Item: pLLItem; Obj: Pointer);
var
  NewItem: pLLItem;
begin
  new(NewItem);
  NewItem.Next := nil;
  NewItem.prev := nil;
  NewItem^.Data := Obj;
  Inc(fCount);

  if (Item = nil) and (fLast <> nil) then
    Item := fLast;
  if Item <> nil then
  begin
    NewItem.Next := Item.Next;
    if assigned(Item.Next) then
      Item.Next.prev := NewItem;
    Item.Next := NewItem;
    NewItem.prev := Item;
    if Item = fLast then
      fLast := NewItem;
  end
  else
  begin
    fLast := NewItem;
    fFirst := NewItem;
  end;
end;

procedure TLinkedList.AddBefore(Item: pLLItem; Obj: Pointer);
var
  NewItem: pLLItem;
begin
  new(NewItem);
  NewItem.Next := nil;
  NewItem.prev := nil;
  NewItem^.Data := Obj;
  Inc(fCount);

  if Item = nil then
    Item := fFirst;
  if Item = nil then
  begin
    fFirst := NewItem;
    fLast := NewItem;
  end
  else
  begin
    NewItem.prev := Item.prev;
    if assigned(Item.prev) then
      Item.prev.Next := NewItem;
    Item.prev := NewItem;
    NewItem.Next := Item;
    if Item = fFirst then
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

    dispose(p1);
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
    exit;

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

  dispose(p);
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

procedure TLinkedList.Delete(Index: integer);
var
  p: pLLItem;
begin
  p := LLItems[Index];

  if p <> nil then
    Delete(p);
end;

destructor TLinkedList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TLinkedList.Exchange(i1, i2: integer);
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

function TLinkedList.GetItem(Index: integer): Pointer;
var
  p: pLLItem;
begin
  p := LLItems[Index];
  if p <> nil then
    Result := p^.Data
  else
    Result := nil;
end;

function TLinkedList.GetLLItems(Index: integer): pLLItem;
var
  i: integer;
begin
  if (fCount - Index) > Index then
  begin
    Result := fFirst;
    i := 0;
    while (Result <> nil) and (i <> Index) do
    begin
      Result := Result.Next;
      Inc(i);
    end;
  end
  else
  begin
    Result := fLast;
    i := fCount - 1;
    while (Result <> nil) and (i <> Index) do
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

function TLinkedList.IndexOf(p: Pointer; var LLItem: pLLItem): integer;
var
  pi: pLLItem;
begin
  pi := First;
  Result := -1;
  while pi <> nil do
  begin
    Inc(Result);
    if pi = p then
    begin
      if LLItem <> nil then
        LLItem := pi;
      exit;
    end
    else
      pi := pi.Next;
  end;
  Result := -1;
end;

procedure TLinkedList.Setitem(Index: integer; const Value: Pointer);
var
  p: pLLItem;
begin
  p := LLItems[Index];
  if p <> nil then
    p^.Data := Value;
end;

{ TSimpleList<T> }

function TSimpleList<t>.add(const Value: t): boolean;
var
  c, i: integer;
begin

  if not FSorted then
  begin
    Result := True;
    if Count + 1 >= Capacity then
      Capacity := Round((Capacity + 1) * GrowthFactor);

    Items[fCount] := Value;
    Inc(fCount);
  end
  else
  begin
    if self.Count = 0 then
    begin
      Count := 1;
      Items[0] := Value;
      exit(True);
    end
    else
    begin
      { a small speedup:
        we know, that we mostly add the next bigger item,
        in that case, let us first check the last item, that way we can skip a lot of comparisions }
      i := Count - 1;
      c := fComparer.Compare(Items[i], Value);
      if FDupIgnore and (c = 0) then
        exit(False)
      else if c < 0 then
        i := Count
      else if find(Value, i) and FDupIgnore then
        exit(False);
    end;
    Result := True;
    Insert(i, Value);
  end;
end;

procedure TSimpleList<t>.addList(const avalues: TSimpleList<t>);
begin
  if avalues.Count = 0 then
    exit;

  if avalues.Sorted then
    self.addSortedArray(avalues.Items, 0, avalues.Count)
  else
    self.append(avalues.Items, 0, avalues.Count);
end;

procedure TSimpleList<t>.addSortedArray(const avalues: array of t; aStartIndex: integer; acount: integer);
var
  X, i1, i2: integer;
  elementsToMove, NewPosToMove: integer;
begin
  if acount = -1 then
    acount := length(avalues) - aStartIndex;

  if fHasManagedType then
  begin
    // managed types must be properly assigned, not via move memory
    for X := aStartIndex to (aStartIndex + acount) - 1 do
      add(avalues[X]);
    exit;
  end;

  if acount = 1 then
  begin
    add(avalues[0]);
    exit;
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
      { // can we append the whole Array somewhere In the middle ? }if (not find(avalues[aStartIndex], i1))
      and
      (not find(avalues[(aStartIndex + acount) - 1], i2))
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

procedure TSimpleList<t>.append(const Values: TArray<t>; aStartIndex: integer; acount: integer);
var
  X, i: integer;
  lValues: TArray<t>;
begin
  if acount = -1 then
    acount := length(Values) - aStartIndex;

  if acount = 0 then
    exit;

  if self.Sorted and (not fHasManagedType) then
  begin
    lValues := copy(Values, aStartIndex, acount);
    TArray.sort<t>(lValues, fComparer, 0, length(lValues));
    self.addSortedArray(lValues, 0, length(lValues));

  end
  else if fHasManagedType then
  begin

    // we need to add it one by one....
    for X := aStartIndex to (aStartIndex + acount) - 1 do
      add(Values[X]);

  end
  else
  begin

    i := Count;
    Count := Count + acount;
    move(Values[aStartIndex], Items[i], acount * sizeOf(t));
  end;
end;

procedure TSimpleList<t>.Clear;
var
  X: integer;
begin
  if fHasManagedType then
    for X := 0 to fCount - 1 do
      Items[X] := default(t);

  fCount := 0;
end;

function TSimpleList<t>.Contains(const aValue: t): boolean;
var
  i: integer;
begin
  if FSorted then
    Result := find(aValue, i)
  else
    Result := IndexOf(aValue) <> -1;
end;

constructor TSimpleList<t>.Create;
begin
  Create(nil);
end;

constructor TSimpleList<t>.Create(aComparer: IComparer<t>);
begin
  inherited Create;
  fHasManagedType := isManaged(TypeInfo(t));
  fComparer := aComparer;
  if fComparer = nil then
    fComparer := TComparer<t>.default;
  FGrowthFactor := 2.0;
  fCount := 0;
  Items := nil;
end;

procedure TSimpleList<t>.Delete(i: integer);
begin
  if fHasManagedType then
    Items[i] := default(t);

  if i <> fCount - 1 then
  begin
    move(Items[i + 1], Items[i], (Count - (i + 1)) * sizeOf(Items[0]));

    // important to zero it here, because we moved the memory, if we would assign to a default(T) then we would reduce the reference count
    if fHasManagedType then
      ZeroMemory(@Items[fCount - 1], sizeOf(Items[0]));
  end;

  Dec(fCount);
end;

procedure TSimpleList<t>.Delete(aStartIndex, aNumOfItemsToDelete: integer);
var
  X: integer;
  TrailingItemsCount: integer;
begin
  // check left bound
  if aStartIndex >= fCount then
    exit;

  // check right bound
  if (aStartIndex + aNumOfItemsToDelete) > fCount then
    aNumOfItemsToDelete := fCount - aStartIndex;

  if fHasManagedType then
    for X := aStartIndex to aStartIndex + aNumOfItemsToDelete - 1 do
      Items[X] := default(t);

  TrailingItemsCount := Count - (aStartIndex + aNumOfItemsToDelete);
  if TrailingItemsCount > 0 then
  begin
    move(Items[aStartIndex + aNumOfItemsToDelete],
      Items[aStartIndex],
      TrailingItemsCount * sizeOf(t));

    // important to zero it here, because we moved the memory, if we would assign to a default(T) then we would reduce the reference count
    if fHasManagedType then
      ZeroMemory(@Items[fCount - TrailingItemsCount], TrailingItemsCount * sizeOf(t));
  end;

  Dec(fCount, aNumOfItemsToDelete);
end;

destructor TSimpleList<t>.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSimpleList<t>.Exchange(i1, i2: integer);
var
  p: t;
begin
  p := Items[i1];
  Items[i1] := Items[i2];
  Items[i2] := p;
end;

function TSimpleList<t>.getCapacity: integer;
begin
  Result := length(Items);
end;

function TSimpleList<t>.GetItem(Index: integer): t;
begin
  Result := Items[Index];
end;

function TSimpleList<t>.GetRangeIndices(const LeftValue, RightValue: t;
  out LeftIndex, RightIndex: integer): boolean;
var
  c: integer;
begin
  Result := False;

  if not FSorted then
    raise Exception.Create(classname + '.GetRangeIndices requires sorted property to be true!');

  if self.Count = 0 then
    exit(False);

  find(LeftValue, LeftIndex);

  // left index outside bounds?
  if LeftIndex >= Count then
    exit(False);

  c := fComparer.Compare(Items[LeftIndex], RightValue);
  // if they are equal, we do not need to search for the right index
  if c = 0 then
  begin
    RightIndex := LeftIndex;
    exit(True);
  end;

  // but... if leftIndex points to a value greater then the RightValue, then.... we too need to exit
  if c > 0 then
    exit(False);

  if not find(RightValue, RightIndex) then
    if RightIndex >= self.Count then
      RightIndex := self.Count - 1;

  Result := True; // finally, if we are here, all is fine!
end;

function TSimpleList<t>.IndexOf(const Value: t): integer;
var
  i, X: integer;
begin
  Result := -1;

  if FSorted then
  begin
    if find(Value, i) then
      Result := i;
    exit;
  end;

  for X := 0 to Count - 1 do
    if self.fComparer.Compare(Items[X], Value) = 0 then
      exit(X);
end;

procedure TSimpleList<t>.SetCapacity(const Value: integer);
begin
  if Capacity <> Value then
  begin
    SetLength(Items, Value);
    if fCount > Capacity then
      fCount := Capacity;
  end;
end;

procedure TSimpleList<t>.setCount(const Value: integer);
var
  oldCount: integer;
  X: integer;
begin
  oldCount := fCount;

  fCount := Value;

  // finalize managed types
  if fHasManagedType then
    if fCount < oldCount then
      for X := fCount to oldCount - 1 do
        Items[X] := default(t);

  if fCount > Capacity then
    Capacity := fCount;
end;

procedure TSimpleList<t>.SetDupIgnore(const Value: boolean);
begin
  FDupIgnore := Value;
  if Value then
    Sorted := True;
end;

procedure TSimpleList<t>.SetGrowthFactor(const Value: Single);
begin
  if Value <= 1 then
    raise Exception.Create('GrowthFactor can not be 1 or less');

  FGrowthFactor := Value;
end;

{ TIDSortedListItem }

procedure TSortedList<TKey, TValue>.AddOrSet(const Value: TValue; const Id: TKey);
var
  i: integer;
begin
  if find(Id, i) then
    self.Items[i].Data := Value
  else
    self.Insert(i, Value, Id);
end;

function TSortedList<TKey, TValue>.add(const Value: TValue; const Id: TKey): boolean;
var
  c, i: integer;
begin
  if self.Count = 0 then
    i := 0
  else
  begin
    { a small speedup:
      we know, that we mostly add the next bigger item,
      in that case, let us first check the last item, that way we can skip a lot of comparisions }
    i := Count - 1;
    c := fComparer.Compare(Items[i].IID, Id);
    if FDupIgnore and (c = 0) then
      exit(False)
    else if c < 0 then
      i := Count
    else if find(Id, i) and FDupIgnore then
      exit(False);
  end;

  Result := True;

  Insert(i, Value, Id);
end;

procedure TSortedList<TKey, TValue>.Clear;
var
  X: integer;
begin
  if fHasManagedType then
    for X := 0 to fCount - 1 do
      Items[X] := default(TIDSortedListItem);

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

procedure TSortedList<TKey, TValue>.Delete(Index: integer);
begin
  if self.fHasManagedType then
    Items[Index] := default(TIDSortedListItem);

  if Index <> fCount - 1 then
  begin
    move(Items[Index + 1], Items[Index], (fCount - (Index + 1)) * sizeOf(Items[0]));

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

function TSortedList<TKey, TValue>.find(const Id: TKey;
  out Index: integer): boolean;
var
  l, H: integer;
begin
  l := 0;
  H := Count - 1;
  Result := DoFind(Id, l, H, Index);
end;

function TSortedList<TKey, TValue>.DoFind(const Id: TKey;
  var l, H: integer;
  out Index: integer): boolean;
var
  cmp, c, i: integer;
begin
  Result := False;

  while l <= H do
  begin
    i := (l + H) shr 1;

    c := fComparer.Compare(Items[i].IID, Id);

    if c = 0 then
    begin
      Index := i;
      exit(True);
    end;

    if c < 0 then
      l := i + 1
    else
      H := i - 1;
  end;
  Index := l;
end;

function TSortedList<TKey, TValue>.getCapacity: integer;
begin
  Result := length(Items);
end;

function TSortedList<TKey, TValue>.GetFirst: TValue;
begin
  if fCount <> 0 then
    Result := Items[0].Data
  else
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
end;

function TSortedList<TKey, TValue>.GetItem(Index: integer): TValue;
begin
  if (Index < fCount) and (Index < Capacity) and (Index >= 0) then
    Result := Items[Index].Data
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

function TSortedList<TKey, TValue>.IndexOf(const Value: TValue; aOffset: integer = 0;
  aComparer: IComparer<TValue> = nil): integer;
var
  X: integer;
  c: IComparer<TValue>;
begin
  if aComparer <> nil then
    c := aComparer
  else
    c := TComparer<TValue>.default;

  for X := aOffset to fCount - 1 do
    if c.Compare(Items[X].Data, Value) = 0 then
      exit(X);

  Result := -1;
end;

procedure TSortedList<TKey, TValue>.Insert(aIndex: integer;
  const Value: TValue; const Id: TKey);
begin
  if Count + 1 >= Capacity then
    Capacity := Round((Capacity + 1) * GrowthFactor);

  if aIndex <> fCount then
  begin
    move(Items[aIndex], Items[aIndex + 1],
      sizeOf(Items[0]) * (Count - aIndex));
    ZeroMemory(@Items[aIndex], sizeOf(Items[0]));
  end;

  Items[aIndex].Data := Value;
  Items[aIndex].IID := Id;

  Inc(fCount);
end;

procedure TSortedList<TKey, TValue>.LoadFromMemory(
  var
  pb: pByte);
var
  c: integer;
begin
  c := pInteger(pb)^;
  Inc(pb, 4);
  Capacity := c;
  Count := c;
  if c > 0 then
  begin
    move(pb^, Items[0], c * sizeOf(Items[0]));
    Inc(pb, c * sizeOf(Items[0]));
  end;
end;

procedure TSortedList<TKey, TValue>.LoadFromStream(Stream: TStream);
var
  c: integer;
begin
  Stream.readBuffer(c, 4);
  Capacity := c;
  Count := c;
  if c > 0 then
    Stream.readBuffer(Items[0], c * sizeOf(Items[0]));
end;

procedure TSortedList<TKey, TValue>.SaveToMemory(var bytes: pByte);
var
  s: integer;
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

function TSortedList<TKey, TValue>.SaveToMemorySize: integer;
begin
  Result := 4 + Count * sizeOf(Items[0]);
end;

procedure TSortedList<TKey, TValue>.SaveToStream(Stream: TStream);
begin
  Stream.writebuffer(fCount, 4);
  if Count > 0 then
    Stream.writebuffer(Items[0], Count * sizeOf(Items[0]));
end;

procedure TSortedList<TKey, TValue>.SetCapacity(const Value: integer);
begin
  if Value <> Capacity then
    SetLength(Items, Value);

end;

procedure TSimpleList<t>.Setitem(Index: integer; const Value: t);
begin
  Items[Index] := Value;
end;

procedure TSimpleList<t>.SetSorted(const Value: boolean);
begin
  if FSorted = Value then
    exit;
  if not Value then
    FDupIgnore := False;

  FSorted := Value;

  if FSorted then
    if fCount <> 0 then
      TArray.sort<t>(Items, fComparer, 0, fCount);
end;

function TSimpleList<t>.Slice(acount: integer): TArray<t>;
begin
  SetLength(Result, acount);
  if acount <> 0 then
    move(Items[0], Result[0], sizeOf(t) * acount);
end;

function TSimpleList<t>.toarray: TArray<t>;
begin
  Result := copy(Items, 0, Count);
end;

procedure TSimpleList<t>.TrimExcess;
begin
  Capacity := Count;
end;

function TSimpleList<t>.find(const Value: t; out Index: integer): boolean;
var
  cmp, c, l, H, i: integer;
begin
  Result := False;

  if not FSorted then
    raise Exception.Create(classname + '.Find requires sorted property to be true!');

  l := 0;
  H := fCount - 1;
  while l <= H do
  begin
    i := (l + H) shr 1;

    c := fComparer.Compare(Items[i], Value);

    if c = 0 then
    begin
      Index := i;
      exit(True);
    end;

    if c < 0 then
      l := i + 1
    else
      H := i - 1;

  end;
  Index := l;
end;

procedure TSimpleList<t>.Insert(Index: integer; const Value: t);
begin
  if Count >= Capacity then
    Capacity := Round((Capacity + 1) * GrowthFactor);

  if Index < fCount then
  begin
    move(Items[Index], Items[Index + 1],
      sizeOf(t) * (Count - Index));
    ZeroMemory(@Items[Index], sizeOf(Items[0]));
  end;
  Items[Index] := Value;

  Inc(fCount);
end;

procedure TSimpleList<t>.SaveToStream(Stream: TStream);
begin
  Stream.writebuffer(fCount, 4);
  if fCount <> 0 then
    Stream.writebuffer(Items[0], sizeOf(t) * fCount);
end;

procedure TSimpleList<t>.LoadFromStream(Stream: TStream);
var
  c: integer;
begin
  Clear;

  Stream.readBuffer(c, 4);
  Capacity := c;
  fCount := c;
  if fCount <> 0 then
    Stream.readBuffer(Items[0], sizeOf(t) * fCount);
end;

{ TIndexedIDSortedList }

procedure TIndexedIDSortedList.add(p: Pointer;
  Id:
  Single;
  pIndex:
  pInteger);
var
  i, X: integer;
begin
  find(Id, i);
  if Count >= Capacity then
    Capacity := Capacity + 1;

  if i < fCount then
  begin
    move(Items[i], Items[i + 1], sizeOf(TIndexedIDSortedListItem) * (Count - i));
    for X := i + 1 to fCount do
      Items[X].pIndex^ := X;
  end;

  Items[i].Data := p;
  Items[i].fid := Id;
  Items[i].pIndex := pIndex;
  Items[i].pIndex^ := i;

  Inc(fCount);

end;

procedure TIndexedIDSortedList.add(p: Pointer;
  Id:
  integer;
  pIndex:
  pInteger);
var
  i, X: integer;
begin
  find(Id, i);
  if Count >= Capacity then
    Capacity := Capacity + 1;

  if i < fCount then
  begin
    move(Items[i], Items[i + 1], sizeOf(TIndexedIDSortedListItem) * (Count - i));
    for X := i + 1 to fCount do
      Items[X].pIndex^ := X;
  end;

  Items[i].Data := p;
  Items[i].IID := Id;
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

procedure TIndexedIDSortedList.Delete(i: integer);
var
  X: integer;
begin
  if (fCount = 1) or (i = fCount - 1) then
  begin
    Dec(fCount);
    exit;
  end;

  move(Items[i + 1], Items[i], (fCount - (i + 1)) * sizeOf(TIndexedIDSortedListItem));
  for X := i to fCount - 2 do
    Items[X].pIndex^ := X;
  Dec(fCount);
end;

destructor TIndexedIDSortedList.Destroy;
begin
  Capacity := 0;
  inherited;
end;

function TIndexedIDSortedList.find(Id: integer;
  out Index: integer): boolean;
var
  c, l, H, i: integer;
begin
  Result := False;
  l := 0;
  H := fCount - 1;
  while l <= H do
  begin
    i := (l + H) shr 1;

    c := Items[i].IID - Id;

    if c = 0 then
    begin
      Result := True;
      Index := i;
      exit;
    end;

    if c < 0 then
      l := i + 1
    else
      H := i - 1;

  end;
  Index := l;
end;

function TIndexedIDSortedList.find(Id: Single;
  out Index: integer): boolean;
var
  c: Single;
  l, H, i: integer;
begin
  Result := False;
  l := 0;
  H := fCount - 1;
  while l <= H do
  begin
    i := (l + H) shr 1;

    c := Items[i].fid - Id;

    if c = 0 then
    begin
      Result := True;
      Index := i;
      exit;
    end;

    if c < 0 then
      l := i + 1
    else
      H := i - 1;

  end;
  Index := l;
end;

function TIndexedIDSortedList.GetFirst: Pointer;
begin
  if fCount > 0 then
    Result := Items[0].Data
  else
    Result := nil;
end;

function TIndexedIDSortedList.GetItem(Index: integer): Pointer;
begin
  Result := Items[Index].Data;
end;

function TIndexedIDSortedList.GetLast: Pointer;
begin
  if fCount > 0 then
    Result := Items[fCount - 1].Data
  else
    Result := nil;
end;

function TIndexedIDSortedList.IndexOf(p: Pointer): integer;
var
  X: integer;
begin
  Result := -1;
  for X := 0 to fCount - 1 do
  begin
    if Items[X].Data = p then
    begin
      Result := X;
      exit;
    end;
  end;
end;

procedure TIndexedIDSortedList.SetCapacity(
  const
  Value:
  integer);
begin
  if Value <> FCapacity then
  begin
    FCapacity := Value;
    SetLength(Items, FCapacity);
  end;
end;

procedure TSortedList<TKey, TValue>.setCount(const Value: integer);
var
  oldCount: integer;
  X: integer;
begin
  if fCount <> Value then
  begin
    oldCount := fCount;
    fCount := Value;

    if fHasManagedType then
      for X := fCount to oldCount - 1 do
        Items[X] := default(TIDSortedListItem);

    if Capacity < Value then
      Capacity := Value;
  end;
end;

procedure TSortedList<TKey, TValue>.Setitem(Index: integer;
  const
  Value:
  TValue);
begin
  if (Index < fCount) and (Index < Capacity) and (Index >= 0) then
    self.Items[Index].Data := Value
  else
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
end;

procedure TSortedList<TKey, TValue>.sort;
var
  lComparer: IComparer<TIDSortedListItem>;
begin
  lComparer := TComparer<TIDSortedListItem>.Construct(function(const Item1, Item2: TIDSortedListItem): integer
    begin
      Result := fComparer.Compare(Item1.IID, Item2.IID);
    end);

  TArray.sort<TIDSortedListItem>(Items, lComparer, 0, fCount);
end;

{ TIDSortedList2 }

procedure TIDSortedList2.add(p1, p2: Pointer; Id: integer);
var
  d: TIDSortedListItem2;
begin
  d.Data1 := p1;
  d.Data2 := p2;

  inherited add(d, Id);
end;

function TIDSortedList2.GetFirst_Data1: Pointer;
begin
  Result := GetFirst.Data1;
end;

function TIDSortedList2.GetFirst_Data2: Pointer;
begin
  Result := GetFirst.Data2;
end;

function TIDSortedList2.GetItem_Data1(Index: integer): Pointer;
begin
  Result := Items[Index].Data.Data1;
end;

function TIDSortedList2.GetItem_Data2(Index: integer): Pointer;
begin
  Result := Items[Index].Data.Data2;
end;

function TIDSortedList2.GetLast_Data1: Pointer;
begin
  Result := GetLast.Data1;
end;

function TIDSortedList2.GetLast_Data2: Pointer;
begin
  Result := GetLast.Data2;
end;

function TIDSortedList2.IndexOf_data1(p: Pointer): integer;
var
  X: integer;
begin
  for X := 0 to fCount - 1 do
    if Items[X].Data.Data1 = p then
      exit(X);

  Result := -1;

end;

function TIDSortedList2.IndexOf_data2(p: Pointer): integer;
var
  X: integer;
begin
  for X := 0 to fCount - 1 do
    if Items[X].Data.Data2 = p then
      exit(X);

  Result := -1;
end;

procedure TIDSortedList2.SetItem_Data1(Index: integer;
  const
  Value:
  Pointer);
begin
  Items[Index].Data.Data1 := Value;
end;

procedure TIDSortedList2.SetItem_Data2(Index: integer;
  const
  Value:
  Pointer);
begin

  Items[Index].Data.Data2 := Value;
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
  X: integer;
begin
  fSec.Enter;
  for X := 0 to fList.Count - 1 do
    fList[X].Free;
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

function TThreadSaveObjectList.GetCount: integer;
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
  Single);
begin
  if Value > 1.0 then
    FGrowthFactor := Value;
end;

procedure TSortedList<TKey, TValue>.TrimExcess;
begin
  Capacity := Count;
end;

class
  function TSortedList<TKey, TValue>.GetMinRequiredmemorySizeForSaving: integer;
  begin
    Result := 4;
  end;

  function TSortedList<TKey, TValue>.FindMinMaxIndex(const aId: TKey; out aLowIndex, aHighIndex: integer): boolean;
  var
    i, c: integer;
    lLow, lHigh: integer;
  begin
    Result := False;
    if dupIgnore then
    begin
      Result := find(aId, aLowIndex);
      aHighIndex := aLowIndex;
    end
    else
    begin
      // we need to make sure we catch the most "left" of the items with the same id...
      lLow := 0;
      lHigh := Count - 1;
      aLowIndex := Count + 1; // will cause to exit of this method if we do not find anything
      // while searching, it will change the low and high values...
      while DoFind(aId, lLow, lHigh, i) do
      begin
        aLowIndex := i;
        lHigh := i - 1; // let search again, this time from the current low bound to the new high bound
        Result := True;
      end;

      // left index outside bounds? - this will be the case if find() did not find anything
      if not Result then
      begin
        aHighIndex := aLowIndex;
        exit(False);
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
    out aLowIndex, aHighIndex: integer): boolean;
  var
    c: integer;
    lLow, lHigh: integer;
  begin
    Result := False;
    if self.Count = 0 then
      exit(False);

    if dupIgnore then
    begin
      find(aLeftKey, aLowIndex);
      // note: find returns either the proper index or the index to insert the next item at
      if aLowIndex >= Count then
        exit(False);
      if not find(aRightKey, aHighIndex) then
        Dec(aHighIndex);
      Result := aHighIndex >= aLowIndex;
    end
    else
    begin
      FindMinMaxIndex(aLeftKey, aLowIndex, lHigh);
      // note: find returns either the proper index or the index to insert the next item at
      if aLowIndex >= Count then
        exit(False);
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

    procedure TListAsRecord<t>.add(const Value: t);
    begin
      if fCount <= Capacity then
        Capacity := (fCount + 1) * 2;

      Items[fCount] := Value;
      Inc(fCount);
    end;

    procedure TListAsRecord<t>.Clear;
    begin
      fCount := 0;
    end;

    class
      function TListAsRecord<t>.Create: TListAsRecord<t>;
      begin
        Result := default(TListAsRecord<t>);
      end;

      function TListAsRecord<t>.getCapacity: integer;
      begin
        Result := length(Items);
      end;

      function TListAsRecord<t>.GetItem(Index: integer): t;
      begin
        Result := Items[Index];
      end;

      procedure TListAsRecord<t>.SetCapacity(const Value: integer);
      begin
        if Value <> Capacity then
        begin
          SetLength(Items, Value);

          if Capacity < fCount then
            fCount := Capacity;
        end;
      end;

      procedure TListAsRecord<t>.setCount(const Value: integer);
      begin
        fCount := Value;
        if Capacity < fCount then
          Capacity := fCount * 2;
      end;

      procedure TListAsRecord<t>.Setitem(Index: integer; const Value: t);
      begin
        Items[Index] := Value;
      end;

    end.

