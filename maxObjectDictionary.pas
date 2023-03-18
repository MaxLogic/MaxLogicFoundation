unit maxObjectDictionary;
{ Version: 1.1
  history:
  2014-12-23: enforced the TValue be of class type }

interface

uses
  windows, classes, sysUtils, generics.collections, generics.defaults, System.TypInfo, System.SysConst;

type
  TmaxObjectDictionaryWithValueItems<TKey; TValue: class> = class(TObjectDictionary<TKey, TValue>)
  private
    fValues: Tlist<TValue>;
    function GetValueItem(index: Integer): TValue;
  protected
    procedure ValueNotify(const Value: TValue; Action: TCollectionNotification); override;
  public
    constructor Create; overload;
    constructor Create(ACapacity: Integer); overload;
    constructor Create(Ownerships: TDictionaryOwnerships); overload;
    constructor Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer); overload;

    destructor destroy; override;

    procedure Clear; reintroduce;

    Property ValueItem[index: Integer]: TValue read GetValueItem; default;
  end;

implementation

{ TmaxObjectDictionaryWithValueItems<TKey, TValue> }

procedure TmaxObjectDictionaryWithValueItems<TKey, TValue>.Clear;
begin
  // clear first the Value list.
  // this will prevent us from deleting each value one by one, performing a fValues.indexOf for each value...
  fValues.Clear;

  inherited;
end;

constructor TmaxObjectDictionaryWithValueItems<TKey, TValue>.Create;
begin

  Create([doOwnsValues], 0);

end;

constructor TmaxObjectDictionaryWithValueItems<TKey, TValue>.Create(ACapacity: Integer);
begin
  Create([doOwnsValues], ACapacity);
end;

constructor TmaxObjectDictionaryWithValueItems<TKey, TValue>.Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer);
begin
  inherited Create(Ownerships, ACapacity);
  fValues := Tlist<TValue>.Create;
  fValues.Capacity := ACapacity;

  if (TypeInfo(TValue) = nil) or (PTypeInfo(TypeInfo(TValue))^.Kind <> tkClass) then
    raise EInvalidCast.CreateRes(@SInvalidCast);
end;

constructor TmaxObjectDictionaryWithValueItems<TKey, TValue>.Create(Ownerships: TDictionaryOwnerships);
begin
  Create(Ownerships, 0);
end;

destructor TmaxObjectDictionaryWithValueItems<TKey, TValue>.destroy;
begin

  // clear the items, so the notifier will not be called after the value list was destroyed
  Clear;

  fValues.Free;
  fValues := nil;

  inherited;
end;

function TmaxObjectDictionaryWithValueItems<TKey, TValue>.GetValueItem(index: Integer): TValue;
begin
  result := fValues[index];
end;

procedure TmaxObjectDictionaryWithValueItems<TKey, TValue>.ValueNotify(const Value: TValue; Action: TCollectionNotification);
var
  i: Integer;
  o1, o2: TValue;
begin

  inherited;

  if fValues = nil then
    exit;

  case Action of
    cnAdded:
      fValues.Add(Value);
    cnExtracted,
      cnRemoved:
      begin
        o1 := Value;
        for i := 0 to fValues.Count - 1 do
        begin
          o2 := fValues[i];
          if o1 = o2 then
          begin
            fValues.Delete(i);
            break;
          end;
        end;
      end;
  end;

end;

end.
