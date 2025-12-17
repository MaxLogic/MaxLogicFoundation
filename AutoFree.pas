unit AutoFree;

interface

uses
  System.Classes, System.Generics.Collections, System.SysUtils;

type
  IGarbo = interface
    ['{A6E17957-C233-4433-BCBD-3B53C0C2C596}']
    function Obj: TObject;
    // called after the object is freed
    function AddAfterFree(aProc: TThreadProcedure): IGarbo;
  end;

  // Aggregator for keeping multiple IGarbo references alive to the same scope
  TGarbos = record
  public
    Items: array of IGarbo;
    Count: Integer;

    procedure Add(const aGarbo: IGarbo); overload;
    function Add(aObj: TObject): IGarbo; overload;

    procedure Clear;

    class operator Initialize(out aGarbos: TGarbos);
    class operator Finalize(var aGarbos: TGarbos);
  end;

  TGarbo = class(TInterfacedObject, IGarbo)
  private
    fObj: TObject;
    fAfterFreedProcedures: TList<TThreadProcedure>;
  public
    constructor Create(aObjectToGC: TObject);
    destructor Destroy; override;

    function Obj: TObject;
    function AddAfterFree(aProc: TThreadProcedure): IGarbo;

    class function GC<T: class>(out aInstance: T; aObj: T): IGarbo; overload; static; inline;
    class procedure GC<T: class>(out aInstance: T; aObj: T; out aGarboInstance: IGarbo); overload; static; inline;
    class procedure GC<T: class>(out aInstance: T; aObj: T; var aGarbos: TGarbos); overload; static; inline;
  end;

  TProc = System.SysUtils.TProc;

  ICallProcOnDestroy = interface
    ['{7D3DDF6A-3A2F-44F8-A83B-D7F66F66C559}']
    procedure SetProc(const aValue: TProc);
    function GetProc: TProc;
    property Proc: TProc read GetProc write SetProc;
  end;

  TCallProcOnDestroy = class(TInterfacedObject, ICallProcOnDestroy)
  private
    fProc: TProc;
    procedure SetProc(const aValue: TProc);
    function GetProc: TProc;
  public
    constructor Create(aProc: TProc);
    destructor Destroy; override;
    class function New(aProc: TProc): ICallProcOnDestroy;

    property Proc: TProc read GetProc write SetProc;
  end;

procedure GC(aObj: TObject; out aGarboInstance: IGarbo); overload;
procedure GC(aObj: TObject; var aGarbos: TGarbos); overload;
function GC(aObj: TObject): IGarbo; overload; inline;

procedure GC(var aInstance; aObj: TObject; out aGarboInstance: IGarbo); overload;
procedure GC(var aInstance; aObj: TObject; var aGarbos: TGarbos); overload;
function GC(var aInstance; aObj: TObject): IGarbo; overload; inline;

// run this anonymous method when exiting the current scope
function RunOnExit(aProc: TProc): ICallProcOnDestroy; inline;
// shorter alias for RunOnExit
function Trap(aProc: TProc): ICallProcOnDestroy; inline;

implementation

function RunOnExit(aProc: TProc): ICallProcOnDestroy;
begin
  Result := TCallProcOnDestroy.New(aProc);
end;

function Trap(aProc: TProc): ICallProcOnDestroy;
begin
  Result := TCallProcOnDestroy.New(aProc);
end;

{ TGarbo }

class procedure TGarbo.GC<T>(out aInstance: T; aObj: T; out aGarboInstance: IGarbo);
begin
  aInstance := aObj;
  aGarboInstance := AutoFree.GC(TObject(aObj));
end;

class procedure TGarbo.GC<T>(out aInstance: T; aObj: T; var aGarbos: TGarbos);
var
  lGarbo: IGarbo;
begin
  aInstance := aObj;
  lGarbo := AutoFree.GC(TObject(aObj));
  aGarbos.Add(lGarbo);
end;

class function TGarbo.GC<T>(out aInstance: T; aObj: T): IGarbo;
begin
  aInstance := aObj;
  Result := AutoFree.GC(TObject(aObj));
end;

function TGarbo.AddAfterFree(aProc: TThreadProcedure): IGarbo;
begin
  if not Assigned(fAfterFreedProcedures) then
  begin
    fAfterFreedProcedures := TList<TThreadProcedure>.Create;
  end;

  fAfterFreedProcedures.Add(aProc);
  Result := Self;
end;

constructor TGarbo.Create(aObjectToGC: TObject);
begin
  inherited Create;
  fObj := aObjectToGC;
end;

destructor TGarbo.Destroy;
var
  x: Integer;
begin
  FreeAndNil(fObj);

  if Assigned(fAfterFreedProcedures) then
  begin
    try
      for x := 0 to fAfterFreedProcedures.Count - 1 do
      begin
        fAfterFreedProcedures[x]();
      end;
    finally
      fAfterFreedProcedures.Free;
    end;
  end;

  inherited;
end;

function TGarbo.Obj: TObject;
begin
  Result := fObj;
end;

{ TCallProcOnDestroy }

constructor TCallProcOnDestroy.Create(aProc: TProc);
begin
  inherited Create;
  fProc := aProc;
end;

destructor TCallProcOnDestroy.Destroy;
begin
  if Assigned(fProc) then
  begin
    fProc();
  end;

  inherited;
end;

function TCallProcOnDestroy.GetProc: TProc;
begin
  Result := fProc;
end;

class function TCallProcOnDestroy.New(aProc: TProc): ICallProcOnDestroy;
begin
  Result := TCallProcOnDestroy.Create(aProc);
end;

procedure TCallProcOnDestroy.SetProc(const aValue: TProc);
begin
  fProc := aValue;
end;

{ Global GC overloads }

procedure GC(var aInstance; aObj: TObject; out aGarboInstance: IGarbo);
begin
  aGarboInstance := GC(aInstance, aObj);
end;

procedure GC(var aInstance; aObj: TObject; var aGarbos: TGarbos);
begin
  aGarbos.Add(GC(aInstance, aObj));
end;

procedure GC(aObj: TObject; out aGarboInstance: IGarbo);
begin
  aGarboInstance := GC(aObj);
end;

procedure GC(aObj: TObject; var aGarbos: TGarbos);
begin
  aGarbos.Add(GC(aObj));
end;

function GC(var aInstance; aObj: TObject): IGarbo;
begin
  // untyped var cannot be assigned directly -> cast to TObject reference
  TObject(aInstance) := aObj;
  Result := TGarbo.Create(aObj);
end;

function GC(aObj: TObject): IGarbo;
begin
  Result := TGarbo.Create(aObj);
end;

{ TGarbos }

procedure TGarbos.Add(const aGarbo: IGarbo);
var
  lCapacity: Integer;
begin
  lCapacity := Length(Items);

  if Count >= lCapacity then
  begin
    if lCapacity = 0 then
      lCapacity := 1
    else if lCapacity = 1 then
      lCapacity := 4
    else if lCapacity >= 1024 then
      lCapacity := lCapacity + 1024
    else
      lCapacity := lCapacity * 2;

    SetLength(Items, lCapacity);
  end;

  Items[Count] := aGarbo;
  Inc(Count);
end;

function TGarbos.Add(aObj: TObject): IGarbo;
begin
  Result := GC(aObj);
  Add(Result);
end;

procedure TGarbos.Clear;
var
  x: Integer;
begin
  // release in reverse to preserve intended lifetime order
  for x := Count - 1 downto 0 do
    Items[x] := nil;

  Items := nil;
  Count := 0;
end;

class operator TGarbos.Initialize(out aGarbos: TGarbos);
begin
  aGarbos.Items := nil;
  aGarbos.Count := 0;
end;

class operator TGarbos.Finalize(var aGarbos: TGarbos);
begin
  aGarbos.Clear;
end;

end.

