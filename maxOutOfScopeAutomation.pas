unit maxOutOfScopeAutomation;

{ a simple automation for things that have to be executed when the interface goes out of scope.
  When does a interface go out of scope?
  Let see, for one, when you declate a interface variable as a local variable, inside your method, then the interface goes out of scope, when the method exits.
  if the interface is part of a class, then the interface goes out of scope, when the class is destroyed.
  if the interfae is a field of a record, then the interfacce goes out of scope when the record goes out of scope. double rumble... but a record has no callback when it goes out of scope, an interface calls its destructor. Thats why it sometimes make sense to use a interface as a record field.

  usage:
  var
  Auto: iOutOfScopeAutomation
  begin
  // create MyObject here, it doesn't matter what it is., TStringList, TEdit...
  Auto:= TOutOfScopeAutomation.AutoFree(myObject);
  ...
  end;
  or
  you can use a anonymous procedure as well, for a bit more complex tasks

  auto:= TOutOfScopeAutomation.AutoCall(procedure
  begin
  // do something
  end);
}
interface

uses
  classes, sysutils;

type
  iOutOfScopeAutomation = interface
    ['{A0066486-E591-4336-A3EB-A2BF608A3FB0}']
    Procedure AddToAutoFree(Obj: TObject);
  end;

  TOutOfScopeAutomation = class(TInterfacedobject, iOutOfScopeAutomation)
  private
    fObjects: TList;
    fProc: TThreadProcedure;
  public
    Destructor Destroy; override;

    Procedure AddToAutoFree(Obj: TObject);

    class Function AutoFree(aObject: TObject): iOutOfScopeAutomation; overload;
    class Function AutoFree(aObjects: array of TObject): iOutOfScopeAutomation; overload;
    class Function AutoCall(proc: TThreadProcedure): iOutOfScopeAutomation;
  end;

implementation

{ TOutOfScopeAutomation }

procedure TOutOfScopeAutomation.AddToAutoFree(Obj: TObject);
begin
  if not assigned(fObjects) then
    fObjects := TList.Create;
  fObjects.Add(Obj);
end;

class function TOutOfScopeAutomation.AutoFree(aObject: TObject): iOutOfScopeAutomation;
var
  auto: iOutOfScopeAutomation;
begin
  auto := TOutOfScopeAutomation.Create;
  auto.AddToAutoFree(aObject);
  result := auto;
end;

class function TOutOfScopeAutomation.AutoCall(proc: TThreadProcedure): iOutOfScopeAutomation;
var
  auto: TOutOfScopeAutomation;
begin
  auto := TOutOfScopeAutomation.Create;
  auto.fProc := proc;
  result := auto;
end;

class function TOutOfScopeAutomation.AutoFree(aObjects: array of TObject): iOutOfScopeAutomation;
var
  auto: TOutOfScopeAutomation;
  x: Integer;
begin
  auto := TOutOfScopeAutomation.Create;

  if not assigned(auto.fObjects) then
    auto.fObjects := TList.Create;

  auto.fObjects.capacity := length(aObjects);

  for x := 0 to length(aObjects) - 1 do
    auto.fObjects.Add(aObjects[x]);

  result := auto;
end;

destructor TOutOfScopeAutomation.Destroy;
var
  x: Integer;
begin
  if assigned(fProc) then
    fProc();

  if assigned(fObjects) then
  begin
    for x := 0 to fObjects.Count - 1 do
      TObject(fObjects[x]).Free;
    fObjects.Free;
  end;

  inherited;
end;

end.
