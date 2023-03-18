Unit AutoFree;

{$I JEDI.INC}


{ Copyright: Pawel Piotrowski, MaxLogic, www.maxlogic.eu
  License: free to use, no warranty of any kind

  Version: 1.7
  History:
  2022-05-01: all "old" projects are migrated, so I adding back the overloded gc() function just to make thinks cleaner again
  2022-02-17: fix for delphi 11: previously the TGarbo was a array, that was released in the order first-in-last-out, in delphi 11, it is the other way around... first-in-first-out... which destroys the order of the destruction of interfaces contained in this array. So now it is a managed record
  2021-05-28: stupid delphi 10.4 broke the life cycle of the interfaces..., if gc() is called in begin end, then the interface will be destroyed on "end" and not as before, when the method exits...
  2017-05-17: added Method RunOnExit(TProc)
  2016-07-26: added an interface that calls a proc when it is destroyed

  Inspired by:
  http://stackoverflow.com/questions/415958/how-to-automatically-free-classes-objects


}

Interface

Uses
  sysUtils, classes, Generics.collections;

Type

  IGarbo = Interface
    ['{A6E17957-C233-4433-BCBD-3B53C0C2C596}']
    Function Obj: TObject;
    // this function will be called after the obj is freed
    Function addAfterFree(aProc: TThreadProcedure): IGarbo;
  End;

  // this one will allow you to declare a single local variable and then use it to store all the references to iGarbo instances produced by gc()
  TGarbos = record
    Items: Array Of IGarbo;
    {$IFDEF DELPHI27_UP            }
    class operator Finalize (var aGarbos: TGarbos);
    {$ENDIF}
  end;

  TGarbo = Class(TInterfacedObject, IGarbo)
  Private
    FObj: TObject;
    fAfterFreedProcedures: TList<TThreadProcedure>;
  Public
    Constructor Create(AObjectToGC: TObject);
    Destructor Destroy; Override;
    Function Obj: TObject;

    // this function will be called after the obj is freed
    Function addAfterFree(aProc: TThreadProcedure): IGarbo;
  End;

  Tproc = reference To Procedure;

  iCallProcOnDestroy = Interface
    ['{7D3DDF6A-3A2F-44F8-A83B-D7F66F66C559}']

    Procedure SetProc(Const Value: Tproc);
    Function GetProc: Tproc;
    Property Proc: Tproc Read GetProc Write SetProc;
  End;

  TCallProcOnDestroy = Class(TInterfacedObject, iCallProcOnDestroy)
  Private
    FProc: Tproc;
    Procedure SetProc(Const Value: Tproc);
    Function GetProc: Tproc;
  Public
    Constructor Create(aProc: Tproc);
    Destructor Destroy; Override;
    Class Function New(aProc: Tproc): iCallProcOnDestroy;

    Property Proc: Tproc Read GetProc Write SetProc;
  End;

  { The GC function simply returns an object in the form of an interface.
    just call gc(MyObjectInstance); and you can forgett about the free part
    NOTE: it is not necessary to assign the result to a local variable, delphi does it for you in the background
    Except delphi 10 releases the instance when it exit the begin...end block, not neccessary when exiting the method..
  }
Function GC2(Obj: TObject): IGarbo; Overload;

// To help enforce the move To d10.4 .. .
Procedure GC(Obj: TObject; Out garboInstance: IGarbo); Overload;
Procedure GC(Obj: TObject; Var garboInstances: TGarbos); Overload;

{$IFNDEF EnforceSeparateDeclarations}
Function GC(Obj: TObject):IGarbo; Overload; inline;
{$ENDIF}

// this one allows this syntax:
// var myList:TStringList
// gc(myList, TStringList.create);
Function GC2(Var aInstance; Obj: TObject): IGarbo; Overload;

// To help enforce the move To d10.4 .. .
Procedure GC(Var aInstance; Obj: TObject; Out garboInstance: IGarbo); Overload;
Procedure GC(Var aInstance; Obj: TObject; Var garboInstances: TGarbos); Overload;

{$IFNDEF EnforceSeparateDeclarations}
Function GC(Var aInstance; Obj: TObject): IGarbo; Overload; inline;
{$ENDIF}

// run this anonymous method when exiting the current method
// ATTENTION: on delphi 10.4 will be run when exiting the begin..end block
Function RunOnExit(aProc: Tproc): iCallProcOnDestroy;

Implementation


Function GC2(Obj: TObject): IGarbo;
Begin
  Result := TGarbo.Create(Obj);
End;

Function GC2(Var aInstance; Obj: TObject): IGarbo;
Begin
  TObject(aInstance) := Obj;
  Result := TGarbo.Create(Obj);
End;

Function RunOnExit(aProc: Tproc): iCallProcOnDestroy;
Begin
  Result := TCallProcOnDestroy.New(aProc);
End;

{ TGarbo }

Function TGarbo.addAfterFree(aProc: TThreadProcedure): IGarbo;
Begin
  If Not assigned(fAfterFreedProcedures) Then
    fAfterFreedProcedures := TList<TThreadProcedure>.Create;
  fAfterFreedProcedures.add(aProc);
  Result := self;
End;

Constructor TGarbo.Create(AObjectToGC: TObject);
Begin
  Inherited Create;
  FObj := AObjectToGC;
End;

Destructor TGarbo.Destroy;
Var
  x: Integer;
Begin
  If assigned(FObj) Then
    FreeAndNil(FObj);

  If assigned(fAfterFreedProcedures) Then
  Begin
    Try
      For x := 0 To fAfterFreedProcedures.Count - 1 Do
        fAfterFreedProcedures[x](); // run it
    Finally
      fAfterFreedProcedures.Free;
    End;
  End;

  Inherited;
End;

Function TGarbo.Obj: TObject;
Begin
  Result := FObj;
End;

{ TCallProcOnDestroy }

Destructor TCallProcOnDestroy.Destroy;
Begin
  If assigned(FProc) Then
    FProc();
  Inherited;
End;

Procedure TCallProcOnDestroy.SetProc(Const Value: Tproc);
Begin
  FProc := Value;
End;

Function TCallProcOnDestroy.GetProc: Tproc;
Begin
  Result := FProc;
End;

Constructor TCallProcOnDestroy.Create(aProc: Tproc);
Begin
  Inherited Create;
  FProc := aProc;
End;

Class Function TCallProcOnDestroy.New(aProc: Tproc): iCallProcOnDestroy;
Begin
  Result := TCallProcOnDestroy.Create(aProc);
End;

Procedure GC(Var aInstance; Obj: TObject; Out garboInstance: IGarbo);
Begin
  garboInstance := GC2(aInstance, Obj)
End;

Procedure GC(Var aInstance; Obj: TObject; Var garboInstances: TGarbos);
Var
  len: Integer;
Begin
  len := Length(garboInstances.Items);
  setLength(garboInstances.Items, len + 1);
  garboInstances.Items[len] := GC2(aInstance, Obj);
End;

Procedure GC(Obj: TObject; Out garboInstance: IGarbo);
Begin
  garboInstance := GC2(Obj);
End;


{$IFNDEF EnforceSeparateDeclarations}
Function GC(Var aInstance; Obj: TObject): IGarbo;
begin
  Result := GC2(aInstance, Obj);
end;
Function GC(Obj: TObject):IGarbo;
begin
  Result := GC2(Obj);
end;
{$ENDIF}

Procedure GC(Obj: TObject; Var garboInstances: TGarbos);
Var
  len: Integer;
Begin
  len := Length(garboInstances.Items);
  setLength(garboInstances.Items, len + 1);
  garboInstances.Items[len] := GC2(Obj)
End;


{ TGarbos }

{$IFDEF DELPHI27_UP            }
class operator TGarbos.Finalize (var aGarbos: TGarbos);
var
  x: Integer;
begin
  for x := length(aGarbos.items)-1 downto 0 do
    aGarbos.items[x]:= Nil;

  aGarbos.items:= Nil;
end;
{$ENDIF}

End.
