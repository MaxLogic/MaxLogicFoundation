unit maxLogic.SelfSaveableObjects;

interface

uses windows, SysUtils, classes, types,
  OmniXml,
  TypInfo, RTTI;

type

  TSelfSaveAbleObject = class(TPersistent)
  private

  protected

  public
    Constructor Create;
    Destructor Destroy; override;

        Procedure LoadFromFile(Const FileName: string); Virtual;
    Procedure SaveToFile(Const FileName: string); Virtual;

    // TVisibilityClasses = set of (vcPrivate, vcProtected, vcPublic, vcPublished);
  end;

  TObjectMap = class
  public

  end;




Procedure SaveObjectToNode(Obj: Tobject; Node: ixMLNode);

implementation


Procedure SaveObjectToNode(Obj: Tobject; Node: ixMLNode);
{ 01.program Project11;
  03.uses
  04.StdCtrls, TypInfo, Classes, Rtti;
  05.
  06.var
  07.c : TRttiContext;
  08.m : TRttiMethod;
  09.t : TRttiInstanceType;
  10.SL : TValue;
  11.Lines : TValue;
  12.begin
  13.c := TRttiContext.Create;
  14.t := (c.FindType('Classes.TStringList') as TRttiInstanceType);
  15.SL := t.GetMethod('Create').Invoke(t.MetaclassType,[]);
  16.t.GetMethod('Add').Invoke(SL,['Hello Do you like my hat?']);
  17.t.GetMethod('Add').Invoke(SL,['I like that hat, what a party hat!']);
  18.Lines := t.GetProperty('Text').GetValue(SL.AsObject);

  ???
  19.Writeln(Lines.ToString);
  20.c.Free;
  21.readln;
  22.end. }
var
  c: TRttiContext;
  t: TRttiType;
  ti: TRttiInstanceType;
  v: TValue;
  m : TRttiMember;
  Prop: TRttiProperty;
Props: TArray<TRttiProperty>;
Field: TRttiField;
Fields: TArray<TRttiField>;

begin
  c := TRttiContext.Create;
  try
    t := c.GetType(Obj.ClassInfo);

    //GetFields: TArray<TRttiField>; virtual;
    //GetProperties: TArray<TRttiProperty>; virtual;

    //t.GetProperty('Text').GetValue(SL.AsObject);

    finally
      c.Free;
    end;

    end;

    { TSelfSaveAbleObject }



    constructor TSelfSaveAbleObject.Create;
    begin
      inherited Create;

    end;

    destructor TSelfSaveAbleObject.Destroy;
    begin

      inherited;
    end;

    procedure TSelfSaveAbleObject.LoadFromFile(
    const
    FileName:
      string);
    begin

    end;

    procedure TSelfSaveAbleObject.SaveToFile(
    const
    FileName:
      string);
    begin

    end;

    end.
