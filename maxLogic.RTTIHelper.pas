Unit maxLogic.RTTIHelper;

{ Version: 0.2 }

Interface

Uses
  classes, sysUtils, RTTI;

Type
  TRTTIHelper = Class
  Private
    class var fCtx: TRttiContext;
    class Constructor CreateClass;
    class Destructor DestroyClass;
  Public

    class Function ReadObjectProperty(Const aInstance: Tobject; Const aPropName: String): Tobject;
    class Function ReadProperty(Const aInstance: Tobject; Const aPropName: String): String;
    class Procedure WriteProperty(Const aInstance: Tobject; Const aPropName, aValue: String);
    class Function has(aInstance: Tobject; Const aPropertyName: String): Boolean;
    class Function PropertyIsString(aInstance: Tobject; Const aPropertyName: String): Boolean;
    class Function Call(aInstance: Tobject; const aMethodName: String; const aArgs: Array of TValue): TValue;
  End;

Implementation

Uses
  TypInfo;

class Constructor TRTTIHelper.CreateClass;
Begin
  fCtx := TRttiContext.Create;
End;

class Destructor TRTTIHelper.DestroyClass;
Begin
  fCtx.free;
End;

class Function TRTTIHelper.has(aInstance: Tobject; Const aPropertyName: String): Boolean;
Var
  TypObj: TRttiType;
  Prop: TRttiProperty;
Begin
  TypObj := fCtx.GetType(aInstance.ClassInfo);
  Prop := TypObj.GetProperty(aPropertyName);
  result := assigned(Prop);
End;

class Function TRTTIHelper.PropertyIsString(
  aInstance: Tobject;
  Const aPropertyName: String): Boolean;
Var
  TypObj: TRttiType;
  Prop: TRttiProperty;
Begin
  result := false;
  TypObj := fCtx.GetType(aInstance.ClassInfo);
  Prop := TypObj.GetProperty(aPropertyName);
  If assigned(Prop) Then
    Case Prop.PropertyType.TypeKind Of
      tkString, tkLString, tkWString, tkUString:
        result := True;
    End;
End;

class Function TRTTIHelper.ReadObjectProperty(Const aInstance: Tobject; Const aPropName: String): Tobject;
Var
  TypObj: TRttiType;
  Prop: TRttiProperty;
  propCls: TClass;
Begin
  result := Nil;

  TypObj := fCtx.GetType(aInstance.ClassInfo);
  Prop := TypObj.GetProperty(aPropName);
  If assigned(Prop) Then
  Begin
    If (Prop.PropertyType Is TRttiInstanceType) Then
    Begin
      propCls := TRttiInstanceType(Prop.PropertyType).MetaclassType;
      If (propCls.InheritsFrom(Tobject)) Then
      Begin
        result := Prop.GetValue(aInstance).AsObject;
      End;
    End;
  End;
End;

class Function TRTTIHelper.ReadProperty(Const aInstance: Tobject; Const aPropName: String): String;
Var
  TypObj: TRttiType;
  Prop: TRttiProperty;
Begin
  TypObj := fCtx.GetType(aInstance.ClassInfo);
  Prop := TypObj.GetProperty(aPropName);
  If assigned(Prop) Then
    result := Prop.GetValue(aInstance).asString
  Else
    result := '';
End;

class Procedure TRTTIHelper.WriteProperty(Const aInstance: Tobject; Const aPropName, aValue: String);
Var
  TypObj: TRttiType;
  Prop: TRttiProperty;
Begin
  TypObj := fCtx.GetType(aInstance.ClassInfo);
  Prop := TypObj.GetProperty(aPropName);
  If assigned(Prop) Then
    Prop.SetValue(aInstance, aValue);

End;

class Function TRTTIHelper.Call(aInstance: Tobject; const aMethodName: String; const aArgs: Array of TValue): TValue;
var
  RttiMethod: TRttiMethod;
  TypObj: TRttiType;
begin
  TypObj := fCtx.GetType(aInstance.ClassInfo);

  RttiMethod := TypObj.GetMethod(aMethodName);
  result := RttiMethod.Invoke(aInstance, aArgs);
end;

End.
