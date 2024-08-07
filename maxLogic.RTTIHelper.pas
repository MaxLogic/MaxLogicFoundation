Unit maxLogic.RTTIHelper;

{ Version: 0.2 }

Interface

Uses
  system.classes, system.sysUtils, system.RTTI,
  {$IFNDEF CONSOLE}
  Controls,
  {$ENDIF}
  // for cloning of an object
  rest.JSON, rest.JSON.Types, rest.JsonReflect, system.JSON;

Type
  TRTTIHelper = Class
  Private
    class var fCtx: TRttiContext;
    class Constructor CreateClass;
    class Destructor DestroyClass;
  Public

    class Function ReadObjectProperty(Const aInstance: Tobject; Const aPropName: String): Tobject;
    class Function ReadProperty(Const aInstance: Tobject; Const aPropName: String): String;
    class Function ReadProp(Const aInstance: Tobject; Const aPropName: String): TValue;
    // retrives the name of the type of that property, like "string, "integer" or "TCustomImage"
    class function GetPropertyTypeName(const aInstance: Tobject; const aPropName: String): String;

    class Procedure WriteProperty(Const aInstance: Tobject; Const aPropName, aValue: String); overload;
    class Procedure WriteProperty(Const aInstance: Tobject; Const aPropName: String; const aValue: TValue); overload;
    class Function has(aInstance: Tobject; Const aPropertyName: String): Boolean;
    class Function PropertyIsString(aInstance: Tobject; Const aPropertyName: String): Boolean;
    class Function Call(aInstance: Tobject; const aMethodName: String; const aArgs: Array of TValue): TValue;

    class Function FindMethodByAttribute<T: TCustomAttribute>(aInstance: Tobject;
      out aMethodName: String; out aAttribute: T): Boolean; overload;
    class Function FindMethodByAttribute<T: TCustomAttribute>(aClass: TClass;
      out aMethodName: String; out aAttribute: T): Boolean; overload;
    class Function FindMethodByAttribute<T: TCustomAttribute>(aClass: TClass;
      out aMethodName: String; out aAttribute: T; out aRttiMethod: TRttiMethod): Boolean; overload;
    class Function FindMethodByAttribute<T: TCustomAttribute>(aInstance: Tobject;
      out aMethodName: String): Boolean; overload;

    class Function GetAttribute<T: TCustomAttribute>(aInstance: Tobject): T; overload;
    class Function GetAttribute<T: TCustomAttribute>(aClass: TClass): T; overload;

    // invokes a parameterless constructor
    class Function CreateClassInstanceFromRttiType(aRttiType: TRttiType): Tobject;
    class Function CreateInstanceFromTClass(aClass: TClass): Tobject;

    // CallEventHandler: calls an Event handler, that is a bit different then the method...
    // see notes and source and orginal author:
    // https://delphihaven.wordpress.com/2009/09/13/calling-an-event-handler-using-rtti/
    // usage:
    // CallEventHandler(MyButton, 'OnClick', [MyButton]);
    class function CallEventHandler(Instance: Tobject; const EventName: string; const Args: array of TValue): TValue; overload;
    class function CallEventHandler(Instance: Tobject; Event: TRttiProperty; const Args: array of TValue): TValue; overload;

    // clones an object using json serialization
    class function CloneObject<T: Class, constructor>(aObj: T): T;
  End;

  {$IF CompilerVersion < 35.0}

  // Delphi 11 Alexandria
  TCustomAttributeClass = class of TCustomAttribute;

  TRttiObjectHelper = class helper for TRttiObject
    function GetAttribute(AAttrClass: TCustomAttributeClass): TCustomAttribute; overload;
    function GetAttribute<T: TCustomAttribute>: T; overload; inline;
    function HasAttribute(AAttrClass: TCustomAttributeClass): Boolean; overload; inline;
    function HasAttribute<T: TCustomAttribute>: Boolean; overload; inline;
  end;
  {$IFEND}


Implementation

Uses
  TypInfo;

resourcestring
  SMissingEvent = '%s does not have an event called %s';
  SPropertyNotAnEvent = '%s.%s is not an event';
  SEventHandlerHasInsufficientRTTI = 'Event handler does not ' +
    'have the required RTTI to be dynamically invoked';

  { TRTTIHelper }

class Constructor TRTTIHelper.CreateClass;
Begin
  fCtx := TRttiContext.Create;
End;

class function TRTTIHelper.CreateClassInstanceFromRttiType(
  aRttiType: TRttiType): Tobject;
var
  lConstructor: TRttiMethod;
  lParams: TArray<TRttiParameter>;
begin
  Result := nil;
  if aRttiType.TypeKind = tkClass then
    for lConstructor in aRttiType.GetMethods('Create') do
      if lConstructor.IsConstructor then
      begin
        lParams := lConstructor.GetParameters;
        if (Length(lParams) = 0) then
        begin
          Result := lConstructor.Invoke(aRttiType.AsInstance.MetaclassType, []).AsObject;
          break; // for lConstructor
        end
      end;
end;

class function TRTTIHelper.CreateInstanceFromTClass(
  aClass: TClass): Tobject;
var
  T: TRttiType;
  v: TValue;
begin
  T := fCtx.GetType(aClass);
  v := T.GetMethod('Create').Invoke(T.AsInstance.MetaclassType, []);
  Result := v.AsObject;
end;

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
  Result := assigned(Prop);
End;

class Function TRTTIHelper.PropertyIsString(
  aInstance: Tobject;
  Const aPropertyName: String): Boolean;
Var
  TypObj: TRttiType;
  Prop: TRttiProperty;
Begin
  Result := false;
  TypObj := fCtx.GetType(aInstance.ClassInfo);
  Prop := TypObj.GetProperty(aPropertyName);
  If assigned(Prop) Then
    Case Prop.PropertyType.TypeKind Of
      tkString, tkLString, tkWString, tkUString:
        Result := True;
    End;
End;

class Function TRTTIHelper.ReadObjectProperty(Const aInstance: Tobject; Const aPropName: String): Tobject;
Var
  TypObj: TRttiType;
  Prop: TRttiProperty;
  propCls: TClass;
Begin
  Result := Nil;

  TypObj := fCtx.GetType(aInstance.ClassInfo);
  Prop := TypObj.GetProperty(aPropName);
  If assigned(Prop) Then
  Begin
    If (Prop.PropertyType Is TRttiInstanceType) Then
    Begin
      propCls := TRttiInstanceType(Prop.PropertyType).MetaclassType;
      If (propCls.InheritsFrom(Tobject)) Then
      Begin
        Result := Prop.GetValue(aInstance).AsObject;
      End;
    End;
  End;
End;

class function TRTTIHelper.GetPropertyTypeName(const aInstance: Tobject; const aPropName: String): String;
var
  TypObj: TRttiType;
  Prop: TRttiProperty;
begin
  Result := '';

  // Get the RTTI type for the given instance
  TypObj := fCtx.GetType(aInstance.ClassInfo);
  if not assigned(TypObj) then
    Exit;

  // Get the RTTI property for the given property name
  Prop := TypObj.GetProperty(aPropName);
  if not assigned(Prop) then
    Exit;

  // Get the name of the property type
  Result := Prop.PropertyType.ToString;
end;

class Function TRTTIHelper.ReadProperty(Const aInstance: Tobject; Const aPropName: String): String;
var
  v: TValue;
begin
  v := ReadProp(aInstance, aPropName);
  if v.IsEmpty then
    Result := ''
  else
    Result := v.ToString;
end;

class Function TRTTIHelper.ReadProp(Const aInstance: Tobject; Const aPropName: String): TValue;
Var
  TypObj: TRttiType;
  Prop: TRttiProperty;
Begin
  TypObj := fCtx.GetType(aInstance.ClassInfo);
  Prop := TypObj.GetProperty(aPropName);
  If assigned(Prop) Then
    Result := Prop.GetValue(aInstance)
  Else
    Result := TValue.Empty;
End;

class Procedure TRTTIHelper.WriteProperty(Const aInstance: Tobject; Const aPropName, aValue: String);
var
  vTmp, vNew, vOld: TValue;
  i32: integer;
  i64: int64;
  d: double;
  c: Char;
begin
  vTmp := aValue;
  vOld := ReadProp(aInstance, aPropName);
  if not vTmp.TryCast(vOld.typeInfo, vNew) then
  begin
    case vOld.Kind of
      tkInteger:
        begin
          i32 := StrToInt(aValue);
          vNew := i32;
        end;
      tkInt64:
        begin
          i64 := StrToInt(aValue);
          vNew := i64;
        end;

      tkChar, tkWChar:
        begin
          if aValue <> '' then
            c := aValue[1]
          else
            c := #0;
          vNew := c;
        end;

      tkFloat:
        begin
          if aValue = '' then
            d := 0
          else
            d := strToFloat(aValue);
          vNew := d;
        end;
      tkEnumeration:
        begin
          // GetEnumValue will return an integer that you can then typecast back to the value in the enum. Assuming we have a variable called value of type Integer and we use the S variable from earlier
          i32 := GetEnumValue(vOld.typeInfo, aValue);
          // and now cast to the proper type
          vNew := TValue.FromOrdinal(vOld.typeInfo, i32);
        end;

      { tkString: ;
        : ;
        tkSet: ;
        tkClass: ;
        tkMethod: ;
        tkLString: ;
        tkWString: ;
        tkVariant: ;
        tkArray: ;
        tkRecord: ;
        tkInterface: ;
        tkDynArray: ;
        tkUString: ;
        tkClassRef: ;
        tkPointer: ;
        tkProcedure: ;
        tkMRecord: ; }
    else
      raise EConvertError.Create('Error can not convert "' + aValue + '" to TValue of Kind: ' +
          TRttiEnumerationType.GetName<TTypeKind>(vOld.Kind));
    end;
  end;

  WriteProperty(aInstance, aPropName, vNew);
end;

class Procedure TRTTIHelper.WriteProperty(Const aInstance: Tobject; Const aPropName: String; const aValue: TValue);
Var
  TypObj: TRttiType;
  Prop: TRttiProperty;
Begin
  TypObj := fCtx.GetType(aInstance.ClassInfo);
  Prop := TypObj.GetProperty(aPropName);
  If assigned(Prop) Then
    Prop.SetValue(aInstance, aValue);

End;

class Function TRTTIHelper.GetAttribute<T>(aInstance: Tobject): T;
begin
  Result := GetAttribute<T>(aInstance.ClassType);
end;

class Function TRTTIHelper.GetAttribute<T>(aClass: TClass): T;
var
  lContext: TRttiContext;
  lType: TRttiType;
  lAttribute: TCustomAttribute;
begin
  Result := nil;
  lContext := TRttiContext.Create;
  try
    lType := lContext.GetType(aClass);
    if lType.HasAttribute<T> then
      Result := lType.GetAttribute<T>
  finally
    lContext.free
  end;

end;

class Function TRTTIHelper.FindMethodByAttribute<T>(aInstance: Tobject; out aMethodName: String): Boolean;
var
  a: T;
begin
  Result := FindMethodByAttribute<T>(aInstance, aMethodName, a);
end;

class Function TRTTIHelper.FindMethodByAttribute<T>(aInstance: Tobject; out aMethodName: String; out aAttribute: T): Boolean;
begin
  Result := FindMethodByAttribute<T>(aInstance.ClassType, aMethodName, aAttribute);
end;

class Function TRTTIHelper.FindMethodByAttribute<T>(aClass: TClass; out aMethodName: String; out aAttribute: T): Boolean;
var
  lRttiMethod: TRttiMethod;
begin
  Result := FindMethodByAttribute<T>(aClass, aMethodName, aAttribute, lRttiMethod);
end;

class Function TRTTIHelper.FindMethodByAttribute<T>(aClass: TClass;
  out aMethodName: String; out aAttribute: T; out aRttiMethod: TRttiMethod): Boolean;
var
  lTypObj: TRttiType;
  lAttribute: TCustomAttribute;
  lRttiMethod: TRttiMethod;
begin
  Result := false;
  lTypObj := fCtx.GetType(aClass);
  for lRttiMethod in lTypObj.GetMethods do
  begin

    if lRttiMethod.HasAttribute<T> then
    begin
      aAttribute := lRttiMethod.GetAttribute<T>;
      aRttiMethod := lRttiMethod;
      aMethodName := lRttiMethod.Name;
      Exit(True);
    end;
  end;
end;

class Function TRTTIHelper.Call(aInstance: Tobject; const aMethodName: String; const aArgs: Array of TValue): TValue;
var
  RttiMethod: TRttiMethod;
  TypObj: TRttiType;
begin
  TypObj := fCtx.GetType(aInstance.ClassInfo);

  RttiMethod := TypObj.GetMethod(aMethodName);
  Result := RttiMethod.Invoke(aInstance, aArgs);
end;

class function TRTTIHelper.CallEventHandler(Instance: Tobject; Event: TRttiProperty;
  const Args: array of TValue): TValue;
var
  HandlerValue: TValue;
  HandlerObj: Tobject;
  MethodRecPtr: ^TMethod;
  RttiMethod: TRttiMethod;
begin
  if Event.PropertyType.TypeKind <> tkMethod then
    raise EInvocationError.CreateFmt(SPropertyNotAnEvent, [Instance.ClassName, Event.Name]);
  Result := nil;
  HandlerValue := Event.GetValue(Instance);
  if HandlerValue.IsEmpty then
    Exit;
  MethodRecPtr := HandlerValue.GetReferenceToRawData;
  { check for event types we know }
  if HandlerValue.typeInfo = typeInfo(TNotifyEvent) then
  begin
    TNotifyEvent(MethodRecPtr^)(Args[0].AsObject);
    Exit;
  end;
  {$IFNDEF CONSOLE}
  if HandlerValue.typeInfo = typeInfo(TMouseEvent) then
  begin
    TMouseEvent(MethodRecPtr^)(
      Args[0].AsObject,
      TMouseButton(Args[1].AsOrdinal),
      Args[2].AsType<TShiftState>,
      Args[3].AsInteger, Args[4].AsInteger);
    Exit;
  end;
  if HandlerValue.typeInfo = typeInfo(TMouseMoveEvent) then
  begin
    TMouseMoveEvent(MethodRecPtr^)(Args[0].AsObject,
      Args[1].AsType<TShiftState>, Args[2].AsInteger, Args[3].AsInteger);
    Exit;
  end;
  {$ENDIF}

  { still here? well, let's go for the generic approach }
  HandlerObj := MethodRecPtr.Data;
  for RttiMethod in fCtx.GetType(HandlerObj.ClassType).GetMethods do
    if RttiMethod.CodeAddress = MethodRecPtr.Code then
    begin
      Result := RttiMethod.Invoke(HandlerObj, Args);
      Exit;
    end;
  raise EInsufficientRtti.Create(SEventHandlerHasInsufficientRTTI);
end;

class function TRTTIHelper.CloneObject<T>(aObj: T): T;
var
  lJson: String;
begin
  lJson := TJson.ObjectToJsonString(aObj);
  Result := TJson.JsonToObject<T>(lJson);
end;

class function TRTTIHelper.CallEventHandler(Instance: Tobject; const EventName: string;
  const Args: array of TValue): TValue;
var
  Prop: TRttiProperty;
begin
  Prop := fCtx.GetType(Instance.ClassType).GetProperty(EventName);
  if Prop = nil then
    raise EInvocationError.CreateFmt(SMissingEvent, [Instance.ClassName, EventName]);
  Result := CallEventHandler(Instance, Prop, Args);
end;

{ TRttiObjectHelper }

{$IF CompilerVersion < 35.0}


// Delphi 11 Alexandria
function TRttiObjectHelper.GetAttribute(
  AAttrClass: TCustomAttributeClass): TCustomAttribute;
var
  LAttr: TCustomAttribute;
begin
  for LAttr in GetAttributes do
    if LAttr is AAttrClass then
      Exit(LAttr);
  Result := nil;

end;

function TRttiObjectHelper.GetAttribute<T>: T;
begin
  Result := T(GetAttribute(T));
end;

function TRttiObjectHelper.HasAttribute(
  AAttrClass: TCustomAttributeClass): Boolean;
begin
  Result := GetAttribute(AAttrClass) <> nil;
end;

function TRttiObjectHelper.HasAttribute<T>: Boolean;
begin
  Result := HasAttribute(T);
end;
{$IFEND}

End.
