Unit maxLogic.RTTIHelper;

{ Version: 0.2 }

Interface

Uses
  classes, sysUtils, RTTI, Controls;

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
  End;

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
  v: TValue;
begin
  v := aValue;
  WriteProperty(aInstance, aPropName, v);
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

      { for lAttribute in  lType.GetAttributes do
        begin
        if lAttribute is aLookForAttributeClass then
        Exit(lAttribute);
        end; }
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
  if HandlerValue.TypeInfo = TypeInfo(TNotifyEvent) then
  begin
    TNotifyEvent(MethodRecPtr^)(Args[0].AsObject);
    Exit;
  end;
  if HandlerValue.TypeInfo = TypeInfo(TMouseEvent) then
  begin
    TMouseEvent(MethodRecPtr^)(
      Args[0].AsObject,
      TMouseButton(Args[1].AsOrdinal),
      Args[2].AsType<TShiftState>,
      Args[3].AsInteger, Args[4].AsInteger);
    Exit;
  end;
  if HandlerValue.TypeInfo = TypeInfo(TMouseMoveEvent) then
  begin
    TMouseMoveEvent(MethodRecPtr^)(Args[0].AsObject,
      Args[1].AsType<TShiftState>, Args[2].AsInteger, Args[3].AsInteger);
    Exit;
  end;
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

End.
