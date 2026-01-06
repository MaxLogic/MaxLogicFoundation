unit MaxLogic.DI.Bootstrap;

{$REGION 'docu'}
(**
  This unit provides a reusable foundation for setting up a Dependency Injection Composition Root using Spring4D.

  Key Components:
  - TDependencyRegistry: A singleton that acts as a central, application-wide
    "shopping list" of required interface dependencies. It is designed to
    ensure application stability, especially in hybrid legacy projects.

  - TDICompositionRoot: A base class that orchestrates the registration
    and validation of dependencies using the Container.

  How to use this:
  1. In your project-specific unit (e.g., Toggl.AppRoot), create a class
     that inherits from TDICompositionRoot.
  2. In the `initialization` section of any unit that has a dependency,
     register that dependency's interface type with the TDependencyRegistry.
     Example:
        `TDependencyRegistry.Instance.RegisterRequirement(TypeInfo(IMyService));`
        or
        `TDependencyRegistry.Instance.RegisterRequirementsFrom(TMyClass);

3. In your application's startup code (.dpr), create your AppRoot and call
   its SetUp method before running the main form. SetUp will:

   - call BeforeRegisterDependencies
   - register all modules
   - build the container
   - optionally ValidateDependencies
   - call AfterRegisterDependencies


1.  **Ultimate Safety in Hybrid/Legacy Systems:** This is its killer feature. In a large, old application, you cannot possibly put every single class into the DI container. You will have old, legacy classes that are created manually (`TMyLegacyClass.Create`). If such a class needs an `ILogger`, it can't get it from a constructor. It will likely ask a global service locator for it. The `TDependencyRegistry` allows this legacy class to still participate in the startup validation. In its unit's `initialization` section, it declares, "Hey, I exist, and I'm going to need an `ILogger`!" The registry adds `ILogger` to the master list, ensuring that even this legacy component's needs are verified at startup.
2.  **Decouples the "Need" from the "Implementation":** A unit containing a service interface and its consumer doesn't need to know *anything* about the concrete implementation. It only needs to know about the `TDependencyRegistry`. It declares its need, and it's up to a completely different part of the application (the `AppRoot`) to provide the implementation. This maintains excellent separation of concerns.
3.  **Prevents Runtime "Time Bombs":** The alternative is discovering a missing dependency at runtime, often triggered by a specific user action (like clicking a button that opens a new form). This is the worst possible time for an application to fail. The registry moves this failure from a random point at runtime to a predictable, guaranteed point at startup, where a developer can immediately see it and fix it.
4.  **Serves as Living Documentation:** A developer can look at the `ValidateDependencies` method and, by extension, the contents of the registry at runtime, to get a perfect, up-to-date list of all the core services the application is built upon. It's a self-documenting map of the application's architecture.

The `TDependencyRegistry` pattern is most valuable and shines brightest in these scenarios:
*   **Gradual Refactoring of Legacy Applications:** This is the #1 use case. When you are slowly introducing DI into a large, existing codebase, this pattern is your safety net. It allows new and old code to coexist safely.
*   **Large, Modular Applications:** In systems with many modules or plugins, where each module can have its own dependencies, the registry allows each module to independently declare its needs without creating tight coupling between them. The main application shell can then validate that all requirements from all loaded modules are met.
*   **High-Reliability Systems:** In applications where runtime failures are unacceptable, this startup validation provides a critical layer of defense against configuration errors.
*   **Teams with Varying Skill Levels:** It creates a simple, easy-to-follow pattern ("If your class needs a service, add it to the registry in the `initialization` section"). This reduces the mental overhead for developers and makes it less likely they will forget a crucial step.



Remark:
  if spring4D does not call your constructors , ensure your have the following declared:
  {$RTTI INHERIT
    METHODS([vcPublic, vcPublished])
    PROPERTIES([vcPublic, vcPublished])
    FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])
  }


**)

{$ENDREGION 'docu'}

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.generics.collections, System.TypInfo,
  Spring, Spring.Container, Spring.Container.Common, Spring.Services;

type

  TValidationMode = (vmRegisteredOnly, vmResolveAtStartup, vmIgnore);


  { Put this on ctor parameters, fields, or properties
  Usage:
    constructor TSomeService.Create(
      [DIRequire] Logger: ILogger;                           // just must be registered
      [DIRequire(vmResolveAtStartup)] Repo: IUserRepository;  // must be resolvable at startup
      const TenantId: Integer                                // runtime param => NOT annotated
    );
  }
  DIRequireAttribute = class(TCustomAttribute)
  public
    Mode: TValidationMode;
    constructor Create(aMode: TValidationMode = vmRegisteredOnly);
  end;


  // Forward declaration
  TDependencyRegistry = class;
  TDICompositionRoot = class;

  /// <summary>
  /// A singleton registry to collect all required dependency types from across the application.
  /// Its primary purpose is to enable robust, fail-fast validation at application startup.
  /// </summary>
  TDependencyRegistry = class
  private
    fRequired: TDictionary<PTypeInfo, TValidationMode>;
    fDirty: Boolean;
    class var fInstance: TDependencyRegistry;
    constructor Create;
    class destructor DestroyClass;
    // Core scanner that works purely on TRttiType (no instance required)
    procedure RegisterRequirementsFromRttiType(const aRttiType: TRttiType);
  public
    destructor Destroy; override;

    /// <summary>
    /// Provides access to the single instance of the registry.
    /// </summary>
    class function Instance: TDependencyRegistry;

    /// <summary>
    /// Declares that the application has a requirement for a service implementing
    /// the interface specified by aTypeInfo. This should be called from the
    /// `initialization` section of units that contain classes with dependencies.
    /// </summary>
    procedure RegisterRequirement(aTypeInfo: PTypeInfo; aMode: TValidationMode = vmRegisteredOnly); overload;
    procedure RegisterRequirement<T>; overload;

    /// <summary>
    /// Scans the given class type for [Inject]-and/or [DIRequire] decorated fields and properties
    /// and registers their service types as requirements.
    /// </summary>
    procedure RegisterRequirementsFrom(aClass: TClass); overload;

    /// Generic convenience overload that scans type T for [Inject] and [DIRequire]fields
    procedure RegisterRequirementsFrom<T: class>; overload;

    /// <summary>
    /// Returns an array of all unique interface types that have been registered
    /// as requirements for the application.
    /// </summary>
    function GetRequiredTypes: TArray<PTypeInfo>;
    function GetMode(aTypeInfo: PTypeInfo): TValidationMode;

    /// <summary>
    /// Returns the list of required service types as a JSON array of strings.
    /// Useful for generating "live architecture" docs."
    /// </summary>
    function DumpRequiredTypesToJson: string;


    /// <summary>
    /// Invokes a callback for each required type with its qualified name.
    /// Use this to log or display the dependency manifest.
    /// </summary>
    procedure LogRequiredTypes(const aLogger: TProc<string>);
  end;

  TDIRegistrationModule = class
  public
    procedure BeforeRegisterDependencies(aRoot: TDICompositionRoot); virtual;
    // Centralized method for registering all application dependencies.
    procedure RegisterDependencies(aContainer: TContainer); virtual;
    // called after RegisterDependencies + fContainer.Build
    procedure AfterRegisterDependencies(aRoot: TDICompositionRoot); virtual;
  end;

  /// <summary>
  /// A base class that provides the core logic for a Composition Root.
  /// It orchestrates the registration and validation of dependencies.
  /// </summary>
  TDICompositionRoot = class
  private Type
    TRegistrationEntry = record
    private
      fModule: TDIRegistrationModule;   // owned by root
      fProc: TProc<TContainer>;         // external procedure-based registration
    end;


  private
    fRegistrationEntries: TList<TRegistrationEntry>;
  protected
    fContainer: TContainer;
    procedure BeforeRegisterDependencies; virtual;
    // Centralized method for registering all application dependencies.
    procedure RegisterDependencies; virtual;
    // called after RegisterDependencies + fContainer.Build
    procedure AfterRegisterDependencies; virtual;

    // Force a fast-fail if a required dependency cannot be resolved.
    procedure ValidateDependencies; virtual;
  public
    // NOTE: if aContainer = nil, then the global container will be used
    constructor Create(aContainer: TContainer = nil);
    destructor destroy; override;

    // Centralized method for registering all application dependencies.
    procedure SetUp(aValidate: Boolean = True); virtual;

    // those methods will be invoked in the `RegisterDependencies` method
    procedure AddModule(aProc: TProc<TContainer>); overload;
    procedure AddModule(const aProcArray: TArray<TProc<TContainer>>); overload;
    // Registers a class-based DI module. The composition root takes ownership of aModule and will free it in its destructor.
    procedure AddModule(aModule: TDIRegistrationModule); overload;
    procedure AddModule(const aModules: TArray<TDIRegistrationModule>); overload;

    property Container: TContainer read fContainer;
  end;

  TContainerHelper = class helper for TContainer
    // lField/property injection for legacy objects that are not created by the container.
    procedure BuildUp(const AInstance: TObject; aOverwriteExisting: Boolean = false);
  end;

implementation

{ helper methods }


function GetValidationMode(const aAttributes: TArray<TCustomAttribute>; out aReq: DIRequireAttribute): TValidationMode;
begin
  aReq:= nil;
  Result:= vmRegisteredOnly;
  for var attr  in aAttributes do
    if attr is DIRequireAttribute then
    begin
      aReq := DIRequireAttribute(attr);
      Result:= aReq.Mode;
      Break;
    end;
end;

function GetInject(const aAttributes: TArray<TCustomAttribute>; out aInject: InjectAttribute): Boolean;
begin
  Result:= False;
  aInject:= nil;
  for var lAttr in aAttributes do
    if lAttr is InjectAttribute then
    begin
      aInject:= InjectAttribute(lAttr);
      Exit(True);
    end;
end;



function ServiceTypeFromAttrOrMember(const aAttr: InjectAttribute; const aMemberType: TRttiType): PTypeInfo;
begin
  Result := nil;
  if Assigned(aAttr) then
    Result := aAttr.ServiceType; // may be nil
  if (Result = nil) and Assigned(aMemberType) then
    Result := aMemberType.Handle;
end;


{$IFDEF DEBUG}
var fPrevInitProc : Procedure;
procedure ValidateRegisteredDependencies;
begin
  if TDependencyRegistry.Instance.fDirty then
    raise Exception.Create(TDependencyRegistry.UnitName + ': You have untested dependencies registered with '+TDependencyRegistry.className);
  if Assigned(fPrevInitProc) then
    fPrevInitProc ();
end;
{$ENDIF}

{ TDependencyRegistry }

class function TDependencyRegistry.Instance: TDependencyRegistry;
begin
  if not assigned(fInstance) then
    fInstance := TDependencyRegistry.Create;
  Result := fInstance;
end;

constructor TDependencyRegistry.Create;
begin
  inherited Create;
  fRequired:= TDictionary<PTypeInfo, TValidationMode>.Create;
end;

destructor TDependencyRegistry.Destroy;
begin
  fRequired.Free;
  inherited;
end;

class destructor TDependencyRegistry.DestroyClass;
begin
  FreeAndNil(fInstance);
end;

procedure TDependencyRegistry.RegisterRequirement<T>;
begin
  RegisterRequirement(TypeInfo(T));
end;


procedure TDependencyRegistry.RegisterRequirement(aTypeInfo: PTypeInfo; aMode: TValidationMode);
var
  lExisting: TValidationMode;
begin
  if not Assigned(aTypeInfo) then
    Exit;
  if aMode = vmIgnore then
    Exit;

  fDirty := True;

  if fRequired.TryGetValue(aTypeInfo, lExisting) then
  begin
    // "ResolveAtStartup" wins
    if Ord(aMode) > Ord(lExisting) then
      fRequired[aTypeInfo] := aMode;
  end else
    fRequired.Add(aTypeInfo, aMode);
end;



procedure TDependencyRegistry.RegisterRequirementsFromRttiType(const aRttiType: TRttiType);
var
  lField: TRttiField;
  lProp: TRttiProperty;
  lAttr: TCustomAttribute;
  lInject: InjectAttribute;
  lServiceType: PTypeInfo;


var
  m: TRttiMethod;
  p: TRttiParameter;
  lReq: DIRequireAttribute;
  lMode: TValidationMode;
begin
  if aRttiType = nil then
    Exit;

  // Fields with [Inject]
  for lField in aRttiType.GetFields do
  begin
    lMode:= GetValidationMode(lField.GetAttributes , lReq);
    lInject:= nil;
    if lMode<> vmIgnore then
      if Assigned(lReq) or GetInject(lField.GetAttributes , lInject) then
      begin
        lServiceType := ServiceTypeFromAttrOrMember(lInject, lField.FieldType);
        RegisterRequirement(lServiceType, lMode);
      end;
  end;

  // Writable properties with [Inject]
  for lProp in aRttiType.GetProperties do
  begin
    if not (lProp.IsWritable) then
      Continue;

    lMode:= GetValidationMode(lProp.GetAttributes , lReq);
    lInject := nil;
    if lMode <> vmIgnore then
      if Assigned(lReq) or GetInject(lProp.GetAttributes , lInject) then
      begin
        lServiceType := ServiceTypeFromAttrOrMember(lInject, lProp.PropertyType);
        RegisterRequirement(lServiceType, lMode);
      end;
  end;

  // Constructors
  for m in aRttiType.GetMethods do
    if m.IsConstructor and (m.Visibility = mvPublic) then
      if Length(m.GetParameters) > 0 then
      begin
        for p in m.GetParameters do
        begin
          if GetValidationMode(p.GetAttributes , lReq) <> vmIgnore then
            if Assigned(lReq) then
              RegisterRequirement(p.ParamType.Handle, lReq.Mode);
        end;
      end;

end;

procedure TDependencyRegistry.RegisterRequirementsFrom(aClass: TClass);
var
  lCtx: TRttiContext;
  lType: TRttiType;
begin
  if not Assigned(aClass) then
    Exit;

  lCtx := TRttiContext.Create;
  try
    lType := lCtx.GetType(aClass); // RTTI by class ref, no instance needed
    RegisterRequirementsFromRttiType(lType);
  finally
    lCtx.Free;
  end;
end;

procedure TDependencyRegistry.RegisterRequirementsFrom<T>;
var
  lCtx: TRttiContext;
  lType: TRttiType;
begin
  lCtx := TRttiContext.Create;
  try
    lType := lCtx.GetType(TypeInfo(T));
    RegisterRequirementsFromRttiType(lType);
  finally
    lCtx.Free;
  end;
end;

function TDependencyRegistry.GetMode(aTypeInfo: PTypeInfo): TValidationMode;
begin
  if not fRequired.TryGetValue(aTypeInfo, Result) then
    Result:= TValidationMode.vmRegisteredOnly; // default
end;

function TDependencyRegistry.GetRequiredTypes: TArray<PTypeInfo>;
begin
  Result := fRequired.Keys.ToArray;
end;


function TDependencyRegistry.DumpRequiredTypesToJson: String;
var
  lTypes: TArray<PTypeInfo>;
  lCtx: TRttiContext;
  lFirst: Boolean;
  lTypeInfo: PTypeInfo;
  lTypeName: string;
  lJson: string;

  function JsonEscape(const aValue: string): string;
  var
    lCh: Char;
  begin
    Result := '';
    for lCh in aValue do
    begin
      case lCh of
        '"':  Result := Result + '\"';
        '\':  Result := Result + '\\';
        #8:   Result := Result + '\b';
        #9:   Result := Result + '\t';
        #10:  Result := Result + '\n';
        #12:  Result := Result + '\f';
        #13:  Result := Result + '\r';
      else
        // Keep it simple: assume everything else is fine as-is
        Result := Result + lCh;
      end;
    end;
  end;

begin
  Result:= '';

  lTypes := GetRequiredTypes;
  lCtx := TRttiContext.Create;
  try
    // Start JSON array
    lJson := '[';
    lFirst := True;

    for lTypeInfo in lTypes do
    begin
      if not Assigned(lTypeInfo) then
      begin
        Continue;
      end;

      lTypeName := lCtx.GetType(lTypeInfo).QualifiedName;

      if not lFirst then
        lJson := lJson + ',';
      lFirst := False;

      lJson := lJson + '"' + JsonEscape(lTypeName) + '"';
    end;

    lJson := lJson + ']';
    Result:= lJson;
  finally
    lCtx.Free;
  end;
end;

procedure TDependencyRegistry.LogRequiredTypes(const aLogger: TProc<string>);
var
  lTypes: TArray<PTypeInfo>;
  lCtx: TRttiContext;
  lTypeInfo: PTypeInfo;
  lTypeName: string;
begin
  if not Assigned(aLogger) then
  begin
    Exit;
  end;

  lTypes := GetRequiredTypes;
  lCtx := TRttiContext.Create;
  try
    for lTypeInfo in lTypes do
    begin
      if not Assigned(lTypeInfo) then
      begin
        Continue;
      end;

      lTypeName := lCtx.GetType(lTypeInfo).QualifiedName;
      aLogger(lTypeName);
    end;
  finally
    lCtx.Free;
  end;
end;


{ TDICompositionRoot }

procedure TDICompositionRoot.AfterRegisterDependencies;
var
  lIndex: Integer;
begin
  // loop over a list that might grow during the iteration
  lIndex:= 0;
  while lIndex < fRegistrationEntries.Count do
  begin
    if fRegistrationEntries[lIndex].fModule <> nil then
      fRegistrationEntries[lIndex].fModule.AfterRegisterDependencies(self);
    inc(lIndex);
  end;
end;

procedure TDICompositionRoot.BeforeRegisterDependencies;
var
  lIndex: Integer;
begin
  // loop over a list that might grow during the iteration
  lIndex:= 0;
  while lIndex < fRegistrationEntries.Count do
  begin
    if fRegistrationEntries[lIndex].fModule <> nil then
      fRegistrationEntries[lIndex].fModule.BeforeRegisterDependencies(self);
    inc(lIndex);
  end;
end;

procedure TDICompositionRoot.AddModule(aProc: TProc<TContainer>);
var
  lModule: TRegistrationEntry;
begin
  if not Assigned(aProc) then
    Exit;

  lModule.fModule := nil;
  lModule.fProc := aProc;
  fRegistrationEntries.Add(lModule);
end;

procedure TDICompositionRoot.AddModule(const aProcArray: TArray<TProc<TContainer>>);
begin
  for var lProc in aProcArray do
    AddModule(lProc);
end;

procedure TDICompositionRoot.AddModule(aModule: TDIRegistrationModule);
var
  lModule: TRegistrationEntry;
begin
  if not Assigned(aModule) then
    Exit;

  lModule.fModule := aModule;
  lModule.fProc := nil;
  fRegistrationEntries.Add(lModule);
end;

procedure TDICompositionRoot.AddModule(const aModules: TArray<TDIRegistrationModule>);
var
  lMod: TDIRegistrationModule;
begin
  for lMod in aModules do
    AddModule(lMod);
end;

constructor TDICompositionRoot.Create(aContainer: TContainer);
begin
  inherited Create;
  if aContainer = nil then
    fContainer := GlobalContainer
  else
    fContainer := aContainer;
  fRegistrationEntries:= TList<TRegistrationEntry>.Create;
end;

destructor TDICompositionRoot.destroy;
begin
  for var x := 0 to fRegistrationEntries.Count-1 do
  begin
    if assigned(fRegistrationEntries[x].fModule) then
      fRegistrationEntries[x].fModule.Free;
  end;
  fRegistrationEntries.Free;
  inherited;
end;

procedure TDICompositionRoot.RegisterDependencies;
var
  lIndex: Integer;
begin
  // loop over a list that might grow during the iteration
  lIndex:= 0;
  while lIndex < fRegistrationEntries.Count do
  begin
    if fRegistrationEntries[lIndex].fModule <> nil then
      fRegistrationEntries[lIndex].fModule.RegisterDependencies(fContainer)
    else if Assigned(fRegistrationEntries[lIndex].fProc) then
      fRegistrationEntries[lIndex].fProc(fContainer);
    inc(lIndex);
  end;
end;

procedure TDICompositionRoot.SetUp(aValidate: Boolean = True);
begin
  BeforeRegisterDependencies;
  RegisterDependencies;
  fContainer.Build;

  if aValidate then
  begin
    // Fail fast if anything is mis-registered or missing.
    ValidateDependencies;
  end;

  AfterRegisterDependencies;
end;

procedure TDICompositionRoot.ValidateDependencies;
var
  lRequiredType: PTypeInfo;
  lTypeName: string;
  lServiceLocator: IServiceLocator;
  lCtx: TRttiContext;
begin
  if not TDependencyRegistry.Instance.fDirty then
    Exit;
  TDependencyRegistry.Instance.fDirty:= False;

  lServiceLocator := TServiceLocatorAdapter.Create(fContainer);

  lCtx := TRttiContext.Create;
  try
    for lRequiredType in TDependencyRegistry.Instance.GetRequiredTypes do
    begin
      lTypeName := lCtx.GetType(lRequiredType).QualifiedName;

      // 1. Check if the interface is registered using the correct method.
      if not lServiceLocator.HasService(lRequiredType) then
        raise EContainerException.CreateFmt(
          'Startup Validation Failed: The required service "%s" is declared as a dependency but is not registered in the DI container.',
          [lTypeName]
          );

      // 2. Try to actually resolve it.
      if TDependencyRegistry.Instance.GetMode(lRequiredType) = vmResolveAtStartup then
        try
          fContainer.Resolve(lRequiredType);
        except
          on e: Exception do
            raise EContainerException.CreateFmt(
              'Startup Validation Failed: Could not resolve the required service "%s". Check its own dependencies. Original Error: %s',
              [lTypeName, e.Message]
              );
        end;
    end;
  finally
    lCtx.Free;
  end;
end;

{ TContainerHelper }

procedure TContainerHelper.BuildUp(const AInstance: TObject; aOverwriteExisting: Boolean = false);
var
  Ctx: TRttiContext;
  lCurType: TRttiType;
  lField: TRttiField;
  lProp: TRttiProperty;

  function IsUnset(const V: TValue): Boolean;
  begin
    if V.IsEmpty then Exit(True);
    if V.Kind = tkClass then Exit(V.AsObject = nil);
    if V.Kind = tkInterface then Exit(V.AsInterface = nil);
    Result := False;
  end;

  function IsInjectableType(aTypeInfo: PTypeInfo): Boolean;
  begin
    Result :=
      Assigned(aTypeInfo) and
      (aTypeInfo^.Kind in [tkInterface, tkClass]);
  end;

  function TryGetInjectionType(
    const Attrs: TArray<TCustomAttribute>;
    const MemberType: TRttiType;
    out TypeInfoToResolve: PTypeInfo
  ): Boolean;
  var
    lReq: DIRequireAttribute;
    lMode: TValidationMode;
    lInject: InjectAttribute;
  begin
    TypeInfoToResolve := nil;

    lMode := GetValidationMode(Attrs, lReq);
    if lMode = vmIgnore then
      Exit(False);

    GetInject(Attrs, lInject);

    // inject if DIRequire present OR Inject present
    if (lReq = nil) and (lInject = nil) then
      Exit(False);

    TypeInfoToResolve := ServiceTypeFromAttrOrMember(lInject, MemberType);
    Result := IsInjectableType(TypeInfoToResolve);
  end;

  procedure InjectFieldIfNeeded(const Obj: TObject; const F: TRttiField; const TI: PTypeInfo);
  var
    lCur: TValue;
  begin
    lCur := F.GetValue(Obj);
    if aOverwriteExisting or IsUnset(lCur) then
      F.SetValue(Obj, Self.Resolve(TI));
  end;

  procedure InjectPropIfNeeded(const Obj: TObject; const P: TRttiProperty; const TI: PTypeInfo);
  var
    lCur: TValue;
  begin
    if (not P.IsWritable) then
      Exit;

    lCur := P.GetValue(Obj);
    if aOverwriteExisting or IsUnset(lCur) then
      P.SetValue(Obj, Self.Resolve(TI));
  end;

var
  TI: PTypeInfo;
begin
  if not Assigned(AInstance) then
    Exit;

  Ctx := TRttiContext.Create;
  try
    lCurType := Ctx.GetType(AInstance.ClassType);
    // scan this class and all its BaseTypes. This makes BuildUp robust even if some derived class has strict RTTI settings.
    while Assigned(lCurType) do
    begin

      // Fields
      for lField in lCurType.GetFields do
      begin
        if not TryGetInjectionType(lField.GetAttributes, lField.FieldType, TI) then
          Continue;

        try
          InjectFieldIfNeeded(AInstance, lField, TI);
        except
          on E: Exception do
            raise EContainerException.CreateFmt(
              'BuildUp failed for %s.%s (%s): %s',
              [AInstance.ClassName, lField.Name, lField.FieldType.Name, E.Message]
            );
        end;
      end;

      // Properties
      for lProp in lCurType.GetProperties do
      begin
        if (not lProp.IsWritable) then
          Continue;

        if not TryGetInjectionType(lProp.GetAttributes, lProp.PropertyType, TI) then
          Continue;

        try
          InjectPropIfNeeded(AInstance, lProp, TI);
        except
          on E: Exception do
            raise EContainerException.CreateFmt(
              'BuildUp failed for %s.%s (%s): %s',
              [AInstance.ClassName, lProp.Name, lProp.PropertyType.Name, E.Message]
            );
        end;
      end;
      lCurType := lCurType.BaseType;
    end;

  finally
    Ctx.Free;
  end;
end;


{ TDIRegistrationModule }

procedure TDIRegistrationModule.AfterRegisterDependencies(
  aRoot: TDICompositionRoot);
begin

end;

procedure TDIRegistrationModule.BeforeRegisterDependencies(
  aRoot: TDICompositionRoot);
begin

end;

procedure TDIRegistrationModule.RegisterDependencies(
  aContainer: TContainer);
begin

end;

{ DIRequireAttribute }

constructor DIRequireAttribute.Create(aMode: TValidationMode);
begin
  inherited Create;
  Mode := aMode;
end;

initialization

  {$IFDEF DEBUG}
  // Deferred validation: called from Application.Initialize in DEBUG builds
  // in case the dependencies are never explicitly validated during startup.
  fPrevInitProc:= InitProc;
  InitProc := @ValidateRegisteredDependencies;
  {$ENDIF}

end.

