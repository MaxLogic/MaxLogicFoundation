unit maxLogic.CustomCompositionRoot;

{
  This unit provides a reusable foundation for setting up a Dependency Injection Composition Root using Spring4D.

  Key Components:
  - TDependencyRegistry: A singleton that acts as a central, application-wide
    "shopping list" of required interface dependencies. It is designed to
    ensure application stability, especially in hybrid legacy projects.

  - TCustomCompositionRoot: A base class that orchestrates the registration
    and validation of dependencies using the Container.

  How to use this:
  1. In your project-specific unit (e.g., Toggl.AppRoot), create a class
     that inherits from TCustomCompositionRoot.
  2. In the `initialization` section of any unit that has a dependency,
     register that dependency's interface type with the TDependencyRegistry.
     Example: `TDependencyRegistry.Instance.RegisterRequirement(TypeInfo(IMyService));`
  3. In your application's startup code (.dpr), call your AppRoot's
     `RegisterDependencies` and then `ValidateDependencies` methods before
     running the main form. This ensures a "fail-fast" startup if any
     dependency contract is not met.

1.  **Ultimate Safety in Hybrid/Legacy Systems:** This is its killer feature. In a large, old application, you cannot possibly put every single class into the DI container. You will have old, legacy classes that are created manually (`TMyLegacyClass.Create`). If such a class needs an `ILogger`, it can't get it from a constructor. It will likely ask a global service locator for it. The `TDependencyRegistry` allows this legacy class to still participate in the startup validation. In its unit's `initialization` section, it declares, "Hey, I exist, and I'm going to need an `ILogger`!" The registry adds `ILogger` to the master list, ensuring that even this legacy component's needs are verified at startup.
2.  **Decouples the "Need" from the "Implementation":** A unit containing a service interface and its consumer doesn't need to know *anything* about the concrete implementation. It only needs to know about the `TDependencyRegistry`. It declares its need, and it's up to a completely different part of the application (the `AppRoot`) to provide the implementation. This maintains excellent separation of concerns.
3.  **Prevents Runtime "Time Bombs":** The alternative is discovering a missing dependency at runtime, often triggered by a specific user action (like clicking a button that opens a new form). This is the worst possible time for an application to fail. The registry moves this failure from a random point at runtime to a predictable, guaranteed point at startup, where a developer can immediately see it and fix it.
4.  **Serves as Living Documentation:** A developer can look at the `ValidateDependencies` method and, by extension, the contents of the registry at runtime, to get a perfect, up-to-date list of all the core services the application is built upon. It's a self-documenting map of the application's architecture.

The `TDependencyRegistry` pattern is most valuable and shines brightest in these scenarios:
*   **Gradual Refactoring of Legacy Applications:** This is the #1 use case. When you are slowly introducing DI into a large, existing codebase, this pattern is your safety net. It allows new and old code to coexist safely.
*   **Large, Modular Applications:** In systems with many modules or plugins, where each module can have its own dependencies, the registry allows each module to independently declare its needs without creating tight coupling between them. The main application shell can then validate that all requirements from all loaded modules are met.
*   **High-Reliability Systems:** In applications where runtime failures are unacceptable, this startup validation provides a critical layer of defense against configuration errors.
*   **Teams with Varying Skill Levels:** It creates a simple, easy-to-follow pattern ("If your class needs a service, add it to the registry in the `initialization` section"). This reduces the mental overhead for developers and makes it less likely they will forget a crucial step.

}

(** Remark:
  if spring4D does not call your construcot, ensure your have the following declared:
  {$RTTI EXPLICIT METHODS([vcPublic, vcPublished]) PROPERTIES([vcPublic, vcPublished])}
**)

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.generics.collections, System.TypInfo,
  Spring, Spring.Container, Spring.Container.Common, Spring.Services;

type
  // Forward declaration
  TDependencyRegistry = class;
  TCustomCompositionRoot = class;

  /// <summary>
  /// A singleton registry to collect all required dependency types from across the application.
  /// Its primary purpose is to enable robust, fail-fast validation at application startup.
  /// </summary>
  TDependencyRegistry = class
  private
    fRequiredTypes: THashSet<PTypeInfo>;
    fDirty: Boolean;
    class var fInstance: TDependencyRegistry;
    constructor Create;
    class destructor DestroyClass;
    // Core scanner that works purely on TRttiType (no instance required)
    procedure RegisterRequirementsFromRttiType(const aRttiType: TRttiType);
  public
    destructor Destroy; override;

    /// <summary>
    /// Provides thread-safe access to the single instance of the registry.
    /// </summary>
    class function Instance: TDependencyRegistry;

    /// <summary>
    /// Declares that the application has a requirement for a service implementing
    /// the interface specified by aTypeInfo. This should be called from the
    /// `initialization` section of units that contain classes with dependencies.
    /// </summary>
    procedure RegisterRequirement(aTypeInfo: PTypeInfo); overload;
    procedure RegisterRequirement<T>; overload;

    /// <summary>
    ///   scan an object for [Inject] on fields/properties and register those dependencies
    /// </summary>
    procedure RegisterRequirementsFrom(aClass: TClass); overload;

    // generic convenience
    procedure RegisterRequirementsFrom<T: class>; overload;


    /// <summary>
    /// Returns an array of all unique interface types that have been registered
    /// as requirements for the application.
    /// </summary>
    function GetRequiredTypes: TArray<PTypeInfo>;
  end;

  /// <summary>
  /// A base class that provides the core logic for a Composition Root.
  /// It orchestrates the registration and validation of dependencies.
  /// </summary>
  TCustomCompositionRoot = class
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
    // Centralized method for registering all application dependencies.
    procedure SetUp(aValidate: Boolean = True); virtual;

    // field injection good for legacy objects, that are not created by the container
    procedure BuildUp(const AInstance: TObject; aOverwriteExisting: Boolean = false);

    property Container: TContainer read fContainer;
  end;

  TContainerHelper = class helper for TContainer
    // field injection good for legacy objects, that are not created by the container
    procedure BuildUp(const AInstance: TObject; aOverwriteExisting: Boolean = false);
  end;

implementation

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
  fRequiredTypes := THashSet<PTypeInfo>.Create;
end;

destructor TDependencyRegistry.Destroy;
begin
  fRequiredTypes.Free;
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

procedure TDependencyRegistry.RegisterRequirement(aTypeInfo: PTypeInfo);
begin
  if not assigned(aTypeInfo) then
    exit;
  fDirty:= True;

  // Using the type name as the key prevents duplicate entries.
  fRequiredTypes.Add(aTypeInfo);
end;



procedure TDependencyRegistry.RegisterRequirementsFromRttiType(const aRttiType: TRttiType);
var
  lField: TRttiField;
  lProp: TRttiProperty;
  lAttr: TCustomAttribute;
  lInject: InjectAttribute;
  lServiceType: PTypeInfo;

  function ServiceTypeFromAttrOrMember(const aAttr: InjectAttribute; const aMemberType: TRttiType): PTypeInfo;
  begin
    Result := nil;
    if Assigned(aAttr) then
      Result := aAttr.ServiceType; // may be nil
    if (Result = nil) and Assigned(aMemberType) then
      Result := aMemberType.Handle;
  end;

begin
  if aRttiType = nil then
    Exit;

  // Fields with [Inject]
  for lField in aRttiType.GetFields do
  begin
    lInject := nil;
    for lAttr in lField.GetAttributes do
      if lAttr is InjectAttribute then
      begin
        lInject := InjectAttribute(lAttr);
        Break;
      end;

    if Assigned(lInject) then
    begin
      lServiceType := ServiceTypeFromAttrOrMember(lInject, lField.FieldType);
      RegisterRequirement(lServiceType);
    end;
  end;

  // Writable properties with [Inject]
  for lProp in aRttiType.GetProperties do
  begin
    if not lProp.IsWritable then
      Continue;

    lInject := nil;
    for lAttr in lProp.GetAttributes do
      if lAttr is InjectAttribute then
      begin
        lInject := InjectAttribute(lAttr);
        Break;
      end;

    if Assigned(lInject) then
    begin
      lServiceType := ServiceTypeFromAttrOrMember(lInject, lProp.PropertyType);
      RegisterRequirement(lServiceType);
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

function TDependencyRegistry.GetRequiredTypes: TArray<PTypeInfo>;
begin
  Result := fRequiredTypes.ToArray;
end;

{ TCustomCompositionRoot }

procedure TCustomCompositionRoot.AfterRegisterDependencies;
begin

end;

procedure TCustomCompositionRoot.BeforeRegisterDependencies;
begin

end;

procedure TCustomCompositionRoot.BuildUp(const AInstance: TObject; aOverwriteExisting: Boolean = false);
begin
  fContainer.BuildUp(AInstance, aOverwriteExisting);
end;

constructor TCustomCompositionRoot.Create(aContainer: TContainer);
begin
  inherited Create;
  if aContainer = nil then
    fContainer := GlobalContainer
  else
    fContainer := aContainer;
end;

procedure TCustomCompositionRoot.RegisterDependencies;
begin

end;

procedure TCustomCompositionRoot.SetUp(aValidate: Boolean = True);
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

procedure TCustomCompositionRoot.ValidateDependencies;
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

      // 2. Try to actually resolve it. This part remains the same and is correct.
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
  lCtx: TRttiContext;
  lType: TRttiType;
  lField: TRttiField;
  lProp: TRttiProperty;
  lAttr: TCustomAttribute;
  lInjectAttr: InjectAttribute;
  lServiceType: PTypeInfo;

  function IsUnset(const aValue: TValue): Boolean;
  begin
    // Treat Nil object/interface or empty value as "unset"
    if aValue.IsEmpty then
      Exit(True);
    if aValue.Kind = tkClass then
      Exit(aValue.AsObject = nil);
    if aValue.Kind = tkInterface then
      Exit(aValue.AsInterface = nil);
    // for other kinds we don't inject here
    Result := False;
  end;


  procedure InjectFieldIfNeeded(const aObj: TObject; const aField: TRttiField; const aServiceType: PTypeInfo);
  var
    lCurrent: TValue;
  begin
    lCurrent := aField.GetValue(aObj);
    if aOverwriteExisting or IsUnset(lCurrent) then
      aField.SetValue(aObj, Self.Resolve(aServiceType));
  end;

  procedure InjectPropIfNeeded(const aObj: TObject; const aProp: TRttiProperty; const aServiceType: PTypeInfo);
  var
    lCurrent: TValue;
  begin
    if not aProp.IsWritable then
      Exit;
    lCurrent := aProp.GetValue(aObj);
    if aOverwriteExisting or IsUnset(lCurrent) then
      aProp.SetValue(aObj, Self.Resolve(aServiceType));
  end;


begin
  if not Assigned(AInstance) then
  begin
    exit;
  end;

  lCtx := TRttiContext.Create;
  try
    lType := lCtx.GetType(AInstance.ClassType);

    // Fields with [Inject]
    for lField in lType.GetFields do
    begin
      lInjectAttr := nil;
      for lAttr in lField.GetAttributes do
      begin
        if lAttr is InjectAttribute then
        begin
          lInjectAttr := InjectAttribute(lAttr);
          break;
        end;
      end;

      if Assigned(lInjectAttr) then
      begin
        lServiceType := lInjectAttr.ServiceType;
        if lServiceType = nil then
        begin
          lServiceType := lField.FieldType.Handle;
        end;

        try
          InjectFieldIfNeeded(AInstance, lField, lServiceType);
        except
          on E: Exception do
            raise EContainerException.CreateFmt(
              'BuildUp failed for %s.%s (%s): %s',
              [AInstance.ClassName, lField.Name, lField.FieldType.Name, E.Message]
            );
        end;
      end;
    end;

    // Writable properties with [Inject]
    for lProp in lType.GetProperties do
    begin
      if not lProp.IsWritable then
      begin
        continue;
      end;

      lInjectAttr := nil;
      for lAttr in lProp.GetAttributes do
      begin
        if lAttr is InjectAttribute then
        begin
          lInjectAttr := InjectAttribute(lAttr);
          break;
        end;
      end;

      if Assigned(lInjectAttr) then
      begin
        lServiceType := lInjectAttr.ServiceType;
        if lServiceType = nil then
        begin
          lServiceType := lProp.PropertyType.Handle;
        end;

        try
          InjectPropIfNeeded(AInstance, lProp, lServiceType);
        except
          on E: Exception do
            raise EContainerException.CreateFmt(
              'BuildUp failed for %s.%s (%s): %s',
              [AInstance.ClassName, lProp.Name, lProp.PropertyType.Name, E.Message]
            );
        end;
      end;
    end;

  finally
    lCtx.Free;
  end;
end;


initialization

  {$IFDEF DEBUG}
  // defered test, will be called by Application.Initialize, just in ase the dependancies are not tested manually
  fPrevInitProc:= InitProc;
  InitProc := @ValidateRegisteredDependencies;
  {$ENDIF}

end.

