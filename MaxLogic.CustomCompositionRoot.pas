unit MaxLogic.CustomCompositionRoot;

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

interface

uses
  system.sysUtils, system.Classes, system.syncObjs, System.Rtti,
  Spring, Spring.Container, Spring.Container.Common, Spring.Services,
  Spring.Container.Injection,

  System.Generics.Collections;


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
    fRequiredTypes: TDictionary<string, PTypeInfo>;
    fLock: TCriticalSection;
    class var fInstance: TDependencyRegistry;
    constructor Create;
    class destructor DestroyClass;
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
    procedure RegisterRequirement(aTypeInfo: PTypeInfo);

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
  public
    // NOTE: if aContainer = nil, then the global container will be used
    constructor Create(aContainer: TContainer = nil);
    // Centralized method for registering all application dependencies.
    procedure RegisterDependencies; virtual;

    // Force a fast-fail if a required dependency cannot be resolved.
    procedure ValidateDependencies; virtual;

    // field injection good for legacy objects, that are not created by the container
    procedure BuildUp(const AInstance: TObject);

    property Container: TContainer read fContainer;
  end;

implementation


{ TDependencyRegistry }

class function TDependencyRegistry.Instance: TDependencyRegistry;
var
  lInstance: TDependencyRegistry;
  lSuccess: Boolean;
begin
  // This is a standard, simple singleton pattern. For multithreaded apps,
  // a more robust double-checked locking pattern might be used, but for
  // initialization code that runs on the main thread, this is sufficient.
  if not Assigned(fInstance) then
  begin
    lInstance := TDependencyRegistry.Create;
    TInterlocked.CompareExchange<TDependencyRegistry>(fInstance, lInstance, nil, lSuccess);
    if not lSuccess then
      lInstance.Free;
  end;
  Result := fInstance;
end;

constructor TDependencyRegistry.Create;
begin
  inherited Create;
  fLock := TCriticalSection.Create;
  // Use a dictionary to automatically handle duplicate registrations.
  fRequiredTypes := TDictionary<string, PTypeInfo>.Create;
end;

destructor TDependencyRegistry.Destroy;
begin
  fRequiredTypes.Free;
  fLock.Free;
  inherited;
end;

class destructor TDependencyRegistry.DestroyClass;
begin
  FreeAndNil(fInstance);
end;

procedure TDependencyRegistry.RegisterRequirement(aTypeInfo: PTypeInfo);
begin
  if not Assigned(aTypeInfo) then
    Exit;

  // Ensure thread-safety, as unit initialization order is not guaranteed
  // and could theoretically happen across threads in some hosts.
  fLock.Acquire;
  try
    // Using the type name as the key prevents duplicate entries.
    fRequiredTypes.AddOrSetValue(aTypeInfo.Name, aTypeInfo);
  finally
    fLock.Release;
  end;
end;

function TDependencyRegistry.GetRequiredTypes: TArray<PTypeInfo>;
begin
  fLock.Acquire;
  try
    Result := fRequiredTypes.Values.ToArray;
  finally
    fLock.Release;
  end;
end;

{ TCustomCompositionRoot }

procedure TCustomCompositionRoot.BuildUp(const AInstance: TObject);
var
  lContext: TRttiContext;
  lType: TRttiType;
  lField: TRttiField;
  lAttribute: TCustomAttribute;
  lServiceType: PTypeInfo;
  lResolvedValue: TValue;
begin
  if not Assigned(AInstance) then
    Exit;

  lContext := TRttiContext.Create;
  try
    lType := lContext.GetType(AInstance.ClassType);

    // Iterate over all fields of the instance's class
    for lField in lType.GetFields do
    begin
      // Check if the field has the [Inject] attribute
      for lAttribute in lField.GetAttributes do
      begin
        if lAttribute is InjectAttribute then
        begin
          // We found a dependency. Let's resolve it.
          lServiceType := lField.FieldType.Handle;
          try
            lResolvedValue := GlobalContainer.Resolve(lServiceType);
            if not lResolvedValue.IsEmpty then
            begin
              // Inject the resolved service into the field.
              lField.SetValue(AInstance, lResolvedValue);
            end;
          except
            on E: Exception do
              raise EContainerException.CreateFmt(
                'BuildUp failed for type "%s". Could not resolve dependency for field "%s" (%s). Original Error: %s',
                [AInstance.ClassName, lField.Name, lField.FieldType.Name, E.Message]
              );
          end;
          // Found the [Inject] attribute, no need to check other attributes on this field
          Break;
        end;
      end;
    end;
  finally
    lContext.Free;
  end;
end;


constructor TCustomCompositionRoot.Create(aContainer: TContainer);
begin
  inherited Create;
  if aContainer = nil then
    fContainer:= GlobalContainer
  else
    fContainer:= aContainer;
end;

procedure TCustomCompositionRoot.RegisterDependencies;
begin
  // This is the point where the DI container is finalized.
  fContainer.Build;
end;

procedure TCustomCompositionRoot.ValidateDependencies;
var
  lRequiredType: PTypeInfo;
  lTypeName: string;
  lServiceLocator: IServiceLocator;
begin
  lServiceLocator := TServiceLocatorAdapter.Create(fContainer);

  for lRequiredType in TDependencyRegistry.Instance.GetRequiredTypes do
  begin
    lTypeName := lRequiredType.Name;

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
      on E: Exception do
        raise EContainerException.CreateFmt(
          'Startup Validation Failed: Could not resolve the required service "%s". Check its own dependencies. Original Error: %s',
          [lTypeName, E.Message]
        );
    end;
  end;
end;

end.
