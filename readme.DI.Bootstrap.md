# MaxLogic.DI.Bootstrap

A small “composition root + dependency manifest” helper built on **Spring4D**.

This unit gives you:

- **A Composition Root base class** (`TDICompositionRoot`) to centralize container setup.
- **A dependency manifest / registry** (`TDependencyRegistry`) that collects “things the app needs” across units.
- **Fail-fast startup validation** so missing registrations blow up at startup, not at 2am when a user clicks a forgotten button.
- **A legacy-friendly `BuildUp()`** helper for injecting into objects not created by the container.

It’s designed for *hybrid/legacy* Delphi applications where not everything is DI-created, but you still want DI-level safety.

---

## What problem this solves

In old codebases you often have:

- classes instantiated manually (`TLegacyForm.Create`, `TMyThing.Create`),
- service locator usage in random places,
- and DI registration scattered / incomplete.

This unit lets you:

1. **Declare required services** from anywhere (even legacy units),
2. **Build the container** in one place,
3. **Validate the required services** at startup,
4. **Optionally inject** into existing objects with `BuildUp()`.

---

## Core concepts

### 1) Validation modes

```delphi
TValidationMode = (vmRegisteredOnly, vmResolveAtStartup, vmIgnore);
```

- **vmRegisteredOnly**  
  Service must be registered in the container (but not necessarily resolvable without runtime params).

- **vmResolveAtStartup**  
  Service must be resolvable during startup validation (`Container.Resolve(...)` succeeds).  
  Use this for “core services” that should always be constructible immediately.

- **vmIgnore**  
  Opt-out: do not register as a requirement and do not inject (for fields/properties).

### 2) `[DIRequire]` attribute

Put this on:

- constructor parameters (for *validation only*),
- fields,
- properties.

```delphi
constructor TSomeService.Create(
  [DIRequire] Logger: ILogger;
  [DIRequire(vmResolveAtStartup)] Repo: IUserRepository;
  const TenantId: Integer // runtime param => not annotated
);
```

**In this unit’s conventions:**
- `[DIRequire]` means: “this dependency matters and should be validated”.
- For **fields/properties**, `[DIRequire]` also acts as an **injection marker**, same as `[Inject]`.

### 3) `[Inject]` attribute (Spring4D)

Spring4D’s attribute for field/property injection.  
If `[Inject(ServiceType)]` is used, the service type can be overridden.  
If `[Inject]` is used with no service type, the member type is used.

### 4) “Dependency manifest” pattern

Instead of hoping you remembered to register everything, you can build a manifest by calling:

- `TDependencyRegistry.Instance.RegisterRequirement(TypeInfo(IMySvc));`
- or scanning a class: `TDependencyRegistry.Instance.RegisterRequirementsFrom(TMyClass);`

Then `TDICompositionRoot.ValidateDependencies` checks that the container satisfies the manifest.

---

## How to use

### Step 1 — Define your App Root

Create your application-specific root class:

```delphi
type
  TAppRoot = class(TDICompositionRoot)
  protected
    procedure RegisterDependencies; override;
  end;

procedure TAppRoot.RegisterDependencies;
begin
  inherited;
  // add modules here or direct container regs
  // Container.RegisterType<TFoo>.Implements<IFoo>;
end;
```

### Step 2 — Register dependencies from “consumer” units

In any unit that **needs** a service (especially legacy code), declare the requirement in `initialization`:

```delphi
initialization
  TDependencyRegistry.Instance.RegisterRequirement(TypeInfo(ILogger));
  // or:
  TDependencyRegistry.Instance.RegisterRequirementsFrom(TMyLegacyClass);
```

This keeps consumers decoupled from implementations and moves “missing DI wire” failures to startup.

### Step 3 — Startup (dpr / initialization)

In your `.dpr` (or app startup point):

```delphi
var
  Root: TAppRoot;
begin
  Root := TAppRoot.Create;
  try
    Root.SetUp(True); // True = validate
    Application.Initialize;
    Application.Run;
  finally
    Root.Free;
  end;
end;
```

`SetUp(True)` does:

1. `BeforeRegisterDependencies`
2. `RegisterDependencies`
3. `Container.Build`
4. `ValidateDependencies` (optional)
5. `AfterRegisterDependencies`

---

## Modules

You can register dependencies in modules:

- Procedure-based modules:

```delphi
Root.AddModule(
  procedure(C: TContainer)
  begin
    C.RegisterType<TFoo>.Implements<IFoo>;
  end
);
```

- Class-based modules:

```delphi
type
  TFooModule = class(TDIRegistrationModule)
  public
    procedure RegisterDependencies(aContainer: TContainer); override;
  end;

procedure TFooModule.RegisterDependencies(aContainer: TContainer);
begin
  aContainer.RegisterType<TFoo>.Implements<IFoo>;
end;

Root.AddModule(TFooModule.Create);
```

The root **owns** class-based modules and frees them in its destructor.

---

## Scanning rules (what gets picked up)

### Registry scanning (`RegisterRequirementsFrom...`)

A field/property is considered a requirement if:

- it has `[DIRequire]` (unless mode is `vmIgnore`), **or**
- it has `[Inject]` (unless `[DIRequire(vmIgnore)]` is also present).

Constructor parameters are requirements **only if they have `[DIRequire]`**.

### BuildUp injection rules (`TContainerHelper.BuildUp`)

A field/property is injected if:

- it has `[DIRequire]` (unless `vmIgnore`), **or**
- it has `[Inject]` (unless `[DIRequire(vmIgnore)]` is also present).

Service type selection:

- If `[Inject(ServiceType)]` provides a service type, it is used.
- Otherwise, the member’s RTTI type is used.

Only types with RTTI kind `tkInterface` or `tkClass` are injected.

Overwrite behavior:

- `aOverwriteExisting=False` (default): inject only if the current value is “unset”.
- `aOverwriteExisting=True`: inject even if something is already assigned.

---

## Using BuildUp (legacy objects)

If you create an object manually but still want Spring4D field/property injection:

```delphi
var
  Obj: TMyLegacyThing;
begin
  Obj := TMyLegacyThing.Create;
  GlobalContainer.BuildUp(Obj); // or Root.Container.BuildUp(...)
end;
```

This is intended for:

- forms / frames created normally,
- legacy classes created with `Create`,
- objects that *can’t* be created by the container yet.

### Important: property getter side effects

`BuildUp()` checks whether a property is “unset” by calling `Prop.GetValue(...)`.

That means the **property getter will run**.

Best practice:
- **Keep injected property getters side-effect-free** (no lazy init that depends on injected values, no exceptions, no DB calls).

If a getter is “unsafe during startup”, prefer **field injection** for that dependency.

---

## Best practices (how to not shoot yourself)

### 1) Decide what must be resolvable at startup
Use `vmResolveAtStartup` for:
- core app services,
- logging,
- configuration,
- DB connection factories,
- anything that should always be constructible immediately.

Use `vmRegisteredOnly` for:
- services that require runtime parameters,
- services that depend on environment state not ready at startup.

### 2) Be intentional about object lifetimes (tkClass)
Because `BuildUp()` can inject class instances (`tkClass`), you must be clear about ownership:

- If the container owns the instance (singleton/transient controlled by Spring4D),
  **do not `Free` it manually** unless you explicitly created/owned it.
- Prefer injecting **interfaces** where possible.

### 3) Avoid “surprise wiring”
This unit allows `[DIRequire]` to imply injection for fields/properties.
That’s convenient, but it also means someone adding `[DIRequire]` can change runtime behavior.

Recommended team rule:
- Add `[DIRequire]` only when you genuinely want both validation **and** injection for members.

Use `[DIRequire(vmIgnore)]` to explicitly block injection/validation for a specific member.

### 4) RTTI must be enabled for what you scan/inject
If Spring4D “doesn’t call your constructors” or RTTI scanning is incomplete, ensure RTTI is enabled, e.g.:

```delphi
{$RTTI EXPLICIT METHODS([vcPublic, vcPublished]) PROPERTIES([vcPublic, vcPublished])}
```

---

## Debug-only safety net

In `DEBUG` builds the unit hooks `InitProc` so if requirements were registered but never validated, it raises an error early.

This catches “we forgot to call `SetUp(True)` / ValidateDependencies” in debug testing.

---

## FAQ

### “Do I need both `[Inject]` and `[DIRequire]`?”
- For **constructor params**: use `[DIRequire]` if you want them validated.
- For **fields/properties**: either works.  
  `[DIRequire]` implies injection in this design.  
  `[Inject]` is classic Spring4D injection.

### “Can I scan a class instead of registering interfaces manually?”
Yes:

```delphi
TDependencyRegistry.Instance.RegisterRequirementsFrom(TMyType);
```

This picks up `[Inject]`/`[DIRequire]` on fields/properties and `[DIRequire]` on constructor params.

---

## Quick example

```delphi
type
  TFoo = class
  private
    [DIRequire(vmResolveAtStartup)]
    FLog: ILogger;

    [Inject]
    property Repo: IUserRepository read FRepo write FRepo;
  end;

initialization
  TDependencyRegistry.Instance.RegisterRequirementsFrom(TFoo);
```

Startup:

```delphi
Root := TAppRoot.Create;
Root.SetUp(True);
```

Legacy object:

```delphi
Obj := TFoo.Create;
GlobalContainer.BuildUp(Obj);
```

---

## License / ownership

Internal MaxLogic helper unit. Use freely within the codebase.
