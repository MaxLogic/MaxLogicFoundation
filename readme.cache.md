# MaxLogic.Cache — Multi-namespace, thread-safe in-process cache (Delphi)

## Overview

This framework provides a reusable, in-process cache for Delphi apps/services where:

- We want **fast reads** (in-memory snapshots) with controlled refresh.
- We want **single-flight loading** per `(Namespace, Key)` to avoid stampedes.
- We want **multiple namespaces** to reduce contention between unrelated key groups.
- We want **time-based expiration** (TTL and/or idle sliding window) plus optional **size caps**.
- We want **group invalidation** via tags and **lazy dependency validation** (pull model) for “file-backed” or “external state” entries.

### High-level architecture (components + responsibilities)

- **`IMaxCache` / `TMaxCache`**: the cache core (get-or-load, invalidation, metrics, shutdown). (`maxlogic.cache.pas :: IMaxCache`, `maxlogic.cache.pas :: TMaxCache`)
- **Namespace buckets**: one dictionary per namespace, each protected by a `TLightweightMREW` for efficient read-heavy access. (Implementation detail: `maxlogic.cache.pas :: TMaxCache.TMaxNamespaceBucket`)
- **Entries**: per-key state machine with entry-level monitor object (`EntrySync`) used to coordinate waiters during loads. (Implementation detail: `maxlogic.cache.pas :: TMaxCache.TMaxCacheEntry`)
- **Tag index**: a global map from “scoped tag” → set of `(Namespace, Key)` references used for `InvalidateByTag`. (`maxlogic.cache.pas :: TMaxCache.RegisterTags`, `maxlogic.cache.pas :: TMaxCache.GetTagRefsCopy`)
- **Dependencies**: optional objects that can declare an entry stale on access; dependency validation is rate-limited and stampede-protected. (`maxlogic.cache.pas :: IMaxCacheDependency`, `maxlogic.cache.pas :: TMaxCache.ValidateDependency`)
- **Sweeper**: optional background timer that evicts obsolete/expired entries and enforces size caps in bounded batches. (`maxlogic.cache.pas :: TMaxCache.SweepTick`)

## Key Concepts

- **Namespace**: a string that partitions the key-space. Namespaces map to independent buckets (dictionaries). (`maxlogic.cache.pas :: TMaxCache.GetOrCreateBucket`)
- **Key**: a string within a namespace, used as the dictionary key.
- **Entry**: a stateful record/object holding the cached value, timestamps, and metadata (dependency, tags). (`maxlogic.cache.pas :: TMaxCache.TMaxCacheEntry`)
- **Snapshot value**: the cached `IInterface` instance returned to callers. We should treat it as immutable after publishing. (Inferred from design; the cache never clones or deep-copies values; `maxlogic.cache.pas :: TMaxCache.GetOrCreate`)
- **Loader**: a `TFunc<IInterface>` invoked when the cache needs a new value. Loader code runs outside bucket locks. (`maxlogic.cache.pas :: TMaxCache.GetOrCreate`)
- **TTL (time-to-live)**: absolute expiration; if `TtlMs > 0`, entry expires at `CreatedAtMs + TtlMs`. (`maxlogic.cache.pas :: TMaxCache.IsExpired`, `maxlogic.cache.pas :: TMaxCache.GetOrCreate`)
- **Idle expiration**: sliding window; if `IdleMs > 0`, each successful `GetOrCreate` hit extends `IdleExpiresAtMs`. (`maxlogic.cache.pas :: TMaxCache.TouchEntry`)
- **Dependency**: an object that can detect staleness (pull model). On access, the cache may ask the dependency if the entry is stale. (`maxlogic.cache.pas :: IMaxCacheDependency`, `maxlogic.cache.pas :: TMaxCache.ValidateDependency`)
- **Invalidation**: marking an entry obsolete and (sometimes) removing it from the bucket dictionary so the next access reloads. (`maxlogic.cache.pas :: TMaxCache.InvalidateInternal`)
- **Tag**: a label attached to an entry at publish time, used for “invalidate everything in a group”. (`maxlogic.cache.pas :: TMaxCache.RegisterTags`)
- **Scoped tag**: internal normalization used by the cache to avoid cross-namespace tag collisions: `Namespace + '|' + Tag` unless the tag starts with `global:`. Tags containing `|` are rejected to prevent cross-namespace injection. (`maxlogic.cache.pas :: TMaxCache.ScopedTag`)
- **Single-flight loading**: at most one thread runs the loader for a given `(Namespace, Key)`; other threads wait on that entry’s `EntrySync`. (`maxlogic.cache.pas :: TMaxCache.TryBeginLoad`, `maxlogic.cache.pas :: TMaxCache.WaitForEntry`)

## Public API Reference

This section lists the public types in `MaxLogic.Cache` and `MaxLogic.Cache.RepositoryBase` with their role and behavioral notes.

### Exceptions

- `EMaxCacheException`: base exception for cache errors. (`maxlogic.cache.pas :: EMaxCacheException`)
- `EMaxCacheLoadException`: raised when a loader fails or when a cached failure is rethrown. (`maxlogic.cache.pas :: EMaxCacheLoadException`, `maxlogic.cache.pas :: TMaxCache.GetOrCreate`)
- `EMaxCacheShutdown`: raised when the cache is shutting down and cache operations fail fast. (`maxlogic.cache.pas :: EMaxCacheShutdown`, `maxlogic.cache.pas :: TMaxCache.EnsureNotShuttingDown`)
- `EMaxCacheWaitTimeout`: raised when a waiter times out waiting for a `Loading` entry to finish. (`maxlogic.cache.pas :: EMaxCacheWaitTimeout`, `maxlogic.cache.pas :: TMaxCache.WaitForEntry`)

### `TMaxDependencyStamp`

A small record used to represent a dependency “stamp” for change detection. (`maxlogic.cache.pas :: TMaxDependencyStamp`)

- Fields:
  - `MTimeUtcMs: Int64`
  - `SizeBytes: Int64`
  - `Hash32: Cardinal`
  - `HasHash: Boolean`
- `class function FromFile(const aFileName: string; const aIncludeHash: Boolean = False): TMaxDependencyStamp;`
  - Reads file metadata and (optionally) hashes file contents with `xxHash32`.
  - When file does not exist or `aFileName=''`, returns a stamp with `SizeBytes=-1` and `MTimeUtcMs=0`. (`maxlogic.cache.pas :: TMaxDependencyStamp.FromFile`)
- `function Equals(const aOther: TMaxDependencyStamp): Boolean;`
  - Compares `MTimeUtcMs` and `SizeBytes`.
  - Compares `Hash32` only if **both** stamps have `HasHash=True`. (`maxlogic.cache.pas :: TMaxDependencyStamp.Equals`)

### `IMaxCacheDependency`

Dependency interface for pull-based staleness checks. (`maxlogic.cache.pas :: IMaxCacheDependency`)

- `function IsStale(out aNewStamp: TMaxDependencyStamp): Boolean;`
  - Returns `True` when the dependency indicates the entry is stale.
  - Important: in the core cache, an exception thrown from `IsStale` is treated as “stale”. (`maxlogic.cache.pas :: TMaxCache.ValidateDependency`)
- `function GetStamp: TMaxDependencyStamp;`
  - Called by the cache at publish time to store the “current stamp” for the entry. (`maxlogic.cache.pas :: TMaxCache.GetOrCreate`)

### `TMaxFileDependency`

Built-in dependency implementation for file-backed entries. (`maxlogic.cache.pas :: TMaxFileDependency`)

- Constructor:
  - `constructor Create(const aFileName: string; const aAlwaysHashBelowBytes: Int64 = 51200; const aForceHash: Boolean = False);`
    - `aAlwaysHashBelowBytes` default `51200` bytes (50KB). (`maxlogic.cache.pas :: TMaxFileDependency.Create`)
- Implements `IMaxCacheDependency`:
  - `function GetStamp: TMaxDependencyStamp;`
    - Recomputes stamp and updates internal baseline (`fStamp`).
    - Includes a hash when `aForceHash=True` or when `FileSizeBytes <= aAlwaysHashBelowBytes`. (`maxlogic.cache.pas :: TMaxFileDependency.GetStamp`)
  - `function IsStale(out aNewStamp: TMaxDependencyStamp): Boolean;`
    - Recomputes a new stamp and compares it to internal baseline (`fStamp`).
    - Uses hashing when forced, when file is small, or when metadata `(mtime,size)` differs from baseline. (`maxlogic.cache.pas :: TMaxFileDependency.IsStale`, `maxlogic.cache.pas :: TMaxFileDependency.ShouldHash`)
- Properties:
  - `property FileName: string read fFileName;`
  - `property ForceHash: Boolean read fForceHash;`

### `TMaxCacheKeySize`

Used by metrics to report large keys. (`maxlogic.cache.pas :: TMaxCacheKeySize`)

- Fields:
  - `Namespace: string`
  - `Key: string`
  - `EstimatedBytes: Int64`

### `TMaxCacheMetrics`

Aggregate metrics across all namespaces. (`maxlogic.cache.pas :: TMaxCacheMetrics`, `maxlogic.cache.pas :: TMaxCache.GetMetrics`)

- Fields:
  - `NamespaceCount`, `EntryCount`, `EstimatedMemoryKB`
  - Counters: `Hits`, `Misses`, `Loads`, `LoadFailures`
  - Invalidation counters: `InvalidationsDirect`, `InvalidationsByTag`
  - Eviction counters: `EvictionsTtl`, `EvictionsIdle`, `EvictionsSize`
  - Latency: `AvgLoadTimeMs`, `P95LoadTimeMs` (histogram-based estimate)
  - `TopTenKeysBySize: TArray<TMaxCacheKeySize>`
- Behavioral notes:
  - These counters are updated with `TInterlocked` and are intended for monitoring/tuning rather than exact accounting. (Inferred from implementation; `maxlogic.cache.pas :: TMaxCache.GetMetrics`, `maxlogic.cache.pas :: TMaxCache.RecordLoadTimeMs`)

### `TMaxNamespaceCacheMetrics`

Namespace-only metrics. (`maxlogic.cache.pas :: TMaxNamespaceCacheMetrics`, `maxlogic.cache.pas :: TMaxCache.GetNamespaceMetrics`)

- Fields:
  - `Namespace`, `EntryCount`, `EstimatedMemoryKB`

### `TMaxCacheConfig`

Global cache configuration record. (`maxlogic.cache.pas :: TMaxCacheConfig`)

- Fields:
  - Default policy values (used when no per-entry options exist yet):
    - `DefaultTtlMs`, `DefaultIdleMs`, `DefaultValidateIntervalMs`
    - `ReturnStaleOnFailure` (default policy for loads; see below)
  - Size caps:
    - `MaxEntriesGlobal`, `MaxBytesGlobal`
    - `MaxEntriesPerNamespace`, `MaxBytesPerNamespace`
  - Failure/timeout behavior:
    - `MinFailCacheMs` (floor for failure caching)
    - `DefaultWaitTimeoutMs` (waiters timeout)
  - File dependency default helper:
    - `AlwaysHashBelowBytes`
  - Sweeper:
    - `SweepIntervalMs`, `SweepBatchSize`
  - Key behavior:
    - `CaseSensitiveKeys`
- `class function Default: TMaxCacheConfig;`
  - Default values (as of current implementation):
    - `DefaultTtlMs=0`, `DefaultIdleMs=0`, `DefaultValidateIntervalMs=0`
    - `MaxEntriesGlobal=0`, `MaxBytesGlobal=0`, `MaxEntriesPerNamespace=0`, `MaxBytesPerNamespace=0`
    - `MinFailCacheMs=500`
    - `DefaultWaitTimeoutMs=30000`
    - `AlwaysHashBelowBytes=51200`
    - `SweepIntervalMs=10000`, `SweepBatchSize=200`
    - `CaseSensitiveKeys=True`
    - `ReturnStaleOnFailure=False` (`maxlogic.cache.pas :: TMaxCacheConfig.Default`)

### `TMaxCacheOptions`

Per-call (per-entry publish) options object. The cache does not own it; we create/free it in our calling code. (`maxlogic.cache.pas :: TMaxCacheOptions`)

- Fields (all are public fields, not properties):
  - `TtlMs`, `IdleMs`, `ValidateIntervalMs`, `FailCacheMs`
  - `SizeEstimateBytes`
  - `Tags: TArray<string>`
  - `Dependency: IMaxCacheDependency`
  - `ReturnStaleOnFailure: Boolean`
- Constructor:
  - `constructor Create;` initializes all fields to “disabled” defaults (`0`, `[]`, `nil`, `False`). (`maxlogic.cache.pas :: TMaxCacheOptions.Create`)
- Behavioral notes:
  - If `aOptions=nil`, the cache chooses options as follows:
    - If the entry already has a value, it reuses the entry’s previously published settings.
    - Otherwise it uses `TMaxCacheConfig` defaults. (`maxlogic.cache.pas :: TMaxCache.GetOrCreate`)

### `IMaxCache`

Public cache interface used by application code. (`maxlogic.cache.pas :: IMaxCache`)

- `function GetOrCreate(const aNamespace, aKey: string; const aLoader: TFunc<IInterface>; const aOptions: TMaxCacheOptions = nil): IInterface;`
  - Returns the cached value as `IInterface`; we typically cast it to a more specific snapshot interface at the call site.
  - Side effects:
    - May run loader (single-flight) and publish new value.
    - Updates hit/miss/load metrics.
    - Updates idle expiration on successful ready hits. (`maxlogic.cache.pas :: TMaxCache.GetOrCreate`, `maxlogic.cache.pas :: TMaxCache.TouchEntry`)
- `function TryGet(const aNamespace, aKey: string; out aValue: IInterface): Boolean;`
  - “Read-only probe”: returns `True` only if the entry exists and is `Ready` and **not expired**.
  - Does **not** validate dependencies and does **not** update `LastAccessMs` / idle expiration. (`maxlogic.cache.pas :: TMaxCache.TryGet`)
- `procedure Invalidate(const aNamespace, aKey: string);`
  - Marks the entry `Obsolete`, sets `WasInvalidated := 1`, clears `Value := nil`, wakes waiters, then removes the entry if it is still the same instance and unregisters tags. (`maxlogic.cache.pas :: TMaxCache.InvalidateInternal`)
- `procedure InvalidateMany(const aNamespace: string; const aKeys: array of string);`
  - Convenience to invalidate multiple keys. (`maxlogic.cache.pas :: TMaxCache.InvalidateMany`)
- `procedure InvalidateNamespace(const aNamespace: string);`
  - Snapshots entries, marks each `Obsolete`, clears values to `nil`, wakes waiters, removes entries if they are still the same instance, and unregisters tags. (`maxlogic.cache.pas :: TMaxCache.InvalidateNamespaceInternal`)
- `procedure InvalidateByTag(const aScopedTag: string);`
  - Invalidates all keys registered under a tag.
  - Requires `aScopedTag` to be either:
    - a `global:` tag, or
    - a scoped tag containing `|` (`Namespace|Tag`).
  - Throws `EMaxCacheException` if the tag is not scoped/global. (`maxlogic.cache.pas :: TMaxCache.InvalidateByTag`)
- `function GetMetrics: TMaxCacheMetrics;`
- `function GetNamespaceMetrics(const aNamespace: string): TMaxNamespaceCacheMetrics;`
- `procedure Shutdown;`
  - Sets the cache into “shutting down” mode, marks entries invalidated/obsolete, and wakes waiters.
  - After shutdown, `GetOrCreate` and `TryGet` fail fast by raising `EMaxCacheShutdown`. (`maxlogic.cache.pas :: TMaxCache.Shutdown`, `maxlogic.cache.pas :: TMaxCache.EnsureNotShuttingDown`)

### `TMaxCache`

Concrete implementation of `IMaxCache`. (`maxlogic.cache.pas :: TMaxCache`)

- Factory methods:
  - `class function New(const aConfig: TMaxCacheConfig): IMaxCache; overload; static;`
  - `class function New: IMaxCache; overload; static;` (uses `TMaxCacheConfig.Default`)
- Tag helper:
- `class function ScopedTag(const aNamespace, aTag: string): string; static;`
  - If `aTag` starts with `global:` (case-insensitive) → returns it unchanged.
  - If `aTag` contains `|` → raises `EMaxCacheException` (invalid untrusted scope).
  - Else returns `aNamespace + '|' + aTag`. (`maxlogic.cache.pas :: TMaxCache.ScopedTag`)

### `TMaxCacheRepositoryBase`

Repository helper intended to standardize namespace naming and carry a cache reference. (`MaxLogic.Cache.RepositoryBase.pas :: TMaxCacheRepositoryBase`)

- Constructor:
  - `constructor Create(const aCache: IMaxCache);`
    - Requires non-`nil` cache; raises `EArgumentNilException` otherwise. (`MaxLogic.Cache.RepositoryBase.pas :: TMaxCacheRepositoryBase.Create`)
- Protected helpers:
  - `class function TenantNamespace(const aDbName: string): string; static;` → `'tenant:' + aDbName`
  - `class function FilesNamespace: string; static;` → `'files'`
  - `class function SiteNamespace(const aSiteId: string): string; static;` → `'site:' + aSiteId`
  - `property Cache: IMaxCache read fCache;`

## Repository Model

`TMaxCacheRepositoryBase` is intentionally minimal: it gives us a shared `Cache` reference and consistent namespace conventions, while leaving all key naming and load logic to derived repositories. (`MaxLogic.Cache.RepositoryBase.pas :: TMaxCacheRepositoryBase`)

### Intended usage

- We create repositories as *thin adapters* around `IMaxCache`:
  - one method per cached “thing” (taxes snapshot, countries snapshot, template, etc.),
  - each method selects:
    - namespace (`TenantNamespace`, `FilesNamespace`, `SiteNamespace`, or custom),
    - key (string),
    - loader (returns `IInterface` snapshot),
    - options (TTL/idle/dependency/tags/failure policy).

### Recommended patterns

- Use **interface-based snapshots** as the cached value type (e.g., `IMySnapshot`) to keep object lifetimes safe across concurrent readers and refresh. (Inferred; cache stores `IInterface` without copying; `maxlogic.cache.pas :: TMaxCache.GetOrCreate`)
- Use **stable key naming** and keep keys *purely functional* (derived only from inputs), because invalidation and tags key off those strings.
- Put “group” tags in a predictable naming scheme (e.g., `'group:config'`, `'group:templates'`) so invalidation can be broad and cheap.

### Common gotchas

- **Re-entrant loads for the same key**: if our loader calls `GetOrCreate` for the same `(Namespace, Key)`, we will wait on our own `Loading` entry and likely time out with `EMaxCacheWaitTimeout`. (`maxlogic.cache.pas :: TMaxCache.GetOrCreate`, `maxlogic.cache.pas :: TMaxCache.WaitForEntry`)
- **`TryGet` does not “touch” idle expiry**: if we use `TryGet` as our hot-path, idle expiration will still advance and the entry can expire even under heavy `TryGet` usage. (`maxlogic.cache.pas :: TMaxCache.TryGet`, `maxlogic.cache.pas :: TMaxCache.TouchEntry`)
- **`InvalidateByTag` requires a scoped/global tag**: passing `'group:config'` directly raises `EMaxCacheException`; we must pass `'tenant:ACME|group:config'` or use `TMaxCache.ScopedTag(...)`. (`maxlogic.cache.pas :: TMaxCache.InvalidateByTag`, `maxlogic.cache.pas :: TMaxCache.ScopedTag`)
- **Dependency exceptions are treated as stale**: if our dependency’s `IsStale` raises, the cache treats it as stale and will force reload on next access. (`maxlogic.cache.pas :: TMaxCache.ValidateDependency`)

## Typical Usage

All examples below use the real public signatures from `IMaxCache` and related types.

### Minimal example

```delphi
uses
  System.SysUtils,
  MaxLogic.Cache;

type
  IMySnapshot = interface(IInterface)
    ['{F3B589B0-3C0B-4E49-9D84-4CF8AA6B2B1E}']
  end;

  TMySnapshot = class(TInterfacedObject, IMySnapshot)
  end;

function LoadMySnapshot: IMySnapshot;
begin
  // Our loader returns an immutable snapshot interface.
  Result := TMySnapshot.Create;
end;

procedure Example;
var
  Cache: IMaxCache;
  Options: TMaxCacheOptions;
  Value: IMySnapshot;
begin
  Cache := TMaxCache.New; // uses TMaxCacheConfig.Default

  Options := TMaxCacheOptions.Create;
  try
    Options.ValidateIntervalMs := 5000;
    Options.Tags := ['group:config'];

    Value := Cache.GetOrCreate('tenant:ACME', 'Taxes',
      function: IInterface
      begin
        Result := LoadMySnapshot as IInterface;
      end,
      Options) as IMySnapshot;
  finally
    Options.Free;
  end;
end;
```

### Example with custom repository

```delphi
uses
  System.SysUtils,
  MaxLogic.Cache,
  MaxLogic.Cache.RepositoryBase;

type
  ITaxesSnapshot = interface(IInterface)
    ['{8C72E7F4-7A22-4D5A-9F7E-46E99BDE5D77}']
  end;

  TTaxesSnapshot = class(TInterfacedObject, ITaxesSnapshot)
  end;

  TTaxesRepository = class(TMaxCacheRepositoryBase)
  public
    function GetTaxes(const aDbName: string): ITaxesSnapshot;
  end;

function TTaxesRepository.GetTaxes(const aDbName: string): ITaxesSnapshot;
var
  Options: TMaxCacheOptions;
  Ns: string;
begin
  Ns := TenantNamespace(aDbName);

  Options := TMaxCacheOptions.Create;
  try
    Options.Tags := ['group:config'];
    Options.ValidateIntervalMs := 5000;
    Options.ReturnStaleOnFailure := True;

    Result := Cache.GetOrCreate(Ns, 'Taxes',
      function: IInterface
      begin
        Result := TTaxesSnapshot.Create; // our real loader goes here
      end,
      Options) as ITaxesSnapshot;
  finally
    Options.Free;
  end;
end;
```

### Example: invalidation / refresh

Direct invalidation:

```delphi
Cache.Invalidate('tenant:ACME', 'Taxes');
```

Invalidate by tag (namespace-scoped tag):

```delphi
Cache.InvalidateByTag(TMaxCache.ScopedTag('tenant:ACME', 'group:config'));
```

Using a global tag (not namespace-scoped by the cache):

```delphi
Cache.InvalidateByTag('global:templates');
```

## Expiration, Invalidation, and Consistency

### Expiration (TTL + idle)

- TTL:
  - Enabled when `TtlMs > 0`.
  - Stored as `ExpiresAtMs := NowMs + TtlMs` at publish time. (`maxlogic.cache.pas :: TMaxCache.GetOrCreate`)
- Idle expiration:
  - Enabled when `IdleMs > 0`.
  - Extended on successful `GetOrCreate` hit via `TouchEntry`, which sets `IdleExpiresAtMs := NowMs + IdleMs`. (`maxlogic.cache.pas :: TMaxCache.TouchEntry`)
- Expiration checks:
  - `GetOrCreate` checks both TTL and idle expiry (and then dependency) before returning a ready value. (`maxlogic.cache.pas :: TMaxCache.GetOrCreate`, `maxlogic.cache.pas :: TMaxCache.IsExpired`)
  - `TryGet` checks TTL/idle expiry but does not validate dependencies. (`maxlogic.cache.pas :: TMaxCache.TryGet`)

### Invalidation mechanics

- `Invalidate(Namespace, Key)`:
  - Marks the entry `Obsolete` under the entry monitor, sets `WasInvalidated := 1`, clears `Value := nil`, and pulses waiters.
  - Then removes the entry from the bucket **only if it is still the same instance** and unregisters its tags. (`maxlogic.cache.pas :: TMaxCache.InvalidateInternal`)
- `InvalidateNamespace(Namespace)`:
  - Snapshots the current entries, then for each entry:
    - marks `Obsolete`, clears value, pulses waiters,
    - removes the entry **only if it is still the same instance**,
    - unregisters tags. (`maxlogic.cache.pas :: TMaxCache.InvalidateNamespaceInternal`)
  - Direct invalidation metric increments once per namespace call. (`maxlogic.cache.pas :: TMaxCache.InvalidateNamespaceInternal`)
- `InvalidateByTag(ScopedTag)`:
  - Copies tag references under tag-index read lock and then invalidates keys one-by-one (no tag lock held during invalidation). (`maxlogic.cache.pas :: TMaxCache.GetTagRefsCopy`, `maxlogic.cache.pas :: TMaxCache.InvalidateByTag`)

### Consistency guarantees (and non-guarantees)

- Single-flight per key:
  - At most one loader runs for a given `(Namespace, Key)` at a time. (`maxlogic.cache.pas :: TMaxCache.TryBeginLoad`)
- “Freshness”:
  - A `Ready` value is returned if it is not expired and its dependency is considered valid (subject to validation interval / stampede protection). (`maxlogic.cache.pas :: TMaxCache.GetOrCreate`, `maxlogic.cache.pas :: TMaxCache.ValidateDependency`)
- Non-guarantees:
  - This is an in-process cache; there is no cross-process invalidation or coherence mechanism in the core API. (Inferred from public surface; `maxlogic.cache.pas :: IMaxCache`)
  - Dependency validation is **optimistic** under stampede: if another thread is validating, we return the cached value without blocking. (`maxlogic.cache.pas :: TMaxCache.ValidateDependency`)

## Concurrency and Thread Safety

### What is safe concurrently

- Concurrent calls to:
  - `GetOrCreate`, `TryGet`, `Invalidate*`, `GetMetrics`, `GetNamespaceMetrics` are designed for multi-threaded usage. (Inferred from locking/atomics; `maxlogic.cache.pas :: TMaxCache`)
- Concurrent access to returned snapshots:
  - The cache stores/returns an `IInterface`; as long as our snapshots are immutable (or internally thread-safe), concurrent callers can safely hold them while the cache reloads/replaces the entry. (Inferred; `maxlogic.cache.pas :: TMaxCache.GetOrCreate`)

### Locking strategy (how it works)

- Namespace registry and tag index use `TLightweightMREW` read/write locking. (`maxlogic.cache.pas :: TMaxCache.TryGetBucket`, `maxlogic.cache.pas :: TMaxCache.RegisterTags`)
- Single-flight coordination uses `TMonitor` on a per-entry `EntrySync` object. (`maxlogic.cache.pas :: TMaxCache.WaitForEntry`)
- Dependency validation stampede protection uses `TInterlocked.CompareExchange` on `IsValidating`. (`maxlogic.cache.pas :: TMaxCache.ValidateDependency`)
- The loader itself is executed outside bucket locks. (`maxlogic.cache.pas :: TMaxCache.GetOrCreate`)
- On Win32, we use atomic reads/writes for entry `Int64` timestamps and sizes to avoid torn reads. (`maxlogic.cache.pas :: TMaxCache.ReadInt64`, `maxlogic.cache.pas :: TMaxCache.WriteInt64`)

### Lifecycle assumptions

- `TMaxCache.New` returns an `IMaxCache` interface. When our last reference is released, the object is destroyed; destructor calls `Shutdown` and stops the sweeper if running. (`maxlogic.cache.pas :: TMaxCache.Destroy`)
- `Shutdown` is idempotent; subsequent calls do nothing. (`maxlogic.cache.pas :: TMaxCache.Shutdown`)

## Errors and Diagnostics

### Error types and how they surface

- Loader not assigned:
  - `GetOrCreate` raises `EMaxCacheException` when `aLoader` is not assigned. (`maxlogic.cache.pas :: TMaxCache.GetOrCreate`)
- Loader failure:
  - If loader raises, cache stores failure state and raises `EMaxCacheLoadException` with `E.ClassName + ': ' + E.Message`. (`maxlogic.cache.pas :: TMaxCache.GetOrCreate`)
  - Failures are negative-cached until `RetryAtMs`, computed as `NowMs + max(FailCacheMs, MinFailCacheMs)`. (`maxlogic.cache.pas :: TMaxCache.GetOrCreate`)
- Wait timeout:
  - Waiters may raise `EMaxCacheWaitTimeout` if the entry remains `Loading` after `DefaultWaitTimeoutMs` (or 30s fallback if configured <= 0). (`maxlogic.cache.pas :: TMaxCache.WaitForEntry`)
  - On timeout, we mark the entry `Failed`, set `RetryAtMs := NowMs + max(FailCacheMs, MinFailCacheMs)`, pulse waiters, and raise. (`maxlogic.cache.pas :: TMaxCache.WaitForEntry`)
- Shutdown:
  - `GetOrCreate` / `TryGet` raise `EMaxCacheShutdown` after shutdown. (`maxlogic.cache.pas :: TMaxCache.EnsureNotShuttingDown`)
- InvalidateByTag misuse:
  - `InvalidateByTag` raises `EMaxCacheException` if tag is not scoped (`Namespace|Tag`) or `global:`. (`maxlogic.cache.pas :: TMaxCache.InvalidateByTag`)
- ScopedTag misuse:
  - `ScopedTag` raises `EMaxCacheException` if `aTag` contains `|` and is not `global:`. (`maxlogic.cache.pas :: TMaxCache.ScopedTag`)

### Diagnostics and observability

- Metrics:
  - `GetMetrics` provides aggregate counters + load time stats + largest keys by estimated size. (`maxlogic.cache.pas :: TMaxCache.GetMetrics`)
  - `GetNamespaceMetrics` provides per-namespace entry count and estimated memory. (`maxlogic.cache.pas :: TMaxCache.GetNamespaceMetrics`)
- There are no logging hooks in the public API. If we need structured logging for loader failures, we do it in our loader/repository code (catch/rethrow) or extend the cache core. (Inferred from public surface; `maxlogic.cache.pas :: IMaxCache`)

## Extending the Framework

### Adding a new repository type

- Derive from `TMaxCacheRepositoryBase`.
- Use `Cache.GetOrCreate(...)` and cast to our snapshot interface at the call site.
- Standardize:
  - namespaces (`TenantNamespace`, `FilesNamespace`, `SiteNamespace` or additional helpers),
  - key naming,
  - tag conventions for group invalidation.

### Adding a new dependency type (custom staleness logic)

- Implement `IMaxCacheDependency`.
- `IsStale` should be:
  - fast on average (it can run on request paths),
  - thread-safe (many threads can call into it across different entries),
  - exception-safe: if it raises, the cache treats it as stale and will reload. (`maxlogic.cache.pas :: TMaxCache.ValidateDependency`)
- `GetStamp` should return the baseline stamp for comparison and can update internal baseline state if the implementation needs it. (`maxlogic.cache.pas :: TMaxCache.GetOrCreate`)

### Adding or changing cache policies

Current policies exposed by public types are:

- Expiration: `TtlMs`, `IdleMs` (`TMaxCacheOptions`, `TMaxCacheConfig`)
- Dependency validation rate-limit: `ValidateIntervalMs` (`TMaxCacheOptions`, `TMaxCacheConfig`)
- Failure caching: `FailCacheMs` and global floor `MinFailCacheMs` (`TMaxCacheOptions`, `TMaxCacheConfig`, `maxlogic.cache.pas :: TMaxCache.GetOrCreate`)
- “Return stale on failure”: `ReturnStaleOnFailure` (`TMaxCacheOptions`, `TMaxCacheConfig`, `maxlogic.cache.pas :: TMaxCache.GetOrCreate`)
- Size caps and eviction: `MaxEntries*`, `MaxBytes*`, `SweepIntervalMs`, `SweepBatchSize` (`maxlogic.cache.pas :: TMaxCache.EnforceSizeCaps`, `maxlogic.cache.pas :: TMaxCache.EnforceGlobalSizeCaps`)

If we need new policies (e.g., per-entry max-size enforcement, custom eviction scoring, additional diagnostics), the integration points are:

- `TMaxCache.GetOrCreate` (decision to reuse vs load; publish rules; failure handling)
- `TMaxCache.ValidateDependency` (validation cadence and semantics)
- `TMaxCache.SweepTick` / `SweepBucket` / `EnforceSizeCaps` / `EnforceGlobalSizeCaps` (eviction behavior)
