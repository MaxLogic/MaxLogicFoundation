unit MaxLogic.Cache;

{$I fpc_delphimode.inc}
{$SCOPEDENUMS ON}

interface

uses
  System.Classes, System.Diagnostics, System.Generics.Collections, System.Generics.Defaults, System.IOUtils,
  System.SyncObjs, System.SysUtils,
  MaxLogic.hash.xxHash, MaxLogic.PortableTimer, maxLogic.StrUtils;

type
  EMaxCacheException = class(Exception);
  EMaxCacheLoadException = class(EMaxCacheException);
  EMaxCacheShutdown = class(EMaxCacheException);
  EMaxCacheWaitTimeout = class(EMaxCacheException);

  TMaxDependencyStamp = record
    MTimeUtcMs: Int64;
    SizeBytes: Int64;
    Hash32: Cardinal;
    HasHash: Boolean;
    class function FromFile(const aFileName: string; const aIncludeHash: Boolean = False): TMaxDependencyStamp; static;
    function Equals(const aOther: TMaxDependencyStamp): Boolean;
  end;

  IMaxCacheDependency = interface
    ['{1CB9C196-0DCC-4B88-A59B-62A0AEE0FC82}']
    function IsStale(out aNewStamp: TMaxDependencyStamp): Boolean;
    function GetStamp: TMaxDependencyStamp;
  end;

  TMaxFileDependency = class(TInterfacedObject, IMaxCacheDependency)
  private
    fAlwaysHashBelowBytes: Int64;
    fFileName: string;
    fForceHash: Boolean;
    fStamp: TMaxDependencyStamp;
    function ComputeStamp(const aIncludeHash: Boolean): TMaxDependencyStamp;
    function ShouldHash(const aBaseStamp: TMaxDependencyStamp; const aForce: Boolean): Boolean;
  public
    constructor Create(const aFileName: string; const aAlwaysHashBelowBytes: Int64 = 51200; const aForceHash: Boolean = False);
    function GetStamp: TMaxDependencyStamp;
    function IsStale(out aNewStamp: TMaxDependencyStamp): Boolean;
    property FileName: string read fFileName;
    property ForceHash: Boolean read fForceHash;
  end;

  TMaxCacheKeySize = record
    Namespace: string;
    Key: string;
    EstimatedBytes: Int64;
  end;

  TMaxCacheMetrics = record
    NamespaceCount: Integer;
    EntryCount: Integer;
    EstimatedMemoryKB: Int64;
    Hits: Int64;
    Misses: Int64;
    Loads: Int64;
    LoadFailures: Int64;
    InvalidationsDirect: Int64;
    InvalidationsByTag: Int64;
    EvictionsTtl: Int64;
    EvictionsIdle: Int64;
    EvictionsSize: Int64;
    AvgLoadTimeMs: Double;
    P95LoadTimeMs: Double;
    TopTenKeysBySize: TArray<TMaxCacheKeySize>;
  end;

  TMaxNamespaceCacheMetrics = record
    Namespace: string;
    EntryCount: Integer;
    EstimatedMemoryKB: Int64;
  end;

  TMaxCacheConfig = record
    DefaultTtlMs: Integer;
    DefaultIdleMs: Integer;
    DefaultValidateIntervalMs: Integer;
    MaxEntriesGlobal: Integer;
    MaxBytesGlobal: Int64;
    MaxEntriesPerNamespace: Integer;
    MaxBytesPerNamespace: Int64;
    MinFailCacheMs: Integer;
    DefaultWaitTimeoutMs: Integer;
    AlwaysHashBelowBytes: Integer;
    SweepIntervalMs: Integer;
    SweepBatchSize: Integer;
    CaseSensitiveKeys: Boolean;
    ReturnStaleOnFailure: Boolean;
    class function Default: TMaxCacheConfig; static;
  end;

  TMaxCacheOptions = class
  public
    TtlMs: Integer;
    IdleMs: Integer;
    ValidateIntervalMs: Integer;
    FailCacheMs: Integer;
    SizeEstimateBytes: Integer;
    Tags: TArray<string>;
    Dependency: IMaxCacheDependency;
    ReturnStaleOnFailure: Boolean;
    constructor Create;
  end;

  IMaxCache = interface
    ['{9C9FA4E8-B650-485F-8F52-2E6B4C93320D}']
    function GetOrCreate(const aNamespace, aKey: string; const aLoader: TFunc<IInterface>; const aOptions: TMaxCacheOptions = nil): IInterface;
    procedure Invalidate(const aNamespace, aKey: string);
    procedure InvalidateNamespace(const aNamespace: string);
    procedure InvalidateByTag(const aScopedTag: string);
    procedure InvalidateMany(const aNamespace: string; const aKeys: array of string);
    function TryGet(const aNamespace, aKey: string; out aValue: IInterface): Boolean;
    function GetMetrics: TMaxCacheMetrics;
    function GetNamespaceMetrics(const aNamespace: string): TMaxNamespaceCacheMetrics;
    procedure Shutdown;
  end;

  TMaxCache = class(TInterfacedObject, IMaxCache)
  public
    class function New(const aConfig: TMaxCacheConfig): IMaxCache; overload; static;
    class function New: IMaxCache; overload; static;
    class function ScopedTag(const aNamespace, aTag: string): string; static;
  public
    constructor Create(const aConfig: TMaxCacheConfig);
    destructor Destroy; override;

    function GetOrCreate(const aNamespace, aKey: string; const aLoader: TFunc<IInterface>; const aOptions: TMaxCacheOptions = nil): IInterface;
    procedure Invalidate(const aNamespace, aKey: string);
    procedure InvalidateNamespace(const aNamespace: string);
    procedure InvalidateByTag(const aScopedTag: string);
    procedure InvalidateMany(const aNamespace: string; const aKeys: array of string);
    function TryGet(const aNamespace, aKey: string; out aValue: IInterface): Boolean;
    function GetMetrics: TMaxCacheMetrics;
    function GetNamespaceMetrics(const aNamespace: string): TMaxNamespaceCacheMetrics;
    procedure Shutdown;
  private
    type
      TMaxCacheEntryState = (Empty, Loading, Ready, Failed, Obsolete);

      TMaxCacheEntry = class
      private
        fRefCount: Integer;
      public
        EntrySync: TObject;
        State: TMaxCacheEntryState;
        Value: IInterface;
        CreatedAtMs: Int64;
        LastAccessMs: Int64;
        ExpiresAtMs: Int64;
        IdleExpiresAtMs: Int64;
        NextValidateMs: Int64;
        ValidateIntervalMs: Integer;
        IsValidating: Integer;
        WasInvalidated: Integer;
        Dependency: IMaxCacheDependency;
        Stamp: TMaxDependencyStamp;
        Tags: TArray<string>;
        SizeEstimateBytes: Int64;
        RetryAtMs: Int64;
        FailCacheMs: Integer;
        ReturnStaleOnFailure: Boolean;
        TtlMs: Integer;
        IdleMs: Integer;
        LastLoadTimeMs: Integer;
        LastErrorMsg: string;
        LastErrorClassName: string;
        constructor Create;
        destructor Destroy; override;
        procedure AddRef;
        procedure Release;
      end;

      TMaxNamespaceBucket = class
      public
        Lock: TLightweightMREW;
        Items: TDictionary<string, TMaxCacheEntry>;
        constructor Create(const aComparer: IEqualityComparer<string>);
        destructor Destroy; override;
      end;

      TMaxTagRef = record
        Namespace: string;
        Key: string;
      end;
  private
    const
      cTagRefSep = #31;
      cScopedTagSep = '|';
      cGlobalTagPrefix = 'global:';
      cMinSweepIntervalMs = 1;
      cHistogramBucketCount = 8;
  private
    fConfig: TMaxCacheConfig;
    fComparer: IEqualityComparer<string>;

    fNamespacesLock: TLightweightMREW;
    fNamespaces: TDictionary<string, TMaxNamespaceBucket>;

    fTagIndexLock: TLightweightMREW;
    fTagIndex: TDictionary<string, TDictionary<string, Byte>>;

    fHits: Int64;
    fMisses: Int64;
    fLoads: Int64;
    fLoadFailures: Int64;
    fInvalidationsDirect: Int64;
    fInvalidationsByTag: Int64;
    fEvictionsTtl: Int64;
    fEvictionsIdle: Int64;
    fEvictionsSize: Int64;
    fTotalLoadTimeMs: Int64;
    fLoadTimeHistogram: array[0..cHistogramBucketCount - 1] of Int64;

    fIsShuttingDown: Integer;

    fSweeper: TPortableTimer;
    fSweepRunning: Integer;
  private
    class function NowMs: Int64; static;
    class function ClampNonNegative(const aValue: Integer): Integer; static;

    function GetOrCreateBucket(const aNamespace: string): TMaxNamespaceBucket;
    function TryGetBucket(const aNamespace: string): TMaxNamespaceBucket;

    function TryGetEntry(const aBucket: TMaxNamespaceBucket; const aKey: string): TMaxCacheEntry;
    function TryBeginLoad(
      const aKey: string;
      const aBucket: TMaxNamespaceBucket;
      const aNowMs: Int64;
      const aForceLoad: Boolean;
      out aEntry: TMaxCacheEntry): Boolean;

    procedure WaitForEntry(const aEntry: TMaxCacheEntry);
    procedure EnsureNotShuttingDown;
    function IsExpired(const aEntry: TMaxCacheEntry; const aNowMs: Int64; out aEvictKind: Integer): Boolean;
    procedure TouchEntry(const aEntry: TMaxCacheEntry; const aNowMs: Int64);
    function ValidateDependency(const aEntry: TMaxCacheEntry; const aNowMs: Int64): Boolean;
    procedure RegisterTags(const aNamespace, aKey: string; const aTags: TArray<string>);
    procedure UnregisterTags(const aNamespace, aKey: string; const aTags: TArray<string>);
    function GetTagRefsCopy(const aScopedTag: string): TArray<TMaxTagRef>;

    procedure RecordLoadTimeMs(const aElapsedMs: Integer);
    class function EstimateP95Ms(const aHistogram: array of Int64): Double; static;

    procedure SweeperTimer(aSender: TObject);
    procedure SweepTick;
    procedure SweepBucket(const aNamespace: string; const aBucket: TMaxNamespaceBucket; const aNowMs: Int64; var aRemaining: Integer);
    procedure EnforceSizeCaps(const aNamespace: string; const aBucket: TMaxNamespaceBucket; const aNowMs: Int64; var aRemaining: Integer);
    procedure EnforceGlobalSizeCaps(const aBucketPairs: TArray<TPair<string, TMaxNamespaceBucket>>; var aRemaining: Integer);

    function InvalidateInternal(const aNamespace, aKey: string; const aCountDirectMetric: Boolean): Boolean;
    procedure InvalidateNamespaceInternal(const aNamespace: string; const aCountDirectMetric: Boolean);
  end;

implementation

{ TMaxDependencyStamp }

class function TMaxDependencyStamp.FromFile(const aFileName: string; const aIncludeHash: Boolean): TMaxDependencyStamp;
var
  lDt: TDateTime;
  lBytes: TBytes;
begin
  Result.MTimeUtcMs := 0;
  Result.SizeBytes := -1;
  Result.Hash32 := 0;
  Result.HasHash := False;

  if (aFileName = '') or (not TFile.Exists(aFileName)) then
    Exit;

  Result.SizeBytes := TFile.GetSize(aFileName);
  lDt := TFile.GetLastWriteTimeUtc(aFileName);
  Result.MTimeUtcMs := Round(lDt * 86400000.0);

  if aIncludeHash then
  begin
    lBytes := TFile.ReadAllBytes(aFileName);
    if Length(lBytes) = 0 then
      Result.Hash32 := xxHash32(nil, 0, 0)
    else
      Result.Hash32 := xxHash32(@lBytes[0], Length(lBytes), 0);
    Result.HasHash := True;
  end;
end;

function TMaxDependencyStamp.Equals(const aOther: TMaxDependencyStamp): Boolean;
begin
  if MTimeUtcMs <> aOther.MTimeUtcMs then
    Exit(False);
  if SizeBytes <> aOther.SizeBytes then
    Exit(False);
  if HasHash and aOther.HasHash and (Hash32 <> aOther.Hash32) then
    Exit(False);
  Result := True;
end;

{ TMaxFileDependency }

constructor TMaxFileDependency.Create(const aFileName: string; const aAlwaysHashBelowBytes: Int64; const aForceHash: Boolean);
begin
  inherited Create;
  fFileName := aFileName;
  fAlwaysHashBelowBytes := aAlwaysHashBelowBytes;
  fForceHash := aForceHash;
  fStamp.MTimeUtcMs := 0;
  fStamp.SizeBytes := -1;
  fStamp.Hash32 := 0;
  fStamp.HasHash := False;
end;

function TMaxFileDependency.ShouldHash(const aBaseStamp: TMaxDependencyStamp; const aForce: Boolean): Boolean;
begin
  if aForce then
    Exit(True);
  if (aBaseStamp.SizeBytes >= 0) and (aBaseStamp.SizeBytes <= fAlwaysHashBelowBytes) then
    Exit(True);
  if not fStamp.Equals(aBaseStamp) then
    Exit(True);
  Result := False;
end;

function TMaxFileDependency.ComputeStamp(const aIncludeHash: Boolean): TMaxDependencyStamp;
begin
  Result := TMaxDependencyStamp.FromFile(fFileName, aIncludeHash);
end;

function TMaxFileDependency.GetStamp: TMaxDependencyStamp;
var
  lBase: TMaxDependencyStamp;
  lIncludeHash: Boolean;
begin
  lBase := ComputeStamp(False);
  lIncludeHash := fForceHash or ((lBase.SizeBytes >= 0) and (lBase.SizeBytes <= fAlwaysHashBelowBytes));
  if lIncludeHash then
    fStamp := ComputeStamp(True)
  else
    fStamp := lBase;
  Result := fStamp;
end;

function TMaxFileDependency.IsStale(out aNewStamp: TMaxDependencyStamp): Boolean;
var
  lBase: TMaxDependencyStamp;
  lIncludeHash: Boolean;
begin
  lBase := ComputeStamp(False);
  lIncludeHash := ShouldHash(lBase, fForceHash);
  if lIncludeHash then
    aNewStamp := ComputeStamp(True)
  else
    aNewStamp := lBase;
  Result := not fStamp.Equals(aNewStamp);
end;

{ TMaxCacheConfig }

class function TMaxCacheConfig.Default: TMaxCacheConfig;
begin
  Result.DefaultTtlMs := 0;
  Result.DefaultIdleMs := 0;
  Result.DefaultValidateIntervalMs := 0;
  Result.MaxEntriesGlobal := 0;
  Result.MaxBytesGlobal := 0;
  Result.MaxEntriesPerNamespace := 0;
  Result.MaxBytesPerNamespace := 0;
  Result.MinFailCacheMs := 500;
  Result.DefaultWaitTimeoutMs := 30000;
  Result.AlwaysHashBelowBytes := 51200;
  Result.SweepIntervalMs := 10000;
  Result.SweepBatchSize := 200;
  Result.CaseSensitiveKeys := True;
  Result.ReturnStaleOnFailure := False;
end;

{ TMaxCacheOptions }

constructor TMaxCacheOptions.Create;
begin
  inherited Create;
  TtlMs := 0;
  IdleMs := 0;
  ValidateIntervalMs := 0;
  FailCacheMs := 0;
  SizeEstimateBytes := 0;
  Tags := [];
  Dependency := nil;
  ReturnStaleOnFailure := False;
end;

{ TMaxCache.TMaxCacheEntry }

constructor TMaxCache.TMaxCacheEntry.Create;
begin
  inherited Create;
  fRefCount := 1;
  EntrySync := TObject.Create;
  State := TMaxCacheEntryState.Empty;
  Value := nil;
  CreatedAtMs := 0;
  LastAccessMs := 0;
  ExpiresAtMs := 0;
  IdleExpiresAtMs := 0;
  NextValidateMs := 0;
  ValidateIntervalMs := 0;
  IsValidating := 0;
  WasInvalidated := 0;
  Dependency := nil;
  Stamp.MTimeUtcMs := 0;
  Stamp.SizeBytes := -1;
  Stamp.Hash32 := 0;
  Stamp.HasHash := False;
  Tags := [];
  SizeEstimateBytes := 0;
  RetryAtMs := 0;
  FailCacheMs := 0;
  ReturnStaleOnFailure := False;
  TtlMs := 0;
  IdleMs := 0;
  LastLoadTimeMs := 0;
  LastErrorMsg := '';
  LastErrorClassName := '';
end;

destructor TMaxCache.TMaxCacheEntry.Destroy;
begin
  EntrySync.Free;
  inherited Destroy;
end;

procedure TMaxCache.TMaxCacheEntry.AddRef;
begin
  TInterlocked.Increment(fRefCount);
end;

procedure TMaxCache.TMaxCacheEntry.Release;
begin
  if TInterlocked.Decrement(fRefCount) = 0 then
    Free;
end;

{ TMaxCache.TMaxNamespaceBucket }

constructor TMaxCache.TMaxNamespaceBucket.Create(const aComparer: IEqualityComparer<string>);
begin
  inherited Create;
  Items := TDictionary<string, TMaxCacheEntry>.Create(aComparer);
end;

destructor TMaxCache.TMaxNamespaceBucket.Destroy;
var
  lEntry: TMaxCacheEntry;
begin
  for lEntry in Items.Values do
    lEntry.Release;
  Items.Free;
  inherited Destroy;
end;

{ TMaxCache }

class function TMaxCache.New(const aConfig: TMaxCacheConfig): IMaxCache;
begin
  Result := TMaxCache.Create(aConfig);
end;

class function TMaxCache.New: IMaxCache;
begin
  Result := TMaxCache.Create(TMaxCacheConfig.Default);
end;

class function TMaxCache.ScopedTag(const aNamespace, aTag: string): string;
begin
  if aTag.StartsWith(cGlobalTagPrefix, True) then
    Exit(aTag);
  if aTag.IndexOf(cScopedTagSep) >= 0 then
    Exit(aTag);
  Result := aNamespace + cScopedTagSep + aTag;
end;

constructor TMaxCache.Create(const aConfig: TMaxCacheConfig);
var
  lInterval: Integer;
begin
  inherited Create;
  fConfig := aConfig;

  if fConfig.CaseSensitiveKeys then
    fComparer := TFastCaseAwareComparer.Ordinal
  else
    fComparer := TFastCaseAwareComparer.OrdinalIgnoreCase;

  fNamespaces := TDictionary<string, TMaxNamespaceBucket>.Create(fComparer);
  fTagIndex := TDictionary<string, TDictionary<string, Byte>>.Create(fComparer);

  fHits := 0;
  fMisses := 0;
  fLoads := 0;
  fLoadFailures := 0;
  fInvalidationsDirect := 0;
  fInvalidationsByTag := 0;
  fEvictionsTtl := 0;
  fEvictionsIdle := 0;
  fEvictionsSize := 0;
  fTotalLoadTimeMs := 0;
  FillChar(fLoadTimeHistogram, SizeOf(fLoadTimeHistogram), 0);

  fIsShuttingDown := 0;
  fSweeper := nil;
  fSweepRunning := 0;

  lInterval := ClampNonNegative(fConfig.SweepIntervalMs);
  if lInterval < cMinSweepIntervalMs then
    lInterval := cMinSweepIntervalMs;

  if (fConfig.SweepIntervalMs > 0) and (fConfig.SweepBatchSize > 0) then
  begin
    fSweeper := TPortableTimer.Create;
    fSweeper.SchedulingMode := tmFixedDelay;
    fSweeper.OnTimer := SweeperTimer;
    fSweeper.Start(Cardinal(lInterval));
  end;
end;

destructor TMaxCache.Destroy;
var
  lBucket: TMaxNamespaceBucket;
  lSet: TDictionary<string, Byte>;
begin
  Shutdown;

  if fSweeper <> nil then
  begin
    fSweeper.Stop;
    fSweeper.Free;
    fSweeper := nil;
  end;

  fNamespacesLock.BeginWrite;
  try
    for lBucket in fNamespaces.Values do
      lBucket.Free;
    fNamespaces.Free;
    fNamespaces := nil;
  finally
    fNamespacesLock.EndWrite;
  end;

  fTagIndexLock.BeginWrite;
  try
    if fTagIndex <> nil then
    begin
      for lSet in fTagIndex.Values do
        lSet.Free;
      fTagIndex.Free;
    end;
    fTagIndex := nil;
  finally
    fTagIndexLock.EndWrite;
  end;

  inherited Destroy;
end;

class function TMaxCache.NowMs: Int64;
begin
  Result := (TStopwatch.GetTimeStamp * 1000) div TStopwatch.Frequency;
end;

class function TMaxCache.ClampNonNegative(const aValue: Integer): Integer;
begin
  if aValue < 0 then
    Exit(0);
  Result := aValue;
end;

procedure TMaxCache.EnsureNotShuttingDown;
begin
  if TInterlocked.CompareExchange(fIsShuttingDown, 0, 0) <> 0 then
    raise EMaxCacheShutdown.Create('Cache is shutting down');
end;

function TMaxCache.TryGetBucket(const aNamespace: string): TMaxNamespaceBucket;
begin
  Result := nil;
  fNamespacesLock.BeginRead;
  try
    fNamespaces.TryGetValue(aNamespace, Result);
  finally
    fNamespacesLock.EndRead;
  end;
end;

function TMaxCache.GetOrCreateBucket(const aNamespace: string): TMaxNamespaceBucket;
begin
  Result := TryGetBucket(aNamespace);
  if Result <> nil then
    Exit;

  fNamespacesLock.BeginWrite;
  try
    if not fNamespaces.TryGetValue(aNamespace, Result) then
    begin
      Result := TMaxNamespaceBucket.Create(fComparer);
      fNamespaces.Add(aNamespace, Result);
    end;
  finally
    fNamespacesLock.EndWrite;
  end;
end;

function TMaxCache.TryGetEntry(const aBucket: TMaxNamespaceBucket; const aKey: string): TMaxCacheEntry;
begin
  Result := nil;
  aBucket.Lock.BeginRead;
  try
    if aBucket.Items.TryGetValue(aKey, Result) then
      Result.AddRef
    else
      Result := nil;
  finally
    aBucket.Lock.EndRead;
  end;
end;

function TMaxCache.IsExpired(const aEntry: TMaxCacheEntry; const aNowMs: Int64; out aEvictKind: Integer): Boolean;
begin
  aEvictKind := 0;
  if (aEntry.ExpiresAtMs > 0) and (aNowMs >= aEntry.ExpiresAtMs) then
  begin
    aEvictKind := 1;
    Exit(True);
  end;
  if (aEntry.IdleExpiresAtMs > 0) and (aNowMs >= aEntry.IdleExpiresAtMs) then
  begin
    aEvictKind := 2;
    Exit(True);
  end;
  Result := False;
end;

procedure TMaxCache.TouchEntry(const aEntry: TMaxCacheEntry; const aNowMs: Int64);
begin
  aEntry.LastAccessMs := aNowMs;
  if aEntry.IdleMs > 0 then
    aEntry.IdleExpiresAtMs := aNowMs + aEntry.IdleMs;
end;

function TMaxCache.ValidateDependency(const aEntry: TMaxCacheEntry; const aNowMs: Int64): Boolean;
var
  lShouldValidate: Boolean;
  lNewStamp: TMaxDependencyStamp;
  lIsStale: Boolean;
  lNext: Int64;
begin
  if aEntry.Dependency = nil then
    Exit(True);

  if aEntry.ValidateIntervalMs = 0 then
    lShouldValidate := True
  else
    lShouldValidate := (aNowMs >= aEntry.NextValidateMs);

  if not lShouldValidate then
    Exit(True);

  if TInterlocked.CompareExchange(aEntry.IsValidating, 1, 0) <> 0 then
    Exit(True); // another thread validates; optimistic

  try
    // Another thread may have completed validation between our first check and winning the CAS.
    // Re-check "due" under the entry monitor to guarantee at most one validation per interval.
    TMonitor.Enter(aEntry.EntrySync);
    try
      if aEntry.ValidateIntervalMs = 0 then
        lShouldValidate := True
      else
        lShouldValidate := (aNowMs >= aEntry.NextValidateMs);
    finally
      TMonitor.Exit(aEntry.EntrySync);
    end;

    if not lShouldValidate then
      Exit(True);

    lIsStale := False;
    try
      lIsStale := aEntry.Dependency.IsStale(lNewStamp);
    except
      lIsStale := True;
    end;

    TMonitor.Enter(aEntry.EntrySync);
    try
      if aEntry.ValidateIntervalMs = 0 then
        lNext := aNowMs
      else
        lNext := aNowMs + aEntry.ValidateIntervalMs;
      aEntry.NextValidateMs := lNext;
      if lIsStale and (aEntry.State = TMaxCacheEntryState.Ready) then
        aEntry.State := TMaxCacheEntryState.Obsolete;
    finally
      TMonitor.Exit(aEntry.EntrySync);
    end;
    Result := not lIsStale;
  finally
    TInterlocked.Exchange(aEntry.IsValidating, 0);
  end;
end;

procedure TMaxCache.WaitForEntry(const aEntry: TMaxCacheEntry);
var
  lTimeout: Integer;
begin
  lTimeout := fConfig.DefaultWaitTimeoutMs;
  if lTimeout <= 0 then
    lTimeout := 30000;

  TMonitor.Enter(aEntry.EntrySync);
  try
    if aEntry.State <> TMaxCacheEntryState.Loading then
      Exit;

    if not TMonitor.Wait(aEntry.EntrySync, lTimeout) then
    begin
      if aEntry.State = TMaxCacheEntryState.Loading then
        raise EMaxCacheWaitTimeout.Create('Timed out waiting for cache entry load');
    end;
  finally
    TMonitor.Exit(aEntry.EntrySync);
  end;
end;

function TMaxCache.TryBeginLoad(
  const aKey: string;
  const aBucket: TMaxNamespaceBucket;
  const aNowMs: Int64;
  const aForceLoad: Boolean;
  out aEntry: TMaxCacheEntry): Boolean;
var
  lEntry: TMaxCacheEntry;
begin
  aEntry := nil;
  Result := False;

  aBucket.Lock.BeginWrite;
  try
    if not aBucket.Items.TryGetValue(aKey, lEntry) then
    begin
      lEntry := TMaxCacheEntry.Create;
      lEntry.State := TMaxCacheEntryState.Loading;
      lEntry.CreatedAtMs := aNowMs;
      lEntry.LastAccessMs := aNowMs;
      aBucket.Items.Add(aKey, lEntry);
      lEntry.AddRef;
      aEntry := lEntry;
      Exit(True);
    end;

    lEntry.AddRef;
    aEntry := lEntry;

    if lEntry.State = TMaxCacheEntryState.Loading then
      Exit(False);

    if lEntry.State = TMaxCacheEntryState.Failed then
    begin
      if aNowMs < lEntry.RetryAtMs then
        Exit(False);
    end;

    if (not aForceLoad) and (lEntry.State = TMaxCacheEntryState.Ready) then
      Exit(False);

    lEntry.State := TMaxCacheEntryState.Loading;
    Result := True;
  finally
    aBucket.Lock.EndWrite;
  end;
end;

procedure TMaxCache.RegisterTags(const aNamespace, aKey: string; const aTags: TArray<string>);
var
  lTag: string;
  lScoped: string;
  lRefKey: string;
  lSet: TDictionary<string, Byte>;
begin
  if Length(aTags) = 0 then
    Exit;

  lRefKey := aNamespace + cTagRefSep + aKey;

  fTagIndexLock.BeginWrite;
  try
    for lTag in aTags do
    begin
      lScoped := ScopedTag(aNamespace, lTag);
      if not fTagIndex.TryGetValue(lScoped, lSet) then
      begin
        lSet := TDictionary<string, Byte>.Create(fComparer);
        fTagIndex.Add(lScoped, lSet);
      end;
      lSet.AddOrSetValue(lRefKey, 0);
    end;
  finally
    fTagIndexLock.EndWrite;
  end;
end;

procedure TMaxCache.UnregisterTags(const aNamespace, aKey: string; const aTags: TArray<string>);
var
  lTag: string;
  lScoped: string;
  lRefKey: string;
  lSet: TDictionary<string, Byte>;
begin
  if Length(aTags) = 0 then
    Exit;

  lRefKey := aNamespace + cTagRefSep + aKey;

  fTagIndexLock.BeginWrite;
  try
    for lTag in aTags do
    begin
      lScoped := ScopedTag(aNamespace, lTag);
      if fTagIndex.TryGetValue(lScoped, lSet) then
      begin
        lSet.Remove(lRefKey);
        if lSet.Count = 0 then
        begin
          fTagIndex.Remove(lScoped);
          lSet.Free;
        end;
      end;
    end;
  finally
    fTagIndexLock.EndWrite;
  end;
end;

function TMaxCache.GetTagRefsCopy(const aScopedTag: string): TArray<TMaxTagRef>;
var
  lSet: TDictionary<string, Byte>;
  lRefKey: string;
  lSepPos: Integer;
  lIdx: Integer;
begin
  Result := [];

  fTagIndexLock.BeginRead;
  try
    if not fTagIndex.TryGetValue(aScopedTag, lSet) then
      Exit;

    SetLength(Result, lSet.Count);
    lIdx := 0;
    for lRefKey in lSet.Keys do
    begin
      lSepPos := lRefKey.IndexOf(cTagRefSep);
      if lSepPos > 0 then
      begin
        Result[lIdx].Namespace := lRefKey.Substring(0, lSepPos);
        Result[lIdx].Key := lRefKey.Substring(lSepPos + 1);
        Inc(lIdx);
      end;
    end;
    if lIdx <> Length(Result) then
      SetLength(Result, lIdx);
  finally
    fTagIndexLock.EndRead;
  end;
end;

procedure TMaxCache.RecordLoadTimeMs(const aElapsedMs: Integer);
const
  cBounds: array[0..6] of Integer = (1, 5, 10, 50, 100, 500, 1000);
var
  lIdx: Integer;
  lMs: Integer;
begin
  lMs := aElapsedMs;
  if lMs < 0 then
    lMs := 0;

  lIdx := 0;
  while (lIdx < Length(cBounds)) and (lMs >= cBounds[lIdx]) do
    Inc(lIdx);

  TInterlocked.Increment(fLoadTimeHistogram[lIdx]);
  TInterlocked.Add(fTotalLoadTimeMs, lMs);
end;

class function TMaxCache.EstimateP95Ms(const aHistogram: array of Int64): Double;
const
  cBounds: array[0..cHistogramBucketCount - 1] of Integer = (1, 5, 10, 50, 100, 500, 1000, 1000);
var
  lTotal: Int64;
  lTarget: Int64;
  lCum: Int64;
  i: Integer;
begin
  lTotal := 0;
  for i := Low(aHistogram) to High(aHistogram) do
    Inc(lTotal, aHistogram[i]);

  if lTotal <= 0 then
    Exit(0);

  lTarget := (lTotal * 95 + 99) div 100; // ceil
  lCum := 0;
  for i := Low(aHistogram) to High(aHistogram) do
  begin
    Inc(lCum, aHistogram[i]);
    if lCum >= lTarget then
      Exit(cBounds[i]);
  end;

  Result := cBounds[High(cBounds)];
end;

function TMaxCache.GetOrCreate(const aNamespace, aKey: string; const aLoader: TFunc<IInterface>; const aOptions: TMaxCacheOptions): IInterface;
var
  lBucket: TMaxNamespaceBucket;
  lEntry: TMaxCacheEntry;
  lNow: Int64;
  lEvictKind: Integer;
  lNeedLoad: Boolean;
  lRunLoader: Boolean;
  lTtlMs: Integer;
  lIdleMs: Integer;
  lValidateIntervalMs: Integer;
  lFailCacheMs: Integer;
  lSizeEstimateBytes: Integer;
  lReturnStaleOnFailure: Boolean;
  lDependency: IMaxCacheDependency;
  lOldTags: TArray<string>;
  lNewTags: TArray<string>;
  lRetryAt: Int64;
  lSw: TStopwatch;
  lLoadedIntf: IInterface;
  lAbandonedByInvalidation: Boolean;
begin
  EnsureNotShuttingDown;

  if not Assigned(aLoader) then
    raise EMaxCacheException.Create('Loader must be assigned');

  lBucket := GetOrCreateBucket(aNamespace);

  while True do
  begin
    EnsureNotShuttingDown;
    lNow := NowMs;
    lNeedLoad := False;

    lEntry := TryGetEntry(lBucket, aKey);
    if lEntry <> nil then
    begin
      try
        case lEntry.State of
          TMaxCacheEntryState.Ready:
          begin
            if IsExpired(lEntry, lNow, lEvictKind) then
              lNeedLoad := True
            else if not ValidateDependency(lEntry, lNow) then
              lNeedLoad := True
            else
            begin
              TouchEntry(lEntry, lNow);
              Result := lEntry.Value;
              TInterlocked.Increment(fHits);
              Exit;
            end;
          end;

          TMaxCacheEntryState.Loading:
          begin
            TInterlocked.Increment(fMisses);
            WaitForEntry(lEntry);
            Continue;
          end;

          TMaxCacheEntryState.Failed:
          begin
            if lNow < lEntry.RetryAtMs then
            begin
              if lEntry.ReturnStaleOnFailure and (lEntry.Value <> nil) then
              begin
                TouchEntry(lEntry, lNow);
                Result := lEntry.Value;
                TInterlocked.Increment(fHits);
                Exit;
              end;
              raise EMaxCacheLoadException.Create(lEntry.LastErrorClassName + ': ' + lEntry.LastErrorMsg);
            end;
            lNeedLoad := True;
          end;

          TMaxCacheEntryState.Empty,
          TMaxCacheEntryState.Obsolete:
            lNeedLoad := True;
        else
          lNeedLoad := True;
        end;
      finally
        lEntry.Release;
      end;
    end
    else
      lNeedLoad := True;

    if not lNeedLoad then
      Continue;

    TInterlocked.Increment(fMisses);

    lRunLoader := TryBeginLoad(aKey, lBucket, lNow, True, lEntry);
    if lEntry = nil then
      Continue;
    try
      if not lRunLoader then
      begin
        if lEntry.State = TMaxCacheEntryState.Loading then
          WaitForEntry(lEntry);
        Continue;
      end;

      if TInterlocked.CompareExchange(fIsShuttingDown, 0, 0) <> 0 then
      begin
        TMonitor.Enter(lEntry.EntrySync);
        try
          lEntry.WasInvalidated := 1;
          lEntry.State := TMaxCacheEntryState.Obsolete;
          TMonitor.PulseAll(lEntry.EntrySync);
        finally
          TMonitor.Exit(lEntry.EntrySync);
        end;
        raise EMaxCacheShutdown.Create('Cache is shutting down');
      end;

      if lEntry.WasInvalidated <> 0 then
      begin
        TMonitor.Enter(lEntry.EntrySync);
        try
          lEntry.State := TMaxCacheEntryState.Obsolete;
          TMonitor.PulseAll(lEntry.EntrySync);
        finally
          TMonitor.Exit(lEntry.EntrySync);
        end;
        Continue;
      end;

      TInterlocked.Increment(fLoads);

      if aOptions <> nil then
      begin
        lTtlMs := aOptions.TtlMs;
        lIdleMs := aOptions.IdleMs;
        lValidateIntervalMs := aOptions.ValidateIntervalMs;
        lFailCacheMs := aOptions.FailCacheMs;
        lSizeEstimateBytes := aOptions.SizeEstimateBytes;
        lNewTags := aOptions.Tags;
        lDependency := aOptions.Dependency;
        lReturnStaleOnFailure := aOptions.ReturnStaleOnFailure;
      end
      else if lEntry.Value <> nil then
      begin
        lTtlMs := lEntry.TtlMs;
        lIdleMs := lEntry.IdleMs;
        lValidateIntervalMs := lEntry.ValidateIntervalMs;
        lFailCacheMs := lEntry.FailCacheMs;
        lSizeEstimateBytes := Integer(lEntry.SizeEstimateBytes);
        lNewTags := lEntry.Tags;
        lDependency := lEntry.Dependency;
        lReturnStaleOnFailure := lEntry.ReturnStaleOnFailure;
      end
      else
      begin
        lTtlMs := fConfig.DefaultTtlMs;
        lIdleMs := fConfig.DefaultIdleMs;
        lValidateIntervalMs := fConfig.DefaultValidateIntervalMs;
        lFailCacheMs := 0;
        lSizeEstimateBytes := 0;
        lNewTags := [];
        lDependency := nil;
        lReturnStaleOnFailure := fConfig.ReturnStaleOnFailure;
      end;

      lSw := TStopwatch.StartNew;
      try
        lLoadedIntf := aLoader();
      except
        on E: Exception do
        begin
          TInterlocked.Increment(fLoadFailures);

          if lFailCacheMs < fConfig.MinFailCacheMs then
            lFailCacheMs := fConfig.MinFailCacheMs;
          lRetryAt := NowMs + lFailCacheMs;

          TMonitor.Enter(lEntry.EntrySync);
          try
            lEntry.State := TMaxCacheEntryState.Failed;
            lEntry.RetryAtMs := lRetryAt;
            lEntry.LastErrorMsg := E.Message;
            lEntry.LastErrorClassName := E.ClassName;
            lEntry.FailCacheMs := lFailCacheMs;
            lEntry.ReturnStaleOnFailure := lReturnStaleOnFailure;
            TMonitor.PulseAll(lEntry.EntrySync);
          finally
            TMonitor.Exit(lEntry.EntrySync);
          end;

          if lReturnStaleOnFailure and (lEntry.Value <> nil) then
          begin
            TouchEntry(lEntry, NowMs);
            Result := lEntry.Value;
            TInterlocked.Increment(fHits);
            Exit;
          end;

          raise EMaxCacheLoadException.Create(E.ClassName + ': ' + E.Message);
        end;
      end;

      lNow := NowMs;
      lEntry.LastLoadTimeMs := lSw.ElapsedMilliseconds;
      RecordLoadTimeMs(lEntry.LastLoadTimeMs);

      if lDependency <> nil then
        lEntry.Stamp := lDependency.GetStamp
      else
      begin
        lEntry.Stamp.MTimeUtcMs := 0;
        lEntry.Stamp.SizeBytes := -1;
        lEntry.Stamp.Hash32 := 0;
        lEntry.Stamp.HasHash := False;
      end;

      lOldTags := [];
      lAbandonedByInvalidation := False;

      TMonitor.Enter(lEntry.EntrySync);
      try
        if TInterlocked.CompareExchange(fIsShuttingDown, 0, 0) <> 0 then
        begin
          lEntry.State := TMaxCacheEntryState.Obsolete;
          TMonitor.PulseAll(lEntry.EntrySync);
          raise EMaxCacheShutdown.Create('Cache is shutting down');
        end;

        if lEntry.WasInvalidated <> 0 then
        begin
          lAbandonedByInvalidation := True;
          lEntry.State := TMaxCacheEntryState.Obsolete;
          TMonitor.PulseAll(lEntry.EntrySync);
        end;

        if not lAbandonedByInvalidation then
        begin
          lOldTags := lEntry.Tags;

          lEntry.Value := lLoadedIntf;
          lEntry.State := TMaxCacheEntryState.Ready;
          lEntry.CreatedAtMs := lNow;
          lEntry.LastAccessMs := lNow;

          lEntry.TtlMs := lTtlMs;
          lEntry.IdleMs := lIdleMs;
          lEntry.ValidateIntervalMs := lValidateIntervalMs;
          lEntry.FailCacheMs := lFailCacheMs;
          lEntry.ReturnStaleOnFailure := lReturnStaleOnFailure;

          lEntry.Dependency := lDependency;
          lEntry.Tags := lNewTags;
          if lSizeEstimateBytes > 0 then
            lEntry.SizeEstimateBytes := lSizeEstimateBytes;

          if lTtlMs > 0 then
            lEntry.ExpiresAtMs := lNow + lTtlMs
          else
            lEntry.ExpiresAtMs := 0;

          if lIdleMs > 0 then
            lEntry.IdleExpiresAtMs := lNow + lIdleMs
          else
            lEntry.IdleExpiresAtMs := 0;

          if lValidateIntervalMs = 0 then
            lEntry.NextValidateMs := lNow
          else
            lEntry.NextValidateMs := lNow + lValidateIntervalMs;

          lEntry.RetryAtMs := 0;
          lEntry.LastErrorMsg := '';
          lEntry.LastErrorClassName := '';

          TMonitor.PulseAll(lEntry.EntrySync);
        end;
      finally
        TMonitor.Exit(lEntry.EntrySync);
      end;

      if Length(lOldTags) > 0 then
        UnregisterTags(aNamespace, aKey, lOldTags);
      if (not lAbandonedByInvalidation) and (lEntry.WasInvalidated = 0) and (Length(lNewTags) > 0) then
        RegisterTags(aNamespace, aKey, lNewTags);

      if lAbandonedByInvalidation then
        Continue;

      Result := lLoadedIntf;
      Exit;
    finally
      lEntry.Release;
    end;
  end;
end;

function TMaxCache.TryGet(const aNamespace, aKey: string; out aValue: IInterface): Boolean;
var
  lBucket: TMaxNamespaceBucket;
  lEntry: TMaxCacheEntry;
  lNow: Int64;
  lEvictKind: Integer;
begin
  aValue := nil;
  EnsureNotShuttingDown;

  lBucket := TryGetBucket(aNamespace);
  if lBucket = nil then
    Exit(False);

  lNow := NowMs;

  lEntry := TryGetEntry(lBucket, aKey);
  if lEntry = nil then
    Exit(False);
  try
    if lEntry.State <> TMaxCacheEntryState.Ready then
      Exit(False);
    if IsExpired(lEntry, lNow, lEvictKind) then
      Exit(False);

    aValue := lEntry.Value;
    Result := True;
  finally
    lEntry.Release;
  end;
end;

function TMaxCache.InvalidateInternal(const aNamespace, aKey: string; const aCountDirectMetric: Boolean): Boolean;
var
  lBucket: TMaxNamespaceBucket;
  lEntry: TMaxCacheEntry;
  lTags: TArray<string>;
begin
  Result := False;

  lBucket := TryGetBucket(aNamespace);
  if lBucket = nil then
    Exit(False);

  lEntry := nil;
  lTags := [];
  lBucket.Lock.BeginWrite;
  try
    if lBucket.Items.TryGetValue(aKey, lEntry) then
    begin
      lBucket.Items.Remove(aKey);
    end;
  finally
    lBucket.Lock.EndWrite;
  end;

  if lEntry = nil then
    Exit(False);
  try
    TMonitor.Enter(lEntry.EntrySync);
    try
      lTags := lEntry.Tags;
      lEntry.WasInvalidated := 1;
      lEntry.State := TMaxCacheEntryState.Obsolete;
      lEntry.Value := nil;
      TMonitor.PulseAll(lEntry.EntrySync);
    finally
      TMonitor.Exit(lEntry.EntrySync);
    end;
  finally
    lEntry.Release;
  end;

  Result := True;

  if aCountDirectMetric then
    TInterlocked.Increment(fInvalidationsDirect);

  if Length(lTags) > 0 then
    UnregisterTags(aNamespace, aKey, lTags);
end;

procedure TMaxCache.Invalidate(const aNamespace, aKey: string);
begin
  InvalidateInternal(aNamespace, aKey, True);
end;

procedure TMaxCache.InvalidateMany(const aNamespace: string; const aKeys: array of string);
var
  k: string;
begin
  for k in aKeys do
    InvalidateInternal(aNamespace, k, True);
end;

procedure TMaxCache.InvalidateNamespaceInternal(const aNamespace: string; const aCountDirectMetric: Boolean);
var
  lBucket: TMaxNamespaceBucket;
  lPairs: TArray<TPair<string, TMaxCacheEntry>>;
  lIdx: Integer;
  lKey: string;
  lEntry: TMaxCacheEntry;
  lTags: TArray<string>;
begin
  lBucket := TryGetBucket(aNamespace);
  if lBucket = nil then
    Exit;

  lBucket.Lock.BeginWrite;
  try
    SetLength(lPairs, lBucket.Items.Count);
    lIdx := 0;
    for lKey in lBucket.Items.Keys do
    begin
      lEntry := lBucket.Items[lKey];
      lPairs[lIdx] := TPair<string, TMaxCacheEntry>.Create(lKey, lEntry);
      Inc(lIdx);
    end;
    lBucket.Items.Clear;
  finally
    lBucket.Lock.EndWrite;
  end;

  for lIdx := Low(lPairs) to High(lPairs) do
  begin
    lKey := lPairs[lIdx].Key;
    lEntry := lPairs[lIdx].Value;
    lTags := [];
    try
      TMonitor.Enter(lEntry.EntrySync);
      try
        lTags := lEntry.Tags;
        lEntry.WasInvalidated := 1;
        lEntry.State := TMaxCacheEntryState.Obsolete;
        lEntry.Value := nil;
        TMonitor.PulseAll(lEntry.EntrySync);
      finally
        TMonitor.Exit(lEntry.EntrySync);
      end;
    finally
      if Length(lTags) > 0 then
        UnregisterTags(aNamespace, lKey, lTags);
      lEntry.Release;
    end;
  end;

  if aCountDirectMetric then
    TInterlocked.Increment(fInvalidationsDirect);
end;

procedure TMaxCache.InvalidateNamespace(const aNamespace: string);
begin
  InvalidateNamespaceInternal(aNamespace, True);
end;

procedure TMaxCache.InvalidateByTag(const aScopedTag: string);
var
  lRefs: TArray<TMaxTagRef>;
  lRef: TMaxTagRef;
begin
  if (not aScopedTag.StartsWith(cGlobalTagPrefix, True)) and (aScopedTag.IndexOf(cScopedTagSep) < 0) then
    raise EMaxCacheException.Create('InvalidateByTag requires a scoped tag (Namespace|Tag) or a global: tag');

  TInterlocked.Increment(fInvalidationsByTag);

  lRefs := GetTagRefsCopy(aScopedTag);
  for lRef in lRefs do
    InvalidateInternal(lRef.Namespace, lRef.Key, False);
end;

function TMaxCache.GetNamespaceMetrics(const aNamespace: string): TMaxNamespaceCacheMetrics;
var
  lBucket: TMaxNamespaceBucket;
  lEntry: TMaxCacheEntry;
  lBytes: Int64;
begin
  Result.Namespace := aNamespace;
  Result.EntryCount := 0;
  Result.EstimatedMemoryKB := 0;

  lBucket := TryGetBucket(aNamespace);
  if lBucket = nil then
    Exit;

  lBytes := 0;
  lBucket.Lock.BeginRead;
  try
    Result.EntryCount := lBucket.Items.Count;
    for lEntry in lBucket.Items.Values do
      Inc(lBytes, lEntry.SizeEstimateBytes);
  finally
    lBucket.Lock.EndRead;
  end;
  Result.EstimatedMemoryKB := (lBytes + 1023) div 1024;
end;

function TMaxCache.GetMetrics: TMaxCacheMetrics;
var
  lBuckets: TArray<TPair<string, TMaxNamespaceBucket>>;
  lBucketPair: TPair<string, TMaxNamespaceBucket>;
  lBucket: TMaxNamespaceBucket;
  lNs: string;
  lEntry: TMaxCacheEntry;
  lBytes: Int64;
  lKeys: TArray<TMaxCacheKeySize>;
  lKey: string;
  lIdx: Integer;
  i: Integer;
  lTotalLoads: Int64;
  lHistogram: array[0..cHistogramBucketCount - 1] of Int64;
begin
  Result.NamespaceCount := 0;
  Result.EntryCount := 0;
  Result.EstimatedMemoryKB := 0;
  Result.Hits := TInterlocked.Read(PInt64(@fHits)^);
  Result.Misses := TInterlocked.Read(PInt64(@fMisses)^);
  Result.Loads := TInterlocked.Read(PInt64(@fLoads)^);
  Result.LoadFailures := TInterlocked.Read(PInt64(@fLoadFailures)^);
  Result.InvalidationsDirect := TInterlocked.Read(PInt64(@fInvalidationsDirect)^);
  Result.InvalidationsByTag := TInterlocked.Read(PInt64(@fInvalidationsByTag)^);
  Result.EvictionsTtl := TInterlocked.Read(PInt64(@fEvictionsTtl)^);
  Result.EvictionsIdle := TInterlocked.Read(PInt64(@fEvictionsIdle)^);
  Result.EvictionsSize := TInterlocked.Read(PInt64(@fEvictionsSize)^);

  lTotalLoads := Result.Loads;
  if lTotalLoads > 0 then
    Result.AvgLoadTimeMs := TInterlocked.Read(PInt64(@fTotalLoadTimeMs)^) / lTotalLoads
  else
    Result.AvgLoadTimeMs := 0;

  for i := 0 to cHistogramBucketCount - 1 do
    lHistogram[i] := TInterlocked.Read(PInt64(@fLoadTimeHistogram[i])^);
  Result.P95LoadTimeMs := EstimateP95Ms(lHistogram);

  fNamespacesLock.BeginRead;
  try
    SetLength(lBuckets, fNamespaces.Count);
    lIdx := 0;
    for lBucketPair in fNamespaces do
    begin
      lBuckets[lIdx] := lBucketPair;
      Inc(lIdx);
    end;
  finally
    fNamespacesLock.EndRead;
  end;

  lBytes := 0;
  SetLength(lKeys, 0);

  for lBucketPair in lBuckets do
  begin
    lNs := lBucketPair.Key;
    lBucket := lBucketPair.Value;

    lBucket.Lock.BeginRead;
    try
      Inc(Result.EntryCount, lBucket.Items.Count);
      for lKey in lBucket.Items.Keys do
      begin
        lEntry := lBucket.Items[lKey];
        Inc(lBytes, lEntry.SizeEstimateBytes);

        if lEntry.SizeEstimateBytes > 0 then
        begin
          SetLength(lKeys, Length(lKeys) + 1);
          lKeys[High(lKeys)].Namespace := lNs;
          lKeys[High(lKeys)].Key := lKey;
          lKeys[High(lKeys)].EstimatedBytes := lEntry.SizeEstimateBytes;
        end;
      end;
    finally
      lBucket.Lock.EndRead;
    end;
  end;

  Result.NamespaceCount := Length(lBuckets);
  Result.EstimatedMemoryKB := (lBytes + 1023) div 1024;

  TArray.Sort<TMaxCacheKeySize>(lKeys,
    TComparer<TMaxCacheKeySize>.Construct(
      function(const aLeft, aRight: TMaxCacheKeySize): Integer
      begin
        if aLeft.EstimatedBytes > aRight.EstimatedBytes then
          Exit(-1);
        if aLeft.EstimatedBytes < aRight.EstimatedBytes then
          Exit(1);
        Result := 0;
      end));

  if Length(lKeys) > 10 then
    SetLength(lKeys, 10);
  Result.TopTenKeysBySize := lKeys;
end;

procedure TMaxCache.Shutdown;
var
  lBucketPair: TPair<string, TMaxNamespaceBucket>;
  lBucket: TMaxNamespaceBucket;
  lEntryPair: TPair<string, TMaxCacheEntry>;
  lEntry: TMaxCacheEntry;
begin
  if TInterlocked.CompareExchange(fIsShuttingDown, 1, 0) <> 0 then
    Exit;

  fNamespacesLock.BeginRead;
  try
    for lBucketPair in fNamespaces do
    begin
      lBucket := lBucketPair.Value;
      lBucket.Lock.BeginRead;
      try
        for lEntryPair in lBucket.Items do
        begin
          lEntry := lEntryPair.Value;
          TMonitor.Enter(lEntry.EntrySync);
          try
            lEntry.WasInvalidated := 1;
            lEntry.State := TMaxCacheEntryState.Obsolete;
            TMonitor.PulseAll(lEntry.EntrySync);
          finally
            TMonitor.Exit(lEntry.EntrySync);
          end;
        end;
      finally
        lBucket.Lock.EndRead;
      end;
    end;
  finally
    fNamespacesLock.EndRead;
  end;
end;

procedure TMaxCache.SweeperTimer(aSender: TObject);
begin
  if TInterlocked.CompareExchange(fSweepRunning, 1, 0) <> 0 then
    Exit;
  try
    SweepTick;
  finally
    TInterlocked.Exchange(fSweepRunning, 0);
  end;
end;

procedure TMaxCache.SweepTick;
var
  lNow: Int64;
  lBucketPairs: TArray<TPair<string, TMaxNamespaceBucket>>;
  lBucketPair: TPair<string, TMaxNamespaceBucket>;
  lRemaining: Integer;
  lGlobalRemaining: Integer;
  lIdx: Integer;
begin
  if TInterlocked.CompareExchange(fIsShuttingDown, 0, 0) <> 0 then
    Exit;

  lNow := NowMs;
  lRemaining := fConfig.SweepBatchSize;
  if lRemaining <= 0 then
    Exit;

  lGlobalRemaining := 0;
  if (fConfig.MaxEntriesGlobal > 0) or (fConfig.MaxBytesGlobal > 0) then
  begin
    lGlobalRemaining := lRemaining div 4;
    if lGlobalRemaining < 1 then
      lGlobalRemaining := 1;
    if lGlobalRemaining > lRemaining then
      lGlobalRemaining := lRemaining;
    Dec(lRemaining, lGlobalRemaining);
  end;

  fNamespacesLock.BeginRead;
  try
    SetLength(lBucketPairs, fNamespaces.Count);
    lIdx := 0;
    for lBucketPair in fNamespaces do
    begin
      lBucketPairs[lIdx] := lBucketPair;
      Inc(lIdx);
    end;
  finally
    fNamespacesLock.EndRead;
  end;

  for lBucketPair in lBucketPairs do
  begin
    if lRemaining <= 0 then
      Break;
    SweepBucket(lBucketPair.Key, lBucketPair.Value, lNow, lRemaining);
  end;

  if lGlobalRemaining > 0 then
  begin
    Inc(lGlobalRemaining, lRemaining);
    EnforceGlobalSizeCaps(lBucketPairs, lGlobalRemaining);
  end;
end;

procedure TMaxCache.SweepBucket(const aNamespace: string; const aBucket: TMaxNamespaceBucket; const aNowMs: Int64; var aRemaining: Integer);
type
  TEvictCandidate = record
    Key: string;
    Kind: Integer; // 0=obsolete cleanup, 1=ttl, 2=idle
  end;
var
  lCandidates: TArray<TEvictCandidate>;
  lCandidate: TEvictCandidate;
  lKey: string;
  lEntry: TMaxCacheEntry;
  lEvictKind: Integer;
  lTags: TArray<string>;
begin
  if aRemaining <= 0 then
    Exit;

  lCandidates := [];
  aBucket.Lock.BeginRead;
  try
    for lKey in aBucket.Items.Keys do
    begin
      if aRemaining <= 0 then
        Break;
      Dec(aRemaining);

      lEntry := aBucket.Items[lKey];
      if lEntry.State = TMaxCacheEntryState.Loading then
        Continue;

      if lEntry.State = TMaxCacheEntryState.Obsolete then
      begin
        SetLength(lCandidates, Length(lCandidates) + 1);
        lCandidates[High(lCandidates)].Key := lKey;
        lCandidates[High(lCandidates)].Kind := 0;
        Continue;
      end;

      if IsExpired(lEntry, aNowMs, lEvictKind) then
      begin
        SetLength(lCandidates, Length(lCandidates) + 1);
        lCandidates[High(lCandidates)].Key := lKey;
        lCandidates[High(lCandidates)].Kind := lEvictKind;
      end;
    end;
  finally
    aBucket.Lock.EndRead;
  end;

  for lCandidate in lCandidates do
  begin
    lEntry := nil;
    lTags := [];

    aBucket.Lock.BeginWrite;
    try
      if aBucket.Items.TryGetValue(lCandidate.Key, lEntry) then
      begin
        if lEntry.State = TMaxCacheEntryState.Loading then
        begin
          lEntry := nil;
          Continue;
        end;
        aBucket.Items.Remove(lCandidate.Key);
      end;
    finally
      aBucket.Lock.EndWrite;
    end;

    if lEntry = nil then
      Continue;
    try
      TMonitor.Enter(lEntry.EntrySync);
      try
        lTags := lEntry.Tags;
        lEntry.WasInvalidated := 1;
        lEntry.State := TMaxCacheEntryState.Obsolete;
        TMonitor.PulseAll(lEntry.EntrySync);
      finally
        TMonitor.Exit(lEntry.EntrySync);
      end;
    finally
      if Length(lTags) > 0 then
        UnregisterTags(aNamespace, lCandidate.Key, lTags);
      lEntry.Release; // drop dictionary ref
    end;

    case lCandidate.Kind of
      1: TInterlocked.Increment(fEvictionsTtl);
      2: TInterlocked.Increment(fEvictionsIdle);
    end;
  end;

  EnforceSizeCaps(aNamespace, aBucket, aNowMs, aRemaining);
end;

procedure TMaxCache.EnforceSizeCaps(const aNamespace: string; const aBucket: TMaxNamespaceBucket; const aNowMs: Int64; var aRemaining: Integer);
type
  TEntryInfo = record
    Key: string;
    LastAccessMs: Int64;
    SizeBytes: Int64;
  end;
var
  lMaxEntries: Integer;
  lMaxBytes: Int64;
  lInfos: TArray<TEntryInfo>;
  lTotalBytes: Int64;
  lNeedEvictCount: Integer;
  lNeedEvictBytes: Int64;
  lKey: string;
  lEntry: TMaxCacheEntry;
  lIdx: Integer;
  lToEvict: Integer;
  lEvictKeys: TArray<string>;
  lTags: TArray<string>;
begin
  if aRemaining <= 0 then
    Exit;

  lMaxEntries := fConfig.MaxEntriesPerNamespace;
  lMaxBytes := fConfig.MaxBytesPerNamespace;
  if (lMaxEntries <= 0) and (lMaxBytes <= 0) then
    Exit;

  aBucket.Lock.BeginRead;
  try
    SetLength(lInfos, aBucket.Items.Count);
    lIdx := 0;
    lTotalBytes := 0;
    for lKey in aBucket.Items.Keys do
    begin
      lEntry := aBucket.Items[lKey];
      if lEntry.State = TMaxCacheEntryState.Loading then
        Continue;
      lInfos[lIdx].Key := lKey;
      lInfos[lIdx].LastAccessMs := lEntry.LastAccessMs;
      lInfos[lIdx].SizeBytes := lEntry.SizeEstimateBytes;
      Inc(lTotalBytes, lEntry.SizeEstimateBytes);
      Inc(lIdx);
    end;
    if lIdx <> Length(lInfos) then
      SetLength(lInfos, lIdx);
  finally
    aBucket.Lock.EndRead;
  end;

  lNeedEvictCount := 0;
  if (lMaxEntries > 0) and (Length(lInfos) > lMaxEntries) then
    lNeedEvictCount := Length(lInfos) - lMaxEntries;

  lNeedEvictBytes := 0;
  if (lMaxBytes > 0) and (lTotalBytes > lMaxBytes) then
    lNeedEvictBytes := lTotalBytes - lMaxBytes;

  if (lNeedEvictCount <= 0) and (lNeedEvictBytes <= 0) then
    Exit;

  TArray.Sort<TEntryInfo>(lInfos,
    TComparer<TEntryInfo>.Construct(
      function(const aLeft, aRight: TEntryInfo): Integer
      begin
        if aLeft.LastAccessMs < aRight.LastAccessMs then
          Exit(-1);
        if aLeft.LastAccessMs > aRight.LastAccessMs then
          Exit(1);
        Result := 0;
      end));

  lToEvict := lNeedEvictCount;
  if lToEvict <= 0 then
    lToEvict := 0;

  if lNeedEvictBytes > 0 then
  begin
    lIdx := 0;
    while (lIdx < Length(lInfos)) and (lNeedEvictBytes > 0) do
    begin
      Dec(lNeedEvictBytes, lInfos[lIdx].SizeBytes);
      Inc(lIdx);
    end;
    if lIdx > lToEvict then
      lToEvict := lIdx;
  end;

  if lToEvict <= 0 then
    Exit;

  if lToEvict > aRemaining then
    lToEvict := aRemaining;

  SetLength(lEvictKeys, lToEvict);
  for lIdx := 0 to lToEvict - 1 do
    lEvictKeys[lIdx] := lInfos[lIdx].Key;

  for lKey in lEvictKeys do
  begin
    if aRemaining <= 0 then
      Break;
    Dec(aRemaining);

    lEntry := nil;
    lTags := [];
    aBucket.Lock.BeginWrite;
    try
      if aBucket.Items.TryGetValue(lKey, lEntry) then
      begin
        if lEntry.State = TMaxCacheEntryState.Loading then
        begin
          lEntry := nil;
          Continue;
        end;
        aBucket.Items.Remove(lKey);
      end;
    finally
      aBucket.Lock.EndWrite;
    end;

    if lEntry = nil then
      Continue;

    try
      TMonitor.Enter(lEntry.EntrySync);
      try
        lTags := lEntry.Tags;
        lEntry.WasInvalidated := 1;
        lEntry.State := TMaxCacheEntryState.Obsolete;
        TMonitor.PulseAll(lEntry.EntrySync);
      finally
        TMonitor.Exit(lEntry.EntrySync);
      end;
    finally
      if Length(lTags) > 0 then
        UnregisterTags(aNamespace, lKey, lTags);
      lEntry.Release; // drop dictionary ref
    end;

    TInterlocked.Increment(fEvictionsSize);
  end;
end;

procedure TMaxCache.EnforceGlobalSizeCaps(const aBucketPairs: TArray<TPair<string, TMaxNamespaceBucket>>; var aRemaining: Integer);
type
  TEntryInfo = record
    Namespace: string;
    Key: string;
    LastAccessMs: Int64;
    SizeBytes: Int64;
  end;
var
  lMaxEntries: Integer;
  lMaxBytes: Int64;
  lInfos: TArray<TEntryInfo>;
  lBucketPair: TPair<string, TMaxNamespaceBucket>;
  lBucket: TMaxNamespaceBucket;
  lNs: string;
  lKey: string;
  lEntry: TMaxCacheEntry;
  lTotalBytes: Int64;
  lTotalEntries: Integer;
  lNeedEvictCount: Integer;
  lNeedEvictBytes: Int64;
  lIdx: Integer;
  lToEvict: Integer;
begin
  if aRemaining <= 0 then
    Exit;

  lMaxEntries := fConfig.MaxEntriesGlobal;
  lMaxBytes := fConfig.MaxBytesGlobal;
  if (lMaxEntries <= 0) and (lMaxBytes <= 0) then
    Exit;

  lInfos := [];
  lTotalBytes := 0;
  lTotalEntries := 0;

  for lBucketPair in aBucketPairs do
  begin
    lNs := lBucketPair.Key;
    lBucket := lBucketPair.Value;

    lBucket.Lock.BeginRead;
    try
      for lKey in lBucket.Items.Keys do
      begin
        lEntry := lBucket.Items[lKey];
        if lEntry.State = TMaxCacheEntryState.Loading then
          Continue;

        Inc(lTotalEntries);
        Inc(lTotalBytes, lEntry.SizeEstimateBytes);

        SetLength(lInfos, Length(lInfos) + 1);
        lInfos[High(lInfos)].Namespace := lNs;
        lInfos[High(lInfos)].Key := lKey;
        lInfos[High(lInfos)].LastAccessMs := lEntry.LastAccessMs;
        lInfos[High(lInfos)].SizeBytes := lEntry.SizeEstimateBytes;
      end;
    finally
      lBucket.Lock.EndRead;
    end;
  end;

  lNeedEvictCount := 0;
  if (lMaxEntries > 0) and (lTotalEntries > lMaxEntries) then
    lNeedEvictCount := lTotalEntries - lMaxEntries;

  lNeedEvictBytes := 0;
  if (lMaxBytes > 0) and (lTotalBytes > lMaxBytes) then
    lNeedEvictBytes := lTotalBytes - lMaxBytes;

  if (lNeedEvictCount <= 0) and (lNeedEvictBytes <= 0) then
    Exit;

  TArray.Sort<TEntryInfo>(lInfos,
    TComparer<TEntryInfo>.Construct(
      function(const aLeft, aRight: TEntryInfo): Integer
      begin
        if aLeft.LastAccessMs < aRight.LastAccessMs then
          Exit(-1);
        if aLeft.LastAccessMs > aRight.LastAccessMs then
          Exit(1);
        Result := 0;
      end));

  lToEvict := lNeedEvictCount;
  if lToEvict < 0 then
    lToEvict := 0;

  if lNeedEvictBytes > 0 then
  begin
    lIdx := 0;
    while (lIdx < Length(lInfos)) and (lNeedEvictBytes > 0) do
    begin
      Dec(lNeedEvictBytes, lInfos[lIdx].SizeBytes);
      Inc(lIdx);
    end;
    if lIdx > lToEvict then
      lToEvict := lIdx;
  end;

  if lToEvict <= 0 then
    Exit;

  if lToEvict > aRemaining then
    lToEvict := aRemaining;

  for lIdx := 0 to lToEvict - 1 do
  begin
    if aRemaining <= 0 then
      Break;
    Dec(aRemaining);
    if InvalidateInternal(lInfos[lIdx].Namespace, lInfos[lIdx].Key, False) then
      TInterlocked.Increment(fEvictionsSize);
  end;
end;

end.
