unit MaxLogic.Cache.Tests;

{$I fpc_delphimode.inc}

interface

uses
  System.Classes, System.IOUtils, System.SyncObjs, System.SysUtils,
  DUnitX.TestFramework,
  MaxLogic.Cache;

type
  ITestValue = interface
    ['{66D06364-0A51-4DF9-8A0A-2C0A27CE9052}']
    function GetId: Integer;
    function GetText: string;
    property Id: Integer read GetId;
    property Text: string read GetText;
  end;

  TTestValue = class(TInterfacedObject, ITestValue)
  private
    fId: Integer;
    fText: string;
  public
    constructor Create(const aId: Integer; const aText: string = '');
    function GetId: Integer;
    function GetText: string;
  end;

  TFakeDependency = class(TInterfacedObject, IMaxCacheDependency)
  private
    fCallCount: Integer;
    fDelayMs: Integer;
    fIsStale: Boolean;
    fStamp: TMaxDependencyStamp;
  public
    constructor Create(const aStamp: TMaxDependencyStamp);
    procedure SetStale(const aValue: Boolean);
    procedure SetDelayMs(const aValue: Integer);
    function CallCount: Integer;
    function GetStamp: TMaxDependencyStamp;
    function IsStale(out aNewStamp: TMaxDependencyStamp): Boolean;
  end;

  [TestFixture]
  TMaxCacheTests = class
  private
    function NewConfigNoSweep: TMaxCacheConfig;
  public
    [Test] procedure SingleFlight_SameKey_OneLoader;
    [Test] procedure SingleFlight_DifferentNamespaces_NoGlobalLock;
    [Test] procedure FailureCaching_NegativeCacheWindow;
    [Test] procedure Waiting_TimesOut;
    [Test] procedure Invalidate_DuringLoading_IsSafe;
    [Test] procedure TagInvalidation_ScopedTag;
    [Test] procedure TagInvalidation_NoGhostAfterConcurrentInvalidate;
    [Test] procedure ScopedTag_RejectsPreScoped;
    [Test] procedure DependencyValidation_StampedeProtected;
    [Test] procedure FileDependency_HashDetectsTimestampPreservedChange;
    [Test] procedure Sweeper_TtlEvicts;
    [Test] procedure Sweeper_SizeCapEvictsOldest;
    [Test] procedure OptionalApis_TryGetDoesNotLoad;
    [Test] procedure Shutdown_FailFastAndWakesWaiters;
    [Test] procedure ReturnStaleOnFailure_ReturnsPreviousSnapshot;
  end;

implementation

{ TTestValue }

constructor TTestValue.Create(const aId: Integer; const aText: string);
begin
  inherited Create;
  fId := aId;
  fText := aText;
end;

function TTestValue.GetId: Integer;
begin
  Result := fId;
end;

function TTestValue.GetText: string;
begin
  Result := fText;
end;

{ TFakeDependency }

constructor TFakeDependency.Create(const aStamp: TMaxDependencyStamp);
begin
  inherited Create;
  fCallCount := 0;
  fDelayMs := 0;
  fIsStale := False;
  fStamp := aStamp;
end;

procedure TFakeDependency.SetStale(const aValue: Boolean);
begin
  fIsStale := aValue;
end;

procedure TFakeDependency.SetDelayMs(const aValue: Integer);
begin
  if aValue < 0 then
    fDelayMs := 0
  else
    fDelayMs := aValue;
end;

function TFakeDependency.CallCount: Integer;
begin
  Result := TInterlocked.CompareExchange(fCallCount, 0, 0);
end;

function TFakeDependency.GetStamp: TMaxDependencyStamp;
begin
  Result := fStamp;
end;

function TFakeDependency.IsStale(out aNewStamp: TMaxDependencyStamp): Boolean;
begin
  TInterlocked.Increment(fCallCount);
  if fDelayMs > 0 then
    Sleep(fDelayMs);
  aNewStamp := fStamp;
  if fIsStale then
    aNewStamp.MTimeUtcMs := aNewStamp.MTimeUtcMs + 1;
  Result := fIsStale;
end;

{ TMaxCacheTests }

function TMaxCacheTests.NewConfigNoSweep: TMaxCacheConfig;
begin
  Result := TMaxCacheConfig.Default;
  Result.SweepIntervalMs := 0;
  Result.SweepBatchSize := 0;
end;

procedure TMaxCacheTests.SingleFlight_SameKey_OneLoader;
const
  cThreads = 50;
  cTimeoutMs = 6000;
var
  lCache: IMaxCache;
  lConfig: TMaxCacheConfig;
  lStart: TEvent;
  lDone: TEvent;
  lLock: TCriticalSection;
  lResults: TInterfaceList;
  lErrors: TStringList;
  lDoneCount: Integer;
  lLoadCount: Integer;
  lThreads: array[0..cThreads - 1] of TThread;
  i: Integer;
  lFirst: ITestValue;
begin
  lConfig := NewConfigNoSweep;
  lConfig.DefaultWaitTimeoutMs := 3000;
  lCache := TMaxCache.New(lConfig);

  lStart := TEvent.Create(nil, True, False, '');
  lDone := TEvent.Create(nil, True, False, '');
  lLock := TCriticalSection.Create;
  lResults := TInterfaceList.Create;
  lErrors := TStringList.Create;
  try
    lDoneCount := 0;
    lLoadCount := 0;
    for i := Low(lThreads) to High(lThreads) do
    begin
      lThreads[i] := TThread.CreateAnonymousThread(
        procedure
        var
          lValue: ITestValue;
          lErr: string;
        begin
          lValue := nil;
          lErr := '';
          if lStart.WaitFor(cTimeoutMs) <> wrSignaled then
          begin
            lErr := 'start timeout';
          end else
          begin
            try
              lValue := lCache.GetOrCreate('ns', 'k',
                function: IInterface
                var
                  lCall: Integer;
                begin
                  lCall := TInterlocked.Increment(lLoadCount);
                  Sleep(50);
                  Result := TTestValue.Create(lCall, 'v') as ITestValue;
                end) as ITestValue;
            except
              on E: Exception do
                lErr := E.ClassName + ': ' + E.Message;
            end;
          end;

          lLock.Acquire;
          try
            if lErr <> '' then
              lErrors.Add(lErr)
            else
              lResults.Add(lValue);
          finally
            lLock.Release;
          end;

          if TInterlocked.Increment(lDoneCount) = cThreads then
            lDone.SetEvent;
        end);
      lThreads[i].FreeOnTerminate := False;
      lThreads[i].Start;
    end;

    lStart.SetEvent;
    Assert.AreEqual(wrSignaled, lDone.WaitFor(cTimeoutMs), 'threads did not finish in time');

    for i := Low(lThreads) to High(lThreads) do
    begin
      lThreads[i].WaitFor;
      lThreads[i].Free;
      lThreads[i] := nil;
    end;

    Assert.AreEqual(0, lErrors.Count, 'thread raised: ' + lErrors.Text);
    Assert.AreEqual(cThreads, lResults.Count, 'unexpected result count');

    Assert.AreEqual(1, lLoadCount, 'loader should run once');
    lFirst := lResults[0] as ITestValue;
    Assert.IsTrue(lFirst <> nil);
    for i := 0 to lResults.Count - 1 do
      Assert.IsTrue((lResults[i] as ITestValue) = lFirst, 'results should be the same snapshot instance');
  finally
    lErrors.Free;
    lResults.Free;
    lLock.Free;
    lDone.Free;
    lStart.Free;
  end;
end;

procedure TMaxCacheTests.SingleFlight_DifferentNamespaces_NoGlobalLock;
const
  cTimeoutMs = 4000;
var
  lCache: IMaxCache;
  lConfig: TMaxCacheConfig;
  lEnteredA: TEvent;
  lEnteredB: TEvent;
  lDone: TEvent;
  lDoneCount: Integer;
  lErrors: array[0..1] of string;
  lThreads: array[0..1] of TThread;
  i: Integer;
begin
  lConfig := NewConfigNoSweep;
  lConfig.DefaultWaitTimeoutMs := 3000;
  lCache := TMaxCache.New(lConfig);

  lEnteredA := TEvent.Create(nil, True, False, '');
  lEnteredB := TEvent.Create(nil, True, False, '');
  lDone := TEvent.Create(nil, True, False, '');
  try
    lDoneCount := 0;
    lErrors[0] := '';
    lErrors[1] := '';

    lThreads[0] := TThread.CreateAnonymousThread(
      procedure
      begin
        try
          lCache.GetOrCreate('A', 'k',
            function: IInterface
            begin
              lEnteredA.SetEvent;
              if lEnteredB.WaitFor(500) <> wrSignaled then
                raise Exception.Create('B loader did not start');
              Result := TTestValue.Create(1, 'A') as ITestValue;
            end);
        except
          on E: Exception do
            lErrors[0] := E.ClassName + ': ' + E.Message;
        end;
        if TInterlocked.Increment(lDoneCount) = 2 then
          lDone.SetEvent;
      end);

    lThreads[1] := TThread.CreateAnonymousThread(
      procedure
      begin
        try
          lCache.GetOrCreate('B', 'k',
            function: IInterface
            begin
              lEnteredB.SetEvent;
              if lEnteredA.WaitFor(500) <> wrSignaled then
                raise Exception.Create('A loader did not start');
              Result := TTestValue.Create(1, 'B') as ITestValue;
            end);
        except
          on E: Exception do
            lErrors[1] := E.ClassName + ': ' + E.Message;
        end;
        if TInterlocked.Increment(lDoneCount) = 2 then
          lDone.SetEvent;
      end);

    for i := 0 to 1 do
    begin
      lThreads[i].FreeOnTerminate := False;
      lThreads[i].Start;
    end;

    Assert.AreEqual(wrSignaled, lDone.WaitFor(cTimeoutMs), 'threads did not finish in time');
    for i := 0 to 1 do
    begin
      lThreads[i].WaitFor;
      lThreads[i].Free;
      lThreads[i] := nil;
      Assert.AreEqual('', lErrors[i], 'thread raised: ' + lErrors[i]);
    end;
  finally
    lDone.Free;
    lEnteredB.Free;
    lEnteredA.Free;
  end;
end;

procedure TMaxCacheTests.FailureCaching_NegativeCacheWindow;
var
  lCache: IMaxCache;
  lConfig: TMaxCacheConfig;
  lOptions: TMaxCacheOptions;
  lCalls: Integer;
begin
  lConfig := NewConfigNoSweep;
  lConfig.MinFailCacheMs := 80;
  lConfig.DefaultWaitTimeoutMs := 1000;
  lCache := TMaxCache.New(lConfig);

  lCalls := 0;
  lOptions := TMaxCacheOptions.Create;
  try
    lOptions.FailCacheMs := 0;

    Assert.WillRaise(
      procedure
      begin
        lCache.GetOrCreate('ns', 'fail',
          function: IInterface
          begin
            TInterlocked.Increment(lCalls);
            raise Exception.Create('boom');
          end, lOptions);
      end, EMaxCacheLoadException);

    Assert.WillRaise(
      procedure
      begin
        lCache.GetOrCreate('ns', 'fail',
          function: IInterface
          begin
            TInterlocked.Increment(lCalls);
            raise Exception.Create('boom');
          end, lOptions);
      end, EMaxCacheLoadException);

    Assert.AreEqual(1, lCalls, 'loader should be suppressed inside fail-cache window');

    Sleep(120);

    Assert.WillRaise(
      procedure
      begin
        lCache.GetOrCreate('ns', 'fail',
          function: IInterface
          begin
            TInterlocked.Increment(lCalls);
            raise Exception.Create('boom');
          end, lOptions);
      end, EMaxCacheLoadException);

    Assert.AreEqual(2, lCalls, 'loader should retry after fail-cache window');
  finally
    lOptions.Free;
  end;
end;

procedure TMaxCacheTests.Waiting_TimesOut;
var
  lCache: IMaxCache;
  lConfig: TMaxCacheConfig;
  lRelease: TEvent;
  lLoaderStarted: TEvent;
  lThreadError: string;
  lThread: TThread;
begin
  lConfig := NewConfigNoSweep;
  lConfig.DefaultWaitTimeoutMs := 120;
  lCache := TMaxCache.New(lConfig);

  lRelease := TEvent.Create(nil, True, False, '');
  lLoaderStarted := TEvent.Create(nil, True, False, '');
  lThread := nil;
  lThreadError := '';
  try
    lThread := TThread.CreateAnonymousThread(
      procedure
      begin
        try
          lCache.GetOrCreate('ns', 'slow',
            function: IInterface
            begin
              lLoaderStarted.SetEvent;
              lRelease.WaitFor(2000);
              Result := TTestValue.Create(1, 'ok') as ITestValue;
            end);
        except
          on E: Exception do
            lThreadError := E.ClassName + ': ' + E.Message;
        end;
      end);
    lThread.FreeOnTerminate := False;
    lThread.Start;

    Assert.AreEqual(wrSignaled, lLoaderStarted.WaitFor(1000), 'loader did not start');

    Assert.WillRaise(
      procedure
      begin
        lCache.GetOrCreate('ns', 'slow',
          function: IInterface
          begin
            Result := TTestValue.Create(2, 'should not run') as ITestValue;
          end);
      end, EMaxCacheWaitTimeout);
  finally
    lRelease.SetEvent;
    if lThread <> nil then
    begin
      lThread.WaitFor;
      lThread.Free;
    end;
    Assert.AreEqual('', lThreadError, 'loader thread should not raise');
    lLoaderStarted.Free;
    lRelease.Free;
  end;
end;

procedure TMaxCacheTests.Invalidate_DuringLoading_IsSafe;
const
  cTimeoutMs = 6000;
var
  lCache: IMaxCache;
  lConfig: TMaxCacheConfig;
  lFirstStarted: TEvent;
  lFirstRelease: TEvent;
  lLoadCount: Integer;
  lResA: ITestValue;
  lResB: ITestValue;
  lErrA: string;
  lErrB: string;
  lDone: TEvent;
  lDoneCount: Integer;
  lLoader: TFunc<IInterface>;
  lThreadA: TThread;
  lThreadB: TThread;
begin
  lConfig := NewConfigNoSweep;
  lConfig.DefaultWaitTimeoutMs := 3000;
  lCache := TMaxCache.New(lConfig);

  lFirstStarted := TEvent.Create(nil, True, False, '');
  lFirstRelease := TEvent.Create(nil, True, False, '');
  lDone := TEvent.Create(nil, True, False, '');
  lThreadA := nil;
  lThreadB := nil;
  try
    lLoadCount := 0;
    lDoneCount := 0;
    lErrA := '';
    lErrB := '';
    lResA := nil;
    lResB := nil;

    lLoader :=
      function: IInterface
      var
        lCall: Integer;
      begin
        lCall := TInterlocked.Increment(lLoadCount);
        if lCall = 1 then
        begin
          lFirstStarted.SetEvent;
          lFirstRelease.WaitFor(2000);
        end;
        Result := TTestValue.Create(lCall, 'v') as ITestValue;
      end;

    lThreadA := TThread.CreateAnonymousThread(
      procedure
      begin
        try
          lResA := lCache.GetOrCreate('ns', 'k', lLoader) as ITestValue;
        except
          on E: Exception do
            lErrA := E.ClassName + ': ' + E.Message;
        end;
        if TInterlocked.Increment(lDoneCount) = 2 then
          lDone.SetEvent;
      end);
    lThreadA.FreeOnTerminate := False;
    lThreadA.Start;

    Assert.AreEqual(wrSignaled, lFirstStarted.WaitFor(1000), 'first loader did not start');

    lThreadB := TThread.CreateAnonymousThread(
      procedure
      begin
        try
          lResB := lCache.GetOrCreate('ns', 'k', lLoader) as ITestValue;
        except
          on E: Exception do
            lErrB := E.ClassName + ': ' + E.Message;
        end;
        if TInterlocked.Increment(lDoneCount) = 2 then
          lDone.SetEvent;
      end);
    lThreadB.FreeOnTerminate := False;
    lThreadB.Start;

    lCache.Invalidate('ns', 'k');
    lFirstRelease.SetEvent;

    Assert.AreEqual(wrSignaled, lDone.WaitFor(cTimeoutMs), 'threads did not finish');
    lThreadA.WaitFor;
    lThreadB.WaitFor;

    Assert.AreEqual('', lErrA, 'A raised: ' + lErrA);
    Assert.AreEqual('', lErrB, 'B raised: ' + lErrB);
    Assert.IsTrue(lResA <> nil);
    Assert.IsTrue(lResB <> nil);
    Assert.AreEqual(2, lLoadCount, 'should perform abandoned load + fresh load');
    Assert.AreEqual(2, lResA.Id, 'A should return the fresh snapshot');
    Assert.AreEqual(2, lResB.Id, 'B should return the fresh snapshot');
  finally
    lFirstRelease.SetEvent;
    if lThreadB <> nil then
    begin
      lThreadB.WaitFor;
      lThreadB.Free;
    end;
    if lThreadA <> nil then
    begin
      lThreadA.WaitFor;
      lThreadA.Free;
    end;
    lLoader := nil; // break anonymous-method self-reference cycle (ActRec -> lLoader -> ActRec)
    lResA := nil;
    lResB := nil;
    lCache := nil;
    lDone.Free;
    lFirstRelease.Free;
    lFirstStarted.Free;
  end;
end;

procedure TMaxCacheTests.TagInvalidation_ScopedTag;
var
  lCache: IMaxCache;
  lConfig: TMaxCacheConfig;
  lOptions: TMaxCacheOptions;
  lCallsA: Integer;
  lCallsB: Integer;
  lA1: ITestValue;
  lB1: ITestValue;
  lA2: ITestValue;
  lB2: ITestValue;
begin
  lConfig := NewConfigNoSweep;
  lCache := TMaxCache.New(lConfig);

  lCallsA := 0;
  lCallsB := 0;

    lOptions := TMaxCacheOptions.Create;
  try
    lOptions.Tags := ['group:1'];

    lA1 := lCache.GetOrCreate('ns1', 'a',
      function: IInterface
      begin
        Result := TTestValue.Create(TInterlocked.Increment(lCallsA), 'a') as ITestValue;
      end, lOptions) as ITestValue;

    lB1 := lCache.GetOrCreate('ns1', 'b',
      function: IInterface
      begin
        Result := TTestValue.Create(TInterlocked.Increment(lCallsB), 'b') as ITestValue;
      end, lOptions) as ITestValue;

    lCache.InvalidateByTag(TMaxCache.ScopedTag('ns1', 'group:1'));

    lA2 := lCache.GetOrCreate('ns1', 'a',
      function: IInterface
      begin
        Result := TTestValue.Create(TInterlocked.Increment(lCallsA), 'a') as ITestValue;
      end, lOptions) as ITestValue;

    lB2 := lCache.GetOrCreate('ns1', 'b',
      function: IInterface
      begin
        Result := TTestValue.Create(TInterlocked.Increment(lCallsB), 'b') as ITestValue;
      end, lOptions) as ITestValue;

    Assert.AreEqual(2, lCallsA);
    Assert.AreEqual(2, lCallsB);
    Assert.IsFalse(lA1 = lA2, 'A should reload');
    Assert.IsFalse(lB1 = lB2, 'B should reload');
  finally
    lOptions.Free;
  end;
end;

procedure TMaxCacheTests.TagInvalidation_NoGhostAfterConcurrentInvalidate;
const
  cTimeoutMs = 4000;
var
  lCache: IMaxCache;
  lConfig: TMaxCacheConfig;
  lOptionsA: TMaxCacheOptions;
  lOptionsB: TMaxCacheOptions;
  lHookEntered: TEvent;
  lHookContinue: TEvent;
  lThread: TThread;
  lErr: string;
  lLoadCount: Integer;
begin
  lConfig := NewConfigNoSweep;
  lCache := TMaxCache.New(lConfig);

  lOptionsA := TMaxCacheOptions.Create;
  lOptionsB := TMaxCacheOptions.Create;
  lOptionsA.Tags := ['group:A'];
  lOptionsB.Tags := ['group:B'];

  lHookEntered := TEvent.Create(nil, True, False, '');
  lHookContinue := TEvent.Create(nil, True, False, '');
  lThread := nil;
  lErr := '';
  lLoadCount := 0;
  try
    MaxCache_SetTagRegisterHook(
      procedure(const aNamespace, aKey: string; const aTags: TArray<string>)
      begin
        if (aNamespace = 'ns') and (aKey = 'k') and (Length(aTags) > 0) and (aTags[0] = 'group:A') then
        begin
          lHookEntered.SetEvent;
          lHookContinue.WaitFor(cTimeoutMs);
        end;
      end);

    lThread := TThread.CreateAnonymousThread(
      procedure
      begin
        try
          lCache.GetOrCreate('ns', 'k',
            function: IInterface
            begin
              TInterlocked.Increment(lLoadCount);
              Result := TTestValue.Create(1, 'A') as ITestValue;
            end, lOptionsA);
        except
          on E: Exception do
            lErr := E.ClassName + ': ' + E.Message;
        end;
      end);
    lThread.FreeOnTerminate := False;
    lThread.Start;

    Assert.AreEqual(wrSignaled, lHookEntered.WaitFor(cTimeoutMs), 'hook not entered');
    lCache.Invalidate('ns', 'k');
    lHookContinue.SetEvent;

    lThread.WaitFor;
    Assert.AreEqual('', lErr, 'loader thread should not raise');

    MaxCache_ClearTagRegisterHook;

    lCache.GetOrCreate('ns', 'k',
      function: IInterface
      begin
        TInterlocked.Increment(lLoadCount);
        Result := TTestValue.Create(2, 'B') as ITestValue;
      end, lOptionsB);

    lCache.InvalidateByTag(TMaxCache.ScopedTag('ns', 'group:A'));

    lCache.GetOrCreate('ns', 'k',
      function: IInterface
      begin
        TInterlocked.Increment(lLoadCount);
        Result := TTestValue.Create(3, 'should not load') as ITestValue;
      end);

    Assert.AreEqual(2, lLoadCount, 'tag A should not invalidate new entry');
  finally
    MaxCache_ClearTagRegisterHook;
    lHookContinue.SetEvent;
    if lThread <> nil then
    begin
      lThread.WaitFor;
      lThread.Free;
    end;
    lHookContinue.Free;
    lHookEntered.Free;
    lOptionsA.Free;
    lOptionsB.Free;
  end;
end;

procedure TMaxCacheTests.ScopedTag_RejectsPreScoped;
begin
  Assert.WillRaise(
    procedure
    begin
      TMaxCache.ScopedTag('ns1', 'other|tag');
    end, EMaxCacheException);
end;

procedure TMaxCacheTests.DependencyValidation_StampedeProtected;
const
  cThreads = 20;
  cTimeoutMs = 6000;
var
  lCache: IMaxCache;
  lConfig: TMaxCacheConfig;
  lOptions: TMaxCacheOptions;
  lDep: TFakeDependency;
  lStamp: TMaxDependencyStamp;
  lStart: TEvent;
  lDone: TEvent;
  lDoneCount: Integer;
  lThreads: array[0..cThreads - 1] of TThread;
  i: Integer;
begin
  lConfig := NewConfigNoSweep;
  lCache := TMaxCache.New(lConfig);

  lStamp.MTimeUtcMs := 1;
  lStamp.SizeBytes := 1;
  lStamp.Hash32 := 0;
  lStamp.HasHash := False;
  lDep := TFakeDependency.Create(lStamp);
  lDep.SetDelayMs(80);

    lOptions := TMaxCacheOptions.Create;
  try
    lOptions.Dependency := lDep;
    lOptions.ValidateIntervalMs := 0;

    lCache.GetOrCreate('ns', 'k',
      function: IInterface
      begin
        Result := TTestValue.Create(1, 'v') as ITestValue;
      end, lOptions);

    lStart := TEvent.Create(nil, True, False, '');
    lDone := TEvent.Create(nil, True, False, '');
    try
      lDoneCount := 0;
      for i := Low(lThreads) to High(lThreads) do
      begin
        lThreads[i] := TThread.CreateAnonymousThread(
          procedure
          begin
            lStart.WaitFor(cTimeoutMs);
            lCache.GetOrCreate('ns', 'k',
              function: IInterface
              begin
                Result := TTestValue.Create(2, 'should not run') as ITestValue;
              end, lOptions);
            if TInterlocked.Increment(lDoneCount) = cThreads then
              lDone.SetEvent;
          end);
        lThreads[i].FreeOnTerminate := False;
        lThreads[i].Start;
      end;

      lStart.SetEvent;
      Assert.AreEqual(wrSignaled, lDone.WaitFor(cTimeoutMs), 'threads did not finish');
    finally
      for i := Low(lThreads) to High(lThreads) do
      begin
        lThreads[i].WaitFor;
        lThreads[i].Free;
        lThreads[i] := nil;
      end;
      lDone.Free;
      lStart.Free;
    end;

    Assert.AreEqual(1, lDep.CallCount, 'only one dependency validation should run concurrently');
  finally
    lOptions.Free;
  end;
end;

procedure TMaxCacheTests.FileDependency_HashDetectsTimestampPreservedChange;
var
  lCache: IMaxCache;
  lConfig: TMaxCacheConfig;
  lOptions: TMaxCacheOptions;
  lFileName: string;
  lDt: TDateTime;
  lV1: ITestValue;
  lV2: ITestValue;
begin
  lConfig := NewConfigNoSweep;
  lCache := TMaxCache.New(lConfig);

  lFileName := TPath.GetTempFileName;
  try
    TFile.WriteAllText(lFileName, 'abc');
    lDt := TFile.GetLastWriteTimeUtc(lFileName);

    lOptions := TMaxCacheOptions.Create;
    try
      lOptions.Dependency := TMaxFileDependency.Create(lFileName, 51200, False);
      lOptions.ValidateIntervalMs := 0;

      lV1 := lCache.GetOrCreate('files', lFileName,
        function: IInterface
        begin
          Result := TTestValue.Create(1, TFile.ReadAllText(lFileName)) as ITestValue;
        end, lOptions) as ITestValue;

      TFile.WriteAllText(lFileName, 'xyz');
      TFile.SetLastWriteTimeUtc(lFileName, lDt); // preserve mtime

      lV2 := lCache.GetOrCreate('files', lFileName,
        function: IInterface
        begin
          Result := TTestValue.Create(2, TFile.ReadAllText(lFileName)) as ITestValue;
        end, lOptions) as ITestValue;

      Assert.AreEqual('abc', lV1.Text);
      Assert.AreEqual('xyz', lV2.Text);
      Assert.IsFalse(lV1 = lV2, 'should reload after hash-detected change');
    finally
      lOptions.Free;
    end;
  finally
    TFile.Delete(lFileName);
  end;
end;

procedure TMaxCacheTests.Sweeper_TtlEvicts;
var
  lConfig: TMaxCacheConfig;
  lCache: IMaxCache;
  lOptions: TMaxCacheOptions;
  lMetrics: TMaxCacheMetrics;
begin
  lConfig := TMaxCacheConfig.Default;
  lConfig.SweepIntervalMs := 20;
  lConfig.SweepBatchSize := 100;
  lCache := TMaxCache.New(lConfig);

  lOptions := TMaxCacheOptions.Create;
  try
    lOptions.TtlMs := 30;
    lCache.GetOrCreate('ns', 'ttl',
      function: IInterface
      begin
        Result := TTestValue.Create(1, 'v') as ITestValue;
      end, lOptions);
  finally
    lOptions.Free;
  end;

  Sleep(200);

  lMetrics := lCache.GetMetrics;
  Assert.AreEqual(0, lMetrics.EntryCount, 'TTL entry should be swept out');
  Assert.IsTrue(lMetrics.EvictionsTtl >= 1, 'TTL eviction counter should increment');
end;

procedure TMaxCacheTests.Sweeper_SizeCapEvictsOldest;
var
  lConfig: TMaxCacheConfig;
  lCache: IMaxCache;
  lOptions: TMaxCacheOptions;
  lTmp: ITestValue;
  lHit: ITestValue;
  lIntf: IInterface;
begin
  lConfig := TMaxCacheConfig.Default;
  lConfig.SweepIntervalMs := 20;
  lConfig.SweepBatchSize := 200;
  lConfig.MaxEntriesPerNamespace := 2;
  lCache := TMaxCache.New(lConfig);

  lOptions := TMaxCacheOptions.Create;
  try
    lOptions.SizeEstimateBytes := 10;

    lCache.GetOrCreate('ns', 'k1',
      function: IInterface
      begin
        Result := TTestValue.Create(1, 'k1') as ITestValue;
      end, lOptions);
    Sleep(10);
    lCache.GetOrCreate('ns', 'k2',
      function: IInterface
      begin
        Result := TTestValue.Create(1, 'k2') as ITestValue;
      end, lOptions);
    Sleep(10);
    lCache.GetOrCreate('ns', 'k3',
      function: IInterface
      begin
        Result := TTestValue.Create(1, 'k3') as ITestValue;
      end, lOptions);

    // keep k2 hot so k1 becomes the oldest
    lTmp := lCache.GetOrCreate('ns', 'k2',
      function: IInterface
      begin
        Result := TTestValue.Create(2, 'k2') as ITestValue;
      end, lOptions) as ITestValue;
    Assert.AreEqual('k2', lTmp.Text);
  finally
    lOptions.Free;
  end;

  Sleep(200);

  Assert.IsFalse(lCache.TryGet('ns', 'k1', lIntf), 'oldest key should be evicted');
  Assert.IsTrue(lCache.TryGet('ns', 'k2', lIntf), 'hot key should remain');
  lHit := lIntf as ITestValue;
  Assert.IsTrue(lCache.TryGet('ns', 'k3', lIntf), 'new key should remain');
  lHit := lIntf as ITestValue;
end;

procedure TMaxCacheTests.OptionalApis_TryGetDoesNotLoad;
var
  lCache: IMaxCache;
  lConfig: TMaxCacheConfig;
  lValue: IInterface;
begin
  lConfig := NewConfigNoSweep;
  lCache := TMaxCache.New(lConfig);

  Assert.IsFalse(lCache.TryGet('ns', 'missing', lValue));
  Assert.IsTrue(lValue = nil);
end;

procedure TMaxCacheTests.Shutdown_FailFastAndWakesWaiters;
const
  cTimeoutMs = 6000;
var
  lConfig: TMaxCacheConfig;
  lCache: IMaxCache;
  lRelease: TEvent;
  lStarted: TEvent;
  lLoaderErr: string;
  lWaiterErr: string;
  lDone: TEvent;
  lDoneCount: Integer;
  lLoader: TThread;
  lWaiter: TThread;
begin
  lConfig := NewConfigNoSweep;
  lConfig.DefaultWaitTimeoutMs := 2000;
  lCache := TMaxCache.New(lConfig);

  lRelease := TEvent.Create(nil, True, False, '');
  lStarted := TEvent.Create(nil, True, False, '');
  lDone := TEvent.Create(nil, True, False, '');
  lLoader := nil;
  lWaiter := nil;
  try
    lLoaderErr := '';
    lWaiterErr := '';
    lDoneCount := 0;

    lLoader := TThread.CreateAnonymousThread(
      procedure
      begin
        try
          lCache.GetOrCreate('ns', 'k',
            function: IInterface
            begin
              lStarted.SetEvent;
              lRelease.WaitFor(2000);
              Result := TTestValue.Create(1, 'v') as ITestValue;
            end);
        except
          on E: Exception do
            lLoaderErr := E.ClassName;
        end;
        if TInterlocked.Increment(lDoneCount) = 2 then
          lDone.SetEvent;
      end);
    lLoader.FreeOnTerminate := False;
    lLoader.Start;

    Assert.AreEqual(wrSignaled, lStarted.WaitFor(1000), 'loader did not start');

    lWaiter := TThread.CreateAnonymousThread(
      procedure
      begin
        try
          lCache.GetOrCreate('ns', 'k',
            function: IInterface
            begin
              Result := TTestValue.Create(2, 'should not run') as ITestValue;
            end);
        except
          on E: Exception do
            lWaiterErr := E.ClassName;
        end;
        if TInterlocked.Increment(lDoneCount) = 2 then
          lDone.SetEvent;
      end);
    lWaiter.FreeOnTerminate := False;
    lWaiter.Start;

    lCache.Shutdown;
    lRelease.SetEvent;

    Assert.AreEqual(wrSignaled, lDone.WaitFor(cTimeoutMs), 'threads did not finish');
    lLoader.WaitFor;
    lWaiter.WaitFor;

    Assert.AreEqual('EMaxCacheShutdown', lWaiterErr, 'waiter should fail fast on shutdown');

    try
      lCache.GetOrCreate('ns', 'after',
        function: IInterface
        begin
          Result := TTestValue.Create(3, 'nope') as ITestValue;
        end);
      Assert.Fail('Expected EMaxCacheShutdown after Shutdown');
    except
      on E: EMaxCacheShutdown do
        ; // expected
    end;
  finally
    lRelease.SetEvent;
    if lWaiter <> nil then
    begin
      lWaiter.WaitFor;
      lWaiter.Free;
    end;
    if lLoader <> nil then
    begin
      lLoader.WaitFor;
      lLoader.Free;
    end;
    lDone.Free;
    lStarted.Free;
    lRelease.Free;
  end;
end;

procedure TMaxCacheTests.ReturnStaleOnFailure_ReturnsPreviousSnapshot;
var
  lCache: IMaxCache;
  lConfig: TMaxCacheConfig;
  lOptions: TMaxCacheOptions;
  lValue1: ITestValue;
  lValue2: ITestValue;
begin
  lConfig := NewConfigNoSweep;
  lConfig.MinFailCacheMs := 80;
  lCache := TMaxCache.New(lConfig);

  lOptions := TMaxCacheOptions.Create;
  try
    lOptions.TtlMs := 40;
    lOptions.FailCacheMs := 0;
    lOptions.ReturnStaleOnFailure := True;

    lValue1 := lCache.GetOrCreate('ns', 'k',
      function: IInterface
      begin
        Result := TTestValue.Create(1, 'ok') as ITestValue;
      end, lOptions) as ITestValue;

    Sleep(80); // expire TTL

    lValue2 := lCache.GetOrCreate('ns', 'k',
      function: IInterface
      begin
        raise Exception.Create('reload failed');
      end, lOptions) as ITestValue;

    Assert.IsTrue(lValue2 = lValue1, 'should return stale snapshot on reload failure');
  finally
    lOptions.Free;
  end;
end;

end.
