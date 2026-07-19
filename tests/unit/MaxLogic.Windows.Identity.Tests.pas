unit MaxLogic.Windows.Identity.Tests;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TWindowsIdentityTests = class
  public
    [Test] procedure BootIdentity_ConcurrentCallersShareOneLookup;
    [Test] procedure BootIdentity_IsStrongStableAndCached;
    [Test] procedure BootIdentity_QueryReceivesBoundedCallerTimeout;
    [Test] procedure BootIdentity_SecondProcessAgrees;
    [Test] procedure BootIdentity_UnavailableSourceCannotBeCached;
    [Test] procedure BootIdentity_UnavailableIsCachedUntilExplicitRetry;
    [Test] procedure ProcessStart_CurrentProcessIsStableUtcMilliseconds;
    [Test] procedure ProcessStart_InvalidProcessIsUnavailable;
  end;

implementation

uses
  AutoFree,
  System.Classes,
  System.DateUtils,
  System.Generics.Collections,
  System.IOUtils,
  System.SyncObjs,
  System.SysUtils,
  Winapi.Windows,
  MaxLogic.Windows.Identity;

const
  cBootIdentityProbePrefix = '--probe-windows-boot-identity=';

type
  TConcurrentIdentityCallState = class
  public
    fAllReady: TEvent;
    fCallerGate: TEvent;
    fReadyCount: Integer;
    fResults: TArray<Boolean>;
    fValues: TArray<Int64>;
  end;

function CurrentUtcMilliseconds: Int64;
var
  lUtcNow: TDateTime;
begin
  lUtcNow := TTimeZone.Local.ToUniversalTime(Now);
  Result := DateTimeToUnix(lUtcNow, True) * 1000 + MilliSecondOf(lUtcNow);
end;

function CreateBootIdentityCaller(
  const aIndex: Integer;
  const aState: TConcurrentIdentityCallState): TThread;
begin
  Result := TThread.CreateAnonymousThread(
    procedure
    var
      lIdentity: TWindowsBootIdentity;
    begin
      if TInterlocked.Increment(aState.fReadyCount) =
        Length(aState.fResults) then
        aState.fAllReady.SetEvent;
      aState.fCallerGate.WaitFor(INFINITE);
      aState.fResults[aIndex] :=
        TryGetWindowsBootIdentity(1000, True, lIdentity);
      aState.fValues[aIndex] := lIdentity.UtcMilliseconds;
    end);
  Result.FreeOnTerminate := False;
end;

function ReadBootIdentityFromSecondProcess: Int64;
const
  cProcessTimeoutMilliseconds = 5000;
var
  lCommandLine: string;
  lExitCode: Cardinal;
  lIdentityText: string;
  lProcessInformation: TProcessInformation;
  lProbeFileName: string;
  lStartupInfo: TStartupInfo;
  lWaitResult: Cardinal;
begin
  lProbeFileName := TPath.GetTempFileName;
  TFile.Delete(lProbeFileName);
  try
    lCommandLine := Format('"%s" %s"%s"', [
      ParamStr(0),
      cBootIdentityProbePrefix,
      lProbeFileName]);
    lStartupInfo := Default(TStartupInfo);
    lStartupInfo.cb := SizeOf(lStartupInfo);
    lProcessInformation := Default(TProcessInformation);
    UniqueString(lCommandLine);

    Assert.IsTrue(
      CreateProcess(
        nil,
        PChar(lCommandLine),
        nil,
        nil,
        False,
        CREATE_NO_WINDOW,
        nil,
        PChar(ExtractFilePath(ParamStr(0))),
        lStartupInfo,
        lProcessInformation),
      'Could not start the second identity probe process');
    try
      lWaitResult := WaitForSingleObject(
        lProcessInformation.hProcess,
        cProcessTimeoutMilliseconds);
      Assert.AreEqual<Cardinal>(
        WAIT_OBJECT_0,
        lWaitResult,
        'The second identity probe process timed out');
      Assert.IsTrue(
        GetExitCodeProcess(lProcessInformation.hProcess, lExitCode),
        'Could not read the second identity probe exit code');
      Assert.AreEqual<Cardinal>(
        0,
        lExitCode,
        'The second identity probe could not resolve boot identity');
    finally
      CloseHandle(lProcessInformation.hThread);
      CloseHandle(lProcessInformation.hProcess);
    end;

    Assert.IsTrue(
      TFile.Exists(lProbeFileName),
      'The second identity probe did not write its result');
    lIdentityText := Trim(TFile.ReadAllText(lProbeFileName, TEncoding.UTF8));
    Assert.IsTrue(
      TryStrToInt64(lIdentityText, Result),
      'The second identity probe returned an invalid timestamp');
  finally
    if TFile.Exists(lProbeFileName) then
      TFile.Delete(lProbeFileName);
  end;
end;

procedure TWindowsIdentityTests.BootIdentity_ConcurrentCallersShareOneLookup;
const
  cCallerCount = 8;
  cExpectedIdentity = Int64(987654321);
  cTimeoutMilliseconds = 2000;
var
  g: TGarbos;
  i: Integer;
  lAllReady: TEvent;
  lCallerGate: TEvent;
  lQueryCount: Integer;
  lQueryEntered: TEvent;
  lQueryRelease: TEvent;
  lState: TConcurrentIdentityCallState;
  lThreads: TObjectList<TThread>;
begin
  GC(lAllReady, TEvent.Create(nil, True, False, ''), g);
  GC(lCallerGate, TEvent.Create(nil, True, False, ''), g);
  GC(lQueryEntered, TEvent.Create(nil, True, False, ''), g);
  GC(lQueryRelease, TEvent.Create(nil, True, False, ''), g);
  GC(lState, TConcurrentIdentityCallState.Create, g);
  GC(lThreads, TObjectList<TThread>.Create(True), g);
  lQueryCount := 0;
  lState.fAllReady := lAllReady;
  lState.fCallerGate := lCallerGate;
  SetLength(lState.fResults, cCallerCount);
  SetLength(lState.fValues, cCallerCount);
  SetWindowsBootIdentityQueryForTesting(
    function(
      const aTimeoutMilliseconds: Cardinal;
      out aIdentity: TWindowsBootIdentity): Boolean
    begin
      TInterlocked.Increment(lQueryCount);
      lQueryEntered.SetEvent;
      if aTimeoutMilliseconds = 0 then
        Exit(False);
      if lQueryRelease.WaitFor(cTimeoutMilliseconds) <> wrSignaled then
        Exit(False);
      aIdentity.UtcMilliseconds := cExpectedIdentity;
      aIdentity.Source := wbisWindowsRecorded;
      Result := True;
    end);
  ResetWindowsBootIdentityCacheForTesting;
  try
    for i := 0 to cCallerCount - 1 do
    begin
      lThreads.Add(
        CreateBootIdentityCaller(
          i,
          lState));
      lThreads.Last.Start;
    end;
    Assert.AreEqual<TWaitResult>(
      wrSignaled,
      lAllReady.WaitFor(cTimeoutMilliseconds),
      'Identity callers did not become ready');
    lCallerGate.SetEvent;
    Assert.AreEqual<TWaitResult>(
      wrSignaled,
      lQueryEntered.WaitFor(cTimeoutMilliseconds),
      'The shared identity query did not start');
    Sleep(50);
    lQueryRelease.SetEvent;
    for i := 0 to lThreads.Count - 1 do
      lThreads[i].WaitFor;

    Assert.AreEqual(
      1,
      TInterlocked.CompareExchange(lQueryCount, 0, 0),
      'Concurrent callers must share one lookup');
    for i := 0 to cCallerCount - 1 do
    begin
      Assert.IsTrue(
        lState.fResults[i],
        Format('Caller %d should receive the shared result', [i]));
      Assert.AreEqual<Int64>(
        cExpectedIdentity,
        lState.fValues[i],
        Format('Caller %d received the wrong identity', [i]));
    end;
  finally
    lCallerGate.SetEvent;
    lQueryRelease.SetEvent;
    for i := 0 to lThreads.Count - 1 do
      lThreads[i].WaitFor;
    SetWindowsBootIdentityQueryForTesting(nil);
    ResetWindowsBootIdentityCacheForTesting;
  end;
end;

procedure TWindowsIdentityTests.BootIdentity_IsStrongStableAndCached;
var
  lCachedIdentity: TWindowsBootIdentity;
  lFirstIdentity: TWindowsBootIdentity;
  lSecondIdentity: TWindowsBootIdentity;
begin
  ResetWindowsBootIdentityCacheForTesting;
  Assert.IsTrue(
    TryGetWindowsBootIdentity(1000, True, lFirstIdentity),
    'Windows recorded boot identity should be available');
  Assert.AreEqual<TWindowsBootIdentitySource>(
    wbisWindowsRecorded,
    lFirstIdentity.Source,
    'Boot identity must be marked as Windows-recorded strong evidence');
  Assert.IsTrue(
    lFirstIdentity.UtcMilliseconds > 0,
    'Boot identity must be a positive UTC millisecond value');
  Assert.IsTrue(
    Abs(
      lFirstIdentity.UtcMilliseconds -
      (CurrentUtcMilliseconds - Int64(GetTickCount64))) < 300000,
    'WMI boot identity should be UTC and agree with Windows uptime');

  Assert.IsTrue(
    TryGetWindowsBootIdentity(1000, False, lSecondIdentity),
    'A repeated boot identity lookup should use the cached value');
  Assert.AreEqual<Int64>(
    lFirstIdentity.UtcMilliseconds,
    lSecondIdentity.UtcMilliseconds,
    'Repeated boot identity lookups must be stable');

  Assert.IsTrue(
    TryGetCachedWindowsBootIdentity(lCachedIdentity),
    'The non-blocking cached API should return the resolved identity');
  Assert.AreEqual<Int64>(
    lFirstIdentity.UtcMilliseconds,
    lCachedIdentity.UtcMilliseconds,
    'The cached API must return the same identity');
end;

procedure TWindowsIdentityTests.BootIdentity_QueryReceivesBoundedCallerTimeout;
const
  cCallerTimeoutMilliseconds = 17;
var
  lIdentity: TWindowsBootIdentity;
  lObservedTimeoutMilliseconds: Integer;
begin
  lObservedTimeoutMilliseconds := -1;
  SetWindowsBootIdentityQueryForTesting(
    function(
      const aTimeoutMilliseconds: Cardinal;
      out aIdentity: TWindowsBootIdentity): Boolean
    begin
      TInterlocked.Exchange(
        lObservedTimeoutMilliseconds,
        Integer(aTimeoutMilliseconds));
      aIdentity := Default(TWindowsBootIdentity);
      Result := False;
    end);
  ResetWindowsBootIdentityCacheForTesting;
  try
    Assert.IsFalse(
      TryGetWindowsBootIdentity(
        cCallerTimeoutMilliseconds,
        True,
        lIdentity));
    Assert.AreEqual(
      cCallerTimeoutMilliseconds,
      TInterlocked.CompareExchange(
        lObservedTimeoutMilliseconds,
        0,
        0),
      'The WMI query must receive the caller timeout after bounding');
  finally
    SetWindowsBootIdentityQueryForTesting(nil);
    ResetWindowsBootIdentityCacheForTesting;
  end;
end;

procedure TWindowsIdentityTests.BootIdentity_SecondProcessAgrees;
var
  lIdentity: TWindowsBootIdentity;
  lSecondProcessIdentity: Int64;
begin
  ResetWindowsBootIdentityCacheForTesting;
  Assert.IsTrue(
    TryGetWindowsBootIdentity(1000, True, lIdentity),
    'The first process should resolve boot identity');
  lSecondProcessIdentity := ReadBootIdentityFromSecondProcess;
  Assert.AreEqual<Int64>(
    lIdentity.UtcMilliseconds,
    lSecondProcessIdentity,
    'Two processes on the same boot must agree on boot identity');
end;

procedure TWindowsIdentityTests.BootIdentity_UnavailableSourceCannotBeCached;
var
  lIdentity: TWindowsBootIdentity;
begin
  SetWindowsBootIdentityQueryForTesting(
    function(
      const aTimeoutMilliseconds: Cardinal;
      out aIdentity: TWindowsBootIdentity): Boolean
    begin
      aIdentity := Default(TWindowsBootIdentity);
      Result := aTimeoutMilliseconds <= 1000;
    end);
  ResetWindowsBootIdentityCacheForTesting;
  try
    Assert.IsFalse(
      TryGetWindowsBootIdentity(1000, True, lIdentity),
      'Unavailable source metadata cannot become an authoritative identity');
    Assert.IsFalse(
      TryGetCachedWindowsBootIdentity(lIdentity),
      'An unavailable source must not enter the strong/degraded cache');
  finally
    SetWindowsBootIdentityQueryForTesting(nil);
    ResetWindowsBootIdentityCacheForTesting;
  end;
end;

procedure TWindowsIdentityTests.BootIdentity_UnavailableIsCachedUntilExplicitRetry;
var
  lIdentity: TWindowsBootIdentity;
  lQueryCount: Integer;
begin
  lQueryCount := 0;
  SetWindowsBootIdentityQueryForTesting(
    function(
      const aTimeoutMilliseconds: Cardinal;
      out aIdentity: TWindowsBootIdentity): Boolean
    begin
      aIdentity := Default(TWindowsBootIdentity);
      if TInterlocked.Increment(lQueryCount) = 1 then
        Exit(False);
      aIdentity.UtcMilliseconds := 123456789;
      aIdentity.Source := wbisWindowsRecorded;
      Result := True;
    end);
  ResetWindowsBootIdentityCacheForTesting;
  try
    Assert.IsFalse(
      TryGetWindowsBootIdentity(1000, False, lIdentity),
      'The controlled first lookup should be unavailable');
    Assert.AreEqual(1, TInterlocked.CompareExchange(lQueryCount, 0, 0));

    Assert.IsFalse(
      TryGetWindowsBootIdentity(1000, False, lIdentity),
      'Unavailable identity should be cached');
    Assert.AreEqual(
      1,
      TInterlocked.CompareExchange(lQueryCount, 0, 0),
      'A cached unavailable result must not repeat WMI work');

    Assert.IsTrue(
      TryGetWindowsBootIdentity(1000, True, lIdentity),
      'An owning background workflow should be able to retry explicitly');
    Assert.AreEqual(
      2,
      TInterlocked.CompareExchange(lQueryCount, 0, 0),
      'Explicit retry should perform exactly one new query');
    Assert.AreEqual<Int64>(123456789, lIdentity.UtcMilliseconds);
  finally
    SetWindowsBootIdentityQueryForTesting(nil);
    ResetWindowsBootIdentityCacheForTesting;
  end;
end;

procedure TWindowsIdentityTests.ProcessStart_CurrentProcessIsStableUtcMilliseconds;
var
  lFirstStartedAt: Int64;
  lNowMilliseconds: Int64;
  lSecondStartedAt: Int64;
begin
  Assert.IsTrue(
    TryGetProcessStartedAtUtcMilliseconds(
      Winapi.Windows.GetCurrentProcessId,
      lFirstStartedAt),
    'Current-process creation time should be available');
  Assert.IsTrue(
    TryGetProcessStartedAtUtcMilliseconds(
      Winapi.Windows.GetCurrentProcessId,
      lSecondStartedAt),
    'Repeated current-process creation time should be available');
  Assert.AreEqual<Int64>(
    lFirstStartedAt,
    lSecondStartedAt,
    'Process creation identity must be stable');

  lNowMilliseconds := CurrentUtcMilliseconds;
  Assert.IsTrue(
    (lFirstStartedAt > 0) and (lFirstStartedAt <= lNowMilliseconds),
    'Process creation must be a positive UTC millisecond timestamp');
end;

procedure TWindowsIdentityTests.ProcessStart_InvalidProcessIsUnavailable;
var
  lStartedAt: Int64;
begin
  lStartedAt := -1;
  Assert.IsFalse(
    TryGetProcessStartedAtUtcMilliseconds(High(Cardinal), lStartedAt),
    'An invalid process must report unavailable');
  Assert.AreEqual<Int64>(
    0,
    lStartedAt,
    'Unavailable process identity must not leak a stale value');
end;

initialization
  TDUnitX.RegisterTestFixture(TWindowsIdentityTests);

end.
