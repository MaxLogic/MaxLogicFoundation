unit MaxLogic.Process.Tests;

{$I fpc_delphimode.inc}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMaxLogicProcessTests = class
  public
    [Test] procedure CurrentProcessId_IsNonZero;
    [Test] procedure FindPid_CurrentExecutable_ContainsCurrentProcess;
    [Test] procedure RamUsageBytes_CurrentProcess_IsPositiveOnWindows;
    [Test] procedure CpuUsage_CurrentProcess_DoesNotFailOnWindows;
  end;

implementation

uses
  System.Classes,
  System.Diagnostics,
  System.SyncObjs,
  System.SysUtils,
  MaxLogic.Process;

function PidArrayContains(const aItems: TPidArray; const aPid: TPid): Boolean;
var
  lPid: TPid;
begin
  Result := False;
  for lPid in aItems do
    if lPid = aPid then
      Exit(True);
end;

procedure StartCpuBurners(const aStopEvent: TEvent; out aThreads: TArray<TThread>);
var
  lThreadCount: Integer;
  i: Integer;
begin
  lThreadCount := System.CPUCount;
  if lThreadCount < 1 then
    lThreadCount := 1;

  SetLength(aThreads, lThreadCount);

  for i := 0 to High(aThreads) do
  begin
    aThreads[i] := TThread.CreateAnonymousThread(
      procedure
      var
        lX: Int64;
        lJ: Integer;
      begin
        lX := 1;
        while aStopEvent.WaitFor(0) <> wrSignaled do
        begin
          for lJ := 1 to 250000 do
          begin
            lX := lX + lJ;
            if lX > 1000000000 then
              lX := lX - 1000000000;
          end;
        end;

        if lX = 0 then
          Sleep(0);
      end);
    aThreads[i].FreeOnTerminate := False;
    aThreads[i].Start;
  end;
end;

procedure WaitAndFreeThreads(var aThreads: TArray<TThread>);
var
  i: Integer;
begin
  for i := 0 to High(aThreads) do
  begin
    aThreads[i].WaitFor;
    aThreads[i].Free;
    aThreads[i] := nil;
  end;
end;

procedure TMaxLogicProcessTests.CurrentProcessId_IsNonZero;
begin
  Assert.IsTrue(CurrentProcessId <> 0, 'CurrentProcessId should not be zero');
end;

procedure TMaxLogicProcessTests.FindPid_CurrentExecutable_ContainsCurrentProcess;
var
  lExeName: string;
  lPids: TPidArray;
  lCurrentPid: TPid;
begin
  lCurrentPid := CurrentProcessId;
  lExeName := ExtractFileName(ParamStr(0));
  lPids := FindPid(lExeName);

  Assert.IsTrue(
    PidArrayContains(lPids, lCurrentPid),
    Format('Current PID %d was not found for pattern "%s"', [lCurrentPid, lExeName]));
end;

procedure TMaxLogicProcessTests.RamUsageBytes_CurrentProcess_IsPositiveOnWindows;
{$IFDEF MSWINDOWS}
var
  lBytes: UInt64;
begin
  lBytes := RamUsageBytes(CurrentProcessId);
  Assert.IsTrue(lBytes > 0, 'Expected RAM usage > 0 bytes for current process');
end;
{$ELSE}
begin
  Assert.IsTrue(True, 'Skipped on non-Windows');
end;
{$ENDIF}

procedure TMaxLogicProcessTests.CpuUsage_CurrentProcess_DoesNotFailOnWindows;
{$IFDEF MSWINDOWS}
const
  cIdleWindowMs = 1200;
  cLoadedWindowSeconds = 10;
  cMinOverallIncrease = 5.0;
  cMinSingleCoreIncrease = 20.0;
var
  lPid: TPid;
  lBefore: TCpuUsage;
  lNow: TCpuUsage;
  lLoadMaxOverall: Double;
  lLoadMaxSingle: Double;
  lStopEvent: TEvent;
  lThreads: TArray<TThread>;
  lSec: Integer;
begin
  lPid := CurrentProcessId;

  // Warm-up sample.
  CpuUsage(lPid);

  Sleep(cIdleWindowMs);
  lBefore := CpuUsage(lPid);

  lLoadMaxOverall := 0;
  lLoadMaxSingle := 0;
  lStopEvent := TEvent.Create(nil, True, False, '');
  try
    StartCpuBurners(lStopEvent, lThreads);
    CpuUsage(lPid); // reset sample right before loaded window

    for lSec := 1 to cLoadedWindowSeconds do
    begin
      Sleep(1000);
      lNow := CpuUsage(lPid);
      if lNow.OverallPercent > lLoadMaxOverall then
        lLoadMaxOverall := lNow.OverallPercent;
      if lNow.SingleCorePercent > lLoadMaxSingle then
        lLoadMaxSingle := lNow.SingleCorePercent;
    end;
  finally
    lStopEvent.SetEvent;
    WaitAndFreeThreads(lThreads);
    lStopEvent.Free;
  end;

  Assert.IsTrue(lBefore.SingleCorePercent >= 0, 'Baseline SingleCorePercent should be non-negative');
  Assert.IsTrue(lBefore.OverallPercent >= 0, 'Baseline OverallPercent should be non-negative');
  Assert.IsTrue(lLoadMaxSingle >= 0, 'Loaded SingleCorePercent should be non-negative');
  Assert.IsTrue(lLoadMaxOverall >= 0, 'Loaded OverallPercent should be non-negative');

  Assert.IsTrue(
    lLoadMaxOverall > (lBefore.OverallPercent + cMinOverallIncrease),
    Format('Overall CPU usage did not increase enough (before=%.2f, loaded-max=%.2f)', [lBefore.OverallPercent, lLoadMaxOverall]));
  Assert.IsTrue(
    lLoadMaxSingle > (lBefore.SingleCorePercent + cMinSingleCoreIncrease),
    Format('Single-core CPU usage did not increase enough (before=%.2f, loaded-max=%.2f)', [lBefore.SingleCorePercent, lLoadMaxSingle]));
end;
{$ELSE}
begin
  Assert.IsTrue(True, 'Skipped on non-Windows');
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TMaxLogicProcessTests);

end.
