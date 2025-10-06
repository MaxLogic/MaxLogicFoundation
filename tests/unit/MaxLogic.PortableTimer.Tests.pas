unit MaxLogic.PortableTimer.Tests;

{$I fpc_delphimode.inc}

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs,
  DUnitX.TestFramework,
  maxlogic.fpc.compatibility, MaxLogic.Utils,
  MaxLogic.PortableTimer;

type
  [TestFixture]
  TPortableTimerTests = class
  private
    const cSampleSlots = 6;
  private
    fTimer: TPortableTimer;
    fLock: TCriticalSection;
    fSignal: TEvent;
    fCounter: Integer;
    fTimes: array[0..cSampleSlots - 1] of QWord;
    fRecordTimes: Boolean;
    fStopAfter: Integer;
    fHandlerSleepMs: Cardinal;
    fAfterTick: TProc;
    fAlternateHandlerHit: Boolean;

    procedure TimerHandler(aSender: TObject);
    procedure AlternateHandler(aSender: TObject);
    procedure ResetState;
    function WaitForCounter(const aTarget: Integer; const aTimeoutMs: Cardinal): Boolean;
  public
    [Setup] procedure SetUp;
    [TearDown] procedure TearDown;

    [Test] procedure StartAndStopFiresOnce;
    [Test] procedure FixedRateKeepsCadence;
    [Test] procedure FixedDelayWaitsForHandlerCompletion;
    [Test] procedure IntervalChangeAppliesNextTick;
    [Test] procedure ModeSwitchApplies;
    [Test] procedure HandlerSwapTakesEffect;
    [Test] procedure DestroyInHandlerIsSafe;
    [Test] procedure EnableTogglePausesAndResumes;
    [Test] procedure ZeroIntervalGetsCoerced;
  end;

implementation

function NowMs: QWord; inline;
begin
  Result := GetTickCount64;
end;

procedure TPortableTimerTests.ResetState;
var
  lIdx: Integer;
begin
  fLock.Acquire;
  try
    fCounter := 0;
    for lIdx := Low(fTimes) to High(fTimes) do
      fTimes[lIdx] := 0;
  finally
    fLock.Release;
  end;

  fRecordTimes := False;
  fStopAfter := 0;
  fHandlerSleepMs := 0;
  fAfterTick := nil;
  fAlternateHandlerHit := False;
  if fSignal <> nil then
    fSignal.ResetEvent;
end;

procedure TPortableTimerTests.SetUp;
begin
  fLock := TCriticalSection.Create;
  fSignal := TEvent.Create(nil, False, False, '');
  fTimer := TPortableTimer.Create;
  fTimer.OnTimer := TimerHandler;
  fTimer.SchedulingMode := tmFixedDelay;
  ResetState;
end;

procedure TPortableTimerTests.TearDown;
begin
  if fTimer <> nil then
  begin
    fTimer.Stop;
    fTimer.Free;
    fTimer := nil;
  end;
  if fSignal <> nil then
  begin
    fSignal.Free;
    fSignal := nil;
  end;
  if fLock <> nil then
  begin
    fLock.Free;
    fLock := nil;
  end;
  fAfterTick := nil;
end;

procedure TPortableTimerTests.TimerHandler(aSender: TObject);
var
  lNextIndex: Integer;
  lShouldStop: Boolean;
  lProc: TProc;
begin
  if fHandlerSleepMs > 0 then
    Sleep(fHandlerSleepMs);

  fLock.Acquire;
  try
    Inc(fCounter);
    lNextIndex := fCounter;
    if fRecordTimes and (lNextIndex <= cSampleSlots) then
      fTimes[lNextIndex - 1] := NowMs;
    lShouldStop := (fStopAfter > 0) and (fCounter >= fStopAfter);
    lProc := fAfterTick;
  finally
    fLock.Release;
  end;

  if Assigned(lProc) then
    lProc;

  if lShouldStop then
    fTimer.Stop;

  fSignal.SetEvent;
end;

procedure TPortableTimerTests.AlternateHandler(aSender: TObject);
begin
  fAlternateHandlerHit := True;
  fTimer.OnTimer := TimerHandler;
  TimerHandler(aSender);
end;

function TPortableTimerTests.WaitForCounter(const aTarget: Integer; const aTimeoutMs: Cardinal): Boolean;
var
  lDeadline: QWord;
  lNow: QWord;
  lRemaining: Cardinal;
  lWait: TWaitResult;
begin
  if aTarget <= 0 then
    Exit(True);

  lDeadline := NowMs + QWord(aTimeoutMs);
  repeat
    lNow := NowMs;
    if lNow >= lDeadline then
      Exit(False);

    if (lDeadline - lNow) > High(Cardinal) then
      lRemaining := High(Cardinal)
    else
      lRemaining := Cardinal(lDeadline - lNow);

    if lRemaining = 0 then
      lRemaining := 1;

    lWait := fSignal.WaitFor(lRemaining);
    if lWait = wrSignaled then
    begin
      fLock.Acquire;
      try
        if fCounter >= aTarget then
          Exit(True);
      finally
        fLock.Release;
      end;
    end else
    if (lWait = wrTimeout) or (lWait = wrAbandoned) or (lWait = wrError) then
      Exit(False)
    else
      Exit(False);
  until False;
end;

procedure TPortableTimerTests.StartAndStopFiresOnce;
const
  cTimeoutMs = 600;
begin
  ResetState;
  fStopAfter := 1;
  fTimer.Start(40);

  Assert.IsTrue(WaitForCounter(1, cTimeoutMs), 'Timer did not fire within the expected time');

  fLock.Acquire;
  try
    Assert.AreEqual(1, fCounter, 'Timer fired an unexpected number of times');
  finally
    fLock.Release;
  end;
end;

procedure TPortableTimerTests.FixedRateKeepsCadence;
const
  cInterval = 50;
  cTolerance = 60;
  cTimeoutMs = 2000;
var
  lFirst: QWord;
  lSecond: QWord;
  lThird: QWord;
  lDelta1: Int64;
  lDelta2: Int64;
begin
  ResetState;
  fRecordTimes := True;
  fStopAfter := 3;
  fHandlerSleepMs := 10;

  fTimer.SchedulingMode := tmFixedRate;
  fTimer.Start(cInterval);

  Assert.IsTrue(WaitForCounter(3, cTimeoutMs), 'Fixed-rate timer did not produce three ticks');

  fLock.Acquire;
  try
    lFirst := fTimes[0];
    lSecond := fTimes[1];
    lThird := fTimes[2];
  finally
    fLock.Release;
  end;

  Assert.IsTrue((lFirst <> 0) and (lSecond <> 0) and (lThird <> 0), 'Missing captured timestamps');

  lDelta1 := Int64(lSecond) - Int64(lFirst);
  lDelta2 := Int64(lThird) - Int64(lSecond);

  Assert.IsTrue((lDelta1 >= cInterval - cTolerance) and (lDelta1 <= cInterval + cTolerance),
    Format('First interval drifted beyond tolerance (actual=%d)', [lDelta1]));
  Assert.IsTrue((lDelta2 >= cInterval - cTolerance) and (lDelta2 <= cInterval + cTolerance),
    Format('Second interval drifted beyond tolerance (actual=%d)', [lDelta2]));
end;

procedure TPortableTimerTests.FixedDelayWaitsForHandlerCompletion;
const
  cInterval = 25;
  cSleepMs = 120;
  cTimeoutMs = 2500;
var
  lFirst: QWord;
  lSecond: QWord;
  lThird: QWord;
  lDelta1: Int64;
  lDelta2: Int64;
begin
  ResetState;
  fRecordTimes := True;
  fStopAfter := 3;
  fHandlerSleepMs := cSleepMs;

  fTimer.SchedulingMode := tmFixedDelay;
  fTimer.Start(cInterval);

  Assert.IsTrue(WaitForCounter(3, cTimeoutMs), 'Fixed-delay timer did not produce three ticks');

  fLock.Acquire;
  try
    lFirst := fTimes[0];
    lSecond := fTimes[1];
    lThird := fTimes[2];
  finally
    fLock.Release;
  end;

  Assert.IsTrue((lFirst <> 0) and (lSecond <> 0) and (lThird <> 0), 'Missing captured timestamps');

  lDelta1 := Int64(lSecond) - Int64(lFirst);
  lDelta2 := Int64(lThird) - Int64(lSecond);

  Assert.IsTrue(lDelta1 >= cSleepMs, 'Fixed-delay mode must wait for handler completion');
  Assert.IsTrue(lDelta2 >= cSleepMs, 'Fixed-delay mode must wait for handler completion');
end;

procedure TPortableTimerTests.IntervalChangeAppliesNextTick;
const
  cInitial = 35;
  cNew = 150;
  cTimeoutMs = 4000;
var
  lDelta1: Int64;
  lDelta2: Int64;
begin
  ResetState;
  fRecordTimes := True;
  fStopAfter := 4;

  fAfterTick := procedure
  begin
    if fCounter = 1 then
      fTimer.Interval := cNew;
    if fCounter >= 4 then
      fTimer.Enabled := False;
  end;

  fTimer.Start(cInitial);

  Assert.IsTrue(WaitForCounter(4, cTimeoutMs), 'Timer did not reach expected ticks');

  fLock.Acquire;
  try
    Assert.IsTrue((fTimes[0] <> 0) and (fTimes[1] <> 0) and (fTimes[2] <> 0), 'Missing timestamp data');
    lDelta1 := Int64(fTimes[1]) - Int64(fTimes[0]);
    lDelta2 := Int64(fTimes[2]) - Int64(fTimes[1]);
  finally
    fLock.Release;
  end;

  Assert.IsTrue(lDelta1 > 0, 'Initial interval capture failed');
  Assert.IsTrue(lDelta2 >= cNew - 60, Format('Interval change not reflected (delta=%d)', [lDelta2]));
end;

procedure TPortableTimerTests.ModeSwitchApplies;
const
  cInterval = 40;
  cTimeoutMs = 5000;
var
  lDeltaBefore: Int64;
  lDeltaAfter: Int64;
begin
  ResetState;
  fRecordTimes := True;
  fStopAfter := 5;
  fHandlerSleepMs := 70;

  fAfterTick := procedure
  begin
    if fCounter = 2 then
      fTimer.SchedulingMode := tmFixedRate;
    if fCounter >= 5 then
      fTimer.Enabled := False;
  end;

  fTimer.Start(cInterval);

  Assert.IsTrue(WaitForCounter(5, cTimeoutMs), 'Timer did not reach expected ticks');

  fLock.Acquire;
  try
    Assert.IsTrue((fTimes[0] <> 0) and (fTimes[1] <> 0) and (fTimes[2] <> 0) and (fTimes[3] <> 0), 'Missing timestamp data');
    lDeltaBefore := Int64(fTimes[1]) - Int64(fTimes[0]);
    lDeltaAfter := Int64(fTimes[3]) - Int64(fTimes[2]);
  finally
    fLock.Release;
  end;

  Assert.IsTrue(lDeltaBefore > 0, 'Baseline interval capture failed');
  Assert.IsTrue(lDeltaAfter <= lDeltaBefore, Format('Fixed-rate switch failed to tighten cadence (before=%d after=%d)', [lDeltaBefore, lDeltaAfter]));
end;

procedure TPortableTimerTests.HandlerSwapTakesEffect;
const
  cTimeoutMs = 3000;
begin
  ResetState;
  fStopAfter := 4;

  fAfterTick := procedure
  begin
    if fCounter = 1 then
      fTimer.OnTimer := AlternateHandler
    else
    if fCounter >= 4 then
      fTimer.Enabled := False;
  end;

  fTimer.Start(45);

  Assert.IsTrue(WaitForCounter(4, cTimeoutMs), 'Timer did not reach expected ticks');
  Assert.IsTrue(fAlternateHandlerHit, 'Alternate handler was never invoked');
end;

procedure TPortableTimerTests.DestroyInHandlerIsSafe;
const
  cTimeoutMs = 2000;
var
  lDestroyed: Boolean;
begin
  ResetState;
  lDestroyed := False;

  fAfterTick := procedure
  begin
    if fCounter = 1 then
    begin
      fTimer.Free;
      fTimer := nil;
      lDestroyed := True;
    end;
  end;

  fTimer.Start(30);

  Assert.IsTrue(WaitForCounter(1, cTimeoutMs), 'Timer never fired before destruction');
  Assert.IsTrue(lDestroyed, 'Timer was not freed inside handler');
end;

procedure TPortableTimerTests.EnableTogglePausesAndResumes;
const
  cTimeoutMs = 5000;
var
  lCountBeforePause: Integer;
begin
  ResetState;

  fAfterTick := procedure
  begin
    if fCounter = 5 then
      fTimer.Enabled := False;
  end;

  fTimer.Start(35);

  Assert.IsTrue(WaitForCounter(2, cTimeoutMs), 'Timer did not reach initial ticks');

  fLock.Acquire;
  try
    lCountBeforePause := fCounter;
  finally
    fLock.Release;
  end;

  fTimer.Enabled := False;
  Sleep(150);

  fLock.Acquire;
  try
    Assert.AreEqual(lCountBeforePause, fCounter, 'Timer advanced while disabled');
  finally
    fLock.Release;
  end;

  fTimer.Enabled := True;

  Assert.IsTrue(WaitForCounter(5, cTimeoutMs), 'Timer did not resume after enabling');
end;

procedure TPortableTimerTests.ZeroIntervalGetsCoerced;
const
  cTimeoutMs = 2000;
var
  lDelta1: Int64;
  lDelta2: Int64;
begin
  ResetState;
  fRecordTimes := True;
  fStopAfter := 3;

  fAfterTick := procedure
  begin
    if fCounter >= 3 then
      fTimer.Enabled := False;
  end;

  fTimer.Start(0);

  Assert.IsTrue(WaitForCounter(3, cTimeoutMs), 'Timer did not fire with zero interval');

  fLock.Acquire;
  try
    Assert.IsTrue((fTimes[0] <> 0) and (fTimes[1] <> 0) and (fTimes[2] <> 0), 'Missing timestamp data');
    lDelta1 := Int64(fTimes[1]) - Int64(fTimes[0]);
    lDelta2 := Int64(fTimes[2]) - Int64(fTimes[1]);
  finally
    fLock.Release;
  end;

  Assert.IsTrue((lDelta1 > 0) and (lDelta2 > 0), 'Zero interval was not coerced to positive cadence');
end;

end.
