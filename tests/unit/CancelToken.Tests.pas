unit CancelToken.Tests;

{$I fpc_delphimode.inc}

interface

uses
  System.Classes, System.SyncObjs,
  DUnitX.TestFramework,
  AutoFree, CancelToken;

type
  [TestFixture]
  TCancelTokenTests = class
  public
    [Test] procedure Cancel_ShouldPublishToWaitingThread;
  end;

implementation

procedure TCancelTokenTests.Cancel_ShouldPublishToWaitingThread;
const
  cExpectedPayload = 42;
  cTimeoutMs = 2000;
var
  g: TGarbos;
  lDone: TEvent;
  lDoneResult: TWaitResult;
  lEmergencyStop: TEvent;
  lObservedPayload: Integer;
  lPayload: Integer;
  lStarted: TEvent;
  lStartedResult: TWaitResult;
  lThread: TThread;
  lToken: iCancelToken;
begin
  GC(lStarted, TEvent.Create(nil, True, False, ''), g);
  GC(lDone, TEvent.Create(nil, True, False, ''), g);
  GC(lEmergencyStop, TEvent.Create(nil, True, False, ''), g);
  lObservedPayload := 0;
  lPayload := 0;
  lToken := TCancelToken.Create;
  GC(lThread, TThread.CreateAnonymousThread(
    procedure
    begin
      lStarted.SetEvent;
      while (not lToken.Canceled) and
        (lEmergencyStop.WaitFor(0) <> wrSignaled) do
        TThread.Yield;
      if lToken.Canceled then
      begin
        lObservedPayload := lPayload;
        lDone.SetEvent;
      end;
    end), g);
  lThread.FreeOnTerminate := False;

  lThread.Start;
  lStartedResult := lStarted.WaitFor(cTimeoutMs);

  lPayload := cExpectedPayload;
  lToken.Cancel;

  lDoneResult := lDone.WaitFor(cTimeoutMs);
  lEmergencyStop.SetEvent;
  lThread.WaitFor;
  Assert.AreEqual<TWaitResult>(wrSignaled, lStartedResult,
    'Worker did not start');
  Assert.AreEqual<TWaitResult>(wrSignaled, lDoneResult,
    'Cancellation was not visible to the worker');
  Assert.AreEqual(cExpectedPayload, lObservedPayload,
    'Writes preceding cancellation were not visible to the worker');
end;

end.
