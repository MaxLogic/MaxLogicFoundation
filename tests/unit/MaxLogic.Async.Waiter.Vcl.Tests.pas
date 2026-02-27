unit MaxLogic.Async.Waiter.Vcl.Tests;

{$I fpc_delphimode.inc}

interface

uses
  System.Classes, System.Diagnostics, System.SyncObjs, System.SysUtils,
  DUnitX.TestFramework,
  maxAsync;

type
  TNonMaxAsync = class(TInterfacedObject, iAsync)
  public
    function Finished: Boolean;
    {$IFDEF MsWindows}
    function GetThreadPriority: TThreadPriority;
    procedure msgwaitfor;
    procedure SetThreadPriority(const aValue: TThreadPriority);
    {$ENDIF}
    procedure WaitFor;
    procedure WakeUp(aProc: TThreadProcedure; const aTaskName: string); overload;
    procedure WakeUp; overload;
  end;

  TQueuedEventNotifier = class
  private
    fEvent: TEvent;
  public
    constructor Create(aEvent: TEvent);
    procedure Signal;
  end;

  [TestFixture]
  TWaiterVclTests = class
  public
    [Test] procedure WaitForEventsProcessesQueuedCallsWhenMessagePumpEnabled;
    [Test] procedure WaitForEventsDefersQueuedCallsWhenMessagePumpDisabled;
    [Test] procedure WaitForEventsReturnsFalseOnTimeout;
    [Test] procedure WaitForAsyncSupportsSimpleAsyncCall;
    [Test] procedure WaitForAsyncRejectsNonMaxAsyncImplementation;
  end;

implementation

function TNonMaxAsync.Finished: Boolean;
begin
  Result := True;
end;

{$IFDEF MsWindows}
function TNonMaxAsync.GetThreadPriority: TThreadPriority;
begin
  Result := TThreadPriority.tpNormal;
end;

procedure TNonMaxAsync.msgwaitfor;
begin
end;

procedure TNonMaxAsync.SetThreadPriority(const aValue: TThreadPriority);
begin
end;
{$ENDIF}

procedure TNonMaxAsync.WaitFor;
begin
end;

procedure TNonMaxAsync.WakeUp;
begin
end;

procedure TNonMaxAsync.WakeUp(aProc: TThreadProcedure; const aTaskName: string);
begin
end;

constructor TQueuedEventNotifier.Create(aEvent: TEvent);
begin
  inherited Create;
  fEvent := aEvent;
end;

procedure TQueuedEventNotifier.Signal;
begin
  if fEvent <> nil then
    fEvent.SetEvent;
end;

procedure TWaiterVclTests.WaitForEventsProcessesQueuedCallsWhenMessagePumpEnabled;
const
  cTimeoutMs = 5000;
var
  lDoneEvent: TEvent;
  lQueuedEvent: TEvent;
  lQueuedNotifier: TQueuedEventNotifier;
  lWaitResult: Boolean;
  lWorker: TThread;
begin
  lDoneEvent := TEvent.Create(nil, True, False, '');
  lQueuedEvent := TEvent.Create(nil, True, False, '');
  lQueuedNotifier := TQueuedEventNotifier.Create(lQueuedEvent);
  lWorker := nil;
  try
    lWorker := TThread.CreateAnonymousThread(
      procedure
      begin
        Sleep(50);
        TThread.Queue(nil, lQueuedNotifier.Signal);
        Sleep(50);
        lDoneEvent.SetEvent;
      end);
    lWorker.FreeOnTerminate := False;
    lWorker.Start;

    lWaitResult := TWaiter.WaitFor([lDoneEvent], cTimeoutMs, True);

    Assert.IsTrue(lWaitResult, 'TWaiter should complete before timeout');
    Assert.AreEqual<TWaitResult>(wrSignaled, lQueuedEvent.WaitFor(0),
      'Queued call should execute while waiting with DoProcessMessages=True');
  finally
    if lWorker <> nil then
      lWorker.WaitFor;
    while CheckSynchronize(0) do
    begin
    end;
    lWorker.Free;
    lQueuedNotifier.Free;
    lQueuedEvent.Free;
    lDoneEvent.Free;
  end;
end;

procedure TWaiterVclTests.WaitForEventsDefersQueuedCallsWhenMessagePumpDisabled;
const
  cTimeoutMs = 5000;
var
  lDoneEvent: TEvent;
  lQueuedEvent: TEvent;
  lQueuedNotifier: TQueuedEventNotifier;
  lWaitResult: Boolean;
  lWorker: TThread;
begin
  lDoneEvent := TEvent.Create(nil, True, False, '');
  lQueuedEvent := TEvent.Create(nil, True, False, '');
  lQueuedNotifier := TQueuedEventNotifier.Create(lQueuedEvent);
  lWorker := nil;
  try
    lWorker := TThread.CreateAnonymousThread(
      procedure
      begin
        Sleep(50);
        TThread.Queue(nil, lQueuedNotifier.Signal);
        Sleep(50);
        lDoneEvent.SetEvent;
      end);
    lWorker.FreeOnTerminate := False;
    lWorker.Start;

    lWaitResult := TWaiter.WaitFor([lDoneEvent], cTimeoutMs, False);

    Assert.IsTrue(lWaitResult, 'TWaiter should complete before timeout');
    Assert.AreEqual<TWaitResult>(wrTimeout, lQueuedEvent.WaitFor(0),
      'Queued call should not run while DoProcessMessages=False');

    CheckSynchronize(cTimeoutMs);
    Assert.AreEqual<TWaitResult>(wrSignaled, lQueuedEvent.WaitFor(0),
      'Queued call should run after explicit synchronize processing');
  finally
    if lWorker <> nil then
      lWorker.WaitFor;
    while CheckSynchronize(0) do
    begin
    end;
    lWorker.Free;
    lQueuedNotifier.Free;
    lQueuedEvent.Free;
    lDoneEvent.Free;
  end;
end;

procedure TWaiterVclTests.WaitForEventsReturnsFalseOnTimeout;
const
  cTimeoutMs = 150;
var
  lDoneEvent: TEvent;
  lStopwatch: TStopwatch;
  lWaitResult: Boolean;
begin
  lDoneEvent := TEvent.Create(nil, True, False, '');
  try
    lStopwatch := TStopwatch.StartNew;
    lWaitResult := TWaiter.WaitFor([lDoneEvent], cTimeoutMs, True);
    lStopwatch.Stop;

    Assert.IsFalse(lWaitResult, 'TWaiter should report timeout for unsignaled events');
    Assert.IsTrue(lStopwatch.ElapsedMilliseconds >= (cTimeoutMs - 40),
      Format('Timeout returned too quickly: %d ms', [lStopwatch.ElapsedMilliseconds]));
  finally
    lDoneEvent.Free;
  end;
end;

procedure TWaiterVclTests.WaitForAsyncSupportsSimpleAsyncCall;
const
  cTimeoutMs = 5000;
var
  lAsyncA: iAsync;
  lAsyncB: iAsync;
  lCallCount: Integer;
  lWaitResult: Boolean;
begin
  lCallCount := 0;
  lAsyncA := SimpleAsyncCall(
    procedure
    begin
      Sleep(70);
      TInterlocked.Increment(lCallCount);
    end,
    'WaitForAsyncSupportsSimpleAsyncCall.A');
  lAsyncB := SimpleAsyncCall(
    procedure
    begin
      Sleep(130);
      TInterlocked.Increment(lCallCount);
    end,
    'WaitForAsyncSupportsSimpleAsyncCall.B');

  lWaitResult := TWaiter.WaitFor([lAsyncA, lAsyncB], cTimeoutMs, True);

  Assert.IsTrue(lWaitResult, 'TWaiter should complete for maxAsync iAsync instances');
  Assert.AreEqual<Integer>(2, lCallCount);
  Assert.IsTrue(lAsyncA.Finished);
  Assert.IsTrue(lAsyncB.Finished);
end;

procedure TWaiterVclTests.WaitForAsyncRejectsNonMaxAsyncImplementation;
var
  lAsync: iAsync;
begin
  lAsync := TNonMaxAsync.Create;

  try
    TWaiter.WaitFor([lAsync], 250, False);
    Assert.Fail('TWaiter should reject iAsync implementations that are not from maxAsync');
  except
    on lException: Exception do
      Assert.IsTrue(Pos('only supports maxAsync iAsync instances', lException.Message) > 0,
        'Unexpected exception message: ' + lException.Message);
  end;
end;

end.
