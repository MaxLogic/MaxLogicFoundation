unit MaxLogic.Async.Tests;

{$I fpc_delphimode.inc}

interface

uses
  System.Classes, System.Generics.Collections, System.Generics.Defaults, System.Math, System.SyncObjs,
  System.SysUtils,
  DUnitX.TestFramework,
  maxAsync;

type
  TAsyncWorkItem = class
  public
    Value: Integer;
    constructor Create(const aValue: Integer);
  end;

  TFindItem = class
  public
    Value: Integer;
    constructor Create(const aValue: Integer);
  end;

  [TestFixture]
  TSimpleAsyncCallTests = class
  public
    [Test] procedure SimpleAsyncCallRunsProcedure;
    [Test] procedure WakeUpRunsProcedureAgain;
    [Test] procedure WakeUpCanReplaceProcedure;
    [Test] procedure InsideMainThreadReflectsThreadContext;
    [Test] procedure GetThreadNameReturnsCurrentTaskName;
  end;

  [TestFixture]
  TAsyncLoopCoverageTests = class
  public
    [Test] procedure ExecuteAndWaitForCompletesRange;
    [Test] procedure RunAndWaitUsesDefaultThreadCountForNegativeValue;
    [Test] procedure RunSignalsOnDoneWhenBodyRaises;
    [Test] procedure CancelIsIdempotentForLargeMax;
  end;

  [TestFixture]
  TAsyncCollectionProcessorTests = class
  public
    [Test] procedure ProcessesItemsFromAllAddOverloads;
    [Test] procedure SupportsMultipleBatches;
    [Test] procedure UsesWorkerWhenThreadCountIsZero;
    [Test] procedure ProcessorCompletesWhenProcRaises;
    [Test] procedure WaitForIncludesWorkQueuedFromOnFinished;
    [Test] procedure WaitForIncludesConcurrentAddTriggeredFromOnFinished;
    [Test] procedure OnFinishedCanCallWaitForOnSameProcessor;
    [Test] procedure ConcurrentProducersProcessAllItems;
    [Test] procedure WaitForIncludesOnFinishedMultiProducerBurst;
    [Test] procedure DestroyWhileBusyCompletesWithoutException;
    [Test] procedure LongRunStressProcessesAllItems;
    [Test] procedure AddWithoutProcRaises;
  end;

  [TestFixture]
  TAsyncCollectionProcessorBoundedQueueScaffoldTests = class
  public
    [Test] procedure BoundedQueueTryAddReportsFullQueue;
    [Test] procedure BoundedQueueBlockingModeBackpressuresProducers;
    [Test] procedure TryAddRangeReturnsPartialCountWhenQueueIsFull;
    [Test] procedure LockFreeAddRangeProcessesBatchAndSignalsFinished;
    [Test] procedure LockFreeQueueModeProcessesAllItemsWithConcurrentProducers;
    [Test] procedure QueueModeCanSwitchWhenIdle;
  end;

  [TestFixture]
  TAsyncDataStructuresTests = class
  public
    [Test] procedure SafeValueNewInitializesAndStoresValue;
    [Test] procedure SafeValueSupportsConcurrentReadWrite;
    [Test] procedure SafeValueDefaultSupportsConcurrentFirstTouch;
    [Test] procedure FindInSortedListFindsItemAndInsertionIndex;
    [Test] procedure LockFreeLockGuardsCriticalSection;
  end;

implementation

constructor TAsyncWorkItem.Create(const aValue: Integer);
begin
  inherited Create;
  Value := aValue;
end;

constructor TFindItem.Create(const aValue: Integer);
begin
  inherited Create;
  Value := aValue;
end;

procedure TSimpleAsyncCallTests.SimpleAsyncCallRunsProcedure;
var
  lAsync: iAsync;
  lCalls: Integer;
begin
  lCalls := 0;
  lAsync := SimpleAsyncCall(
    procedure
    begin
      TInterlocked.Increment(lCalls);
    end,
    'SimpleAsyncCallRunsProcedure');
  lAsync.WaitFor;
  Assert.AreEqual<Integer>(1, lCalls);
  Assert.IsTrue(lAsync.Finished);
end;

procedure TSimpleAsyncCallTests.WakeUpRunsProcedureAgain;
var
  lAsync: iAsync;
  lCalls: Integer;
begin
  lCalls := 0;
  lAsync := SimpleAsyncCall(
    procedure
    begin
      TInterlocked.Increment(lCalls);
    end,
    'WakeUpRunsProcedureAgain');
  lAsync.WaitFor;

  lAsync.WakeUp;
  lAsync.WaitFor;

  Assert.AreEqual<Integer>(2, lCalls);
end;

procedure TSimpleAsyncCallTests.WakeUpCanReplaceProcedure;
var
  lAsync: iAsync;
  lValue: Integer;
begin
  lValue := 0;
  lAsync := SimpleAsyncCall(
    procedure
    begin
      lValue := 1;
    end,
    'WakeUpCanReplaceProcedure.First');
  lAsync.WaitFor;

  lAsync.WakeUp(
    procedure
    begin
      lValue := 2;
    end,
    'WakeUpCanReplaceProcedure.Second');
  lAsync.WaitFor;

  Assert.AreEqual<Integer>(2, lValue);
end;

procedure TSimpleAsyncCallTests.InsideMainThreadReflectsThreadContext;
var
  lAsync: iAsync;
  lInsideFromWorker: Boolean;
begin
  Assert.IsTrue(InsideMainThread, 'Test body should run in the main thread');

  lInsideFromWorker := True;
  lAsync := SimpleAsyncCall(
    procedure
    begin
      lInsideFromWorker := InsideMainThread;
    end,
    'InsideMainThreadReflectsThreadContext');
  lAsync.WaitFor;

  Assert.IsFalse(lInsideFromWorker, 'Worker code should not run in the main thread');
end;

procedure TSimpleAsyncCallTests.GetThreadNameReturnsCurrentTaskName;
var
  lAsync: iAsync;
  lThreadId: TThreadID;
  lTaskName: string;
  lResolvedName: string;
begin
  lThreadId := 0;
  lTaskName := 'GetThreadNameReturnsCurrentTaskName';
  lAsync := SimpleAsyncCall(
    procedure
    begin
      lThreadId := TThread.CurrentThread.ThreadId;
    end,
    lTaskName);
  lAsync.WaitFor;

  lResolvedName := TmaxAsyncGlobal.GetThreadName(lThreadId);
  Assert.AreEqual(lTaskName, lResolvedName);
end;

procedure TAsyncLoopCoverageTests.ExecuteAndWaitForCompletesRange;
var
  lLoop: TAsyncLoop;
  lMax: Integer;
  lCalls: Integer;
begin
  lLoop := TAsyncLoop.Create;
  try
    lMax := 250;
    lCalls := 0;
    lLoop.SimultanousThreadCount := 4;
    lLoop.Execute(0, lMax,
      procedure(aCurIndex: integer)
      begin
        TInterlocked.Increment(lCalls);
      end);

    lLoop.WaitFor;
    Assert.AreEqual<Integer>(lMax + 1, lCalls);
  finally
    lLoop.Free;
  end;
end;

procedure TAsyncLoopCoverageTests.RunAndWaitUsesDefaultThreadCountForNegativeValue;
var
  lCalls: Integer;
  lMax: Integer;
begin
  lCalls := 0;
  lMax := 250;
  try
    TAsyncLoop.RunAndWait(0, lMax,
      procedure(aCurIndex: integer; var aCancel: boolean)
      begin
        TInterlocked.Increment(lCalls);
        aCancel := False;
      end,
      -1);
  except
    on lException: Exception do
      Assert.Fail('Negative ThreadCount should not raise: ' + lException.ClassName + ' - ' + lException.Message);
  end;

  Assert.AreEqual<Integer>(lMax + 1, lCalls);
end;

procedure TAsyncLoopCoverageTests.RunSignalsOnDoneWhenBodyRaises;
const
  cTimeoutMs = 4000;
var
  lDone: TEvent;
begin
  lDone := TEvent.Create(nil, True, False, '');
  try
    TAsyncLoop.Run(0, 1200,
      procedure(aCurIndex: integer; var aCancel: boolean)
      begin
        aCancel := False;
        if aCurIndex = 10 then
          raise Exception.Create('Expected async loop exception');
      end,
      procedure
      begin
        lDone.SetEvent;
      end,
      4);

    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs),
      'OnDone should be signaled even when a worker callback raises');
  finally
    lDone.Free;
  end;
end;

procedure TAsyncLoopCoverageTests.CancelIsIdempotentForLargeMax;
const
  cTimeoutMs = 4000;
var
  lLoop: TAsyncLoop;
  lDone: TEvent;
  lIterations: Integer;
begin
  lLoop := TAsyncLoop.Create;
  lDone := TEvent.Create(nil, True, False, '');
  try
    lIterations := 0;
    lLoop.SimultanousThreadCount := 1;
    lLoop.OnFinished :=
      procedure
      begin
        lDone.SetEvent;
      end;

    lLoop.Execute(0, High(Integer) - 2,
      procedure(aCurIndex: integer)
      begin
        if TInterlocked.Increment(lIterations) = 1 then
        begin
          lLoop.Cancel;
          lLoop.Cancel;
        end;

        if lIterations > 50 then
          raise Exception.Create('Cancel is not idempotent for large max values');
      end);

    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs),
      'Loop should finish quickly after repeated cancel');
    lLoop.WaitFor;
    Assert.IsTrue(lIterations <= 3,
      Format('Too many iterations after repeated cancel: %d', [lIterations]));
  finally
    lDone.Free;
    lLoop.Free;
  end;
end;

procedure TAsyncCollectionProcessorTests.ProcessesItemsFromAllAddOverloads;
const
  cTimeoutMs = 4000;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
  lDone: TEvent;
  lList: TList<TAsyncWorkItem>;
  lArray: TArray<TAsyncWorkItem>;
  lProcessedCount: Integer;
  lProcessedSum: Integer;
  lWaitResult: TWaitResult;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  lDone := TEvent.Create(nil, True, False, '');
  lList := TList<TAsyncWorkItem>.Create;
  try
    lProcessedCount := 0;
    lProcessedSum := 0;

    lProcessor.SimultanousThreadCount := 4;
    lProcessor.Proc :=
      procedure(const aItem: TAsyncWorkItem)
      begin
        try
          TInterlocked.Increment(lProcessedCount);
          TInterlocked.Add(lProcessedSum, aItem.Value);
        finally
          aItem.Free;
        end;
      end;
    lProcessor.OnFinished :=
      procedure
      begin
        lDone.SetEvent;
      end;

    SetLength(lArray, 2);
    lArray[0] := TAsyncWorkItem.Create(2);
    lArray[1] := TAsyncWorkItem.Create(3);

    lList.Add(TAsyncWorkItem.Create(5));
    lList.Add(TAsyncWorkItem.Create(7));

    lProcessor.Add(TAsyncWorkItem.Create(1));
    lProcessor.Add(lArray);
    lProcessor.Add(lList);

    lWaitResult := lDone.WaitFor(cTimeoutMs);
    if lWaitResult = wrSignaled then
      lProcessor.WaitFor;

    Assert.AreEqual<TWaitResult>(wrSignaled, lWaitResult);
    Assert.AreEqual<Integer>(5, lProcessedCount);
    Assert.AreEqual<Integer>(18, lProcessedSum);
  finally
    lList.Free;
    lDone.Free;
    lProcessor.Free;
  end;
end;

procedure TAsyncCollectionProcessorTests.SupportsMultipleBatches;
const
  cTimeoutMs = 4000;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
  lDone: TEvent;
  lDoneCount: Integer;
  lProcessedCount: Integer;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  lDone := TEvent.Create(nil, True, False, '');
  try
    lDoneCount := 0;
    lProcessedCount := 0;
    lProcessor.SimultanousThreadCount := 3;
    lProcessor.Proc :=
      procedure(const aItem: TAsyncWorkItem)
      begin
        try
          TInterlocked.Increment(lProcessedCount);
        finally
          aItem.Free;
        end;
      end;
    lProcessor.OnFinished :=
      procedure
      begin
        TInterlocked.Increment(lDoneCount);
        lDone.SetEvent;
      end;

    lDone.ResetEvent;
    lProcessor.Add(TAsyncWorkItem.Create(1));
    lProcessor.Add(TAsyncWorkItem.Create(2));
    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs));
    lProcessor.WaitFor;

    lDone.ResetEvent;
    lProcessor.Add(TAsyncWorkItem.Create(3));
    lProcessor.Add(TAsyncWorkItem.Create(4));
    lProcessor.Add(TAsyncWorkItem.Create(5));
    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs));
    lProcessor.WaitFor;

    Assert.AreEqual<Integer>(5, lProcessedCount);
    Assert.IsTrue(lDoneCount >= 2,
      Format('OnFinished should run at least once per explicit batch, got %d calls.', [lDoneCount]));
  finally
    lDone.Free;
    lProcessor.Free;
  end;
end;

procedure TAsyncCollectionProcessorTests.UsesWorkerWhenThreadCountIsZero;
const
  cTimeoutMs = 2000;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
  lDone: TEvent;
  lProcessedCount: Integer;
  lWaitResult: TWaitResult;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  lDone := TEvent.Create(nil, True, False, '');
  try
    lProcessedCount := 0;
    lProcessor.Proc :=
      procedure(const aItem: TAsyncWorkItem)
      begin
        try
          TInterlocked.Increment(lProcessedCount);
        finally
          aItem.Free;
        end;
      end;
    lProcessor.OnFinished :=
      procedure
      begin
        lDone.SetEvent;
      end;

    lProcessor.SimultanousThreadCount := 0;
    lProcessor.Add(TAsyncWorkItem.Create(1));
    lWaitResult := lDone.WaitFor(cTimeoutMs);

    if lWaitResult <> wrSignaled then
    begin
      lProcessor.SimultanousThreadCount := 1;
      lProcessor.Add(TAsyncWorkItem.Create(2));
      lDone.WaitFor(cTimeoutMs);
    end;
    lProcessor.WaitFor;

    Assert.AreEqual<TWaitResult>(wrSignaled, lWaitResult);
    Assert.AreEqual<Integer>(1, lProcessedCount);
  finally
    lDone.Free;
    lProcessor.Free;
  end;
end;

procedure TAsyncCollectionProcessorTests.ProcessorCompletesWhenProcRaises;
const
  cTimeoutMs = 4000;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
  lDone: TEvent;
  lProcessedCount: Integer;
  lCallNo: Integer;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  lDone := TEvent.Create(nil, True, False, '');
  try
    lProcessedCount := 0;
    lCallNo := 0;

    lProcessor.SimultanousThreadCount := 2;
    lProcessor.Proc :=
      procedure(const aItem: TAsyncWorkItem)
      var
        lCurrentCallNo: Integer;
      begin
        try
          lCurrentCallNo := TInterlocked.Increment(lCallNo);
          TInterlocked.Increment(lProcessedCount);
          if lCurrentCallNo = 1 then
            raise Exception.Create('Expected processor exception');
        finally
          aItem.Free;
        end;
      end;
    lProcessor.OnFinished :=
      procedure
      begin
        lDone.SetEvent;
      end;

    lProcessor.Add(TAsyncWorkItem.Create(1));
    lProcessor.Add(TAsyncWorkItem.Create(2));
    lProcessor.Add(TAsyncWorkItem.Create(3));
    lProcessor.Add(TAsyncWorkItem.Create(4));
    lProcessor.Add(TAsyncWorkItem.Create(5));

    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs),
      'Processor should still complete when one callback raises');
    lProcessor.WaitFor;
    Assert.AreEqual<Integer>(5, lProcessedCount);
  finally
    lDone.Free;
    lProcessor.Free;
  end;
end;

procedure TAsyncCollectionProcessorTests.WaitForIncludesWorkQueuedFromOnFinished;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
  lAllDone: TEvent;
  lProcessedCount: Integer;
  lBatchNo: Integer;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  lAllDone := TEvent.Create(nil, True, False, '');
  try
    lProcessedCount := 0;
    lBatchNo := 0;

    lProcessor.SimultanousThreadCount := 1;
    lProcessor.Proc :=
      procedure(const aItem: TAsyncWorkItem)
      begin
        try
          Sleep(120);
          TInterlocked.Increment(lProcessedCount);
        finally
          aItem.Free;
        end;
      end;
    lProcessor.OnFinished :=
      procedure
      var
        lCurrentBatch: Integer;
      begin
        lCurrentBatch := TInterlocked.Increment(lBatchNo);
        if lCurrentBatch = 1 then
          lProcessor.Add(TAsyncWorkItem.Create(2))
        else if lCurrentBatch = 2 then
          lAllDone.SetEvent;
      end;

    lProcessor.Add(TAsyncWorkItem.Create(1));
    lProcessor.WaitFor;
    Assert.AreEqual<TWaitResult>(wrSignaled, lAllDone.WaitFor(0),
      'WaitFor returned before the batch queued from OnFinished was completed');

    Assert.AreEqual<Integer>(2, lProcessedCount,
      'WaitFor should include work that OnFinished queued before returning');
  finally
    lAllDone.Free;
    lProcessor.Free;
  end;
end;

procedure TAsyncCollectionProcessorTests.WaitForIncludesConcurrentAddTriggeredFromOnFinished;
const
  cTimeoutMs = 6000;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
  lAllDone: TEvent;
  lAdderDone: TEvent;
  lOnFinishedCalls: Integer;
  lProcessedCount: Integer;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  lAllDone := TEvent.Create(nil, True, False, '');
  lAdderDone := TEvent.Create(nil, True, False, '');
  try
    lOnFinishedCalls := 0;
    lProcessedCount := 0;

    lProcessor.SimultanousThreadCount := 1;
    lProcessor.Proc :=
      procedure(const aItem: TAsyncWorkItem)
      begin
        try
          TInterlocked.Increment(lProcessedCount);
        finally
          aItem.Free;
        end;
      end;
    lProcessor.OnFinished :=
      procedure
      var
        lCallNo: Integer;
      begin
        lCallNo := TInterlocked.Increment(lOnFinishedCalls);
        if lCallNo = 1 then
        begin
          lAdderDone.ResetEvent;
          TThread.CreateAnonymousThread(
            procedure
            begin
              try
                lProcessor.Add(TAsyncWorkItem.Create(2));
              finally
                lAdderDone.SetEvent;
              end;
            end).Start;

          if lAdderDone.WaitFor(cTimeoutMs) <> wrSignaled then
            raise Exception.Create('Timed out while adding work from OnFinished');
        end
        else if lCallNo = 2 then
          lAllDone.SetEvent;
      end;

    lProcessor.Add(TAsyncWorkItem.Create(1));
    lProcessor.WaitFor;

    Assert.AreEqual<TWaitResult>(wrSignaled, lAllDone.WaitFor(0),
      'WaitFor returned before work added during OnFinished was completed');
    Assert.AreEqual<Integer>(2, lProcessedCount);
  finally
    lAdderDone.Free;
    lAllDone.Free;
    lProcessor.Free;
  end;
end;

procedure TAsyncCollectionProcessorTests.OnFinishedCanCallWaitForOnSameProcessor;
const
  cInitialTimeoutMs = 300;
  cCleanupTimeoutMs = 4000;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
  lOnFinishedEntered: TEvent;
  lOnFinishedReturned: TEvent;
  lCleanupDone: TEvent;
  lOnFinishedCalls: Integer;
  lWaitResult: TWaitResult;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  lOnFinishedEntered := TEvent.Create(nil, True, False, '');
  lOnFinishedReturned := TEvent.Create(nil, True, False, '');
  lCleanupDone := TEvent.Create(nil, True, False, '');
  try
    lOnFinishedCalls := 0;
    lProcessor.SimultanousThreadCount := 1;
    lProcessor.Proc :=
      procedure(const aItem: TAsyncWorkItem)
      begin
        aItem.Free;
      end;
    lProcessor.OnFinished :=
      procedure
      var
        lCallNo: Integer;
      begin
        lCallNo := TInterlocked.Increment(lOnFinishedCalls);
        if lCallNo = 1 then
        begin
          lOnFinishedEntered.SetEvent;
          lProcessor.WaitFor;
          lOnFinishedReturned.SetEvent;
        end
        else
          lCleanupDone.SetEvent;
      end;

    lProcessor.Add(TAsyncWorkItem.Create(1));
    Assert.AreEqual<TWaitResult>(wrSignaled, lOnFinishedEntered.WaitFor(cCleanupTimeoutMs),
      'OnFinished was not entered');

    lWaitResult := lOnFinishedReturned.WaitFor(cInitialTimeoutMs);
    if lWaitResult <> wrSignaled then
    begin
      // unblock potential deadlock path for cleanup
      lProcessor.Add(TAsyncWorkItem.Create(2));
      Assert.AreEqual<TWaitResult>(wrSignaled, lCleanupDone.WaitFor(cCleanupTimeoutMs),
        'Cleanup OnFinished did not run');
      Assert.AreEqual<TWaitResult>(wrSignaled, lOnFinishedReturned.WaitFor(cCleanupTimeoutMs),
        'Initial OnFinished did not return after cleanup work');
    end;

    Assert.AreEqual<TWaitResult>(wrSignaled, lWaitResult,
      'WaitFor called from OnFinished on the same processor should return immediately');
    lProcessor.WaitFor;
  finally
    lCleanupDone.Free;
    lOnFinishedReturned.Free;
    lOnFinishedEntered.Free;
    lProcessor.Free;
  end;
end;

procedure TAsyncCollectionProcessorTests.ConcurrentProducersProcessAllItems;
const
  cProducerCount = 8;
  cItemsPerProducer = 500;
  cTimeoutMs = 12000;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
  lDone: TEvent;
  lProcessedCount: Integer;
  lAddErrors: Integer;
  lProducers: TArray<iAsync>;
  lProducer: Integer;
  i: Integer;
  lExpectedCount: Integer;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  lDone := TEvent.Create(nil, True, False, '');
  try
    lProcessedCount := 0;
    lAddErrors := 0;
    lExpectedCount := cProducerCount * cItemsPerProducer;

    lProcessor.SimultanousThreadCount := Max(2, TThread.ProcessorCount);
    lProcessor.Proc :=
      procedure(const aItem: TAsyncWorkItem)
      begin
        try
          TInterlocked.Increment(lProcessedCount);
        finally
          aItem.Free;
        end;
      end;
    lProcessor.OnFinished :=
      procedure
      begin
        lDone.SetEvent;
      end;

    SetLength(lProducers, cProducerCount);
    for lProducer := 0 to High(lProducers) do
      lProducers[lProducer] := SimpleAsyncCall(
        procedure
        var
          lItem: TAsyncWorkItem;
          j: Integer;
        begin
          for j := 1 to cItemsPerProducer do
          begin
            lItem := TAsyncWorkItem.Create(j);
            try
              lProcessor.Add(lItem);
            except
              lItem.Free;
              TInterlocked.Increment(lAddErrors);
            end;
          end;
        end,
        'ConcurrentProducersProcessAllItems.' + IntToStr(lProducer));

    for i := 0 to High(lProducers) do
      lProducers[i].WaitFor;

    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs),
      'Processor did not report completion for concurrent producers');
    lProcessor.WaitFor;

    Assert.AreEqual<Integer>(0, lAddErrors);
    Assert.AreEqual<Integer>(lExpectedCount, lProcessedCount);
  finally
    lDone.Free;
    lProcessor.Free;
  end;
end;

procedure TAsyncCollectionProcessorTests.WaitForIncludesOnFinishedMultiProducerBurst;
const
  cTimeoutMs = 15000;
  cProducerCount = 4;
  cItemsPerProducer = 300;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
  lDone: TEvent;
  lOnFinishedCalls: Integer;
  lProcessedCount: Integer;
  lExpectedCount: Integer;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  lDone := TEvent.Create(nil, True, False, '');
  try
    lOnFinishedCalls := 0;
    lProcessedCount := 0;
    lExpectedCount := 1 + (cProducerCount * cItemsPerProducer);

    lProcessor.SimultanousThreadCount := Max(2, TThread.ProcessorCount);
    lProcessor.Proc :=
      procedure(const aItem: TAsyncWorkItem)
      begin
        try
          TInterlocked.Increment(lProcessedCount);
        finally
          aItem.Free;
        end;
      end;
    lProcessor.OnFinished :=
      procedure
      var
        lCallNo: Integer;
        lProducer: Integer;
        lProducers: TArray<iAsync>;
        i: Integer;
      begin
        lCallNo := TInterlocked.Increment(lOnFinishedCalls);
        if lCallNo = 1 then
        begin
          SetLength(lProducers, cProducerCount);
          for lProducer := 0 to High(lProducers) do
            lProducers[lProducer] := SimpleAsyncCall(
              procedure
              var
                j: Integer;
              begin
                for j := 1 to cItemsPerProducer do
                  lProcessor.Add(TAsyncWorkItem.Create(j));
              end,
              'WaitForIncludesOnFinishedMultiProducerBurst.' + IntToStr(lProducer));

          for i := 0 to High(lProducers) do
            lProducers[i].WaitFor;
        end else begin
          lDone.SetEvent;
        end;
      end;

    lProcessor.Add(TAsyncWorkItem.Create(1));
    lProcessor.WaitFor;

    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(0),
      'WaitFor returned before multi-producer burst triggered from OnFinished completed');
    Assert.AreEqual<Integer>(lExpectedCount, lProcessedCount);
    Assert.AreEqual<Integer>(2, lOnFinishedCalls,
      'Processor should become idle twice: initial item, then producer burst');
  finally
    lDone.Free;
    lProcessor.Free;
  end;
end;

procedure TAsyncCollectionProcessorTests.DestroyWhileBusyCompletesWithoutException;
const
  cItemCount = 64;
  cTimeoutMs = 15000;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
  lDestroyDone: TEvent;
  lDestroyAsync: iAsync;
  lDestroyError: string;
  lProcessedCount: Integer;
  lAlmostDone: TEvent;
  i: Integer;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  lDestroyDone := TEvent.Create(nil, True, False, '');
  lAlmostDone := TEvent.Create(nil, True, False, '');
  lDestroyAsync := nil;
  try
    lDestroyError := '';
    lProcessedCount := 0;

    lProcessor.SimultanousThreadCount := Max(2, TThread.ProcessorCount);
    lProcessor.Proc :=
      procedure(const aItem: TAsyncWorkItem)
      begin
        try
          Sleep(1);
          if TInterlocked.Increment(lProcessedCount) >= (cItemCount - 1) then
            lAlmostDone.SetEvent;
        finally
          aItem.Free;
        end;
      end;

    for i := 1 to cItemCount do
      lProcessor.Add(TAsyncWorkItem.Create(i));

    Assert.AreEqual<TWaitResult>(wrSignaled, lAlmostDone.WaitFor(cTimeoutMs),
      'Processor did not reach near-complete state in time');

    lDestroyAsync := SimpleAsyncCall(
      procedure
      begin
        try
          lProcessor.Free;
          lProcessor := nil;
        except
          on lException: Exception do
            lDestroyError := lException.ClassName + ': ' + lException.Message;
        end;
        lDestroyDone.SetEvent;
      end,
      'DestroyWhileBusyCompletesWithoutException.Destroyer');

    Assert.AreEqual<TWaitResult>(wrSignaled, lDestroyDone.WaitFor(cTimeoutMs),
      'Destroy while busy timed out');
    if lDestroyAsync <> nil then
      lDestroyAsync.WaitFor;

    Assert.AreEqual('', lDestroyError);
    Assert.IsTrue(lProcessedCount > 0, 'Processor should process some items before shutdown completes');
  finally
    lAlmostDone.Free;
    lDestroyDone.Free;
    if lProcessor <> nil then
      lProcessor.Free;
  end;
end;

procedure TAsyncCollectionProcessorTests.LongRunStressProcessesAllItems;
const
  cTotalItems = 30000;
  cBatchSize = 128;
  cTimeoutMs = 25000;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
  lDone: TEvent;
  lProcessedCount: Integer;
  lCurrentValue: Integer;
  lBatchCount: Integer;
  lBatch: TArray<TAsyncWorkItem>;
  i: Integer;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  lDone := TEvent.Create(nil, True, False, '');
  try
    lProcessedCount := 0;
    lCurrentValue := 1;

    lProcessor.SimultanousThreadCount := Max(2, TThread.ProcessorCount);
    lProcessor.Proc :=
      procedure(const aItem: TAsyncWorkItem)
      begin
        try
          TInterlocked.Increment(lProcessedCount);
        finally
          aItem.Free;
        end;
      end;
    lProcessor.OnFinished :=
      procedure
      begin
        lDone.SetEvent;
      end;

    while lCurrentValue <= cTotalItems do
    begin
      lBatchCount := Min(cBatchSize, cTotalItems - lCurrentValue + 1);
      SetLength(lBatch, lBatchCount);
      for i := 0 to lBatchCount - 1 do
        lBatch[i] := TAsyncWorkItem.Create(lCurrentValue + i);

      lProcessor.Add(lBatch);
      Inc(lCurrentValue, lBatchCount);
    end;

    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs),
      'Long-run stress batch did not complete in time');
    lProcessor.WaitFor;
    Assert.AreEqual<Integer>(cTotalItems, lProcessedCount);
  finally
    lDone.Free;
    lProcessor.Free;
  end;
end;

procedure TAsyncCollectionProcessorTests.AddWithoutProcRaises;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        lProcessor.Add(TAsyncWorkItem(nil));
      end, Exception);
  finally
    lProcessor.Free;
  end;
end;

procedure TAsyncCollectionProcessorBoundedQueueScaffoldTests.BoundedQueueTryAddReportsFullQueue;
const
  cTimeoutMs = 6000;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
  lAllowProcessing: TEvent;
  lFirstItemStarted: TEvent;
  lDone: TEvent;
  lRejectedItem: TAsyncWorkItem;
  lTryAddResult: Boolean;
  lProcessedCount: Integer;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  lAllowProcessing := TEvent.Create(nil, True, False, '');
  lFirstItemStarted := TEvent.Create(nil, True, False, '');
  lDone := TEvent.Create(nil, True, False, '');
  try
    lProcessedCount := 0;
    lProcessor.SimultanousThreadCount := 1;
    lProcessor.QueueMode := acpqmLockedRingQueue;
    lProcessor.QueueCapacity := 2;
    lProcessor.BackpressureMode := acpbmReject;
    lProcessor.Proc :=
      procedure(const aItem: TAsyncWorkItem)
      begin
        try
          lFirstItemStarted.SetEvent;
          Assert.AreEqual<TWaitResult>(wrSignaled, lAllowProcessing.WaitFor(cTimeoutMs),
            'Timed out while waiting to release bounded queue worker.');
          TInterlocked.Increment(lProcessedCount);
        finally
          aItem.Free;
        end;
      end;
    lProcessor.OnFinished :=
      procedure
      begin
        lDone.SetEvent;
      end;

    lProcessor.Add(TAsyncWorkItem.Create(1)); // active worker item
    Assert.AreEqual<TWaitResult>(wrSignaled, lFirstItemStarted.WaitFor(cTimeoutMs),
      'Worker did not start processing the first bounded-queue item.');
    lProcessor.Add(TAsyncWorkItem.Create(2)); // queue slot 1
    lProcessor.Add(TAsyncWorkItem.Create(3)); // queue slot 2 (full)

    lRejectedItem := TAsyncWorkItem.Create(4);
    lTryAddResult := lProcessor.TryAdd(lRejectedItem);
    if lTryAddResult then
      Assert.Fail('TryAdd should return False when bounded queue is full.');
    if not lTryAddResult then
      lRejectedItem.Free;

    lAllowProcessing.SetEvent;
    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs));
    lProcessor.WaitFor;
    Assert.AreEqual<Integer>(3, lProcessedCount);
  finally
    lDone.Free;
    lFirstItemStarted.Free;
    lAllowProcessing.Free;
    lProcessor.Free;
  end;
end;

procedure TAsyncCollectionProcessorBoundedQueueScaffoldTests.BoundedQueueBlockingModeBackpressuresProducers;
const
  cTimeoutMs = 6000;
  cShortTimeoutMs = 250;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
  lAllowProcessing: TEvent;
  lFirstItemStarted: TEvent;
  lProducerDone: TEvent;
  lDone: TEvent;
  lProducerAsync: iAsync;
  lProcessedCount: Integer;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  lAllowProcessing := TEvent.Create(nil, True, False, '');
  lFirstItemStarted := TEvent.Create(nil, True, False, '');
  lProducerDone := TEvent.Create(nil, True, False, '');
  lDone := TEvent.Create(nil, True, False, '');
  try
    lProcessedCount := 0;
    lProcessor.SimultanousThreadCount := 1;
    lProcessor.QueueMode := acpqmLockedRingQueue;
    lProcessor.QueueCapacity := 1;
    lProcessor.BackpressureMode := acpbmBlock;
    lProcessor.Proc :=
      procedure(const aItem: TAsyncWorkItem)
      begin
        try
          lFirstItemStarted.SetEvent;
          Assert.AreEqual<TWaitResult>(wrSignaled, lAllowProcessing.WaitFor(cTimeoutMs),
            'Timed out while waiting to release bounded queue worker.');
          TInterlocked.Increment(lProcessedCount);
        finally
          aItem.Free;
        end;
      end;
    lProcessor.OnFinished :=
      procedure
      begin
        lDone.SetEvent;
      end;

    lProcessor.Add(TAsyncWorkItem.Create(1)); // active worker
    Assert.AreEqual<TWaitResult>(wrSignaled, lFirstItemStarted.WaitFor(cTimeoutMs),
      'Worker did not start processing the first bounded-queue item.');
    lProducerAsync := SimpleAsyncCall(
      procedure
      begin
        lProcessor.Add(TAsyncWorkItem.Create(2)); // queue slot (full)
        lProcessor.Add(TAsyncWorkItem.Create(3)); // should block until space frees
        lProducerDone.SetEvent;
      end,
      'BoundedQueueBlockingModeBackpressuresProducers');

    Assert.AreEqual<TWaitResult>(wrTimeout, lProducerDone.WaitFor(cShortTimeoutMs),
      'Producer should block when queue is full in blocking mode.');

    lAllowProcessing.SetEvent;
    Assert.AreEqual<TWaitResult>(wrSignaled, lProducerDone.WaitFor(cTimeoutMs),
      'Producer did not resume after queue space became available.');
    lProducerAsync.WaitFor;
    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs));
    lProcessor.WaitFor;
    Assert.AreEqual<Integer>(3, lProcessedCount);
  finally
    lDone.Free;
    lProducerDone.Free;
    lFirstItemStarted.Free;
    lAllowProcessing.Free;
    lProcessor.Free;
  end;
end;

procedure TAsyncCollectionProcessorBoundedQueueScaffoldTests.TryAddRangeReturnsPartialCountWhenQueueIsFull;
const
  cTimeoutMs = 6000;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
  lAllowProcessing: TEvent;
  lFirstItemStarted: TEvent;
  lDone: TEvent;
  lBatch: TArray<TAsyncWorkItem>;
  lAddedCount: Integer;
  lProcessedCount: Integer;
  i: Integer;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  lAllowProcessing := TEvent.Create(nil, True, False, '');
  lFirstItemStarted := TEvent.Create(nil, True, False, '');
  lDone := TEvent.Create(nil, True, False, '');
  try
    lProcessedCount := 0;
    lProcessor.SimultanousThreadCount := 1;
    lProcessor.QueueMode := acpqmLockFreeMpmcRingQueue;
    lProcessor.QueueCapacity := 2;
    lProcessor.BackpressureMode := acpbmReject;
    lProcessor.Proc :=
      procedure(const aItem: TAsyncWorkItem)
      begin
        try
          lFirstItemStarted.SetEvent;
          Assert.AreEqual<TWaitResult>(wrSignaled, lAllowProcessing.WaitFor(cTimeoutMs),
            'Timed out while waiting to release lock-free queue worker.');
          TInterlocked.Increment(lProcessedCount);
        finally
          aItem.Free;
        end;
      end;
    lProcessor.OnFinished :=
      procedure
      begin
        lDone.SetEvent;
      end;

    lProcessor.Add(TAsyncWorkItem.Create(1)); // active worker item
    Assert.AreEqual<TWaitResult>(wrSignaled, lFirstItemStarted.WaitFor(cTimeoutMs),
      'Worker did not start processing the first lock-free item.');
    lProcessor.Add(TAsyncWorkItem.Create(2)); // queued item

    SetLength(lBatch, 3);
    for i := 0 to High(lBatch) do
      lBatch[i] := TAsyncWorkItem.Create(100 + i);

    lAddedCount := lProcessor.TryAddRange(lBatch);
    Assert.AreEqual<Integer>(1, lAddedCount,
      'TryAddRange should accept only the remaining free slot when queue is full.');

    for i := lAddedCount to High(lBatch) do
      lBatch[i].Free;

    lAllowProcessing.SetEvent;
    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs),
      'Processor did not finish after releasing queue worker.');
    lProcessor.WaitFor;
    Assert.AreEqual<Integer>(3, lProcessedCount);
  finally
    lDone.Free;
    lFirstItemStarted.Free;
    lAllowProcessing.Free;
    lProcessor.Free;
  end;
end;

procedure TAsyncCollectionProcessorBoundedQueueScaffoldTests.LockFreeAddRangeProcessesBatchAndSignalsFinished;
const
  cItemCount = 1200;
  cTimeoutMs = 12000;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
  lDone: TEvent;
  lItems: TArray<TAsyncWorkItem>;
  lProcessedCount: Integer;
  i: Integer;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  lDone := TEvent.Create(nil, True, False, '');
  try
    lProcessedCount := 0;
    lProcessor.SimultanousThreadCount := Max(2, TThread.ProcessorCount);
    lProcessor.QueueMode := acpqmLockFreeMpmcRingQueue;
    lProcessor.QueueCapacity := 2048;
    lProcessor.BackpressureMode := acpbmBlock;
    lProcessor.Proc :=
      procedure(const aItem: TAsyncWorkItem)
      begin
        try
          TInterlocked.Increment(lProcessedCount);
        finally
          aItem.Free;
        end;
      end;
    lProcessor.OnFinished :=
      procedure
      begin
        lDone.SetEvent;
      end;

    SetLength(lItems, cItemCount);
    for i := 0 to High(lItems) do
      lItems[i] := TAsyncWorkItem.Create(i + 1);

    lProcessor.AddRange(lItems);
    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs),
      'Lock-free AddRange batch did not complete in time.');
    lProcessor.WaitFor;
    Assert.AreEqual<Integer>(cItemCount, lProcessedCount);
  finally
    lDone.Free;
    lProcessor.Free;
  end;
end;

procedure TAsyncCollectionProcessorBoundedQueueScaffoldTests.LockFreeQueueModeProcessesAllItemsWithConcurrentProducers;
const
  cProducerCount = 4;
  cItemsPerProducer = 2000;
  cTimeoutMs = 15000;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
  lDone: TEvent;
  lProcessedCount: Integer;
  lExpectedCount: Integer;
  lAddErrors: Integer;
  lProducers: TArray<iAsync>;
  lProducer: Integer;
  i: Integer;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  lDone := TEvent.Create(nil, True, False, '');
  try
    lProcessedCount := 0;
    lAddErrors := 0;
    lExpectedCount := cProducerCount * cItemsPerProducer;

    lProcessor.SimultanousThreadCount := Max(2, TThread.ProcessorCount);
    lProcessor.QueueMode := acpqmLockFreeMpmcRingQueue;
    lProcessor.QueueCapacity := 1024;
    lProcessor.BackpressureMode := acpbmBlock;
    lProcessor.Proc :=
      procedure(const aItem: TAsyncWorkItem)
      begin
        try
          TInterlocked.Increment(lProcessedCount);
        finally
          aItem.Free;
        end;
      end;
    lProcessor.OnFinished :=
      procedure
      begin
        lDone.SetEvent;
      end;

    SetLength(lProducers, cProducerCount);
    for lProducer := 0 to High(lProducers) do
      lProducers[lProducer] := SimpleAsyncCall(
        procedure
        var
          j: Integer;
          lItem: TAsyncWorkItem;
        begin
          for j := 1 to cItemsPerProducer do
          begin
            lItem := TAsyncWorkItem.Create(j);
            try
              lProcessor.Add(lItem);
            except
              lItem.Free;
              TInterlocked.Increment(lAddErrors);
            end;
          end;
        end,
        'LockFreeQueueModeProcessesAllItemsWithConcurrentProducers.' + IntToStr(lProducer));

    for i := 0 to High(lProducers) do
      lProducers[i].WaitFor;

    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs));
    lProcessor.WaitFor;
    Assert.AreEqual<Integer>(0, lAddErrors);
    Assert.AreEqual<Integer>(lExpectedCount, lProcessedCount);
  finally
    lDone.Free;
    lProcessor.Free;
  end;
end;

procedure TAsyncCollectionProcessorBoundedQueueScaffoldTests.QueueModeCanSwitchWhenIdle;
const
  cTimeoutMs = 6000;
var
  lProcessor: TAsyncCollectionProcessor<TAsyncWorkItem>;
  lDone: TEvent;
  lProcessedCount: Integer;
begin
  lProcessor := TAsyncCollectionProcessor<TAsyncWorkItem>.Create;
  lDone := TEvent.Create(nil, True, False, '');
  try
    lProcessedCount := 0;
    lProcessor.SimultanousThreadCount := 2;
    lProcessor.Proc :=
      procedure(const aItem: TAsyncWorkItem)
      begin
        try
          TInterlocked.Increment(lProcessedCount);
        finally
          aItem.Free;
        end;
      end;
    lProcessor.OnFinished :=
      procedure
      begin
        lDone.SetEvent;
      end;

    lProcessor.QueueMode := acpqmLockFreeMpmcRingQueue;
    lProcessor.QueueCapacity := 512;
    lDone.ResetEvent;
    lProcessor.Add(TAsyncWorkItem.Create(1));
    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs));
    lProcessor.WaitFor;

    lProcessor.QueueMode := acpqmLegacyLockedQueue;
    lDone.ResetEvent;
    lProcessor.Add(TAsyncWorkItem.Create(2));
    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs));
    lProcessor.WaitFor;

    Assert.AreEqual<Integer>(2, lProcessedCount);
  finally
    lDone.Free;
    lProcessor.Free;
  end;
end;

procedure TAsyncDataStructuresTests.SafeValueNewInitializesAndStoresValue;
var
  lSafe: TSafeValue<Integer>;
begin
  lSafe := TSafeValue<Integer>.new;
  Assert.AreEqual<Integer>(0, lSafe.Value);
  lSafe.Value := 42;
  Assert.AreEqual<Integer>(42, lSafe.Value);
end;

procedure TAsyncDataStructuresTests.SafeValueSupportsConcurrentReadWrite;
var
  lSafe: TSafeValue<Integer>;
  lMax: Integer;
begin
  lSafe := TSafeValue<Integer>.new;
  lMax := 2000;
  TAsyncLoop.RunAndWait(0, lMax,
    procedure(aCurIndex: integer; var aCancel: boolean)
    var
      lCurrent: Integer;
    begin
      if (aCurIndex and 1) = 0 then
        lSafe.Value := aCurIndex
      else
        lCurrent := lSafe.Value;
      aCancel := False;
    end,
    8);

  Assert.IsTrue((lSafe.Value >= 0) and (lSafe.Value <= lMax));
end;

procedure TAsyncDataStructuresTests.SafeValueDefaultSupportsConcurrentFirstTouch;
var
  lSafe: TSafeValue<Integer>;
  lMax: Integer;
  lErrorCount: Integer;
begin
  lSafe := default(TSafeValue<Integer>);
  lMax := 3000;
  lErrorCount := 0;

  TAsyncLoop.RunAndWait(0, lMax,
    procedure(aCurIndex: integer; var aCancel: boolean)
    var
      lCurrent: Integer;
    begin
      try
        if (aCurIndex and 1) = 0 then
          lSafe.Value := aCurIndex
        else
          lCurrent := lSafe.Value;
      except
        TInterlocked.Increment(lErrorCount);
      end;
      aCancel := False;
    end,
    8);

  Assert.AreEqual<Integer>(0, lErrorCount,
    'Concurrent first-touch initialization of TSafeValue raised an exception');
end;

procedure TAsyncDataStructuresTests.FindInSortedListFindsItemAndInsertionIndex;
var
  lList: TList<TFindItem>;
  lComparer: IComparer<TFindItem>;
  lFinder: TFindInSortedList<TFindItem>;
  lNeedle: TFindItem;
  lIndex: Integer;
  lFound: Boolean;
begin
  lList := TList<TFindItem>.Create;
  try
    lComparer := TComparer<TFindItem>.Construct(
      function(const aLeft, aRight: TFindItem): Integer
      begin
        Result := aLeft.Value - aRight.Value;
      end);
    lFinder := TFindInSortedList<TFindItem>.Create(lList, lComparer);
    try
      lList.Add(TFindItem.Create(10));
      lList.Add(TFindItem.Create(20));
      lList.Add(TFindItem.Create(30));

      lNeedle := TFindItem.Create(20);
      try
        lFound := lFinder.find(lNeedle, lIndex);
      finally
        lNeedle.Free;
      end;
      Assert.IsTrue(lFound);
      Assert.AreEqual<Integer>(1, lIndex);

      lNeedle := TFindItem.Create(25);
      try
        lFound := lFinder.find(lNeedle, lIndex);
      finally
        lNeedle.Free;
      end;
      Assert.IsFalse(lFound);
      Assert.AreEqual<Integer>(2, lIndex);
    finally
      lFinder.Free;
    end;
  finally
    while lList.Count > 0 do
    begin
      lList.Last.Free;
      lList.Delete(lList.Count - 1);
    end;
    lList.Free;
  end;
end;

procedure TAsyncDataStructuresTests.LockFreeLockGuardsCriticalSection;
const
  cIterations = 5000;
var
  lLock: TLockFreeLock;
  lCounter: Integer;
begin
  lLock := default(TLockFreeLock);
  lCounter := 0;
  TAsyncLoop.RunAndWait(1, cIterations,
    procedure(aCurIndex: integer; var aCancel: boolean)
    begin
      lLock.lock;
      try
        Inc(lCounter);
      finally
        lLock.unlock;
      end;
      aCancel := False;
    end,
    8);

  Assert.AreEqual<Integer>(cIterations, lCounter);
end;

end.
