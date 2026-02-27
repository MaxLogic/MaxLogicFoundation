program MaxLogicAsyncBenchmark;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.Classes,
  System.Diagnostics,
  System.Math,
  System.SyncObjs,
  System.SysUtils,
  maxAsync in '..\maxAsync.pas',
  MaxLogic.PortableTimer in '..\MaxLogic.PortableTimer.pas';

type
  TBenchmarkResult = record
    Name: string;
    UnitsProcessed: Int64;
    DurationMs: Double;
  end;

  TWorkItem = class
  public
    Value: Integer;
    constructor Create(const aValue: Integer);
  end;

  TTimerTickTracker = class
  private
    fDone: TEvent;
    fTargetTicks: Integer;
    fTickCount: Integer;
  public
    constructor Create(aDone: TEvent; const aTargetTicks: Integer);
    procedure HandleTimer(aSender: TObject);
    property TickCount: Integer read fTickCount;
  end;

const
  cSimpleAsyncCalls = 25000;
  cAsyncLoopIterations = 300000;
  cCollectionItems = 300000;
  cCollectionBatchSize = 128;
  cMultiProducerCount = 8;
  cMultiProducerItemsPerProducer = 40000;
  cTimerTicks = 200;
  cTimerIntervalMs = 5;
  cTimerTimeoutMs = 30000;

constructor TWorkItem.Create(const aValue: Integer);
begin
  inherited Create;
  Value := aValue;
end;

constructor TTimerTickTracker.Create(aDone: TEvent; const aTargetTicks: Integer);
begin
  inherited Create;
  fDone := aDone;
  fTargetTicks := aTargetTicks;
  fTickCount := 0;
end;

procedure TTimerTickTracker.HandleTimer(aSender: TObject);
var
  lTicks: Integer;
begin
  lTicks := TInterlocked.Increment(fTickCount);
  if lTicks >= fTargetTicks then
    fDone.SetEvent;
end;

function ThroughputPerSecond(const aUnitsProcessed: Int64; const aDurationMs: Double): Double;
begin
  if aDurationMs <= 0 then
    Exit(0);
  Result := aUnitsProcessed / (aDurationMs / 1000);
end;

procedure PrintResult(const aResult: TBenchmarkResult);
var
  lThroughput: Double;
begin
  lThroughput := ThroughputPerSecond(aResult.UnitsProcessed, aResult.DurationMs);
  Writeln(Format('%-38s %12d units  %10.3f ms  %14.0f units/s',
    [aResult.Name, aResult.UnitsProcessed, aResult.DurationMs, lThroughput]));
end;

function BenchmarkSimpleAsyncCall: TBenchmarkResult;
var
  lAsync: iAsync;
  i: Integer;
  lStopwatch: TStopwatch;
begin
  Result.Name := 'SimpleAsyncCall (sequential wait)';
  Result.UnitsProcessed := cSimpleAsyncCalls;

  lStopwatch := TStopwatch.StartNew;
  for i := 1 to cSimpleAsyncCalls do
  begin
    lAsync := SimpleAsyncCall(
      procedure
      begin
      end,
      'AsyncBenchmark.SimpleAsyncCall');
    lAsync.WaitFor;
  end;
  lStopwatch.Stop;

  Result.DurationMs := lStopwatch.Elapsed.TotalMilliseconds;
end;

function BenchmarkAsyncLoop: TBenchmarkResult;
var
  lIterationsDone: Integer;
  lStopwatch: TStopwatch;
  lThreadCount: Integer;
begin
  Result.Name := 'TAsyncLoop.RunAndWait';
  Result.UnitsProcessed := cAsyncLoopIterations;

  lIterationsDone := 0;
  lThreadCount := Max(1, TThread.ProcessorCount);

  lStopwatch := TStopwatch.StartNew;
  TAsyncLoop.RunAndWait(0, cAsyncLoopIterations - 1,
    procedure(aCurIndex: integer; var aCancel: boolean)
    begin
      TInterlocked.Increment(lIterationsDone);
      aCancel := False;
    end,
    lThreadCount);
  lStopwatch.Stop;

  if lIterationsDone <> cAsyncLoopIterations then
    raise Exception.CreateFmt('TAsyncLoop processed %d/%d iterations.', [lIterationsDone, cAsyncLoopIterations]);

  Result.DurationMs := lStopwatch.Elapsed.TotalMilliseconds;
end;

function BenchmarkAsyncCollectionProcessor: TBenchmarkResult;
var
  i: Integer;
  lProcessor: TAsyncCollectionProcessor<TWorkItem>;
  lProcessedCount: Integer;
  lStopwatch: TStopwatch;
  lThreadCount: Integer;
begin
  Result.Name := 'TAsyncCollectionProcessor';
  Result.UnitsProcessed := cCollectionItems;

  lThreadCount := Max(1, TThread.ProcessorCount);
  lProcessedCount := 0;

  lProcessor := TAsyncCollectionProcessor<TWorkItem>.Create;
  try
    lProcessor.SimultanousThreadCount := lThreadCount;
    lProcessor.Proc :=
      procedure(const aItem: TWorkItem)
      begin
        try
          TInterlocked.Increment(lProcessedCount);
        finally
          aItem.Free;
        end;
      end;

    lStopwatch := TStopwatch.StartNew;
    for i := 1 to cCollectionItems do
      lProcessor.Add(TWorkItem.Create(1));

    lProcessor.WaitFor;
    lStopwatch.Stop;
  finally
    lProcessor.Free;
  end;

  if lProcessedCount <> cCollectionItems then
    raise Exception.CreateFmt('TAsyncCollectionProcessor processed %d/%d items.', [lProcessedCount, cCollectionItems]);

  Result.DurationMs := lStopwatch.Elapsed.TotalMilliseconds;
end;

function BenchmarkAsyncCollectionProcessorBatched: TBenchmarkResult;
var
  lProcessor: TAsyncCollectionProcessor<TWorkItem>;
  lProcessedCount: Integer;
  lStopwatch: TStopwatch;
  lThreadCount: Integer;
  lCurrentValue: Integer;
  lBatchCount: Integer;
  lBatch: TArray<TWorkItem>;
  i: Integer;
begin
  Result.Name := 'TAsyncCollectionProcessor.BatchedAddStorm';
  Result.UnitsProcessed := cCollectionItems;

  lThreadCount := Max(1, TThread.ProcessorCount);
  lProcessedCount := 0;
  lCurrentValue := 1;

  lProcessor := TAsyncCollectionProcessor<TWorkItem>.Create;
  try
    lProcessor.SimultanousThreadCount := lThreadCount;
    lProcessor.Proc :=
      procedure(const aItem: TWorkItem)
      begin
        try
          TInterlocked.Increment(lProcessedCount);
        finally
          aItem.Free;
        end;
      end;

    lStopwatch := TStopwatch.StartNew;
    while lCurrentValue <= cCollectionItems do
    begin
      lBatchCount := Min(cCollectionBatchSize, cCollectionItems - lCurrentValue + 1);
      SetLength(lBatch, lBatchCount);
      for i := 0 to lBatchCount - 1 do
        lBatch[i] := TWorkItem.Create(lCurrentValue + i);

      lProcessor.Add(lBatch);
      Inc(lCurrentValue, lBatchCount);
    end;

    lProcessor.WaitFor;
    lStopwatch.Stop;
  finally
    lProcessor.Free;
  end;

  if lProcessedCount <> cCollectionItems then
    raise Exception.CreateFmt('Batched processor processed %d/%d items.', [lProcessedCount, cCollectionItems]);

  Result.DurationMs := lStopwatch.Elapsed.TotalMilliseconds;
end;

function BenchmarkAsyncCollectionProcessorMultiProducer: TBenchmarkResult;
var
  lProcessor: TAsyncCollectionProcessor<TWorkItem>;
  lProcessedCount: Integer;
  lStopwatch: TStopwatch;
  lThreadCount: Integer;
  lProducerCount: Integer;
  lProducerIndex: Integer;
  i: Integer;
  lExpected: Integer;
  lProducers: TArray<iAsync>;
begin
  lProducerCount := Min(cMultiProducerCount, Max(2, TThread.ProcessorCount));
  lExpected := lProducerCount * cMultiProducerItemsPerProducer;

  Result.Name := 'TAsyncCollectionProcessor.MultiProducerContention';
  Result.UnitsProcessed := lExpected;

  lThreadCount := Max(1, TThread.ProcessorCount);
  lProcessedCount := 0;

  lProcessor := TAsyncCollectionProcessor<TWorkItem>.Create;
  try
    lProcessor.SimultanousThreadCount := lThreadCount;
    lProcessor.Proc :=
      procedure(const aItem: TWorkItem)
      begin
        try
          TInterlocked.Increment(lProcessedCount);
        finally
          aItem.Free;
        end;
      end;

    SetLength(lProducers, lProducerCount);
    lStopwatch := TStopwatch.StartNew;
    for lProducerIndex := 0 to lProducerCount - 1 do
      lProducers[lProducerIndex] := SimpleAsyncCall(
        procedure
        var
          j: Integer;
        begin
          for j := 1 to cMultiProducerItemsPerProducer do
            lProcessor.Add(TWorkItem.Create(j));
        end,
        'AsyncBenchmark.MultiProducer.' + IntToStr(lProducerIndex));

    for i := 0 to High(lProducers) do
      lProducers[i].WaitFor;

    lProcessor.WaitFor;
    lStopwatch.Stop;
  finally
    lProcessor.Free;
  end;

  if lProcessedCount <> lExpected then
    raise Exception.CreateFmt('Multi-producer processor processed %d/%d items.', [lProcessedCount, lExpected]);

  Result.DurationMs := lStopwatch.Elapsed.TotalMilliseconds;
end;

function BenchmarkAsyncTimerCompatibility: TBenchmarkResult;
var
  lDone: TEvent;
  lTickCount: Integer;
  lTimer: TAsyncTimer;
  lStopwatch: TStopwatch;
begin
  Result.Name := 'TAsyncTimer (compat wrapper)';
  Result.UnitsProcessed := cTimerTicks;

  lDone := TEvent.Create(nil, True, False, '');
  lTickCount := 0;
  lTimer := nil;
  try
    lTimer := TAsyncTimer.Create(
      procedure
      begin
        if TInterlocked.Increment(lTickCount) >= cTimerTicks then
          lDone.SetEvent;
      end,
      cTimerIntervalMs,
      True);

    lStopwatch := TStopwatch.StartNew;
    if lDone.WaitFor(cTimerTimeoutMs) <> wrSignaled then
      raise Exception.Create('TAsyncTimer benchmark timed out.');
    lStopwatch.Stop;

    lTimer.Enabled := False;
  finally
    lTimer.Free;
    lDone.Free;
  end;

  Result.DurationMs := lStopwatch.Elapsed.TotalMilliseconds;
end;

function BenchmarkPortableTimer: TBenchmarkResult;
var
  lDone: TEvent;
  lTimer: TPortableTimer;
  lTracker: TTimerTickTracker;
  lStopwatch: TStopwatch;
begin
  Result.Name := 'TPortableTimer';
  Result.UnitsProcessed := cTimerTicks;

  lDone := TEvent.Create(nil, True, False, '');
  lTimer := nil;
  lTracker := nil;
  try
    lTracker := TTimerTickTracker.Create(lDone, cTimerTicks);
    lTimer := TPortableTimer.Create;
    lTimer.Interval := cTimerIntervalMs;
    lTimer.OnTimer := lTracker.HandleTimer;

    lStopwatch := TStopwatch.StartNew;
    lTimer.Enabled := True;
    if lDone.WaitFor(cTimerTimeoutMs) <> wrSignaled then
      raise Exception.Create('TPortableTimer benchmark timed out.');
    lStopwatch.Stop;

    lTimer.Enabled := False;

    if lTracker.TickCount < cTimerTicks then
      raise Exception.CreateFmt('TPortableTimer produced %d/%d ticks.', [lTracker.TickCount, cTimerTicks]);
  finally
    lTimer.Free;
    lTracker.Free;
    lDone.Free;
  end;

  Result.DurationMs := lStopwatch.Elapsed.TotalMilliseconds;
end;

procedure PrintHeader;
begin
  Writeln('maxAsync baseline benchmark');
  Writeln(Format('CPU threads: %d', [TThread.ProcessorCount]));
  Writeln(Format('SimpleAsyncCall calls: %d', [cSimpleAsyncCalls]));
  Writeln(Format('AsyncLoop iterations: %d', [cAsyncLoopIterations]));
  Writeln(Format('Collection items: %d', [cCollectionItems]));
  Writeln(Format('Collection batch size: %d', [cCollectionBatchSize]));
  Writeln(Format('Multi-producer adders: %d x %d items', [cMultiProducerCount, cMultiProducerItemsPerProducer]));
  Writeln(Format('Timer ticks: %d @ %d ms', [cTimerTicks, cTimerIntervalMs]));
  Writeln(StringOfChar('-', 92));
  Writeln(Format('%-38s %12s  %10s  %14s', ['Benchmark', 'Units', 'Time', 'Throughput']));
  Writeln(StringOfChar('-', 92));
end;

begin
  try
    PrintHeader;
    PrintResult(BenchmarkSimpleAsyncCall);
    PrintResult(BenchmarkAsyncLoop);
    PrintResult(BenchmarkAsyncCollectionProcessor);
    PrintResult(BenchmarkAsyncCollectionProcessorBatched);
    PrintResult(BenchmarkAsyncCollectionProcessorMultiProducer);
    PrintResult(BenchmarkAsyncTimerCompatibility);
    PrintResult(BenchmarkPortableTimer);

    Writeln(StringOfChar('-', 92));
  except
    on lException: Exception do
    begin
      Writeln(lException.ClassName, ': ', lException.Message);
      System.ExitCode := 1;
    end;
  end;
end.
