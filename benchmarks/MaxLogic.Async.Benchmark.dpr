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
