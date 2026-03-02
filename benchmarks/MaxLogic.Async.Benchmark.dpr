program MaxLogicAsyncBenchmark;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.Classes,
  System.DateUtils,
  System.Diagnostics,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.IOUtils,
  System.Math,
  System.StrUtils,
  System.SyncObjs,
  System.SysUtils,
  System.Threading,
  maxAsync in '..\maxAsync.pas',
  MaxLogic.PortableTimer in '..\MaxLogic.PortableTimer.pas';

type
  TBenchmarkResult = record
    Name: string;
    UnitsProcessed: Int64;
    DurationMs: Double;
  end;

  TBenchmarkMetrics = record
    SimpleAsyncCallOpsPerSec: Int64;
    TaskSimpleAsyncCallOpsPerSec: Int64;
    AsyncLoopOpsPerSec: Int64;
    TaskParallelForOpsPerSec: Int64;
    AsyncCollectionProcessorOpsPerSec: Int64;
    AsyncCollectionProcessorBatchedOpsPerSec: Int64;
    AsyncCollectionProcessorMultiProducerOpsPerSec: Int64;
    TaskThreadedQueueProcessorOpsPerSec: Int64;
    AsyncTimerOpsPerSec: Int64;
    PortableTimerOpsPerSec: Int64;
  end;

  TBenchmarkRun = record
    SimpleAsyncResult: TBenchmarkResult;
    TaskSimpleAsyncResult: TBenchmarkResult;
    AsyncLoopResult: TBenchmarkResult;
    TaskParallelForResult: TBenchmarkResult;
    CollectionResult: TBenchmarkResult;
    CollectionBatchedResult: TBenchmarkResult;
    CollectionMultiProducerResult: TBenchmarkResult;
    TaskQueueProcessorResult: TBenchmarkResult;
    AsyncTimerResult: TBenchmarkResult;
    PortableTimerResult: TBenchmarkResult;
    Metrics: TBenchmarkMetrics;
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
  cTaskQueueBound = 4096;
  cTimerTicks = 200;
  cTimerIntervalMs = 5;
  cTimerTimeoutMs = 30000;
  cDefaultWarmupRuns = 1;
  cDefaultMeasuredRuns = 5;

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

function ThroughputAsInt64(const aUnitsProcessed: Int64; const aDurationMs: Double): Int64;
begin
  Result := Round(ThroughputPerSecond(aUnitsProcessed, aDurationMs));
end;

function EscapeJsonString(const aValue: string): string;
begin
  Result := StringReplace(aValue, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
end;

function FloatToInvariant(const aValue: Double): string;
var
  lFormatSettings: TFormatSettings;
begin
  lFormatSettings := TFormatSettings.Invariant;
  Result := FloatToStrF(aValue, ffFixed, 15, 3, lFormatSettings);
end;

function FormatUtcIso8601Now: string;
var
  lUtcNow: TDateTime;
begin
  lUtcNow := TTimeZone.Local.ToUniversalTime(Now);
  Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"', lUtcNow);
end;

function FormatDateOnlyUtcNow: string;
var
  lUtcNow: TDateTime;
begin
  lUtcNow := TTimeZone.Local.ToUniversalTime(Now);
  Result := FormatDateTime('yyyy"-"mm"-"dd', lUtcNow);
end;

function BuildBenchmarkDirectoryPath: string;
begin
  Result := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..'));
end;

procedure CollectMetrics(
  const aSimpleAsyncResult: TBenchmarkResult;
  const aTaskSimpleAsyncResult: TBenchmarkResult;
  const aAsyncLoopResult: TBenchmarkResult;
  const aTaskParallelForResult: TBenchmarkResult;
  const aCollectionResult: TBenchmarkResult;
  const aCollectionBatchedResult: TBenchmarkResult;
  const aCollectionMultiProducerResult: TBenchmarkResult;
  const aTaskQueueProcessorResult: TBenchmarkResult;
  const aAsyncTimerResult: TBenchmarkResult;
  const aPortableTimerResult: TBenchmarkResult;
  out aMetrics: TBenchmarkMetrics);
begin
  aMetrics.SimpleAsyncCallOpsPerSec := ThroughputAsInt64(aSimpleAsyncResult.UnitsProcessed, aSimpleAsyncResult.DurationMs);
  aMetrics.TaskSimpleAsyncCallOpsPerSec := ThroughputAsInt64(aTaskSimpleAsyncResult.UnitsProcessed, aTaskSimpleAsyncResult.DurationMs);
  aMetrics.AsyncLoopOpsPerSec := ThroughputAsInt64(aAsyncLoopResult.UnitsProcessed, aAsyncLoopResult.DurationMs);
  aMetrics.TaskParallelForOpsPerSec := ThroughputAsInt64(aTaskParallelForResult.UnitsProcessed, aTaskParallelForResult.DurationMs);
  aMetrics.AsyncCollectionProcessorOpsPerSec := ThroughputAsInt64(aCollectionResult.UnitsProcessed, aCollectionResult.DurationMs);
  aMetrics.AsyncCollectionProcessorBatchedOpsPerSec := ThroughputAsInt64(aCollectionBatchedResult.UnitsProcessed, aCollectionBatchedResult.DurationMs);
  aMetrics.AsyncCollectionProcessorMultiProducerOpsPerSec := ThroughputAsInt64(aCollectionMultiProducerResult.UnitsProcessed, aCollectionMultiProducerResult.DurationMs);
  aMetrics.TaskThreadedQueueProcessorOpsPerSec := ThroughputAsInt64(aTaskQueueProcessorResult.UnitsProcessed, aTaskQueueProcessorResult.DurationMs);
  aMetrics.AsyncTimerOpsPerSec := ThroughputAsInt64(aAsyncTimerResult.UnitsProcessed, aAsyncTimerResult.DurationMs);
  aMetrics.PortableTimerOpsPerSec := ThroughputAsInt64(aPortableTimerResult.UnitsProcessed, aPortableTimerResult.DurationMs);
end;

procedure WriteLatestJson(
  const aFilePath: string;
  const aRunAtUtc: string;
  const aDateOnly: string;
  const aAggregation: string;
  const aMeasuredRuns: Integer;
  const aWarmupRuns: Integer;
  const aMetrics: TBenchmarkMetrics);
var
  lJson: TStringList;
begin
  lJson := TStringList.Create;
  try
    lJson.Add('{');
    lJson.Add('  "benchmark": "MaxLogic.Async.Benchmark",');
    lJson.Add('  "run_at_utc": "' + EscapeJsonString(aRunAtUtc) + '",');
    lJson.Add('  "date": "' + EscapeJsonString(aDateOnly) + '",');
    lJson.Add('  "aggregation": "' + EscapeJsonString(aAggregation) + '",');
    lJson.Add('  "measured_runs": ' + IntToStr(aMeasuredRuns) + ',');
    lJson.Add('  "warmup_runs": ' + IntToStr(aWarmupRuns) + ',');
    lJson.Add('  "metrics": {');
    lJson.Add('    "simple_async_call_ops_per_sec": ' + IntToStr(aMetrics.SimpleAsyncCallOpsPerSec) + ',');
    lJson.Add('    "ttask_simple_async_call_ops_per_sec": ' + IntToStr(aMetrics.TaskSimpleAsyncCallOpsPerSec) + ',');
    lJson.Add('    "async_loop_ops_per_sec": ' + IntToStr(aMetrics.AsyncLoopOpsPerSec) + ',');
    lJson.Add('    "ttask_parallel_for_ops_per_sec": ' + IntToStr(aMetrics.TaskParallelForOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_batched_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorBatchedOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_multi_producer_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorMultiProducerOpsPerSec) + ',');
    lJson.Add('    "ttask_threaded_queue_processor_ops_per_sec": ' + IntToStr(aMetrics.TaskThreadedQueueProcessorOpsPerSec) + ',');
    lJson.Add('    "async_timer_ops_per_sec": ' + IntToStr(aMetrics.AsyncTimerOpsPerSec) + ',');
    lJson.Add('    "portable_timer_ops_per_sec": ' + IntToStr(aMetrics.PortableTimerOpsPerSec));
    lJson.Add('  }');
    lJson.Add('}');
    lJson.SaveToFile(aFilePath, TEncoding.UTF8);
  finally
    lJson.Free;
  end;
end;

procedure WriteResultsJson(
  const aFilePath: string;
  const aSimpleAsyncResult: TBenchmarkResult;
  const aTaskSimpleAsyncResult: TBenchmarkResult;
  const aAsyncLoopResult: TBenchmarkResult;
  const aTaskParallelForResult: TBenchmarkResult;
  const aCollectionResult: TBenchmarkResult;
  const aCollectionBatchedResult: TBenchmarkResult;
  const aCollectionMultiProducerResult: TBenchmarkResult;
  const aTaskQueueProcessorResult: TBenchmarkResult;
  const aAsyncTimerResult: TBenchmarkResult;
  const aPortableTimerResult: TBenchmarkResult;
  const aAggregation: string;
  const aMeasuredRuns: Integer;
  const aWarmupRuns: Integer;
  const aMetrics: TBenchmarkMetrics);
var
  lJson: TStringList;
begin
  lJson := TStringList.Create;
  try
    lJson.Add('{');
    lJson.Add('  "benchmark": "MaxLogic.Async.Benchmark",');
    lJson.Add('  "run_at_utc": "' + EscapeJsonString(FormatUtcIso8601Now) + '",');
    lJson.Add('  "aggregation": "' + EscapeJsonString(aAggregation) + '",');
    lJson.Add('  "measured_runs": ' + IntToStr(aMeasuredRuns) + ',');
    lJson.Add('  "warmup_runs": ' + IntToStr(aWarmupRuns) + ',');
    lJson.Add('  "metrics": {');
    lJson.Add('    "simple_async_call_ops_per_sec": ' + IntToStr(aMetrics.SimpleAsyncCallOpsPerSec) + ',');
    lJson.Add('    "ttask_simple_async_call_ops_per_sec": ' + IntToStr(aMetrics.TaskSimpleAsyncCallOpsPerSec) + ',');
    lJson.Add('    "async_loop_ops_per_sec": ' + IntToStr(aMetrics.AsyncLoopOpsPerSec) + ',');
    lJson.Add('    "ttask_parallel_for_ops_per_sec": ' + IntToStr(aMetrics.TaskParallelForOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_batched_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorBatchedOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_multi_producer_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorMultiProducerOpsPerSec) + ',');
    lJson.Add('    "ttask_threaded_queue_processor_ops_per_sec": ' + IntToStr(aMetrics.TaskThreadedQueueProcessorOpsPerSec) + ',');
    lJson.Add('    "async_timer_ops_per_sec": ' + IntToStr(aMetrics.AsyncTimerOpsPerSec) + ',');
    lJson.Add('    "portable_timer_ops_per_sec": ' + IntToStr(aMetrics.PortableTimerOpsPerSec));
    lJson.Add('  },');
    lJson.Add('  "results": [');
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aSimpleAsyncResult.Name), aSimpleAsyncResult.UnitsProcessed, FloatToInvariant(aSimpleAsyncResult.DurationMs), aMetrics.SimpleAsyncCallOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aTaskSimpleAsyncResult.Name), aTaskSimpleAsyncResult.UnitsProcessed, FloatToInvariant(aTaskSimpleAsyncResult.DurationMs), aMetrics.TaskSimpleAsyncCallOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aAsyncLoopResult.Name), aAsyncLoopResult.UnitsProcessed, FloatToInvariant(aAsyncLoopResult.DurationMs), aMetrics.AsyncLoopOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aTaskParallelForResult.Name), aTaskParallelForResult.UnitsProcessed, FloatToInvariant(aTaskParallelForResult.DurationMs), aMetrics.TaskParallelForOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aCollectionResult.Name), aCollectionResult.UnitsProcessed, FloatToInvariant(aCollectionResult.DurationMs), aMetrics.AsyncCollectionProcessorOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aCollectionBatchedResult.Name), aCollectionBatchedResult.UnitsProcessed, FloatToInvariant(aCollectionBatchedResult.DurationMs), aMetrics.AsyncCollectionProcessorBatchedOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aCollectionMultiProducerResult.Name), aCollectionMultiProducerResult.UnitsProcessed, FloatToInvariant(aCollectionMultiProducerResult.DurationMs), aMetrics.AsyncCollectionProcessorMultiProducerOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aTaskQueueProcessorResult.Name), aTaskQueueProcessorResult.UnitsProcessed, FloatToInvariant(aTaskQueueProcessorResult.DurationMs), aMetrics.TaskThreadedQueueProcessorOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aAsyncTimerResult.Name), aAsyncTimerResult.UnitsProcessed, FloatToInvariant(aAsyncTimerResult.DurationMs), aMetrics.AsyncTimerOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d}',
      [EscapeJsonString(aPortableTimerResult.Name), aPortableTimerResult.UnitsProcessed, FloatToInvariant(aPortableTimerResult.DurationMs), aMetrics.PortableTimerOpsPerSec]));
    lJson.Add('  ]');
    lJson.Add('}');
    lJson.SaveToFile(aFilePath, TEncoding.UTF8);
  finally
    lJson.Free;
  end;
end;

procedure AppendHistoryCsv(
  const aFilePath: string;
  const aRunAtUtc: string;
  const aMetrics: TBenchmarkMetrics);
const
  cHeader = 'run_at_utc,simple_async_call_ops_per_sec,ttask_simple_async_call_ops_per_sec,async_loop_ops_per_sec,ttask_parallel_for_ops_per_sec,async_collection_processor_ops_per_sec,async_collection_processor_batched_ops_per_sec,async_collection_processor_multi_producer_ops_per_sec,ttask_threaded_queue_processor_ops_per_sec,async_timer_ops_per_sec,portable_timer_ops_per_sec';
  cLegacyHeader = 'run_at_utc,simple_async_call_ops_per_sec,async_loop_ops_per_sec,async_collection_processor_ops_per_sec,async_collection_processor_batched_ops_per_sec,async_collection_processor_multi_producer_ops_per_sec,async_timer_ops_per_sec,portable_timer_ops_per_sec';
var
  lLines: TStringList;
  i: Integer;
  lParts: TArray<string>;
begin
  lLines := TStringList.Create;
  try
    if TFile.Exists(aFilePath) then
      lLines.LoadFromFile(aFilePath, TEncoding.UTF8);

    if lLines.Count = 0 then
      lLines.Add(cHeader)
    else if Trim(lLines[0]) = cLegacyHeader then
    begin
      lLines[0] := cHeader;
      for i := 1 to lLines.Count - 1 do
      begin
        if Trim(lLines[i]) <> '' then
        begin
          lParts := lLines[i].Split([',']);
          if Length(lParts) >= 8 then
          begin
            lLines[i] := Format('%s,%s,,%s,,%s,%s,%s,,%s,%s',
              [lParts[0], lParts[1], lParts[2], lParts[3], lParts[4], lParts[5], lParts[6], lParts[7]]);
          end else
            lLines[i] := lLines[i] + ',,,';
        end;
      end;
    end else if Trim(lLines[0]) <> cHeader then
      lLines.Insert(0, cHeader);

    lLines.Add(Format('%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d',
      [aRunAtUtc,
       aMetrics.SimpleAsyncCallOpsPerSec,
       aMetrics.TaskSimpleAsyncCallOpsPerSec,
       aMetrics.AsyncLoopOpsPerSec,
       aMetrics.TaskParallelForOpsPerSec,
       aMetrics.AsyncCollectionProcessorOpsPerSec,
       aMetrics.AsyncCollectionProcessorBatchedOpsPerSec,
       aMetrics.AsyncCollectionProcessorMultiProducerOpsPerSec,
       aMetrics.TaskThreadedQueueProcessorOpsPerSec,
       aMetrics.AsyncTimerOpsPerSec,
       aMetrics.PortableTimerOpsPerSec]));

    lLines.SaveToFile(aFilePath, TEncoding.UTF8);
  finally
    lLines.Free;
  end;
end;

procedure SaveBenchmarkArtifacts(
  const aRun: TBenchmarkRun;
  const aAggregation: string;
  const aMeasuredRuns: Integer;
  const aWarmupRuns: Integer);
var
  lBenchmarkDir: string;
  lLatestJsonPath: string;
  lResultsJsonPath: string;
  lHistoryCsvPath: string;
  lRunAtUtc: string;
  lDateOnly: string;
begin
  lBenchmarkDir := BuildBenchmarkDirectoryPath;
  lLatestJsonPath := TPath.Combine(lBenchmarkDir, 'MaxLogic.Async.Benchmark.latest.json');
  lResultsJsonPath := TPath.Combine(lBenchmarkDir, 'MaxLogic.Async.Benchmark.results.json');
  lHistoryCsvPath := TPath.Combine(lBenchmarkDir, 'MaxLogic.Async.Benchmark.history.csv');
  lRunAtUtc := FormatUtcIso8601Now;
  lDateOnly := FormatDateOnlyUtcNow;

  WriteLatestJson(lLatestJsonPath, lRunAtUtc, lDateOnly, aAggregation, aMeasuredRuns, aWarmupRuns, aRun.Metrics);
  WriteResultsJson(
    lResultsJsonPath,
    aRun.SimpleAsyncResult,
    aRun.TaskSimpleAsyncResult,
    aRun.AsyncLoopResult,
    aRun.TaskParallelForResult,
    aRun.CollectionResult,
    aRun.CollectionBatchedResult,
    aRun.CollectionMultiProducerResult,
    aRun.TaskQueueProcessorResult,
    aRun.AsyncTimerResult,
    aRun.PortableTimerResult,
    aAggregation,
    aMeasuredRuns,
    aWarmupRuns,
    aRun.Metrics);
  AppendHistoryCsv(lHistoryCsvPath, lRunAtUtc, aRun.Metrics);
end;

procedure PrintResult(const aResult: TBenchmarkResult; const aOpsPerSec: Int64);
begin
  Writeln(Format('%-38s %12d units  %10.3f ms  %14d units/s',
    [aResult.Name, aResult.UnitsProcessed, aResult.DurationMs, aOpsPerSec]));
end;

function TryReadIntegerOption(const aOptionName: string; out aOptionValue: Integer): Boolean;
var
  i: Integer;
  lArgument: string;
  lPrefix: string;
  lValueText: string;
begin
  Result := False;
  aOptionValue := 0;
  lPrefix := '--' + aOptionName + '=';

  for i := 1 to ParamCount do
  begin
    lArgument := ParamStr(i);
    if StartsText(lPrefix, lArgument) then
    begin
      lValueText := Copy(lArgument, Length(lPrefix) + 1, MaxInt);
      if not TryStrToInt(Trim(lValueText), aOptionValue) then
        raise Exception.CreateFmt('Invalid --%s option value: %s', [aOptionName, lValueText]);
      Result := True;
      Exit;
    end;
  end;
end;

procedure ParseBenchmarkConfig(out aWarmupRuns: Integer; out aMeasuredRuns: Integer);
var
  i: Integer;
  lArgument: string;
  lOptionValue: Integer;
begin
  aWarmupRuns := cDefaultWarmupRuns;
  aMeasuredRuns := cDefaultMeasuredRuns;

  for i := 1 to ParamCount do
  begin
    lArgument := ParamStr(i);
    if SameText(lArgument, '--help') then
    begin
      Writeln('Usage: MaxLogic.Async.Benchmark.exe [--warmup=<n>] [--repeats=<n>]');
      Writeln('  --warmup=<n>   Number of warmup runs (default 1, minimum 0).');
      Writeln('  --repeats=<n>  Number of measured runs (default 5, minimum 1).');
      Halt(0);
    end;
  end;

  if TryReadIntegerOption('warmup', lOptionValue) then
  begin
    if lOptionValue < 0 then
      raise Exception.Create('--warmup must be >= 0.');
    aWarmupRuns := lOptionValue;
  end;

  if TryReadIntegerOption('repeats', lOptionValue) then
  begin
    if lOptionValue <= 0 then
      raise Exception.Create('--repeats must be >= 1.');
    aMeasuredRuns := lOptionValue;
  end;
end;

function MedianInt64(const aValues: TArray<Int64>): Int64;
var
  lSorted: TArray<Int64>;
  lCount: Integer;
  lMiddleIndex: Integer;
begin
  if Length(aValues) = 0 then
    raise Exception.Create('Cannot compute median for empty Int64 array.');

  lSorted := Copy(aValues);
  TArray.Sort<Int64>(lSorted);
  lCount := Length(lSorted);
  lMiddleIndex := lCount div 2;

  if Odd(lCount) then
    Exit(lSorted[lMiddleIndex]);

  Result := (lSorted[lMiddleIndex - 1] + lSorted[lMiddleIndex]) div 2;
end;

function MedianDouble(const aValues: TArray<Double>): Double;
var
  lSorted: TArray<Double>;
  lCount: Integer;
  lMiddleIndex: Integer;
begin
  if Length(aValues) = 0 then
    raise Exception.Create('Cannot compute median for empty Double array.');

  lSorted := Copy(aValues);
  TArray.Sort<Double>(lSorted);
  lCount := Length(lSorted);
  lMiddleIndex := lCount div 2;

  if Odd(lCount) then
    Exit(lSorted[lMiddleIndex]);

  Result := (lSorted[lMiddleIndex - 1] + lSorted[lMiddleIndex]) / 2;
end;

function BenchmarkSimpleAsyncCall: TBenchmarkResult; forward;
function BenchmarkTTaskSimpleAsyncCall: TBenchmarkResult; forward;
function BenchmarkAsyncLoop: TBenchmarkResult; forward;
function BenchmarkTTaskParallelFor: TBenchmarkResult; forward;
function BenchmarkAsyncCollectionProcessor: TBenchmarkResult; forward;
function BenchmarkAsyncCollectionProcessorBatched: TBenchmarkResult; forward;
function BenchmarkAsyncCollectionProcessorMultiProducer: TBenchmarkResult; forward;
function BenchmarkTTaskThreadedQueueProcessor: TBenchmarkResult; forward;
function BenchmarkAsyncTimerCompatibility: TBenchmarkResult; forward;
function BenchmarkPortableTimer: TBenchmarkResult; forward;

procedure RunBenchmarks(out aRun: TBenchmarkRun);
begin
  aRun.SimpleAsyncResult := BenchmarkSimpleAsyncCall;
  aRun.TaskSimpleAsyncResult := BenchmarkTTaskSimpleAsyncCall;
  aRun.AsyncLoopResult := BenchmarkAsyncLoop;
  aRun.TaskParallelForResult := BenchmarkTTaskParallelFor;
  aRun.CollectionResult := BenchmarkAsyncCollectionProcessor;
  aRun.CollectionBatchedResult := BenchmarkAsyncCollectionProcessorBatched;
  aRun.CollectionMultiProducerResult := BenchmarkAsyncCollectionProcessorMultiProducer;
  aRun.TaskQueueProcessorResult := BenchmarkTTaskThreadedQueueProcessor;
  aRun.AsyncTimerResult := BenchmarkAsyncTimerCompatibility;
  aRun.PortableTimerResult := BenchmarkPortableTimer;

  CollectMetrics(
    aRun.SimpleAsyncResult,
    aRun.TaskSimpleAsyncResult,
    aRun.AsyncLoopResult,
    aRun.TaskParallelForResult,
    aRun.CollectionResult,
    aRun.CollectionBatchedResult,
    aRun.CollectionMultiProducerResult,
    aRun.TaskQueueProcessorResult,
    aRun.AsyncTimerResult,
    aRun.PortableTimerResult,
    aRun.Metrics);
end;

function BuildAggregatedRun(const aRuns: TArray<TBenchmarkRun>): TBenchmarkRun;
var
  lCount: Integer;
  i: Integer;
  lSimpleOps: TArray<Int64>;
  lTaskSimpleOps: TArray<Int64>;
  lAsyncLoopOps: TArray<Int64>;
  lTaskParallelOps: TArray<Int64>;
  lCollectionOps: TArray<Int64>;
  lCollectionBatchedOps: TArray<Int64>;
  lCollectionMultiProducerOps: TArray<Int64>;
  lTaskQueueOps: TArray<Int64>;
  lAsyncTimerOps: TArray<Int64>;
  lPortableTimerOps: TArray<Int64>;
  lSimpleDurationMs: TArray<Double>;
  lTaskSimpleDurationMs: TArray<Double>;
  lAsyncLoopDurationMs: TArray<Double>;
  lTaskParallelDurationMs: TArray<Double>;
  lCollectionDurationMs: TArray<Double>;
  lCollectionBatchedDurationMs: TArray<Double>;
  lCollectionMultiProducerDurationMs: TArray<Double>;
  lTaskQueueDurationMs: TArray<Double>;
  lAsyncTimerDurationMs: TArray<Double>;
  lPortableTimerDurationMs: TArray<Double>;
begin
  if Length(aRuns) = 0 then
    raise Exception.Create('Cannot aggregate benchmark runs because no measured runs are available.');

  Result := aRuns[High(aRuns)];
  lCount := Length(aRuns);

  SetLength(lSimpleOps, lCount);
  SetLength(lTaskSimpleOps, lCount);
  SetLength(lAsyncLoopOps, lCount);
  SetLength(lTaskParallelOps, lCount);
  SetLength(lCollectionOps, lCount);
  SetLength(lCollectionBatchedOps, lCount);
  SetLength(lCollectionMultiProducerOps, lCount);
  SetLength(lTaskQueueOps, lCount);
  SetLength(lAsyncTimerOps, lCount);
  SetLength(lPortableTimerOps, lCount);

  SetLength(lSimpleDurationMs, lCount);
  SetLength(lTaskSimpleDurationMs, lCount);
  SetLength(lAsyncLoopDurationMs, lCount);
  SetLength(lTaskParallelDurationMs, lCount);
  SetLength(lCollectionDurationMs, lCount);
  SetLength(lCollectionBatchedDurationMs, lCount);
  SetLength(lCollectionMultiProducerDurationMs, lCount);
  SetLength(lTaskQueueDurationMs, lCount);
  SetLength(lAsyncTimerDurationMs, lCount);
  SetLength(lPortableTimerDurationMs, lCount);

  for i := 0 to lCount - 1 do
  begin
    lSimpleOps[i] := aRuns[i].Metrics.SimpleAsyncCallOpsPerSec;
    lTaskSimpleOps[i] := aRuns[i].Metrics.TaskSimpleAsyncCallOpsPerSec;
    lAsyncLoopOps[i] := aRuns[i].Metrics.AsyncLoopOpsPerSec;
    lTaskParallelOps[i] := aRuns[i].Metrics.TaskParallelForOpsPerSec;
    lCollectionOps[i] := aRuns[i].Metrics.AsyncCollectionProcessorOpsPerSec;
    lCollectionBatchedOps[i] := aRuns[i].Metrics.AsyncCollectionProcessorBatchedOpsPerSec;
    lCollectionMultiProducerOps[i] := aRuns[i].Metrics.AsyncCollectionProcessorMultiProducerOpsPerSec;
    lTaskQueueOps[i] := aRuns[i].Metrics.TaskThreadedQueueProcessorOpsPerSec;
    lAsyncTimerOps[i] := aRuns[i].Metrics.AsyncTimerOpsPerSec;
    lPortableTimerOps[i] := aRuns[i].Metrics.PortableTimerOpsPerSec;

    lSimpleDurationMs[i] := aRuns[i].SimpleAsyncResult.DurationMs;
    lTaskSimpleDurationMs[i] := aRuns[i].TaskSimpleAsyncResult.DurationMs;
    lAsyncLoopDurationMs[i] := aRuns[i].AsyncLoopResult.DurationMs;
    lTaskParallelDurationMs[i] := aRuns[i].TaskParallelForResult.DurationMs;
    lCollectionDurationMs[i] := aRuns[i].CollectionResult.DurationMs;
    lCollectionBatchedDurationMs[i] := aRuns[i].CollectionBatchedResult.DurationMs;
    lCollectionMultiProducerDurationMs[i] := aRuns[i].CollectionMultiProducerResult.DurationMs;
    lTaskQueueDurationMs[i] := aRuns[i].TaskQueueProcessorResult.DurationMs;
    lAsyncTimerDurationMs[i] := aRuns[i].AsyncTimerResult.DurationMs;
    lPortableTimerDurationMs[i] := aRuns[i].PortableTimerResult.DurationMs;
  end;

  Result.Metrics.SimpleAsyncCallOpsPerSec := MedianInt64(lSimpleOps);
  Result.Metrics.TaskSimpleAsyncCallOpsPerSec := MedianInt64(lTaskSimpleOps);
  Result.Metrics.AsyncLoopOpsPerSec := MedianInt64(lAsyncLoopOps);
  Result.Metrics.TaskParallelForOpsPerSec := MedianInt64(lTaskParallelOps);
  Result.Metrics.AsyncCollectionProcessorOpsPerSec := MedianInt64(lCollectionOps);
  Result.Metrics.AsyncCollectionProcessorBatchedOpsPerSec := MedianInt64(lCollectionBatchedOps);
  Result.Metrics.AsyncCollectionProcessorMultiProducerOpsPerSec := MedianInt64(lCollectionMultiProducerOps);
  Result.Metrics.TaskThreadedQueueProcessorOpsPerSec := MedianInt64(lTaskQueueOps);
  Result.Metrics.AsyncTimerOpsPerSec := MedianInt64(lAsyncTimerOps);
  Result.Metrics.PortableTimerOpsPerSec := MedianInt64(lPortableTimerOps);

  Result.SimpleAsyncResult.DurationMs := MedianDouble(lSimpleDurationMs);
  Result.TaskSimpleAsyncResult.DurationMs := MedianDouble(lTaskSimpleDurationMs);
  Result.AsyncLoopResult.DurationMs := MedianDouble(lAsyncLoopDurationMs);
  Result.TaskParallelForResult.DurationMs := MedianDouble(lTaskParallelDurationMs);
  Result.CollectionResult.DurationMs := MedianDouble(lCollectionDurationMs);
  Result.CollectionBatchedResult.DurationMs := MedianDouble(lCollectionBatchedDurationMs);
  Result.CollectionMultiProducerResult.DurationMs := MedianDouble(lCollectionMultiProducerDurationMs);
  Result.TaskQueueProcessorResult.DurationMs := MedianDouble(lTaskQueueDurationMs);
  Result.AsyncTimerResult.DurationMs := MedianDouble(lAsyncTimerDurationMs);
  Result.PortableTimerResult.DurationMs := MedianDouble(lPortableTimerDurationMs);
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

function BenchmarkTTaskSimpleAsyncCall: TBenchmarkResult;
var
  lTask: ITask;
  i: Integer;
  lStopwatch: TStopwatch;
begin
  Result.Name := 'TTask.Run (sequential wait)';
  Result.UnitsProcessed := cSimpleAsyncCalls;

  lStopwatch := TStopwatch.StartNew;
  for i := 1 to cSimpleAsyncCalls do
  begin
    lTask := TTask.Run(
      procedure
      begin
      end);
    lTask.Wait;
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

function BenchmarkTTaskParallelFor: TBenchmarkResult;
var
  lIterationsDone: Integer;
  lStopwatch: TStopwatch;
begin
  Result.Name := 'TParallel.For';
  Result.UnitsProcessed := cAsyncLoopIterations;
  lIterationsDone := 0;

  lStopwatch := TStopwatch.StartNew;
  TParallel.&For(0, cAsyncLoopIterations - 1,
    procedure(aIndex: Integer)
    begin
      TInterlocked.Increment(lIterationsDone);
    end);
  lStopwatch.Stop;

  if lIterationsDone <> cAsyncLoopIterations then
    raise Exception.CreateFmt('TParallel.For processed %d/%d iterations.', [lIterationsDone, cAsyncLoopIterations]);

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

function BenchmarkTTaskThreadedQueueProcessor: TBenchmarkResult;
var
  lWorkerCount: Integer;
  lQueue: TThreadedQueue<TWorkItem>;
  lWorkers: TArray<ITask>;
  lProcessedCount: Integer;
  lStopwatch: TStopwatch;
  lWorkerIndex: Integer;
  i: Integer;
begin
  Result.Name := 'TTask + TThreadedQueue';
  Result.UnitsProcessed := cCollectionItems;

  lWorkerCount := Max(1, TThread.ProcessorCount);
  lProcessedCount := 0;
  lQueue := TThreadedQueue<TWorkItem>.Create(cTaskQueueBound, 1000, 1000);
  try
    SetLength(lWorkers, lWorkerCount);
    for lWorkerIndex := 0 to High(lWorkers) do
    begin
      lWorkers[lWorkerIndex] := TTask.Run(
        procedure
        var
          lItem: TWorkItem;
          lPopResult: TWaitResult;
        begin
          repeat
            lPopResult := lQueue.PopItem(lItem);
            if lPopResult <> wrSignaled then
              Continue;

            if lItem = nil then
              Break;

            try
              TInterlocked.Increment(lProcessedCount);
            finally
              lItem.Free;
            end;
          until False;
        end);
    end;

    lStopwatch := TStopwatch.StartNew;
    for i := 1 to cCollectionItems do
    begin
      if lQueue.PushItem(TWorkItem.Create(i)) <> wrSignaled then
        raise Exception.Create('TThreadedQueue push failed.');
    end;

    for i := 1 to lWorkerCount do
    begin
      if lQueue.PushItem(nil) <> wrSignaled then
        raise Exception.Create('TThreadedQueue sentinel push failed.');
    end;

    TTask.WaitForAll(lWorkers);
    lStopwatch.Stop;
  finally
    lQueue.Free;
  end;

  if lProcessedCount <> cCollectionItems then
    raise Exception.CreateFmt('TTask queue processor processed %d/%d items.', [lProcessedCount, cCollectionItems]);

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

procedure PrintHeader(const aWarmupRuns: Integer; const aMeasuredRuns: Integer);
begin
  Writeln('maxAsync baseline benchmark');
  Writeln(Format('CPU threads: %d', [TThread.ProcessorCount]));
  Writeln(Format('Warmup runs: %d', [aWarmupRuns]));
  Writeln(Format('Measured runs: %d (median aggregation)', [aMeasuredRuns]));
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

procedure PrintAggregatedResults(const aRun: TBenchmarkRun);
begin
  PrintResult(aRun.SimpleAsyncResult, aRun.Metrics.SimpleAsyncCallOpsPerSec);
  PrintResult(aRun.TaskSimpleAsyncResult, aRun.Metrics.TaskSimpleAsyncCallOpsPerSec);
  PrintResult(aRun.AsyncLoopResult, aRun.Metrics.AsyncLoopOpsPerSec);
  PrintResult(aRun.TaskParallelForResult, aRun.Metrics.TaskParallelForOpsPerSec);
  PrintResult(aRun.CollectionResult, aRun.Metrics.AsyncCollectionProcessorOpsPerSec);
  PrintResult(aRun.CollectionBatchedResult, aRun.Metrics.AsyncCollectionProcessorBatchedOpsPerSec);
  PrintResult(aRun.CollectionMultiProducerResult, aRun.Metrics.AsyncCollectionProcessorMultiProducerOpsPerSec);
  PrintResult(aRun.TaskQueueProcessorResult, aRun.Metrics.TaskThreadedQueueProcessorOpsPerSec);
  PrintResult(aRun.AsyncTimerResult, aRun.Metrics.AsyncTimerOpsPerSec);
  PrintResult(aRun.PortableTimerResult, aRun.Metrics.PortableTimerOpsPerSec);
end;

var
  lWarmupRuns: Integer;
  lMeasuredRuns: Integer;
  lWarmupRunIndex: Integer;
  lMeasuredRunIndex: Integer;
  lMeasuredRunResults: TArray<TBenchmarkRun>;
  lRun: TBenchmarkRun;
  lAggregatedRun: TBenchmarkRun;
begin
  try
    ParseBenchmarkConfig(lWarmupRuns, lMeasuredRuns);
    PrintHeader(lWarmupRuns, lMeasuredRuns);

    for lWarmupRunIndex := 1 to lWarmupRuns do
    begin
      Writeln(Format('Warmup run %d/%d ...', [lWarmupRunIndex, lWarmupRuns]));
      RunBenchmarks(lRun);
    end;

    SetLength(lMeasuredRunResults, lMeasuredRuns);
    for lMeasuredRunIndex := 1 to lMeasuredRuns do
    begin
      Writeln(Format('Measured run %d/%d ...', [lMeasuredRunIndex, lMeasuredRuns]));
      RunBenchmarks(lMeasuredRunResults[Pred(lMeasuredRunIndex)]);
    end;

    lAggregatedRun := BuildAggregatedRun(lMeasuredRunResults);
    Writeln(StringOfChar('-', 92));
    Writeln('Aggregated results (median):');
    PrintAggregatedResults(lAggregatedRun);

    Writeln(StringOfChar('-', 92));

    SaveBenchmarkArtifacts(
      lAggregatedRun,
      'median_of_runs',
      lMeasuredRuns,
      lWarmupRuns);
  except
    on lException: Exception do
    begin
      Writeln(lException.ClassName, ': ', lException.Message);
      System.ExitCode := 1;
    end;
  end;
end.
