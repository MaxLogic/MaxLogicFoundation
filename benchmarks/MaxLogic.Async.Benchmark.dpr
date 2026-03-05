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
  {$IFDEF MSWINDOWS}Winapi.Windows,{$ENDIF}
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
    SimpleAsyncCallNoNameOpsPerSec: Int64;
    SimpleAsyncCallWakeReuseOpsPerSec: Int64;
    TaskSimpleAsyncCallOpsPerSec: Int64;
    AsyncLoopOpsPerSec: Int64;
    TaskParallelForOpsPerSec: Int64;
    AsyncCollectionProcessorOpsPerSec: Int64;
    AsyncCollectionProcessorLockedRingOpsPerSec: Int64;
    AsyncCollectionProcessorLockFreeOpsPerSec: Int64;
    AsyncCollectionProcessorBatchedOpsPerSec: Int64;
    AsyncCollectionProcessorBatchedLockedRingOpsPerSec: Int64;
    AsyncCollectionProcessorBatchedLockFreeOpsPerSec: Int64;
    AsyncCollectionProcessorMultiProducerOpsPerSec: Int64;
    AsyncCollectionProcessorMultiProducerLockedRingOpsPerSec: Int64;
    AsyncCollectionProcessorMultiProducerLockFreeOpsPerSec: Int64;
    TaskThreadedQueueProcessorOpsPerSec: Int64;
    AsyncTimerOpsPerSec: Int64;
    PortableTimerOpsPerSec: Int64;
  end;

  TBenchmarkRun = record
    SimpleAsyncResult: TBenchmarkResult;
    SimpleAsyncNoNameResult: TBenchmarkResult;
    SimpleAsyncWakeReuseResult: TBenchmarkResult;
    TaskSimpleAsyncResult: TBenchmarkResult;
    AsyncLoopResult: TBenchmarkResult;
    TaskParallelForResult: TBenchmarkResult;
    CollectionResult: TBenchmarkResult;
    CollectionLockedRingResult: TBenchmarkResult;
    CollectionLockFreeResult: TBenchmarkResult;
    CollectionBatchedResult: TBenchmarkResult;
    CollectionBatchedLockedRingResult: TBenchmarkResult;
    CollectionBatchedLockFreeResult: TBenchmarkResult;
    CollectionMultiProducerResult: TBenchmarkResult;
    CollectionMultiProducerLockedRingResult: TBenchmarkResult;
    CollectionMultiProducerLockFreeResult: TBenchmarkResult;
    TaskQueueProcessorResult: TBenchmarkResult;
    AsyncTimerResult: TBenchmarkResult;
    PortableTimerResult: TBenchmarkResult;
    Metrics: TBenchmarkMetrics;
  end;

  TBenchmarkSeriesStats = record
    MinOpsPerSec: Int64;
    MaxOpsPerSec: Int64;
    MeanOpsPerSec: Double;
    MedianOpsPerSec: Int64;
    StdDevOpsPerSec: Double;
    RelativeStdDevPct: Double;
  end;

  TBenchmarkVarianceSummary = record
    SimpleAsyncCall: TBenchmarkSeriesStats;
    AsyncLoop: TBenchmarkSeriesStats;
    AsyncCollectionProcessor: TBenchmarkSeriesStats;
  end;

  TBenchmarkExecutionConfig = record
    WarmupRuns: Integer;
    MeasuredRuns: Integer;
    StableMode: Boolean;
    HasProcessPriority: Boolean;
    ProcessPriorityClass: Cardinal;
    ProcessPriorityLabel: string;
    HasMainThreadPriority: Boolean;
    MainThreadPriority: TThreadPriority;
    MainThreadPriorityLabel: string;
    HasAffinityMask: Boolean;
    AffinityMask: UInt64;
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
  cCollectionQueueCapacity = 4096;
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

function BoolToJson(const aValue: Boolean): string;
begin
  if aValue then
    Exit('true');
  Result := 'false';
end;

function FormatHexUInt64(const aValue: UInt64): string;
begin
  Result := '$' + IntToHex(aValue, 1);
end;

function TryReadStringOption(const aOptionName: string; out aOptionValue: string): Boolean;
var
  i: Integer;
  lArgument: string;
  lPrefix: string;
begin
  Result := False;
  aOptionValue := '';
  lPrefix := '--' + aOptionName + '=';

  for i := 1 to ParamCount do
  begin
    lArgument := ParamStr(i);
    if StartsText(lPrefix, lArgument) then
    begin
      aOptionValue := Trim(Copy(lArgument, Length(lPrefix) + 1, MaxInt));
      Result := True;
      Exit;
    end;
  end;
end;

function HasSwitch(const aSwitchName: string): Boolean;
var
  i: Integer;
  lArgument: string;
begin
  Result := False;
  for i := 1 to ParamCount do
  begin
    lArgument := Trim(ParamStr(i));
    if SameText(lArgument, '--' + aSwitchName) then
      Exit(True);
  end;
end;

function TryParseUInt64Option(const aValueText: string; out aValue: UInt64): Boolean;
var
  lValueText: string;
begin
  lValueText := Trim(aValueText);
  if lValueText = '' then
    Exit(False);

  if StartsText('0x', lValueText) then
    lValueText := '$' + Copy(lValueText, 3, MaxInt)
  else if StartsText('&H', lValueText) then
    lValueText := '$' + Copy(lValueText, 3, MaxInt);

  Result := TryStrToUInt64(lValueText, aValue);
end;

function BuildDefaultStableAffinityMask: UInt64;
var
  lProcessorCount: Integer;
  lPinnedThreadCount: Integer;
  i: Integer;
begin
  lProcessorCount := Max(1, TThread.ProcessorCount);
  lPinnedThreadCount := Min(8, lProcessorCount);
  Result := 0;
  for i := 0 to lPinnedThreadCount - 1 do
    Result := Result or (UInt64(1) shl i);
end;

function ThreadPriorityToLabel(const aPriority: TThreadPriority): string;
begin
  case aPriority of
    tpIdle:
      Result := 'idle';
    tpLowest:
      Result := 'lowest';
    tpLower:
      Result := 'lower';
    tpNormal:
      Result := 'normal';
    tpHigher:
      Result := 'higher';
    tpHighest:
      Result := 'highest';
    tpTimeCritical:
      Result := 'time-critical';
  else
    Result := 'normal';
  end;
end;

procedure InitializeExecutionConfig(out aConfig: TBenchmarkExecutionConfig);
begin
  aConfig.WarmupRuns := cDefaultWarmupRuns;
  aConfig.MeasuredRuns := cDefaultMeasuredRuns;
  aConfig.StableMode := False;
  aConfig.HasProcessPriority := False;
  aConfig.ProcessPriorityClass := 0;
  aConfig.ProcessPriorityLabel := 'default';
  aConfig.HasMainThreadPriority := False;
  aConfig.MainThreadPriority := tpNormal;
  aConfig.MainThreadPriorityLabel := ThreadPriorityToLabel(tpNormal);
  aConfig.HasAffinityMask := False;
  aConfig.AffinityMask := 0;
end;

procedure ParseProcessPriority(const aValueText: string; out aPriorityClass: Cardinal; out aLabel: string);
var
  lValueText: string;
begin
  lValueText := AnsiLowerCase(Trim(aValueText));
  if (lValueText = 'normal') then
  begin
    aPriorityClass := {$IFDEF MSWINDOWS}NORMAL_PRIORITY_CLASS{$ELSE}0{$ENDIF};
    aLabel := 'normal';
  end else if (lValueText = 'above-normal') or (lValueText = 'abovenormal') then
  begin
    aPriorityClass := {$IFDEF MSWINDOWS}ABOVE_NORMAL_PRIORITY_CLASS{$ELSE}0{$ENDIF};
    aLabel := 'above-normal';
  end else if (lValueText = 'high') then
  begin
    aPriorityClass := {$IFDEF MSWINDOWS}HIGH_PRIORITY_CLASS{$ELSE}0{$ENDIF};
    aLabel := 'high';
  end else if (lValueText = 'realtime') or (lValueText = 'real-time') then
  begin
    aPriorityClass := {$IFDEF MSWINDOWS}REALTIME_PRIORITY_CLASS{$ELSE}0{$ENDIF};
    aLabel := 'realtime';
  end else
    raise Exception.CreateFmt('Invalid --process-priority value: %s', [aValueText]);
end;

procedure ParseThreadPriority(const aValueText: string; out aPriority: TThreadPriority; out aLabel: string);
var
  lValueText: string;
begin
  lValueText := AnsiLowerCase(Trim(aValueText));
  if (lValueText = 'idle') then
    aPriority := tpIdle
  else if (lValueText = 'lowest') then
    aPriority := tpLowest
  else if (lValueText = 'lower') then
    aPriority := tpLower
  else if (lValueText = 'normal') then
    aPriority := tpNormal
  else if (lValueText = 'higher') then
    aPriority := tpHigher
  else if (lValueText = 'highest') then
    aPriority := tpHighest
  else if (lValueText = 'time-critical') or (lValueText = 'timecritical') then
    aPriority := tpTimeCritical
  else
    raise Exception.CreateFmt('Invalid --main-thread-priority value: %s', [aValueText]);

  aLabel := ThreadPriorityToLabel(aPriority);
end;

procedure ApplyExecutionConfig(const aConfig: TBenchmarkExecutionConfig);
{$IFDEF MSWINDOWS}
var
  lProcessHandle: THandle;
  lAffinityMask: NativeUInt;
{$ENDIF}
begin
  TThread.CurrentThread.Priority := aConfig.MainThreadPriority;
  {$IFDEF MSWINDOWS}
  lProcessHandle := GetCurrentProcess;
  if aConfig.HasProcessPriority then
  begin
    if not SetPriorityClass(lProcessHandle, aConfig.ProcessPriorityClass) then
      RaiseLastOSError;
  end;

  if aConfig.HasAffinityMask then
  begin
    lAffinityMask := NativeUInt(aConfig.AffinityMask);
    if lAffinityMask = 0 then
      raise Exception.Create('Configured process affinity mask resolves to zero.');
    if not SetProcessAffinityMask(lProcessHandle, lAffinityMask) then
      RaiseLastOSError;
  end;
  {$ENDIF}
end;

function MedianInt64(const aValues: TArray<Int64>): Int64; forward;

function BuildSeriesStats(const aValues: TArray<Int64>): TBenchmarkSeriesStats;
var
  i: Integer;
  lCount: Integer;
  lValue: Int64;
  lSum: Extended;
  lVarianceSum: Extended;
  lDelta: Extended;
begin
  if Length(aValues) = 0 then
    raise Exception.Create('Cannot build series stats for empty value set.');

  Result.MinOpsPerSec := aValues[0];
  Result.MaxOpsPerSec := aValues[0];
  Result.MedianOpsPerSec := MedianInt64(aValues);
  lCount := Length(aValues);
  lSum := 0;
  for i := 0 to lCount - 1 do
  begin
    lValue := aValues[i];
    if lValue < Result.MinOpsPerSec then
      Result.MinOpsPerSec := lValue;
    if lValue > Result.MaxOpsPerSec then
      Result.MaxOpsPerSec := lValue;
    lSum := lSum + lValue;
  end;

  Result.MeanOpsPerSec := lSum / lCount;
  lVarianceSum := 0;
  for i := 0 to lCount - 1 do
  begin
    lDelta := aValues[i] - Result.MeanOpsPerSec;
    lVarianceSum := lVarianceSum + (lDelta * lDelta);
  end;
  Result.StdDevOpsPerSec := Sqrt(lVarianceSum / lCount);
  if Result.MeanOpsPerSec > 0 then
    Result.RelativeStdDevPct := (Result.StdDevOpsPerSec / Result.MeanOpsPerSec) * 100
  else
    Result.RelativeStdDevPct := 0;
end;

function BuildVarianceSummary(const aRuns: TArray<TBenchmarkRun>): TBenchmarkVarianceSummary;
var
  i: Integer;
  lSimpleAsyncOps: TArray<Int64>;
  lAsyncLoopOps: TArray<Int64>;
  lAsyncCollectionOps: TArray<Int64>;
begin
  SetLength(lSimpleAsyncOps, Length(aRuns));
  SetLength(lAsyncLoopOps, Length(aRuns));
  SetLength(lAsyncCollectionOps, Length(aRuns));

  for i := 0 to High(aRuns) do
  begin
    lSimpleAsyncOps[i] := aRuns[i].Metrics.SimpleAsyncCallOpsPerSec;
    lAsyncLoopOps[i] := aRuns[i].Metrics.AsyncLoopOpsPerSec;
    lAsyncCollectionOps[i] := aRuns[i].Metrics.AsyncCollectionProcessorOpsPerSec;
  end;

  Result.SimpleAsyncCall := BuildSeriesStats(lSimpleAsyncOps);
  Result.AsyncLoop := BuildSeriesStats(lAsyncLoopOps);
  Result.AsyncCollectionProcessor := BuildSeriesStats(lAsyncCollectionOps);
end;

procedure CollectMetrics(
  const aSimpleAsyncResult: TBenchmarkResult;
  const aSimpleAsyncNoNameResult: TBenchmarkResult;
  const aSimpleAsyncWakeReuseResult: TBenchmarkResult;
  const aTaskSimpleAsyncResult: TBenchmarkResult;
  const aAsyncLoopResult: TBenchmarkResult;
  const aTaskParallelForResult: TBenchmarkResult;
  const aCollectionResult: TBenchmarkResult;
  const aCollectionLockedRingResult: TBenchmarkResult;
  const aCollectionLockFreeResult: TBenchmarkResult;
  const aCollectionBatchedResult: TBenchmarkResult;
  const aCollectionBatchedLockedRingResult: TBenchmarkResult;
  const aCollectionBatchedLockFreeResult: TBenchmarkResult;
  const aCollectionMultiProducerResult: TBenchmarkResult;
  const aCollectionMultiProducerLockedRingResult: TBenchmarkResult;
  const aCollectionMultiProducerLockFreeResult: TBenchmarkResult;
  const aTaskQueueProcessorResult: TBenchmarkResult;
  const aAsyncTimerResult: TBenchmarkResult;
  const aPortableTimerResult: TBenchmarkResult;
  out aMetrics: TBenchmarkMetrics);
begin
  aMetrics.SimpleAsyncCallOpsPerSec := ThroughputAsInt64(aSimpleAsyncResult.UnitsProcessed, aSimpleAsyncResult.DurationMs);
  aMetrics.SimpleAsyncCallNoNameOpsPerSec := ThroughputAsInt64(aSimpleAsyncNoNameResult.UnitsProcessed, aSimpleAsyncNoNameResult.DurationMs);
  aMetrics.SimpleAsyncCallWakeReuseOpsPerSec := ThroughputAsInt64(aSimpleAsyncWakeReuseResult.UnitsProcessed, aSimpleAsyncWakeReuseResult.DurationMs);
  aMetrics.TaskSimpleAsyncCallOpsPerSec := ThroughputAsInt64(aTaskSimpleAsyncResult.UnitsProcessed, aTaskSimpleAsyncResult.DurationMs);
  aMetrics.AsyncLoopOpsPerSec := ThroughputAsInt64(aAsyncLoopResult.UnitsProcessed, aAsyncLoopResult.DurationMs);
  aMetrics.TaskParallelForOpsPerSec := ThroughputAsInt64(aTaskParallelForResult.UnitsProcessed, aTaskParallelForResult.DurationMs);
  aMetrics.AsyncCollectionProcessorOpsPerSec := ThroughputAsInt64(aCollectionResult.UnitsProcessed, aCollectionResult.DurationMs);
  aMetrics.AsyncCollectionProcessorLockedRingOpsPerSec := ThroughputAsInt64(aCollectionLockedRingResult.UnitsProcessed, aCollectionLockedRingResult.DurationMs);
  aMetrics.AsyncCollectionProcessorLockFreeOpsPerSec := ThroughputAsInt64(aCollectionLockFreeResult.UnitsProcessed, aCollectionLockFreeResult.DurationMs);
  aMetrics.AsyncCollectionProcessorBatchedOpsPerSec := ThroughputAsInt64(aCollectionBatchedResult.UnitsProcessed, aCollectionBatchedResult.DurationMs);
  aMetrics.AsyncCollectionProcessorBatchedLockedRingOpsPerSec := ThroughputAsInt64(aCollectionBatchedLockedRingResult.UnitsProcessed, aCollectionBatchedLockedRingResult.DurationMs);
  aMetrics.AsyncCollectionProcessorBatchedLockFreeOpsPerSec := ThroughputAsInt64(aCollectionBatchedLockFreeResult.UnitsProcessed, aCollectionBatchedLockFreeResult.DurationMs);
  aMetrics.AsyncCollectionProcessorMultiProducerOpsPerSec := ThroughputAsInt64(aCollectionMultiProducerResult.UnitsProcessed, aCollectionMultiProducerResult.DurationMs);
  aMetrics.AsyncCollectionProcessorMultiProducerLockedRingOpsPerSec := ThroughputAsInt64(aCollectionMultiProducerLockedRingResult.UnitsProcessed, aCollectionMultiProducerLockedRingResult.DurationMs);
  aMetrics.AsyncCollectionProcessorMultiProducerLockFreeOpsPerSec := ThroughputAsInt64(aCollectionMultiProducerLockFreeResult.UnitsProcessed, aCollectionMultiProducerLockFreeResult.DurationMs);
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
  const aMetrics: TBenchmarkMetrics;
  const aVarianceSummary: TBenchmarkVarianceSummary;
  const aExecutionConfig: TBenchmarkExecutionConfig);
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
    lJson.Add('    "simple_async_call_no_name_ops_per_sec": ' + IntToStr(aMetrics.SimpleAsyncCallNoNameOpsPerSec) + ',');
    lJson.Add('    "simple_async_call_wake_reuse_ops_per_sec": ' + IntToStr(aMetrics.SimpleAsyncCallWakeReuseOpsPerSec) + ',');
    lJson.Add('    "ttask_simple_async_call_ops_per_sec": ' + IntToStr(aMetrics.TaskSimpleAsyncCallOpsPerSec) + ',');
    lJson.Add('    "async_loop_ops_per_sec": ' + IntToStr(aMetrics.AsyncLoopOpsPerSec) + ',');
    lJson.Add('    "ttask_parallel_for_ops_per_sec": ' + IntToStr(aMetrics.TaskParallelForOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_locked_ring_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorLockedRingOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_lock_free_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorLockFreeOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_batched_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorBatchedOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_batched_locked_ring_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorBatchedLockedRingOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_batched_lock_free_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorBatchedLockFreeOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_multi_producer_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorMultiProducerOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_multi_producer_locked_ring_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorMultiProducerLockedRingOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_multi_producer_lock_free_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorMultiProducerLockFreeOpsPerSec) + ',');
    lJson.Add('    "ttask_threaded_queue_processor_ops_per_sec": ' + IntToStr(aMetrics.TaskThreadedQueueProcessorOpsPerSec) + ',');
    lJson.Add('    "async_timer_ops_per_sec": ' + IntToStr(aMetrics.AsyncTimerOpsPerSec) + ',');
    lJson.Add('    "portable_timer_ops_per_sec": ' + IntToStr(aMetrics.PortableTimerOpsPerSec));
    lJson.Add('  },');
    lJson.Add('  "stabilization": {');
    lJson.Add('    "stable_mode": ' + BoolToJson(aExecutionConfig.StableMode) + ',');
    lJson.Add('    "process_priority": "' + EscapeJsonString(aExecutionConfig.ProcessPriorityLabel) + '",');
    lJson.Add('    "main_thread_priority": "' + EscapeJsonString(aExecutionConfig.MainThreadPriorityLabel) + '",');
    if aExecutionConfig.HasAffinityMask then
      lJson.Add('    "affinity_mask": "' + EscapeJsonString(FormatHexUInt64(aExecutionConfig.AffinityMask)) + '"')
    else
      lJson.Add('    "affinity_mask": null');
    lJson.Add('  },');
    lJson.Add('  "variance": {');
    lJson.Add(Format('    "simple_async_call": {"min": %d, "max": %d, "mean": %s, "stddev": %s, "rsd_pct": %s},',
      [aVarianceSummary.SimpleAsyncCall.MinOpsPerSec,
       aVarianceSummary.SimpleAsyncCall.MaxOpsPerSec,
       FloatToInvariant(aVarianceSummary.SimpleAsyncCall.MeanOpsPerSec),
       FloatToInvariant(aVarianceSummary.SimpleAsyncCall.StdDevOpsPerSec),
       FloatToInvariant(aVarianceSummary.SimpleAsyncCall.RelativeStdDevPct)]));
    lJson.Add(Format('    "async_loop": {"min": %d, "max": %d, "mean": %s, "stddev": %s, "rsd_pct": %s},',
      [aVarianceSummary.AsyncLoop.MinOpsPerSec,
       aVarianceSummary.AsyncLoop.MaxOpsPerSec,
       FloatToInvariant(aVarianceSummary.AsyncLoop.MeanOpsPerSec),
       FloatToInvariant(aVarianceSummary.AsyncLoop.StdDevOpsPerSec),
       FloatToInvariant(aVarianceSummary.AsyncLoop.RelativeStdDevPct)]));
    lJson.Add(Format('    "async_collection_processor": {"min": %d, "max": %d, "mean": %s, "stddev": %s, "rsd_pct": %s}',
      [aVarianceSummary.AsyncCollectionProcessor.MinOpsPerSec,
       aVarianceSummary.AsyncCollectionProcessor.MaxOpsPerSec,
       FloatToInvariant(aVarianceSummary.AsyncCollectionProcessor.MeanOpsPerSec),
       FloatToInvariant(aVarianceSummary.AsyncCollectionProcessor.StdDevOpsPerSec),
       FloatToInvariant(aVarianceSummary.AsyncCollectionProcessor.RelativeStdDevPct)]));
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
  const aSimpleAsyncNoNameResult: TBenchmarkResult;
  const aSimpleAsyncWakeReuseResult: TBenchmarkResult;
  const aTaskSimpleAsyncResult: TBenchmarkResult;
  const aAsyncLoopResult: TBenchmarkResult;
  const aTaskParallelForResult: TBenchmarkResult;
  const aCollectionResult: TBenchmarkResult;
  const aCollectionLockedRingResult: TBenchmarkResult;
  const aCollectionLockFreeResult: TBenchmarkResult;
  const aCollectionBatchedResult: TBenchmarkResult;
  const aCollectionBatchedLockedRingResult: TBenchmarkResult;
  const aCollectionBatchedLockFreeResult: TBenchmarkResult;
  const aCollectionMultiProducerResult: TBenchmarkResult;
  const aCollectionMultiProducerLockedRingResult: TBenchmarkResult;
  const aCollectionMultiProducerLockFreeResult: TBenchmarkResult;
  const aTaskQueueProcessorResult: TBenchmarkResult;
  const aAsyncTimerResult: TBenchmarkResult;
  const aPortableTimerResult: TBenchmarkResult;
  const aAggregation: string;
  const aMeasuredRuns: Integer;
  const aWarmupRuns: Integer;
  const aMetrics: TBenchmarkMetrics;
  const aVarianceSummary: TBenchmarkVarianceSummary;
  const aExecutionConfig: TBenchmarkExecutionConfig);
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
    lJson.Add('    "simple_async_call_no_name_ops_per_sec": ' + IntToStr(aMetrics.SimpleAsyncCallNoNameOpsPerSec) + ',');
    lJson.Add('    "simple_async_call_wake_reuse_ops_per_sec": ' + IntToStr(aMetrics.SimpleAsyncCallWakeReuseOpsPerSec) + ',');
    lJson.Add('    "ttask_simple_async_call_ops_per_sec": ' + IntToStr(aMetrics.TaskSimpleAsyncCallOpsPerSec) + ',');
    lJson.Add('    "async_loop_ops_per_sec": ' + IntToStr(aMetrics.AsyncLoopOpsPerSec) + ',');
    lJson.Add('    "ttask_parallel_for_ops_per_sec": ' + IntToStr(aMetrics.TaskParallelForOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_locked_ring_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorLockedRingOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_lock_free_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorLockFreeOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_batched_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorBatchedOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_batched_locked_ring_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorBatchedLockedRingOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_batched_lock_free_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorBatchedLockFreeOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_multi_producer_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorMultiProducerOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_multi_producer_locked_ring_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorMultiProducerLockedRingOpsPerSec) + ',');
    lJson.Add('    "async_collection_processor_multi_producer_lock_free_ops_per_sec": ' + IntToStr(aMetrics.AsyncCollectionProcessorMultiProducerLockFreeOpsPerSec) + ',');
    lJson.Add('    "ttask_threaded_queue_processor_ops_per_sec": ' + IntToStr(aMetrics.TaskThreadedQueueProcessorOpsPerSec) + ',');
    lJson.Add('    "async_timer_ops_per_sec": ' + IntToStr(aMetrics.AsyncTimerOpsPerSec) + ',');
    lJson.Add('    "portable_timer_ops_per_sec": ' + IntToStr(aMetrics.PortableTimerOpsPerSec));
    lJson.Add('  },');
    lJson.Add('  "stabilization": {');
    lJson.Add('    "stable_mode": ' + BoolToJson(aExecutionConfig.StableMode) + ',');
    lJson.Add('    "process_priority": "' + EscapeJsonString(aExecutionConfig.ProcessPriorityLabel) + '",');
    lJson.Add('    "main_thread_priority": "' + EscapeJsonString(aExecutionConfig.MainThreadPriorityLabel) + '",');
    if aExecutionConfig.HasAffinityMask then
      lJson.Add('    "affinity_mask": "' + EscapeJsonString(FormatHexUInt64(aExecutionConfig.AffinityMask)) + '"')
    else
      lJson.Add('    "affinity_mask": null');
    lJson.Add('  },');
    lJson.Add('  "variance": {');
    lJson.Add(Format('    "simple_async_call": {"min": %d, "max": %d, "mean": %s, "stddev": %s, "rsd_pct": %s},',
      [aVarianceSummary.SimpleAsyncCall.MinOpsPerSec,
       aVarianceSummary.SimpleAsyncCall.MaxOpsPerSec,
       FloatToInvariant(aVarianceSummary.SimpleAsyncCall.MeanOpsPerSec),
       FloatToInvariant(aVarianceSummary.SimpleAsyncCall.StdDevOpsPerSec),
       FloatToInvariant(aVarianceSummary.SimpleAsyncCall.RelativeStdDevPct)]));
    lJson.Add(Format('    "async_loop": {"min": %d, "max": %d, "mean": %s, "stddev": %s, "rsd_pct": %s},',
      [aVarianceSummary.AsyncLoop.MinOpsPerSec,
       aVarianceSummary.AsyncLoop.MaxOpsPerSec,
       FloatToInvariant(aVarianceSummary.AsyncLoop.MeanOpsPerSec),
       FloatToInvariant(aVarianceSummary.AsyncLoop.StdDevOpsPerSec),
       FloatToInvariant(aVarianceSummary.AsyncLoop.RelativeStdDevPct)]));
    lJson.Add(Format('    "async_collection_processor": {"min": %d, "max": %d, "mean": %s, "stddev": %s, "rsd_pct": %s}',
      [aVarianceSummary.AsyncCollectionProcessor.MinOpsPerSec,
       aVarianceSummary.AsyncCollectionProcessor.MaxOpsPerSec,
       FloatToInvariant(aVarianceSummary.AsyncCollectionProcessor.MeanOpsPerSec),
       FloatToInvariant(aVarianceSummary.AsyncCollectionProcessor.StdDevOpsPerSec),
       FloatToInvariant(aVarianceSummary.AsyncCollectionProcessor.RelativeStdDevPct)]));
    lJson.Add('  },');
    lJson.Add('  "results": [');
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aSimpleAsyncResult.Name), aSimpleAsyncResult.UnitsProcessed, FloatToInvariant(aSimpleAsyncResult.DurationMs), aMetrics.SimpleAsyncCallOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aSimpleAsyncNoNameResult.Name), aSimpleAsyncNoNameResult.UnitsProcessed, FloatToInvariant(aSimpleAsyncNoNameResult.DurationMs), aMetrics.SimpleAsyncCallNoNameOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aSimpleAsyncWakeReuseResult.Name), aSimpleAsyncWakeReuseResult.UnitsProcessed, FloatToInvariant(aSimpleAsyncWakeReuseResult.DurationMs), aMetrics.SimpleAsyncCallWakeReuseOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aTaskSimpleAsyncResult.Name), aTaskSimpleAsyncResult.UnitsProcessed, FloatToInvariant(aTaskSimpleAsyncResult.DurationMs), aMetrics.TaskSimpleAsyncCallOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aAsyncLoopResult.Name), aAsyncLoopResult.UnitsProcessed, FloatToInvariant(aAsyncLoopResult.DurationMs), aMetrics.AsyncLoopOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aTaskParallelForResult.Name), aTaskParallelForResult.UnitsProcessed, FloatToInvariant(aTaskParallelForResult.DurationMs), aMetrics.TaskParallelForOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aCollectionResult.Name), aCollectionResult.UnitsProcessed, FloatToInvariant(aCollectionResult.DurationMs), aMetrics.AsyncCollectionProcessorOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aCollectionLockedRingResult.Name), aCollectionLockedRingResult.UnitsProcessed, FloatToInvariant(aCollectionLockedRingResult.DurationMs), aMetrics.AsyncCollectionProcessorLockedRingOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aCollectionLockFreeResult.Name), aCollectionLockFreeResult.UnitsProcessed, FloatToInvariant(aCollectionLockFreeResult.DurationMs), aMetrics.AsyncCollectionProcessorLockFreeOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aCollectionBatchedResult.Name), aCollectionBatchedResult.UnitsProcessed, FloatToInvariant(aCollectionBatchedResult.DurationMs), aMetrics.AsyncCollectionProcessorBatchedOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aCollectionBatchedLockedRingResult.Name), aCollectionBatchedLockedRingResult.UnitsProcessed, FloatToInvariant(aCollectionBatchedLockedRingResult.DurationMs), aMetrics.AsyncCollectionProcessorBatchedLockedRingOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aCollectionBatchedLockFreeResult.Name), aCollectionBatchedLockFreeResult.UnitsProcessed, FloatToInvariant(aCollectionBatchedLockFreeResult.DurationMs), aMetrics.AsyncCollectionProcessorBatchedLockFreeOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aCollectionMultiProducerResult.Name), aCollectionMultiProducerResult.UnitsProcessed, FloatToInvariant(aCollectionMultiProducerResult.DurationMs), aMetrics.AsyncCollectionProcessorMultiProducerOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aCollectionMultiProducerLockedRingResult.Name), aCollectionMultiProducerLockedRingResult.UnitsProcessed, FloatToInvariant(aCollectionMultiProducerLockedRingResult.DurationMs), aMetrics.AsyncCollectionProcessorMultiProducerLockedRingOpsPerSec]));
    lJson.Add(Format('    {"name":"%s","units_processed":%d,"duration_ms":%s,"ops_per_sec":%d},',
      [EscapeJsonString(aCollectionMultiProducerLockFreeResult.Name), aCollectionMultiProducerLockFreeResult.UnitsProcessed, FloatToInvariant(aCollectionMultiProducerLockFreeResult.DurationMs), aMetrics.AsyncCollectionProcessorMultiProducerLockFreeOpsPerSec]));
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
  cHeader = 'run_at_utc,simple_async_call_ops_per_sec,simple_async_call_no_name_ops_per_sec,simple_async_call_wake_reuse_ops_per_sec,ttask_simple_async_call_ops_per_sec,async_loop_ops_per_sec,ttask_parallel_for_ops_per_sec,async_collection_processor_ops_per_sec,async_collection_processor_locked_ring_ops_per_sec,async_collection_processor_lock_free_ops_per_sec,async_collection_processor_batched_ops_per_sec,async_collection_processor_batched_locked_ring_ops_per_sec,async_collection_processor_batched_lock_free_ops_per_sec,async_collection_processor_multi_producer_ops_per_sec,async_collection_processor_multi_producer_locked_ring_ops_per_sec,async_collection_processor_multi_producer_lock_free_ops_per_sec,ttask_threaded_queue_processor_ops_per_sec,async_timer_ops_per_sec,portable_timer_ops_per_sec';
  cPreviousHeader = 'run_at_utc,simple_async_call_ops_per_sec,simple_async_call_no_name_ops_per_sec,simple_async_call_wake_reuse_ops_per_sec,ttask_simple_async_call_ops_per_sec,async_loop_ops_per_sec,ttask_parallel_for_ops_per_sec,async_collection_processor_ops_per_sec,async_collection_processor_batched_ops_per_sec,async_collection_processor_multi_producer_ops_per_sec,ttask_threaded_queue_processor_ops_per_sec,async_timer_ops_per_sec,portable_timer_ops_per_sec';
  cOlderHeader = 'run_at_utc,simple_async_call_ops_per_sec,ttask_simple_async_call_ops_per_sec,async_loop_ops_per_sec,ttask_parallel_for_ops_per_sec,async_collection_processor_ops_per_sec,async_collection_processor_batched_ops_per_sec,async_collection_processor_multi_producer_ops_per_sec,ttask_threaded_queue_processor_ops_per_sec,async_timer_ops_per_sec,portable_timer_ops_per_sec';
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
    else if Trim(lLines[0]) = cPreviousHeader then
    begin
      lLines[0] := cHeader;
      for i := 1 to lLines.Count - 1 do
      begin
        if Trim(lLines[i]) <> '' then
        begin
          lParts := lLines[i].Split([',']);
          if Length(lParts) >= 13 then
          begin
            lLines[i] := Format('%s,%s,%s,%s,%s,%s,%s,%s,,,%s,,,%s,,,%s,%s,%s',
              [lParts[0], lParts[1], lParts[2], lParts[3], lParts[4], lParts[5], lParts[6],
               lParts[7], lParts[8], lParts[9], lParts[10], lParts[11], lParts[12]]);
          end else
            lLines[i] := lLines[i] + ',,,,,,';
        end;
      end;
    end
    else if Trim(lLines[0]) = cOlderHeader then
    begin
      lLines[0] := cHeader;
      for i := 1 to lLines.Count - 1 do
      begin
        if Trim(lLines[i]) <> '' then
        begin
          lParts := lLines[i].Split([',']);
          if Length(lParts) >= 11 then
          begin
            lLines[i] := Format('%s,%s,,,%s,%s,%s,%s,,,%s,,,%s,,,%s,%s,%s',
              [lParts[0], lParts[1], lParts[2], lParts[3], lParts[4], lParts[5], lParts[6],
               lParts[7], lParts[8], lParts[9], lParts[10]]);
          end else
            lLines[i] := lLines[i] + ',,,,,,,,';
        end;
      end;
    end
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
            lLines[i] := Format('%s,%s,,,,%s,,%s,,,%s,,,%s,,,,%s,%s',
              [lParts[0], lParts[1], lParts[2], lParts[3], lParts[4], lParts[5], lParts[6], lParts[7]]);
          end else
            lLines[i] := lLines[i] + ',,,,,,,,,,,';
        end;
      end;
    end else if Trim(lLines[0]) <> cHeader then
      lLines.Insert(0, cHeader);

    lLines.Add(Format('%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d',
      [aRunAtUtc,
       aMetrics.SimpleAsyncCallOpsPerSec,
       aMetrics.SimpleAsyncCallNoNameOpsPerSec,
       aMetrics.SimpleAsyncCallWakeReuseOpsPerSec,
       aMetrics.TaskSimpleAsyncCallOpsPerSec,
       aMetrics.AsyncLoopOpsPerSec,
       aMetrics.TaskParallelForOpsPerSec,
       aMetrics.AsyncCollectionProcessorOpsPerSec,
       aMetrics.AsyncCollectionProcessorLockedRingOpsPerSec,
       aMetrics.AsyncCollectionProcessorLockFreeOpsPerSec,
       aMetrics.AsyncCollectionProcessorBatchedOpsPerSec,
       aMetrics.AsyncCollectionProcessorBatchedLockedRingOpsPerSec,
       aMetrics.AsyncCollectionProcessorBatchedLockFreeOpsPerSec,
       aMetrics.AsyncCollectionProcessorMultiProducerOpsPerSec,
       aMetrics.AsyncCollectionProcessorMultiProducerLockedRingOpsPerSec,
       aMetrics.AsyncCollectionProcessorMultiProducerLockFreeOpsPerSec,
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
  const aWarmupRuns: Integer;
  const aVarianceSummary: TBenchmarkVarianceSummary;
  const aExecutionConfig: TBenchmarkExecutionConfig);
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

  WriteLatestJson(
    lLatestJsonPath,
    lRunAtUtc,
    lDateOnly,
    aAggregation,
    aMeasuredRuns,
    aWarmupRuns,
    aRun.Metrics,
    aVarianceSummary,
    aExecutionConfig);
  WriteResultsJson(
    lResultsJsonPath,
    aRun.SimpleAsyncResult,
    aRun.SimpleAsyncNoNameResult,
    aRun.SimpleAsyncWakeReuseResult,
    aRun.TaskSimpleAsyncResult,
    aRun.AsyncLoopResult,
    aRun.TaskParallelForResult,
    aRun.CollectionResult,
    aRun.CollectionLockedRingResult,
    aRun.CollectionLockFreeResult,
    aRun.CollectionBatchedResult,
    aRun.CollectionBatchedLockedRingResult,
    aRun.CollectionBatchedLockFreeResult,
    aRun.CollectionMultiProducerResult,
    aRun.CollectionMultiProducerLockedRingResult,
    aRun.CollectionMultiProducerLockFreeResult,
    aRun.TaskQueueProcessorResult,
    aRun.AsyncTimerResult,
    aRun.PortableTimerResult,
    aAggregation,
    aMeasuredRuns,
    aWarmupRuns,
    aRun.Metrics,
    aVarianceSummary,
    aExecutionConfig);
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

procedure ParseBenchmarkConfig(out aConfig: TBenchmarkExecutionConfig);
var
  i: Integer;
  lArgument: string;
  lOptionValue: Integer;
  lOptionText: string;
begin
  InitializeExecutionConfig(aConfig);

  for i := 1 to ParamCount do
  begin
    lArgument := ParamStr(i);
    if SameText(lArgument, '--help') then
    begin
      Writeln('Usage: MaxLogic.Async.Benchmark.exe [--warmup=<n>] [--repeats=<n>] [--stable]');
      Writeln('                                   [--process-priority=<normal|above-normal|high|realtime>]');
      Writeln('                                   [--main-thread-priority=<idle|lowest|lower|normal|higher|highest|time-critical>]');
      Writeln('                                   [--affinity=<hex-mask|decimal-mask>]');
      Writeln('  --warmup=<n>   Number of warmup runs (default 1, minimum 0).');
      Writeln('  --repeats=<n>  Number of measured runs (default 5, minimum 1).');
      Writeln('  --stable       Use stable benchmark profile: high process priority, highest main thread, affinity pinning.');
      Halt(0);
    end;
  end;

  if TryReadIntegerOption('warmup', lOptionValue) then
  begin
    if lOptionValue < 0 then
      raise Exception.Create('--warmup must be >= 0.');
    aConfig.WarmupRuns := lOptionValue;
  end;

  if TryReadIntegerOption('repeats', lOptionValue) then
  begin
    if lOptionValue <= 0 then
      raise Exception.Create('--repeats must be >= 1.');
    aConfig.MeasuredRuns := lOptionValue;
  end;

  aConfig.StableMode := HasSwitch('stable');

  if TryReadStringOption('process-priority', lOptionText) then
  begin
    ParseProcessPriority(lOptionText, aConfig.ProcessPriorityClass, aConfig.ProcessPriorityLabel);
    aConfig.HasProcessPriority := True;
  end;

  if TryReadStringOption('main-thread-priority', lOptionText) then
  begin
    ParseThreadPriority(lOptionText, aConfig.MainThreadPriority, aConfig.MainThreadPriorityLabel);
    aConfig.HasMainThreadPriority := True;
  end;

  if TryReadStringOption('affinity', lOptionText) then
  begin
    if not TryParseUInt64Option(lOptionText, aConfig.AffinityMask) then
      raise Exception.CreateFmt('Invalid --affinity value: %s', [lOptionText]);
    if aConfig.AffinityMask = 0 then
      raise Exception.Create('--affinity must not resolve to zero.');
    aConfig.HasAffinityMask := True;
  end;

  if aConfig.StableMode then
  begin
    if not aConfig.HasProcessPriority then
    begin
      aConfig.HasProcessPriority := True;
      aConfig.ProcessPriorityClass := {$IFDEF MSWINDOWS}HIGH_PRIORITY_CLASS{$ELSE}0{$ENDIF};
      aConfig.ProcessPriorityLabel := 'high';
    end;

    if not aConfig.HasMainThreadPriority then
    begin
      aConfig.HasMainThreadPriority := True;
      aConfig.MainThreadPriority := tpHighest;
      aConfig.MainThreadPriorityLabel := ThreadPriorityToLabel(tpHighest);
    end;

    if not aConfig.HasAffinityMask then
    begin
      aConfig.HasAffinityMask := True;
      aConfig.AffinityMask := BuildDefaultStableAffinityMask;
    end;
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
function BenchmarkSimpleAsyncCallNoName: TBenchmarkResult; forward;
function BenchmarkSimpleAsyncCallWakeReuse: TBenchmarkResult; forward;
function BenchmarkTTaskSimpleAsyncCall: TBenchmarkResult; forward;
function BenchmarkAsyncLoop: TBenchmarkResult; forward;
function BenchmarkTTaskParallelFor: TBenchmarkResult; forward;
function BenchmarkAsyncCollectionProcessor: TBenchmarkResult; forward;
function BenchmarkAsyncCollectionProcessorLockedRing: TBenchmarkResult; forward;
function BenchmarkAsyncCollectionProcessorLockFree: TBenchmarkResult; forward;
function BenchmarkAsyncCollectionProcessorBatched: TBenchmarkResult; forward;
function BenchmarkAsyncCollectionProcessorBatchedLockedRing: TBenchmarkResult; forward;
function BenchmarkAsyncCollectionProcessorBatchedLockFree: TBenchmarkResult; forward;
function BenchmarkAsyncCollectionProcessorMultiProducer: TBenchmarkResult; forward;
function BenchmarkAsyncCollectionProcessorMultiProducerLockedRing: TBenchmarkResult; forward;
function BenchmarkAsyncCollectionProcessorMultiProducerLockFree: TBenchmarkResult; forward;
function BenchmarkTTaskThreadedQueueProcessor: TBenchmarkResult; forward;
function BenchmarkAsyncTimerCompatibility: TBenchmarkResult; forward;
function BenchmarkPortableTimer: TBenchmarkResult; forward;

procedure RunBenchmarks(out aRun: TBenchmarkRun);
begin
  aRun.SimpleAsyncResult := BenchmarkSimpleAsyncCall;
  aRun.SimpleAsyncNoNameResult := BenchmarkSimpleAsyncCallNoName;
  aRun.SimpleAsyncWakeReuseResult := BenchmarkSimpleAsyncCallWakeReuse;
  aRun.TaskSimpleAsyncResult := BenchmarkTTaskSimpleAsyncCall;
  aRun.AsyncLoopResult := BenchmarkAsyncLoop;
  aRun.TaskParallelForResult := BenchmarkTTaskParallelFor;
  aRun.CollectionResult := BenchmarkAsyncCollectionProcessor;
  aRun.CollectionLockedRingResult := BenchmarkAsyncCollectionProcessorLockedRing;
  aRun.CollectionLockFreeResult := BenchmarkAsyncCollectionProcessorLockFree;
  aRun.CollectionBatchedResult := BenchmarkAsyncCollectionProcessorBatched;
  aRun.CollectionBatchedLockedRingResult := BenchmarkAsyncCollectionProcessorBatchedLockedRing;
  aRun.CollectionBatchedLockFreeResult := BenchmarkAsyncCollectionProcessorBatchedLockFree;
  aRun.CollectionMultiProducerResult := BenchmarkAsyncCollectionProcessorMultiProducer;
  aRun.CollectionMultiProducerLockedRingResult := BenchmarkAsyncCollectionProcessorMultiProducerLockedRing;
  aRun.CollectionMultiProducerLockFreeResult := BenchmarkAsyncCollectionProcessorMultiProducerLockFree;
  aRun.TaskQueueProcessorResult := BenchmarkTTaskThreadedQueueProcessor;
  aRun.AsyncTimerResult := BenchmarkAsyncTimerCompatibility;
  aRun.PortableTimerResult := BenchmarkPortableTimer;

  CollectMetrics(
    aRun.SimpleAsyncResult,
    aRun.SimpleAsyncNoNameResult,
    aRun.SimpleAsyncWakeReuseResult,
    aRun.TaskSimpleAsyncResult,
    aRun.AsyncLoopResult,
    aRun.TaskParallelForResult,
    aRun.CollectionResult,
    aRun.CollectionLockedRingResult,
    aRun.CollectionLockFreeResult,
    aRun.CollectionBatchedResult,
    aRun.CollectionBatchedLockedRingResult,
    aRun.CollectionBatchedLockFreeResult,
    aRun.CollectionMultiProducerResult,
    aRun.CollectionMultiProducerLockedRingResult,
    aRun.CollectionMultiProducerLockFreeResult,
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
  lSimpleNoNameOps: TArray<Int64>;
  lSimpleWakeReuseOps: TArray<Int64>;
  lTaskSimpleOps: TArray<Int64>;
  lAsyncLoopOps: TArray<Int64>;
  lTaskParallelOps: TArray<Int64>;
  lCollectionOps: TArray<Int64>;
  lCollectionLockedRingOps: TArray<Int64>;
  lCollectionLockFreeOps: TArray<Int64>;
  lCollectionBatchedOps: TArray<Int64>;
  lCollectionBatchedLockedRingOps: TArray<Int64>;
  lCollectionBatchedLockFreeOps: TArray<Int64>;
  lCollectionMultiProducerOps: TArray<Int64>;
  lCollectionMultiProducerLockedRingOps: TArray<Int64>;
  lCollectionMultiProducerLockFreeOps: TArray<Int64>;
  lTaskQueueOps: TArray<Int64>;
  lAsyncTimerOps: TArray<Int64>;
  lPortableTimerOps: TArray<Int64>;
  lSimpleDurationMs: TArray<Double>;
  lSimpleNoNameDurationMs: TArray<Double>;
  lSimpleWakeReuseDurationMs: TArray<Double>;
  lTaskSimpleDurationMs: TArray<Double>;
  lAsyncLoopDurationMs: TArray<Double>;
  lTaskParallelDurationMs: TArray<Double>;
  lCollectionDurationMs: TArray<Double>;
  lCollectionLockedRingDurationMs: TArray<Double>;
  lCollectionLockFreeDurationMs: TArray<Double>;
  lCollectionBatchedDurationMs: TArray<Double>;
  lCollectionBatchedLockedRingDurationMs: TArray<Double>;
  lCollectionBatchedLockFreeDurationMs: TArray<Double>;
  lCollectionMultiProducerDurationMs: TArray<Double>;
  lCollectionMultiProducerLockedRingDurationMs: TArray<Double>;
  lCollectionMultiProducerLockFreeDurationMs: TArray<Double>;
  lTaskQueueDurationMs: TArray<Double>;
  lAsyncTimerDurationMs: TArray<Double>;
  lPortableTimerDurationMs: TArray<Double>;
begin
  if Length(aRuns) = 0 then
    raise Exception.Create('Cannot aggregate benchmark runs because no measured runs are available.');

  Result := aRuns[High(aRuns)];
  lCount := Length(aRuns);

  SetLength(lSimpleOps, lCount);
  SetLength(lSimpleNoNameOps, lCount);
  SetLength(lSimpleWakeReuseOps, lCount);
  SetLength(lTaskSimpleOps, lCount);
  SetLength(lAsyncLoopOps, lCount);
  SetLength(lTaskParallelOps, lCount);
  SetLength(lCollectionOps, lCount);
  SetLength(lCollectionLockedRingOps, lCount);
  SetLength(lCollectionLockFreeOps, lCount);
  SetLength(lCollectionBatchedOps, lCount);
  SetLength(lCollectionBatchedLockedRingOps, lCount);
  SetLength(lCollectionBatchedLockFreeOps, lCount);
  SetLength(lCollectionMultiProducerOps, lCount);
  SetLength(lCollectionMultiProducerLockedRingOps, lCount);
  SetLength(lCollectionMultiProducerLockFreeOps, lCount);
  SetLength(lTaskQueueOps, lCount);
  SetLength(lAsyncTimerOps, lCount);
  SetLength(lPortableTimerOps, lCount);

  SetLength(lSimpleDurationMs, lCount);
  SetLength(lSimpleNoNameDurationMs, lCount);
  SetLength(lSimpleWakeReuseDurationMs, lCount);
  SetLength(lTaskSimpleDurationMs, lCount);
  SetLength(lAsyncLoopDurationMs, lCount);
  SetLength(lTaskParallelDurationMs, lCount);
  SetLength(lCollectionDurationMs, lCount);
  SetLength(lCollectionLockedRingDurationMs, lCount);
  SetLength(lCollectionLockFreeDurationMs, lCount);
  SetLength(lCollectionBatchedDurationMs, lCount);
  SetLength(lCollectionBatchedLockedRingDurationMs, lCount);
  SetLength(lCollectionBatchedLockFreeDurationMs, lCount);
  SetLength(lCollectionMultiProducerDurationMs, lCount);
  SetLength(lCollectionMultiProducerLockedRingDurationMs, lCount);
  SetLength(lCollectionMultiProducerLockFreeDurationMs, lCount);
  SetLength(lTaskQueueDurationMs, lCount);
  SetLength(lAsyncTimerDurationMs, lCount);
  SetLength(lPortableTimerDurationMs, lCount);

  for i := 0 to lCount - 1 do
  begin
    lSimpleOps[i] := aRuns[i].Metrics.SimpleAsyncCallOpsPerSec;
    lSimpleNoNameOps[i] := aRuns[i].Metrics.SimpleAsyncCallNoNameOpsPerSec;
    lSimpleWakeReuseOps[i] := aRuns[i].Metrics.SimpleAsyncCallWakeReuseOpsPerSec;
    lTaskSimpleOps[i] := aRuns[i].Metrics.TaskSimpleAsyncCallOpsPerSec;
    lAsyncLoopOps[i] := aRuns[i].Metrics.AsyncLoopOpsPerSec;
    lTaskParallelOps[i] := aRuns[i].Metrics.TaskParallelForOpsPerSec;
    lCollectionOps[i] := aRuns[i].Metrics.AsyncCollectionProcessorOpsPerSec;
    lCollectionLockedRingOps[i] := aRuns[i].Metrics.AsyncCollectionProcessorLockedRingOpsPerSec;
    lCollectionLockFreeOps[i] := aRuns[i].Metrics.AsyncCollectionProcessorLockFreeOpsPerSec;
    lCollectionBatchedOps[i] := aRuns[i].Metrics.AsyncCollectionProcessorBatchedOpsPerSec;
    lCollectionBatchedLockedRingOps[i] := aRuns[i].Metrics.AsyncCollectionProcessorBatchedLockedRingOpsPerSec;
    lCollectionBatchedLockFreeOps[i] := aRuns[i].Metrics.AsyncCollectionProcessorBatchedLockFreeOpsPerSec;
    lCollectionMultiProducerOps[i] := aRuns[i].Metrics.AsyncCollectionProcessorMultiProducerOpsPerSec;
    lCollectionMultiProducerLockedRingOps[i] := aRuns[i].Metrics.AsyncCollectionProcessorMultiProducerLockedRingOpsPerSec;
    lCollectionMultiProducerLockFreeOps[i] := aRuns[i].Metrics.AsyncCollectionProcessorMultiProducerLockFreeOpsPerSec;
    lTaskQueueOps[i] := aRuns[i].Metrics.TaskThreadedQueueProcessorOpsPerSec;
    lAsyncTimerOps[i] := aRuns[i].Metrics.AsyncTimerOpsPerSec;
    lPortableTimerOps[i] := aRuns[i].Metrics.PortableTimerOpsPerSec;

    lSimpleDurationMs[i] := aRuns[i].SimpleAsyncResult.DurationMs;
    lSimpleNoNameDurationMs[i] := aRuns[i].SimpleAsyncNoNameResult.DurationMs;
    lSimpleWakeReuseDurationMs[i] := aRuns[i].SimpleAsyncWakeReuseResult.DurationMs;
    lTaskSimpleDurationMs[i] := aRuns[i].TaskSimpleAsyncResult.DurationMs;
    lAsyncLoopDurationMs[i] := aRuns[i].AsyncLoopResult.DurationMs;
    lTaskParallelDurationMs[i] := aRuns[i].TaskParallelForResult.DurationMs;
    lCollectionDurationMs[i] := aRuns[i].CollectionResult.DurationMs;
    lCollectionLockedRingDurationMs[i] := aRuns[i].CollectionLockedRingResult.DurationMs;
    lCollectionLockFreeDurationMs[i] := aRuns[i].CollectionLockFreeResult.DurationMs;
    lCollectionBatchedDurationMs[i] := aRuns[i].CollectionBatchedResult.DurationMs;
    lCollectionBatchedLockedRingDurationMs[i] := aRuns[i].CollectionBatchedLockedRingResult.DurationMs;
    lCollectionBatchedLockFreeDurationMs[i] := aRuns[i].CollectionBatchedLockFreeResult.DurationMs;
    lCollectionMultiProducerDurationMs[i] := aRuns[i].CollectionMultiProducerResult.DurationMs;
    lCollectionMultiProducerLockedRingDurationMs[i] := aRuns[i].CollectionMultiProducerLockedRingResult.DurationMs;
    lCollectionMultiProducerLockFreeDurationMs[i] := aRuns[i].CollectionMultiProducerLockFreeResult.DurationMs;
    lTaskQueueDurationMs[i] := aRuns[i].TaskQueueProcessorResult.DurationMs;
    lAsyncTimerDurationMs[i] := aRuns[i].AsyncTimerResult.DurationMs;
    lPortableTimerDurationMs[i] := aRuns[i].PortableTimerResult.DurationMs;
  end;

  Result.Metrics.SimpleAsyncCallOpsPerSec := MedianInt64(lSimpleOps);
  Result.Metrics.SimpleAsyncCallNoNameOpsPerSec := MedianInt64(lSimpleNoNameOps);
  Result.Metrics.SimpleAsyncCallWakeReuseOpsPerSec := MedianInt64(lSimpleWakeReuseOps);
  Result.Metrics.TaskSimpleAsyncCallOpsPerSec := MedianInt64(lTaskSimpleOps);
  Result.Metrics.AsyncLoopOpsPerSec := MedianInt64(lAsyncLoopOps);
  Result.Metrics.TaskParallelForOpsPerSec := MedianInt64(lTaskParallelOps);
  Result.Metrics.AsyncCollectionProcessorOpsPerSec := MedianInt64(lCollectionOps);
  Result.Metrics.AsyncCollectionProcessorLockedRingOpsPerSec := MedianInt64(lCollectionLockedRingOps);
  Result.Metrics.AsyncCollectionProcessorLockFreeOpsPerSec := MedianInt64(lCollectionLockFreeOps);
  Result.Metrics.AsyncCollectionProcessorBatchedOpsPerSec := MedianInt64(lCollectionBatchedOps);
  Result.Metrics.AsyncCollectionProcessorBatchedLockedRingOpsPerSec := MedianInt64(lCollectionBatchedLockedRingOps);
  Result.Metrics.AsyncCollectionProcessorBatchedLockFreeOpsPerSec := MedianInt64(lCollectionBatchedLockFreeOps);
  Result.Metrics.AsyncCollectionProcessorMultiProducerOpsPerSec := MedianInt64(lCollectionMultiProducerOps);
  Result.Metrics.AsyncCollectionProcessorMultiProducerLockedRingOpsPerSec := MedianInt64(lCollectionMultiProducerLockedRingOps);
  Result.Metrics.AsyncCollectionProcessorMultiProducerLockFreeOpsPerSec := MedianInt64(lCollectionMultiProducerLockFreeOps);
  Result.Metrics.TaskThreadedQueueProcessorOpsPerSec := MedianInt64(lTaskQueueOps);
  Result.Metrics.AsyncTimerOpsPerSec := MedianInt64(lAsyncTimerOps);
  Result.Metrics.PortableTimerOpsPerSec := MedianInt64(lPortableTimerOps);

  Result.SimpleAsyncResult.DurationMs := MedianDouble(lSimpleDurationMs);
  Result.SimpleAsyncNoNameResult.DurationMs := MedianDouble(lSimpleNoNameDurationMs);
  Result.SimpleAsyncWakeReuseResult.DurationMs := MedianDouble(lSimpleWakeReuseDurationMs);
  Result.TaskSimpleAsyncResult.DurationMs := MedianDouble(lTaskSimpleDurationMs);
  Result.AsyncLoopResult.DurationMs := MedianDouble(lAsyncLoopDurationMs);
  Result.TaskParallelForResult.DurationMs := MedianDouble(lTaskParallelDurationMs);
  Result.CollectionResult.DurationMs := MedianDouble(lCollectionDurationMs);
  Result.CollectionLockedRingResult.DurationMs := MedianDouble(lCollectionLockedRingDurationMs);
  Result.CollectionLockFreeResult.DurationMs := MedianDouble(lCollectionLockFreeDurationMs);
  Result.CollectionBatchedResult.DurationMs := MedianDouble(lCollectionBatchedDurationMs);
  Result.CollectionBatchedLockedRingResult.DurationMs := MedianDouble(lCollectionBatchedLockedRingDurationMs);
  Result.CollectionBatchedLockFreeResult.DurationMs := MedianDouble(lCollectionBatchedLockFreeDurationMs);
  Result.CollectionMultiProducerResult.DurationMs := MedianDouble(lCollectionMultiProducerDurationMs);
  Result.CollectionMultiProducerLockedRingResult.DurationMs := MedianDouble(lCollectionMultiProducerLockedRingDurationMs);
  Result.CollectionMultiProducerLockFreeResult.DurationMs := MedianDouble(lCollectionMultiProducerLockFreeDurationMs);
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

function BenchmarkSimpleAsyncCallNoName: TBenchmarkResult;
var
  lAsync: iAsync;
  i: Integer;
  lStopwatch: TStopwatch;
begin
  Result.Name := 'SimpleAsyncCall (no name)';
  Result.UnitsProcessed := cSimpleAsyncCalls;

  lStopwatch := TStopwatch.StartNew;
  for i := 1 to cSimpleAsyncCalls do
  begin
    lAsync := SimpleAsyncCall(
      procedure
      begin
      end);
    lAsync.WaitFor;
  end;
  lStopwatch.Stop;

  Result.DurationMs := lStopwatch.Elapsed.TotalMilliseconds;
end;

function BenchmarkSimpleAsyncCallWakeReuse: TBenchmarkResult;
var
  lAsync: iAsync;
  lProc: TThreadProcedure;
  i: Integer;
  lStopwatch: TStopwatch;
begin
  Result.Name := 'SimpleAsyncCall.WakeUpReuse';
  Result.UnitsProcessed := cSimpleAsyncCalls;

  lProc := procedure
    begin
    end;

  lAsync := SimpleAsyncCall(lProc);
  lStopwatch := TStopwatch.StartNew;
  lAsync.WaitFor;
  for i := 2 to cSimpleAsyncCalls do
  begin
    lAsync.WakeUp(lProc, '');
    lAsync.WaitFor;
  end;
  lStopwatch.Stop;
  lAsync := nil;

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

procedure ConfigureCollectionProcessorQueueMode(
  const aProcessor: TAsyncCollectionProcessor<TWorkItem>;
  const aThreadCount: Integer;
  const aQueueMode: TAsyncCollectionProcessorQueueMode);
begin
  aProcessor.SimultanousThreadCount := aThreadCount;
  if aQueueMode <> acpqmLegacyLockedQueue then
  begin
    aProcessor.QueueMode := aQueueMode;
    aProcessor.QueueCapacity := cCollectionQueueCapacity;
    aProcessor.BackpressureMode := acpbmBlock;
  end;
end;

function BenchmarkAsyncCollectionProcessorMode(
  const aQueueMode: TAsyncCollectionProcessorQueueMode;
  const aResultName: string): TBenchmarkResult;
var
  i: Integer;
  lProcessor: TAsyncCollectionProcessor<TWorkItem>;
  lProcessedCount: Integer;
  lStopwatch: TStopwatch;
  lThreadCount: Integer;
begin
  Result.Name := aResultName;
  Result.UnitsProcessed := cCollectionItems;

  lThreadCount := Max(1, TThread.ProcessorCount);
  lProcessedCount := 0;

  lProcessor := TAsyncCollectionProcessor<TWorkItem>.Create;
  try
    ConfigureCollectionProcessorQueueMode(lProcessor, lThreadCount, aQueueMode);
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
    raise Exception.CreateFmt('%s processed %d/%d items.', [aResultName, lProcessedCount, cCollectionItems]);

  Result.DurationMs := lStopwatch.Elapsed.TotalMilliseconds;
end;

function BenchmarkAsyncCollectionProcessorBatchedMode(
  const aQueueMode: TAsyncCollectionProcessorQueueMode;
  const aResultName: string): TBenchmarkResult;
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
  Result.Name := aResultName;
  Result.UnitsProcessed := cCollectionItems;

  lThreadCount := Max(1, TThread.ProcessorCount);
  lProcessedCount := 0;
  lCurrentValue := 1;

  lProcessor := TAsyncCollectionProcessor<TWorkItem>.Create;
  try
    ConfigureCollectionProcessorQueueMode(lProcessor, lThreadCount, aQueueMode);
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
    raise Exception.CreateFmt('%s processed %d/%d items.', [aResultName, lProcessedCount, cCollectionItems]);

  Result.DurationMs := lStopwatch.Elapsed.TotalMilliseconds;
end;

function BenchmarkAsyncCollectionProcessorMultiProducerMode(
  const aQueueMode: TAsyncCollectionProcessorQueueMode;
  const aResultName: string): TBenchmarkResult;
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

  Result.Name := aResultName;
  Result.UnitsProcessed := lExpected;

  lThreadCount := Max(1, TThread.ProcessorCount);
  lProcessedCount := 0;

  lProcessor := TAsyncCollectionProcessor<TWorkItem>.Create;
  try
    ConfigureCollectionProcessorQueueMode(lProcessor, lThreadCount, aQueueMode);
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
    raise Exception.CreateFmt('%s processed %d/%d items.', [aResultName, lProcessedCount, lExpected]);

  Result.DurationMs := lStopwatch.Elapsed.TotalMilliseconds;
end;

function BenchmarkAsyncCollectionProcessor: TBenchmarkResult;
begin
  Result := BenchmarkAsyncCollectionProcessorMode(acpqmLegacyLockedQueue, 'TAsyncCollectionProcessor');
end;

function BenchmarkAsyncCollectionProcessorLockedRing: TBenchmarkResult;
begin
  Result := BenchmarkAsyncCollectionProcessorMode(acpqmLockedRingQueue, 'TAsyncCollectionProcessor.LockedRingQueue');
end;

function BenchmarkAsyncCollectionProcessorLockFree: TBenchmarkResult;
begin
  Result := BenchmarkAsyncCollectionProcessorMode(acpqmLockFreeMpmcRingQueue, 'TAsyncCollectionProcessor.LockFreeQueue');
end;

function BenchmarkAsyncCollectionProcessorBatched: TBenchmarkResult;
begin
  Result := BenchmarkAsyncCollectionProcessorBatchedMode(acpqmLegacyLockedQueue, 'TAsyncCollectionProcessor.BatchedAddStorm');
end;

function BenchmarkAsyncCollectionProcessorBatchedLockedRing: TBenchmarkResult;
begin
  Result := BenchmarkAsyncCollectionProcessorBatchedMode(acpqmLockedRingQueue, 'TAsyncCollectionProcessor.BatchedAddStorm.LockedRingQueue');
end;

function BenchmarkAsyncCollectionProcessorBatchedLockFree: TBenchmarkResult;
begin
  Result := BenchmarkAsyncCollectionProcessorBatchedMode(acpqmLockFreeMpmcRingQueue, 'TAsyncCollectionProcessor.BatchedAddStorm.LockFreeQueue');
end;

function BenchmarkAsyncCollectionProcessorMultiProducer: TBenchmarkResult;
begin
  Result := BenchmarkAsyncCollectionProcessorMultiProducerMode(acpqmLegacyLockedQueue, 'TAsyncCollectionProcessor.MultiProducerContention');
end;

function BenchmarkAsyncCollectionProcessorMultiProducerLockedRing: TBenchmarkResult;
begin
  Result := BenchmarkAsyncCollectionProcessorMultiProducerMode(acpqmLockedRingQueue, 'TAsyncCollectionProcessor.MultiProducerContention.LockedRingQueue');
end;

function BenchmarkAsyncCollectionProcessorMultiProducerLockFree: TBenchmarkResult;
begin
  Result := BenchmarkAsyncCollectionProcessorMultiProducerMode(acpqmLockFreeMpmcRingQueue, 'TAsyncCollectionProcessor.MultiProducerContention.LockFreeQueue');
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

procedure PrintHeader(const aConfig: TBenchmarkExecutionConfig);
begin
  Writeln('maxAsync baseline benchmark');
  Writeln(Format('CPU threads: %d', [TThread.ProcessorCount]));
  Writeln(Format('Warmup runs: %d', [aConfig.WarmupRuns]));
  Writeln(Format('Measured runs: %d (median aggregation)', [aConfig.MeasuredRuns]));
  Writeln(Format('Stable mode: %s', [BoolToStr(aConfig.StableMode, True)]));
  Writeln(Format('Process priority: %s', [aConfig.ProcessPriorityLabel]));
  Writeln(Format('Main thread priority: %s', [aConfig.MainThreadPriorityLabel]));
  if aConfig.HasAffinityMask then
    Writeln(Format('Process affinity mask: %s', [FormatHexUInt64(aConfig.AffinityMask)]))
  else
  begin
    Writeln('Process affinity mask: default');
  end;
  Writeln(Format('SimpleAsyncCall calls: %d', [cSimpleAsyncCalls]));
  Writeln(Format('AsyncLoop iterations: %d', [cAsyncLoopIterations]));
  Writeln(Format('Collection items: %d', [cCollectionItems]));
  Writeln(Format('Collection batch size: %d', [cCollectionBatchSize]));
  Writeln(Format('Collection bounded queue capacity: %d', [cCollectionQueueCapacity]));
  Writeln(Format('Multi-producer adders: %d x %d items', [cMultiProducerCount, cMultiProducerItemsPerProducer]));
  Writeln(Format('Timer ticks: %d @ %d ms', [cTimerTicks, cTimerIntervalMs]));
  Writeln(StringOfChar('-', 92));
  Writeln(Format('%-38s %12s  %10s  %14s', ['Benchmark', 'Units', 'Time', 'Throughput']));
  Writeln(StringOfChar('-', 92));
end;

procedure PrintAggregatedResults(const aRun: TBenchmarkRun);
begin
  PrintResult(aRun.SimpleAsyncResult, aRun.Metrics.SimpleAsyncCallOpsPerSec);
  PrintResult(aRun.SimpleAsyncNoNameResult, aRun.Metrics.SimpleAsyncCallNoNameOpsPerSec);
  PrintResult(aRun.SimpleAsyncWakeReuseResult, aRun.Metrics.SimpleAsyncCallWakeReuseOpsPerSec);
  PrintResult(aRun.TaskSimpleAsyncResult, aRun.Metrics.TaskSimpleAsyncCallOpsPerSec);
  PrintResult(aRun.AsyncLoopResult, aRun.Metrics.AsyncLoopOpsPerSec);
  PrintResult(aRun.TaskParallelForResult, aRun.Metrics.TaskParallelForOpsPerSec);
  PrintResult(aRun.CollectionResult, aRun.Metrics.AsyncCollectionProcessorOpsPerSec);
  PrintResult(aRun.CollectionLockedRingResult, aRun.Metrics.AsyncCollectionProcessorLockedRingOpsPerSec);
  PrintResult(aRun.CollectionLockFreeResult, aRun.Metrics.AsyncCollectionProcessorLockFreeOpsPerSec);
  PrintResult(aRun.CollectionBatchedResult, aRun.Metrics.AsyncCollectionProcessorBatchedOpsPerSec);
  PrintResult(aRun.CollectionBatchedLockedRingResult, aRun.Metrics.AsyncCollectionProcessorBatchedLockedRingOpsPerSec);
  PrintResult(aRun.CollectionBatchedLockFreeResult, aRun.Metrics.AsyncCollectionProcessorBatchedLockFreeOpsPerSec);
  PrintResult(aRun.CollectionMultiProducerResult, aRun.Metrics.AsyncCollectionProcessorMultiProducerOpsPerSec);
  PrintResult(aRun.CollectionMultiProducerLockedRingResult, aRun.Metrics.AsyncCollectionProcessorMultiProducerLockedRingOpsPerSec);
  PrintResult(aRun.CollectionMultiProducerLockFreeResult, aRun.Metrics.AsyncCollectionProcessorMultiProducerLockFreeOpsPerSec);
  PrintResult(aRun.TaskQueueProcessorResult, aRun.Metrics.TaskThreadedQueueProcessorOpsPerSec);
  PrintResult(aRun.AsyncTimerResult, aRun.Metrics.AsyncTimerOpsPerSec);
  PrintResult(aRun.PortableTimerResult, aRun.Metrics.PortableTimerOpsPerSec);
end;

procedure PrintSeriesStats(const aLabel: string; const aStats: TBenchmarkSeriesStats);
begin
  Writeln(Format('  %-27s median=%10d  min=%10d  max=%10d  mean=%10.1f  stddev=%10.1f  rsd=%6.2f%%',
    [aLabel,
     aStats.MedianOpsPerSec,
     aStats.MinOpsPerSec,
     aStats.MaxOpsPerSec,
     aStats.MeanOpsPerSec,
     aStats.StdDevOpsPerSec,
     aStats.RelativeStdDevPct]));
end;

procedure PrintVarianceSummary(const aVarianceSummary: TBenchmarkVarianceSummary);
begin
  Writeln('Variance summary (measured runs):');
  PrintSeriesStats('simple_async_call_ops/s', aVarianceSummary.SimpleAsyncCall);
  PrintSeriesStats('async_loop_ops/s', aVarianceSummary.AsyncLoop);
  PrintSeriesStats('async_collection_ops/s', aVarianceSummary.AsyncCollectionProcessor);
end;

var
  lConfig: TBenchmarkExecutionConfig;
  lWarmupRunIndex: Integer;
  lMeasuredRunIndex: Integer;
  lMeasuredRunResults: TArray<TBenchmarkRun>;
  lRun: TBenchmarkRun;
  lAggregatedRun: TBenchmarkRun;
  lVarianceSummary: TBenchmarkVarianceSummary;
begin
  try
    ParseBenchmarkConfig(lConfig);
    ApplyExecutionConfig(lConfig);
    PrintHeader(lConfig);

    for lWarmupRunIndex := 1 to lConfig.WarmupRuns do
    begin
      Writeln(Format('Warmup run %d/%d ...', [lWarmupRunIndex, lConfig.WarmupRuns]));
      RunBenchmarks(lRun);
    end;

    SetLength(lMeasuredRunResults, lConfig.MeasuredRuns);
    for lMeasuredRunIndex := 1 to lConfig.MeasuredRuns do
    begin
      Writeln(Format('Measured run %d/%d ...', [lMeasuredRunIndex, lConfig.MeasuredRuns]));
      RunBenchmarks(lMeasuredRunResults[Pred(lMeasuredRunIndex)]);
    end;

    lAggregatedRun := BuildAggregatedRun(lMeasuredRunResults);
    lVarianceSummary := BuildVarianceSummary(lMeasuredRunResults);
    Writeln(StringOfChar('-', 92));
    Writeln('Aggregated results (median):');
    PrintAggregatedResults(lAggregatedRun);
    Writeln(StringOfChar('-', 92));
    PrintVarianceSummary(lVarianceSummary);

    Writeln(StringOfChar('-', 92));

    SaveBenchmarkArtifacts(
      lAggregatedRun,
      'median_of_runs',
      lConfig.MeasuredRuns,
      lConfig.WarmupRuns,
      lVarianceSummary,
      lConfig);
  except
    on lException: Exception do
    begin
      Writeln(lException.ClassName, ': ', lException.Message);
      System.ExitCode := 1;
    end;
  end;
end.
