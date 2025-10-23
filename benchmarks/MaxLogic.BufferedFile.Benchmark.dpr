program MaxLogicBufferedFileBenchmark;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.IOUtils,
  System.Diagnostics,
  MaxLogic.BufferedFile in '..\MaxLogic.BufferedFile.pas';

type
  TOperation = (opRead, opWrite);

  TBenchmarkResult = record
    Engine: string;
    Operation: TOperation;
    ChunkSize: Integer;
    BytesProcessed: Int64;
    DurationMs: Double;
    Completed: Boolean;
    Checksum: UInt64;
  end;

const
  cTargetBytes = Int64(64) * 1024 * 1024; // 64 MiB
  cChunkSizes: array[0..10] of Integer = (1, 4, 16, 64, 256, 1024, 4096, 16384, 65536, 262144, 1048576);
  cTimeoutSeconds: Double = 30.0;
  cTimeoutMilliseconds = 30000;

function OperationName(const aValue: TOperation): string;
begin
  case aValue of
    opRead:
      Result := 'Read';
    opWrite:
      Result := 'Write';
  else
    Result := 'Unknown';
  end;
end;

function FormatBytes(const aBytes: Int64; const aDecimals: Integer = 2): string;
const
  cUnits: array[0..4] of string = ('B', 'KB', 'MB', 'GB', 'TB');
var
  lValue: Double;
  lUnit: Integer;
begin
  if aBytes = 0 then
    Exit('0 B');

  lValue := aBytes;
  lUnit := 0;
  while (lValue >= 1024) and (lUnit < High(cUnits)) do
  begin
    lValue := lValue / 1024;
    Inc(lUnit);
  end;

  if (aDecimals <= 0) or (lUnit = 0) then
    Result := Format('%d %s', [Round(lValue), cUnits[lUnit]])
  else
    Result := Format('%.*f %s', [aDecimals, lValue, cUnits[lUnit]]);
end;

function SumBuffer(const aBuffer: TBytes; const aCount: Integer): UInt64; inline;
var
  lIndex: Integer;
begin
  Result := 0;
  for lIndex := 0 to aCount - 1 do
    Result := Result + aBuffer[lIndex];
end;

function CreateSampleFile(const aSize: Int64): string;
var
  lStream: TFileStream;
  lBuffer: TBytes;
  lRemaining: Int64;
  lChunk: Integer;
  lIndex: Integer;
begin
  Result := TPath.Combine(TPath.GetTempPath, 'buffered-file-bench-' + TPath.GetRandomFileName);
  lStream := TFileStream.Create(Result, fmCreate or fmShareDenyWrite);
  try
    SetLength(lBuffer, 1024 * 1024);
    for lIndex := 0 to High(lBuffer) do
      lBuffer[lIndex] := Byte(lIndex);
    lRemaining := aSize;
    while lRemaining > 0 do
    begin
      lChunk := Length(lBuffer);
      if lRemaining < lChunk then
        lChunk := Integer(lRemaining);
      lStream.WriteBuffer(lBuffer[0], lChunk);
      Dec(lRemaining, lChunk);
    end;
  finally
    lStream.Free;
  end;
end;

function CreateTempFileName(const aPrefix: string): string;
begin
  Result := TPath.Combine(TPath.GetTempPath, aPrefix + '-' + TPath.GetRandomFileName);
end;

function EnsureChunkSize(const aChunkSize: Integer): Integer;
begin
  if aChunkSize > 0 then
    Result := aChunkSize
  else
    Result := 1;
end;

function BenchmarkBufferedFileRead(const aFileName: string; const aChunkSize: Integer;
  const aTimeoutMs: Int64; const aTargetBytes: Int64): TBenchmarkResult;
var
  lReader: TBufferedFile;
  lBuffer: TBytes;
  lStopwatch: TStopwatch;
  lBytesProcessed: Int64;
  lToRead: Integer;
  lRemaining: Int64;
  lElapsedMs: Double;
  lChecksum: UInt64;
  lEffectiveChunk: Integer;
  lBufferSize: Integer;
  lTarget: Int64;
begin
  Result.Engine := 'TBufferedFile';
  Result.Operation := opRead;
  Result.ChunkSize := aChunkSize;
  Result.BytesProcessed := 0;
  Result.DurationMs := 0;
  Result.Completed := False;
  Result.Checksum := 0;

  lEffectiveChunk := EnsureChunkSize(aChunkSize);
  if lEffectiveChunk < TBufferedFile.cDefaultBlockSize then
    lBufferSize := TBufferedFile.cDefaultBlockSize
  else
    lBufferSize := lEffectiveChunk * 10; // keep the in memory buffer of our TBufferedFile always greater then the amount we will read at once

  SetLength(lBuffer, lEffectiveChunk);

  lReader := TBufferedFile.Create(lBufferSize);
  try
    lReader.Open(aFileName);
    lTarget := lReader.Size;
    if (aTargetBytes > 0) and (aTargetBytes < lTarget) then
      lTarget := aTargetBytes;

    lBytesProcessed := 0;
    lChecksum := 0;
    lStopwatch := TStopwatch.StartNew;
    while lBytesProcessed < lTarget do
    begin
      if lStopwatch.ElapsedMilliseconds >= aTimeoutMs then
        Break;

      lRemaining := lTarget - lBytesProcessed;
      if lRemaining <= 0 then
        Break;

      lToRead := lEffectiveChunk;
      if lRemaining < lToRead then
        lToRead := Integer(lRemaining);
      if lToRead <= 0 then
        Break;

      lReader.copyBytes(lReader.Position, lToRead, lBuffer);
      lReader.Seek(lToRead);
      Inc(lBytesProcessed, lToRead);
      Inc(lChecksum, SumBuffer(lBuffer, lToRead));

      if lReader.Position >= lReader.Size then
        Break;
    end;
    lStopwatch.Stop;
    lElapsedMs := lStopwatch.ElapsedMilliseconds;
  finally
    lReader.Free;
  end;

  Result.BytesProcessed := lBytesProcessed;
  Result.DurationMs := lElapsedMs;
  Result.Completed := lBytesProcessed >= lTarget;
  Result.Checksum := lChecksum;
end;

function BenchmarkBufferedFileStreamRead(const aFileName: string; const aChunkSize: Integer;
  const aTimeoutMs: Int64; const aTargetBytes: Int64): TBenchmarkResult;
var
  lStream: TBufferedFileStream;
  lBuffer: TBytes;
  lStopwatch: TStopwatch;
  lBytesProcessed: Int64;
  lRead: Integer;
  lToRead: Integer;
  lRemaining: Int64;
  lElapsedMs: Double;
  lChecksum: UInt64;
  lEffectiveChunk: Integer;
  lTarget: Int64;
begin
  Result.Engine := 'TBufferedFileStream';
  Result.Operation := opRead;
  Result.ChunkSize := aChunkSize;
  Result.BytesProcessed := 0;
  Result.DurationMs := 0;
  Result.Completed := False;
  Result.Checksum := 0;

  lEffectiveChunk := EnsureChunkSize(aChunkSize);
  SetLength(lBuffer, lEffectiveChunk);

  lStream := TBufferedFileStream.Create(aFileName, fmOpenRead, fmShareDenyNone);
  try
    lTarget := lStream.Size;
    if (aTargetBytes > 0) and (aTargetBytes < lTarget) then
      lTarget := aTargetBytes;

    lBytesProcessed := 0;
    lChecksum := 0;
    lStopwatch := TStopwatch.StartNew;
    while lBytesProcessed < lTarget do
    begin
      if lStopwatch.ElapsedMilliseconds >= aTimeoutMs then
        Break;

      lRemaining := lTarget - lBytesProcessed;
      if lRemaining <= 0 then
        Break;

      lToRead := lEffectiveChunk;
      if lRemaining < lToRead then
        lToRead := Integer(lRemaining);
      lRead := lStream.Read(lBuffer[0], lToRead);
      if lRead <= 0 then
        Break;

      Inc(lBytesProcessed, lRead);
      Inc(lChecksum, SumBuffer(lBuffer, lRead));
    end;
    lStopwatch.Stop;
    lElapsedMs := lStopwatch.ElapsedMilliseconds;
  finally
    lStream.Free;
  end;

  Result.BytesProcessed := lBytesProcessed;
  Result.DurationMs := lElapsedMs;
  Result.Completed := lBytesProcessed >= lTarget;
  Result.Checksum := lChecksum;
end;

function BenchmarkFileStreamRead(const aFileName: string; const aChunkSize: Integer;
  const aTimeoutMs: Int64; const aTargetBytes: Int64): TBenchmarkResult;
var
  lStream: TFileStream;
  lBuffer: TBytes;
  lStopwatch: TStopwatch;
  lBytesProcessed: Int64;
  lToRead: Integer;
  lRemaining: Int64;
  lElapsedMs: Double;
  lChecksum: UInt64;
  lEffectiveChunk: Integer;
  lTarget: Int64;
begin
  Result.Engine := 'TFileStream.ReadBuffer';
  Result.Operation := opRead;
  Result.ChunkSize := aChunkSize;
  Result.BytesProcessed := 0;
  Result.DurationMs := 0;
  Result.Completed := False;
  Result.Checksum := 0;

  lEffectiveChunk := EnsureChunkSize(aChunkSize);
  SetLength(lBuffer, lEffectiveChunk);

  lStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  try
    lTarget := lStream.Size;
    if (aTargetBytes > 0) and (aTargetBytes < lTarget) then
      lTarget := aTargetBytes;

    lBytesProcessed := 0;
    lChecksum := 0;
    lStopwatch := TStopwatch.StartNew;
    while lBytesProcessed < lTarget do
    begin
      if lStopwatch.ElapsedMilliseconds >= aTimeoutMs then
        Break;

      lRemaining := lTarget - lBytesProcessed;
      if lRemaining <= 0 then
        Break;

      lToRead := lEffectiveChunk;
      if lRemaining < lToRead then
        lToRead := Integer(lRemaining);
      if lToRead <= 0 then
        Break;

      try
        lStream.ReadBuffer(lBuffer[0], lToRead);
        Inc(lBytesProcessed, lToRead);
        Inc(lChecksum, SumBuffer(lBuffer, lToRead));
      except
        on EReadError do
          Break;
      end;
    end;
    lStopwatch.Stop;
    lElapsedMs := lStopwatch.ElapsedMilliseconds;
  finally
    lStream.Free;
  end;

  Result.BytesProcessed := lBytesProcessed;
  Result.DurationMs := lElapsedMs;
  Result.Completed := lBytesProcessed >= lTarget;
  Result.Checksum := lChecksum;
end;

function BenchmarkBufferedFileStreamWrite(const aChunkSize: Integer;
  const aTimeoutMs: Int64; const aTargetBytes: Int64): TBenchmarkResult;
var
  lStream: TBufferedFileStream;
  lBuffer: TBytes;
  lStopwatch: TStopwatch;
  lBytesProcessed: Int64;
  lToWrite: Integer;
  lRemaining: Int64;
  lElapsedMs: Double;
  lChecksum: UInt64;
  lEffectiveChunk: Integer;
  lTempFile: string;
  lIndex: Integer;
begin
  Result.Engine := 'TBufferedFileStream';
  Result.Operation := opWrite;
  Result.ChunkSize := aChunkSize;
  Result.BytesProcessed := 0;
  Result.DurationMs := 0;
  Result.Completed := False;
  Result.Checksum := 0;

  lEffectiveChunk := EnsureChunkSize(aChunkSize);
  SetLength(lBuffer, lEffectiveChunk);
  for lIndex := 0 to lEffectiveChunk - 1 do
    lBuffer[lIndex] := Byte((lIndex + lEffectiveChunk) and $FF);

  lTempFile := CreateTempFileName('buffered-file-write');
  lBytesProcessed := 0;
  lChecksum := 0;
  lElapsedMs := 0;

  lStream := TBufferedFileStream.Create(lTempFile, fmCreate, fmShareExclusive);
  try
    lStopwatch := TStopwatch.StartNew;
    while lBytesProcessed < aTargetBytes do
    begin
      if lStopwatch.ElapsedMilliseconds >= aTimeoutMs then
        Break;

      lRemaining := aTargetBytes - lBytesProcessed;
      if lRemaining <= 0 then
        Break;

      lToWrite := lEffectiveChunk;
      if lRemaining < lToWrite then
        lToWrite := Integer(lRemaining);
      if lToWrite <= 0 then
        Break;

      lStream.WriteBuffer(lBuffer[0], lToWrite);
      Inc(lBytesProcessed, lToWrite);
      Inc(lChecksum, SumBuffer(lBuffer, lToWrite));
    end;
    lStopwatch.Stop;
    lElapsedMs := lStopwatch.ElapsedMilliseconds;
  finally
    lStream.Free;
    if (lTempFile <> '') and TFile.Exists(lTempFile) then
      TFile.Delete(lTempFile);
  end;

  Result.BytesProcessed := lBytesProcessed;
  Result.DurationMs := lElapsedMs;
  Result.Completed := lBytesProcessed >= aTargetBytes;
  Result.Checksum := lChecksum;
end;

function BenchmarkFileStreamWrite(const aChunkSize: Integer;
  const aTimeoutMs: Int64; const aTargetBytes: Int64): TBenchmarkResult;
var
  lStream: TFileStream;
  lBuffer: TBytes;
  lStopwatch: TStopwatch;
  lBytesProcessed: Int64;
  lToWrite: Integer;
  lRemaining: Int64;
  lElapsedMs: Double;
  lChecksum: UInt64;
  lEffectiveChunk: Integer;
  lTempFile: string;
  lIndex: Integer;
begin
  Result.Engine := 'TFileStream.WriteBuffer';
  Result.Operation := opWrite;
  Result.ChunkSize := aChunkSize;
  Result.BytesProcessed := 0;
  Result.DurationMs := 0;
  Result.Completed := False;
  Result.Checksum := 0;

  lEffectiveChunk := EnsureChunkSize(aChunkSize);
  SetLength(lBuffer, lEffectiveChunk);
  for lIndex := 0 to lEffectiveChunk - 1 do
    lBuffer[lIndex] := Byte((lIndex + lEffectiveChunk) and $FF);

  lTempFile := CreateTempFileName('filestream-write');
  lBytesProcessed := 0;
  lChecksum := 0;
  lElapsedMs := 0;

  lStream := TFileStream.Create(lTempFile, fmCreate or fmShareDenyWrite);
  try
    lStopwatch := TStopwatch.StartNew;
    while lBytesProcessed < aTargetBytes do
    begin
      if lStopwatch.ElapsedMilliseconds >= aTimeoutMs then
        Break;

      lRemaining := aTargetBytes - lBytesProcessed;
      if lRemaining <= 0 then
        Break;

      lToWrite := lEffectiveChunk;
      if lRemaining < lToWrite then
        lToWrite := Integer(lRemaining);
      if lToWrite <= 0 then
        Break;

      lStream.WriteBuffer(lBuffer[0], lToWrite);
      Inc(lBytesProcessed, lToWrite);
      Inc(lChecksum, SumBuffer(lBuffer, lToWrite));
    end;
    lStopwatch.Stop;
    lElapsedMs := lStopwatch.ElapsedMilliseconds;
  finally
    lStream.Free;
    if (lTempFile <> '') and TFile.Exists(lTempFile) then
      TFile.Delete(lTempFile);
  end;

  Result.BytesProcessed := lBytesProcessed;
  Result.DurationMs := lElapsedMs;
  Result.Completed := lBytesProcessed >= aTargetBytes;
  Result.Checksum := lChecksum;
end;

procedure PrintResults(const aResults: TArray<TBenchmarkResult>);
var
  lResult: TBenchmarkResult;
  lCurrentOperation: TOperation;
  lFirst: Boolean;
  lRate: Double;
  lRateText: string;
  lCompletedText: string;
  lChunkText: string;
  lBytesText: string;
begin
  if Length(aResults) = 0 then
  begin
    Writeln('No benchmark results to display.');
    Exit;
  end;

  Writeln;
  Writeln('Benchmark results (timeout ', cTimeoutSeconds:0:0, ' s, target ', FormatBytes(cTargetBytes), ')');

  lFirst := True;
  lCurrentOperation := opRead;
  for lResult in aResults do
  begin
    if lFirst or (lResult.Operation <> lCurrentOperation) then
    begin
      Writeln;
      Writeln(UpperCase(OperationName(lResult.Operation)), ' benchmarks');
      Writeln('---------------------------------------------');
      lCurrentOperation := lResult.Operation;
      lFirst := False;
    end;

    if (lResult.DurationMs <= 0) or (lResult.BytesProcessed = 0) then
      lRate := 0
    else
      lRate := (lResult.BytesProcessed / 1024 / 1024) / (lResult.DurationMs / 1000);

    if lRate <= 0 then
      lRateText := '   n/a'
    else
      lRateText := Format('%7.2f', [lRate]);

    if lResult.Completed then
      lCompletedText := 'yes'
    else
      lCompletedText := 'no';

    lChunkText := FormatBytes(lResult.ChunkSize, 0);
    lBytesText := FormatBytes(lResult.BytesProcessed);

    Writeln(Format('%5s | chunk=%10s | engine=%-24s | bytes=%12s | time=%10.3f ms | rate=%8s MB/s | completed=%s',
      [OperationName(lResult.Operation),
       lChunkText,
       lResult.Engine,
       lBytesText,
       lResult.DurationMs,
       lRateText,
       lCompletedText]));
  end;
end;

procedure RunReadBenchmarks(const aFileName: string; const aTimeoutMs: Int64;
  const aTargetBytes: Int64; const aResults: TList<TBenchmarkResult>);
var
  lChunk: Integer;
begin
  for lChunk in cChunkSizes do
  begin
    Writeln(Format('Running read benchmarks (chunk %s)...', [FormatBytes(lChunk, 0)]));
    Writeln('  • TBufferedFile');
    aResults.Add(BenchmarkBufferedFileRead(aFileName, lChunk, aTimeoutMs, aTargetBytes));
    Writeln('  • TBufferedFileStream');
    aResults.Add(BenchmarkBufferedFileStreamRead(aFileName, lChunk, aTimeoutMs, aTargetBytes));
    Writeln('  • TFileStream.ReadBuffer');
    aResults.Add(BenchmarkFileStreamRead(aFileName, lChunk, aTimeoutMs, aTargetBytes));
  end;
end;

procedure RunWriteBenchmarks(const aTimeoutMs: Int64; const aTargetBytes: Int64;
  const aResults: TList<TBenchmarkResult>);
var
  lChunk: Integer;
begin
  for lChunk in cChunkSizes do
  begin
    Writeln(Format('Running write benchmarks (chunk %s)...', [FormatBytes(lChunk, 0)]));
    Writeln('  • TBufferedFileStream');
    aResults.Add(BenchmarkBufferedFileStreamWrite(lChunk, aTimeoutMs, aTargetBytes));
    Writeln('  • TFileStream.WriteBuffer');
    aResults.Add(BenchmarkFileStreamWrite(lChunk, aTimeoutMs, aTargetBytes));
  end;
end;

var
  lSampleFile: string;
  lResults: TList<TBenchmarkResult>;
  lTimeoutMs: Int64;
begin
  ReportMemoryLeaksOnShutdown := True;
  try
    Writeln('MaxLogic buffered file benchmark');
    Writeln('Preparing sample data ...');

    lResults := TList<TBenchmarkResult>.Create;
    try
      lTimeoutMs := cTimeoutMilliseconds;
      lSampleFile := CreateSampleFile(cTargetBytes);
      try
        RunReadBenchmarks(lSampleFile, lTimeoutMs, cTargetBytes, lResults);
      finally
        if (lSampleFile <> '') and TFile.Exists(lSampleFile) then
          TFile.Delete(lSampleFile);
      end;

      RunWriteBenchmarks(lTimeoutMs, cTargetBytes, lResults);
      PrintResults(lResults.ToArray);
    finally
      lResults.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
