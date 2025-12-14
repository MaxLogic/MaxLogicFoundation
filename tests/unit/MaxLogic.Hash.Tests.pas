unit MaxLogic.Hash.Tests;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  THashAlgorithmTests = class
  public
    // Correctness tests
    [Test] procedure FNV1a_KnownVectors;
    [Test] procedure xxHash_KnownVectors;
    [Test] procedure Murmur3_KnownVectors;
    
    [Test] procedure FNV1a_EmptyString;
    [Test] procedure xxHash_EmptyString;
    [Test] procedure Murmur3_EmptyString;
    
    [Test] procedure FNV1a_SingleChar;
    [Test] procedure xxHash_SingleChar;
    [Test] procedure Murmur3_SingleChar;
    
    [Test] procedure FNV1a_Unicode;
    [Test] procedure xxHash_Unicode;
    [Test] procedure Murmur3_Unicode;
    
    // Collision resistance (basic check)
    [Test] procedure FNV1a_SimilarStrings_DifferentHashes;
    [Test] procedure xxHash_SimilarStrings_DifferentHashes;
    [Test] procedure Murmur3_SimilarStrings_DifferentHashes;
    
    // Performance benchmarks
    [Test] procedure Benchmark_KeyLikeStrings;
    [Test] procedure Benchmark_ComplexStrings;
    [Test] procedure Benchmark_LargeBuffers;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.Diagnostics, System.Character,
  MaxLogic.hash.FNV1a, MaxLogic.hash.xxHash, MaxLogic.hash.Murmur;

const
  // ASCII test string for known vector tests
  CTestStringASCII = 'The quick brown fox jumps over the lazy dog';
  
  // Expected hash values for UTF-16LE byte representation (Delphi string)
  // Computed by hashing the UTF-16LE bytes of the test string above
  CFNVExpected_Fox = Cardinal($22E95EE2);      // FNV-1a of UTF-16LE bytes
  CXXHashExpected_Fox = Cardinal($9B674131);   // xxHash of UTF-16LE bytes
  CMurmur3Expected_Fox = Cardinal($709CC343);  // MurmurHash3 of UTF-16LE bytes

function IsSilent: Boolean;
var
  lValue: string;
begin
  lValue := GetEnvironmentVariable('DUNITX_SILENT');
  Result := SameText(lValue, '1') or SameText(lValue, 'true');
end;

procedure LogBench(const aMessage: string);
begin
  if not IsSilent then
  begin
    System.Writeln;
    System.Writeln(aMessage);
  end;
end;

function TicksToMicroseconds(const aTicks: Int64): Double;
begin
  if aTicks <= 0 then
    Exit(0);
  Result := (aTicks / TStopwatch.Frequency) * 1E6;
end;

function TicksToMilliseconds(const aTicks: Int64): Double;
begin
  if aTicks <= 0 then
    Exit(0);
  Result := (aTicks / TStopwatch.Frequency) * 1E3;
end;

{ Correctness Tests }

procedure THashAlgorithmTests.FNV1a_KnownVectors;
var
  lHash: Cardinal;
begin
  lHash := FNV1a32(CTestStringASCII);
  Assert.AreEqual(CFNVExpected_Fox, lHash, 
    Format('FNV-1a hash mismatch. Expected $%.8x, got $%.8x', [CFNVExpected_Fox, lHash]));
end;

procedure THashAlgorithmTests.xxHash_KnownVectors;
var
  lHash: Cardinal;
begin
  lHash := xxHash32(CTestStringASCII);
  Assert.AreEqual(CXXHashExpected_Fox, lHash,
    Format('xxHash32 hash mismatch. Expected $%.8x, got $%.8x', [CXXHashExpected_Fox, lHash]));
end;

procedure THashAlgorithmTests.Murmur3_KnownVectors;
var
  lHash: Cardinal;
begin
  lHash := MurmurHash3_32(CTestStringASCII);
  Assert.AreEqual(CMurmur3Expected_Fox, lHash,
    Format('MurmurHash3 hash mismatch. Expected $%.8x, got $%.8x', [CMurmur3Expected_Fox, lHash]));
end;

procedure THashAlgorithmTests.FNV1a_EmptyString;
var
  lHash: Cardinal;
begin
  lHash := FNV1a32('');
  Assert.AreEqual(Cardinal($811C9DC5), lHash, 'FNV-1a empty string should return offset basis');
end;

procedure THashAlgorithmTests.xxHash_EmptyString;
var
  lHash: Cardinal;
begin
  lHash := xxHash32('');
  Assert.AreNotEqual(Cardinal(0), lHash, 'xxHash empty string should return non-zero');
end;

procedure THashAlgorithmTests.Murmur3_EmptyString;
var
  lHash: Cardinal;
begin
  lHash := MurmurHash3_32('');
  Assert.AreNotEqual(Cardinal(0), lHash, 'MurmurHash3 empty string should return non-zero');
end;

procedure THashAlgorithmTests.FNV1a_SingleChar;
var
  lHash1, lHash2: Cardinal;
begin
  lHash1 := FNV1a32('a');
  lHash2 := FNV1a32('b');
  Assert.AreNotEqual(lHash1, lHash2, 'Different single chars should hash differently');
end;

procedure THashAlgorithmTests.xxHash_SingleChar;
var
  lHash1, lHash2: Cardinal;
begin
  lHash1 := xxHash32('a');
  lHash2 := xxHash32('b');
  Assert.AreNotEqual(lHash1, lHash2, 'Different single chars should hash differently');
end;

procedure THashAlgorithmTests.Murmur3_SingleChar;
var
  lHash1, lHash2: Cardinal;
begin
  lHash1 := MurmurHash3_32('a');
  lHash2 := MurmurHash3_32('b');
  Assert.AreNotEqual(lHash1, lHash2, 'Different single chars should hash differently');
end;

procedure THashAlgorithmTests.FNV1a_Unicode;
var
  lHash1, lHash2: Cardinal;
begin
  lHash1 := FNV1a32('Hello');
  lHash2 := FNV1a32('Hełło');  // Polish ł
  Assert.AreNotEqual(lHash1, lHash2, 'ASCII vs Unicode should hash differently');
  
  lHash1 := FNV1a32('世界');  // Chinese
  lHash2 := FNV1a32('世畍');  // Similar Chinese chars
  Assert.AreNotEqual(lHash1, lHash2, 'Similar Unicode should hash differently');
end;

procedure THashAlgorithmTests.xxHash_Unicode;
var
  lHash1, lHash2: Cardinal;
begin
  lHash1 := xxHash32('Hello');
  lHash2 := xxHash32('Hełło');
  Assert.AreNotEqual(lHash1, lHash2, 'ASCII vs Unicode should hash differently');
  
  lHash1 := xxHash32('世界');
  lHash2 := xxHash32('世畍');
  Assert.AreNotEqual(lHash1, lHash2, 'Similar Unicode should hash differently');
end;

procedure THashAlgorithmTests.Murmur3_Unicode;
var
  lHash1, lHash2: Cardinal;
begin
  lHash1 := MurmurHash3_32('Hello');
  lHash2 := MurmurHash3_32('Hełło');
  Assert.AreNotEqual(lHash1, lHash2, 'ASCII vs Unicode should hash differently');
  
  lHash1 := MurmurHash3_32('世界');
  lHash2 := MurmurHash3_32('世畍');
  Assert.AreNotEqual(lHash1, lHash2, 'Similar Unicode should hash differently');
end;

procedure THashAlgorithmTests.FNV1a_SimilarStrings_DifferentHashes;
var
  lHash1, lHash2, lHash3: Cardinal;
begin
  lHash1 := FNV1a32('Key123');
  lHash2 := FNV1a32('Key124');
  lHash3 := FNV1a32('Key223');
  
  Assert.AreNotEqual(lHash1, lHash2, 'Key123 vs Key124');
  Assert.AreNotEqual(lHash1, lHash3, 'Key123 vs Key223');
  Assert.AreNotEqual(lHash2, lHash3, 'Key124 vs Key223');
end;

procedure THashAlgorithmTests.xxHash_SimilarStrings_DifferentHashes;
var
  lHash1, lHash2, lHash3: Cardinal;
begin
  lHash1 := xxHash32('Key123');
  lHash2 := xxHash32('Key124');
  lHash3 := xxHash32('Key223');
  
  Assert.AreNotEqual(lHash1, lHash2, 'Key123 vs Key124');
  Assert.AreNotEqual(lHash1, lHash3, 'Key123 vs Key223');
  Assert.AreNotEqual(lHash2, lHash3, 'Key124 vs Key223');
end;

procedure THashAlgorithmTests.Murmur3_SimilarStrings_DifferentHashes;
var
  lHash1, lHash2, lHash3: Cardinal;
begin
  lHash1 := MurmurHash3_32('Key123');
  lHash2 := MurmurHash3_32('Key124');
  lHash3 := MurmurHash3_32('Key223');
  
  Assert.AreNotEqual(lHash1, lHash2, 'Key123 vs Key124');
  Assert.AreNotEqual(lHash1, lHash3, 'Key123 vs Key223');
  Assert.AreNotEqual(lHash2, lHash3, 'Key124 vs Key223');
end;

{ Performance Benchmarks }

procedure THashAlgorithmTests.Benchmark_KeyLikeStrings;
const
  cIterations = 100000;
var
  lKeys: TArray<string>;
  lIndex: Integer;
  lSW: TStopwatch;
  lFNVTicks, lXXTicks, lMurmurTicks: Int64;
  lAccum: Cardinal;
  lMsg: string;
begin
  // Generate realistic key-like strings: 20-50 chars, mostly ASCII
  SetLength(lKeys, cIterations);
  for lIndex := 0 to High(lKeys) do
    lKeys[lIndex] := Format('ENV_VAR_%d_CONFIG_SETTING_%d', [lIndex mod 1000, lIndex]);

  lAccum := 0;

  // FNV-1a
  lSW := TStopwatch.StartNew;
  for lIndex := 0 to High(lKeys) do
    lAccum := lAccum xor FNV1a32(lKeys[lIndex]);
  lFNVTicks := lSW.ElapsedTicks;

  // xxHash
  lSW := TStopwatch.StartNew;
  for lIndex := 0 to High(lKeys) do
    lAccum := lAccum xor xxHash32(lKeys[lIndex]);
  lXXTicks := lSW.ElapsedTicks;

  // MurmurHash3
  lSW := TStopwatch.StartNew;
  for lIndex := 0 to High(lKeys) do
    lAccum := lAccum xor MurmurHash3_32(lKeys[lIndex]);
  lMurmurTicks := lSW.ElapsedTicks;

  // Prevent optimization
  Assert.AreNotEqual(Cardinal(0), lAccum);

  lMsg := Format('Key-like strings (%d × ~35 chars):', [cIterations]) + sLineBreak +
    Format('  FNV-1a:  %.0f μs', [TicksToMicroseconds(lFNVTicks)]) + sLineBreak +
    Format('  xxHash:  %.0f μs  (%.2fx vs FNV)', [TicksToMicroseconds(lXXTicks), lFNVTicks / lXXTicks]) + sLineBreak +
    Format('  Murmur3: %.0f μs  (%.2fx vs FNV)', [TicksToMicroseconds(lMurmurTicks), lFNVTicks / lMurmurTicks]);
  
  LogBench(lMsg);
end;

procedure THashAlgorithmTests.Benchmark_ComplexStrings;
const
  cIterations = 50000;
var
  lStrings: TArray<string>;
  lIndex: Integer;
  lSW: TStopwatch;
  lFNVTicks, lXXTicks, lMurmurTicks: Int64;
  lAccum: Cardinal;
  lMsg: string;
begin
  // Complex strings: mixed ASCII/Unicode, varying lengths
  SetLength(lStrings, cIterations);
  for lIndex := 0 to High(lStrings) do
  begin
    case lIndex mod 5 of
      0: lStrings[lIndex] := Format('User_%d_αβγδε_Settings', [lIndex]);
      1: lStrings[lIndex] := Format('Config_世界_%d_Test', [lIndex]);
      2: lStrings[lIndex] := Format('PATH_C:\Users\Test_%d\AppData\Local\Temp', [lIndex]);
      3: lStrings[lIndex] := 'Short' + IntToStr(lIndex);
      4: lStrings[lIndex] := Format('VeryLongConfigurationKeyNameWithLotsOfCharacters_%d_End', [lIndex]);
    end;
  end;

  lAccum := 0;

  // FNV-1a
  lSW := TStopwatch.StartNew;
  for lIndex := 0 to High(lStrings) do
    lAccum := lAccum xor FNV1a32(lStrings[lIndex]);
  lFNVTicks := lSW.ElapsedTicks;

  // xxHash
  lSW := TStopwatch.StartNew;
  for lIndex := 0 to High(lStrings) do
    lAccum := lAccum xor xxHash32(lStrings[lIndex]);
  lXXTicks := lSW.ElapsedTicks;

  // MurmurHash3
  lSW := TStopwatch.StartNew;
  for lIndex := 0 to High(lStrings) do
    lAccum := lAccum xor MurmurHash3_32(lStrings[lIndex]);
  lMurmurTicks := lSW.ElapsedTicks;

  Assert.AreNotEqual(Cardinal(0), lAccum);

  lMsg := Format('Complex strings (%d mixed Unicode/ASCII):', [cIterations]) + sLineBreak +
    Format('  FNV-1a:  %.0f μs', [TicksToMicroseconds(lFNVTicks)]) + sLineBreak +
    Format('  xxHash:  %.0f μs  (%.2fx vs FNV)', [TicksToMicroseconds(lXXTicks), lFNVTicks / lXXTicks]) + sLineBreak +
    Format('  Murmur3: %.0f μs  (%.2fx vs FNV)', [TicksToMicroseconds(lMurmurTicks), lFNVTicks / lMurmurTicks]);
  
  LogBench(lMsg);
end;

procedure THashAlgorithmTests.Benchmark_LargeBuffers;
const
  cBufferSize = 1024 * 1024; // 1 MB
  cIterations = 100;
var
  lBuffer: TBytes;
  lIndex: Integer;
  lSW: TStopwatch;
  lFNVTicks, lXXTicks, lMurmurTicks: Int64;
  lFNVHash, lXXHash, lMurmurHash: Cardinal;
  lMsg: string;
begin
  // Create 1MB buffer with pseudo-random data
  SetLength(lBuffer, cBufferSize);
  for lIndex := 0 to High(lBuffer) do
    lBuffer[lIndex] := Byte(lIndex xor (lIndex shr 8) xor (lIndex shr 16));

  // FNV-1a
  lSW := TStopwatch.StartNew;
  for lIndex := 1 to cIterations do
    lFNVHash := FNV1a32(@lBuffer[0], cBufferSize);
  lFNVTicks := lSW.ElapsedTicks;

  // xxHash
  lSW := TStopwatch.StartNew;
  for lIndex := 1 to cIterations do
    lXXHash := xxHash32(@lBuffer[0], cBufferSize);
  lXXTicks := lSW.ElapsedTicks;

  // MurmurHash3
  lSW := TStopwatch.StartNew;
  for lIndex := 1 to cIterations do
    lMurmurHash := MurmurHash3_32(@lBuffer[0], cBufferSize);
  lMurmurTicks := lSW.ElapsedTicks;

  // Verify hashes are non-zero and different
  Assert.AreNotEqual(Cardinal(0), lFNVHash, 'FNV hash should not be zero');
  Assert.AreNotEqual(Cardinal(0), lXXHash, 'xxHash should not be zero');
  Assert.AreNotEqual(Cardinal(0), lMurmurHash, 'Murmur hash should not be zero');
  Assert.AreNotEqual(lFNVHash, lXXHash, 'FNV and xxHash should differ');

  lMsg := Format('Large buffers (%d × 1 MB):', [cIterations]) + sLineBreak +
    Format('  FNV-1a:  %.1f ms  (%.1f MB/s)', [
      TicksToMilliseconds(lFNVTicks), 
      (cIterations * cBufferSize / 1024.0 / 1024.0) / (lFNVTicks / TStopwatch.Frequency)]) + sLineBreak +
    Format('  xxHash:  %.1f ms  (%.1f MB/s, %.2fx vs FNV)', [
      TicksToMilliseconds(lXXTicks),
      (cIterations * cBufferSize / 1024.0 / 1024.0) / (lXXTicks / TStopwatch.Frequency),
      lFNVTicks / lXXTicks]) + sLineBreak +
    Format('  Murmur3: %.1f ms  (%.1f MB/s, %.2fx vs FNV)', [
      TicksToMilliseconds(lMurmurTicks),
      (cIterations * cBufferSize / 1024.0 / 1024.0) / (lMurmurTicks / TStopwatch.Frequency),
      lFNVTicks / lMurmurTicks]);
  
  LogBench(lMsg);
end;

initialization
  TDUnitX.RegisterTestFixture(THashAlgorithmTests);

end.
