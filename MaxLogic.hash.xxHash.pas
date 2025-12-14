unit MaxLogic.hash.xxHash;

{
  xxHash32 - Extremely fast non-cryptographic hash algorithm
  
  Original C implementation: https://github.com/Cyan4973/xxHash
  Copyright (C) 2012-2021 Yann Collet
  
  Key features:
  - PARALLEL: Uses 4 independent accumulators for CPU-level parallelism
  - FAST: ~2-3× faster than FNV-1a on data > 64 bytes
  - COMPLEX: More code, harder to maintain than FNV-1a
  
  Best for: large buffers (> 256 bytes), proven hot paths in profiling
  
  Algorithm overview:
  1. For buffers >= 16 bytes: process 16-byte chunks with 4 parallel lanes
  2. Merge the 4 accumulators
  3. Process remaining bytes in 4-byte and 1-byte chunks
  4. Final avalanche mixing for bit distribution
}

interface

/// <summary>
/// Computes xxHash32 of a string (UTF-16 byte representation).
/// </summary>
function xxHash32(const aValue: string; aSeed: Cardinal = 0): Cardinal; overload;

/// <summary>
/// Computes xxHash32 of a raw byte buffer.
/// </summary>
function xxHash32(const aData: PByte; aLength: NativeInt; aSeed: Cardinal = 0): Cardinal; overload;

implementation

const
  CXXPrime1 = Cardinal(2654435761);
  CXXPrime2 = Cardinal(2246822519);
  CXXPrime3 = Cardinal(3266489917);
  CXXPrime4 = Cardinal(668265263);
  CXXPrime5 = Cardinal(374761393);

function Rotl32(aValue: Cardinal; aBits: Integer): Cardinal; inline;
begin
  Result := (aValue shl aBits) or (aValue shr (32 - aBits));
end;

function xxHash32(const aData: PByte; aLength: NativeInt; aSeed: Cardinal = 0): Cardinal;
var
  lPtr, lEnd: PByte;
  v1, v2, v3, v4: Cardinal;
begin
{$IFOPT Q+}
  {$DEFINE XXHASH_QPLUS}
  {$Q-}
{$ENDIF}
{$IFOPT R+}
  {$DEFINE XXHASH_RPLUS}
  {$R-}
{$ENDIF}
  if (aData = nil) or (aLength = 0) then
    Exit(aSeed + CXXPrime5);

  lPtr := aData;
  lEnd := aData + aLength;

  // Step 1: Process 16-byte blocks with 4 parallel accumulators
  if aLength >= 16 then
  begin
    // Initialize accumulators
    v1 := aSeed + CXXPrime1 + CXXPrime2;
    v2 := aSeed + CXXPrime2;
    v3 := aSeed;
    v4 := aSeed - CXXPrime1;

    repeat
      // Process 4×4 bytes in parallel (instruction-level parallelism)
      v1 := Rotl32(v1 + PCardinal(lPtr)^ * CXXPrime2, 13) * CXXPrime1;
      Inc(lPtr, 4);
      v2 := Rotl32(v2 + PCardinal(lPtr)^ * CXXPrime2, 13) * CXXPrime1;
      Inc(lPtr, 4);
      v3 := Rotl32(v3 + PCardinal(lPtr)^ * CXXPrime2, 13) * CXXPrime1;
      Inc(lPtr, 4);
      v4 := Rotl32(v4 + PCardinal(lPtr)^ * CXXPrime2, 13) * CXXPrime1;
      Inc(lPtr, 4);
    until lPtr > (lEnd - 16);

    // Step 2: Merge accumulators
    Result := Rotl32(v1, 1) + Rotl32(v2, 7) + Rotl32(v3, 12) + Rotl32(v4, 18);
  end
  else
    Result := aSeed + CXXPrime5;

  // Step 3: Add total length
  Result := Result + Cardinal(aLength);

  // Step 4: Process remaining 4-byte chunks
  while (lEnd - lPtr) >= 4 do
  begin
    Result := Rotl32(Result + PCardinal(lPtr)^ * CXXPrime3, 17) * CXXPrime4;
    Inc(lPtr, 4);
  end;

  // Step 5: Process remaining 1-byte chunks
  while lPtr < lEnd do
  begin
    Result := Rotl32(Result + lPtr^ * CXXPrime5, 11) * CXXPrime1;
    Inc(lPtr);
  end;

  // Step 6: Final avalanche mixing (ensures all bits affect final hash)
  Result := Result xor (Result shr 15);
  Result := Result * CXXPrime2;
  Result := Result xor (Result shr 13);
  Result := Result * CXXPrime3;
  Result := Result xor (Result shr 16);
{$IFDEF XXHASH_RPLUS}
  {$UNDEF XXHASH_RPLUS}
  {$R+}
{$ENDIF}
{$IFDEF XXHASH_QPLUS}
  {$UNDEF XXHASH_QPLUS}
  {$Q+}
{$ENDIF}
end;

function xxHash32(const aValue: string; aSeed: Cardinal = 0): Cardinal;
begin
  if Length(aValue) = 0 then
    Exit(xxHash32(nil, 0, aSeed));
  Result := xxHash32(PByte(PChar(aValue)), Length(aValue) * SizeOf(Char), aSeed);
end;

end.
