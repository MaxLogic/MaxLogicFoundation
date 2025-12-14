unit MaxLogic.hash.FNV1a;

{
  FNV-1a (Fowler-Noll-Vo) 32-bit hash algorithm
  
  Fast, simple, non-cryptographic hash with good distribution.
  Best for: small to medium keys (< 256 bytes), hash tables, checksums.
  
  Trade-offs:
  - FAST: Single-pass, minimal operations per byte
  - SIMPLE: Easy to understand and maintain
  - SEQUENTIAL: Cannot parallelize (each step depends on prior result)
  
  Compared to alternatives:
  - vs xxHash: ~30-40% slower on large data, but simpler
  - vs Murmur: Similar speed/quality, slightly simpler
}

interface

/// <summary>
/// Computes FNV-1a hash of a string (UTF-16 byte representation).
/// </summary>
function FNV1a32(const aValue: string): Cardinal; overload;

/// <summary>
/// Computes FNV-1a hash of a raw byte buffer.
/// </summary>
function FNV1a32(const aData: PByte; aLength: NativeInt): Cardinal; overload;

implementation

const
  CFNVOffsetBasis32 = Cardinal($811C9DC5);
  CFNVPrime32 = Cardinal(16777619);

function FNV1a32(const aData: PByte; aLength: NativeInt): Cardinal;
var
  lPtr, lEnd: PByte;
begin
{$IFOPT Q+}
  {$DEFINE FNV_QPLUS}
  {$Q-}
{$ENDIF}
{$IFOPT R+}
  {$DEFINE FNV_RPLUS}
  {$R-}
{$ENDIF}
  Result := CFNVOffsetBasis32;
  if (aData = nil) or (aLength = 0) then
    Exit;
  
  lPtr := aData;
  lEnd := aData + aLength;
  
  // Core FNV-1a loop: xor byte, then multiply by prime
  while lPtr < lEnd do
  begin
    Result := (Result xor lPtr^) * CFNVPrime32;
    Inc(lPtr);
  end;
{$IFDEF FNV_RPLUS}
  {$UNDEF FNV_RPLUS}
  {$R+}
{$ENDIF}
{$IFDEF FNV_QPLUS}
  {$UNDEF FNV_QPLUS}
  {$Q+}
{$ENDIF}
end;

function FNV1a32(const aValue: string): Cardinal;
begin
  if Length(aValue) = 0 then
    Exit(CFNVOffsetBasis32);
  Result := FNV1a32(PByte(PChar(aValue)), Length(aValue) * SizeOf(Char));
end;

end.
