unit MaxLogic.hash.Murmur;

{
  MurmurHash family - Fast, non-cryptographic hash algorithms
  
  Version: 1.2
  History:
  - 2015-07-21: added Q-/R- to disable overflow/range checking
  - 2024-12-14: added string overload for MurmurHash3_32
  
  Variants:
  - Murmur2A: For AnsiString (legacy, 8-bit chars)
  - Murmur2: Generic byte buffer
  - MurmurHash3_32: Latest version, best distribution
  
  Original: https://github.com/aappleby/smhasher
  MurmurHash is public domain by Austin Appleby
}

interface

/// <summary>
/// MurmurHash2A for AnsiString (legacy 8-bit).
/// </summary>
function Murmur2A(const s: AnsiString; const Seed: LongWord = $9747B28C): LongWord;

/// <summary>
/// MurmurHash2 for raw byte buffer.
/// </summary>
function Murmur2(buffer: pByte; BufferSize: LongWord; const Seed: LongWord = $9747B28C): LongWord;

/// <summary>
/// MurmurHash3_32 for raw byte buffer (recommended variant).
/// </summary>
function MurmurHash3_32(AKey: pByte; ALength: uInt32; ASeed: uInt32 = $9747B28C): uInt32; overload;

/// <summary>
/// MurmurHash3_32 for string (UTF-16 byte representation).
/// </summary>
function MurmurHash3_32(const aValue: string; ASeed: uInt32 = $9747B28C): uInt32; overload;

implementation

uses
  System.SysUtils;

{$Q-}
{$R-}


function Murmur2A(const s: AnsiString; const Seed: LongWord = $9747B28C): LongWord;
var
  h: LongWord;
  len: LongWord;
  k: LongWord;
  data: integer;
const
  // 'm' and 'r' are mixing constants generated offline.
  // They're not really 'magic', they just happen to work well.
  m = $5BD1E995;
  r = 24;
begin
{$Q-}
{$R-}
  len := Length(s);

  // The default seed, $9747b28c, is from the original C library

  // Initialize the hash to a 'random' value
  h := Seed xor len;

  // Mix 4 bytes at a time into the hash
  data := 1;

  while (len >= 4) do
  begin
    k := PLongWord(@s[data])^;

    k := k * m;
    k := k xor (k shr r);
    k := k * m;

    h := h * m;
    h := h xor k;

    data := data + 4;
    len := len - 4;
  end;

  { Handle the last few bytes of the input array
    S: ... $69 $18 $2f
  }
  Assert(len <= 3);
  if len = 3 then
    h := h xor (LongWord(s[data + 2]) shl 16);
  if len >= 2 then
    h := h xor (LongWord(s[data + 1]) shl 8);
  if len >= 1 then
  begin
    h := h xor (LongWord(s[data]));
    h := h * m;
  end;

  // Do a few final mixes of the hash to ensure the last few
  // bytes are well-incorporated.
  h := h xor (h shr 13);
  h := h * m;
  h := h xor (h shr 15);

  result := h;
end;

function Murmur2(buffer: pByte; BufferSize: LongWord; const Seed: LongWord = $9747B28C): LongWord;
var
  h: LongWord;
  len: LongWord;
  k: LongWord;
const
  // 'm' and 'r' are mixing constants generated offline.
  // They're not really 'magic', they just happen to work well.
  m = $5BD1E995;
  r = 24;
begin
{$Q-}
{$R-}
  len := BufferSize;

  // The default seed, $9747b28c, is from the original C library

  // Initialize the hash to a 'random' value
  h := Seed xor len;

  // Mix 4 bytes at a time into the hash

  while (len >= 4) do
  begin
    try
      k := PLongWord(buffer)^;
    except
      on E: Exception do
      begin
        raise Exception.Create(E.Message +
          '; len=' + IntToStr(len) +
          '; BufferSize=' + IntToStr(BufferSize));
      end;
    end;

    k := k * m;
    k := k xor (k shr r);
    k := k * m;

    h := h * m;
    h := h xor k;

    Inc(buffer, 4);
    dec(len, 4);
  end;

  { Handle the last few bytes of the input array
    S: ... $69 $18 $2f
  }
  Assert(len <= 3);
  if len = 3 then
    h := h xor (pByte(buffer + 2)^ shl 16);
  if len >= 2 then
    h := h xor (pByte(buffer + 1)^ shl 8);
  if len >= 1 then
  begin
    h := h xor buffer^;
    h := h * m;
  end;

  // Do a few final mixes of the hash to ensure the last few
  // bytes are well-incorporated.
  h := h xor (h shr 13);
  h := h * m;
  h := h xor (h shr 15);

  result := h;
end;

// Originally converted by Lionell Delafosse
function MurmurHash3_32(AKey: pByte; ALength: uInt32; ASeed: uInt32 = $9747B28C): uInt32;
const
  c1 = $CC9E2D51;
  c2 = $1B873593;
var
  i: integer;
  h1,
    k1: uInt32;
begin
{$Q-}
{$R-}
  h1 := ASeed;
  i := ALength div SizeOf(uInt32);

  while i <> 0 do
  begin
    k1 := PCardinal(AKey)^ * c1;
    k1 := ((k1 shl 15) or (k1 shr 17)) * c2;
    h1 := h1 xor k1;
    h1 := (h1 shl 13) or (h1 shr 19);
    h1 := ((h1 shl 2) + h1) + $E6546B64;
    Inc(AKey, SizeOf(uInt32));
    dec(i);
  end;

  // tail
  if (ALength and 1) <> 0 then
  begin
    if (ALength and 2) <> 0 then
      k1 := (((uInt32(pByte(AKey + 2)^) shl 16) xor PWord(AKey)^) * c1) // 3 bytes
    else
      k1 := uInt32(pByte(AKey)^) * c1; // 1 bytes
    h1 := h1 xor (((k1 shl 16) or (k1 shr 16)) * c2);
  end
  else if (ALength and 2) <> 0 then
  begin
    k1 := uInt32(PWord(AKey)^) * c1; // 2 bytes
    h1 := h1 xor (((k1 shl 16) or (k1 shr 16)) * c2);
  end;

  // finalization mix - force all bits of hash block to avalanche within 0.25% bias
  h1 := h1 xor ALength;
  h1 := (h1 xor (h1 shr 16)) * $85EBCA6B;
  h1 := (h1 xor (h1 shr 13)) * $C2B2AE35;
  result := h1 xor (h1 shr 16);
end;

function MurmurHash3_32(const aValue: string; ASeed: uInt32 = $9747B28C): uInt32;
begin
  if Length(aValue) = 0 then
    Exit(ASeed);
  Result := MurmurHash3_32(PByte(PChar(aValue)), Length(aValue) * SizeOf(Char), ASeed);
end;

end.
