unit maxLogic.AESHelper;

{ Version: 1.1

  Description: a simple helper to encrypt and decrypt using the SynCrypto unit

  info

  Key Size: 256 bits / 32 bytes
  Cipher Mode: CFB
  Padding: PKCS#7
  Block Size: 16 bytes
  Initialization Vector: 16 bytes

  the password used is created from Sha256 of the password provided

  see:
  https://synopse.info/files/html/api-1.18/SynCrypto.html#TAESABSTRACT_ENCRYPTPKCS7

  ATTENTION: the decrypted and encrypted buffers can be of different sizes! The encrypted data is in blocks of 16 bytes.

  you can use this as well:
  TAESCFBCRC.MACEncrypt
  or
  TAESCFBCRC.SimpleEncrypt
}
interface

uses
  winApi.Windows, System.SysUtils, System.Classes;

const
  /// default buffer size used by EncryptStream/DecryptStream (4 MiB)
  cDefaultBufferSize = 4 * 1024 * 1024; // fits well in modern L3 cache
  C_PBKDF2_ROUNDS = 600000;

function TextFromEncryptedFile(const FileName: string; const Password: string): string;
procedure TextToEncryptedFile(const Text, FileName: string; const Password: string; encoding: TEncoding);

function EncryptBytes(const SrcBuff: TBytes; const Password: string): TBytes; overload;
function EncryptBytes(const SrcBuff: TBytes; const Password: string; const aSalt: rawByteString; aRounds: integer = 600000): TBytes; overload;
function EncryptBytes(const SrcBuff: TBytes; const Password: string; const aSalt: TBytes; aRounds: integer = 600000): TBytes; overload;

function DecryptBytes(const SrcBuff: TBytes; const Password: string): TBytes; overload;
function DecryptBytes(const SrcBuff: TBytes; const Password: string; const aSalt: rawByteString; aRounds: integer = 600000): TBytes; overload;
function DecryptBytes(const SrcBuff: TBytes; const Password: string; const aSalt: TBytes; aRounds: integer = 600000): TBytes; overload;

// if size is <= 0 then the entire stream will be processed
// The buffer size will be automatically adjusted to be a multiple of 16.
// This function respects the initial Position of both streams, allowing for concatenation.
function EncryptStream(const aSrcStream, aDstStream: TStream; const aPassword: string;
  const aSize: Int64 = 0; aBufferSize: integer = cDefaultBufferSize): boolean; overload;
function EncryptStream(const aSrcStream, aDstStream: TStream; const aPassword: string; const aSalt: rawByteString;
  aRounds: integer = C_PBKDF2_ROUNDS; const aSize: Int64 = 0; aBufferSize: integer = cDefaultBufferSize): boolean; overload;
function EncryptStream(const aSrcStream, aDstStream: TStream; const aPassword: string; const aSalt: TBytes;
  aRounds: integer = C_PBKDF2_ROUNDS; const aSize: Int64 = 0; aBufferSize: integer = cDefaultBufferSize): boolean; overload;

// if size is <= 0 then the entire stream will be processed
// The buffer size will be automatically adjusted to be a multiple of 16.
// This function respects the initial Position of both streams, allowing for concatenation.
// The DstStream is not required to be seekable.
function DecryptStream(const aSrcStream, aDstStream: TStream; const aPassword: string;
  const aSize: Int64 = 0; aBufferSize: integer = cDefaultBufferSize): boolean; overload;
function DecryptStream(const aSrcStream, aDstStream: TStream; const aPassword: string; const aSalt: rawByteString;
  aRounds: integer = C_PBKDF2_ROUNDS; const aSize: Int64 = 0; aBufferSize: integer = cDefaultBufferSize): boolean; overload;
function DecryptStream(const aSrcStream, aDstStream: TStream; const aPassword: string; const aSalt: TBytes;
  aRounds: integer = C_PBKDF2_ROUNDS; const aSize: Int64 = 0; aBufferSize: integer = cDefaultBufferSize): boolean; overload;

function EncryptUtf8Str(const s: string; const Password: string): TBytes;
function DecryptUtf8Str(const bytes: TBytes; const Password: string): string;

// not really an encryption but helps to store binary data as string
function Base64Encode(s: string): string; overload;
function Base64Encode(const bytes: TBytes): string; overload;
function Base64Encode(const Buffer; Size: integer): string; overload;
function Base64Decode(const s: string): TBytes;

// helper to clear memory
procedure ZeroBytes(var b: TBytes);
function RandomBytes(aLen: Integer): TBytes;

type

  TSelfTest = class
  private type
    // Anonymous types for passing encryption/decryption functions as parameters
    TBytesEncryptFunc = reference to function(const aSrc: TBytes; const aPw: string): TBytes;
    TBytesDecryptFunc = reference to function(const aSrc: TBytes; const aPw: string): TBytes;
    TStreamEncryptFunc = reference to function(const aSrc, aDst: TStream; const aPw: string): boolean;
    TStreamDecryptFunc = reference to function(const aSrc, aDst: TStream; const aPw: string): boolean;

    TBytesEncryptFuncSalt = reference to function(const aSrc: TBytes; const aPw: string; const aSalt: TBytes; aRounds: integer): TBytes;
    TBytesDecryptFuncSalt = reference to function(const aSrc: TBytes; const aPw: string; const aSalt: TBytes; aRounds: integer): TBytes;
    TStreamEncryptFuncSalt = reference to function(const aSrc, aDst: TStream; const aPw: string; const aSalt: TBytes; aRounds: integer): boolean;
    TStreamDecryptFuncSalt = reference to function(const aSrc, aDst: TStream; const aPw: string; const aSalt: TBytes; aRounds: integer): boolean;

  private
    class function RandomPw: string; static;
    class procedure TestBytes(aEncrypt: TBytesEncryptFunc; aDecrypt: TBytesDecryptFunc;
      const aOriginal: TBytes; const aPw, aTestName: string); static;
    class procedure TestStream(aEncrypt: TStreamEncryptFunc; aDecrypt: TStreamDecryptFunc;
      const aOriginal: TBytes; const aPw, aTestName: string); static;
    class procedure TestCrossCompatibility(const aOriginal: TBytes; const aPw, aTestName: string); static;
    class procedure RunTestCycle(const aOriginal: TBytes; const aPw: string); static;
  public
    class procedure Execute; static;
  end;

implementation

uses
  System.IOUtils, System.netEncoding, SynCrypto, SynCommons, AutoFree, MaxLogic.strUtils;


function RandomBytes(aLen: Integer): TBytes;
begin
  Result:= TAESPRNG.Main.FillRandomBytes(aLen);
end;

function EncryptStreamCore(const aCipher: TAESCFB; const aSrcStream, aDstStream: TStream;
  const aSize: Int64; aBufferSize: integer): boolean; forward;
function DecryptStreamCore(const aCipher: TAESCFB; const aSrcStream, aDstStream: TStream;
  const aSize: Int64; aBufferSize: integer): boolean; forward;


function TextFromEncryptedFile(const FileName: string; const Password: string): string;
var
  bytes: TBytes;
  encoding: TEncoding;
  BOMLength: integer;
begin
  encoding := nil;
  bytes := TFile.ReadAllBytes(FileName);
  bytes := DecryptBytes(bytes, Password);

  BOMLength := TEncoding.GetBufferEncoding(bytes, encoding);
  Result := encoding.GetString(bytes, BOMLength, length(bytes) - BOMLength);
  ZeroBytes(bytes);
end;

procedure TextToEncryptedFile(const Text, FileName: string; const Password: string; encoding: TEncoding);
var
  Buff: TBytes;
  bytes: TBytes;
  ms: TBytesStream;
begin
  ms := TBytesStream.Create;
  try
    if encoding = nil then
      encoding := TEncoding.default;

    Buff := encoding.GetPreamble;
    if length(Buff) <> 0 then
      ms.WriteBuffer(Buff, length(Buff));

    Buff := encoding.GetBytes(Text);
    ms.WriteBuffer(Buff, length(Buff));

    // now encrypt
    Buff := ms.bytes;
    bytes := EncryptBytes(Buff, Password);
    TFile.WriteAllBytes(FileName, bytes);

  finally
    ms.Free;
    ZeroBytes(Buff);
  end;
end;

function EncryptBytes(const SrcBuff: TBytes; const Password: string): TBytes;
var
  AES: TAESCFB;
  PW: RawUTF8;
begin
  PW := SynCommons.StringToUTF8(Password);
  AES := TAESCFB.CreateFromSha256(PW);
  try
    Result := AES.EncryptPKCS7(SrcBuff, True);
  finally
    AES.Free;
    ZeroMemory(Pointer(PW), length(PW));
  end;
end;

function DecryptBytes(const SrcBuff: TBytes; const Password: string): TBytes;
var
  AES: TAESCFB;
  PW: RawUTF8;
begin
  PW := SynCommons.StringToUTF8(Password);
  AES := TAESCFB.CreateFromSha256(PW);
  try
    Result := AES.DecryptPKCS7(SrcBuff, True);
  finally
    AES.Free;
    ZeroMemory(Pointer(PW), length(PW));
  end;
end;

function EncryptBytes(const SrcBuff: TBytes; const Password: string; const aSalt: TBytes; aRounds: integer = 600000): TBytes;
begin
  Result:= EncryptBytes(SrcBuff, Password, BytesTorawStr(aSalt), aRounds);
end;

function EncryptBytes(const SrcBuff: TBytes; const Password: string; const aSalt: rawByteString; aRounds: integer = 600000): TBytes;
var
  AES: TAESCFB;
  PW: RawUTF8;
begin
  PW := SynCommons.StringToUTF8(Password);
  AES := TAESCFB.CreateFromPBKDF2(PW, aSalt, aRounds);
  try
    Result := AES.EncryptPKCS7(SrcBuff, True);
  finally
    AES.Free;
    ZeroMemory(Pointer(PW), length(PW));
  end;
end;

function DecryptBytes(const SrcBuff: TBytes; const Password: string; const aSalt: TBytes; aRounds: integer = 600000): TBytes;
begin
  Result:= DecryptBytes(SrcBuff, Password, BytesTorawStr(aSalt), aRounds);
end;

function DecryptBytes(const SrcBuff: TBytes; const Password: string; const aSalt: rawByteString; aRounds: integer = 600000): TBytes;
var
  AES: TAESCFB;
  PW: RawUTF8;
begin
  PW := SynCommons.StringToUTF8(Password);
  AES := TAESCFB.CreateFromPBKDF2(PW, aSalt, aRounds);
  try
    Result := AES.DecryptPKCS7(SrcBuff, True);
  finally
    AES.Free;
    ZeroMemory(Pointer(PW), length(PW));
  end;
end;

function EncryptUtf8Str(const s: string; const Password: string): TBytes;
var
  b: TBytes;
begin
  b := TEncoding.utf8.GetBytes(s);
  Result := EncryptBytes(b, Password);
  ZeroBytes(b);
end;

function DecryptUtf8Str(const bytes: TBytes; const Password: string): string;
var
  b: TBytes;
begin
  b := DecryptBytes(bytes, Password);
  Result := TEncoding.utf8.GetString(b);
  ZeroBytes(b);
end;

function Base64Encode(s: string): string;
var
  LBase64: TNetEncoding;
begin
  LBase64 := TBase64Encoding.Create(0, '');
  try
    Result := LBase64.Encode(s);
  finally
    LBase64.Free;
  end;
end;

function Base64Encode(const bytes: TBytes): string;
var
  LBase64: TNetEncoding;
begin
  LBase64 := TBase64Encoding.Create(0, '');
  try
    Result := LBase64.EncodeBytesToString(bytes);
  finally
    LBase64.Free;
  end;
end;

function Base64Encode(const Buffer; Size: integer): string;
var
  LBase64: TNetEncoding;
begin
  LBase64 := TBase64Encoding.Create(0, '');
  try
    Result := LBase64.EncodeBytesToString(@Buffer, Size);
  finally
    LBase64.Free;
  end;
end;

function Base64Decode(const s: string): TBytes;
var
  LBase64: TNetEncoding;
begin
  LBase64 := TBase64Encoding.Create(0, '');
  try
    Result := LBase64.DecodeStringToBytes(s);
  finally
    LBase64.Free;
  end;
end;

procedure ZeroBytes(var b: TBytes);
begin
  if length(b) <> 0 then
    ZeroMemory(@b[0], length(b));
end;


function EncryptStream(const aSrcStream, aDstStream: TStream; const aPassword: string;
  const aSize: Int64; aBufferSize: integer): boolean;
var
  lAES: TAESCFB;
  lPW: RawUTF8;
begin
  lPW := StringToUTF8(aPassword);
  try
    gc(lAES, TAESCFB.CreateFromSha256(lPW));
    Result := EncryptStreamCore(lAES, aSrcStream, aDstStream, aSize, aBufferSize);
  finally
    ZeroMemory(Pointer(lPW), length(lPW));
  end;
end;

function EncryptStream(const aSrcStream, aDstStream: TStream; const aPassword: string; const aSalt: TBytes;
  aRounds: integer; const aSize: Int64; aBufferSize: integer): Boolean;
begin
  Result:= EncryptStream(aSrcStream, aDstStream, aPassword, BytesTorawStr(aSalt), aRounds, aSize, aBufferSize);
end;

function EncryptStream(const aSrcStream, aDstStream: TStream; const aPassword: string; const aSalt: rawByteString;
  aRounds: integer; const aSize: Int64; aBufferSize: integer): boolean;
var
  lAES: TAESCFB;
  lPW: RawUTF8;
begin
  lPW := StringToUTF8(aPassword);
  try
    gc(lAES, TAESCFB.CreateFromPBKDF2(lPW, aSalt, aRounds));
    Result := EncryptStreamCore(lAES, aSrcStream, aDstStream, aSize, aBufferSize);
  finally
    ZeroMemory(Pointer(lPW), length(lPW));
  end;
end;

function DecryptStream(const aSrcStream, aDstStream: TStream; const aPassword: string;
  const aSize: Int64; aBufferSize: integer): boolean;
var
  lAES: TAESCFB;
  lPW: RawUTF8;
begin
  lPW := StringToUTF8(aPassword);
  try
    gc(lAES, TAESCFB.CreateFromSha256(lPW));
    Result := DecryptStreamCore(lAES, aSrcStream, aDstStream, aSize, aBufferSize);
  finally
    ZeroMemory(Pointer(lPW), length(lPW));
  end;
end;

function DecryptStream(const aSrcStream, aDstStream: TStream; const aPassword: string; const aSalt: TBytes;
  aRounds: integer; const aSize: Int64; aBufferSize: integer): Boolean;
begin
  Result:= DecryptStream(aSrcStream, aDstStream, aPassword, BytesTorawStr(aSalt), aRounds, aSize, aBufferSize);
end;
function DecryptStream(const aSrcStream, aDstStream: TStream; const aPassword: string; const aSalt: rawByteString;
  aRounds: integer; const aSize: Int64; aBufferSize: integer): boolean;
var
  lAES: TAESCFB;
  lPW: RawUTF8;
begin
  lPW := StringToUTF8(aPassword);
  try
    gc(lAES, TAESCFB.CreateFromPBKDF2(lPW, aSalt, aRounds));
    Result := DecryptStreamCore(lAES, aSrcStream, aDstStream, aSize, aBufferSize);
  finally
    ZeroMemory(Pointer(lPW), length(lPW));
  end;
end;

function EncryptStreamCore(const aCipher: TAESCFB; const aSrcStream, aDstStream: TStream;
  const aSize: Int64; aBufferSize: integer): boolean;
var
  ChunkSize: integer;
  Remaining: Int64;
  Buf: pByte;
  lIV: TAESBlock;
begin
  Result := False;

  // Auto-correct buffer size to be a multiple of 16, with a minimum of 16.
  if aBufferSize < 16 then
    aBufferSize := 16;
  if aBufferSize <> (aBufferSize div 16) * 16 then
    aBufferSize := ((aBufferSize div 16) + 1) * 16;

  if (aSrcStream = nil) or (aDstStream = nil) then
    exit;

  // 1. Generate a new, random IV for this encryption session.
  TAESPRNG.Main.FillRandom(lIV);

  // 2. Write the non-secret IV to the start of the destination stream.
  //    The decryptor will read this first.
  aDstStream.WriteBuffer(lIV, SizeOf(lIV));

  // 3. Initialize the cipher with our random IV.
  aCipher.IV := lIV;

  GetMem(Buf, aBufferSize);
  try
    if aSize <= 0 then
      Remaining := aSrcStream.Size - aSrcStream.Position
    else
      Remaining := aSize;

    while Remaining > aBufferSize do
    begin
      if aSrcStream.Read(Buf^, aBufferSize) <> aBufferSize then
        raise EStreamError.Create('EncryptStream: unable to read');

      aCipher.encrypt(Buf, Buf, aBufferSize);

      if aDstStream.Write(Buf^, aBufferSize) <> aBufferSize then
        raise EStreamError.Create('EncryptStream: unable to write');
      Dec(Remaining, aBufferSize);
    end;

    // 4. Handle the final block and add PKCS#7 padding
    if Remaining > 0 then
    begin
      if aSrcStream.Read(Buf^, Remaining) <> Remaining then
        raise EStreamError.Create('EncryptStream: unable to read final block');
    end;

    var PaddingSize := 16 - (Remaining mod 16);
    // PKCS#7 rule: if data is a perfect multiple of 16, a full block of padding is added.
    if PaddingSize = 0 then
      PaddingSize := 16;

    // Fill the buffer with the padding value
    fillchar(Buf[Remaining], PaddingSize, PaddingSize);

    var FinalBlockSize := Remaining + PaddingSize;
    aCipher.encrypt(Buf, Buf, FinalBlockSize);

    if aDstStream.Write(Buf^, FinalBlockSize) <> FinalBlockSize then
      raise EStreamError.Create('EncryptStream: unable to write final padded block');

    Result := True;
  finally
    // scrub sensitive data before releasing
    if assigned(Buf) then
    begin
      fillchar(Buf^, aBufferSize, 0);
      FreeMem(Buf);
    end;
  end;
end;

function DecryptStreamCore(const aCipher: TAESCFB; const aSrcStream, aDstStream: TStream;
  const aSize: Int64; aBufferSize: integer): boolean;
var
  ChunkSize: integer;
  Remaining: Int64;
  Buf: pByte;
  lIV: TAESBlock;
begin
  Result := False;

  if aBufferSize < 16 then
    aBufferSize := 16;
  if aBufferSize <> (aBufferSize div 16) * 16 then
    aBufferSize := ((aBufferSize div 16) + 1) * 16;

  if (aSrcStream = nil) or (aDstStream = nil) then
    exit;

  GetMem(Buf, aBufferSize);
  try
    // 1. Read the IV from the start of the source stream.
    if aSrcStream.Read(lIV, SizeOf(lIV)) <> SizeOf(lIV) then
      raise EStreamError.Create('DecryptStream: unable to read IV');

    // 2. Initialize the cipher with the IV we just read.
    aCipher.IV := lIV;

    if aSize <= 0 then
      Remaining := aSrcStream.Size - aSrcStream.Position
    else
      Remaining := aSize - SizeOf(lIV); // Adjust for the IV we just read

    if (Remaining < 0) or (Remaining mod 16 <> 0) then
      raise EStreamError.Create('DecryptStream: encrypted stream size is invalid.');

    // Process all blocks except the very last one, which contains padding.
    while Remaining > aBufferSize do
    begin
      if aSrcStream.Read(Buf^, aBufferSize) <> aBufferSize then
        raise EStreamError.Create('DecryptStream: unable to read');

      aCipher.decrypt(Buf, Buf, aBufferSize);
      if aDstStream.Write(Buf^, aBufferSize) <> aBufferSize then
        raise EStreamError.Create('DecryptStream: unable to write');
      Dec(Remaining, aBufferSize);
    end;

    // 3. Process the final block in memory to handle un-padding correctly.
    if Remaining > 0 then
    begin
      if aSrcStream.Read(Buf^, Remaining) <> Remaining then
        raise EStreamError.Create('DecryptStream: unable to read final block');

      aCipher.decrypt(Buf, Buf, Remaining);

      // Determine padding from the last byte of the decrypted block and validate it.
      // A correct PKCS#7 padding is essential for security to prevent padding oracle attacks.
      var PaddingValue := Buf[Remaining - 1];
      var FinalDataSize: Int64;

      // 1. The padding value must be in the valid range [1..16] for AES.
      //    It also cannot be larger than the size of the data block we have.
      if (PaddingValue = 0) or (PaddingValue > 16) or (PaddingValue > Remaining) then
        raise EStreamError.Create('DecryptStream: Invalid PKCS7 padding value detected.');

      // 2. All padding bytes must be identical to the padding value.
      //    We check the last 'PaddingValue' bytes of the buffer.
      for var i := 1 to PaddingValue do // Check all padding bytes, from last to first.
      begin
        if Buf[Remaining - i] <> PaddingValue then
          raise EStreamError.Create('DecryptStream: Invalid PKCS7 padding bytes detected.');
      end;

      // If we're here, the padding is valid. Calculate the final data size.
      FinalDataSize := Remaining - PaddingValue;

      if FinalDataSize > 0 then
        if aDstStream.Write(Buf^, FinalDataSize) <> FinalDataSize then
          raise EStreamError.Create('DecryptStream: unable to write final data');
    end;

    Result := True;
  finally
    if assigned(Buf) then
    begin
      fillchar(Buf^, aBufferSize, 0);
      FreeMem(Buf);
    end;
  end;
end;


{ TSelfTest }


{ TSelfTest }

class procedure TSelfTest.Execute;
var
  i: integer;
  lOriginalBytes: TBytes;
  lPassword: string;
begin
  randomize;
  for i := 1 to 100 do
  begin
    lOriginalBytes := RandomBytes(i * 4 + random($FF));
    lPassword := RandomPw;
    RunTestCycle(lOriginalBytes, lPassword);
  end;
end;


class function TSelfTest.RandomPw: string;
var
  i, len: integer;
begin
  len := 5 + random(50);
  SetLength(Result, len);
  for i := 1 to len do
    Result[i] := char(random(200) + 1);
end;

class procedure TSelfTest.RunTestCycle(const aOriginal: TBytes; const aPw: string);
var
  lSalt: TBytes;
  lEncryptedBytes, lDecryptedBytes: TBytes;
  lSrcStream, lEncryptedStream, lDecryptedStream: TMemoryStream;
begin
  // --- SHA256 Tests ---
  TestBytes(
    function(const aSrc: TBytes; const aPw: string): TBytes
    begin Result := EncryptBytes(aSrc, aPw); end,
    function(const aSrc: TBytes; const aPw: string): TBytes
    begin Result := DecryptBytes(aSrc, aPw); end,
    aOriginal, aPw, 'SHA256 Bytes'
    );

  TestStream(
    function(const aSrc, aDst: TStream; const aPw: string): boolean
    begin Result := EncryptStream(aSrc, aDst, aPw); end,
    function(const aSrc, aDst: TStream; const aPw: string): boolean
    begin Result := DecryptStream(aSrc, aDst, aPw); end,
    aOriginal, aPw, 'SHA256 Stream'
    );

  TestCrossCompatibility(aOriginal, aPw, 'SHA256 Cross-Compatibility');

  // --- PBKDF2 Tests ---
  lSalt := RandomBytes(16); // Common salt size

  // Test Bytes with PBKDF2
  lEncryptedBytes := EncryptBytes(aOriginal, aPw, BytesToRawStr(lSalt), 1000); // Fewer rounds for faster testing
  lDecryptedBytes := DecryptBytes(lEncryptedBytes, aPw, BytesToRawStr(lSalt), 1000);
  if (length(aOriginal) <> length(lDecryptedBytes)) or
     ((length(aOriginal) > 0) and not CompareMem(Pointer(aOriginal), Pointer(lDecryptedBytes), length(aOriginal))) then
    raise Exception.Create('PBKDF2 Bytes test failed');

  // Test Streams with PBKDF2
  gc(lSrcStream, TMemoryStream.Create);
  gc(lEncryptedStream, TMemoryStream.Create);
  gc(lDecryptedStream, TMemoryStream.Create);

  if length(aOriginal) > 0 then
    lSrcStream.WriteBuffer(aOriginal[0], length(aOriginal));
  lSrcStream.Position := 0;

  if not EncryptStream(lSrcStream, lEncryptedStream, aPw, BytesToRawStr(lSalt), 1000) then
    raise Exception.Create('PBKDF2 Stream encrypt failed');

  lEncryptedStream.Position := 0;
  if not DecryptStream(lEncryptedStream, lDecryptedStream, aPw, BytesToRawStr(lSalt), 1000) then
    raise Exception.Create('PBKDF2 Stream decrypt failed');

  if lSrcStream.Size <> lDecryptedStream.Size then
    raise Exception.CreateFmt('PBKDF2 Stream test: invalid decrypted stream size. Expected %d, got %d',
      [lSrcStream.Size, lDecryptedStream.Size]);
end;

class procedure TSelfTest.TestBytes(aEncrypt: TBytesEncryptFunc; aDecrypt: TBytesDecryptFunc;
  const aOriginal: TBytes; const aPw, aTestName: string);
var
  lEncrypted, lDecrypted: TBytes;
begin
  lEncrypted := aEncrypt(aOriginal, aPw);
  lDecrypted := aDecrypt(lEncrypted, aPw);

  if length(aOriginal) <> length(lDecrypted) then
    raise Exception.CreateFmt('%s test: invalid buffer length', [aTestName]);

  if (length(aOriginal) > 0) and not CompareMem(Pointer(aOriginal), Pointer(lDecrypted), length(aOriginal)) then
    raise Exception.CreateFmt('%s test: buffers are not equal!', [aTestName]);
end;

class procedure TSelfTest.TestCrossCompatibility(const aOriginal: TBytes; const aPw, aTestName: string);
var
  lEncryptedBytes, lDecryptedBytes: TBytes;
  lEncryptedStream, lDecryptedStream: TMemoryStream;
begin
  // --- Encrypt with Stream, Decrypt with Bytes ---
  gc(lEncryptedStream, TMemoryStream.Create);
  var lSrcStream: TMemoryStream;
  gc(lSrcStream, TMemoryStream.Create);
  if length(aOriginal) > 0 then
    lSrcStream.WriteBuffer(aOriginal[0], length(aOriginal));
  lSrcStream.Position := 0;

  if not EncryptStream(lSrcStream, lEncryptedStream, aPw) then
    raise Exception.CreateFmt('%s (Stream->Bytes) encrypt failed', [aTestName]);

  lEncryptedStream.Position := 0;
  SetLength(lEncryptedBytes, lEncryptedStream.Size);
  if length(lEncryptedBytes) > 0 then
    lEncryptedStream.ReadBuffer(lEncryptedBytes[0], length(lEncryptedBytes));

  lDecryptedBytes := DecryptBytes(lEncryptedBytes, aPw);

  if length(aOriginal) <> length(lDecryptedBytes) then
    raise Exception.CreateFmt('%s (Stream->Bytes): invalid buffer length', [aTestName]);
  if (length(aOriginal) > 0) and not CompareMem(Pointer(aOriginal), Pointer(lDecryptedBytes), length(aOriginal)) then
    raise Exception.CreateFmt('%s (Stream->Bytes): buffers are not equal!', [aTestName]);

  // --- Encrypt with Bytes, Decrypt with Stream ---
  lEncryptedBytes := EncryptBytes(aOriginal, aPw);

  gc(lEncryptedStream, TMemoryStream.Create);
  gc(lDecryptedStream, TMemoryStream.Create);

  if length(lEncryptedBytes) > 0 then
    lEncryptedStream.WriteBuffer(lEncryptedBytes[0], length(lEncryptedBytes));
  lEncryptedStream.Position := 0;

  if not DecryptStream(lEncryptedStream, lDecryptedStream, aPw) then
    raise Exception.CreateFmt('%s (Bytes->Stream) decrypt failed', [aTestName]);

  if lDecryptedStream.Size <> length(aOriginal) then
    raise Exception.CreateFmt('%s (Bytes->Stream): invalid decrypted stream size. Expected %d, got %d',
      [aTestName, length(aOriginal), lDecryptedStream.Size]);

  if lDecryptedStream.Size > 0 then
  begin
    SetLength(lDecryptedBytes, lDecryptedStream.Size);
    lDecryptedStream.Position := 0;
    lDecryptedStream.ReadBuffer(lDecryptedBytes[0], length(lDecryptedBytes));
    if not CompareMem(Pointer(aOriginal), Pointer(lDecryptedBytes), length(aOriginal)) then
      raise Exception.CreateFmt('%s (Bytes->Stream): decrypted stream content mismatch!', [aTestName]);
  end;
end;

class procedure TSelfTest.TestStream(aEncrypt: TStreamEncryptFunc; aDecrypt: TStreamDecryptFunc;
  const aOriginal: TBytes; const aPw, aTestName: string);
var
  lSrc, lEncrypted, lDecrypted: TMemoryStream;
  lDecryptedBytes: TBytes;
begin
  gc(lSrc, TMemoryStream.Create);
  gc(lEncrypted, TMemoryStream.Create);
  gc(lDecrypted, TMemoryStream.Create);

  if length(aOriginal) > 0 then
    lSrc.WriteBuffer(aOriginal[0], length(aOriginal));
  lSrc.Position := 0;

  if not aEncrypt(lSrc, lEncrypted, aPw) then
    raise Exception.CreateFmt('%s test: EncryptStream failed', [aTestName]);

  lEncrypted.Position := 0;
  if not aDecrypt(lEncrypted, lDecrypted, aPw) then
    raise Exception.CreateFmt('%s test: DecryptStream failed', [aTestName]);

  if lSrc.Size <> lDecrypted.Size then
    raise Exception.CreateFmt('%s test: invalid decrypted stream size. Expected %d, got %d',
      [aTestName, lSrc.Size, lDecrypted.Size]);

  if lDecrypted.Size > 0 then
  begin
    SetLength(lDecryptedBytes, lDecrypted.Size);
    lDecrypted.Position := 0;
    lDecrypted.ReadBuffer(lDecryptedBytes[0], length(lDecryptedBytes));
    if not CompareMem(Pointer(aOriginal), Pointer(lDecryptedBytes), length(aOriginal)) then
      raise Exception.CreateFmt('%s test: decrypted stream content mismatch!', [aTestName]);
  end;
end;

initialization
  // TSelfTest.Execute;
end.

