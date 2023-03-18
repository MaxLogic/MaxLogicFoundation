unit maxLogic.AESHelper;

{ Version: 1

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
  windows, sysUtils, classes;

Function TextFromEncryptedFile(const FileName: string; const Password: String): string;
Procedure TextToEncryptedFile(const Text, FileName: string; const Password: String; encoding: TEncoding);

function EncryptBytes(const SrcBuff: TBytes; const Password: String): TBytes;
function DecryptBytes(const SrcBuff: TBytes; const Password: String): TBytes;

function EncryptUtf8Str(const s: string; const Password: String): TBytes;
Function DecryptUtf8Str(const bytes: TBytes; const Password: String): string;

// not really an encryption but helps to store binary data as string
Function Base64Encode(s: string): String; overload;
Function Base64Encode(const bytes: TBytes): string; overload;
Function Base64Encode(const Buffer; size: integer): string; overload;
Function Base64Decode(const s: string): TBytes;

// helper to clear memory
procedure ZeroBytes(var b: TBytes);

procedure selfTest;

implementation

uses
  ioUtils, system.netEncoding, SynCrypto, SynCommons;

Function TextFromEncryptedFile(const FileName: string; const Password: String): string;
var
  bytes: TBytes;
  encoding: TEncoding;
  BOMLength: integer;
begin
  encoding := nil;
  bytes := TFile.ReadAllBytes(FileName);
  bytes := DecryptBytes(bytes, Password);

  BOMLength := TEncoding.GetBufferEncoding(bytes, encoding);
  result := encoding.GetString(bytes, BOMLength, length(bytes) - BOMLength);
  ZeroBytes(bytes);
end;

Procedure TextToEncryptedFile(const Text, FileName: string; const Password: String; encoding: TEncoding);
var
  Buff: TBytes;
  bytes: TBytes;
  ms: TBytesStream;
begin
  ms := TBytesStream.create;
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

function EncryptBytes(const SrcBuff: TBytes; const Password: String): TBytes;
var
  AES: TAESCFB;
  pw: RawUTF8;
begin
  pw := SynCommons.StringToUTF8(Password);
  AES := TAESCFB.CreateFromSha256(pw);
  try
    result := AES.EncryptPKCS7(SrcBuff, true);
  finally
    AES.Free;
    ZeroMemory(pointer(pw), length(pw));
  end;
end;

function DecryptBytes(const SrcBuff: TBytes; const Password: String): TBytes;
var
  AES: TAESCFB;
  pw: RawUTF8;
begin
  pw := SynCommons.StringToUTF8(Password);
  AES := TAESCFB.CreateFromSha256(pw);
  try
    result := AES.DecryptPKCS7(SrcBuff, true);
  finally
    AES.Free;
    ZeroMemory(pointer(pw), length(pw));
  end;
end;

function EncryptUtf8Str(const s: string; const Password: String): TBytes;
var
  b: TBytes;
begin
  b := TEncoding.utf8.GetBytes(s);
  result := EncryptBytes(b, Password);
  ZeroBytes(b);
end;

Function DecryptUtf8Str(const bytes: TBytes; const Password: String): string;
var
  b: TBytes;
begin
  b := DecryptBytes(bytes, Password);
  result := TEncoding.utf8.GetString(b);
  ZeroBytes(b);
end;

Function Base64Encode(s: string): String;
var
  LBase64: TNetEncoding;
begin
  LBase64 := TBase64Encoding.create(0, '');
  try
    result := LBase64.Encode(s);
  finally
    LBase64.Free;
  end;
end;

Function Base64Encode(const bytes: TBytes): string;
var
  LBase64: TNetEncoding;
begin
  LBase64 := TBase64Encoding.create(0, '');
  try
    result := LBase64.EncodeBytesToString(bytes);
  finally
    LBase64.Free;
  end;
end;

Function Base64Encode(const Buffer; size: integer): string;
var
  LBase64: TNetEncoding;
begin
  LBase64 := TBase64Encoding.create(0, '');
  try
    result := LBase64.EncodeBytesToString(@Buffer, size);
  finally
    LBase64.Free;
  end;
end;

Function Base64Decode(const s: string): TBytes;
var
  LBase64: TNetEncoding;
begin
  LBase64 := TBase64Encoding.create(0, '');
  try
    result := LBase64.DecodeStringToBytes(s);
  finally
    LBase64.Free;
  end;
end;

procedure selfTest;
  function randomPw: string;
  var
    x, len: integer;
  begin
    len := 5 + random(50);
    setLength(result, len);

    for x := 1 to len do
      result[x] := char(random(200) + 1);
  end;

  function RandomBytes(size: integer): TBytes;
  var
    x: integer;
  begin
    setLength(result, size);
    for x := 0 to size - 1 do
      result[x] := random($FF);
  end;

var
  b1, b2, b3: TBytes;
  x: integer;
  pw: string;
begin
  randomize;
  for x := 1 to 1000 do
  begin
    b1 := RandomBytes(x * 32 + random($FFFF));
    pw := randomPw;
    b2 := EncryptBytes(b1, pw);
    b3 := DecryptBytes(b2, pw);

    if length(b1) <> length(b3) then
      raise Exception.create('inbalid byffer len');
    if not CompareMem(@b1[0], @b3[0], length(b1)) then
      raise Exception.create('buffers are not equal!');

  end;
end;

procedure ZeroBytes(var b: TBytes);
begin
  if length(b) <> 0 then
    ZeroMemory(@b[0], length(b));
end;

end.
