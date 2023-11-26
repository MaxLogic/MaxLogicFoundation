unit MaxLogic.SimpleEncryption;

{ uses SCOP algo
  this is mostly for historical backwards compatibility.
  for new projects use maxASEAdapter.pas }

{
  Version:3.2
  History:
  2017-06-18: DEC is discarded, SCOP cipher is extracted to maxLogic.Cipher.SCOP
  2017-01-27: added EncryptBytes / EncryptUtf8Str / Base64Encode and their counterparts
  2017-01-16: added TextFromEncryptedFile and TextToEncryptedFile
}

interface

uses
  system.sysUtils, system.classes, system.hash,
  MaxLogic.Cipher.SCOP;

procedure EncryptStream(InStream, OutStream: TStream; DataSize: integer; Const Password: Ansistring; Out InStreamMD5HashAsBase64: string);
procedure DecryptStream(InStream, OutStream: TStream; DataSize: integer; Const Password: Ansistring; Out OutStreamMD5HashAsBase64: string);

Function TextFromEncryptedFile(const FileName: string; const Password: Ansistring): string;
Procedure TextToEncryptedFile(const Text, FileName: string; const Password: Ansistring; encoding: TEncoding);

function EncryptBytes(const SrcBuff; size: integer; const Password: Ansistring): TBytes; overload;
function EncryptBytes(const SrcBuff: TBytes; const Password: Ansistring): TBytes; overload;
function DecryptBytes(const SrcBuff; size: integer; const Password: Ansistring): TBytes; overload;
function DecryptBytes(const SrcBuff: TBytes; const Password: Ansistring): TBytes; overload;

function EncryptUtf8Str(const s: string; const Password: Ansistring): TBytes;
Function DecryptUtf8Str(const bytes: TBytes; const Password: Ansistring): string;

// not really an encryption but helps to store binary data as string
Function Base64Encode(s: string): String; overload;
Function Base64Encode(const bytes: TBytes): string; overload;
Function Base64Encode(const Buffer; size: integer): string; overload;
Function Base64Decode(s: string): TBytes;

procedure EncryptBuffer(const SrcBuff; var OutBuff; DataSize: integer; Const Password: Ansistring);
procedure DecryptBuffer(const SrcBuff; var OutBuff; DataSize: integer; Const Password: Ansistring);

// simple helper function to get the hash as a base64 encoded string in one go
procedure EncryptBufferAndGetHash(const SrcBuff; var OutBuff; DataSize: integer; Const Password: Ansistring; Out MD5Hash: string);
procedure DecryptBufferAndGetHash(const SrcBuff; var OutBuff; DataSize: integer; Const Password: Ansistring; Out MD5Hash: string);

// as mime64 encoded
function md5(const s: string): String; overload;
function md5(const Buff; BuffSize: cardinal): string; overload;
function md5(astream: TStream; DataSize: cardinal): string; overload;

implementation

uses
  system.ioUtils, system.netEncoding;

procedure DecryptBuffer(const SrcBuff; var OutBuff; DataSize: integer; Const Password: Ansistring);
var
  Cipher: TScopCipher;
begin
  Cipher := TScopCipher.Create(Password);
  Cipher.InitKey(Password);
  Cipher.DecodeBuffer(SrcBuff, OutBuff, DataSize);
  Cipher.Free;
end;

procedure DecryptBufferAndGetHash(const SrcBuff; var OutBuff; DataSize: integer; Const Password: Ansistring; Out MD5Hash: string);
begin
  DecryptBuffer(SrcBuff, OutBuff, DataSize, Password);
  MD5Hash := md5(OutBuff, DataSize);
end;

procedure EncryptBufferAndGetHash(const SrcBuff; var OutBuff; DataSize: integer; Const Password: Ansistring; Out MD5Hash: string);
begin
  MD5Hash := md5(SrcBuff, DataSize);
  EncryptBuffer(SrcBuff, OutBuff, DataSize, Password);
end;

procedure DecryptStream(InStream, OutStream: TStream; DataSize: integer; Const Password: Ansistring; Out OutStreamMD5HashAsBase64: string);
var
  Cipher: TScopCipher;
  pos: integer;
begin
  pos := OutStream.Position;

  Cipher := TScopCipher.Create(Password);
  Cipher.InitKey(Password);
  Cipher.DecodeStream(InStream, OutStream, DataSize);
  Cipher.Free;

  OutStream.Position := pos;
  OutStreamMD5HashAsBase64 := md5(OutStream, OutStream.size);
end;

procedure EncryptBuffer(const SrcBuff; var OutBuff; DataSize: integer; Const Password: Ansistring);
var
  Cipher: TScopCipher;
begin
  Cipher := TScopCipher.Create(Password);
  Cipher.InitKey(Password);
  Cipher.EncodeBuffer(SrcBuff, OutBuff, DataSize);
  Cipher.Free;
end;

procedure EncryptStream(InStream, OutStream: TStream; DataSize: integer; Const Password: Ansistring; Out InStreamMD5HashAsBase64: string);
var
  pos: integer;
  Cipher: TScopCipher;
begin
  pos := InStream.Position;
  InStreamMD5HashAsBase64 := md5(InStream, InStream.size);
  InStream.Position := pos;

  Cipher := TScopCipher.Create(Password);
  Cipher.InitKey(Password);
  Cipher.EncodeStream(InStream, OutStream, DataSize);
  Cipher.Free;
end;

function md5(const s: string): String;
var
  h: system.hash.THashMD5;
begin
  result :=
    Base64Encode(
    system.hash.THashMD5.GetHashBytes(s));
end;

function md5(const Buff; BuffSize: cardinal): string;
var
  h: system.hash.THashMD5;
begin
  h := system.hash.THashMD5.Create;
  h.Update(Buff, BuffSize);

  result :=
    Base64Encode(
    h.HashAsBytes);
end;

function md5(astream: TStream; DataSize: cardinal): string;
const
  BUFFERSIZE = 4 * 1024;
var
  LMD5: THashMD5;
  LBuffer: TBytes;
  LBytesRead: Longint;
  BytesLeft: cardinal;
begin
  LMD5 := THashMD5.Create;
  SetLength(LBuffer, BUFFERSIZE);
  BytesLeft := DataSize;

  while BytesLeft <> 0 do
  begin
    if BUFFERSIZE > BytesLeft then
      LBytesRead := BytesLeft
    else
      LBytesRead := BUFFERSIZE;

    LBytesRead := astream.ReadData(LBuffer, LBytesRead);
    dec(BytesLeft, LBytesRead);

    if LBytesRead = 0 then
      Break;

    LMD5.Update(LBuffer, LBytesRead);
  end;

  result :=
    Base64Encode(
    LMD5.HashAsBytes);
end;

Function TextFromEncryptedFile(const FileName: string; const Password: Ansistring): string;
var
  EncryptedStream: TMemorystream;
  DeCryptedStream: TBytesStream;
  MD5Hash: string;
  encoding: TEncoding;
  BOMLength: integer;
begin
  encoding := nil;
  EncryptedStream := TMemorystream.Create;
  DeCryptedStream := TBytesStream.Create;

  try
    EncryptedStream.loadFromFile(FileName);
    EncryptedStream.Position := 0;
    DeCryptedStream.size := EncryptedStream.size;

    DecryptStream(EncryptedStream, DeCryptedStream,
      EncryptedStream.size, Password, MD5Hash);

    DeCryptedStream.Position := 0;
    BOMLength := TEncoding.GetBufferEncoding(DeCryptedStream.bytes, encoding);
    result := encoding.GetString(DeCryptedStream.bytes, BOMLength, DeCryptedStream.size - BOMLength);

  finally
    EncryptedStream.Free;
    DeCryptedStream.Free;
  end;

end;

Procedure TextToEncryptedFile(const Text, FileName: string; const Password: Ansistring; encoding: TEncoding);
var
  Buff: TBytes;
  EncryptedStream, ms: TMemorystream;
  MD5Hash: string;
begin
  ms := TMemorystream.Create;
  EncryptedStream := TMemorystream.Create;

  try
    if encoding = nil then
      encoding := TEncoding.default;

    Buff := encoding.GetPreamble;
    if Length(Buff) <> 0 then
      ms.WriteBuffer(Buff, Length(Buff));

    Buff := encoding.GetBytes(Text);
    ms.WriteBuffer(Buff, Length(Buff));

    // free memory
    Buff := nil;

    // now encrypt
    ms.Position := 0;
    EncryptStream(ms, EncryptedStream, ms.size, Password, MD5Hash);
    EncryptedStream.savetofile(FileName);

  finally
    ms.Free;
    EncryptedStream.Free;
  end;
end;

function EncryptBytes(const SrcBuff; size: integer; const Password: Ansistring): TBytes;
begin
  SetLength(result, size);
  EncryptBuffer(SrcBuff, result[0], size, Password);
end;

function EncryptBytes(const SrcBuff: TBytes; const Password: Ansistring): TBytes;
begin
  result := EncryptBytes(SrcBuff[0], Length(SrcBuff), Password);
end;

function DecryptBytes(const SrcBuff; size: integer; const Password: Ansistring): TBytes;
begin
  SetLength(result, size);
  DecryptBuffer(SrcBuff, result[0], size, Password);
end;

function DecryptBytes(const SrcBuff: TBytes; const Password: Ansistring): TBytes;
begin
  result := DecryptBytes(SrcBuff[0], Length(SrcBuff), Password);
end;

function EncryptUtf8Str(const s: string; const Password: Ansistring): TBytes;
var
  c: TBytes;
begin
  c := TEncoding.utf8.GetBytes(s);
  result := EncryptBytes(c, Password);
end;

Function DecryptUtf8Str(const bytes: TBytes; const Password: Ansistring): string;
var
  c: TBytes;
begin
  c := DecryptBytes(bytes, Password);
  result := TEncoding.utf8.GetString(c);
end;

Function Base64Encode(s: string): String;
var
  LBase64: TNetEncoding;
begin
  LBase64 := TBase64Encoding.Create(0, '');
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
  LBase64 := TBase64Encoding.Create(0, '');
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
  LBase64 := TBase64Encoding.Create(0, '');
  try
    result := LBase64.EncodeBytesToString(@Buffer, size);
  finally
    LBase64.Free;
  end;
end;

Function Base64Decode(s: string): TBytes;
var
  LBase64: TNetEncoding;
begin
  LBase64 := TBase64Encoding.Create(0, '');
  try
    result := LBase64.DecodeStringToBytes(s);
  finally
    LBase64.Free;
  end;
end;

end.
