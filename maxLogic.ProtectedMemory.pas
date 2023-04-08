unit maxLogic.ProtectedMemory;

{
  this is there to store really sensible data in the memory.

  NOTE: after usage, always fill the data with zeroes

  helpful functions:
  SynCommons.rawByteStringToBytes
  SynCommons.bytesToRawByteString
  SynCommons.StringToUTF8 and utf8ToString

  /// convert a Delphi 2009+ or FPC Unicode string into our UTF-8 string
  function UnicodeStringToUtf8(const S: UnicodeString): RawUTF8; inline;

  // this function is the same as direct RawUTF8=AnsiString(CP_UTF8) assignment
  // but is faster, since it uses no Win32 API call
  function UTF8DecodeToUnicodeString(const S: RawUTF8): UnicodeString; overload; inline;

}

interface

uses
  windows, classes, sysUtils, SecureString, SynCrypto;

type
  iProtectedMemory = interface
    ['{F73FE946-EE14-43A7-9BD0-72A5AA59A216}']
    procedure retrive(var adata: rawByteString); OVERLOAD;
    // might be a bit slower, due to conversion from rawByteString to bytes
    procedure retrive(var adata: TBytes); OVERLOAD;
    procedure retrive(var adata: string); OVERLOAD;
    procedure retrive(var i: integer); OVERLOAD;
  END;

  // factory functions
  // NOTE: you should zero the input after calling one of the functions, so it will not remain in memory
function newProtection(const str: rawByteString): iProtectedMemory; overload;
function newProtection(const bytes: TBytes): iProtectedMemory; overload;
function newProtection(buffer: pointer; BufferSize: integer): iProtectedMemory; overload;
function newProtection(const s: string): iProtectedMemory; overload;
function newProtection(const i: integer): iProtectedMemory; overload;
procedure ZeroBytes(var b: TBytes);

procedure selfTest;

implementation

uses
  pawel1, InternPwHolder, maxLogic.AESHelper, ioUtils,
  SynLZ, SynCommons, diagnostics, clipBrd;

type
  TProtectedMemory = class(TInterfacedObject, iProtectedMemory)
  strict private
    fData: rawByteString;
    fSeed: cardinal;
    fPasswordSalt: rawByteString;
    procedure GetPassword(var pw: rawByteString); inline;
  private
    procedure DoProtect(const adata: rawByteString);
  public
    constructor Create;
    destructor destroy; override;

    procedure retrive(var adata: rawByteString); OVERLOAD;
    procedure retrive(var adata: TBytes); OVERLOAD;
    procedure retrive(var adata: string); OVERLOAD;
    procedure retrive(var i: integer); OVERLOAD;
  end;

procedure TProtectedMemory.retrive(var i: integer);
var
  s: rawByteString;
  len: integer;
begin
  retrive(s);
  len := Length(s);
  if len > 4 then
    len := 4;
  if len <> 0 then
    move(s[1], i, len)
  else
    i := 0;
end;

procedure TProtectedMemory.retrive(var adata: string);
var
  s: rawByteString;
begin
  retrive(s);
  adata := SynCommons.utf8ToString(s);
  fillZero(s);
end;

procedure TProtectedMemory.retrive(var adata: TBytes);
VAR
  s: rawByteString;
BEGIN
  retrive(s);
  RawByteStringToBytes(s, adata);
  fillZero(s);
END;

procedure TProtectedMemory.retrive(var adata: rawByteString);
var
  pw: rawByteString;
begin
  adata := '';
  if Length(fData) = 0 then
    exit();

  try
    GetPassword(pw);

    adata := SynCrypto.CryptDataForCurrentUser(fData, pw, false);

  finally
    fillZero(pw);
  end;
end;

procedure TProtectedMemory.DoProtect(const adata: rawByteString);
var
  pw: rawByteString;
begin
  fillZero(fData);
  fData := '';

  if Length(adata) = 0 then
    exit;

  try
    GetPassword(pw);

    fData := SynCrypto.CryptDataForCurrentUser(adata, pw, true);

  finally
    fillZero(pw);
  end;
end;

procedure TProtectedMemory.GetPassword(var pw: rawByteString);
var
  s, g: string;
begin
  g := '{8F423419-0713-4E9D-8656-A516BEF1C66C}';
  s := pawel1.GenerateUniqueMachineIdEx + TInternPwHolder.generate(g, fSeed).data;

  pw := fPasswordSalt + SynCommons.StringToUTF8(s);

  ZeroString(g);
  ZeroString(s);
end;

function newProtection(const bytes: TBytes): iProtectedMemory;
VAR
  s: rawByteString;
BEGIN
  bytesToRawByteString(bytes, s);
  result := newProtection(s);
END;

function newProtection(const str: rawByteString): iProtectedMemory; overload;
var
  p: TProtectedMemory;
begin
  p := TProtectedMemory.Create;
  p.DoProtect(str);
  result := p;
end;

function newProtection(const i: integer): iProtectedMemory; overload;
var
  r: rawByteString;
begin
  setLength(r, 4);
  move(i, r[1], 4);
  result := newProtection(r);
  fillZero(r);
end;

function newProtection(const s: string): iProtectedMemory;
var
  r: rawByteString;
begin
  if s <> '' then
  begin
    // bytes := TEncoding.utf8.GetBytes(s);
    r := SynCommons.StringToUTF8(s);
  end
  else
    r := '';

  result := newProtection(r);
end;

function newProtection(buffer: pointer; BufferSize: integer): iProtectedMemory;
var
  bytes: TBytes;
begin
  if BufferSize = 0 then
  begin
    bytes := [];
  end else begin
    setLength(bytes, BufferSize);
    move(buffer^, bytes[0], BufferSize);
  end;

  result := newProtection(bytes);
end;

procedure ZeroBytes(var b: TBytes);
begin
  if Length(b) <> 0 then
    ZeroMemory(@b[0], Length(b));
end;

constructor TProtectedMemory.Create;
begin
  inherited Create;

  TAESPRNG.Main.FillRandom(@fSeed, 4);
  setLength(fPasswordSalt, 64);
  TAESPRNG.Main.FillRandom(@fPasswordSalt[1], Length(fPasswordSalt));
end;

procedure selfTest;
const
  BufferSize = 1024 * 1024;
  InterationCount = 999;
var
  p: iProtectedMemory;
  bytes, b1, b2: TBytes;
  x: integer;
  firstTime: int64;
  st: TStopWatch;
  s1, s2: rawByteString;
begin
  setLength(bytes, BufferSize);
  TAESPRNG.Main.FillRandom(@bytes[0], BufferSize);
  bytesToRawByteString(bytes, s1);

  st := TStopWatch.startNew;
  for x := 0 to InterationCount do
  begin
    s2 := copy(s1, 1, Length(s1));
    p := newProtection(s2);
    p.retrive(s2);

    if s1 <> s2 then
      raise Exception.Create('failed to compare');
  end;

  firstTime := st.elapsedMilliseconds;

  st := TStopWatch.startNew;
  for x := 0 to InterationCount do
  begin
    b1 := copy(bytes);
    b2 := copy(b1);

    if Length(b2) <> BufferSize then
      raise Exception.Create('Error invalid size');
    if not compareMem(@bytes[0], @b2[0], BufferSize) then
      raise Exception.Create('failed to compare');
  end;
  st.stop;
  clipBoard.asText := IntToStr(firstTime) + cr + IntToStr(st.elapsedMilliseconds);

  beep;
end;

destructor TProtectedMemory.destroy;
begin
  // burn the data
  fillZero(fData);
  fSeed := 0;
  fillZero(fPasswordSalt);

  inherited;
end;

initialization

// selfTest;

end.
