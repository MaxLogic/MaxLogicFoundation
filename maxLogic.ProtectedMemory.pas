unit maxLogic.ProtectedMemory;

{
  this is there to store really sensible data in the memory.
  just "best-effort in-process hardening" nothing more.

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
  Windows, Classes, SysUtils, SecureString, SynCrypto,
  maxLogic.MachineFingerprint;

type
  iProtectedMemory = interface
    ['{F73FE946-EE14-43A7-9BD0-72A5AA59A216}']
    procedure retrieve(var aData: rawByteString); overload;
    // might be a bit slower, due to conversion from rawByteString to bytes
    procedure retrieve(var aData: TBytes); overload;
    procedure retrieve(var aData: string); overload;
    procedure retrieve(var i: integer); overload;
  end;

// factory functions
// NOTE: you should zero the input after calling one of the functions, so it will not remain in memory
function newProtection(const Str: rawByteString): iProtectedMemory; overload;
function newProtection(const Bytes: TBytes): iProtectedMemory; overload;
function newProtection(Buffer: Pointer; BufferSize: integer): iProtectedMemory; overload;
function newProtection(const s: string): iProtectedMemory; overload;
function newProtection(const i: integer): iProtectedMemory; overload;
procedure ZeroBytes(var b: TBytes);

{$IF defined(msWindows) AND defined(debug) AND (not defined(NoVcl))}
procedure SelfTest;
{$IFEND}

implementation

uses
  {$IF defined(msWindows) AND defined(debug) AND (not defined(NoVcl))}
  ClipBrd,
  {$IFEND}
  InternPwHolder, maxLogic.AESHelper, IOUtils,
  SynLZ, SynCommons, diagnostics;

type
  TProtectedMemory = class(TInterfacedObject, iProtectedMemory)
  strict private
    fData: rawByteString;
    fSeed: Cardinal;
    fPasswordSalt: rawByteString;
    procedure GetPassword(var PW: rawByteString); inline;
  private
    procedure DoProtect(const aData: rawByteString);
  public
    constructor Create;
    destructor Destroy; override;

    procedure retrieve(var aData: rawByteString); overload;
    procedure retrieve(var aData: TBytes); overload;
    procedure retrieve(var aData: string); overload;
    procedure retrieve(var i: integer); overload;
  end;

procedure TProtectedMemory.retrieve(var i: integer);
var
  s: rawByteString;
  Len: integer;
begin
  retrieve(s);
  Len := length(s);
  if Len > 4 then
    Len := 4;
  if Len <> 0 then
    move(s[1], i, Len)
  else
    i := 0;
end;

procedure TProtectedMemory.retrieve(var aData: string);
var
  s: rawByteString;
begin
  retrieve(s);
  aData := SynCommons.UTF8ToString(s);
  FillZero(s);
end;

procedure TProtectedMemory.retrieve(var aData: TBytes);
var
  s: rawByteString;
begin
  retrieve(s);
  RawByteStringToBytes(s, aData);
  FillZero(s);
end;

procedure TProtectedMemory.retrieve(var aData: rawByteString);
var
  PW: rawByteString;
begin
  aData := '';
  if length(fData) = 0 then
    exit();

  try
    GetPassword(PW);

    aData := SynCrypto.CryptDataForCurrentUser(fData, PW, False);

  finally
    FillZero(PW);
  end;
end;

procedure TProtectedMemory.DoProtect(const aData: rawByteString);
var
  PW: rawByteString;
begin
  FillZero(fData);
  fData := '';

  if length(aData) = 0 then
    exit;

  try
    GetPassword(PW);

    fData := SynCrypto.CryptDataForCurrentUser(aData, PW, True);

  finally
    FillZero(PW);
  end;
end;

procedure TProtectedMemory.GetPassword(var PW: rawByteString);
var
  s, g: string;
begin
  g := '{8F423419-0713-4E9D-8656-A516BEF1C66C}';
  s := maxLogic.MachineFingerprint.GetMachineFingerprint(BytesOf('{76535522-4B82-431A-93BF-E8D60F899048}'))
    + TInternPwHolder.generate(g, fSeed).data;

  PW := fPasswordSalt + SynCommons.StringToUTF8(s);

  ZeroString(g);
  ZeroString(s);
end;

function newProtection(const Bytes: TBytes): iProtectedMemory;
var
  s: rawByteString;
begin
  BytesToRawByteString(Bytes, s);
  Result := newProtection(s);
end;

function newProtection(const Str: rawByteString): iProtectedMemory; overload;
var
  p: TProtectedMemory;
begin
  p := TProtectedMemory.Create;
  p.DoProtect(Str);
  Result := p;
end;

function newProtection(const i: integer): iProtectedMemory; overload;
var
  r: rawByteString;
begin
  SetLength(r, 4);
  move(i, r[1], 4);
  Result := newProtection(r);
  FillZero(r);
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

  Result := newProtection(r);
end;

function newProtection(Buffer: Pointer; BufferSize: integer): iProtectedMemory;
var
  Bytes: TBytes;
begin
  if BufferSize = 0 then
  begin
    Bytes := [];
  end else begin
    SetLength(Bytes, BufferSize);
    move(Buffer^, Bytes[0], BufferSize);
  end;

  Result := newProtection(Bytes);
end;

procedure ZeroBytes(var b: TBytes);
begin
  if length(b) <> 0 then
    ZeroMemory(@b[0], length(b));
end;

constructor TProtectedMemory.Create;
begin
  inherited Create;

  TAESPRNG.Main.FillRandom(@fSeed, 4);
  SetLength(fPasswordSalt, 64);
  TAESPRNG.Main.FillRandom(@fPasswordSalt[1], length(fPasswordSalt));
end;

{$IF defined(msWindows) AND defined(debug) AND (not defined(NoVcl))}
procedure SelfTest;
const
  BufferSize = 1024 * 1024;
  InterationCount = 999;
var
  p: iProtectedMemory;
  Bytes, b1, b2: TBytes;
  X: integer;
  firstTime: Int64;
  st: TStopWatch;
  S1, S2: rawByteString;
begin
  SetLength(Bytes, BufferSize);
  TAESPRNG.Main.FillRandom(@Bytes[0], BufferSize);
  BytesToRawByteString(Bytes, S1);

  st := TStopWatch.startNew;
  for X := 0 to InterationCount do
  begin
    S2 := copy(S1, 1, length(S1));
    p := newProtection(S2);
    p.retrieve(S2);

    if S1 <> S2 then
      raise Exception.Create('failed to compare');
  end;

  firstTime := st.ElapsedMilliseconds;

  st := TStopWatch.startNew;
  for X := 0 to InterationCount do
  begin
    b1 := copy(Bytes);
    b2 := copy(b1);

    if length(b2) <> BufferSize then
      raise Exception.Create('Error invalid size');
    if not compareMem(@Bytes[0], @b2[0], BufferSize) then
      raise Exception.Create('failed to compare');
  end;
  st.stop;
  ClipBoard.AsText := IntToStr(firstTime) + sLineBreak + IntToStr(st.ElapsedMilliseconds);

  beep;
end;
{$IFEND}

destructor TProtectedMemory.Destroy;
begin
  // burn the data
  FillZero(fData);
  fSeed := 0;
  FillZero(fPasswordSalt);

  inherited;
end;

initialization
  {$IF defined(msWindows) AND defined(debug) AND (not defined(NoVcl))}
  // selfTest;
  {$IFEND}

end.

