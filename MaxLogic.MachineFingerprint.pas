unit MaxLogic.MachineFingerprint;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Hash,
  System.NetEncoding;

{ Returns the best available raw machine identifier for the current OS:
  - Windows : HKLM\SOFTWARE\Microsoft\Cryptography\MachineGuid
  - Linux   : /etc/machine-id or /var/lib/dbus/machine-id
  - macOS   : IOPlatformUUID (via `ioreg`)
  - Others  : fallback based on hostname, OS version and executable path }
function GetRawMachineId: string;

{ Returns a stable, app-specific machine fingerprint:
  fingerprint = Base64URL( HMAC-SHA256( raw_machine_id, aAppKey ) )

  - aAppKey must be a non-empty secret key (same on client and server).
  - Uses UTF-8 encoding for the raw machine ID string. }
function GetMachineFingerprint(const aAppKey: TBytes): string;

implementation

uses
  System.IOUtils,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  System.Win.Registry,
  {$ENDIF}
  {$IFDEF LINUX}
  Posix.Unistd,
  {$ENDIF}
  {$IFDEF MACOS}
  Posix.Unistd,
  Posix.Stdlib,
  Posix.Stdio,
  {$ENDIF}
  System.StrUtils;

{$IFDEF MACOS}
type
  PFILE = Pointer; // forward alias; Posix.Stdio also defines this, but this is harmless
{$ENDIF}

function NormalizeMachineId(const aValue: string): string;
var
  lValue: string;
begin
  lValue := Trim(aValue);
  Result := LowerCase(lValue);
end;

function BuildFallbackId: string;
var
  lHost: string;
begin
  lHost := GetEnvironmentVariable('COMPUTERNAME');
  if lHost = '' then
    lHost := GetEnvironmentVariable('HOSTNAME');

  if lHost = '' then
    lHost := 'unknown-host';

  Result := Format('host=%s;os=%s;exe=%s',
    [lHost, TOSVersion.ToString, ParamStr(0)]);
end;

{$IFDEF MSWINDOWS}
function GetWindowsMachineGuid: string;
var
  lReg: TRegistry;
  lValue: string;
begin
  Result := '';
  lReg := TRegistry.Create(KEY_READ);
  try
    lReg.RootKey := HKEY_LOCAL_MACHINE;
    if lReg.OpenKeyReadOnly('SOFTWARE\Microsoft\Cryptography') then
    begin
      lValue := lReg.ReadString('MachineGuid');
      Result := Trim(lValue);
    end;
  finally
    lReg.Free;
  end;
end;
{$ENDIF}

{$IFDEF LINUX}
function GetLinuxMachineIdFromFile(const aPath: string): string;
var
  lContent: string;
begin
  Result := '';
  if not TFile.Exists(aPath) then
    Exit;

  try
    lContent := TFile.ReadAllText(aPath, TEncoding.UTF8);
  except
    Exit;
  end;

  Result := Trim(lContent);
end;

function GetLinuxMachineId: string;
var
  lId: string;
begin
  lId := GetLinuxMachineIdFromFile('/etc/machine-id');
  if lId = '' then
    lId := GetLinuxMachineIdFromFile('/var/lib/dbus/machine-id');

  Result := lId;
end;
{$ENDIF}

{$IFDEF MACOS}
function GetMacHardwareUUID: string;
const
  cCommand: AnsiString = 'ioreg -rd1 -c IOPlatformExpertDevice';
var
  lPipe: PFILE;
  lBuf: array[0..1023] of AnsiChar;
  lLine: string;
  lFound: Boolean;
  lEqPos: Integer;
  lFirstQuote: Integer;
  lSecondQuote: Integer;
begin
  Result := '';
  lPipe := popen(PAnsiChar(cCommand), PAnsiChar(AnsiString('r')));
  if lPipe = nil then
    Exit;

  lFound := False;
  lLine := '';

  try
    while fgets(@lBuf[0], SizeOf(lBuf), lPipe) <> nil do
    begin
      lLine := string(UTF8String(AnsiString(lBuf)));
      if Pos('IOPlatformUUID', lLine) > 0 then
      begin
        lFound := True;
        Break;
      end;
    end;
  finally
    pclose(lPipe);
  end;

  if not lFound then
    Exit;

  lLine := Trim(lLine);

  // expected format:
  // "IOPlatformUUID" = "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"
  lEqPos := Pos('=', lLine);
  if lEqPos > 0 then
  begin
    lFirstQuote := PosEx('"', lLine, lEqPos);
    if lFirstQuote > 0 then
    begin
      Inc(lFirstQuote);
      lSecondQuote := PosEx('"', lLine, lFirstQuote);
      if (lSecondQuote > lFirstQuote) then
        Result := Copy(lLine, lFirstQuote, lSecondQuote - lFirstQuote);
    end;
  end;

  Result := Trim(Result);
end;
{$ENDIF}

function GetRawMachineId: string;
var
  lId: string;
begin
  lId := '';

  {$IFDEF MSWINDOWS}
  lId := GetWindowsMachineGuid;
  {$ENDIF}

  {$IFDEF LINUX}
  lId := GetLinuxMachineId;
  {$ENDIF}

  {$IFDEF MACOS}
  lId := GetMacHardwareUUID;
  {$ENDIF}

  if lId = '' then
    lId := BuildFallbackId;

  Result := NormalizeMachineId(lId);
end;

function GetMachineFingerprint(const aAppKey: TBytes): string;
var
  lRawId: string;
  lRawBytes: TBytes;
  lHmac: TBytes;
begin
  if Length(aAppKey) = 0 then
    raise EArgumentException.Create('GetMachineFingerprint: aAppKey must not be empty.');

  lRawId := GetRawMachineId;
  // Use UTF-8 for deterministic encoding of the raw machine ID
  lRawBytes := TEncoding.UTF8.GetBytes(lRawId);

  lHmac := THashSHA2.GetHMACAsBytes(
    lRawBytes,
    aAppKey,
    THashSHA2.TSHA2Version.SHA256
  );

  Result := TNetEncoding.Base64URL.EncodeBytesToString(lHmac);
end;

end.

