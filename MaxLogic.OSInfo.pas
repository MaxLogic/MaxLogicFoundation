unit MaxLogic.OSInfo;

interface

uses
  System.SysUtils;

{ Returns the current user locale as a BCP-47 style language tag, e.g.:
    'en-US', 'pl-PL', 'de-DE'.

  On Windows, this uses GetUserDefaultLocaleName.
  On other platforms, it is derived from LC_ALL / LANG environment variables
  (e.g. 'pl_PL.UTF-8' -> 'pl-PL'). }
function GetLocalOsLanguage: string;

{ Returns a human-readable OS description, e.g.:
    'Windows 11 64-bit (10.0.22621)'
    'Linux 64-bit (5.15.0-72)'
    'macOS 14 64-bit (23.0)'

  Built from TOSVersion.Name / Major / Minor / Build and CPU architecture. }
function GetOsDisplayName: string;

{ Returns CPU architecture as a string: '32-bit' or '64-bit'. }
function GetOsArchitectureString: string;

{ Returns True if the OS is 64-bit, False otherwise. }
function Is64BitOS: Boolean;

{ Returns the machine/host name.

  Priority:
    1. COMPUTERNAME env var (Windows)
    2. HOSTNAME env var (Unix-like)
    3. GetComputerName (Windows API)
  Fallback: 'unknown-host'. }
function GetMachineName: string;

{ Returns the current user name.

  Priority:
    1. USERNAME env var (Windows)
    2. USER env var (Unix-like)
    3. GetUserName (Windows API)
  Fallback: 'unknown-user'. }
function GetCurrentUserName: string;

implementation

uses
  System.StrUtils
  {$IFDEF MsWindows}
  , Winapi.Windows
  {$ENDIF}
  ;

function NormalizeLocaleTag(const aLocale: string): string;
var
  lLocale: string;
  lPos: Integer;
begin
  lLocale := Trim(aLocale);

  if lLocale = '' then
  begin
    Result := '';
    Exit;
  end;

  // Strip encoding/variant suffix like '.UTF-8' or '@variant'
  lPos := Pos('.', lLocale);
  if lPos > 0 then
  begin
    Delete(lLocale, lPos, MaxInt);
  end;

  lPos := Pos('@', lLocale);
  if lPos > 0 then
  begin
    Delete(lLocale, lPos, MaxInt);
  end;

  // Normalize underscore to hyphen: 'pl_PL' -> 'pl-PL'
  lLocale := StringReplace(lLocale, '_', '-', [rfReplaceAll]);

  Result := lLocale;
end;

function GetLocalOsLanguage: string;
{$IFDEF MsWindows}
var
  lName: array [0 .. LOCALE_NAME_MAX_LENGTH - 1] of Char;
  lLen: Integer;
begin
  // GetUserDefaultLocaleName returns e.g. 'en-US', 'pl-PL'
  lLen := GetUserDefaultLocaleName(@lName[0], LOCALE_NAME_MAX_LENGTH);
  if lLen > 0 then
  begin
    Result := string(PChar(@lName[0]));
  end
  else
  begin
    Result := '';
  end;

  if Result = '' then
  begin
    Result := 'en-US';
  end;
end;
{$ELSE}
var
  lLocale: string;
begin
  // Typical Unix locale: 'pl_PL.UTF-8', 'en_US.UTF-8'
  lLocale := GetEnvironmentVariable('LC_ALL');
  if lLocale = '' then
  begin
    lLocale := GetEnvironmentVariable('LANG');
  end;

  if lLocale <> '' then
  begin
    Result := NormalizeLocaleTag(lLocale);
  end
  else
  begin
    // Last-resort fallback
    Result := 'en';
  end;
end;
{$ENDIF}

function GetOsArchitectureString: string;
begin
  // TOSVersion.Architecture: arIntelX86, arIntelX64, arARM32, arARM64
  case TOSVersion.Architecture of
    arIntelX64, arARM64:
      Result := '64-bit';
  else
    Result := '32-bit';
  end;
end;

function Is64BitOS: Boolean;
begin
  Result :=
    TOSVersion.Architecture in [arIntelX64, arARM64];
end;

function GetOsDisplayName: string;
var
  lArch: string;
begin
  lArch := GetOsArchitectureString;

  // TOSVersion.Name is a friendly OS name; Major/Minor/Build are version numbers
  Result := Format('%s %s (%d.%d.%d)',
    [TOSVersion.Name, lArch, TOSVersion.Major, TOSVersion.Minor, TOSVersion.Build]);
end;

function GetMachineName: string;
var
  lName: string;
  {$IFDEF MsWindows}
  lSize: DWORD;
  {$ENDIF}
begin
  // Environment first – cheapest and cross-platform
  lName := GetEnvironmentVariable('COMPUTERNAME');
  if lName = '' then
  begin
    lName := GetEnvironmentVariable('HOSTNAME');
  end;

  {$IFDEF MsWindows}
  if lName = '' then
  begin
    // Fallback to WinAPI GetComputerNameW
    lSize := MAX_COMPUTERNAME_LENGTH + 1;
    SetLength(lName, lSize);

    if GetComputerName(PChar(lName), lSize) then
    begin
      // lSize is number of chars without trailing null
      SetLength(lName, lSize);
    end
    else
    begin
      lName := '';
    end;
  end;
  {$ENDIF}

  if lName = '' then
  begin
    Result := 'unknown-host';
  end
  else
  begin
    Result := lName;
  end;
end;

function GetCurrentUserName: string;
var
  lName: string;
  {$IFDEF MsWindows}
  lSize: DWORD;
  {$ENDIF}
begin
  // Common env vars
  lName := GetEnvironmentVariable('USERNAME');
  if lName = '' then
  begin
    lName := GetEnvironmentVariable('USER');
  end;

  {$IFDEF MsWindows}
  if lName = '' then
  begin
    // Fallback to WinAPI GetUserNameW
    lSize := 256;
    SetLength(lName, lSize);

    if GetUserName(PChar(lName), lSize) then
    begin
      // lSize includes terminating null
      if lSize > 0 then
      begin
        SetLength(lName, lSize - 1);
      end;
    end
    else
    begin
      lName := '';
    end;
  end;
  {$ENDIF}

  if lName = '' then
  begin
    Result := 'unknown-user';
  end
  else
  begin
    Result := lName;
  end;
end;

end.

