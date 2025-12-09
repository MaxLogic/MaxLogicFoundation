unit maxLogic.IOUtils;

{
  work in progress: for now only some methods exported from old big the pawel1.pas
  Version: 0.20
}

{$IF DEFINED(FRAMEWORK_VCL) OR DEFINED(FRAMEWORK_FMXs)}
{$IFNDEF CONSOLE}
{$DEFINE CanUseApplicationInstance}
{$ENDIF}
{$IFEND}

interface

uses
  {$IFDEF MadExcept}madExcept, {$ENDIF}
  {$IF DEFINED( MsWINDOWS)}
  winApi.Windows, System.Win.ComObj, winApi.WinInet, winApi.ShLwApi, winApi.ShlObj, winApi.SHFolder,
  maxConsoleRunner,
  {$ELSEIF DEFINED(POSIX)}
  Posix.Stdlib,
  Posix.Unistd,
  Posix.Dlfcn,
  {$IFEND}
  System.Classes, System.SysUtils, generics.collections, generics.defaults;

function GetCurrentDLLName: string;
{$IFDEF MSWINDOWS}
function GetBuildInfo: string; overload;
function GetBuildInfo(const FileName: string; out V1, V2, v3, v4: Word): string; overload;
function GetVersionString(const prefix: string = ' v'): string;
// as GetBuildInfo but returns only the last number which is responsible for the build version or build number
function GetBuildNumber: Word; overload;
function GetBuildNumber(const FileName: string): Word; overload;
{$ENDIF}

{$IFDEF MSWINDOWS}
{ Default Encoding is utf8
  include in tc file like that:
  <ResName>RCDATA <FileName>
}
function LoatStringFromResource(const aResName: string; out aValue: string; const aDefault: string = ''; aEncoding: TEncoding = nil): boolean; overload;
// Default is ''
function LoatStringFromResource(const aResName: string; aEncoding: TEncoding = nil): string; overload;

{$ENDIF}

// works also for dll files
function GetInstallDir: string;

{$IFDEF MSWINDOWS}
procedure exec(const FileName: string; const Parameter: string = ''; StartDir: string = '');

procedure ExecuteFile(const aFileName: string;
  const AParameter, ACurrentDir: string; AWait: boolean;
  aRunHidden: boolean = False); overload;
procedure ExecuteFile(const Cmd, ACurrentDir: string; AWait: boolean;
  aRunHidden: boolean = False); overload;

type
  TStrProc = maxConsoleRunner.TDataReadyProc;

// similar to the above, but now we also get the stdOut and errOut and the exit code
procedure ExecuteFile(const Cmd, ACurrentDir: string;
  out aExitCode: integer;
  aOnStdOut: TStrProc = nil;
  aOnErrOut: TStrProc = nil;
  aRunHidden: boolean = True); overload;

{$ENDIF}

// default encoding is utf8
// aWaitBetweenRetries is in milliseconds
// returns true if the write was successful false otherwise
function SafeAppendToFile(const aFileName, aText: string; aRetryCount: integer = 5; aWaitBetweenRetries: integer = 10; aEncoding: TEncoding = nil): boolean;

{$IFDEF MSWINDOWS}
function RecycleItem(const ItemName: string; const DeleteToRecycle: boolean = True; const ShowConfirm: boolean = True; const TotalSilence: boolean = False): boolean;
{$ENDIF}

function CombinePath(const aParts: array of string; aAddFinalTrailingPathDelimiter: boolean = False): string;

/// <summary>
/// <b>OBSOLETE.</b>  Alias for <see cref="SanitizeFileName" />.
/// </summary>
/// <remarks>
/// *Kept for backward compatibility only.*  New code should call
/// <see cref="SanitizeFileName" /> directly.  The implementation
/// simply forwards to the new helper, so behaviour is identical.
/// </remarks>
{$WARN SYMBOL_DEPRECATED ON}
function ConvertToValidDirectoryName(const aText: string;
  aReplaceInvalidCharsWith: char = '_'): string;
  deprecated 'ConvertToValidDirectoryName is obsolete. Use SanitizeFileName instead.';

/// <summary>
/// Makes a file or directory *name* safe for all supported platforms.
/// Optionally processes a full path while preserving path separators.
/// </summary>
/// <param name="aRawName">
///   Untrusted input: either a single name (default mode) or a full path.
/// </param>
/// <param name="aReplacement">
///   Replacement for each invalid character (defaults to '_').
///   If set to #0, invalid characters are removed.
/// </param>
/// <param name="aHonorSeparators">
///   When *true*, treats directory separators as boundaries and sanitizes each segment;
///  when *false* (default), sanitizes the whole string as a single name (no separators allowed).
/// </param>
/// <returns>
///   A safe name/path; never empty (falls back to 'untitled').
///   On Windows, segments are also stripped of trailing dot/space and
///   avoid reserved device names.
/// </returns>

function SanitizeFileName(const aRawName: string;
  const aReplacement: char = '_';
  const aHonorSeparators: boolean = False): string;

/// <summary>
/// This function checks the provided filename to determine if it exceeds the Windows path length limit. It then appropriately prefixes the filename to support extended-length paths. The function also distinguishes between local and network paths, applying a different prefix based on the path type. Note that for network paths, the initial double backslashes (`\\`) are replaced with the `\\?\UNC\` prefix, while local paths simply receive the `\\?\` prefix.
/// Remember, even with these prefixes, some Windows APIs and applications may not support extended-length paths or may require certain group policy settings to be enabled for support.
/// on non windows systems the result is the same as the input
/// </summary>
function LongFileNameFix(const aFileName: string): string;

// ensures the last char is TPath.DirectorySeparatorChar
function slash(const aPath: string): string;
function UnSlash(const aPath: string): string;

// NOTE: Environment Changes Scope: As on Windows, changes made by setenv in this way only affect the current process and its child processes. They do not affect the system-wide environment or persist after the application terminates.
procedure SetEnvironmentPath(const aPath: string);
function FilePathToURL(const aFilePath: string): string;

// for windows it will exchange "/" with "\"
// it will also replace "//" with "/" with a single /
function NormalizePath(const aPath: string): string;

// This forces the path into a system-compatible ANSI encoding.
// on non windows OS just returns the input
function GetAnsiFileName(const aLongPath: string): string;
// ATTENTION: shortens the path only on windows, on other systems it just returns the input
function GetShortPath(const aLongPath: string): string;

/// <summary>
/// Properly quotes a string for use as a command-line argument in the current platform's shell.
/// Automatically selects the appropriate quoting method based on the platform (Windows or Unix/Linux).
/// </summary>
/// <param name="aValue">The string to be quoted for shell use</param>
/// <returns>A properly quoted string safe to use as a shell command argument</returns>
function QuoteShellArgument(const aValue: string): string;

/// <summary>
/// Properly quotes a string for use in Unix/Linux shell commands using single quotes.
/// Handles special characters and escapes embedded single quotes according to bash conventions.
/// Example: test\'1 becomes 'test'\''1' which is the proper Linux way to escape single quotes.
/// </summary>
/// <param name="aValue">The string to be quoted for Unix shell use</param>
/// <returns>A properly quoted string safe to use in bash/sh command lines</returns>
function QuoteUnixShellArgument(const aValue: string): string;

/// <summary>
/// Properly quotes a string for use in Windows command shell (cmd.exe).
/// Handles Windows' complex escaping rules, with special treatment for:
/// - Backslashes before quotes (doubled)
/// - Double quotes (escaped with backslashes)
/// - Spaces and other special characters
/// </summary>
/// <param name="aValue">The string to be quoted for Windows shell use</param>
/// <returns>A properly quoted string safe to use in Windows command lines</returns>
function QuoteWindowsShellArgument(const aValue: string): string;

function GetLocalAppDataPath: string;

{
  lProgramFiles := GetSpecialWindowsFolder(CSIDL_PROGRAM_FILES);
  lProgramFilesX86 := GetSpecialWindowsFolder(CSIDL_PROGRAM_FILESX86);
  lAppData:= GetSpecialWindowsFolder(CSIDL_APPDATA);
}
{$IFDEF MSWINDOWS}
function GetSpecialWindowsFolder(aFolder: integer): string;
{$ENDIF}

function IsInPath(const aPath, aFileName: string): boolean;

function GetFileDate(out aCreateTime, aLastAccess, aLastWriteTime: TDateTime; const aFileName: string): boolean; overload;
function GetFileDate(const aFileName: string): TDateTime; overload;

// smark BOM detection. If no bom preset, checks if file is utf8.
function LoadStrFromFile(const aFileName: string): string;

// normalizes the path, then compares
function SamePath(const aPath1, aPath2: string): boolean;

type
  // keep your existing enum as-is
  TSpecialFolderKind = (
    sfkCurUser_My_Documents,
    sfkAllUsers_Application_Data,
    sfkUserSpecific_Application_Data,
    sfkProgramFiles,
    sfkAllUsers_Documents
    // no good cross-platform analogue for Desktop and Drives
    {$IFDEF MSWINDOWS}
    , sfkDesktop,
    sfkDrives
    {$ENDIF}
    );

function GetSpecialFolderPath(FolderKind: TSpecialFolderKind): string;

// some bytes read/write helpers
procedure writeToBytesAndIncOffset(const Buf; BuffSize: integer;
  var Bytes: TBytes; var offset: integer);
procedure writeToBytes(const Buf; BufSize: integer; var Bytes: TBytes;
  offset: integer);
procedure readFromBytesAndIncOffset(var Buf; BuffSize: integer;
  const Bytes: TBytes; var offset: integer);
procedure readFromBytes(var Buf; BufSize: integer; const Bytes: TBytes;
  offset: integer);
procedure FillZeroBytes(var Bytes: TBytes);


implementation

uses
  {$IFDEF MSWINDOWS}
  ShellApi,
  {$ENDIF}
  {$IFDEF CanUseApplicationInstance}
  Forms,
  {$ENDIF}
  System.IOUtils, System.StrUtils, AutoFree;

{$IFDEF MSWINDOWS}

function LoatStringFromResource(const aResName: string; out aValue: string; const aDefault: string = ''; aEncoding: TEncoding = nil): boolean;
var
  RS: TResourceStream;
  Bytes: TBytes;
begin
  aValue := aDefault;
  Result := False;
  if aEncoding = nil then
    aEncoding := TEncoding.UTF8;

  RS := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    if RS.Size > 0 then
    begin
      SetLength(Bytes, RS.Size);
      RS.ReadBuffer(Bytes[0], RS.Size);
      aValue := aEncoding.GetString(Bytes);
      Result := True;
    end;
  finally
    RS.Free;
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}

function LoatStringFromResource(const aResName: string; aEncoding: TEncoding = nil): string;
begin
  if not LoatStringFromResource(aResName, Result) then
    Result := '';
end;

{$ENDIF}

{$IFDEF MSWINDOWS}

function GetVersionString(const prefix: string = ' v'): string;
var
  s: string;
begin
  s := GetBuildInfo;
  if s = '' then
    Result := ''
  else
    Result := { Application.Title + }prefix + s;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}

function GetBuildNumber: Word;
begin
  Result := GetBuildNumber(GetCurrentDLLName);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}

function GetBuildNumber(const FileName: string): Word;
var
  V1, V2, v3: Word;
begin
  GetBuildInfo(FileName, V1, V2, v3, Result);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}

function GetBuildInfo: string;
var
  V1, V2, v3, v4: Word;
begin
  V1 := 0;
  V2 := 0;
  v3 := 0;
  v4 := 0;
  try
    Result := GetBuildInfo(GetCurrentDLLName(), V1, V2, v3, v4);
  except
    Result := '';
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}

function GetBuildInfo(const FileName: string; out V1, V2, v3, v4: Word): string;
var
  VerInfoSize: Cardinal;
  VerInfo: Pointer;
  VerValueSize: Cardinal;
  VerValue: PVSFixedFileInfo;
  dummy: Cardinal;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), dummy);
  if VerInfoSize = 0 then
  begin
    dummy := GetLastError;

    Result := '';
    exit;
  end; { if }
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  with VerValue^ do
  begin
    V1 := dwFileVersionMS shr 16;
    V2 := dwFileVersionMS and $FFFF;
    v3 := dwFileVersionLS shr 16;
    v4 := dwFileVersionLS and $FFFF;
  end;
  FreeMem(VerInfo, VerInfoSize);
  Result := IntToStr(V1) + '.' + IntToStr(V2) + '.' + IntToStr(v3) + '.' +
    IntToStr(v4);
end;
{$ENDIF}

function GetCurrentDLLName: string;
var
  s: string;
  i: integer;
begin
  SetLength(s, max_path);
  i := GetModuleFileName(HInstance, @s[1], max_path);
  if i > 0 then
  begin
    SetLength(s, i);
    Result := s;
  end
  else
    Result := '';
end;

function GetInstallDir: string;
begin
  Result := GetCurrentDLLName;
  Result := ExtractFilePath(Result);
end;

{$IFDEF MSWINDOWS}

procedure exec(const FileName: string; const Parameter: string = ''; StartDir: string = '');
begin
  if StartDir = '' then
    StartDir := ExtractFilePath(FileName);
  ShellExecute(0, nil, PChar(FileName), PChar(Parameter), PChar(StartDir),
    SW_NORMAL);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}

procedure ExecuteFile(const Cmd, ACurrentDir: string;
  out aExitCode: integer;
  aOnStdOut: TStrProc = nil;
  aOnErrOut: TStrProc = nil;
  aRunHidden: boolean = True);
var
  lRunner: TmaxConsoleRunner;
begin
  lRunner := TmaxConsoleRunner.Create;
  try
    lRunner.DecodeCommand(Cmd);
    lRunner.OnStdDataRead := aOnStdOut;
    lRunner.OnErrorDataRead := aOnErrOut;
    lRunner.RunHidden := aRunHidden;

    lRunner.Execute;
    aExitCode := lRunner.ExitCode;
  finally
    lRunner.Free;
  end;
end;

procedure ExecuteFile(
  const aFileName: string;
  const AParameter, ACurrentDir: string; AWait: boolean;
  aRunHidden: boolean = False);
var
  Cmd: string;
  dir: string;
begin
  dir := ACurrentDir;
  if dir = '' then
    dir := ExtractFilePath(aFileName);

  Cmd := aFileName;
  if (Cmd <> '') and (Cmd[1] <> '"') then
    Cmd := '"' + Cmd + '"';

  if AParameter <> '' then
    Cmd := Cmd + ' ' + AParameter;

  ExecuteFile(Cmd, dir, AWait, aRunHidden);
end;
{$ENDIF}
{$IFDEF MSWINDOWS}

procedure ExecuteFile(
  const Cmd, ACurrentDir: string; AWait: boolean;
  aRunHidden: boolean = False); overload;
var
  si: TStartupInfo;
  pi: TProcessInformation;
  lCmd, lDir: string;
begin
  lDir := ACurrentDir;

  ZeroMemory(@si, SizeOf(si));
  with si do
  begin
    cb := SizeOf(si);
    dwFlags := STARTF_USESHOWWINDOW;
    if aRunHidden then
      wShowWindow := SW_HIDE
    else
      wShowWindow := SW_NORMAL;
  end;

  ZeroMemory(@pi, SizeOf(pi));

  lCmd := Cmd;
  UniqueString(lCmd);

  if CreateProcess(nil, PChar(lCmd), nil, nil, False,
    CREATE_DEFAULT_ERROR_MODE or CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
    nil, PChar(lDir), si, pi) then
  begin
    try
      if AWait then
      begin
        {$IFDEF MadExcept}madExcept.PauseFreezeCheck(True);
        {$ENDIF}
        WaitForSingleObject(pi.hProcess, INFINITE);
        {$IFDEF MadExcept}madExcept.PauseFreezeCheck(False);
        {$ENDIF}
      end;
    finally
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    end;
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}

function RecycleItem(const ItemName: string; const DeleteToRecycle: boolean = True; const ShowConfirm: boolean = True; const TotalSilence: boolean = False): boolean;
var
  SHFileOpStruct: TSHFileOpStruct;
begin
  fillchar(SHFileOpStruct, SizeOf(SHFileOpStruct), #0);
  {$IFDEF CanUseApplicationInstance}
  if assigned(application) and assigned(application.MainForm) then
    SHFileOpStruct.wnd := application.MainForm.Handle { Others are using 0. But Application.MainForm.Handle is better because otherwise, the 'Are you sure you want to delete' will be hidden under program's window }
  else
    {$ENDIF}
    SHFileOpStruct.wnd := 0;

  SHFileOpStruct.wFunc := FO_DELETE;
  SHFileOpStruct.pFrom := PChar(ItemName + #0);
  SHFileOpStruct.pTo := nil;
  SHFileOpStruct.hNameMappings := nil;

  if DeleteToRecycle then
    SHFileOpStruct.fFlags := SHFileOpStruct.fFlags or FOF_ALLOWUNDO;

  if TotalSilence then
    SHFileOpStruct.fFlags := SHFileOpStruct.fFlags or FOF_NO_UI
  else
    if not ShowConfirm then
    SHFileOpStruct.fFlags := SHFileOpStruct.fFlags or FOF_NOCONFIRMATION;

  Result := SHFileOperation(SHFileOpStruct) = 0;
end;
{$ENDIF}

function SafeAppendToFile(const aFileName, aText: string; aRetryCount: integer = 5; aWaitBetweenRetries: integer = 10; aEncoding: TEncoding = nil): boolean;
var
  RetryCounter: integer;
begin
  Result := False;
  RetryCounter := 0;
  if aRetryCount < 1 then
    aRetryCount := 1;
  if aEncoding = nil then
    aEncoding := TEncoding.UTF8;

  repeat
    try
      TFile.AppendAllText(aFileName, aText, aEncoding);
      exit(True);
    except
      Inc(RetryCounter);
      Sleep(aWaitBetweenRetries);
    end;
  until RetryCounter >= aRetryCount;
end;

function CombinePath(const aParts: array of string; aAddFinalTrailingPathDelimiter: boolean = False): string;
var
  X: integer;
begin
  if length(aParts) = 0 then
    exit('');

  Result := aParts[0];
  for X := 1 to length(aParts) - 1 do
    Result := TPath.Combine(Result, aParts[X]);
  if aAddFinalTrailingPathDelimiter then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function SanitizeFileName(const aRawName: string;
  const aReplacement: char;
  const aHonorSeparators: boolean): string;

  function ReplaceOrRemoveInvalid(const s: string; const repl: char): string;
  var
    i: integer;
  begin
    Result := s;
    for i := 1 to length(Result) do
      if not TPath.IsValidFileNameChar(Result[i]) then // cross-platform invalid-char check
      begin
        if repl = #0 then
          Result[i] := #0 // mark for deletion, compact later
        else
          Result[i] := repl;
      end;

    if repl = #0 then
    begin
      // compact string by removing #0 markers
      Result := Result.Replace(#0, '');
    end;
  end;

  {$IFDEF MSWINDOWS}
  function WindowsFixups(const seg: string): string;
  const
    // Windows reserved device names (case-insensitive)
    CReserved: array[0..21] of string = (
      'CON', 'PRN', 'AUX', 'NUL',
      'COM1', 'COM2', 'COM3', 'COM4', 'COM5', 'COM6', 'COM7', 'COM8', 'COM9',
      'LPT1', 'LPT2', 'LPT3', 'LPT4', 'LPT5', 'LPT6', 'LPT7', 'LPT8', 'LPT9'
      );
  var
    u: string;
  begin
    Result := seg;

    // strip trailing space or dot (Windows UI/Win32 rules)
    while (Result <> '') and ((Result[High(Result)] = ' ') or (Result[High(Result)] = '.')) do
      delete(Result, length(Result), 1);

    // avoid reserved DOS device names
    u := UpperCase(Result);
    if matchText(u, CReserved) then
      Result := '_' + Result;
  end;

  {$ELSE}
  function WindowsFixups(const seg: string): string;
  begin
    // no-op on non-Windows
    Result := seg;
  end;
  {$ENDIF}

  function FixEmptyToUntitled(const s: string): string;
  const
    cUntitled = 'untitled';
  begin
    if s.IsEmpty then
      Result := cUntitled
    else
      Result := s;
  end;

  function SanitizeSegment(const Raw: string; const repl: char): string;
  begin
    Result := ReplaceOrRemoveInvalid(Raw, repl);
    Result := WindowsFixups(Result);
    Result := FixEmptyToUntitled(Result);
  end;

  function IsDirSep(const ch: char): boolean;
  begin
    // Accept both primary and alternate separators on all platforms
    Result := (ch = TPath.DirectorySeparatorChar) or (ch = TPath.AltDirectorySeparatorChar);
  end;

  function SanitizeFullPath(const Raw: string; const repl: char): string;
  var
    i, segStart: integer;
    seg, sep: string;
    c: char;

    // helper to flush the current segment to output
    procedure FlushSegment;
    begin
      seg := copy(Raw, segStart, i - segStart);
      Result := Result + SanitizeSegment(seg, repl) + sep;
      sep := '';
      segStart := i + 1;
    end;

    function IsVolumeSepHere: boolean;
    begin
      // treat "C:" drive as a boundary on Windows only
      {$IFDEF MSWINDOWS}
      Result := (i = 2) and (Raw[i] = TPath.VolumeSeparatorChar) and
        (length(Raw) >= 2) and CharInSet(string(Raw[1]).ToUpper[1], ['A'..'Z']);
      {$ELSE}
      Result := False;
      {$ENDIF}
    end;
  begin
    Result := '';
    if Raw = '' then
      exit(FixEmptyToUntitled(Raw));

    segStart := 1;
    sep := '';
    i := 1;
    while i <= length(Raw) do
    begin
      c := Raw[i];

      if IsDirSep(c) then
      begin
        sep := c; // keep the exact separator as in input
        FlushSegment; // sanitize segment before the separator
      end
      else if IsVolumeSepHere then
      begin
        // keep "<drive>:"
        Result := Result + copy(Raw, segStart, i - segStart + 1);
        segStart := i + 1;
      end;

      Inc(i);
    end;

    // tail segment (no trailing separator)
    seg := copy(Raw, segStart, length(Raw) - segStart + 1);
    Result := Result + SanitizeSegment(seg, repl);
  end;

begin
  if not aHonorSeparators then
    Result := SanitizeSegment(aRawName, aReplacement)
  else
    Result := SanitizeFullPath(aRawName, aReplacement);
end;

function ConvertToValidDirectoryName(const aText: string; aReplaceInvalidCharsWith: char): string;
begin
  // Simple alias for backward compatibility
  Result := SanitizeFileName(aText, aReplaceInvalidCharsWith);
end;

function LongFileNameFix(const aFileName: string): string;
const
  MaxPathLength = 260;
  LocalPrefix = '\\?\';
  NetworkPrefix = '\\?\UNC\';
var
  IsNetworkPath: boolean;
begin
  Result := aFileName;
  {$IFDEF MSWINDOWS}
  // do we really have to use the long path format?
  if length(Result) < MaxPathLength then
    exit;

  // Check if the filename is already prefixed
  if startsText(LocalPrefix, Result) or startsText(NetworkPrefix, Result) then
    exit;

  IsNetworkPath := startsText('\\', Result);
  if IsNetworkPath then
    // Remove the leading '\\' for network paths to correctly append the UNC prefix
    Result := copy(Result, 3, length(Result));

  if IsNetworkPath then
    Result := NetworkPrefix + Result
  else
    Result := LocalPrefix + Result;

  {$ENDIF}
end;

function UnSlash(const aPath: string): string;
begin
  if (aPath <> '') and (aPath[length(aPath)] = TPath.DirectorySeparatorChar) then
    Result := copy(aPath, 1, length(aPath) - 1)
  else
    Result := aPath;
end;

function slash(const aPath: string): string;
begin
  if (aPath <> '') and (aPath[length(aPath)] <> TPath.DirectorySeparatorChar) then
    Result := aPath + TPath.DirectorySeparatorChar
  else
    Result := aPath;
end;

procedure SetEnvironmentPath(const aPath: string);
const
  cName = 'PATH';
var
  lEnvVarValue: string;
  lPath: string;
begin
  lPath := ExpandFileName(aPath);

  lEnvVarValue := System.SysUtils.GetEnvironmentVariable(cName);

  // Remove trailing semicolons
  while endsText(';', lEnvVarValue) do
    SetLength(lEnvVarValue, length(lEnvVarValue) - 1);

  // Append new path
  lEnvVarValue := lEnvVarValue + ';' + lPath;

  // Use the appropriate function to set the environment variable
  {$IFDEF MSWINDOWS}
  if not winApi.Windows.SetEnvironmentVariable(PChar(cName), PChar(lEnvVarValue)) then
    raise Exception.Create('Failed to set environment variable.');
  {$ELSEIF DEFINED(POSIX)}
  if setenv(pAnsiChar(AnsiString(cName)), pAnsiChar(AnsiString(lEnvVarValue)), 1) <> 0 then
    raise Exception.Create('Failed to set environment variable.');
  {$ELSE}
  {$MESSAGE ERROR 'Not implemented for this platform!'}
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}

function FilePathToURL(const aFilePath: string): string;
var
  lBufferLen: dword;
begin
  lBufferLen := INTERNET_MAX_URL_LENGTH;
  SetLength(Result, lBufferLen);
  OleCheck(UrlCreateFromPath(PChar(aFilePath), PChar(Result), @lBufferLen, 0));
  SetLength(Result, lBufferLen);
end;
{$ELSE}

function FilePathToURL(const aFilePath: string): string;
var
  lLen: integer;

  procedure append(const s: string);
  begin
    if length(Result) <= lLen + length(s) then
      SetLength(Result, (lLen + length(s)) * 2);
    for var X := 1 to length(s) do
      Result[lLen + X] := s[X];
    Inc(lLen, length(s));
  end;

  function makeHex(const c: char): string;
  begin
    Result := '%' + IntTohex(Ord(c));
  end;

begin
  Result := 'file:///';
  lLen := length(Result);
  SetLength(Result, length(aFilePath) * 3);
  for var X := 1 to length(aFilePath) do
  begin
    if CharInSet(aFilePath[X], ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '/', ',', '.', ':', '!', '@']) then
      append(aFilePath[X])
    else if aFilePath[X] = '\' then
      append('/')
    else
      append(makeHex(aFilePath[X]));
  end;
  SetLength(Result, lLen); // truncate
end;
{$ENDIF}

function NormalizePath(const aPath: string): string;
var
  badSlash, goodSlash: char;
  s: string;
begin
  s := aPath;
  goodSlash := TPath.DirectorySeparatorChar;

  if goodSlash = '/' then
    s := Stringreplace(s, '\', goodSlash, [rfReplaceAll])
  else
    s := Stringreplace(s, '/', goodSlash, [rfReplaceAll]);
  s := Stringreplace(s, goodSlash + goodSlash, goodSlash, [rfReplaceAll]);

  Result := s;
end;

function GetAnsiFileName(const aLongPath: string): string;
var
  AnsiBuffer: array[0..max_path] of AnsiChar;
begin
  {$IFNDEF MsWIndows}
  Result := aLongPath;
  {$ELSE}
  WideCharToMultiByte(CP_ACP, 0, pWideChar(aLongPath), -1, AnsiBuffer, max_path, nil, nil);
  Result := string(AnsiBuffer);
  {$ENDIF}
end;

function GetShortPath(const aLongPath: string): string;
{$IFDEF MSWINDOWS}
var
  Buffer: array[0..max_path] of char;
  {$ENDIF}
begin
  {$IFNDEF MsWIndows}
  Result := aLongPath;
  {$ELSE}
  // Attempt to get the short path name
  if GetShortPathName(PChar(aLongPath), Buffer, max_path) > 0 then
    Result := Buffer
  else
    Result := aLongPath; // Fallback if conversion fails
  {$ENDIF}
end;

function QuoteShellArgument(const aValue: string): string;
begin
  {$IFDEF MSWINDOWS}
  Result := QuoteWindowsShellArgument(aValue);
  {$ELSE}
  Result := QuoteUnixShellArgument(aValue);
  {$ENDIF}
end;

function QuoteWindowsShellArgument(const aValue: string): string;
var
  lNeedsQuoting: boolean;
  i: integer;
  lBuilder: TStringBuilder;
  lBackslashCount: integer;
begin
  // Check if we need to quote the string at all
  lNeedsQuoting := False;
  for i := 1 to length(aValue) do
  begin
    if CharInSet(aValue[i], [' ', #9, #10, #13, '&', '|', '(', ')', '<', '>', '^', '"', '''']) then
    begin
      lNeedsQuoting := True;
      break;
    end;
  end;

  if not lNeedsQuoting and (aValue <> '') then
    exit(aValue);

  Result := QuotedStr(aValue);
end;

function QuoteUnixShellArgument(const aValue: string): string;
var
  lNeedsQuoting: boolean;
  i: integer;
  c: char;
  lBuilder: TStringBuilder;
begin
  // Check if we need to quote the string at all
  lNeedsQuoting := False;
  for i := 1 to length(aValue) do
  begin
    c := aValue[i];
    if CharInSet(c, [' ', #9, #10, #13, ';', '&', '|', '<', '>', '(', ')', '$', '`', '\', '"', '''', #0]) then
    begin
      lNeedsQuoting := True;
      break;
    end;
  end;

  if not lNeedsQuoting then
    exit(aValue);

  // Create string builder with extra capacity for escaping
  gc(lBuilder, TStringBuilder.Create(length(aValue) * 2 + 2));

  // Use single quotes for bash - simplest and safest escape method for most cases
  lBuilder.append('''');

  // Special handling for single quotes - bash can't escape within single quotes
  // The technique is to close the quote, insert the quote with different quoting,
  // then reopen the single quote
  for i := 1 to length(aValue) do
  begin
    c := aValue[i];
    if c = '''' then
      lBuilder.append('''\''''') // Close quote, escaped quote, reopen quote
    else
      lBuilder.append(c);
  end;

  lBuilder.append('''');
  Result := lBuilder.ToString;
end;

function GetLocalAppDataPath: string;
begin
  {$IF Defined(MSWINDOWS)}
  Result := GetEnvironmentVariable('AppData');
  {$ELSEIF Defined(MACOS)}
  Result := TPath.Combine(TPath.GetHomePath, 'Library/Application Support');
  {$ELSEIF Defined(LINUX)}
  Result := GetEnvironmentVariable('XDG_CONFIG_HOME');
  if Result.IsEmpty then
    Result := TPath.Combine(TPath.GetHomePath, '.config');
  {$ELSEIF Defined(ANDROID)}
  Result := TPath.GetDocumentsPath; // or use GetSharedDownloadsPath for shared app data
  {$ELSE}
  raise Exception.Create('Unsupported platform for GetLocalAppDataPath');
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
function GetSpecialWindowsFolder(aFolder: integer): string;
var
  lPath: array[0..max_path] of char;
begin
  if SHGetFolderPath(0, aFolder, 0, 0, @lPath[0]) = S_OK then
    Result := lPath
  else
    Result := '';
end;
{$ENDIF}

function IsInPath(const aPath, aFileName: string): boolean;
var
  fn, lPath: string;
begin
  lPath := IncludeTrailingPathDelimiter(ExpandUNCFileName(NormalizePath(aPath)));
  fn := ExpandUNCFileName(NormalizePath(aFileName));
  Result := SameText(copy(fn, 1, length(lPath)), lPath)
end;

function GetFileDate(const aFileName: string): TDateTime;
begin
  if TFile.Exists(aFileName) then
    Result := TFile.GetLastWriteTime(aFileName)
  else
    Result := 0;
end;

function GetFileDate(out aCreateTime, aLastAccess, aLastWriteTime: TDateTime; const aFileName: string): boolean;
begin
  if not TFile.Exists(aFileName) then
    exit(False);
  aCreateTime := TFile.GetCreationTime(aFileName);
  aLastAccess := TFile.GetLastAccessTime(aFileName);
  aLastWriteTime := TFile.GetLastWriteTime(aFileName);
  Result := True;
end;

function LoadStrFromFile(const aFileName: string): string;
var
  lBytes: TBytes;
  lEncoding: TEncoding;
  lOffset: integer;
begin
  if not TFile.Exists(aFileName) then
    exit('');

  lBytes := TFile.ReadAllBytes(aFileName);

  if length(lBytes) = 0 then
    exit('');

  // 1) Let RTL detect BOM and give us some encoding as a starting point
  lEncoding := nil;
  lOffset := TEncoding.GetBufferEncoding(lBytes, lEncoding);
  // With ADefaultEncoding=nil this falls back to TEncoding.Default when no BOM

  // 2) No BOM: decide between UTF-8 and Default using IsBufferValid
  if lOffset = 0 then
  begin
    if TEncoding.UTF8.IsBufferValid(lBytes) then
    begin
      // BOM-less, but valid UTF-8 → treat as UTF-8
      lEncoding := TEncoding.UTF8;
      // lOffset stays 0 (no BOM bytes to skip)
    end else begin
      // Not valid UTF-8 → fall back to whatever GetBufferEncoding chose
      // (normally TEncoding.Default). Just in case, guarantee non-nil:
      if lEncoding = nil then
        lEncoding := TEncoding.default;
    end;
  end;

  Result := lEncoding.GetString(lBytes, lOffset, length(lBytes) - lOffset);
end;

function SamePath(const aPath1, aPath2: string): boolean;
var
  lP1, lP2: string;
begin
  lP1 := aPath1.Trim;
  lP2 := aPath2.Trim;

  // Treat two empty/invalid-normalized paths as equal, anything else as unequal
  if (lP1 = '') or (lP2 = '') then
    exit(lP1 = lP2); // true if both are empty, false otherwise

  lP1 := slash(TPath.GetFullPath(NormalizePath(lP1)));
  lP2 := slash(TPath.GetFullPath(NormalizePath(lP2)));

  // SameFileName uses AnsiCompareFileName under the hood:
  // - case-insensitive on Windows
  // - case-sensitive on macOS / (effectively) POSIX
  Result := SameFileName(lP1, lP2);
end;

{$IFDEF MSWINDOWS}
function GetWindowsFolder(const aCsidl: integer): string;
const
  SHGFP_TYPE_CURRENT = 0;
var
  lPath: array[0..max_path] of char;
begin
  Result := '';

  if Succeeded(SHGetFolderPath(0, aCsidl, 0, SHGFP_TYPE_CURRENT, @lPath[0])) then
  begin
    // SHGetFolderPath is Unicode-aware in modern Delphi; cast is fine
    Result := IncludeTrailingPathDelimiter(string(lPath));
  end;
end;
{$ENDIF}

function GetSpecialFolderPath(FolderKind: TSpecialFolderKind): string;
begin
  case FolderKind of
    // *** Cross-platform, via RTL ***

    // User's documents folder on all platforms
    // (user/application-agnostic on desktop, app-sandboxed on mobile) :contentReference[oaicite:0]{index=0}
    sfkCurUser_My_Documents:
      Result := IncludeTrailingPathDelimiter(TPath.GetDocumentsPath);

    {$IFDEF MSWINDOWS}
    // *** Windows-specific semantics, preserved ***

    // C:\ProgramData (CSIDL_COMMON_APPDATA) :contentReference[oaicite:1]{index=1}
    sfkAllUsers_Application_Data:
      Result := GetWindowsFolder(CSIDL_COMMON_APPDATA);

    // C:\Users\<user>\AppData\Local (CSIDL_LOCAL_APPDATA)
    sfkUserSpecific_Application_Data:
      Result := GetWindowsFolder(CSIDL_LOCAL_APPDATA);

    // C:\Program Files (CSIDL_PROGRAM_FILES)
    sfkProgramFiles:
      Result := GetWindowsFolder(CSIDL_PROGRAM_FILES);

    // Public Documents (CSIDL_COMMON_DOCUMENTS)
    sfkAllUsers_Documents:
      Result := GetWindowsFolder(CSIDL_COMMON_DOCUMENTS);

    // Physical desktop folder (CSIDL_DESKTOPDIRECTORY)
    sfkDesktop:
      Result := GetWindowsFolder(CSIDL_DESKTOPDIRECTORY);

    // “My Computer” – virtual shell folder, keep your GUID string
    sfkDrives:
      Result := '::{20D04FE0-3AEA-1069-A2D8-08002B30309D}';
    {$ELSE}
    // *** Non-Windows best-effort mappings using TPath ***

    // some shared, app-agnostic data for all users/apps on the box :contentReference[oaicite:2]{index=2}
    sfkAllUsers_Application_Data:
      Result := IncludeTrailingPathDelimiter(TPath.GetPublicPath);

    // per-user app data/settings; on non-Windows this is the correct API :contentReference[oaicite:3]{index=3}
    sfkUserSpecific_Application_Data:
      Result := IncludeTrailingPathDelimiter(TPath.GetHomePath);

    // generic “where apps live / libraries” – not 1:1 with Program Files, but portable
    sfkProgramFiles:
      Result := IncludeTrailingPathDelimiter(TPath.GetLibraryPath);

    // shared documents between users/apps :contentReference[oaicite:4]{index=4}
    sfkAllUsers_Documents:
      Result := IncludeTrailingPathDelimiter(TPath.GetSharedDocumentsPath);

    {$ENDIF}
  else
    raise EArgumentOutOfRangeException.Create('GetSpecialFolderPath: Unknown TSpecialFolderKind');
  end;
end;

procedure writeToBytesAndIncOffset(const Buf; BuffSize: integer; var Bytes: TBytes; var offset: integer);
begin
  writeToBytes(Buf, BuffSize, Bytes, offset);
  Inc(offset, BuffSize);
end;

procedure writeToBytes(const Buf; BufSize: integer; var Bytes: TBytes; offset: integer);
var
  reqSize: integer;
begin
  reqSize := offset + BufSize;
  if length(Bytes) < reqSize then
    SetLength(Bytes, reqSize);
  move(Buf, Bytes[offset], BufSize);
end;

procedure readFromBytesAndIncOffset(var Buf; BuffSize: integer; const Bytes: TBytes; var offset: integer);
begin
  readFromBytes(Buf, BuffSize, Bytes, offset);
  Inc(offset, BuffSize);
end;

procedure readFromBytes(var Buf; BufSize: integer; const Bytes: TBytes; offset: integer);
begin
  move(Bytes[offset], Buf, BufSize);
end;

procedure FillZeroBytes(var Bytes: TBytes);
begin
  if length(Bytes) <> 0 then
    ZeroMemory(@Bytes[0], length(Bytes));
end;


end.

