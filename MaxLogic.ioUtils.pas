Unit MaxLogic.ioUtils;

{
  work in progress: for now only some methods exported from old big the pawel1.pas
  Version: 0.20
}

{$IF DEFINED(FRAMEWORK_VCL) OR DEFINED(FRAMEWORK_FMXs)}
{$IFNDEF CONSOLE}
{$DEFINE CanUseApplicationInstance}
{$ENDIF}
{$IFEND}

Interface

Uses
  {$IFDEF MadExcept}MadExcept, {$ENDIF}
  {$IF DEFINED( MsWINDOWS)}
  winApi.windows, System.Win.ComObj, Winapi.WinInet, Winapi.ShLwApi,
  maxConsoleRunner,
  {$ELSEIF DEFINED(POSIX)}
  Posix.Stdlib,
  Posix.Unistd,
  Posix.Dlfcn,
  {$IFEND}
  system.classes, system.sysUtils, generics.Collections, generics.Defaults;

Function GetCurrentDLLName: String;
{$IFDEF MSWINDOWS}
Function GetBuildInfo: String; Overload;
Function GetBuildInfo(Const Filename: String; Out v1, v2, v3, v4: word): String; Overload;
Function GetVersionString(Const Prefix: String = ' v'): String;
// as GetBuildInfo but returns only the last number which is responsible for the build version or build number
Function GetBuildNumber: word; Overload;
Function GetBuildNumber(Const Filename: String): word; Overload;
{$ENDIF}

{$IFDEF MSWINDOWS}
{ Default Encoding is utf8
  include in tc file like that:
  <ResName>RCDATA <FileName>
}
Function LoatStringFromResource(Const aResName: String; Out aValue: String; Const aDefault: String = ''; aEncoding: TEncoding = Nil): Boolean; Overload;
// Default is ''
Function LoatStringFromResource(Const aResName: String; aEncoding: TEncoding = Nil): String; Overload;

{$ENDIF}

// works also for dll files
Function GetInstallDir: String;

{$IFDEF MsWindows}
Procedure Exec(Const Filename: String; Const Parameter: String = ''; StartDir: String = '');

Procedure ExecuteFile(Const aFileName: String;
  Const AParameter, ACurrentDir: String; AWait: Boolean;
  aRunHidden: Boolean = False); Overload;
Procedure ExecuteFile(Const Cmd, ACurrentDir: String; AWait: Boolean;
  aRunHidden: Boolean = False); Overload;

type
  TStrProc = maxConsoleRunner.TDataReadyProc;

// similar to the above, but now we also get the stdOut and errOut and the exit code
Procedure ExecuteFile(Const Cmd, ACurrentDir: String;
  out aExitCode: Integer;
  aOnStdOut: TStrProc = nil;
  aOnErrOut: TStrProc = nil;
  aRunHidden: Boolean = True); Overload;

{$ENDIF}

// default encoding is utf8
// aWaitBetweenRetries is in milliseconds
// returns true if the write was successful false otherwise
Function SafeAppendToFile(const aFileName, aText: String; aRetryCount: Integer = 5; aWaitBetweenRetries: Integer = 10; aEncoding: TEncoding = nil): Boolean;

{$IFDEF MSWINDOWS}
function RecycleItem(CONST ItemName: string; CONST DeleteToRecycle: Boolean = True; CONST ShowConfirm: Boolean = True; CONST TotalSilence: Boolean = False): Boolean;
{$ENDIF}

Function CombinePath(const aParts: array of String; aAddFinalTrailingPathDelimiter: Boolean = False): String;
function ConvertToValidDirectoryName(const aText: string; aReplaceInvalidCharsWith: Char = '_'): string;

/// <summary>
/// This function checks the provided filename to determine if it exceeds the Windows path length limit. It then appropriately prefixes the filename to support extended-length paths. The function also distinguishes between local and network paths, applying a different prefix based on the path type. Note that for network paths, the initial double backslashes (`\\`) are replaced with the `\\?\UNC\` prefix, while local paths simply receive the `\\?\` prefix.
/// Remember, even with these prefixes, some Windows APIs and applications may not support extended-length paths or may require certain group policy settings to be enabled for support.
/// on non windows systems the result is the same as the input
/// </summary>
function LongFileNameFix(const aFileName: String): String;

// ensures the last char is TPath.DirectorySeparatorChar
function Slash(const aPath: String): String;
function UnSlash(const aPath: String): String;

// NOTE: Environment Changes Scope: As on Windows, changes made by setenv in this way only affect the current process and its child processes. They do not affect the system-wide environment or persist after the application terminates.
procedure SetEnvironmentPath(const aPath: String);
function FilePathToURL(const aFilePath: string): String;

// for windows it will exchange "/" with "\"
// it will also replace "//" with "/" with a single /
Function NormalizePath(Const aPath: String): String;

// This forces the path into a system-compatible ANSI encoding.
// on non windows OS just returns the input
function GetAnsiFileName(const aLongPath: string): String;
// ATTENTION: shortens the path only on windows, on other systems it just returns the input
function GetShortPath(const aLongPath: string): String;



/// <summary>
/// Properly quotes a string for use as a command-line argument in the current platform's shell.
/// Automatically selects the appropriate quoting method based on the platform (Windows or Unix/Linux).
/// </summary>
/// <param name="aValue">The string to be quoted for shell use</param>
/// <returns>A properly quoted string safe to use as a shell command argument</returns>
function QuoteShellArgument(const aValue: String): String;

/// <summary>
/// Properly quotes a string for use in Unix/Linux shell commands using single quotes.
/// Handles special characters and escapes embedded single quotes according to bash conventions.
/// Example: test\'1 becomes 'test'\''1' which is the proper Linux way to escape single quotes.
/// </summary>
/// <param name="aValue">The string to be quoted for Unix shell use</param>
/// <returns>A properly quoted string safe to use in bash/sh command lines</returns>
function QuoteUnixShellArgument(const aValue: String): String;

/// <summary>
/// Properly quotes a string for use in Windows command shell (cmd.exe).
/// Handles Windows' complex escaping rules, with special treatment for:
/// - Backslashes before quotes (doubled)
/// - Double quotes (escaped with backslashes)
/// - Spaces and other special characters
/// </summary>
/// <param name="aValue">The string to be quoted for Windows shell use</param>
/// <returns>A properly quoted string safe to use in Windows command lines</returns>
function QuoteWindowsShellArgument(const aValue: String): String;

Implementation

Uses
  {$IFDEF MSWINDOWS}
  shellApi,
  {$ENDIF}
  {$IFDEF CanUseApplicationInstance}
  forms,
  {$ENDIF}
  system.ioUtils, system.strUtils, autoFree;

{$IFDEF MSWINDOWS}


Function LoatStringFromResource(Const aResName: String; Out aValue: String; Const aDefault: String = ''; aEncoding: TEncoding = Nil): Boolean;
Var
  RS: TResourceStream;
  BYTES: TBytes;
Begin
  aValue := aDefault;
  Result := False;
  If aEncoding = Nil Then
    aEncoding := TEncoding.UTF8;

  RS := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  Try
    If RS.size > 0 Then
    Begin
      SetLength(BYTES, RS.size);
      RS.readbuffer(BYTES[0], RS.size);
      aValue := aEncoding.GetString(BYTES);
      Result := True;
    End;
  Finally
    RS.Free;
  End;
End;
{$ENDIF}

{$IFDEF MSWINDOWS}


Function LoatStringFromResource(Const aResName: String; aEncoding: TEncoding = Nil): String;
Begin
  If Not LoatStringFromResource(aResName, Result) Then
    Result := '';
End;

{$ENDIF}

{$IFDEF MSWINDOWS}


Function GetVersionString(Const Prefix: String = ' v'): String;
Var
  s: String;
Begin
  s := GetBuildInfo;
  If s = '' Then
    Result := ''
  Else
    Result := { Application.Title + } Prefix + s;
End;
{$ENDIF}

{$IFDEF MSWINDOWS}


Function GetBuildNumber: word;
Begin
  Result := GetBuildNumber(GetCurrentDLLName);
End;
{$ENDIF}

{$IFDEF MSWINDOWS}


Function GetBuildNumber(Const Filename: String): word;
Var
  v1, v2, v3: word;
Begin
  GetBuildInfo(Filename, v1, v2, v3, Result);
End;
{$ENDIF}

{$IFDEF MSWINDOWS}


Function GetBuildInfo: String;
Var
  v1, v2, v3, v4: word;
Begin
  v1 := 0;
  v2 := 0;
  v3 := 0;
  v4 := 0;
  Try
    Result := GetBuildInfo(GetCurrentDLLName(), v1, v2, v3, v4);
  Except
    Result := '';
  End;
End;
{$ENDIF}

{$IFDEF MSWINDOWS}


Function GetBuildInfo(Const Filename: String; Out v1, v2, v3, v4: word): String;
Var
  VerInfoSize: Cardinal;
  VerInfo: Pointer;
  VerValueSize: Cardinal;
  VerValue: PVSFixedFileInfo;
  Dummy: Cardinal;
Begin
  VerInfoSize := GetFileVersionInfoSize(PChar(Filename), Dummy);
  If VerInfoSize = 0 Then
  Begin
    Dummy := GetLastError;

    Result := '';
    Exit;
  End; { if }
  getmem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(Filename), 0, VerInfoSize, VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  With VerValue^ Do
  Begin
    v1 := dwFileVersionMS Shr 16;
    v2 := dwFileVersionMS And $FFFF;
    v3 := dwFileVersionLS Shr 16;
    v4 := dwFileVersionLS And $FFFF;
  End;
  freemem(VerInfo, VerInfoSize);
  Result := IntToStr(v1) + '.' + IntToStr(v2) + '.' + IntToStr(v3) + '.' +
    IntToStr(v4);
End;
{$ENDIF}


Function GetCurrentDLLName: String;
Var
  s: String;
  i: Integer;
Begin
  SetLength(s, max_path);
  i := GetModuleFileName(HInstance, @s[1], max_path);
  If i > 0 Then
  Begin
    SetLength(s, i);
    Result := s;
  End
  Else
    Result := '';
End;

Function GetInstallDir: String;
Begin
  Result := GetCurrentDLLName;
  Result := ExtractFilePath(Result);
End;

{$IFDEF MsWindows}


Procedure Exec(Const Filename: String; Const Parameter: String = ''; StartDir: String = '');
Begin
  If StartDir = '' Then
    StartDir := ExtractFilePath(Filename);
  ShellExecute(0, Nil, PChar(Filename), PChar(Parameter), PChar(StartDir),
    SW_NORMAL);
End;
{$ENDIF}

{$IFDEF MsWindows}


Procedure ExecuteFile(Const Cmd, ACurrentDir: String;
  out aExitCode: Integer;
  aOnStdOut: TStrProc = nil;
  aOnErrOut: TStrProc = nil;
  aRunHidden: Boolean = True);
var
  lRunner: TmaxConsoleRunner;
begin
  lRunner := TmaxConsoleRunner.Create;
  try
    lRunner.DecodeCommand(Cmd);
    lRunner.OnStdDataRead := aOnStdOut;
    lRunner.OnErrorDataRead := aOnErrOut;
    lRunner.RunHidden := aRunHidden;

    lRunner.execute;
    aExitCode := lRunner.ExitCode;
  finally
    lRunner.Free;
  end;
end;

Procedure ExecuteFile(
  Const aFileName: String;
  Const AParameter, ACurrentDir: String; AWait: Boolean;
  aRunHidden: Boolean = False);
Var
  Cmd: String;
  dir: String;
Begin
  dir := ACurrentDir;
  If dir = '' Then
    dir := ExtractFilePath(aFileName);

  Cmd := aFileName;
  If (Cmd <> '') And (Cmd[1] <> '"') Then
    Cmd := '"' + Cmd + '"';

  If AParameter <> '' Then
    Cmd := Cmd + ' ' + AParameter;

  ExecuteFile(Cmd, dir, AWait, aRunHidden);
End;
{$ENDIF}
{$IFDEF MsWindows}


Procedure ExecuteFile(
  Const Cmd, ACurrentDir: String; AWait: Boolean;
  aRunHidden: Boolean = False); Overload;
Var
  si: TStartupInfo;
  pi: TProcessInformation;
  lCmd, lDir: String;
Begin
  lDir := ACurrentDir;

  ZeroMemory(@si, SizeOf(si));
  With si Do
  Begin
    cb := SizeOf(si);
    dwFlags := STARTF_USESHOWWINDOW;
    If aRunHidden Then
      wShowWindow := SW_HIDE
    Else
      wShowWindow := SW_NORMAL;
  End;

  ZeroMemory(@pi, SizeOf(pi));

  lCmd := Cmd;
  UniqueString(lCmd);

  If CreateProcess(Nil, PChar(lCmd), Nil, Nil, False,
    CREATE_DEFAULT_ERROR_MODE Or CREATE_NEW_CONSOLE Or NORMAL_PRIORITY_CLASS,
    Nil, PChar(lDir), si, pi) Then
  Begin
    Try
      If AWait Then
      Begin
        {$IFDEF madExcept}MadExcept.PauseFreezeCheck(True);
        {$ENDIF}
        WaitForSingleObject(pi.hProcess, INFINITE);
        {$IFDEF madExcept}MadExcept.PauseFreezeCheck(False);
        {$ENDIF}
      End;
    Finally
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    End;
  End;
End;
{$ENDIF}

{$IFDEF MSWINDOWS}


function RecycleItem(CONST ItemName: string; CONST DeleteToRecycle: Boolean = True; CONST ShowConfirm: Boolean = True; CONST TotalSilence: Boolean = False): Boolean;
VAR
  SHFileOpStruct: TSHFileOpStruct;
begin
  FillChar(SHFileOpStruct, SizeOf(SHFileOpStruct), #0);
  {$IFDEF CanUseApplicationInstance}
  if Assigned(Application) and Assigned(Application.MainForm) then
    SHFileOpStruct.wnd := Application.MainForm.Handle { Others are using 0. But Application.MainForm.Handle is better because otherwise, the 'Are you sure you want to delete' will be hidden under program's window }
  else
    {$ENDIF}
    SHFileOpStruct.wnd := 0;

  SHFileOpStruct.wFunc := FO_DELETE;
  SHFileOpStruct.pFrom := PChar(ItemName + #0);
  SHFileOpStruct.pTo := NIL;
  SHFileOpStruct.hNameMappings := NIL;

  if DeleteToRecycle
  then
    SHFileOpStruct.fFlags := SHFileOpStruct.fFlags OR FOF_ALLOWUNDO;

  if TotalSilence
  then
    SHFileOpStruct.fFlags := SHFileOpStruct.fFlags OR FOF_NO_UI
  else
    if NOT ShowConfirm
  then
    SHFileOpStruct.fFlags := SHFileOpStruct.fFlags OR FOF_NOCONFIRMATION;

  Result := SHFileOperation(SHFileOpStruct) = 0;
end;
{$ENDIF}


Function SafeAppendToFile(const aFileName, aText: String; aRetryCount: Integer = 5; aWaitBetweenRetries: Integer = 10; aEncoding: TEncoding = nil): Boolean;
var
  RetryCounter: Integer;
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
      Exit(True);
    Except
      inc(RetryCounter);
      sleep(aWaitBetweenRetries);
    end;
  until RetryCounter >= aRetryCount;
end;

Function CombinePath(const aParts: array of String; aAddFinalTrailingPathDelimiter: Boolean = False): String;
var
  x: Integer;
begin
  if Length(aParts) = 0 Then
    Exit('');

  Result := aParts[0];
  for x := 1 to Length(aParts) - 1 do
    Result := TPath.combine(Result, aParts[x]);
  if aAddFinalTrailingPathDelimiter then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function ConvertToValidDirectoryName(const aText: string; aReplaceInvalidCharsWith: Char = '_'): string;
var
  i: Integer;
begin
  Result := aText;
  for i := Low(Result) to High(Result) do
  begin
    if not TPath.IsValidFileNameChar(Result[i]) then
      Result[i] := aReplaceInvalidCharsWith;
  end;
  Result := Trim(Result);
end;

function LongFileNameFix(const aFileName: String): String;
const
  MaxPathLength = 260;
  LocalPrefix = '\\?\';
  NetworkPrefix = '\\?\UNC\';
var
  IsNetworkPath: Boolean;
begin
  Result := aFileName;
  {$IFDEF MsWindows}
  // do we really have to use the long path format?
  if Length(Result) < MaxPathLength then
    Exit;

  // Check if the filename is already prefixed
  if StartsText(LocalPrefix, Result) or StartsText(NetworkPrefix, Result) then
    Exit;

  IsNetworkPath := StartsText('\\', Result);
  if IsNetworkPath then
    // Remove the leading '\\' for network paths to correctly append the UNC prefix
    Result := Copy(Result, 3, Length(Result));

  if IsNetworkPath then
    Result := NetworkPrefix + Result
  else
    Result := LocalPrefix + Result;

  {$ENDIF}
end;

function UnSlash(const aPath: String): String;
begin
  if (aPath <> '') and (aPath[Length(aPath)] = TPath.DirectorySeparatorChar) then
    Result := Copy(aPath, 1, Length(aPath) - 1)
  else
    Result := aPath;
end;

function Slash(const aPath: String): String;
begin
  if (aPath <> '') and (aPath[Length(aPath)] <> TPath.DirectorySeparatorChar) then
    Result := aPath + TPath.DirectorySeparatorChar
  else
    Result := aPath;
end;

procedure SetEnvironmentPath(const aPath: String);
const
  cName = 'PATH';
var
  lEnvVarValue: String;
  lPath: String;
begin
  lPath := ExpandFileName(aPath);

  lEnvVarValue := system.sysUtils.GetEnvironmentVariable(cName);

  // Remove trailing semicolons
  while EndsText(';', lEnvVarValue) do
    SetLength(lEnvVarValue, Length(lEnvVarValue) - 1);

  // Append new path
  lEnvVarValue := lEnvVarValue + ';' + lPath;

  // Use the appropriate function to set the environment variable
  {$IFDEF MSWINDOWS}
  if not winApi.windows.SetEnvironmentVariable(PChar(cName), PChar(lEnvVarValue)) then
    raise Exception.Create('Failed to set environment variable.');
  {$ELSEIF DEFINED(POSIX)}
  if setenv(PAnsiChar(AnsiString(cName)), PAnsiChar(AnsiString(lEnvVarValue)), 1) <> 0 then
    raise Exception.Create('Failed to set environment variable.');
  {$ELSE}
  {$MESSAGE ERROR 'Not implemented for this platform!'}
  {$ENDIF}
end;

{$IFDEF MsWindows}


function FilePathToURL(const aFilePath: string): string;
var
  lBufferLen: DWORD;
begin
  lBufferLen := INTERNET_MAX_URL_LENGTH;
  SetLength(Result, lBufferLen);
  OleCheck(UrlCreateFromPath(PChar(aFilePath), PChar(Result), @lBufferLen, 0));
  SetLength(Result, lBufferLen);
end;
{$ELSE}


function FilePathToURL(const aFilePath: string): String;
var
  lLen: Integer;
  Procedure Append(const s: String);
  begin
    if Length(Result) <= lLen + Length(s) then
      SetLength(Result, (lLen + Length(s)) * 2);
    for var x := 1 to Length(s) do
      Result[lLen + x] := s[x];
    inc(lLen, Length(s));
  end;
  function makeHex(const c: Char): String;
  begin
    Result := '%' + IntToHex(Ord(c));
  end;

begin
  Result := 'file:///';
  lLen := Length(Result);
  SetLength(Result, Length(aFilePath) * 3);
  for var x := 1 to Length(aFilePath) do
  begin
    if charInSet(aFilePath[x], ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '-', '_', '/', ',', '.', ':', '!', '@']) then
      Append(aFilePath[x])
    else if aFilePath[x] = '\' then
      Append('/')
    else
      Append(makeHex(aFilePath[x]));
  end;
  SetLength(Result, lLen); // truncate
end;
{$ENDIF}

Function NormalizePath(Const aPath: String): String;
var
  badSlash, goodSlash: Char;
  s: String;
Begin
  s:= aPath;
  goodSlash:= TPath.DirectorySeparatorChar;

  if goodSlash='/' then
    s := StringReplace(s, '\', goodSlash, [rfReplaceAll])
  else
    s := StringReplace(s, '/', goodSlash, [rfReplaceAll]);
  s:= StringReplace(s, goodSlash+goodSlash, goodSlash, [rfReplaceAll]);

  Result:= s;
End;

function GetAnsiFileName(const aLongPath: string): string;
var
  AnsiBuffer: array[0..MAX_PATH] of AnsiChar;
begin
  {$IFNDEF MsWIndows}
  Result:= aLongPath;
  {$ELSE}
  WideCharToMultiByte(CP_ACP, 0, PWideChar(aLongPath), -1, AnsiBuffer, MAX_PATH, nil, nil);
  Result := String(AnsiBuffer);
  {$ENDIF}
end;


function GetShortPath(const aLongPath: String): string;
{$IFDEF MsWindows}
var
  Buffer: array[0..MAX_PATH] of Char;
{$ENDIF}
begin
  {$IFNDEF MsWindows}
  Result := aLongPath;
  {$ELSE}
  // Attempt to get the short path name
  if GetShortPathName(PChar(aLongPath), Buffer, MAX_PATH) > 0 then
    Result := Buffer
  else
    Result := aLongPath; // Fallback if conversion fails
  {$ENDIF}
end;



function QuoteShellArgument(const aValue: String): String;
begin
  {$IFDEF MSWINDOWS}
  Result := QuoteWindowsShellArgument(aValue);
  {$ELSE}
  Result := QuoteUnixShellArgument(aValue);
  {$ENDIF}
end;





function QuoteWindowsShellArgument(const aValue: String): String;
var
  lNeedsQuoting: Boolean;
  i: Integer;
  lBuilder: TStringBuilder;
  lBackslashCount: Integer;
begin
  // Check if we need to quote the string at all
  lNeedsQuoting := False;
  for i := 1 to Length(aValue) do
  begin
    if CharInSet(aValue[i], [' ', #9, #10, #13, '&', '|', '(', ')', '<', '>', '^', '"', '''']) then
    begin
      lNeedsQuoting := True;
      Break;
    end;
  end;

  if not lNeedsQuoting and (aValue <> '') then
    Exit(aValue);

  Result:= QuotedStr(aValue);
end;



function QuoteUnixShellArgument(const aValue: String): String;
var
  lNeedsQuoting: Boolean;
  i: Integer;
  c: Char;
  lBuilder: TStringBuilder;
begin
  // Check if we need to quote the string at all
  lNeedsQuoting := False;
  for i := 1 to Length(aValue) do
  begin
    c := aValue[i];
    if CharInSet(c, [' ', #9, #10, #13, ';', '&', '|', '<', '>', '(', ')', '$', '`', '\', '"', '''', #0]) then
    begin
      lNeedsQuoting := True;
      Break;
    end;
  end;

  if not lNeedsQuoting then
    Exit(aValue);

  // Create string builder with extra capacity for escaping
  gc(lBuilder, TStringBuilder.Create(Length(aValue) * 2 + 2));

  // Use single quotes for bash - simplest and safest escape method for most cases
  lBuilder.Append('''');

  // Special handling for single quotes - bash can't escape within single quotes
  // The technique is to close the quote, insert the quote with different quoting,
  // then reopen the single quote
  for i := 1 to Length(aValue) do
  begin
    c := aValue[i];
    if c = '''' then
      lBuilder.Append('''\''''')  // Close quote, escaped quote, reopen quote
    else
      lBuilder.Append(c);
  end;

  lBuilder.Append('''');
  Result := lBuilder.ToString;
end;



End.
