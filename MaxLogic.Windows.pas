unit maxLogic.Windows;

interface

uses
  winApi.Windows, System.Classes, System.SysUtils, System.generics.collections;

type
  TSystemUser = record
    Node,
      Username,
      SID: string;
    rawRow: string; // the sv row as a whole
  end;

function RetrieveWindowsUsers: TArray<TSystemUser>;

{ Run a DOS program and retrieve its output dynamically while it is running. }
function GetDosOutput(CommandLine: string; aWorkDir: string = 'C:\'): string;

type
  TStrProc = reference to procedure(const line: string);
// Capture console output in [Realtime] and return its char right away
procedure CaptureConsoleOutput(const aExeFileName, AParameters: string;
  OnLineReady: TStrProc; const aWorkingDir: string = '');

function GetFullProcessImageName(APid: dword): string;

/// <summary>
/// checks if the application is already running
/// if aCheckFullPath is true you need to specify the fill file name including the path
/// if aCheckFullPath is false, then only the filename without the path will betested
/// </summary>
function ProcessIsRunning(const aExeName: string; aCheckFullPath: boolean): boolean;

// the appExeName can be either just the filename or a full file name, that is path + filename
function CheckCountProcess(const AppExeName: string; ProcessIds: TList<THandle> = nil): integer;

function QueryFullProcessImageName(
  hProcess: THandle;
  dwFlags: dword;
  lpExeName: PChar;
  out lpdwSize: dword
  ): bool;
  stdcall external kernel32 Name 'QueryFullProcessImageNameW';

function RunAsAdmin(hwnd: hwnd; FileName: string; Parameters: string): boolean;

function IsDelphiRunning: boolean;
function ForceForegroundWindow(hwnd: THandle): boolean;

implementation

uses
  System.IOUtils, System.StrUtils, maxLogic.IOUtils, AutoFree,
  ShellApi, TlHelp32, jclSysInfo;

function RetrieveWindowsUsers: TArray<TSystemUser>;
var
  Cmd: string;
  Row, l: TStringList;
  NodeIndex, NameIndex, SidIndex, X: integer;
  u: TSystemUser;
  Items: TList<TSystemUser>;
begin
  Result := [];

  gc(l, TStringList.Create);
  gc(Row, TStringList.Create);

  Cmd := 'wmic.exe useraccount get name,sid /format:csv';
  l.Text := GetDosOutput(Cmd, GetInstallDir);

  Row.StrictDelimiter := True;
  Row.delimiter := ',';
  Row.CaseSensitive := False;

  // there are some empty lines there... get rid of them...
  for X := l.Count - 1 downto 0 do
    if Trim(l[X]) = '' then
      l.delete(X);
  if l.Count < 2 then
    exit;

  Row.delimitedText := l[0];
  NodeIndex := Row.IndexOf('Node');
  NameIndex := Row.IndexOf('Name');
  SidIndex := Row.IndexOf('SID');
  gc(Items, TList<TSystemUser>.Create);
  for X := 1 to l.Count - 1 do
  begin
    Row.delimitedText := l[X];
    u := default(TSystemUser);
    if NameIndex <> -1 then
      u.Username := Row[NameIndex];
    if NodeIndex <> -1 then
      u.Node := Row[NodeIndex];
    if SidIndex <> -1 then
      u.SID := Row[SidIndex];
    u.rawRow := l[X];
    Items.Add(u);
  end;
  Result := Items.ToArray;
end;

{ Run a DOS program and retrieve its output dynamically while it is running. }

function GetDosOutput(CommandLine: string; aWorkDir: string = 'C:\'): string;
var
  SecurityAttributes: TSecurityAttributes;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: boolean;
  pCommandLine: array[0..256] of AnsiChar;
  BytesRead: Cardinal;
  sCmd, WorkDir: string;
  isOk: boolean;
begin
  Result := '';
  with SecurityAttributes do
  begin
    nLength := SizeOf(SecurityAttributes);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;
  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SecurityAttributes, 0);
  try

    StartupInfo := default(TStartupInfo);
    StartupInfo.cb := SizeOf(StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    StartupInfo.wShowWindow := SW_HIDE;
    StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);

    // don't redirect stdin
    StartupInfo.hStdOutput := StdOutPipeWrite;
    StartupInfo.hStdError := StdOutPipeWrite;

    WorkDir := aWorkDir;

    sCmd := 'cmd.exe /C ' + CommandLine;
    UniqueString(sCmd);
    isOk := CreateProcess(nil, PChar(sCmd), nil, nil,
      True, 0, nil, PChar(WorkDir), StartupInfo, ProcessInfo);

    CloseHandle(StdOutPipeWrite);
    if isOk then
    try
      repeat
        // To read from the pipe, a process uses the read isOK in a call to the ReadFile function.
        // ReadFile returns when one of the following is true:
        // a write operation completes on the write end of the pipe,
        // the number of bytes requested has been read,
        // or an error occurs.
        // When a process uses WriteFile to write to an anonymous pipe, the write operation is not completed until all bytes are
        // written. If the pipe buffer is full before all bytes are written, WriteFile does not return until another process
        // or thread uses ReadFile to make more buffer space available.
        WasOK := winApi.Windows.ReadFile(StdOutPipeRead, pCommandLine, 255, BytesRead, nil);
        if BytesRead > 0 then
        begin
          pCommandLine[BytesRead] := #0;
          Result := Result + pCommandLine;
        end;
      until not WasOK or (BytesRead = 0);
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    finally
      CloseHandle(ProcessInfo.hThread);
      CloseHandle(ProcessInfo.hProcess);
    end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
  Result := Trim(Result);
end;

// Capture console output in [Realtime] and return its char right away

procedure CaptureConsoleOutput(const aExeFileName, AParameters: string;
  OnLineReady: TStrProc; const aWorkingDir: string = '');
const
  CReadBuffer = 2400;
var
  sCmd: string;
  SecurityAttributes: TSecurityAttributes;
  hRead: THandle;
  hWrite: THandle;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  pBuffer: array[0..CReadBuffer + 1] of AnsiChar;
  dRead: dword;
  dRunning: dword;
  WorkingDir: string;
begin
  SecurityAttributes.nLength := SizeOf(TSecurityAttributes);
  SecurityAttributes.bInheritHandle := True;
  SecurityAttributes.lpSecurityDescriptor := nil;

  if CreatePipe(hRead, hWrite, @SecurityAttributes, 0) then
  begin
    StartupInfo := default(TStartupInfo);
    StartupInfo.cb := SizeOf(TStartupInfo);
    StartupInfo.hStdInput := hRead;
    StartupInfo.hStdOutput := hWrite;
    StartupInfo.hStdError := hWrite;
    StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := SW_HIDE;

    sCmd := aExeFileName + ' ' + AParameters;
    UniqueString(sCmd);

    WorkingDir := aWorkingDir;
    if WorkingDir = '' then
      WorkingDir := GetInstallDir;

    if CreateProcess(nil, PChar(sCmd), @SecurityAttributes,
      @SecurityAttributes, True,
      NORMAL_PRIORITY_CLASS,
      nil, PChar(WorkingDir), StartupInfo, ProcessInfo) then
    begin
      repeat
        dRunning := WaitForSingleObject(ProcessInfo.hProcess, 100);
        // Application.ProcessMessages();
        repeat
          dRead := 0;
          ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
          pBuffer[dRead] := #0;

          // OemToAnsi(pBuffer, pBuffer);
          OnLineReady(string(pBuffer));
        until (dRead < CReadBuffer);
      until (dRunning <> WAIT_TIMEOUT);

      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end;

    CloseHandle(hRead);
    CloseHandle(hWrite);
  end;
end;

function GetFullProcessImageName(APid: dword): string;
var
  LBuffer: PChar;
  LSize: dword;
  LProcess: THandle;
begin
  Result := '';
  LProcess := OpenProcess(PROCESS_QUERY_INFORMATION, False, APid);
  if LProcess <> INVALID_HANDLE_VALUE then
  begin
    try
      LSize := max_path;
      LBuffer := GetMemory(LSize * SizeOf(char));
      try
        if QueryFullProcessImageName(LProcess, 0, LBuffer, LSize) then
        begin
          Result := LBuffer;
        end;
      finally
        FreeMemory(LBuffer);
      end;
    finally
      CloseHandle(LProcess);
    end;
  end;
end;

function ProcessIsRunning(const aExeName: string; aCheckFullPath: boolean): boolean;
var
  LSnapshot: THandle;
  LProcess: TProcessEntry32;
  LExe: string;
begin
  LSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  try
    LProcess.dwSize := SizeOf(LProcess);
    if Process32First(LSnapshot, LProcess) then
    begin
      LExe := ExtractFileName(aExeName);
      repeat
        if SameText(LProcess.szExeFile, LExe)
          and
          ((not aCheckFullPath)
          or SameText(GetFullProcessImageName(LProcess.th32ProcessID), aExeName)) then
          exit(True);
      until not Process32Next(LSnapshot, LProcess);
    end;
    Result := False;
  finally
    CloseHandle(LSnapshot)
  end;
end;

function RunAsAdmin(hwnd: hwnd; FileName: string; Parameters: string): boolean;
{
  source: https://stackoverflow.com/questions/923350/delphi-prompt-for-uac-elevation-when-needed

  See Step 3: Redesign for UAC Compatibility (UAC)
  http://msdn.microsoft.com/en-us/library/bb756922.aspx

  This code is released into the public domain. No attribution required.
}
var
  sei: TShellExecuteInfo;
begin
  ZeroMemory(@sei, SizeOf(sei));
  sei.cbSize := SizeOf(TShellExecuteInfo);
  sei.wnd := hwnd;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  sei.lpVerb := PChar('runas');
  sei.lpFile := PChar(FileName); // PAnsiChar;
  if Parameters <> '' then
    sei.lpParameters := PChar(Parameters); // PAnsiChar;
  sei.nShow := SW_SHOWNORMAL; // Integer;

  Result := ShellExecuteEx(@sei);
end;

function IsDelphiRunning: boolean;
begin
  Result := FindWindow('TAppBuilder', nil) > 0;
end;

function ForceForegroundWindow(hwnd: THandle): boolean;
begin
  if IsIconic(hwnd) then
    ShowWindow(hwnd, SW_RESTORE);

  SetActiveWindow(hwnd);
  // BringWindowToTop(hWnd);
  SetForegroundWindow(hwnd);

  Result := (GetForegroundWindow = hwnd);
end; { ForceForegroundWindow }

function CheckCountProcess(const AppExeName: string; ProcessIds: TList<THandle> = nil): integer;
var
  FHandle: THandle;
  l: TStringList;
  UseFullPath: boolean;
  fn: string;
  X: integer;
begin
  Result := 0;

  UseFullPath := pos('\', AppExeName) > 0;
  fn := AnsiLowercase(AppExeName);

  l := TStringList.Create;
  jclSysInfo.RunningProcessesList(l, UseFullPath);

  for X := 0 to l.Count - 1 do
    if fn = AnsiLowercase(l[X]) then
    begin
      Result := Result + 1;
      if assigned(ProcessIds) then
        ProcessIds.Add(THandle(l.Objects[X]))
    end;

  l.Free;
end;

end.

