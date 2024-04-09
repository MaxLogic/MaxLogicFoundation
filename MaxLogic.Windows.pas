unit MaxLogic.Windows;

Interface

uses
  winApi.Windows, system.classes, system.sysUtils, system.generics.Collections;

Type
  TSystemUser = record
    Node,
      UserName,
      SID: String;
    rawRow: String; // the sv row as a whole
  end;

Function retriveWindowsUsers: TArray<TSystemUser>;

{ Run a DOS program and retrieve its output dynamically while it is running. }
Function GetDosOutput(CommandLine: String; aWorkDir: String = 'C:\'): String;

Type
  tStrProc = reference To Procedure(Const line: String);
  // Capture console output in [Realtime] and return its char right away
Procedure CaptureConsoleOutput(Const aExeFileName, AParameters: String;
  OnLineReady: tStrProc; Const aWorkingDir: String = '');

function GetFullProcessImageName(APid: DWord): string;

/// <summary>
/// checks if the application is already running
/// if aCheckFullPath is true you need to specify the fill file name including the path
/// if aCheckFullPath is false, then only the filename without the path will betested
/// </summary>
function ProcessIsRunning(const aExeName: String; aCheckFullPath: Boolean): Boolean;

function QueryFullProcessImageName(
  hProcess: THandle;
  dwFlags: DWord;
  lpExeName: PChar;
  out lpdwSize: DWord
  ): BOOL;
stdcall external kernel32 name 'QueryFullProcessImageNameW';

function RunAsAdmin(hWnd: HWND; filename: string; Parameters: string): Boolean;

Implementation

uses
  system.ioUtils, system.strUtils, MaxLogic.ioUtils, autoFree,
  ShellApi, TLHelp32;

Function retriveWindowsUsers: TArray<TSystemUser>;
var
  cmd: String;
  row, l: TStringList;
  NodeIndex, NameIndex, SidIndex, x: Integer;
  u: TSystemUser;
  items: TList<TSystemUser>;
begin
  Result := [];

  gc(l, TStringList.create);
  gc(row, TStringList.create);

  cmd := 'wmic.exe useraccount get name,sid /format:csv';
  l.Text := GetDosOutput(cmd, getInstallDir);

  row.StrictDelimiter := True;
  row.delimiter := ',';
  row.CaseSensitive := false;

  // there are some empty lines there... get rid of them...
  for x := l.count - 1 downto 0 do
    if trim(l[x]) = '' then
      l.delete(x);
  if l.count < 2 then
    exit;

  row.delimitedText := l[0];
  NodeIndex := row.IndexOf('Node');
  NameIndex := row.IndexOf('Name');
  SidIndex := row.IndexOf('SID');
  gc(items, TList<TSystemUser>.create);
  For x := 1 to l.count - 1 do
  begin
    row.delimitedText := l[x];
    u := default (TSystemUser);
    if NameIndex <> -1 then
      u.UserName := row[NameIndex];
    if NodeIndex <> -1 then
      u.Node := row[NodeIndex];
    if SidIndex <> -1 then
      u.SID := row[SidIndex];
    u.rawRow := l[x];
    items.add(u);
  end;
  Result := items.ToArray;
end;

{ Run a DOS program and retrieve its output dynamically while it is running. }

Function GetDosOutput(CommandLine: String; aWorkDir: String = 'C:\'): String;
Var
  SecurityAttributes: TSecurityAttributes;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  pCommandLine: Array [0 .. 256] Of ansiChar;
  BytesRead: Cardinal;
  sCmd, WorkDir: String;
  isOK: Boolean;
Begin
  Result := '';
  With SecurityAttributes Do
  Begin
    nLength := sizeOf(SecurityAttributes);
    bInheritHandle := True;
    lpSecurityDescriptor := Nil;
  End;
  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SecurityAttributes, 0);
  Try

    StartupInfo := Default (TStartupInfo);
    StartupInfo.cb := sizeOf(StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW Or STARTF_USESTDHANDLES;
    StartupInfo.wShowWindow := SW_HIDE;
    StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);

    // don't redirect stdin
    StartupInfo.hStdOutput := StdOutPipeWrite;
    StartupInfo.hStdError := StdOutPipeWrite;

    WorkDir := aWorkDir;

    sCmd := 'cmd.exe /C ' + CommandLine;
    UniqueString(sCmd);
    isOK := CreateProcess(Nil, PChar(sCmd), Nil, Nil,
      True, 0, Nil, PChar(WorkDir), StartupInfo, ProcessInfo);

    closeHandle(StdOutPipeWrite);
    If isOK Then
      Try
        Repeat
          // To read from the pipe, a process uses the read isOK in a call to the ReadFile function.
          // ReadFile returns when one of the following is true:
          // a write operation completes on the write end of the pipe,
          // the number of bytes requested has been read,
          // or an error occurs.
          // When a process uses WriteFile to write to an anonymous pipe, the write operation is not completed until all bytes are
          // written. If the pipe buffer is full before all bytes are written, WriteFile does not return until another process
          // or thread uses ReadFile to make more buffer space available.
          WasOK := Windows.ReadFile(StdOutPipeRead, pCommandLine, 255, BytesRead, Nil);
          If BytesRead > 0 Then
          Begin
            pCommandLine[BytesRead] := #0;
            Result := Result + pCommandLine;
          End;
        Until Not WasOK Or (BytesRead = 0);
        WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      Finally
        closeHandle(ProcessInfo.hThread);
        closeHandle(ProcessInfo.hProcess);
      End;
  Finally
    closeHandle(StdOutPipeRead);
  End;
End;

// Capture console output in [Realtime] and return its char right away

Procedure CaptureConsoleOutput(Const aExeFileName, AParameters: String;
  OnLineReady: tStrProc; Const aWorkingDir: String = '');
Const
  CReadBuffer = 2400;
Var
  sCmd: String;
  SecurityAttributes: TSecurityAttributes;
  hRead: THandle;
  hWrite: THandle;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  pBuffer: Array [0 .. CReadBuffer + 1] Of ansiChar;
  dRead: DWord;
  dRunning: DWord;
  WorkingDir: String;
Begin
  SecurityAttributes.nLength := sizeOf(TSecurityAttributes);
  SecurityAttributes.bInheritHandle := True;
  SecurityAttributes.lpSecurityDescriptor := Nil;

  If CreatePipe(hRead, hWrite, @SecurityAttributes, 0) Then
  Begin
    StartupInfo := Default (TStartupInfo);
    StartupInfo.cb := sizeOf(TStartupInfo);
    StartupInfo.hStdInput := hRead;
    StartupInfo.hStdOutput := hWrite;
    StartupInfo.hStdError := hWrite;
    StartupInfo.dwFlags := STARTF_USESTDHANDLES Or STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := SW_HIDE;

    sCmd := aExeFileName + ' ' + AParameters;
    UniqueString(sCmd);

    WorkingDir := aWorkingDir;
    If WorkingDir = '' Then
      WorkingDir := getInstallDir;

    If CreateProcess(Nil, PChar(sCmd), @SecurityAttributes,
      @SecurityAttributes, True,
      NORMAL_PRIORITY_CLASS,
      Nil, PChar(WorkingDir), StartupInfo, ProcessInfo) Then
    Begin
      Repeat
        dRunning := WaitForSingleObject(ProcessInfo.hProcess, 100);
        // Application.ProcessMessages();
        Repeat
          dRead := 0;
          ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, Nil);
          pBuffer[dRead] := #0;

          // OemToAnsi(pBuffer, pBuffer);
          OnLineReady(String(pBuffer));
        Until (dRead < CReadBuffer);
      Until (dRunning <> WAIT_TIMEOUT);

      closeHandle(ProcessInfo.hProcess);
      closeHandle(ProcessInfo.hThread);
    End;

    closeHandle(hRead);
    closeHandle(hWrite);
  End;
End;

function GetFullProcessImageName(APid: DWord): string;
var
  LBuffer: PChar;
  LSize: DWord;
  LProcess: THandle;
begin
  Result := '';
  LProcess := OpenProcess(PROCESS_QUERY_INFORMATION, false, APid);
  if LProcess <> INVALID_HANDLE_VALUE then
  begin
    try
      LSize := MAX_PATH;
      LBuffer := GetMemory(LSize * sizeOf(Char));
      try
        if QueryFullProcessImageName(LProcess, 0, LBuffer, LSize) then
        begin
          Result := LBuffer;
        end;
      finally
        FreeMemory(LBuffer);
      end;
    finally
      closeHandle(LProcess);
    end;
  end;
end;

function ProcessIsRunning(const aExeName: String; aCheckFullPath: Boolean): Boolean;
var
  LSnapshot: THandle;
  LProcess: TProcessEntry32;
  LExe: string;
begin
  LSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  try
    LProcess.dwSize := sizeOf(LProcess);
    if Process32First(LSnapshot, LProcess) then
    begin
      LExe := ExtractFileName(aExeName);
      repeat
        if SameText(LProcess.szExeFile, LExe)
          and
          ((not aCheckFullPath)
            or SameText(GetFullProcessImageName(LProcess.th32ProcessID), aExeName))
        then
          exit(True);
      until not Process32Next(LSnapshot, LProcess);
    end;
    Result := false;
  finally
    closeHandle(LSnapshot)
  end;
end;

function RunAsAdmin(hWnd: HWND; filename: string; Parameters: string): Boolean;
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
    sei.Wnd := hwnd;
    sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
    sei.lpVerb := PChar('runas');
    sei.lpFile := PChar(Filename); // PAnsiChar;
    if parameters <> '' then
        sei.lpParameters := PChar(parameters); // PAnsiChar;
    sei.nShow := SW_SHOWNORMAL; //Integer;

    Result := ShellExecuteEx(@sei);
end;

end.
