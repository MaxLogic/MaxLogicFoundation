Unit MaxLogic.ioUtils;

{
  work in progress: for now only some methods exported from old big the pawel1.pas
  Version: 0.16

  History:
  2023-03-18: added SafeAppendToFile
  2022-04-12: ported executeFile methods
  2022-04-12: ported Exec method
}

Interface

Uses
  {$IFDEF MadExcept}MadExcept, {$ENDIF}
  {$IFDEF MSWINDOWS}
  windows,
  {$ENDIF}
  classes, sysUtils;

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
{$ENDIF}

// default encoding is utf8
// aWaitBetweenRetries is in milliseconds
// returns true if the write was successful false otherwise
Function SafeAppendToFile(const aFileName, aText: String; aRetryCount: Integer = 5; aWaitBetweenRetries: Integer = 10; aEncoding: TEncoding = nil): Boolean;

{$IFDEF MSWINDOWS}
function RecycleItem(CONST ItemName: string; CONST DeleteToRecycle: Boolean = TRUE; CONST ShowConfirm: Boolean = TRUE; CONST TotalSilence: Boolean = False): Boolean;
{$ENDIF}

Implementation

Uses
  {$IFDEF MSWINDOWS}
  shellApi,
  {$ENDIF}
  {$IFNDEF LINUX}
  forms,
  {$ENDIF}
  ioUtils;
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
      Result := TRUE;
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
    // ShowMessage(IntToStr(Dummy));
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
  pI: TProcessInformation;
  sCmd, dir: String;
Begin
  dir := ACurrentDir;

  ZeroMemory(@si, sizeOf(si));
  With si Do
  Begin
    cb := sizeOf(si);
    dwFlags := STARTF_USESHOWWINDOW;
    If aRunHidden Then
      wShowWindow := SW_HIDE
    Else
      wShowWindow := SW_NORMAL;
  End;

  ZeroMemory(@pI, sizeOf(pI));

  sCmd := Cmd;
  UniqueString(sCmd);

  If CreateProcess(Nil, PChar(sCmd), Nil, Nil, False,
    CREATE_DEFAULT_ERROR_MODE Or CREATE_NEW_CONSOLE Or NORMAL_PRIORITY_CLASS,
    Nil, PChar(dir), si, pI) Then
  Begin
    Try
      If AWait Then
      Begin
        {$IFDEF madExcept}MadExcept.PauseFreezeCheck(TRUE);
        {$ENDIF}
        WaitForSingleObject(pI.hProcess, INFINITE);
        {$IFDEF madExcept}MadExcept.PauseFreezeCheck(False);
        {$ENDIF}
      End;
    Finally
      closeHandle(pI.hProcess);
      closeHandle(pI.hThread);
    End;
  End;
End;
{$ENDIF}

{$IFDEF MSWINDOWS}

function RecycleItem(CONST ItemName: string; CONST DeleteToRecycle: Boolean = TRUE; CONST ShowConfirm: Boolean = TRUE; CONST TotalSilence: Boolean = False): Boolean;
VAR
  SHFileOpStruct: TSHFileOpStruct;
begin
  FillChar(SHFileOpStruct, sizeOf(SHFileOpStruct), #0);
  if assigned(Application) and assigned(Application.MainForm) then
    SHFileOpStruct.wnd := Application.MainForm.Handle { Others are using 0. But Application.MainForm.Handle is better because otherwise, the 'Are you sure you want to delete' will be hidden under program's window }
  else
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
      Exit(TRUE);
    Except
      inc(RetryCounter);
      sleep(aWaitBetweenRetries);
    end;
  until RetryCounter >= aRetryCount;
end;

End.
