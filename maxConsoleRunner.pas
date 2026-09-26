unit maxConsoleRunner;

interface

uses
  Windows, Messages, SysUtils, Classes, maxAsync, diagnostics,
  generics.collections;

const
  cBufferSize = 16 * 1024; // 16kb

type
  TPipeThread = class; // forward declaration

  TDataReadyProc = reference to procedure(const aText: string);

  TmaxConsoleRunner = class
  private
    FExeName: string;
    fWorkDir: string;
    fParamsString: string;


    // pipe handles
    fPipeErrorsRead: THandle;
    fPipeErrorsWrite: THandle;
    fPipeStdRead: THandle;
    fPipeStdWrite: THandle;
    fStdInput: THandle;

    fPipeStdReadThread,
      fPipeErrorReadThread: TPipeThread;

    fProcessInfo: TProcessInformation;
    fSecurityAttributes: TSecurityAttributes;
    fStartupInfo: TStartupInfo;

    // store or delegate the output
    FOnStdDataRead: TDataReadyProc;
    fErrorOutput: string;
    FOnErrorDataRead: TDataReadyProc;
    fStrOutput: string;
    fSignal: iSignal;
    fExitCode: Integer;
    FRedirectErrOutToStdOut: Boolean;
    fRunHidden: Boolean;

    procedure SetexeName(const Value: string);
    procedure SetWorkDir(const Value: string);

    procedure prepareSecurityAttributes;
    function preparePipes: boolean;
    procedure ClosehandleAndZeroIt(var ahandle: THandle);
    procedure prepareStartUpInfo;
    function startProcess: boolean;
    function quote(const s: string): string;
    procedure StartAsyncReadPipes;
    procedure StopReadingPipes;
    procedure WaitForProcessAndPipes;
    procedure SetParamsString(const Value: String);
    procedure PullData;
    procedure SetOnErrorDataRead(const Value: TDataReadyProc);
    procedure SetOnStdDataRead(const Value: TDataReadyProc);
    function GetCmdExeFullPath: string;
    procedure SetRedirectErrOutToStdOut(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    function Execute: boolean;

    //  will parse the command and set the exeFilename and ParamString and WorkDir
    procedure DecodeCommand(const aCmd: String);

    // helper function to execute in one single call
    class function run(const aExeName, aParamsString: string; aOnStdDataReady, aOnErrorDataReady: TDataReadyProc; const aWorkDir: string = ''): boolean;

    // ExeName can also be a *.bat or a *.cmd
    property ExeName: string read FExeName write SetexeName;

    // params must already be properly formated and quoted
    property ParamsString: String read fParamsString write SetParamsString;

    // if work dir is empty, it will be set to the path of ExeFileName
    property WorkDir: string read fWorkDir write SetWorkDir;

    // output properties
    // NOTE: if you specify an OnDataReady event,
    // then the StrOutput and the ErrorOutput strings will remain empty!
    // use the one or the other, not both!
    property StrOutput: string read fStrOutput;
    property OnStdDataRead: TDataReadyProc read FOnStdDataRead write SetOnStdDataRead;
    property ErrorOutput: string read fErrorOutput;
    property OnErrorDataRead: TDataReadyProc read FOnErrorDataRead write SetOnErrorDataRead;

    // if you prefer, you can redirect the Errout to stdout.. This way you end with a single pipe and a single output string
    property RedirectErrOutToStdOut: Boolean read FRedirectErrOutToStdOut write SetRedirectErrOutToStdOut;

    // return code of the executed application
    property ExitCode: Integer read fExitCode;

    // RunHidden is true per default
    property RunHidden: Boolean read fRunHidden write fRunHidden;
  end;

  TPipeThread = class
  strict private
    fData: TList<string>;
  private
    fHandle: THandle;
    fThread: TThread;
    fDone: boolean;
    fParent: TmaxConsoleRunner;
    fEmpty: boolean;
    fClosing: boolean;
    Buffer: array [0 .. cBufferSize + 1] of AnsiChar;

    procedure asyncRead;
    procedure lock;
    procedure unLock;
  public
    constructor Create(aParent: TmaxConsoleRunner);
    destructor Destroy; override;

    procedure StartReading(const aHandle: THandle);
    procedure safeRetrieveOutput(out aOutput: string);
    // stops reading (cancelling a blocked read) and waits for the reader to finish
    procedure DoClosing;
  end;

implementation

uses
  system.strUtils, system.ioUtils, MaxLogic.strUtils, MaxLogic.ioUtils;

const
  // after the process exits, a grandchild may still hold the write ends of our pipes;
  // we read for at most this long before cancelling the readers
  cPipeGraceMs = 5000;
  // not declared in Winapi.Windows (Delphi 12)
  PROC_THREAD_ATTRIBUTE_HANDLE_LIST = $00020002;

type
  TStartupInfoExW = record
    StartupInfo: TStartupInfoW;
    lpAttributeList: PProcThreadAttributeList;
  end;

{ TmaxConsoleRunner }

procedure TmaxConsoleRunner.StartAsyncReadPipes;
begin
  fPipeStdReadThread.StartReading(fPipeStdRead);
  if not RedirectErrOutToStdOut then
    fPipeErrorReadThread.StartReading(fPipeErrorsRead);
end;

procedure TmaxConsoleRunner.StopReadingPipes;
begin
  fPipeStdReadThread.DoClosing;
  fPipeErrorReadThread.DoClosing;
end;

procedure TmaxConsoleRunner.ClosehandleAndZeroIt(var ahandle: THandle);
begin
  if (aHandle <> 0) and (aHandle <> INVALID_HANDLE_VALUE) then
  begin
    closeHandle(aHandle);
    ahandle:= INVALID_HANDLE_VALUE;
  end;
end;

constructor TmaxConsoleRunner.Create;
begin
  inherited Create;
  fRunHidden:= True;
  fSignal := TSignal.Create;
  fPipeStdReadThread := TPipeThread.Create(self);
  fPipeErrorReadThread := TPipeThread.Create(self);

  fPipeStdRead:= INVALID_HANDLE_VALUE;
  fPipeStdWrite:= INVALID_HANDLE_VALUE;
  fPipeErrorsRead:= INVALID_HANDLE_VALUE;
  fPipeErrorsWrite:= INVALID_HANDLE_VALUE;
  fStdInput:= INVALID_HANDLE_VALUE;
end;

procedure TmaxConsoleRunner.DecodeCommand(const aCmd: String);
var
  i: Integer;
begin
  if startsText('"', aCmd) then
  begin
    // a quoted exe name may contain spaces, so it ends at the closing quote
    i:= PosEx('"', aCmd, 2);
    if i = 0 then
      i:= Length(aCmd) + 1;
    FExeName:= Copy(aCmd, 2, i - 2);
    fParamsString:= TrimLeft(Copy(aCmd, i + 1, MaxInt));
  end else begin
    i := pos(' ', aCmd);
    if i>0 then
    begin
      FExeName:= Copy(aCmd, 1, i-1);
      fParamsString:= Copy(aCmd, i+1, Length(aCmd));
    end else begin
      fExeName:= aCmd;
      fParamsString:= '';
    end;
  end;
  fWorkDir:= ExtractFilepath(FExeName);
end;

function TmaxConsoleRunner.GetCmdExeFullPath: string;
var
  SystemDir: array[0..MAX_PATH - 1] of Char;
begin
  // Get the path of the system directory (e.g., C:\Windows\System32)
  if GetSystemDirectory(SystemDir, MAX_PATH) > 0 then
    Result := IncludeTrailingPathDelimiter(SystemDir) + 'cmd.exe'
  else
    RaiseLastOSError; // Raise an error if unable to retrieve the system directory
end;


function TmaxConsoleRunner.startProcess: boolean;
var
  lCmd: string;
  lWorkingDir: string;
  lExt, lExeName: string;
  lStartupInfo: TStartupInfoExW;
  lInherit: array [0 .. 2] of THandle;
  lInheritCount: Integer;
  lAttributesSize: NativeUInt;
begin
  Result := false;
  FillChar(fProcessInfo, sizeOf(TProcessInformation), 0);

  lWorkingDir := self.fWorkDir;
  if lWorkingDir = '' then
    lWorkingDir := ExtractFilePath(Self.fExeName);

  lExeName := self.FExeName;
  lExt:= extractFileExt(lExeName);
  if sameText('.bat', lExt) or sameText('.cmd', lExt)  then
  begin
    lExeName := GetCmdExeFullPath;
    lCmd := trim(quote(lExeName) + ' /C ' + quote(FExeName) +' ' + fParamsString);
  end else
    lCmd := trim(quote(FExeName) + ' ' + fParamsString);

  lCmd := lCmd + #0;
  UniqueString(lCmd);

  lExeName := lExeName + #0;
  UniqueString(lExeName);

  // inherit only the child's std handles, never those of parallel runners or of our host
  lInherit[0] := fStdInput;
  lInherit[1] := fPipeStdWrite;
  lInheritCount := 2;
  if not RedirectErrOutToStdOut then
  begin
    lInherit[2] := fPipeErrorsWrite;
    lInheritCount := 3;
  end;

  lStartupInfo := default (TStartupInfoExW);
  lStartupInfo.StartupInfo := fStartupInfo;
  lStartupInfo.StartupInfo.cb := sizeOf(TStartupInfoExW);
  lAttributesSize := 0;
  InitializeProcThreadAttributeList(nil, 1, 0, lAttributesSize);
  GetMem(lStartupInfo.lpAttributeList, lAttributesSize);
  try
    if not InitializeProcThreadAttributeList(lStartupInfo.lpAttributeList, 1, 0, lAttributesSize) then
      RaiseLastOSError;
    try
      if not UpdateProcThreadAttribute(lStartupInfo.lpAttributeList, 0,
        PROC_THREAD_ATTRIBUTE_HANDLE_LIST, @lInherit, lInheritCount * sizeOf(THandle),
        nil, PNativeUInt(nil)^) then // lpReturnSize is reserved: NULL
        RaiseLastOSError;

      if createProcess(
        PChar(lExeName),
        PChar(lCmd), nil, nil, True,
        NORMAL_PRIORITY_CLASS or EXTENDED_STARTUPINFO_PRESENT,
        nil, Pointer(lWorkingDir), lStartupInfo.StartupInfo, fProcessInfo)
      then
        Result := True
      else begin
        fExitCode := GetLastError; // Return the error code if process creation fails
        RaiseLastOSError(fExitCode);
      end;
    finally
      DeleteProcThreadAttributeList(lStartupInfo.lpAttributeList);
    end;
  finally
    FreeMem(lStartupInfo.lpAttributeList);
  end;
end;

procedure TmaxConsoleRunner.WaitForProcessAndPipes;
var
  lHandles: array [0 .. 1] of THandle;
  lWatch: TStopwatch;
begin
  lHandles[0] := fSignal.GetEvent.Handle;
  lHandles[1] := fProcessInfo.hProcess;
  repeat
    WaitForMultipleObjects(Length(lHandles), @lHandles, False, INFINITE);
    fSignal.SetNonSignaled;
    PullData;
  until WaitForSingleObject(fProcessInfo.hProcess, 0) = WAIT_OBJECT_0;

  // the readers end when all write ends are closed; that normally follows the exit at once
  lWatch := TStopwatch.StartNew;
  while not (fPipeStdReadThread.fDone and fPipeErrorReadThread.fDone)
    and (lWatch.ElapsedMilliseconds < cPipeGraceMs) do
  begin
    fSignal.WaitForSignaled(50);
    fSignal.SetNonSignaled;
    PullData;
  end;
end;

destructor TmaxConsoleRunner.Destroy;
begin
  fPipeStdReadThread.free;
  fPipeErrorReadThread.free;

  inherited;
end;

function TmaxConsoleRunner.execute: boolean;
var
  lExitCode: DWORD;
begin
  Result := false;
  FillChar(fProcessInfo, sizeOf(TProcessInformation), 0);
  try
    prepareSecurityAttributes;
    if not preparePipes then
      RaiseLastOSError;
    prepareStartUpInfo;

    if startProcess then
    begin
      Result := True;
      ClosehandleAndZeroIt(fProcessInfo.hThread);

      // the child owns its copies now; ours would keep the pipes open forever
      ClosehandleAndZeroIt(fPipeStdWrite);
      ClosehandleAndZeroIt(fPipeErrorsWrite);
      ClosehandleAndZeroIt(fStdInput);

      fSignal.SetNonSignaled;
      StartAsyncReadPipes;
      WaitForProcessAndPipes;
      StopReadingPipes;
      PullData;

      if not GetExitCodeProcess(fProcessInfo.hProcess, lExitCode) then
        fExitCode := -1 // Return -1 if exit code retrieval fails
      else
        fExitCode := Integer(lExitCode);
    end;
  finally
    // the readers must stop before their handles close, or they may read a recycled handle
    StopReadingPipes;
    ClosehandleAndZeroIt(fProcessInfo.hProcess);
    ClosehandleAndZeroIt(fProcessInfo.hThread);
    ClosehandleAndZeroIt(fPipeStdRead);
    ClosehandleAndZeroIt(fPipeStdWrite);
    ClosehandleAndZeroIt(fPipeErrorsRead);
    ClosehandleAndZeroIt(fPipeErrorsWrite);
    ClosehandleAndZeroIt(fStdInput);
  end;
end;

var
  lastTick: DWORD = 0;

procedure TmaxConsoleRunner.PullData;
var
  s: String;
begin
  fPipeStdReadThread.safeRetrieveOutput(s);

  if s <> '' then
  begin
    if assigned(FOnStdDataRead) then
      FOnStdDataRead(s)
    else
      fStrOutput := fStrOutput + s;
  end;

  s := '';

  fPipeErrorReadThread.safeRetrieveOutput(s);
  if s <> '' then
  begin
    if assigned(FOnErrorDataRead) then
      FOnErrorDataRead(s)
    else
      fErrorOutput := fErrorOutput + s;
  end;

end;

// on failure, Execute closes whatever was created
function TmaxConsoleRunner.preparePipes: boolean;
begin
  // only the child's write ends may be inherited; an inherited read end keeps the pipe alive
  Result := CreatePipe(fPipeStdRead, fPipeStdWrite, @fSecurityAttributes, cBufferSize)
    and SetHandleInformation(fPipeStdRead, HANDLE_FLAG_INHERIT, 0);

  if Result and not RedirectErrOutToStdOut then
    Result := CreatePipe(fPipeErrorsRead, fPipeErrorsWrite, @fSecurityAttributes, cBufferSize)
      and SetHandleInformation(fPipeErrorsRead, HANDLE_FLAG_INHERIT, 0);

  if Result then
  begin
    // the child reads an empty stdin
    fStdInput := CreateFile('NUL', GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
      @fSecurityAttributes, OPEN_EXISTING, 0, 0);
    Result := fStdInput <> INVALID_HANDLE_VALUE;
  end;
end;

procedure TmaxConsoleRunner.prepareSecurityAttributes;
begin
  FillChar(fSecurityAttributes, sizeOf(TSecurityAttributes), 0);

  fSecurityAttributes.nLength := sizeOf(TSecurityAttributes);
  fSecurityAttributes.bInheritHandle := True;
  fSecurityAttributes.lpSecurityDescriptor := nil;
end;

procedure TmaxConsoleRunner.prepareStartUpInfo;
begin
  fStartupInfo := default (TStartupInfo);

  fStartupInfo.cb := sizeOf(TStartupInfo);
  fStartupInfo.hStdInput := fStdInput;
  fStartupInfo.hStdOutput := fPipeStdWrite;
  if not RedirectErrOutToStdOut then
    fStartupInfo.hStdError := fPipeErrorsWrite
  else
    fStartupInfo.hStdError := fPipeStdWrite;
  if fRunHidden then
    fStartupInfo.wShowWindow := SW_HIDE
  else
    fStartupInfo.wShowWindow := SW_NORMAL;
  fStartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
end;



function TmaxConsoleRunner.quote(const s: string): string;
var
  lNeedsQuotes: Boolean;
begin
  lNeedsQuotes:= False;
  // some special characters besides the space also require quotes to wor properly... so just do it this way here
  for var x := 1 to length(s) do
    if not CharInSet(s[x], ['a'..'z', 'A'..'Z', '0'..'9', '-', '.', '_', ',', ':', '\']) then
    begin
      lNeedsQuotes:= True;
      break;
    end;

  if lNeedsQuotes then
    Result := '"' + StringReplace(s, '"', '\"', [rfReplaceAll]) + '"'
  else
    Result:= s;
end;


class function TmaxConsoleRunner.run(const aExeName, aParamsString: string;
  aOnStdDataReady, aOnErrorDataReady: TDataReadyProc; const aWorkDir: string = ''): boolean;
var
  r: TmaxConsoleRunner;
begin
  r := TmaxConsoleRunner.Create;
  try
    r.exeName := aExeName;
    r.ParamsString := aParamsString;
    r.OnStdDataRead := aOnStdDataReady;
    r.OnErrorDataRead := aOnErrorDataReady;
    r.workDir := aWorkDir;

    Result := r.execute;
  finally
    r.free
  end;
end;

procedure TmaxConsoleRunner.SetexeName(const Value: string);
begin
  FExeName := Value;
end;

procedure TmaxConsoleRunner.SetOnErrorDataRead(const Value: TDataReadyProc);
begin
  FOnErrorDataRead := Value;
end;

procedure TmaxConsoleRunner.SetOnStdDataRead(const Value: TDataReadyProc);
begin
  FOnStdDataRead := Value;
end;

procedure TmaxConsoleRunner.SetParamsString(const Value: String);
begin
  fParamsString := Value;
end;

procedure TmaxConsoleRunner.SetRedirectErrOutToStdOut(
  const Value: Boolean);
begin
  FRedirectErrOutToStdOut := Value;
end;

procedure TmaxConsoleRunner.SetWorkDir(const Value: string);
begin
  fWorkDir := Value;
end;

{ TPipeThread }

procedure TPipeThread.asyncRead;
var
  NumberOfBytesRead: DWORD;
  s: string;
begin
  try
    // ReadFile blocks until data arrives and fails with ERROR_BROKEN_PIPE once
    // all write ends are closed, so everything the child wrote is read
    while (not fClosing) and ReadFile(fHandle, Buffer, cBufferSize, NumberOfBytesRead, nil) do
      if NumberOfBytesRead > 0 then
      begin
        Buffer[NumberOfBytesRead] := #0;
        s := String(Buffer);
        lock;
        Try
          fData.add(s);
          fEmpty := false;
        Finally
          unLock;
        End;

        fParent.fSignal.SetSignaled;
      end;
  finally
    fDone := True;
    fParent.fSignal.SetSignaled;
  end;
end;

constructor TPipeThread.Create(aParent: TmaxConsoleRunner);
begin
  inherited Create;
  fDone := True;
  fData := TList<string>.Create;
  fParent := aParent;
end;

destructor TPipeThread.Destroy;
begin
  DoClosing;
  fData.free;
  inherited;
end;

procedure TPipeThread.DoClosing;
begin
  fClosing := True;
  if fThread = nil then
    exit;
  // a grandchild may still hold the write end; cancel the blocked read until the reader exits
  while WaitForSingleObject(fThread.Handle, 50) = WAIT_TIMEOUT do
    CancelSynchronousIo(fThread.Handle);
  FreeAndNil(fThread);
end;

procedure TPipeThread.lock;
begin
  system.tmonitor.Enter(self);
end;

procedure TPipeThread.safeRetrieveOutput(out aOutput: string);
var
  l2, l: TList<string>;
  x: Integer;
begin
  aOutput := '';
  if fEmpty then
    exit;

  l := TList<string>.Create;
  lock;
  try
    // exchange the lists
    l2 := fData;
    fData := l;
    l := l2;
    fEmpty := True;
  finally
    unLock;
  end;

  // now build the output string
  aOutput := '';
  for x := 0 to l.count - 1 do
    aOutput := aOutput + l[x];
  l.free;

end;

procedure TPipeThread.StartReading(const aHandle: THandle);
begin
  DoClosing;
  fHandle := aHandle;
  fEmpty := True;
  fData.Clear;
  fDone := false;
  fClosing := false;

  // a dedicated thread, so CancelSynchronousIo cannot hit unrelated pooled work
  fThread := TThread.CreateAnonymousThread(
    procedure
    begin
      asyncRead;
    end);
  fThread.FreeOnTerminate := False;
  fThread.Start;
end;

procedure TPipeThread.unLock;
begin
  system.tmonitor.exit(self);
end;

// initialization
// TmaxConsoleRunner.tst('pg_dump.exe', '--version');
// TmaxConsoleRunner.run('pg_dump.exe', '--version', nil, nil);

end.
