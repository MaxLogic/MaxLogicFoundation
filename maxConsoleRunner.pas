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
    fProcessFinished: boolean;
    fWaitForProcess: iAsync;
    fExitCode: Integer;
    FRedirectErrOutToStdOut: Boolean;
    fRunHidden: Boolean;

    procedure SetexeName(const Value: string);
    procedure SetWorkDir(const Value: string);

    procedure prepareSecurityAttributes;
    function preparePipes: boolean;
    procedure ClosehandleAndZeroIt(var ahandle: THandle);
    function CreateAsyncPipe(const name: string): THandle;
    procedure prepareStartUpInfo;
    function startProcess: boolean;
    function getCommand: string;
    function quote(const s: string): string;
    procedure StartAsyncReadPipes;
    procedure startWaitForProcessFinished;
    procedure asyncWaitForProcessFinished;
    procedure SetParamsString(const Value: String);
    procedure PullData;
    procedure SetOnErrorDataRead(const Value: TDataReadyProc);
    procedure SetOnStdDataRead(const Value: TDataReadyProc);
    function GetCmdExeFullPath: string;
    procedure SetRedirectErrOutToStdOut(const Value: Boolean);
  public
    constructor Create;
    destructor destroy; override;

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
    fAsync: iAsync;
    fDone: boolean;
    fParent: TmaxConsoleRunner;
    fEmpty: boolean;
    fWaitSignal: iSignal;
    fClosing: boolean;
    Buffer: array [0 .. cBufferSize + 1] of AnsiChar;

    procedure asyncRead;
    procedure lock;
    procedure unLock;
  public
    constructor Create(aParent: TmaxConsoleRunner);
    destructor destroy; override;

    procedure StartReading(const aHandle: THandle);
    procedure safeRetriveOutput(out aOutput: string);
    procedure DoClosing;
  end;

implementation

uses
  system.strUtils, system.ioUtils, MaxLogic.strUtils, MaxLogic.ioUtils;

{ TmaxConsoleRunner }

procedure TmaxConsoleRunner.StartAsyncReadPipes;
begin
  fPipeStdReadThread.StartReading(fPipeStdRead);
  if not RedirectErrOutToStdOut then
    fPipeErrorReadThread.StartReading(fPipeErrorsRead);
end;

procedure TmaxConsoleRunner.ClosehandleAndZeroIt(var ahandle: THandle);
begin
  if aHandle <> INVALID_HANDLE_VALUE then
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

  fPipeErrorsRead:= INVALID_HANDLE_VALUE;
  fPipeErrorsWrite:= INVALID_HANDLE_VALUE;
end;

function TmaxConsoleRunner.CreateAsyncPipe(const name: string): THandle;
const
  cPipeFormat = '\\%s\pipe\%s';
var
  s: string;
begin
  Result := 0;
  { s:= Format(cPipeFormat, [
    // server name
    className,,
    // pipename
    name]);

    Result:= CreateNamedPipe(
    PChar(s), PIPE_ACCESS_DUPLEX,
    PIPE_TYPE_MESSAGE or PIPE_READMODE_MESSAGE or PIPE_WAIT,
    PIPE_UNLIMITED_INSTANCES,
    5*cBufferSize ,
    5*cBufferSize ,
    NMPWAIT_USE_DEFAULT_WAIT,
    nil);
    // check if pipe was created
    if FHandle = INVALID_HANDLE_VALUE then
    raise Exception.Create('Could not create PIPE.');


    if ConnectNamedPipe(Result, nil) then
    begin
    end; }
  // DisconnectNamedPipe(FHandle);
end;

procedure TmaxConsoleRunner.DecodeCommand(const aCmd: String);
var
  lCmd: String;
  i: Integer;
begin
  lCmd:= aCmd;
  if startsText('"', lCmd) then
  begin
    Delete(lCmd, 1, 1);
    i:= pos('"', lCmd);
    if i>0 then
      Delete(lCmd, i, 1);
  end;

  i := pos(' ', lCmd);
  if i>0 then
  begin
    FExeName:= Copy(lCmd, 1, i-1);
    fParamsString:= Copy(lCmd, i+1, Length(lCmd));
  end else begin
    fExeName:= lCmd;
    fParamsString:= '';
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

  if createProcess(
    PChar(lExeName),
    PChar(lCmd), @fSecurityAttributes,
    @fSecurityAttributes, True,
    NORMAL_PRIORITY_CLASS,
    nil, PChar(lWorkingDir), fStartupInfo, fProcessInfo)
  then
    Result := True
  else begin
    fExitCode := GetLastError; // Return the error code if process creation fails
    RaiseLastOSError;
  end;
end;

procedure TmaxConsoleRunner.startWaitForProcessFinished;
begin
  fProcessFinished := false;
  fWaitForProcess := maxAsync.SimpleAsyncCall(
    procedure
    begin
      asyncWaitForProcessFinished;
    end);
end;

procedure TmaxConsoleRunner.asyncWaitForProcessFinished;
begin
  WaitForSingleObject(fProcessInfo.hProcess, infinite);
  fProcessFinished := True;
  fSignal.SetSignaled;
end;

destructor TmaxConsoleRunner.destroy;
begin
  fPipeStdReadThread.free;
  fPipeErrorReadThread.free;

  inherited;
end;

function TmaxConsoleRunner.execute: boolean;
begin
  Result := false;

  prepareSecurityAttributes;
  preparePipes;
  prepareStartUpInfo;

  if startProcess then
  begin
    Result := True;

    // close Write-Pipes
    ClosehandleAndZeroIt(fPipeStdWrite);
    if not RedirectErrOutToStdOut then
      ClosehandleAndZeroIt(fPipeErrorsWrite);

    fSignal.SetNonSignaled;
    StartAsyncReadPipes;
    startWaitForProcessFinished;

    repeat
      PullData;
      fSignal.WaitForSignaled();
      fSignal.SetNonSignaled;
      PullData;
    until fProcessFinished;

    if not GetExitCodeProcess(fProcessInfo.hProcess, DWORD(fExitCode)) then
      fExitCode := -1; // Return -1 if exit code retrieval fails
    ClosehandleAndZeroIt(fProcessInfo.hProcess);
    ClosehandleAndZeroIt(fProcessInfo.hThread);

    // give the threads a last chance to read out the data
    if (not fPipeStdReadThread.fDone) or (not fPipeErrorReadThread.fDone) then
      sleep(10);

    fPipeStdReadThread.DoClosing;
    fPipeErrorReadThread.DoClosing;

    ClosehandleAndZeroIt(fPipeStdRead);
    ClosehandleAndZeroIt(fPipeErrorsRead);

    PullData;

    // ensure this closes
    fWaitForProcess.WaitFor;
    fWaitForProcess := nil;
  end;
end;

function TmaxConsoleRunner.getCommand: string;
begin
  Result := trim(trim(quote(FExeName)) + ' ' + trim(fParamsString));
end;

var
  lastTick: DWORD = 0;

procedure TmaxConsoleRunner.PullData;
var
  s: String;
begin
  fPipeStdReadThread.safeRetriveOutput(s);

  if s <> '' then
  begin
    if assigned(FOnStdDataRead) then
      FOnStdDataRead(s)
    else
      fStrOutput := fStrOutput + s;
  end;

  s := '';

  fPipeErrorReadThread.safeRetriveOutput(s);
  if s <> '' then
  begin
    if assigned(FOnErrorDataRead) then
      FOnErrorDataRead(s)
    else
      fErrorOutput := fErrorOutput + s;
  end;

end;

function TmaxConsoleRunner.preparePipes: boolean;
begin
  Result := false;

  if CreatePipe(fPipeStdRead, fPipeStdWrite, @fSecurityAttributes, cBufferSize) then
  begin
    if RedirectErrOutToStdOut then
      Result:= true
    else if CreatePipe(fPipeErrorsRead, fPipeErrorsWrite, @fSecurityAttributes, cBufferSize) then
      Result := True
    else
    begin
      // close the handles that were created
      ClosehandleAndZeroIt(fPipeStdRead);
      ClosehandleAndZeroIt(fPipeStdWrite);
    end;
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
  fStartupInfo.hStdInput := fPipeStdRead;
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
  PipeSize, lpBytesLeftThisMessage: DWORD;
begin
  NumberOfBytesRead := 0;

  while True do
  begin
    NumberOfBytesRead := 0;
    lpBytesLeftThisMessage := 0;
    PipeSize := 0;
    try
      PeekNamedPipe(fHandle, nil, cBufferSize, @NumberOfBytesRead, @PipeSize, @lpBytesLeftThisMessage);
    except
      if fClosing then
        break;
      continue;
    end;
    if NumberOfBytesRead = 0 then
    begin
      if fClosing then
        break;

      fWaitSignal.WaitForSignaled(40);
      continue;
    end else begin
      PipeSize := NumberOfBytesRead;
      NumberOfBytesRead := 0;
      if not ReadFile(fHandle, Buffer, PipeSize, NumberOfBytesRead, nil) then
        break;

      if NumberOfBytesRead > 0 then
      begin
        Buffer[NumberOfBytesRead] := #0;
        s := Buffer;
        lock;
        Try
          fData.add(s);
          fEmpty := false;
        Finally
          unLock;
        End;

        fParent.fSignal.SetSignaled;
      end;
    end;
  end;
  fDone := True;
end;

constructor TPipeThread.Create(aParent: TmaxConsoleRunner);
begin
  inherited Create;
  fWaitSignal := TSignal.Create;
  fWaitSignal.SetNonSignaled;
  fData := TList<string>.Create;
  fParent := aParent;
end;

destructor TPipeThread.destroy;
begin
  if fAsync <> nil then
    fAsync.WaitFor;
  fData.free;
  inherited;
end;

procedure TPipeThread.DoClosing;
begin
  fClosing := True;
  fWaitSignal.SetSignaled;
end;

procedure TPipeThread.lock;
begin
  system.tmonitor.Enter(self);
end;

procedure TPipeThread.safeRetriveOutput(out aOutput: string);
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
  fHandle := aHandle;
  fEmpty := True;
  fData.Clear;
  fDone := false;

  fAsync := maxAsync.SimpleAsyncCall(
    procedure
    begin
      asyncRead;
    end);
end;

procedure TPipeThread.unLock;
begin
  system.tmonitor.exit(self);
end;

// initialization
// TmaxConsoleRunner.tst('pg_dump.exe', '--version');
// TmaxConsoleRunner.run('pg_dump.exe', '--version', nil, nil);

end.
