unit maxConsoleRunner.Tests;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMaxConsoleRunnerTests = class
  private
    fTempDir: string;
    fBigFile: string;
    fBigText: string;
    function CmdExe: string;
    function RunCmd(const aParams: string; aRedirect: Boolean = False): string;
    function RunCmdStdErr(const aParams: string): string;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure Execute_LargeStdOut_IsCapturedCompletely;
    [Test] procedure Execute_LargeStdErr_IsCapturedCompletely;
    [Test] procedure Execute_RedirectedStdErr_IsCapturedCompletely;
    [Test] procedure Execute_Callbacks_ReceiveAllDataOnCallingThread;
    [Test] procedure Execute_ParallelRunners_CaptureCompleteOutput;
    [Test] procedure Execute_GrandchildHoldingPipe_ReturnsAfterGrace;
    [Test] procedure DecodeCommand_QuotedExeWithSpace_KeepsFullPath;
    [Test] procedure ExecuteFile_ExePathWithSpace_Runs;
    [Test] procedure Execute_MissingExe_RaisesWithoutLeakingHandles;
  end;

implementation

uses
  System.Classes, System.Diagnostics, System.IOUtils, System.SysUtils,
  Winapi.Windows,
  maxConsoleRunner, MaxLogic.ioUtils;

const
  cRepeats = 20;

function GetProcessHandleCount(hProcess: THandle; var pdwHandleCount: DWORD): BOOL; stdcall;
  external kernel32 name 'GetProcessHandleCount';

function CurrentHandleCount: Integer;
var
  lCount: DWORD;
begin
  lCount := 0;
  if not GetProcessHandleCount(GetCurrentProcess, lCount) then
    RaiseLastOSError;
  Result := lCount;
end;

function StartRunnerThread(const aExe, aParams, aExpected: string; aFailures: PInteger): TThread;
begin
  Result := TThread.CreateAnonymousThread(
    procedure
    var
      lRun: Integer;
      lRunner: TmaxConsoleRunner;
    begin
      for lRun := 1 to 5 do
      begin
        lRunner := TmaxConsoleRunner.Create;
        try
          lRunner.ExeName := aExe;
          lRunner.ParamsString := aParams;
          lRunner.Execute;
          if lRunner.StrOutput <> aExpected then
            Inc(aFailures^);
        finally
          lRunner.Free;
        end;
      end;
    end);
  Result.FreeOnTerminate := False;
  Result.Start;
end;

{ TMaxConsoleRunnerTests }

procedure TMaxConsoleRunnerTests.Setup;
var
  lBuilder: TStringBuilder;
  i: Integer;
begin
  fTempDir := TPath.Combine(TPath.GetTempPath, 'maxConsoleRunner tests ' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(fTempDir);
  // ~1 MB of numbered ASCII lines; the numbers reveal any lost chunk
  lBuilder := TStringBuilder.Create;
  try
    for i := 1 to 16000 do
      lBuilder.Append('line ').Append(i).Append(' abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMN').Append(#13#10);
    fBigText := lBuilder.ToString;
  finally
    lBuilder.Free;
  end;
  fBigFile := TPath.Combine(fTempDir, 'big output.txt');
  TFile.WriteAllText(fBigFile, fBigText, TEncoding.ASCII);
end;

procedure TMaxConsoleRunnerTests.TearDown;
begin
  try
    TDirectory.Delete(fTempDir, True);
  except
    // a lingering grandchild (ping) may still hold the folder; the OS temp cleanup handles it
  end;
end;

function TMaxConsoleRunnerTests.CmdExe: string;
begin
  Result := TPath.Combine(GetEnvironmentVariable('SystemRoot'), 'System32\cmd.exe');
end;

function TMaxConsoleRunnerTests.RunCmd(const aParams: string; aRedirect: Boolean): string;
var
  lRunner: TmaxConsoleRunner;
begin
  lRunner := TmaxConsoleRunner.Create;
  try
    lRunner.ExeName := CmdExe;
    lRunner.ParamsString := aParams;
    lRunner.RedirectErrOutToStdOut := aRedirect;
    lRunner.Execute;
    Result := lRunner.StrOutput;
  finally
    lRunner.Free;
  end;
end;

function TMaxConsoleRunnerTests.RunCmdStdErr(const aParams: string): string;
var
  lRunner: TmaxConsoleRunner;
begin
  lRunner := TmaxConsoleRunner.Create;
  try
    lRunner.ExeName := CmdExe;
    lRunner.ParamsString := aParams;
    lRunner.Execute;
    Result := lRunner.ErrorOutput;
  finally
    lRunner.Free;
  end;
end;

procedure TMaxConsoleRunnerTests.Execute_LargeStdOut_IsCapturedCompletely;
var
  i, lFailures: Integer;
  lOutput: string;
begin
  lFailures := 0;
  for i := 1 to cRepeats do
  begin
    lOutput := RunCmd('/c type "' + fBigFile + '"');
    if lOutput <> fBigText then
      Inc(lFailures);
  end;
  Assert.AreEqual(0, lFailures, Format('%d of %d runs lost stdout data', [lFailures, cRepeats]));
end;

procedure TMaxConsoleRunnerTests.Execute_LargeStdErr_IsCapturedCompletely;
var
  i, lFailures: Integer;
begin
  lFailures := 0;
  for i := 1 to cRepeats do
    if RunCmdStdErr('/c type "' + fBigFile + '" 1>&2') <> fBigText then
      Inc(lFailures);
  Assert.AreEqual(0, lFailures, Format('%d of %d runs lost stderr data', [lFailures, cRepeats]));
end;

procedure TMaxConsoleRunnerTests.Execute_RedirectedStdErr_IsCapturedCompletely;
var
  i, lFailures: Integer;
begin
  lFailures := 0;
  for i := 1 to cRepeats do
    if RunCmd('/c type "' + fBigFile + '" 1>&2', True) <> fBigText then
      Inc(lFailures);
  Assert.AreEqual(0, lFailures, Format('%d of %d runs lost redirected stderr data', [lFailures, cRepeats]));
end;

procedure TMaxConsoleRunnerTests.Execute_Callbacks_ReceiveAllDataOnCallingThread;
var
  lRunner: TmaxConsoleRunner;
  lOut, lErr: TStringBuilder;
  lForeignThread: Boolean;
begin
  lForeignThread := False;
  lOut := TStringBuilder.Create;
  lErr := TStringBuilder.Create;
  lRunner := TmaxConsoleRunner.Create;
  try
    lRunner.ExeName := CmdExe;
    lRunner.ParamsString := '/c type "' + fBigFile + '" & echo err-tail 1>&2';
    lRunner.OnStdDataRead :=
      procedure(const aText: string)
      begin
        if TThread.CurrentThread.ThreadID <> MainThreadID then
          lForeignThread := True;
        lOut.Append(aText);
      end;
    lRunner.OnErrorDataRead :=
      procedure(const aText: string)
      begin
        lErr.Append(aText);
      end;
    Assert.IsTrue(lRunner.Execute);
    Assert.AreEqual(0, lRunner.ExitCode);
    Assert.AreEqual('', lRunner.StrOutput, 'callbacks replace StrOutput');
    Assert.IsTrue(lOut.ToString = fBigText, 'stdout passed to the callback is incomplete');
    Assert.AreEqual('err-tail ' + sLineBreak, lErr.ToString);
    Assert.IsFalse(lForeignThread, 'callbacks must run on the thread that called Execute');
  finally
    lRunner.OnStdDataRead := nil;
    lRunner.OnErrorDataRead := nil;
    lRunner.Free;
    lErr.Free;
    lOut.Free;
  end;
end;

procedure TMaxConsoleRunnerTests.Execute_ParallelRunners_CaptureCompleteOutput;
const
  cThreads = 4;
var
  lThreads: array[0..cThreads - 1] of TThread;
  lFailures: array[0..cThreads - 1] of Integer;
  i, lTotal: Integer;
begin
  for i := 0 to cThreads - 1 do
  begin
    lFailures[i] := 0;
    lThreads[i] := StartRunnerThread(CmdExe, '/c type "' + fBigFile + '"', fBigText, @lFailures[i]);
  end;
  lTotal := 0;
  for i := 0 to cThreads - 1 do
  begin
    lThreads[i].WaitFor;
    if Assigned(lThreads[i].FatalException) then
      Inc(lTotal, 100);
    lThreads[i].Free;
    Inc(lTotal, lFailures[i]);
  end;
  Assert.AreEqual(0, lTotal, 'parallel runs lost output or raised');
end;
procedure TMaxConsoleRunnerTests.Execute_GrandchildHoldingPipe_ReturnsAfterGrace;
var
  lWatch: TStopwatch;
  lOutput: string;
begin
  // ping inherits the stdout pipe and outlives cmd.exe by ~20 s
  lWatch := TStopwatch.StartNew;
  lOutput := RunCmd('/c "echo first& start "" /b ping -n 20 127.0.0.1"');
  lWatch.Stop;
  Assert.IsTrue(lOutput.StartsWith('first'), 'unexpected output: ' + lOutput);
  Assert.IsTrue(lWatch.ElapsedMilliseconds < 15000,
    Format('Execute waited %d ms for the grandchild', [lWatch.ElapsedMilliseconds]));
end;

procedure TMaxConsoleRunnerTests.DecodeCommand_QuotedExeWithSpace_KeepsFullPath;
var
  lRunner: TmaxConsoleRunner;
begin
  lRunner := TmaxConsoleRunner.Create;
  try
    lRunner.DecodeCommand('"C:\Program Files\Some Tool\tool.exe" -a "b c"');
    Assert.AreEqual('C:\Program Files\Some Tool\tool.exe', lRunner.ExeName);
    Assert.AreEqual('-a "b c"', lRunner.ParamsString);
    Assert.AreEqual('C:\Program Files\Some Tool\', lRunner.WorkDir);

    lRunner.DecodeCommand('"C:\Program Files\Some Tool\tool.exe"');
    Assert.AreEqual('C:\Program Files\Some Tool\tool.exe', lRunner.ExeName);
    Assert.AreEqual('', lRunner.ParamsString);

    lRunner.DecodeCommand('C:\Tools\tool.exe -v');
    Assert.AreEqual('C:\Tools\tool.exe', lRunner.ExeName);
    Assert.AreEqual('-v', lRunner.ParamsString);
  finally
    lRunner.Free;
  end;
end;

procedure TMaxConsoleRunnerTests.ExecuteFile_ExePathWithSpace_Runs;
var
  lExe, lOutput: string;
  lExitCode: Integer;
begin
  lExe := TPath.Combine(fTempDir, 'sub dir\my cmd.exe');
  TDirectory.CreateDirectory(ExtractFileDir(lExe));
  TFile.Copy(CmdExe, lExe);
  lOutput := '';
  lExitCode := -1;
  MaxLogic.ioUtils.ExecuteFile('"' + lExe + '" /c echo hello& exit 7', '', lExitCode,
    procedure(const aText: string)
    begin
      lOutput := lOutput + aText;
    end);
  Assert.AreEqual(7, lExitCode);
  Assert.AreEqual('hello' + sLineBreak, lOutput);
end;

procedure TMaxConsoleRunnerTests.Execute_MissingExe_RaisesWithoutLeakingHandles;
const
  cAttempts = 50;
var
  lRunner: TmaxConsoleRunner;
  lMissing: string;
  i, lBefore, lAfter: Integer;
begin
  lMissing := TPath.Combine(fTempDir, 'missing\none.exe');
  lRunner := TmaxConsoleRunner.Create;
  try
    lRunner.ExeName := lMissing;
    // warm-up: lazily created RTL/OS handles must not count as a leak
    Assert.WillRaise(procedure begin lRunner.Execute; end, EOSError);
    lBefore := CurrentHandleCount;
    for i := 1 to cAttempts do
      Assert.WillRaise(procedure begin lRunner.Execute; end, EOSError);
    lAfter := CurrentHandleCount;
  finally
    lRunner.Free;
  end;
  Assert.IsTrue(lAfter - lBefore < 10,
    Format('%d handles leaked over %d failed starts', [lAfter - lBefore, cAttempts]));
end;

initialization
  TDUnitX.RegisterTestFixture(TMaxConsoleRunnerTests);

end.
