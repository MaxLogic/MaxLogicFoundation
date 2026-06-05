unit MaxLogic.MadExcept.AiRunner.Tests;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMaxLogicMadExceptAiRunnerTests = class
  strict private
    {$IF DEFINED(MSWINDOWS) AND DEFINED(madExcept)}
    function RunCrashFixture(const aFixtureExeName: string; out aExitCode: Cardinal): Boolean;
    {$IFEND}
  public
    [Test] procedure EnvironmentValueEnablesAiRunnerAcceptsOnlyOne;
    [Test] procedure DeleteStaleBugReportRemovesOldFile;
    [Test] procedure TrimBugReportForAiRemovesNoisySections;
    [Test] procedure TrimBugReportForAiCapsOutput;
    {$IF DEFINED(MSWINDOWS) AND DEFINED(madExcept)}
    [Test] procedure UnhandledExceptionWritesAiBugReportAndExitsWithSpecialCode;
    {$IFEND}
  end;

implementation

uses
  System.IOUtils, System.SysUtils
  {$IF DEFINED(MSWINDOWS)}
  , Winapi.Windows
  {$IFEND}
  , MaxLogic.MadExcept.AiRunner;

{$IF DEFINED(MSWINDOWS) AND DEFINED(madExcept)}
function TMaxLogicMadExceptAiRunnerTests.RunCrashFixture(const aFixtureExeName: string;
  out aExitCode: Cardinal): Boolean;
const
  cTimeoutMs = 30000;
var
  lCommandLine: string;
  lProcessInfo: TProcessInformation;
  lStartupInfo: TStartupInfo;
  lWaitResult: DWORD;
begin
  Result := False;
  aExitCode := 0;

  FillChar(lProcessInfo, SizeOf(lProcessInfo), 0);
  FillChar(lStartupInfo, SizeOf(lStartupInfo), 0);
  lStartupInfo.cb := SizeOf(lStartupInfo);
  lStartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  lStartupInfo.wShowWindow := SW_HIDE;

  lCommandLine := '"' + aFixtureExeName + '"';
  UniqueString(lCommandLine);

  if not CreateProcess(nil, PChar(lCommandLine), nil, nil, False, CREATE_NO_WINDOW, nil,
    PChar(ExtractFilePath(aFixtureExeName)), lStartupInfo, lProcessInfo) then
    RaiseLastOSError;

  try
    lWaitResult := WaitForSingleObject(lProcessInfo.hProcess, cTimeoutMs);
    if lWaitResult = WAIT_TIMEOUT then
    begin
      TerminateProcess(lProcessInfo.hProcess, 254);
      Assert.Fail('madExcept AI crash fixture did not exit within timeout');
    end;

    if lWaitResult <> WAIT_OBJECT_0 then
      RaiseLastOSError;

    if not GetExitCodeProcess(lProcessInfo.hProcess, aExitCode) then
      RaiseLastOSError;

    Result := True;
  finally
    CloseHandle(lProcessInfo.hThread);
    CloseHandle(lProcessInfo.hProcess);
  end;
end;
{$IFEND}

procedure TMaxLogicMadExceptAiRunnerTests.DeleteStaleBugReportRemovesOldFile;
var
  lFileName: string;
begin
  lFileName := TPath.Combine(TPath.GetTempPath, TGUID.NewGuid.ToString + '.txt');
  TFile.WriteAllText(lFileName, 'stale bugreport', TEncoding.UTF8);

  DeleteStaleBugReport(lFileName);

  Assert.IsFalse(TFile.Exists(lFileName), 'Expected stale bugreport file to be deleted');
end;

procedure TMaxLogicMadExceptAiRunnerTests.EnvironmentValueEnablesAiRunnerAcceptsOnlyOne;
begin
  Assert.IsTrue(EnvironmentValueEnablesAiRunner('1'));
  Assert.IsTrue(EnvironmentValueEnablesAiRunner(' 1 '));
  Assert.IsFalse(EnvironmentValueEnablesAiRunner(''));
  Assert.IsFalse(EnvironmentValueEnablesAiRunner('0'));
  Assert.IsFalse(EnvironmentValueEnablesAiRunner('true'));
end;

procedure TMaxLogicMadExceptAiRunnerTests.TrimBugReportForAiCapsOutput;
var
  lReport: string;
  lTrimmed: string;
begin
  lReport :=
    'exception class    : EAccessViolation' + sLineBreak +
    'exception message  : boom' + sLineBreak +
    'MainVclThread ($1234):' + sLineBreak +
    StringOfChar('x', 2000);

  lTrimmed := TrimBugReportForAi(lReport, 512);

  Assert.IsTrue(Length(lTrimmed) <= 512, 'Expected AI bugreport to respect the configured size cap');
  Assert.Contains(lTrimmed, 'truncated');
end;

procedure TMaxLogicMadExceptAiRunnerTests.TrimBugReportForAiRemovesNoisySections;
var
  lReport: string;
  lTrimmed: string;
begin
  lReport :=
    'date/time          : 2026-06-02, 13:07:08, 810ms' + sLineBreak +
    'command line       : "App.exe" --headless' + sLineBreak +
    'callstack crc      : $d4e840ed, $12ed0151, $12ed0151' + sLineBreak +
    'exception class    : EAccessViolation' + sLineBreak +
    'exception message  : Access violation' + sLineBreak +
    sLineBreak +
    'MainVclThread ($d0fc):' + sLineBreak +
    '009294a0 +000 App.exe  My.Project.Unit  124 +0 TThing.Run' + sLineBreak +
    sLineBreak +
    'thread $f130 (worker):' + sLineBreak +
    '75d25d47 +17 KERNEL32.DLL BaseThreadInitThunk' + sLineBreak +
    sLineBreak +
    'modules:' + sLineBreak +
    '00920000 App.exe 1.0.0.0 F:\app' + sLineBreak +
    sLineBreak +
    'processes:' + sLineBreak +
    '00004 System 0 0 0';

  lTrimmed := TrimBugReportForAi(lReport, 4096);

  Assert.Contains(lTrimmed, 'exception class    : EAccessViolation');
  Assert.Contains(lTrimmed, 'exception message  : Access violation');
  Assert.Contains(lTrimmed, 'My.Project.Unit');
  Assert.IsFalse(lTrimmed.Contains('009294a0'), 'Expected absolute callstack addresses to be removed');
  Assert.IsFalse(lTrimmed.Contains('modules:'), 'Expected modules section to be removed');
  Assert.IsFalse(lTrimmed.Contains('processes:'), 'Expected processes section to be removed');
  Assert.IsFalse(lTrimmed.Contains('thread $f130'), 'Expected non-crashing thread sections to be removed');
  Assert.IsFalse(lTrimmed.Contains('KERNEL32.DLL BaseThreadInitThunk'), 'Expected non-crashing thread details to be removed');
end;

{$IF DEFINED(MSWINDOWS) AND DEFINED(madExcept)}
procedure TMaxLogicMadExceptAiRunnerTests.UnhandledExceptionWritesAiBugReportAndExitsWithSpecialCode;
var
  lBugReportFileName: string;
  lFixtureExeName: string;
  lExitCode: Cardinal;
  lReport: string;
begin
  lFixtureExeName := TPath.Combine(ExtractFilePath(ParamStr(0)),
    'fixtures\MaxLogic.MadExcept.AiRunner.CrashFixture.exe');
  Assert.IsTrue(TFile.Exists(lFixtureExeName), 'Expected crash fixture executable to be built');

  lBugReportFileName := TPath.Combine(ExtractFilePath(lFixtureExeName), cMadExceptAiRunnerDefaultReportFileName);
  TFile.WriteAllText(lBugReportFileName, 'stale bugreport', TEncoding.UTF8);
  try
    Assert.IsTrue(RunCrashFixture(lFixtureExeName, lExitCode), 'Expected crash fixture process to run');
    Assert.AreEqual(cMadExceptAiRunnerDefaultExitCode, Integer(lExitCode));
    Assert.IsTrue(TFile.Exists(lBugReportFileName), 'Expected child process to write bugreport.txt');

    lReport := TFile.ReadAllText(lBugReportFileName, TEncoding.UTF8);
    Assert.Contains(lReport, 'exception class    : EMadExceptAiRunnerFixture');
    Assert.Contains(lReport, 'exception message  : MAD_EXCEPT_AI_CRASH_FIXTURE');
    Assert.Contains(lReport, 'RunMadExceptAiCrashFixture');
    Assert.IsFalse(lReport.Contains('stale bugreport'), 'Expected child startup to delete stale bugreport first');
    Assert.IsFalse(lReport.Contains('modules:'), 'Expected AI bugreport to exclude modules section');
    Assert.IsFalse(lReport.Contains('processes:'), 'Expected AI bugreport to exclude processes section');
  finally
    DeleteStaleBugReport(lBugReportFileName);
  end;
end;
{$IFEND}

initialization
  TDUnitX.RegisterTestFixture(TMaxLogicMadExceptAiRunnerTests);

end.
