unit MaxLogic.MadExcept.AiRunner;

interface

// AI-friendly madExcept setup for automated DEBUG runs.
//
// Recommended use in application projects:
//
//   Add this unit early in the DPR uses list, then call:
//
//     {$IF DEFINED(madExcept) AND DEFINED(DEBUG)}
//     MaxLogic.MadExcept.AiRunner.ConfigureFromEnvironment;
//     {$IFEND}
//
//   Set MAXLOGIC_MADEXCEPT_AI=1 when the run is controlled by an AI/test agent.
//   In that mode we suppress the madExcept UI, delete any stale AI bugreport on
//   startup, write one AI-sized last bugreport, and terminate with the configured
//   special exit code.
//
//   Normal developer/debug runs stay unchanged unless the environment variable is
//   set. Release builds do not enable this recommended opt-in path.
//
// Unit test projects can call ConfigureMadExceptForAiRunner directly inside
// {$IFDEF madExcept} because unattended test runners should never show modal
// madExcept UI.

const
  cMadExceptAiRunnerEnvironmentVariable = 'MAXLOGIC_MADEXCEPT_AI';
  cMadExceptAiRunnerDefaultExitCode = 207;
  cMadExceptAiRunnerDefaultMaxReportChars = 65536;
  cMadExceptAiRunnerDefaultReportFileName = 'bugreport.txt';
  cMadExceptAiRunnerFallbackReportFileName = 'bugreport-ai-runner-error.txt';

procedure ConfigureMadExceptForAiRunner(const aBugReportFileName: string = '';
  const aExitCode: Integer = cMadExceptAiRunnerDefaultExitCode;
  const aMaxReportChars: Integer = cMadExceptAiRunnerDefaultMaxReportChars);
function ConfigureFromEnvironment(const aEnvironmentVariable: string = cMadExceptAiRunnerEnvironmentVariable;
  const aBugReportFileName: string = '';
  const aExitCode: Integer = cMadExceptAiRunnerDefaultExitCode;
  const aMaxReportChars: Integer = cMadExceptAiRunnerDefaultMaxReportChars): Boolean;
procedure DeleteStaleBugReport(const aBugReportFileName: string);
function EnvironmentValueEnablesAiRunner(const aValue: string): Boolean;
procedure SaveAiBugReportAndTerminate(const aRawBugReport: string; const aBugReportFileName: string = '';
  const aExitCode: Integer = cMadExceptAiRunnerDefaultExitCode;
  const aMaxReportChars: Integer = cMadExceptAiRunnerDefaultMaxReportChars);
function TrimBugReportForAi(const aBugReport: string;
  const aMaxReportChars: Integer = cMadExceptAiRunnerDefaultMaxReportChars): string;

implementation

uses
  System.IOUtils, System.StrUtils, System.SysUtils
  {$IF DEFINED(MSWINDOWS)}
  , Winapi.Windows
  {$IFEND}
  {$IF DEFINED(madExcept)}
  , madExcept
  {$IFEND};

var
  glBugReportFileName: string = '';
  glExitCode: Integer = cMadExceptAiRunnerDefaultExitCode;
  glMaxReportChars: Integer = cMadExceptAiRunnerDefaultMaxReportChars;
  glRegistered: Boolean = False;

function ApplyMaxReportChars(const aReport: string; const aMaxReportChars: Integer): string;
const
  cTruncatedMarker = #13#10'[madExcept AI report truncated]';
var
  lCutLength: Integer;
begin
  if (aMaxReportChars <= 0) or (Length(aReport) <= aMaxReportChars) then
    Exit(aReport);

  if aMaxReportChars <= Length(cTruncatedMarker) then
    Exit(Copy(cTruncatedMarker, 1, aMaxReportChars));

  lCutLength := aMaxReportChars - Length(cTruncatedMarker);
  Result := Copy(aReport, 1, lCutLength) + cTruncatedMarker;
end;

procedure AppendReportLine(var aReport: string; const aLine: string);
begin
  if aReport <> '' then
    aReport := aReport + #13#10;
  aReport := aReport + aLine;
end;

function IsNoisySectionHeader(const aLine: string): Boolean;
var
  lLine: string;
begin
  lLine := LowerCase(Trim(aLine));
  Result :=
    (lLine = 'modules:') or
    (lLine = 'processes:') or
    (lLine = 'memory dump:') or
    (lLine = 'cpu registers:') or
    (lLine = 'disassembly:') or
    (lLine = 'stack dump:');
end;

function IsHexAddressColumn(const aLine: string): Boolean;
var
  i: Integer;
begin
  Result := Length(aLine) >= 9;
  if not Result then
    Exit;

  for i := 1 to 8 do
  begin
    if not CharInSet(aLine[i], ['0'..'9', 'A'..'F', 'a'..'f']) then
      Exit(False);
  end;

  Result := CharInSet(aLine[9], [' ', #9]);
end;

function IsReportHeaderStart(const aLine: string): Boolean;
begin
  Result := StartsText('date/time', TrimLeft(aLine));
end;

function IsThreadHeader(const aLine: string): Boolean;
var
  lLine: string;
begin
  lLine := TrimLeft(aLine);
  Result :=
    StartsText('thread $', lLine) or
    StartsText('mainvclthread', LowerCase(lLine)) or
    (Pos('Thread ($', lLine) > 0);
end;

function NormalizeLineBreaks(const aText: string): string;
begin
  Result := aText.Replace(#13#10, #10).Replace(#13, #10);
end;

function RemoveLeadingCallstackAddress(const aLine: string): string;
var
  lIndex: Integer;
  lLine: string;
begin
  lLine := TrimLeft(aLine);
  if not IsHexAddressColumn(lLine) then
    Exit(aLine);

  lLine := TrimLeft(Copy(lLine, 9, MaxInt));
  if StartsText('+', lLine) then
  begin
    lIndex := Pos(' ', lLine);
    if lIndex > 0 then
      lLine := TrimLeft(Copy(lLine, lIndex + 1, MaxInt));
  end;

  Result := lLine;
end;

function ResolveBugReportFileName(const aBugReportFileName: string): string;
var
  lFileName: string;
begin
  lFileName := Trim(aBugReportFileName);
  if lFileName = '' then
    lFileName := cMadExceptAiRunnerDefaultReportFileName;

  if TPath.IsPathRooted(lFileName) then
    Exit(lFileName);

  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), lFileName);
end;

procedure TerminateWithExitCode(const aExitCode: Integer);
begin
  {$IF DEFINED(MSWINDOWS)}
  TerminateProcess(GetCurrentProcess, Cardinal(aExitCode));
  {$IFEND}
  System.Halt(aExitCode);
end;

procedure WriteDebugFailure(const aMessage: string);
begin
  {$IF DEFINED(MSWINDOWS)}
  OutputDebugString(PChar(aMessage));
  {$ELSE}
  Writeln(aMessage);
  {$IFEND}
end;

procedure WriteFallbackBugReport(const aMessage: string);
var
  lFallbackFileName: string;
begin
  lFallbackFileName := TPath.Combine(ExtractFilePath(ParamStr(0)), cMadExceptAiRunnerFallbackReportFileName);
  try
    TFile.WriteAllText(lFallbackFileName, aMessage, TEncoding.UTF8);
  except
    on lException: Exception do
      WriteDebugFailure('madExcept AI runner failed to write fallback bugreport: ' +
        lException.ClassName + ': ' + lException.Message);
  end;
end;

{$IF DEFINED(madExcept)}
procedure ApplyMadExceptAiSettings(const aBugReportFileName: string; const aMaxReportChars: Integer);
var
  lSettings: IMEModuleSettings;
begin
  lSettings := MESettings;

  lSettings.GeneralShowSetting := ssNothing;
  lSettings.Filter1ShowSetting := ssNothing;
  lSettings.Filter2ShowSetting := ssNothing;
  lSettings.ShowPleaseWaitBox := False;

  lSettings.AutoSave := False;
  lSettings.AutoSend := False;
  lSettings.AutoClipboard := False;
  lSettings.AutoContinue := False;
  lSettings.AutoRestart := 0;
  lSettings.AutoClose := 0;

  lSettings.BugReportFile := aBugReportFileName;
  lSettings.AppendBugReports := False;

  lSettings.Filter1NoBugReport := False;
  lSettings.Filter2NoBugReport := False;
  lSettings.GeneralNoBugReport := False;
  lSettings.Filter1NoHandlers := False;
  lSettings.Filter2NoHandlers := False;
  lSettings.GeneralNoHandlers := False;
  lSettings.Filter1NoScreenShot := True;
  lSettings.Filter2NoScreenShot := True;
  lSettings.GeneralNoScreenShot := True;
  lSettings.Filter1NoSuspend := True;
  lSettings.Filter2NoSuspend := True;
  lSettings.GeneralNoSuspend := True;

  lSettings.ListThreads := False;
  lSettings.ShowCpuRegisters := False;
  lSettings.ShowStackDump := False;
  lSettings.ShowDisAsm := False;
  lSettings.HideUglyItems := True;
  lSettings.ShowRelativeAddrs := False;
  lSettings.ShowRelativeLines := True;
  lSettings.ScreenShotDepth := 0;

  if aMaxReportChars > 0 then
    lSettings.BugReportFileSize := Cardinal(aMaxReportChars);
end;

procedure MadExceptAiRunnerHandler(const aExceptIntf: IMEException; var aHandled: Boolean);
var
  lBugReport: string;
begin
  try
    aExceptIntf.ShowPleaseWaitBox := False;
    lBugReport := aExceptIntf.GetBugReport(True);
  except
    on lException: Exception do
    begin
      WriteFallbackBugReport('madExcept AI runner failed to compose bugreport: ' +
        lException.ClassName + ': ' + lException.Message);
      WriteDebugFailure('madExcept AI runner failed to compose bugreport: ' +
        lException.ClassName + ': ' + lException.Message);
      lBugReport := '';
    end;
  end;

  aHandled := True;
  SaveAiBugReportAndTerminate(lBugReport, glBugReportFileName, glExitCode, glMaxReportChars);
end;
{$IFEND}

function ConfigureFromEnvironment(const aEnvironmentVariable, aBugReportFileName: string;
  const aExitCode, aMaxReportChars: Integer): Boolean;
begin
  Result := EnvironmentValueEnablesAiRunner(GetEnvironmentVariable(aEnvironmentVariable));
  if Result then
  begin
    {$IF DEFINED(madExcept)}
    ConfigureMadExceptForAiRunner(aBugReportFileName, aExitCode, aMaxReportChars);
    {$ELSE}
    Result := False;
    {$IFEND}
  end;
end;

procedure ConfigureMadExceptForAiRunner(const aBugReportFileName: string;
  const aExitCode, aMaxReportChars: Integer);
begin
  glBugReportFileName := ResolveBugReportFileName(aBugReportFileName);
  glExitCode := aExitCode;
  glMaxReportChars := aMaxReportChars;

  DeleteStaleBugReport(glBugReportFileName);
  DeleteStaleBugReport(TPath.Combine(ExtractFilePath(ParamStr(0)), cMadExceptAiRunnerFallbackReportFileName));

  {$IF DEFINED(madExcept)}
  ApplyMadExceptAiSettings(glBugReportFileName, glMaxReportChars);
  if not glRegistered then
  begin
    RegisterExceptionHandler(MadExceptAiRunnerHandler, stTrySyncCallAlways, epMainPhase);
    glRegistered := True;
  end;
  {$IFEND}
end;

procedure DeleteStaleBugReport(const aBugReportFileName: string);
begin
  if (aBugReportFileName <> '') and TFile.Exists(aBugReportFileName) then
    TFile.Delete(aBugReportFileName);
end;

function EnvironmentValueEnablesAiRunner(const aValue: string): Boolean;
begin
  Result := SameText(Trim(aValue), '1');
end;

procedure SaveAiBugReportAndTerminate(const aRawBugReport, aBugReportFileName: string;
  const aExitCode, aMaxReportChars: Integer);
var
  lBugReport: string;
  lBugReportFileName: string;
  lDirectory: string;
begin
  lBugReportFileName := ResolveBugReportFileName(aBugReportFileName);
  try
    lBugReport := TrimBugReportForAi(aRawBugReport, aMaxReportChars);

    lDirectory := ExtractFilePath(lBugReportFileName);
    if lDirectory <> '' then
      TDirectory.CreateDirectory(lDirectory);

    TFile.WriteAllText(lBugReportFileName, lBugReport, TEncoding.UTF8);
  except
    on lException: Exception do
    begin
      WriteFallbackBugReport('madExcept AI runner failed to write ' + lBugReportFileName + ': ' +
        lException.ClassName + ': ' + lException.Message);
      WriteDebugFailure('madExcept AI runner failed to write bugreport: ' +
        lException.ClassName + ': ' + lException.Message);
    end;
  end;

  TerminateWithExitCode(aExitCode);
end;

function TrimBugReportForAi(const aBugReport: string; const aMaxReportChars: Integer): string;
var
  lLines: TArray<string>;
  lLine: string;
  lNormalized: string;
  lReport: string;
  lSawFirstReportHeader: Boolean;
  lSawFirstThreadHeader: Boolean;
  lSkipThread: Boolean;
begin
  lNormalized := NormalizeLineBreaks(aBugReport);
  lLines := lNormalized.Split([#10]);
  lReport := '';
  lSawFirstReportHeader := False;
  lSawFirstThreadHeader := False;
  lSkipThread := False;

  for lLine in lLines do
  begin
    if IsReportHeaderStart(lLine) then
    begin
      if lSawFirstReportHeader then
        Break;
      lSawFirstReportHeader := True;
    end;

    if IsNoisySectionHeader(lLine) then
      Break;

    if IsThreadHeader(lLine) then
    begin
      if lSawFirstThreadHeader then
      begin
        lSkipThread := True;
        Continue;
      end;

      lSawFirstThreadHeader := True;
      lSkipThread := False;
    end;

    if lSkipThread then
      Continue;

    AppendReportLine(lReport, RemoveLeadingCallstackAddress(lLine));
  end;

  Result := ApplyMaxReportChars(TrimRight(lReport), aMaxReportChars);
end;

end.
