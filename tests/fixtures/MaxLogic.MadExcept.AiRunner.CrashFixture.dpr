program MaxLogicMadExceptAiRunnerCrashFixture;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Winapi.Windows,
  madExcept, madLinkDisAsm, madListHardware, madListModules, madListProcesses,
  MaxLogic.MadExcept.AiRunner in '..\..\MaxLogic.MadExcept.AiRunner.pas';

type
  EMadExceptAiRunnerFixture = class(Exception);

procedure RunMadExceptAiCrashFixture;
var
  lBugReport: string;
  lExceptionIntf: IMEException;
begin
  try
    raise EMadExceptAiRunnerFixture.Create('MAD_EXCEPT_AI_CRASH_FIXTURE');
  except
    on lException: Exception do
    begin
      lExceptionIntf := madExcept.NewException(etNormal, lException, ExceptAddr, False,
        GetCurrentThreadId, 0, 0, nil, MESettings, esManual);
      lBugReport := lExceptionIntf.GetBugReport(True);
      MaxLogic.MadExcept.AiRunner.SaveAiBugReportAndTerminate(lBugReport);
      System.Halt(1);
    end;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := False;
  MaxLogic.MadExcept.AiRunner.ConfigureMadExceptForAiRunner;
  RunMadExceptAiCrashFixture;
end.
