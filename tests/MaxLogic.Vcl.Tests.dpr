program MaxLogicVclTests;

{$APPTYPE GUI}
{$STRONGLINKTYPES ON}

uses
   madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  System.SysUtils,
  DUnitX.TestFramework,
  DUnitX.Loggers.Xml.NUnit,
  maxAsync in '..\maxAsync.pas',
  MaxLogic.Async.Waiter.Vcl.Tests in 'unit\MaxLogic.Async.Waiter.Vcl.Tests.pas';

var
  lRunner: ITestRunner;
  lResults: IRunResults;
  lXmlLogger: ITestLogger;

begin
  ReportMemoryLeaksOnShutdown := False;
  Application.Initialize;
  Application.ShowMainForm := False;
  try
    TDUnitX.CheckCommandLine;
    lRunner := TDUnitX.CreateRunner;
    lRunner.UseRTTI := True;
    lRunner.FailsOnNoAsserts := False;

    lXmlLogger := TDUnitXXMLNUnitFileLogger.Create(ExtractFilePath(ParamStr(0)) + 'dunitx-vcl-results.xml');
    lRunner.AddLogger(lXmlLogger);

    lResults := lRunner.Execute;
    if not lResults.AllPassed then
      System.ExitCode := 1
    else
      System.ExitCode := 0;
  except
    on lException: Exception do
    begin
      Writeln(lException.ClassName, ': ', lException.Message);
      System.ExitCode := 1;
    end;
  end;
end.
