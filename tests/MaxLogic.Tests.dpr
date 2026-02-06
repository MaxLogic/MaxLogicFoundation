program MaxLogicTests;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
   madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
 System.SysUtils,
  DUnitX.TestFramework,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  MaxLogic.BufferedFile in '..\MaxLogic.BufferedFile.pas',
  MaxLogic.BufferedFile.Tests in 'unit\MaxLogic.BufferedFile.Tests.pas',
  maxAsync in '..\maxAsync.pas',
  MaxLogic.AsyncLoop.Tests in 'unit\MaxLogic.AsyncLoop.Tests.pas',
  MaxLogic.DotEnv.Tests in 'unit\MaxLogic.DotEnv.Tests.pas',
  maxlogic.DotEnv in '..\maxlogic.DotEnv.pas',
  MaxLogic.RichIniFile in '..\MaxLogic.RichIniFile.pas',
  MaxLogic.RichIniFiles.Tests in 'unit\MaxLogic.RichIniFiles.Tests.pas',
  maxLogic.StrUtils in '..\MaxLogic.StrUtils.pas',
  MaxLogic.PortableTimer.Tests in 'unit\MaxLogic.PortableTimer.Tests.pas',
  MaxLogic.StrUtils.Tests in 'unit\MaxLogic.StrUtils.Tests.pas',
  MaxLogic.hash.FNV1a in '..\MaxLogic.hash.FNV1a.pas',
  MaxLogic.hash.xxHash in '..\MaxLogic.hash.xxHash.pas',
  MaxLogic.hash.Murmur in '..\MaxLogic.hash.Murmur.pas',
  MaxLogic.Cache in '..\MaxLogic.Cache.pas',
  MaxLogic.Cache.RepositoryBase in '..\MaxLogic.Cache.RepositoryBase.pas',
  MaxLogic.Cache.Tests in 'unit\MaxLogic.Cache.Tests.pas',
  MaxLogic.Hash.Tests in 'unit\MaxLogic.Hash.Tests.pas',
  MaxLogic.Process.Tests in 'unit\MaxLogic.Process.Tests.pas';

var
  Runner: ITestRunner;
  Results: IRunResults;
  Logger: ITestLogger;
  XMLLogger: ITestLogger;
begin
  ReportMemoryLeaksOnShutdown := True;
  try
    TDUnitX.CheckCommandLine;
    Runner := TDUnitX.CreateRunner;
    Runner.UseRTTI := True;
    Runner.FailsOnNoAsserts := False;

    Logger := TDUnitXConsoleLogger.Create(True);
    Runner.AddLogger(Logger);

    XMLLogger := TDUnitXXMLNUnitFileLogger.Create;
    Runner.AddLogger(XMLLogger);

    Results := Runner.Execute;
    if not Results.AllPassed then
      System.ExitCode := 1
    else
      System.ExitCode := 0;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      System.ExitCode := 1;
    end;
  end;
end.
