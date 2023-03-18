Unit AppWideStopWatch;

Interface

Uses
  sysUtils, diagnostics;

Var
  AppStopWatch: TStopWatch;
  AppStartDateTime: TDateTime;

Implementation


Initialization

AppStopWatch := TStopWatch.StartNew;
AppStartDateTime := now();

End.
