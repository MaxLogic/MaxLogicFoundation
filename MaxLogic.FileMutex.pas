Unit MaxLogic.FileMutex;

Interface

Uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  sysUtils, classes;

Type
  /// <summary>
  /// you can use THandleStream to read/write from/to the underlying file
  /// </summary>
  TFileMutex = Class
  Private
    FFileHandle: THandle;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Function Open(Const aFileName: String; aTimeOutInMs: Integer): Boolean;
    Procedure Close;

    Property Handle: THandle Read FFileHandle;
  End;

Implementation

Uses
  ioUtils, diagnostics, math;

Function TFileMutex.Open(Const aFileName: String; aTimeOutInMs: Integer): Boolean;
Var
  st: TStopWatch;
  sleepMs: Integer;
Begin
  Close;
  st := TStopWatch.startNew;
  Repeat
    If TFile.Exists(aFileName) Then
      FFileHandle := FileOpen(aFileName, fmOpenReadWrite OR fmShareExclusive)
    else begin
      // chmod is as octal value
      FFileHandle := FileCreate(aFileName, fmOpenReadWrite Or fmShareExclusive, 420);
    end;

    // we do not spin forever, the cpu would not like that. Rather go to sleep after some time
    If (FFileHandle = INVALID_HANDLE_VALUE) And (st.ElapsedMilliseconds < aTimeOutInMs) And (st.ElapsedMilliseconds > 30) Then
    Begin
      sleepMs := min(5, (aTimeOutInMs - st.ElapsedMilliseconds) - 1);
      If sleepMs > 0 Then
        sleep(sleepMs);
    End;

    until (FFileHandle <> INVALID_HANDLE_VALUE) or (st.ElapsedMilliseconds >= aTimeOutInMs);

  Result := (FFileHandle <> INVALID_HANDLE_VALUE);
End;

Destructor TFileMutex.Destroy;
Begin
  Close;
  Inherited;
End;

Constructor TFileMutex.Create;
Begin
  Inherited Create;
  FFileHandle:= INVALID_HANDLE_VALUE;
End;

Procedure TFileMutex.Close;
Begin
  if fFileHandle <> INVALID_HANDLE_VALUE Then
  Begin
    FileClose(FFileHandle);
    FFileHandle := INVALID_HANDLE_VALUE;
  End;
End;

End.
