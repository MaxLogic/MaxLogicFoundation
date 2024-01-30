unit maxSignal;

interface

Uses
  {$IFDEF MSWINDOWS}
  winApi.Windows, vcl.Forms, winApi.Messages, System.UITypes, vcl.dialogs,
  {$ENDIF}
  {$IFDEF LINUX}
  posix.sysTypes,
  posix.pthread,
  {$ENDIF}
  system.Classes, system.SysUtils, system.SyncObjs,
  generics.defaults, generics.Collections;

Type

  iSignal = Interface
    ['{2211FCAA-8365-41C2-8DCF-2534C8610539}']
    Procedure SetSignaled;
    Procedure SetNonSignaled;

    { Return code/values
      TWaitResult = (wrSignaled, wrTimeout, wrAbandoned, wrError, wrIOCompletion);
      (for more details see http://msdn.microsoft.com/en-us/library/windows/desktop/ms687032%28v=vs.85%29.aspx)
      WAIT_ABANDONED -  The specified object is a mutex object that was not released by the thread that owned the mutex object before the owning thread terminated. Ownership of the mutex object is granted to the calling thread and the mutex state is set to nonsignaled. If the mutex was protecting persistent state information, you should check it for consistency.
      WAIT_OBJECT_0 - The state of the specified object is signaled.
      WAIT_TIMEOUT - The function has failed. To get extended error information, call GetLastError.
      WAIT_FAILED -  The function has failed. To get extended error information, call GetLa }
    Function WaitForSignaled(TimeOut: dword = infinite): TWaitResult;
    {$IFDEF MsWindows}
    // note if this is called outside of the main thread, it falls back to the simple WaitFor function, no messages are handled
    Procedure MsgWaitForSignaled(TimeOut: dword = infinite);
    {$ENDIF}
    function GetEvent: TEvent;
    property Event: TEvent read GetEvent;
  End;

  TSignal = Class(tInterfacedObject, iSignal)
  Private
    fEvent: TEvent;
    function GetEvent: TEvent;
  Public
    Constructor Create(aAutoReset: boolean = false);
    Destructor Destroy; Override;

    Procedure SetSignaled;
    Procedure SetNonSignaled;
    Function WaitForSignaled(TimeOut: dword = infinite): TWaitResult;
    {$IFDEF MsWindows}
    // note if this is called outside of the main thread, it falls back to the simple WaitFor function, no messages are handled
    Procedure MsgWaitForSignaled(TimeOut: dword = infinite);
    {$ENDIF}
    property Event: TEvent read GetEvent;
  End;

  // this function will tell us if we are in the main thread or not
Function InsideMainThread: boolean;

{$IFDEF MSWINDOWS}
Procedure MsgWaitForSingleObject(Handle: THandle; TimeOut: dword = infinite);
{$ENDIF}

implementation

uses
  diagnostics;

Function InsideMainThread: boolean;
Begin
  Result :=
  {$IFDEF MSWINDOWS}
    GetCurrentThreadId
  {$ELSE}
    TThread.CurrentThread.ThreadId
  {$ENDIF}
    = MainThreadID;
End;

{$IFDEF MSWINDOWS}


Procedure MsgWaitForSingleObject(Handle: THandle; TimeOut: dword = infinite);
Var
  StopWatch: TStopWatch;
  TimeLeft: dword;
  diff: integer;
Begin
  If Not InsideMainThread Then
  Begin
    WaitForSingleObject(Handle, TimeOut);
    Exit;
  End;

  If TimeOut <> infinite Then
    StopWatch := TStopWatch.StartNew;

  TimeLeft := infinite;

  // this makes sens only if we are in the main vcl thread
  Repeat
    If TimeOut <> infinite Then
    Begin
      diff := TimeOut - StopWatch.ElapsedMilliseconds;
      If diff <= 0 Then
        break;
      TimeLeft := diff;
    End;

    If MsgWaitForMultipleObjects(1, Handle, false,
      TimeLeft, QS_ALLINPUT) =
      WAIT_OBJECT_0 + 1 Then
      Application.ProcessMessages
    Else
      break;
  Until false;
End; // MsgWaitForSingleObject
{$ENDIF}

{ TSignal }

Constructor TSignal.Create(aAutoReset: boolean = false);
Begin
  Inherited Create;
  fEvent := TEvent.Create(

    // A pointer to a SECURITY_ATTRIBUTES structure. If this parameter is NULL, the handle cannot be inherited by child processes.
    Nil,

    // If this parameter is TRUE, the function creates a manual-reset event object, which requires the use of the ResetEvent function to set the event state to nonsignaled. If this parameter is FALSE, the function creates an auto-reset event object, and system automatically resets the event state to nonsignaled after a single waiting thread has been released.
    Not aAutoReset,
    // bInitialState [in]
    false,
    '');

End;

Destructor TSignal.Destroy;
Begin
  fEvent.Free;
  inherited;
End;

function TSignal.GetEvent: TEvent;
begin
  Result := fEvent;
end;

{$IFDEF MsWindows}

procedure TSignal.MsgWaitForSignaled(TimeOut: dword);
begin
  MsgWaitForSingleObject(fEvent.Handle, TimeOut);
end;
{$ENDIF}


Procedure TSignal.SetNonSignaled;
Begin
  fEvent.ResetEvent;
End;

Procedure TSignal.SetSignaled;
Begin
  fEvent.SetEvent;
End;

Function TSignal.WaitForSignaled(TimeOut: dword = infinite): TWaitResult;
Begin
  Result := fEvent.WaitFor(TimeOut);
End;

end.
