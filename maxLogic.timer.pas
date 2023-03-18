Unit maxLogic.timer;

{
  Description:
  The standard VCL TTimer component is not that exact. Especially on intervalls of les then 20ms
  It is also called in the main VCL thread.
  I needed a better timer with threading support.

  Version: 2,7
}
Interface

Uses
  Windows, Messages, SysUtils, Classes;

Type
  { for full details, see: https://docs.microsoft.com/en-us/windows/desktop/api/threadpoollegacyapiset/nf-threadpoollegacyapiset-createtimerqueuetimer
    Creates a timer-queue timer.
    This timer expires at the specified due time, then after every specified period.
    When the timer expires, the callback function is called. }
  TmaxTimer = Class
  Private
    fTimerHandle: THandle;
    fWindowHandle: HWND;
    fDueTime: Cardinal;
    fPeriod: Cardinal;
    fEnabled: Boolean;
    fSynchronized: Boolean;
    fOnTimer: TNotifyEvent;

    Procedure DoOnTimer;

    // usef for syncing
    Procedure WndProc(Var Msg: TMessage);
    Procedure SetEnabled(Value: Boolean);
    Procedure SetSynchronized(Const Value: Boolean);

    Procedure SetDueTime(Const Value: Cardinal);
    Procedure SetPeriod(Const Value: Cardinal);
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure Start;
    Procedure Stop;

    Property enabled: Boolean Read fEnabled Write SetEnabled;

    { The amount of time in milliseconds relative to the current time that must elapse before the timer is signaled for the first time.
      NOTE: has no effect if  the timer is already running. Please restart it for the new values to be applied }
    Property DueTime: Cardinal Read fDueTime Write SetDueTime;

    { The period of the timer, in milliseconds. If this parameter is zero, the timer is signaled once. If this parameter is greater than zero, the timer is periodic. A periodic timer automatically reactivates each time the period elapses, until the timer is canceled.
      NOTE: has no effect if  the timer is already running. Please restart it for the new values to be applied }
    Property Period: Cardinal Read fPeriod Write SetPeriod;

    Property OnTimer: TNotifyEvent Read fOnTimer Write fOnTimer;

    { The OnTimer event is called in a separate thread, if you  really need to synchronize set Synchronized to true }
    Property Synchronized: Boolean Read fSynchronized Write SetSynchronized;
  End;

Type
  TWaitOrTimerCallback = Procedure(lpParameter: Pointer; TimerOrWaitFired: Boolean); Stdcall;

  // declare the methods from the kernel32 .dll
Function CreateTimerQueueTimer(
  { A pointer to a buffer that receives a handle to the timer-queue timer on return. When this handle has expired and is no longer required, release it by calling DeleteTimerQueueTimer. }
  Var timer: THandle;
  { A handle to the timer queue. This handle is returned by the CreateTimerQueue function.
    If this parameter is NULL, the timer is associated with the default timer queue. }
  TimerQueue: THandle;
  Callback: TWaitOrTimerCallback;
  Parameter: Pointer;
  { The amount of time in milliseconds relative to the current time that must elapse before the timer is signaled for the first time. }
  DueTime: LongWord;
  { The period of the timer, in milliseconds. If this parameter is zero, the timer is signaled once. If this parameter is greater than zero, the timer is periodic. A periodic timer automatically reactivates each time the period elapses, until the timer is canceled. }
  Period: LongWord;
  Flags: LongWord
  ): BOOL; Stdcall;

Function DeleteTimerQueueTimer(
  TimerQueue: THandle;
  timer: THandle;
  CompletionEvent: THandle
  ): BOOL; Stdcall;

Implementation

Uses
  forms;

Const
  wm_TimerEvent = WM_APP + 1;

Function CreateTimerQueueTimer; External kernel32 Name 'CreateTimerQueueTimer';
Function DeleteTimerQueueTimer; External kernel32 Name 'DeleteTimerQueueTimer';

Procedure TimerCallback(timer: TmaxTimer; TimerOrWaitFired: Boolean); Stdcall;
Begin
  If timer.fSynchronized Then
  Begin
      PostMessage(timer.fWindowHandle, wm_TimerEvent, 0, 0)
  End
  Else
    timer.DoOnTimer;
End;

{ TmaxTimer }

Constructor TmaxTimer.Create;
Begin
  Inherited;

  fDueTime := 1000;
  fPeriod := 1000;
End;

Destructor TmaxTimer.Destroy;
Begin
  SetEnabled(False);
  SetSynchronized(False);
  Inherited;
End;

Procedure TmaxTimer.WndProc(Var Msg: TMessage);
Begin
  With Msg Do
  Begin
    If Msg <> wm_TimerEvent Then
      Result := DefWindowProc(fWindowHandle, Msg, wParam, lParam)
    Else
    Begin
      Try
        DoOnTimer;
      Except
        Application.HandleException(Self);
      End;
    End;
  End;
End;

Procedure TmaxTimer.SetEnabled(Value: Boolean);
Begin
  If Value <> fEnabled Then
  Begin
    If Value Then
    Begin
        fEnabled := CreateTimerQueueTimer(fTimerHandle, 0, @TimerCallback, Self,
        fDueTime, fPeriod, 0);
    End
    Else Begin
        DeleteTimerQueueTimer(0, fTimerHandle, 0);
      fEnabled := False;
    End;
  End;
End;

Procedure TmaxTimer.SetDueTime(Const Value: Cardinal);
Begin
  fDueTime := Value;
End;

Procedure TmaxTimer.SetPeriod(Const Value: Cardinal);
Begin
  fPeriod := Value;
End;

Procedure TmaxTimer.SetSynchronized(Const Value: Boolean);
Begin
  If fSynchronized <> Value Then
  Begin
      fSynchronized := Value;
    If Value Then
      fWindowHandle := Classes.AllocateHWnd(WndProc)
    Else
      Classes.DeallocateHWnd(fWindowHandle);
  End;
End;

Procedure TmaxTimer.Start;
Begin
  SetEnabled(true);
End;

Procedure TmaxTimer.Stop;
Begin
  SetEnabled(False);
End;

Procedure TmaxTimer.DoOnTimer;
Begin
  If Assigned(fOnTimer) Then
    fOnTimer(Self);
End;

End.
