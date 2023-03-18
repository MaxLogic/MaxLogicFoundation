Unit maxLogic.WndMsgReceiver;

Interface

Uses
  windows, classes, sysUtils;

Type
  TMsgReceiver = Class(TObject)
  protected
    fWnd: HWND;
    Procedure WndMethod(Var Msg: TMessage); virtual;
  Public
    Constructor Create;
    Destructor Destroy; Override;
  End;

Implementation

Constructor TMsgReceiver.Create;
Begin
  Inherited Create;

  fWnd := AllocateHWnd(WndMethod);
End;

Destructor TMsgReceiver.Destroy;
Begin
  DeallocateHWnd(fWnd);
  Inherited;
End;

Procedure TMsgReceiver.WndMethod(Var Msg: TMessage);
Begin
  {If Msg.Msg = WM_MY_UNIQUE_MESSAGE Then
  Begin
    // do something
  End
  Else}
    Msg.Result := DefWindowProc(fWnd, Msg.Msg, Msg.wParam, Msg.lParam);
End;

End.
