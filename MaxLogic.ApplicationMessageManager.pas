unit MaxLogic.ApplicationMessageManager;

{
  Version: 1;.2
  how to use:
  for auto scrolling of scrollboxes do the following:

  fRedirectMouseWheel := TAppMessagehandlerRedirectMouseWheel.Create;

  // no registration is needed, as the handlers will auto register themselves
}

interface

uses
  winApi.windows, system.classes, vcl.controls, system.sysUtils, messages, vcl.forms, generics.collections;

type
  // forward declarations
  TApplicationMessageHandler = class;
  TApplicationMessageManager = class;

  // singelton, there may onle be one. it is attached to the application.onmessage event
  TApplicationMessageManager = class
  private
    class var fInstance: TApplicationMessageManager;
  private
    FAppMessageHandlers: TList<TApplicationMessageHandler>;
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
    class function GetSingeltonInstance: TApplicationMessageManager; static;
    class Destructor DestroyClass;

    class Procedure RegisterHandler(aHandler: TApplicationMessageHandler);
    class Procedure UnRegisterHandler(aHandler: TApplicationMessageHandler);
  public
    Constructor Create;
    Destructor Destroy; override;

    class function Receivername(wnd: HWnd): string;
    property AppMessageHandlers: TList<TApplicationMessageHandler> read FAppMessageHandlers;
    class property instance: TApplicationMessageManager read GetSingeltonInstance;
  end;

  // is the base class for all the handlers
  TApplicationMessageHandler = class
  private
    fParent: TApplicationMessageManager;
  PROTECTED
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean); VIRTUAL; ABSTRACT;
    procedure MoveToTopParent(var f: TForm);
  public
    constructor Create;
    Destructor Destroy; override;
    class function MessageToText(aMsg: Cardinal): string;

    property Parent: TApplicationMessageManager read fParent;
  end;

  TAppMessagehandlerRedirectMouseWheel = class(TApplicationMessageHandler)
  private
    FAutoScrollScrollBoxes: Boolean;
    procedure SetAutoScrollScrollBoxes(const Value: Boolean);
  private
    procedure DoScroll(sb: TScrollbox; var Msg: TMsg);
  PROTECTED
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean); override;
  public
    constructor Create;
    property AutoScrollScrollBoxes: Boolean read FAutoScrollScrollBoxes write SetAutoScrollScrollBoxes;
  end;

  TAppMessageHandlerLogKey = class(TApplicationMessageHandler)
  PROTECTED
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean); override;
    procedure ProcessKeyMessage(var Msg: TMsg; var Handled: Boolean); virtual;

  public
    class function DisplayKeyMsg(const Msg: TMsg): string;
    class function KeyName(Keydata: longint): string;

    // Keydata is the Msg.lparam
    class function RepeatCount(Keydata: lparam): lparam;
    // lParam is a nativeInt
    class function Scancode(Keydata: lparam): lparam;
    class function Flags(Keydata: lparam): string;
  end;

  THybernateAppMessageHandler = class(TApplicationMessageHandler)
  private
    fOnGoToHybernate, fOnWakeUpFromHybernate: TNotifyEvent;
  PROTECTED
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean); override;
  public
    constructor Create(aOnGoToHybernate, aOnWakeUpFromHybernate: TNotifyEvent);
  end;

implementation

{ TApplicationMessageManager }

procedure TApplicationMessageManager.AppMessage(var Msg: TMsg; var Handled: Boolean);
var
  H: TApplicationMessageHandler;
begin
  for H in FAppMessageHandlers do
  begin
    H.AppMessage(Msg, Handled);
    if Handled then
      BREAK;
  end;
end;

class function TApplicationMessageManager.Receivername(wnd: HWnd): string;
var
  ctrl: TWinControl;
begin
  ctrl := FindControl(wnd);
  if Assigned(ctrl) then
    Result := ctrl.Name
  else
    Result := IntToHex(Cardinal(wnd), 8);
end;

class procedure TApplicationMessageManager.RegisterHandler(aHandler: TApplicationMessageHandler);
begin
  aHandler.fParent := GetSingeltonInstance;
  GetSingeltonInstance.FAppMessageHandlers.add(aHandler);
end;

class procedure TApplicationMessageManager.UnRegisterHandler(aHandler: TApplicationMessageHandler);
var
  i: Integer;
begin
  i := fInstance.FAppMessageHandlers.indexof(aHandler);
  if i <> -1 then
    fInstance.FAppMessageHandlers.delete(i);
end;

constructor TApplicationMessageManager.Create;
begin
  inherited Create;
  FAppMessageHandlers := TList<TApplicationMessageHandler>.Create;
  Application.OnMessage := AppMessage;
end;

destructor TApplicationMessageManager.Destroy;
begin
  Application.OnMessage := nil;
  FAppMessageHandlers.free;
  inherited;
end;

class destructor TApplicationMessageManager.DestroyClass;
begin
  if Assigned(fInstance) then
    FreeAndNIL(fInstance);
end;

class function TApplicationMessageManager.GetSingeltonInstance: TApplicationMessageManager;
begin
  if not Assigned(fInstance) then
    fInstance := TApplicationMessageManager.Create;

  Result := fInstance;
end;

{ TAppMessageHandlerLogKey }

procedure TAppMessageHandlerLogKey.AppMessage(var Msg: TMsg; var Handled: Boolean);
begin
  // the only messages that we are interesting in are the key events.
  case Msg.message of
    WM_KEYFIRST .. WM_KEYLAST:
      ProcessKeyMessage(Msg, Handled);
    // end of case
  end;
end;

{ : Display the information contained in the message parameters for a key message.
  @param Msg is the original message record.
  @desc See the MS Platform SDK docs for one of the key messsages for a description of the message parameters. The decoding of the individual parts is left to helper methods. }
class function TAppMessageHandlerLogKey.DisplayKeyMsg(const Msg: TMsg): string;
var
  PWmKey: ^TWMKey;

  { : Returns the key label, usually matching the text shown on the key caps, or the character created by the key. }
  function KeyAsstring: string;
  begin
    case Msg.message of
      WM_KEYDOWN, WM_KEYUP, WM_SYSKEYDOWN, WM_SYSKEYUP:
        Result := KeyName(PWmKey^.Keydata);
    else
      Result := Chr(PWmKey^.CharCode);
    end; { case }
  end;

{ : Returns a prefix to use in the display string for key or character messages. }
  function CharOrKey: string;
  begin
    case Msg.message of
      WM_KEYDOWN, WM_KEYUP, WM_SYSKEYDOWN, WM_SYSKEYUP:
        Result := 'Key';
    else
      Result := 'Char';
    end; { case }
  end;

var
  ScancodeStr: string;
begin
  Result := '';
  PWmKey := @Msg.message;

  Try
    ScancodeStr := '$' + IntToHex(Scancode(Msg.lparam), 1);
  except
    ScancodeStr := '[$' + IntToHex(Msg.lparam, 1) + ']';
  end;

  Result := format(
    'Message: %s, Receiver: %s, %scode: $%x (%s), ' +
    'Repeat count %d, Scancode %s, Flags: %s',

    [MessageToText(Msg.message),
    TApplicationMessageManager.Receivername(Msg.HWnd),
    CharOrKey,
    PWmKey^.CharCode,
    KeyAsstring,
    RepeatCount(Msg.lparam),
    ScancodeStr,
    Flags(Msg.lparam)]);
end;

class function TAppMessageHandlerLogKey.Flags(Keydata: lparam): string;
var
  FlagByte: Byte;
  sl: TStringlist;
begin
{$R-}{$Q-}
  FlagByte := Hi(Hiword(Keydata));
  if FlagByte <> 0 then
  begin
    sl := TStringlist.Create;
    try
      { Bit 0 identifies an extended key like the right Ctrl and Alt }
      if (FlagByte and 1) <> 0 then
        sl.add('ExtendedKey');
      { Bit 5 is set when the Alt key is down }
      if (FlagByte and $20) <> 0 then
        sl.add('AltDown');
      { Bit 6 is the previous state of the key, down (0) or up (1) }
      if (FlagByte and $40) <> 0 then
        sl.add('KeyIsDown');
      { Bit 7 is the transition state, 0 if the key went down, 1 if it
        went up. }
      if (FlagByte and $80) <> 0 then
        sl.add('KeyUp');
      Result := format('[%s]', [sl.Commatext]);
    finally
      sl.free;
    end; { finally }
  end { if }
  else
    Result := '[]';
{$R+}{$Q+}
end;

class function TAppMessageHandlerLogKey.KeyName(Keydata: Integer): string;
var
  Buffer: array [0 .. 64] of char;
begin
  Buffer[0] := #0;
  GetKeyNameText(Keydata, Buffer, Sizeof(Buffer));
  Result := Buffer;
end;

procedure TAppMessageHandlerLogKey.ProcessKeyMessage(var Msg: TMsg; var Handled: Boolean);
var
  s: string;
begin
  s := DisplayKeyMsg(Msg);
end;

class function TAppMessageHandlerLogKey.RepeatCount(Keydata: lparam): lparam;
begin
  Result := Keydata and $FFFF;
end;

class function TAppMessageHandlerLogKey.Scancode(Keydata: lparam): lparam;
begin
{$R-}{$Q-}
  Result := Hiword(Keydata) and $FF;
{$R+}{$Q+}
end;

{ TApplicationMessageHandler }

destructor TApplicationMessageHandler.Destroy;
begin
  if fParent <> nil then
  begin
    fParent.UnRegisterHandler(self);
    fParent := nil;
  end;
  inherited;
end;

class function TApplicationMessageHandler.MessageToText(aMsg: Cardinal): string;

{ : Convert a message constant to a display string. We only expect key and mouse messages here, anything else will just return the message constants value as a hexadecimal representation. }
const
  WM_XBUTTONDOWN = $020B;
  WM_XBUTTONUP = $020C;
  WM_XBUTTONDBLCLK = $020D;
begin
  case aMsg of
    WM_COMMAND:
      Result := 'wm_COMMAND';
    WM_SYSCOMMAND:
      Result := 'WM_SYSCOMMAND';
    WM_KEYDOWN:
      Result := 'WM_KEYDOWN';
    WM_SYSKEYDOWN:
      Result := 'WM_SYSKEYDOWN';
    WM_CHAR:
      Result := 'WM_CHAR';
    WM_SYSCHAR:
      Result := 'WM_SYSCHAR';
    WM_KEYUP:
      Result := 'WM_KEYUP';
    WM_SYSKEYUP:
      Result := 'WM_SYSKEYUP';
    WM_SYSDEADCHAR:
      Result := 'WM_SYSDEADCHAR';
    WM_DEADCHAR:
      Result := 'WM_DEADCHAR';
    WM_UNICHAR:
      Result := 'WM_UNICHAR';
    WM_NCMOUSEMOVE:
      Result := 'WM_NCMOUSEMOVE';
    WM_NCLBUTTONDOWN:
      Result := 'WM_NCLBUTTONDOWN';
    WM_NCLBUTTONUP:
      Result := 'WM_NCLBUTTONUP';
    WM_NCLBUTTONDBLCLK:
      Result := 'WM_NCLBUTTONDBLCLK';
    WM_NCRBUTTONDOWN:
      Result := 'WM_NCRBUTTONDOWN';
    WM_NCRBUTTONUP:
      Result := 'WM_NCRBUTTONUP';
    WM_NCRBUTTONDBLCLK:
      Result := 'WM_NCRBUTTONDBLCLK';
    WM_NCMBUTTONDOWN:
      Result := 'WM_NCMBUTTONDOWN';
    WM_NCMBUTTONUP:
      Result := 'WM_NCMBUTTONUP';
    WM_NCMBUTTONDBLCLK:
      Result := 'WM_NCMBUTTONDBLCLK';
    WM_NCXBUTTONDOWN:
      Result := 'WM_NCXBUTTONDOWN';
    WM_NCXBUTTONUP:
      Result := 'WM_NCXBUTTONUP';
    WM_NCXBUTTONDBLCLK:
      Result := 'WM_NCXBUTTONDBLCLK';
    WM_MOUSEMOVE:
      Result := 'WM_MOUSEMOVE';
    WM_LBUTTONDOWN:
      Result := 'WM_LBUTTONDOWN';
    WM_LBUTTONUP:
      Result := 'WM_LBUTTONUP';
    WM_LBUTTONDBLCLK:
      Result := 'WM_LBUTTONDBLCLK';
    WM_RBUTTONDOWN:
      Result := 'WM_RBUTTONDOWN';
    WM_RBUTTONUP:
      Result := 'WM_RBUTTONUP';
    WM_RBUTTONDBLCLK:
      Result := 'WM_RBUTTONDBLCLK';
    WM_MBUTTONDOWN:
      Result := 'WM_MBUTTONDOWN';
    WM_MBUTTONUP:
      Result := 'WM_MBUTTONUP';
    WM_MBUTTONDBLCLK:
      Result := 'WM_MBUTTONDBLCLK';
    WM_MOUSEWHEEL:
      Result := 'WM_MOUSEWHEEL';
    WM_XBUTTONDOWN:
      Result := 'WM_XBUTTONDOWN';
    WM_XBUTTONUP:
      Result := 'WM_XBUTTONUP';
    WM_XBUTTONDBLCLK:
      Result := 'WM_XBUTTONDBLCLK';
    WM_MOUSEHOVER:
      Result := 'WM_MOUSEHOVER';
    WM_MOUSELEAVE:
      Result := 'WM_MOUSELEAVE';
    WM_NCMOUSEHOVER:
      Result := 'WM_NCMOUSEHOVER';
    WM_NCMOUSELEAVE:
      Result := 'WM_NCMOUSELEAVE';
    WM_TIMER:
      Result := 'WM_TIMER';
    WM_GETDLGCODE:
      Result := 'WM_GETDLGCODE';
    CM_ACTIVATE:
      Result := 'CM_ACTIVATE';
    CM_DEACTIVATE:
      Result := 'CM_DEACTIVATE';
    CM_GOTFOCUS:
      Result := 'CM_GOTFOCUS';
    CM_LOSTFOCUS:
      Result := 'CM_LOSTFOCUS';
    CM_CANCELMODE:
      Result := 'CM_CANCELMODE';
    CM_DIALOGKEY:
      Result := 'CM_DIALOGKEY';
    CM_DIALOGCHAR:
      Result := 'CM_DIALOGCHAR';
    CM_FOCUSCHANGED:
      Result := 'CM_FOCUSCHANGED';
    CM_PARENTFONTCHANGED:
      Result := 'CM_PARENTFONTCHANGED';
    CM_PARENTCOLORCHANGED:
      Result := 'CM_PARENTCOLORCHANGED';
    CM_HITTEST:
      Result := 'CM_HITTEST';
    CM_VISIBLECHANGED:
      Result := 'CM_VISIBLECHANGED';
    CM_ENABLEDCHANGED:
      Result := 'CM_ENABLEDCHANGED';
    CM_COLORCHANGED:
      Result := 'CM_COLORCHANGED';
    CM_FONTCHANGED:
      Result := 'CM_FONTCHANGED';
    CM_CURSORCHANGED:
      Result := 'CM_CURSORCHANGED';
    CM_CTL3DCHANGED:
      Result := 'CM_CTL3DCHANGED';
    CM_PARENTCTL3DCHANGED:
      Result := 'CM_PARENTCTL3DCHANGED';
    CM_TEXTCHANGED:
      Result := 'CM_TEXTCHANGED';
    CM_MOUSEENTER:
      Result := 'CM_MOUSEENTER';
    CM_MOUSELEAVE:
      Result := 'CM_MOUSELEAVE';
    CM_MENUCHANGED:
      Result := 'CM_MENUCHANGED';
    CM_APPKEYDOWN:
      Result := 'CM_APPKEYDOWN';
    CM_APPSYSCOMMAND:
      Result := 'CM_APPSYSCOMMAND';
    CM_BUTTONPRESSED:
      Result := 'CM_BUTTONPRESSED';
    CM_SHOWINGCHANGED:
      Result := 'CM_SHOWINGCHANGED';
    CM_ENTER:
      Result := 'CM_ENTER';
    CM_EXIT:
      Result := 'CM_EXIT';
    CM_DESIGNHITTEST:
      Result := 'CM_DESIGNHITTEST';
    CM_ICONCHANGED:
      Result := 'CM_ICONCHANGED';
    CM_WANTSPECIALKEY:
      Result := 'CM_WANTSPECIALKEY';
    CM_INVOKEHELP:
      Result := 'CM_INVOKEHELP';
    CM_WINDOWHOOK:
      Result := 'CM_WINDOWHOOK';
    CM_RELEASE:
      Result := 'CM_RELEASE';
    CM_SHOWHINTCHANGED:
      Result := 'CM_SHOWHINTCHANGED';
    CM_PARENTSHOWHINTCHANGED:
      Result := 'CM_PARENTSHOWHINTCHANGED';
    CM_SYSCOLORCHANGE:
      Result := 'CM_SYSCOLORCHANGE';
    CM_WININICHANGE:
      Result := 'CM_WININICHANGE';
    CM_FONTCHANGE:
      Result := 'CM_FONTCHANGE';
    CM_TIMECHANGE:
      Result := 'CM_TIMECHANGE';
    CM_TABSTOPCHANGED:
      Result := 'CM_TABSTOPCHANGED';
    CM_UIACTIVATE:
      Result := 'CM_UIACTIVATE';
    CM_UIDEACTIVATE:
      Result := 'CM_UIDEACTIVATE';
    CM_DOCWINDOWACTIVATE:
      Result := 'CM_DOCWINDOWACTIVATE';
    CM_CONTROLLISTCHANGE:
      Result := 'CM_CONTROLLISTCHANGE';
    CM_GETDATALINK:
      Result := 'CM_GETDATALINK';
    CM_CHILDKEY:
      Result := 'CM_CHILDKEY';
    CM_DRAG:
      Result := 'CM_DRAG';
    CM_HINTSHOW:
      Result := 'CM_HINTSHOW';
    CM_DIALOGHANDLE:
      Result := 'CM_DIALOGHANDLE';
    CM_ISTOOLCONTROL:
      Result := 'CM_ISTOOLCONTROL';
    CM_RECREATEWND:
      Result := 'CM_RECREATEWND';
    CM_INVALIDATE:
      Result := 'CM_INVALIDATE';
    CM_SYSFONTCHANGED:
      Result := 'CM_SYSFONTCHANGED';
    CM_CONTROLCHANGE:
      Result := 'CM_CONTROLCHANGE';
    CM_CHANGED:
      Result := 'CM_CHANGED';
    CM_DOCKCLIENT:
      Result := 'CM_DOCKCLIENT';
    CM_UNDOCKCLIENT:
      Result := 'CM_UNDOCKCLIENT';
    CM_FLOAT:
      Result := 'CM_FLOAT';
    CM_BORDERCHANGED:
      Result := 'CM_BORDERCHANGED';
    CM_BIDIMODECHANGED:
      Result := 'CM_BIDIMODECHANGED';
    CM_PARENTBIDIMODECHANGED:
      Result := 'CM_PARENTBIDIMODECHANGED';
    CM_ALLCHILDRENFLIPPED:
      Result := 'CM_ALLCHILDRENFLIPPED';
    CM_ACTIONUPDATE:
      Result := 'CM_ACTIONUPDATE';
    CM_ACTIONEXECUTE:
      Result := 'CM_ACTIONEXECUTE';
    CM_HINTSHOWPAUSE:
      Result := 'CM_HINTSHOWPAUSE';
    CM_DOCKNOTIFICATION:
      Result := 'CM_DOCKNOTIFICATION';
    CM_MOUSEWHEEL:
      Result := 'CM_MOUSEWHEEL';
    CM_ISSHORTCUT:
      Result := 'CM_ISSHORTCUT';
    CM_INVALIDATEDOCKHOST:
      Result := 'CM_INVALIDATEDOCKHOST';
    CM_SETACTIVECONTROL:
      Result := 'CM_SETACTIVECONTROL';
    CM_POPUPHWNDDESTROY:
      Result := 'CM_POPUPHWNDDESTROY';
    CM_CREATEPOPUP:
      Result := 'CM_CREATEPOPUP';
    CM_DESTROYHANDLE:
      Result := 'CM_DESTROYHANDLE';
    CM_MOUSEACTIVATE:
      Result := 'CM_MOUSEACTIVATE';
    CM_CONTROLLISTCHANGING:
      Result := 'CM_CONTROLLISTCHANGING';
    CM_BUFFEREDPRINTCLIENT:
      Result := 'CM_BUFFEREDPRINTCLIENT';
    CM_UNTHEMECONTROL:
      Result := 'CM_UNTHEMECONTROL';
    CN_CHARTOITEM:
      Result := 'CN_CHARTOITEM';
    CN_COMMAND:
      Result := 'CN_COMMAND';
    CN_COMPAREITEM:
      Result := 'CN_COMPAREITEM';
    CN_CTLCOLORBTN:
      Result := 'CN_CTLCOLORBTN';
    CN_CTLCOLORDLG:
      Result := 'CN_CTLCOLORDLG';
    CN_CTLCOLOREDIT:
      Result := 'CN_CTLCOLOREDIT';
    CN_CTLCOLORLISTBOX:
      Result := 'CN_CTLCOLORLISTBOX';
    CN_CTLCOLORMSGBOX:
      Result := 'CN_CTLCOLORMSGBOX';
    CN_CTLCOLORSCROLLBAR:
      Result := 'CN_CTLCOLORSCROLLBAR';
    CN_CTLCOLORSTATIC:
      Result := 'CN_CTLCOLORSTATIC';
    CN_DELETEITEM:
      Result := 'CN_DELETEITEM';
    CN_DRAWITEM:
      Result := 'CN_DRAWITEM';
    CN_HSCROLL:
      Result := 'CN_HSCROLL';
    CN_MEASUREITEM:
      Result := 'CN_MEASUREITEM';
    CN_PARENTNOTIFY:
      Result := 'CN_PARENTNOTIFY';
    CN_VKEYTOITEM:
      Result := 'CN_VKEYTOITEM';
    CN_VSCROLL:
      Result := 'CN_VSCROLL';
    CN_KEYDOWN:
      Result := 'CN_KEYDOWN';
    CN_KEYUP:
      Result := 'CN_KEYUP';
    CN_CHAR:
      Result := 'CN_CHAR';
    CN_SYSKEYDOWN:
      Result := 'CN_SYSKEYDOWN';
    CN_SYSCHAR:
      Result := 'CN_SYSCHAR';
    CN_NOTIFY:
      Result := 'CN_NOTIFY';
  else
    Result := format('$%4.4X', [aMsg]);
  end; { case }
end;

procedure TApplicationMessageHandler.MoveToTopParent(var f: TForm);
var
  wc: TWinControl;
begin
  wc := f;
  while wc.Parent <> nil do
  begin
    wc := wc.Parent;

    if wc is TForm then
      f := wc as TForm;
  end;

end;

constructor TApplicationMessageHandler.Create;
begin
  inherited Create;
  TApplicationMessageManager.RegisterHandler(self);
end;

{ TAppMessagehandlerRedirectMouseWheel }

procedure TAppMessagehandlerRedirectMouseWheel.AppMessage(var Msg: TMsg; var Handled: Boolean);
var
  form: TForm;
  FirstIteration, res: Boolean;
  s: string;
  control: TControl;
begin
  // mouse wheel scrolling for the control under the mouse
  if not AutoScrollScrollBoxes then
    exit;

  if Msg.message <> WM_MOUSEWHEEL then
    exit;

  form := screen.ActiveForm;
  if form = nil then
  begin
    Handled := true;
    exit;
  end;

  MoveToTopParent(form);

  if (form.activecontrol = nil) or (form.activecontrol.Handle <> Msg.HWnd) then
  begin
    // ControlAtPos() gets the immediate child, so if the control is in panel, it returns the panel.
    // FindVCLWindow(Mouse.CursorPos) returns the correct control.
    // control := form.ControlAtPos(form.ScreenToClient(MousePos), False, True, True);
    control := FindVCLWindow(Mouse.CursorPos);

    if control <> nil then
    begin
      Handled := control.Perform(CM_MOUSEWHEEL, Msg.WParam, Msg.lparam) = 0;

      // go up to find a TScrollBox
      while control <> nil do
      begin
        if control is TScrollbox then
        begin
          DoScroll(control as TScrollbox, Msg);
          Handled := true;
          BREAK;
        end
        else
          control := control.Parent;
      end;
    end;
  end;

end;

constructor TAppMessagehandlerRedirectMouseWheel.Create;
begin
  inherited;
  FAutoScrollScrollBoxes := true;
end;

procedure TAppMessagehandlerRedirectMouseWheel.DoScroll(sb: TScrollbox; var Msg: TMsg);
var
  WheelDelta: ShortInt;
  i, NewScrollPos: Integer;
begin
{$R-}{$Q-}
  WheelDelta := Hiword(Msg.WParam);
{$R+}{$Q+}
  for i := 1 to Mouse.WheelScrollLines do
    try
      if WheelDelta > 0 then
        sb.Perform(WM_VSCROLL, SB_LINEUP, 0)
      else
        sb.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
    finally
      sb.Perform(WM_VSCROLL, SB_ENDSCROLL, 0);
    end;

end;

procedure TAppMessagehandlerRedirectMouseWheel.SetAutoScrollScrollBoxes(const Value: Boolean);
begin
  FAutoScrollScrollBoxes := Value;
end;

{ THybernateAppMessageHandler }

constructor THybernateAppMessageHandler.Create(aOnGoToHybernate,
  aOnWakeUpFromHybernate: TNotifyEvent);
begin
  inherited Create;
  fOnGoToHybernate := aOnGoToHybernate;
  fOnWakeUpFromHybernate := aOnWakeUpFromHybernate;
end;

procedure THybernateAppMessageHandler.AppMessage(var Msg: TMsg;
  var Handled: Boolean);
begin
  if Msg.message = WM_POWERBROADCAST then
  begin
    case Msg.WParam of
      PBT_APMSUSPEND:
        if Assigned(fOnGoToHybernate) then
          fOnGoToHybernate(self);

      PBT_APMRESUMESUSPEND:
        if Assigned(fOnWakeUpFromHybernate) then
          fOnWakeUpFromHybernate(self);
    end;

  end;
end;

end.
