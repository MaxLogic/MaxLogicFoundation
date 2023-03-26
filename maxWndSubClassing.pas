unit maxWndSubClassing;

{ version: 2.2
  History:
  2016-02-09: added an Ex version of that class
  2014-02-25: the procedure defHandler is now public }
interface

uses
  windows, classes, Messages, SysUtils, forms;

type
  TOnMessageEvent = procedure(var Msg: Messages.TMessage; var PrevendDefaultHandler: boolean) of Object;

  TWndSubClass = class(TComponent)
  private
    wnd: hwnd;
    OldWndProc: TFarProc;
    NewWndProc: TFarProc;
    FOnMessage: TOnMessageEvent;
    function GetHookedWnd: hwnd;
    procedure SetHookedWnd(const Value: hwnd);
    procedure SetOnMessage(const Value: TOnMessageEvent);

    procedure NewWndMethod(var Msg: TMessage);
    procedure FreeHook;
  protected
    procedure InternHandler(var Msg: TMessage; var PreventDefaultHandler: boolean); virtual;
  public
    Constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // attention do not call this if you do not know what your are doing.
    // it is intendet to be called manually if you prevent in your OnMessage event the orginal handler, but anyway want to call it.
    procedure DefHandler(var Msg: TMessage);

  published
    property OnMessage: TOnMessageEvent read FOnMessage write SetOnMessage;
    property HookedWnd: hwnd read GetHookedWnd write SetHookedWnd;
  end;

TWndSubClassEx < TData >= class(TWndSubClass)
  private type
  TOnMessageEventEx =
procedure(Sender: TWndSubClassEx<TData>; var Msg: Messages.TMessage; var PrevendDefaultHandler: boolean) of Object;
private
  FOnMessageEx: TOnMessageEventEx;
  FData: TData;
  procedure SetData(const Value: TData);
  procedure SetOnMessageEx(const Value: TOnMessageEventEx);

protected
  property OnMessage;
  procedure InternOnMessage(var Msg: Messages.TMessage; var PrevendDefaultHandler: boolean);
public
  Constructor Create(AOwner: TComponent);
  override;

  property Data: TData read FData write SetData;
  property OnMessageEx: TOnMessageEventEx read FOnMessageEx write SetOnMessageEx;
  end;

implementation

{ TWndSubClass }

constructor TWndSubClass.Create;
begin
  inherited;
  wnd := 0;
end;

destructor TWndSubClass.Destroy;
begin

  FreeHook;
  inherited;
end;

procedure TWndSubClass.FreeHook;
begin
  if Assigned(NewWndProc) then
  begin
    SetWindowLong(wnd, gwl_WndProc, longint(OldWndProc));
    FreeObjectInstance(NewWndProc);
    NewWndProc := NIL;
  end;
end;

function TWndSubClass.GetHookedWnd: hwnd;
begin
  result := wnd;
end;

procedure TWndSubClass.InternHandler(var Msg: TMessage;
  var PreventDefaultHandler: boolean);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Msg, PreventDefaultHandler);
end;

procedure TWndSubClass.NewWndMethod(var Msg: TMessage);
var
  PreventDefaultHandler: boolean;
begin
  PreventDefaultHandler := false;
  InternHandler(Msg, PreventDefaultHandler);
  if not PreventDefaultHandler then
    DefHandler(Msg);
end;

procedure TWndSubClass.SetHookedWnd(const Value: hwnd);
begin
  if Value = wnd then
    exit;
  if wnd <> 0 then
    FreeHook;

  wnd := Value;

  if wnd <> 0 then
  begin
    NewWndProc := MakeObjectInstance(NewWndMethod);
    OldWndProc := Pointer(GetWindowLong(wnd, gwl_WndProc));
    SetWindowLong(wnd, gwl_WndProc, longint(NewWndProc));
  end;
end;

procedure TWndSubClass.SetOnMessage(const Value: TOnMessageEvent);
begin
  FOnMessage := Value;
end;

procedure TWndSubClass.DefHandler(var Msg: TMessage);
begin
  Msg.result := CallWindowProc(OldWndProc, wnd, Msg.Msg, Msg.wParam, Msg.lParam);
end;

{ TWndSubClassEx<TData> }

procedure TWndSubClassEx<TData>.SetData(const Value: TData);
begin
  FData := Value;
end;

procedure TWndSubClassEx<TData>.SetOnMessageEx(
  const Value: TOnMessageEventEx);
begin
  FOnMessageEx := Value;
end;

constructor TWndSubClassEx<TData>.Create(AOwner: TComponent);
begin
  inherited;
  self.FOnMessage := InternOnMessage;
end;

procedure TWndSubClassEx<TData>.InternOnMessage(var Msg: Messages.TMessage;
  var PrevendDefaultHandler: boolean);
begin
  if Assigned(FOnMessageEx) then
    FOnMessageEx(self, Msg, PrevendDefaultHandler);
end;

end.

