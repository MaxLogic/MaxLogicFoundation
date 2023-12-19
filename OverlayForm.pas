unit OverlayForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Vcl.StdCtrls, Vcl.Buttons, maxWndSubClassing;

type
  TOffsetBy = (ofTopLeft, ofTopRight, ofBottomLeft, ofBottomRight);
  TOffsetTo = (otClientRect, otNormalRect);

  TOverlayFrm = class(TForm)
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message wm_NCHitTest;
    procedure FormCreate(Sender: TObject);
  private type
    TData = record
      PreventMousehandler: boolean;
      WC: TWinControl;
    end;
  private
    FOffsetToControl: TControl;
    FOffsetBy: TOffsetBy;
    FOffsetPosition: TPoint;
    FAlpha: double;
    FVisible: boolean;
    FOffsetTo: TOffsetTo;
    procedure OnSubClassMessage(Sender: TWndSubClassEx<TData>; var Msg: Messages.TMessage; var PrevendDefaultHandler: boolean);
    procedure SetOffsetBy(const Value: TOffsetBy);
    procedure SetOffsetPosition(const Value: TPoint);
    procedure SetOffsetToControl(const Value: TControl);
    procedure SetAlpha(const Value: double);
    procedure SetVisible(const Value: boolean);
    procedure SetOffsetTo(const Value: TOffsetTo);

  protected
    procedure Paint; override;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMDisplayChange(var Message: TMessage); message WM_DISPLAYCHANGE;
  public
    constructor CreateOverlay(AForm: TForm);
    destructor Destroy; override;

    // the form should be non focuseable, but some controls like TBitBtn always result in focussing of the form when clicked, so we can prevent this to happen here:
    Procedure RegisterControl(c: TWinControl; PreventMousehandler: boolean);
    // if you for some reasons need to move the control from this form or to enable it again, then call this function
    Procedure UnRegisterControl(c: TWinControl);

    // need to be called from the parent form OnFormresize and
    // procedure WMMove(var Message: TWMMove); message WM_MOVE;
    // do not forgett to call iinherited inside of WMMove
    procedure UpdateOverlay;

    Procedure ShowWithoutFocussing;

    // you can stick this form to a control.
    // OffsetBy determines which corner is used for both
    // OffsetPosition are the px by which the overlay should offset Use positive values to place the overlay inside the control and negative to place it outside the control
    // example: when the parent is 100 width/height and the overlay is 10 width/height.
    // OffsetBy = ofTopLeft and OffsetPosition = (10,10), then the relative position is 10,10
    // OffsetBy = ofBottomRIght and offsetPosition = (10,10), then re relative position is 80,80 (the right border of the overlay is 10px from the right border of the parent)
    // call UpdateOverlay; to apply the changes
    property OffsetPosition: TPoint read FOffsetPosition write SetOffsetPosition;
    // per default the parent
    property OffsetToControl: TControl read FOffsetToControl write SetOffsetToControl;
    property OffsetBy: TOffsetBy read FOffsetBy write SetOffsetBy;
    property OffsetTo: TOffsetTo read FOffsetTo write SetOffsetTo;

    // this is an override for the AlphaValue stored as a double 0= invisible 1 = full visible.
    property Alpha: double read FAlpha write SetAlpha;
    property Visible: boolean read FVisible write SetVisible;
  end;

implementation

uses
  ClipBrd;

{$R *.dfm}


constructor TOverlayFrm.CreateOverlay(AForm: TForm);

begin
  inherited Create(AForm);
  PopupParent := AForm;
  FOffsetPosition := point(0, 0);
  FOffsetToControl := AForm;

  // SetWindowLong(self.Handle, GWL_EXSTYLE,
  // GetWindowLong(self.Handle, GWL_EXSTYLE) or WS_EX_NOACTIVATE);

    self.ControlStyle:=
    self.ControlStyle -[csOpaque];
end;

destructor TOverlayFrm.Destroy;
begin

  inherited;
end;

procedure TOverlayFrm.Paint;
begin
  inherited;
  // Canvas.Draw(0, 0, FBmp);
end;

procedure TOverlayFrm.UpdateOverlay;
var
  Pt: TPoint;
  R: TRect;
  br: TPoint;
  aLeft, aTop, aBottom, aRight: integer;
begin
  case FOffsetTo of

    otNormalRect:
      begin
        if FOffsetToControl.parent <> nil then
          Pt := FOffsetToControl.parent.Clienttoscreen(point(FOffsetToControl.left, FOffsetToControl.top))
        else
          Pt := FOffsetToControl.Clienttoscreen(point(0, 0));
        R.TopLeft := Pt;
        R.width := FOffsetToControl.width;
        R.height := FOffsetToControl.height;
      end;
    otClientRect:
      begin
        Pt := FOffsetToControl.ClientOrigin;
        R := FOffsetToControl.ClientRect;
      end;
  end;

  br := Pt + point(R.width, R.height);

  aLeft := Pt.x + OffsetPosition.x;
  aTop := Pt.y + OffsetPosition.y;
  aBottom := br.y - OffsetPosition.y - self.height;
  aRight := br.x - OffsetPosition.x - self.width;

  case OffsetBy of
    ofTopLeft:
      Pt := point(aLeft, aTop);
    ofTopRight:
      Pt := point(aRight, aTop);

    ofBottomLeft:
      Pt := point(aLeft, aBottom);
    ofBottomRight:
      Pt := point(aRight, aBottom);
  end;

  SetBounds(Pt.x, Pt.y,
    width, height);

  // if Showing then
  Invalidate
  // else
  // ShowWindow(Handle, SW_SHOWNOACTIVATE);

end;

procedure TOverlayFrm.WMDisplayChange(var Message: TMessage);
begin
  inherited;
  UpdateOverlay;
end;

procedure TOverlayFrm.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TOverlayFrm.ShowWithoutFocussing;
begin
  ShowWindow(Handle, SW_SHOWNOACTIVATE);
  inherited visible:=true;
end;

procedure TOverlayFrm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Exstyle := Params.Exstyle or WS_EX_NOACTIVATE;

end;

procedure TOverlayFrm.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  inherited;
  if self.AlphaBlendValue = 0 then
    Msg.Result := HTTRANSPARENT;
end;

procedure TOverlayFrm.RegisterControl(c: TWinControl; PreventMousehandler: boolean);

  procedure RegisterControl2(c: TWinControl; const Data: TData);
  var
    sc: TWndSubClassEx<TData>;
    x: integer;
  begin
    sc := TWndSubClassEx<TData>.Create(c);
    sc.Data := Data;
    sc.OnMessageEx := OnSubClassMessage;
    sc.HookedWnd := c.Handle;

    for x := 0 to c.ControlCount - 1 do
      if c.Controls[x] is TWinControl then
        RegisterControl2(c.Controls[x] as TWinControl, Data);
  end;

var
  Data: TData;
begin
  Data.WC := c;
  Data.PreventMousehandler := PreventMousehandler;
  RegisterControl2(c, Data);
end;

var
  Called: boolean = false;

procedure TOverlayFrm.OnSubClassMessage(Sender: TWndSubClassEx<TData>;
  var Msg: Messages.TMessage; var PrevendDefaultHandler: boolean);
const
  cLeftMouse = 513;
  cRightMouse = 516;
var
  MouseMsg: word;

  dx, dy: word;
  mp: TPoint;
  WC: TWinControl;
  lParam: dword;
begin
  case Msg.Msg of
    WM_MOUSEACTIVATE:
      begin
        if not Sender.Data.PreventMousehandler then

          Msg.Result := MA_NOACTIVATE
        else
          Msg.Result := MA_NOACTIVATEANDEAT;

        PrevendDefaultHandler := true;

      end;
  end;

end;

procedure TOverlayFrm.UnRegisterControl(c: TWinControl);
var
  sc: TWndSubClass;
  x: integer;
begin

  for x := c.ComponentCount - 1 downto 0 do
  begin
    if c.components[x] is TWndSubClass then
    begin
      sc := c.components[x] as TWndSubClass;
      sc.free;
    end;
  end;

  for x := c.ControlCount - 1 downto 0 do
    if c.Controls[x] is TWinControl then
      UnRegisterControl(c.Controls[x] as TWinControl);

end;

procedure TOverlayFrm.SetOffsetBy(const Value: TOffsetBy);
begin
  FOffsetBy := Value;
end;

procedure TOverlayFrm.SetOffsetPosition(const Value: TPoint);
begin
  FOffsetPosition := Value;
end;

procedure TOverlayFrm.SetOffsetToControl(const Value: TControl);
begin
  FOffsetToControl := Value;
end;

procedure TOverlayFrm.SetAlpha(const Value: double);
var
  i: integer;
begin

  FAlpha := Value;
  if FAlpha < 0 then
    FAlpha := 0
  else if FAlpha > 1 then
    FAlpha := 1;

  i := round(FAlpha * $FF);

  if i > $FF then
    i := $FF;

  self.AlphaBlendValue := i;
end;

procedure TOverlayFrm.SetVisible(const Value: boolean);
begin
  FVisible := Value;
  if Value then
    self.ShowWithoutFocussing
  else
    Alpha := 0;
end;

procedure TOverlayFrm.FormCreate(Sender: TObject);
begin
  Alpha := 0;
  inherited Visible := true;
end;

procedure TOverlayFrm.SetOffsetTo(const Value: TOffsetTo);
begin
  FOffsetTo := Value;
end;

end.
