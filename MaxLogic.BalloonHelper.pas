unit MaxLogic.BalloonHelper;

{
  How to use the normal TBalloonHint
  - put a Edit1 on form

  - put Balloonhint1 on the form
  - set Edit1 .hint to some text
  - set Edit1.customhint := Ballonhint >> better set TForm.customHint to balloonhint1
  - enable edit1.showhint := true; >> or better do it for the form itself
  - add Timagelist
  - add an image to timagelist
  - add timagelist to balloonhint1

  set the edit1.hint like this:
  Hello|How are you today|5

  will have
  a title of "Hello"
  body text of "How are you today"
  and the 5th image in the list }

interface

uses
  windows, classes, sysUtils, controls, ExtCtrls,
  StdCtrls, generics.collections, maxWndSubClassing, messages, forms,
  MaxLogic.BalloonDefaultImageList;

Type
  TBalloonHelper = class(TComponent)
  private const
    cHideAfter = 5000;
  private
    FBalloonHint: TBalloonHint;
    fFormSubClassing: TWndSubClass;
    function GetBalloonHint: TBalloonHint;
    function GetParentFormWnd(wc: TWinControl): hwnd;
    class function GetParentForm(wc: TWinControl): TForm;
    procedure MsgHideBalloon(var Msg: messages.TMessage; var PrevendDefaultHandler: boolean);
  public
    constructor Create(aOwner: TComponent);
    destructor Destroy; override;

    Procedure ShowBalloonHint(FocusOnControl: TWinControl; const atitle, adescription: string; HideAfter: integer = cHideAfter; aImageIndex: integer = 0);
    class Procedure ShowBalloonHint2(FocusOnControl: TWinControl; const atitle, adescription: string; aImageIndex: integer = 0);

    property BalloonHint: TBalloonHint read GetBalloonHint;
  end;

implementation

{ TBalloonHelper }

constructor TBalloonHelper.Create(aOwner: TComponent);
begin
  inherited;

end;

destructor TBalloonHelper.Destroy;
begin

  if assigned(fFormSubClassing) then
    freeAndNIL(fFormSubClassing);
  if assigned(FBalloonHint) then
    freeAndNIL(FBalloonHint);
  inherited;
end;

function TBalloonHelper.GetBalloonHint: TBalloonHint;
begin
  if not assigned(FBalloonHint) then
  begin
    FBalloonHint := TBalloonHint.Create(application.mainform);
    // FBalloonHint.style := bhsBalloon;
    // FBalloonHint.style := bhsStandard;
    FBalloonHint.HideAfter := cHideAfter;
    FBalloonHint.Delay := 0;

    FBalloonHint.Images := TImageListForBalloonForm.Instance.ImageList1;
    FBalloonHint.ImageIndex := 0;
  end;
  result := FBalloonHint;
end;

procedure TBalloonHelper.ShowBalloonHint(FocusOnControl: TWinControl; const atitle, adescription: string; HideAfter: integer; aImageIndex: integer);
var
  r: TRect;
  FormWnd: hwnd;
begin
  r := rect(0, 0, FocusOnControl.width, FocusOnControl.height);
  r.TopLeft := FocusOnControl.ClientToScreen(r.TopLeft);
  r.BottomRight := FocusOnControl.ClientToScreen(r.BottomRight);

  BalloonHint.Description := adescription + sLineBreak;
  BalloonHint.Title := atitle;
  BalloonHint.HideAfter := HideAfter;
  BalloonHint.ImageIndex := aImageIndex;

  if assigned(fFormSubClassing) then
    freeAndNIL(fFormSubClassing);

  FormWnd := GetParentFormWnd(FocusOnControl);
  if FormWnd <> 0 then
  begin
    fFormSubClassing := TWndSubClass.Create(self);
    fFormSubClassing.OnMessage := MsgHideBalloon;
    fFormSubClassing.HookedWnd := FormWnd;
  end;
  FBalloonHint.ShowHint(r);
end;

class function TBalloonHelper.GetParentForm(wc: TWinControl): TForm;
var
  c: TWinControl;
begin
  result := nil;

  c := wc;
  while c <> nil do
  begin
    if (c.parent = nil) and (c is TForm) then
      exit(c as TForm);

    c := c.parent;
  end;

end;

class procedure TBalloonHelper.ShowBalloonHint2(FocusOnControl: TWinControl; const atitle, adescription: string; aImageIndex: integer = 0);
const
  cDefaultname = 'maxBalloonHelperDefaultInstance';
var
  f: TForm;
  x: integer;
  bl: TBalloonHelper;
begin
  // find the top level form
  f := GetParentForm(FocusOnControl);
  if f = nil then
    raise Exception.Create('could not find parent form');

  // find if there is an instance of a default balloon heler already
  bl := nil;
  for x := f.ComponentCount - 1 downto 0 do
    if (f.Components[x] is TBalloonHelper) and (f.Components[x].Name = cDefaultname) then
    begin
      bl := f.Components[x] as TBalloonHelper;
      break;
    end;

  // if there was none, then create it...
  if bl = nil then
  begin
    bl := TBalloonHelper.Create(f);
    bl.Name := cDefaultname;
  end;

  bl.ShowBalloonHint(FocusOnControl, atitle, adescription, cHideAfter, aImageIndex);
end;

function TBalloonHelper.GetParentFormWnd(wc: TWinControl): hwnd;
var
  c: TWinControl;
begin
  result := 0;

  c := wc;
  while c <> nil do
  begin
    if (c.parent = nil) and (c is TForm) then
      exit(c.handle);

    c := c.parent;
  end;
end;

procedure TBalloonHelper.MsgHideBalloon(var Msg: messages.TMessage; var PrevendDefaultHandler: boolean);
begin
  if Msg.Msg in [WM_WINDOWPOSCHANGED, WM_ACTIVATEAPP] then
    if assigned(FBalloonHint) then
      FBalloonHint.HideHint;
end;

end.
