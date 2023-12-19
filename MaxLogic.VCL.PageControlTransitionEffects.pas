Unit MaxLogic.VCL.PageControlTransitionEffects;

{
  requires PicShow: http://www.delphiarea.com/products/delphi-components/picshow/
}

Interface

Uses
  windows, classes, sysUtils, VCL.ComCtrls, forms, graphics, controls, ExtCtrls, messages,
  PicShow, PSEffect, diagnostics, OverlayForm;

Type
  // forward declaration
  TOverlayForm = Class;

  TTransition = Class(TComponent)
  Private
    fOverlay: TOverlayFrm;
    PicShow: TPicShow;
    fTabIndices: Array [0 .. 1] Of integer;
    fBmp: TBitmap;

    FPageControl: TPageControl;
    fOrgPageControlChange: TNotifyEvent;
    fOrgPageChanging: TTabChangingEvent;
    fStyle: integer;
    FDuration: word;
    fStyleBack: integer;
    FOnTransitionStart: TNotifyEvent;
    FOnTransitionCompleted: TNotifyEvent;
    FEnabled: Boolean;

    Procedure PageControlChanging(Sender: TObject; Var AllowChange: Boolean);
    Procedure PageControlChange(Sender: TObject);

    Function SelectStyleBasedOnTabDirection: integer;

    // events
    Procedure PicShowStart(Sender: TObject; Picture, Screen: TBitmap);
    Procedure PicShowProgress(Sender: TObject);
    Procedure PicShowStop(Sender: TObject);
    Procedure PicShowComplete(Sender: TObject);

    Procedure ShowOverlay;
    Procedure PreparePicShow;
    Procedure ApplyDurationToPicShow;
    Function GetParentForm: TForm;
    Procedure RestoreOrgState;
    Procedure TakeScreenofCurTabSheet;

    Procedure initOverlay;
    Procedure initPicShow;

    // property setters
    Procedure SetPageControl(Const Value: TPageControl);
    Procedure SetStyle(Const Value: integer);
    Procedure SetDuration(Const Value: word);
    Procedure SetStyleBack(Const Value: integer);
    Procedure SetOnTransitionCompleted(Const Value: TNotifyEvent);
    Procedure SetOnTransitionStart(Const Value: TNotifyEvent);
    procedure SetEnabled(const Value: Boolean);
  Public
    Constructor Create(aOwner: TComponent);
    Destructor destroy; Override;

    Procedure GetStyleNames(Names: TStrings);

    Property PageControl: TPageControl Read FPageControl Write SetPageControl;
    // NOTE: is 1 based!
    // Style is using when the from tab is positioned left to the target tab
    Property Style: integer Read fStyle Write SetStyle;
    // Style Back is used when the start tab is positioned right to the target tab
    Property StyleBack: integer Read fStyleBack Write SetStyleBack;
    // the duration of the transition effect
    Property Duration: word Read FDuration Write SetDuration;
    Property OnTransitionCompleted: TNotifyEvent Read FOnTransitionCompleted Write SetOnTransitionCompleted;
    Property OnTransitionStart: TNotifyEvent Read FOnTransitionStart Write SetOnTransitionStart;
    property Enabled: Boolean read FEnabled write SetEnabled;
  End;

  TOverlayForm = Class(TForm)
  Private
    fTransition: TTransition;
  Protected
    Procedure CreateParams(Var Params: TCreateParams); Override;
    Procedure WMNCHitTest(Var Msg: TWMNCHitTest); Message wm_NCHitTest;
  Public
    Destructor destroy; Override;
  End;

Implementation

{ Uses
  pawel1; }

{$R *.dfm}

{ TTransition }

Procedure TTransition.ApplyDurationToPicShow;
Var
  f: double;
  i: integer;
Begin
  // try to use 30fps if possible
  i := 1;
  Repeat
    f := FDuration / (101 / i);

    // this is the minimum steps neccessary for any half way decent showing...
    If i >= 20 Then
      break;

    // it is quite possible, that we will not be able to display that fast...
    If f < 30 Then
      inc(i)
    Else
      break;
  Until false;
  // step is from 1..100, the percent to go
  PicShow.Step := i;
  // delay is the time between the steps I suppose?
  PicShow.Delay := round(f);
End;

Constructor TTransition.Create(aOwner: TComponent);
Begin
  Inherited;
  FEnabled := True;
  fBmp := TBitmap.Create;

  FDuration := 1000;
  fStyle := 131;
  fStyleBack := 130;
End;

Destructor TTransition.destroy;
Begin
  RestoreOrgState;
  fBmp.free;

  If assigned(fOverlay) Then
    If GetParentForm <> Nil Then
    Begin
      // ??? fOverlay.fTransition := nil;
      fOverlay.free;
    End;

  Inherited;
End;

Procedure TTransition.initOverlay;
Begin
  freeAndNil(fOverlay);
  fOverlay := TOverlayFrm.Create(GetParentForm);
  fOverlay.BorderStyle := bsNone;

  // must be set because without it the position will be resetted to poDefault when setting visible to true
  fOverlay.Position := poDesigned;
  fOverlay.AlphaBlend := True; // ensure the underlying controls are still painted
  fOverlay.DoubleBuffered := True;
  fOverlay.Visible := false;

End;

Procedure TTransition.initPicShow;
Begin

  PicShow := TPicShow.Create(fOverlay);
  PicShow.parent := fOverlay;
  PicShow.Align := alClient;

  PicShow.FrameWidth := 0;
  PicShow.Style := fStyle;
  PicShow.BgMode := bmTiled;
  PicShow.Proportional := True;
  PicShow.Stretch := false;
  PicShow.FrameColor := fOverlay.Color;
  PicShow.Threaded := True;
  PicShow.ExactTiming := True;

  PicShow.OnStart := PicShowStart;
  PicShow.OnProgress := PicShowProgress;
  PicShow.OnStop := PicShowStop;
  PicShow.OnComplete := PicShowComplete;
  PicShow.Manual := false;
  PicShow.Visible := True;

End;

Function TTransition.GetParentForm: TForm;
Var
  c: TWinControl;
Begin
  result := Nil;

  c := self.FPageControl.parent;
  While c <> Nil Do
  Begin
    If c Is TForm Then
      exit(c As TForm);

    c := c.parent;
  End;
End;

Procedure TTransition.GetStyleNames(Names: TStrings);
Var
  x: integer;
Begin
  Names.BeginUpdate;
  Try
    For x := 0 To PSEffects.Count - 1 Do
    Begin
      Names.Add('#' + IntToStr(x) + ' ' + PSEffects[x].Name);
    End;
  Finally
    Names.EndUpdate;
  End;
End;

Procedure TTransition.PageControlChange(Sender: TObject);
Begin

  If assigned(fOrgPageControlChange) Then
    fOrgPageControlChange(Sender);

  fTabIndices[1] := FPageControl.ActivePageIndex;

  if not FEnabled then
    exit;

  FPageControl.Update;

  TakeScreenofCurTabSheet;

  PicShow.Picture.assign(fBmp);
  PicShow.Style := SelectStyleBasedOnTabDirection;
  PicShow.Execute;
End;

Procedure TTransition.PageControlChanging(Sender: TObject; Var AllowChange: Boolean);
Begin
  If assigned(fOrgPageChanging) Then
    fOrgPageChanging(Sender, AllowChange);

  If Not AllowChange Then
    exit;
  if not FEnabled then
    exit;

  fTabIndices[0] := FPageControl.ActivePageIndex;
  TakeScreenofCurTabSheet;
  ShowOverlay;

End;

Procedure TTransition.PicShowComplete(Sender: TObject);
Begin

  fOverlay.Visible := false;

  If assigned(self.FOnTransitionCompleted) Then
    FOnTransitionCompleted(self);

End;

Procedure TTransition.PicShowProgress(Sender: TObject);
Begin

End;

Procedure TTransition.PicShowStart(Sender: TObject; Picture, Screen: TBitmap);
Begin
  If assigned(self.FOnTransitionStart) Then
    FOnTransitionStart(self);
  fOverlay.TransparentColor := false;
End;

Procedure TTransition.PicShowStop(Sender: TObject);
Begin

End;

Procedure TTransition.PreparePicShow;
Begin
  // if PicShow is playing, stops it
  If PicShow.Busy Then
    PicShow.Stop;

  ApplyDurationToPicShow;

  PicShow.Clear;
  PicShow.BgPicture.assign(fBmp);
End;

Procedure TTransition.RestoreOrgState;
Begin
  If Not assigned(FPageControl) Then
    exit;

  FPageControl.onchange := fOrgPageControlChange;
  FPageControl.Onchanging := fOrgPageChanging;
End;

Function TTransition.SelectStyleBasedOnTabDirection: integer;
Begin
  // are moving from left to right?
  If fTabIndices[0] < fTabIndices[1] Then
    result := fStyle
  Else
    result := fStyleBack;
End;

Procedure TTransition.SetDuration(Const Value: word);
Begin
  FDuration := Value;
  If assigned(PicShow) Then
    ApplyDurationToPicShow;
End;

procedure TTransition.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

Procedure TTransition.SetOnTransitionCompleted(Const Value: TNotifyEvent);
Begin
  FOnTransitionCompleted := Value;
End;

Procedure TTransition.SetOnTransitionStart(Const Value: TNotifyEvent);
Begin
  FOnTransitionStart := Value;
End;

Procedure TTransition.SetPageControl(Const Value: TPageControl);
Begin
  If FPageControl = Value Then
    exit;

  RestoreOrgState;

  FPageControl := Value;
  // FPageControl.DoubleBuffered := True;
  // FPageControl.parent.DoubleBuffered := True;

  fOrgPageControlChange := FPageControl.onchange;
  fOrgPageChanging := FPageControl.Onchanging;

  FPageControl.onchange := PageControlChange;
  FPageControl.Onchanging := PageControlChanging;

  initOverlay;

  fOverlay.parent := FPageControl.parent;
  fOverlay.handleNeeded;
  initPicShow;
  PicShow.handleNeeded;

  // GetParentForm.AlphaBlend:=true;
End;

Procedure TTransition.SetStyle(Const Value: integer);
Begin
  fStyle := Value;
End;

Procedure TTransition.SetStyleBack(Const Value: integer);
Begin
  fStyleBack := Value;
End;

Procedure TTransition.ShowOverlay;
Var
  tsPos, parentPos: TPoint;
  r: TRect;
Begin
  tsPos := FPageControl.ActivePage.ClientToScreen(point(0, 0));
  parentPos := FPageControl.parent.ClientToScreen(point(0, 0));

  fOverlay.SetBounds(
    (tsPos.x - parentPos.x),
    (tsPos.y - parentPos.y),
    FPageControl.ActivePage.width,
    FPageControl.ActivePage.height);

  PreparePicShow;

  // PicShow.Update;

  fOverlay.AlphaBlendValue := 0;

  ShowWindow(fOverlay.Handle, SW_SHOWNOACTIVATE);
  fOverlay.AlphaBlendValue := $FF;
  fOverlay.Visible := True;
  fOverlay.BringToFront; // ensure the form is over the controls
  fOverlay.Update;
End;

Procedure TTransition.TakeScreenofCurTabSheet;
Var
  dc: hDc;
  b: TBitmap;
  ts: TTabSheet;
Begin
  b := fBmp;

  ts := FPageControl.ActivePage;

  if (b.width <> ts.width) or (b.height <> ts.height) then
    b.SetSize(ts.width, ts.height);

  dc := GetDC(ts.Handle);
  try
    BitBlt(b.Canvas.Handle, 0, 0, b.width, b.height, dc, 0, 0, SRCCOPY);
  finally
    ReleaseDC(ts.Handle, dc);
  end;

End;

{ TOverlayForm }

Procedure TOverlayForm.CreateParams(Var Params: TCreateParams);
Begin
  Inherited;
  Params.Exstyle := Params.Exstyle Or WS_EX_NOACTIVATE;

End;

Destructor TOverlayForm.destroy;
Begin
  If fTransition <> Nil Then
  Begin
    fTransition.fOverlay := Nil;
    fTransition.PicShow := Nil;
  End;
  Inherited;
End;

Procedure TOverlayForm.WMNCHitTest(Var Msg: TWMNCHitTest);
Begin
  Msg.result := HTTRANSPARENT;
End;

End.
