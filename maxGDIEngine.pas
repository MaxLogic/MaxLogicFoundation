unit maxGDIEngine;
{ Version2
  History
  2013-06-05: make it independant of ExperimentData }
{$I JEDI.INC}

interface

uses
  windows, classes, sysUtils,
  graphics, jpeg, Direct3D9, D3DX9, clHPC, AdvancedShapes,
  Generics.Collections, maxWndSubClassing,
{$IFNDEF DELPHI2009_UP}
  GDIPOBJ, GDIPAPI;
{$ELSE}
GdiPlus;
{$ENDIF}


CONST
  MIN_SCALE = 0.3;
  MAX_SCALE = 30;

  // visible margin
  VISIBLE_MIN_IMAGE_MARGIN = 50;

type
  // forward declarations
  TViewContext = class;
  TArtifact = class;

  TMaxGDIEngine = class
  private
    FViewContext: TViewContext;
    FOffscreenColor: TgpColor;

    // fCachedFirstLayer: IGPCachedBitmap;

    procedure SetOffscreenColor(const Value: TgpColor);
    procedure SetViewoffset(const Value: TgpPointf);
    procedure SetViewSize(const Value: TSize);
    procedure TrapTheImage(var X, Y: single; w, h: single);
    procedure SetViewContext(const Value: TViewContext);

  protected
    fFirstLayer: igpBitMap;
    fViewSize: TSize;
    fViewoffset: TgpPointf;
    // GDI+
    fBackBuffer: igpBitMap;
    fSolidBrush: IGPSolidBrush;
    fSolidPen, DotPen: igpPen;
    fFontFamily: IGPFontFamily;
    fFont: IGPFont;
    fFrontGraphics,
      fGraphics: IGPGraphics;

    fAnimalTriangle: array [0 .. 2] of TgpPointf;
    fHPC: THPC;
    FBackGroundColor: TgpColor;
    FScale: single;
    FDirty: Boolean;

    Procedure InitGDI;
    procedure PrepareGraphics(graphics: IGPGraphics);
    procedure Example2;
    Procedure GDITextOut(const p: TgpPointf; const s: string; const color: TgpColor);
    procedure GDISaveToPNGFile(const Filename: string);
    procedure SetBackGroundColor(const Value: TgpColor);
    Procedure DrawAnimalArrow(const ptHead, ptBody: Tpoint);
    procedure DrawZoneOuterLine(z: TadvShape; color: TgpColor; aWidth: integer);
    procedure FillZone(z: TadvShape; color: TgpColor; graphics: IGPGraphics = NIL);

    Procedure DrawDotLine(const p1, p2: Tpoint); overload;
    Procedure DrawDotLine(const p1, p2: TD3DxVector2); overload;
    Procedure DrawDotLine(const p1, p2: TgpPointf); overload;

    procedure DrawDot(const Center: TD3DxVector2; aWidth: single; CenterColor, OuterColor: TgpColor); overload;
    procedure DrawDot(const Center: Tpoint; aWidth: single; CenterColor, OuterColor: TgpColor); overload;
    procedure DrawDot(const Center: TgpPointf; aWidth: single; CenterColor, OuterColor: TgpColor); overload;

    procedure PaintTextcentered(const s: string; p: TgpPointf; aColor: TgpColor); overload;
    procedure PaintTextcentered(const s: string; p: TgpPointf; aColor, ShadowColor: TgpColor; ShadowOffset: integer); overload;
    function GetTextHeight(const s: string): single;
    function GetTextWidth(const s: string): single;

    procedure SetScale(const aValue: single);
    procedure SetDirty(const Value: Boolean);

    procedure DoRender; virtual;
    procedure SetTransform;
  public

    constructor Create;
    Destructor Destroy; override;
    Procedure AssignFirstLayer(bmp: TBitMap);
    Procedure ClearFirstlayer;
    Procedure CenterOnPoint(const MapPoint: Tpoint); overload;
    Procedure CenterOnPoint(const MapPoint: TgpPointf); overload;
    Procedure CenterOnPoint(const MapX, MapY: single); overload;
    Function GetMapPointAtViewPoint(const ViewPoint: TgpPointf): TgpPointf; overload;
    Function GetMapPointAtViewPoint(const ViewPoint: Tpoint): Tpoint; overload;
    Function GetMapPointAtViewPoint(const ViewX, ViewY: integer): TgpPointf; overload;

    Procedure PaintFrame; virtual;
    Procedure Flush(DestBmp: TBitMap; X, Y: single; const CanvasColor: TgpColor);
    class procedure RotateBitmap(bmp: TBitMap; Degs: integer; AdjustSize: Boolean; BkColor: TColor); static;

    // if no background image is set, then this will be used to fill in the canvas
    property BackGroundColor: TgpColor read FBackGroundColor write SetBackGroundColor;
    // when the image is scaled down, white space can show up, we can use a different color if we like
    property OffscreenColor: TgpColor read FOffscreenColor write SetOffscreenColor;

    property BackBuffer: igpBitMap read fBackBuffer;

    property Scale: single read FScale write SetScale;
    // this is the size of the control the image is displayed in, including an offset
    property ViewSize: TSize read fViewSize write SetViewSize;
    property ViewOffset: TgpPointf read fViewoffset write SetViewoffset;
    property Dirty: Boolean read FDirty write SetDirty;
    Property ViewContext: TViewContext read FViewContext write SetViewContext;
  end;

  TZoneOuterPoly = class
  public
    points: array of TgpPointf;
    Zone: TadvShape;
    Destructor Destroy; override;
    procedure Clear;
  end;

  // this will help to paint the experiment frame to the paintbox.
  TViewContext = class
  private
    fMainForm: TForm;
    fSubClass: TWndSubClass;
    fTimer: TTimer;
    fOrgPaintBoxMouseMove: TMouseMoveEvent;
    fIsDraggingImage: Boolean;
    fImageDragPos: Tpoint;

    fPaintBox: TWinControl;
    fEngine: TCustomExperimentPainter;

    FedZoom: TCustomEdit;
    fBlockZoomEditChangeEvent: Boolean;

    procedure edZoomChange(Sender: TObject);

    procedure SetZoom(const Value: double);
    Procedure PaintBoxPaint(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);

    function GetZoom: double;
    procedure SetedZoom(const Value: TCustomEdit);
    procedure TimerTimer(Sender: TObject);
    Procedure Repaint;
    procedure WndProc(var Msg: TMessage; var PrevendDefaultHandler: Boolean);
    procedure WMEraseBkgndEx(WinControl: TWinControl; var Message: TWmEraseBkgnd);
  public
    constructor Create(PaintBox: TWinControl; aEngine: TMaxGDIEngine);
    Destructor Destroy; override;

    Procedure CenterOn(const pt: Tpoint);

    // if you wish the image to be moved and zoomed using they keyboard, you have to call this method from your Form on which the Paintbox is embeded
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState; var Handled: Boolean);
    // if you wish the image to be zoomed using the mouse wheel call this method from your form, on which the paintbox is embeded
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: Tpoint; var Handled: Boolean);

    property Zoom: double read GetZoom write SetZoom;
    // counted from the center of the screen

    // set this to have an TEdit to be automatically used for zooming
    property edZoom: TCustomEdit read FedZoom write SetedZoom;
  end;

  TArtifact = class
  private
    fParent: TArtifact;
    fArtifacts: TObjectList<TArtifact>;
    fAnchor: TgpPointf;
    FName: string;
    FVisible: Boolean;
    FEditable: Boolean;
    FWorldMatrix: iGPMatrix;
    FRotation: single;
    FScale: Single;
    FWidth: Single;
    FVerticalAlignment: TVerticalAlignment;
    FAlignment: TAlignment;
    FTop: Single;
    Fheight: Single;
    FLeft: Single;

    procedure SetName(const Value: string);
    procedure SetVisible(const Value: Boolean);
    procedure SetEditable(const Value: Boolean);
    procedure SetRotation(const Value: single);
    procedure SetScale(const Value: Single);
    function GetWorldMatrix: iGPMatrix;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetAnchor(const Value: TgpPointf);
    procedure Setheight(const Value: single);
    procedure SetLeft(const Value: single);
    procedure SetTop(const Value: single);
    procedure SetVerticalAlignment(const Value: TVerticalAlignment);
    procedure SetWidth(const Value: single);
  protected
               fEngine: TMaxGDIEngine;
               procedure RecalcWorldmatrix; virtual;
    // this will be called before the frame gets rendered. this is the place to update the bounngboxes and other values
    Procedure PrepareFrame; virtual;
    procedure DoRender; virtual;
    procedure RecalcMinMax; virtual;
  public
    Constructor Create(aEngine: TMaxGDIEngine); virtual;
    Destructor Destroy; override;

    property Artifacts: TObjectList<TArtifact> read fArtifacts;
    property parentArtifact: TArtifact read fParent;
    property Anchor: TgpPointf read fAnchor write SetAnchor;
    property Rotation: single read FRotation write SetRotation;
    property Scale: Single read FScale write SetScale;

    property Left: Single read FLeft write SetLeft;
    property Top: single  read FTop write SetTop;
    property Width: single  read FWidth write SetWidth;
    property height: single read Fheight write Setheight;
        property Alignment: TAlignment read FAlignment write SetAlignment;
    property VerticalAlignment: TVerticalAlignment read FVerticalAlignment write SetVerticalAlignment;

    property WorldMatrix: iGPMatrix read GetWorldMatrix;

    property Name: string read FName write SetName;
    property Visible: Boolean read FVisible write SetVisible;
    property Editable: Boolean read FEditable write SetEditable;
  end;

  TText = class(TArtifact)
  private
    fgpFontFamily: IGPFontFamily;
    fFont: IGPFont;
    FFontSize: integer;
    FFontColor: TgpColor;
    fFontFamily: string;
    FFontStyle: TGPFontStyle;
    FShadowOffset: single;
    FShadowColor: TgpColor;

    Fcaption: string;
    procedure SetFontColor(const Value: TgpColor);
    procedure SetFontFamily(const Value: string);
    procedure SetFontSize(const Value: integer);
    procedure SetFontStyle(const Value: TGPFontStyle);
    procedure SetShadowColor(const Value: TgpColor);
    procedure SetShadowOffset(const Value: single);
    procedure Setcaption(const Value: string);
    procedure ReInitFont;
  protected
    procedure DoRender; override;
    procedure RecalcMinMax; override;
  public
    Constructor Create(aEngine: TMaxGDIEngine); override;

    property FontFamily: string read fFontFamily write SetFontFamily;
    property FontSize: integer read FFontSize write SetFontSize;
    property FontColor: TgpColor read FFontColor write SetFontColor;
    property FontStyle: TGPFontStyle read FFontStyle write SetFontStyle;
    property ShadowOffset: single read FShadowOffset write SetShadowOffset;
    property ShadowColor: TgpColor read FShadowColor write SetShadowColor;

    property caption: string read Fcaption write Setcaption;
    property Rotation: single read FRotation write SetRotation;
  end;

implementation

uses
  bsUTILS, pawel1, ClipBrd, math, max3DMath, max3DCollision;

{ TZoneOuterPoly }

procedure TZoneOuterPoly.Clear;
begin
  SetLength(points, 0);
  points := NIL;
end;

destructor TZoneOuterPoly.Destroy;
begin
  points := NIL;
  inherited;
end;

{ TCanvasSize }

{ TCustomEpxerimentPainter }
procedure TMaxGDIEngine.PrepareGraphics(graphics: IGPGraphics);
begin

  // Graphics.TextRenderingHint := TextRenderingHintAntiAlias;
  graphics.TextRenderingHint := TextRenderingHintClearTypeGridFit;
  graphics.CompositingQuality := CompositingQualityGammaCorrected;
  graphics.SmoothingMode := SmoothingModeAntiAlias;
  // Graphics.SmoothingMode := SmoothingModeHighSpeed;
end;

procedure TMaxGDIEngine.InitGDI;
var
  BlackBrush, BlueBrush: IGPBrush;
  // for transparencies
  OpaqueBrush, SemiTransBrush: IGPSolidBrush;
  points: array [0 .. 2] of tgpPoint;
  OpaquePen, SemiTransPen: igpPen;
  Brush: IGPLinearGradientBrush;
begin
  fFontFamily := TGPFontFamily.Create('Arial');
  fFont := TGPFont.Create(fFontFamily, 14, FontStyleRegular, UnitPixel);

  fSolidBrush := TGPSolidBrush.Create(TgpColor.White);
  // SolidBrush.GammaCorrection := True;

  fSolidPen := TgpPen.Create(TgpColor.Create(TgpColor.White));
  DotPen := TgpPen.Create(TgpColor.Create(TgpColor.White));
  DotPen.DashStyle := DashStyleDashDot;

  /// //////////////////////////////////////
  /// LightPen.DashStyle := DashStyleDot;

  // Brush := TGPLinearGradientBrush.Create(TGPPoint.Create(0, 10), TGPPoint.Create(200, 10), TGPColor.Red, TGPColor.Blue);
  // Graphics.FillRectangle(Brush, 0, 0, 200, 50);


  // OpaquePen := TGPPen.Create(TGPColor.Create(255, 0, 0, 255), 15);
  // SemiTransPen := TGPPen.Create(TGPColor.Create(128, 0, 0, 255), 15);
  // Graphics.DrawLine(OpaquePen, 0, 20, 100, 20);
  // Graphics.DrawLine(SemiTransPen, 0, 40, 100, 40);
  // Graphics.FillEllipse(SemiTransBrush, 86, 45, 45, 30);
  // Graphics.FillEllipse(SemiTransBrush, 40, 90, 86, 30);

  // Points[0].Initialize(250, Y);
  // Points[1].Initialize(250, Y + 60);
  // Points[2].Initialize(260, Y + 60);
  // Graphics.FillPolygon(BlueBrush, Points);
  // Graphics.FillEllipse(BlueBrush, 380, Y, 200, 60);
  // Graphics.ResetTransform;
end;

procedure TMaxGDIEngine.AssignFirstLayer(bmp: TBitMap);
var
  graphics: TgpGraphics;
begin
  ClearFirstlayer;

  fFirstLayer := TGPBitmap.Create(bmp.handle, bmp.Palette);
end;

procedure TMaxGDIEngine.CenterOnPoint(const MapPoint: Tpoint);
begin
  CenterOnPoint(
    TgpPointf.Create(MapPoint.X, MapPoint.Y));
end;

procedure TMaxGDIEngine.CenterOnPoint(const MapPoint: TgpPointf);
var
  X, Y, w, h: single;
begin
  w := fViewSize.Width;
  h := fViewSize.Height;

  fViewoffset.X := (w / 2) - (MapPoint.X * FScale);
  fViewoffset.Y := (h / 2) - (MapPoint.Y * FScale);

  FDirty := true;
end;

procedure TMaxGDIEngine.CenterOnPoint(const MapX, MapY: single);
begin
  CenterOnPoint(
    TgpPointf.Create(MapX, MapY));
end;

procedure TMaxGDIEngine.ClearFirstlayer;
begin
  // fCachedFirstLayer := NIL;
  fFirstLayer := nil;
end;

constructor TMaxGDIEngine.Create;
var
  X: integer;
begin
  inherited Create;
  FViewContext := TViewContext.Create(apaintbox, self);

  FScale := 1;

  fAnimalTriangle[0].initialize(0, -10);
  fAnimalTriangle[1].initialize(5, 10);
  fAnimalTriangle[2].initialize(-5, 10);

  fHPC := THPC.Create;
  fHPC.getTimeKey;

  FBackGroundColor := TgpColor.Gray;

  fViewSize := TSize.Create(apaintbox.Width, apaintbox.Height);
  CenterOnPoint(apaintbox.Width / 2, apaintbox.Height / 2);

  InitGDI;
end;

destructor TMaxGDIEngine.Destroy;
begin
  fHPC.free;
  FViewContext.free;
  inherited;
end;

procedure TMaxGDIEngine.DoRender;
begin

end;

procedure TMaxGDIEngine.DrawAnimalArrow(const ptHead, ptBody: Tpoint);
var
  Brush: iGPPathGradientBrush;
  Path: IGPGraphicsPath;
  SurroundColors: array [0 .. 0] of TgpColor;
  X: integer;
  p: TgpPointf;
  Angle: single;
  pt: TgpPointf;
  color: TgpColor;
  dir: TD3DxVector2;
  angle2: single;
  v2, body, head, v: TD3DxVector2;
begin
  if equal(ptBody, ptHead) then
  begin
    DrawDot(ptHead, 10, TgpColor.White, TgpColor.Orange);
    exit;
  end;

  body := Vector2(ptBody);
  head := Vector2(ptHead);
  dir := VectorSub(head, body);
  dir := VectorNormalize(dir);
  // v := VectorAdd(Head, VectorMuls(dir, 200));
  // DrawLineToNearestPoint(v, a.animal.head);

  v := VectorMuls(dir, 10);
  v := VectorAdd(v, head);
  fAnimalTriangle[0].initialize(v.X, v.Y);

  v2 := Rotate(90, dir);
  v := VectorAdd(VectorMuls(dir, -10), VectorMuls(v2, 5));
  v := VectorAdd(v, head);
  fAnimalTriangle[1].initialize(v.X, v.Y);

  v := VectorAdd(VectorMuls(dir, -10), VectorMuls(v2, -5));
  v := VectorAdd(v, head);
  fAnimalTriangle[2].initialize(v.X, v.Y);

  Path := TGPGraphicsPath.Create;
  Path.AddLines(fAnimalTriangle);

  Brush := TGPPathGradientBrush.Create(Path);

  SurroundColors[0] := TgpColor.Blue;
  Brush.CenterColor := TgpColor.LightSkyBlue;

  Brush.GammaCorrection := true;
  Brush.SetSurroundColors(SurroundColors);
  Brush.SetFocusScales(0.25, 0.25);

  fGraphics.FillPath(Brush, Path);
end;

procedure TMaxGDIEngine.Example2;
const
  points: array [0 .. 9] of tgpPoint = (
    (X: 75; Y: 120), (X: 100; Y: 170),
    (X: 150; Y: 170), (X: 112; Y: 195),
    (X: 150; Y: 270), (X: 75; Y: 220),
    (X: 0; Y: 270), (X: 37; Y: 195),
    (X: 0; Y: 170), (X: 50; Y: 170));
var
  Path: IGPGraphicsPath;
  Brush: iGPPathGradientBrush;
  Colors: array [0 .. 9] of TgpColor;
  graphics: IGPGraphics;
begin
  Path := TGPGraphicsPath.Create;
  Path.AddLines(points);

  Brush := TGPPathGradientBrush.Create(Path);
  Brush.CenterColor := TgpColor.Red;

  Colors[0].initialize(255, 0, 0, 0);
  Colors[1].initialize(255, 0, 255, 0);
  Colors[2].initialize(255, 0, 0, 255);
  Colors[3].initialize(255, 255, 255, 255);
  Colors[4].initialize(255, 0, 0, 0);
  Colors[5].initialize(255, 0, 255, 0);
  Colors[6].initialize(255, 0, 0, 255);
  Colors[7].initialize(255, 255, 255, 255);
  Colors[8].initialize(255, 0, 0, 0);
  Colors[9].initialize(255, 0, 255, 0);
  Brush.SetSurroundColors(Colors);

  graphics.FillPath(Brush, Path);
  Brush.GammaCorrection := true;
  graphics.TranslateTransform(200, 0);
  graphics.FillPath(Brush, Path);
end;

procedure TMaxGDIEngine.GDITextOut;
begin
  fSolidBrush.color := color;
  fGraphics.DrawString(s, fFont, p, fSolidBrush);
end;

procedure TMaxGDIEngine.GDISaveToPNGFile(const Filename: string);
var
  Image: iGPImage;
begin
  fBackBuffer.Save(Filename, TGPImageFormat.Png);
end;

procedure TMaxGDIEngine.SetBackGroundColor(const Value: TgpColor);
begin
  FBackGroundColor := Value;
  FDirty := true;
end;

procedure TMaxGDIEngine.DrawDotLine(const p1, p2: Tpoint);
var
  gpP1, gpP2: TgpPointf;
begin
  gpP1.X := p1.X;
  gpP1.Y := p1.Y;

  gpP2.X := p2.X;
  gpP2.Y := p2.Y;

  DrawDotLine(gpP1, gpP2);
end;

procedure TMaxGDIEngine.DrawDotLine(const p1, p2: TD3DxVector2);
var
  gpP1, gpP2: TgpPointf;
begin
  gpP1.X := p1.X;
  gpP1.Y := p1.Y;

  gpP2.X := p2.X;
  gpP2.Y := p2.Y;

  DrawDotLine(gpP1, gpP2);
end;

procedure TMaxGDIEngine.DrawDotLine(const p1, p2: TgpPointf);
begin
  DotPen.Width := 1;
  DotPen.color := TgpColor.White;
  fGraphics.DrawLine(DotPen, p1, p2);
end;

procedure TMaxGDIEngine.DrawZoneOuterLine(z: TadvShape; color: TgpColor; aWidth: integer);
var
  points: array of tgpPoint;
  i: integer;
  Poly: TadvPolygon;
begin
  fSolidPen.color := color;
  fSolidPen.Width := aWidth;

  case z.Typ of
    skRect:
      fGraphics.DrawRectangle(fSolidPen, Tgprect.Create(z.Rect));
    skEllipse:
      fGraphics.DrawEllipse(fSolidPen, Tgprect.Create(z.Rect));
    skPolygon:
      begin
        Poly := (z as TadvPolygon);
        SetLength(points, Length(Poly.points));
        for i := 0 to Length(Poly.points) - 1 do
          with Poly.points[i] do
            points[i].initialize(X, Y);

        fGraphics.DrawPolygon(fSolidPen, points);
        points := NIL;
      end;
  end;

end;

procedure TMaxGDIEngine.FillZone;
var
  points: array of tgpPoint;
  i: integer;
  Poly: TadvPolygon;
begin
  if graphics = NIL then
    graphics := fGraphics;

  fSolidBrush.color := color;

  case z.Typ of
    skRect:
      graphics.FillRectangle(fSolidBrush, Tgprect.Create(z.Rect));
    skEllipse:
      graphics.FillEllipse(fSolidBrush, Tgprect.Create(z.Rect));
    skPolygon:
      begin
        Poly := (z as TadvPolygon);
        SetLength(points, Length(Poly.points));
        for i := 0 to Length(Poly.points) - 1 do
          with Poly.points[i] do
            points[i].initialize(X, Y);
        graphics.FillPolygon(fSolidBrush, points);
        points := NIL;
      end;
  end;

end;

procedure TMaxGDIEngine.DrawDot(const Center: TD3DxVector2; aWidth: single; CenterColor, OuterColor: TgpColor);
var
  p: TgpPointf;
begin
  p.X := Center.X;
  p.Y := Center.Y;
  DrawDot(p, aWidth, CenterColor, OuterColor);
end;

procedure TMaxGDIEngine.DrawDot(const Center: Tpoint; aWidth: single; CenterColor, OuterColor: TgpColor);
var
  p: TgpPointf;
begin
  p.X := Center.X;
  p.Y := Center.Y;
  DrawDot(p, aWidth, CenterColor, OuterColor);

end;

procedure TMaxGDIEngine.DrawDot(const Center: TgpPointf; aWidth: single; CenterColor, OuterColor: TgpColor);
var
  Brush: iGPPathGradientBrush;
  Path: IGPGraphicsPath;
  SurroundColors: array [0 .. 0] of TgpColor;
  r: tgpRectf;
begin
  Path := TGPGraphicsPath.Create;

  r.X := Center.X - aWidth / 2;
  r.Y := Center.Y - aWidth / 2;
  r.Width := aWidth;
  r.Height := aWidth;

  Path.AddEllipse(r);

  Brush := TGPPathGradientBrush.Create(Path);
  Brush.CenterColor := CenterColor;
  Brush.GammaCorrection := true;
  SurroundColors[0] := OuterColor;
  Brush.SetSurroundColors(SurroundColors);
  Brush.SetFocusScales(0.2, 0.2);
  fGraphics.FillPath(Brush, Path);
end;

procedure TMaxGDIEngine.PaintTextcentered(const s: string; p: TgpPointf; aColor: TgpColor);
var
  r: tgpRectf;
begin
  r := fGraphics.MeasureString(s, fFont,
    TgpPointf.Create(0, 0));

  p.X := p.X - r.Width / 2;
  p.Y := p.Y - r.Height / 2;

  GDITextOut(p, s, aColor);
end;

function TMaxGDIEngine.GetMapPointAtViewPoint(const ViewPoint: TgpPointf): TgpPointf;
var
  p: TgpPointf;
begin
  p := GetTranslatedOffset;

  p.X := ViewPoint.X - p.X;
  p.Y := ViewPoint.Y - p.Y;

  p.X := p.X / FScale;
  p.Y := p.Y / FScale;

  result := p;
end;

function TMaxGDIEngine.GetMapPointAtViewPoint(const ViewX, ViewY: integer): TgpPointf;
begin
  result := GetMapPointAtViewPoint(
    TgpPointf.Create(ViewX, ViewY));
end;

function TMaxGDIEngine.GetMapPointAtViewPoint(const ViewPoint: Tpoint): Tpoint;
var
  pf: TgpPointf;
begin
  pf := GetMapPointAtViewPoint(ViewPoint.X, ViewPoint.Y);
  result.X := round(pf.X);
  result.Y := round(pf.Y);
end;

function TMaxGDIEngine.GetTextHeight(const s: string): single;
var
  r: tgpRectf;
begin
  r := fGraphics.MeasureString(s, fFont, TgpPointf.Create(0, 0));
  result := r.Height;
end;

function TMaxGDIEngine.GetTextWidth(const s: string): single;
var
  r: tgpRectf;
begin
  r := fGraphics.MeasureString(s, fFont, TgpPointf.Create(0, 0));
  result := r.Width;

end;

procedure TMaxGDIEngine.SetScale(const aValue: single);
var
  p: TgpPointf;
  Value: single;
begin
  Value := aValue;
  if Value < MIN_SCALE then
    Value := MIN_SCALE
  else if Value > MAX_SCALE then
    Value := MAX_SCALE;

  if FScale <> Value then
  begin
    p := GetMapPointAtViewPoint(TgpPointf.Create(fViewSize.Width / 2, fViewSize.Height / 2));
    FScale := Value;
    CenterOnPoint(p);
    FDirty := true;
  end;
end;

procedure TMaxGDIEngine.SetTransform;
begin
  fGraphics.ResetTransform;
  fGraphics.TranslateTransform(fViewoffset.X, fViewoffset.Y);
  fGraphics.ScaleTransform(FScale, FScale);
end;

procedure TMaxGDIEngine.SetViewContext(const Value: TViewContext);
begin
  FViewContext := Value;
end;

procedure TMaxGDIEngine.SetViewoffset(const Value: TgpPointf);
begin

end;

procedure TMaxGDIEngine.SetViewSize(const Value: TSize);
begin
  if not(fViewSize = Value) then
    FDirty := true;
  fViewSize := Value;
end;

procedure TMaxGDIEngine.SetDirty(const Value: Boolean);
begin
  FDirty := Value;
end;

procedure TMaxGDIEngine.SetOffscreenColor(const Value: TgpColor);
begin
  FDirty := true;
  FOffscreenColor := Value;
end;

procedure TMaxGDIEngine.PaintFrame;
var
  Region: IGPRegion;
  AreaIndex: integer;
  d: double;
  dir: string;
  off: TgpPointf;
begin
  if not FDirty then
    exit;

  d := fHPC.getTimeKey;
  if (fGraphics = nil) or (ViewSize.Width <> fBackBuffer.Width) or (ViewSize.Height <> fBackBuffer.Height) then
  begin
    // fCachedFirstLayer := nil;
    fGraphics := NIL;
    fBackBuffer := NIL;

    fBackBuffer := TGPBitmap.Create(ViewSize.Width, ViewSize.Height);
    fGraphics := TgpGraphics.Create(fBackBuffer);
    PrepareGraphics(fGraphics);
  end;

  FDirty := false;

  SetTransform;

  fGraphics.Clear(FOffscreenColor);
  // fSolidBrush.color := FBackGroundColor;
  // fGraphics.FillRectangle(fSolidBrush, tgpRect.Create(0, 0, fMapSize.Width, fMapSize.Height));

  { if fFirstLayer <> NIL then
    begin
    // if fCachedFirstLayer = NIL then
    // fCachedFirstLayer := TGPCachedBitmap.Create(fFirstLayer, fGraphics);

    // fGraphics.drawcachedBitmap(fCachedFirstLayer, 0, 0);
    fGraphics.DrawImage(fFirstLayer, 0, 0);
    end; }

  DoRender;

  d := (fHPC.getTimeKey - d) * 1000;

end;

procedure TMaxGDIEngine.PaintTextcentered(const s: string; p: TgpPointf; aColor, ShadowColor: TgpColor; ShadowOffset: integer);
var
  sp: TgpPointf;
  r: tgpRectf;
begin
  r := fGraphics.MeasureString(s, fFont,
    TgpPointf.Create(0, 0));

  p.X := p.X - r.Width / 2;
  p.Y := p.Y - r.Height / 2;

  sp.X := p.X + ShadowOffset;
  sp.Y := p.Y + ShadowOffset;
  GDITextOut(sp, s, ShadowColor);
  GDITextOut(p, s, aColor);
end;

procedure TMaxGDIEngine.TrapTheImage(var X, Y: single; w, h: single);

  Procedure Trap(var Coordinate: single; ImageSize: single; var off: single; AreaSize: integer);
  var
    dx: single;
  begin
    // min side
    dx := VISIBLE_MIN_IMAGE_MARGIN - ImageSize;
    if Coordinate < dx then
    begin
      dx := dx - Coordinate;
      Coordinate := Coordinate + dx;
      off := trunc(off + dx);
    end else begin
      // right side
      dx := AreaSize - VISIBLE_MIN_IMAGE_MARGIN;
      if Coordinate > dx then
      begin
        dx := dx - Coordinate;
        Coordinate := Coordinate + dx;
        off := trunc(off + dx);
      end;
    end;
  end;

begin
  Trap(X, w, fViewoffset.X, fViewSize.Width);
  Trap(Y, h, fViewoffset.Y, fViewSize.Height);
end;

procedure TMaxGDIEngine.Flush(DestBmp: TBitMap; X, Y: single; const CanvasColor: TgpColor);
var
  graphics: IGPGraphics;
begin
  graphics := TgpGraphics.Create(DestBmp.canvas.handle);
  graphics.Clear(CanvasColor);
  graphics.DrawImage(BackBuffer, X, Y);
  graphics := NIL;
end;

class procedure TMaxGDIEngine.RotateBitmap;
var
  Tmp: igpBitMap;
  Matrix: iGPMatrix;
  C: single;
  s: single;
  NewSize: TSize;
  Graphs: IGPGraphics;
  p: TgpPointf;
begin
  Tmp := TGPBitmap.Create(bmp.handle, bmp.Palette);
  Matrix := TGPMatrix.Create;
  Matrix.RotateAt(Degs, TgpPointf.Create(0.5 * bmp.Width, 0.5 * bmp.Height));
  if AdjustSize then
  begin
    C := Cos(DegToRad(Degs));
    s := Sin(DegToRad(Degs));
    NewSize.cx := round(bmp.Width * Abs(C) + bmp.Height * Abs(s));
    NewSize.cy := round(bmp.Width * Abs(s) + bmp.Height * Abs(C));
    bmp.Width := NewSize.cx;
    bmp.Height := NewSize.cy;
  end;
  Graphs := TgpGraphics.Create(bmp.canvas.handle);

  Graphs.Clear(TgpColor.Create(graphics.ColorToRGB(BkColor)));
  Graphs.SetTransform(Matrix);
  Graphs.DrawImage(Tmp, (Cardinal(bmp.Width) - Tmp.GetWidth) div 2,
    (Cardinal(bmp.Height) - Tmp.GetHeight) div 2);

end;

type
  // allows us to access the protected members
  TCustomEditProtectedHack = class(StdCtrls.TCustomEdit);

  { TExperimentPainterHelper }

procedure TViewContext.CenterOn(const pt: Tpoint);
begin
  fEngine.CenterOnPoint(pt);
end;

constructor TViewContext.Create;
begin
  inherited Create;
  fMainForm := PaintBox.Owner as TForm;
  fSubClass := TWndSubClass.Create(PaintBox.Owner);
  fSubClass.OnMessage := WndProc;
  fSubClass.HookedWnd := (PaintBox.Owner as TForm).handle;

  fTimer := TTimer.Create(nil);
  fTimer.Enabled := true;
  fTimer.Interval := trunc(1000 / 100);
  fTimer.OnTimer := TimerTimer;

  fEngine := ExperimentPainter;
  fPaintBox := PaintBox;
  fPaintBox.OnPaint := PaintBoxPaint;
  fPaintBox.OnMouseDown := PaintBoxMouseDown;
  fOrgPaintBoxMouseMove := fPaintBox.OnMouseMove;
  fPaintBox.OnMouseMove := PaintBoxMouseMove;
  fPaintBox.OnMouseUp := PaintBoxMouseUp;

  fPaintBox.ControlStyle := fPaintBox.ControlStyle + [csOpaque];
end;

destructor TViewContext.Destroy;
begin
  fSubClass.free;
  fTimer.free;
  if assigned(FedZoom) then
    TCustomEditProtectedHack(FedZoom).onchange := nil;

  fPaintBox.OnMouseDown := nil;
  fPaintBox.OnMouseUp := nil;
  fPaintBox.OnPaint := NIL;
  inherited;
end;

procedure TViewContext.edZoomChange(Sender: TObject);
var
  Value: double;
begin
  if fBlockZoomEditChangeEvent then
    exit;

  Value := StrToFloatWccDef(FedZoom.text, self.Zoom * 100) / 100;
  fBlockZoomEditChangeEvent := true;
  try
    Zoom := Value;
  finally
    fBlockZoomEditChangeEvent := false;
  end;

end;

procedure TViewContext.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState; var Handled: Boolean);
const
  INCREMENT_AMOUNT = 20;
begin
  if Key = vk_left then
  begin
    fEngine.ViewOffset := TgpPointf.Create(fEngine.ViewOffset.X + INCREMENT_AMOUNT, fEngine.ViewOffset.Y);
    // Inc(FOffset.X, INCREMENT_AMOUNT);
    Handled := true;
  end
  else if Key = vk_Right then
  begin
    fEngine.ViewOffset := TgpPointf.Create(fEngine.ViewOffset.X - INCREMENT_AMOUNT, fEngine.ViewOffset.Y);
    // Inc(FOffset.X, -INCREMENT_AMOUNT);
    Handled := true;
  end
  else if Key = vk_up then
  begin
    fEngine.ViewOffset := TgpPointf.Create(fEngine.ViewOffset.X, fEngine.ViewOffset.Y + INCREMENT_AMOUNT);
    // Inc(FOffset.Y, INCREMENT_AMOUNT);
    Handled := true;
  end
  else if Key = vk_down then
  begin
    fEngine.ViewOffset := TgpPointf.Create(fEngine.ViewOffset.X, fEngine.ViewOffset.Y - INCREMENT_AMOUNT);
    // Inc(FOffset.Y, -INCREMENT_AMOUNT);;
    Handled := true;
  end
  else if Key = vk_add then
  begin
    Zoom := Zoom * 1.1;
    Handled := true;
  end
  else if Key = VK_SUBTRACT then
  begin
    Zoom := Zoom * 0.9;
    Handled := true;
  end;

end;

procedure TViewContext.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: Tpoint; var Handled: Boolean);
var
  r: TRect;
  p: Tpoint;
begin
  p := Mouse.CursorPos;
  p := fPaintBox.ScreenToClient(p);
  r := Rect(0, 0, fPaintBox.Width, fPaintBox.Height);

  if PtInRect(r, p) then
  begin
    Handled := true;
    if WheelDelta > 0 then
      Zoom := Zoom * 1.1
    else if WheelDelta < 0 then
      Zoom := Zoom * 0.9;
  end;
end;

procedure TViewContext.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  fIsDraggingImage := true;
  fImageDragPos := Point(X, Y);
  if assigned(fOrgPaintBoxMouseMove) then
    fOrgPaintBoxMouseMove(Sender, Shift, X, Y)
end;

procedure TViewContext.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  dx, dy: single;
begin
  if fIsDraggingImage then
  begin
    dx := X - fImageDragPos.X;
    dy := Y - fImageDragPos.Y;

    dx := dx + fEngine.ViewOffset.X;
    dy := dy + fEngine.ViewOffset.Y;

    try
      fEngine.ViewOffset := TgpPointf.Create(dx, dy);
    except
      // do nothing
    end;
    fImageDragPos := Point(X, Y);
  end;
end;

procedure TViewContext.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  fIsDraggingImage := false;
  fImageDragPos := Point(0, 0);
end;

procedure TViewContext.PaintBoxPaint(Sender: TObject);
begin
  // do nothing
end;

procedure TViewContext.Repaint;
var
  graphics: IGPGraphics;
  size: TSize;
begin
  size := TSize.Create(fPaintBox.Width, fPaintBox.Height);
  if fEngine.ViewSize <> size then
  begin
    fEngine.ViewSize := size;
  end;

  fEngine.PaintFrame;

  if fEngine.BackBuffer = NIL then
    exit;

  try
    graphics := TgpGraphics.Create(fPaintBox.canvas.handle);
    graphics.DrawImage(fEngine.BackBuffer, 0, 0)
  except
    // do noting
  end;
  graphics := NIL;

end;

procedure TViewContext.SetedZoom(const Value: TCustomEdit);
begin
  FedZoom := Value;
  TCustomEditProtectedHack(FedZoom).onchange := edZoomChange;
end;

procedure TViewContext.SetZoom(const Value: double);
begin
  fEngine.Scale := Value;
  try
    fBlockZoomEditChangeEvent := true
  finally
    edZoom.text := IntToStr(round(self.Zoom * 100));
    fBlockZoomEditChangeEvent := false;
  end;
end;

procedure TViewContext.TimerTimer(Sender: TObject);
begin
  Repaint;
end;

procedure TViewContext.WMEraseBkgndEx(WinControl: TWinControl; var Message: TWmEraseBkgnd);

var
  i, Clip, SaveIndex: integer;
begin
  { Only erase background if we're not doublebuffering or painting to memory }
  with WinControl do
    if not DoubleBuffered or (TMessage(Message).wParam = TMessage(Message).lParam) then
    begin
      SaveIndex := SaveDC(Message.DC);
      Clip := SimpleRegion;
      if ControlCount > 0 then
      begin
        for i := 0 to ControlCount - 1 do
          if not(controls[i] is TWinControl) then
            { child windows already excluded }
            with controls[i] do
            begin
              if (Visible or (csDesigning in ComponentState) and not
                (csNoDesignVisible in ControlStyle))
                and (csOpaque in ControlStyle) then
              begin
                Clip := ExcludeClipRect(Message.DC, Left, Top, Left +
                  Width, Top + Height);
                if Clip = NullRegion then
                  break;
              end;
            end;
      end;
      if Clip <> NullRegion then
        FillRect(Message.DC, ClientRect, Brush.handle);
      RestoreDC(Message.DC, SaveIndex);
    end;
  Message.result := 1;

end;

procedure TViewContext.WndProc(var Msg: TMessage; var PrevendDefaultHandler: Boolean);
var
  em: ^TWmEraseBkgnd;
begin
  if Msg.Msg = WM_ERASEBKGND then
  begin
    PrevendDefaultHandler := true;
    em := @Msg;
    WMEraseBkgndEx(fMainForm, em^);
  end;
end;

function TViewContext.GetZoom: double;
begin
  result := fEngine.Scale;
end;

{ TArtifact }

constructor TArtifact.Create;
begin
  inherited
    FAlignment := taLeftJustify;
  FVerticalAlignment :=taAlignTop;
    FEditable := false;
  fEngine := aEngine;
  fArtifacts := TObjetList<TArtifact>.Create;
  fAnchor.initialize(0, 0);
  FRotation := 0;
  FScale := 1;
  FName := Copy(className, 2, Length(className));
end;

destructor TArtifact.Destroy;
begin
  fArtifacts.free;
  inherited;
end;

procedure TArtifact.DoRender;
var
  Artifact: TArtifact;
begin
for Artifact in fArtifacts do
begin
Artifact.PrepareFrame;
  Artifact.DoRender;
end;


end;



function TArtifact.GetWorldMatrix: iGPMatrix;
begin
result := FWorldMatrix;
end;

procedure TArtifact.PrepareFrame;
begin
fEngine.fGraphics.settransform(FWorldMatrix);
end;

procedure TArtifact.SetAlignment(const Value: TAlignment);
begin
if FAlignment<>value then
begin
  FAlignment := Value;

  fengine.Dirty:= true;
  RecalcMinMax
end;
end;

procedure TArtifact.SetAnchor(const Value: TgpPointf);
begin
if fAnchor<>value then
begin
  fAnchor := Value;

  fengine.Dirty:= true;
  RecalcMinMax
end;
end;

procedure TArtifact.SetEditable(const Value: Boolean);
begin
  FEditable := Value;
end;

procedure TArtifact.Setheight(const Value: single);
begin
if fheight<>value then
begin
  Fheight := Value;

  fengine.Dirty:= true;
  RecalcMinMax
end;
end;

procedure TArtifact.SetLeft(const Value: single);
begin
if flevt<> value then
begin
  FLeft := Value;

  fengine.Dirty:= true;
  RecalcMinMax
end;
end;

procedure TArtifact.RecalcMinMax;
var
begin
  case FAlignment of
    taLeftJustify:
      left := 0;
    taRightJustify:
      left := -Width;
    taCenter:
      left := -Width / 2;
  end;

  case FVerticalAlignment of
    taAlignTop:
      Top := 0;
    taAlignBottom:
      Top :=  - Height;
    taVerticalCenter:
      Top :=  - Height / 2;
  end;

end;

procedure TArtifact.RecalcWorldmatrix;
begin



if fParent<> nil then
FWorldMatrix.as
end;

procedure TArtifact.SetName(const Value: string);
begin
  FName := Value;
end;


procedure TArtifact.SetRotation(const Value: single);
begin
if frotation<>value then
begin
  FRotation := Value;
  fengine.Dirty:= true;
end;
end;

procedure TArtifact.SetScale(const Value: Single);
begin
if fscale<>value then
begin
  FScale := Value;
  fEngine.Dirty:=true;
end;
end;

procedure TArtifact.SetTop(const Value: single);
begin
if ftop<>velue then
begin
  FTop := Value;

  fengine.Dirty:= true;
  RecalcMinMax
end;
end;

procedure TArtifact.SetVerticalAlignment(const Value: TVerticalAlignment);
begin
if FVerticalAlignment<>value then
begin
  FVerticalAlignment := Value;
  fengine.Dirty:= true;
  RecalcMinMax
end;
end;

procedure TArtifact.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;



procedure TArtifact.SetWidth(const Value: single);
begin
if fwidth<>value then
begin
  FWidth := Value;
  fengine.Dirty:= true;
  RecalcMinMax
end;
end;


{ TText }

constructor TText.Create(aEngine: TMaxGDIEngine);
begin
  inherited;
  FAlignment := taCenter;
  FVerticalAlignment := taVerticalCenter;
  FFontSize := 14;
  FFontStyle := FontStyleRegular;
  fFontFamily := 'Arial';

  FShadowOffset := 1;
  FShadowColor := TgpColor.DarkGray;
  Fcaption := Name;

  ReInitFont;
end;

procedure TText.DoRender;
var
  sp, p: TgpPointf;
  begin
  p.Initialize(left, top);

 if FShadowOffset <> 0 then
  begin
    fEngine.fSolidBrush.color := FShadowColor;
    sp.initialize(p.X + FShadowOffset, p.Y + FShadowOffset);
    fEngine.fGraphics.DrawString(Fcaption, fFont, sp, fEngine.fSolidBrush);
  end;

  fEngine.fSolidBrush.color := FFontColor;
  fEngine.fGraphics.DrawString(Fcaption, fFont, p, fEngine.fSolidBrush);

  inherited;
end;







procedure TText.RecalcMinMax;
var
  p: TgpPointf;
  r:= tgpRectf;
begin
  r:= fGraphics.MeasureString(Fcaption, fFont, TgpPointf.Create(0, 0));
               fWidth:= r.width;
               fHeight := rheight;

               inherited;
end;

procedure TText.ReInitFont;
begin
  fFont := nil;
  fgpFontFamily := nil;

  fgpFontFamily := TGPFontFamily.Create(fFontFamily);
  fFont := TGPFont.Create(fgpFontFamily, FFontSize, FFontStyle, UnitPixel);
  RecalcMinMax;
end;


procedure TText.Setcaption(const Value: string);
begin
  if Fcaption <> Value then
  begin
    Fcaption := Value;
    fEngine.Dirty := true;
    RecalcMinMax;
  end;
end;

procedure TText.SetFontColor(const Value: TgpColor);
begin
  if Value <> FFontColor then
  begin
    FFontColor := Value;
    fEngine.Dirty := true;
  end;
end;

procedure TText.SetFontFamily(const Value: string);
begin
  if fFontFamily <> Value then
  begin
    fFontFamily := Value;
    fEngine.Dirty := true;
    ReInitFont;
  end;
end;

procedure TText.SetFontSize(const Value: integer);
begin
  if Value <> FFontSize then
  begin
    FFontSize := Value;
    fEngine.Dirty := true;
    ReInitFont;
  end;
end;

procedure TText.SetFontStyle(const Value: TGPFontStyle);
begin
  if FFontStyle <> Value then
  begin
    FFontStyle := Value;
    fEngine.Dirty := true;
    ReInitFont;
  end;
end;


procedure TText.SetShadowColor(const Value: TgpColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    fEngine.Dirty := true;
  end;
end;

procedure TText.SetShadowOffset(const Value: single);
begin
  if FShadowOffset <> Value then
  begin
    FShadowOffset := Value;
    fEngine.Dirty := true;
  end;
end;


end.
