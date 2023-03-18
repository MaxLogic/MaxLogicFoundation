unit MaxLogic.vcl.highDpi;

{
  to review:
  https://web.archive.org/web/20130106235952/http://msdn.microsoft.com/en-us/library/windows/desktop/dd464660%28v=vs.85%29.aspx
  http://zarko-gajic.iz.hr/making-the-glyph-property-high-dpi-aware-for-tbitbtn-and-tspeedbutton/
  http://zarko-gajic.iz.hr/resizing-delphis-timagelist-bitmaps-to-fit-high-dpi-scaling-size-for-menus-toolbars-trees-etc/
  http://zarko-gajic.iz.hr/delphi-high-dpi-road-ensuring-your-ui-looks-correctly/
  http://stackoverflow.com/questions/17614566/how-to-display-a-tbitmap-with-alpha-channel-on-timage-correctly
  https://zarko-gajic.iz.hr/delphi-high-dpi-road-ensuring-your-ui-looks-correctly/
}

{
  Version: 2.2
  History:
  2022-06-22: remove threading + remove monitor dpi dependancy, juse TControl.ScaleFactor
  2022-06-09: the newer versions of delphi have new properties for controls like ScaleFactor and methods like ScaleValue(), that superseedes methods that were used in this unit.
  2017-02-18: first implementation

  Description:
  adjusts some bitmaps for high DPI.
  it is quite simple it resises glyphs in:
  TImageList, TBitBtn, TSpeedButton, TImage
}

interface

uses
  GR32, GR32_Resamplers,
  windows, classes, sysUtils, controls, StdCtrls, ExtCtrls, graphics, forms,
  buttons, ComCtrls,
  SyncObjs, generics.collections, messages,
  JvFormPlacement;

type
  // forward declarations
  THighDpiMarker = class; // will be attacked to any form that is adjusted
  THighDpiAdjuster = class; // the main class that performs the adjustments

  THighDpiAdjuster = class
  private
    class var fSingelton: THighDpiAdjuster;
    class constructor CreateClass;
    class destructor DestroyClass;

    class function GetMarker(c: TComponent): THighDpiMarker;
    class function GetOrAddMarker(c: TComponent): THighDpiMarker;
  private
    fOrgOnActiveFormChange: TNotifyEvent;

    procedure AdjustTImageList(il: TImageList);
    procedure DiscardUnusedSpace(b1, b2: TBitmap; il: TImageList);
    function CopyBmp(b: TBitmap; hb: HBITMAP): boolean;

    procedure AdjustTImage(img: TImage);

    procedure AdjustBitBtn(b: TBitBtn);
    procedure AdjustSpeedButton(b: TSpeedButton);
    procedure AdjustGlyph(b: TBitmap; NumGlyphs: integer; Marker: THighDpiMarker);

    // those adjustments do not require a bitmap to be adjusted
    procedure AdjustColorBox(cb: TColorBox);

    // those may be called in a separate thread
    procedure ScaleBitmaps(const OrgBitmaps, ScaledBitmaps: TArray<TBitmap>; nw, nh: integer);
    procedure ScaleMaskedBitmaps(const OrgBitmaps, ScaledBitmaps: TArray<TBitmap>; nw, nh: integer; aBackGroundColor: TColor);
    procedure ScaleBmpUsingGraphics32(input, output: TBitmap32);
    procedure screenOnActiveFormChange(sender: TObject);

    // masked glyphs need to be scaled a bit differently to preserve the mask properly
    // iterate all pixels, replace those with aOldColor with aNewColor
    procedure OverrideColor(bmp: TBitmap; aOldColor, aNewColor: TColor);
    // this will replace all pixels with color of aMaskColor with black, and all other colors with white.
    procedure DoConvertBmpToBlackWhite(bmp: TBitmap; aMaskColor: TColor); overload;
    // similar as above, but this will just sift all colors to their extrems, so all colors above 125 will become 255, all below will become 0
    procedure DoConvertBmpToBlackWhite(bmp: TBitmap); overload;
    // this will apply the mask to the bmp
    // all pixels on mask of color equal to aMaskColor will be replaced on the bmp with the aNewColor
    procedure ApplyMask(bmp, mask: TBitmap; aNewColor, aMaskColor: TColor);

    procedure AdjustWinControl(aControl: TWinControl);
    procedure AdjustControl(aControl: TControl);
  public
    constructor Create;
    destructor Destroy; override;

    class function Singelton: THighDpiAdjuster;

    procedure ScaleBmp(Src, dst: TBitmap; newWidth, NewHeight: integer);
    // masked glyphs need to be scaled a bit differently to preserve the mask properly... remember, left, top pixel is used as the mask color
    procedure ScaleMaskedBmp(Src, dst: TBitmap; newWidth, NewHeight: integer; aBackGroundColor: TColor);

    // will adjust all controls on a form
    class procedure AdjustForm(f: TCustomForm);

    // this will mark the component and all its children as non scalable by this class
    class procedure MarkAsNonScalable(c: TComponent);

    // this method will register itself to the screen.onActiveFormChange event
    class procedure EnableAutoAdjusting;
  end;

  THighDpiMarker = class(TComponent)
  private type
    TBitmapsProc = reference to procedure(m: THighDpiMarker; const bitmaps: TArray<TBitmap>);
    TGetNewSizeProc = reference to procedure(Marker: THighDpiMarker; out newWidth, NewHeight: integer);
    TGetColorProc = reference to procedure(out aColor: TColor);
  private
    FdoNotScale: boolean;
    procedure ClearOrgBitmaps;
    procedure SetdoNotScale(const Value: boolean);
  public
    // this is important to know which size the bitmap was orginally
    AdjustmentDoneForScaleFactor: Single;
    OrgBitmaps: TArray<TBitmap>;
    OrgWidth: integer;
    OrgHeight: integer;
    OrgWIdths: TArray<integer>;
    // Glyphs use the top left pixel as a mask color, it is not easy to properly scale that
    BitmapsAreMasked: boolean;

    // here are some getters and setters to move the data from and to the controls
    PushBitmapsToControl: TBitmapsProc;
    GetScaledSize: TGetNewSizeProc;
    GetBackgroundColor: TGetColorProc;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure AdjustBitmaps;
    // if we call adjust, then it will automatically set the last used dpi, but some controls do have no bitmaps to adjust, so... we ned to update the last dpi here
    procedure UpdateLastAdjustDpi;
    function HasOrgBitmaps: boolean;
    procedure SetorgBitmap(g: TGraphic);
    // this will check the DoNotScale flag and it will compare the screen dpi with the last adjustment
    function CanScale: boolean;
    // either the owner or its owner....
    function Parent: TControl;

    property doNotScale: boolean read FdoNotScale write SetdoNotScale;
  end;

  TFormChangeScaleTrigger = class(TControl)
  protected
    fOrgMonitorDpiChangedEvent: TMonitorDpiChangedEvent;
    procedure FormOnAfterMonitorDpiChanged(sender: TObject; OldDPI: integer; NewDPI: integer);
    procedure ChangeScale(m, D: integer; isDpiChange: boolean); override;
  public
    // checks if that form already has a TFormChangeScaleTrigger instance
    class function has(aForm: TCustomForm): boolean;

    constructor Create(aOwner: TCustomForm); reintroduce;
  end;

  TGraphicStretching = record
  private
    class procedure DrawSrcToDst(Src, dst: TBitmap32); static;
  public
    class procedure stretch(bmp: TBitmap; newWidth, NewHeight: integer); static;
    class procedure stretchUsingGDIPlus(bmp: TBitmap; newWidth, NewHeight: integer); static;
    class procedure resizePng(const SourcePngFileName, newPngFileName: string; newWidth, NewHeight: integer); static;
  end;

  {This class will help to scale a picture inside a TImage to the best size of the timage}
  TAutoScaleToFit = class(TComponent)
  private
    fOrgOnResize: TNotifyEvent;
    fOrgPicture: TBitmap;
    fImage: TImage;
    procedure ImageOnResize(sender: TObject);
    procedure ClearPicture;
  public
    constructor Create(aOwner: TImage); reintroduce;
    destructor Destroy; override;
    // that will be called in the constructor
    procedure AssignOrginalImage(aPicture: TPicture); overload;
    procedure AssignOrginalImage(aGraphic: TGraphic); overload;
    // this will scale the backbuffer and assign to the TImage
    procedure Render;

    // will create an instance for each tImage it finds in the components property of the aOwner (if not already present)
    // NOTE: will attach itself only to those which have strech property set to true
    class procedure Setup(aOwner: TControl);

    // will check if an instance is already attached to the control
    class function hasAutoScaleToFit(aComponent: TComponent): boolean;
  end;

// those ethods helps to recalculate a value that was stored (in a config or somewhere else) with a different scaleFactor then the one currently used by the control
function ReScaleValue(const aValue: Double; aControl: TControl; const aOldScaleFactor: Single): Double; overload;
function ReScaleValue(const aValue: integer; aControl: TControl; const aOldScaleFactor: Single): integer; overload;
function ReScaleValue(const aValue: nativeInt; aControl: TControl; const aOldScaleFactor: Single): nativeInt; overload;

// when we save scaleFactors as strings, we might loose some detail, so we assume all scale factor not different by more then 0.001 are the same
function sameScaleFactor(const v1, v2: Single): boolean;

implementation

uses
  utsQuickPixel,
  GDIPAPI, GDIPOBJ, ClipBrd, math, utsWndSubClassing, pngImage,
  maxCallMeLater, autoFree;

type
  OpenControl = class(TControl)
  published
    property OnResize;
    property Color;
  end;

  OpenForm = class(TCustomForm)
  published
    property OnAfterMonitorDpiChanged;
  end;

  { THighDpiAdjuster }

class procedure THighDpiAdjuster.AdjustForm(f: TCustomForm);
var
  x: integer;
  c: TComponent;
  lRequireEnableDrawing: boolean;
  Marker: TFormChangeScaleTrigger;
begin
  if not TFormChangeScaleTrigger.has(f) then
    TFormChangeScaleTrigger.Create(f);

  if f.visible then
  begin
    SendMessage(f.Handle, WM_SETREDRAW, integer(False), 0);
    lRequireEnableDrawing := true;
  end
  else
    lRequireEnableDrawing := False;
  f.DisableAlign;
  try
    Singelton.AdjustWinControl(f);
  finally
    if lRequireEnableDrawing then
    begin
      SendMessage(f.Handle, WM_SETREDRAW, integer(true), 0);
      RedrawWindow((f as TWinControl).Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);
    end;
    f.EnableAlign;
  end;
end;

class function THighDpiAdjuster.GetMarker(c: TComponent): THighDpiMarker;
var
  x: integer;
begin
  result := nil;

  for x := c.ComponentCount - 1 downto 0 do
    if c.Components[x] is THighDpiMarker then
      Exit(THighDpiMarker(c.Components[x]));
end;

class constructor THighDpiAdjuster.CreateClass;
begin

end;

class function THighDpiAdjuster.GetOrAddMarker(c: TComponent): THighDpiMarker;
begin
  result := GetMarker(c);
  if not assigned(result) then
    result := THighDpiMarker.Create(c);
end;

procedure THighDpiAdjuster.ScaleBmpUsingGraphics32(input, output: TBitmap32);
begin
  TGraphicStretching.DrawSrcToDst(input, output);
end;

procedure THighDpiAdjuster.ScaleMaskedBitmaps(const OrgBitmaps,
  ScaledBitmaps: TArray<TBitmap>; nw, nh: integer; aBackGroundColor: TColor);
var
  x: integer;
begin
  for x := 0 to length(OrgBitmaps) - 1 do
    ScaleMaskedBmp(OrgBitmaps[x], ScaledBitmaps[x], nw, nh, aBackGroundColor);
end;

procedure THighDpiAdjuster.ScaleMaskedBmp(Src, dst: TBitmap; newWidth,
  NewHeight: integer; aBackGroundColor: TColor);
var
  src2, ScaledSrc2: TBitmap;
  mask, ScaledMask: TBitmap;
  MaskColor: TColor;
begin
  if not assigned(Src) then
    Exit;
  if (Src.Width = newWidth) and (Src.Height = NewHeight) then
  begin
    dst.Assign(Src);
    Exit;
  end;

  gc2(src2, TBitmap.Create);
  gc2(mask, TBitmap.Create);
  gc2(ScaledSrc2, TBitmap.Create);
  gc2(ScaledMask, TBitmap.Create);

  src2.Assign(Src);
  mask.Assign(Src);
  MaskColor := src2.Canvas.Pixels[0, src2.Height - 1];

  OverrideColor(src2, MaskColor, aBackGroundColor);

  // changes mask color to black and all other to white, and makes pixelFormat = pf1Bit
  DoConvertBmpToBlackWhite(mask, MaskColor);
  ScaleBmp(src2, ScaledSrc2, newWidth, NewHeight);
  ScaleBmp(mask, ScaledMask, newWidth, NewHeight);
  DoConvertBmpToBlackWhite(ScaledMask);

  ApplyMask(ScaledSrc2, ScaledMask, MaskColor, clBlack);
  // ensure to left bottom pixel holds the proper mask color
  ScaledSrc2.Canvas.Pixels[0, ScaledSrc2.Height - 1] := MaskColor;

  dst.Assign(ScaledSrc2);
end;

procedure THighDpiAdjuster.AdjustTImageList(il: TImageList);
var
  Marker: THighDpiMarker;
  b1, b2: TBitmap;
begin
  if il.Count = 0 then
    Exit;

  Marker := GetOrAddMarker(il);

  if not Marker.HasOrgBitmaps then
  begin
    // get the orginal size
    Marker.OrgWidth := il.Width;
    Marker.OrgHeight := il.Height;

    b1 := TBitmap.Create;
    b2 := TBitmap.Create;

    CopyBmp(b1, il.GetImageBitmap);
    CopyBmp(b2, il.GetMaskBitmap);

    // the images are actually biger then required, so limit them, we do not wanna to scale the whole buffer
    DiscardUnusedSpace(b1, b2, il);

    Marker.OrgBitmaps := [b1, b2];
  end;

  Marker.GetScaledSize :=

procedure(m: THighDpiMarker; out nw, nh: integer)
var
  ImagesPerRow, ImagesperColumn: integer;
begin
  ImagesPerRow := (m.OrgBitmaps[0].Width div m.OrgWidth);
  ImagesperColumn := (m.OrgBitmaps[0].Height div m.OrgHeight);

  nw := m.Parent.ScaleValue(m.OrgWidth);
  nh := m.Parent.ScaleValue(m.OrgHeight);

  il.SetSize(nw, nh);

  nw := nw * ImagesPerRow;
  nh := nh * ImagesperColumn;
end;

Marker.PushBitmapsToControl :=
  procedure(m: THighDpiMarker; const bitmaps: TArray<TBitmap>)
begin
  il.beginUpdate;
  try
    il.Clear;
    il.Add(bitmaps[0], bitmaps[1]);
  finally
    il.EndUpdate;
  end;
end;

Marker.AdjustBitmaps;
end;

function THighDpiAdjuster.CopyBmp(b: TBitmap; hb: HBITMAP): boolean;
var
  Src: TBitmap;
begin
  if hb = 0 then
    Exit(False);

  // http://docwiki.embarcadero.com/Libraries/Berlin/en/Vcl.Graphics.TBitmap.Handle
  // Handle is the HBITMAP encapsulated by the bitmap object. Avoid grabbing the handle directly since it causes the HBITMAP to be copied if more than one TBitmap shares the handle.
  // ok but we wanna a real copy, so we need to create a bitmap, grap the handle, and then copy it to the actuall result bitmap
  result := true;
  Src := TBitmap.Create;
  Src.Handle := hb;

  b.PixelFormat := Src.PixelFormat;
  b.Width := Src.Width;
  b.Height := Src.Height;

  b.Canvas.CopyRect(
    b.Canvas.ClipRect,
    Src.Canvas,
    b.Canvas.ClipRect);
  Src.ReleaseHandle;
  Src.Free;
end;

procedure THighDpiAdjuster.AdjustColorBox(cb: TColorBox);
var
  m: THighDpiMarker;
begin
  m := GetOrAddMarker(cb);
  if m.OrgHeight = 0 then
    m.OrgHeight := cb.ItemHeight;

  cb.ItemHeight := m.Parent.ScaleValue(m.OrgHeight);
  m.UpdateLastAdjustDpi;
end;

procedure THighDpiAdjuster.AdjustControl(aControl: TControl);
var
  Marker: THighDpiMarker;
  c: TControl;
begin
  c := aControl;

  if (c is TFormChangeScaleTrigger) then
    Exit;
  Marker := GetMarker(c);

  // we wanna prevent double adjustments, so we add a dummy component to the item, so we know that it was already adjusted
  // Note: it would be no problem if we would call it manually, but most likely we will call this every now and then.... meaning when the active form changes
  if (Marker = nil) or (Marker.CanScale) then
  begin
    if (c is TColorBox) then
      AdjustColorBox(TColorBox(c))
    else if c is TImage then
      AdjustTImage(TImage(c))
    else if c is TBitBtn then
      AdjustBitBtn(TBitBtn(c))
    else if c is TSpeedButton then
      AdjustSpeedButton(TSpeedButton(c));
  end;

  // now adjust all the children as well, but only if the current object was not marked to not be scalable
  if (Marker = nil) or (Marker.CanScale) then
    if c is TWinControl then
      AdjustWinControl(c as TWinControl);
end;

procedure THighDpiAdjuster.AdjustTImage(img: TImage);
var
  Marker: THighDpiMarker;
begin
  if TAutoScaleToFit.hasAutoScaleToFit((img)) then
    Exit;
  if (not img.AutoSize) then
    Exit;
  Marker := GetMarker(img);
  if assigned(Marker) and (Marker.doNotScale) then
    Exit;

  img.AutoSize := False;
  img.Proportional := true;

  TAutoScaleToFit.Create(img);
end;

procedure THighDpiAdjuster.AdjustBitBtn(b: TBitBtn);
var
  m: THighDpiMarker;
begin
  m := GetOrAddMarker(b);
  m.GetBackgroundColor := procedure(out aColor: TColor)
  begin
    aColor := OpenControl(b).Color;
  end;
  AdjustGlyph(b.Glyph, b.NumGlyphs, m);
end;

procedure THighDpiAdjuster.ScaleBmp(Src, dst: TBitmap; newWidth, NewHeight: integer);
var
  input, output: TBitmap32;
begin
  if not assigned(Src) then
    Exit;
  if (Src.Width = newWidth) and (Src.Height = NewHeight) then
  begin
    dst.Assign(Src);
    Exit;
  end;

  input := TBitmap32.Create();
  output := TBitmap32.Create;
  output.Width := newWidth;
  output.Height := NewHeight;

  input.Assign(Src);
  ScaleBmpUsingGraphics32(input, output);

  dst.Assign(output);
  input.Free;
  output.Free;

  { dst.PixelFormat := pf32Bit;
    dst.HandleType := bmDIB;
    dst.alphaformat := afDefined; }

  dst.HandleType := Src.HandleType;
  dst.AlphaFormat := Src.AlphaFormat;
end;

constructor THighDpiAdjuster.Create;
begin
  inherited;

end;

destructor THighDpiAdjuster.Destroy;
begin

  inherited;
end;

procedure THighDpiAdjuster.DiscardUnusedSpace(b1, b2: TBitmap; il: TImageList);
var
  nh, rowCount, ImagesPerRow: integer;
  f: Double;
begin
  ImagesPerRow := b1.Width div il.Width;
  f := il.Count / ImagesPerRow;
  rowCount := trunc(f);
  if frac(f) > 0 then
    inc(rowCount);

  nh := rowCount * il.Height;
  b1.Height := nh;
  b2.Height := nh;
end;

procedure THighDpiAdjuster.DoConvertBmpToBlackWhite(bmp: TBitmap);
var
  qp: TQuickPixel;
  x, y: integer;
  c1, b, w: utsQuickPixel.TRGB;
  grey: byte;
begin
  b := utsQuickPixel.ColorToRgb(clBlack);
  w := utsQuickPixel.ColorToRgb(clWhite);

  gc2(qp, TQuickPixel.Create(bmp));
  for y := 0 to bmp.Height - 1 do
  begin
    for x := 0 to bmp.Width - 1 do
    begin
      c1 := qp.RGBPixel[x, y];
      if not (c1.Equal(b) or c1.Equal(w)) then
      begin
        grey := (c1.R + c1.g + c1.b) div 3;
        if grey < 125 then
          qp.RGBPixel[x, y] := b
        else
          qp.RGBPixel[x, y] := w;
      end;
    end;
  end;
end;

procedure THighDpiAdjuster.DoConvertBmpToBlackWhite(bmp: TBitmap; aMaskColor: TColor);
var
  qp: TQuickPixel;
  x, y: integer;
  c1, b, w: utsQuickPixel.TRGB;
begin
  c1 := utsQuickPixel.ColorToRgb(aMaskColor);
  b := utsQuickPixel.ColorToRgb(clBlack);
  w := utsQuickPixel.ColorToRgb(clWhite);

  gc2(qp, TQuickPixel.Create(bmp));

  for y := 0 to bmp.Height - 1 do
  begin
    for x := 0 to bmp.Width - 1 do
    begin
      if qp.RGBPixel[x, y].Equal(c1) then
        qp.RGBPixel[x, y] := b
      else
        qp.RGBPixel[x, y] := w;
    end;
  end;

end;

class procedure THighDpiAdjuster.EnableAutoAdjusting;
begin
  Singelton.fOrgOnActiveFormChange := Screen.OnActiveFormChange;
  Screen.OnActiveFormChange := Singelton.screenOnActiveFormChange;
end;

procedure THighDpiAdjuster.AdjustWinControl(aControl: TWinControl);
var
  x: integer;
  c: TControl;
  comp: TComponent;
begin

  for x := 0 to aControl.ComponentCount - 1 do
  begin
    comp := aControl.Components[x];
    if comp is TImageList then
      AdjustTImageList(aControl.Components[x] as TImageList)
    else if comp is TControl then
      AdjustControl(comp as TControl);
  end;

  for x := 0 to aControl.controlCount - 1 do
  begin
    c := aControl.controls[x];
    AdjustControl(c);
  end;
end;

procedure THighDpiAdjuster.ApplyMask(bmp, mask: TBitmap; aNewColor,
  aMaskColor: TColor);
var
  MaskQp, DstQp: TQuickPixel;
  x, y: integer;
  c1, c2: utsQuickPixel.TRGB;
  rgba: utsQuickPixel.TRGBA;
  alfa: byte;
begin
  c1 := utsQuickPixel.ColorToRgb(aMaskColor);
  c2 := utsQuickPixel.ColorToRgb(aNewColor);

  bmp.PixelFormat := pf32bit;
  gc2(DstQp, TQuickPixel.Create(bmp));
  gc2(MaskQp, TQuickPixel.Create(mask));

  for y := 0 to bmp.Height - 1 do
    for x := 0 to bmp.Width - 1 do
    begin
      if MaskQp.RGBPixel[x, y].Equal(c1) then
      begin
        DstQp.RGBPixel[x, y] := c2;
        // The last parameter to the rgba() function is the "alpha" or "opacity" parameter. If you set it to 0 it will mean "completely transparent", ...
        alfa := 0;
      end
      else
        alfa := 255;

      rgba := DstQp.RGBAPixel[x, y];
      rgba.A := alfa;
      DstQp.RGBAPixel[x, y] := rgba;
    end;
end;

class destructor THighDpiAdjuster.DestroyClass;
begin
  if assigned(fSingelton) then
  begin
    fSingelton.Free;
    fSingelton := nil;
  end;
end;

class function THighDpiAdjuster.Singelton: THighDpiAdjuster;
begin
  if not assigned(fSingelton) then
    fSingelton := THighDpiAdjuster.Create;
  result := fSingelton;
end;

procedure THighDpiAdjuster.AdjustSpeedButton(b: TSpeedButton);
var
  m: THighDpiMarker;
begin
  m := GetOrAddMarker(b);
  m.GetBackgroundColor := procedure(out aColor: TColor)
  begin
    aColor := OpenControl(b).Color;
  end;
  AdjustGlyph(b.Glyph, b.NumGlyphs, m);
end;

procedure THighDpiAdjuster.screenOnActiveFormChange(sender: TObject);
begin
  AdjustForm(Screen.activecustomform);
  if assigned(fOrgOnActiveFormChange) then
    fOrgOnActiveFormChange(sender);
end;

procedure THighDpiAdjuster.AdjustGlyph(b: TBitmap; NumGlyphs: integer; Marker: THighDpiMarker);
var
  b1: TBitmap;
begin
  if not assigned(b) then
    Exit;

  if not Marker.HasOrgBitmaps then
  begin
    b1 := TBitmap.Create();
    b1.Assign(b);
    Marker.OrgBitmaps := [b1];
    Marker.BitmapsAreMasked := true;
  end;

  Marker.GetScaledSize := (
    procedure(m: THighDpiMarker; out nw, nh: integer)
    begin
      nh := m.Parent.ScaleValue(m.OrgBitmaps[0].Height);
      // why the division and then the multiplication?
      // imagine what happens if you will scale a 25x25 bitmap consisting of 3 glyphs by a factor of 1.5?
      // your images will be somehow off...
      nw := m.OrgBitmaps[0].Width div NumGlyphs;
      nw := m.Parent.ScaleValue(nw) * NumGlyphs;
    end);

  Marker.PushBitmapsToControl :=
    procedure(m: THighDpiMarker; const bitmaps: TArray<TBitmap>)
  begin
    b.Assign(bitmaps[0]);
  end;

  Marker.AdjustBitmaps;
end;

procedure THighDpiAdjuster.ScaleBitmaps(const OrgBitmaps, ScaledBitmaps: TArray<TBitmap>; nw, nh: integer);
var
  i: integer;
  x: integer;
begin
  for i := 0 to length(OrgBitmaps) - 1 do
    ScaleBmp(OrgBitmaps[i], ScaledBitmaps[i], nw, nh);
end;

class procedure THighDpiAdjuster.MarkAsNonScalable(c: TComponent);
var
  m: THighDpiMarker;
begin
  m := GetOrAddMarker(c);
  m.doNotScale := true;
end;

procedure THighDpiAdjuster.OverrideColor(bmp: TBitmap; aOldColor,
  aNewColor: TColor);
var
  qp: TQuickPixel;
  x, y: integer;
  c1, c2: utsQuickPixel.TRGB;
begin
  c1 := utsQuickPixel.ColorToRgb(aOldColor);
  c2 := utsQuickPixel.ColorToRgb(aNewColor);
  if c1.Equal(c2) then
    Exit; // nothing to do...

  gc2(qp, TQuickPixel.Create(bmp));

  for y := 0 to bmp.Height - 1 do
    for x := 0 to bmp.Width - 1 do
      if qp.RGBPixel[x, y].Equal(c1) then
        qp.RGBPixel[x, y] := c2;
end;

{ THighDpiMarker }

constructor THighDpiMarker.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  AdjustmentDoneForScaleFactor := 1; // initialize, so we will not scale anything in normal conditions
  FdoNotScale := False;
end;

destructor THighDpiMarker.Destroy;
var
  b: TBitmap;
begin
  ClearOrgBitmaps;

  inherited;
end;

procedure THighDpiMarker.AdjustBitmaps;
var
  nw, nh: integer;
  ScaledBitmaps: TArray<TBitmap>;
  x: integer;
  BackGroundColor: TColor;
begin
  // no need to do anything if the dpi did not change, or if there are no bitmaps to scale
  if (AdjustmentDoneForScaleFactor = self.Parent.ScaleFactor) or (length(OrgBitmaps) = 0) then
    // free the references that may be there
    PushBitmapsToControl := nil
  else
  begin
    AdjustmentDoneForScaleFactor := self.Parent.ScaleFactor;

    self.GetScaledSize(self, nw, nh);
    SetLength(ScaledBitmaps, length(OrgBitmaps));

    for x := 0 to length(ScaledBitmaps) - 1 do
      ScaledBitmaps[x] := TBitmap.Create;

    if BitmapsAreMasked then
    begin
      if assigned(GetBackgroundColor) then
        GetBackgroundColor(BackGroundColor)
      else
        BackGroundColor := clBtnFace;

      THighDpiAdjuster.Singelton.ScaleMaskedBitmaps(OrgBitmaps, ScaledBitmaps, nw, nh, BackGroundColor);
    end
    else
      THighDpiAdjuster.Singelton.ScaleBitmaps(OrgBitmaps, ScaledBitmaps, nw, nh);

    PushBitmapsToControl(self, ScaledBitmaps);
    if owner is TControl then
      OpenControl(owner).invalidate;

    for x := 0 to length(ScaledBitmaps) - 1 do
      ScaledBitmaps[x].Free;
    ScaledBitmaps := nil;
  end;
end;

function THighDpiMarker.HasOrgBitmaps: boolean;
begin
  result := length(OrgBitmaps) <> 0;
end;

function THighDpiMarker.Parent: TControl;
var
  c: TComponent;
begin
  Result := Nil;
  c := owner;
  while c <> nil do
  begin
    if c is TControl then
      Exit(c as TControl);
    c := c.owner;
  end;
end;

procedure THighDpiMarker.SetdoNotScale(const Value: boolean);
begin
  FdoNotScale := Value;
end;

procedure THighDpiMarker.SetorgBitmap(g: TGraphic);
begin
  ClearOrgBitmaps;
  OrgBitmaps := TArray<TBitmap>.Create(TBitmap.Create);
  OrgBitmaps[0].Assign(g);
end;

procedure THighDpiMarker.ClearOrgBitmaps;
var
  b: TBitmap;
begin
  for b in OrgBitmaps do
  begin
    b.Free;
  end;
  OrgBitmaps := nil;
end;

function THighDpiMarker.CanScale: boolean;
begin
  result := (doNotScale = False) and
    (AdjustmentDoneForScaleFactor <> self.Parent.ScaleFactor);
end;

procedure THighDpiMarker.UpdateLastAdjustDpi;
begin
  AdjustmentDoneForScaleFactor := self.Parent.ScaleFactor;
end;

{ TGraphicStretching }

class procedure TGraphicStretching.stretch(bmp: TBitmap; newWidth, NewHeight: integer);
begin

end;

class procedure TGraphicStretching.DrawSrcToDst(Src, dst: TBitmap32);
var
  R: TKernelResampler;
begin
  {
    in case of speed problems, see this comment:
    Perhaps it was slow for him because he was doing fast updates each second? I had the same performance problem and this was on an Intel Quad Core I5 with plenty of memory. I switched from a kernel resampler to a draft resampler (TDraftResampler) and the performance problem went away. In my application's case I was updating a 1100w x 1100h image 20 times a second and then downsampling and the Lanczos kernel was too slow to keep up. Thanks for the tip about the Graphics32 library though. It's a real blessing and it really helped improve the look of my downsampled image. – Robert Oschler Oct 10 '10 at 14:42
  }
  R := TKernelResampler.Create(Src);
  R.Kernel := TLanczosKernel.Create;
  dst.Draw(dst.BoundsRect, Src.BoundsRect, Src);
end;

class procedure TGraphicStretching.stretchUsingGDIPlus(bmp: TBitmap; newWidth, NewHeight: integer);
var
  gr: TGPGraphics;
  Src: TGPBitmap;
  Bounds: TGPRectF;
  dstBitMap: TBitmap;
begin
  // set up the new scaled destination bitmap
  dstBitMap := TBitmap.Create;
  dstBitMap.Width := newWidth;
  dstBitMap.Height := NewHeight;

  // now create the graphics that will draw to that new created destination bitmap
  gr := TGPGraphics.Create(dstBitMap.Canvas.Handle);
  Src := TGPBitmap.Create(bmp.Handle, bmp.Palette);

  gr.SetInterpolationMode(InterpolationModeHighQualityBicubic);
  gr.SetSmoothingMode(SmoothingModeHighQuality);
  gr.DrawImage(Src, 0, 0, newWidth, NewHeight);

  Src.Free;
  gr.Free;

  bmp.Assign(dstBitMap);
  dstBitMap.Free;
end;

class procedure TGraphicStretching.resizePng(const SourcePngFileName, newPngFileName: string; newWidth, NewHeight: integer);
const
  // guids for writing
  gGIf: TGUID = '{557CF402-1A04-11D3-9A73-0000F81EF32E}';
  gPNG: TGUID = '{557CF406-1A04-11D3-9A73-0000F81EF32E}';
  gPJG: TGUID = '{557CF401-1A04-11D3-9A73-0000F81EF32E}';
  gBMP: TGUID = '{557CF400-1A04-11D3-9A73-0000F81EF32E}';
  gTIF: TGUID = '{557CF405-1A04-11D3-9A73-0000F81EF32E}';

var
  input: TGPImage;
  output: TGPBitmap;
  Encoder: TGUID;
  graphics: TGPGraphics;
begin
  input := TGPImage.Create(SourcePngFileName);
  try
    // create the output bitmap in desired size
    output := TGPBitmap.Create(newWidth, NewHeight, PixelFormat32bppARGB);
    try
      // create graphics object for output image
      graphics := TGPGraphics.Create(output);
      try
        // set the composition mode to copy
        graphics.SetCompositingMode(CompositingModeSourceCopy);
        // set high quality rendering modes
        graphics.SetInterpolationMode(InterpolationModeHighQualityBicubic);
        graphics.SetPixelOffsetMode(PixelOffsetModeHighQuality);
        graphics.SetSmoothingMode(SmoothingModeHighQuality);
        // draw the input image on the output in modified size
        graphics.DrawImage(input, 0, 0, output.GetWidth, output.GetHeight);
      finally
        graphics.Free;
      end;
      // get encoder and encode the output image
      // if GetEncoderClsid('image/png', Encoder) <> -1 then
      Encoder := gPNG;
      output.Save(newPngFileName, Encoder)
        // else
// raise Exception.Create('Failed to get encoder.');
    finally
      output.Free;
    end;
  finally
    input.Free;
  end;
end;

{ Others }

function ReScaleValue(const aValue: Double; aControl: TControl; const aOldScaleFactor: Single): Double;
var
  v: Double;
begin
  if sameScaleFactor(aControl.ScaleFactor, aOldScaleFactor) then
    result := aValue
  else
  begin
    v := aValue / aOldScaleFactor;
    result := aControl.ScaleValue(v);
  end;
end;

function ReScaleValue(const aValue: integer; aControl: TControl; const aOldScaleFactor: Single): integer;
var
  f1, f2: Double;
begin
  if sameScaleFactor(aControl.ScaleFactor, aOldScaleFactor) then
    result := aValue
  else
  begin
    f1 := aValue;
    f2 := ReScaleValue(f1, aControl, aOldScaleFactor);
    result := round(f2);
  end;
end;

function ReScaleValue(const aValue: nativeInt; aControl: TControl; const aOldScaleFactor: Single): nativeInt; overload;
var
  f1, f2: Double;
begin
  if sameScaleFactor(aControl.ScaleFactor, aOldScaleFactor) then
    result := aValue
  else
  begin
    f1 := aValue;
    f2 := ReScaleValue(f1, aControl, aOldScaleFactor);
    result := round(f2);
  end;
end;

function sameScaleFactor(const v1, v2: Single): boolean;
var
  f: Single;
begin
  f := v1 - v2;
  if f < 0 then
    f := f * -1;
  if f < 0.001 then
    result := true
  else
    result := False;
end;

{ TAutoScaleToFit }

procedure TAutoScaleToFit.ClearPicture;
begin
  if assigned(fOrgPicture) then
    FreeAndNil(fOrgPicture);
  if assigned(fImage.Picture) then
    fImage.Picture := nil;
end;

procedure TAutoScaleToFit.AssignOrginalImage(aPicture: TPicture);
begin
  if not assigned(aPicture) then
    ClearPicture
  else
    AssignOrginalImage(aPicture.Graphic);
end;

procedure TAutoScaleToFit.AssignOrginalImage(aGraphic: TGraphic);
begin
  if not assigned(aGraphic) then
    ClearPicture
  else
  begin
    if not assigned(fOrgPicture) then
      fOrgPicture := TBitmap.Create;
    fOrgPicture.Assign(aGraphic);
    Render;
  end;

end;

constructor TAutoScaleToFit.Create(aOwner: TImage);
begin
  inherited Create(aOwner);
  fImage := aOwner;
  fOrgOnResize := OpenControl(aOwner).OnResize;
  OpenControl(aOwner).OnResize := self.ImageOnResize;
  self.AssignOrginalImage(aOwner.Picture);
end;

destructor TAutoScaleToFit.Destroy;
begin
  if assigned(fOrgOnResize) and assigned(self.owner) then
    OpenControl(self.owner).OnResize := fOrgOnResize;
  if assigned(fOrgPicture) then
    fOrgPicture.Free;
  inherited;
end;

class function TAutoScaleToFit.hasAutoScaleToFit(
  aComponent: TComponent): boolean;
var
  c: TComponent;
  x: integer;
begin
  result := False;

  for x := 0 to aComponent.ComponentCount - 1 do
  begin
    c := aComponent.Components[x];
    if c is TAutoScaleToFit then
      Exit(true);
  end;
end;

procedure TAutoScaleToFit.ImageOnResize(sender: TObject);
begin
  Render;
  if assigned(fOrgOnResize) then
    fOrgOnResize(sender);
end;

procedure TAutoScaleToFit.Render;
var
  w, h: integer;
  rw, rh: Double; // ratio for width and height
  ratio, scale: Double;
  b: TBitmap;
begin
  if not assigned(fOrgPicture) then
    Exit;

  rw := fImage.Width / fOrgPicture.Width;
  rh := fImage.Height / fOrgPicture.Height;
  ratio := fOrgPicture.Height / fOrgPicture.Width;

  if rw < rh then
    scale := rw
  else
    scale := rh;

  if scale = 1 then
    fImage.Picture.Assign(fOrgPicture)
  else
  begin
    w := round(fOrgPicture.Width * scale);
    if w > fImage.Width then
      w := fImage.Width;

    h := round(w * ratio);
    if h > fImage.Height then
      h := fImage.Height;

    b := TBitmap.Create;
    try
      THighDpiAdjuster.Singelton.ScaleBmp(fOrgPicture, b, w, h);
      fImage.Picture.Assign(b);
    finally
      b.Free;
    end;
  end;
end;

class procedure TAutoScaleToFit.Setup(aOwner: TControl);
var
  c: TComponent;
  x: integer;
begin
  for x := 0 to aOwner.ComponentCount - 1 do
  begin
    c := aOwner.Components[x];
    if c is TImage then
      if (c as TImage).stretch then
        if not hasAutoScaleToFit(c) then
          TAutoScaleToFit.Create(c as TImage);

  end;
end;

{ TFormChangeScaleTrigger }

procedure TFormChangeScaleTrigger.ChangeScale(m, D: integer;
  isDpiChange: boolean);
begin
  inherited;

  maxCallMeLater.CallmeLater(
    procedure
    begin
      THighDpiAdjuster.AdjustForm(owner as TCustomForm);
    end, 50, self);
end;

constructor TFormChangeScaleTrigger.Create(aOwner: TCustomForm);
begin
  inherited Create(aOwner);
  visible := False;
  Parent := aOwner;

  fOrgMonitorDpiChangedEvent := OpenForm(aOwner).OnAfterMonitorDpiChanged;
  OpenForm(aOwner).OnAfterMonitorDpiChanged := FormOnAfterMonitorDpiChanged;
end;

procedure TFormChangeScaleTrigger.FormOnAfterMonitorDpiChanged(
  sender: TObject; OldDPI, NewDPI: integer);
begin
  if assigned(fOrgMonitorDpiChangedEvent) then
    fOrgMonitorDpiChangedEvent(sender, OldDPI, NewDPI);
  THighDpiAdjuster.AdjustForm(owner as TCustomForm);
end;

class function TFormChangeScaleTrigger.has(aForm: TCustomForm): boolean;
var
  x: integer;
begin
  result := False;
  for x := aForm.ComponentCount - 1 downto 0 do
    if aForm.Components[x] is TFormChangeScaleTrigger then
      Exit(true);
end;

end.



