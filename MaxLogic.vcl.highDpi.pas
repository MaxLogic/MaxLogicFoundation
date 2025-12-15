unit maxLogic.vcl.highDpi;

{ Requires:
  https://github.com/MahdiSafsafi/DDetours
}

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
  2024-02-14: add support for TImageCollection and TVirtualImageList to move glyphs and images there on app startup for all forms to enable high dpi support for legacy applications
  2022-06-22: remove threading + remove monitor dpi dependancy, use TControl.ScaleFactor
  added more RTTI based replacement to support a wider array of components
  2022-06-09: the newer versions of delphi have new properties for controls like ScaleFactor and methods like ScaleValue(), that superseedes methods that were used in this unit.
  2017-02-18: first implementation

  Description:
  adjusts some bitmaps for high DPI.
  it is quite simple it resises glyphs in:
  TImageList, TBitBtn, TSpeedButton, TImage
}

interface

uses
  winApi.Windows, system.Classes, system.SysUtils, vcl.Controls, vcl.StdCtrls, vcl.ExtCtrls, Graphics, vcl.Forms,
  Buttons, ComCtrls, vcl.ImgList,
  system.syncObjs, generics.collections, Messages,
  vcl.BaseImageCollection, vcl.ImageCollection, vcl.VirtualImageList,
  maxLogic.GraphicUtils,
  DDetours,
  System.Generics.Defaults, System.Types;

type
  // forward declarations
  THighDpiMarker = class; // will be attached to any form that is adjusted
  THighDpiAdjuster = class; // the main class that performs the adjustments

  TStrProc = reference to procedure(const s: string);

  THighDpiAdjuster = class
  public
    class var OnLog: TStrProc;
    class procedure log(const s: string); overload;
    class procedure log(const s: String; C: TComponent); overload;
  private type
      // a Glyph in TBitBtn or TSpeedbutton and similar can have multiple elements, up to 4 on a single bitmap... we need to split this up
      TGlyphCollectionItem = class
      public
        PropPrefix: string;
        Image: TBitmap;
        ImageName: string;
          Index: integer;
        constructor Create(aIndex: integer);
        destructor Destroy; override;
      end;

      TGlyphCollection = class
      private
        fTransparentColor: TColor;
        fHasTransparentColor: boolean;
        fMainGlyph: TBitmap;
        procedure applyTransparency(aBmp: TBitmap);
      public
        Items: TObjectList<TGlyphCollectionItem>;
        constructor Create(aParent: THighDpiAdjuster; aGlyph: TGraphic; aNumGlyphs: integer);
        destructor Destroy; override;
        procedure EnsureDisabledImagePresent(aVirtualImageList: TVirtualImageList);
      end;
  private
    class var fSingelton: THighDpiAdjuster;
    class constructor CreateClass;
    class destructor DestroyClass;
  private
    fDataModule: TDataModule;
    fImageCollection: TImageCollection;
    fOrgOnActiveFormChange: TNotifyEvent;
    fOrgOnActiveFormChangeAssigned: boolean;
    fMaxComponentCount: integer;

    function AddToImageCol(aGraphic: TGraphic; out aImageName: String): boolean;

    function CreateVirtualImgList(aOwner: TComponent; aMarker: THighDpiMarker; aWidth, aHeight: integer): TVirtualImageList;
    function GetOrCreateVirtualImgList(aOwner: TComponent; aMarker: THighDpiMarker; aWidth, aHeight: integer): TVirtualImageList;

    function AdjustTImageList(aImageList: TImageList): TVirtualImageList;
    procedure AdjustTImage(aImage: TImage);

    function RedirectImageListReferenceToVirtualImage(c: TComponent): boolean;
    function AdjustUsingRttiLookingForGlyphAndImageName(c: TComponent): boolean;
    // returns a sha256 hash as hex string
    function CalcHash(aGraphic: TGraphic): RawByteString;


    procedure screenOnActiveFormChange(Sender: TObject);

  public
    constructor Create;
    destructor Destroy; override;

    procedure AdjustComponent(aComponent: TComponent; aAlreadyProcessedDic: TDictionary<TComponent, boolean>);

    // thread safe
    class function Singelton: THighDpiAdjuster;

    // will adjust all components and controls on a form
    class procedure AdjustForm(AfORM: TCustomForm);

    // this will mark the component and all its children as non scalable by this class
    class procedure MarkAsNonScalable(c: TComponent);

    // this method will register itself to the screen.onActiveFormChange event
    class procedure EnableAutoAdjusting;

    // marker related, mostly for internal use
    class function GetMarker(c: TComponent): THighDpiMarker;
    class function GetOrAddMarker(c: TComponent): THighDpiMarker; overload;
    class function GetOrAddMarker(c: TComponent; out aIsNew: boolean): THighDpiMarker; overload;
  public
    property ImageCollection: TImageCollection read fImageCollection;
  end;

  THighDpiMarker = class(TComponent)
  private
    FdoNotScale: boolean;
    procedure SetdoNotScale(const Value: boolean);
  public
    VirtualImageList: TVirtualImageList; // used for TImageList

    // this will check the DoNotScale flag and it will compare the screen dpi with the last adjustment
    function CanScale: boolean;

    property doNotScale: boolean read FdoNotScale write SetdoNotScale;
  end;

  TTrampolineTImageDestRect = function(Sender: TObject): TRect;

  TTImageMarker = class(THighDpiMarker)
  private
    class var TrampolineDestRect: TTrampolineTImageDestRect;
    class var Intercepted: boolean;
    class destructor ClassDestroy;
  public
    ImageHash: AnsiString; // as hex enoded sha256
    ImageSize: TPoint; // used for TImage
    WasAutoSized: boolean;

    // returns nil if none was found
    class function GetMarker(aComponent: TComponent): TTImageMarker; static;
    class function GetOrCreateMarker(aComponent: TComponent; out aIsNew: boolean): TTImageMarker; static;
  end;

  TFormChangeScaleTrigger = class(TComponent)
  private
    fLastPixelsperInch: integer;
  protected
    fOrgMonitorDpiChangedEvent: TMonitorDpiChangedEvent;
    procedure FormOnAfterMonitorDpiChanged(Sender: TObject; OldDPI: integer; NewDPI: integer);
  public
    // checks if that form already has a TFormChangeScaleTrigger instance
    class function has(AfORM: TCustomForm): boolean;
    class function get(AfORM: TCustomForm; out aMarker: TFormChangeScaleTrigger): boolean;

    constructor Create(aOwner: TCustomForm); reintroduce;
    destructor Destroy; override;

    property LastPixelsperInch: integer read fLastPixelsperInch;
  end;

// those methods helps to recalculate a value that was stored (in a config or somewhere else) with a different scaleFactor then the one currently used by the control
function ReScaleValue(const aValue: double; aControl: TControl; const aOldScaleFactor: Single): double; overload;
function ReScaleValue(const aValue: integer; aControl: TControl; const aOldScaleFactor: Single): integer; overload;
function ReScaleValue(const aValue: int64; aControl: TControl; const aOldScaleFactor: Single): int64; overload;

// when we save scaleFactors as strings, we might loose some detail, so we assume all scale factor not different by more then 0.001 are the same
function sameScaleFactor(const v1, v2: Single): boolean;

function TryScale(aOwner: TComponent; const p: TPoint): TPoint;

implementation

uses
  maxLogic.QuickPixel,
  System.Math, System.StrUtils,
  AutoFree, maxLogic.RTTIHelper, System.Hash,
  pngImage, System.Rtti, Maxlogic.GdiUtils;

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

  OpenVirtualImageList = class(TVirtualImageList)

  end;

  TOpenImage = class(TImage)

  end;

  TImageHelper = class helper for TImage
  public
    function CloneOfdDestRect: TRect;
  end;

function TImageHelper.CloneOfdDestRect: TRect;
var
  w, h, cw, ch: integer;
  xyaspect: double;
begin
  w := Picture.Width;
  h := Picture.Height;
  cw := ClientWidth;
  ch := ClientHeight;
  if Stretch or (Proportional and ((w > cw) or (h > ch))) then
  begin
    if Proportional and (w > 0) and (h > 0) then
    begin
      xyaspect := w / h;
      if w > h then
      begin
        w := cw;
        h := trunc(cw / xyaspect);
        if h > ch then // woops, too big
        begin
          h := ch;
          w := trunc(ch * xyaspect);
        end;
      end
      else
      begin
        h := ch;
        w := trunc(ch * xyaspect);
        if w > cw then // woops, too big
        begin
          w := cw;
          h := trunc(cw / xyaspect);
        end;
      end;
    end
    else
    begin
      w := cw;
      h := ch;
    end;
  end;

  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := w;
    Bottom := h;
  end;

  if Center then
    OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);
end;

function TryScale(aOwner: TComponent; const p: TPoint): TPoint;
var
  lDone: boolean;
begin
  Result := p;
  lDone := False;
  while (not lDone) and (aOwner <> nil) do
  begin
    lDone := True;
    if aOwner is TControl then
      Result := (aOwner as TControl).ScaleValue(p)
    else if aOwner is TForm then
      Result := (aOwner as TForm).ScaleValue(p)
    else if aOwner is TFrame then
      Result := (aOwner as TFrame).ScaleValue(p)
    else
    begin
      lDone := False;
      aOwner := aOwner.Owner;
    end;
  end;
end;

{ THighDpiAdjuster }

class procedure THighDpiAdjuster.AdjustForm(AfORM: TCustomForm);
begin
  if assigned(application)
    and (not application.Terminated)
    and assigned(AfORM)
    and (not (csDestroying in AfORM.ComponentState)) then
  begin
    Singelton.AdjustComponent(AfORM, nil);
    AfORM.Invalidate;
    Singelton.fImageCollection.Change;
  end;
end;

class function THighDpiAdjuster.GetMarker(c: TComponent): THighDpiMarker;
var
  X: integer;
begin
  Result := nil;

  for X := c.ComponentCount - 1 downto 0 do
    if c.Components[X] is THighDpiMarker then
      exit(THighDpiMarker(c.Components[X]));
end;

class constructor THighDpiAdjuster.CreateClass;
begin
  fSingelton := nil;
end;

function THighDpiAdjuster.GetOrCreateVirtualImgList(aOwner: TComponent;
  aMarker: THighDpiMarker; aWidth, aHeight: integer): TVirtualImageList;
var
  lComponentName: string;
  lComponent: TComponent;
begin
  Result := nil;
  lComponentName := 'MaxLogicHighDpiVirtualImageList_' + aWidth.ToString + 'x' + aHeight.ToString;
  lComponent := aOwner.FindComponent(lComponentName);

  if lComponent = nil then
  begin
    Result := CreateVirtualImgList(aOwner, aMarker, aWidth, aHeight);
    Result.Name := lComponentName;
  end
  else
  begin
    Result := lComponent as TVirtualImageList;
    if aMarker <> nil then
      aMarker.VirtualImageList := Result;
  end;
end;

class procedure THighDpiAdjuster.log(const s: String; C: TComponent);
var
  n: String;
begin
  if c = nil then
    n:= 'NIL'
  else begin
    n:= c.Name+':'+c.ClassName;
    while c.Owner <> nil do
    begin
      c:= c.Owner;
      n:= c.Name+':'+c.ClassName + '->' +n;
    end;
  end;
  Log(Format(s, [n]));
end;

class procedure THighDpiAdjuster.log(const s: string);
begin
  if assigned(OnLog) then
    OnLog(s);
end;

function THighDpiAdjuster.CreateVirtualImgList(aOwner: TComponent;
  aMarker: THighDpiMarker; aWidth, aHeight: integer): TVirtualImageList;
var
  p: TPoint;
begin
  Result := TVirtualImageList.Create(aOwner);

  if aMarker <> nil then
    aMarker.VirtualImageList := Result;

  Result.PreserveItems := True;
  Result.ImageCollection := fImageCollection;
  p := TryScale(aOwner, point(aWidth, aHeight));
  Result.Width := p.X;
  Result.Height := p.y;

  Result.ColorDepth := TColorDepth.cd32Bit;
  // Result.DrawWWingStyle:= TDrawingStyle.dsTransparent;
  // Result.BkGColor := TRGB.Create(255,0,255).ToColor;
  // Result.BlendColor := clNone;
  // Result.Masked := True;
end;

class function THighDpiAdjuster.GetOrAddMarker(c: TComponent): THighDpiMarker;
var
  lIsNew: boolean;
begin
  Result := GetOrAddMarker(c, lIsNew);
end;

class function THighDpiAdjuster.GetOrAddMarker(c: TComponent; out aIsNew: boolean): THighDpiMarker;
begin
  Result := GetMarker(c);
  aIsNew := not assigned(Result);
  if not assigned(Result) then
    Result := THighDpiMarker.Create(c);
end;

function THighDpiAdjuster.AdjustTImageList(aImageList: TImageList): TVirtualImageList;
var
  lMarker: THighDpiMarker;
  lIsNew: Boolean;
  lBitmap: TBitmap;
  lGlyphBitmap: TBitmap;
  lSrcQP: TQuickPixel;
  lDstQP: TQuickPixel;
  lImagesPerRow, lImagesPerColumn: Integer;
  lSrcRect: TRect;
  lPt: TPoint;
  lImageName: string;
  lVL: TVirtualImageList;
begin
  Result := nil;

  // Basic sanity checks
  if (aImageList = nil) or (aImageList.Count = 0) then
    Exit;

  if (aImageList.Width <= 0) or (aImageList.Height <= 0) then
    Exit; // prevents division by zero and invalid bitmap sizing

  lMarker := GetOrAddMarker(aImageList, lIsNew);
  if (not lIsNew) and Assigned(lMarker.VirtualImageList) then
    Exit(lMarker.VirtualImageList); // already processed

  // Source sheet copy
  gc(lBitmap, TBitmap.Create);
  if not maxLogic.QuickPixel.CopyBmp(aImageList.GetImageBitmap, lBitmap) then
    Exit;

  if (lBitmap.Width <= 0) or (lBitmap.Height <= 0) then
    Exit; // prevents division by zero and invalid rects

  Log('THighDpiAdjuster.AdjustTImageList: %s', aImageList);

  lVL := CreateVirtualImgList(aImageList.Owner, lMarker, aImageList.Width, aImageList.Height);
  Result := lMarker.VirtualImageList;

  // Prepare quick-pixel helpers and a per-glyph work bitmap
  gc(lSrcQP, TQuickPixel.Create(lBitmap));

  // Ensure transparency before slicing, if a bg color is set
  aImageList.BkColor := clFuchsia;
  if aImageList.BkColor <> clNone then
    lSrcQP.TransparencyToAlphaChannel(aImageList.BkColor);

  // Safe, non-zero values (Max(1, …)) to avoid div-by-zero in mod/div
  lImagesPerRow := System.Math.Max(1, lBitmap.Width div aImageList.Width);
  lImagesPerColumn := System.Math.Max(1, lBitmap.Height div aImageList.Height);

  // Independent per-glyph bitmap
  gc(lGlyphBitmap, TBitmap.Create);
  lGlyphBitmap.Assign(lBitmap);
  lGlyphBitmap.FreeImage; // detach from source
  lGlyphBitmap.SetSize(aImageList.Width, aImageList.Height);

  gc(lDstQP, TQuickPixel.Create(lGlyphBitmap));

  lSrcRect := Rect(0, 0, aImageList.Width, aImageList.Height);

  for var lGlyphIndex := 0 to aImageList.Count - 1 do
  begin
    if lGlyphIndex <> 0 then
    begin
      // Calculate the offset of the glyph on the sheet
      lPt.X := (lGlyphIndex mod lImagesPerRow) * lGlyphBitmap.Width;
      lPt.Y := (lGlyphIndex div lImagesPerRow) * lGlyphBitmap.Height;

      // Optional: clamp to source bounds to be extra safe
      if (lPt.X + lSrcRect.Width > lBitmap.Width) or
         (lPt.Y + lSrcRect.Height > lBitmap.Height) then
      begin
        // Out-of-bounds slice -> stop or continue; here we stop silently
        Break;
      end;

      lSrcRect.Location := lPt;
    end;

    // Copy from sheet to per-glyph bitmap
    lDstQP.CopyRect(Point(0, 0), lSrcQP, lSrcRect);

    if AddToImageCol(lGlyphBitmap, lImageName) then
      lVL.Add(lImageName, lImageName, False);
  end;
end;

function THighDpiAdjuster.AdjustUsingRttiLookingForGlyphAndImageName(
  c: TComponent): boolean;
var
  lImagesObj, lGlyphObj: TObject;
  lGlyph: TGraphic;
  lNumGlyphs: integer;
  lBitmaps: array[0..3] of TBitmap;
  lNames: array[0..3] of string;
  vl: TVirtualImageList;
  lImageName: string;
  lImages: TGlyphCollection;
  lBmp: TBitmap;
  lGarbo: iGarbo;
  lOrgEnabled: boolean;
  lSuccess: boolean;
begin
  Result := False;
  lSuccess := False;

  // those are required
  if not (TRttiHelper.has(c, 'ImageName')
    and TRttiHelper.has(c, 'Images')
    and TRttiHelper.has(c, 'ImageIndex')
    and TRttiHelper.has(c, 'Glyph')
    ) then
    exit;

  lImagesObj := TRttiHelper.ReadObjectProperty(c, 'Images');
  if (lImagesObj <> nil) and (lImagesObj is TVirtualImageList) then
    exit; // already using virtual image list

  lGlyphObj := TRttiHelper.ReadObjectProperty(c, 'Glyph');
  if (lGlyphObj = nil) then
    exit;
  lGlyph := lGlyphObj as TGraphic;

  {
    Glyph can provide up to four images within a single bitmap. All images must be the same size and next to each other in a horizontal row. TSpeedButton displays one of these images depending on the state of the button.
    Image position 	Button state 	Description

    First  | Up       | This image appears when the button is unselected. If no other images exist in the bitmap, this image is used for all states.
    Second | Disabled | This image usually appears dimmed to indicate that the button can't be selected.
    Third  | Clicked  | This image appears when the button is clicked. If GroupIndex is 0, the Up image reappears when the user releases the mouse button.
    Fourth | Down     | This image appears when the button stays down indicating that it remains selected.

    If only one image is present, TSpeedButton attempts to represent the other states by altering the image slightly for each state, although the Down state is always the same as the Up state.
    If the bitmap contains multiple images, specify the number of images in the bitmap with the NumGlyphs property.
    Note: The lower left pixel of the bitmap is reserved for the "transparent" color. Any pixel in the bitmap that matches the lower left pixel will be transparent.

    The new Controls have the following extra properties
    DisabledImageIndex
    DisabledImageName
    HotImageIndex
    HotImageName
    ImageIndex
    ImageName
    PressedImageIndex
    PressedImageName
    SelectedImageIndex
    SelectedImageName
  }
  lNumGlyphs := 0;
  if TRttiHelper.has(c, 'NumGlyphs') then
    lNumGlyphs := StrToIntDef(TRttiHelper.ReadProperty(c, 'NumGlyphs'), 0);

  gc(lImages, TGlyphCollection.Create(self, lGlyph, lNumGlyphs));
  if (not assigned(lImages.Items)) or (lImages.Items.Count = 0) then
    exit;

  Log('THighDpiAdjuster.AdjustUsingRttiLookingForGlyphAndImageName: %s', c);
  vl := GetOrCreateVirtualImgList(c.Owner, nil,
    lImages.Items[0].Image.Width,
    lGlyph.Height);

  lImages.EnsureDisabledImagePresent(vl);

  vl.BeginUpdate;
  try
    for var lItem in lImages.Items do
    begin
      // lImageName := 'GLYPH_' + CalcHash(lItem.Image);

      if AddToImageCol(lItem.Image, lImageName) then
      begin
        lItem.ImageName := lImageName;
        vl.add(lImageName, lImageName, False);
        lSuccess := True;
      end;
    end;

    if not lSuccess then
      exit;

    // set the properties
    TRttiHelper.WriteProperty(c, 'Glyph', nil);
    TRttiHelper.WriteProperty(c, 'Images', vl);
    for var lItem in lImages.Items do
      TRttiHelper.WriteProperty(c, lItem.PropPrefix + 'ImageName', lItem.ImageName);

    Result := True;
  finally
    vl.EndUpdate;
  end;
end;

procedure THighDpiAdjuster.AdjustComponent(aComponent: TComponent; aAlreadyProcessedDic: TDictionary<TComponent, boolean>);
var
  lMarker: THighDpiMarker;
  c: TComponent;
  lGarbo: iGarbo;
  lControl: TWinControl;
  lFormMarker: TFormChangeScaleTrigger;
  lForm: TCustomForm;
begin
  c := aComponent;

  if (c is TFormChangeScaleTrigger) or (c is THighDpiMarker) then
    exit;

  lGarbo := nil;
  if aAlreadyProcessedDic = nil then
  begin
    fMaxComponentCount := Max(1000, fMaxComponentCount);
    aAlreadyProcessedDic := TDictionary<TComponent, boolean>.Create(fMaxComponentCount);
    lGarbo := gc(aAlreadyProcessedDic);
  end
  else if aAlreadyProcessedDic.containskey(c) then
    exit;

  aAlreadyProcessedDic.add(c, True);

  lMarker := GetMarker(c);
  if assigned(lMarker) and (not lMarker.CanScale) then
    exit;

  if c is TCustomForm then
  begin
    lForm := c as TCustomForm;
    if not TFormChangeScaleTrigger.has(lForm) then
      TFormChangeScaleTrigger.Create(lForm);
  end;

  if c is TImage then
    AdjustTImage(TImage(c))
  else if (c is TImageList) and (not (c is TVirtualImageList)) then
    AdjustTImageList(c as TImageList)
  else
  begin
    if not AdjustUsingRttiLookingForGlyphAndImageName(c) then
      RedirectImageListReferenceToVirtualImage(c);
  end;

  for var X := 0 to c.ComponentCount - 1 do
    AdjustComponent(c.Components[X], aAlreadyProcessedDic);

  if c is TWinControl then
  begin
    lControl := c as TWinControl;
    for var X := 0 to lControl.ControlCount - 1 do
      AdjustComponent(lControl.Controls[X], aAlreadyProcessedDic);
  end;

  // update the max count, so we have a better preset next time
  if lGarbo <> nil then
    fMaxComponentCount := Max(fMaxComponentCount, aAlreadyProcessedDic.Count);
end;

function interceptTTImageDestRect(Sender: TObject): TRect;
var
  lImage: TOpenImage;
  lMarker: TTImageMarker;
  lSize: TPoint;
  w, h, cw, ch: integer;
begin
  lImage := nil;
  if (Sender <> nil) and (Sender is TImage) then
    lImage := TOpenImage(Sender);

  if assigned(TTImageMarker.TrampolineDestRect) then
    Result := TTImageMarker.TrampolineDestRect(Sender)
  else
  begin
    if assigned(lImage) then
      Result := lImage.CloneOfdDestRect
    else
      Result := TRect.Empty;
  end;

  if assigned(lImage) then
  begin
    if (not lImage.Stretch) then
    begin
      lMarker := TTImageMarker.GetMarker(lImage);
      if lMarker <> nil then
      begin
        lSize := point(Result.Width, Result.Height);
        lSize := lImage.ScaleValue(lSize);
        if lSize <> point(Result.Width, Result.Height) then
        begin
          Result.Width := lSize.X;
          Result.Height := lSize.y;

          if lImage.Center then
          begin
            Result.Location := point(0, 0);
            cw := lImage.ClientWidth;
            ch := lImage.ClientHeight;
            w := Result.Width;
            h := Result.Height;
            OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);
          end;
        end;
      end;
    end;
  end;
end;

procedure THighDpiAdjuster.AdjustTImage(aImage: TImage);
type
  TDestRectMethod = function: TRect of object;

var
  lMarker: TTImageMarker;
  lHash: RawByteString;
  lIsNew: boolean;
  lSize: TPoint;
  lImage: TOpenImage;
  lTargetproc: Pointer;
  lDestRectMethod: TDestRectMethod;
begin
  // if there is no image or if it is already stretched... then just do nothing
  if (aImage.Picture.Graphic = nil) or aImage.Stretch then
    exit;

  lMarker := TTImageMarker.GetOrCreateMarker(aImage, lIsNew);
  if (not lMarker.CanScale) then
    exit;

  lHash := CalcHash(aImage.Picture.Graphic);
  if (not lIsNew) and (lMarker.ImageHash <> lHash) then
    lIsNew := True;
  lMarker.ImageHash := lHash;

  // let images that auto scaled manage themself
  if aImage.autosize or (lMarker.WasAutoSized) then
  begin
    aImage.autosize := False;
    lMarker.ImageSize := point(aImage.Picture.Graphic.Width, aImage.Picture.Graphic.Height);
    lSize := aImage.ScaleValue(lMarker.ImageSize);
    aImage.Width := lSize.X;
    aImage.Height := lSize.y;
    aImage.Stretch := True;
    aImage.Proportional := True;
    lMarker.WasAutoSized := True;
    exit;
  end;

  if not lMarker.Intercepted then
  begin
    lImage := TOpenImage(aImage);

    { In Delphi, you cannot directly obtain the address of a method or function of an object using the @ operator as you would with procedural variables.
      The DestRect in TImage is a method, not a field, so its address cannot be taken directly. To achieve what you want, you can use a method reference.
      Define a procedural type that matches the signature of the DestRect method.
      Assign the method to a variable of that type.
      Retrieve the address from that variable. }
    lDestRectMethod := lImage.DestRect;
    lTargetproc := TMethod(lDestRectMethod).code;

    // now intercept it
    lMarker.TrampolineDestRect :=
      InterceptCreate(
      lTargetproc,
      @interceptTTImageDestRect);
    lMarker.Intercepted := True;
  end;
  aImage.Invalidate; // force repaint
end;

function THighDpiAdjuster.AddToImageCol(aGraphic: TGraphic; out aImageName: String): Boolean;
var
  lIndex: integer;
  lImageElement: TImageCollectionItem;
  lBmp: TBitmap;
  lGarbo: iGarbo;
begin
  Result := True;
  aImageName:= Self.CalcHash(aGraphic);

  Log('THighDpiAdjuster.AddToImageCol("'+aImageName+'")');

  lIndex := fImageCollection.GetIndexByName(aImageName);
  Log('  lIndex: '+lIndex.ToString);
  if lIndex <> -1 then
  begin
    // do not add it if it is already there
    {lImageElement := fImageCollection.Images[lIndex];
    try
      lImageElement.SourceImages[0].Image.Assign(aGraphic);
    except
      Result := False;
    end;}

  end else begin
    lImageElement := fImageCollection.Images.add;
    Log('  ImagesCount: '+fImageCollection.Images.Count.ToString);
    lImageElement.Name := aImageName;
    var ImageCollectionSourceItem := lImageElement.SourceImages.add;
    try
      ImageCollectionSourceItem.Image.Assign(aGraphic);
    except
      on e: Exception do
      begin
        Log('  Exception '+e.classname+': '+e.Message);
        Result := False;
      end;
    end;

    // remove the invalid item...
    if not Result then
      if Assigned(lImageElement) then
      begin
        lIndex := fImageCollection.GetIndexByName(aImageName);
        if lIndex <> -1 then
          fImageCollection.Images.Delete(lIndex);
      end;
  end;

  Log('  GetUserObjectCount: '+IntToStr(GetUserObjectCount));
  Log('  GetGDIObjectCount : '+IntToStr(GetGDIObjectCount));
end;

function THighDpiAdjuster.CalcHash(aGraphic: TGraphic): RawByteString;
var
  lStream: TMemoryStream;
  lHashValue: Integer;
begin
  gc(lStream, TMemoryStream.Create);
  aGraphic.SaveToStream(lStream);
  lStream.Position := 0;
  // as hex
  Result:= BobJenkinsHash(lStream.Memory^, lStream.Size, 0).ToHexString;
end;

constructor THighDpiAdjuster.Create;
begin
  inherited;

  fDataModule := TDataModule.Create(nil);
  fImageCollection := TImageCollection.Create(fDataModule);
end;

destructor THighDpiAdjuster.Destroy;
begin
  fDataModule.Free;

  try
    if assigned(application)
      and assigned(Screen)
      and (not application.Terminated) then
    begin
      Screen.OnActiveFormChange := self.fOrgOnActiveFormChange;
      self.fOrgOnActiveFormChange := nil;
    end;
  except
    // do nothing
  end;

  inherited;
end;

class procedure THighDpiAdjuster.EnableAutoAdjusting;
begin
  if not Singelton.fOrgOnActiveFormChangeAssigned then
  begin
    Singelton.fOrgOnActiveFormChangeAssigned := True;
    Singelton.fOrgOnActiveFormChange := Screen.OnActiveFormChange;
    Screen.OnActiveFormChange := Singelton.screenOnActiveFormChange;
  end;
end;

class destructor THighDpiAdjuster.DestroyClass;
begin
  FreeAndNil(fSingelton);
end;

class function THighDpiAdjuster.Singelton: THighDpiAdjuster;
var
  lInstance: THighDpiAdjuster;
begin
  if not assigned(fSingelton) then
  begin
    lInstance := THighDpiAdjuster.Create;
    if TInterlocked.CompareExchange<THighDpiAdjuster>(fSingelton, lInstance, nil) <> nil then
      FreeAndNil(lInstance)
  end;
  Result := fSingelton;
end;

procedure THighDpiAdjuster.screenOnActiveFormChange(Sender: TObject);
var
  lForm: TForm;
begin
  if assigned(application)
    and assigned(Screen)
    and (not application.Terminated)
    and (Screen.activeform <> nil) then
  begin
    lForm := Screen.activeform;
    if (not (csDestroying in lForm.ComponentState)) then
    begin
      if not TFormChangeScaleTrigger.has(lForm) then
        AdjustForm(lForm);

      if assigned(fOrgOnActiveFormChange) then
        fOrgOnActiveFormChange(Sender);
    end;
  end;
end;

class procedure THighDpiAdjuster.MarkAsNonScalable(c: TComponent);
var
  m: THighDpiMarker;
begin
  m := GetOrAddMarker(c);
  m.doNotScale := True;
end;

function THighDpiAdjuster.RedirectImageListReferenceToVirtualImage(
  c: TComponent): boolean;
var
  lObj: TObject;
  lImageList: TImageList;
  lVirtualImageList: TVirtualImageList;
  lTypeName: string;
begin
  Result := False;

  // only TCustomImageList  are supported
  // some controls like TBalloonHint use directly TImageList
  lTypeName := TRttiHelper.GetPropertyTypeName(c, 'Images');
  if (lTypeName = '')
    or (not System.StrUtils.MatchText(lTypeName, ['TCustomImageList', 'TVirtualImageList'])) then
    exit;

  lObj := TRttiHelper.ReadObjectProperty(c, 'Images');
  if (lObj = nil) or (not (lObj is TImageList)) then
    exit;

  lImageList := lObj as TImageList;
  lVirtualImageList := AdjustTImageList(lImageList);
  TRttiHelper.WriteProperty(c, 'Images', lVirtualImageList);

  Result := True;
end;

{ THighDpiMarker }

procedure THighDpiMarker.SetdoNotScale(const Value: boolean);
begin
  FdoNotScale := Value;
end;

function THighDpiMarker.CanScale: boolean;
begin
  Result := (doNotScale = False)
end;

{ Others }

function ReScaleValue(const aValue: double; aControl: TControl; const aOldScaleFactor: Single): double;
var
  v: double;
begin
  if sameScaleFactor(aControl.ScaleFactor, aOldScaleFactor) then
    Result := aValue
  else
  begin
    v := aValue / aOldScaleFactor;
    Result := aControl.ScaleValue(v);
  end;
end;

function ReScaleValue(const aValue: int64; aControl: TControl; const aOldScaleFactor: Single): int64; overload;
begin
  Result:= ReScaleValue(Integer(aValue), aControl, aOldScaleFactor);
end;

function ReScaleValue(const aValue: integer; aControl: TControl; const aOldScaleFactor: Single): integer;
var
  f1, f2: double;
begin
  if sameScaleFactor(aControl.ScaleFactor, aOldScaleFactor) then
    Result := aValue
  else
  begin
    f1 := aValue;
    f2 := ReScaleValue(f1, aControl, aOldScaleFactor);
    Result := Round(f2);
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
    Result := True
  else
    Result := False;
end;

{ TFormChangeScaleTrigger }

constructor TFormChangeScaleTrigger.Create(aOwner: TCustomForm);
begin
  inherited Create(aOwner);

  fOrgMonitorDpiChangedEvent := OpenForm(aOwner).OnAfterMonitorDpiChanged;
  OpenForm(aOwner).OnAfterMonitorDpiChanged := FormOnAfterMonitorDpiChanged;
  fLastPixelsperInch := OpenForm(aOwner).PixelsPerInch;
end;

destructor TFormChangeScaleTrigger.Destroy;
begin
  if assigned(Owner) and (Owner is TCustomForm) then
  begin
    OpenForm(Owner).OnAfterMonitorDpiChanged := fOrgMonitorDpiChangedEvent;
    fOrgMonitorDpiChangedEvent := nil;
  end;

  inherited;
end;

procedure TFormChangeScaleTrigger.FormOnAfterMonitorDpiChanged(
  Sender: TObject; OldDPI, NewDPI: integer);
begin
  try
    if assigned(fOrgMonitorDpiChangedEvent) then
      fOrgMonitorDpiChangedEvent(Sender, OldDPI, NewDPI);
  finally
    if assigned(THighDpiAdjuster.fSingelton)
      and assigned(application)
      and (not application.Terminated)
      and assigned(self.Owner)
      and (not (csDestroying in self.Owner.ComponentState)) then
      THighDpiAdjuster.AdjustForm(Owner as TCustomForm);
  end;
end;

class function TFormChangeScaleTrigger.has(AfORM: TCustomForm): boolean;
var
  lMarker: TFormChangeScaleTrigger;
begin
  Result := get(AfORM, lMarker);
end;

class function TFormChangeScaleTrigger.get(AfORM: TCustomForm; out aMarker: TFormChangeScaleTrigger): boolean;
var
  X: integer;
begin
  Result := False;
  for X := AfORM.ComponentCount - 1 downto 0 do
    if AfORM.Components[X] is TFormChangeScaleTrigger then
    begin
      aMarker := AfORM.Components[X] as TFormChangeScaleTrigger;
      exit(True);
    end;
end;

{ THighDpiAdjuster.TGlyphCollectionItem }

constructor THighDpiAdjuster.TGlyphCollectionItem.Create;
begin
  inherited Create;
  Image := TBitmap.Create;
  Image.pixelformat := pf32bit;
  Index := aIndex;
end;

destructor THighDpiAdjuster.TGlyphCollectionItem.Destroy;
begin
  Image.Free;
  inherited;
end;

{ THighDpiAdjuster.TGlyphCollection }

procedure THighDpiAdjuster.TGlyphCollection.applyTransparency(
  aBmp: TBitmap);
var
  qp: TQuickPixel;
begin
  TQuickPixel.MaskToAlphaChannel(aBmp);
end;

constructor THighDpiAdjuster.TGlyphCollection.Create(
  aParent: THighDpiAdjuster;
  aGlyph: TGraphic;
  aNumGlyphs: integer);
var
  lIndex: integer;
  lMaskColor: TColor;
  w: integer;
  lItem: TGlyphCollectionItem;
  lPrefix, lName: string;
  lBitmap: TBitmap;
  qp: TQuickPixel;
begin
  inherited Create;

  if (not assigned(aGlyph)) or (aGlyph.Width = 0) or (aGlyph.Height = 0) then
    exit;

  if not (aGlyph is TBitmap) then
    fHasTransparentColor := False
  else
  begin
    fTransparentColor := (aGlyph as TBitmap).Canvas.Pixels[0, aGlyph.Height - 1];
    fHasTransparentColor := True;
  end;
  gc(lBitmap, TBitmap.Create);
  lBitmap.Assign(aGlyph);
  if (lBitmap.Width = 0) or (lBitmap.Height = 0) then
    exit;
  lBitmap.FreeImage; // creates an independant copy

  // TQuickPixel.MaskToAlphaChannel(lBitmap);

  Items := TObjectList<TGlyphCollectionItem>.Create;
  if aNumGlyphs = 0 then
    aNumGlyphs := 1;

  w := aGlyph.Width div aNumGlyphs;

  for var X := 0 to aNumGlyphs - 1 do
  begin
    lItem := TGlyphCollectionItem.Create(X);
    Items.add(lItem);
    lItem.Image.Assign(lBitmap);
    lItem.Image.FreeImage; // creates an independant copy

    if X <> 0 then
    begin
      lItem.Image.Canvas.copyRect(
        Rect(0, 0, w, lBitmap.Height),
        lBitmap.Canvas,
        Rect(w * X, 0, w * (X + 1), lBitmap.Height)
        );
    end;
    lItem.Image.SetSize(w, aGlyph.Height); // truncate size

    // we need a copy before alpha is applied
    if (X = 0) and (aNumGlyphs = 1) then
    begin
      fMainGlyph := TBitmap.Create;
      fMainGlyph.Assign(lItem.Image);
      fMainGlyph.FreeImage; // create independant copy
    end;

    applyTransparency(lItem.Image);
  end;

  // naming
  for var X := 0 to Items.Count - 1 do
  begin
    case X of
      // Second | Disabled | This image usually appears dimmed to indicate that the button can't be selected.
      1: lPrefix := 'Disabled';
      // Third  | Clicked  | This image appears when the button is clicked. If GroupIndex is 0, the Up image reappears when the user releases the mouse button.
      2: lPrefix := 'Pressed';
      // Fourth | Down     | This image appears when the button stays down indicating that it remains selected.
      3: lPrefix := 'Selected'
    else
      // First  | Up       | This image appears when the button is unselected. If no other images exist in the bitmap, this image is used for all states.
      lPrefix := '';
    end;
    Items[X].PropPrefix := lPrefix;
  end;
end;

procedure THighDpiAdjuster.TGlyphCollection.EnsureDisabledImagePresent;
var
  i: integer;
  src: Pointer;
  Gray: Byte;
  lItem: TGlyphCollectionItem;
  qp0, qp1: TQuickPixel;
  lTransparentRGB: TRGBA;
  lRgb: TRGB;
begin
  if (Items.Count <> 1) or (not assigned(fMainGlyph)) then
    exit;

  lItem := TGlyphCollectionItem.Create(1);
  Items.add(lItem);
  lItem.Image.Assign(fMainGlyph);
  lItem.Image.FreeImage; // creates an independant copy

  lItem.Image.pixelformat := pf24bit;
  lItem.Image.pixelformat := pf32bit;
  lItem.Image.Transparent := True;
  lItem.Image.AlphaFormat := afDefined;

  lItem.PropPrefix := 'Disabled';

  OpenVirtualImageList(aVirtualImageList).CreateDisabledBitmap(lItem.Image);

  gc(qp0, TQuickPixel.Create(fMainGlyph));
  gc(qp1, TQuickPixel.Create(Items[1].Image));

  lTransparentRGB := ColorToRGBA(clFuchsia);
  lTransparentRGB.a := 0;

  for var y := 0 to qp0.Height - 1 do
    for var X := 0 to qp0.Width - 1 do
    begin
      if qp0.Pixel[X, y] = fTransparentColor then
        qp1.RGBAPixel[X, y] := lTransparentRGB;
    end;
end;

destructor THighDpiAdjuster.TGlyphCollection.Destroy;
begin
  Items.Free;
  fMainGlyph.Free;
  inherited;
end;

{ TTImageMarker }

class destructor TTImageMarker.ClassDestroy;
begin
  if Intercepted then
  begin
    InterceptRemove(@TrampolineDestRect);
    TrampolineDestRect := nil;
    Intercepted := False;
  end;

  inherited;
end;

class function TTImageMarker.GetMarker(
  aComponent: TComponent): TTImageMarker;
begin
  Result := nil;
  for var X := aComponent.ComponentCount - 1 downto 0 do
    if aComponent.Components[X] is TTImageMarker then
      exit(aComponent.Components[X] as TTImageMarker);

end;

class function TTImageMarker.GetOrCreateMarker(
  aComponent: TComponent; out aIsNew: boolean): TTImageMarker;
begin
  Result := GetMarker(aComponent);
  aIsNew := Result = nil;
  if Result = nil then
    Result := TTImageMarker.Create(aComponent);
end;

end.

