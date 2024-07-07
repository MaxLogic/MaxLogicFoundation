unit MaxLogic.vcl.highDpi;

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
  windows, classes, sysUtils, controls, StdCtrls, ExtCtrls, graphics, forms,
  buttons, ComCtrls, vcl.ImgList,
  SyncObjs, generics.collections, messages,
  vcl.BaseImageCollection, vcl.ImageCollection, vcl.VirtualImageList,
  MaxLogic.GraphicUtils,
  DDetours;

type
  // forward declarations
  THighDpiMarker = class; // will be attached to any form that is adjusted
  THighDpiAdjuster = class; // the main class that performs the adjustments

  THighDpiAdjuster = class
  private type
    // a Glyph in TBitBtn or TSpeedbutton and similar can have multiple elements, up to 4 on a single bitmap... we need to split this up
    TGlyphCollectionItem = class
    public
      PropPrefix: String;
      Image: TBitmap;
      ImageName: String;
      constructor Create;
      destructor Destroy; override;
    end;

    TGlyphCollection = class
    public
      Items: TObjectList<TGlyphCollectionItem>;
      constructor Create(aParent: THighDpiAdjuster; aGlyph: TGraphic; aNumGlyphs: integer);
      destructor Destroy; override;
    end;
  private
    class var fSingelton: THighDpiAdjuster;
    class constructor CreateClass;
    class destructor DestroyClass;

  private
    fDataModule: TDataModule;
    fImageCollection: TImageCollection;
    fOrgOnActiveFormChange: TNotifyEvent;
    fOrgOnActiveFormChangeAssigned: Boolean;
    fMaxComponentCount: integer;

    procedure AddToImageCol(const aImageName: String; aGraphic: TGraphic);

    function CreateVirtualImgList(aOwner: TComponent; aMarker: THighDpiMarker; aWidth, aHeight: integer): TVirtualImageList;
    function GetOrCreateVirtualImgList(aOwner: TComponent; aMarker: THighDpiMarker; aWidth, aHeight: integer): TVirtualImageList;

    function AdjustTImageList(aImageList: TImageList): TVirtualImageList;
    procedure AdjustTImage(aImage: TImage);

    function RedirectImageListReferenceToVirtualImage(c: TComponent): Boolean;
    function AdjustUsingRttiLookingForGlyphAndImageName(c: TComponent): Boolean;
    // returns a sha256 hash as hex string
    function CalcHash(aGraphic: TGraphic): RawByteString;

    procedure screenOnActiveFormChange(sender: TObject);

  public
    constructor Create;
    destructor Destroy; override;

    procedure AdjustComponent(aComponent: TComponent; aAlreadyProcessedDic: TDictionary<TComponent, Boolean>);

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
    class function GetOrAddMarker(c: TComponent; out aIsNew: Boolean): THighDpiMarker; overload;
  public
    property ImageCollection: TImageCollection read fImageCollection;
  end;

  THighDpiMarker = class(TComponent)
  private
    FdoNotScale: Boolean;
    procedure SetdoNotScale(const Value: Boolean);
  public
    VirtualImageList: TVirtualImageList; // used for TImageList

    // this will check the DoNotScale flag and it will compare the screen dpi with the last adjustment
    function CanScale: Boolean;

    property doNotScale: Boolean read FdoNotScale write SetdoNotScale;
  end;

  TTrampolineTImageDestRect = function(sender: TObject): TRect;

  TTImageMarker = class(THighDpiMarker)
  private
    class var TrampolineDestRect: TTrampolineTImageDestRect;
    class var Intercepted: Boolean;
    class destructor ClassDestroy;
  public
    ImageHash: AnsiString; // as hex enoded sha256
    ImageSize: TPoint; // used for TImage
    WasAutoSized: Boolean;

    // returns nil if none was found
    class function GetMarker(aComponent: TComponent): TTImageMarker; static;
    class function GetOrCreateMarker(aComponent: TComponent; out aIsNew: Boolean): TTImageMarker; static;
  end;

  TFormChangeScaleTrigger = class(TComponent)
  private
    fLastPixelsperInch: integer;
  protected
    fOrgMonitorDpiChangedEvent: TMonitorDpiChangedEvent;
    procedure FormOnAfterMonitorDpiChanged(sender: TObject; OldDPI: integer; NewDPI: integer);
  public
    // checks if that form already has a TFormChangeScaleTrigger instance
    class function has(AfORM: TCustomForm): Boolean;
    class function get(AfORM: TCustomForm; out aMarker: TFormChangeScaleTrigger): Boolean;

    constructor Create(aOwner: TCustomForm); reintroduce;
    destructor Destroy; override;

    property LastPixelsperInch: integer read fLastPixelsperInch;
  end;

  // those methods helps to recalculate a value that was stored (in a config or somewhere else) with a different scaleFactor then the one currently used by the control
function ReScaleValue(const aValue: Double; aControl: TControl; const aOldScaleFactor: Single): Double; overload;
function ReScaleValue(const aValue: integer; aControl: TControl; const aOldScaleFactor: Single): integer; overload;

// when we save scaleFactors as strings, we might loose some detail, so we assume all scale factor not different by more then 0.001 are the same
function sameScaleFactor(const v1, v2: Single): Boolean;

function TryScale(aOwner: TComponent; const p: TPoint): TPoint;

implementation

uses
  MaxLogic.QuickPixel,
  system.math, system.StrUtils,
  autoFree, MaxLogic.RTTIHelper, system.Hash,
  pngImage, system.RTTI;

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

  TOpenImage = class(TImage)

  end;

  TImageHelper = class Helper for TImage
  public
    function CloneOfdDestRect: TRect;
  end;

function TImageHelper.CloneOfdDestRect: TRect;
var
  w, h, cw, ch: integer;
  xyaspect: Double;
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
        h := Trunc(cw / xyaspect);
        if h > ch then // woops, too big
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
      end
      else
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
        if w > cw then // woops, too big
        begin
          w := cw;
          h := Trunc(cw / xyaspect);
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
  lDone: Boolean;
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
    else begin
      lDone := False;
      aOwner := aOwner.Owner;
    end;
  end;
end;

{ THighDpiAdjuster }

class procedure THighDpiAdjuster.AdjustForm(AfORM: TCustomForm);
begin
  if assigned(Application)
    and (not Application.Terminated)
    and assigned(AfORM)
    and (not(csDestroying in AfORM.ComponentState)) then
  begin
    Singelton.AdjustComponent(AfORM, nil);
    AfORM.Invalidate;
    Singelton.fImageCollection.Change;
  end;
end;

class function THighDpiAdjuster.GetMarker(c: TComponent): THighDpiMarker;
var
  x: integer;
begin
  Result := nil;

  for x := c.ComponentCount - 1 downto 0 do
    if c.Components[x] is THighDpiMarker then
      Exit(THighDpiMarker(c.Components[x]));
end;

class constructor THighDpiAdjuster.CreateClass;
begin
  fSingelton := nil;
end;

function THighDpiAdjuster.GetOrCreateVirtualImgList(aOwner: TComponent;
  aMarker: THighDpiMarker; aWidth, aHeight: integer): TVirtualImageList;
var
  lComponentName: String;
  lComponent: TComponent;
begin
  Result := nil;
  lComponentName := 'MaxLogicHighDpiVirtualImageList_' + aWidth.ToString + 'x' + aHeight.ToString;
  lComponent := aOwner.FindComponent(lComponentName);

  if lComponent = nil then
  begin
    Result := CreateVirtualImgList(aOwner, aMarker, aWidth, aHeight);
    Result.Name := lComponentName;
  end else begin
    Result := lComponent as TVirtualImageList;
    if aMarker <> nil then
      aMarker.VirtualImageList := Result;
  end;
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
  Result.Width := p.x;
  Result.Height := p.y;

  Result.ColorDepth := TColorDepth.cd32Bit;
  // Result.DrawWWingStyle:= TDrawingStyle.dsTransparent;
  // Result.BkGColor := TRGB.Create(255,0,255).ToColor;
  // Result.BlendColor := clNone;
  // Result.Masked := True;
end;

class function THighDpiAdjuster.GetOrAddMarker(c: TComponent): THighDpiMarker;
var
  lIsNew: Boolean;
begin
  Result := GetOrAddMarker(c, lIsNew);
end;

class function THighDpiAdjuster.GetOrAddMarker(c: TComponent; out aIsNew: Boolean): THighDpiMarker;
begin
  Result := GetMarker(c);
  aIsNew := not assigned(Result);
  if not assigned(Result) then
    Result := THighDpiMarker.Create(c);
end;

function THighDpiAdjuster.AdjustTImageList(aImageList: TImageList): TVirtualImageList;
var
  lMarker: THighDpiMarker;
  i: integer;
  lBitmap: TBitmap;
  lIsNew: Boolean;
  vl: TVirtualImageList;
  lImageName: String;
  lImages: TGlyphCollection;
  b: TBitmap;
  qp, qpItem: TQuickPixel;
  ImagesPerRow, ImagesperColumn: integer;
  srcRect: TRect;
  p: TPoint;
begin
  Result := nil;

  lMarker := GetOrAddMarker(aImageList, lIsNew);
  if not lIsNew then
  begin
    if lMarker <> nil then
      Result := lMarker.VirtualImageList;
    Exit; // we already processed this
  end;

  vl := CreateVirtualImgList(aImageList.Owner, lMarker, aImageList.Width, aImageList.Height);
  Result := lMarker.VirtualImageList;

  gc(lBitmap, TBitmap.Create);

  b := TBitmap.Create;
  try
    b.Handle := aImageList.GetImageBitmap;
    lBitmap.assign(b);
  finally
    b.Free;
  end;

  qpItem := nil;
  b := TBitmap.Create;
  qp := TQuickPixel.Create(lBitmap);
  try
    aImageList.BkColor := clFuchsia;
    if aImageList.BkColor <> clNone then
    begin
      qp.TransparencyToAlphaChannel(aImageList.BkColor);
    end;

    ImagesPerRow := lBitmap.Width div aImageList.Width;
    ImagesperColumn := lBitmap.Height div aImageList.Height;

    b.assign(lBitmap);
    b.SetSize(aImageList.Width, aImageList.Height);
    srcRect := rect(0, 0, aImageList.Width, aImageList.Height);
    qpItem := TQuickPixel.Create(b);
    for var lglyphIndex := 0 to aImageList.Count - 1 do
    begin
      if lglyphIndex <> 0 then
      begin
        // Calculate the offset of the glyph
        p.x := (lglyphIndex mod ImagesPerRow) * b.Width;
        p.y := (lglyphIndex div ImagesPerRow) * b.Height;
        srcRect.Location := p;
      end;
      qpItem.copyRect(point(0, 0), qp, srcRect);
      lImageName := 'IL_' + IntToHex(nativeInt(Pointer(aImageList)), 1) + '_' + lglyphIndex.ToString;
      AddToImageCol(lImageName, b);
      vl.Add(lImageName, lImageName, False);
    end;
  finally
    qp.Free;
    qpItem.Free;
    b.Free;
  end;

  {
    lBitmap.SetSize(aImageList.Width, aImageList.Height);
    lBitmap.pixelformat := pf32bit;

    for i := 0 to aImageList.Count - 1 do
    begin
    aImageList.GetBitmap(i, lBitmap); // Copy the image from the ImageList to the bitmap
    lImageName := 'IL_' + IntToHex(nativeInt(Pointer(aImageList)), 1) + '_' + i.ToString;
    AddToImageCol(lImageName, lBitmap);
    vl.Add(lImageName, lImageName, False);
    end; }

end;

function THighDpiAdjuster.AdjustUsingRttiLookingForGlyphAndImageName(
  c: TComponent): Boolean;
var
  lImagesObj, lGlyphObj: TObject;
  lGlyph: TGraphic;
  lNumGlyphs: integer;
  lBitmaps: array [0 .. 3] of TBitmap;
  lNames: array [0 .. 3] of String;
  vl: TVirtualImageList;
  lImageName: String;
  lImages: TGlyphCollection;
begin
  Result := False;

  // those are required
  if not(TRttiHelper.has(c, 'ImageName')
      and TRttiHelper.has(c, 'Images')
      and TRttiHelper.has(c, 'ImageIndex')
      and TRttiHelper.has(c, 'Glyph')
    ) then
    Exit;

  lImagesObj := TRttiHelper.ReadObjectProperty(c, 'Images');
  if (lImagesObj <> nil) and (lImagesObj is TVirtualImageList) then
    Exit; // already using virtual image list

  lGlyphObj := TRttiHelper.ReadObjectProperty(c, 'Glyph');
  if (lGlyphObj = nil) then
    Exit;
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

  gc(lImages, TGlyphCollection.Create(Self, lGlyph, lNumGlyphs));
  if (not assigned(lImages.Items)) or (lImages.Items.Count = 0) then
    Exit;

  vl := GetOrCreateVirtualImgList(c.Owner, nil,
    lImages.Items[0].Image.Width,
    lGlyph.Height);

  for var lItem in lImages.Items do
  begin
    lImageName := 'GLYPH_' + CalcHash(lItem.Image);
    lItem.ImageName := lImageName;
    AddToImageCol(lImageName, lItem.Image);
    vl.Add(lImageName, lImageName, True)
  end;

  // set the properties
  TRttiHelper.WriteProperty(c, 'Glyph', nil);
  TRttiHelper.WriteProperty(c, 'Images', vl);
  for var lItem in lImages.Items do
    TRttiHelper.WriteProperty(c, lItem.PropPrefix + 'ImageName', lItem.ImageName);

  Result := True;
end;

procedure THighDpiAdjuster.AdjustComponent(aComponent: TComponent; aAlreadyProcessedDic: TDictionary<TComponent, Boolean>);
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
    Exit;

  lGarbo := nil;
  if aAlreadyProcessedDic = nil then
  begin
    fMaxComponentCount := max(1000, fMaxComponentCount);
    aAlreadyProcessedDic := TDictionary<TComponent, Boolean>.Create(fMaxComponentCount);
    lGarbo := gc(aAlreadyProcessedDic);
  end else if aAlreadyProcessedDic.containskey(c) then
    Exit;

  aAlreadyProcessedDic.Add(c, True);

  lMarker := GetMarker(c);
  if assigned(lMarker) and (not lMarker.CanScale) then
    Exit;

  if c is TCustomForm then
  begin
    lForm := c as TCustomForm;
    if not TFormChangeScaleTrigger.has(lForm) then
      TFormChangeScaleTrigger.Create(lForm);
  end;

  if c is TImage then
    AdjustTImage(TImage(c))
  else if (c is TImageList) and (not(c is TVirtualImageList)) then
    AdjustTImageList(c as TImageList)
  else begin
    if not AdjustUsingRttiLookingForGlyphAndImageName(c) then
      RedirectImageListReferenceToVirtualImage(c);
  end;

  for var x := 0 to c.ComponentCount - 1 do
    AdjustComponent(c.Components[x], aAlreadyProcessedDic);

  if c is TWinControl then
  begin
    lControl := c as TWinControl;
    for var x := 0 to lControl.controlCount - 1 do
      AdjustComponent(lControl.controls[x], aAlreadyProcessedDic);
  end;

  // update the max count, so we have a better preset next time
  if lGarbo <> nil then
    fMaxComponentCount := max(fMaxComponentCount, aAlreadyProcessedDic.Count);
end;

function interceptTTImageDestRect(sender: TObject): TRect;
var
  lImage: TOpenImage;
  lMarker: TTImageMarker;
  lSize: TPoint;
  w, h, cw, ch: integer;
begin
  lImage := nil;
  if (sender <> nil) and (sender is TImage) then
    lImage := TOpenImage(sender);

  if assigned(TTImageMarker.TrampolineDestRect) then
    Result := TTImageMarker.TrampolineDestRect(sender)
  else begin
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
          Result.Width := lSize.x;
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
  lIsNew: Boolean;
  lSize: TPoint;
  lImage: TOpenImage;
  lTargetproc: Pointer;
  lDestRectMethod: TDestRectMethod;
begin
  // if there is no image or if it is already stretched... then just do nothing
  if (aImage.Picture.Graphic = nil) or aImage.Stretch then
    Exit;

  lMarker := TTImageMarker.GetOrCreateMarker(aImage, lIsNew);
  if (not lMarker.CanScale) then
    Exit;

  lHash := CalcHash(aImage.Picture.Graphic);
  if (not lIsNew) and (lMarker.ImageHash <> lHash) then
    lIsNew := True;
  lMarker.ImageHash := lHash;

  // let images that auto scaled manage themself
  if aImage.AutoSize or (lMarker.WasAutoSized) then
  begin
    aImage.AutoSize := False;
    lMarker.ImageSize := point(aImage.Picture.Graphic.Width, aImage.Picture.Graphic.Height);
    lSize := aImage.ScaleValue(lMarker.ImageSize);
    aImage.Width := lSize.x;
    aImage.Height := lSize.y;
    aImage.Stretch := True;
    aImage.Proportional := True;
    lMarker.WasAutoSized := True;
    Exit;
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

procedure THighDpiAdjuster.AddToImageCol(const aImageName: String;
  aGraphic: TGraphic);
var
  lIndex: integer;
  lImageElement: TImageCollectionItem;
  lBmp: TBitmap;
  lGarbo: iGarbo;
begin
  // fix up the transparency
  if aGraphic is TBitmap then
  begin
    lGarbo := gc(lBmp, TBitmap.Create);
    lBmp.assign(aGraphic);
    TQuickPixel.MaskToAlphaChannel(lBmp);
    aGraphic := lBmp;
  end;

  lIndex := fImageCollection.GetIndexByName(aImageName);
  if lIndex <> -1 then
  begin
    lImageElement := fImageCollection.Images[lIndex];
    lImageElement.SourceImages[0].Image.assign(aGraphic);
    // lImageElement.Change;
  end else begin
    lImageElement := fImageCollection.Images.Add;
    lImageElement.Name := aImageName;
    lImageElement.SourceImages.Add.Image.assign(aGraphic);
    // lImageElement.Change;
  end;
end;

function THighDpiAdjuster.CalcHash(aGraphic: TGraphic): RawByteString;
var
  lStream: TMemoryStream;
  lHash: THashSHA2;
begin
  gc(lStream, TMemoryStream.Create);
  aGraphic.SaveToStream(lStream);
  lStream.Position := 0;
  // as hex
  Result := RawByteString(THashSHA2.GetHashString(lStream));
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
    if assigned(Application)
      and assigned(Screen)
      and (not Application.Terminated) then
    begin
      Screen.OnActiveFormChange := Self.fOrgOnActiveFormChange;
      Self.fOrgOnActiveFormChange := nil;
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

procedure THighDpiAdjuster.screenOnActiveFormChange(sender: TObject);
var
  lForm: TForm;
begin
  if assigned(Application)
    and assigned(Screen)
    and (not Application.Terminated)
    and (Screen.activeform <> nil) then
  begin
    lForm := Screen.activeform;
    if (not(csDestroying in lForm.ComponentState)) then
    begin
      if not TFormChangeScaleTrigger.has(lForm) then
        AdjustForm(lForm);

      if assigned(fOrgOnActiveFormChange) then
        fOrgOnActiveFormChange(sender);
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
  c: TComponent): Boolean;
var
  lObj: TObject;
  lImageList: TImageList;
  lVirtualImageList: TVirtualImageList;
  lTypeName: String;
begin
  Result := False;

  // only TCustomImageList  are supported
  // some controls like TBalloonHint use directly TImageList
  lTypeName := TRttiHelper.GetPropertyTypeName(c, 'Images');
  if (lTypeName = '')
    or (not system.StrUtils.MatchText(lTypeName, ['TCustomImageList', 'TVirtualImageList'])) then
    Exit;

  lObj := TRttiHelper.ReadObjectProperty(c, 'Images');
  if (lObj = nil) or (not(lObj is TImageList)) then
    Exit;

  lImageList := lObj as TImageList;
  lVirtualImageList := AdjustTImageList(lImageList);
  TRttiHelper.WriteProperty(c, 'Images', lVirtualImageList);

  Result := True;
end;

{ THighDpiMarker }

procedure THighDpiMarker.SetdoNotScale(const Value: Boolean);
begin
  FdoNotScale := Value;
end;

function THighDpiMarker.CanScale: Boolean;
begin
  Result := (doNotScale = False)
end;

{ Others }

function ReScaleValue(const aValue: Double; aControl: TControl; const aOldScaleFactor: Single): Double;
var
  v: Double;
begin
  if sameScaleFactor(aControl.ScaleFactor, aOldScaleFactor) then
    Result := aValue
  else
  begin
    v := aValue / aOldScaleFactor;
    Result := aControl.ScaleValue(v);
  end;
end;

function ReScaleValue(const aValue: integer; aControl: TControl; const aOldScaleFactor: Single): integer;
var
  f1, f2: Double;
begin
  if sameScaleFactor(aControl.ScaleFactor, aOldScaleFactor) then
    Result := aValue
  else
  begin
    f1 := aValue;
    f2 := ReScaleValue(f1, aControl, aOldScaleFactor);
    Result := round(f2);
  end;
end;

function sameScaleFactor(const v1, v2: Single): Boolean;
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
  sender: TObject; OldDPI, NewDPI: integer);
begin
  try
    if assigned(fOrgMonitorDpiChangedEvent) then
      fOrgMonitorDpiChangedEvent(sender, OldDPI, NewDPI);
  finally
    if assigned(THighDpiAdjuster.fSingelton)
      and assigned(Application)
      and (not Application.Terminated)
      and assigned(Self.Owner)
      and (not(csDestroying in Self.Owner.ComponentState))
    then
      THighDpiAdjuster.AdjustForm(Owner as TCustomForm);
  end;
end;

class function TFormChangeScaleTrigger.has(AfORM: TCustomForm): Boolean;
var
  lMarker: TFormChangeScaleTrigger;
begin
  Result := get(AfORM, lMarker);
end;

class function TFormChangeScaleTrigger.get(AfORM: TCustomForm; out aMarker: TFormChangeScaleTrigger): Boolean;
var
  x: integer;
begin
  Result := False;
  for x := AfORM.ComponentCount - 1 downto 0 do
    if AfORM.Components[x] is TFormChangeScaleTrigger then
    begin
      aMarker := AfORM.Components[x] as TFormChangeScaleTrigger;
      Exit(True);
    end;
end;

{ THighDpiAdjuster.TGlyphCollectionItem }

constructor THighDpiAdjuster.TGlyphCollectionItem.Create;
begin
  inherited Create;
  Image := TBitmap.Create;
  Image.pixelformat := pf32bit;
end;

destructor THighDpiAdjuster.TGlyphCollectionItem.Destroy;
begin
  Image.Free;
  inherited;
end;

{ THighDpiAdjuster.TGlyphCollection }

constructor THighDpiAdjuster.TGlyphCollection.Create(
  aParent: THighDpiAdjuster;
  aGlyph: TGraphic;
  aNumGlyphs: integer);
var
  lIndex: integer;
  lMaskColor: TColor;
  w: integer;
  lItem: TGlyphCollectionItem;
  lPrefix, lName: String;
  lBitmap: TBitmap;
  qp: TQuickPixel;
begin
  inherited Create;

  if (not assigned(aGlyph)) or (aGlyph.Width = 0) or (aGlyph.Height = 0) then
    Exit;

  gc(lBitmap, TBitmap.Create);
  lBitmap.assign(aGlyph);
  if (lBitmap.Width = 0) or (lBitmap.Height = 0) then
    Exit;

  TQuickPixel.MaskToAlphaChannel(lBitmap);

  Items := TObjectList<TGlyphCollectionItem>.Create;
  if aNumGlyphs = 0 then
    aNumGlyphs := 1;

  w := aGlyph.Width div aNumGlyphs;

  for var x := 0 to aNumGlyphs - 1 do
  begin
    lItem := TGlyphCollectionItem.Create;
    Items.Add(lItem);
    lItem.Image.assign(lBitmap);

    if x <> 0 then
    begin
      qp := TQuickPixel.Create(lItem.Image);
      try
        qp.copyRect(
          point(0, 0), // dest on self
          qp, // source is also self
          rect(w * x, 0, w * (x + 1), lBitmap.Height));
      finally
        FreeAndNil(qp);
      end;
    end;
    lItem.Image.SetSize(w, aGlyph.Height); // truncate size
  end;

  // naming
  for var x := 0 to Items.Count - 1 do
  begin
    case x of
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
    Items[x].PropPrefix := lPrefix;
  end;
end;

destructor THighDpiAdjuster.TGlyphCollection.Destroy;
begin
  Items.Free;
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
  for var x := aComponent.ComponentCount - 1 downto 0 do
    if aComponent.Components[x] is TTImageMarker then
      Exit(aComponent.Components[x] as TTImageMarker);

end;

class function TTImageMarker.GetOrCreateMarker(
  aComponent: TComponent; out aIsNew: Boolean): TTImageMarker;
begin
  Result := GetMarker(aComponent);
  aIsNew := Result = nil;
  if Result = nil then
    Result := TTImageMarker.Create(aComponent);
end;

end.
