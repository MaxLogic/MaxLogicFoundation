unit maxLogic.QuickPixel;

{
  Version: 1.6

  History:
  2024-06-30: adding some of alpha and mask methods
  2019-11-29: add Grayscale functions
  2017-01-04: small fix in the Makenegative function
  2016-09-10: added Makenegative function
  2016-07-28: remove a warrning
  2015-12-16: added some compare functions
}
interface

uses
  winApi.Windows, system.sysUtils, system.Classes, Graphics, Dialogs, types;

type

  pRGB = ^TRGB;

  TRGB = packed record
  public
    B: Byte;
    G: Byte;
    R: Byte;
    class function Equal(const Rgb1, Rgb2: TRGB): boolean; overload; inline; static;
    function Equal(const aRgb: TRGB): boolean; overload; inline;
    procedure Clear(SetAllTo: Byte);
    function ToColor: TColor;
    class function Create(R, G, B: Byte): TRGB; static;
    class function FromColor(aColor: TColor): TRGB; static;
  end;

  pRGBA = ^TRGBA;

  TRGBA = packed record
  public
    B: Byte;
    G: Byte;
    R: Byte;
    A: Byte;
    function Equal(const aRgba: TRGBA): boolean; overload; inline;
    function ToColor: TColor;
    class function Create(R, G, B, A: Byte): TRGBA; static;
    class function FromColor(aColor: TColor): TRGBA; static;
  end;

  TQuickPixel = class
  private
    FBitmap: TBitmap;
    FScanLines: array of Pointer;

    PaletteCount: DWORD;
    Palette: array of TPaletteEntry;
    Colors: array of TColor;
    function GetLineScan(y: integer): Pointer;
    procedure SetLineScan(y: integer; const Value: Pointer);
    function GetBit(x, y: integer): boolean;
    procedure SetBit(x, y: integer; const Value: boolean);

    function GetPixelFormat: TPixelFormat;

    function GetByPalette(x, y: integer; out PaletteEntry: TPaletteEntry): boolean;
    function GetPaletteIndex(x, y: integer): word;

    function GetPixel(x, y: integer): TColor;
    function GetRGBA(x, y: integer): TRGBA;
    function GetRGB(x, y: integer): TRGB;

    procedure SetPixel(x, y: integer; const Value: TColor);
    procedure SetRGBA(x, y: integer; const Rgba: TRGBA);
    procedure SetRGB(x, y: integer; const Rgb: TRGB);

    function GetHeight: integer;
    function GetWidth: integer;
    procedure SetBitmap(const Value: TBitmap);
  public
    constructor Create(ABitmap: TBitmap);
    destructor Destroy; override;

    // does not take ownership, reference only
    property Bitmap: TBitmap read FBitmap write SetBitmap;

    property RGBPixel[x, y: integer]: TRGB read GetRGB write SetRGB;
    property RGBAPixel[x, y: integer]: TRGBA read GetRGBA write SetRGBA;
    property Bit[x, y: integer]: boolean read GetBit write SetBit;
    property Pixel[x, y: integer]: TColor read GetPixel write SetPixel;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property PixelFormat: TPixelFormat read GetPixelFormat;
    property LineScan[y: integer]: Pointer read GetLineScan write SetLineScan;

    // reverts all colors to make a negative image
    // ATTENTION works only for 24bits
    procedure MakeNegative;

    procedure copyRect(const aDest: TPoint; aSource: TQuickPixel; const aSourceRect: TRect); overload;
    procedure copyRect(const aDest: TPoint; aSource: TBitmap; const aSourceRect: TRect); overload;

    procedure copyRectTo(const aSourceRect: TRect; aDestination: TQuickPixel; const aDestPoint: TPoint); overload;
    procedure copyRectTo(const aSourceRect: TRect; aDestination: TBitmap; const aDestPoint: TPoint); overload;

    // converts the underlying bitmap so it uses alpha instead of transparent color
    // ATTENTION: this will change the pixelformat to pf32bit if required!
    procedure TransparencyToAlphaChannel; overload;
    procedure TransparencyToAlphaChannel(aTransparentColor: TColor); overload;
    // checks the alpha channel, if it contains only 0 or 255 (fully invisible or fully visible), or if it contains also semi transparent pixels
    function AlphaIsAllOrNothing: boolean;
    // invisible pixels are having alpha=255, invisible pixels have alpha=0, and all other are semi visible
    procedure ScanAlphaChannel(out aHasVisiblePixels, aHasInvisiblePixels, aHasSemiTransparentPixels: boolean);
    // converts the alphaChannel to a transparent color
    // aMaxInvisibleValue is the max value of the alpha byte that will be treated as fully invisible
    procedure ConvertAlphaChannelToTransparentColor(aTransparentColor: TColor; aMaxInvisibleValue: Byte = 0); overload;
    procedure ConvertAlphaChannelToTransparentColor(const aTransparentColor: TRGB; aMaxInvisibleValue: Byte = 0); overload;
    { mask and cur image bust have the same size
      this methods iterates the pixels of the masks
      where it is 0 there will be the transparent olor applied to the underlying bitmap
      Transparent will be set to true and transparentMode to tmFixed }
    procedure ApplyMask(aMask: TBitmap; aUseTransparentColor: TColor);

    // converts the mask to the alpha chanell of the bitmap
    class procedure MaskToAlphaChannel(ABitmap: TBitmap); overload;
    procedure MaskToAlphaChannel; overload;
  end;

function ColorToRGB(Color: TColor): TRGB;
function ColorToRGBA(Color: TColor): TRGBA;
function RGBToColor(Rgb: TRGB): TColor;
function RGBAToColor(Rgba: TRGBA): TColor;
function RGBToRGBA(Rgb: TRGB): TRGBA;
function RGBAToRGB(Rgba: TRGBA): TRGB;
function PaletteToCOlor(const p: TPaletteEntry): TColor;
function pf1bitsToByte(Bits, PixelIndex: Byte): Byte;
function pf1bitsToBool(Bits, PixelIndex: Byte): boolean;
function pf4bitsToByte(Bits: Byte; FirstPixel: boolean): Byte;
function pf15bitToRGB(w: word): TRGB;
function RGBTopf15Bit(Rgb: TRGB): word;
function pf16bitToRGB(w: word): TRGB;
function RandomColor: TColor;
function PixelFormatToStr(PixelFormat: TPixelFormat): string;

function GetPaletteEntries(Palette: HPALETTE; StartIndex, NumEntries: UINT;
  PaletteEntries: pPaletteEntry): UINT; stdcall;
function GetBpp(PixelFormat: TPixelFormat): Byte;

// copies from a HBitmap to a TBitmap
function CopyBmp(aSource: HBitmap; aDestination: TBitmap): boolean;

implementation

uses
  system.math, autoFree, maxLogic.ioUtils;

const
  CR = #13 + #10;

function CopyBmp(aSource: HBitmap; aDestination: TBitmap): boolean;
var
  lSrc: TBitmap;
begin
  Result := False;
  if (aSource = 0) then
    Exit(False);

  // http://docwiki.embarcadero.com/Libraries/Berlin/en/Vcl.Graphics.TBitmap.Handle
  // Handle is the HBitmap encapsulated by the bitmap object. Avoid grabbing the handle directly since it causes the HBitmap to be copied if more than one TBitmap shares the handle.
  // ok but we wanna a real copy, so we need to create a bitmap, grap the handle, and then copy it to the actuall result bitmap
  try
    aDestination.Handle := aSource;
    aDestination.FreeImage; // creates an independant copy
    Result := True;
  except
    Result := False;
  end;
end;

function fstr(s: single; vs, ns: Byte): string;
begin
  str(s: vs: ns, Result);
end;

function PixelFormatToStr(PixelFormat: TPixelFormat): string;
begin
  case PixelFormat of
    pfDevice:
      Result := 'pfDevice';
    pf1bit:
      Result := 'pf1bit';
    pf4bit:
      Result := 'pf4bit';
    pf8bit:
      Result := 'pf8bit';
    pf15bit:
      Result := 'pf15bit';
    pf16bit:
      Result := 'pf16bit';
    pf24bit:
      Result := 'pf24bit';
    pf32bit:
      Result := 'pf32bit';
    pfCustom:
      Result := 'pfCustom';
  end;
end;

function GetBpp(PixelFormat: TPixelFormat): Byte;
begin
  case PixelFormat of
    pf1bit:
      Result := 1;
    pf4bit:
      Result := 4;
    pf8bit:
      Result := 8;
    pf15bit:
      Result := 15;
    pf16bit:
      Result := 16;
    pf24bit:
      Result := 24;
    pf32bit:
      Result := 21;
  else
    MessageDlg('Unknown PixelFormat', mtError, [mbOK], 0);
    Result := 0;
  end;
end;

function RandomColor: TColor;
begin
  Result := (random(256) shl 16) + (random(256) shl 8) + random(256);
  ;
end;

const
  pf24Topf15Ratio = 31 / 256;

const
  pf15Topf24Ratio = 256 / 31;

const
  pf16Topf24Ratio = 256 / ($FF shr 2);

function RGBTopf15Bit(Rgb: TRGB): word;
begin
  with Rgb do
    Result :=
      (Trunc(R * pf24Topf15Ratio) shl 10) or
      (Trunc(G * pf24Topf15Ratio) shl 5) or
      Trunc(B * pf24Topf15Ratio);
end;

function Trb(s: single): Byte;
var
  w: word;
begin
  w := Trunc(s);
  if w > $FF then
    Result := $FF
  else
    Result := w;
end;

// pf15bit:   0 rrrrr ggggg bbbbb
function pf15bitToRGB(w: word): TRGB;
var
  m: word;
begin
  m := $FF shr 3;
  with Result do
  begin
    B := Trb((w and m) * pf15Topf24Ratio);
    G := Trb(((w shr 5) and m) * pf15Topf24Ratio);
    R := Trb(((w shr 10) and m) * pf15Topf24Ratio);
  end;
end;

// pf16bit:  rrrrr gggggg bbbbb
function pf16bitToRGB(w: word): TRGB;
var
  m: word;
  be: Byte;
begin
  m := $FF shr 3;
  with Result do
  begin
    be := (w and m);
    B := Trb(be * pf15Topf24Ratio);
    be := ((w shr 5) and ($FF shr 2));
    G := Trb(be * pf16Topf24Ratio);
    be := ((w shr 11) and m);
    R := Trb(be * pf15Topf24Ratio);
  end;

end;

function pf1bitsToBool(Bits, PixelIndex: Byte): boolean;
begin
  Result := pf1bitsToByte(Bits, PixelIndex) = 1;
end;

function pf1bitsToByte(Bits, PixelIndex: Byte): Byte;
begin
  Bits := Bits and (1 shl (7 - PixelIndex));
  Bits := Bits shr (7 - PixelIndex);
  Result := Bits;
end;

function pf4bitsToByte(Bits: Byte; FirstPixel: boolean): Byte;
begin
  if FirstPixel then
  begin
    Result := (Bits shr 4);
  end
  else
  begin

    Result := Bits and ($FF shr 4);
  end;
end;

const
  gdi32 = 'gdi32.dll';

function GetPaletteEntries; external gdi32 name 'GetPaletteEntries';

function RGBToRGBA(Rgb: TRGB): TRGBA;
begin
  with Result do
  begin
    R := Rgb.R;
    G := Rgb.G;
    B := Rgb.B;
    A := 0;
  end;
end;

function RGBAToRGB(Rgba: TRGBA): TRGB;
begin
  with Result do
  begin
    R := Rgba.R;
    G := Rgba.G;
    B := Rgba.B;
  end;
end;

function ColorToRGB(Color: TColor): TRGB;
begin
  if Color < 0 then
    Color := GetSysColor(Color and $000000FF);

  Result.R := ($000000FF and Color);
  Result.G := ($0000FF00 and Color) shr 8;
  Result.B := ($00FF0000 and Color) shr 16;
end;

function RGBToColor(Rgb: TRGB): TColor;
begin
  Result := (Rgb.B shl 16) or (Rgb.G shl 8) or Rgb.R
end;

function RGBAToColor(Rgba: TRGBA): TColor;
begin
  Result := (Rgba.A shl 24) or (Rgba.B shl 16) or (Rgba.G shl 8) or Rgba.R
end;

function PaletteToCOlor(const p: TPaletteEntry): TColor;
begin
  Result :=
  // (p.peFlags shl 24) or
    (p.peBlue shl 16) or
    (p.peGreen shl 8) or
    p.peRed
end;

function ColorToRGBA(Color: TColor): TRGBA;
begin
  if Color < 0 then
    Color := GetSysColor(Color and $000000FF);

  Result.R := ($000000FF and Color);
  Result.G := ($0000FF00 and Color) shr 8;
  Result.B := ($00FF0000 and Color) shr 16;
  Result.A := ($FF000000 and Color) shr 24;
end;
{ TQuickPixel }

function TQuickPixel.AlphaIsAllOrNothing: boolean;
var
  lHasVisiblePixels, lHasInvisiblePixels, lHasSemiTransparentPixels: boolean;
begin
  ScanAlphaChannel(lHasVisiblePixels, lHasInvisiblePixels, lHasSemiTransparentPixels);
  Result := not lHasSemiTransparentPixels;
end;

procedure TQuickPixel.ApplyMask(aMask: TBitmap;
  aUseTransparentColor: TColor);
var
  m: TQuickPixel;
  lTrans: TRGB;
begin
  lTrans := TRGB.FromColor(aUseTransparentColor);
  m := TQuickPixel.Create(aMask);
  try
    for var y := 0 to min(aMask.Height, self.Height) - 1 do
      for var x := 0 to min(aMask.Width, self.Width) - 1 do
      begin
        if m.RGBPixel[x, y].R <> 0 then
          self.RGBPixel[x, y] := lTrans;
      end;
    FBitmap.transparent := True;
    FBitmap.TransparentMode := tmFixed;
    FBitmap.TransparentColor := aUseTransparentColor;
  finally
    m.Free;
  end;
end;

procedure TQuickPixel.ConvertAlphaChannelToTransparentColor(
  const aTransparentColor: TRGB; aMaxInvisibleValue: Byte);
var
  p: pRGBA;
  lTransparentPixel: TRGBA;
begin
  lTransparentPixel.R := aTransparentColor.R;
  lTransparentPixel.G := aTransparentColor.G;
  lTransparentPixel.B := aTransparentColor.B;
  lTransparentPixel.A := 255;

  if FBitmap.PixelFormat <> pf32bit then
    Exit;

  for var y := 0 to Height - 1 do
  begin
    p := FBitmap.ScanLine[y];
    for var x := 0 to Width - 1 do
    begin
      if p.A <= aMaxInvisibleValue then
        p^ := lTransparentPixel
      else
        p.A := 255;
    end;
  end;
end;

procedure TQuickPixel.ConvertAlphaChannelToTransparentColor(
  aTransparentColor: TColor; aMaxInvisibleValue: Byte);
begin
  ConvertAlphaChannelToTransparentColor(
    TRGB.FromColor(aTransparentColor),
    aMaxInvisibleValue);

end;

procedure TQuickPixel.copyRect(const aDest: TPoint; aSource: TBitmap;
  const aSourceRect: TRect);
var
  lQ: TQuickPixel;
begin
  lQ := nil;
  try
    lQ := TQuickPixel.Create(aSource);
    copyRect(aDest, lQ, aSourceRect);
  finally
    lQ.Free;
  end;
end;

procedure TQuickPixel.copyRectTo(const aSourceRect: TRect;
  aDestination: TBitmap; const aDestPoint: TPoint);
var
  lQ: TQuickPixel;
begin
  lQ := nil;
  try
    lQ := TQuickPixel.Create(aDestination);
    copyRectTo(aSourceRect, lQ, aDestPoint);
  finally
    lQ.Free;
  end;
end;

procedure TQuickPixel.copyRectTo(const aSourceRect: TRect;
  aDestination: TQuickPixel; const aDestPoint: TPoint);
begin
  aDestination.copyRect(aDestPoint, self, aSourceRect);
end;

procedure TQuickPixel.copyRect(const aDest: TPoint; aSource: TQuickPixel;
  const aSourceRect: TRect);
var
  lDiff, x, y: integer;
  lRgba: TRGBA;
  lSourceRect: TRect;
  lDestPoint, p1, p2: TPoint;
begin
  lSourceRect := aSourceRect;
  lDestPoint := aDest;

  // check left destination bounds
  if lDestPoint.x < 0 then
  begin
    lSourceRect.Left := lSourceRect.Left - lDestPoint.x;
    lSourceRect.Width := lSourceRect.Width + lDestPoint.x;
    lDestPoint.x := 0;
  end;

  // check top destination bounds
  if lDestPoint.y < 0 then
  begin
    lSourceRect.top := lSourceRect.top - lDestPoint.y;
    lSourceRect.Height := lSourceRect.Height + lDestPoint.y;
    lDestPoint.y := 0;
  end;

  // computes an intersection of the source rect and the actual source bounds
  lSourceRect := TRect.Intersect(lSourceRect, rect(0, 0, aSource.Width, aSource.Height));
  if lSourceRect.IsEmpty then
    Exit;

  // check the right destination bounds
  lDiff := (lDestPoint.x + lSourceRect.Width) - self.Width;
  if lDiff > 0 then
    lSourceRect.Width := lSourceRect.Width - lDiff;

  // check the bottom destination bounds
  lDiff := (lDestPoint.y + lSourceRect.Height) - self.Height;
  if lDiff > 0 then
    lSourceRect.Height := lSourceRect.Height - lDiff;

  if lSourceRect.IsEmpty then
    Exit;

  for y := 0 to lSourceRect.Height - 1 do
  begin
    p1.y := y + lSourceRect.top;
    p2.y := y + lDestPoint.y;

    for x := 0 to aSource.Width - 1 do
    begin
      p1.x := x + lSourceRect.Left;
      p2.x := x + lDestPoint.x;

      lRgba := aSource.RGBAPixel[p1.x, p1.y];
      self.RGBAPixel[p2.x, p2.y] := lRgba;
    end;
  end;
end;

constructor TQuickPixel.Create(ABitmap: TBitmap);
begin
  inherited Create;
  SetBitmap(ABitmap);
end;

destructor TQuickPixel.Destroy;
begin
  FScanLines := nil;
  Palette := nil;
  Colors := nil;
  try
    if assigned(FBitmap) then
      FBitmap.Canvas.Unlock;
  Except
    // do nothing
  end;
  inherited;
end;

function TQuickPixel.GetBit(x, y: integer): boolean;
begin
  if PixelFormat <> pf1bit then
    raise exception.Create('Bits are accesable only for FixelFormat pf1bit')
  else
  begin
    Result := self.GetPaletteIndex(x, y) = 1;
  end;
end;

function TQuickPixel.GetByPalette(x, y: integer;
  out PaletteEntry: TPaletteEntry): boolean;
var
  index: word;
begin
  index := GetPaletteIndex(x, y);
  PaletteEntry := self.Palette[index];
  Result := True;
end;

function TQuickPixel.GetHeight: integer;
begin
  Result := FBitmap.Height;
end;

function TQuickPixel.GetLineScan(y: integer): Pointer;
begin
  Result := self.FScanLines[y];
end;

function TQuickPixel.GetPaletteIndex(x, y: integer): word;
var
  pb: pByte;
  ByteIndex, BitIndex: integer;
begin
  pb := FScanLines[y];
  case PixelFormat of
    pf1bit:
      begin
        ByteIndex := x div 8;
        BitIndex := x - (ByteIndex * 8);
        inc(pb, ByteIndex);
        Result := pf1bitsToByte(pb^, BitIndex)
      end;
    pf4bit:
      begin
        ByteIndex := x div 2;
        inc(pb, ByteIndex);
        Result := pf4bitsToByte(pb^, 0 = x - (ByteIndex * 2))

      end;
    pf8bit:
      begin
        inc(pb, x);
        Result := pb^;
      end;
  else
    raise exception.Create(PixelFormatToStr(PixelFormat) + ' is not a Pallette supported Pixel Format');
  end;
end;

function TQuickPixel.GetPixel(x, y: integer): TColor;
var
  pw: pWord;
begin
  case PixelFormat of
    pf4bit,
      pf8bit,
      pf1bit:
      begin
        Result := Colors[GetPaletteIndex(x, y)];
      end;

    pf15bit:
      begin
        pw := FScanLines[y];
        inc(pw, x);
        Result := RGBToColor(pf15bitToRGB(pw^));
      end;
    pf16bit:
      begin
        pw := FScanLines[y];
        inc(pw, x);
        Result := RGBToColor(pf16bitToRGB(pw^));
      end;
    pf24bit:
      Result := RGBToColor(GetRGB(x, y));
    pf32bit:
      Result := RGBAToColor(GetRGBA(x, y));
  else
    Result := FBitmap.Canvas.pixels[x, y];
  end;

end;

function TQuickPixel.GetPixelFormat: TPixelFormat;
begin
  Result := FBitmap.PixelFormat;
end;

function TQuickPixel.GetRGB(x, y: integer): TRGB;
var
  pb: pByte;
  p24: pRGB;
  PaletteEntry: TPaletteEntry;
  pw: pWord;
begin
  pb := FScanLines[y];

  case PixelFormat of
    pf1bit,
      pf4bit,
      pf8bit:
      begin
        if GetByPalette(x, y, PaletteEntry) then
          with Result do
          begin
            R := PaletteEntry.peRed;
            G := PaletteEntry.peGreen;
            B := PaletteEntry.peBlue;
          end
        else
          Result := ColorToRGB(FBitmap.Canvas.pixels[x, y]);
      end;
    pf15bit:
      begin
        pw := FScanLines[y];
        inc(pw, x);
        Result := pf15bitToRGB(pw^);
      end;
    pf16bit:
      begin
        pw := FScanLines[y];
        inc(pw, x);
        Result := pf16bitToRGB(pw^);
      end;
    pf24bit:
      begin
        p24 := pRGB(pb);
        inc(p24, x);
        Result := p24^;
      end;
    pf32bit:
      begin
        inc(pb, x * 4);
        Result := pRGB(pb)^;
      end;
  else
    Result := ColorToRGB(FBitmap.Canvas.pixels[x, y]);
  end;
end;

function TQuickPixel.GetRGBA(x, y: integer): TRGBA;
var
  p: pRGBA;
  PaletteEntry: TPaletteEntry;
  pb: pByte;
  pw: pWord;
begin
  case PixelFormat of
    pf1bit,
      pf4bit,
      pf8bit:
      begin
        if GetByPalette(x, y, PaletteEntry) then
          with Result do
          begin
            R := PaletteEntry.peRed;
            G := PaletteEntry.peGreen;
            B := PaletteEntry.peBlue;
            A := $FF - PaletteEntry.peFlags;
          end
        else
          Result := ColorToRGBA(FBitmap.Canvas.pixels[x, y]);
      end;
    pf15bit:
      begin
        pw := FScanLines[y];
        inc(pw, x);
        Result := RGBToRGBA(pf15bitToRGB(pw^));
      end;
    pf16bit:
      begin
        pw := FScanLines[y];
        inc(pw, x);
        Result := RGBToRGBA(pf16bitToRGB(pw^));
      end;
    pf24bit:
      begin
        pb := FScanLines[y];
        inc(pb, x * 3);
        Result := RGBToRGBA(pRGB(pb)^);
      end;
    pf32bit:
      begin
        p := FScanLines[y];
        inc(p, x);
        Result := p^;
      end;
  else
    Result := ColorToRGBA(FBitmap.Canvas.pixels[x, y]);
  end;
end;

function TQuickPixel.GetWidth: integer;
begin
  Result := FBitmap.Width;
end;

procedure TQuickPixel.MakeNegative;
var
  x, y: integer;
  Rgb: TRGB;
begin
  for y := 0 to self.Height - 1 do
    for x := 0 to self.Width - 1 do
    begin
      Rgb := self.RGBPixel[x, y];
      Rgb.R := $FF - Rgb.R;
      Rgb.G := $FF - Rgb.G;
      Rgb.B := $FF - Rgb.B;
      self.RGBPixel[x, y] := Rgb;
    end;
end;

procedure TQuickPixel.MaskToAlphaChannel;
var
  lMask: TBitmap;
  qpMask: TQuickPixel;
  lHas1, lHas0: boolean;
begin
  // if fBitmap.Transparent then
  if not FBitmap.SupportsPartialTransparency then
  begin
    lMask := TBitmap.Create;
    qpMask := nil;
    try
      CopyBmp(FBitmap.MaskHandle, lMask);
      qpMask := TQuickPixel.Create(lMask);
      if not FBitmap.transparent then // well, sometimes it still is transparent
      begin
        lHas1 := False;
        lHas0 := False;
        for var y := 0 to lMask.Height - 1 do
        begin
          for var x := 0 to lMask.Width - 1 do
          begin
            if qpMask.RGBPixel[x, y].R = 0 then
              lHas0 := True
            else
              lHas1 := True;
            if lHas1 and lHas0 then
              break;
          end;
          if lHas1 and lHas0 then
            break;
        end;
        if lHas1 and lHas0 then
          FBitmap.transparent := True
      end;

      if FBitmap.transparent then
      begin
        self.ApplyMask(lMask, clFuchsia);
        self.TransparencyToAlphaChannel(clFuchsia);
      end;
    except
      // do nothing
    end;
    lMask.Free;
    qpMask.Free;
  end;
end;

class procedure TQuickPixel.MaskToAlphaChannel(ABitmap: TBitmap);
var
  qp: TQuickPixel;
begin
  gc(qp, TQuickPixel.Create(ABitmap));
  qp.MaskToAlphaChannel;
end;

procedure TQuickPixel.ScanAlphaChannel(out aHasVisiblePixels, aHasInvisiblePixels, aHasSemiTransparentPixels: boolean);
begin
  aHasVisiblePixels := False;
  aHasInvisiblePixels := False;
  aHasSemiTransparentPixels := False;
  if FBitmap.PixelFormat <> pf32bit then
    Exit;
  for var y := 0 to Height - 1 do
  begin
    for var x := 0 to Width - 1 do
    begin
      case self.RGBAPixel[x, y].A of
        0:
          aHasInvisiblePixels := True;
        255:
          aHasVisiblePixels := True;
      else
        aHasSemiTransparentPixels := True;
      end;
      // does it make sense to continue scanning?
      if aHasVisiblePixels and aHasInvisiblePixels and aHasSemiTransparentPixels then
        Exit;
    end;
  end;
end;

procedure TQuickPixel.SetBit(x, y: integer; const Value: boolean);
var
  pb: pByte;
  ByteIndex, BitIndex: integer;
begin
  if PixelFormat <> pf1bit then
    raise exception.Create('bits are accessable only for pixelformat = pf1bit')
  else
  begin
    pb := FScanLines[y];
    ByteIndex := x div 8;
    BitIndex := x - (ByteIndex * 8);
    inc(pb, ByteIndex);
    // pf1bitsToByte(pb^, BitIndex)

    if Value then
      pb^ := pb^ or (1 shl (7 - BitIndex))
    else
      pb^ := pb^ and (not(1 shl (7 - BitIndex)));
  end;
end;

procedure TQuickPixel.SetBitmap(const Value: TBitmap);
var
  x, i: integer;
begin
  if assigned(FBitmap) then
    FBitmap.Canvas.Unlock;

  FBitmap := Value;
  FBitmap.Canvas.Lock;

  SetLength(FScanLines, FBitmap.Height);
  for i := 0 to FBitmap.Height - 1 do
    FScanLines[i] := FBitmap.ScanLine[i];

  PaletteCount := GetPaletteEntries(
    FBitmap.Palette,
    0,
    PaletteCount,
    nil);
  if PaletteCount > 0 then
  begin
    SetLength(Palette, PaletteCount);
    PaletteCount := winApi.Windows.GetPaletteEntries(
      FBitmap.Palette,
      0,
      PaletteCount,
      Palette[0]);

    SetLength(Colors, PaletteCount);
    for x := 0 to PaletteCount - 1 do
      Colors[x] := PaletteToCOlor(Palette[x]);
  end else begin
    Colors := nil;
    Palette := nil;
  end;
end;

procedure TQuickPixel.SetLineScan(y: integer; const Value: Pointer);
begin

end;

procedure TQuickPixel.SetPixel(x, y: integer; const Value: TColor);
begin
  case PixelFormat of
    pf32bit:
      SetRGBA(x, y, ColorToRGBA(Value));
    pf24bit:
      SetRGB(x, y, ColorToRGB(Value));
  else
    FBitmap.Canvas.pixels[x, y] := Value;
  end
end;

procedure TQuickPixel.SetRGB(x, y: integer; const Rgb: TRGB);
var
  p: pRGB;
  pb: pByte;
begin
  pb := FScanLines[y];

  case PixelFormat of
    // pf1bit: ;
    // pf4bit: ;
    // pf8bit: ;
    // pf15bit: ;
    // pf16bit: ;
    pf24bit:
      begin
        p := pRGB(pb);
        inc(p, x);
        p^ := Rgb;
      end;
    pf32bit:
      begin
        inc(pb, x * 4);
        pRGB(pb)^ := Rgb;
        inc(pb, 3);
        pb^ := $FF;
      end;
  else
    FBitmap.Canvas.pixels[x, y] := RGBToColor(Rgb);
  end;
end;

procedure TQuickPixel.SetRGBA(x, y: integer; const Rgba: TRGBA);
var
  pb: pByte;
begin
  case PixelFormat of
    pf32bit:
      begin
        pb := FScanLines[y];
        inc(pb, x * 4);
        pRGBA(pb)^ := Rgba;
      end;
    pf24bit:
      SetRGB(x, y, RGBAToRGB(Rgba));
  else
    FBitmap.Canvas.pixels[x, y] := RGBAToColor(Rgba);
  end;
end;

procedure TQuickPixel.TransparencyToAlphaChannel(
  aTransparentColor: TColor);
var
  lNeedReassignBitmap: boolean;
  x, y: integer;
  lRgba: TRGBA;
  lTransparentRGB: TRGB;
begin
  FBitmap.transparent := False;
  if FBitmap.PixelFormat = pf32bit then
    lNeedReassignBitmap := False
  else begin
    FBitmap.PixelFormat := pf32bit;
    lNeedReassignBitmap := True;
  end;

  if lNeedReassignBitmap then
    SetBitmap(FBitmap);

  lTransparentRGB := ColorToRGB(aTransparentColor);
  for y := 0 to self.Height - 1 do
    for x := 0 to self.Width - 1 do
    begin
      lRgba := RGBAPixel[x, y];
      if (lRgba.R = lTransparentRGB.R)
        and (lRgba.G = lTransparentRGB.G)
        and (lRgba.B = lTransparentRGB.B) then
      begin
        lRgba.A := 0;
        RGBAPixel[x, y] := lRgba;
      end else if lRgba.A <> 255 then
      begin
        lRgba.A := 255;
        RGBAPixel[x, y] := lRgba;
      end;
    end;

  FBitmap.AlphaFormat := afDefined;
end;

procedure TQuickPixel.TransparencyToAlphaChannel;
begin
  TransparencyToAlphaChannel(FBitmap.TransparentColor);
end;

procedure TestRGBAToCOlor;
var
  c0, c1: integer;
  B: TBitmap;
  x: integer;
  pf: TPixelFormat;
begin

  for pf := Low(TPixelFormat) to High(TPixelFormat) do
  begin
    if pf in [pfDevice, pfCustom] then
      continue;

    B := TBitmap.Create;
    B.PixelFormat := pf;
    B.Width := $FFFF;
    B.Height := 1;
    for x := 0 to $FFFF - 1 do
      // B.canvas.pixels[x,0] := (random(256)shl 16) + (random(256)shl 8)+random(256);
      B.Canvas.pixels[x, 0] := RandomColor;

    with TQuickPixel.Create(B) do
    begin
      for x := 0 to $FFFF - 1 do
      begin
        c0 := B.Canvas.pixels[x, 0];
        c1 := Pixel[x, 0];
        if c0 <> c1 then
        begin
          c0 := B.Canvas.pixels[x, 0];
          c1 := Pixel[x, 0];
          if c0 <> c1 then
            sleep(0);
          if ColorToRGBA(c0).A = 0 then
            sleep(0);
        end;

        Pixel[x, 0] := c0;
        c1 := Pixel[x, 0];
        if c0 <> c1 then
        begin
          c0 := B.Canvas.pixels[x, 0];
          c1 := Pixel[x, 0];
          if c0 <> c1 then
            sleep(0);
        end;
      end;
      Free;
    end;
    B.Free;
  end;

end;

type
  tTime = record
    canvas_read, canvas_write: DWORD;
    canvas_compareError: int64;
    qp_read, qp_write: DWORD;
    QP_compareError: int64;
  end;

procedure Speedtest;
const
  w = 1024;
  H = 1024;

var
  pf: TPixelFormat;
  B: TBitmap;
  x: integer;
  c0, c1, c2: TColor;
  OrgColor: array of array of TColor;
  times: array [Low(TPixelFormat) .. High(TPixelFormat)] of tTime;
  y: integer;
  d: DWORD;
  s: string;
begin
  c0 := 0;
  ZeroMemory(@times, sizeof(times));
  // prepare the color matrix
  SetLength(OrgColor, w, H);
  for x := 0 to w - 1 do
    for y := 0 to H - 1 do
      OrgColor[x, y] := RandomColor;

  for pf := Low(TPixelFormat) to High(TPixelFormat) do
  begin
    if pf in [pfDevice, pfCustom] then
      continue;
    B := TBitmap.Create;
    B.PixelFormat := pf;
    B.Width := w;
    B.Height := H;

    // canvas write test
    d := GetTickCount;
    for y := 0 to H - 1 do
      for x := 0 to w - 1 do
        B.Canvas.pixels[x, y] := OrgColor[x, y];
    times[pf].canvas_write := GetTickCount - d;

    // canvas read test
    d := GetTickCount;
    times[pf].canvas_compareError := 0;
    for y := 0 to H - 1 do
      for x := 0 to w - 1 do
        if OrgColor[x, y] <> B.Canvas.pixels[x, y] then
          c0 := OrgColor[x, y];
    times[pf].canvas_read := GetTickCount - d;
    // dummy entry to remove the warrning : C0 never used
    if c0 = 0 then
      c0 := 1;

    with TQuickPixel.Create(B) do
    begin
      // QUICK PIXEL Write TEST           // canvas write test
      d := GetTickCount;
      for y := 0 to H - 1 do
        for x := 0 to w - 1 do
          Pixel[x, y] := OrgColor[x, y];
      times[pf].qp_write := GetTickCount - d;

      // Quick Pixel read test
      d := GetTickCount;
      times[pf].canvas_compareError := 0;
      for y := 0 to H - 1 do
        for x := 0 to w - 1 do
          if OrgColor[x, y] <> Pixel[x, y] then
            c0 := OrgColor[x, y];
      times[pf].qp_read := GetTickCount - d;
      // dummy entry to remove the c0 never used Warrning
      if c0 = 0 then
        sleep(0);

      // Compare
      for y := 0 to H - 1 do
        for x := 0 to w - 1 do
        begin
          c0 := OrgColor[x, y];
          c1 := B.Canvas.pixels[x, y];
          c2 := Pixel[x, y];
          if c1 <> c2 then
            inc(times[pf].QP_compareError);
          if c0 <> c2 then
            inc(times[pf].canvas_compareError);
        end;
      Free;
    end;

    B.Free;
  end;
  SetLength(OrgColor, 0, 0);
  OrgColor := nil;

  s := '';
  for pf := Low(TPixelFormat) to High(TPixelFormat) do
  begin
    if pf in [pfDevice, pfCustom] then
      continue;
    s := s + IntToStr(GetBpp(pf)) + ':' + CR;

    with times[pf] do
    begin
      s := s + 'Canvas:' + CR +
        '  read  : ' + fstr(canvas_read / 1000, 3, 3) + ' sec' + CR +
        '  write : ' + fstr(canvas_write / 1000, 3, 3) + ' sec' + CR +
        '  error : ' + IntToStr(canvas_compareError) + CR +
        CR +
        'QuickPixel:' + CR +
        '  read  : ' + fstr(qp_read / 1000, 3, 3) + ' sec' + CR +
        '  write : ' + fstr(qp_write / 1000, 3, 3) + ' sec' + CR +
        '  error : ' + IntToStr(QP_compareError) + CR +
        CR +
        ' Improvment:' + CR +
        '  read  : ' + fstr(qp_read / canvas_read * 100, 3, 3) + ' %' + CR +
        '  write : ' + fstr(qp_write / canvas_write * 100, 3, 3) + ' %' + CR +
        CR + CR;
    end;
  end;
  with TStringList.Create do
  begin
    add(s);
    saveToFile(getInstallDir + 'QPSpeedTest.txt');
    Free;
  end;
  // Exec(getInstallDir+'QPSpeedTest.txt');
end;

{ TRGB }

class function TRGB.Equal(const Rgb1, Rgb2: TRGB): boolean;
begin
  Result := (Rgb1.R = Rgb2.R) and (Rgb1.G = Rgb2.G) and (Rgb1.B = Rgb2.B);
end;

procedure TRGB.Clear(SetAllTo: Byte);
begin
  R := SetAllTo;
  G := SetAllTo;
  B := SetAllTo;
end;

class function TRGB.Create(R, G, B: Byte): TRGB;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

function TRGB.Equal(const aRgb: TRGB): boolean;
begin
  Result := TRGB.Equal(self, aRgb);
end;

class function TRGB.FromColor(aColor: TColor): TRGB;
begin
  Result := ColorToRGB(aColor);
end;

function TRGB.ToColor: TColor;
begin
  Result := RGBToColor(self);
end;

{ TRGBA }

class function TRGBA.Create(R, G, B, A: Byte): TRGBA;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
end;

function TRGBA.Equal(const aRgba: TRGBA): boolean;
begin
  Result := cardinal(self) = cardinal(aRgba);
end;

class function TRGBA.FromColor(aColor: TColor): TRGBA;
begin
  Result := ColorToRGBA(aColor);
end;

function TRGBA.ToColor: TColor;
begin
  Result := RGBAToColor(self);
end;

end.
