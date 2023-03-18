unit maxLogic.QuickPixel;

{
  Version: 1.6

  History:
  2019-11-29: add Grayscale functions
  2017-01-04: small fix in the Makenegative function
  2016-09-10: added Makenegative function
  2016-07-28: remove a warrning
  2015-12-16: added some compare functions
}
interface

uses
  Windows, sysUtils, Classes, Graphics, Dialogs, bsUTILS;

type

  pRGB = ^TRGB;

  TRGB = packed record
    B: Byte;
    G: Byte;
    R: Byte;
    class function Equal(const Rgb1, Rgb2: TRGB): boolean; overload; inline; static;
    function Equal(const aRgb: TRGB): boolean; overload; inline;
    procedure Clear(SetAllTo: Byte);
  end;

  pRGBA = ^TRGBA;

  TRGBA = packed record
    B: Byte;
    G: Byte;
    R: Byte;
    A: Byte;
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

  public
    constructor Create(const ABitmap: TBitmap);
    destructor Destroy; override;

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

implementation

const
  CR = #13 + #10;

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

constructor TQuickPixel.Create(const ABitmap: TBitmap);
var
  I: integer;
  x: integer;
begin
  inherited Create;

  FBitmap := ABitmap;
  FBitmap.Canvas.Lock;
  // FBitmap.PixelFormat:= pf24bit;
  SetLength(FScanLines, FBitmap.Height);

  for I := 0 to FBitmap.Height - 1 do
    FScanLines[I] := FBitmap.ScanLine[I];

  PaletteCount := GetPaletteEntries(
    FBitmap.Palette,
    0,
    PaletteCount,
    nil);
  if PaletteCount > 0 then
  begin
    SetLength(Palette, PaletteCount);
    PaletteCount := Windows.GetPaletteEntries(
      FBitmap.Palette,
      0,
      PaletteCount,
      Palette[0]);
    SetLength(Colors, PaletteCount);
    for x := 0 to PaletteCount - 1 do
      Colors[x] := PaletteToCOlor(Palette[x]);
  end
  else
  begin
    Colors := nil;
    Palette := nil;
  end;

  // if PaletteCount > 0 then
  // begin
  // SetLength(Palette, PaletteCount);
  // PaletteCount := GetPaletteEntries(fBitMap.Handle, 0,$FFFFFFFF, @Palette[0]);
  // end;
end;

destructor TQuickPixel.Destroy;
begin
  FScanLines := nil;
  Palette := nil;
  Colors := nil;
  FBitmap.Canvas.Unlock;

  inherited;
end;

function TQuickPixel.GetBit(x, y: integer): boolean;
begin
  if PixelFormat <> pf1bit then
    raise exception.Create('Bits are accesable only for FixelFormat pf1bit')
  else
  begin
    Result := SELF.GetPaletteIndex(x, y) = 1;
  end;
end;

function TQuickPixel.GetByPalette(x, y: integer;
  out PaletteEntry: TPaletteEntry): boolean;
var
  index: word;
begin
  index := GetPaletteIndex(x, y);
  PaletteEntry := SELF.Palette[index];
  Result := True;
end;

function TQuickPixel.GetHeight: integer;
begin
  Result := FBitmap.Height;
end;

function TQuickPixel.GetLineScan(y: integer): Pointer;
begin
  Result := SELF.FScanLines[y];
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
  for y := 0 to SELF.Height - 1 do
    for x := 0 to SELF.Width - 1 do
    begin
      Rgb := SELF.RGBPixel[x, y];
      Rgb.R := $FF - Rgb.R;
      Rgb.G := $FF - Rgb.G;
      Rgb.B := $FF - Rgb.B;
      SELF.RGBPixel[x, y] := Rgb;
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
      pb^ := pb^ and (not (1 shl (7 - BitIndex)));
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
      free;
    end;
    B.free;
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
  times: array[Low(TPixelFormat)..High(TPixelFormat)] of tTime;
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
      free;
    end;

    B.free;
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
    free;
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

function TRGB.Equal(const aRgb: TRGB): boolean;
begin
  Result := TRGB.Equal(SELF, aRgb);
end;

end.

