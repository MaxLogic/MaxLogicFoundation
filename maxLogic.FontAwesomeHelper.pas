unit maxLogic.FontAwesomeHelper;
{ Version: 1.3

  Inspired by:
  http://thundaxsoftware.blogspot.com/2017/09/make-your-delphi-applications-pop-with.html

  Description:
  this simple unit installs the font on app start up, so it is available even if the machine you are running on usually doesn't have this font installed

  cheatsheet:
  https://fontawesome.com/cheatsheet }

interface

uses
  windows, sysUtils, classes, System.Zip, Graphics, forms, ioUtils, messages, diagnostics, Buttons;

type
  TFontAwesome = class
  private
    class var fn1, fn2: string;
    class function registerFont(const bytes: TBytes): string;
    class Destructor DestroyClass;
  public
    const
    FontNameSolid = 'Font Awesome 5 Free Solid';
    FontNameRegular = 'Font Awesome 5 Free Regular';
    class procedure InstallFontAwesome;
    // the glyph will have the required size + the scaled margins as required by the dpi
    class function MakeGlyph(const GlyphChar: string;
      FontName: string = FontNameSolid;
      FontSize: Integer = -11;
      FontColor: TColor = clWindowText;
      BackGroundColor: TColor = clFuchsia;
      margins: Integer = 1): TBitmap;

    class procedure MakeBitBtnGlyph(
      btn: TBitBtn;
      const GlyphChar: string;
      FontName: string = FontNameSolid;
      FontSize: Integer = 0; // if 0 it will be read out of the bitbtn.font property
      FontColor: TColor = clWindowText;
      BackGroundColor: TColor = clFuchsia;
      margins: Integer = 1);

  end;

implementation



{$R fontAwesome5.RES}


class function TFontAwesome.registerFont(const bytes: TBytes): string;
var
  fn: string;
  res: Integer;
  count: dword;
begin
  result := TPath.GetTempFileName;
  TFile.WriteAllBytes(result, bytes);
  res := AddFontResourceEx(pChar(result), FR_PRIVATE, nil);
end;

class destructor TFontAwesome.DestroyClass;
begin
  RemoveFontResourceEx(pChar(fn1), FR_PRIVATE, nil);
  RemoveFontResourceEx(pChar(fn2), FR_PRIVATE, nil);

  deleteFile(fn1);
  deleteFile(fn2);
end;

class procedure TFontAwesome.InstallFontAwesome;
var
  ResStream: tResourceStream;
  ZipFile: TZipFile;
  bytes: TBytes;
  st: TStopWatch;
begin
  st := TStopWatch.startNew;

  ResStream := tResourceStream.Create(hInstance, 'fontAwesome5', 'ZIP');
  ZipFile := TZipFile.Create; // Zipfile: TZipFile
  ZipFile.Open(ResStream, zmRead);

  ZipFile.Read(0, bytes);
  fn1 := registerFont(bytes);

  bytes := nil;
  ZipFile.Read(1, bytes);
  fn2 := registerFont(bytes);

  ResStream.free;
  ZipFile.free;

  // we need to inform our app that a new font is there...
  SendMessage(application.handle, WM_FONTCHANGE, 0, 0);
end;

class function TFontAwesome.MakeGlyph(const GlyphChar: string;
  FontName: string = FontNameSolid;
  FontSize: Integer = -11;
  FontColor: TColor = clWindowText;
  BackGroundColor: TColor = clFuchsia;
  margins: Integer = 1): TBitmap;
var
  b: TBitmap;
  w, h: Integer;
begin
  b := TBitmap.Create;
  b.pixelformat := pf24bit;
  b.width := 16; // do not have to be scaled for dpi, as it will be overwritten later on
  b.Height := 16;

  b.canvas.font.name := FontName;
  b.canvas.font.size := FontSize;
  b.canvas.brush.color := BackGroundColor;
  b.canvas.brush.Style := bsSolid;

  b.TransparentColor := BackGroundColor;
  b.TransparentMode := tmFixed;
  b.Transparent := true;

  w := b.canvas.TextWidth(GlyphChar) + margins * 2;
  h := b.canvas.Textheight(GlyphChar) + margins * 2;

  b.width := w;
  b.Height := h;

  b.canvas.FillRect(rect(0, 0, b.width, b.Height));

  b.canvas.font.color := FontColor;
  b.canvas.pen.color := FontColor;
  b.canvas.TextOut(margins, margins, GlyphChar);

  result := b;
end;

class procedure TFontAwesome.MakeBitBtnGlyph(btn: TBitBtn;
  const GlyphChar: string; FontName: string; FontSize: Integer; FontColor,
  BackGroundColor: TColor; margins: Integer);
var
  b: TBitmap;
begin
  if FontSize = 0 then
    FontSize := btn.font.size;

  b := TFontAwesome.MakeGlyph(GlyphChar,
    FontName,
    FontSize,
    FontColor,
    BackGroundColor,
    margins);

  btn.glyph := b;
  b.free;
end;

initialization

TFontAwesome.InstallFontAwesome;

end.
