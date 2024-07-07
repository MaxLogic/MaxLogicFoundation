unit MaxLogic.GraphicUtils;

interface

uses
  winApi.Windows, system.Bindings.Graph, system.SysUtils, system.Classes, Vcl.ExtCtrls,
  ShellApi, Commctrl, ShlObj, Graphics, pngImage;

// large icon extracting
// source: https://stackoverflow.com/questions/1703186/can-48x48-or-64x64-icons-be-obtained-from-the-vista-shell
const
  SHIL_LARGE = $00; // The image size is normally 32x32 pixels. However, if the Use large icons option is selected from the Effects section of the Appearance tab in Display Properties, the image is 48x48 pixels.
  SHIL_SMALL = $01; // These images are the Shell standard small icon size of 16x16, but the size can be customized by the user.
  SHIL_EXTRALARGE = $02; // These images are the Shell standard extra-large icon size. This is typically 48x48, but the size can be customized by the user.
  SHIL_SYSSMALL = $03; // These images are the size specified by GetSystemMetrics called with SM_CXSMICON and GetSystemMetrics called with SM_CYSMICON.
  SHIL_JUMBO = $04; // Windows Vista and later. The image is normally 256x256 pixels.
  IID_IImageList: TGUID = '{46EB5926-582E-4017-9FDF-E8998DAA0950}';

function GetShellImageList(aFlag: Cardinal = SHIL_JUMBO): HIMAGELIST;
Procedure GetIconFromFile(const aFile: String; var aIcon: TIcon; aFlag: Cardinal = SHIL_JUMBO);
Procedure LoadIconIntoImage(aImage: TImage; const aFileName: String; aIconFlag: Cardinal = SHIL_JUMBO);
// tiMAGE HAS PROBLEMS SCALING THE ICON DOWN... SO WE USE THIS TO DRAW IT FIRST ON A BITMAP IN THE PROPER SIZE AND THEN ASSIGN TO THE IMAGE.PICTURE
Procedure LoadIconIntoImagesTRETCHED(aImage: TImage; const aFileName: String;
  aNewWidth, aNewHeight: Integer; aProportional: Boolean = True;
aIconFlag: Cardinal = SHIL_JUMBO);

                                                                           // the bitmap must have pixelformat pf32bit!
// source: https://stackoverflow.com/questions/64597831/how-to-use-correctly-tbitmap-object-to-save-a-file-with-transparency
procedure SaveTransparentBitmap(ABitmap: TBitmap; const AFileName: string);


// Source: http://www.entwickler-ecke.de/topic_Bitmap+pf32bit+mit+Alpha+afPremultipied+zu+PNG+speichern_103159,0.html
function PNGToTransparentBitMap(aBitmap:TBitmap): TPNGImage;

implementation

uses
  autoFree, JclGraphics;


function GetShellImageList(aFlag: Cardinal): HIMAGELIST;
type
  TSHGetImageListFunc = function(iImageList: integer; const riid: TGUID; var ppv: Pointer): hResult; stdcall;
var
  lHandle: THandle;
  lFunc: TSHGetImageListFunc;
begin
  Result := 0;
  lHandle := LoadLibrary('Shell32.dll');
  if (lHandle <> 0) and (lHandle <> INVALID_HANDLE_VALUE) then
    try
      lFunc := GetProcAddress(lHandle, PChar(727));
      if Assigned(lFunc) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
        lFunc(aFlag, IID_IImageList, Pointer(Result));
    finally
      FreeLibrary(lHandle);
    end;
end;

procedure GetIconFromFile(const aFile: string; var aIcon: TIcon; aFlag: Cardinal);
var
  lImageList: HIMAGELIST;
  lSHFileInfo: TSHFileInfo;
  lIndex: integer;
begin
  // Get the index of the imagelist
  SHGetFileInfo(PChar(aFile), FILE_ATTRIBUTE_NORMAL, lSHFileInfo, SizeOf(TSHFileInfo),
    SHGFI_ICON or SHGFI_LARGEICON or SHGFI_SHELLICONSIZE or SHGFI_SYSICONINDEX or SHGFI_TYPENAME or SHGFI_DISPLAYNAME);
  if not Assigned(aIcon) then
    aIcon := TIcon.Create;

  // get the imagelist
  lImageList := GetShellImageList(aFlag);

  // get index
  lIndex := lSHFileInfo.iIcon;

  // extract the icon handle
  aIcon.Handle := ImageList_GetIcon(lImageList, lIndex, ILD_NORMAL);
end;


Procedure LoadIconIntoImagesTRETCHED(aImage: TImage; const aFileName: String;
  aNewWidth, aNewHeight: Integer; aProportional: Boolean = True;
aIconFlag: Cardinal = SHIL_JUMBO);
var
  lIcon:TIcon;
  lBmp:TBitmap;
  w, h: Cardinal;
  lRatio, lRatioW, lRatioH: double;
begin
  gc(lIcon, TIcon.Create);
  gc(lBmp, TBitMap.create);

  GetIconFromFile(aFileName, lIcon);

  if aProportional then
  begin
    lRatioW:= aNewWidth / lIcon.Width;
    lRatioH:=  aNewHeight / lIcon.Height ;
    if lRatioW < lratioH then
      lRatio:= lRatioW
    else
      lRatio:= lRatioH;
    w:= round(lIcon.Width* lRatio);
    h:= round(lIcon.Height* lRatio);
  end else begin
    w:=aNewWidth;
    h:= aNewHeight;
  end;

  lBmp.SetSize(w, h);
  lBmp.PixelFormat:= pf32bit;

  JclGraphics.Stretch(W, H, TResamplingFilter.rfLanczos3, 0, lIcon, lBmp);
  lbmp.Transparent:= True;
  aimage.Picture.Assign(lBmp);
end;

Procedure LoadIconIntoImage(aImage: TImage; const aFileName: String; aIconFlag: Cardinal = SHIL_JUMBO);
var
  lIcon: TIcon;
begin
  lIcon := TIcon.Create;
  try
    GetIconFromFile(aFileName, lIcon, aIconFlag);
    aImage.Picture.Icon.Assign(lIcon);
  finally
    lIcon.Free;
  end;
end;


procedure SaveTransparentBitmap(ABitmap: TBitmap; const AFileName: string);
var
  FS: TFileStream;
  BFH: TBitmapFileHeader;
  BIH: TBitmapV5Header;
  y: Integer;
  sl: PUInt64;
begin

  // ABitmap MUST have the GIMP BGRA format.

  FS := TFileStream.Create(AFileName, fmCreate);
  try

    // Bitmap file header
    FillChar(BFH, SizeOf(BFH), 0);
    BFH.bfType := $4D42;  // BM
    BFH.bfSize := 4 * ABitmap.Width * ABitmap.Height + SizeOf(BFH) + SizeOf(BIH);
    BFH.bfOffBits := SizeOf(BFH) + SizeOf(BIH);
    FS.Write(BFH, SizeOf(BFH));

    // Bitmap info header
    FillChar(BIH, SizeOf(BIH), 0);
    BIH.bV5Size := SizeOf(BIH);
    BIH.bV5Width := ABitmap.Width;
    BIH.bV5Height := ABitmap.Height;
    BIH.bV5Planes := 1;
    BIH.bV5BitCount := 32;
    BIH.bV5Compression := BI_BITFIELDS;
    BIH.bV5SizeImage := 4 * ABitmap.Width * ABitmap.Height;
    BIH.bV5XPelsPerMeter := 11811;
    BIH.bV5YPelsPerMeter := 11811;
    BIH.bV5ClrUsed := 0;
    BIH.bV5ClrImportant := 0;
    BIH.bV5RedMask :=   $00FF0000;
    BIH.bV5GreenMask := $0000FF00;
    BIH.bV5BlueMask :=  $000000FF;
    BIH.bV5AlphaMask := $FF000000;
    BIH.bV5CSType := $73524742; // BGRs
    BIH.bV5Intent := LCS_GM_GRAPHICS;
    FS.Write(BIH, SizeOf(BIH));

    // Pixels
    for y := ABitmap.Height - 1 downto 0 do
    begin
      sl := ABitmap.ScanLine[y];
      FS.Write(sl^, 4 * ABitmap.Width);
    end;

  finally
    FS.Free;
  end;
end;




// Source: http://www.entwickler-ecke.de/topic_Bitmap+pf32bit+mit+Alpha+afPremultipied+zu+PNG+speichern_103159,0.html
                                    function PNGToTransparentBitMap(aBitmap:TBitmap): TPNGImage;
type
  TRGB = packed record B, G, R: byte end;
  TRGBA = packed record B, G, R, A: byte end;
  TRGBAArray = array[0..0] of TRGBA;
var
  X, Y: integer;
  BmpRGBA: ^TRGBAArray;
  PngRGB: ^TRGB;
begin
  //201011 Thomas Wassermann

  Result := TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, aBitmap.Width , aBitmap.Height);

  Result.CreateAlpha;
  Result.Canvas.CopyMode:= cmSrcCopy;
  Result.Canvas.Draw(0, 0, aBitmap);

  for Y := 0 to Pred(aBitmap.Height) do
  begin
    BmpRGBA := aBitmap.ScanLine[Y];
    PngRGB:= Result.Scanline[Y];

    for X := 0 to Pred(aBitmap.width) do
    begin
      Result.AlphaScanline[Y][X] :=  BmpRGBA[X].A;
      if aBitmap.AlphaFormat in [afDefined, afPremultiplied] then
      begin
        if BmpRGBA[X].A <> 0 then
        begin
          PngRGB^.B := Round(BmpRGBA[X].B / BmpRGBA[X].A * 255);
          PngRGB^.R := Round(BmpRGBA[X].R / BmpRGBA[X].A * 255);
          PngRGB^.G := Round(BmpRGBA[X].G / BmpRGBA[X].A * 255);
        end else begin
          PngRGB^.B := Round(BmpRGBA[X].B * 255);
          PngRGB^.R := Round(BmpRGBA[X].R * 255);
          PngRGB^.G := Round(BmpRGBA[X].G * 255);
        end;
      end;
      Inc(PngRGB);
    end;
  end;
end;

end.
