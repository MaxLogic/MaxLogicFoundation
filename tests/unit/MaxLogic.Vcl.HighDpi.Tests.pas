unit MaxLogic.Vcl.HighDpi.Tests;

{$I fpc_delphimode.inc}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  THighDpiImageTests = class
  public
    [Test]
    procedure FixedImageGraphicScalesWithoutChangingControlSemantics;
    [Test]
    procedure RepeatedDpiChangesAlwaysScaleFromOriginalGraphic;
    [Test]
    procedure ReplacedGraphicBecomesNewScalingSource;
    [Test]
    procedure ZeroSizedGraphicIsIgnored;
  end;

implementation

uses
  System.Types,
  Vcl.ExtCtrls, Vcl.Forms, Vcl.Graphics,
  AutoFree, MaxLogic.vcl.highDpi;

procedure THighDpiImageTests.FixedImageGraphicScalesWithoutChangingControlSemantics;
const
  cDesignPpi = 96;
  cTargetPpi = 192;
  cSourceHeight = 10;
  cSourceWidth = 16;
var
  g: TGarbos;
  lBoundsAfterVclScaling: TRect;
  lForm: TForm;
  lImage: TImage;
begin
  GC(lForm, TForm.CreateNew(nil), g);
  lForm.Scaled := True;
  lForm.PixelsPerInch := cDesignPpi;

  lImage := TImage.Create(lForm);
  lImage.Parent := lForm;
  lImage.AutoSize := False;
  lImage.Center := True;
  lImage.Proportional := False;
  lImage.Stretch := False;
  lImage.SetBounds(8, 12, 100, 60);
  lImage.Picture.Bitmap.SetSize(cSourceWidth, cSourceHeight);
  lImage.Picture.Bitmap.Canvas.Brush.Color := clRed;
  lImage.Picture.Bitmap.Canvas.FillRect(Rect(0, 0, cSourceWidth, cSourceHeight));

  lForm.ScaleForPPI(cTargetPpi);
  lBoundsAfterVclScaling := lImage.BoundsRect;

  THighDpiAdjuster.AdjustForm(lForm);

  Assert.AreEqual<Integer>(cSourceWidth * 2, lImage.Picture.Graphic.Width,
    'The fixed image graphic must be materialized at the current DPI');
  Assert.AreEqual<Integer>(cSourceHeight * 2, lImage.Picture.Graphic.Height,
    'The fixed image graphic must preserve its aspect ratio at the current DPI');
  Assert.AreEqual<TColor>(ColorToRGB(clRed),
    ColorToRGB(lImage.Picture.Bitmap.Canvas.Pixels[16, 10]),
    'DPI materialization must preserve the source image content');
  Assert.AreEqual<TRect>(lBoundsAfterVclScaling, lImage.BoundsRect,
    'Image scaling must not change the bounds already scaled by the VCL');
  Assert.IsFalse(lImage.AutoSize, 'Image scaling must preserve AutoSize');
  Assert.IsTrue(lImage.Center, 'Image scaling must preserve Center');
  Assert.IsFalse(lImage.Proportional, 'Image scaling must preserve Proportional');
  Assert.IsFalse(lImage.Stretch, 'Image scaling must preserve Stretch');
end;

procedure THighDpiImageTests.RepeatedDpiChangesAlwaysScaleFromOriginalGraphic;
const
  cDesignPpi = 96;
  cSourceHeight = 10;
  cSourceWidth = 16;
var
  g: TGarbos;
  lForm: TForm;
  lImage: TImage;
begin
  GC(lForm, TForm.CreateNew(nil), g);
  lForm.Scaled := True;
  lForm.PixelsPerInch := cDesignPpi;

  lImage := TImage.Create(lForm);
  lImage.Parent := lForm;
  lImage.AutoSize := False;
  lImage.Stretch := False;
  lImage.Picture.Bitmap.SetSize(cSourceWidth, cSourceHeight);

  lForm.ScaleForPPI(192);
  THighDpiAdjuster.AdjustForm(lForm);
  Assert.AreEqual<Integer>(32, lImage.Picture.Graphic.Width);
  Assert.AreEqual<Integer>(20, lImage.Picture.Graphic.Height);

  lForm.ScaleForPPI(144);
  THighDpiAdjuster.AdjustForm(lForm);
  Assert.AreEqual<Integer>(24, lImage.Picture.Graphic.Width,
    'The second DPI change must use the original width');
  Assert.AreEqual<Integer>(15, lImage.Picture.Graphic.Height,
    'The second DPI change must use the original height');

  lForm.ScaleForPPI(cDesignPpi);
  THighDpiAdjuster.AdjustForm(lForm);
  Assert.AreEqual<Integer>(cSourceWidth, lImage.Picture.Graphic.Width);
  Assert.AreEqual<Integer>(cSourceHeight, lImage.Picture.Graphic.Height);
end;

procedure THighDpiImageTests.ReplacedGraphicBecomesNewScalingSource;
const
  cDesignPpi = 96;
  cTargetPpi = 192;
var
  g: TGarbos;
  lForm: TForm;
  lImage: TImage;
begin
  GC(lForm, TForm.CreateNew(nil), g);
  lForm.Scaled := True;
  lForm.PixelsPerInch := cDesignPpi;

  lImage := TImage.Create(lForm);
  lImage.Parent := lForm;
  lImage.AutoSize := False;
  lImage.Stretch := False;
  lImage.Picture.Bitmap.SetSize(16, 10);

  lForm.ScaleForPPI(cTargetPpi);
  THighDpiAdjuster.AdjustForm(lForm);
  Assert.AreEqual<Integer>(32, lImage.Picture.Graphic.Width);
  Assert.AreEqual<Integer>(20, lImage.Picture.Graphic.Height);

  lImage.Picture.Bitmap.SetSize(12, 7);
  lImage.Picture.Bitmap.Canvas.Brush.Color := clBlue;
  lImage.Picture.Bitmap.Canvas.FillRect(Rect(0, 0, 12, 7));
  THighDpiAdjuster.AdjustForm(lForm);

  Assert.AreEqual<Integer>(24, lImage.Picture.Graphic.Width,
    'A replacement graphic must become the new unscaled source');
  Assert.AreEqual<Integer>(14, lImage.Picture.Graphic.Height,
    'The replacement graphic must use the current DPI');
end;

procedure THighDpiImageTests.ZeroSizedGraphicIsIgnored;
var
  g: TGarbos;
  lForm: TForm;
  lImage: TImage;
begin
  GC(lForm, TForm.CreateNew(nil), g);
  lImage := TImage.Create(lForm);
  lImage.Parent := lForm;
  lImage.AutoSize := False;
  lImage.Stretch := False;
  lImage.Picture.Bitmap.PixelFormat := pf32bit;

  THighDpiAdjuster.AdjustForm(lForm);

  Assert.AreEqual<Integer>(0, lImage.Picture.Bitmap.Width);
  Assert.AreEqual<Integer>(0, lImage.Picture.Bitmap.Height);
end;

initialization
  TDUnitX.RegisterTestFixture(THighDpiImageTests);

end.
