unit maxPDFPrintHelper;

{
  Printing PDF files is not trivial.
  Usually,  you do it either by parsing the pdf file and sending it directly to the printer or by asking acrobat reader to do it for you.
  Parsing PDF files is possible only with expensive 3rd Components...

  Adobe isnt great at that, there are some problems with the new version...
  Because of that, I use the free Sumatra PDF
  https://www.sumatrapdfreader.org/free-pdf-reader.html

}

interface

uses
  winApi.Windows, system.classes, system.sysUtils, vcl.printers, System.Win.registry, Winapi.shellAPI, Winapi.ShLwApi, generics.Collections;

type
  TmaxPDFPrintHelper = class
  private
    fFiles: TStringList;

    class var
      fsumatraPdfExeFileName: string;
    class constructor CreateClass;
  private
    FshowPrinterDialog: boolean;
    FPrinterName: string;
    FPrinterSettings: string;
    FLogDir: string;
    procedure SetshowPrinterDialog(const Value: boolean);
    procedure SetPrinterName(const Value: string);
    procedure SetPrinterSettings(const Value: string);
    procedure SetlogDir(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    { if aPrinterName is empty, then the default printer is used
      aPrintSettings is a command separated list of parameters for the following command line param:    }

    procedure Print;
    class procedure selfTest;
  public
    // set back defaults;
    procedure reset;
    procedure AddFile(const aPdfFileName: string);

    { -print-settings <settings-list>
      Used in combination with -print-to and -print-to-default . Allows to tweak some of the printing related settings without using the Print dialog. The settings-list is a comma separated list of page ranges and advanced options such as
      even or odd
      noscale , shrink and fit
      color or monochrome
      duplex , duplexshort , duplexlong and simplex
      bin=<num or name> : select tray to print to
      E.g. -print-settings "1-3,5,10-8,odd,fit,bin=2" prints pages 1, 3, 5, 9 (i.e. the odd pages from the ranges 1-3, 5-5 and 10-8) and scales them so that they fit into the printable area of the paper.
      -print-settings "3x" : prints the document 3 times
    }
    property PrinterSettings: string read FPrinterSettings write SetPrinterSettings;

    // leave blank to print to the default printer
    property PrinterName: string read FPrinterName write SetPrinterName;
    property showPrinterDialog: boolean read FshowPrinterDialog write SetshowPrinterDialog;
    property logDir: string read FLogDir write SetlogDir;

    // per default it is the same directory as the application Exe
    // it is a class property, so you need to set it up just once at start up
    class property sumatraPdfExeFileName: string read fsumatraPdfExeFileName write fsumatraPdfExeFileName;
  end;

implementation

uses
  autoFree, MaxLogic.ioUtils,
  system.ioUtils, system.StrUtils;

{ TmaxPDFPrintHelper }

procedure TmaxPDFPrintHelper.AddFile(const aPdfFileName: string);
begin
  fFiles.add(aPdfFileName)
end;

constructor TmaxPDFPrintHelper.Create;
begin
  inherited Create;
  fFiles := TStringList.Create;
end;

class constructor TmaxPDFPrintHelper.CreateClass;
begin
  fsumatraPdfExeFileName := getInstallDir + 'SumatraPDF.exe';
end;

destructor TmaxPDFPrintHelper.Destroy;
begin
  fFiles.FREE;
  inherited;
end;

procedure TmaxPDFPrintHelper.Print;
var
  params: TList<String>;
  x: integer;
  s: string;
  garbos: TGarbos;
begin
  gc(params, TList<String>.Create, garbos);

  if not FshowPrinterDialog then
  begin
    if FPrinterName = '' then
      // -print-to-default : Prints all files indicated on this command line to the system default printer.
      // After printing, SumatraPDF exits immediately (check the error code for failure).
      params.add('-print-to-default')
    else
    begin
      // -print-to <printer-name> : Prints all files indicated on this command line to the named printer.
      // After printing, SumatraPDF exits immediately (check the error code for failure).
      // E.g. -print-to "Microsoft XPS Document Writer" prints all indicated files to the XPS virtual printer.
      params.add('-print-to ' + AnsiQuotedStr(Trim(FPrinterName), '"'));
    end;
  end;

  if FPrinterSettings <> '' then
    params.add('-print-settings ' + AnsiQuotedStr(Trim(FPrinterSettings), '"'));

  if FshowPrinterDialog then
  begin
    // -print-dialog : Displays the Print dialog for all the files indicated on this command line.
    params.add('-print-dialog');
    // -exit-when-done : Used in combination with -print-dialog (and -stress-test ). Exits SumatraPDF after the Print dialog has been dismissed and the document printed.
    params.add('-exit-when-done');
  end;

  for x := 0 to fFiles.count - 1 do
    params.add(AnsiQuotedStr(fFiles[x], '"'));

  s := String.Join(' ', params.ToArray);

  if FLogDir <> '' then
    if TDirectory.Exists(FLogDir) then
    begin
      TFile.AppendAllText(
        TPath.combine(FLogDir,  ClassName + '.log'),
        'Run:' + sLineBreak +
        fsumatraPdfExeFileName + ' ' + s + sLineBreak,
        TEncoding.UTF8);
    end;

  MaxLogic.ioUtils.ExecuteFile(
    fsumatraPdfExeFileName,
    s,
    extractFilePath(fsumatraPdfExeFileName),
    true);
end;

procedure TmaxPDFPrintHelper.reset;
begin
  fFiles.Clear;
  FshowPrinterDialog := false;
  FPrinterName := '';
  FPrinterSettings := '';
end;

class procedure TmaxPDFPrintHelper.selfTest;
var
  W: TmaxPDFPrintHelper;
  garbos: TGarbos;
begin
  gc(W, TmaxPDFPrintHelper.Create, garbos);

end;

procedure TmaxPDFPrintHelper.SetlogDir(const Value: string);
begin
  FLogDir := Value;
end;

procedure TmaxPDFPrintHelper.SetPrinterName(const Value: string);
begin
  FPrinterName := Value;
end;

procedure TmaxPDFPrintHelper.SetPrinterSettings(const Value: string);
begin
  FPrinterSettings := Value;
end;

procedure TmaxPDFPrintHelper.SetshowPrinterDialog(const Value: boolean);
begin
  FshowPrinterDialog := Value;
end;

initialization

  TmaxPDFPrintHelper.selfTest;

end.

