unit Maxlogic.PdfTk.Wrapper;

{
  a simple wraper for the pdftk CLI
  see
  home page:
  https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/
  man page:
  https://www.pdflabs.com/docs/pdftk-man-page/
  samples:
  https://www.pdflabs.com/docs/pdftk-cli-examples/

}

interface

uses
  system.SysUtils, system.Classes, generics.Collections,

  { see
  https://lib-docs.delphidabbler.com/ConsoleApp/3/Examples/Example1.html
  https://github.com/ddablib/consoleapp
  }
  PJConsoleApp,

  {see
  https://github.com/ddablib/ioutils
  https://lib-docs.delphidabbler.com/IOUtils/1/API.html
  }
  PJPipe;

Type
  TPdfTk = class
  private
    fLastError: TStringList;
    fPdfTkPath: String;
    fOutPipe: TPJPipe;
    fOutStream: TStream;
    procedure WorkHandler(Sender: TObject);
    function Q(const s: String): String;
  public
    constructor Create(const aPdfTkPath: String = '');
    destructor Destroy; override;

    // general CLI method
    // returns the exit code of pdfpk, the error message can be found in the LastErrorMessage property
    Function PdfTk(const aParams: String): integer;
    // various helper functions, they all will prepare the parameters and then call the general pdftkMethod
    Function Merge(const aInputFiles: TArray<String>; const aOutputFileName: String): integer;
    Function Decrypt(const aInputFileName, aPassword, aOutputFileName: String): integer;
    Function Encrypt(const aInputFileName, aPassword, aOutputFileName: String; aAllowPrintingWithoutPw: Boolean = True; const aPasswordForOpening: String = ''): integer;

    /// <summary>
    ///   This will extract all embeded files to the out directory
    ///  Most useful for embeded xml files for digital invoices
    /// </summary>
    function ExtractEmbededFiles(const aPdfFilename, aOutDir: String): integer;

    property LastError: TStringList read fLastError;
  end;

implementation

uses
  Maxlogic.ioUtils, system.ioUtils, system.StrUtils, autoFree;

{ TPdfTk }

constructor TPdfTk.Create(const aPdfTkPath: String);
begin
  inherited Create;
  fPdfTkPath := aPdfTkPath;
  fLastError := TStringList.Create;
end;

Function TPdfTk.Q(const s: String): String;
begin
  Result := AnsiQuotedStr(s, '"');
end;

procedure TPdfTk.WorkHandler(Sender: TObject);
begin
  fOutPipe.CopyToStream(fOutStream, 0);
end;

function TPdfTk.Decrypt(const aInputFileName, aPassword,
  aOutputFileName: String): integer;
begin
  Result := PdfTk(Q(aInputFileName) + ' input_pw ' + Q(aPassword) + ' output ' + Q(aOutputFileName));
end;

destructor TPdfTk.Destroy;
begin
  fLastError.Free;
  inherited;
end;

function TPdfTk.Encrypt(const aInputFileName, aPassword,
  aOutputFileName: String; aAllowPrintingWithoutPw: Boolean;
  const aPasswordForOpening: String): integer;
var
  cmd: String;
begin
  cmd := Q(aInputFileName) + ' output ' + Q(aOutputFileName) + ' owner_pw ' + Q(aPassword) +
    ifThen(aPasswordForOpening = '', '', ' user_pw ' + Q(aPasswordForOpening)) +
    ifThen(aAllowPrintingWithoutPw, ' allow printing', '');

  Result := PdfTk(cmd);
end;

function TPdfTk.ExtractEmbededFiles(const aPdfFilename, aOutDir: String):integer;
var
  lCmd: String;
begin
  ForceDirectories(aOutDir);
  lCmd := Q(aPdfFileName) +
  ' unpack_files output ' + Q(aOutDir);

  Result := PdfTk(lCmd);
end;

function TPdfTk.Merge(const aInputFiles: TArray<String>;
  const aOutputFileName: String): integer;
var
  fn: String;
  cmd: String;
begin
  case length(aInputFiles) of
    0:
      Exit(-1);
    1:
      begin
        TFile.Copy(aInputFiles[0], aOutputFileName);
        Exit(0);
      end;
  end;

  cmd := '';
  for fn in aInputFiles do
    cmd := cmd + Q(fn) + ' ';

  cmd := cmd + 'output ' + Q(aOutputFileName);
  Result := PdfTk(cmd);
end;

function TPdfTk.PdfTk(const aParams: String): integer;
var
  App: TPJConsoleApp;
  CommandLine: String;
  execFn: String;
begin
  fLastError.Clear;

  execFn := 'pdftk';
  if fPdfTkPath <> '' then
    execFn := Q(TPath.Combine(fPdfTkPath, execFn));

  CommandLine := execFn + ' ' + aParams;

  // Create out pipe and stream that receives out pipe's data
  fOutPipe := TPJPipe.Create;
  fOutStream := TMemoryStream.Create;
  try
    gc(App, TPJConsoleApp.Create);
    App.MaxExecTime := INFINITE; // don't time out
    App.TimeSlice := 2; // forces more than one OnWork event
    App.OnWork := WorkHandler;
    App.StdErr := fOutPipe.WriteHandle;
    App.Visible := False; // ensure we see the app
    if App.Execute(CommandLine) then
      Result := App.ExitCode
    else
      Result := -1;

    if fOutStream.Size <> 0 then
    begin
      fOutStream.Position := 0;
      fLastError.LoadFromStream(fOutStream);
    end;
  finally
    FreeAndNil(fOutPipe);
    FreeAndNil(fOutStream);
  end;
end;

end.
