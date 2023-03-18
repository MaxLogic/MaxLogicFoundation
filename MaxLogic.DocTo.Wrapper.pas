unit MaxLogic.DocTo.Wrapper;

interface

uses
  windows, classes, sysUtils;

Type
  TConvertResult = (
    failUnknown, // failed for unknown reasons
    failInputFileNotPresent, // the input file was not present
    failUnsupportedInputExtension, // only some extensions are supported
    failOutputFormatNotSupported, // not all input files can be converted to all output formats
    Success // all is fine
    );

  TDocToWrapper = class
  private
    fInExt: String;
    fOutExt: String;
    fParams: TStringList;
    Function DoConvert(const InFile, OutFile: String): TConvertResult;
    Function ChooseOutFormat(const aPdfCode, aHtmlCode: String): String;
  strict private
    class var fDocToExeFilename: String;
    class function GetDocToExeFilename: String; static;
    class procedure SetDocToExeFilename(const Value: String); static;
  public
    Constructor Create;
    Destructor Destroy; override;

    // tries to convert in file to out file
    // supported input formats: doc, docx, rtf, txt, xls, xlsx, ppt
    // supported ouput formats: html, pdf*
    // ppt does not support exporting to html
    Class Function Convert(const InFile, OutFile: String): TConvertResult;
    // checks if this input extension can be exported to pdf or html
    Class Function InputSupported(const aFileName: String): Boolean;

    // per default it assumes it is in the same directory as the exe / dll file, but you might like to specify a differnt directory
    class property DocToExeFilename: String read GetDocToExeFilename write SetDocToExeFilename;
  end;

implementation

uses
  strUtils, ioUtils, MaxLogic.ioUtils, autoFree;

{ TDocToWrapper }

constructor TDocToWrapper.Create;
begin
  inherited;
  fParams := TStringList.Create;
end;

destructor TDocToWrapper.Destroy;
begin
  fParams.Free;
  inherited;
end;

function TDocToWrapper.DoConvert(const InFile, OutFile: String): TConvertResult;
var
  lOutputFormatCode: String;
  exeFn, lParams: String;
Begin
  Result := TConvertResult.failUnknown; // default;

  if not TFile.Exists(InFile) then
    exit(TConvertResult.failInputFileNotPresent);
  fInExt := ExtractFileExt(InFile);
  fOutExt := ExtractFileExt(OutFile);

  // use xls?
  if MatchText(fInExt, ['.xls', '.xlsx']) then
  begin
    fParams.Add('-XL'); // -XL Use Excel for Conversion. Help '-h -xl';
    lOutputFormatCode := ChooseOutFormat('xlTypePDF', 'xlHtml');
  end else if MatchText(fInExt, ['.ppt']) then
  begin
    fParams.Add('-PP'); // -PP Use Powerpoint for Conversion. help '-h -pp';
    lOutputFormatCode := ChooseOutFormat('ppSaveAsPDF', '');
  end else if MatchText(fInExt, ['.doc', '.docx', '.rtf', '.txt']) then
  begin
    // word is default, we do not need to add -WD
    lOutputFormatCode := ChooseOutFormat('wdFormatPDF', 'wdFormatHTML');
  end
  else
    exit(TConvertResult.failUnsupportedInputExtension);

  if lOutputFormatCode = '' then
    exit(TConvertResult.failOutputFormatNotSupported);

  fParams.Add('-Q');
  fParams.Add('-f "' + InFile + '"');
  fParams.Add('-O "' + OutFile + '"');
  fParams.Add('-T ' + lOutputFormatCode);

  if TFile.Exists(OutFile) then
    TFile.Delete(OutFile);

  fParams.LineBreak := ' ';
  lParams := fParams.Text;
  exeFn := DocToExeFilename;
  MaxLogic.ioUtils.ExecuteFile(exeFn, lParams, '', True, True);

  if TFile.Exists(OutFile) then
    Result := TConvertResult.Success;
End;

function TDocToWrapper.ChooseOutFormat(const aPdfCode,
  aHtmlCode: String): String;
begin
  if sameText(fOutExt, '.pdf') then
    Result := aPdfCode
  else if sameText(fOutExt, '.html') then
    Result := aHtmlCode
  else
    Result := '';
end;

class function TDocToWrapper.Convert(const InFile, OutFile: String): TConvertResult;
var
  c: TDocToWrapper;
begin
  gc2(c, TDocToWrapper.Create);
  Result := c.DoConvert(InFile, OutFile);
end;

class function TDocToWrapper.GetDocToExeFilename: String;
begin
  if fDocToExeFilename = '' then
    fDocToExeFilename := MaxLogic.ioUtils.GetInstallDir + 'DocTo.exe';
  Result := fDocToExeFilename;
end;

class function TDocToWrapper.InputSupported(
  const aFileName: String): Boolean;
begin
  Result := MatchText(ExtractFileExt(aFileName), ['.xls', '.xlsx', '.ppt', '.doc', '.docx', '.rtf', '.txt']);
end;

class procedure TDocToWrapper.SetDocToExeFilename(const Value: String);
begin
  fDocToExeFilename := Value;
end;

end.
