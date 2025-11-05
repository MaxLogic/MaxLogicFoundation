unit MaxLogic.TabularReader;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  maxCsv,
  {$IFDEF MSWINDOWS}
  Vcl.FlexCel.Core,
  {$ELSE}
  SKIA.FlexCel.Core,
  {$ENDIF}
  FlexCel.XLSAdapter, Variants;

type
  TRow = maxCsv.TRow;

  ITableReader = interface(IInterface)
    ['{1B7EAD9E-8D2A-48A4-8977-224422D304D1}']
    function ReadRow: boolean;
    function GetEOF: boolean;
    function GetHeader: TRow;
    function GetRowCount: Integer;

    // Excel specific features
    function GetSheetCount: integer;
    function GetSheetNames: TArray<string>;
    procedure SelectSheet(const aSheetName: string); overload;
    procedure SelectSheet(const aSheetIndex: integer); overload; // 1-based index
    function GetCurrentSheetName: string;
    function GetCurrentSheetIndex: integer; // 1-based index

    // Row access by column name
    function GetValue(const aColName: string): string;
    function TryGetValue(const aColName: string; out aValue: string): boolean;

    procedure Open(const aFileName: string);

    property EOF: boolean read GetEOF;
    property Header: TRow read GetHeader;
    property RowCount: Integer read GetRowCount;
    property Values[const aColName: string]: string read GetValue; default;
    property SheetCount: integer read GetSheetCount;
    property SheetNames: TArray<string> read GetSheetNames;
    property CurrentSheetName: string read GetCurrentSheetName;
    property CurrentSheetIndex: integer read GetCurrentSheetIndex;

    function GetCurrentRow: TRow;
    property CurrentRow: TRow read GetCurrentRow;
  end;

  ITabularReaderStrategy = interface(IInterface)
    ['{8A9561E8-6B40-4A0B-975B-31B72863F73D}']
    function ReadRow(var aRow: TRow): boolean;
    function GetEOF: boolean;
    function GetHeader: TRow;
    function GetRowCount: Integer;

    // Excel specific
    function GetSheetCount: integer;
    function GetSheetNames: TArray<string>;
    procedure SelectSheet(const aSheetName: string); overload;
    procedure SelectSheet(const aSheetIndex: integer); overload;
    function GetCurrentSheetName: string;
    function GetCurrentSheetIndex: integer;

    property EOF: boolean read GetEOF;
    property Header: TRow read GetHeader;
    property RowCount: integer read GetRowCount;
  end;

  // Facade class
  TTabularReader = class(TInterfacedObject, ITableReader)
  private
    fStrategy: ITabularReaderStrategy;
    fRowReaderWriter: TRowReaderWriter;
    fCurrentRow: TRow;
    procedure RequireOpen;
    function GetCurrentSheetIndex: integer;
    function GetCurrentSheetName: string;
    function GetEOF: boolean;
    function GetHeader: TRow;
    function GetRowCount: Integer;
    function GetSheetCount: integer;
    function GetSheetNames: TArray<string>;
    function GetValue(const aColName: string): string;
    procedure SelectSheet(const aSheetName: string); overload;
    procedure SelectSheet(const aSheetIndex: integer); overload;
    function TryGetValue(const aColName: string; out aValue: string): boolean;
    function GetCurrentRow: TRow;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Open(const aFileName: string);
    function ReadRow: boolean;
  end;

implementation

uses
  System.IOUtils;

type
  { TTabularCsvAdapter }

  TTabularCsvAdapter = class(TInterfacedObject, ITabularReaderStrategy)
  private
    fReader: TCsvReader;
    fHeader: TRow;
    function GetEOF: boolean;
    function GetHeader: TRow;
    function GetRowCount: Integer;
    function GetSheetCount: integer;
    function GetSheetNames: TArray<string>;
    procedure SelectSheet(const aSheetName: string); overload;
    procedure SelectSheet(const aSheetIndex: integer); overload;
    function GetCurrentSheetName: string;
    function GetCurrentSheetIndex: integer;
  public
    constructor Create(const aFileName: string);
    destructor Destroy; override;
    function ReadRow(var aRow: TRow): boolean;


    property EOF: boolean read GetEOF;
    property Header: TRow read GetHeader;
    property RowCount: integer read GetRowCount;
  end;

  { TTabularFlexCelAdapter }

  TTabularFlexCelAdapter = class(TInterfacedObject, ITabularReaderStrategy)
  private
    fXls: TXLSFile;
    fHeader: TRow;
    fCurrentRowIndex: integer;
    fRowCount: Integer;
    function GetCellData(aRow, aCol: integer): string;
    function GetEOF: boolean;
    function GetHeader: TRow;
    function GetRowCount: Integer;
    function GetSheetCount: integer;
    function GetSheetNames: TArray<string>;
    procedure SelectSheet(const aSheetName: string); overload;
    procedure SelectSheet(const aSheetIndex: integer); overload;
    function GetCurrentSheetName: string;
    function GetCurrentSheetIndex: integer;
    procedure InternalSelectSheet(aIndex: integer);
  public
    constructor Create(const aFileName: string);
    destructor Destroy; override;
    function ReadRow(var aRow: TRow): boolean;


    property EOF: boolean read GetEOF;
    property Header: TRow read GetHeader;
    property RowCount: integer read GetRowCount;
  end;

{ TTabularReader }

constructor TTabularReader.Create;
begin
  inherited Create;
  fRowReaderWriter := TRowReaderWriter.Create;
end;

destructor TTabularReader.Destroy;
begin
  fStrategy := nil;
  fRowReaderWriter.Free;
  inherited;
end;

procedure TTabularReader.RequireOpen;
begin
  if not Assigned(fStrategy) then
    raise Exception.Create('Tabular reader is not opened. Call Open before use.');
end;

procedure TTabularReader.Open(const aFileName: string);
var
  lExt: string;
  lStrategy: ITabularReaderStrategy;
  lOldStrategy: ITabularReaderStrategy;
begin
  lExt := TPath.GetExtension(aFileName).ToLower;
  if (lExt = '.xls') or (lExt = '.xlsx') then
    lStrategy := TTabularFlexCelAdapter.Create(aFileName)
  else
    lStrategy := TTabularCsvAdapter.Create(aFileName);

  lOldStrategy := fStrategy;
  fStrategy := lStrategy;
  lOldStrategy := nil;

  fRowReaderWriter.Header := fStrategy.Header;
  SetLength(fCurrentRow, 0);
  fRowReaderWriter.Data := fCurrentRow;
end;

function TTabularReader.GetCurrentRow: TRow;
begin
  RequireOpen;
  Result := fCurrentRow;
end;

function TTabularReader.GetEOF: boolean;
begin
  RequireOpen;
  Result := fStrategy.EOF;
end;

function TTabularReader.GetHeader: TRow;
begin
  RequireOpen;
  Result := fRowReaderWriter.Header;
end;

function TTabularReader.GetRowCount: Integer;
begin
  RequireOpen;
  Result := fStrategy.RowCount;
end;

function TTabularReader.GetCurrentSheetIndex: integer;
begin
  RequireOpen;
  Result := fStrategy.GetCurrentSheetIndex;
end;

function TTabularReader.GetCurrentSheetName: string;
begin
  RequireOpen;
  Result := fStrategy.GetCurrentSheetName;
end;

function TTabularReader.GetSheetCount: integer;
begin
  RequireOpen;
  Result := fStrategy.GetSheetCount;
end;

function TTabularReader.GetSheetNames: TArray<string>;
begin
  RequireOpen;
  Result := fStrategy.GetSheetNames;
end;

function TTabularReader.GetValue(const aColName: string): string;
begin
  RequireOpen;
  Result := fRowReaderWriter.Values[aColName];
end;

function TTabularReader.ReadRow: boolean;
begin
  RequireOpen;
  Result := fStrategy.ReadRow(fCurrentRow);
  if Result then
    fRowReaderWriter.Data := fCurrentRow;
end;

procedure TTabularReader.SelectSheet(const aSheetIndex: integer);
begin
  RequireOpen;
  fStrategy.SelectSheet(aSheetIndex);
  fRowReaderWriter.Header := fStrategy.Header;
end;

procedure TTabularReader.SelectSheet(const aSheetName: string);
begin
  RequireOpen;
  fStrategy.SelectSheet(aSheetName);
  fRowReaderWriter.Header := fStrategy.Header;
end;

function TTabularReader.TryGetValue(const aColName: string; out aValue: string): boolean;
begin
  RequireOpen;
  Result := fRowReaderWriter.TryGetByName(aColName, aValue);
end;

{ TTabularCsvAdapter }

constructor TTabularCsvAdapter.Create(const aFileName: string);
begin
  inherited Create;
  fReader := TCsvReader.Create;
  try
    fReader.Open(aFileName);
    if not fReader.EOF then
      fReader.ReadRow(fHeader);
  except
    FreeAndNil(fReader);
    raise;
  end;
end;

destructor TTabularCsvAdapter.Destroy;
begin
  fReader.Free;
  inherited;
end;

function TTabularCsvAdapter.GetEOF: boolean;
begin
  Result := fReader.EOF;
end;

function TTabularCsvAdapter.GetHeader: TRow;
begin
  Result := fHeader;
end;

function TTabularCsvAdapter.GetRowCount: Integer;
begin
  raise Exception.Create('Row count is not available for CSV files.');
end;

function TTabularCsvAdapter.GetCurrentSheetIndex: integer;
begin
  Result := 1;
end;

function TTabularCsvAdapter.GetCurrentSheetName: string;
begin
  Result := 'default';
end;

function TTabularCsvAdapter.GetSheetCount: integer;
begin
  Result := 1;
end;

function TTabularCsvAdapter.GetSheetNames: TArray<string>;
begin
  Result := ['default'];
end;

function TTabularCsvAdapter.ReadRow(var aRow: TRow): boolean;
begin
  Result := fReader.ReadRow(aRow);
end;

procedure TTabularCsvAdapter.SelectSheet(const aSheetIndex: integer);
begin
  if aSheetIndex <> 1 then
    raise Exception.Create('CSV files only have one sheet.');
end;

procedure TTabularCsvAdapter.SelectSheet(const aSheetName: string);
begin
  if not SameText(aSheetName, 'default') then
    raise Exception.Create('CSV files only have one sheet named "default".');
end;

{ TTabularFlexCelAdapter }

constructor TTabularFlexCelAdapter.Create(const aFileName: string);
begin
  inherited Create;
  fXls := TXLSFile.Create(true);
  fXls.Open(aFileName);
  fCurrentRowIndex := 0; // 0 means before first row
  if fXls.SheetCount > 0 then
    InternalSelectSheet(1);
end;

destructor TTabularFlexCelAdapter.Destroy;
begin
  fXls.Free;
  inherited;
end;

function TTabularFlexCelAdapter.GetCellData(aRow, aCol: integer): string;
var
  v: TCellValue;
  xf: Integer;
  fmt: TFlxFormat;
  s: String;
  color: TUIColor;
  hasDate, hasTime: boolean;
begin
  xf := -1;
  v := fXls.GetCellValue(aRow, aCol, xf);
  if v.IsString then
    Result := v.ToString
  else if v.IsNumber then
  begin
    fmt := fXls.GetCellVisibleFormatDef(aRow, aCol);
    s := TFlxNumberFormat.FormatValue(v, fmt.Format, color, fXls, hasDate, hasTime).ToString;
    if hasDate or hasTime then
    begin
      var dt := v.ToDateTime(fXls.OptionsDates1904);
      if HasDate and HasTime then
        Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', dt)
      else if HasDate then
        Result := FormatDateTime('yyyy-mm-dd', dt)
      else
        Result := FormatDateTime('hh:nn:ss', dt);
    end else
      Result := s;
  end
  else if v.IsBoolean then
    Result := System.SysUtils.BoolToStr(v.AsBoolean, true)
  else
    Result := '';
end;

function TTabularFlexCelAdapter.GetEOF: boolean;
begin
  Result := fCurrentRowIndex >= fRowCount;
end;

function TTabularFlexCelAdapter.GetHeader: TRow;
begin
  Result := fHeader;
end;

function TTabularFlexCelAdapter.GetRowCount: Integer;
begin
  // if there's a header, rowcount is total rows minus header row
  if Length(fHeader) > 0 then
    Result := fRowCount - 1
  else
    Result := fRowCount;
  if Result < 0 then
    Result := 0;
end;

procedure TTabularFlexCelAdapter.InternalSelectSheet(aIndex: integer);
var
  i: integer;
  lColcount: integer;
begin
  fXls.ActiveSheet := aIndex;
  fRowCount := fXls.RowCount;
  fCurrentRowIndex := 0;
  fHeader := [];

  if fRowCount > 0 then
  begin
    lColcount := fXls.ColCountInRow(1);
    if lColcount > 0 then
    begin
      SetLength(fHeader, lColcount);
      for i := 1 to lColcount do
        fHeader[i - 1] := GetCellData(1, i).Trim;
    end;
  end;
end;

function TTabularFlexCelAdapter.GetCurrentSheetIndex: integer;
begin
  Result := fXls.ActiveSheet;
end;

function TTabularFlexCelAdapter.GetCurrentSheetName: string;
begin
  Result := fXls.SheetName;
end;

function TTabularFlexCelAdapter.GetSheetCount: integer;
begin
  Result := fXls.SheetCount;
end;

function TTabularFlexCelAdapter.GetSheetNames: TArray<string>;
var
  i: integer;
begin
  SetLength(Result, fXls.SheetCount);
  for i := 1 to fXls.SheetCount do
    Result[i-1] := fXls.GetSheetName(i);
end;

function TTabularFlexCelAdapter.ReadRow(var aRow: TRow): boolean;
var
  i, lColcount: integer;
begin
  if fCurrentRowIndex = 0 then // First read after header
  begin
    if Length(fHeader) > 0 then
      fCurrentRowIndex := 1
    else
      fCurrentRowIndex := 0;
  end;

  Inc(fCurrentRowIndex);

  if Self.EOF then
  begin
    aRow := [];
    Result := False;
    exit;
  end;

  lColcount := Length(fHeader);
  SetLength(aRow, lColcount);
  for i := 1 to lColcount do
    aRow[i - 1] := GetCellData(fCurrentRowIndex, i);

  Result := True;
end;

procedure TTabularFlexCelAdapter.SelectSheet(const aSheetIndex: integer);
begin
  InternalSelectSheet(aSheetIndex);
end;

procedure TTabularFlexCelAdapter.SelectSheet(const aSheetName: string);
var
  i: integer;
begin
  for i := 1 to fXls.SheetCount do
  begin
    if SameText(fXls.GetSheetName(i), aSheetName) then
    begin
      InternalSelectSheet(i);
      exit;
    end;
  end;
  raise Exception.CreateFmt('Sheet "%s" not found.', [aSheetName]);
end;

end.
