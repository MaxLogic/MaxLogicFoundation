unit maxCsv;

{ Copyright: pawel Piotrowski
  Version: 1.4
  History:
  2023-04-18: added TRowReaderWriter for faster read/write to a row
  2023-04-11: added property CloseRowWithDelimiter
  2013-04-03: writer can now use different lineBreaks
  2016-09-12: update

  Info:
  according to the following spec:
  https://tools.ietf.org/html/rfc4180
}

{$IFNDEF DEBUG}
{$DEFINE USE_INLINE}
{$ENDIF}

interface

uses
  maxLogic.BufferedFile,
  System.Classes, System.SysUtils, System.Rtti, generics.collections;

type
  // forward declarations
  TCSVBase = class;
  TCsvReader = class;
  TCsvWriter = class;
  TRow = TArray<string>;
  TRawRow = TArray<rawByteString>;

  TRawRowHelper = record helper for TRawRow
  public
    function ToUnicode: TRow; inline;
  end;

  TCSVBase = class
  protected const
      CR = #13 + #10;
  protected
    fDelimiter: AnsiChar;
    FQuoteChar: AnsiChar;
    procedure SetQuoteChar(const Value: AnsiChar); virtual;
    procedure SetSeparator(const Value: AnsiChar); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property QuoteChar: AnsiChar read FQuoteChar write SetQuoteChar;
    property Delimiter: AnsiChar read fDelimiter write SetSeparator;
  end;

  TCsvReader = class(TCSVBase)
  private const
      cBufferSize = 1024 * 1024;
  private
    class var fPotentialDelimiters: array[AnsiChar] of boolean;
    class constructor CreateClass;
  private
    fBuffer: TBufferedFile;
    fUtf8BomDetected: boolean;

    fMaxColCountFound: integer;
    fForcedColCount: integer;
    fCsvFileSize: Int64;

    function GetEof: boolean;
    {$IFDEF USE_INLINE} inline;
    {$ENDIF}
    procedure DetectBom;

    // those methods are split for performance reasons. mostly quotes do not need to be checked, and we know when to check when the first char of a column is a quote char
    procedure ReadNonQuotedColValue(var aValue: rawByteString; var LineEndDetected: boolean);
    {$IFDEF USE_INLINE} inline;
    {$ENDIF}
    procedure ReadQuotedColValue(var aValue: rawByteString; var LineEndDetected: boolean);
    {$IFDEF USE_INLINE} inline;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    // this also calls detect delimiter if aUseDelimiter = #0. ATTENTION: the Delimiter will be converted to a ansiChar
    procedure Open(const FileName: string; aShareMode: Cardinal = fmShareDenyWrite; aUseDelimiter: char = #0); overload;
    procedure Open(const FileName: string; aUseDelimiter: char); overload;
    procedure Open(aStream: TStream; aTakeOwnerShipOfStream: boolean = False; aUseDelimiter: char = #0); overload;

    procedure Close;

    // it tries to find the proper delimiter
    // valid characters are: ,;| and #9 (tab)
    // the first found valid character is returned
    // only max 1024 bytes are checked
    // quoted columns are supported
    // default is comma ","
    function DetectDelimiter: AnsiChar;

    function ReadRow(var Row: TRawRow): boolean; overload;
    {$IFDEF USE_INLINE} inline;
    {$ENDIF}
    // this will try to convert the rawByteStrings to unicode
    function ReadRow(var Row: TRow): boolean; overload;
    {$IFDEF USE_INLINE} inline;
    {$ENDIF}

    // convenience helpers to load the whole CSV into memory
    /// <summary>
    ///  Reads the complete CSV file into a <see cref="TList<TRow>"/> where each entry
    ///  represents a row. Caller owns the list and its contained dynamic arrays.
    /// </summary>
    class function ReadAllToList(const aFileName: string; aUseDelimiter: char = #0): TList<TRow>; static;
    /// <summary>
    ///  Reads the complete CSV file and returns all data as dynamic array of rows.
    /// </summary>
    class function ReadAllToArray(const aFileName: string; aUseDelimiter: char = #0): TArray<TRow>; static;

    property EOF: boolean read GetEof;
    property Utf8BomDetected: boolean read fUtf8BomDetected;
    property MaxColCountFound: integer read fMaxColCountFound;
    // you can force the CSV reader to always output that amount of columns
    property ForcedColCount: integer read fForcedColCount write fForcedColCount;

    // some info about the underlying file
    property CsvFileSize: Int64 read fCsvFileSize;
  end;

  TCsvWriter = class(TCSVBase)
  private const
      cBufferSize = 1024 * 1024;
  private
    fBufferSize: integer;
    fStream: TStream;
    fOwnsStream: boolean;
    fBuffer: TMemoryStream;
    fEncoding: TEncoding;
    FLineBreak: rawByteString;
    FCloseRowWithDelimiter: boolean;
    fControlChars: array[char] of boolean;

    procedure WriteCell(const Value: string); overload;
    procedure WriteCell(const Value: rawByteString); overload;
    procedure SetLineBreak(const Value: rawByteString);
    procedure SetCloseRowWithDelimiter(const Value: boolean);
    procedure UpdateControlChars;
    procedure SetQuoteChar(const Value: AnsiChar); override;
    procedure SetSeparator(const Value: AnsiChar); override;
  public
    constructor Create(aStream: TStream; aEncoding: TEncoding = nil; aBufferSize: integer = cBufferSize); overload;
    constructor Create(const aFileName: string; aEncoding: TEncoding = nil; aBufferSize: integer = cBufferSize); overload;
    destructor Destroy; override;

    // Note: this will encapsulate all values in quotes if it either find a separator cha or a quote char in the cell values
    procedure WriteRow(const Row: TRow); overload;
    // no encoding will be performed here
    procedure WriteRow(const Row: TRawRow); overload;
    // per default no bom is written, but you can do it yourself here. just do it before you write any values
    procedure WriteUtf8Bom;

    // flushes the internal buffer to the file stream. will be called automatically when the buffer exceeds the defined buffer size and on destroy
    procedure Flush;


    class procedure WriteAll(const aRows: TArray<TRow>;
      aStream: TStream; aEncoding: TEncoding = nil; aDelimiter: Char = ',';
      const aLineBreak: RawByteString = sLineBreak;
      aCloseRowWithDelimiter: Boolean = False); overload;

    class procedure WriteAll(const aRows: TList<TRow>;
      aStream: TStream; aEncoding: TEncoding = nil; aDelimiter: Char = ',';
      const aLineBreak: RawByteString = sLineBreak;
      aCloseRowWithDelimiter: Boolean = False); overload;

    class procedure WriteAllToFile(const aRows: TArray<TRow>;
      const aFilename: string; aEncoding: TEncoding = nil; aDelimiter: Char = ',';
      const aLineBreak: RawByteString = sLineBreak;
      aCloseRowWithDelimiter: Boolean = False); overload;

    class procedure WriteAllToFile(const aRows: TList<TRow>;
      const aFilename: string; aEncoding: TEncoding = nil; aDelimiter: Char = ',';
      const aLineBreak: RawByteString = sLineBreak;
      aCloseRowWithDelimiter: Boolean = False); overload;

    class function WriteAllToString(const aRows: TArray<TRow>;
      const aLineBreak: RawByteString = sLineBreak;
      aCloseRowWithDelimiter: Boolean = False): string; overload;

    class function WriteAllToString(const aRows: TList<TRow>;
      const aLineBreak: RawByteString = sLineBreak;
      aCloseRowWithDelimiter: Boolean = False): string; overload;

    property LineBreak: rawByteString read FLineBreak write SetLineBreak;
    property CloseRowWithDelimiter: boolean read FCloseRowWithDelimiter write SetCloseRowWithDelimiter;
  end;

  TMissingColumnHandling = (mcOnReadReturnEmptyString, mcOnWriteIgnore, mcNoErrorsOnReadWrite);

  TRowReaderWriter = class
  private
    fDic: TDictionary<string, integer>;
    FHeader: TRow;
    FColCount: integer;
    FData: TRow;
    FMissingColumnHandling: TMissingColumnHandling;

    function GetHeaderAsCommaSeparatedText: string;
    procedure SetColCount(const Value: integer);
    procedure SetData(const Value: TRow);
    procedure SetHeader(const Value: TRow);
    procedure SetHeaderAsCommaSeparatedText(const Value: string);
    function getValue(const aColName: string): string;
    procedure SetValue(const aColName, Value: string);
    procedure SetMissingColumnHandling(
      const Value: TMissingColumnHandling);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function IndexOf(const aColName: string): integer; inline;
    function TryGetByName(const aname: string; out aValue: string): boolean;
    // empties the data array
    procedure reset;

    // is used to set up the index
    property Header: TRow read FHeader write SetHeader;
    property HeaderAsCommaSeparatedText: string read GetHeaderAsCommaSeparatedText write SetHeaderAsCommaSeparatedText;
    property ColCount: integer read FColCount write SetColCount;
    // when setting, the length will be checked to be >= ColCount, if it is less, then it will be increased
    property Data: TRow read FData write SetData;
    // access the column values by their names
    property Values[const aColName: string]: string read getValue write SetValue; default;
    // how the class should behave when you access a missing column?
    property MissingColumnHandling: TMissingColumnHandling read FMissingColumnHandling write SetMissingColumnHandling;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  maxInMemoryFile,
  {$ENDIF}
  System.IOUtils, System.AnsiStrings, System.WideStrUtils, AutoFree;

{ TCSVBase }

constructor TCSVBase.Create;
begin
  inherited Create;
  FQuoteChar := '"';
  fDelimiter := ',';
end;

destructor TCSVBase.Destroy;
begin

  inherited;
end;

procedure TCSVBase.SetQuoteChar(const Value: AnsiChar);
begin
  FQuoteChar := Value;
end;

procedure TCSVBase.SetSeparator(const Value: AnsiChar);
begin
  fDelimiter := Value;
end;

{ TCsvWriter }

constructor TCsvWriter.Create(aStream: TStream; aEncoding: TEncoding = nil; aBufferSize: integer = cBufferSize);
begin
  inherited Create;
  self.FLineBreak := sLineBreak;
  fBuffer := TMemoryStream.Create;
  fStream := aStream;
  fOwnsStream := False;
  fEncoding := aEncoding;
  if fEncoding = nil then
    fEncoding := TEncoding.UTF8;
  fBufferSize := aBufferSize;
  fBuffer.Size := fBufferSize; // preallocate memory
  UpdateControlChars;
end;

destructor TCsvWriter.Destroy;
begin
  Flush;
  fBuffer.Free;
  if fOwnsStream then
    if assigned(fStream) then
      fStream.Free;

  inherited;
end;

procedure TCsvWriter.WriteCell(const Value: string);
var
  s: string;
  X: integer;
  RequireQuote: boolean;
  bytes: TBytes;
begin
  s := Value;
  RequireQuote := False;

  for X := 1 to length(s) do
    // optimization:
    // If charInSet(s[x], [FQuoteChar, fDelimiter, #10, #13]) Then
    if fControlChars[s[X]] then
    begin
      RequireQuote := True;
      break;
    end;

  if RequireQuote then
  begin
    var lQuoteChar := string(FQuoteChar); // prevent compuler warning: Implicit string cast from 'AnsiChar' to 'string'
    s := lQuoteChar +
      Stringreplace(s, lQuoteChar, lQuoteChar + lQuoteChar, [rfReplaceAll]) +
      lQuoteChar;
  end;

  bytes := fEncoding.getbytes(s);
  if length(bytes) <> 0 then
    fBuffer.WriteBuffer(bytes[0], length(bytes))
end;

procedure TCsvWriter.WriteRow(const Row: TRow);
var
  X: integer;
begin
  for X := 0 to length(Row) - 1 do
  begin
    if X <> 0 then
      fBuffer.Write(fDelimiter, 1);
    WriteCell(Row[X]);
  end;
  if CloseRowWithDelimiter then
    fBuffer.Write(fDelimiter, 1);
  fBuffer.Write(self.FLineBreak[1], length(self.FLineBreak));
  if fBuffer.Size > fBufferSize then
    Flush;
end;

constructor TCsvWriter.Create(const aFileName: string; aEncoding: TEncoding = nil; aBufferSize: integer = cBufferSize);
var
  stream: TStream;
begin
  {$IFNDEF MSWINDOWS}
  stream := TFileStream.Create(aFileName, fmCreate);
  {$ELSE}
  stream := TMMFStream.Create(aFileName, mmfCreate);
  {$ENDIF}
  Create(stream, aEncoding, aBufferSize);
  fOwnsStream := True;
end;

procedure TCsvWriter.WriteUtf8Bom;
var
  b: TBytes;
begin
  b := TEncoding.UTF8.GetPreamble;
  fStream.Write(b[0], length(b));
end;

procedure TCsvWriter.WriteRow(const Row: TRawRow);
var
  X: integer;
begin
  for X := 0 to length(Row) - 1 do
  begin
    if X <> 0 then
      fBuffer.Write(fDelimiter, 1);
    WriteCell(Row[X]);
  end;
  if CloseRowWithDelimiter then
    fBuffer.Write(fDelimiter, 1);
  fBuffer.Write(self.FLineBreak[1], length(self.FLineBreak));
  if fBuffer.Size > fBufferSize then
    Flush;
end;


class procedure TCsvWriter.WriteAll(const aRows: TArray<TRow>; aStream: TStream;
  aEncoding: TEncoding; aDelimiter: Char; const aLineBreak: RawByteString;
  aCloseRowWithDelimiter: Boolean);
var
  lWriter: TCsvWriter;
begin
  gc(lWriter, TCsvWriter.Create(aStream, aEncoding));
  lWriter.Delimiter := ansiChar(aDelimiter);
  lWriter.LineBreak := aLineBreak;
  lWriter.CloseRowWithDelimiter := aCloseRowWithDelimiter;

  for var lRow in aRows do
    lWriter.WriteRow(lRow);
end;

class procedure TCsvWriter.WriteAll(const aRows: TList<TRow>; aStream: TStream;
  aEncoding: TEncoding; aDelimiter: Char; const aLineBreak: RawByteString;
  aCloseRowWithDelimiter: Boolean);
var
  lWriter: TCsvWriter;
begin
  gc(lWriter, TCsvWriter.Create(aStream, aEncoding));
  lWriter.Delimiter := ansiChar(aDelimiter);
  lWriter.LineBreak := aLineBreak;
  lWriter.CloseRowWithDelimiter := aCloseRowWithDelimiter;

  for var lRow in aRows do
    lWriter.WriteRow(lRow);
end;

class procedure TCsvWriter.WriteAllToFile(const aRows: TArray<TRow>;
  const aFilename: string; aEncoding: TEncoding; aDelimiter: Char;
  const aLineBreak: RawByteString; aCloseRowWithDelimiter: Boolean);
var
  lStream: TFileStream;
begin
  gc(lStream, TFileStream.Create(aFilename, fmCreate));
  WriteAll(aRows, lStream, aEncoding, aDelimiter, aLineBreak,
    aCloseRowWithDelimiter);
end;

class procedure TCsvWriter.WriteAllToFile(const aRows: TList<TRow>;
  const aFilename: string; aEncoding: TEncoding; aDelimiter: Char;
  const aLineBreak: RawByteString; aCloseRowWithDelimiter: Boolean);
var
  lStream: TFileStream;
begin
  gc(lStream, TFileStream.Create(aFilename, fmCreate));
  WriteAll(aRows, lStream, aEncoding, aDelimiter, aLineBreak,
    aCloseRowWithDelimiter);
end;

class function TCsvWriter.WriteAllToString(const aRows: TArray<TRow>;
  aDelimiter: Char; const aLineBreak: RawByteString;
  aCloseRowWithDelimiter: Boolean): string;
var
  lStream: TStringStream;
begin
  gc(lStream, TStringStream.Create('', TEncoding.UTF8));
  WriteAll(aRows, lStream, TEncoding.UTF8, aDelimiter, aLineBreak,
    aCloseRowWithDelimiter);
  Result := lStream.DataString;
end;

class function TCsvWriter.WriteAllToString(const aRows: TList<TRow>;
  aDelimiter: Char; const aLineBreak: RawByteString;
  aCloseRowWithDelimiter: Boolean): string;
var
  lStream: TStringStream;
begin
  gc(lStream, TStringStream.Create('', TEncoding.UTF8));
  WriteAll(aRows, lStream, TEncoding.UTF8, aDelimiter, aLineBreak,
    aCloseRowWithDelimiter);
  Result := lStream.DataString;
end;

procedure TCsvWriter.Flush;
begin
  if fBuffer.Position <> 0 then
  begin
    fStream.Write(fBuffer.Memory^, fBuffer.Position);
    fBuffer.Position := 0;
  end;
end;

procedure TCsvWriter.SetCloseRowWithDelimiter(const Value: boolean);
begin
  FCloseRowWithDelimiter := Value;
end;

procedure TCsvWriter.SetLineBreak(const Value: rawByteString);
begin
  FLineBreak := Value;
  UpdateControlChars;
end;

procedure TCsvWriter.SetQuoteChar(const Value: AnsiChar);
begin
  inherited;
  UpdateControlChars;
end;

procedure TCsvWriter.SetSeparator(const Value: AnsiChar);
begin
  inherited;
  UpdateControlChars;
end;

procedure TCsvWriter.UpdateControlChars;
var
  c: char;
  lChar: AnsiChar;
begin
  fillchar(fControlChars[#0], SizeOf(fControlChars), 0);
  for lChar in [FQuoteChar, fDelimiter, #10, #13] do
  begin
    c := wideChar(Ord(lChar));
    fControlChars[c] := True;
  end;

end;

procedure TCsvWriter.WriteCell(const Value: rawByteString);
var
  s: rawByteString;
  RequireQuote: boolean;
  X: integer;
begin
  s := Value;
  RequireQuote := False;

  for X := 1 to length(s) do
    if s[X] in [FQuoteChar, fDelimiter, #10, #13] then
    begin
      RequireQuote := True;
      break;
    end;

  if RequireQuote then
    s := self.FQuoteChar +
      System.AnsiStrings.Stringreplace(s, FQuoteChar, FQuoteChar + FQuoteChar, [rfReplaceAll]) +
      self.FQuoteChar;

  fBuffer.WriteBuffer(s[1], length(s));
end;

{ TCsvReader }

procedure TCsvReader.Close;
begin
  fCsvFileSize := 0;
  if assigned(fBuffer) then
  begin
    fBuffer.Free;
    fBuffer := nil;
  end;

  fUtf8BomDetected := False;

  fMaxColCountFound := 0;
end;

constructor TCsvReader.Create;
begin
  inherited Create;

end;

class constructor TCsvReader.CreateClass;
var
  c: AnsiChar;
begin
  // this should be a bit faster then using charInSet
  fillchar(fPotentialDelimiters[#0], SizeOf(fPotentialDelimiters), 0);
  for c in [',', ';', '|', #9] do
    fPotentialDelimiters[c] := True;
end;

destructor TCsvReader.Destroy;
begin
  Close;
  inherited;
end;

function TCsvReader.GetEof: boolean;
begin
  Result := fBuffer.EOF;
end;

procedure TCsvReader.Open(const FileName: string; aUseDelimiter: char);
begin
  Open(FileName, fmShareDenyWrite, aUseDelimiter);
end;

procedure TCsvReader.Open(const FileName: string; aShareMode: Cardinal = fmShareDenyWrite; aUseDelimiter: char = #0);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead, aShareMode);
  Open(fs, True, aUseDelimiter);
end;

procedure TCsvReader.Open(aStream: TStream; aTakeOwnerShipOfStream: boolean = False; aUseDelimiter: char = #0);
begin
  Close;
  fCsvFileSize := aStream.Size;
  fBuffer := TBufferedFile.Create(cBufferSize);
  fBuffer.Open(aStream, aTakeOwnerShipOfStream);
  DetectBom;
  if aUseDelimiter = #0 then
    fDelimiter := DetectDelimiter
  else
    fDelimiter := AnsiChar(aUseDelimiter);
end;

function TCsvReader.ReadRow(var Row: TRawRow): boolean;
var
  ColIndex: integer;
  colCapacity: integer;
  X: integer;
  LineEndDetected: boolean;
begin
  if EOF then
    exit(False);

  LineEndDetected := False;

  if fMaxColCountFound <> 0 then
    colCapacity := fMaxColCountFound
  else
    colCapacity := 255;
  if length(Row) < colCapacity then
    SetLength(Row, colCapacity);

  ColIndex := 0;
  while True do
  begin

    if ColIndex >= colCapacity then
    begin
      colCapacity := colCapacity * 2;
      SetLength(Row, colCapacity);
    end;
    if (fBuffer.CharCursor <> FQuoteChar) then
      ReadNonQuotedColValue(Row[ColIndex], LineEndDetected)
    else
      ReadQuotedColValue(Row[ColIndex], LineEndDetected);

    Inc(ColIndex);
    if LineEndDetected then
      break;
  end;

  if fMaxColCountFound < ColIndex then
    fMaxColCountFound := ColIndex;

  if fForcedColCount <> 0 then
    SetLength(Row, fForcedColCount)
  else if length(Row) <> ColIndex then
    SetLength(Row, ColIndex);

  // ensure no old date is present
  for X := ColIndex to length(Row) - 1 do
    Row[X] := '';

  Result := True;
end;

function TCsvReader.DetectDelimiter: AnsiChar;
var
  X: integer;
  orgPos: Int64;
  LineEndDetected: boolean;
  s: rawByteString;
begin
  Result := ',';
  LineEndDetected := False;

  if fBuffer.EOF then // do nothing if there is no data
    exit;

  orgPos := fBuffer.Position;

  if fBuffer.CharCursor = '"' then
  begin
    ReadQuotedColValue(s, LineEndDetected);
    // after reading, the cursor is moved to the next column start, so we need to go one back
    fBuffer.Seek(-1);
    // optimization:
    // If charInSet(fBuffer.CharCursor, [',', ';', '|', #9]) Then
    if fPotentialDelimiters[fBuffer.CharCursor] then
      Result := fBuffer.CharCursor;
  end else begin
    for X := 1 to 1024 do
    begin
      fBuffer.NextByte;
      // optimization:
      // If charInSet(fBuffer.CharCursor, [',', ';', '|', #9]) Then
      if fPotentialDelimiters[fBuffer.CharCursor] then
      begin
        Result := fBuffer.CharCursor;
        break;
      end;
    end;
  end;

  // reset the position
  fBuffer.Position := orgPos;
end;

procedure TCsvReader.DetectBom;
var
  b1, b2: TBytes;
begin
  b1 := TEncoding.UTF8.GetPreamble;
  b2 := fBuffer.copyBytes(0, length(b1));

  if (length(b1) = length(b2))
    and CompareMem(@b1[0], @b2[0], length(b1)) then
  begin
    fUtf8BomDetected := True;
    fBuffer.Position := length(b1);
  end
  else
    fUtf8BomDetected := False;
end;

function TCsvReader.ReadRow(var Row: TRow): boolean;
var
  rawRow: TRawRow;
begin
  Result := ReadRow(rawRow);
  if not Result then
    Row := []
  else
    Row := rawRow.ToUnicode;
end;

class function TCsvReader.ReadAllToList(const aFileName: string; aUseDelimiter: char): TList<TRow>;
var
  lReader: TCsvReader;
  lRow: TRow;
begin
  Result := TList<TRow>.Create;
  gc(lReader, TCsvReader.Create);
  lReader.Open(aFileName, aUseDelimiter);
  while lReader.ReadRow(lRow) do
    Result.Add(lRow);
end;

class function TCsvReader.ReadAllToArray(const aFileName: string; aUseDelimiter: char): TArray<TRow>;
var
  lList: TList<TRow>;
begin
  lList := ReadAllToList(aFileName, aUseDelimiter);
  try
    Result := lList.ToArray;
  finally
    lList.Free;
  end;
end;

procedure TCsvReader.ReadNonQuotedColValue(var aValue: rawByteString; var LineEndDetected: boolean);
var
  start, Count: Int64;
begin
  start := fBuffer.Position;
  while not fBuffer.EOF do
  begin
    if fBuffer.CharCursor = fDelimiter then
    begin
      Count := (fBuffer.Position - start);
      aValue := fBuffer.CopyRawByteString(start, Count);
      fBuffer.NextByte; // move to the start of the next value
      exit;

    end else if fBuffer.Cursor^ in [10, 13] then
    begin
      LineEndDetected := True;

      Count := (fBuffer.Position - start);
      aValue := fBuffer.CopyRawByteString(start, Count);

      if fBuffer.Cursor^ = 10 then
        fBuffer.NextByte // move to the start of the next value
      else if fBuffer.Cursor^ = 13 then // skip windows line break
        fBuffer.Seek(2);

      exit;

    end
    else
      fBuffer.NextByte;

  end;

  // if we are here, nothing was found, so retrive all
  Count := (fBuffer.Position - start);
  aValue := fBuffer.CopyRawByteString(start, Count);
  LineEndDetected := True;
end;

procedure TCsvReader.ReadQuotedColValue(var aValue: rawByteString; var LineEndDetected: boolean);
var
  start, Count: Int64;
begin
  fBuffer.NextByte; // move beyond the first quote
  start := fBuffer.Position;
  aValue := '';

  while not fBuffer.EOF do
  begin
    if fBuffer.CharCursor = FQuoteChar then
    begin
      Count := (fBuffer.Position - start);
      aValue := aValue + fBuffer.CopyRawByteString(start, Count); // copy all until before the quote

      fBuffer.NextByte; // move either to the delimiter after the quote, an quote that makrs quote escape, or to a line break character
      // was this quote escaped?
      if not fBuffer.EOF and (fBuffer.CharCursor = FQuoteChar) then
      begin
        // so this quote is escaped...
        aValue := aValue + FQuoteChar;
        fBuffer.NextByte;
        start := fBuffer.Position; // next time we will copy from here
        Continue;
      end;
      // right now the cursor points just after the quote char that closes the column value

      if fBuffer.EOF then
      begin
        LineEndDetected := True;
        exit;
      end;

      if (not fBuffer.EOF) and (fBuffer.Cursor^ in [10, 13]) then
      begin
        LineEndDetected := True;
        if fBuffer.Cursor^ = 10 then
          fBuffer.NextByte // move to the start of the next value
        else if fBuffer.Cursor^ = 13 then // skip windows line break
          fBuffer.Seek(2);
      end
      else
        fBuffer.NextByte; // move to the start of the next value

      exit;

    end
    else
      fBuffer.NextByte;

  end;

  // if we are here, nothing was found, so retrive all
  Count := (fBuffer.Position - start) + 1;
  aValue := aValue + fBuffer.CopyRawByteString(start, Count);
  LineEndDetected := True;
end;

{ TRawRowHelper }

function TRawRowHelper.ToUnicode: TRow;
var
  X: integer;
  encoding: TEncodeType;
begin
  SetLength(Result, length(self));
  for X := 0 to length(self) - 1 do
  begin
    encoding := System.WideStrUtils.DetectUTF8Encoding(self[X]);
    case encoding of
      etANSI,
        etUSAscii:
        Result[X] := string(self[X]);
      etUTF8:
        Result[X] := Utf8ToString(self[X]);

    end;
  end;
end;

{ TRowReaderWriter }

procedure TRowReaderWriter.Clear;
begin
  fDic.Clear;
  FHeader := [];
  FColCount := 0;
  FData := [];
end;

constructor TRowReaderWriter.Create;
begin
  inherited Create;
  fDic := TDictionary<string, integer>.Create;
end;

destructor TRowReaderWriter.Destroy;
begin
  fDic.Free;
  inherited;
end;

function TRowReaderWriter.GetHeaderAsCommaSeparatedText: string;
var
  l: TStringList;
  s: string;
begin
  gc(l, TStringList.Create);
  l.StrictDelimiter := True;
  for s in FHeader do
    l.Add(s);
  Result := l.Commatext;
end;

function TRowReaderWriter.getValue(const aColName: string): string;
var
  i: integer;
begin
  i := IndexOf(aColName);
  if (i <> -1) then
    Result := FData[i]
  else begin
    Result := '';
    if not (FMissingColumnHandling in [mcOnReadReturnEmptyString, mcNoErrorsOnReadWrite]) then
      raise Exception.Create('ERROR: column "' + aColName + '" not found');
  end;
end;

function TRowReaderWriter.IndexOf(const aColName: string): integer;
begin
  if not fDic.TryGetValue(aColName, Result) then
    Result := -1;
end;

procedure TRowReaderWriter.reset;
var
  ar: TArray<string>;
begin
  SetLength(ar, FColCount);
  self.FData := ar;
end;

procedure TRowReaderWriter.SetColCount(const Value: integer);
begin
  FColCount := Value;
  if length(FData) < FColCount then
    SetLength(FData, FColCount);
  if length(FHeader) < FColCount then
    SetLength(FHeader, FColCount);
end;

procedure TRowReaderWriter.SetData(const Value: TRow);
begin
  FData := Value;
  if (FColCount <> 0) and (length(FData) < FColCount) then
    SetLength(FData, FColCount);

end;

procedure TRowReaderWriter.SetHeader(const Value: TRow);
var
  X: integer;
begin
  FHeader := Value;
  ColCount := length(FHeader);
  fDic.Clear;
  for X := 0 to length(FHeader) - 1 do
    fDic.Add(FHeader[X], X);
end;

procedure TRowReaderWriter.SetHeaderAsCommaSeparatedText(
  const Value: string);
var
  l: TStringList;
  ar: TArray<string>;
  X: integer;
begin
  gc(l, TStringList.Create);
  l.StrictDelimiter := True;
  l.Commatext := Value;
  SetLength(ar, l.Count);
  for X := 0 to l.Count - 1 do
    ar[X] := l[X];
  self.Header := ar;
end;

procedure TRowReaderWriter.SetMissingColumnHandling(
  const Value: TMissingColumnHandling);
begin
  FMissingColumnHandling := Value;
end;

procedure TRowReaderWriter.SetValue(const aColName, Value: string);
var
  i: integer;
begin
  i := IndexOf(aColName);
  if (i <> -1) then
    FData[i] := Value
  else begin
    if not (FMissingColumnHandling in [mcOnWriteIgnore, mcNoErrorsOnReadWrite]) then
      raise Exception.Create('ERROR: column "' + aColName + '" not found');
  end;
end;

function TRowReaderWriter.TryGetByName(const aname: string;
  out aValue: string): boolean;
var
  i: integer;
begin
  Result := False;
  if fDic.TryGetValue(aname, i) then
  begin
    Result := True;
    aValue := FData[i];
  end;
end;

end.

