Unit maxCsv;

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

Interface

Uses
  MaxLogic.BufferedFile,
  classes, sysUtils, RTTI, generics.collections;

Type
  // forward declarations
  TCSVBase = Class;
  TCsvReader = Class;
  TCsvWriter = Class;
  TRow = TArray<String>;
  TRawRow = TArray<rawByteString>;

  TRawRowHelper = Record Helper For TRawRow
  Public
    Function ToUnicode: TRow; inline;
  End;

  TCSVBase = Class
  protected Const
    CR = #13 + #10;
  protected
    fDelimiter: ansichar;
    FQuoteChar: ansichar;
    Procedure SetQuoteChar(Const Value: ansichar); virtual;
    Procedure SetSeparator(Const Value: ansichar); virtual;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Property QuoteChar: ansichar Read FQuoteChar Write SetQuoteChar;
    Property Delimiter: ansichar Read fDelimiter Write SetSeparator;
  End;

  TCsvReader = Class(TCSVBase)
  Private Const
    cBufferSize = 1024 * 1024;
  private
    class var fPotentialDelimiters:array[ansiChar] of Boolean;
    class constructor CreateClass;
  Private
    fBuffer: TBufferedFile;
    fUtf8BomDetected: Boolean;

    fMaxColCountFound: Integer;
    fForcedColCount: Integer;
    fCsvFileSize: int64;


    Function GetEof: Boolean; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    Procedure DetectBom;

    // those methods are split for performance reasons. mostly quotes do not need to be checked, and we know when to check when the first char of a column is a quote char
    Procedure ReadNonQuotedColValue(Var aValue: rawByteString; Var LineEndDetected: Boolean); {$IFDEF USE_INLINE}Inline; {$ENDIF}
    Procedure ReadQuotedColValue(Var aValue: rawByteString; Var LineEndDetected: Boolean); {$IFDEF USE_INLINE}Inline; {$ENDIF}
  Public
    Constructor Create;
    Destructor Destroy; Override;

    // this also calls detect delimiter if aUseDelimiter = #0. ATTENTION: the Delimiter will be converted to a ansiChar
    Procedure Open(Const Filename: String; aShareMode: Cardinal = fmShareDenyWrite; aUseDelimiter: Char = #0); Overload;
    Procedure Open(Const Filename: String;aUseDelimiter: Char); Overload;
    Procedure Open(aStream: TStream; aTakeOwnerShipOfStream: Boolean = False; aUseDelimiter: Char = #0); Overload;

    Procedure Close;

    // it tries to find the proper delimiter
    // valid characters are: ,;| and #9 (tab)
    // the first found valid character is returned
    // only max 1024 bytes are checked
    // quoted columns are supported
    // default is comma ","
    Function DetectDelimiter: ansichar;

    Function ReadRow(Var row: TRawRow): Boolean; Overload; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    // this will try to convert the rawByteStrings to unicode
    Function ReadRow(Var row: TRow): Boolean; Overload; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    Property Eof: Boolean Read GetEof;
    Property Utf8BomDetected: Boolean Read fUtf8BomDetected;
    Property MaxColCountFound: Integer Read fMaxColCountFound;
    // you can force the CSV reader to always output that amount of columns
    Property ForcedColCount: Integer Read fForcedColCount Write fForcedColCount;

    // some info about the underlying file
    property CsvFileSize : int64 read fCsvFileSize ;
  End;

  TCsvWriter = Class(TCSVBase)
  Private Const
    cBufferSize = 1024 * 1024;
  Private
    fBufferSize: Integer;
    fStream: TStream;
    fOwnsStream: Boolean;
    fBuffer: TMemoryStream;
    fEncoding: TEncoding;
    FLineBreak: rawByteString;
    FCloseRowWithDelimiter: Boolean;
    fControlChars:array[Char] of Boolean;

    Procedure WriteCell(Const Value: String); Overload;
    Procedure WriteCell(Const Value: rawByteString); Overload;
    procedure SetLineBreak(const Value: rawByteString);
    procedure SetCloseRowWithDelimiter(const Value: Boolean);
    procedure UpdateControlChars;
    procedure SetQuoteChar(Const Value: ansichar); override;
    Procedure SetSeparator(Const Value: ansichar); override;
  Public
    Constructor Create(aStream: TStream; aEncoding: TEncoding = Nil; aBufferSize: Integer = cBufferSize); Overload;
    Constructor Create(Const aFilename: String; aEncoding: TEncoding = Nil; aBufferSize: Integer = cBufferSize); Overload;
    Destructor Destroy; Override;

    // Note: this will encapsulate all values in quotes if it either find a separator cha or a quote char in the cell values
    Procedure WriteRow(Const row: TRow); Overload;
    // no encoding will be performed here
    Procedure WriteRow(Const row: TRawRow); Overload;
    // per default no bom is written, but you can do it yourself here. just do it before you write any values
    Procedure WriteUtf8Bom;

    // flushes the internal buffer to the file stream. will be called automatically when the buffer exceeds the defined buffer size and on destroy
    Procedure Flush;

    property LineBreak: rawByteString read FLineBreak write SetLineBreak;
    property CloseRowWithDelimiter: Boolean read FCloseRowWithDelimiter write SetCloseRowWithDelimiter;
  End;

  TMissingColumnHandling = (mcOnReadReturnEmptyString, mcOnWriteIgnore, mcNoErrorsOnReadWrite);

  TRowReaderWriter = class
  private
    fDic: TDictionary<String, Integer>;
    FHeader: TRow;
    FColCount: Integer;
    FData: TRow;
    FMissingColumnHandling: TMissingColumnHandling;

    function GetHeaderAsCommaSeparatedText: String;
    procedure SetColCount(const Value: Integer);
    procedure SetData(const Value: TRow);
    procedure SetHeader(const Value: TRow);
    procedure SetHeaderAsCommaSeparatedText(const Value: String);
    function GetValue(const aColName: String): String;
    procedure SetValue(const aColName, Value: String);
    procedure SetMissingColumnHandling(
      const Value: TMissingColumnHandling);
  public
    Constructor Create;
    Destructor Destroy; override;

    Function IndexOf(const aColName: String): Integer; inline;
    function TryGetByName(const aname: String; out aValue: String): Boolean;
    // empties the data array
    Procedure Reset;

    // is used to set up the index
    property Header: TRow read FHeader write SetHeader;
    Property HeaderAsCommaSeparatedText: String read GetHeaderAsCommaSeparatedText write SetHeaderAsCommaSeparatedText;
    Property ColCount: Integer read FColCount write SetColCount;
    // when setting, the length will be checked to be >= ColCount, if it is less, then it will be increased
    Property Data: TRow read FData write SetData;
    // access the column values by their names
    Property Values[const aColName: String]: String read GetValue write SetValue; default;
    // how the class should behave when you access a missing column?
    Property MissingColumnHandling:TMissingColumnHandling read FMissingColumnHandling write SetMissingColumnHandling;
  end;

Implementation

Uses
  {$IFDEF MSWINDOWS}
  maxInMemoryFile,
  {$ENDIF}
  ioUtils, ansiStrings, System.WideStrUtils, autoFree;

{ TCSVBase }

Constructor TCSVBase.Create;
Begin
  Inherited Create;
  FQuoteChar := '"';
  fDelimiter := ',';
End;

Destructor TCSVBase.Destroy;
Begin

  Inherited;
End;

Procedure TCSVBase.SetQuoteChar(Const Value: ansichar);
Begin
  FQuoteChar := Value;
End;

Procedure TCSVBase.SetSeparator(Const Value: ansichar);
Begin
  fDelimiter := Value;
End;

{ TCsvWriter }

Constructor TCsvWriter.Create(aStream: TStream; aEncoding: TEncoding = Nil; aBufferSize: Integer = cBufferSize);
Begin
  Inherited Create;
  self.FLineBreak := sLineBreak;
  fBuffer := TMemoryStream.Create;
  fStream := aStream;
  fOwnsStream := False;
  fEncoding := aEncoding;
  If fEncoding = Nil Then
    fEncoding := TEncoding.UTF8;
  fBufferSize := aBufferSize;
  fBuffer.Size := fBufferSize; // preallocate memory
  UpdateControlChars;
End;

Destructor TCsvWriter.Destroy;
Begin
  Flush;
  fBuffer.Free;
  If fOwnsStream Then
    If assigned(fStream) Then
      fStream.Free;

  Inherited;
End;

Procedure TCsvWriter.WriteCell(Const Value: String);
Var
  s: String;
  x: Integer;
  RequireQuote: Boolean;
  bytes: TBytes;
Begin
  s := Value;
  RequireQuote := False;

  For x := 1 To length(s) Do
    // optimization:
    // If charInSet(s[x], [FQuoteChar, fDelimiter, #10, #13]) Then
    if fControlChars[s[x]] then
    Begin
      RequireQuote := True;
      Break;
    End;

  If RequireQuote Then
  begin
    var lQuoteChar:= string(FQuoteChar); // prevent compuler warning: Implicit string cast from 'AnsiChar' to 'string'
    s := lQuoteChar +
      StringReplace(s, lQuoteChar, lQuoteChar + lQuoteChar, [rfReplaceAll]) +
      lQuoteChar;
  end;

  bytes := fEncoding.getbytes(s);
  If length(bytes) <> 0 Then
    fBuffer.WriteBuffer(bytes[0], length(bytes))
End;

Procedure TCsvWriter.WriteRow(Const row: TRow);
Var
  x: Integer;
Begin
  For x := 0 To length(row) - 1 Do
  begin
    if x <> 0 then
      fBuffer.Write(fDelimiter, 1);
    WriteCell(row[x]);
  end;
  if CloseRowWithDelimiter then
    fBuffer.Write(fDelimiter, 1);
  fBuffer.Write(self.FLineBreak[1], length(self.FLineBreak));
  If fBuffer.Size > fBufferSize Then
    Flush;
End;

Constructor TCsvWriter.Create(Const aFilename: String; aEncoding: TEncoding = Nil; aBufferSize: Integer = cBufferSize);
Var
  stream: TStream;
Begin
  {$IFNDEF MSWINDOWS}
  stream := TFileStream.Create(aFilename, fmCreate);
  {$ELSE}
  stream := TMMFStream.Create(aFilename, mmfCreate);
  {$ENDIF}
  Create(stream, aEncoding, aBufferSize);
  fOwnsStream := True;
End;

Procedure TCsvWriter.WriteUtf8Bom;
Var
  b: TBytes;
Begin
  b := TEncoding.UTF8.GetPreamble;
  fStream.Write(b[0], length(b));
End;

Procedure TCsvWriter.WriteRow(Const row: TRawRow);
Var
  x: Integer;
Begin
  For x := 0 To length(row) - 1 Do
  begin
    if x <> 0 then
      fBuffer.Write(fDelimiter, 1);
    WriteCell(row[x]);
  end;
  if CloseRowWithDelimiter then
    fBuffer.Write(fDelimiter, 1);
  fBuffer.Write(self.FLineBreak[1], length(self.FLineBreak));
  If fBuffer.Size > fBufferSize Then
    Flush;
End;

Procedure TCsvWriter.Flush;
Begin
  If fBuffer.Position <> 0 Then
  Begin
    fStream.Write(fBuffer.Memory^, fBuffer.Position);
    fBuffer.Position := 0;
  End;
End;

procedure TCsvWriter.SetCloseRowWithDelimiter(const Value: Boolean);
begin
  FCloseRowWithDelimiter := Value;
end;

procedure TCsvWriter.SetLineBreak(const Value: rawByteString);
begin
  FLineBreak := Value;
  UpdateControlChars;
end;

procedure TCsvWriter.SetQuoteChar(const Value: ansichar);
begin
  inherited;
  UpdateControlChars;
end;

procedure TCsvWriter.SetSeparator(const Value: ansichar);
begin
  inherited;
  UpdateControlChars;
end;

procedure TCsvWriter.UpdateControlChars;
var
  c: Char;
  lChar: ansiChar;
begin
  FillChar(fControlChars[#0], SizeOf(fControlChars), 0);
  for lChar  in [FQuoteChar, fDelimiter, #10, #13] do
  begin
    c:= wideChar(Ord(lChar));
    fControlChars[c]:= True;
  end;

end;

Procedure TCsvWriter.WriteCell(Const Value: rawByteString);
Var
  s: rawByteString;
  RequireQuote: Boolean;
  x: Integer;
Begin
  s := Value;
  RequireQuote := False;

  For x := 1 To length(s) Do
    If s[x] In [FQuoteChar, fDelimiter, #10, #13] Then
    Begin
      RequireQuote := True;
      Break;
    End;

  If RequireQuote Then
    s := self.FQuoteChar +
      ansiStrings.StringReplace(s, FQuoteChar, FQuoteChar + FQuoteChar, [rfReplaceAll]) +
      self.FQuoteChar;

  fBuffer.WriteBuffer(s[1], length(s));
End;

{ TCsvReader }

Procedure TCsvReader.Close;
Begin
  fCsvFileSize := 0;
  If assigned(fBuffer) Then
  Begin
    fBuffer.Free;
    fBuffer := Nil;
  End;

  fUtf8BomDetected := False;

  fMaxColCountFound := 0;
End;

Constructor TCsvReader.Create;
Begin
  Inherited Create;

End;

class constructor TCsvReader.CreateClass;
var
  c: ansiChar;
begin
  // this should be a bit faster then using charInSet
  FillChar(fPotentialDelimiters[#0], SizeOf(fPotentialDelimiters), 0);
  for c in [',', ';', '|', #9] do
    fPotentialDelimiters[c]:= True;
end;

Destructor TCsvReader.Destroy;
Begin
  Close;
  Inherited;
End;

Function TCsvReader.GetEof: Boolean;
Begin
  result := fBuffer.Eof;
End;

procedure TCsvReader.Open(const Filename: String; aUseDelimiter: Char);
begin
Open(Filename, fmShareDenyWrite, aUseDelimiter);
end;

Procedure TCsvReader.Open(Const Filename: String; aShareMode: Cardinal = fmShareDenyWrite; aUseDelimiter: Char = #0);
Var
  fs: TFileStream;
Begin
  fs := TFileStream.Create(Filename, fmOpenRead, aShareMode);
  Open(fs, True, aUseDelimiter);
End;

Procedure TCsvReader.Open(aStream: TStream; aTakeOwnerShipOfStream: Boolean = False; aUseDelimiter: Char = #0);
Begin
  Close;
  fCsvFileSize := aStream.Size;
  fBuffer := TBufferedFile.Create(cBufferSize);
  fBuffer.Open(aStream, aTakeOwnerShipOfStream);
  DetectBom;
  if aUseDelimiter = #0 then
    fDelimiter := DetectDelimiter
  else
    fDelimiter:= ansiChar( aUseDelimiter );
End;

Function TCsvReader.ReadRow(Var row: TRawRow): Boolean;
Var
  ColIndex: Integer;
  colCapacity: Integer;
  x: Integer;
  LineEndDetected: Boolean;
Begin
  If Eof Then
    exit(False);

  LineEndDetected := False;

  If fMaxColCountFound <> 0 Then
    colCapacity := fMaxColCountFound
  Else
    colCapacity := 255;
  If length(row) < colCapacity Then
    SetLength(row, colCapacity);

  ColIndex := 0;
  While True Do
  Begin

    If ColIndex >= colCapacity Then
    Begin
      colCapacity := colCapacity * 2;
      SetLength(row, colCapacity);
    End;
    If (fBuffer.CharCursor <> FQuoteChar) Then
      ReadNonQuotedColValue(row[ColIndex], LineEndDetected)
    Else
      ReadQuotedColValue(row[ColIndex], LineEndDetected);

    inc(ColIndex);
    If LineEndDetected Then
      Break;
  End;

  If fMaxColCountFound < ColIndex Then
    fMaxColCountFound := ColIndex;

  If fForcedColCount <> 0 Then
    SetLength(row, fForcedColCount)
  Else If length(row) <> ColIndex Then
    SetLength(row, ColIndex);

  // ensure no old date is present
  For x := ColIndex To length(row) - 1 Do
    row[x] := '';

  result := True;
End;

Function TCsvReader.DetectDelimiter: ansichar;
Var
  x: Integer;
  orgPos: Int64;
  LineEndDetected: Boolean;
  s: rawByteString;
Begin
  Result := ',';
  LineEndDetected := False;

  If fBuffer.Eof Then // do nothing if there is no data
    exit;

  orgPos := fBuffer.Position;

  If fBuffer.CharCursor = '"' Then
  Begin
    ReadQuotedColValue(s, LineEndDetected);
    // after reading, the cursor is moved to the next column start, so we need to go one back
    fBuffer.Seek(-1);
    // optimization:
    // If charInSet(fBuffer.CharCursor, [',', ';', '|', #9]) Then
    if fPotentialDelimiters[fBuffer.CharCursor] then
      result := fBuffer.CharCursor;
  End Else Begin
    For x := 1 To 1024 Do
    Begin
      fBuffer.NextByte;
      // optimization:
      // If charInSet(fBuffer.CharCursor, [',', ';', '|', #9]) Then
      if fPotentialDelimiters[fBuffer.CharCursor] then
      Begin
        result := fBuffer.CharCursor;
        Break;
      End;
    End;
  End;

  // reset the position
  fBuffer.Position := orgPos;
End;

Procedure TCsvReader.DetectBom;
Var
  b1, b2: TBytes;
Begin
  b1 := TEncoding.UTF8.GetPreamble;
  b2 := fBuffer.copyBytes(0, length(b1));

  If (length(b1) = length(b2))
    And CompareMem(@b1[0], @b2[0], length(b1)) Then
  Begin
    fUtf8BomDetected := True;
    fBuffer.Position := length(b1);
  End
  Else
    fUtf8BomDetected := False;
End;

Function TCsvReader.ReadRow(Var row: TRow): Boolean;
Var
  rawRow: TRawRow;
Begin
  result := ReadRow(rawRow);
  If Not result Then
    row := []
  Else
    row := rawRow.ToUnicode;
End;

Procedure TCsvReader.ReadNonQuotedColValue(Var aValue: rawByteString; Var LineEndDetected: Boolean);
Var
  start, count: Int64;
Begin
  start := fBuffer.Position;
  While Not fBuffer.Eof Do
  Begin
    If fBuffer.CharCursor = fDelimiter Then
    Begin
      count := (fBuffer.Position - start);
      aValue := fBuffer.CopyRawByteString(start, count);
      fBuffer.NextByte; // move to the start of the next value
      exit;

    End Else If fBuffer.Cursor^ In [10, 13] Then
    Begin
      LineEndDetected := True;

      count := (fBuffer.Position - start);
      aValue := fBuffer.CopyRawByteString(start, count);

      If fBuffer.Cursor^ = 10 Then
        fBuffer.NextByte // move to the start of the next value
      Else If fBuffer.Cursor^ = 13 Then // skip windows line break
        fBuffer.Seek(2);

      exit;

    End
    Else
      fBuffer.NextByte;

  End;

  // if we are here, nothing was found, so retrive all
  count := (fBuffer.Position - start);
  aValue := fBuffer.CopyRawByteString(start, count);
  LineEndDetected := True;
End;

Procedure TCsvReader.ReadQuotedColValue(Var aValue: rawByteString; Var LineEndDetected: Boolean);
Var
  start, count: Int64;
Begin
  fBuffer.NextByte; // move beyond the first quote
  start := fBuffer.Position;
  aValue := '';

  While Not fBuffer.Eof Do
  Begin
    If fBuffer.CharCursor = FQuoteChar Then
    Begin
      count := (fBuffer.Position - start);
      aValue := aValue + fBuffer.CopyRawByteString(start, count); // copy all until before the quote

      fBuffer.NextByte; // move either to the delimiter after the quote, an quote that makrs quote escape, or to a line break character
      // was this quote escaped?
      If Not fBuffer.Eof And (fBuffer.CharCursor = FQuoteChar) Then
      Begin
        // so this quote is escaped...
        aValue := aValue + FQuoteChar;
        fBuffer.NextByte;
        start := fBuffer.Position; // next time we will copy from here
        Continue;
      End;
      // right now the cursor points just after the quote char that closes the column value

      If fBuffer.Eof Then
      Begin
        LineEndDetected := True;
        exit;
      End;

      If (Not fBuffer.Eof) And (fBuffer.Cursor^ In [10, 13]) Then
      Begin
        LineEndDetected := True;
        If fBuffer.Cursor^ = 10 Then
          fBuffer.NextByte // move to the start of the next value
        Else If fBuffer.Cursor^ = 13 Then // skip windows line break
          fBuffer.Seek(2);
      End
      Else
        fBuffer.NextByte; // move to the start of the next value

      exit;

    End
    Else
      fBuffer.NextByte;

  End;

  // if we are here, nothing was found, so retrive all
  count := (fBuffer.Position - start) + 1;
  aValue := aValue + fBuffer.CopyRawByteString(start, count);
  LineEndDetected := True;
End;

{ TRawRowHelper }

Function TRawRowHelper.ToUnicode: TRow;
Var
  x: Integer;
  encoding: TEncodeType;
Begin
  SetLength(result, length(self));
  For x := 0 To length(self) - 1 Do
  Begin
    encoding := System.WideStrUtils.DetectUTF8Encoding(self[x]);
    Case encoding Of
      etANSI,
        etUSASCII:
        result[x] := String(self[x]);
      etUTF8:
        result[x] := Utf8ToString(self[x]);

    End;
  End;
End;

{ TRowReaderWriter }

constructor TRowReaderWriter.Create;
begin
  inherited Create;
  fDic:= TDictionary<String, Integer>.Create;
end;

destructor TRowReaderWriter.Destroy;
begin
  fDic.Free;
  inherited;
end;

function TRowReaderWriter.GetHeaderAsCommaSeparatedText: String;
var
  l:TStringList;
  s: String;
begin
  gc(l, TStringList.Create);
  l.StrictDelimiter:= True;
  for s in fHeader do
    l.Add(s);
  Result:= l.CommaText;
end;

function TRowReaderWriter.GetValue(const aColName: String): String;
var
  i: Integer;
begin
  i:= IndexOf(aColName);
  if (i<>-1) then
    Result:= fData[i]
  else begin
    Result:= '';
    if not(FMissingColumnHandling in [mcOnReadReturnEmptyString, mcNoErrorsOnReadWrite] ) then
      raise Exception.Create('ERROR: column "'+aColName+'" not found');
  end;
end;

function TRowReaderWriter.IndexOf(const aColName: String): Integer;
begin
  if not fDic.TryGetValue(aColName, Result) then
    Result:= -1;
end;

procedure TRowReaderWriter.Reset;
var
  ar: TArray<String>;
begin
  setLength(ar, FColCount);
  self.FData := ar;
end;

procedure TRowReaderWriter.SetColCount(const Value: Integer);
begin
  FColCount := Value;
  if Length(fData) < fColCount then
    SetLength(fData, fColCount);
  if length(fHeader) < fColCount then
    SetLength(fHeader, fColCount);
end;

procedure TRowReaderWriter.SetData(const Value: TRow);
begin
  FData := Value;
  if (fColCount <> 0) and (length(fData)<fColCount) then
    setLength(fData, fColCount);

end;

procedure TRowReaderWriter.SetHeader(const Value: TRow);
var
  x: Integer;
begin
  FHeader := Value;
  ColCount:= length(fHeader);
  fDic.Clear;
  for x := 0 to length(fHeader)-1 do
    fDic.Add(fHeader[x], x);
end;

procedure TRowReaderWriter.SetHeaderAsCommaSeparatedText(
  const Value: String);
var
  l: TStringList;
  ar: TArray<String>;
  x: Integer;
begin
  gc(l, TStringList.create);
  l.StrictDelimiter:= True;
  l.CommaText:= value;
  setLength(ar, l.Count);
  for x := 0 to l.Count-1 do
    ar[x]:= l[x];
  self.Header:= ar;
end;

procedure TRowReaderWriter.SetMissingColumnHandling(
  const Value: TMissingColumnHandling);
begin
  FMissingColumnHandling := Value;
end;

procedure TRowReaderWriter.SetValue(const aColName, Value: String);
var
  i: Integer;
begin
  i:= IndexOf(aColName);
  if (i<>-1) then
    fData[i]:= Value
  else begin
    if not(FMissingColumnHandling in [mcOnWriteIgnore, mcNoErrorsOnReadWrite] ) then
      raise Exception.Create('ERROR: column "'+aColName+'" not found');
  end;
end;

function TRowReaderWriter.TryGetByName(const aname: String;
  out aValue: String): Boolean;
var
  i: Integer;
begin
  Result:= False;
  if fDic.TryGetValue(aName, i) then
  begin
    Result:= True;
    aValue:= fData[i];
  end;
end;

End.
