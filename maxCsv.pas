Unit maxCsv;

{ Copyright: pawel Piotrowski
  Version: 1.0
  History:
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
    Function ToUnicode: TRow;
  End;

  TCSVBase = Class
  Private Const
    CR = #13 + #10;
  Private
    fDelimiter: Byte;
    FQuoteCharAsByte: Byte;
    Procedure SetQuoteChar(Const Value: ansichar);
    Procedure SetSeparator(Const Value: ansichar);
    function GetQuoteChar: ansichar;
    function GetDelimiter: ansichar;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Property QuoteChar: ansichar Read GetQuoteChar Write SetQuoteChar;
    Property Delimiter: ansichar Read GetDelimiter Write SetSeparator;
  End;

  TCsvReader = Class(TCSVBase)
  Private Const
    cBufferSize = 16 * 1024;
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

    // if aDelimiter = #0 then this will also call detect delimiter
    Procedure Open(Const Filename: String; aShareMode: Cardinal = fmShareDenyWrite; aDelimiter: ansichar = #0); Overload;
    Procedure Open(Const Filename: String; aDelimiter: char); Overload;
    Procedure Open(aStream: TStream; aTakeOwnerShipOfStream: Boolean = False; aDelimiter: ansichar = #0); Overload;

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

    // sometimes you want to know the file of the file you just opened
    Property CsvFileSize: int64 Read fCsvFileSize;
  End;

  TCsvWriter = Class(TCSVBase)
  Private Const
    cBufferSize = 16 * 1024;
  Private
    fBufferSize: Integer;
    fStream: TStream;
    fOwnsStream: Boolean;
    fBuffer: TMemoryStream;
    fEncoding: TEncoding;

    Procedure WriteCell(Const Value: String); Overload;
    Procedure WriteCell(Const Value: rawByteString); Overload;
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
  End;

Implementation

Uses
{$IFDEF MSWINDOWS}
  maxInMemoryFile,
{$ENDIF}
  ioUtils, ansiStrings, System.WideStrUtils;

{ TCSVBase }

Constructor TCSVBase.Create;
Begin
  Inherited Create;
  QuoteChar := '"';
  Delimiter := ',';
End;

Destructor TCSVBase.Destroy;
Begin

  Inherited;
End;

Procedure TCSVBase.SetQuoteChar(Const Value: ansichar);
Begin
  FQuoteCharAsByte:= byte(Value);
End;

Procedure TCSVBase.SetSeparator(Const Value: ansichar);
Begin
  fDelimiter := byte(Value);
End;

function TCSVBase.GetQuoteChar: ansichar;
begin
result:=AnsiChar(FQuoteCharAsByte);
end;

function TCSVBase.GetDelimiter: ansichar;
begin
result:=ansiChar(fDelimiter );
end;

{ TCsvWriter }

Constructor TCsvWriter.Create(aStream: TStream; aEncoding: TEncoding = Nil; aBufferSize: Integer = cBufferSize);
Begin
  Inherited Create;
  fBuffer := TMemoryStream.Create;
  fStream := aStream;
  fOwnsStream := False;
  fEncoding := aEncoding;
  If fEncoding = Nil Then
    fEncoding := TEncoding.UTF8;
  fBufferSize := aBufferSize;
  fBuffer.Size := fBufferSize; // preallocate memory
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
    If charInSet(s[x], [Char(FQuoteCharAsByte), char(fDelimiter), #10, #13]) Then
    Begin
      RequireQuote := True;
      Break;
    End;

  If RequireQuote Then
    s := char(self.FQuoteCharAsByte) +
      sysUtils.StringReplace(s, char(FQuoteCharAsByte), String(char(FQuoteCharAsByte)+ char(FQuoteCharAsByte)), [rfReplaceAll]) +
      char(self.FQuoteCharAsByte) +
      char(self.fDelimiter)
  Else
    s := s + char(self.fDelimiter);

  bytes := fEncoding.getbytes(s);
  If length(bytes) <> 0 Then
    fBuffer.WriteBuffer(bytes[0], length(bytes))
End;

Procedure TCsvWriter.WriteRow(Const row: TRow);
Var
  x: Integer;
Begin
  For x := 0 To length(row) - 1 Do
    WriteCell(row[x]);
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
  fStream.write(b[0], length(b));
End;

Procedure TCsvWriter.WriteRow(Const row: TRawRow);
Var
  x: Integer;
Begin
  For x := 0 To length(row) - 1 Do
    WriteCell(row[x]);
  If fBuffer.Size > fBufferSize Then
    Flush;
End;

Procedure TCsvWriter.Flush;
Begin
  If fBuffer.Position <> 0 Then
  Begin
    fStream.write(fBuffer.Memory^, fBuffer.Position);
    fBuffer.Position := 0;
  End;
End;

Procedure TCsvWriter.WriteCell(Const Value: rawByteString);
Var
  raw: rawByteString;
  RequireQuote: Boolean;
  x: Integer;
Begin
  raw := Value;
  RequireQuote := False;

  For x := 1 To length(raw) Do
    If raw[x] In [char(FQuoteCharAsByte), Delimiter, #10, #13] Then
    Begin
      RequireQuote := True;
      Break;
    End;

  If RequireQuote Then
    raw := self.QuoteChar+
      ansiStrings.StringReplace(raw, QuoteChar, QuoteChar + QuoteChar , [rfReplaceAll]) +
      self.QuoteChar +
      self.Delimiter
  Else
    raw := raw + self.Delimiter;

  fBuffer.WriteBuffer(raw[1], length(raw));
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

Destructor TCsvReader.Destroy;
Begin
  Close;
  Inherited;
End;

Function TCsvReader.GetEof: Boolean;
Begin
  result := fBuffer.Eof;
End;

Procedure TCsvReader.Open(Const Filename: String; aShareMode: Cardinal = fmShareDenyWrite; aDelimiter: ansichar = #0);
Var
  fs: TFileStream;
Begin
  fs := TFileStream.Create(Filename, fmOpenRead, aShareMode);
  Open(fs, True, aDelimiter);
End;

Procedure TCsvReader.Open(aStream: TStream; aTakeOwnerShipOfStream: Boolean = False; aDelimiter: ansichar = #0);
Const
KB= 1024;
MB = 1024*KB;
Begin
  Close;
  fCsvFileSize := aStream.Size;
  fBuffer := TBufferedFile.Create(16*KB, MB, 16*KB, MB);
  fBuffer.Open(aStream, aTakeOwnerShipOfStream);
  DetectBom;
  If aDelimiter = #0 Then
    Delimiter := DetectDelimiter
  Else
    Delimiter := aDelimiter;
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

    fBuffer.TrimBuffer;

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
    If (fBuffer.CursorByte<> FQuoteCharAsByte) Then
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
  LineEndDetected: Boolean;
  s: rawByteString;
  lBuffer:RawByteString;
  lBufferSize:Integer;
  pc:PAnsiChar;
Begin
  result := ',';
  LineEndDetected := False;

  lBufferSize:=1024;
  if lBufferSize> fBuffer.BytesRightOfCursor then
  lBufferSize:=fBuffer.BytesRightOfCursor;


  if lBufferSize=0 then
    exit;

  setLength(lbuffer,lBufferSize+1);
  move(fBuffer.CursorPByte^, lBuffer[1], lBufferSize);
  lBuffer[lBufferSize+1] :=#0;
  pc:=@lBuffer[1];


                  // case 1: we have a quoted value, in this case find the closing ", the char after that is our delimiter... or end of buffer
  if pc^='"' then
  begin
    var QuotaOpen:= True;
    repeat
    inc(pc);
        if pc^ ='"' Then
        QuotaOpen:=not QuotaOpen;
    until (pc^=#0)or((not QuotaOpen ) and (pc^<>'"'));

    if pc^<>#0 then
    result:=pc^;


  end else
    while pc^<>#0 do
    Begin
      If pc^ in [',', ';', '|', #9] Then
      Begin
        result := pc^;
        Break;
      End;

      inc(pc);
    End;
End;

Procedure TCsvReader.DetectBom;
Var
  b1, b2: TBytes;
  x, len:Integer;
Begin
fUtf8BomDetected := False;

  b1 := TEncoding.UTF8.GetPreamble;
  len:=length(b1);
  SetLength(b2, len);
  if fBuffer.BytesRightOfCursor<len then
  exit; // nothing to detect...

move(fBuffer.Buffer[0], b2[0], len);

  If (length(b1) = length(b2))
    And CompareMem(@b1[0], @b2[0], length(b1)) Then
  Begin
    fUtf8BomDetected := True;
    // skip bom
    for x := 1 to len do
      fBuffer.Next;
  End;


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
  StartPos, count: integer;
  b:Byte;
Begin
StartPos:=fBuffer.Cursor;

  While Not fBuffer.Eof Do
  Begin
  b:=fBuffer.CursorByte;
    If b= fDelimiter Then
    Begin

fBuffer.CopyRawByteString(startPos, (fBuffer.Cursor-StartPos), aValue );
      fBuffer.Next; // move to the start of the next value
      exit;

    End Else If (b In [10, 13]) Then
    Begin
      LineEndDetected := True;
      fBuffer.CopyRawByteString(startPos, (fBuffer.Cursor-StartPos), aValue );


      If b= 10 Then
        fBuffer.Next // move to the start of the next value
      Else If b = 13 Then // skip windows line break
begin
  fbuffer.next; fbuffer.next;
end;

      exit; // line detected nothing else to od

    End Else
      fBuffer.Next;
  End;

  // if we are here, nothing was found, so retrive all
  count := (fBuffer.Cursor- StartPos);
fBuffer.CopyRawByteString(startPos, count, aValue );
  LineEndDetected := True;
End;

Procedure TCsvReader.ReadQuotedColValue(Var aValue: rawByteString; Var LineEndDetected: Boolean);
Var
  startPos, count: integer;
b:Byte;
v:RawByteString;
Begin
  fBuffer.Next; // move beyond the first quote
  startPos := fBuffer.Cursor;
  aValue := '';


  While Not fBuffer.Eof Do
  Begin
  b:= fBuffer.CursorByte;


    If b= FQuoteCharAsByte Then
    Begin
      fBuffer.CopyRawByteString(startPos, (fBuffer.Cursor-StartPos), v);
      aValue := aValue + v; // copy all until before the quote

      fBuffer.Next; // move either to the delimiter after the quote, an quote that makrs quote escape, or to a line break character
      // was this quote escaped?
      If Not fBuffer.Eof And (fBuffer.CursorByte= FQuoteCharAsByte) Then
      Begin
        // so this quote is escaped...
        aValue := aValue + ansiChar(FQuoteCharAsByte);
        fBuffer.Next;
        startPos := fBuffer.Cursor; // next time we will copy from here
        Continue;
      End;
      // right now the cursor points just after the quote char that closes the column value

      If fBuffer.Eof Then
      Begin
        LineEndDetected := True;
        exit;
      End;


      b:=fBuffer.CursorByte;
      If (Not fBuffer.Eof) And (b In [10, 13]) Then
      Begin
        LineEndDetected := True;
        If b = 10 Then
          fBuffer.Next // move to the start of the next value
        Else If b = 13 Then // skip windows line break
begin
  fBuffer.Next; fBuffer.Next;
end;
      End
      Else
        fBuffer.Next; // move to the start of the next value

      exit; // we are now either on the next cell value start or at the next line start. in both cases, let us exit
    End
    Else
      fBuffer.Next;

  End;


  // if we are here, nothing was found, so retrive all
  count := (fBuffer.Cursor- StartPos);
fBuffer.CopyRawByteString(startPos, count, v);
aValue:=aValue + v;
  LineEndDetected := True;
End;

Procedure TCsvReader.Open(Const Filename: String; aDelimiter: char);
Begin
  Open(Filename, fmShareDenyWrite,
    ansichar(aDelimiter));
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

End.
