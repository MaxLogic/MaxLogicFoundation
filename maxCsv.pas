Unit maxCsv;

{ Copyright: pawel Piotrowski
  Version: 1.1
  History:
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
    Function ToUnicode: TRow;
  End;

  TCSVBase = Class
  Private Const
    CR = #13 + #10;
  Private
    fDelimiter: ansichar;
    FQuoteChar: ansichar;
    Procedure SetQuoteChar(Const Value: ansichar);
    Procedure SetSeparator(Const Value: ansichar);
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Property QuoteChar: ansichar Read FQuoteChar Write SetQuoteChar;
    Property Delimiter: ansichar Read fDelimiter Write SetSeparator;
  End;

  TCsvReader = Class(TCSVBase)
  Private Const
    cBufferSize = 16 * 1024;
  Private
    fBuffer: TBufferedFile;
    fUtf8BomDetected: Boolean;

    fMaxColCountFound: Integer;
    fForcedColCount: Integer;

    Function GetEof: Boolean; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    Procedure DetectBom;

    // those methods are split for performance reasons. mostly quotes do not need to be checked, and we know when to check when the first char of a column is a quote char
    Procedure ReadNonQuotedColValue(Var aValue: rawByteString; Var LineEndDetected: Boolean); {$IFDEF USE_INLINE}Inline; {$ENDIF}
    Procedure ReadQuotedColValue(Var aValue: rawByteString; Var LineEndDetected: Boolean); {$IFDEF USE_INLINE}Inline; {$ENDIF}
  Public
    Constructor Create;
    Destructor Destroy; Override;

    // this also calls detect delimiter
    Procedure Open(Const Filename: String; aShareMode: Cardinal = fmShareDenyWrite); Overload;
    Procedure Open(aStream: TStream; aTakeOwnerShipOfStream: Boolean = False); Overload;

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
    FLineBreak: AnsiString;

    Procedure WriteCell(Const Value: String); Overload;
    Procedure WriteCell(Const Value: rawByteString); Overload;
    procedure SetLineBreak(const Value: AnsiString);
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

    property LineBreak: AnsiString read FLineBreak write SetLineBreak;
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
    If charInSet(s[x], [FQuoteChar, fDelimiter, #10, #13]) Then
    Begin
      RequireQuote := True;
      Break;
    End;

  If RequireQuote Then
    s := self.FQuoteChar +
      StringReplace(s, FQuoteChar, FQuoteChar + FQuoteChar, [rfReplaceAll]) +
      self.FQuoteChar;;

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

procedure TCsvWriter.SetLineBreak(const Value: AnsiString);
begin
  FLineBreak := Value;
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

Procedure TCsvReader.Open(Const Filename: String; aShareMode: Cardinal = fmShareDenyWrite);
Var
  fs: TFileStream;
Begin
  fs := TFileStream.Create(Filename, fmOpenRead, aShareMode);
  Open(fs, True);
End;

Procedure TCsvReader.Open(aStream: TStream; aTakeOwnerShipOfStream: Boolean = False);
Begin
  Close;
  fBuffer := TBufferedFile.Create(cBufferSize);
  fBuffer.Open(aStream, aTakeOwnerShipOfStream);
  DetectBom;
  fDelimiter := DetectDelimiter;
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
  result := ',';
  LineEndDetected := False;

  If fBuffer.Eof Then // do nothing if there is no data
    exit;

  orgPos := fBuffer.Position;

  If fBuffer.CharCursor = '"' Then
  Begin
    ReadQuotedColValue(s, LineEndDetected);
    // after reading, the cursor is moved to the next column start, so we need to go one back
    fBuffer.Seek(-1);
    If charInSet(fBuffer.CharCursor, [',', ';', '|', #9]) Then
      result := fBuffer.CharCursor;
  End Else Begin
    For x := 1 To 1024 Do
    Begin
      fBuffer.NextByte;
      If charInSet(fBuffer.CharCursor, [',', ';', '|', #9]) Then
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

End.
