unit MaxLogic.TextFileEncodingHelper;

{
  we have often the problem that a file is encoded using multiple encodings so you have utf8 in one line... then ansi win 1250 in an other... and maybe something else in an other line
  this unit helps to load and process those files

  remember utf8 has those properties:
  If the code point is < 128, it’s represented by the corresponding byte value.
  If the code point is >= 128, it’s turned into a sequence of two, three, or four bytes, where each byte of the sequence is between 128 and 255.
}

Interface

Uses
  MaxLogic.BufferedFile,
  classes, sysUtils, RTTI, generics.collections;

Type
  TTextFileEncodingHelper = Class
  Private
    fUtf8BomFoundInFile: Boolean;
    fLineBreakChars: RawByteString;
    fLines: TArray<RawByteString>;
    Function DetectUsedLineBreak(Const aBody: RawByteString): Boolean;
    Procedure LoadAllText(Const aFileName: String; Out Body: RawByteString; Out BodyLen: Integer);
    Function GetCount: Integer;
    Procedure SplitLines(Const aBody: RawByteString);
    Procedure SplitUsing1ByteLineBreak(Const aBody: RawByteString; Var aCount, aCapacity: Integer);
    Procedure SplitUsing2ByteLineBreak(Const aBody: RawByteString; Var aCount, aCapacity: Integer);
    Procedure AddLine(Const aBody: RawByteString; index, len: Integer; Var aCount, aCapacity: Integer);
  Public
    Constructor Create;
    Destructor destroy; Override;

    Procedure Clear;
    Procedure LoadFromFile(Const aFileName: String);
    Procedure ConvertTo(l: TStringList); Overload;
    // saves the lines using utf8 to a stream
    Procedure ConvertTo(Utf8Stream: TStream; Const aLineBreak: String); Overload;

    Class Function RawToUnicode(Const s: RawByteString): String;

    Property Utf8BomFoundInFile: Boolean Read fUtf8BomFoundInFile;
    Property count: Integer Read GetCount;
    Property LineBreakChars: RawByteString Read fLineBreakChars;
  End;

Implementation

Uses
{$IFDEF MSWINDOWS}
  maxInMemoryFile,
{$ENDIF}
  ioUtils, ansiStrings, System.WideStrUtils;

{ TTextFileEncodingHelper }

Constructor TTextFileEncodingHelper.Create;
Begin

End;

Destructor TTextFileEncodingHelper.destroy;
Begin

  Inherited;
End;

Procedure TTextFileEncodingHelper.LoadFromFile(Const aFileName: String);
Var
  Body: RawByteString;
  BodyLen: Integer;
Begin
  Clear;
  LoadAllText(aFileName, Body, BodyLen);

  If BodyLen = 0 Then
    Exit;

  If DetectUsedLineBreak(Body) Then
    SplitLines(Body)
  Else
    fLines := [Body];
End;

Procedure TTextFileEncodingHelper.ConvertTo(l: TStringList);
Var
  x: Integer;
Begin
  l.Clear;
  For x := 0 To count - 1 Do
    l.Add(RawToUnicode(fLines[x]));
End;

Class Function TTextFileEncodingHelper.RawToUnicode(Const s: RawByteString): String;
Var
  encoding: TEncodeType;
Begin

  encoding := System.WideStrUtils.DetectUTF8Encoding(s);
  Case encoding Of
    etANSI:
      result := String(s);
    etUSASCII:
      result := String(s);
    etUTF8:
      result := Utf8ToString(s);
  End;

End;

Procedure TTextFileEncodingHelper.AddLine(Const aBody: RawByteString; index, len: Integer; Var aCount, aCapacity: Integer);
Begin
  // inc the capacity?
  If aCount = aCapacity Then
  Begin
    aCapacity := aCapacity * 2;
    SetLength(fLines, aCapacity);
  End;

  If len > 0 Then
    fLines[aCount] := copy(aBody, Index, len)
  Else
    fLines[aCount] := '';

  inc(aCount);

End;

Procedure TTextFileEncodingHelper.LoadAllText(Const aFileName: String; Out Body: RawByteString; Out BodyLen: Integer);
Var
  fs: TFileStream;
  b1, b2: TBytes;
Begin
  fs := TFileStream.Create(aFileName, fmOpenRead);
  BodyLen := fs.Size;
  If BodyLen > 0 Then
  Begin
    // skip Bom
    If System.WideStrUtils.HasUTF8BOM(fs) Then
    Begin
      dec(BodyLen, 3);
      fUtf8BomFoundInFile := True;
      fs.Position := 3; // skip bom
    End;

    SetLength(Body, BodyLen);
    fs.ReadBuffer(Body[1], BodyLen);
  End;
  fs.Free;
End;

Procedure TTextFileEncodingHelper.Clear;
Begin
  fUtf8BomFoundInFile := false;
  fLineBreakChars := '';
  fLines := Nil;
End;

Function TTextFileEncodingHelper.DetectUsedLineBreak(Const aBody: RawByteString): Boolean;
Var
  x: Integer;
Begin
  result := false;

  For x := 1 To length(aBody) Do
    If aBody[x] In [#10, #13] Then
    Begin
      fLineBreakChars := aBody[x];

      // is it a 2 char line break?
      If x + 1 <= length(aBody) Then
        If aBody[x + 1] <> aBody[x] Then
          If aBody[x + 1] In [#10, #13] Then
            fLineBreakChars := fLineBreakChars + aBody[x + 1];

      result := True;
      break;
    End;
End;

Function TTextFileEncodingHelper.GetCount: Integer;
Begin
  result := length(fLines);
End;

Procedure TTextFileEncodingHelper.SplitLines(Const aBody: RawByteString);
Var
  lCapacity, lCount: Integer;
Begin
  // init capacity and fLines array
  lCapacity := (length(aBody) Div 10) + 1; // just a starting point
  lCount := 0;
  SetLength(fLines, lCapacity);

  // most will use just #10 as line break. we can compare faster if we just always check 1 byte
  If length(fLineBreakChars) = 1 Then
    SplitUsing1ByteLineBreak(aBody, lCount, lCapacity)
  Else
    SplitUsing2ByteLineBreak(aBody, lCount, lCapacity);

  // truncate
  SetLength(fLines, lCount);
End;

Procedure TTextFileEncodingHelper.SplitUsing1ByteLineBreak(Const aBody: RawByteString; Var aCount, aCapacity: Integer);
Var
  i1: Integer;
  x: Integer;
  len: Integer;
  pb1: pByte;
  b: Byte;
Begin
  i1 := 1;
  b := Byte(fLineBreakChars[1]);
  pb1 := @aBody[1];

  For x := 1 To length(aBody) Do
  Begin
    If b = pb1^ Then
    Begin
      len := (x - i1);
      AddLine(aBody, i1, len, aCount, aCapacity);
      i1 := x + 1;
    End;

    inc(pb1);
  End;

  // add the tail

  If i1 <= length(aBody) Then
  Begin
    len := (length(aBody) - i1) + 1;
    AddLine(aBody, i1, len, aCount, aCapacity);
  End;
End;

Procedure TTextFileEncodingHelper.SplitUsing2ByteLineBreak(Const aBody: RawByteString; Var aCount, aCapacity: Integer);
Var
  i1: Integer;
  x: Integer;
  len: Integer;
  pb1: pByte;
  w: word;
Begin
  i1 := 1;
  move(fLineBreakChars[1], w, 2);
  pb1 := @aBody[1];

  For x := 1 To length(aBody) Do
  Begin
    If w = pWord(pb1)^ Then
    Begin
      len := (x - i1);
      AddLine(aBody, i1, len, aCount, aCapacity);
      i1 := x + 2;
    End;

    inc(pb1);
  End;

  // add the tail

  If i1 <= length(aBody) Then
  Begin
    len := (length(aBody) - i1) + 1;
    AddLine(aBody, i1, len, aCount, aCapacity);
  End;
End;

Procedure TTextFileEncodingHelper.ConvertTo(Utf8Stream: TStream; Const aLineBreak: String);
Var
  x: Integer;
  lLineBreakBytes, bytes: TBytes;
  NlLen: Integer;
Begin
  lLineBreakBytes := TEncoding.UTF8.GetBytes(aLineBreak);
  NlLen := length(lLineBreakBytes);

  For x := 0 To count - 1 Do
  Begin
    bytes := TEncoding.UTF8.GetBytes(RawToUnicode(fLines[x]));
    If length(bytes) <> 0 Then
      Utf8Stream.WriteBuffer(bytes[0], length(bytes));

    If NlLen <> 0 Then
      Utf8Stream.WriteBuffer(lLineBreakBytes[0], NlLen);
  End;
End;

End.
