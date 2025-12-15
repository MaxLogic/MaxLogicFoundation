Unit maxTextStream;

{$DEFINE USE_INLINE }
{ Copyright Paweł Piotrowski
  Version: 1.11
  History:
  2021-06-24: linux compatible
  2018-05-09: added hasBom and CanUseUtf8OnThis methods which sould help a bit with finding a proper encoding for the given file. also fixed a bug, where when setting a bad encoding it might cause out of memory error
  2017-12-21: aded the enumerator
  2015-12-17: improved ReadLine function. It supports now files with #10 as linebreak as opposed to the previous #13#10 line breaks
  2014-03-19: fixed a small bug in the reset method, the problem was that the variable StreamIsEmpty was not reseted
  2014-03-19: many speed improvements }

Interface

Uses
  classes, sysUtils,
  {$IFDEF MSWINDOWS}
  maxInMemoryFile,
  {$ENDIF}
  generics.defaults, generics.Collections;

Type
  // forward declaration of the most useable classes
  tmaxTextReader = Class; // the basic class
  TmaxLineReader = Class; // a helper to enumerate all lines of a file

  TLineEnumerator = Class
  Private
    fCurLine: String;
    fReader: tmaxTextReader;
  Protected
    Function GetCurrent: String; Virtual;
  Public
    Constructor Create(Const aFileName: String);
    Destructor Destroy; Override;

    Function MoveNext: Boolean;
    Property Current: String Read GetCurrent;
  End;

  iLineEnumFactory = Interface
    ['{5AB8BF2C-5E79-4650-97F1-461CDBF31E7F}']
    Function GetEnumerator: TLineEnumerator;
  End;

  TLineEnumFactory = Class(TInterfacedObject, iLineEnumFactory)
  Private
    fFilename: String;
  Public
    Function GetEnumerator: TLineEnumerator;
  End;

  // allows to use a for in loop like this
  // for MyLine in TMaxLineReader.FromFile() do
  TmaxLineReader = Class
  Public
    Class Function FromFile(Const aFileName: String): iLineEnumFactory;
  End;

  tmaxTextReader = Class
  Private
    fByteBuffer, fByteBufferPointer: pByte;
    fBytesInBuffer, fByteBuffercapacity: integer;

    fCharBufferPointer, fCharBuffer: pWideChar;
    fCharsInCharBuffer, fCharBuffercapacity: integer;
    FNoDataInStream: Boolean;

    FEncoding: TEncoding;
    fIsSingleByte: Boolean;
    fIsDefaultEncoding: Boolean;
    FStream: TStream;
    fStreamStartPos: int64;
    FOwnStream: Boolean;
    Function GetEOF: Boolean;
    Procedure SetOwnStream(Const Value: Boolean);
    Procedure DoDetectBOM;
    Procedure ReadInByteBuffer; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    Procedure ReadInCharBuffer; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    Procedure ReadInCharBufferForNonDefaultEncoding;
    Function Min(i1, i2: integer): integer; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    Procedure Push(Var s: UnicodeString; CharCount: integer); {$IFDEF USE_INLINE}Inline; {$ENDIF}
    Procedure ExpandByteBuffer;
    Procedure expandCharBuffer;
  Public
    Constructor Create(Const Filename: UnicodeString; Encoding: TEncoding = Nil; DetectBOM: Boolean = True; BufferSize: integer = 1024); Overload;

    Constructor Create(Const aStream: TStream; Encoding: TEncoding = Nil; DetectBOM: Boolean = True; BufferSize: integer = 1024); Overload;
    Destructor Destroy; Override;

    Function ReadLine: UnicodeString;

    // jump to the start of the stream
    Procedure Reset;

    Function Read(Const Buffer: TCharArray; Index, Count: integer): integer; Overload;
    // count is the number of chars wanted, result is the number of chars copied
    Function Read(Var str: UnicodeString; Count: integer): integer; Overload;
    Function Read(Buffer: pWideChar; Count: integer): integer; Overload;

    // this is a helper function.
    // using it we can try if the file can be parsed using utf8
    // NOTE: if it returns true, this by all means doesn't mean that it is actually utf8. But the result of false means that it is for sure NOT utf8
    // ATTEMTION: use only with text data without a bom. For BOM detection use the HasBOM function below
    Class Function CanUseUtf8OnThis(Const Filename: String): Boolean; Overload;
    Class Function CanUseUtf8OnThis(stream: TStream): Boolean; Overload;
    Class Function CanUseUtf8OnThis(Const bytes: TBytes; byteCount: integer): Boolean; Overload;
    Class Function CanUseUtf8OnThis(pb: pByte; byteCount: integer): Boolean; Overload;

    // this is a helper function.
    // using it, we can find out if the text has a BOM which will determine the encoding
    // returns nil if no bom was detected. And the encoding otherwise
    Class Function HasBom(Const Filename: String): TEncoding; Overload;
    Class Function HasBom(stream: TStream): TEncoding; Overload;
    Class Function HasBom(stream: TStream; Out Encoding: TEncoding): Boolean; Overload;
    Class Function HasBom(Const bytes: TBytes): TEncoding; Overload;

    Property BaseStream: TStream Read FStream;
    Property OwnStream: Boolean Read FOwnStream Write SetOwnStream;
    Property CurrentEncoding: TEncoding Read FEncoding;
    Property Eof: Boolean Read GetEOF;
    // just an alias to simplify legacy code conversion
    Property EndofStream: Boolean Read GetEOF;

    // just an alias to simplify legacy code conversion
    Property ReadLn: String Read ReadLine;
  End;

Implementation

uses
  ioUtils;

Type
  // just a hack to access some protected methods
  TMyEncoding = Class Helper For TEncoding
    Function GetCharCount2(bytes: pByte; byteCount: integer): integer; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    Function GetChars2(bytes: pByte; byteCount: integer; Chars: PChar; CharCount: integer): integer; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    Function GetByteCount2(Chars: PChar; CharCount: integer): integer; {$IFDEF USE_INLINE}Inline; {$ENDIF}
  End;

  { tmaxTextReader }

Class Function tmaxTextReader.CanUseUtf8OnThis(Const Filename: String): Boolean;
Var
  fs: TFileStream;
Begin
  fs := TFileStream.Create(Filename, fmOpenRead);
  result := tmaxTextReader.CanUseUtf8OnThis(fs);
  fs.Free;
End;

Class Function tmaxTextReader.CanUseUtf8OnThis(stream: TStream): Boolean;
Var
  pb: pByte;
  p, len, byteCount: integer;
  bytes: TBytes;
Begin
  p := stream.Position;
  byteCount := stream.Size - p;

  // we can go and use a pointer if available , that is better then copy the data first to a byte array
  If stream Is TMemorystream Then
  Begin
    pb := (stream As TMemorystream).Memory;
    inc(pb, p);
  End Else If stream Is TBytesStream Then
  Begin
    pb := @(stream As TBytesStream).bytes[0];
    inc(pb, p);
  End Else Begin
    SetLength(bytes, byteCount);
    stream.ReadBuffer(bytes[0], byteCount);
    pb := @bytes[0];
    stream.Position := p;
  End;

  result := CanUseUtf8OnThis(pb, byteCount);
End;

Class Function tmaxTextReader.CanUseUtf8OnThis(Const bytes: TBytes; byteCount: integer): Boolean;
Var
  pb: pByte;
Begin
  If (byteCount > 0) And (length(bytes) > 0) Then
  Begin
    pb := @bytes[0];
    result := CanUseUtf8OnThis(pb, byteCount);
  End
  Else
    result := false;
End;

Constructor tmaxTextReader.Create(Const aStream: TStream; Encoding: TEncoding; DetectBOM: Boolean; BufferSize: integer);
Begin
  Inherited Create;

  FStream := aStream;
  fStreamStartPos := FStream.Position;

  fByteBuffercapacity := BufferSize;
  If fByteBuffercapacity < 128 Then
    fByteBuffercapacity := 128;
  fCharBuffercapacity := fByteBuffercapacity;
  GetMem(fByteBuffer, fByteBuffercapacity);
  fByteBufferPointer := fByteBuffer;

  // one extra char to store #0
  GetMem(fCharBuffer, fCharBuffercapacity * SizeOf(widechar));
  fCharBufferPointer := fCharBuffer;

  ReadInByteBuffer;

  FEncoding := Encoding;
  If DetectBOM Then
    DoDetectBOM;

  If Not Assigned(FEncoding) Then
    FEncoding := TEncoding.default;

  ExpandByteBuffer;
  ReadInCharBuffer
End;

Constructor tmaxTextReader.Create(Const Filename: UnicodeString; Encoding: TEncoding; DetectBOM: Boolean; BufferSize: integer);
Var
  stream: TStream;
Begin
  {$IFDEF MSWINDOWS}
  stream := TmmfStream.Create(Filename, mmfRead);
  {$ELSE}
  Stream := TFileStream.Create(FileName, fmOpenRead);
  {$ENDIF}
  { ms:= TMemorystream.Create;
    ms.LoadFromFile(Filename);
    stream:= ms; }

  Create(stream, Encoding, DetectBOM, BufferSize);
  FOwnStream := True;
End;

Destructor tmaxTextReader.Destroy;
Begin
  FreeMem(fByteBuffer, fByteBuffercapacity);
  FreeMem(fCharBuffer, fCharBuffercapacity * SizeOf(widechar));
  If OwnStream Then
    FStream.Free;
  Inherited;
End;

Procedure tmaxTextReader.DoDetectBOM;
Var
  bytes: TBytes;
  LEncoding: TEncoding;
  PreambleLength: integer;
Begin
  If fBytesInBuffer = 0 Then
    exit;

  // try to automatically detect the buffer encoding
  SetLength(bytes, fBytesInBuffer);
  Move(fByteBufferPointer^, bytes[0], fBytesInBuffer);

  LEncoding := Nil;
  PreambleLength := TEncoding.GetBufferEncoding(bytes, LEncoding);

  If (LEncoding <> TEncoding.default) Then
    FEncoding := LEncoding;

  If PreambleLength > 0 Then
  Begin
    inc(fByteBufferPointer, PreambleLength);
    dec(fBytesInBuffer, PreambleLength);
    fStreamStartPos := fStreamStartPos + PreambleLength; // for when we call the reset method
  End;
End;

Procedure tmaxTextReader.ExpandByteBuffer;
Var
  MaxBytesPosible: integer;
  pb: pByte;
Begin
  MaxBytesPosible := FEncoding.GetMaxByteCount(fCharBuffercapacity);
  If MaxBytesPosible > fByteBuffercapacity Then
  Begin
    GetMem(pb, MaxBytesPosible);
    If fBytesInBuffer > 0 Then
      Move(fByteBufferPointer^, pb^, fBytesInBuffer);

    FreeMem(fByteBuffer, fByteBuffercapacity);
    fByteBuffercapacity := MaxBytesPosible;
    fByteBuffer := pb;
    fByteBufferPointer := pb;
    ReadInByteBuffer;
  End;
End;

Procedure tmaxTextReader.expandCharBuffer;
Var
  pw: pWideChar;
  c: integer;
Begin
  c := fCharBuffercapacity * 2;
  GetMem(pw, c * SizeOf(widechar));
  If fCharsInCharBuffer > 0 Then
    Move(fCharBufferPointer^, pw^, fCharsInCharBuffer * SizeOf(widechar));
  FreeMem(fCharBuffer, fCharBuffercapacity * SizeOf(widechar));
  fCharBuffercapacity := c;
  fCharBuffer := pw;
  fCharBufferPointer := pw;
  ExpandByteBuffer;
End;

Function tmaxTextReader.Read(Var str: UnicodeString; Count: integer): integer;
Begin
  SetLength(str, Count);
  If Count > 0 Then
    result := Read(@str[1], Count)
  Else
    result := 0;

  SetLength(str, result);
End;

Function tmaxTextReader.GetEOF: Boolean;
Begin
  result := FNoDataInStream And (fBytesInBuffer = 0) And (fCharsInCharBuffer = 0);
End;

Class Function tmaxTextReader.HasBom(stream: TStream): TEncoding;
Var
  bytes: TBytes;
  p, c: integer;
Begin
  SetLength(bytes, 32); // most boms are fairly small, but... maybe there will come atime when they will grow a bit...
  p := stream.Position;
  c := stream.Read(bytes[0], 32);

  // go back to orginal position
  stream.Position := p;

  If c <> length(bytes) Then
    SetLength(bytes, c);

  result := HasBom(bytes);
End;

Class Function tmaxTextReader.HasBom(Const Filename: String): TEncoding;
Var
  fs: TFileStream;
Begin
  fs := TFileStream.Create(Filename, fmOpenRead);
  result := tmaxTextReader.HasBom(fs);
  fs.Free;
End;

Class Function tmaxTextReader.HasBom(Const bytes: TBytes): TEncoding;
Var
  PreambleLength: integer;
Begin
  result := Nil;
  PreambleLength := TEncoding.GetBufferEncoding(bytes, result);
  If PreambleLength = 0 Then
    result := Nil;
End;

Class Function tmaxTextReader.HasBom(stream: TStream; Out Encoding: TEncoding): Boolean;
Begin
  Encoding := HasBom(stream);
  result := Assigned(Encoding);
End;

Function tmaxTextReader.Min(i1, i2: integer): integer;
Begin
  If i1 < i2 Then
    result := i1
  Else
    result := i2;
End;

Procedure tmaxTextReader.Push(Var s: UnicodeString; CharCount: integer);
Begin
  SetString(s, fCharBufferPointer, CharCount);
  inc(fCharBufferPointer, CharCount);
  dec(fCharsInCharBuffer, CharCount);
End;

Function tmaxTextReader.Read(Const Buffer: TCharArray; Index, Count: integer): integer;
Begin
  result := Read(@Buffer[Index], Count);
End;

Function tmaxTextReader.Read(Buffer: pWideChar; Count: integer): integer;
Var
  BytesReaded: integer;
  bytesLeft: integer;
Begin
  result := 0;

  If fCharsInCharBuffer = 0 Then
  Begin
    ReadInCharBuffer;
    If (fCharsInCharBuffer = 0) Or (Count = 0) Then
      exit(0);
  End;

  bytesLeft := Count;

  Repeat
    BytesReaded := Min(bytesLeft, fCharsInCharBuffer);
    dec(bytesLeft, BytesReaded);

    Move(fCharBufferPointer^, Buffer^, BytesReaded * SizeOf(widechar));

    inc(Buffer, BytesReaded);
    inc(fCharBufferPointer, BytesReaded);
    dec(fCharsInCharBuffer, BytesReaded);

    If fCharsInCharBuffer = 0 Then
      ReadInCharBuffer;

  Until (bytesLeft = 0) Or (fCharsInCharBuffer = 0);

  result := Count - bytesLeft;
End;

Procedure tmaxTextReader.ReadInByteBuffer;
Var
  ReadCount, len: integer;
  pb: pByte;
Begin
  If FNoDataInStream Or (fBytesInBuffer = fByteBuffercapacity) Then
    exit;

  If (fByteBufferPointer <> fByteBuffer) Then
  Begin
    // there are some bytes remaining in the middle of buffer, move them to the begining of the buffer, so we can read in more
    If fBytesInBuffer > 0 Then
      Move(fByteBufferPointer^, fByteBuffer^, fBytesInBuffer);
  End;

  pb := fByteBuffer;
  inc(pb, fBytesInBuffer);
  len := fByteBuffercapacity - fBytesInBuffer;
  fByteBufferPointer := fByteBuffer;

  ReadCount := FStream.Read(pb^, len);
  inc(fBytesInBuffer, ReadCount);
  FNoDataInStream := ReadCount < len;
End;

Procedure tmaxTextReader.ReadInCharBuffer;
Begin
  If fBytesInBuffer = 0 Then
  Begin
    ReadInByteBuffer;
    // end of stream was reached...
    If fBytesInBuffer = 0 Then
      exit;
  End;

  // MOVE THE remaining CHARS TO THE BEGINNING OF THE BUFFER, if there are any chars and if they are not already aligned on the start of the buffer

  If fCharBufferPointer <> fCharBuffer Then
  BEGIN
    If fCharsInCharBuffer <> 0 Then
      Move(fCharBufferPointer^, fCharBuffer^, fCharsInCharBuffer * SizeOf(widechar));
    fCharBufferPointer := fCharBuffer;
  END;

  ReadInCharBufferForNonDefaultEncoding;
End;

Procedure tmaxTextReader.ReadInCharBufferForNonDefaultEncoding;
Var
  pw: pWideChar;
  len, MaxBytesPosible, CharsToPush: integer;
Begin
  Repeat
    Repeat
      pw := fCharBufferPointer;

      // move to the end of the charBuffer
      If fCharsInCharBuffer > 0 Then
        inc(pw, fCharsInCharBuffer);

      // how many chars can we read in?
      CharsToPush := fCharBuffercapacity - fCharsInCharBuffer;

      // how many bytes will it be at the maximum?
      MaxBytesPosible := FEncoding.GetMaxByteCount(CharsToPush);

      // how many characters will we get out of the ByteBuffer?
      len := FEncoding.GetCharCount2(fByteBufferPointer, Min(fBytesInBuffer, MaxBytesPosible));

      If len = 0 Then
      Begin
        ReadInByteBuffer;
        len := FEncoding.GetCharCount2(fByteBufferPointer, Min(fBytesInBuffer, MaxBytesPosible));

        If len = 0 Then
        Begin
          If CharsToPush = fCharBuffercapacity Then
            expandCharBuffer
          Else
            exit;
        End;
      End;

    Until len > 0;

    If len < CharsToPush Then
      CharsToPush := len;

    FEncoding.GetChars2(fByteBufferPointer, fBytesInBuffer, pw, CharsToPush);

    If CharsToPush > 0 Then
    Begin
      inc(fCharsInCharBuffer, CharsToPush);

      // now check how many bytes does our UnicodeString actually represent...
      len := FEncoding.GetByteCount2(pw, CharsToPush);
      inc(fByteBufferPointer, len);
      dec(fBytesInBuffer, len);
    End;

    If FNoDataInStream Then
    Begin
      // discard the byteBuffer if it contains no more charackters
      len := FEncoding.GetCharCount2(fByteBufferPointer, fBytesInBuffer);
      If len = 0 Then
      Begin
        fBytesInBuffer := 0;
        fByteBufferPointer := fByteBuffer;
        break;
      End;
    End;

    If (fCharsInCharBuffer <> fCharBuffercapacity) Then
      ReadInByteBuffer;
  Until (fCharsInCharBuffer = fCharBuffercapacity);
End;

Function tmaxTextReader.ReadLine: UnicodeString;
Var
  s: UnicodeString;
  start, pw, EndPoint: pWideChar;
Begin
  result := '';

  If fCharsInCharBuffer = 0 Then
  Begin
    ReadInCharBuffer;
    If fCharsInCharBuffer = 0 Then
      exit;
  End;

  Repeat
    start := fCharBufferPointer;
    pw := start;
    EndPoint := start;
    inc(EndPoint, fCharsInCharBuffer);

    While pw <> EndPoint Do
    Begin
      If charInSet(pw^, [#13, #10]) Then
      Begin
        // we found or line break, return the charackters from this point down to start and then exit
        Push(s, pw - start);
        dec(fCharsInCharBuffer);
        inc(fCharBufferPointer);
        result := result + s;

        // read in one more character, just to make sure we catch a #10
        If pw^ <> #10 Then
        Begin
          inc(pw);
          If pw = EndPoint Then
          Begin
            ReadInCharBuffer;
            // if there are no more characters, then just exit here
            If fCharsInCharBuffer = 0 Then
              exit;
            pw := fCharBufferPointer;
          End;
          If pw^ = #10 Then
          Begin
            dec(fCharsInCharBuffer);
            inc(fCharBufferPointer);
          End;
        End;

        exit;
        // end of if pw^ = #13 or #10
      End;

      inc(pw);
      // end of while pw <> endPoint
    End;

    // we did not found our Line end in this char buffer... push what we have and get a new char buffer
    Push(s, fCharsInCharBuffer);
    result := result + s;
    ReadInCharBuffer;
  Until (fCharsInCharBuffer = 0);
End;

Procedure tmaxTextReader.Reset;
Var
  PreambleLen: integer;
Begin
  FNoDataInStream := false;
  fBytesInBuffer := 0;
  fByteBufferPointer := fByteBuffer;

  fCharsInCharBuffer := 0;
  fCharBufferPointer := fCharBuffer;

  FStream.Position := fStreamStartPos;

  ReadInByteBuffer;
  ReadInCharBuffer
End;

Procedure tmaxTextReader.SetOwnStream(Const Value: Boolean);
Begin
  FOwnStream := Value;
End;

Class Function tmaxTextReader.CanUseUtf8OnThis(pb: pByte; byteCount: integer): Boolean;
Begin
  result := TEncoding.utf8.GetCharCount2(pb, byteCount) > 0;
End;

{ TMyEncoding }

Function TMyEncoding.GetByteCount2(Chars: PChar; CharCount: integer): integer;
Begin
  result := GetByteCount(Chars, CharCount);
End;

Function TMyEncoding.GetCharCount2(bytes: pByte; byteCount: integer): integer;
Begin
  result := GetCharCount(pByte(bytes), byteCount);

End;

Function TMyEncoding.GetChars2(bytes: pByte; byteCount: integer; Chars: PChar; CharCount: integer): integer;
Begin
  result := GetChars(bytes, byteCount, Chars, CharCount);
End;

{ TLineEnumerator }

Constructor TLineEnumerator.Create(Const aFileName: String);
Begin
  Inherited Create;
  fReader := tmaxTextReader.Create(aFileName);
End;

Destructor TLineEnumerator.Destroy;
Begin
  fReader.Free;
  Inherited;
End;

Function TLineEnumerator.GetCurrent: String;
Begin
  result := fCurLine;
End;

Function TLineEnumerator.MoveNext: Boolean;
Begin
  If fReader.Eof Then
    result := false
  Else
  Begin
    fCurLine := fReader.ReadLine;
    result := True;
  End;
End;

{ TMaxLineReader }

Class Function TmaxLineReader.FromFile(Const aFileName: String): iLineEnumFactory;
Var
  factory: TLineEnumFactory;
Begin
  factory := TLineEnumFactory.Create;
  factory.fFilename := aFileName;
  result := factory;
End;

{ TLineEnumFactory }

Function TLineEnumFactory.GetEnumerator: TLineEnumerator;
Begin
  result := TLineEnumerator.Create(fFilename);
End;

End.
