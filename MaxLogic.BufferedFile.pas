Unit MaxLogic.BufferedFile;

{
  TBufferedFile is a single-direction read helper tailored for workloads that
  consume streams byte-by-byte or in very small slices. It maintains an internal
  block cache to amortize disk I/O while keeping cursor movement cheap.

  Best use cases:
    * Parsers or tokenizers that advance one byte/character at a time.
    * Scenarios with frequent random peek/copy operations on small ranges.

  Things it does NOT do:
    * Writing or bidirectional buffering.
    * Replacing RTL TFileStream for bulk reads (use >= 4 KB chunks if throughput
      is the goal — TBufferedFile only wins when the caller cannot batch reads).

  Construction:
    * Create with a buffer size tuned to your smallest hot read.
    * Call Open with a filename or stream. Position/Seek operate relative to the
      underlying stream but remain read-only.

  Keep usages simple:
    * Iterate via Cursor/NextByte for sequential scans.
    * Use copyBytes/CopyRawByteString for range access without disturbing the cursor.
}

{$IFNDEF DEBUG}
{$DEFINE USE_INLINE}
{$ENDIF}

Interface

Uses
  sysUtils, classes;

Type

  TBufferedFile = Class
  Public Const
    cDefaultBlockSize = 16 * 1024;
  Strict Private
    fFile: TStream;
    fOwnsStream: Boolean;
    fMaxBufferSize: Integer; // how many bytes will we read from the file to the buffer
    fBuffer: TBytes;
    FCursor: pByte; // the current processed byte in the fBuffer
    fBufferSize: Integer; // the total amount of bytes in the buffer
    fStartBuffer: pByte;
    fFileSize: int64; // the total file size on disk
    fBufferOffsetInFile: int64; // the position of the first byte in the buffer related to the underlying file
    Procedure CleanUp;
    Procedure readNextBlock; {$IFDEF USE_INLINE}Inline; {$ENDIF}
  Private
    // in file
    fPosition: int64;
    Procedure SetPosition(Const Value: int64);
  Public
    Constructor Create(aBufferSize: Integer = cDefaultBlockSize);
    Destructor Destroy; Override;

    Procedure Open(Const aFileName: String; aShareMode: Cardinal = fmShareDenyWrite); Overload;
    Procedure Open(aStream: TStream; aTakeOwnerShipOfStream: Boolean = False); Overload;
    Procedure Reset; // go back to start of the stream

    Function EoF: Boolean; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    // returns false if we are at EOF
    Function NextByte: Boolean; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    // tries to move in the buffer first, if that fails, it makes a hard re-read of the buffer from the file
    Procedure Seek(Const aCount: int64);

    // a Index referes to the file Position
    // note: this will try to read from the internal buffer first,
    // but if that fails then the fileStream will be accessed
    // NOTE: that does not change the position property
    Function copyBytes(Const aStartIndex, aCount: int64): TBytes; Overload; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    // aStartIndex is in relation to the file, not to the buffer
    Procedure copyBytes(Const aStartIndex, aCount: int64; Var aBuffer: TBytes); Overload; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    Procedure copyBytes(Const aStartIndex, aCount: int64; aBuffer: pointer); Overload; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    Function CopyRawByteString(Const aStartIndex, aCount: int64): rawByteString; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    Function CharCursor: AnsiChar; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    Function pCharCursor: pAnsiChar; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    // the current byte
    Property Cursor: pByte Read FCursor;
    // the total position in the file
    Property Position: int64 Read fPosition Write SetPosition;
    Property Size: int64 Read fFileSize;
  End;

Implementation


{ TBufferedFile }

Procedure TBufferedFile.CleanUp;
Begin
  If assigned(fFile) Then
  Begin
    If fOwnsStream Then
      fFile.Free;
    fFile := Nil;
  End;
End;

Procedure TBufferedFile.copyBytes(Const aStartIndex, aCount: int64;
  Var aBuffer: TBytes);
Begin
  If aCount <= 0 Then
  Begin
    SetLength(aBuffer, 0);
    Exit;
  End;

  SetLength(aBuffer, aCount);

  copyBytes(aStartIndex, aCount, @aBuffer[0]);
End;

Procedure TBufferedFile.copyBytes(Const aStartIndex, aCount: int64; aBuffer: pointer);
Var
  InBufferOffset: int64;
  CurFilePosition: int64;
  Remaining: int64;
  ToRead: Integer;
  ReadCount: Integer;
  Dest: pByte;
Begin
  If (aCount <= 0) Then
    Exit;

  If (aStartIndex < 0) Or (aCount < 0) Then
    raise EArgumentOutOfRangeException.Create('Requested range is invalid.');

  If (aCount > 0) And (aStartIndex > fFileSize - aCount) Then
    raise EArgumentOutOfRangeException.Create('Requested range exceeds file size.');

  If (aStartIndex >= fBufferOffsetInFile) And (aStartIndex + aCount <= fBufferOffsetInFile + fBufferSize) Then
  Begin
    // translate InFileOffset to in BufferOffset
    InBufferOffset := (aStartIndex - fBufferOffsetInFile);
    Move(fBuffer[InBufferOffset], aBuffer^, NativeInt(aCount));
  End Else Begin
    CurFilePosition := fFile.Position; // store the current offset
    Try
      fFile.Position := aStartIndex;
      Dest := pByte(aBuffer);
      Remaining := aCount;
      While Remaining > 0 Do
      Begin
        If Remaining > High(Integer) Then
          ToRead := High(Integer)
        Else
          ToRead := Integer(Remaining);

        ReadCount := fFile.Read(Dest^, ToRead);
        If ReadCount <> ToRead Then
          raise EReadError.Create('Unable to read requested byte range from stream.');

        Inc(Dest, ReadCount);
        Dec(Remaining, ReadCount);
      End;
    Finally
      fFile.Position := CurFilePosition; // restore the current offset
    End;
  End;
End;

Function TBufferedFile.copyBytes(Const aStartIndex, aCount: int64): TBytes;
Begin
  SetLength(result, aCount);
  If aCount <> 0 Then
    copyBytes(aStartIndex, aCount, result);
End;

Constructor TBufferedFile.Create;
Begin
  Inherited Create;
  fMaxBufferSize := aBufferSize;
  SetLength(fBuffer, aBufferSize);
End;

Destructor TBufferedFile.Destroy;
Begin
  CleanUp;
  Inherited;
End;

Function TBufferedFile.EoF: Boolean;
Begin
  result := Position >= fFileSize;
End;

Function TBufferedFile.NextByte: Boolean;
Var
  InBufferOffset: NativeInt;
Begin
  If EoF Then
    exit(False);

  InBufferOffset := NativeInt(FCursor) - NativeInt(fStartBuffer);
  If (InBufferOffset + 1 < fBufferSize) Then
  Begin
    Inc(FCursor);
    Inc(fPosition);
    Exit(True);
  End;

  readNextBlock;

  If fBufferSize = 0 Then
  Begin
    fPosition := fFileSize;
    Result := False;
    Exit;
  End;

  fPosition := fBufferOffsetInFile;
  Result := True;

End;

Procedure TBufferedFile.Open(Const aFileName: String; aShareMode: Cardinal = fmShareDenyWrite);
Var
  fs: TFileStream;
Begin
  fs := TFileStream.Create(aFileName, fmOpenRead, aShareMode);
  Open(fs);
  fOwnsStream := True;
End;

Procedure TBufferedFile.readNextBlock;
Begin
  fBufferOffsetInFile := fFile.Position;
  fPosition := fFile.Position;
  fBufferSize := fFile.Read(fBuffer[0], fMaxBufferSize);
  FCursor := @fBuffer[0];
  fStartBuffer := FCursor;
End;

Procedure TBufferedFile.Reset;
Begin
  If Position <> 0 Then
    Seek(-Position);
End;

Procedure TBufferedFile.Seek(Const aCount: int64);
Var
  Target: int64;
  BufferEnd: int64;
  CursorOffset: Integer;
Begin
  Target := fPosition + aCount;

  If Target < 0 Then
    Target := 0
  Else If Target > fFileSize Then
    Target := fFileSize;

  BufferEnd := fBufferOffsetInFile + fBufferSize;
  If (Target >= fBufferOffsetInFile) And (Target < BufferEnd) Then
  Begin
    fPosition := Target;
    FCursor := fStartBuffer;
    CursorOffset := Integer(Target - fBufferOffsetInFile);
    Inc(FCursor, CursorOffset);
    Exit;
  End;

  fFile.Position := Target;
  readNextBlock;

  fPosition := Target;

  If (fBufferSize > 0) And (Target >= fBufferOffsetInFile) And
    (Target < (fBufferOffsetInFile + fBufferSize)) Then
  Begin
    FCursor := fStartBuffer;
    CursorOffset := Integer(Target - fBufferOffsetInFile);
    Inc(FCursor, CursorOffset);
  End Else
    FCursor := fStartBuffer;
End;

Procedure TBufferedFile.SetPosition(Const Value: int64);
Begin
  Seek(Value - fPosition);
End;

Function TBufferedFile.CharCursor: AnsiChar;
Begin
  result := AnsiChar(FCursor^);
End;

Function TBufferedFile.pCharCursor: pAnsiChar;
Begin
  result := pAnsiChar(FCursor);
End;

Function TBufferedFile.CopyRawByteString(Const aStartIndex, aCount: int64): rawByteString;
Begin
  SetLength(result, aCount);
  If aCount <> 0 Then
    copyBytes(aStartIndex, aCount, @result[1]);
End;

Procedure TBufferedFile.Open(aStream: TStream; aTakeOwnerShipOfStream: Boolean = False);
Begin
  CleanUp;

  fOwnsStream := aTakeOwnerShipOfStream;
  fFile := aStream;
  fFileSize := fFile.Size;
  fPosition := 0;
  readNextBlock;
End;

End.
