Unit MaxLogic.BufferedFile;

{
  This is to optimize reading a file byte by byte
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
  copyBytes(aStartIndex, aCount, @aBuffer[0]);
End;

Procedure TBufferedFile.copyBytes(Const aStartIndex, aCount: int64; aBuffer: pointer);
Var
  InBufferOffset: int64;
  CurFilePosition: int64;
Begin
  If (aStartIndex >= fBufferOffsetInFile) And (aStartIndex + aCount <= fBufferOffsetInFile + fBufferSize) Then
  Begin
    // translate InFileOffset to in BufferOffset
    InBufferOffset := (aStartIndex - fBufferOffsetInFile);
    Move(fBuffer[InBufferOffset], aBuffer^, aCount);
  End Else Begin
    CurFilePosition := fFile.Position; // store the current offset
    Try
      fFile.Position := aStartIndex;
      fFile.Read(aBuffer^, aCount);
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
Begin
  If EoF Then
    exit(False);

  result := True;

  inc(FCursor);
  inc(fPosition);

  If (fPosition >= (fBufferOffsetInFile + fBufferSize))
    Or (fPosition < fBufferOffsetInFile) Then
    readNextBlock;

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
  InBufferOffset: int64;
  requestedInBufferOffset: int64;
Begin
  InBufferOffset := (nativeUInt(FCursor) - nativeUInt(fStartBuffer));
  requestedInBufferOffset := InBufferOffset + aCount;

  // are we still in the boundaries of the buffer?
  If (requestedInBufferOffset >= 0) And (requestedInBufferOffset < fBufferSize) Then
  Begin
    inc(FCursor, aCount);
    inc(fPosition, aCount);
  End Else If ((fBufferOffsetInFile + requestedInBufferOffset) >= fFileSize) Then
  Begin
    fFile.Position := fFileSize;
    readNextBlock;
  End Else Begin
    fFile.Position := (fBufferOffsetInFile + requestedInBufferOffset);
    readNextBlock;
  End;
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
