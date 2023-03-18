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
    cDefaultBlockSize = 1024 * 1024;
  Strict Private
    fFile: TStream;
    fOwnsStream: Boolean;




    fMinBytesLeftOfCursor, fMaxBytesLeftOfCursor,
        fMinBytesRightOfCursor, fMaxBytesRightOfCursor,
        fCurBytesLeftOfCursor, // NOTE: this is also the index of the curso inside the fBuffer
        fCurBytesRightOfCursor: Integer;


    fFileSize: int64; // the total file size on disk
    fCursorPositionInFile: Int64;


    Procedure CleanUp;
    Procedure ReadMoreBytesIntoBuffer; {$IFDEF USE_INLINE}Inline; {$ENDIF}
    Function EndOfFile: Boolean;
  private



  Public
  // direct access to the data, please dont mess with it
      Buffer: TBytes;
    Cursor: Integer; // index to the Buffer above

    Constructor Create(aMinBytesLeftOfCursor, aMaxBytesLeftOfCursor, aMinBytesRightOfCursor, aMaxBytesRightOfCursor: Integer);
    Destructor Destroy; Override;

    Procedure Open(Const aFileName: String; aShareMode: Cardinal = fmShareDenyWrite); Overload;
    Procedure Open(aStream: TStream; aTakeOwnerShipOfStream: Boolean = False); Overload;


    Function EoF: Boolean; {$IFDEF USE_INLINE}Inline; {$ENDIF}

    // returns false if we are at EOF
    // if needed reads more bytes into the buffer
    procedure Next; {$IFDEF USE_INLINE}Inline; {$ENDIF}

    Function CursorByte:Byte;{$IFDEF USE_INLINE}Inline; {$ENDIF}
    Function CursorChar: AnsiChar;{$IFDEF USE_INLINE}Inline; {$ENDIF}
    Function CursorPByte: pByte;{$IFDEF USE_INLINE}Inline; {$ENDIF}
    // copies bytes from the buffer, returns number of bytes copied
    Procedure CopyRawByteString(aInBufferIndex, aCount:Integer; out aValue:RawByteString);

    // as the above method only adds bytes, we sometimes need to clean them.. To do this call TrimBuffer, it will release some bytes from the start of the buffer and move the cursor acordingly
    // ATTENTION: this will change the Cursor and the buffer, so make sure you do not store anywhere any references to the current or to any old cursors.
  procedure TrimBuffer;


    // the total position in the file
    Property Position: int64 Read fCursorPositionInFile ;
    // the number of bytes in the underlying buffer left and right of cursor. or behind and in front of the cursor
    property BytesLeftOfCursor: integer read Cursor;
    property BytesRightOfCursor: Integer read fCurBytesRightOfCursor;

    // the size of the underlying stream
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



fCurBytesRightOfCursor:=0;
fCursorPositionInFile:=0;
Cursor:=0;
End;




Constructor TBufferedFile.Create;
Begin
  Inherited Create;

fMinBytesleftOfCursor:=aMinBytesLeftOfCursor;
fMaxBytesleftOfCursor:=aMaxBytesleftOfCursor;
fMinBytesRightOfCursor:=aMinBytesRightOfCursor;
fMaxBytesRightOfCursor:=aMaxBytesRightOfCursor;
End;

Destructor TBufferedFile.Destroy;
Begin
  CleanUp;
  Inherited;
End;

Function TBufferedFile.EoF: Boolean;
Begin
  result := self.Position >= fFileSize;
End;

procedure TBufferedFile.Next;
Begin
  If EoF Then
    exit;


  inc(Cursor);
  inc(fCursorPositionInFile);
  dec(fCurBytesRightOfCursor);
    ReadMoreBytesIntoBuffer;
End;

Procedure TBufferedFile.Open(Const aFileName: String; aShareMode: Cardinal = fmShareDenyWrite);
Var
  fs: TFileStream;
Begin
  fs := TFileStream.Create(aFileName, fmOpenRead, aShareMode);
  Open(fs);
  fOwnsStream := True;
End;

procedure TBufferedFile.TrimBuffer;
var
NumOfBytesToFree:Integer;
Begin
  if (Cursor> fMaxBytesLeftOfCursor )then
  begin
    NumOfBytesToFree:=Cursor- fMinBytesLeftOfCursor;
    move(Buffer[NumOfBytesToFree],
    Buffer[0],
    ((Cursor+ fCurBytesRightOfCursor+1)-NumOfBytesToFree));
    dec(Cursor, NumOfBytesToFree);
  end;
End;

Procedure TBufferedFile.ReadMoreBytesIntoBuffer;
var
i, BytecountToRead:Integer;
Begin
if (fCurBytesRightOfCursor < fMinBytesRightOfCursor ) and (not EndOfFile)then
begin

  ByteCountToRead:=fMaxBytesRightOfCursor - fCurBytesRightOfCursor ;

    i:=Cursor+fCurBytesRightOfCursor; // position of the end of the buffer
if length(Buffer) < (i+ByteCountToRead+1) then
  setLength(Buffer, i+ByteCountToRead+1);


    fCurBytesRightOfCursor:= fCurBytesRightOfCursor +
    fFile.Read(Buffer[i], ByteCountToRead);
end;
End;



Procedure TBufferedFile.Open(aStream: TStream; aTakeOwnerShipOfStream: Boolean = False);
Begin
  CleanUp;

  fOwnsStream := aTakeOwnerShipOfStream;
  fFile := aStream;
  fFileSize := fFile.Size;
  fCursorPositionInFile := 0;
  ReadMoreBytesIntoBuffer;
End;

function TBufferedFile.EndOfFile: Boolean;
begin
  result:= fFile.Position>=fFileSize;
end;



function TBufferedFile.CursorByte: Byte;
begin
  Result:=Buffer[Cursor];
end;

function TBufferedFile.CursorChar: AnsiChar;
begin
  result:=ansiChar(CursorByte);
end;



function TBufferedFile.CursorPByte: pByte;
begin
result:=@buffer[Cursor];
end;

procedure TBufferedFile.CopyRawByteString(aInBufferIndex, aCount: Integer; out aValue: RawByteString);
var
maxBytesinBuffer:Integer;
begin
       maxBytesInBuffer:= (cursor + fCurBytesLeftOfCursor)+1;
       dec(maxBytesInBuffer, aInBufferIndex);
       if Acount> maxBytesInBuffer then
       aCount:=maxBytesInBuffer;


       if aCount>0 then
       begin
       setLength(aValue, aCount);
       move(Buffer[aInBufferIndex], aValue[1], aCount);
       end else
       aValue:='';
end;

End.
