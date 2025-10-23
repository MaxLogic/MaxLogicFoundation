unit MaxLogic.BufferedFile.Tests;

{$I fpc_delphimode.inc}

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  maxlogic.fpc.compatibility,
  MaxLogic.BufferedFile;

type
  [TestFixture]
  TBufferedFileTests = class
  private
    const cSampleData: RawByteString = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  private
    fBuffer: TBufferedFile;
    procedure PrepareBuffer(const aData: RawByteString; const aBlockSize: Integer = TBufferedFile.cDefaultBlockSize);
    procedure TearDownBuffer;
    class function DataToBytes(const aData: RawByteString): TBytes; static;
    class procedure AssertBytesEqual(const aExpected, aActual: TBytes; const aMessage: string); static;
  public
    [TearDown] procedure TearDown;

    [Test] procedure CursorStartsAtFirstByte;
    [Test] procedure NextByteWalksEntireStream;
    [Test] procedure SeekForwardAndBackward;
    [Test] procedure SeekBeforeStartClampsToZero;
    [Test] procedure CopyBytesWithinBuffer;
    [Test] procedure CopyBytesOutsideBufferUsesStream;
    [Test] procedure CopyRawByteStringMatches;
  end;

implementation

procedure TBufferedFileTests.PrepareBuffer(const aData: RawByteString; const aBlockSize: Integer);
var
  lBytes: TBytes;
  lStream: TBytesStream;
begin
  TearDownBuffer;
  lBytes := DataToBytes(aData);
  lStream := TBytesStream.Create(lBytes);
  try
    fBuffer := TBufferedFile.Create(aBlockSize);
    fBuffer.Open(lStream, True);
    lStream := nil;
  finally
    if lStream <> nil then
      lStream.Free;
  end;
end;

procedure TBufferedFileTests.TearDownBuffer;
begin
  if fBuffer <> nil then
  begin
    fBuffer.Free;
    fBuffer := nil;
  end;
end;

class function TBufferedFileTests.DataToBytes(const aData: RawByteString): TBytes;
var
  lCount: Integer;
begin
  lCount := Length(aData);
  SetLength(Result, lCount);
  if lCount <> 0 then
    Move(PAnsiChar(aData)^, Result[0], lCount);
end;

class procedure TBufferedFileTests.AssertBytesEqual(const aExpected, aActual: TBytes; const aMessage: string);
begin
  Assert.AreEqual<Integer>(Length(aExpected), Length(aActual), aMessage + ' length mismatch');
  if Length(aExpected) <> 0 then
    Assert.IsTrue(CompareMem(@aExpected[0], @aActual[0], Length(aExpected)), aMessage + ' data mismatch');
end;

[TearDown]
procedure TBufferedFileTests.TearDown;
begin
  TearDownBuffer;
end;

[Test]
procedure TBufferedFileTests.CursorStartsAtFirstByte;
begin
  PrepareBuffer(cSampleData, 8);
  Assert.IsTrue(fBuffer.Cursor <> nil, 'Cursor must be assigned');
  Assert.AreEqual<Byte>(Byte(cSampleData[1]), fBuffer.Cursor^, 'Cursor should point to the first byte');
  Assert.AreEqual<Int64>(0, fBuffer.Position, 'Position should start at 0');
end;

[Test]
procedure TBufferedFileTests.NextByteWalksEntireStream;
var
  lBytes: TBytes;
  i: Integer;
begin
  PrepareBuffer(cSampleData, 7);
  lBytes := DataToBytes(cSampleData);
  for i := 0 to High(lBytes) do
  begin
    Assert.AreEqual<Byte>(lBytes[i], fBuffer.Cursor^, Format('Mismatch at index %d', [i]));
    if i < High(lBytes) then
    begin
      Assert.IsTrue(fBuffer.NextByte, 'NextByte should advance while data remains');
      Assert.AreEqual<Int64>(i + 1, fBuffer.Position, 'Position must advance with the cursor');
    end;
  end;
  Assert.IsFalse(fBuffer.NextByte, 'NextByte must report EOF once all bytes are consumed');
  Assert.AreEqual<Int64>(Length(lBytes), fBuffer.Position, 'Position should point to EOF');
end;

[Test]
procedure TBufferedFileTests.SeekForwardAndBackward;
var
  lBytes: TBytes;
begin
  PrepareBuffer(cSampleData, 6);
  lBytes := DataToBytes(cSampleData);

  fBuffer.Position := 15;
  Assert.AreEqual<Int64>(15, fBuffer.Position, 'Position should land on 15');
  Assert.AreEqual<Byte>(lBytes[15], fBuffer.Cursor^, 'Cursor should point to byte 15');

  fBuffer.Position := 3;
  Assert.AreEqual<Int64>(3, fBuffer.Position, 'Position should land on 3');
  Assert.AreEqual<Byte>(lBytes[3], fBuffer.Cursor^, 'Cursor should point to byte 3');
end;

[Test]
procedure TBufferedFileTests.SeekBeforeStartClampsToZero;
var
  lBytes: TBytes;
begin
  PrepareBuffer(cSampleData, 8);
  lBytes := DataToBytes(cSampleData);

  fBuffer.Position := 12;
  Assert.AreEqual<Int64>(12, fBuffer.Position, 'Precondition failed');

  try
    fBuffer.Seek(-20);
  except
    on E: Exception do
      Assert.Fail('Seek should clamp to the start, but raised ' + E.ClassName + ': ' + E.Message);
  end;

  Assert.AreEqual<Int64>(0, fBuffer.Position, 'Seek(-n) should clamp to the start');
  Assert.AreEqual<Byte>(lBytes[0], fBuffer.Cursor^, 'Cursor must point to the first byte after clamp');
end;

[Test]
procedure TBufferedFileTests.CopyBytesWithinBuffer;
var
  lActual: TBytes;
  lExpected: TBytes;
  lBytes: TBytes;
begin
  PrepareBuffer(cSampleData, 16);
  lBytes := DataToBytes(cSampleData);

  lExpected := Copy(lBytes, 2, 5);
  lActual := fBuffer.copyBytes(2, 5);

  AssertBytesEqual(lExpected, lActual, 'copyBytes (within buffer)');
end;

[Test]
procedure TBufferedFileTests.CopyBytesOutsideBufferUsesStream;
var
  lActual: TBytes;
  lExpected: TBytes;
  lBytes: TBytes;
  lStart: Integer;
  lCount: Integer;
  lOriginalPos: Int64;
begin
  PrepareBuffer(cSampleData, 8);
  lBytes := DataToBytes(cSampleData);

  lStart := 18;
  lCount := 6;
  lOriginalPos := fBuffer.Position;
  lActual := fBuffer.copyBytes(lStart, lCount);
  lExpected := Copy(lBytes, lStart, lCount);

  Assert.AreEqual<Int64>(lOriginalPos, fBuffer.Position, 'copyBytes must not change the cursor position');
  AssertBytesEqual(lExpected, lActual, 'copyBytes (stream fallback)');
end;

[Test]
procedure TBufferedFileTests.CopyRawByteStringMatches;
var
  lActual: RawByteString;
  lExpected: RawByteString;
  lStart: Integer;
  lCount: Integer;
begin
  PrepareBuffer(cSampleData, 8);

  lStart := 5;
  lCount := 10;

  lActual := fBuffer.CopyRawByteString(lStart, lCount);
  lExpected := Copy(cSampleData, lStart + 1, lCount);

  Assert.AreEqual<Integer>(Length(lExpected), Length(lActual), 'CopyRawByteString length mismatch');
  if lCount <> 0 then
    Assert.IsTrue(CompareMem(@lExpected[1], @lActual[1], lCount), 'CopyRawByteString data mismatch');
end;

end.
