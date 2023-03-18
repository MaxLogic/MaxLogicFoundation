unit maxLogic.Cipher.SCOP;
{$R-}
{$OVERFLOWCHECKS OFF}


{ Version: 1.0
  History:
  2017-06-17: for historical reason I need this cypher... but I decided to extract it from an older lib.
  This one will be maintained, as it is used in some legacy applications of mine, that still are develiopment.

  for in depth description see:
  https://groups.google.com/forum/#!topic/sci.crypt.research/ZD82NIacVmU

  NOTE: this implementation is platform independant. No assembly code any more

}
interface

uses
  sysUtils, classes;

const
  CheckCipherKeySize: Boolean = False;
  { set to True raises Exception when Size of the Key is too large, (Method Init())
    otherwise will truncate the Key, default mode is False }

type

  PIntArray = ^TIntArray;
  TIntArray = array [0 .. 1023] of LongWord;

  THash_RipeMD256 = class
  private
    FCount: LongWord;
    FBuffer: array [0 .. 63] of Byte;
    FDigest: array [0 .. 9] of LongWord;

    procedure Transform(Buffer: PIntArray);
  public
    const
    DigestKeySize = 32;

    destructor Destroy; override;

    procedure Init;

    procedure Calc(const Data; DataSize: Integer);
    procedure Done;
    function DigestKey: Pointer;
  end;

  TProgressCallback = procedure(sender: Tobject; const Current, Maximal: Integer) of object;

  TCipher = class
  private
    FHash: THash_RipeMD256;
    FKeySize: Integer;
    FBufSize: Integer;
    FUserSize: Integer;
    FBuffer: Pointer;
    FVector: Pointer;
    FFeedback: Pointer;
    FUser: Pointer;
    FFlags: Integer;
    FOnProgress: TProgressCallback;
    procedure InternalCodeStream(Source, Dest: TStream; DataSize: Integer; Encode: Boolean);
    procedure DoProgress(Current, Maximal: Integer);
    procedure InternalCodeFile(const Source, Dest: AnsiString; Encode: Boolean);
    procedure SetOnProgress(const Value: TProgressCallback);
  protected
    function GetFlag(Index: Integer): Boolean;
    procedure SetFlag(Index: Integer; Value: Boolean); virtual;
    { used in method Init() }
    procedure InitBegin(var Size: Integer);
    procedure InitEnd(IVector: Pointer); virtual;
    { must override }
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); virtual;

    { the encode function, must override }
    procedure Encode(Data: Pointer); virtual;
    { the decode function, must override }
    procedure Decode(Data: Pointer); virtual;
    { the individual Userdata and Buffer }
    property User: Pointer read FUser;
    property Buffer: Pointer read FBuffer;
    property UserSize: Integer read FUserSize;
  public
    constructor Create(const Password: AnsiString);
    destructor Destroy; override;
    class function MaxKeySize: Integer;

    { initialization form the Cipher }
    procedure Init(const Key; Size: Integer); virtual;
    procedure InitKey(const Key: AnsiString);
    { reset the Feedbackregister with the actual IVector }
    procedure Done; virtual;
    { protect the security Data's, Feedback, Buffer, Vector etc. }
    procedure Protect; virtual;

    procedure EncodeBuffer(const Source; var Dest; DataSize: Integer);
    procedure DecodeBuffer(const Source; var Dest; DataSize: Integer);

    procedure EncodeStream(const Source, Dest: TStream; DataSize: Integer);
    procedure DecodeStream(const Source, Dest: TStream; DataSize: Integer);

    { the maximal KeySize and BufSize (Size of Feedback, Buffer and Vector }
    property KeySize: Integer read FKeySize;
    property BufSize: Integer read FBufSize;

    { Init() was called }
    property Initialized: Boolean index 1 read GetFlag write SetFlag;
    { the actual IVector, BufSize Bytes long }
    property Vector: Pointer read FVector;
    { the Feedback register, BufSize Bytes long }
    property Feedback: Pointer read FFeedback;
    { the Key is set from InitKey() and the Hash.DigestKey^ include the encrypted Hash-Key }
    property HasHashKey: Boolean index 0 read GetFlag;
    property OnProgress: TProgressCallback read FOnProgress write SetOnProgress;
  end;

  TSCOPCipher = class(TCipher) { Stream Cipher in Blockmode }
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer); override;
    procedure Done; override;
  end;

  // some helper functions
  // XOR's Buffers I1 and I2 Size Bytes to Dest
procedure XORBuffers(const Source1, Source2: Pointer; Size: Integer; Dest: Pointer);
// scrambles a memory using a random seed and running with xor over the buffer
Procedure ScrambleBuffer(var Buffer; Size: Integer);

implementation

const
  sProtectionCircular = 'Circular Protection detected, Protection Object is invalid.';
  sInvalidMACMode = 'Invalid Ciphermode selected to produce a MAC.'#13#10 +
    'Please use Modes cmCBC, ThisOne, cmCFB, cmCBCMAC, cmCFBMAC or cmCTSMAC for CalcMAC.';
  sCantCalc = 'Invalid Ciphermode selected.';
  sNotInitialized = '%s is not initialized call Init() or InitKey() before.';
  sInvalidKeySize = 'Length from Encryptionkey is invalid.'#13#10 +
    'Keysize for %s must be to %d-%d bytes';

const
  SCOP_SIZE = 32; { is the Maximum }

procedure XORBuffers(const Source1, Source2: Pointer; Size: Integer; Dest: Pointer);
type
  myData = uint64;
const
  cBigBlock = SizeOf(myData);
var
  p1, p2, pDest: ^myData;
  x, Count: Integer;
  pb1, pb2, pbDest: pByte;
begin
  p1 := Source1;
  p2 := Source2;
  pDest := Dest;

  Count := Trunc(Size / cBigBlock);

  for x := 0 to Count - 1 do
  begin
    pDest^ := p1^ xor p2^;
    Inc(pDest);
    Inc(p1);
    Inc(p2);
  end;

  // and now the tail if any
  Count := Size - (Count * cBigBlock);
  if Count <> 0 then
  begin
    pb1 := pByte(p1);
    pb2 := pByte(p2);
    pbDest := pByte(pDest);

    for x := 0 to Count - 1 do
    begin
      pbDest^ := pb1^ xor pb2^;
      Inc(pbDest);
      Inc(pb1);
      Inc(pb2);
    end;
  end;

end;

Procedure ScrambleBuffer(var Buffer; Size: Integer);
var
  seed: uint64;
  pf: ^uint64;
  Count, x: Integer;
  pb, pbSeed: pByte;
  dt: double;
begin
  // init the seed with some semi random data
  seed := TThread.GetTickCount() OR (TThread.GetTickCount() shl 32);
  dt := now();
  seed := seed xor puInt64(@dt)^;

  Count := Trunc(Size / SizeOf(seed));
  pf := @Buffer;

  for x := 0 to Count - 1 do
  begin
    pf^ := pf^ xor seed;
    Inc(pf);
  end;

  Count := Size - (Count * SizeOf(seed));
  if Count <> 0 then
  begin
    pb := pByte(pf);
    pbSeed := @seed;

    for x := 0 to Count - 1 do
    begin
      pb^ := pb^ xor pbSeed^;
      Inc(pb);
      Inc(pbSeed);
    end;
  end;
end;

{ TCipher_SCOP }

class procedure TSCOPCipher.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := SCOP_SIZE * SizeOf(Integer);
  AKeySize := 48;
  AUserSize := (384 * 4 + 4 * SizeOf(Integer)) * 2;
end;

procedure TSCOPCipher.Encode(Data: Pointer);
var
  I, J, W: Byte;
  T, T1, T2, T3: Integer;
  P: PIntArray;
  B: PInteger;
begin
  P := User;
  I := P[0];
  J := P[1];
  T3 := P[3];
  P := @P[4 + 128];
  B := Data;
  for W := 1 to SCOP_SIZE do
  begin
    T1 := P[J];
    Inc(J, T3);
    T := P[I - 128];
    T2 := P[J];
    Inc(I);
    T3 := T2 + T;
    P[J] := T3;
    Inc(J, T2);
    Inc(B^, T1 + T2);
    Inc(B);
  end;
end;

procedure TSCOPCipher.Decode(Data: Pointer);
var
  I, J, W: Byte;
  T, T1, T2, T3: Integer;
  P: PIntArray;
  B: PInteger;
begin
  P := User;
  I := P[0];
  J := P[1];
  T3 := P[3];
  P := @P[4 + 128];
  B := Data;
  for W := 1 to SCOP_SIZE do
  begin
    T1 := P[J];
    Inc(J, T3);
    T := P[I - 128];
    T2 := P[J];
    Inc(I);
    T3 := T2 + T;
    P[J] := T3;
    Inc(J, T2);
    Dec(B^, T1 + T2);
    Inc(B);
  end;
end;

procedure TSCOPCipher.Init(const Key; Size: Integer);
var
  Init_State: packed record
    Coef: array [0 .. 7, 0 .. 3] of Byte;
  x: array [0 .. 3] of LongWord;
end;

procedure ExpandKey;
var
  P: PByteArray;
  I, C: Integer;
begin
  C := 1;
  P := @Init_State;
  Move(Key, P^, Size);
  for I := Size to 47 do
    P[I] := P[I - Size] + P[I - Size + 1];
  for I := 0 to 31 do
    if P[I] = 0 then
    begin
      P[I] := C;
      Inc(C);
    end;
end;

procedure GP8(Data: PIntArray);
var
  I, I2: Integer;
  NewX: array [0 .. 3] of LongWord;
  X1, X2, X3, X4: LongWord;
  Y1, Y2: LongWord;
begin
  I := 0;
  while I < 8 do
  begin
    I2 := I shr 1;
    X1 := Init_State.x[I2] shr 16;
    X2 := X1 * X1;
    X3 := X2 * X1;
    X4 := X3 * X1;
    Y1 := Init_State.Coef[I][0] * X4 +
      Init_State.Coef[I][1] * X3 +
      Init_State.Coef[I][2] * X2 +
      Init_State.Coef[I][3] * X1 + 1;
    X1 := Init_State.x[I2] and $FFFF;
    X2 := X1 * X1;
    X3 := X2 * X1;
    X4 := X3 * X1;
    Y2 := Init_State.Coef[I + 1][0] * X4 +
      Init_State.Coef[I + 2][1] * X3 +
      Init_State.Coef[I + 3][2] * X2 +
      Init_State.Coef[I + 4][3] * X1 + 1;
    Data[I2] := Y1 shl 16 or Y2 and $FFFF;
    NewX[I2] := Y1 and $FFFF0000 or Y2 shr 16;
    Inc(I, 2);
  end;
  Init_State.x[0] := NewX[0] shr 16 or NewX[3] shl 16;
  Init_State.x[1] := NewX[0] shl 16 or NewX[1] shr 16;
  Init_State.x[2] := NewX[1] shl 16 or NewX[2] shr 16;
  Init_State.x[3] := NewX[2] shl 16 or NewX[3] shr 16;
end;

var
  I, J: Integer;
  T: array [0 .. 3] of Integer;
  P: PIntArray;
begin
  InitBegin(Size);
  FillChar(Init_State, SizeOf(Init_State), 0);
  FillChar(T, SizeOf(T), 0);
  P := Pointer(pAnsiChar(User) + 12);
  ExpandKey;
  for I := 0 to 7 do
    GP8(@T);
  for I := 0 to 11 do
  begin
    for J := 0 to 7 do
      GP8(@P[I * 32 + J * 4]);
    GP8(@T);
  end;
  GP8(@T);
  I := T[3] and $7F;
  P[I] := P[I] or 1;
  P := User;
  P[0] := T[3] shr 24;
  P[1] := T[3] shr 16;
  P[2] := T[3] shr 8;
  FillChar(Init_State, SizeOf(Init_State), 0);
  InitEnd(nil);
  P := Pointer(pAnsiChar(User) + FUserSize shr 1);
  Move(User^, P^, FUserSize shr 1);
end;

procedure TSCOPCipher.Done;
begin
  inherited Done;
  Move(PByteArray(User)[FUserSize shr 1], User^, FUserSize shr 1);
end;

{ TCipher }

function TCipher.GetFlag(Index: Integer): Boolean;
begin
  Result := FFlags and (1 shl Index) <> 0;
end;

procedure TCipher.SetFlag(Index: Integer; Value: Boolean);
begin
  Index := 1 shl Index;
  if Value then
    FFlags := FFlags or Index
  else
    FFlags := FFlags and not Index;
end;

procedure TCipher.SetOnProgress(const Value: TProgressCallback);
begin
  FOnProgress := Value;
end;

procedure TCipher.InitBegin(var Size: Integer);
begin
  Initialized := False;
  Protect;
  if Size < 0 then
    Size := 0;
  if Size > KeySize then
    if not CheckCipherKeySize then
      Size := KeySize
    else
      raise exception.Create(Format(sInvalidKeySize, [ClassName, 0, KeySize]));
end;

procedure TCipher.InitEnd(IVector: Pointer);
begin
  if IVector = nil then
    Encode(Vector)
  else
    Move(IVector^, Vector^, BufSize);
  Move(Vector^, Feedback^, BufSize);
  Initialized := True;
end;

class procedure TCipher.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 0;
  AKeySize := 0;
  AUserSize := 0;
end;

procedure TCipher.Encode(Data: Pointer);
begin

end;

procedure TCipher.Decode(Data: Pointer);
begin
end;

constructor TCipher.Create(const Password: AnsiString);
begin
  inherited Create();
  FHash := THash_RipeMD256.Create();

  GetContext(FBufSize, FKeySize, FUserSize);
  GetMem(FVector, FBufSize);
  GetMem(FFeedback, FBufSize);
  GetMem(FBuffer, FBufSize);
  GetMem(FUser, FUserSize);
  Protect;
  if Password <> '' then
    InitKey(Password);
end;

destructor TCipher.Destroy;
begin
  Protect;

  ReallocMem(FVector, 0);
  ReallocMem(FFeedback, 0);
  ReallocMem(FBuffer, 0);
  ReallocMem(FUser, 0);

  if assigned(FHash) then
  begin
    FHash.free;
    FHash := nil;
  end;

  inherited Destroy;
end;

class function TCipher.MaxKeySize: Integer;
var
  Dummy: Integer;
begin
  GetContext(Dummy, Result, Dummy);
end;

procedure TCipher.Init(const Key; Size: Integer);
begin
end;

procedure TCipher.InitKey(const Key: AnsiString);
var
  I: Integer;
begin
  FHash.Init;
  FHash.Calc(pAnsiChar(Key)^, Length(Key));
  FHash.Done;

  I := FHash.DigestKeySize;
  if I > FKeySize then
    I := FKeySize; { generaly will truncate to large Keys }
  Init(FHash.DigestKey^, I);
  EncodeBuffer(FHash.DigestKey^, FHash.DigestKey^, FHash.DigestKeySize);
  Done;
  SetFlag(0, True);
end;

procedure TCipher.Done;
begin
  if CompareMem(FVector, FFeedback, FBufSize) then
    Exit;
  Move(FFeedback^, FBuffer^, FBufSize);
  Move(FVector^, FFeedback^, FBufSize);
end;

procedure TCipher.Protect;
begin
  SetFlag(0, False);
  Initialized := False;

  FillChar(FVector^, FBufSize, $AA);
  FillChar(FFeedback^, FBufSize, $AA);
  FillChar(FBuffer^, FBufSize, $AA);
  FillChar(FUser^, FUserSize, $AA);

  FillChar(FVector^, FBufSize, $55);
  FillChar(FFeedback^, FBufSize, $55);
  FillChar(FBuffer^, FBufSize, $55);
  FillChar(FUser^, FUserSize, $55);

  FillChar(FVector^, FBufSize, $FF);
  FillChar(FFeedback^, FBufSize, $FF);
  FillChar(FBuffer^, FBufSize, 0);
  FillChar(FUser^, FUserSize, 0);
end;

procedure TCipher.DoProgress(Current, Maximal: Integer);
begin
  if assigned(FOnProgress) then
    FOnProgress(self, Current, Maximal);

end;

procedure TCipher.InternalCodeStream(Source, Dest: TStream; DataSize: Integer; Encode: Boolean);
const
  maxBufSize = 1024 * 4;
var
  Buf: pAnsiChar;
  SPos: Integer;
  DPos: Integer;
  Len: Integer;
  Proc: procedure(const Source; var Dest; DataSize: Integer) of object;
  Size: Integer;
begin
  if Source = nil then
    Exit;

  if Encode then
    Proc := EncodeBuffer
  else
    Proc := DecodeBuffer;
  if Dest = nil then
    Dest := Source;
  if DataSize < 0 then
  begin
    DataSize := Source.Size;
    Source.Position := 0;
  end;
  Buf := nil;
  Size := DataSize;
  DoProgress(0, Size);
  try
    Buf := AllocMem(maxBufSize);
    DPos := Dest.Position;
    SPos := Source.Position;

    while DataSize > 0 do
    begin
      Source.Position := SPos;
      Len := DataSize;
      if Len > maxBufSize then
        Len := maxBufSize;
      Len := Source.Read(Buf^, Len);
      SPos := Source.Position;
      if Len <= 0 then
        Break;
      Proc(Buf^, Buf^, Len);
      Dest.Position := DPos;
      Dest.Write(Buf^, Len);
      DPos := Dest.Position;
      Dec(DataSize, Len);
      DoProgress(Size - DataSize, Size);
    end;
  finally
    DoProgress(0, 0);
    ReallocMem(Buf, 0);
  end;
end;

procedure TCipher.InternalCodeFile(const Source, Dest: AnsiString; Encode: Boolean);
var
  S, D: TFileStream;
begin
  S := nil;
  D := nil;
  try

    if (AnsiCompareText(Source, Dest) <> 0) and (Trim(Dest) <> '') then
    begin
      S := TFileStream.Create(Source, fmOpenRead or fmShareDenyNone);
      D := TFileStream.Create(Dest, fmCreate);
    end
    else
    begin
      S := TFileStream.Create(Source, fmOpenReadWrite);
      D := S;
    end;
    InternalCodeStream(S, D, -1, Encode);
  finally
    S.free;
    if S <> D then
    begin
{$IFDEF VER_D3H}
      D.Size := D.Position;
{$ENDIF}
      D.free;
    end;
  end;
end;

procedure TCipher.EncodeStream(const Source, Dest: TStream; DataSize: Integer);
begin
  InternalCodeStream(Source, Dest, DataSize, True);
end;

procedure TCipher.DecodeStream(const Source, Dest: TStream; DataSize: Integer);
begin
  InternalCodeStream(Source, Dest, DataSize, False);
end;

procedure TCipher.EncodeBuffer(const Source; var Dest; DataSize: Integer);
var
  S, D, f: pByte;
begin
  if not Initialized then
    raise exception.Create(Format(sNotInitialized, [ClassName]));

  S := @Source;
  D := @Dest;

  while DataSize >= FBufSize do
  begin
    XORBuffers(S, FFeedback, FBufSize, D);
    Encode(D);
    XORBuffers(D, FFeedback, FBufSize, FFeedback);
    Inc(S, FBufSize);
    Inc(D, FBufSize);
    Dec(DataSize, FBufSize);
  end;

  if DataSize > 0 then
  begin
    Move(FFeedback^, FBuffer^, FBufSize);
    Encode(FBuffer);
    XORBuffers(S, FBuffer, DataSize, D);
    XORBuffers(FBuffer, FFeedback, FBufSize, FFeedback);
  end;

end;

procedure TCipher.DecodeBuffer(const Source; var Dest; DataSize: Integer);
var
  S, D, f, B: pByte;
begin
  if not Initialized then
    raise exception.Create(Format(sNotInitialized, [ClassName]));
  S := @Source;
  D := @Dest;

  if S <> D then
    Move(S^, D^, DataSize);
  f := FFeedback;
  B := FBuffer;
  while DataSize >= FBufSize do
  begin
    XORBuffers(D, f, FBufSize, B);
    Decode(D);
    XORBuffers(D, f, FBufSize, D);
    S := B;
    B := f;
    f := S;
    Inc(D, FBufSize);
    Dec(DataSize, FBufSize);
  end;
  if f <> FFeedback then
    Move(f^, FFeedback^, FBufSize);
  if DataSize > 0 then
  begin
    Move(FFeedback^, FBuffer^, FBufSize);
    Encode(FBuffer);
    XORBuffers(FBuffer, D, DataSize, D);
    XORBuffers(FBuffer, FFeedback, FBufSize, FFeedback);
  end;

end;

{ TProtection }

// TProtection Class

{ THash_RipeMD256 }

procedure THash_RipeMD256.Transform(Buffer: PIntArray);
var
  A1, B1, C1, D1: LongWord;
  A2, B2, C2, D2: LongWord;
  T: LongWord;
begin
  A1 := FDigest[0];
  B1 := FDigest[1];
  C1 := FDigest[2];
  D1 := FDigest[3];
  A2 := FDigest[4];
  B2 := FDigest[5];
  C2 := FDigest[6];
  D2 := FDigest[7];

  Inc(A1, B1 xor C1 xor D1 + Buffer[0]);
  A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[1]);
  D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 xor A1 xor B1 + Buffer[2]);
  C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 xor D1 xor A1 + Buffer[3]);
  B1 := B1 shl 12 or B1 shr 20;
  Inc(A1, B1 xor C1 xor D1 + Buffer[4]);
  A1 := A1 shl 5 or A1 shr 27;
  Inc(D1, A1 xor B1 xor C1 + Buffer[5]);
  D1 := D1 shl 8 or D1 shr 24;
  Inc(C1, D1 xor A1 xor B1 + Buffer[6]);
  C1 := C1 shl 7 or C1 shr 25;
  Inc(B1, C1 xor D1 xor A1 + Buffer[7]);
  B1 := B1 shl 9 or B1 shr 23;
  Inc(A1, B1 xor C1 xor D1 + Buffer[8]);
  A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[9]);
  D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 xor A1 xor B1 + Buffer[10]);
  C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 xor D1 xor A1 + Buffer[11]);
  B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 xor C1 xor D1 + Buffer[12]);
  A1 := A1 shl 6 or A1 shr 26;
  Inc(D1, A1 xor B1 xor C1 + Buffer[13]);
  D1 := D1 shl 7 or D1 shr 25;
  Inc(C1, D1 xor A1 xor B1 + Buffer[14]);
  C1 := C1 shl 9 or C1 shr 23;
  Inc(B1, C1 xor D1 xor A1 + Buffer[15]);
  B1 := B1 shl 8 or B1 shr 24;

  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[5] + $50A28BE6);
  A2 := A2 shl 8 or A2 shr 24;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[14] + $50A28BE6);
  D2 := D2 shl 9 or D2 shr 23;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[7] + $50A28BE6);
  C2 := C2 shl 9 or C2 shr 23;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[0] + $50A28BE6);
  B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[9] + $50A28BE6);
  A2 := A2 shl 13 or A2 shr 19;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[2] + $50A28BE6);
  D2 := D2 shl 15 or D2 shr 17;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[11] + $50A28BE6);
  C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[4] + $50A28BE6);
  B2 := B2 shl 5 or B2 shr 27;
  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[13] + $50A28BE6);
  A2 := A2 shl 7 or A2 shr 25;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[6] + $50A28BE6);
  D2 := D2 shl 7 or D2 shr 25;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[15] + $50A28BE6);
  C2 := C2 shl 8 or C2 shr 24;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[8] + $50A28BE6);
  B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[1] + $50A28BE6);
  A2 := A2 shl 14 or A2 shr 18;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[10] + $50A28BE6);
  D2 := D2 shl 14 or D2 shr 18;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[3] + $50A28BE6);
  C2 := C2 shl 12 or C2 shr 20;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[12] + $50A28BE6);
  B2 := B2 shl 6 or B2 shr 26;

  T := A1;
  A1 := A2;
  A2 := T;

  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[7] + $5A827999);
  A1 := A1 shl 7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[4] + $5A827999);
  D1 := D1 shl 6 or D1 shr 26;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[13] + $5A827999);
  C1 := C1 shl 8 or C1 shr 24;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[1] + $5A827999);
  B1 := B1 shl 13 or B1 shr 19;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[10] + $5A827999);
  A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[6] + $5A827999);
  D1 := D1 shl 9 or D1 shr 23;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[15] + $5A827999);
  C1 := C1 shl 7 or C1 shr 25;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[3] + $5A827999);
  B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[12] + $5A827999);
  A1 := A1 shl 7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[0] + $5A827999);
  D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[9] + $5A827999);
  C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[5] + $5A827999);
  B1 := B1 shl 9 or B1 shr 23;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[2] + $5A827999);
  A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[14] + $5A827999);
  D1 := D1 shl 7 or D1 shr 25;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[11] + $5A827999);
  C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[8] + $5A827999);
  B1 := B1 shl 12 or B1 shr 20;

  Inc(A2, (B2 or not C2) xor D2 + Buffer[6] + $5C4DD124);
  A2 := A2 shl 9 or A2 shr 23;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[11] + $5C4DD124);
  D2 := D2 shl 13 or D2 shr 19;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[3] + $5C4DD124);
  C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[7] + $5C4DD124);
  B2 := B2 shl 7 or B2 shr 25;
  Inc(A2, (B2 or not C2) xor D2 + Buffer[0] + $5C4DD124);
  A2 := A2 shl 12 or A2 shr 20;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[13] + $5C4DD124);
  D2 := D2 shl 8 or D2 shr 24;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[5] + $5C4DD124);
  C2 := C2 shl 9 or C2 shr 23;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[10] + $5C4DD124);
  B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, (B2 or not C2) xor D2 + Buffer[14] + $5C4DD124);
  A2 := A2 shl 7 or A2 shr 25;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[15] + $5C4DD124);
  D2 := D2 shl 7 or D2 shr 25;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[8] + $5C4DD124);
  C2 := C2 shl 12 or C2 shr 20;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[12] + $5C4DD124);
  B2 := B2 shl 7 or B2 shr 25;
  Inc(A2, (B2 or not C2) xor D2 + Buffer[4] + $5C4DD124);
  A2 := A2 shl 6 or A2 shr 26;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[9] + $5C4DD124);
  D2 := D2 shl 15 or D2 shr 17;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[1] + $5C4DD124);
  C2 := C2 shl 13 or C2 shr 19;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[2] + $5C4DD124);
  B2 := B2 shl 11 or B2 shr 21;

  T := B1;
  B1 := B2;
  B2 := T;

  Inc(A1, (B1 or not C1) xor D1 + Buffer[3] + $6ED9EBA1);
  A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[10] + $6ED9EBA1);
  D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[14] + $6ED9EBA1);
  C1 := C1 shl 6 or C1 shr 26;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[4] + $6ED9EBA1);
  B1 := B1 shl 7 or B1 shr 25;
  Inc(A1, (B1 or not C1) xor D1 + Buffer[9] + $6ED9EBA1);
  A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[15] + $6ED9EBA1);
  D1 := D1 shl 9 or D1 shr 23;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[8] + $6ED9EBA1);
  C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[1] + $6ED9EBA1);
  B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, (B1 or not C1) xor D1 + Buffer[2] + $6ED9EBA1);
  A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[7] + $6ED9EBA1);
  D1 := D1 shl 8 or D1 shr 24;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[0] + $6ED9EBA1);
  C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[6] + $6ED9EBA1);
  B1 := B1 shl 6 or B1 shr 26;
  Inc(A1, (B1 or not C1) xor D1 + Buffer[13] + $6ED9EBA1);
  A1 := A1 shl 5 or A1 shr 27;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[11] + $6ED9EBA1);
  D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[5] + $6ED9EBA1);
  C1 := C1 shl 7 or C1 shr 25;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[12] + $6ED9EBA1);
  B1 := B1 shl 5 or B1 shr 27;

  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[15] + $6D703EF3);
  A2 := A2 shl 9 or A2 shr 23;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[5] + $6D703EF3);
  D2 := D2 shl 7 or D2 shr 25;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[1] + $6D703EF3);
  C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[3] + $6D703EF3);
  B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[7] + $6D703EF3);
  A2 := A2 shl 8 or A2 shr 24;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[14] + $6D703EF3);
  D2 := D2 shl 6 or D2 shr 26;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[6] + $6D703EF3);
  C2 := C2 shl 6 or C2 shr 26;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[9] + $6D703EF3);
  B2 := B2 shl 14 or B2 shr 18;
  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[11] + $6D703EF3);
  A2 := A2 shl 12 or A2 shr 20;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[8] + $6D703EF3);
  D2 := D2 shl 13 or D2 shr 19;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[12] + $6D703EF3);
  C2 := C2 shl 5 or C2 shr 27;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[2] + $6D703EF3);
  B2 := B2 shl 14 or B2 shr 18;
  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[10] + $6D703EF3);
  A2 := A2 shl 13 or A2 shr 19;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[0] + $6D703EF3);
  D2 := D2 shl 13 or D2 shr 19;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[4] + $6D703EF3);
  C2 := C2 shl 7 or C2 shr 25;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[13] + $6D703EF3);
  B2 := B2 shl 5 or B2 shr 27;

  T := C1;
  C1 := C2;
  C2 := T;

  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[1] + $8F1BBCDC);
  A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[9] + $8F1BBCDC);
  D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[11] + $8F1BBCDC);
  C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[10] + $8F1BBCDC);
  B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[0] + $8F1BBCDC);
  A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[8] + $8F1BBCDC);
  D1 := D1 shl 15 or D1 shr 17;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[12] + $8F1BBCDC);
  C1 := C1 shl 9 or C1 shr 23;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[4] + $8F1BBCDC);
  B1 := B1 shl 8 or B1 shr 24;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[13] + $8F1BBCDC);
  A1 := A1 shl 9 or A1 shr 23;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[3] + $8F1BBCDC);
  D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[7] + $8F1BBCDC);
  C1 := C1 shl 5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[15] + $8F1BBCDC);
  B1 := B1 shl 6 or B1 shr 26;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[14] + $8F1BBCDC);
  A1 := A1 shl 8 or A1 shr 24;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[5] + $8F1BBCDC);
  D1 := D1 shl 6 or D1 shr 26;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[6] + $8F1BBCDC);
  C1 := C1 shl 5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[2] + $8F1BBCDC);
  B1 := B1 shl 12 or B1 shr 20;

  Inc(A2, B2 xor C2 xor D2 + Buffer[8]);
  A2 := A2 shl 15 or A2 shr 17;
  Inc(D2, A2 xor B2 xor C2 + Buffer[6]);
  D2 := D2 shl 5 or D2 shr 27;
  Inc(C2, D2 xor A2 xor B2 + Buffer[4]);
  C2 := C2 shl 8 or C2 shr 24;
  Inc(B2, C2 xor D2 xor A2 + Buffer[1]);
  B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 xor C2 xor D2 + Buffer[3]);
  A2 := A2 shl 14 or A2 shr 18;
  Inc(D2, A2 xor B2 xor C2 + Buffer[11]);
  D2 := D2 shl 14 or D2 shr 18;
  Inc(C2, D2 xor A2 xor B2 + Buffer[15]);
  C2 := C2 shl 6 or C2 shr 26;
  Inc(B2, C2 xor D2 xor A2 + Buffer[0]);
  B2 := B2 shl 14 or B2 shr 18;
  Inc(A2, B2 xor C2 xor D2 + Buffer[5]);
  A2 := A2 shl 6 or A2 shr 26;
  Inc(D2, A2 xor B2 xor C2 + Buffer[12]);
  D2 := D2 shl 9 or D2 shr 23;
  Inc(C2, D2 xor A2 xor B2 + Buffer[2]);
  C2 := C2 shl 12 or C2 shr 20;
  Inc(B2, C2 xor D2 xor A2 + Buffer[13]);
  B2 := B2 shl 9 or B2 shr 23;
  Inc(A2, B2 xor C2 xor D2 + Buffer[9]);
  A2 := A2 shl 12 or A2 shr 20;
  Inc(D2, A2 xor B2 xor C2 + Buffer[7]);
  D2 := D2 shl 5 or D2 shr 27;
  Inc(C2, D2 xor A2 xor B2 + Buffer[10]);
  C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, C2 xor D2 xor A2 + Buffer[14]);
  B2 := B2 shl 8 or B2 shr 24;

  T := D1;
  D1 := D2;
  D2 := T;

  Inc(FDigest[0], A1);
  Inc(FDigest[1], B1);
  Inc(FDigest[2], C1);
  Inc(FDigest[3], D1);
  Inc(FDigest[4], A2);
  Inc(FDigest[5], B2);
  Inc(FDigest[6], C2);
  Inc(FDigest[7], D2);
end;

procedure THash_RipeMD256.Init;
begin
  FillChar(FBuffer, SizeOf(FBuffer), 0);
  FDigest[0] := $67452301;
  FDigest[1] := $EFCDAB89;
  FDigest[2] := $98BADCFE;
  FDigest[3] := $10325476;
  FDigest[4] := $76543210;
  FDigest[5] := $FEDCBA98;
  FDigest[6] := $89ABCDEF;
  FDigest[7] := $01234567;
  FDigest[8] := $01234567;
  FDigest[9] := $3C2D1E0F;
  FCount := 0;
end;

procedure THash_RipeMD256.Done;
var
  I: Integer;
  S: Comp;
begin
  I := FCount and $3F;
  FBuffer[I] := $80;
  Inc(I);
  if I > 64 - 8 then
  begin
    FillChar(FBuffer[I], 64 - I, 0);
    Transform(@FBuffer);
    I := 0;
  end;
  FillChar(FBuffer[I], 64 - I, 0);
  S := FCount * 8;
  Move(S, FBuffer[64 - 8], SizeOf(S));
  Transform(@FBuffer);
  FillChar(FBuffer, SizeOf(FBuffer), 0);
end;

procedure THash_RipeMD256.Calc(const Data; DataSize: Integer);
var
  Index: Integer;
  P: pAnsiChar;
begin
  if DataSize <= 0 then
    Exit;
  Index := FCount and $3F;
  Inc(FCount, DataSize);
  if Index > 0 then
  begin
    if DataSize < 64 - Index then
    begin
      Move(Data, FBuffer[Index], DataSize);
      Exit;
    end;
    Move(Data, FBuffer[Index], 64 - Index);
    Transform(@FBuffer);
    Index := 64 - Index;
    Dec(DataSize, Index);
  end;
  P := @TByteArray(Data)[Index];
  Inc(Index, DataSize and not $3F);
  while DataSize >= 64 do
  begin
    Transform(Pointer(P));
    Inc(P, 64);
    Dec(DataSize, 64);
  end;
  Move(TByteArray(Data)[Index], FBuffer, DataSize);
end;

function THash_RipeMD256.DigestKey: Pointer;
begin
  Result := @FDigest;
end;

destructor THash_RipeMD256.Destroy;
begin
  // clean data, so it will not remain in memory, makes it harder to crack
  FillChar(DigestKey^, DigestKeySize, 0);
  inherited Destroy;
end;

end.
