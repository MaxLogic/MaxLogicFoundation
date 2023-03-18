unit maxLogic.BitOperations;

interface

uses
  Math;  

type
  TBit = 0..1;

function Significance(DigitNr: Byte; Base: Byte): integer; overload;            //DigitNr: Index der Stelle von rechts, angefangen mit 0
function Significance(BitNr: Byte): integer; overload;                          //BitNr: Index des Bits von rechts, angefangen mit 0
procedure SetBit(var Value: longint; BitNr: Byte; NewBit: TBit);                //BitNr: Index des Bits von rechts, angefangen mit 0; NewBit: Neuer Wert des Bits, das verändert werden soll 
function GetBit(Value: longint; BitNr: Byte): TBit;                             //BitNr: Index des Bits von rechts, angefangen mit 0
function Max(nBits: Byte): LongInt;                                             //Gibt den maximalen Wert zurück, der mit nBits dargestellt werden kann
function CharToBit(chr: Char): TBit;
function BitToChar(Bit: TBit): Char;
function BitToBool(Bit: TBit): Boolean;
function BoolToBit(Bool: Boolean): TBit;
function BitToStr(Bit: TBit; ReturnBoolStr: Boolean = False): string;           //ReturnBoolStr: Gibt an, ob in dem String ('True' or 'False') oder ('1' or '0') stehen soll
function InvertBit(Bit: TBit): TBit;                                            //Invertiert das Bit

implementation

function Significance(DigitNr: Byte; Base: Byte): integer;
begin
  result := Round(Power(Base,DigitNr));
end;

function Significance(BitNr: Byte): integer;
begin
  result := Round(Significance(BitNr,2));
end;

procedure SetBit(var Value: longint; BitNr: Byte; NewBit: TBit);
begin
  case NewBit of
    0: Value := Value and (not Significance(BitNr)){111101111};
    1: Value := Value or Significance(BitNr)       {000010000};
  end;                                            
end;

function GetBit(Value: longint; BitNr: Byte): TBit;
begin
  result := Value and Significance(BitNr);
end;

function Max(nBits: Byte): LongInt;
begin
  result := Round(Power(2,nBits) - 1);
end;

function CharToBit(chr: Char): TBit;
begin
  case chr of
    '0': result := 0;
    '1': result := 1;
    else result := 0;
  end;
end;

function BitToChar(Bit: TBit): Char;
begin
  case Bit of
    0: result := '0';
    1: result := '1';
  end;
end;

function BitToBool(Bit: TBit): Boolean;
begin
  case Bit of
    0: result := False;
    1: result := True;
  end;
end;

function BoolToBit(Bool: Boolean): TBit;
begin
  case Bool of
    False: result := 0;
    True: result := 1;
  end;
end;

function BitToStr(Bit: TBit; ReturnBoolStr: Boolean = False): string;
begin
  if ReturnBoolStr then case Bit of
    0: result := 'FALSE';
    1: result := 'TRUE';
  end else case Bit of
    0: result := '0';
    1: result := '1';
  end;
end;

function InvertBit(Bit: TBit): TBit;
begin
  if Bit = 0 then result := 1 else result := 0;
end;

end.
