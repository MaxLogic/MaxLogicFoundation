unit MaxLogic.EanUtils;

{
  baed on https://www.gs1.org/services/how-calculate-check-digit-manually
}

interface

uses
  sysUtils;

type
  TCheckSumResult = (crOK, // input was 12 digits long all digits were numerical
    crEanTooLong, // the ChecksumDigit is calculated only for the first 12 characters
    crEanTooShort, // the CheckSumDigit was calculated with what is there
    crNonNumericValuesFound // no checkSum will be calculated
    );

  TEanTyp = (
    GTIN8 = 8,
    GTIN12 = 12,
    GTIN13 = 13,
    GTIN14 = 14,
    GSIN = 17,
    SSCC = 18);

  TEan = record
  private
    FEanKeyWithoutCheckDigit: String;
    fKind: TEanTyp;
    fCheckDigit: String;
    FErrorCode: TCheckSumResult;
    procedure SetEanKeyWithoutCheckDigit(const Value: String);
  public
    property Kind: TEanTyp read fKind;
    // error code for the last calculation of the checkDigit
    property ErrorCode: TCheckSumResult read FErrorCode;

    // will auto trigger a calculation of the checkDigit
    property EanKeyWithoutCheckDigit: String read FEanKeyWithoutCheckDigit write SetEanKeyWithoutCheckDigit;

    property CheckDigit: String read fCheckDigit;

    Function FullKey: String;
    // returns the CheckDigit or "" in case of an error
    Function Calc(const aKeyWithoutCheckDigit: String): String; overload;
    procedure Calc; overload;

    // aKey can be with or without ChecksumDigit
    Function Expand(const aKey: String): String;
    // aKey can be with or without ChecksumDigit
    // there is also a fullback comparision to plain text just in case we compare two invalid eans (in other words where the input is like a product number instead of the EAN)
    Function Same(const aKey1, aKey2: String): Boolean;
    // checks if the ChecksumDigit is valid. Obviously aKey is with CheckDigit
    Function ValidCheckDigit(const aKey: String): Boolean;

    class Function Create(aKind: TEanTyp): TEan; static;
  end;

  TEans = Class
  public
    class function GTIN8: TEan;
    class function GTIN12: TEan;
    class function GTIN13: TEan;
    class function GTIN14: TEan;
    class function GSIN: TEan;
    class function SSCC: TEan;

  end;

  // aCheckSumDigit will be "" on crNonNumericValuesFound or if the given string is empty
  // this function can calculate the check digit for all of the EAN types, because it is always calculating from the right side and taking all the digits into account.
Function CalcEanCheckDigit(const aEanWithoutCheckDigit: String): String;

implementation

uses
  strUtils, math, clipBrd;

Function CalcEanCheckDigit(const aEanWithoutCheckDigit: String): String;
Var
  digit, i, sum: Integer;
  switch: Boolean;
begin
  result := '';
  if aEanWithoutCheckDigit = '' then
    exit;

  // init values
  sum := 0;
  switch := True;

  For i := length(aEanWithoutCheckDigit) downto 1 do
  begin
    digit := StrToIntDef(aEanWithoutCheckDigit[i], -1);
    if digit = -1 then // invalid input
      exit;

    if switch then
      inc(sum, (digit * 3))
    else
      inc(sum, digit);
    switch := not switch;
  end;

  if (sum mod 10) = 0 then
    sum := 0
  else
    sum := (10 - (sum mod 10));

  result := sum.ToString;
end;

// just for debuging to see if the values are calculated properly
procedure selfTest;
var
  eans: array of String;
  x: Integer;
  messages: String;
begin
  eans := [
    '4260047986217',
    '4260047981878',
    '4260047987290',
    '4003699105564',
    '4260047988839',
    '4260047987054',
    '4260047986385',
    '4003699005659',
    '4260047988716',
    '4260047986798',
    '4260047984923',
    '4003699000180',
    '4003699000432',
    '4260047987580',
    '9780521425575',
    '97814053222744'
    ];

  messages := '';
  for x := 0 to length(eans) - 1 do
  if length(eans[x])=13 then
  begin
    if eans[x] <> TEans.GTIN13.Expand(copy(eans[x], 1, 12)) then
    begin
      messages := messages + 'in: ' + eans[x] + sLineBreak;
      messages := messages +
        TEans.GTIN13.Expand(copy(eans[x], 1, 12)) +
        sLineBreak;
    end;
  end;
  clipboard.asText := messages;
end;

{ TEans }

{ TEan }

class function TEan.Create(aKind: TEanTyp): TEan;
begin
  result := default (TEan);
  result.fKind := aKind;
end;

procedure TEan.SetEanKeyWithoutCheckDigit(const Value: String);
begin
  FEanKeyWithoutCheckDigit := Value;
  Calc;
end;

function TEan.FullKey: String;
begin
  result := FEanKeyWithoutCheckDigit + fCheckDigit;
end;

function TEan.Calc(const aKeyWithoutCheckDigit: String): String;
begin
  FEanKeyWithoutCheckDigit := aKeyWithoutCheckDigit;
  Calc;
  result := fCheckDigit;
end;

function TEan.Expand(const aKey: String): String;
begin
  if length(aKey) = ord(fKind) - 1 then
    result := aKey + CalcEanCheckDigit(aKey)
  else
    result := aKey;
end;

function TEan.Same(const aKey1, aKey2: String): Boolean;
begin
  result := sameText(Expand(aKey1), Expand(aKey2));
end;

function TEan.ValidCheckDigit(const aKey: String): Boolean;
begin
  result := False;

  if length(aKey) = ord(fKind) then

    if CalcEanCheckDigit(copy(aKey, 1, length(aKey) - 1)) =
      copy(aKey, length(aKey), 1) then
      result := True;
end;

procedure TEan.Calc;
begin
  if length(FEanKeyWithoutCheckDigit) < ord(fKind) - 1 then
    FErrorCode := crEanTooShort
  else if length(FEanKeyWithoutCheckDigit) > ord(fKind) - 1 then
    FErrorCode := crEanTooLong
  else
    FErrorCode := crOK;

  fCheckDigit := CalcEanCheckDigit(FEanKeyWithoutCheckDigit);
  if fCheckDigit = '' then
    FErrorCode := crNonNumericValuesFound;
end;

{ TEans }

class function TEans.GTIN8: TEan;
begin
  result := TEan.Create(TEanTyp.GTIN8);
end;

class function TEans.GTIN12: TEan;
begin
  result := TEan.Create(TEanTyp.GTIN12);
end;

class function TEans.GTIN13: TEan;
begin
  result := TEan.Create(TEanTyp.GTIN13);
end;

class function TEans.GTIN14: TEan;
begin
  result := TEan.Create(TEanTyp.GTIN14);
end;

class function TEans.GSIN: TEan;
begin
  result := TEan.Create(TEanTyp.GSIN);
end;

class function TEans.SSCC: TEan;
begin
  result := TEan.Create(TEanTyp.SSCC);
end;

{initialization

selfTest;}

end.