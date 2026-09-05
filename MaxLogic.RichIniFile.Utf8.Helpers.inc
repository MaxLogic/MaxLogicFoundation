// Private UTF8 operations; document algorithms live in the shared include.
type
  TRichIniBuilder = class
  private
    fBuffer: UTF8String;
    fLength: Integer;
    procedure Reserve(aAdditional: Integer);
  public
    constructor Create(aCapacity: Integer = 0);
    procedure Append(const aValue: UTF8String); overload;
    procedure Append(c: AnsiChar); overload;
    function ToString: UTF8String; reintroduce;
  end;

constructor TRichIniBuilder.Create(aCapacity: Integer);
begin
  inherited Create;
  SetLength(fBuffer, aCapacity);
end;

procedure TRichIniBuilder.Reserve(aAdditional: Integer);
var
  lCapacity, lRequired: Integer;
begin
  if aAdditional > MaxInt - fLength then
    raise EOutOfMemory.Create('INI text exceeds maximum string length');
  lRequired := fLength + aAdditional;
  if lRequired <= Length(fBuffer) then
    Exit;
  lCapacity := Length(fBuffer);
  if lCapacity <= MaxInt div 2 then
    lCapacity := lCapacity * 2;
  if lCapacity < 64 then
    lCapacity := 64;
  if lCapacity < lRequired then
    lCapacity := lRequired;
  SetLength(fBuffer, lCapacity);
end;

procedure TRichIniBuilder.Append(const aValue: UTF8String);
var
  lLength: Integer;
begin
  lLength := Length(aValue);
  if lLength = 0 then
    Exit;
  Reserve(lLength);
  Move(PAnsiChar(aValue)^, fBuffer[fLength + 1], lLength);
  Inc(fLength, lLength);
end;

procedure TRichIniBuilder.Append(c: AnsiChar);
begin
  Reserve(1);
  Inc(fLength);
  fBuffer[fLength] := c;
end;

function TRichIniBuilder.ToString: UTF8String;
begin
  // Finish once: return a reference to the buffer, without copying the document.
  SetLength(fBuffer, fLength);
  Result := fBuffer;
end;

function IniTrim(const aValue: UTF8String): UTF8String;
var
  lFirst, lLast: Integer;
begin
  lFirst := 1;
  lLast := Length(aValue);
  while (lFirst <= lLast) and (aValue[lFirst] <= #32) do
    Inc(lFirst);
  while (lLast >= lFirst) and (aValue[lLast] <= #32) do
    Dec(lLast);
  if (lFirst = 1) and (lLast = Length(aValue)) then
    Result := aValue
  else
    Result := Copy(aValue, lFirst, lLast - lFirst + 1);
end;

function IniSameText(const aLeft, aRight: UTF8String): Boolean;
begin
  Result := TFastCaseAwareComparer.OrdinalIgnoreCaseUtf8.Equals(aLeft, aRight);
end;

function IniIntToStr(aValue: Integer): UTF8String;
var
  lBuffer: ShortString;
begin
  Str(aValue, lBuffer);
  SetString(Result, PAnsiChar(@lBuffer[1]), Length(lBuffer));
end;

function IniStrToIntDef(const aValue: UTF8String; aDefault: Integer): Integer;
var
  i, lLength, lBase, lDigit, lStart: Integer;
  lNegative: Boolean;
  lNumber, lLimit: UInt64;
  lSigned: Int64;
  c: AnsiChar;
begin
  Result := aDefault;
  i := 1;
  lLength := Length(aValue);
  while (i <= lLength) and (aValue[i] = ' ') do
    Inc(i);
  if i > lLength then
    Exit;
  lNegative := aValue[i] = '-';
  if lNegative or (aValue[i] = '+') then
    Inc(i);
  lBase := 10;
  if (i < lLength) and (aValue[i] = '0') and (aValue[i + 1] in ['x', 'X']) then
    Inc(i);
  if (i <= lLength) and (aValue[i] in ['$', 'x', 'X']) then
  begin
    lBase := 16;
    Inc(i);
  end;
  if lBase = 16 then
    lLimit := High(Cardinal)
  else
    lLimit := UInt64(High(Integer)) + UInt64(Ord(lNegative));
  lNumber := 0;
  lStart := i;
  while (i <= lLength) and (aValue[i] <> #0) do
  begin
    c := aValue[i];
    case c of
      '0'..'9': lDigit := Ord(c) - Ord('0');
      'a'..'f': lDigit := Ord(c) - Ord('a') + 10;
      'A'..'F': lDigit := Ord(c) - Ord('A') + 10;
    else
      Exit;
    end;
    if lDigit >= lBase then
      Exit;
    lNumber := lNumber * UInt64(lBase) + UInt64(lDigit);
    if lNumber > lLimit then
      Exit;
    Inc(i);
  end;
  if i = lStart then
    Exit;
  lSigned := Int64(lNumber);
  if lNegative then
    lSigned := -lSigned;
  // Hexadecimal accepts the full 32-bit bit pattern, as Delphi StrToIntDef does.
  if lSigned > High(Integer) then
    Dec(lSigned, Int64(High(Cardinal)) + 1)
  else if lSigned < Low(Integer) then
    Inc(lSigned, Int64(High(Cardinal)) + 1);
  Result := Integer(lSigned);
end;

function IniReplace(const aValue, aOld, aNew: UTF8String; aFlags: TReplaceFlags): UTF8String;
var
  g: TGarbos;
  lBuilder: TRichIniBuilder;
  lStart, lPos: Integer;
begin
  lPos := Pos(aOld, aValue);
  if (aOld = '') or (lPos = 0) then
    Exit(aValue);
  GC(lBuilder, TRichIniBuilder.Create(Length(aValue)), g);
  lStart := 1;
  repeat
    lBuilder.Append(Copy(aValue, lStart, lPos - lStart));
    lBuilder.Append(aNew);
    lStart := lPos + Length(aOld);
    if not (rfReplaceAll in aFlags) then
      Break;
    lPos := Pos(aOld, aValue, lStart);
  until lPos = 0;
  lBuilder.Append(Copy(aValue, lStart, MaxInt));
  Result := lBuilder.ToString;
end;

function IniDecode(aEncoding: TEncoding; const aBytes: TBytes; aOffset: Integer = 0;
  aCount: Integer = -1): UTF8String;
begin
  if aCount < 0 then
    aCount := Length(aBytes) - aOffset;
  if aCount = 0 then
    Exit('');
  if aEncoding = TEncoding.UTF8 then
    SetString(Result, PAnsiChar(@aBytes[aOffset]), aCount)
  else
    Result := UTF8String(aEncoding.GetString(aBytes, aOffset, aCount));
end;
