Unit MaxLogic.DateUtils;

{
  Version: 1.1

}

Interface

uses
  SYSutils, classes, MaxLogic.sTRuTILS;

Function etime(aDate: TDateTime): String; Overload;
Function etime: String; Overload;
Function StrETimeToDateTime(Const etime: String): TDateTime;
Function SecToStrTime(sec: double): String;
Function msToPrettyStr(Const ms: int64): String;
Function eTimeWithTimeZone(aDate: TDateTime): String;

// a bit more advanced parsing
// returns 0 if the date could not be properly parsed
function ParseDateTime(const sDate: string): TDateTime;
function TryParseDateTimeUtc(const aValue: string; out aUtcDateTime: TDateTime): Boolean;
function TryParseDateTimeBerlin(const aValue: string; out aBerlinDateTime: TDateTime): Boolean;
function FormatGermanDateTime(const aDateTime: TDateTime): string;
function FormatGermanDateTimeUtc(const aUtcDateTime: TDateTime): string;

Implementation

Uses
  DateUtils, sTRuTILS, autoFree
  {$IFDEF MSWINDOWS}
  , Winapi.Windows, System.Generics.Collections, System.Win.Registry
  {$ENDIF};

type
  TBerlinLocalTimeType = (bltStandard, bltDaylight, bltInvalid, bltAmbiguous);

{$IFDEF MSWINDOWS}
type
  TRegistryTimeZoneInformation = record
    Bias: LongInt;
    StandardBias: LongInt;
    DaylightBias: LongInt;
    StandardDate: TSystemTime;
    DaylightDate: TSystemTime;
  end;

var
  gBerlinYearCache: TDictionary<Word, TTimeZoneInformation>;

{$ENDIF}

function TryParseFixedDigits(const aValue: string; const aStart, aCount: Integer;
  out aNumber: Word): Boolean;
var
  lIndex: Integer;
  lNumber: Integer;
begin
  Result := False;
  aNumber := 0;
  if (aStart < 1) or (aCount < 1) or (aStart + aCount - 1 > Length(aValue)) then
    Exit;

  lNumber := 0;
  for lIndex := aStart to aStart + aCount - 1 do
  begin
    if not CharInSet(aValue[lIndex], ['0'..'9']) then
      Exit;
    lNumber := (lNumber * 10) + Ord(aValue[lIndex]) - Ord('0');
  end;
  aNumber := Word(lNumber);
  Result := True;
end;

function IsFourDigitDateTime(const aDateTime: TDateTime): Boolean;
begin
  Result := (CompareDateTime(aDateTime, EncodeDate(1, 1, 1)) >= 0)
    and (CompareDateTime(aDateTime,
      EncodeDateTime(9999, 12, 31, 23, 59, 59, 999)) <= 0);
end;

function TryParseStrictDateTime(const aValue: string; out aDateTime: TDateTime;
  out aHasExplicitOffset: Boolean): Boolean;
var
  lCanonical: string;
  lDay: Word;
  lFractionDigits: Integer;
  lHour: Word;
  lIndex: Integer;
  lIsIso: Boolean;
  lMillisecond: Word;
  lMinute: Word;
  lMonth: Word;
  lOffsetHour: Word;
  lOffsetMinute: Word;
  lOffsetMinutes: Int64;
  lSecond: Word;
  lValue: string;
  lWallDateTime: TDateTime;
  lYear: Word;
begin
  Result := False;
  aDateTime := 0;
  aHasExplicitOffset := False;
  lOffsetMinutes := 0;
  lValue := Trim(aValue);
  if Length(lValue) < 10 then
    Exit;

  lIsIso := (lValue[5] = '-') and (lValue[8] = '-');
  if lIsIso then
  begin
    if not TryParseFixedDigits(lValue, 1, 4, lYear)
      or not TryParseFixedDigits(lValue, 6, 2, lMonth)
      or not TryParseFixedDigits(lValue, 9, 2, lDay) then
      Exit;
  end else begin
    if (lValue[3] <> '.') or (lValue[6] <> '.')
      or not TryParseFixedDigits(lValue, 1, 2, lDay)
      or not TryParseFixedDigits(lValue, 4, 2, lMonth)
      or not TryParseFixedDigits(lValue, 7, 4, lYear) then
      Exit;
  end;

  if lYear = 0 then
    Exit;

  lHour := 0;
  lMinute := 0;
  lSecond := 0;
  lMillisecond := 0;
  lIndex := 11;
  if lIndex <= Length(lValue) then
  begin
    if lIsIso then
    begin
      if not CharInSet(lValue[lIndex], ['T', ' ']) then
        Exit;
    end else if lValue[lIndex] <> ' ' then
      Exit;

    if (Length(lValue) < 16)
      or not TryParseFixedDigits(lValue, 12, 2, lHour)
      or (lValue[14] <> ':')
      or not TryParseFixedDigits(lValue, 15, 2, lMinute) then
      Exit;
    lIndex := 17;

    if (lIndex <= Length(lValue)) and (lValue[lIndex] = ':') then
    begin
      if not TryParseFixedDigits(lValue, lIndex + 1, 2, lSecond) then
        Exit;
      Inc(lIndex, 3);
      if (lIndex <= Length(lValue)) and (lValue[lIndex] = '.') then
      begin
        Inc(lIndex);
        lFractionDigits := 0;
        while (lIndex + lFractionDigits <= Length(lValue))
          and CharInSet(lValue[lIndex + lFractionDigits], ['0'..'9']) do
          Inc(lFractionDigits);
        if not (lFractionDigits in [1, 2, 3]) then
          Exit;
        if not TryParseFixedDigits(lValue, lIndex, lFractionDigits, lMillisecond) then
          Exit;
        if lFractionDigits = 1 then
          lMillisecond := lMillisecond * 100
        else if lFractionDigits = 2 then
          lMillisecond := lMillisecond * 10;
        Inc(lIndex, lFractionDigits);
      end;
    end;

    if lIndex <= Length(lValue) then
    begin
      if not lIsIso then
        Exit;
      if lValue[lIndex] = 'Z' then
      begin
        if lIndex <> Length(lValue) then
          Exit;
        aHasExplicitOffset := True;
      end else if CharInSet(lValue[lIndex], ['+', '-']) then
      begin
        if (lIndex + 5 <> Length(lValue))
          or not TryParseFixedDigits(lValue, lIndex + 1, 2, lOffsetHour)
          or (lValue[lIndex + 3] <> ':')
          or not TryParseFixedDigits(lValue, lIndex + 4, 2, lOffsetMinute)
          or (lOffsetHour > 14) or (lOffsetMinute > 59)
          or ((lOffsetHour = 14) and (lOffsetMinute <> 0)) then
          Exit;
        aHasExplicitOffset := True;
        lOffsetMinutes := (Int64(lOffsetHour) * MinsPerHour) + lOffsetMinute;
        if lValue[lIndex] = '-' then
          lOffsetMinutes := -lOffsetMinutes;
      end else
        Exit;
    end;
  end;

  if not TryEncodeDateTime(lYear, lMonth, lDay, lHour, lMinute, lSecond,
    lMillisecond, aDateTime) then
    Exit;
  lWallDateTime := aDateTime;

  if aHasExplicitOffset then
  begin
    if (lOffsetMinutes > 0)
      and (CompareDateTime(lWallDateTime,
        IncMinute(EncodeDate(1, 1, 1), lOffsetMinutes)) < 0) then
      Exit;
    if (lOffsetMinutes < 0)
      and (CompareDateTime(lWallDateTime,
        IncMinute(EncodeDateTime(9999, 12, 31, 23, 59, 59, 999),
          lOffsetMinutes)) > 0) then
      Exit;
  end;

  if lIsIso then
    lCanonical := Copy(lValue, 1, 10)
  else
    lCanonical := Format('%.4d-%.2d-%.2d', [lYear, lMonth, lDay]);
  if Length(lValue) > 10 then
  begin
    lCanonical := lCanonical + 'T' + Copy(lValue, 12, Length(lValue) - 11);
    if not TryISO8601ToDate(lCanonical, aDateTime, [ioReturnUTC]) then
      Exit;
  end else if not TryISO8601ToDate(lCanonical, aDateTime, [ioReturnUTC]) then
    Exit;

  if aHasExplicitOffset then
    aDateTime := IncMinute(lWallDateTime, -lOffsetMinutes);
  Result := IsFourDigitDateTime(aDateTime);
end;

{$IFDEF MSWINDOWS}
function TryGetBerlinYearInformation(const aYear: Word;
  out aTimeZone: TTimeZoneInformation): Boolean;
const
  cBerlinKey = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Time Zones\W. Europe Standard Time';
var
  lFirstYear: Integer;
  lLastYear: Integer;
  lRegistry: TRegistry;
  lRegistryTimeZone: TRegistryTimeZoneInformation;
  lRuleYear: Integer;
  lValueName: string;
begin
  Result := False;
  aTimeZone := Default(TTimeZoneInformation);
  TMonitor.Enter(gBerlinYearCache);
  try
    if gBerlinYearCache.TryGetValue(aYear, aTimeZone) then
      Exit(True);
  finally
    TMonitor.Exit(gBerlinYearCache);
  end;

  lRegistry := TRegistry.Create(KEY_READ);
  try
    try
      lRegistry.RootKey := HKEY_LOCAL_MACHINE;
      lRegistryTimeZone := Default(TRegistryTimeZoneInformation);
      if lRegistry.OpenKeyReadOnly(cBerlinKey + '\Dynamic DST') then
      begin
        lFirstYear := lRegistry.ReadInteger('FirstEntry');
        lLastYear := lRegistry.ReadInteger('LastEntry');
        lRuleYear := aYear;
        if lRuleYear < lFirstYear then
          lRuleYear := lFirstYear
        else if lRuleYear > lLastYear then
          lRuleYear := lLastYear;
        lValueName := IntToStr(lRuleYear);
        if lRegistry.ReadBinaryData(lValueName, lRegistryTimeZone,
          SizeOf(lRegistryTimeZone)) <> SizeOf(lRegistryTimeZone) then
          Exit;
      end else begin
        if not lRegistry.OpenKeyReadOnly(cBerlinKey)
          or (lRegistry.ReadBinaryData('TZI', lRegistryTimeZone,
            SizeOf(lRegistryTimeZone)) <> SizeOf(lRegistryTimeZone)) then
          Exit;
      end;

      aTimeZone.Bias := lRegistryTimeZone.Bias;
      aTimeZone.StandardBias := lRegistryTimeZone.StandardBias;
      aTimeZone.DaylightBias := lRegistryTimeZone.DaylightBias;
      aTimeZone.StandardDate := lRegistryTimeZone.StandardDate;
      aTimeZone.DaylightDate := lRegistryTimeZone.DaylightDate;
      Result := True;
    except
      Result := False;
    end;
  finally
    lRegistry.Free;
  end;

  if Result then
  begin
    TMonitor.Enter(gBerlinYearCache);
    try
      gBerlinYearCache.AddOrSetValue(aYear, aTimeZone);
    finally
      TMonitor.Exit(gBerlinYearCache);
    end;
  end;
end;

function TryGetTransition(const aYear: Word; const aRule: TSystemTime;
  out aTransition: TDateTime): Boolean;
var
  lDay: Integer;
  lFirstDayOfWeek: Integer;
begin
  Result := False;
  aTransition := 0;
  if aRule.wMonth = 0 then
    Exit;

  if aRule.wYear <> 0 then
    Exit(TryEncodeDateTime(aRule.wYear, aRule.wMonth, aRule.wDay,
      aRule.wHour, aRule.wMinute, aRule.wSecond, aRule.wMilliseconds,
      aTransition));

  lFirstDayOfWeek := DayOfWeek(EncodeDate(aYear, aRule.wMonth, 1)) - 1;
  lDay := 1 + ((Integer(aRule.wDayOfWeek) - lFirstDayOfWeek + 7) mod 7)
    + (7 * (Integer(aRule.wDay) - 1));
  if lDay > DaysInAMonth(aYear, aRule.wMonth) then
    Dec(lDay, 7);
  Result := TryEncodeDateTime(aYear, aRule.wMonth, lDay,
    aRule.wHour, aRule.wMinute, aRule.wSecond, aRule.wMilliseconds,
    aTransition);
end;

function GetBerlinLocalTimeType(const aLocalDateTime: TDateTime;
  const aTimeZone: TTimeZoneInformation): TBerlinLocalTimeType;
var
  lDaylightOffset: Int64;
  lEndOfDaylight: TDateTime;
  lSave: Int64;
  lStartOfDaylight: TDateTime;
  lStandardOffset: Int64;
  lYear: Word;
begin
  Result := bltStandard;
  lYear := YearOf(aLocalDateTime);
  if not TryGetTransition(lYear, aTimeZone.DaylightDate, lStartOfDaylight)
    or not TryGetTransition(lYear, aTimeZone.StandardDate, lEndOfDaylight) then
    Exit;

  lStandardOffset := -(aTimeZone.Bias + aTimeZone.StandardBias);
  lDaylightOffset := -(aTimeZone.Bias + aTimeZone.DaylightBias);
  lSave := lDaylightOffset - lStandardOffset;
  if lSave = 0 then
    Exit;

  if lSave > 0 then
  begin
    if (CompareDateTime(aLocalDateTime, lStartOfDaylight) >= 0)
      and (CompareDateTime(aLocalDateTime, IncMinute(lStartOfDaylight, lSave)) < 0) then
      Exit(bltInvalid);
    if (CompareDateTime(aLocalDateTime, IncMinute(lEndOfDaylight, -lSave)) >= 0)
      and (CompareDateTime(aLocalDateTime, lEndOfDaylight) < 0) then
      Exit(bltAmbiguous);
  end else begin
    if (CompareDateTime(aLocalDateTime, IncMinute(lStartOfDaylight, lSave)) >= 0)
      and (CompareDateTime(aLocalDateTime, lStartOfDaylight) < 0) then
      Exit(bltInvalid);
    if (CompareDateTime(aLocalDateTime, lEndOfDaylight) >= 0)
      and (CompareDateTime(aLocalDateTime, IncMinute(lEndOfDaylight, -lSave)) < 0) then
      Exit(bltAmbiguous);
  end;

  if CompareDateTime(lStartOfDaylight, lEndOfDaylight) < 0 then
  begin
    if (CompareDateTime(aLocalDateTime, lStartOfDaylight) >= 0)
      and (CompareDateTime(aLocalDateTime, lEndOfDaylight) < 0) then
      Result := bltDaylight;
  end else if (CompareDateTime(aLocalDateTime, lStartOfDaylight) >= 0)
    or (CompareDateTime(aLocalDateTime, lEndOfDaylight) < 0) then
    Result := bltDaylight;
end;

function TryBerlinLocalToUtc(const aLocalDateTime: TDateTime;
  out aUtcDateTime: TDateTime): Boolean;
var
  lLocalType: TBerlinLocalTimeType;
  lTimeZone: TTimeZoneInformation;
  lYear: Word;
begin
  Result := False;
  aUtcDateTime := 0;
  lYear := YearOf(aLocalDateTime);
  if not TryGetBerlinYearInformation(lYear, lTimeZone) then
    Exit;
  lLocalType := GetBerlinLocalTimeType(aLocalDateTime, lTimeZone);
  if lLocalType in [bltInvalid, bltAmbiguous] then
    Exit;
  try
    if lLocalType = bltDaylight then
      aUtcDateTime := IncMinute(aLocalDateTime,
        Int64(lTimeZone.Bias) + lTimeZone.DaylightBias)
    else
      aUtcDateTime := IncMinute(aLocalDateTime,
        Int64(lTimeZone.Bias) + lTimeZone.StandardBias);
    Result := (YearOf(aUtcDateTime) >= 1) and (YearOf(aUtcDateTime) <= 9999);
  except
    Result := False;
  end;
end;

function TryBerlinUtcCandidate(const aUtcDateTime: TDateTime;
  const aDaylight: Boolean; out aLocalDateTime: TDateTime): Boolean;
var
  lBias: Int64;
  lLocalType: TBerlinLocalTimeType;
  lTimeZone: TTimeZoneInformation;
  lYear: Word;
begin
  Result := False;
  aLocalDateTime := 0;
  lYear := YearOf(aUtcDateTime);
  if not TryGetBerlinYearInformation(lYear, lTimeZone) then
    Exit;
  try
    if aDaylight then
      lBias := -(Int64(lTimeZone.Bias) + lTimeZone.DaylightBias)
    else
      lBias := -(Int64(lTimeZone.Bias) + lTimeZone.StandardBias);
    aLocalDateTime := IncMinute(aUtcDateTime, lBias);
    if YearOf(aLocalDateTime) <> lYear then
    begin
      lYear := YearOf(aLocalDateTime);
      if not TryGetBerlinYearInformation(lYear, lTimeZone) then
        Exit;
      if aDaylight then
        lBias := -(Int64(lTimeZone.Bias) + lTimeZone.DaylightBias)
      else
        lBias := -(Int64(lTimeZone.Bias) + lTimeZone.StandardBias);
      aLocalDateTime := IncMinute(aUtcDateTime, lBias);
    end;
    lLocalType := GetBerlinLocalTimeType(aLocalDateTime, lTimeZone);
    if aDaylight then
      Result := lLocalType in [bltDaylight, bltAmbiguous]
    else
      Result := lLocalType in [bltStandard, bltAmbiguous];
  except
    Result := False;
  end;
end;

function TryBerlinUtcToLocal(const aUtcDateTime: TDateTime;
  out aLocalDateTime: TDateTime): Boolean;
var
  lDaylightLocal: TDateTime;
  lHasDaylight: Boolean;
  lHasStandard: Boolean;
  lStandardLocal: TDateTime;
begin
  lHasStandard := TryBerlinUtcCandidate(aUtcDateTime, False, lStandardLocal);
  lHasDaylight := TryBerlinUtcCandidate(aUtcDateTime, True, lDaylightLocal);
  if lHasStandard and lHasDaylight
    and (CompareDateTime(lStandardLocal, lDaylightLocal) = 0) then
    lHasDaylight := False;
  Result := lHasStandard xor lHasDaylight;
  if lHasStandard then
    aLocalDateTime := lStandardLocal
  else if lHasDaylight then
    aLocalDateTime := lDaylightLocal
  else
    aLocalDateTime := 0;
end;

function IsValidBerlinLocal(const aLocalDateTime: TDateTime): Boolean;
var
  lTimeZone: TTimeZoneInformation;
begin
  Result := TryGetBerlinYearInformation(YearOf(aLocalDateTime), lTimeZone)
    and not (GetBerlinLocalTimeType(aLocalDateTime, lTimeZone)
      in [bltInvalid, bltAmbiguous]);
end;
{$ENDIF}

Function etime(aDate: TDateTime): String;
Const
  cFormat = 'yyyy"-"mm"-"dd" "hh":"nn":"ss';
Begin
  Result := '';
  Try
    If aDate <> System.MaxInt Then
      Result := FormatDateTime(cFormat, aDate);
  Except
    Result := '';
  End;

  If Result = '' Then
    Result := FormatDateTime(cFormat, 0);
end;

Function etime: String;
Begin
  Result := etime(Now);
End;

Function StrETimeToDateTime(Const etime: String): TDateTime;
Var
  yyyy, dd, mm, h, min, sec, ms: word;
Begin
  Result := 0;
  If Trim(etime) = '' Then
    Exit;

  Try
    yyyy := StrToIntDef(copy(etime, 1, 4), 0);
    mm := StrToIntDef(copy(etime, 6, 2), 99);
    dd := StrToIntDef(copy(etime, 9, 2), 99);

    h := StrToIntDef(copy(etime, 12, 2), 99);
    min := StrToIntDef(copy(etime, 15, 2), 99);
    sec := StrToIntDef(copy(etime, 18, 2), 99);
    ms := StrToIntDef(copy(etime, 21, 3), 0);

    If (yyyy > 0) And
      (mm >= 1) And (mm <= 12) And
      (dd >= 1) And (dd <= 31) And (dd <= DateUtils.DaysInAMonth(yyyy, mm)) And
      (h <= 24) And
      (min <= 59) And
      (sec <= 59) And
      (ms < 1000) Then
      Result := DateUtils.EncodeDateTime(yyyy, mm, dd, h, min, sec, ms);
  Except
    Result := 0;
  End;
End;

Function SecToStrTime(sec: double): String;
Var
  h, m, s, ms: Integer;
Begin
  h := Trunc(sec / 60 / 60);
  sec := sec - h * 60 * 60;

  m := Trunc(sec / 60);
  sec := sec - m * 60;

  s := Trunc(sec);

  ms := Trunc(frac(sec) * 100);
  Result := putBefore(IntToStr(m), '0', 2) +
    ':' + putBefore(IntToStr(s), '0', 2) +
    '.' + putBefore(IntToStr(ms), '0', 3);
  If h > 0 Then
    Result := putBefore(IntToStr(h), '0', 2) + 'h ' + Result;
End;

Function msToPrettyStr(Const ms: int64): String;
Var
  d, h, m, s: Integer;
Begin
  If ms <= 1000 Then
    Result := Format('%d ms', [ms])
    // smaller then 1 minute
  Else If ms <= 60000 Then
    Result := Format('%5.3f s', [ms / 1000])
    // smaller then 1h
  Else If ms <= 60 * 60000 Then
    Result := Format('%5.2f min', [ms / 60000])
  Else If ms <= 60 * 60 * 60000 Then
    Result := Format('%5.2f h', [ms / 60 * 60000])
End;

Function eTimeWithTimeZone(aDate: TDateTime): String;
Begin
  Result := etime(aDate) + ' ' + DateUtils.TTimeZone.Local.
    GetAbbreviation(aDate);
End;

function ParseDateTime(const sDate: string): TDateTime;
var
  aFormatSettings: TFormatSettings;
  sf: string;
  sep: char;
  formats: TArray<string>;
  separators: TArray<char>;
  HasTimePart: boolean;

  function TryConvert(const sDate: string; var dt: TDateTime; const fs: TFormatSettings): boolean;
  begin
    if HasTimePart then
      Result := TryStrToDateTime(sDate, dt, fs)
    else
      Result := TryStrToDate(sDate, dt, fs);
  end;

begin
  Result := 0;

  if sDate = '' then
    Exit;
  if TryParseDateTimeBerlin(sDate, Result) then
    Exit;
  HasTimePart := (pos(':', sDate) > 0)
    or (pos('T', AnsiUppercase(sDate)) > 0);

  aFormatSettings := TFormatSettings.Create;
  if TryConvert(sDate, Result, aFormatSettings) then
    Exit;

  formats := ['de-de', 'pl-pl', 'en-uk', 'en-us'];
  for sf in formats do
  begin
    aFormatSettings := TFormatSettings.Create(sf);
    if TryConvert(sDate, Result, aFormatSettings) then
      Exit;
  end;

  formats := ['dd/mm/yyyy', 'yyyy/mm/dd', 'd/m/yyyy', 'd/m/yy'];
  separators := ['-', '.'];
  aFormatSettings := TFormatSettings.Create();

  for sep in separators do
    for sf in formats do
    begin
      aFormatSettings.ShortDateFormat := sf;
      aFormatSettings.DateSeparator := sep;

      if TryConvert(sDate, Result, aFormatSettings) then
        Exit;
    end;
end;

function TryParseDateTimeUtc(const aValue: string; out aUtcDateTime: TDateTime): Boolean;
var
  lHasExplicitOffset: Boolean;
  lParsedDateTime: TDateTime;
begin
  aUtcDateTime := 0;
  Result := TryParseStrictDateTime(aValue, lParsedDateTime, lHasExplicitOffset);
  if not Result then
    Exit;
  if lHasExplicitOffset then
  begin
    aUtcDateTime := lParsedDateTime;
    Exit;
  end;
  {$IFDEF MSWINDOWS}
  Result := TryBerlinLocalToUtc(lParsedDateTime, aUtcDateTime);
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TryParseDateTimeBerlin(const aValue: string; out aBerlinDateTime: TDateTime): Boolean;
var
  lHasExplicitOffset: Boolean;
  lParsedDateTime: TDateTime;
begin
  aBerlinDateTime := 0;
  Result := TryParseStrictDateTime(aValue, lParsedDateTime, lHasExplicitOffset);
  if not Result then
    Exit;
  {$IFDEF MSWINDOWS}
  if lHasExplicitOffset then
    Result := TryBerlinUtcToLocal(lParsedDateTime, aBerlinDateTime)
  else begin
    Result := IsValidBerlinLocal(lParsedDateTime);
    if Result then
      aBerlinDateTime := lParsedDateTime;
  end;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function FormatGermanDateTime(const aDateTime: TDateTime): string;
begin
  Result := FormatDateTime('dd"."mm"."yyyy" "hh":"nn":"ss', aDateTime,
    TFormatSettings.Invariant);
end;

function FormatGermanDateTimeUtc(const aUtcDateTime: TDateTime): string;
{$IFDEF MSWINDOWS}
var
  lBerlinDateTime: TDateTime;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if not TryBerlinUtcToLocal(aUtcDateTime, lBerlinDateTime) then
    raise EConvertError.Create(
      'UTC value is outside the supported Europe/Berlin range.');
  Result := FormatGermanDateTime(lBerlinDateTime);
  {$ELSE}
  raise ENotSupportedException.Create(
    'Europe/Berlin conversion is implemented for Windows clients only.');
  {$ENDIF}
end;

initialization
  {$IFDEF MSWINDOWS}
  gBerlinYearCache := TDictionary<Word, TTimeZoneInformation>.Create;
  {$ENDIF}

finalization
  {$IFDEF MSWINDOWS}
  gBerlinYearCache.Free;
  {$ENDIF}

end.
