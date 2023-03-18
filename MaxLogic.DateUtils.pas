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

Implementation

Uses
  DateUtils, sTRuTILS, autoFree;

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
      (h >= 0) And (h <= 24) And
      (min >= 0) And (min <= 59) And
      (sec >= 0) And (sec <= 59) And
      (ms >= 0) And (ms < 1000) Then
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

end.
