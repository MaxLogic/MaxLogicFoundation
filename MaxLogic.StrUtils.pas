unit MaxLogic.StrUtils;

interface

uses
  classes, sysUtils;

function StringMatches(Value, Pattern: string;
  casesensitive: boolean = true): boolean;
Function putBefore(Const AString: String; AChar: char; TotalLength: Integer)
  : String; Overload;
Function putBefore(Num: Integer; AChar: char; TotalLength: Integer)
  : String; Overload;

implementation

uses
  StrUtils;

function StringMatches(Value, Pattern: string;
  casesensitive: boolean = true): boolean;
var
  s: string;
  i1, i2, len, x: Integer;
  ExactStart, ExactEnd: boolean;
  l: TStringList;
begin
  if not casesensitive then
  begin
    Value := AnsiLowercase(Value);
    Pattern := AnsiLowercase(Pattern);
  end;

  Result := False;
  Pattern := StringReplace(Pattern, '**', '*', [rfReplaceAll]);

  if (Value = '') or (Pattern = '') then
    Exit(False);

  if Pattern[1] = '*' then
  begin
    ExactStart := False;
    Delete(Pattern, 1, 1);
  end
  else
    ExactStart := true;

  if Pattern[Length(Pattern)] = '*' then
  begin
    ExactEnd := False;
    Pattern := copy(Pattern, 1, Length(Pattern) - 1);
  end
  else
    ExactEnd := true;

  l := TStringList.Create;
  l.StrictDelimiter := true;
  l.Delimiter := '*';
  l.DelimitedText := Pattern;

  Result := true;
  i1 := 1;
  for x := 0 to l.Count - 1 do
  begin
    i2 := PosEx(l[x], Value, i1);

    if i2 <= 0 then
    begin
      Result := False;
      break;
    end;

    if (x = 0) and (ExactStart) and (i2 <> 1) then
    begin
      Result := False;
      break;
    end;

    if (ExactEnd) and (x = l.Count - 1) then
    begin
      len := Length(l[x]);
      s := copy(Value, (Length(Value) - len) + 1, len);
      if l[x] <> s then
      begin
        Result := False;
        break;
      end;
    end;

    i1 := i2 + Length(l[x]);
  end;

  l.Free;
end;

Function putBefore(Const AString: String; AChar: char; TotalLength: Integer): String;
Var
  x, l: Integer;
Begin
  l := Length(AString);
  If l > TotalLength Then
    Result := AString
  Else
  Begin
      SetLength(Result, TotalLength);
    For x := 1 To TotalLength - l Do
      Result[x] := AChar;
    For x := TotalLength - l + 1 To TotalLength Do
      Result[x] := AString[x - (TotalLength - l)];
  End;
End;

Function putBefore(Num: Integer; AChar: char; TotalLength: Integer): String; Overload;
Begin
  Result := putBefore(IntToStr(Num), AChar, TotalLength);
End;

end.

