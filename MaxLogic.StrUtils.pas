unit MaxLogic.StrUtils;

{$IFDEF MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
{$endif}

interface

uses
  system.classes, system.sysUtils, system.Types, generics.Collections, system.StrUtils;

const
  CR = sLineBreak;

function StringMatches(Value, Pattern: string;
  casesensitive: boolean = true): boolean;
Function putBefore(Const AString: String; AChar: char; TotalLength: Integer): String; Overload;
Function putBefore(Num: Integer; AChar: char; TotalLength: Integer): String; Overload;

Function ExtractString(
  Const aText, aStartMarker, aEndMarker: String;
  Const aStartoffset: Integer;
  Out aValue: String;
  Out aStartMarkerFoundAtIndex: Integer;
  aCasesensitive: boolean = true): boolean;

Type
  TReplacePlaceholderAction = (raReplace,
    raSkip, // does not replace the found placeholder
    raStop, // as raSkip but also stops the search for the next item
    raReplaceAndStop // replaces this last occurence, but stops afterwards
    );
  TReplacePlaceholderOnFoundProc = reference to procedure(
    // the text between startMarker and endMarker
    const aValue: String;
    aStartMarkerFoundAtIndex: Integer;
    // the string to replace the placeholder (StartMarker+value+endmarker)
    // default is the whole placeholder that s the value with the start and end markers, so if you do not touch that, it will act as action=raSkip
    var aReplaceValue: String;
    // default raReplace
    var aAction: TReplacePlaceholderAction);

Function ReplacePlaceholder(
  Const aText, aStartMarker, aEndMarker: String;
  const aOnFoundProc: TReplacePlaceholderOnFoundProc;
  aStartoffset: Integer = 1;
  // will search for the startMarker and Marker case sensitive or not
  aCasesensitive: boolean = true
  ): String;

// will replace occurences of a system enviroment variable name withint the text with its value
// like '%appdata%\tmp\%username%'
Function ExpandEnvVars(const aText: String; const aStartToken: String = '%'; const aEndToken: String = '%'): String;

Function CombineUrl(const aPart1, aPart2: String; aSeparator: String = '/'): String; overload;
Function CombineUrl(const aParts: array of String; aSeparator: String = '/'): String; overload;

// uses internally masks.MatchesMask
// checks if the aText mathes any of the givem filter strings
// returns always true if the filter array is empty
function MatchesFilter(const aText: string; const AFilter: TStringDynArray): boolean;

// Windows Explorer uses StrCmpLogicalW to compare file names. The RTL/VCL does not declare this function so you need to do it yourself.
// on non windows platform we are falling back on CompareStr
Function StrCmpLogical(Const left, right: String): Integer; Inline;
{$IFDEF MSWINDOWS}

Function StrCmpLogicalW(psz1, psz2: PWideChar): Integer; Stdcall; External 'shlwapi.dll' delayed;
{$ENDIF}


type
  {
    TFilterEx - A record for advanced text filtering based on custom filter syntax.

    This filtering algorithm is inspired by the Everything search tool's advanced search syntax.
    (https://www.voidtools.com/support/everything/searching/#advanced_search)
    It allows for complex filtering rules, including combinations of required terms, optional terms,
    and wildcard searches.

    Filter Syntax:
    - "a b"         : The text must contain both 'a' and 'b'.
    - "a b|c"       : The text must contain 'a' and either 'b' or 'c'.
    - "a*"          : The text must start with 'a'.
    - "a* *b|*c"    : The text must start with 'a' and end with either 'b' or 'c'.

    Usage Example:
    - Create a filter: var Filter := TFilterEx.Create('a b|c');
    - Check if a text matches: if Filter.Matches('example text') then ...
  }
  TFilterEx = record
  private type
    TKind = (kMask, kContains, kStarts, kEnds);

    TFilterItem = record
      IsNegated: boolean;
      OrElements: TArray<String>;
      OrElementKinds: TArray<TKind>;
      OrgText: String;
    end;

  private
    fFilter: TArray<TFilterItem>;
    fOrgFilterText: String;
    // Prepares the internal filter structure based on the provided filter text.
    procedure Prepare(const aText: String);
    // we need to take care because of the special ~and quotes
    function SplitBySpace(const aText: String): TArray<String>;
    procedure Preprocess(var p: String; out k: TKind); inline;
  public
    // Creates a TFilterEx instance from the given filter text.
    class function Create(const aFilterText: String): TFilterEx; static;
    /// <summary>
    /// Returns true if the given text matches the filter or if the filter is empty
    /// </summary>
    function Matches(const aText: String): boolean;
  end;

Procedure Split(Const line: String; Delimiter: char; strings: TStringList); overload;
Procedure Split(Const line: String; Delimiter: char; out strings: TArray<String>); overload;
function Split(Delimiter: char; Const line: String): TArray<String>; overload;
Function fstr(Const d: double; vs: Integer = 2; ns: Integer = 2): String;
function GuidToHex(const aGuid: TGuid): String;
function Join(const aSeparator: String; const aValues: TArray<Integer>): String;


implementation

uses
  system.Masks, autoFree;

function OccurrencesOfChar(const S: string; const C: char): Integer;
var
  pc: pChar;
begin
  result := 0;
  if S <> '' then
  begin
    pc := @S[1];
    for var i := 1 to Length(S) do
    begin
      if pc^ = C then
        inc(result);
      inc(pc);
    end;
  end;
end;

function StringMatches(Value, Pattern: string;
  casesensitive: boolean = true): boolean;
var
  S: string;
  i1, i2, len, x: Integer;
  ExactStart, ExactEnd: boolean;
  l: TStringList;
begin
  if not casesensitive then
  begin
    Value := AnsiLowercase(Value);
    Pattern := AnsiLowercase(Pattern);
  end;

  Pattern := StringReplace(Pattern, '**', '*', [rfReplaceAll]);

  if (Value = '') or (Pattern = '') then
    Exit(false);

  if Pattern[1] = '*' then
  begin
    ExactStart := false;
    Delete(Pattern, 1, 1);
  end
  else
    ExactStart := true;

  if Pattern[Length(Pattern)] = '*' then
  begin
    ExactEnd := false;
    Pattern := copy(Pattern, 1, Length(Pattern) - 1);
  end
  else
    ExactEnd := true;

  l := TStringList.Create;
  l.StrictDelimiter := true;
  l.Delimiter := '*';
  l.DelimitedText := Pattern;

  result := true;
  i1 := 1;
  for x := 0 to l.Count - 1 do
  begin
    i2 := PosEx(l[x], Value, i1);

    if i2 <= 0 then
    begin
      result := false;
      break;
    end;

    if (x = 0) and (ExactStart) and (i2 <> 1) then
    begin
      result := false;
      break;
    end;

    if (ExactEnd) and (x = l.Count - 1) then
    begin
      len := Length(l[x]);
      S := copy(Value, (Length(Value) - len) + 1, len);
      if l[x] <> S then
      begin
        result := false;
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
    result := AString
  Else
  Begin
    SetLength(result, TotalLength);
    For x := 1 To TotalLength - l Do
      result[x] := AChar;
    For x := TotalLength - l + 1 To TotalLength Do
      result[x] := AString[x - (TotalLength - l)];
  End;
End;

Function putBefore(Num: Integer; AChar: char; TotalLength: Integer): String; Overload;
Begin
  result := putBefore(IntToStr(Num), AChar, TotalLength);
End;

Function ExtractString(
  Const aText, aStartMarker, aEndMarker: String; Const
    aStartoffset: Integer;
  Out aValue: String; Out aStartMarkerFoundAtIndex: Integer;
  aCasesensitive: boolean = true): boolean;
Var
  i1, i2: Integer;
  lStartMarker, lEndMarker, lText: String;
Begin
  result := false;
  If aCasesensitive Then
    aStartMarkerFoundAtIndex := PosEx(aStartMarker, aText, aStartoffset)
  Else
  Begin
    lStartMarker := AnsiLowercase(aStartMarker);
    lText := AnsiLowercase(aText);
    lEndMarker := AnsiLowercase(aEndMarker);
    aStartMarkerFoundAtIndex := PosEx(lStartMarker, lText, aStartoffset);
  End;

  If aStartMarkerFoundAtIndex >= 1 Then
  Begin
    i1 := aStartMarkerFoundAtIndex + Length(aStartMarker);

    If aCasesensitive Then
      i2 := PosEx(aEndMarker, aText, i1)
    Else
      i2 := PosEx(lEndMarker, lText, i1);

    If i2 >= 1 Then
    Begin
      result := true;
      aValue := copy(aText, i1, i2 - i1);
    End;
  End;
End;

Function ReplacePlaceholder(
  Const aText, aStartMarker, aEndMarker: String;
  const aOnFoundProc: TReplacePlaceholderOnFoundProc;
  aStartoffset: Integer = 1;
  aCasesensitive: boolean = true
  ): String;
var
  lValue, lReplacementValue: String;
  lStartMarkerFoundAtIndex: Integer;
  lAction: TReplacePlaceholderAction;
  lFound: boolean;
Begin
  result := '';
  repeat

    lFound := ExtractString(
      aText, aStartMarker, aEndMarker,
      aStartoffset,
      lValue,
      lStartMarkerFoundAtIndex,
      aCasesensitive);

    if lFound then
    begin
      lAction := raReplace;
      lReplacementValue := copy(aText, lStartMarkerFoundAtIndex, Length(aStartMarker) + Length(lValue) + Length(aEndMarker));
      aOnFoundProc(lValue, lStartMarkerFoundAtIndex, lReplacementValue, lAction);

      Case lAction of
        raStop:
          break;
        raSkip:
          begin
            lReplacementValue := copy(aText, lStartMarkerFoundAtIndex, Length(aStartMarker) + Length(lValue) + Length(aEndMarker));
            lAction := raReplace;
          end;
      End;

      result := result +
        copy(aText, aStartoffset, (lStartMarkerFoundAtIndex - aStartoffset)) +
        lReplacementValue;

      aStartoffset := lStartMarkerFoundAtIndex + Length(aStartMarker) + Length(lValue) + Length(aEndMarker);
    end;
  until (not lFound) or (lAction in [raStop, raReplaceAndStop]);

  // append the rest of the aText
  if aStartoffset < Length(aText) then
    result := result + copy(aText, aStartoffset, Length(aText));
End;

Function CombineUrl(const aPart1, aPart2: String; aSeparator: String = '/'): String;
begin
  result := CombineUrl([aPart1, aPart2], aSeparator);
end;

Function CombineUrl(const aParts: array of String; aSeparator: String = '/'): String;
var
  x: Integer;
begin
  case Length(aParts) of
    0:
      result := '';
    1:
      result := aParts[0];
  else begin
      result := aParts[0];
      for x := 1 to Length(aParts) - 1 do
      begin
        if not endsText(aSeparator, result) then
          result := result + aSeparator;
        if startsText(aSeparator, aParts[x]) then
          result := result + copy(aParts[x], Length(aSeparator) + 1, Length(aParts[x]))
        else
          result := result + aParts[x];
      end;
    end;
  end;
end;

function MatchesFilter(const aText: string;
  const AFilter: TStringDynArray): boolean;
var
  LFilter: string;
begin
  if Length(AFilter) = 0 then
    Exit(true);

  result := false;
  for LFilter in AFilter do
  begin
    result := system.Masks.MatchesMask(aText, LFilter);
    if result then
      break;
  end;

end;

Function ExpandEnvVars(const aText: String; const aStartToken: String = '%'; const aEndToken: String = '%'): String;
begin
  result := ReplacePlaceholder(
    aText, aStartToken, aEndToken,

      procedure(
      // the text between startMarker and endMarker
      const aValue: String;
      aStartMarkerFoundAtIndex: Integer;
      // the string to replace the placeholder (StartMarker+value+endmarker)
      // default is the whole placeholder that s the value with the start and end markers, so if you do not touch that, it will act as action=raSkip
      var aReplaceValue: String;
      // default raReplace
      var aAction: TReplacePlaceholderAction)
    var
      v: String;
    begin
      v := system.sysUtils.GetEnvironmentVariable(aValue);
      if v = '' then
        aAction := raSkip
      else
        aReplaceValue := v;
    end,
    1, false);
end;

Function StrCmpLogical(Const left, right: String): Integer;
Begin
  {$IFDEF MSWINDOWS}
  result := StrCmpLogicalW(PWideChar(left), PWideChar(right));
  {$ELSE}
  result := CompareStr(left, right);
  {$ENDIF}
End;

{ TFilterEx }

class function TFilterEx.Create(const aFilterText: String): TFilterEx;
begin
  result := Default (TFilterEx);
  result.Prepare(aFilterText);
end;

procedure TFilterEx.Prepare(const aText: String);
var
  ar: TArray<String>;
  fi: TFilterItem;
  l: TStringList;
  p: String;
  k: TKind;
begin
  gc(l, TStringList.Create);
  l.StrictDelimiter := true;
  l.Delimiter := '|';
  l.QuoteChar := '"';

  fOrgFilterText := aText;
  ar := SplitBySpace(AnsiLowercase(trim(aText)));
  SetLength(fFilter, Length(ar));
  for var x := 0 to High(ar) do
  begin
    fi := default (TFilterItem);
    p := trim(ar[x]);
    if p = '' then
      continue;
    fi.OrgText := p;
    if startsText('!', p) then
    begin
      Delete(p, 1, 1);
      fi.IsNegated := true;
      if p = '' then
        continue;
    end;

    l.DelimitedText := p;

    SetLength(fi.OrElements, l.Count);
    SetLength(fi.OrElementKinds, l.Count);
    for var Y := 0 to l.Count - 1 do
    begin
      p := l[Y];
      Preprocess(p, k);
      fi.OrElements[Y] := p;
      fi.OrElementKinds[Y] := k;
    end;
    fFilter[x] := fi;
  end;
end;

procedure TFilterEx.Preprocess(var p: String; out k: TKind);
var
  i1, i2: Integer;
begin
  if pos('?', p) > 0 then
    k := kMask
  else begin
    i1 := pos('*', p);
    if i1 < 1 then
      k := kContains
    else if i1 = Length(p) then
    begin
      Delete(p, Length(p), 1);
      k := kStarts;
    end else if i1 = 1 then
    begin
      i2 := 2;
      i2 := PosEx('*', p, i2);
      if i2 < 1 then
      begin
        Delete(p, 1, 1);
        k := kEnds;
      end else if i2 = Length(p) then
      begin
        p := copy(p, 2, Length(p) - 2);
        k := kContains;
      end
      else
        k := kMask;
    end
    else
      k := kMask;
  end;
end;

function TFilterEx.SplitBySpace(const aText: String): TArray<String>;
var
  S, p: String;
  i1, i2: Integer;
  l: TList<String>;
  lIsInQuote: boolean;
  C: char;
begin
  S := aText + ' ';
  gc(l, TList<String>.Create);
  lIsInQuote := false;
  i1 := 1;
  i2 := 1; // why not 2? because the first char may contain a '"' char....

  while i2 <= Length(S) do
  begin
    C := S[i2];
    if lIsInQuote then
    begin
      if C = '"' then
        lIsInQuote := false;

    end else if C = '"' then
      lIsInQuote := true
    else if C = ' ' then
    begin
      if i1 <> i2 then
      begin
        p := copy(S, i1, (i2 - i1));
        l.add(p);
      end;
      i1 := i2 + 1;
    end;
    inc(i2);
  end;

  result := l.ToArray;
end;

function TFilterEx.Matches(const aText: String): boolean;
var
  lMatchesANy: boolean;
  fi: TFilterItem;
  S: String;
  lText: String;
begin
  result := true;

  lText := AnsiLowercase(aText);
  for fi in fFilter do
  begin
    lMatchesANy := false;
    for var x := 0 to Length(fi.OrElements) - 1 do
    begin
      S := fi.OrElements[x];
      case fi.OrElementKinds[x] of
        kMask:
          lMatchesANy := system.Masks.MatchesMask(lText, S);
        kContains:
          lMatchesANy := lText.Contains(S);
        kStarts:
          lMatchesANy := StartsStr(S, lText);
        kEnds:
          lMatchesANy := EndsStr(S, lText);
      end;
      if lMatchesANy then
        break;
    end;

    if fi.IsNegated then
    begin
      if lMatchesANy then
        Exit(false);
    end else if not lMatchesANy then
      Exit(false);
  end;
end;

{ other }

function Split(Delimiter: char; Const line: String): TArray<String>;
begin
  Split(line, Delimiter, result);
end;

Procedure Split(Const line: String; Delimiter: char; out strings: TArray<String>);
Var
  l: TStringList;
Begin
  l := TStringList.Create;
  l.StrictDelimiter := true;
  l.Delimiter := Delimiter;
  l.DelimitedText := line;
  strings := l.ToStringArray;
  l.Free;
end;

Procedure Split(Const line: String; Delimiter: char; strings: TStringList);
Var
  l: TStringList;
Begin
  l := TStringList.Create;
  l.StrictDelimiter := true;
  l.Delimiter := Delimiter;
  l.DelimitedText := line;
  strings.Assign(l);
  l.Free;
end;

Function fstr(Const d: double; vs: Integer = 2; ns: Integer = 2): String;
Var
  S: String;
Begin
  S := '0.' + putBefore('0', '0', ns);
  result := FormatFloat(S, d);
end;

function GuidToHex(const aGuid: TGuid): String;
begin
  SetLength(Result, SizeOf(TGuid)*2);
  BinToHex(@aGuid, PChar(Result), SizeOf(TGuid));
end;

function Join(const aSeparator: String; const aValues: TArray<Integer>): String;
var
  lValues: TArray<String>;
begin
  SetLength(lValues, Length(aValues));
  for var x := 0 to length(aValues)-1 do
    lValues[x]:= aValues[x].ToString;
  Result:= String.Join(aSeparator, lValues);
end;


end.
