unit MaxLogic.StrUtils;

{$IFDEF MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
{$endif}

interface

uses
  system.classes, system.sysUtils, system.Types, generics.Collections, system.StrUtils;

const
  CR = sLineBreak;

function StringMatches(Value, Pattern: String;
  casesensitive: boolean = true): boolean;
Function putBefore(Const AString: String; AChar: char; TotalLength: Integer): String; Overload;
Function putBefore(Num: Integer; AChar: char; TotalLength: Integer): String; Overload;
/// <summary>
/// Extracts a substring located between the first occurrence of aStartMarker
/// and the subsequent first occurrence of aEndMarker, starting the search from aStartOffset.
/// Optionally handles case sensitivity and checks for invalid characters within the potential result.
/// </summary>
/// <param name="aText">The text to search within.</param>
/// <param name="aStartMarker">The starting delimiter string.</param>
/// <param name="aEndMarker">The ending delimiter string.</param>
/// <param name="aStartoffset">The 1-based index within aText to begin searching for aStartMarker.</param>
/// <param name="aValue">Output parameter: Receives the extracted substring (between the markers) if found and valid.</param>
/// <param name="aStartMarkerFoundAtIndex">Output parameter: Receives the 1-based index where aStartMarker was found.</param>
/// <param name="aCasesensitive">If True, the search for markers is case-sensitive. If False, markers and text are compared case-insensitively (using ToLower).</param>
/// <param name="aInvalidChars">An optional array of characters. If any of these characters are found *between* a potential start and end marker pair in the original `aText`, that match is considered invalid, and the search continues.</param>
/// <param name="aInvalidCharsAreSorted">Optimization hint. If True, indicates that the aInvalidChars array is sorted, allowing for a faster binary search for invalid characters. If False, a linear search is performed.</param>
/// <returns>True if a valid substring was found and extracted, False otherwise (markers not found, end marker before start marker, or an invalid character was detected between markers).</returns>
/// <remarks>
/// This is the main public overload. It prepares the text and markers for case-sensitive or case-insensitive searching
/// based on `aCasesensitive` and then calls the internal overload to perform the actual extraction logic.
/// The search for invalid characters (`aInvalidChars`) is *always* performed case-sensitively on the original `aText`.
/// </remarks>
Function ExtractString(
  Const aText, aStartMarker, aEndMarker: String;
  aStartoffset: Integer;
  Out aValue: String;
  Out aStartMarkerFoundAtIndex: Integer;
  aCasesensitive: boolean = True;
  { aInvalidChars: used for the following scenario If we search for #*#, but only on the same line,
  we can pass aInvalidChars  = [#10], in that case if the #10 will come before the end marker, it will
  indicates that this is indeed not a end marker at all
  Note: the search for invalidChars is always case sensitive}
  const aInvalidChars : TArray<Char> = [];
  aInvalidCharsAreSorted: Boolean = False ): boolean; overload;


/// <summary>
/// Internal implementation for ExtractString. Performs the core logic of finding markers and checking for invalid characters.
/// Assumes that text/markers have already been prepared for case sensitivity by the caller.
/// </summary>
/// <param name="aOrgCasedText">The original text with its original casing. Used for the final Copy operation and the invalid character check.</param>
/// <param name="aTextForCaseSensitiveSearch">The text to perform the marker search in. This might be the original text or a lowercase version, depending on the desired case sensitivity.</param>
/// <param name="aStartMarker">The starting delimiter prepared for case-sensitive search.</param>
/// <param name="aEndMarker">The ending delimiter prepared for case-sensitive search.</param>
/// <param name="aStartoffset">The 1-based index within aTextForCaseSensitiveSearch to continue searching for aStartMarker.</param>
/// <param name="aValue">Output parameter: Receives the extracted substring (from aOrgCasedText) if found and valid.</param>
/// <param name="aStartMarkerFoundAtIndex">Output parameter: Receives the 1-based index where aStartMarker was found in aTextForCaseSensitiveSearch (corresponds to the same position in aOrgCasedText).</param>
/// <param name="aInvalidChars">An optional array of characters. If any character from this array is found within the substring *between* the found markers (checked against `aOrgCasedText`), the match is invalid.</param>
/// <param name="aInvalidCharsAreSorted">Optimization hint for the invalid character search.</param>
/// <returns>True if a valid substring was found, False otherwise.</returns>
/// <remarks>
/// This function iterates using a `while true` loop:
/// 1. Finds the next `aStartMarker` using `PosEx` starting from `aStartoffset`. Exits if not found.
/// 2. Calculates the position `i1` immediately after the found `aStartMarker`.
/// 3. Finds the next `aEndMarker` using `PosEx` starting from `i1`. Exits if not found.
/// 4. Uses `CharPosEx` to check if any `aInvalidChars` exist in `aOrgCasedText` between index `i1` and `i2 - 1`.
/// 5. If an invalid character is found:
///    - Updates `aStartoffset` to `aStartMarkerFoundAtIndex + 1` to ensure the next search attempt starts *after* the beginning of the current invalid match.
///    - Executes `Continue` to restart the loop from step 1.
/// 6. If no invalid character is found:
///    - Copies the valid substring from `aOrgCasedText` between `i1` and `i2 - 1` into `aValue`.
///    - Sets Result to True and exits the function.
/// The loop continues until a valid match is found and returned, or until no more potential start/end marker pairs can be found.
/// </remarks>
Function ExtractString(
  Const aOrgCasedText, aTextForCaseSensitiveSearch,
  aStartMarker, aEndMarker: String;
  aStartoffset: Integer;
  Out aValue: String;
  Out aStartMarkerFoundAtIndex: Integer;
  { aInvalidChars: used for the following scenario If we search for #*#, but only on the same line,
  we can pass aInvalidChars  = [#10], in that case if the #10 will come before the end marker, it will
  indicates that this is indeed not a end marker at all
  Note: the search for invalidChars is always case sensitive}
  const aInvalidChars : TArray<Char> = [];
  aInvalidCharsAreSorted: Boolean = False ): Boolean; overload;
function CharPosEx(
  const aText: String;
  const aChars: TArray<Char>;
  aCharsArrayIsSorted: Boolean;
  aStartOffset, aEndOffset: Integer;
  out aFoundAtOffset, aIndexOfCharFound: Integer): Boolean;

Type
  TReplacePlaceholderAction = (
    raReplace,
    raReplaceAndResumeAtSamePosition, // good for nested occurences,, but a bit slower as it requires in place  editing of the source text
    raSkip, // does not replace the found placeholder
    raStop, // as raSkip but also stops the search for the next item
    raReplaceAndStop // replaces this last occurence, but stops afterwards
    );
  TReplacePlaceholderOnFoundProc = reference to procedure(
    // the text between startMarker and endMarker
    const aValue: String;
    aStartMarkerFoundAtIndex: Integer;
    // the String to replace the placeholder (StartMarker+value+endmarker)
    // default is the whole placeholder that s the value with the start and end markers, so if you do not touch that, it will act as action=raSkip
    var aReplaceValue: String;
    // default raReplace
    var aAction: TReplacePlaceholderAction);

/// <summary>
/// Replaces occurrences of placeholders (text between aStartMarker and aEndMarker) within a given text.
/// Calls a provided procedure for each found placeholder to determine the replacement value and action.
/// </summary>
/// <param name="aText">The input text containing potential placeholders.</param>
/// <param name="aStartMarker">The starting delimiter of the placeholder.</param>
/// <param name="aEndMarker">The ending delimiter of the placeholder.</param>
/// <param name="aOnFoundProc">A callback procedure invoked for each valid placeholder found. It receives the placeholder's content (`aValue`), its start index (`aStartMarkerFoundAtIndex`), and allows modification of the replacement string (`aReplaceValue`) and the action (`aAction`) to take.</param>
/// <param name="aStartoffset">The 1-based index in aText to begin searching for placeholders.</param>
/// <param name="aCasesensitive">If True, the search for markers is case-sensitive. If False, markers and text are compared case-insensitively.</param>
/// <param name="aInvalidChars">An optional array of characters that invalidate a potential match if found between the start and end markers (checked case-sensitively on the original text).</param>
/// <param name="aInvalidCharsAreSorted">Optimization hint for the invalid character search within ExtractString.</param>
/// <returns>A new string with placeholders replaced according to the logic defined in aOnFoundProc.</returns>
/// <remarks>
/// The function iterates through the text using the `ExtractString` helper function to find valid placeholders (respecting `aInvalidChars`).
/// For each valid placeholder found:
/// 1. The `aOnFoundProc` callback is invoked.
/// 2. The callback determines the `aReplaceValue` (initially defaulted to the full placeholder including markers) and `aAction`.
/// 3. Based on `aAction`:
///    - `raReplace`: The text before the placeholder and the `aReplaceValue` are appended to the result. The search continues after the replaced section.
///    - `raSkip`: The original placeholder text (including markers) is treated as the replacement value. The search continues after the skipped section.
///    - `raStop`: The loop terminates immediately. The text before the current placeholder is appended, but the placeholder itself and the rest of the original text are *not* included in the result.
///    - `raReplaceAndStop`: The text before the placeholder and `aReplaceValue` are appended. The loop terminates. The rest of the original text is *not* included.
///    - `raReplaceAndResumeAtSamePosition`: This handles nested or overlapping replacements. The text before the placeholder is appended. The original text buffer (`lOrgCasedText`) and its search version (`lTextForCaseSensitiveSearch`) are *modified in-place* by substituting the placeholder with `aReplaceValue`. The search offset (`aStartoffset`) is reset to 1, and the loop continues, effectively rescanning the modified text from the beginning. This can be less performant due to string manipulation but allows for complex replacement scenarios.
/// The function uses `TStringBuilder` internally for efficient construction of the final result string.
/// After the loop finishes (either by reaching the end of the text or via a `raStop`/`raReplaceAndStop` action), any remaining portion of the text after the last processed position (`aStartoffset`) is appended to the result.
/// </remarks>
Function ReplacePlaceholder(
  Const aText, aStartMarker, aEndMarker: String;
  // will be caled for each occurence
  const aOnFoundProc: TReplacePlaceholderOnFoundProc;
  aStartoffset: Integer = 1;
  // will search for the startMarker and Marker case sensitive or not
  aCasesensitive: boolean = True;
  { aInvalidChars: used for the following scenario If we search for #*#, but only on the same line,
  we can pass aInvalidChars  = [#10], in that case if the #10 will come before the end marker, it will
  indicates that this is indeed not a end marker at all
  Note: the search for invalidChars is always case sensitive}
  const aInvalidChars : TArray<Char> = [];
  aInvalidCharsAreSorted: Boolean = False
  ): String;

// will replace occurences of a system enviroment variable name withint the text with its value
// like '%appdata%\tmp\%username%'
Function ExpandEnvVars(const aText: String; const aStartToken: String = '%'; const aEndToken: String = '%'): String;

Function CombineUrl(const aPart1, aPart2: String; aSeparator: String = '/'): String; overload;
Function CombineUrl(const aParts: array of String; aSeparator: String = '/'): String; overload;

// uses internally masks.MatchesMask
// checks if the aText mathes any of the givem filter strings
// returns always true if the filter array is empty
function MatchesFilter(const aText: String; const AFilter: TStringDynArray): boolean;

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

function SplitInHalfBy(const aText: String; aDelim: Char; out aParts: TArray<String>): Boolean;overload;
function SplitInHalfBy(const aText: String; aDelim: Char; out alLeft, alRight: String): Boolean;overload;


Function fstr(Const d: double; vs: Integer = 2; ns: Integer = 2): String;
function GuidToHex(const aGuid: TGuid): String;
function Join(const aSeparator: String; const aValues: TArray<Integer>): String;

// this method ensures the num of bytes does not exceed aMaxByteLength
// it supports unicode surrogate pairs
function Utf8TruncateByCodePoint(const AInput: String; aMaxBytesLength: Integer): TBytes;


// StrTo Float With Comma Correction
Function StrToFloatWCC(Const Text: String): double;
Function TryStrToFloatWCC(Const Text: String; Out Value: double): Boolean;
Function StrToFloatWCCDef(Const Text: String; Const Default: double): double;
Procedure PrepareTextForStrToFloatWcc(Var s: String);


implementation

uses
  System.character, system.Masks, autoFree, system.Math;

function OccurrencesOfChar(const S: String; const C: char): Integer;
var
  pc: pChar;
begin
  Result := 0;
  if S <> '' then
  begin
    pc := @S[1];
    for var i := 1 to Length(S) do
    begin
      if pc^ = C then
        inc(Result);
      inc(pc);
    end;
  end;
end;

function StringMatches(Value, Pattern: String;
      casesensitive: boolean = true): boolean;
    var
      S: String;
      i1, i2, len, x: Integer;
      ExactStart, ExactEnd: boolean;
      lPatternParts: TStringList; // Renamed variable 'l' to 'lPatternParts' for clarity
    begin
      // Use System.Masks for case-insensitive matching as it's likely more optimized/robust
      if not casesensitive then
      begin
        Result:= System.Masks.MatchesMask(Value, Pattern);
        Exit;
      end;

      // Case-Sensitive Logic
      // Note: This custom implementation handles only '*' as a wildcard, not '?' like MatchesMask.
      // Ensure Value and Pattern are treated case-sensitively (no ToLower here)

      Pattern := StringReplace(Pattern, '**', '*', [rfReplaceAll]); // Keep this

      if (Value = '') or (Pattern = '') then
        Exit(false);

      if Pattern[1] = '*' then
      begin
        ExactStart := false;
        Delete(Pattern, 1, 1);
      end else
        ExactStart := true;

      if (Length(Pattern) > 0) and (Pattern[Length(Pattern)] = '*') then // Check Length > 0
      begin
        ExactEnd := false;
        Pattern := copy(Pattern, 1, Length(Pattern) - 1);
      end else
        ExactEnd := true;

      // Handle empty pattern after removing stars
      if Pattern = '' then
      begin
        // If pattern is empty after removing stars, it matches only if Value is also empty
        // or if wildcards were at both ends (ExactStart=False, ExactEnd=False)
        Result := (Value = '') or (not ExactStart and not ExactEnd);
        Exit;
      end;

      gc(lPatternParts, TStringList.Create); // <-- Use gc()
      lPatternParts.StrictDelimiter := true;
      lPatternParts.Delimiter := '*';
      lPatternParts.DelimitedText := Pattern; // Use the potentially modified Pattern

      Result := true;
      i1 := 1; // Start search at index 1
      for x := 0 to lPatternParts.Count - 1 do
      begin
        // Use PosEx case-sensitively on the original Value
        i2 := PosEx(lPatternParts[x], Value, i1);

        if i2 <= 0 then
        begin
          Result := false;
          break; // Part not found
        end;

        // If ExactStart, the first part must start at index 1
        if (x = 0) and ExactStart and (i2 <> 1) then
        begin
          Result := false;
          break;
        end;

        // If ExactEnd, the last part must match the *end* of the Value string
        if ExactEnd and (x = lPatternParts.Count - 1) then
        begin
          len := Length(lPatternParts[x]);
          // Check if the found position + length equals the end of the Value string
          if (i2 + len - 1) <> Length(Value) then
          begin
            Result := false;
            break;
          end;
        end;

        // Advance the search position for the next part
        i1 := i2 + Length(lPatternParts[x]);
      end;
    end;

Function putBefore(Const AString: String; AChar: char; TotalLength: Integer): String;
Var
  x, aMaxBytesLength: Integer;
Begin
  aMaxBytesLength := Length(AString);
  If aMaxBytesLength > TotalLength Then
    Result := AString
  Else
  Begin
    SetLength(Result, TotalLength);
    For x := 1 To TotalLength - aMaxBytesLength Do
      Result[x] := AChar;
    For x := TotalLength - aMaxBytesLength + 1 To TotalLength Do
      Result[x] := AString[x - (TotalLength - aMaxBytesLength)];
  End;
End;

Function putBefore(Num: Integer; AChar: char; TotalLength: Integer): String; Overload;
Begin
  Result := putBefore(IntToStr(Num), AChar, TotalLength);
End;

Function ExtractString(
  Const aText, aStartMarker, aEndMarker: String;
  aStartoffset: Integer;
  Out aValue: String; Out aStartMarkerFoundAtIndex: Integer;
  aCasesensitive: boolean = True;
  { aInvalidChars: used for the following scenario If we search for #*#, but only on the same line,
  we can pass aInvalidChars  = [#10], in that case if the #10 will come before the end marker, it will
  indicates that this is indeed not a end marker at all
  Note: the search for invalidChars is always case sensitive}
  const aInvalidChars: TArray<Char> = [];
  aInvalidCharsAreSorted: Boolean = False ): boolean;
begin
  if aCasesensitive then
    Result:= ExtractString(
      aText, aText, aStartMarker, aEndMarker,
      aStartoffset, aValue, aStartMarkerFoundAtIndex, aInvalidChars, aInvalidCharsAreSorted)
  else
    Result:= ExtractString(
      aText, aText.ToLower, aStartMarker.ToLower, aEndMarker.ToLower,
      aStartoffset, aValue, aStartMarkerFoundAtIndex, aInvalidChars, aInvalidCharsAreSorted);
end;

Function ExtractString(
  Const aOrgCasedText, aTextForCaseSensitiveSearch,
  aStartMarker, aEndMarker: String;
  aStartoffset: Integer;
  Out aValue: String;
  Out aStartMarkerFoundAtIndex: Integer;
  { aInvalidChars: used for the following scenario If we search for #*#, but only on the same line,
  we can pass aInvalidChars  = [#10], in that case if the #10 will come before the end marker, it will
  indicates that this is indeed not a end marker at all
  Note: the search for invalidChars is always case sensitive}
  const aInvalidChars : TArray<Char> = [];
  aInvalidCharsAreSorted: Boolean = False ): boolean;
Var
  i1, i2: Integer;
  lInvalidCharOffset, lIndexOfInvalidChar: Integer;
Begin
  Result := False;

  while true do
  begin
    aStartMarkerFoundAtIndex := PosEx(aStartMarker, aTextForCaseSensitiveSearch, aStartoffset);
    If aStartMarkerFoundAtIndex = 0 Then
      Exit(False);

    i1 := aStartMarkerFoundAtIndex + Length(aStartMarker);

    i2 := PosEx(aEndMarker, aTextForCaseSensitiveSearch, i1);
    If i2 = 0 Then
      Exit(False);

    if CharPosEx(aOrgCasedText,
      aInvalidChars,
      aInvalidCharsAreSorted,
      i1, i2,
      lInvalidCharOffset, lIndexOfInvalidChar) then
    begin
      aStartoffset:= i1;
      Continue;
    end;

    aValue := Copy(aOrgCasedText, i1, i2 - i1);
    Exit(True);
  end;
End;

Function ReplacePlaceholder(
  Const aText, aStartMarker, aEndMarker: String;
  const aOnFoundProc: TReplacePlaceholderOnFoundProc;
  aStartoffset: Integer = 1;
  aCasesensitive: boolean = True;
  { aInvalidChars: used for the following scenario If we search for #*#, but only on the same line,
  we can pass aInvalidChars  = [#10], in that case if the #10 will come before the end marker, it will
  indicates that this is indeed not a end marker at all
  Note: the search for invalidChars is always case sensitive}
  const aInvalidChars : TArray<Char> = [];
  aInvalidCharsAreSorted: Boolean = False
  ): String;
var
  lValue, lReplacementValue: String;
  lStartMarkerFoundAtIndex: Integer;
  lAction: TReplacePlaceholderAction;
  lFound: boolean;
  lOrgCasedText, lTextForCaseSensitiveSearch, lStartMarker, lEndMarker: String;
  lMarkersLen, lPlaceHolderLen: Integer;
  sb: TStringBuilder; // well, I know, there was much hate for the TStringBuilder in delphi. But at least since delphi 12 this is faster then ordinary delphi string + string,expecially for large strings
Begin
  Result := '';
  gc(sb, TStringBuilder.Create);
  sb.Capacity:= Length(aText) * 2;

  if aStartoffset < 1 then
    aStartoffset:= 1;

  // init our output with the text before the start offset, so we do not loose it
  if aStartoffset > 1 then
    sb.Append( Copy(aText, 1, aStartoffset - 1) );

  lOrgCasedText:= aText;
  lMarkersLen:= Length(aStartMarker) + Length(aEndMarker);
  if aCasesensitive then
  begin
    lTextForCaseSensitiveSearch:= aText;
    lStartMarker:= aStartMarker;
    lEndMarker:= aEndMarker;
  end else begin
    lTextForCaseSensitiveSearch:= aText.ToLower;
    lStartMarker:= aStartMarker.ToLower;
    lEndMarker:= aEndMarker.ToLower;
  end;

  Repeat

    lFound := ExtractString(
      lOrgCasedText, lTextForCaseSensitiveSearch, lStartMarker, lEndMarker,
      aStartoffset,
      lValue,
      lStartMarkerFoundAtIndex,
      aInvalidChars, aInvalidCharsAreSorted);

    if not lFound then
      break;

    lAction := raReplace;
    lPlaceHolderLen:= Length(lValue) + lMarkersLen;
    lReplacementValue := copy(lOrgCasedText, lStartMarkerFoundAtIndex, lPlaceHolderLen);
    aOnFoundProc(lValue, lStartMarkerFoundAtIndex, lReplacementValue, lAction);

    Case lAction of
      raStop:
        break;
      raSkip:
        begin
          lReplacementValue := Copy(lOrgCasedText, lStartMarkerFoundAtIndex, lPlaceHolderLen);
          lAction := raReplace;
        end;
      raReplaceAndResumeAtSamePosition:
        begin
          // that one is a bit tricky
          // 1. flush the part before the marker start pos to our output
          sb.Append(
            Copy(lOrgCasedText,
              aStartoffset, (lStartMarkerFoundAtIndex - aStartoffset)));
          // 2. trim both text buffers and prepend with the pepalacement value
          lOrgCasedText:= lReplacementValue +
            Copy(lOrgCasedText, lStartMarkerFoundAtIndex + lPlaceHolderLen, Length(lOrgCasedText));
          if not aCasesensitive then
            lReplacementValue  := lReplacementValue .ToLower;
          lTextForCaseSensitiveSearch:= lReplacementValue +
            Copy(lTextForCaseSensitiveSearch, lStartMarkerFoundAtIndex + lPlaceHolderLen, Length(lTextForCaseSensitiveSearch));
          // 3. Reset the start offset to 1 as we want to search from the start again
          aStartoffset:= 1;
          Continue; // we want to skip the code that follows
        end;
    end;

    // copy the part from the start position until the position of the marker, then add the replacement text to the output
    sb.Append(
      Copy(lOrgCasedText,
        aStartoffset, (lStartMarkerFoundAtIndex - aStartoffset)) );
    sb.Append(lReplacementValue);

    aStartoffset := lStartMarkerFoundAtIndex + lPlaceHolderLen;
  until (not lFound) or (lAction in [raStop, raReplaceAndStop]);

  // append the rest of the Text
  if aStartoffset < Length(lOrgCasedText) then
    sb.Append( Copy(lOrgCasedText, aStartoffset, Length(lOrgCasedText)) );
  Result:= sb.ToString;
End;

Function CombineUrl(const aPart1, aPart2: String; aSeparator: String = '/'): String;
begin
  Result := CombineUrl([aPart1, aPart2], aSeparator);
end;

Function CombineUrl(const aParts: array of String; aSeparator: String = '/'): String;
var
  x: Integer;
begin
  case Length(aParts) of
    0:
      Result := '';
    1:
      Result := aParts[0];
  else begin
      Result := aParts[0];
      for x := 1 to Length(aParts) - 1 do
      begin
        if not endsText(aSeparator, Result) then
          Result := Result + aSeparator;
        if startsText(aSeparator, aParts[x]) then
          Result := Result + copy(aParts[x], Length(aSeparator) + 1, Length(aParts[x]))
        else
          Result := Result + aParts[x];
      end;
    end;
  end;
end;

function MatchesFilter(const aText: String;
  const AFilter: TStringDynArray): boolean;
var
  lFilter: String;
begin
  if Length(AFilter) = 0 then
    Exit(true);

  Result := false;
  for lFilter in AFilter do
  begin
    Result := system.Masks.MatchesMask(aText, lFilter);
    if Result then
      break;
  end;

end;

Function ExpandEnvVars(const aText: String; const aStartToken: String = '%'; const aEndToken: String = '%'): String;
begin
  Result := ReplacePlaceholder(
    aText, aStartToken, aEndToken,

      procedure(
      // the text between startMarker and endMarker
      const aValue: String;
      aStartMarkerFoundAtIndex: Integer;
      // the String to replace the placeholder (StartMarker+value+endmarker)
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
  Result := StrCmpLogicalW(PWideChar(left), PWideChar(right));
  {$ELSE}
  Result := CompareStr(left, right);
  {$ENDIF}
End;

{ TFilterEx }

class function TFilterEx.Create(const aFilterText: String): TFilterEx;
begin
  Result := Default (TFilterEx);
  Result.Prepare(aFilterText);
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
  ar := SplitBySpace(aText.Trim.ToLower);
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

  Result := l.ToArray;
end;

function TFilterEx.Matches(const aText: String): boolean;
var
  lMatchesANy: boolean;
  fi: TFilterItem;
  S: String;
  lText: String;
begin
  Result := true;

  lText := aText.ToLower;
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

function SplitInHalfBy(const aText: String; aDelim: Char; out aParts: TArray<String>): Boolean;
var
  i: Integer;
begin
  Result:= False;
  i:= Pos(aDelim, aText);
  if i<1 then
    aParts:= [aText]
  else begin
    Result:= True;
    aParts:= [
      Copy(aText, 1, i-1),
      Copy(aText, i+1, Length(aText))
    ];
  end;
end;

function SplitInHalfBy(const aText: String; aDelim: Char; out alLeft, alRight: String): Boolean;overload;
var
  i: Integer;
begin
  Result:= False;
  i:= Pos(aDelim, aText);
  if i>=1 then
  begin
    Result:= True;
    alLeft:= Copy(aText, 1, i-1);
    alRight:= Copy(aText, i+1, Length(aText));
  end;
end;

function Split(Delimiter: char; Const line: String): TArray<String>;
begin
  Split(line, Delimiter, Result);
end;

Procedure Split(Const line: String; Delimiter: char; out strings: TArray<String>);
Var
  l: TStringList;
Begin
  gc(l, TStringList.Create);
  l.StrictDelimiter := true;
  l.Delimiter := Delimiter;
  l.DelimitedText := line;
  strings := l.ToStringArray;
end;

Procedure Split(Const line: String; Delimiter: char; strings: TStringList);
Var
  l: TStringList;
Begin
  gc(l, TStringList.Create);
  l.StrictDelimiter := true;
  l.Delimiter := Delimiter;
  l.DelimitedText := line;
  Strings.Clear;
  Strings.AddStrings(l);
end;


Function fstr(Const d: double; vs: Integer = 2; ns: Integer = 2): String;
Var
  S: String;
Begin
  S := '0.' + putBefore('0', '0', ns);
  Result := FormatFloat(S, d);
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

function Utf8TruncateByCodePoint(const AInput: String; aMaxBytesLength: Integer): TBytes;
var
  i, lCharLen: Integer;
  lPartial: String;
  lEncBytes: TBytes;
  lOutBuf: TBytes;
  lSize: Integer;
begin
  // If aMaxBytesLength <= 0, trivially return empty
  if aMaxBytesLength <= 0 then
    Exit(nil);

  // get some more memory just in case
  lSize:= 0;
  SetLength(lOutBuf, aMaxBytesLength);

  i := 1;
  while i <= Length(AInput) do
  begin
    // For a single codepoint, we might need 1 or 2 UTF-16 chars:
    // In most cases, TEncoding.UTF8 can handle surrogates if we pass them together.
    // So check if AInput[i] is a high surrogate and there's a low surrogate next.
    if (i < Length(AInput)) and
       (AInput[i] >= #$D800) and (AInput[i] <= #$DBFF) and
       (AInput[i+1] >= #$DC00) and (AInput[i+1] <= #$DFFF) then
    begin
      // This is a surrogate pair
      lPartial := Copy(AInput, i, 2);
      lCharLen := 2;
    end else begin
      // single UTF-16 code unit
      lPartial := AInput[i];
      lCharLen := 1;
    end;

    // Convert that code point (or pair) to UTF-8 bytes
    lEncBytes := TEncoding.UTF8.GetBytes(lPartial);

    // If adding these bytes would exceed aMaxBytesLength, stop.
    if (lSize+ Length(lEncBytes)) > aMaxBytesLength then
      Break;

    // Otherwise, append them
    Move(lEncBytes[0], lOutBuf[lSize], Length(lEncBytes));
    inc(lSize, Length(lEncBytes));

    // Advance i by the # of UTF-16 code units consumed
    Inc(i, lCharLen);
  end;

  setLength(lOutBuf, lSize);
  Result := lOutBuf;
end;


function CharPosEx(
  const aText: String;
  const aChars: TArray<Char>;
  aCharsArrayIsSorted: Boolean;
  aStartOffset, aEndOffset: Integer;
  out aFoundAtOffset, aIndexOfCharFound: Integer): Boolean;
var
  c: Char;
  i: NativeInt;
begin
  Result:= False;
  if Length(aChars) = 0 then
    Exit(False);
  if aStartOffset < 1 Then
    Exit(False);
  if aEndOffset > Length(aText) then
    aEndOffset:= Length(aText);
  if aEndOffset < aStartOffset then
    Exit(False);
  if aCharsArrayIsSorted then
  begin
    for var x:= aStartOffset to aEndOffset do
    begin
      if TArray.BinarySearch<Char>(aChars, aText[x], i) then
      begin
        aFoundAtOffset:= x;
        aIndexOfCharFound:= i;
        Exit(True);
      end;
    end;
  end else begin
    for var x:= aStartOffset to aEndOffset do
    begin
      c:= aText[x];
      for var y := Low(aChars) to High(aChars) do
      if c = aChars[y] then
      begin
        aIndexOfCharFound:= y;
        aFoundAtOffset:= x;
        Exit(True);
      end;
    end;
  end;
end;


Function StrToFloatWCC(

Const Text: String): double;
Var
  s: String;
Begin
  s := Text;
  PrepareTextForStrToFloatWcc(s);
  Result := StrToFLoat(s);
End;

Function TryStrToFloatWCC(

Const Text: String; Out Value: double): Boolean;
Var
  s: String;
Begin
  s := Text;
  PrepareTextForStrToFloatWcc(s);

  Result := system.sysUtils.TryStrToFloat(s, Value);
End;

Function StrToFloatWCCDef(Const Text: String; Const Default: double): double;
Var
  s: String;
Begin
  s := Text;
  PrepareTextForStrToFloatWcc(s);

  Result := Default;
  Result := StrToFloatDef(s, Default);
End;

Procedure PrepareTextForStrToFloatWcc(Var s: String);
Var
  x: Integer;
  decimalSeparatorIndex, FirstDotIndex, firstCommaIndex, DotCount,
    CommaCount: Integer;
  delComma, delDot: Boolean;
Begin
  For x := Length(s) Downto 1 Do
    // some idiot introduced 0 indexed strings in system.character... so remember to account for that....
    If char.IsWhiteSpace(s, x - 1) Then
      Delete(s, x, 1);

  If s = '' Then
    Exit;

  FirstDotIndex := 0;
  firstCommaIndex := 0;
  DotCount := 0;
  CommaCount := 0;

  For x := 1 To Length(s) Do
  Begin
    If system.sysUtils.CharInSet(s[x], ['a' .. 'z', 'A' .. 'Z']) Then
      Exit;

    If s[x] = '.' Then
    Begin
      If DotCount = 0 Then
        FirstDotIndex := x;
      Inc(DotCount);
    End
    Else If s[x] = ',' Then
    Begin
      If CommaCount = 0 Then
        firstCommaIndex := x;
      Inc(CommaCount);
    End;
  End;

  delComma := true;
  delDot := true;
  decimalSeparatorIndex := -1;
  If (DotCount = 1) And (CommaCount = 1) Then
  Begin
    // if both were found use the most right of them
    If FirstDotIndex > firstCommaIndex Then
    Begin
      decimalSeparatorIndex := FirstDotIndex;
      delDot := False;
    End
    Else
    Begin
      delComma := False;
      decimalSeparatorIndex := firstCommaIndex;
    End;
  End
  Else If DotCount = 1 Then
  Begin
    decimalSeparatorIndex := FirstDotIndex;
    delDot := False;
  End
  Else If CommaCount = 1 Then
  Begin
    decimalSeparatorIndex := firstCommaIndex;
    delComma := False;
  End;

  For x := Length(s) Downto 1 Do
  Begin
    If x = decimalSeparatorIndex Then
      s[x] := FormatSettings.DecimalSeparator
    Else
      Case s[x] Of
        '.':
          If delDot Then
            Delete(s, x, 1);
        ',':
          If delComma Then
            Delete(s, x, 1);
        ' ':
          Delete(s, x, 1);
      End;
  End;
End;


end.
