unit maxLogic.StrUtils;

{$IFDEF MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections, System.Generics.Defaults,
  System.StrUtils, System.RegularExpressions;

const
  CR = sLineBreak;

// like system.masks.matchesMasks but platform independant and with the option to be case sensitive/insensitive
function StringMatches(const aValue, aPattern: string;
  aCaseSensitive: boolean = True): boolean;

function putBefore(const AString: string; AChar: char; TotalLength: integer): string; overload;
function putBefore(num: integer; AChar: char; TotalLength: integer): string; overload;
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
function ExtractString(
  const aText, aStartMarker, aEndMarker: string;
  aStartoffset: integer;
  out aValue: string;
  out aStartMarkerFoundAtIndex: integer;
  aCasesensitive: boolean = True;
  { aInvalidChars: used for the following scenario If we search for #*#, but only on the same line,
  we can pass aInvalidChars  = [#10], in that case if the #10 will come before the end marker, it will
  indicates that this is indeed not a end marker at all
  Note: the search for invalidChars is always case sensitive}
  const aInvalidChars: TArray<char> = [];
  aInvalidCharsAreSorted: boolean = False): boolean; overload;

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
function ExtractString(
  const aOrgCasedText, aTextForCaseSensitiveSearch,
  aStartMarker, aEndMarker: string;
  aStartoffset: integer;
  out aValue: string;
  out aStartMarkerFoundAtIndex: integer;
  { aInvalidChars: used for the following scenario If we search for #*#, but only on the same line,
  we can pass aInvalidChars  = [#10], in that case if the #10 will come before the end marker, it will
  indicates that this is indeed not a end marker at all
  Note: the search for invalidChars is always case sensitive}
  const aInvalidChars: TArray<char> = [];
  aInvalidCharsAreSorted: boolean = False): boolean; overload;
function CharPosEx(
  const aText: string;
  const aChars: TArray<char>;
  aCharsArrayIsSorted: boolean;
  aStartoffset, aEndOffset: integer;
  out aFoundAtOffset, aIndexOfCharFound: integer): boolean;

type
  TReplacePlaceholderAction = (
    raReplace,
    raReplaceAndResumeAtSamePosition, // good for nested occurences,, but a bit slower as it requires in place  editing of the source text
    raSkip, // does not replace the found placeholder
    raStop, // as raSkip but also stops the search for the next item
    raReplaceAndStop // replaces this last occurence, but stops afterwards
    );
  TReplacePlaceholderOnFoundProc = reference to procedure(
    // the text between startMarker and endMarker
    const aValue: string;
    aStartMarkerFoundAtIndex: integer;
    // the String to replace the placeholder (StartMarker+value+endmarker)
    // default is the whole placeholder that s the value with the start and end markers, so if you do not touch that, it will act as action=raSkip
    var aReplaceValue: string;
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
////    - `raStop`: The loop terminates immediately. No additional text is appended within this iteration. After the loop finishes, the function appends the remainder of the original text starting from the current search offset (`aStartoffset`), which includes the placeholder and everything after it.
///    - `raReplaceAndStop`: The text before the placeholder and `aReplaceValue` are appended, then the loop terminates. After the loop finishes, the remainder of the original text starting from the updated search offset (past the replaced placeholder) is appended.
///    - `raReplaceAndResumeAtSamePosition`: This handles nested or overlapping replacements. The text before the placeholder is appended. The original text buffer (`lOrgCasedText`) and its search version (`lTextForCaseSensitiveSearch`) are *modified in-place* by substituting the placeholder with `aReplaceValue`. The search offset (`aStartoffset`) is reset to 1, and the loop continues, effectively rescanning the modified text from the beginning. This can be less performant due to string manipulation but allows for complex replacement scenarios.
/// The function uses `TStringBuilder` internally for efficient construction of the final result string.
/// After the loop finishes (either by reaching the end of the text or via a `raStop`/`raReplaceAndStop` action), any remaining portion of the text after the last processed position (`aStartoffset`) is appended to the result.
/// </remarks>
function ReplacePlaceholder(
  const aText, aStartMarker, aEndMarker: string;
  // will be caled for each occurence
  const aOnFoundProc: TReplacePlaceholderOnFoundProc;
  aStartoffset: integer = 1;
  // will search for the startMarker and Marker case sensitive or not
  aCasesensitive: boolean = True;
  { aInvalidChars: used for the following scenario If we search for #*#, but only on the same line,
  we can pass aInvalidChars  = [#10], in that case if the #10 will come before the end marker, it will
  indicates that this is indeed not a end marker at all
  Note: the search for invalidChars is always case sensitive}
  const aInvalidChars: TArray<char> = [];
  aInvalidCharsAreSorted: boolean = False
  ): string;

// will replace occurences of a system enviroment variable name withint the text with its value
// like '%appdata%\tmp\%username%'
function ExpandEnvVars(const aText: string; const aStartToken: string = '%'; const aEndToken: string = '%'): string;

function CombineUrl(const aPart1, aPart2: string; aSeparator: string = '/'): string; overload;
function CombineUrl(const aParts: array of string; aSeparator: string = '/'): string; overload;

// uses internally masks.MatchesMask
// checks if the aText mathes any of the givem filter strings
// returns always true if the filter array is empty
function MatchesFilter(const aText: string; const AFilter: TStringDynArray): boolean;

// Windows Explorer uses StrCmpLogicalW to compare file names. The RTL/VCL does not declare this function so you need to do it yourself.
// on non windows platform we are falling back on CompareStr
function StrCmpLogical(const Left, right: string): integer; inline;
{$IFDEF MSWINDOWS}

function StrCmpLogicalW(psz1, psz2: PWideChar): integer; stdcall; External 'shlwapi.dll' delayed;
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
        OrElements: TArray<string>;
        OrElementKinds: TArray<TKind>;
        OrgText: string;
      end;

  private
    fFilter: TArray<TFilterItem>;
    fOrgFilterText: string;
    // Prepares the internal filter structure based on the provided filter text.
    procedure Prepare(const aText: string);
    // we need to take care because of the special ~and quotes
    function SplitBySpace(const aText: string): TArray<string>;
    procedure Preprocess(var p: string; out k: TKind); inline;
  public
    // Creates a TFilterEx instance from the given filter text.
    class function Create(const aFilterText: string): TFilterEx; static;
    /// <summary>
    /// Returns true if the given text matches the filter or if the filter is empty
    /// </summary>
    function Matches(const aText: string): boolean;
  end;

procedure Split(const line: string; Delimiter: char; strings: TStringList); overload;
procedure Split(const line: string; Delimiter: char; out strings: TArray<string>); overload;
function Split(Delimiter: char; const line: string): TArray<string>; overload;

function SplitInHalfBy(const aText: string; aDelim: char; out aParts: TArray<string>): boolean; overload;
function SplitInHalfBy(const aText: string; aDelim: char; out alLeft, alRight: string): boolean; overload;

function fStr(const d: double; vs: integer = 2; ns: integer = 2): string;
function GuidToHex(const aGuid: TGuid): string;
function Join(const aSeparator: string; const aValues: TArray<integer>): string;

// this method ensures the num of bytes does not exceed aMaxByteLength
// it supports unicode surrogate pairs
function Utf8TruncateByCodePoint(const AInput: string; aMaxBytesLength: integer): TBytes;

// StrTo Float With Comma Correction
function StrToFloatWCC(const Text: string): double;
function TryStrToFloatWCC(const Text: string; out Value: double): boolean;
function StrToFLoatWccDef(const Text: string; const default: double): double;
procedure PrepareTextForStrToFloatWcc(var s: string);

function BytesToRawStr(const b: TBytes): RawByteString;
function RawStrToBytes(const s: RawByteString): TBytes;

type
  TStringComparerHelper = class helper for TStringComparer
    class function OrdinalIgnoreCase: TCustomComparer<String>; static;
  end;

  TMatchEvaluatorProc = reference to function(const Match: TMatch): String;
  TRegExHelper = record helper for TRegEx
    function Replace(const Input: string; Evaluator: TMatchEvaluator): string; overload;
    function Replace(const Input: string; Evaluator: TMatchEvaluator; Count: Integer): string; overload;

    class function Replace(const Input, Pattern: string; Evaluator: TMatchEvaluatorProc): string; overload; static;
    class function Replace(const Input, Pattern: string; Evaluator: TMatchEvaluatorProc; Options: TRegExOptions): string; overload; static;
  end;

implementation

uses
  System.character, System.Masks, AutoFree, System.Math;

function OccurrencesOfChar(const s: string; const c: char): integer;
var
  PC: PChar;
begin
  Result := 0;
  if s <> '' then
  begin
    PC := @s[1];
    for var i := 1 to length(s) do
    begin
      if PC^ = c then
        Inc(Result);
      Inc(PC);
    end;
  end;
end;

// --- Helper function for guaranteed case-sensitive matching using RegEx ---
// This is needed as a fallback on Windows where MatchesMask is insensitive.
function MatchesMaskCaseSensitive_RegEx(const aText, aPattern: string): Boolean;
var
  lRegexPattern: string;
begin
  // 1. Escape special RegEx characters in the original pattern FIRST.
  // This ensures characters like '.', '+', '\', '(', ')' etc., are treated literally.
  // It also escapes our wildcards '*' and '?' into '\*' and '\?'.
  lRegexPattern := TRegEx.Escape(aPattern);

  // 2. Replace the ESCAPED wildcard characters with their RegEx equivalents.
  // IMPORTANT: Replace '\*' first to avoid issues if '\?' was replaced first
  // and the pattern contained something like '\?*'.
  lRegexPattern := lRegexPattern.Replace('\*', '.*'); // Replace escaped '*' with '.*'
  lRegexPattern := lRegexPattern.Replace('\?', '.');  // Replace escaped '?' with '.'

  // 3. Anchor the pattern to match the whole string from start (^) to end ($)
  lRegexPattern := '^' + lRegexPattern + '$';

  // 4. Perform the case-sensitive match (TRegEx default behavior)
  Result := TRegEx.IsMatch(aText, lRegexPattern);
end;
// --------------------------------------------------------------------------

// Performs wildcard matching ('*' for zero or more chars, '?' for one char).
// Handles case sensitivity correctly across platforms.
function StringMatches(const aValue, aPattern: string;
  aCaseSensitive: boolean = True): boolean;
begin
  if not aCaseSensitive then
  begin
    // --- Case-Insensitive Matching ---
    // Goal: Make the comparison ignore case on ALL platforms.
    {$IFDEF MSWINDOWS}
      // On Windows, MatchesMask is already case-insensitive. Use it directly.
      Result := System.Masks.MatchesMask(aValue, aPattern);
    {$ELSE}
      // On other platforms (Linux, macOS, etc.), MatchesMask is case-sensitive.
      // Force case-insensitivity by comparing lower-case (or upper-case) versions.
      Result := System.Masks.MatchesMask(aValue.ToLower, aPattern.ToLower);
    {$ENDIF}
  end else begin
    // --- Case-Sensitive Matching ---
    // Goal: Make the comparison respect case on ALL platforms.
    {$IFDEF MSWINDOWS}
      // On Windows, MatchesMask is case-insensitive, so we *must* use a fallback.
      // Use our RegEx helper function for guaranteed case-sensitivity.
      Result := MatchesMaskCaseSensitive_RegEx(aValue, aPattern);
    {$ELSE}
      // On other platforms, MatchesMask is already case-sensitive. Use it directly.
      Result := System.Masks.MatchesMask(aValue, aPattern);
    {$ENDIF}
  end;
end;

function putBefore(const AString: string; AChar: char; TotalLength: integer): string;
var
  X, aMaxBytesLength: integer;
begin
  aMaxBytesLength := length(AString);
  if aMaxBytesLength > TotalLength then
    Result := AString
  else
  begin
    SetLength(Result, TotalLength);
    for X := 1 to TotalLength - aMaxBytesLength do
      Result[X] := AChar;
    for X := TotalLength - aMaxBytesLength + 1 to TotalLength do
      Result[X] := AString[X - (TotalLength - aMaxBytesLength)];
  end;
end;

function putBefore(num: integer; AChar: char; TotalLength: integer): string; overload;
begin
  Result := putBefore(IntToStr(num), AChar, TotalLength);
end;

function ExtractString(
  const aText, aStartMarker, aEndMarker: string;
  aStartoffset: integer;
  out aValue: string; out aStartMarkerFoundAtIndex: integer;
  aCasesensitive: boolean = True;
  { aInvalidChars: used for the following scenario If we search for #*#, but only on the same line,
  we can pass aInvalidChars  = [#10], in that case if the #10 will come before the end marker, it will
  indicates that this is indeed not a end marker at all
  Note: the search for invalidChars is always case sensitive}
  const aInvalidChars: TArray<char> = [];
  aInvalidCharsAreSorted: boolean = False): boolean;
begin
  if aCasesensitive then
    Result := ExtractString(
      aText, aText, aStartMarker, aEndMarker,
      aStartoffset, aValue, aStartMarkerFoundAtIndex, aInvalidChars, aInvalidCharsAreSorted)
  else
    Result := ExtractString(
      aText, aText.ToLower, aStartMarker.ToLower, aEndMarker.ToLower,
      aStartoffset, aValue, aStartMarkerFoundAtIndex, aInvalidChars, aInvalidCharsAreSorted);
end;

function ExtractString(
  const aOrgCasedText, aTextForCaseSensitiveSearch,
  aStartMarker, aEndMarker: string;
  aStartoffset: integer;
  out aValue: string;
  out aStartMarkerFoundAtIndex: integer;
  { aInvalidChars: used for the following scenario If we search for #*#, but only on the same line,
  we can pass aInvalidChars  = [#10], in that case if the #10 will come before the end marker, it will
  indicates that this is indeed not a end marker at all
  Note: the search for invalidChars is always case sensitive}
  const aInvalidChars: TArray<char> = [];
  aInvalidCharsAreSorted: boolean = False): boolean;
var
  i1, i2: integer;
  lInvalidCharOffset, lIndexOfInvalidChar: integer;
begin
  Result := False;

  while True do
  begin
    aStartMarkerFoundAtIndex := PosEx(aStartMarker, aTextForCaseSensitiveSearch, aStartoffset);
    if aStartMarkerFoundAtIndex = 0 then
      exit(False);

    i1 := aStartMarkerFoundAtIndex + length(aStartMarker);

    i2 := PosEx(aEndMarker, aTextForCaseSensitiveSearch, i1);
    if i2 = 0 then
      exit(False);

    if CharPosEx(aOrgCasedText,
      aInvalidChars,
      aInvalidCharsAreSorted,
      i1, i2 - 1,
      lInvalidCharOffset, lIndexOfInvalidChar) then
    begin
      aStartoffset := i1;
      Continue;
    end;

    aValue := copy(aOrgCasedText, i1, i2 - i1);
    exit(True);
  end;
end;

function ReplacePlaceholder(
  const aText, aStartMarker, aEndMarker: string;
  const aOnFoundProc: TReplacePlaceholderOnFoundProc;
  aStartoffset: integer = 1;
  aCasesensitive: boolean = True;
  { aInvalidChars: used for the following scenario If we search for #*#, but only on the same line,
  we can pass aInvalidChars  = [#10], in that case if the #10 will come before the end marker, it will
  indicates that this is indeed not a end marker at all
  Note: the search for invalidChars is always case sensitive}
  const aInvalidChars: TArray<char> = [];
  aInvalidCharsAreSorted: boolean = False
  ): string;
var
  lValue, lReplacementValue: string;
  lStartMarkerFoundAtIndex: integer;
  lAction: TReplacePlaceholderAction;
  lFound: boolean;
  lOrgCasedText, lTextForCaseSensitiveSearch, lStartMarker, lEndMarker: string;
  lMarkersLen, lPlaceHolderLen: integer;
  sb: TStringBuilder; // well, I know, there was much hate for the TStringBuilder in delphi. But at least since delphi 12 this is faster then ordinary delphi string + string,expecially for large strings
begin
  Result := '';
  gc(sb, TStringBuilder.Create);
  sb.Capacity := length(aText) * 2;

  if aStartoffset < 1 then
    aStartoffset := 1;

  // init our output with the text before the start offset, so we do not loose it
  if aStartoffset > 1 then
    sb.Append(copy(aText, 1, aStartoffset - 1));

  lOrgCasedText := aText;
  lMarkersLen := length(aStartMarker) + length(aEndMarker);
  if aCasesensitive then
  begin
    lTextForCaseSensitiveSearch := aText;
    lStartMarker := aStartMarker;
    lEndMarker := aEndMarker;
  end
  else
  begin
    lTextForCaseSensitiveSearch := aText.ToLower;
    lStartMarker := aStartMarker.ToLower;
    lEndMarker := aEndMarker.ToLower;
  end;

  repeat

    lFound := ExtractString(
      lOrgCasedText, lTextForCaseSensitiveSearch, lStartMarker, lEndMarker,
      aStartoffset,
      lValue,
      lStartMarkerFoundAtIndex,
      aInvalidChars, aInvalidCharsAreSorted);

    if not lFound then
      break;

    lAction := raReplace;
    lPlaceHolderLen := length(lValue) + lMarkersLen;
    lReplacementValue := copy(lOrgCasedText, lStartMarkerFoundAtIndex, lPlaceHolderLen);
    aOnFoundProc(lValue, lStartMarkerFoundAtIndex, lReplacementValue, lAction);

    case lAction of
      raStop:
        break;
      raSkip:
        begin
          lReplacementValue := copy(lOrgCasedText, lStartMarkerFoundAtIndex, lPlaceHolderLen);
          lAction := raReplace;
        end;
      raReplaceAndResumeAtSamePosition:
        begin
          // that one is a bit tricky
          // 1. flush the part before the marker start pos to our output
          sb.Append(
            copy(lOrgCasedText,
            aStartoffset, (lStartMarkerFoundAtIndex - aStartoffset)));
          // 2. trim both text buffers and prepend with the pepalacement value
          lOrgCasedText := lReplacementValue +
            copy(lOrgCasedText, lStartMarkerFoundAtIndex + lPlaceHolderLen, length(lOrgCasedText));
          if not aCasesensitive then
            lReplacementValue := lReplacementValue.ToLower;
          lTextForCaseSensitiveSearch := lReplacementValue +
            copy(lTextForCaseSensitiveSearch, lStartMarkerFoundAtIndex + lPlaceHolderLen, length(lTextForCaseSensitiveSearch));
          // 3. Reset the start offset to 1 as we want to search from the start again
          aStartoffset := 1;
          Continue; // we want to skip the code that follows
        end;
    end;

    // copy the part from the start position until the position of the marker, then add the replacement text to the output
    sb.Append(
      copy(lOrgCasedText,
      aStartoffset, (lStartMarkerFoundAtIndex - aStartoffset)));
    sb.Append(lReplacementValue);

    aStartoffset := lStartMarkerFoundAtIndex + lPlaceHolderLen;
  until (not lFound) or (lAction in [raStop, raReplaceAndStop]);

  // append the rest of the Text
  if aStartoffset <= length(lOrgCasedText) then
    sb.Append(copy(lOrgCasedText, aStartoffset, length(lOrgCasedText)));
  Result := sb.ToString;
end;

function CombineUrl(const aPart1, aPart2: string; aSeparator: string = '/'): string;
begin
  Result := CombineUrl([aPart1, aPart2], aSeparator);
end;

function CombineUrl(const aParts: array of string; aSeparator: string = '/'): string;
var
  X: integer;
begin
  case length(aParts) of
    0:
      Result := '';
    1:
      Result := aParts[0];
  else
    begin
      Result := aParts[0];
      for X := 1 to length(aParts) - 1 do
      begin
        if not endsText(aSeparator, Result) then
          Result := Result + aSeparator;
        if startsText(aSeparator, aParts[X]) then
          Result := Result + copy(aParts[X], length(aSeparator) + 1, length(aParts[X]))
        else
          Result := Result + aParts[X];
      end;
    end;
  end;
end;

function MatchesFilter(const aText: string;
  const AFilter: TStringDynArray): boolean;
var
  lFilter: string;
begin
  if length(AFilter) = 0 then
    exit(True);

  Result := False;
  for lFilter in AFilter do
  begin
    Result := System.Masks.MatchesMask(aText, lFilter);
    if Result then
      break;
  end;

end;

function ExpandEnvVars(const aText: string; const aStartToken: string = '%'; const aEndToken: string = '%'): string;
begin
  Result := ReplacePlaceholder(
    aText, aStartToken, aEndToken,

    procedure(
      // the text between startMarker and endMarker
      const aValue: string;
      aStartMarkerFoundAtIndex: integer;
      // the String to replace the placeholder (StartMarker+value+endmarker)
      // default is the whole placeholder that s the value with the start and end markers, so if you do not touch that, it will act as action=raSkip
      var aReplaceValue: string;
      // default raReplace
      var aAction: TReplacePlaceholderAction)
    var
      v: string;
    begin
      v := System.SysUtils.GetEnvironmentVariable(aValue);
      if v = '' then
        aAction := raSkip
      else
        aReplaceValue := v;
    end,
    1, False);
end;

function StrCmpLogical(const Left, right: string): integer;
begin
  {$IFDEF MSWINDOWS}
  Result := StrCmpLogicalW(PWideChar(Left), PWideChar(right));
  {$ELSE}
  Result := CompareStr(Left, right);
  {$ENDIF}
end;

{ TFilterEx }

class function TFilterEx.Create(const aFilterText: string): TFilterEx;
begin
  Result := default(TFilterEx);
  Result.Prepare(aFilterText);
end;

procedure TFilterEx.Prepare(const aText: string);
var
  ar: TArray<string>;
  fi: TFilterItem;
  l: TStringList;
  p: string;
  k: TKind;
begin
  gc(l, TStringList.Create);
  l.StrictDelimiter := True;
  l.Delimiter := '|';
  l.QuoteChar := '"';

  fOrgFilterText := aText;
  ar := SplitBySpace(aText.Trim.ToLower);
  SetLength(fFilter, length(ar));
  for var X := 0 to High(ar) do
  begin
    fi := default(TFilterItem);
    p := Trim(ar[X]);
    if p = '' then
      Continue;
    fi.OrgText := p;
    if startsText('!', p) then
    begin
      Delete(p, 1, 1);
      fi.IsNegated := True;
      if p = '' then
        Continue;
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
    fFilter[X] := fi;
  end;
end;

procedure TFilterEx.Preprocess(var p: string; out k: TKind);
var
  i1, i2: integer;
begin
  if pos('?', p) > 0 then
    k := kMask
  else
  begin
    i1 := pos('*', p);
    if i1 < 1 then
      k := kContains
    else if i1 = length(p) then
    begin
      Delete(p, length(p), 1);
      k := kStarts;
    end
    else if i1 = 1 then
    begin
      i2 := 2;
      i2 := PosEx('*', p, i2);
      if i2 < 1 then
      begin
        Delete(p, 1, 1);
        k := kEnds;
      end
      else if i2 = length(p) then
      begin
        p := copy(p, 2, length(p) - 2);
        k := kContains;
      end
      else
        k := kMask;
    end
    else
      k := kMask;
  end;
end;

function TFilterEx.SplitBySpace(const aText: string): TArray<string>;
var
  s, p: string;
  i1, i2: integer;
  l: TList<string>;
  lIsInQuote: boolean;
  c: char;
begin
  s := aText + ' ';
  gc(l, TList<string>.Create);
  lIsInQuote := False;
  i1 := 1;
  i2 := 1; // why not 2? because the first char may contain a '"' char....

  while i2 <= length(s) do
  begin
    c := s[i2];
    if lIsInQuote then
    begin
      if c = '"' then
        lIsInQuote := False;

    end
    else if c = '"' then
      lIsInQuote := True
    else if c = ' ' then
    begin
      if i1 <> i2 then
      begin
        p := copy(s, i1, (i2 - i1));
        l.add(p);
      end;
      i1 := i2 + 1;
    end;
    Inc(i2);
  end;

  Result := l.ToArray;
end;

function TFilterEx.Matches(const aText: string): boolean;
var
  lMatchesANy: boolean;
  fi: TFilterItem;
  s: string;
  lText: string;
begin
  Result := True;

  lText := aText.ToLower;
  for fi in fFilter do
  begin
    lMatchesANy := False;
    for var X := 0 to length(fi.OrElements) - 1 do
    begin
      s := fi.OrElements[X];
      case fi.OrElementKinds[X] of
        kMask:
          lMatchesANy := System.Masks.MatchesMask(lText, s);
        kContains:
          lMatchesANy := lText.Contains(s);
        kStarts:
          lMatchesANy := StartsStr(s, lText);
        kEnds:
          lMatchesANy := EndsStr(s, lText);
      end;
      if lMatchesANy then
        break;
    end;

    if fi.IsNegated then
    begin
      if lMatchesANy then
        exit(False);
    end
    else if not lMatchesANy then
      exit(False);
  end;
end;

{ other }

function SplitInHalfBy(const aText: string; aDelim: char; out aParts: TArray<string>): boolean;
var
  i: integer;
begin
  Result := False;
  i := pos(aDelim, aText);
  if i < 1 then
    aParts := [aText]
  else
  begin
    Result := True;
    aParts := [
      copy(aText, 1, i - 1),
      copy(aText, i + 1, length(aText))
      ];
  end;
end;

function SplitInHalfBy(const aText: string; aDelim: char; out alLeft, alRight: string): boolean; overload;
var
  i: integer;
begin
  Result := False;
  i := pos(aDelim, aText);
  if i >= 1 then
  begin
    Result := True;
    alLeft := copy(aText, 1, i - 1);
    alRight := copy(aText, i + 1, length(aText));
  end;
end;

function Split(Delimiter: char; const line: string): TArray<string>;
begin
  Split(line, Delimiter, Result);
end;

procedure Split(const line: string; Delimiter: char; out strings: TArray<string>);
var
  l: TStringList;
begin
  gc(l, TStringList.Create);
  l.StrictDelimiter := True;
  l.Delimiter := Delimiter;
  l.DelimitedText := line;
  strings := l.ToStringArray;
end;

procedure Split(const line: string; Delimiter: char; strings: TStringList);
var
  l: TStringList;
begin
  gc(l, TStringList.Create);
  l.StrictDelimiter := True;
  l.Delimiter := Delimiter;
  l.DelimitedText := line;
  strings.Clear;
  strings.AddStrings(l);
end;

function fStr(const d: double; vs: integer = 2; ns: integer = 2): string;
var
  s: string;
begin
  s := '0.' + putBefore('0', '0', ns);
  Result := FormatFloat(s, d);
end;

function GuidToHex(const aGuid: TGuid): string;
begin
  SetLength(Result, SizeOf(TGuid) * 2);
  BinToHex(@aGuid, PChar(Result), SizeOf(TGuid));
end;

function Join(const aSeparator: string; const aValues: TArray<integer>): string;
var
  lValues: TArray<string>;
begin
  SetLength(lValues, length(aValues));
  for var X := 0 to length(aValues) - 1 do
    lValues[X] := aValues[X].ToString;
  Result := string.Join(aSeparator, lValues);
end;

function Utf8TruncateByCodePoint(const AInput: string; aMaxBytesLength: integer): TBytes;
var
  src: TBytes;
  i, nextLen, total: Integer;
  b: Byte;
begin
  if aMaxBytesLength <= 0 then
    Exit(nil);

  src := TEncoding.UTF8.GetBytes(AInput);
  if Length(src) <= aMaxBytesLength then
    Exit(src);

  i := 0;
  total := 0;
  while i < Length(src) do
  begin
    b := src[i];
    if b < $80 then
      nextLen := 1
    else if (b and $E0) = $C0 then
      nextLen := 2
    else if (b and $F0) = $E0 then
      nextLen := 3
    else if (b and $F8) = $F0 then
      nextLen := 4
    else
      Break;

    if total + nextLen > aMaxBytesLength then
      Break;

    Inc(total, nextLen);
    Inc(i, nextLen);
  end;

  SetLength(Result, total);
  if total > 0 then
    Move(src[0], Result[0], total);
end;

function CharPosEx(
  const aText: string;
  const aChars: TArray<char>;
  aCharsArrayIsSorted: boolean;
  aStartoffset, aEndOffset: integer;
  out aFoundAtOffset, aIndexOfCharFound: integer): boolean;
var
  c: char;
  i: Integer;
begin
  Result := False;
  if length(aChars) = 0 then
    exit(False);
  if aStartoffset < 1 then
    exit(False);
  if aEndOffset > length(aText) then
    aEndOffset := length(aText);
  if aEndOffset < aStartoffset then
    exit(False);
  if aCharsArrayIsSorted then
  begin
    for var X := aStartoffset to aEndOffset do
    begin
      if TArray.BinarySearch<char>(aChars, aText[X], i) then
      begin
        aFoundAtOffset := X;
        aIndexOfCharFound := i;
        exit(True);
      end;
    end;
  end
  else
  begin
    for var X := aStartoffset to aEndOffset do
    begin
      c := aText[X];
      for var Y := Low(aChars) to High(aChars) do
        if c = aChars[Y] then
        begin
          aIndexOfCharFound := Y;
          aFoundAtOffset := X;
          exit(True);
        end;
    end;
  end;
end;

function StrToFloatWCC(

  const Text: string): double;
var
  s: string;
begin
  s := Text;
  PrepareTextForStrToFloatWcc(s);
  Result := StrToFLoat(s);
end;

function TryStrToFloatWCC(

  const Text: string; out Value: double): boolean;
var
  s: string;
begin
  s := Text;
  PrepareTextForStrToFloatWcc(s);

  Result := System.SysUtils.TryStrToFloat(s, Value);
end;

function StrToFLoatWccDef(const Text: string; const default: double): double;
var
  s: string;
begin
  s := Text;
  PrepareTextForStrToFloatWcc(s);

  Result := default;
  Result := StrToFloatDef(s, default);
end;

procedure PrepareTextForStrToFloatWcc(var s: string);
var
  X: integer;
  decimalSeparatorIndex, FirstDotIndex, firstCommaIndex, DotCount,
    CommaCount: integer;
  delComma, delDot: boolean;
  LastDotIndex, LastCommaIndex: integer;
begin
  for X := length(s) downto 1 do
    // some idiot introduced 0 indexed strings in system.character... so remember to account for that....
    if char.IsWhiteSpace(s, X - 1) then
      Delete(s, X, 1);

  if s = '' then
    exit;

  FirstDotIndex := 0;
  firstCommaIndex := 0;
  DotCount := 0;
  CommaCount := 0;
  LastDotIndex := 0;
  LastCommaIndex := 0;

  for X := 1 to length(s) do
  begin
    if System.SysUtils.CharInSet(s[X], ['a'..'z', 'A'..'Z']) then
      exit;

    if s[X] = '.' then
    begin
      if DotCount = 0 then
        FirstDotIndex := X;
      Inc(DotCount);
      LastDotIndex := X;
    end
    else if s[X] = ',' then
    begin
      if CommaCount = 0 then
        firstCommaIndex := X;
      Inc(CommaCount);
      LastCommaIndex := X;
    end;
  end;

  delComma := True;
  delDot := True;
  decimalSeparatorIndex := -1;

  if (DotCount > 0) and (CommaCount > 0) then
  begin
    // Heuristic:
    // - one dot & multiple commas -> first comma is decimal (e.g. '1.234,567,89' -> 1234.56789)
    // - multiple dots & one comma -> last dot is decimal (e.g. '1,234.567.89' -> 1234567.89)
    // - otherwise -> rightmost of '.' or ',' is decimal
    if (DotCount = 1) and (CommaCount >= 2) then
      decimalSeparatorIndex := firstCommaIndex
    else if (DotCount >= 2) and (CommaCount = 1) then
      decimalSeparatorIndex := LastDotIndex
    else
    begin
      if LastDotIndex > LastCommaIndex then
        decimalSeparatorIndex := LastDotIndex
      else
        decimalSeparatorIndex := LastCommaIndex;
    end;
  end
  else if (DotCount + CommaCount = 1) then
  begin
    // one separator total: decide decimal vs grouping-only
    var lSepIndex: Integer := IfThen(DotCount = 1, FirstDotIndex, firstCommaIndex);
    var lDigitsRight: Integer := 0;
    var lJ: Integer := lSepIndex + 1;

    while (lJ <= Length(s)) and System.SysUtils.CharInSet(s[lJ], ['0'..'9']) do
    begin
      Inc(lDigitsRight);
      Inc(lJ);
    end;

    if (lJ > Length(s)) and (lDigitsRight = 3) then
    begin
      // treat as thousands grouping: keep decimalSeparatorIndex = -1 so the separator is removed
      // delDot/delComma remain True to delete the single separator
    end
    else if DotCount = 1 then
    begin
      decimalSeparatorIndex := FirstDotIndex;
      delDot := False;    // keep the single dot as decimal
    end
    else
    begin
      decimalSeparatorIndex := firstCommaIndex;
      delComma := False;  // keep the single comma as decimal
    end;
  end
  else if DotCount = 1 then
  begin
    decimalSeparatorIndex := FirstDotIndex;
    delDot := False;
  end
  else if CommaCount = 1 then
  begin
    decimalSeparatorIndex := firstCommaIndex;
    delComma := False;
  end;

  for X := length(s) downto 1 do
  begin
    if X = decimalSeparatorIndex then
      s[X] := FormatSettings.DecimalSeparator
    else
      case s[X] of
        '.':
          if delDot then
            Delete(s, X, 1);
        ',':
          if delComma then
            Delete(s, X, 1);
        ' ':
          Delete(s, X, 1);
      end;
  end;
end;


function BytesToRawStr(const b: TBytes): RawByteString;
begin
  setLength(Result, Length(b));
  if length(b) > 0 then
    move(b[0], Result[1], length(b));
end;

function RawStrToBytes(const s: RawByteString): TBytes;
begin
  SetLength(Result, Length(s));
  if length(s)<>0 then
    move(s[1], Result[0], length(s));
end;

{ TStringComparerHelper }
class function TStringComparerHelper.OrdinalIgnoreCase: TCustomComparer<String>;
begin
  Result:= TIStringComparer.Ordinal;
end;

{ TRegExHelper }

{$REGION 'MatchEvaluatorProcHolder'}
type
  TMatchEvaluatorProcHolder = class
  private
    fProc: TMatchEvaluatorProc;
    function Evaluator(const aMatch: TMatch): String;
  end;

function TMatchEvaluatorProcHolder.Evaluator(const aMatch: TMatch): String;
begin
  if assigned(fProc) then
    Result:= fProc(aMatch);
end;
{$ENDREGION 'RegExHelperClass'}


class function TRegExHelper.Replace(const Input, Pattern: string;
  Evaluator: TMatchEvaluatorProc; Options: TRegExOptions): string;
var
  h: TMatchEvaluatorProcHolder ;
begin
  gc(h, TMatchEvaluatorProcHolder.Create);
  h.fProc:= Evaluator;
  Result:= TRegEx.Replace(Input, Pattern, h.Evaluator, Options);
end;

class function TRegExHelper.Replace(const Input, Pattern: string;
  Evaluator: TMatchEvaluatorProc): string;
var
  h: TMatchEvaluatorProcHolder ;
begin
  gc(h, TMatchEvaluatorProcHolder.Create);
  h.fProc:= Evaluator;
  Result:= TRegEx.Replace(Input, Pattern, h.Evaluator);
end;

function TRegExHelper.Replace(const Input: string;
  Evaluator: TMatchEvaluator; Count: Integer): string;
var
  h: TMatchEvaluatorProcHolder ;
begin
  gc(h, TMatchEvaluatorProcHolder.Create);
  h.fProc:= Evaluator;
  Result:= Replace(Input, h.Evaluator, Count);
end;

function TRegExHelper.Replace(const Input: string;
  Evaluator: TMatchEvaluator): string;
var
  h: TMatchEvaluatorProcHolder ;
begin
  gc(h, TMatchEvaluatorProcHolder.Create);
  h.fProc:= Evaluator;
  Result:= Replace(Input, h.Evaluator);
end;

end.

