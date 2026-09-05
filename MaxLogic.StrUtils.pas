unit maxLogic.StrUtils;

{$IFDEF MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

uses
  System.Character, System.Classes, System.Generics.Collections, System.Generics.Defaults,
  System.RegularExpressions, System.StrUtils, System.SysUtils, System.Types;

const
  CR = sLineBreak;

// like system.masks.matchesMasks but platform independant and with the option to be case sensitive/insensitive
function StringMatches(const aValue, aPattern: string;
  aCaseSensitive: boolean = True): boolean; overload;
// Native UTF-8 masks use scalar wildcards and ordinal BMP case folding; no temporary strings.
function StringMatches(const aValue, aPattern: UTF8String;
  aCaseSensitive: Boolean = True): Boolean; overload;

function PutBefore(const AString: string; AChar: char; TotalLength: integer): string; overload;
function PutBefore(num: integer; AChar: char; TotalLength: integer): string; overload;
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
  aCaseSensitive: boolean = True;
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
  ///   The UTF-16 overloads of TFastCaseAwareComparer keep allocations at zero for ASCII-heavy workloads by folding characters
  ///   through a lookup table and feeding them into a highly parallelized XXHash-style logic with overflow checks disabled.
  ///   Non-ASCII inputs fall back to TCharHelper.ToUpper calls on the fly, avoiding string allocations.
  ///   Latest DUnitX perf runs (Debug/Win32, 200k mixed keys) show ~2.8× vs TIStringComparer.Ordinal for case-insensitive hashes
  ///   and ~1.7× vs TStringComparer.Ordinal for ordinal mode, so prefer this comparer when we own both the key creation and dictionary lifetime.
  ///   If a caller can’t tolerate case-folding differences or needs locale-aware comparisons, stick with the RTL comparer.
  ///   Semantics stay strictly ordinal: no locale-driven expansions such as 'ß' → 'SS', ligature breaking, etc.; these inputs remain distinct.
  ///   Consumers that require locale-aware folding must continue using TIStringComparer/CompareText instead of this fast comparer.
  /// </summary>
  TFastCaseAwareComparer = class(TInterfacedObject, IEqualityComparer<string>, IEqualityComparer<UTF8String>)
  private
    FCaseSensitive: Boolean;
    class var FUpperAscii: array[0..255] of Char;
    class var FOrdinalUtf8Comparer: IEqualityComparer<UTF8String>;
    class var FOrdinalIgnoreCaseUtf8Comparer: IEqualityComparer<UTF8String>;
    class var FOrdinalComparer: IEqualityComparer<string>;
    class var FOrdinalIgnoreCaseComparer: IEqualityComparer<string>;
    class constructor Create;
    class destructor Destroy;
    class function FoldCharValue(aChar: Char): Word; static; inline;
    class function FoldPair(const aPair: Cardinal): Cardinal; static; inline;
    class function Rotl32(aValue: Cardinal; aBits: Integer): Cardinal; static; inline;
    class function ReadCardinalUnaligned(const aPtr: Pointer): Cardinal; static; inline;
    class function HashBytes(const aData: PByte; aLength: NativeInt): Cardinal; static;
    class function HashOrdinal(const aValue: string): Integer; static;
    class function HashCaseInsensitive(const aValue: string): Integer; static;
    class function GetOrdinalComparer: IEqualityComparer<string>; static;
    class function GetOrdinalIgnoreCaseComparer: IEqualityComparer<string>; static;
  public
    constructor Create(aCaseSensitive: Boolean);
    class function Ordinal: IEqualityComparer<string>; static;
    class function OrdinalIgnoreCase: IEqualityComparer<string>; static;
    // UTF-8 ordinal mode compares/hashes bytes. Ignore-case reads scalar values directly,
    // folding BMP characters as the UTF-16 comparer does; no temporary strings are allocated.
    // These factories return shared instances; hash values are specific to the string type.
    class function OrdinalUtf8: IEqualityComparer<UTF8String>; static;
    class function OrdinalIgnoreCaseUtf8: IEqualityComparer<UTF8String>; static;
    function Equals(const aLeft, aRight: string): Boolean; reintroduce; overload;
    function Equals(const aLeft, aRight: UTF8String): Boolean; reintroduce; overload;
    function GetHashCode(const aValue: string): Integer; reintroduce; overload;
    function GetHashCode(const aValue: UTF8String): Integer; reintroduce; overload;
  end;

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
  aCaseSensitive: boolean = True;
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
function MatchesFilter(const aText: string; const aFilter: TStringDynArray): boolean; overload;
function MatchesFilter(const aText: UTF8String; const aFilter: TArray<UTF8String>): Boolean; overload;

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

  /// UTF-8-owned filter. AND, OR (|), negation (!) and quoted terms follow TFilterEx syntax.
  /// Matching uses ordinal simple BMP uppercase folding without locale collation/normalization.
  /// Masks support '*', '?', '[abc]', '[!abc]' and '[a-z]'; '?' consumes one Unicode scalar.
  /// Malformed UTF-8 bytes remain distinct opaque values. Matches allocates no heap memory.
  TFilterExUtf8 = record
  private type
    TKind = (ukContains, ukStarts, ukEnds, ukMask);
    TTerm = record
      fText: UTF8String;
      fKind: TKind;
    end;
    TGroup = record
      fIsNegated: Boolean;
      fTerms: TArray<TTerm>;
    end;
  private
    fGroups: TArray<TGroup>;
    class function ParseTerm(const aText: UTF8String): TTerm; static;
  public
    class function Create(const aFilterText: UTF8String): TFilterExUtf8; static;
    function Matches(const aText: UTF8String): Boolean;
  end;

procedure Split(const line: string; delimiter: char; STRINGS: TStringList); overload;
procedure Split(const line: string; delimiter: char; out STRINGS: TArray<string>); overload;
function Split(delimiter: char; const line: string): TArray<string>; overload;
Function splitstring(Const aText: String; aDelim: char; aPerformTrimOnParts: Boolean = False): TArray<String>; 
  
function SplitInHalfBy(const aText: string; aDelim: char; out aParts: TArray<string>): boolean; overload;
function SplitInHalfBy(const aText: string; aDelim: char; out alLeft, alRight: string): boolean; overload;

function fStr(const d: double; vs: integer = 2; ns: integer = 2): string;
function GuidToHex(const aGuid: TGuid): string;
function Join(const aSeparator: string; const aValues: TArray<integer>): string; overload;
function Join(const aSeparator: string; aValues: TStringList): string; overload;


// this method ensures the num of bytes does not exceed aMaxByteLength
// it supports unicode surrogate pairs
function Utf8TruncateByCodePoint(const AInput: string; aMaxBytesLength: integer): TBytes;

// StrTo Float With Comma Correction
function StrToFloatWCC(const Text: string): double;
function TryStrToFLoatWCC(const Text: string; out Value: double): boolean;
function StrToFLoatWccDef(const Text: string; const default: double): double;
procedure PrepareTextForStrToFloatWcc(var s: string);

function BytesToRawStr(const b: TBytes): rawByteString;
function RawStrToBytes(const s: rawByteString): TBytes;

type
  TStringComparerHelper = class helper for TStringComparer
    class function OrdinalIgnoreCase: TCustomComparer<string>; static;
  end;

  // Anonymous-method evaluator type
  TMatchEvaluatorProc = reference to function(const match: TMatch): string;

  TRegExHelper = record helper for TRegEx
  private
    type
      // Bridges anonymous method -> 'of object' method pointer
      TProcAdapter = class
      private
        fProc: TMatchEvaluatorProc;
      public
        constructor Create(const aProc: TMatchEvaluatorProc);
        function Invoke(const match: TMatch): string;
      end;
  public
    // INSTANCE overloads accepting anonymous methods (what your call needs)
    function Replace(const input: string; Evaluator: TMatchEvaluatorProc): string; overload;
    function Replace(const input: string; Evaluator: TMatchEvaluatorProc; Count: integer): string; overload;

    // Optional: keep your class overloads too
    class function Replace(const input, pattern: string; Evaluator: TMatchEvaluatorProc): string; overload; static;
    class function Replace(const input, pattern: string; Evaluator: TMatchEvaluatorProc; Options: TRegExOptions): string; overload; static;
  end;

function PrettyElapsed(const aMs: Int64): string;
function i2s(const i: integer): string; inline;

// converts a double (its binary delphi representation) into a hex string
// aMinLength: will add or remove leading '0' to match this len
function floatToHex(const f: double; aMinLength: integer = SizeOf(double) * 2): string;

// pretty print a size in bytes
Function PrettyPrintSize(Const ByteCount: int64): String;
Function MegaBytesToStr(Const MegaByteCount: extended): String;

{ This function will replace tokenized keywords (=words enclosed by the token char, like :myToken: by its parameters.  (*
  like this:
  ParseStatement ('SELECT :id:, :field2:, from :t WHERE :f1 = :v1', ['id, 'my_id_field',
  'field2',  'my_field2_value,
  't ',  'myTableName',
  'f1 ', 'myf1Value',
  'v1', '''myValue2 please note the quotes''']))
  // this should be easier to read then a long statement with many string concatenations and value retriving code...
  The params are case insensitive
}
Function ParseStatement(Const Statement: String; Const params: TArray<String>;
  Const Token: char = ':'): String;

implementation

uses
  System.Masks, System.Math,
  AutoFree;

function OccurrencesOfChar(const s: string; const c: char): integer;
var
  pc: PChar;
  lEndPtr: PChar;
begin
  Result := 0;
  if s <> '' then
  begin
    pc := PChar(s);
    lEndPtr := pc + Length(s);
    while pc < lEndPtr do
    begin
      if pc^ = c then
        Inc(Result);
      Inc(pc);
    end;
  end;
end;

// --- Helper function for guaranteed case-sensitive matching using RegEx ---
// This is needed as a fallback on Windows where MatchesMask is insensitive.
function MatchesMaskCaseSensitive_RegEx(const aText, aPattern: string): boolean;
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
  lRegexPattern := lRegexPattern.Replace('\?', '.'); // Replace escaped '?' with '.'

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

function PutBefore(const AString: string; AChar: char; TotalLength: integer): string;
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

function PutBefore(num: integer; AChar: char; TotalLength: integer): string; overload;
begin
  Result := PutBefore(IntToStr(num), AChar, TotalLength);
end;

function ExtractString(
  const aText, aStartMarker, aEndMarker: string;
  aStartoffset: integer;
  out aValue: string; out aStartMarkerFoundAtIndex: integer;
  aCaseSensitive: boolean = True;
  { aInvalidChars: used for the following scenario If we search for #*#, but only on the same line,
  we can pass aInvalidChars  = [#10], in that case if the #10 will come before the end marker, it will
  indicates that this is indeed not a end marker at all
  Note: the search for invalidChars is always case sensitive}
  const aInvalidChars: TArray<char> = [];
  aInvalidCharsAreSorted: boolean = False): boolean;
begin
  if aCaseSensitive then
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
      exit;

    i1 := aStartMarkerFoundAtIndex + length(aStartMarker);

    i2 := PosEx(aEndMarker, aTextForCaseSensitiveSearch, i1);
    if i2 = 0 then
      exit;

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
    Result := True;
    exit;
  end;
end;

function ReplacePlaceholder(
  const aText, aStartMarker, aEndMarker: string;
  const aOnFoundProc: TReplacePlaceholderOnFoundProc;
  aStartoffset: integer = 1;
  aCaseSensitive: boolean = True;
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
    sb.append(copy(aText, 1, aStartoffset - 1));

  lOrgCasedText := aText;
  lMarkersLen := length(aStartMarker) + length(aEndMarker);
  if aCaseSensitive then
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
      raReplace:
        begin
          lAction := raReplace;
        end;
      raReplaceAndStop:
        begin
          lAction := raReplaceAndStop;
        end;
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
          sb.append(
            copy(lOrgCasedText,
            aStartoffset, (lStartMarkerFoundAtIndex - aStartoffset)));
          // 2. trim both text buffers and prepend with the pepalacement value
          lOrgCasedText := lReplacementValue +
            copy(lOrgCasedText, lStartMarkerFoundAtIndex + lPlaceHolderLen, length(lOrgCasedText));
          if not aCaseSensitive then
            lReplacementValue := lReplacementValue.ToLower;
          lTextForCaseSensitiveSearch := lReplacementValue +
            copy(lTextForCaseSensitiveSearch, lStartMarkerFoundAtIndex + lPlaceHolderLen, length(lTextForCaseSensitiveSearch));
          // 3. Reset the start offset to 1 as we want to search from the start again
          aStartoffset := 1;
          Continue; // we want to skip the code that follows
        end;
    end;

    // copy the part from the start position until the position of the marker, then add the replacement text to the output
    sb.append(
      copy(lOrgCasedText,
      aStartoffset, (lStartMarkerFoundAtIndex - aStartoffset)));
    sb.append(lReplacementValue);

    aStartoffset := lStartMarkerFoundAtIndex + lPlaceHolderLen;
  until (not lFound) or (lAction in [raStop, raReplaceAndStop]);

  // append the rest of the Text
  if aStartoffset <= length(lOrgCasedText) then
    sb.append(copy(lOrgCasedText, aStartoffset, length(lOrgCasedText)));
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
        if StartsText(aSeparator, aParts[X]) then
          Result := Result + copy(aParts[X], length(aSeparator) + 1, length(aParts[X]))
        else
          Result := Result + aParts[X];
      end;
    end;
  end;
end;

function MatchesFilter(const aText: string;
  const aFilter: TStringDynArray): boolean;
var
  lFilter: string;
begin
  if length(aFilter) = 0 then
    exit(True);

  Result := False;
  for lFilter in aFilter do
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
  l.delimiter := '|';
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
    if StartsText('!', p) then
    begin
      delete(p, 1, 1);
      fi.IsNegated := True;
      if p = '' then
        Continue;
    end;

    l.delimitedText := p;

    SetLength(fi.OrElements, l.Count);
    SetLength(fi.OrElementKinds, l.Count);
    for var y := 0 to l.Count - 1 do
    begin
      p := l[y];
      Preprocess(p, k);
      fi.OrElements[y] := p;
      fi.OrElementKinds[y] := k;
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
      delete(p, length(p), 1);
      k := kStarts;
    end
    else if i1 = 1 then
    begin
      i2 := 2;
      i2 := PosEx('*', p, i2);
      if i2 < 1 then
      begin
        delete(p, 1, 1);
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
        l.Add(p);
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
          lMatchesANy := startsStr(s, lText);
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

function Split(delimiter: char; const line: string): TArray<string>;
begin
  Split(line, delimiter, Result);
end;

procedure Split(const line: string; delimiter: char; out STRINGS: TArray<string>);
var
  l: TStringList;
begin
  gc(l, TStringList.Create);
  l.StrictDelimiter := True;
  l.delimiter := delimiter;
  l.delimitedText := line;
  STRINGS := l.ToStringArray;
end;

Function splitstring(Const aText: String; aDelim: char; aPerformTrimOnParts: Boolean = False): TArray<String>; 
begin
  Result:= Split(aDelim, aText);
  if aPerformTrimOnParts then
    for var x := 0 to length(Result) -1 do
      Result[x]:= Result[x].Trim;
end;

procedure Split(const line: string; delimiter: char; STRINGS: TStringList);
var
  l: TStringList;
begin
  gc(l, TStringList.Create);
  l.StrictDelimiter := True;
  l.delimiter := delimiter;
  l.delimitedText := line;
  STRINGS.Clear;
  STRINGS.AddStrings(l);
end;

function fStr(const d: double; vs: integer = 2; ns: integer = 2): string;
var
  s: string;
begin
  s := '0.' + PutBefore('0', '0', ns);
  Result := FormatFloat(s, d);
end;

function GuidToHex(const aGuid: TGuid): string;
begin
  SetLength(Result, SizeOf(TGuid) * 2);
  BinToHex(@aGuid, PChar(Result), SizeOf(TGuid));
end;

function Join(const aSeparator: string; aValues: TStringList): string; overload;
begin
  Result:= String.Join(aSeparator, aValues.ToStringArray);
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

function Utf8SequenceLength(const aLead: Byte): Integer; inline;
begin
  if (aLead and $80) = 0 then
    Exit(1);
  if (aLead and $E0) = $C0 then
    Exit(2);
  if (aLead and $F0) = $E0 then
    Exit(3);
  if (aLead and $F8) = $F0 then
    Exit(4);
  Result := 1;
end;

function IsUtf8ContinuationByte(const aByte: Byte): Boolean; inline;
begin
  Result := (aByte and $C0) = $80;
end;

function Utf8TruncateByCodePoint(const AInput: string; aMaxBytesLength: integer): TBytes;
var
  src: TBytes;
  lIndex: Integer;
  lSeqLen: Integer;
  lTotal: Integer;
  lCheck: Integer;
begin
  if aMaxBytesLength <= 0 then
    exit(nil);

  src := TEncoding.UTF8.GetBytes(AInput);
  if length(src) <= aMaxBytesLength then
    exit(src);

  lIndex := 0;
  lTotal := 0;
  while lIndex < length(src) do
  begin
    lSeqLen := Utf8SequenceLength(src[lIndex]);
    if (lIndex + lSeqLen) > length(src) then
      Break;
    if (lTotal + lSeqLen) > aMaxBytesLength then
      Break;

    if lSeqLen > 1 then
    begin
      for lCheck := 1 to lSeqLen - 1 do
      begin
        if not IsUtf8ContinuationByte(src[lIndex + lCheck]) then
        begin
          lSeqLen := 1;
          Break;
        end;
      end;
      if (lTotal + lSeqLen) > aMaxBytesLength then
        Break;
    end;

    Inc(lTotal, lSeqLen);
    Inc(lIndex, lSeqLen);
  end;

  SetLength(Result, lTotal);
  if lTotal > 0 then
    Move(src[0], Result[0], lTotal);
end;

function CharPosEx(
  const aText: string;
  const aChars: TArray<char>;
  aCharsArrayIsSorted: boolean;
  aStartoffset, aEndOffset: integer;
  out aFoundAtOffset, aIndexOfCharFound: integer): boolean;
var
  c: char;
  i: integer;
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
      for var y := Low(aChars) to High(aChars) do
        if c = aChars[y] then
        begin
          aIndexOfCharFound := y;
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
  Result := StrToFloat(s);
end;

function TryStrToFLoatWCC(

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
      delete(s, X, 1);

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
            delete(s, X, 1);
        ',':
          if delComma then
            delete(s, X, 1);
        ' ':
          delete(s, X, 1);
      end;
  end;
end;

function BytesToRawStr(const b: TBytes): rawByteString;
begin
  SetLength(Result, length(b));
  if length(b) > 0 then
    move(b[0], Result[1], length(b));
end;

function RawStrToBytes(const s: rawByteString): TBytes;
begin
  SetLength(Result, length(s));
  if length(s) <> 0 then
    move(s[1], Result[0], length(s));
end;

{ TFastCaseAwareComparer }

const
  CFNVOffsetBasis32 = UInt32($811C9DC5);
  CFNVPrime32 = UInt32(16777619);
  CXXPrime1 = UInt32(2654435761);
  CXXPrime2 = UInt32(2246822519);
  CXXPrime3 = UInt32(3266489917);
  CXXPrime4 = UInt32(668265263);
  CXXPrime5 = UInt32(374761393);

class constructor TFastCaseAwareComparer.Create;
var
  i: Integer;
begin
  for i := 0 to 255 do
    FUpperAscii[i] := Char(i).ToUpper; //PALOFF we only cast 0..255 here
  FOrdinalUtf8Comparer := TFastCaseAwareComparer.Create(True);
  FOrdinalIgnoreCaseUtf8Comparer := TFastCaseAwareComparer.Create(False);
end;

class destructor TFastCaseAwareComparer.Destroy;
begin
  FOrdinalUtf8Comparer := nil;
  FOrdinalIgnoreCaseUtf8Comparer := nil;
  FOrdinalComparer := nil;
  FOrdinalIgnoreCaseComparer := nil;
end;

constructor TFastCaseAwareComparer.Create(aCaseSensitive: Boolean);
begin
  inherited Create;
  FCaseSensitive := aCaseSensitive;
end;

class function TFastCaseAwareComparer.FoldCharValue(aChar: Char): Word;
begin
  if Ord(aChar) <= 255 then
    Exit(Ord(FUpperAscii[Ord(aChar)]));
  Result := Ord(aChar.ToUpper);
end;

class function TFastCaseAwareComparer.FoldPair(const aPair: Cardinal): Cardinal;
var
  lLow, lHigh: Word;
begin
  if (aPair and $FF00FF00) = 0 then
    Exit(
      Ord(FUpperAscii[aPair and $FF]) or
      (Cardinal(FUpperAscii[(aPair shr 16) and $FF]) shl 16)
    );

  lLow := FoldCharValue(Char(aPair and $FFFF));
  lHigh := FoldCharValue(Char(aPair shr 16));
  Result := lLow or (Cardinal(lHigh) shl 16);
end;

class function TFastCaseAwareComparer.Rotl32(aValue: Cardinal; aBits: Integer): Cardinal;
begin
  Result := (aValue shl aBits) or (aValue shr (32 - aBits));
end;

class function TFastCaseAwareComparer.ReadCardinalUnaligned(const aPtr: Pointer): Cardinal;
{$IFDEF CPUARM}
begin
  Move(PByte(aPtr)^, Result, SizeOf(Result));
end;
{$ELSE}
begin
  Result := PCardinal(aPtr)^; //PALOFF we intentionally allow unaligned reads on x86/x64
end;
{$ENDIF}



class function TFastCaseAwareComparer.HashBytes(const aData: PByte; aLength: NativeInt): Cardinal;
var
  lPtr, lEnd: PByte;
  v1, v2, v3, v4: Cardinal;
begin
{$IFOPT Q+}
  {$DEFINE FASTCASE_HASHBYTES_QPLUS}
  {$Q-}
{$ENDIF}
{$IFOPT R+}
  {$DEFINE FASTCASE_HASHBYTES_RPLUS}
  {$R-}
{$ENDIF}
  if (aData = nil) or (aLength = 0) then
    Exit(CXXPrime5);

  lPtr := aData;
  lEnd := aData + aLength;

  if aLength >= 16 then
  begin
    v1 := Cardinal(UInt64(CXXPrime1) + UInt64(CXXPrime2));
    v2 := CXXPrime2;
    v3 := 0;
    v4 := 0;
    Dec(v4, CXXPrime1);

    repeat
      v1 := Rotl32(v1 + ReadCardinalUnaligned(lPtr) * CXXPrime2, 13) * CXXPrime1;
      Inc(lPtr, 4);
      v2 := Rotl32(v2 + ReadCardinalUnaligned(lPtr) * CXXPrime2, 13) * CXXPrime1;
      Inc(lPtr, 4);
      v3 := Rotl32(v3 + ReadCardinalUnaligned(lPtr) * CXXPrime2, 13) * CXXPrime1;
      Inc(lPtr, 4);
      v4 := Rotl32(v4 + ReadCardinalUnaligned(lPtr) * CXXPrime2, 13) * CXXPrime1;
      Inc(lPtr, 4);
    until lPtr > (lEnd - 16);

    Result := Rotl32(v1, 1) + Rotl32(v2, 7) + Rotl32(v3, 12) + Rotl32(v4, 18);
  end
  else
    Result := CXXPrime5;

  Result := Result + Cardinal(aLength);

  while (lEnd - lPtr) >= 4 do
  begin
    Result := Rotl32(Result + ReadCardinalUnaligned(lPtr) * CXXPrime3, 17) * CXXPrime4;
    Inc(lPtr, 4);
  end;

  while lPtr < lEnd do
  begin
    Result := Rotl32(Result + lPtr^ * CXXPrime5, 11) * CXXPrime1;
    Inc(lPtr);
  end;

  Result := Result xor (Result shr 15);
  Result := Result * CXXPrime2;
  Result := Result xor (Result shr 13);
  Result := Result * CXXPrime3;
  Result := Result xor (Result shr 16);
{$IFDEF FASTCASE_HASHBYTES_RPLUS}
  {$UNDEF FASTCASE_HASHBYTES_RPLUS}
  {$R+}
{$ENDIF}
{$IFDEF FASTCASE_HASHBYTES_QPLUS}
  {$UNDEF FASTCASE_HASHBYTES_QPLUS}
  {$Q+}
{$ENDIF}
end;

class function TFastCaseAwareComparer.HashOrdinal(const aValue: string): Integer;
var
  lByteLen: NativeInt;
begin
  lByteLen := Length(aValue) * SizeOf(Char);
  if lByteLen = 0 then
    Exit(Integer(HashBytes(nil, 0)));
{$IFOPT R+}
  {$define FASTCASE_HASHORD_RPLUS}
  {$R-}
{$ENDIF}
  Result := Integer(HashBytes(PByte(PChar(aValue)), lByteLen));
{$IFDEF FASTCASE_HASHORD_RPLUS}
  {$undef FASTCASE_HASHORD_RPLUS}
  {$R+}
{$ENDIF}
end;

class function TFastCaseAwareComparer.HashCaseInsensitive(const aValue: string): Integer;
var
  lLen: Integer;
  lPtr, lEnd, lLimit: PChar;
  v1, v2, v3, v4: Cardinal;
  lHash: Cardinal;
  lFolded: Cardinal;
begin
  // xxHash32-style algorithm with case-folding on the fly
  // See MaxLogic.hash.xxHash.pas for algorithm details
  // Key feature: 4 independent accumulators (v1-v4) allow CPU-level parallelism (3-5× faster than FNV-1a)
  lLen := Length(aValue);
  lPtr := PChar(aValue);
  lEnd := lPtr + lLen;

{$IFOPT Q+}
  {$DEFINE FASTCASE_HASHCI_QPLUS}
  {$Q-}
{$ENDIF}
{$IFOPT R+}
  {$DEFINE FASTCASE_HASHCI_RPLUS}
  {$R-}
{$ENDIF}

  // Step 1: Process 8+ char strings with 4 parallel accumulators (main loop processes 8 chars = 16 bytes per iteration)
  if lLen >= 8 then
  begin
    // Initialize accumulators (same as xxHash32 with seed=0)
    v1 := Cardinal(UInt64(CXXPrime1) + UInt64(CXXPrime2));
    v2 := CXXPrime2;
    v3 := 0;
    v4 := 0;
    Dec(v4, CXXPrime1); // v4 := 0 - CXXPrime1 (avoids compile-time overflow check)

    lLimit := lEnd - 8;
    while lPtr <= lLimit do
    begin
      // Process 4 pairs (8 chars) in parallel -> allows CPU to execute multiple lanes simultaneously
      v1 := Rotl32(v1 + FoldPair(ReadCardinalUnaligned(lPtr)) * CXXPrime2, 13) * CXXPrime1;
      Inc(lPtr, 2);
      v2 := Rotl32(v2 + FoldPair(ReadCardinalUnaligned(lPtr)) * CXXPrime2, 13) * CXXPrime1;
      Inc(lPtr, 2);
      v3 := Rotl32(v3 + FoldPair(ReadCardinalUnaligned(lPtr)) * CXXPrime2, 13) * CXXPrime1;
      Inc(lPtr, 2);
      v4 := Rotl32(v4 + FoldPair(ReadCardinalUnaligned(lPtr)) * CXXPrime2, 13) * CXXPrime1;
      Inc(lPtr, 2);
    end;

    // Step 2: Merge accumulators
    lHash := Rotl32(v1, 1) + Rotl32(v2, 7) + Rotl32(v3, 12) + Rotl32(v4, 18);
  end
  else
    lHash := CXXPrime5; // Short string: start with seed value

  // Step 3: Add total byte length
  lHash := lHash + Cardinal(lLen * SizeOf(Char));

  // Step 4: Process remaining 2-char (4-byte) chunks
  while (lEnd - lPtr) >= 2 do
  begin
    lHash := Rotl32(lHash + FoldPair(ReadCardinalUnaligned(lPtr)) * CXXPrime3, 17) * CXXPrime4;
    Inc(lPtr, 2);
  end;

  // Step 5: Process final odd char (if string length was odd)
  if lPtr < lEnd then
  begin
    lFolded := FoldCharValue(lPtr^);
    // Hash UTF-16 char as 2 bytes (low byte, then high byte)
    lHash := Rotl32(lHash + (lFolded and $FF) * CXXPrime5, 11) * CXXPrime1;
    lHash := Rotl32(lHash + (lFolded shr 8) * CXXPrime5, 11) * CXXPrime1;
  end;

  // Step 6: Final avalanche mixing (ensures all bits influence final hash)
  lHash := lHash xor (lHash shr 15);
  lHash := lHash * CXXPrime2;
  lHash := lHash xor (lHash shr 13);
  lHash := lHash * CXXPrime3;
  lHash := lHash xor (lHash shr 16);

  Result := Integer(lHash);
{$IFDEF FASTCASE_HASHCI_RPLUS}
  {$UNDEF FASTCASE_HASHCI_RPLUS}
  {$R+}
{$ENDIF}
{$IFDEF FASTCASE_HASHCI_QPLUS}
  {$UNDEF FASTCASE_HASHCI_QPLUS}
  {$Q+}
{$ENDIF}
end;


function TFastCaseAwareComparer.Equals(const aLeft, aRight: string): Boolean;
var
  lLen: Integer;
  lLeftPtr, lRightPtr, lEndPtr: PChar;
begin
  if Pointer(aLeft) = Pointer(aRight) then
    Exit(True);

  lLen := Length(aLeft);
  if lLen <> Length(aRight) then
    Exit(False);

  if lLen = 0 then
    Exit(True);

  if FCaseSensitive then
    Exit(CompareMem(@aLeft[1], @aRight[1], lLen * SizeOf(Char)));

  lLeftPtr := PChar(aLeft);
  lRightPtr := PChar(aRight);
  lEndPtr := lLeftPtr + lLen;

{$IFOPT Q+}
  {$DEFINE FASTCASE_EQUALS_QPLUS}
  {$DEFINE FASTCASE_EQUALS_RESTORE}
  {$Q-}
{$ENDIF}
{$IFOPT R+}
  {$DEFINE FASTCASE_EQUALS_RPLUS}
  {$DEFINE FASTCASE_EQUALS_RESTORE}
  {$R-}
{$ENDIF}
{$IFDEF FASTCASE_EQUALS_RESTORE}
  try
{$ENDIF}
    while NativeInt(lEndPtr - lLeftPtr) >= 2 do
    begin
      if FoldPair(ReadCardinalUnaligned(lLeftPtr)) <> FoldPair(ReadCardinalUnaligned(lRightPtr)) then
        Exit(False);
      Inc(lLeftPtr, 2);
      Inc(lRightPtr, 2);
    end;

    if lLeftPtr < lEndPtr then
    begin
      if FoldCharValue(lLeftPtr^) <> FoldCharValue(lRightPtr^) then
        Exit(False);
    end;
{$IFDEF FASTCASE_EQUALS_RESTORE}
  finally
{$IFDEF FASTCASE_EQUALS_RPLUS}
    {$UNDEF FASTCASE_EQUALS_RPLUS}
    {$R+}
{$ENDIF}
{$IFDEF FASTCASE_EQUALS_QPLUS}
    {$UNDEF FASTCASE_EQUALS_QPLUS}
    {$Q+}
{$ENDIF}
  end;
  {$UNDEF FASTCASE_EQUALS_RESTORE}
{$ENDIF}

  Result := True;
end;

function TFastCaseAwareComparer.GetHashCode(const aValue: string): Integer;
begin
  if FCaseSensitive then
    Exit(HashOrdinal(aValue));
  Result := HashCaseInsensitive(aValue);
end;

class function TFastCaseAwareComparer.GetOrdinalComparer: IEqualityComparer<string>;
begin
  if FOrdinalComparer = nil then
    FOrdinalComparer := TFastCaseAwareComparer.Create(True);
  Result := FOrdinalComparer;
end;

class function TFastCaseAwareComparer.GetOrdinalIgnoreCaseComparer: IEqualityComparer<string>;
begin
  if FOrdinalIgnoreCaseComparer = nil then
    FOrdinalIgnoreCaseComparer := TFastCaseAwareComparer.Create(False);
  Result := FOrdinalIgnoreCaseComparer;
end;

class function TFastCaseAwareComparer.Ordinal: IEqualityComparer<string>;
begin
  Result := GetOrdinalComparer;
end;

class function TFastCaseAwareComparer.OrdinalIgnoreCase: IEqualityComparer<string>;
begin
  Result := GetOrdinalIgnoreCaseComparer;
end;

// Invalid sequences consume one byte and map above the Unicode range. This keeps
// malformed byte strings distinct without allocating replacements or throwing in hot paths.
function ReadUtf8Multibyte(const aText: UTF8String; var aIndex: Integer): Cardinal;
var
  b, c, lMinimum: Cardinal;
  i, n: Integer;
begin
  b := Ord(aText[aIndex]);
  Inc(aIndex);
  if b < $80 then
    Exit(b);
  if (b >= $C2) and (b <= $DF) then
  begin
    n := 1;
    Result := b and $1F;
    lMinimum := $80;
  end else if (b >= $E0) and (b <= $EF) then
  begin
    n := 2;
    Result := b and $0F;
    lMinimum := $800;
  end else if (b >= $F0) and (b <= $F4) then
  begin
    n := 3;
    Result := b and $07;
    lMinimum := $10000;
  end else
    Exit($110000 + b);
  if n > Length(aText) - aIndex + 1 then
    Exit($110000 + b);
  for i := 0 to n - 1 do
  begin
    c := Ord(aText[aIndex + i]);
    if (c and $C0) <> $80 then
      Exit($110000 + b);
    Result := (Result shl 6) or (c and $3F);
  end;
  if (Result < lMinimum) or (Result > $10FFFF) or
    ((Result >= $D800) and (Result <= $DFFF)) then
    Exit($110000 + b);
  Inc(aIndex, n);
end;

function ReadUtf8CodePoint(const aText: UTF8String; var aIndex: Integer): Cardinal; inline;
begin
  Result := Ord(aText[aIndex]);
  if Result < $80 then
    Inc(aIndex)
  else
    Result := ReadUtf8Multibyte(aText, aIndex);
end;

function ReadPreviousUtf8CodePoint(const aText: UTF8String; var aIndex: Integer): Cardinal;
var
  i, j: Integer;
begin
  i := aIndex;
  while (i > 1) and (aIndex - i < 3) and ((Ord(aText[i]) and $C0) = $80) do
    Dec(i);
  j := i;
  Result := ReadUtf8CodePoint(aText, j);
  if j = aIndex + 1 then
    aIndex := i - 1
  else begin
    Result := $110000 + Ord(aText[aIndex]);
    Dec(aIndex);
  end;
end;

function FoldUtf8CodePoint(aValue: Cardinal): Cardinal; inline;
begin
  if aValue <= $FFFF then
    Result := TFastCaseAwareComparer.FoldCharValue(Char(aValue)) //PALOFF the condition bounds aValue to the BMP
  else
    Result := aValue;
end;

function EqualUtf8CodePoints(aLeft, aRight: Cardinal; aCaseSensitive: Boolean): Boolean; inline;
begin
  Result := (aLeft = aRight) or (not aCaseSensitive and
    (FoldUtf8CodePoint(aLeft) = FoldUtf8CodePoint(aRight)));
end;

function Utf8MatchAt(const aText, aPattern: UTF8String; aIndex: Integer; aPatternIndex: Integer = 1): Boolean;
var
  i: Integer;
  lLeft, lRight: Cardinal;
begin
  i := aPatternIndex;
  while i <= Length(aPattern) do
  begin
    if aIndex > Length(aText) then
      Exit(False);
    lLeft := ReadUtf8CodePoint(aText, aIndex);
    lRight := ReadUtf8CodePoint(aPattern, i);
    if not EqualUtf8CodePoints(lLeft, lRight, False) then
      Exit(False);
  end;
  Result := True;
end;

function Utf8Contains(const aText, aPattern: UTF8String): Boolean;
var
  i, j: Integer;
  lFirst: Cardinal;
begin
  if aPattern = '' then
    Exit(False);
  j := 1;
  lFirst := FoldUtf8CodePoint(ReadUtf8CodePoint(aPattern, j));
  i := 1;
  while i <= Length(aText) do
    if FoldUtf8CodePoint(ReadUtf8CodePoint(aText, i)) = lFirst then
      if Utf8MatchAt(aText, aPattern, i, j) then
        Exit(True);
  Result := False;
end;

function Utf8EndsWith(const aText, aPattern: UTF8String): Boolean;
var
  i, j: Integer;
  lLeft, lRight: Cardinal;
begin
  i := Length(aText);
  j := Length(aPattern);
  while j > 0 do
  begin
    if i = 0 then
      Exit(False);
    lLeft := ReadPreviousUtf8CodePoint(aText, i);
    lRight := ReadPreviousUtf8CodePoint(aPattern, j);
    if not EqualUtf8CodePoints(lLeft, lRight, False) then
      Exit(False);
  end;
  Result := True;
end;

function Utf8MaskTokenMatches(const aPattern: UTF8String; var aIndex: Integer;
  aValue: Cardinal; aCaseSensitive: Boolean): Boolean;
var
  lFirst, lLast: Cardinal;
  lNegated, lHasItems: Boolean;
begin
  case aPattern[aIndex] of
    '?':
      begin
        Inc(aIndex);
        Exit(True);
      end;
    '[':
      begin
        Inc(aIndex);
        lNegated := (aIndex <= Length(aPattern)) and (aPattern[aIndex] = '!');
        if lNegated then
          Inc(aIndex);
        Result := False;
        lHasItems := False;
        if not aCaseSensitive then
          aValue := FoldUtf8CodePoint(aValue);
        while (aIndex <= Length(aPattern)) and (aPattern[aIndex] <> ']') do
        begin
          lFirst := ReadUtf8CodePoint(aPattern, aIndex);
          lLast := lFirst;
          if (aIndex < Length(aPattern)) and (aPattern[aIndex] = '-') and
            (aPattern[aIndex + 1] <> ']') then
          begin
            Inc(aIndex);
            lLast := ReadUtf8CodePoint(aPattern, aIndex);
          end;
          if not aCaseSensitive then
          begin
            lFirst := FoldUtf8CodePoint(lFirst);
            lLast := FoldUtf8CodePoint(lLast);
          end;
          if lFirst > lLast then
            raise EMaskException.Create('Descending range in UTF-8 mask');
          Result := Result or ((aValue >= lFirst) and (aValue <= lLast));
          lHasItems := True;
        end;
        if not lHasItems or (aIndex > Length(aPattern)) then
          raise EMaskException.Create('Invalid character set in UTF-8 mask');
        Inc(aIndex);
        Result := Result xor lNegated;
      end;
  else
    lFirst := ReadUtf8CodePoint(aPattern, aIndex);
    Result := EqualUtf8CodePoints(aValue, lFirst, aCaseSensitive);
  end;
end;

procedure ValidateUtf8Mask(const aPattern: UTF8String; aCaseSensitive: Boolean);
var
  i: Integer;
begin
  i := 1;
  while i <= Length(aPattern) do
    Utf8MaskTokenMatches(aPattern, i, $FFFFFFFF, aCaseSensitive); //PALOFF validation intentionally discards the match result
end;

function Utf8MatchesMask(const aText, aPattern: UTF8String; aCaseSensitive: Boolean): Boolean;
var
  i, j, lNextText, lNextPattern, lStarText, lStarPattern: Integer;
  cp: Cardinal;
begin
  i := 1;
  j := 1;
  lStarText := 0;
  lStarPattern := 0;
  while i <= Length(aText) do
  begin
    if (j <= Length(aPattern)) and (aPattern[j] = '*') then
    begin
      Inc(j);
      lStarPattern := j;
      lStarText := i;
      Continue;
    end;
    if j <= Length(aPattern) then
    begin
      lNextText := i;
      lNextPattern := j;
      cp := ReadUtf8CodePoint(aText, lNextText);
      if Utf8MaskTokenMatches(aPattern, lNextPattern, cp, aCaseSensitive) then
      begin
        i := lNextText;
        j := lNextPattern;
        Continue;
      end;
    end;
    if lStarPattern = 0 then
      Exit(False);
    ReadUtf8CodePoint(aText, lStarText); //PALOFF advance the retry position by one scalar
    i := lStarText;
    j := lStarPattern;
  end;
  while (j <= Length(aPattern)) and (aPattern[j] = '*') do
    Inc(j);
  Result := j > Length(aPattern);
end;

function StringMatches(const aValue, aPattern: UTF8String; aCaseSensitive: Boolean): Boolean;
begin
  ValidateUtf8Mask(aPattern, aCaseSensitive);
  Result := Utf8MatchesMask(aValue, aPattern, aCaseSensitive);
end;

function MatchesFilter(const aText: UTF8String; const aFilter: TArray<UTF8String>): Boolean;
var
  lFilter: UTF8String;
begin
  if Length(aFilter) = 0 then
    Exit(True);
  for lFilter in aFilter do
    if StringMatches(aText, lFilter, False) then
      Exit(True);
  Result := False;
end;

{ TFilterExUtf8 }

class function TFilterExUtf8.ParseTerm(const aText: UTF8String): TTerm;
var
  i, n: Integer;
begin
  Result.fText := aText;
  Result.fKind := TKind.ukContains;
  if (Pos(AnsiChar('?'), aText) > 0) or (Pos(AnsiChar('['), aText) > 0) then
    Result.fKind := TKind.ukMask
  else begin
    i := Pos(AnsiChar('*'), aText);
    n := Length(aText);
    if i > 0 then
    begin
      if i = n then
      begin
        Result.fKind := TKind.ukStarts;
        SetLength(Result.fText, n - 1);
      end else if i = 1 then
      begin
        i := Pos(AnsiChar('*'), aText, 2);
        if i = 0 then
        begin
          Result.fKind := TKind.ukEnds;
          Result.fText := Copy(aText, 2, n - 1);
        end else if i = n then
          Result.fText := Copy(aText, 2, n - 2)
        else
          Result.fKind := TKind.ukMask;
      end else
        Result.fKind := TKind.ukMask;
    end;
  end;
  if Result.fKind = TKind.ukMask then
    ValidateUtf8Mask(Result.fText, False);
end;

class function TFilterExUtf8.Create(const aFilterText: UTF8String): TFilterExUtf8;
var
  lGroups: TList<TGroup>;
  lTerms: TList<TTerm>;
  lGroup: TGroup;
  lToken: UTF8String;
  g: TGarbos;
  i, n, lTokenLength: Integer;
  lInQuote: Boolean;
  c: AnsiChar;
begin
  Result := Default(TFilterExUtf8);
  GC(lGroups, TList<TGroup>.Create, g);
  GC(lTerms, TList<TTerm>.Create, g);
  n := Length(aFilterText);
  SetLength(lToken, n);
  i := 1;
  while i <= n do
  begin
    while (i <= n) and (aFilterText[i] in [#1..#32]) do
      Inc(i);
    if i > n then
      Break;
    lGroup := Default(TGroup);
    lGroup.fIsNegated := aFilterText[i] = '!';
    if lGroup.fIsNegated then
      Inc(i);
    lTerms.Clear;
    lInQuote := False;
    lTokenLength := 0;
    while i <= n do
    begin
      c := aFilterText[i];
      if c = '"' then
      begin
        if lInQuote and (i < n) and (aFilterText[i + 1] = '"') then
        begin
          Inc(lTokenLength);
          lToken[lTokenLength] := '"';
          Inc(i);
        end else
          lInQuote := not lInQuote;
      end else if not lInQuote and ((c = '|') or (c in [#1..#32])) then
      begin
        lTerms.Add(ParseTerm(Copy(lToken, 1, lTokenLength)));
        lTokenLength := 0;
        if c <> '|' then
          Break;
      end else begin
        Inc(lTokenLength);
        lToken[lTokenLength] := c;
      end;
      Inc(i);
    end;
    if (i > n) and ((lTokenLength > 0) or (lTerms.Count > 0)) then
      lTerms.Add(ParseTerm(Copy(lToken, 1, lTokenLength)));
    lGroup.fTerms := lTerms.ToArray;
    lGroups.Add(lGroup);
  end;
  Result.fGroups := lGroups.ToArray;
end;

function TFilterExUtf8.Matches(const aText: UTF8String): Boolean;
var
  i, j: Integer;
  lMatches: Boolean;
begin
  for i := 0 to High(fGroups) do
  begin
    lMatches := False;
    for j := 0 to High(fGroups[i].fTerms) do
    begin
      case fGroups[i].fTerms[j].fKind of
        TKind.ukContains: lMatches := Utf8Contains(aText, fGroups[i].fTerms[j].fText);
        TKind.ukStarts: lMatches := Utf8MatchAt(aText, fGroups[i].fTerms[j].fText, 1);
        TKind.ukEnds: lMatches := Utf8EndsWith(aText, fGroups[i].fTerms[j].fText);
        TKind.ukMask: lMatches := Utf8MatchesMask(aText, fGroups[i].fTerms[j].fText, False);
      end;
      if lMatches then
        Break;
    end;
    if lMatches = fGroups[i].fIsNegated then
      Exit(False);
  end;
  Result := True;
end;

class function TFastCaseAwareComparer.OrdinalUtf8: IEqualityComparer<UTF8String>;
begin
  Result := FOrdinalUtf8Comparer;
end;

class function TFastCaseAwareComparer.OrdinalIgnoreCaseUtf8: IEqualityComparer<UTF8String>;
begin
  Result := FOrdinalIgnoreCaseUtf8Comparer;
end;

function TFastCaseAwareComparer.Equals(const aLeft, aRight: UTF8String): Boolean;
var
  i, j: Integer;
  lLeft, lRight: Cardinal;
begin
  if Pointer(aLeft) = Pointer(aRight) then
    Exit(True);
  if FCaseSensitive then
    Exit((Length(aLeft) = Length(aRight)) and
      ((aLeft = '') or CompareMem(Pointer(aLeft), Pointer(aRight), NativeInt(Length(aLeft)))));
  i := 1;
  j := 1;
  while (i <= Length(aLeft)) and (j <= Length(aRight)) do
  begin
    lLeft := ReadUtf8CodePoint(aLeft, i);
    lRight := ReadUtf8CodePoint(aRight, j);
    if not EqualUtf8CodePoints(lLeft, lRight, False) then
      Exit(False);
  end;
  Result := (i > Length(aLeft)) and (j > Length(aRight));
end;

function TFastCaseAwareComparer.GetHashCode(const aValue: UTF8String): Integer;
var
  lHash, cp: Cardinal;
  i: Integer;
begin
  if FCaseSensitive then
    Exit(Integer(HashBytes(PByte(PAnsiChar(aValue)), Length(aValue))));
  lHash := CFNVOffsetBasis32;
  i := 1;
{$IFOPT Q+}
  {$DEFINE FASTCASE_UTF8_HASH_QPLUS}
  {$Q-}
{$ENDIF}
  while i <= Length(aValue) do
  begin
    cp := FoldUtf8CodePoint(ReadUtf8CodePoint(aValue, i));
    if (cp > $FFFF) and (cp <= $10FFFF) then
    begin
      // Match our existing UTF-16 comparer: supplementary pairs are not case-folded.
      Dec(cp, $10000);
      lHash := (lHash xor ($D800 + (cp shr 10))) * CFNVPrime32;
      cp := $DC00 + (cp and $3FF);
    end;
    lHash := (lHash xor cp) * CFNVPrime32;
  end;
{$IFDEF FASTCASE_UTF8_HASH_QPLUS}
  {$UNDEF FASTCASE_UTF8_HASH_QPLUS}
  {$Q+}
{$ENDIF}
  Result := Integer(lHash);
end;

{ TStringComparerHelper }
class function TStringComparerHelper.OrdinalIgnoreCase: TCustomComparer<string>;
begin
  Result := TIStringComparer.Ordinal;
end;

{ TRegExHelper.TProcAdapter. }

constructor TRegExHelper.TProcAdapter.Create(const aProc: TMatchEvaluatorProc);
begin
  inherited Create;
  fProc := aProc;
end;

function TRegExHelper.TProcAdapter.Invoke(const match: TMatch): string;
begin
  Result := fProc(match);
end;

{ TRegExHelper }

function TRegExHelper.Replace(const input: string; Evaluator: TMatchEvaluatorProc): string;
var
  lAdapter: TProcAdapter;
begin
  gc(lAdapter, TProcAdapter.Create(Evaluator));
  // Call the built-in instance overload that expects 'of object'
  Result := self.Replace(input, lAdapter.Invoke);
end;

function TRegExHelper.Replace(const input: string; Evaluator: TMatchEvaluatorProc; Count: integer): string;
var
  lAdapter: TProcAdapter;
begin
  gc(lAdapter, TProcAdapter.Create(Evaluator));
  Result := self.Replace(input, lAdapter.Invoke, Count);
end;

class function TRegExHelper.Replace(const input, pattern: string; Evaluator: TMatchEvaluatorProc): string;
var
  lAdapter: TProcAdapter;
  lRe: TRegEx;
begin
  gc(lAdapter, TProcAdapter.Create(Evaluator));
  lRe := TRegEx.Create(pattern);
  Result := lRe.Replace(input, lAdapter.Invoke);
end;

class function TRegExHelper.Replace(const input, pattern: string; Evaluator: TMatchEvaluatorProc; Options: TRegExOptions): string;
var
  lAdapter: TProcAdapter;
  lRe: TRegEx;
begin
  gc(lAdapter, TProcAdapter.Create(Evaluator));
  lRe := TRegEx.Create(pattern, Options);
  Result := lRe.Replace(input, lAdapter.Invoke);
end;

function PrettyElapsed(const aMs: Int64): string;
var
  ms, s, m, h: Int64;
begin
  ms := aMs;
  s := ms div 1000;
  ms := ms mod 1000;
  m := s div 60;
  s := s mod 60;
  h := m div 60;
  m := m mod 60;
  if h > 0 then exit(Format('%dh %dm %ds %dms', [h, m, s, ms]));
  if m > 0 then exit(Format('%dm %ds %dms', [m, s, ms]));
  if s > 0 then exit(Format('%ds %dms', [s, ms]));
  Result := Format('%dms', [ms]);
end;

function i2s(const i: integer): string;
begin
  Result := i.ToString;
end;

function floatToHex(const f: double; aMinLength: integer = SizeOf(double) * 2): string;
var
  s: string;
  lToDel, lDiff: integer;
begin
  SetLength(s, SizeOf(f) * 2);
  BinToHex(@f, PChar(s), SizeOf(f));

  lDiff := aMinLength - length(s);
  if lDiff > 0 then
    s := stringOfChar('0', lDiff) + s
  else if lDiff < 0 then
  begin
    lToDel := 0;
    for var X := 1 to length(s) do
    begin
      if (s[X] = '0') then
        Inc(lToDel)
      else
        break;
      Inc(lDiff);
      if lDiff = 0 then
        break;
    end;
    if lToDel <> 0 then
      delete(s, 1, lToDel);
    if s = '' then
      s := '0';
  end;
  Result := s;
end;

Function PrettyPrintSize(Const ByteCount: int64): String;
Const
  n = 1 / (1024 * 1024);
Begin
  Result := MegaBytesToStr(ByteCount * n);
End;

Function MegaBytesToStr(Const MegaByteCount: extended): String;
Const
  nKb = 1024;
  nbytes = 1024 * 1024;
  nGb = 1 / 1024;
  ntb = 1 / (1024 * 1024);
  nPB = 1 / (1024 * 1024 * 1024);
Var
  f: extended;
Begin

  f := MegaByteCount * nPB;
  If f >= 1 Then
    Exit(fstr(f, 2, 2) + ' Petabyte');

  f := MegaByteCount * ntb;
  If f >= 1 Then
    Exit(fstr(f, 2, 2) + ' TB');

  f := MegaByteCount * nGb;
  If f >= 1 Then
    Exit(fstr(f, 2, 2) + ' GB');

  If MegaByteCount >= 1 Then
    Exit(fstr(MegaByteCount, 2, 2) + ' MB');

  f := MegaByteCount * nKb;
  If f >= 1 Then
    Exit(fstr(f, 2, 2) + ' KB');

  Result := IntToStr(round(MegaByteCount * nbytes)) + ' B';
End;

Function ParseStatement(Const Statement: String; Const params: TArray<String>; Const Token: char = ':'): String;
Var
  ParamCount: Integer;
  x: Integer;
  s, n, v: String;
Begin
  s := Statement;
  ParamCount := Length(params) Div 2;
  For x := 0 To ParamCount - 1 Do
  Begin
    n := params[x * 2];
    v := params[x * 2 + 1];

    s := StringReplace(s, Token + n + Token, v, [rfReplaceAll, rfIgnoreCase]);
  End;

  Result := s;
End;

end.
