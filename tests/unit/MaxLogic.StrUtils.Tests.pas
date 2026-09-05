unit MaxLogic.StrUtils.Tests;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMaxLogicStrUtilsTests = class
  public
    [Test] procedure ReplacePlaceholder_BasicReplace;
    [Test] procedure ReplacePlaceholder_SkipMissing;
    [Test] procedure ReplacePlaceholder_CaseInsensitiveMarkers;
    [Test] procedure ReplacePlaceholder_StartOffset;
    [Test] procedure ReplacePlaceholder_InvalidChars_NoCrossLine;
    [Test] procedure ReplacePlaceholder_NoRecursion_WhenReplacementContainsMarkers;
    [Test] procedure ReplacePlaceholder_ReplaceAndResumeAtSamePosition_Nested;
    [Test] procedure ReplacePlaceholder_ReplaceAndStop_AppendsTail;
    [Test] procedure ReplacePlaceholder_Stop_AppendsTailFromOffset;
    [Test] procedure ReplacePlaceholder_TailAppendBoundary;
    [Test] procedure ReplacePlaceholder_CaseSensitiveMarkersMismatch_Unchanged;
    [Test] procedure ReplacePlaceholder_IncompletePlaceholder_Unchanged;

    [Test] procedure ExtractString_InvalidCharsWindow_ExcludesEndMarkerChar;
    [Test] procedure ExtractString_CaseSensitivityAndOffset;
    [Test] procedure ExtractString_NoStartMarker_ReturnsFalse;
    [Test] procedure ExtractString_StartNoEnd_ReturnsFalse;
    [Test] procedure ExtractString_InvalidChars_SortedSet_Rejects;

    [Test] procedure StringMatches_CaseInsensitive_AllPlatforms;
    [Test] procedure StringMatches_CaseSensitive_MismatchCasesReturnFalse;
    [Test] procedure StringMatches_Wildcards_And_Anchoring;
    [Test] procedure StringMatches_RegexEscape_Literal;

    [Test] procedure Utf8Truncate_BasicBoundaries;
    [Test] procedure Utf8Truncate_NoTruncation_ReturnsFull;
    [Test] procedure Utf8Truncate_ExactByteBudget_UsesFullCodePoint;

    [Test] procedure StrToFloatWCC_Formats;
    [Test] procedure StrToFloatWCC_LettersRaise_And_TryFalse_And_Default;
    [Test] procedure StrToFloatWCC_Negative_Numbers_And_SingleSeparatorDecimals;

    [Test] procedure CharPosEx_Unsorted_And_Sorted_And_Bounds;
    [Test] procedure CharPosEx_EndOffsetClamping;

    [Test] procedure ExpandEnvVars_AbsentVar_Stays;
    [Test] procedure ExpandEnvVars_PresentVar_Windows;

    [Test] procedure StrCmpLogical_Windows;

    [Test] procedure FastComparer_CaseInsensitiveEquality;
    [Test] procedure FastComparer_CaseSensitiveDistinguishes;
    [Test] procedure FastComparer_PerformanceVsTIStringComparer;
    [Test] procedure FastComparer_PerformanceVsOrdinalComparer;
    [Test] procedure FastComparer_EmptyStrings;
    [Test] procedure FastComparer_SingletonStrings;
    [Test] procedure FastComparer_UnicodeEdgeCases;
    [Test] procedure FastComparer_LongStrings;
    [Test] procedure FastComparer_HashCollisionResistance;
    [Test] procedure FastComparer_MixedAsciiUnicode;
    [Test] procedure FastComparer_CaseFoldingEdgeCases;
    [Test] procedure FastComparer_SurrogatePairs;
    [Test] procedure FastComparer_NullCharHandling;
    [Test] procedure FastComparer_DictionaryIntegration;
    [Test] procedure FastComparer_HashBoundaryCoverage;
    [Test] procedure FastComparer_VeryLargeInput;

    [Test] procedure Utf8Comparer_OrdinalBytesAndDictionary;
    [Test] procedure Utf8Comparer_IgnoreCaseUnicodeAndDictionary;
    [Test] procedure Utf8Comparer_HashBoundaries;
    [Test] procedure Utf8FilterEx_NativeSyntax;
    [Test] procedure Utf8Masks_UnicodeAndEmptyFilters;
    [Test] procedure Utf8_NativePathsDoNotAllocate;
    [Test] procedure Utf8_Performance;
    [Test] procedure Utf8_MaskScalarsSetsAndBacktracking;
    [Test] procedure Utf8_MalformedBytesStayDistinct;
    [Test] procedure Utf8_FilterParsingAndRecordCopies;
    [Test] procedure Utf8_ComparerAllBmpCaseMappings;


    // Additional tests
    [Test] procedure Utf8Truncate_ZeroAndNegative_ReturnsEmpty;
    [Test] procedure ExpandEnvVars_CustomTokens_Windows;

    // TFilterEx tests
    [Test] procedure FilterEx_AND_Semantics;
    [Test] procedure FilterEx_OR_Semantics;
    [Test] procedure FilterEx_Wildcards_Mask;
    [Test] procedure FilterEx_Starts_Ends;
    [Test] procedure FilterEx_DoubleStar_Contains;
    [Test] procedure FilterEx_Negation;
    [Test] procedure FilterEx_QuotedToken_WithSpaces;
    [Test] procedure FilterEx_OR_WithQuotedTokens;
    [Test] procedure FilterEx_CaseInsensitive;
    [Test] procedure FilterEx_MaskVsContains_Precedence;
    [Test] procedure FilterEx_EmptyFilter_ReturnsTrue;
    [Test] procedure FilterEx_Whitespace_And_QuotesHandling;
    // Remaining API + edge cases
    [Test] procedure PutBefore_StringAndNumber;
    [Test] procedure CombineUrl_SlashesAndParts;
    [Test] procedure MatchesFilter_Basics;
    [Test] procedure Split_And_SplitInHalfBy_And_Join;
    [Test] procedure fStr_Formats_Rounding;
    [Test] procedure GuidToHex_DefaultGuid;
    [Test] procedure RawBytes_RoundTrip;
    [Test] procedure ExtractString_AdjacentMarkers_EmptyValue;
    [Test] procedure ReplacePlaceholder_StartOffsetBeyondEnd_NoChange;
    [Test] procedure CharPosEx_SinglePositionRange;
    [Test] procedure StrToFloatWCC_PlusAndSpaces;
    [Test] procedure ExpandEnvVars_NestedValue_NoRecursion_Windows;
    [Test] procedure ParseStatement_BasicReplacement;
    [Test] procedure ParseStatement_MissingTokensRemainUnchanged;
    [Test] procedure ParseStatement_CustomTokenCharacter;
    [Test] procedure PrettyElapsed_Boundaries;
    [Test] procedure FloatToHex_PaddingAndTrimming;
    [Test] procedure TRegExHelper_AnonymousReplace;
  end;

implementation

uses
  System.Character, System.Classes, System.Diagnostics, System.Generics.Collections, System.Generics.Defaults,
  System.Masks, System.RegularExpressions, System.SysUtils, System.Types,
  {$IFDEF MSWINDOWS}Winapi.Windows,{$ENDIF}
  AutoFree, maxLogic.StrUtils;

var
  fUtf8MemoryManager: TMemoryManagerEx;
threadvar
  fCountUtf8Allocations: Boolean;
  fUtf8AllocationCount: NativeInt;

function CountedUtf8GetMem(aSize: NativeInt): Pointer;
begin
  if fCountUtf8Allocations then
    Inc(fUtf8AllocationCount);
  Result := fUtf8MemoryManager.GetMem(aSize);
end;

function CountedUtf8AllocMem(aSize: NativeInt): Pointer;
begin
  if fCountUtf8Allocations then
    Inc(fUtf8AllocationCount);
  Result := fUtf8MemoryManager.AllocMem(aSize);
end;

function CountedUtf8ReallocMem(p: Pointer; aSize: NativeInt): Pointer;
begin
  if fCountUtf8Allocations then
    Inc(fUtf8AllocationCount);
  Result := fUtf8MemoryManager.ReallocMem(p, aSize);
end;

function CountUtf8Allocations(const aProc: TProc): NativeInt;
var
  lCountingManager: TMemoryManagerEx;
begin
  // Forward to the real allocator and count this test thread only; restore even on failure.
  GetMemoryManager(fUtf8MemoryManager);
  lCountingManager := fUtf8MemoryManager;
  lCountingManager.GetMem := CountedUtf8GetMem;
  lCountingManager.AllocMem := CountedUtf8AllocMem;
  lCountingManager.ReallocMem := CountedUtf8ReallocMem;
  fUtf8AllocationCount := 0;
  SetMemoryManager(lCountingManager);
  try
    fCountUtf8Allocations := True;
    aProc();
    Result := fUtf8AllocationCount;
  finally
    fCountUtf8Allocations := False;
    SetMemoryManager(fUtf8MemoryManager);
  end;
end;

function IsDUnitXSilent: Boolean; forward;
procedure LogPerfMessage(const aMessage: string); forward;
function TicksToMicroseconds(const aTicks: Int64): Double; forward;

function CodePointsOf(const s: string): string;
var
  i: Integer;
  c1: Char;
  cp: Integer;
begin
  Result := '';
  i := 1;
  while i <= Length(s) do
  begin
    c1 := s[i];
    if (i < Length(s)) and c1.IsHighSurrogate and s[i+1].IsLowSurrogate then
    begin
      cp := ((Ord(c1) - $D800) shl 10) + (Ord(s[i+1]) - $DC00) + $10000;
      Inc(i, 2);
    end else begin
      cp := Ord(c1);
      Inc(i);
    end;
    if Result <> '' then
      Result := Result + ' ';
    if cp <= $FFFF then
      Result := Result + 'U+' + IntToHex(cp, 4)
    else
      Result := Result + 'U+' + IntToHex(cp, 6);
  end;
end;

procedure TMaxLogicStrUtilsTests.FastComparer_PerformanceVsOrdinalComparer;
const
  cIterations = 200000;
var
  lFastComparer: IEqualityComparer<string>;
  lRtlComparer: IEqualityComparer<string>;
  lKeys: TArray<string>;
  lIndex: Integer;
  lStopwatch: TStopwatch;
  lFastTicks, lRtlTicks: Int64;
  lAccumulator: Integer;
  lRatio: Double;
  lMessage: string;
begin
  lFastComparer := TFastCaseAwareComparer.Ordinal;
  lRtlComparer := TStringComparer.Ordinal;

  SetLength(lKeys, cIterations);
  for lIndex := 0 to High(lKeys) do
    lKeys[lIndex] := Format('Key%.8x_Value%.8x', [lIndex, (lIndex shl 1) xor lIndex]);

  lAccumulator := 0;
  lStopwatch := TStopwatch.StartNew;
  for lIndex := 0 to High(lKeys) do
    lAccumulator := lAccumulator xor lFastComparer.GetHashCode(lKeys[lIndex]);
  lFastTicks := lStopwatch.ElapsedTicks;
  if lFastTicks = 0 then
    lFastTicks := 1;

  lStopwatch := TStopwatch.StartNew;
  for lIndex := 0 to High(lKeys) do
    lAccumulator := lAccumulator xor lRtlComparer.GetHashCode(lKeys[lIndex]);
  lRtlTicks := lStopwatch.ElapsedTicks;
  if lRtlTicks = 0 then
    lRtlTicks := 1;

  lRatio := lRtlTicks / lFastTicks;
  lMessage := Format('TFastCaseAwareComparer (ordinal) vs TStringComparer.Ordinal: ratio=%.2fx (fast=%.0f μs, rtl=%.0f μs)',
    [lRatio, TicksToMicroseconds(lFastTicks), TicksToMicroseconds(lRtlTicks)]);

  if lRatio > 1 then
    LogPerfMessage(lMessage);

  Assert.IsTrue(lRatio > 1,
    Format('TFastCaseAwareComparer ordinal slower or equal (ratio=%.2fx, fast=%.0f μs, rtl=%.0f μs)',
      [lRatio, TicksToMicroseconds(lFastTicks), TicksToMicroseconds(lRtlTicks)]));

  Assert.AreNotEqual(-1, lAccumulator);
end;

{ Fast comparer tests }

procedure TMaxLogicStrUtilsTests.FastComparer_CaseInsensitiveEquality;
var
  lComparer: IEqualityComparer<string>;
begin
  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCase;
  Assert.IsTrue(lComparer.Equals('Key42', 'kEy42'));
  Assert.AreEqual(lComparer.GetHashCode('Key42'), lComparer.GetHashCode('kEy42'));
end;

procedure TMaxLogicStrUtilsTests.FastComparer_CaseSensitiveDistinguishes;
var
  lComparer: IEqualityComparer<string>;
begin
  lComparer := TFastCaseAwareComparer.Ordinal;
  Assert.IsFalse(lComparer.Equals('Env', 'env'));
  Assert.AreNotEqual(lComparer.GetHashCode('Env'), lComparer.GetHashCode('env'));
end;

procedure TMaxLogicStrUtilsTests.FastComparer_PerformanceVsTIStringComparer;
const
  cIterations = 200000;
var
  lFastComparer: IEqualityComparer<string>;
  lRtlComparer: IEqualityComparer<string>;
  lKeys: TArray<string>;
  lIndex: Integer;
  lStopwatch: TStopwatch;
  lFastTicks, lRtlTicks: Int64;
  lAccumulator: Integer;
  lRatio: Double;
  lMessage: string;
begin
  lFastComparer := TFastCaseAwareComparer.OrdinalIgnoreCase;
  lRtlComparer := TIStringComparer.Ordinal;

  SetLength(lKeys, cIterations);
  for lIndex := 0 to High(lKeys) do
    lKeys[lIndex] := Format('Key%.8x_Value%.8x', [lIndex, (lIndex shl 5) xor lIndex]);

  lAccumulator := 0;
  lStopwatch := TStopwatch.StartNew;
  for lIndex := 0 to High(lKeys) do
    lAccumulator := lAccumulator xor lFastComparer.GetHashCode(lKeys[lIndex]);
  lFastTicks := lStopwatch.ElapsedTicks;
  if lFastTicks = 0 then
    lFastTicks := 1;

  lStopwatch := TStopwatch.StartNew;
  for lIndex := 0 to High(lKeys) do
    lAccumulator := lAccumulator xor lRtlComparer.GetHashCode(lKeys[lIndex]);
  lRtlTicks := lStopwatch.ElapsedTicks;
  if lRtlTicks = 0 then
    lRtlTicks := 1;

  lRatio := lRtlTicks / lFastTicks;
  lMessage := Format('TFastCaseAwareComparer vs TIStringComparer.Ordinal: ratio=%.2fx (fast=%.0f μs, rtl=%.0f μs)',
    [lRatio, TicksToMicroseconds(lFastTicks), TicksToMicroseconds(lRtlTicks)]);

  if lRatio > 1 then
    LogPerfMessage(lMessage);

  Assert.IsTrue(lRatio > 1,
    Format('TFastCaseAwareComparer slower or equal (ratio=%.2fx, fast=%.0f μs, rtl=%.0f μs)',
      [lRatio, TicksToMicroseconds(lFastTicks), TicksToMicroseconds(lRtlTicks)]));

  Assert.AreNotEqual(-1, lAccumulator);
end;

function Utf8HexOf(const s: string): string;
var
  bytes: TBytes;
  i: Integer;
begin
  bytes := TEncoding.UTF8.GetBytes(s);
  Result := '';
  for i := 0 to High(bytes) do
  begin
    if i > 0 then
      Result := Result + ' ';
    Result := Result + IntToHex(bytes[i], 2);
  end;
end;

function ExpectGotMsg(const Expected, Actual: string): string;
begin
  Result := Format('Expected "%s" [CP %s | UTF8 %s] but got "%s" [CP %s | UTF8 %s]',
    [Expected, CodePointsOf(Expected), Utf8HexOf(Expected), Actual, CodePointsOf(Actual), Utf8HexOf(Actual)]);
end;

function IsDUnitXSilent: Boolean;
var
  lValue: string;
begin
  lValue := GetEnvironmentVariable('DUNITX_SILENT');
  Result := SameText(lValue, '1') or SameText(lValue, 'true') or SameText(lValue, 'yes');
end;

procedure LogPerfMessage(const aMessage: string);
begin
  if IsDUnitXSilent then
    Exit;
  System.Writeln;
  System.Writeln(aMessage);
  System.Writeln;
end;

function TicksToMicroseconds(const aTicks: Int64): Double;
begin
  if aTicks <= 0 then
    Exit(0);
  Result := (aTicks / TStopwatch.Frequency) * 1E6;
end;

{ ReplacePlaceholder tests }

procedure TMaxLogicStrUtilsTests.ReplacePlaceholder_BasicReplace;
var
  lRes: string;
begin
  lRes := ReplacePlaceholder(
    'Hello %name%!', '%', '%',
    procedure(const aValue: string; aStartMarkerFoundAtIndex: integer; var aReplaceValue: string; var aAction: TReplacePlaceholderAction)
    begin
      if SameText(aValue, 'name') then
        aReplaceValue := 'Alice'
      else
        aAction := raSkip;
    end
  );
  Assert.AreEqual('Hello Alice!', lRes);
end;

procedure TMaxLogicStrUtilsTests.ReplacePlaceholder_SkipMissing;
var
  lRes: string;
begin
  lRes := ReplacePlaceholder(
    'Hello %MISSING%', '%', '%',
    procedure(const aValue: string; aStartMarkerFoundAtIndex: integer; var aReplaceValue: string; var aAction: TReplacePlaceholderAction)
    begin
      aAction := raSkip;
    end
  );
  Assert.AreEqual('Hello %MISSING%', lRes);
end;

procedure TMaxLogicStrUtilsTests.ReplacePlaceholder_CaseInsensitiveMarkers;
var
  lRes: string;
begin
  lRes := ReplacePlaceholder(
    'Hello %NaMe%', '%', '%',
    procedure(const aValue: string; aStartMarkerFoundAtIndex: integer; var aReplaceValue: string; var aAction: TReplacePlaceholderAction)
    begin
      if SameText(aValue, 'name') then
        aReplaceValue := 'Alice'
      else
        aAction := raSkip;
    end,
    1, False // case-insensitive
  );
  Assert.AreEqual('Hello Alice', lRes);
end;

procedure TMaxLogicStrUtilsTests.ReplacePlaceholder_StartOffset;
var
  lRes: string;
begin
  // Start after 'X ' (index 3)
  lRes := ReplacePlaceholder(
    'X %A% Y %B%', '%', '%',
    procedure(const aValue: string; aStartMarkerFoundAtIndex: integer; var aReplaceValue: string; var aAction: TReplacePlaceholderAction)
    begin
      if aValue = 'A' then aReplaceValue := 'aa'
      else if aValue = 'B' then aReplaceValue := 'bb'
      else aAction := raSkip;
    end,
    3, True
  );
  Assert.AreEqual('X aa Y bb', lRes);
end;

procedure TMaxLogicStrUtilsTests.ReplacePlaceholder_InvalidChars_NoCrossLine;
var
  lRes, lTxt: string;
begin
  lTxt := '%A line' + sLineBreak + 'continues%';
  lRes := ReplacePlaceholder(
    lTxt, '%', '%',
    procedure(const aValue: string; aStartMarkerFoundAtIndex: integer; var aReplaceValue: string; var aAction: TReplacePlaceholderAction)
    begin
      // would replace to 'X' if allowed, but newline invalidates the match
      aReplaceValue := 'X';
    end,
    1, True, TArray<char>.Create(#10), False
  );
  Assert.AreEqual(lTxt, lRes); // unchanged due to invalid char between markers
end;

procedure TMaxLogicStrUtilsTests.ReplacePlaceholder_NoRecursion_WhenReplacementContainsMarkers;
var
  lRes: string;
begin
  lRes := ReplacePlaceholder(
    'x %A%', '%', '%',
    procedure(const aValue: string; aStartMarkerFoundAtIndex: integer; var aReplaceValue: string; var aAction: TReplacePlaceholderAction)
    begin
      if aValue = 'A' then
        aReplaceValue := '%B%' // should not trigger another pass
      else
        aAction := raSkip;
    end
  );
  Assert.AreEqual('x %B%', lRes);
end;

procedure TMaxLogicStrUtilsTests.ReplacePlaceholder_ReplaceAndResumeAtSamePosition_Nested;
var
  lRes: string;
begin
  lRes := ReplacePlaceholder(
    'Start {{outer}}', '{{', '}}',
    procedure(const aValue: string; aStartMarkerFoundAtIndex: integer; var aReplaceValue: string; var aAction: TReplacePlaceholderAction)
    begin
      if aValue = 'outer' then
      begin
        aReplaceValue := '{{inner}}';
        aAction := raReplaceAndResumeAtSamePosition;
      end
      else if aValue = 'inner' then
      begin
        aReplaceValue := 'done';
        aAction := raReplace;
      end
      else
        aAction := raSkip;
    end
  );
  Assert.AreEqual('Start done', lRes);
end;

procedure TMaxLogicStrUtilsTests.ReplacePlaceholder_ReplaceAndStop_AppendsTail;
var
  lRes: string;
begin
  lRes := ReplacePlaceholder(
    'A %X% B %Y%', '%', '%',
    procedure(const aValue: string; aStartMarkerFoundAtIndex: integer; var aReplaceValue: string; var aAction: TReplacePlaceholderAction)
    begin
      if aValue = 'X' then
      begin
        aReplaceValue := 'R';
        aAction := raReplaceAndStop;
      end;
    end
  );
  Assert.AreEqual('A R B %Y%', lRes);
end;

procedure TMaxLogicStrUtilsTests.ReplacePlaceholder_Stop_AppendsTailFromOffset;
var
  lRes, lTxt: string;
begin
  lTxt := 'A %X% B';
  lRes := ReplacePlaceholder(
    lTxt, '%', '%',
    procedure(const aValue: string; aStartMarkerFoundAtIndex: integer; var aReplaceValue: string; var aAction: TReplacePlaceholderAction)
    begin
      if aValue = 'X' then
        aAction := raStop;
    end
  );
  // Implementation appends from current offset on stop -> original text preserved
  Assert.AreEqual(lTxt, lRes);
end;

procedure TMaxLogicStrUtilsTests.ReplacePlaceholder_TailAppendBoundary;
var
  lRes: string;
begin
  lRes := ReplacePlaceholder(
    'X %A% Z', '%', '%',
    procedure(const aValue: string; aStartMarkerFoundAtIndex: integer; var aReplaceValue: string; var aAction: TReplacePlaceholderAction)
    begin
      if aValue = 'A' then aReplaceValue := 'R';
    end
  );
  // Ensures last single trailing char is not dropped
  Assert.AreEqual('X R Z', lRes);
end;

procedure TMaxLogicStrUtilsTests.ReplacePlaceholder_CaseSensitiveMarkersMismatch_Unchanged;
var
  lTxt, lRes: string;
begin
  // text has lowercase markers; search uses uppercase with aCasesensitive=True -> no match
  lTxt := 'x abcVALUExyz y';
  lRes := ReplacePlaceholder(
    lTxt, 'ABC', 'XYZ',
    procedure(const aValue: string; aStartMarkerFoundAtIndex: integer; var aReplaceValue: string; var aAction: TReplacePlaceholderAction)
    begin
      aReplaceValue := 'REPLACED';
      aAction := raReplace;
    end,
    1, True);
  Assert.AreEqual(lTxt, lRes);
end;

procedure TMaxLogicStrUtilsTests.ReplacePlaceholder_IncompletePlaceholder_Unchanged;
var
  lTxt, lRes: string;
begin
  // missing end marker -> unchanged
  lTxt := 'A %X B';
  lRes := ReplacePlaceholder(
    lTxt, '%', '%',
    procedure(const aValue: string; aStartMarkerFoundAtIndex: integer; var aReplaceValue: string; var aAction: TReplacePlaceholderAction)
    begin
      aReplaceValue := 'Z';
      aAction := raReplace;
    end
  );
  Assert.AreEqual(lTxt, lRes);
end;

{ ExtractString tests }

procedure TMaxLogicStrUtilsTests.ExtractString_InvalidCharsWindow_ExcludesEndMarkerChar;
var
  lVal: string;
  lIdx: Integer;
  ok: Boolean;
begin
  ok := ExtractString('[abc]', '[', ']', 1, lVal, lIdx, True, TArray<char>.Create(']'), False);
  Assert.IsTrue(ok);
  Assert.AreEqual('abc', lVal);
end;

procedure TMaxLogicStrUtilsTests.ExtractString_CaseSensitivityAndOffset;
var
  lVal: string;
  lIdx: Integer;
  ok: Boolean;
  lTxt: string;
begin
  lTxt := 'zz <<AbC>>';
  ok := ExtractString(lTxt, '<<', '>>', 3, lVal, lIdx, False);
  Assert.IsTrue(ok);
  Assert.AreEqual('AbC', lVal);
end;

procedure TMaxLogicStrUtilsTests.ExtractString_NoStartMarker_ReturnsFalse;
var
  lVal: string;
  lIdx: Integer;
  lOk: Boolean;
begin
  lOk := ExtractString('plain text', '<<', '>>', 1, lVal, lIdx, True);
  Assert.IsFalse(lOk);
end;

procedure TMaxLogicStrUtilsTests.ExtractString_StartNoEnd_ReturnsFalse;
var
  lVal: string;
  lIdx: Integer;
  lOk: Boolean;
begin
  lOk := ExtractString('<<abc', '<<', '>>', 1, lVal, lIdx, True);
  Assert.IsFalse(lOk);
end;

procedure TMaxLogicStrUtilsTests.ExtractString_InvalidChars_SortedSet_Rejects;
var
  lVal: string;
  lIdx: Integer;
  lOk: Boolean;
  lTxt: string;
begin
  lTxt := '[A' + sLineBreak + 'B]';
  lOk := ExtractString(lTxt, '[', ']', 1, lVal, lIdx, True, TArray<char>.Create(#10), True);
  Assert.IsFalse(lOk);
end;

{ StringMatches tests }

procedure TMaxLogicStrUtilsTests.StringMatches_CaseInsensitive_AllPlatforms;
begin
  Assert.IsTrue(StringMatches('File.TXT', '*.txt', False));
end;

procedure TMaxLogicStrUtilsTests.StringMatches_CaseSensitive_MismatchCasesReturnFalse;
begin
  Assert.IsFalse(StringMatches('Abc', 'abc', True));
end;

procedure TMaxLogicStrUtilsTests.StringMatches_Wildcards_And_Anchoring;
begin
  Assert.IsTrue(StringMatches('abcd', 'ab?d', True));
  Assert.IsFalse(StringMatches('abd', 'ab?d', True));
  Assert.IsFalse(StringMatches('abc', 'b', True));
  Assert.IsTrue(StringMatches('abc', 'ab*c', True));
end;

procedure TMaxLogicStrUtilsTests.StringMatches_RegexEscape_Literal;
begin
  Assert.IsTrue(StringMatches('a.b+c', 'a.b+c', True));
  Assert.IsFalse(StringMatches('aXb+c', 'a.b+c', True));
end;

{ Utf8Truncate tests }

procedure TMaxLogicStrUtilsTests.Utf8Truncate_BasicBoundaries;
var
  b: TBytes;
  s: string;
begin
  // ASCII
  b := Utf8TruncateByCodePoint('Hello', 3);
  s := TEncoding.UTF8.GetString(b);
  Assert.AreEqual('Hel', s, ExpectGotMsg('Hel', s));

  // 2-byte char 'é'
  b := Utf8TruncateByCodePoint('éx', 2);
  s := TEncoding.UTF8.GetString(b);
  Assert.AreEqual('é', s, ExpectGotMsg('é', s));
  b := Utf8TruncateByCodePoint('éx', 1);
  s := TEncoding.UTF8.GetString(b);
  Assert.AreEqual('', s, ExpectGotMsg('', s));

  // 3-byte char '€'
  b := Utf8TruncateByCodePoint('€x', 3);
  s := TEncoding.UTF8.GetString(b);
  Assert.AreEqual('€', s, ExpectGotMsg('€', s));
  b := Utf8TruncateByCodePoint('€x', 2);
  s := TEncoding.UTF8.GetString(b);
  Assert.AreEqual('', s, ExpectGotMsg('', s));

  // 4-byte emoji '😀'
  b := Utf8TruncateByCodePoint('😀x', 4);
  s := TEncoding.UTF8.GetString(b);
  Assert.AreEqual('😀', s, ExpectGotMsg('😀', s));
  b := Utf8TruncateByCodePoint('😀x', 3);
  s := TEncoding.UTF8.GetString(b);
  Assert.AreEqual('', s, ExpectGotMsg('', s));
end;

procedure TMaxLogicStrUtilsTests.Utf8Truncate_NoTruncation_ReturnsFull;
var
  lS, lOut: string;
  lBytes, lOutBytes: TBytes;
begin
  lS := 'aé€😀b';
  lBytes := TEncoding.UTF8.GetBytes(lS);
  lOutBytes := Utf8TruncateByCodePoint(lS, Length(lBytes));
  lOut := TEncoding.UTF8.GetString(lOutBytes);
  Assert.AreEqual(lS, lOut);
end;

procedure TMaxLogicStrUtilsTests.Utf8Truncate_ExactByteBudget_UsesFullCodePoint;
var
  b: TBytes;
begin
  b := Utf8TruncateByCodePoint('€A', 3);
  Assert.AreEqual(3, Length(b));
  Assert.AreEqual(Byte($E2), b[0]);
  Assert.AreEqual(Byte($82), b[1]);
  Assert.AreEqual(Byte($AC), b[2]);
end;

{ StrToFloatWCC tests }

procedure TMaxLogicStrUtilsTests.StrToFloatWCC_Formats;
begin
  Assert.AreEqual(1234.56, StrToFloatWCC('1,234.56'), 1e-9);
  Assert.AreEqual(1234.56, StrToFloatWCC('1.234,56'), 1e-9);
  Assert.AreEqual(1234.56, StrToFloatWCC('1234,56'), 1e-9);
  Assert.AreEqual(1234.56, StrToFloatWCC('1234.56'), 1e-9);

  Assert.AreEqual(1234567.89, StrToFloatWCC('1,234.567.89'), 1e-9);
  Assert.AreEqual(1234.56789, StrToFloatWCC('1.234,567,89'), 1e-9);

  Assert.AreEqual(1234.56, StrToFloatWCC('  1 234  ,  56 '), 1e-9);
end;

procedure TMaxLogicStrUtilsTests.StrToFloatWCC_LettersRaise_And_TryFalse_And_Default;
var
  d: Double;
begin
  // Letters present -> StrToFloatWCC raises
  Assert.WillRaise(
    procedure
    begin
      StrToFloatWCC('123x');
    end,
    EConvertError);

  // TryStrToFloatWCC returns False
  Assert.IsFalse(TryStrToFloatWCC('123x', d));

  // Empty string scenarios
  Assert.WillRaise(
    procedure
    begin
      StrToFloatWCC('');
    end,
    EConvertError);
  Assert.IsFalse(TryStrToFloatWCC('', d));
  Assert.AreEqual(42.5, StrToFLoatWccDef('', 42.5), 1e-12);
end;

procedure TMaxLogicStrUtilsTests.StrToFloatWCC_Negative_Numbers_And_SingleSeparatorDecimals;
begin
  // Negative with both separators
  Assert.AreEqual(-1234.56, StrToFloatWCC('-1,234.56'), 1e-9);
  Assert.AreEqual(-1234.56, StrToFloatWCC('-1.234,56'), 1e-9);
  Assert.AreEqual(-1234.56, StrToFLoatWccDef('-1,234.56', 0), 1e-9);
  Assert.AreEqual(-1234.56, StrToFLoatWccDef('-1.234,56', 0), 1e-9);
  // Single separator with three digits on the right remains a decimal input.
  Assert.AreEqual(1.234, StrToFloatWCC('1,234'), 1e-9);
  Assert.AreEqual(1.234, StrToFloatWCC('1.234'), 1e-9);
  Assert.AreEqual(23.023, StrToFloatWCC('23,023'), 1e-9);
  Assert.AreEqual(23.023, StrToFloatWCC('23.023'), 1e-9);
  Assert.AreEqual(23.023, StrToFLoatWccDef('23,023', 0), 1e-9);
  Assert.AreEqual(23.023, StrToFLoatWccDef('23.023', 0), 1e-9);
end;

{ CharPosEx tests }

procedure TMaxLogicStrUtilsTests.CharPosEx_Unsorted_And_Sorted_And_Bounds;
var
  lFoundAt, lIdx: Integer;
  ok: Boolean;
begin
  // Unsorted
  ok := CharPosEx('abcde', TArray<char>.Create('x','c','a'), False, 2, 4, lFoundAt, lIdx);
  Assert.IsTrue(ok);
  Assert.AreEqual(3, lFoundAt); // 'c'
  Assert.AreEqual(1, lIdx);     // index of 'c' in input array ['x','c','a']

  // Sorted with binary search
  ok := CharPosEx('zay', TArray<char>.Create('a','m','y'), True, 1, 3, lFoundAt, lIdx);
  Assert.IsTrue(ok);
  Assert.AreEqual(2, lFoundAt); // 'a'
  Assert.AreEqual(0, lIdx);     // index of 'a' in sorted array

  // Bounds
  ok := CharPosEx('abc', TArray<char>.Create('a'), False, 0, 1, lFoundAt, lIdx);
  Assert.IsFalse(ok);
  ok := CharPosEx('abc', TArray<char>.Create('a'), False, 3, 2, lFoundAt, lIdx);
  Assert.IsFalse(ok);

  // Empty chars
  ok := CharPosEx('abc', nil, False, 1, 3, lFoundAt, lIdx);
  Assert.IsFalse(ok);
end;

procedure TMaxLogicStrUtilsTests.CharPosEx_EndOffsetClamping;
var
  lFoundAt, lIdx: Integer;
  lOk: Boolean;
begin
  lOk := CharPosEx('abc', TArray<char>.Create('c'), False, 1, 100, lFoundAt, lIdx);
  Assert.IsTrue(lOk);
  Assert.AreEqual(3, lFoundAt);
  Assert.AreEqual(0, lIdx);
end;

{ ExpandEnvVars tests }

procedure TMaxLogicStrUtilsTests.ExpandEnvVars_AbsentVar_Stays;
var
  lTxt, lRes: string;
begin
  lTxt := 'X %UNLIKELY_VAR_987654321% Y';
  lRes := ExpandEnvVars(lTxt);
  Assert.AreEqual(lTxt, lRes);
end;

procedure TMaxLogicStrUtilsTests.ExpandEnvVars_PresentVar_Windows;
{$IFDEF MSWINDOWS}
const
  VARNAME = 'ML_STRUTILS_TEST_VAR';
var
  lRes: string;
begin
  Assert.IsTrue(Winapi.Windows.SetEnvironmentVariable(PChar(VARNAME), PChar('VALUE')));
  try
    lRes := ExpandEnvVars('%' + VARNAME + '%');
    Assert.AreEqual('VALUE', lRes);
  finally
    // Clear variable
    Winapi.Windows.SetEnvironmentVariable(PChar(VARNAME), nil);
  end;
end;
{$ELSE}
begin
  Assert.IsTrue(True, 'Skipped on non-Windows');
end;
{$ENDIF}

{ StrCmpLogical tests }

procedure TMaxLogicStrUtilsTests.StrCmpLogical_Windows;
{$IFDEF MSWINDOWS}
var
  lCmp: Integer;
begin
  lCmp := StrCmpLogical('file2', 'file10');
  Assert.IsTrue(lCmp < 0);
end;
{$ELSE}
begin
  Assert.IsTrue(True, 'Skipped on non-Windows');
end;
{$ENDIF}

{ Additional tests }

procedure TMaxLogicStrUtilsTests.Utf8Truncate_ZeroAndNegative_ReturnsEmpty;
var
  b: TBytes;
begin
  b := Utf8TruncateByCodePoint('Hello', 0);
  Assert.AreEqual(0, Length(b));
  b := Utf8TruncateByCodePoint('Hello', -5);
  Assert.AreEqual(0, Length(b));
end;

procedure TMaxLogicStrUtilsTests.ExpandEnvVars_CustomTokens_Windows;
{$IFDEF MSWINDOWS}
const
  VARNAME = 'ML_STRUTILS_TEST_CUSTOM';
var
  lRes: string;
begin
  Assert.IsTrue(Winapi.Windows.SetEnvironmentVariable(PChar(VARNAME), PChar('CVAL')));
  try
    lRes := ExpandEnvVars('${' + VARNAME + '}', '${', '}');
    Assert.AreEqual('CVAL', lRes);
  finally
    Winapi.Windows.SetEnvironmentVariable(PChar(VARNAME), nil);
  end;
end;
{$ELSE}
begin
  Assert.IsTrue(True, 'Skipped on non-Windows');
end;
{$ENDIF}

{ TFilterEx tests }

procedure TMaxLogicStrUtilsTests.FilterEx_AND_Semantics;
var
  lF: TFilterEx;
begin
  lF := TFilterEx.Create('alpha beta');
  Assert.IsTrue(lF.Matches('alpha beta'));
  Assert.IsFalse(lF.Matches('alpha only'));
end;

procedure TMaxLogicStrUtilsTests.FilterEx_OR_Semantics;
var
  lF: TFilterEx;
begin
  lF := TFilterEx.Create('alpha beta|gamma');
  Assert.IsTrue(lF.Matches('alpha beta'));
  Assert.IsTrue(lF.Matches('alpha gamma'));
  Assert.IsFalse(lF.Matches('beta gamma'));
end;

procedure TMaxLogicStrUtilsTests.FilterEx_Wildcards_Mask;
var
  lF: TFilterEx;
begin
  lF := TFilterEx.Create('a?c');
  Assert.IsTrue(lF.Matches('abc'));
  Assert.IsTrue(lF.Matches('aXc'));
  Assert.IsFalse(lF.Matches('ac'));

  lF := TFilterEx.Create('pre*post');
  Assert.IsTrue(lF.Matches('preXYZpost'));
  Assert.IsFalse(lF.Matches('prepostX'));
  Assert.IsFalse(lF.Matches('Xprepost'));
end;

procedure TMaxLogicStrUtilsTests.FilterEx_Starts_Ends;
var
  lF: TFilterEx;
begin
  lF := TFilterEx.Create('pre* *post');
  Assert.IsTrue(lF.Matches('preX Ypost'));
  Assert.IsFalse(lF.Matches('Xprepost'));
  Assert.IsFalse(lF.Matches('prepostY'));
end;

procedure TMaxLogicStrUtilsTests.FilterEx_DoubleStar_Contains;
var
  lF: TFilterEx;
begin
  lF := TFilterEx.Create('*mid*');
  Assert.IsTrue(lF.Matches('amidb'));
  Assert.IsFalse(lF.Matches('amib'));
end;

procedure TMaxLogicStrUtilsTests.FilterEx_Negation;
var
  lF: TFilterEx;
begin
  lF := TFilterEx.Create('!beta alpha');
  Assert.IsTrue(lF.Matches('alpha'));
  Assert.IsFalse(lF.Matches('alpha beta'));
end;

procedure TMaxLogicStrUtilsTests.FilterEx_QuotedToken_WithSpaces;
var
  lF: TFilterEx;
begin
  lF := TFilterEx.Create('"two words" alpha');
  Assert.IsTrue(lF.Matches('two words and alpha'));
  Assert.IsFalse(lF.Matches('two words'));
  Assert.IsFalse(lF.Matches('alpha only'));
end;

procedure TMaxLogicStrUtilsTests.FilterEx_OR_WithQuotedTokens;
var
  lF: TFilterEx;
begin
  lF := TFilterEx.Create('"new york"|"san francisco"');
  Assert.IsTrue(lF.Matches('I love San Francisco'));
  Assert.IsTrue(lF.Matches('NEW YORK is big'));
  Assert.IsFalse(lF.Matches('Los Angeles'));
end;

procedure TMaxLogicStrUtilsTests.FilterEx_CaseInsensitive;
var
  lF: TFilterEx;
begin
  lF := TFilterEx.Create('ALpha BeTa');
  Assert.IsTrue(lF.Matches('this has alpha and beta'));
  Assert.IsTrue(lF.Matches('this has ALPHA and BeTa'));
end;

procedure TMaxLogicStrUtilsTests.FilterEx_MaskVsContains_Precedence;
var
  lF: TFilterEx;
begin
  lF := TFilterEx.Create('ab?d');
  Assert.IsTrue(lF.Matches('abcd'));
  Assert.IsFalse(lF.Matches('abd'));
end;

procedure TMaxLogicStrUtilsTests.FilterEx_EmptyFilter_ReturnsTrue;
var
  lF: TFilterEx;
begin
  lF := TFilterEx.Create('');
  Assert.IsTrue(lF.Matches('anything'));
end;

procedure TMaxLogicStrUtilsTests.FilterEx_Whitespace_And_QuotesHandling;
var
  lF: TFilterEx;
begin
  lF := TFilterEx.Create('  alpha   beta  ');
  Assert.IsTrue(lF.Matches('...Alpha...and...BETA...'));

  lF := TFilterEx.Create('"new  york"   alpha');
  Assert.IsTrue(lF.Matches('alpha in NEW  YORK city'));
  Assert.IsFalse(lF.Matches('alpha in NEW YORK city')); // missing double space phrase
end;

procedure TMaxLogicStrUtilsTests.PutBefore_StringAndNumber;
var
  s: string;
begin
  s := putBefore('7', '0', 3);
  Assert.AreEqual('007', s);
  s := putBefore('12345', '0', 3);
  Assert.AreEqual('12345', s);
  s := putBefore(-12, '0', 5);
  Assert.AreEqual('00-12', s);
end;

procedure TMaxLogicStrUtilsTests.CombineUrl_SlashesAndParts;
begin
  Assert.AreEqual('a/b', CombineUrl('a', 'b'));
  Assert.AreEqual('a/b', CombineUrl('a/', '/b'));
  Assert.AreEqual('http://ex/api/v1/users', CombineUrl(['http://ex', 'api', '/v1/', '/users']));
  Assert.AreEqual('a::b', CombineUrl('a', 'b', '::'));
end;

procedure TMaxLogicStrUtilsTests.MatchesFilter_Basics;
var
  f: TArray<string>;
begin
  SetLength(f, 0);
  Assert.IsTrue(MatchesFilter('anything', f));
  f := TArray<string>.Create('x*', 'ab?');
  Assert.IsTrue(MatchesFilter('abc', f));
  Assert.IsTrue(MatchesFilter('xyz', f));
  Assert.IsFalse(MatchesFilter('bbb', f));
end;

procedure TMaxLogicStrUtilsTests.Split_And_SplitInHalfBy_And_Join;
var
  arr: TArray<string>;
  sl: TStringList;
  L, R: string;
  parts: TArray<string>;
  ok: Boolean;
begin
  arr := Split(',', 'a,b,c');
  Assert.AreEqual(3, Length(arr));
  Assert.AreEqual('a', arr[0]);
  Assert.AreEqual('c', arr[2]);

  sl := TStringList.Create;
  try
    Split('a,b,c', ',', sl);
    Assert.AreEqual(3, sl.Count);
    Assert.AreEqual('b', sl[1]);
  finally
    sl.Free;
  end;

  ok := SplitInHalfBy('a=b', '=', L, R);
  Assert.IsTrue(ok);
  Assert.AreEqual('a', L);
  Assert.AreEqual('b', R);

  ok := SplitInHalfBy('ab', ':', parts);
  Assert.IsFalse(ok);
  Assert.AreEqual(1, Length(parts));
  Assert.AreEqual('ab', parts[0]);

  Assert.AreEqual('1,2,3', Join(',', [1, 2, 3]));
end;

procedure TMaxLogicStrUtilsTests.fStr_Formats_Rounding;
var
  decSep: Char;
begin
  decSep := FormatSettings.DecimalSeparator;
  Assert.AreEqual('1' + decSep + '23', fStr(1.2345, 2, 2));
  Assert.AreEqual('0' + decSep + '000', fStr(0, 2, 3));
end;

procedure TMaxLogicStrUtilsTests.GuidToHex_DefaultGuid;
var
  expected: string;
begin
  expected := StringOfChar('0', SizeOf(TGuid) * 2);
  Assert.AreEqual(expected, GuidToHex(Default(TGuid)));
end;

procedure TMaxLogicStrUtilsTests.RawBytes_RoundTrip;
var
  b, b2: TBytes;
  s: RawByteString;
  i: Integer;
begin
  b := TBytes.Create(1, 2, 3);
  s := BytesToRawStr(b);
  b2 := RawStrToBytes(s);
  Assert.AreEqual(Length(b), Length(b2));
  for i := 0 to High(b) do
    Assert.AreEqual(b[i], b2[i]);

  // empty
  s := BytesToRawStr(nil);
  b2 := RawStrToBytes(s);
  Assert.AreEqual(0, Length(b2));
end;

procedure TMaxLogicStrUtilsTests.ExtractString_AdjacentMarkers_EmptyValue;
var
  val: string;
  idx: Integer;
  ok: Boolean;
begin
  ok := ExtractString('[]', '[', ']', 1, val, idx, True);
  Assert.IsTrue(ok);
  Assert.AreEqual('', val);
end;

procedure TMaxLogicStrUtilsTests.ReplacePlaceholder_StartOffsetBeyondEnd_NoChange;
var
  r: string;
begin
  r := ReplacePlaceholder(
    'abc', '%', '%',
    procedure(const v: string; aStart: integer; var rep: string; var act: TReplacePlaceholderAction)
    begin
      rep := 'X';
    end,
    100, True);
  Assert.AreEqual('abc', r);
end;

procedure TMaxLogicStrUtilsTests.CharPosEx_SinglePositionRange;
var
  posAt, idx: Integer;
  ok: Boolean;
begin
  ok := CharPosEx('abc', TArray<char>.Create('b'), False, 2, 2, posAt, idx);
  Assert.IsTrue(ok);
  Assert.AreEqual(2, posAt);
  Assert.AreEqual(0, idx);
end;

procedure TMaxLogicStrUtilsTests.StrToFloatWCC_PlusAndSpaces;
begin
  Assert.AreEqual(12.5, StrToFloatWCC(' +12.5 '), 1e-9);
end;

procedure TMaxLogicStrUtilsTests.ExpandEnvVars_NestedValue_NoRecursion_Windows;
{$IFDEF MSWINDOWS}
const
  V1 = 'ML_STRUTILS_TEST_N1';
  V2 = 'ML_STRUTILS_TEST_N2';
var
  res: string;
begin
  Assert.IsTrue(Winapi.Windows.SetEnvironmentVariable(PChar(V1), PChar('%' + V2 + '%')));
  Assert.IsTrue(Winapi.Windows.SetEnvironmentVariable(PChar(V2), PChar('ZZ')));
  try
    res := ExpandEnvVars('%' + V1 + '%');
    Assert.AreEqual('%' + V2 + '%', res);
  finally
    Winapi.Windows.SetEnvironmentVariable(PChar(V1), nil);
    Winapi.Windows.SetEnvironmentVariable(PChar(V2), nil);
  end;
end;
{$ELSE}
begin
  Assert.IsTrue(True, 'Skipped on non-Windows');
end;
{$ENDIF}

procedure TMaxLogicStrUtilsTests.ParseStatement_BasicReplacement;
const
  LStatement = 'select :ID:,:Field2: from :t: where :F1: = :V1:';
var
  lParams: TArray<string>;
  lResult: string;
begin
  lParams := [
    'id', 'users.id',
    'field2', 'users.name',
    't', 'users',
    'f1', 'users.id',
    'v1', '42'
  ];
  lResult := ParseStatement(LStatement, lParams);
  Assert.AreEqual('select users.id,users.name from users where users.id = 42', lResult);
end;

procedure TMaxLogicStrUtilsTests.ParseStatement_MissingTokensRemainUnchanged;
const
  LStatement = 'SELECT :Missing:, :Present:';
var
  lParams: TArray<string>;
  lResult: string;
begin
  lParams := ['present', 'X'];
  lResult := ParseStatement(LStatement, lParams);
  Assert.AreEqual('SELECT :Missing:, X', lResult);
end;

procedure TMaxLogicStrUtilsTests.ParseStatement_CustomTokenCharacter;
const
  LStatement = 'Path ~Root~\~Leaf~';
var
  lParams: TArray<string>;
  lResult: string;
begin
  lParams := [
    'root', 'C:',
    'leaf', 'temp'
  ];
  lResult := ParseStatement(LStatement, lParams, '~');
  Assert.AreEqual('Path C:\temp', lResult);
end;

procedure TMaxLogicStrUtilsTests.PrettyElapsed_Boundaries;
begin
  Assert.AreEqual('750ms', PrettyElapsed(750));
  Assert.AreEqual('1s 5ms', PrettyElapsed(1005));
  Assert.AreEqual('1m 1s 0ms', PrettyElapsed(61000));
  Assert.AreEqual('1h 1m 1s 0ms', PrettyElapsed(3661000));
end;

procedure TMaxLogicStrUtilsTests.FloatToHex_PaddingAndTrimming;
var
  lDefault, lTrimmed, lPadded, lNegative: string;
begin
  lDefault := floatToHex(0.0);
  Assert.AreEqual('0000000000000000', lDefault);

  lTrimmed := floatToHex(1.0, 4);
  Assert.AreEqual('F03F', lTrimmed);

  lPadded := floatToHex(1.0, 20);
  Assert.AreEqual(20, lPadded.Length);
  Assert.IsTrue(lPadded.EndsWith('F03F'));

  lNegative := floatToHex(-2.5);
  Assert.AreEqual('00000000000004C0', lNegative);
end;

procedure TMaxLogicStrUtilsTests.TRegExHelper_AnonymousReplace;
var
  lRegex: TRegEx;
  lResult: string;
begin
  lRegex := TRegEx.Create('(\d+)');
  lResult := lRegex.Replace(
    'abc123def',
    function(const aMatch: TMatch): string
    begin
      Result := '[' + aMatch.Value + ']';
    end);
  Assert.AreEqual('abc[123]def', lResult);
end;

procedure TMaxLogicStrUtilsTests.FastComparer_EmptyStrings;
var
  lComparer: IEqualityComparer<string>;
begin
  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCase;
  Assert.IsTrue(lComparer.Equals('', ''));
  Assert.AreEqual(lComparer.GetHashCode(''), lComparer.GetHashCode(''));

  lComparer := TFastCaseAwareComparer.Ordinal;
  Assert.IsTrue(lComparer.Equals('', ''));
  Assert.AreEqual(lComparer.GetHashCode(''), lComparer.GetHashCode(''));
end;

procedure TMaxLogicStrUtilsTests.FastComparer_SingletonStrings;
var
  lComparer: IEqualityComparer<string>;
begin
  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCase;
  Assert.IsTrue(lComparer.Equals('a', 'A'));
  Assert.AreEqual(lComparer.GetHashCode('a'), lComparer.GetHashCode('A'));
  Assert.IsTrue(lComparer.Equals('Z', 'z'));

  lComparer := TFastCaseAwareComparer.Ordinal;
  Assert.IsFalse(lComparer.Equals('a', 'A'));
  Assert.AreNotEqual(lComparer.GetHashCode('a'), lComparer.GetHashCode('A'));
end;

procedure TMaxLogicStrUtilsTests.FastComparer_UnicodeEdgeCases;
var
  lComparer: IEqualityComparer<string>;
  lHash1, lHash2: Integer;
begin
  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCase;

  // Greek small/upper omega pair – exercise non-ASCII path, hashes must match
  Assert.IsTrue(
    lComparer.Equals('Ω', 'ω'),
    'OrdinalIgnoreCase should treat Greek omega Ω/ω as equal');

  lHash1 := lComparer.GetHashCode('Ω');
  lHash2 := lComparer.GetHashCode('ω');
  Assert.AreEqual(
    lHash1, lHash2,
    Format('Hash mismatch for Ω vs ω (case-insensitive). Hash1=%d, Hash2=%d',
      [lHash1, lHash2]));

  // Case-sensitive comparer must still distinguish them
  lComparer := TFastCaseAwareComparer.Ordinal;
  Assert.IsFalse(
    lComparer.Equals('Ω', 'ω'),
    'Ordinal (case-sensitive) must distinguish Ω vs ω');
end;

procedure TMaxLogicStrUtilsTests.FastComparer_LongStrings;
var
  lComparer: IEqualityComparer<string>;
  lLongStr1, lLongStr2: string;
begin
  lLongStr1 := StringOfChar('a', 300);
  lLongStr2 := StringOfChar('A', 300);

  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCase;
  Assert.IsTrue(lComparer.Equals(lLongStr1, lLongStr2));
  Assert.AreEqual(lComparer.GetHashCode(lLongStr1), lComparer.GetHashCode(lLongStr2));

  lLongStr2 := lLongStr2 + 'B';
  Assert.IsFalse(lComparer.Equals(lLongStr1, lLongStr2));

  lComparer := TFastCaseAwareComparer.Ordinal;
  Assert.IsFalse(lComparer.Equals(lLongStr1, StringOfChar('A', 300)));
end;

procedure TMaxLogicStrUtilsTests.FastComparer_HashCollisionResistance;
var
  lComparer: IEqualityComparer<string>;
  lHash1, lHash2, lHash3: Integer;
begin
  lComparer := TFastCaseAwareComparer.Ordinal;
  
  lHash1 := lComparer.GetHashCode('abc');
  lHash2 := lComparer.GetHashCode('abd');
  lHash3 := lComparer.GetHashCode('cba');
  
  Assert.AreNotEqual(lHash1, lHash2);
  Assert.AreNotEqual(lHash1, lHash3);
  Assert.AreNotEqual(lHash2, lHash3);

  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCase;
  lHash1 := lComparer.GetHashCode('Key123');
  lHash2 := lComparer.GetHashCode('Key124');
  lHash3 := lComparer.GetHashCode('Key223');
  
  Assert.AreNotEqual(lHash1, lHash2);
  Assert.AreNotEqual(lHash1, lHash3);
end;

procedure TMaxLogicStrUtilsTests.FastComparer_MixedAsciiUnicode;
var
  lComparer: IEqualityComparer<string>;
begin
  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCase;
  
  Assert.IsTrue(lComparer.Equals('Test123', 'test123'));
  Assert.IsTrue(lComparer.Equals('αβγ123', 'ΑΒΓ123'));
  Assert.IsTrue(lComparer.Equals('Hello世界', 'HELLO世界'));
  
  Assert.AreEqual(lComparer.GetHashCode('Test123'), lComparer.GetHashCode('TEST123'));
  Assert.AreEqual(lComparer.GetHashCode('αβγ123'), lComparer.GetHashCode('ΑΒΓ123'));

  lComparer := TFastCaseAwareComparer.Ordinal;
  Assert.IsFalse(lComparer.Equals('Hello世界', 'HELLO世界'));
end;

procedure TMaxLogicStrUtilsTests.FastComparer_CaseFoldingEdgeCases;
var
  lComparer: IEqualityComparer<string>;
  lOrdinalComparer: TCustomComparer<string>;
  lOddLeft, lOddRightEqual, lOddRightMismatch: string;
  lSharpLeft, lSharpRight: string;
  lOrdinalIsEqual: Boolean;
begin
  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCase;

  lOddLeft := 'OddLené';
  lOddRightEqual := 'ODDLENÉ';
  Assert.IsTrue(lComparer.Equals(lOddLeft, lOddRightEqual));
  Assert.AreEqual(lComparer.GetHashCode(lOddLeft), lComparer.GetHashCode(lOddRightEqual));

  lOddRightMismatch := 'ODDLENÊ';
  Assert.IsFalse(lComparer.Equals(lOddLeft, lOddRightMismatch));

  lSharpLeft := 'Straße';
  lSharpRight := 'STRASSE';
  lOrdinalComparer := TStringComparer.OrdinalIgnoreCase;
  lOrdinalIsEqual := lOrdinalComparer.Compare(lSharpLeft, lSharpRight) = 0;
  Assert.AreEqual(
    lOrdinalIsEqual,
    lComparer.Equals(lSharpLeft, lSharpRight),
    'Ordinal ignore-case semantics must match TStringComparer.OrdinalIgnoreCase');
  Assert.IsFalse(
    lOrdinalIsEqual,
    'Pure ordinal folding keeps ß distinct from SS; locale-aware callers must use TIStringComparer.');
end;

procedure TMaxLogicStrUtilsTests.FastComparer_SurrogatePairs;
var
  lComparer: IEqualityComparer<string>;
  lLower, lUpper: string;
begin
  lLower := '😀foo';
  lUpper := '😀FOO';

  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCase;
  Assert.IsTrue(lComparer.Equals(lLower, lUpper));
  Assert.AreEqual(lComparer.GetHashCode(lLower), lComparer.GetHashCode(lUpper));

  lComparer := TFastCaseAwareComparer.Ordinal;
  Assert.IsFalse(lComparer.Equals(lLower, lUpper));
end;

procedure TMaxLogicStrUtilsTests.FastComparer_NullCharHandling;
var
  lComparer: IEqualityComparer<string>;
  lLower, lUpper: string;
begin
  lLower := 'abc'#0'def';
  lUpper := 'ABC'#0'DEF';

  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCase;
  Assert.IsTrue(lComparer.Equals(lLower, lUpper));
  Assert.AreEqual(lComparer.GetHashCode(lLower), lComparer.GetHashCode(lUpper));

  lComparer := TFastCaseAwareComparer.Ordinal;
  Assert.IsFalse(lComparer.Equals(lLower, lUpper));
end;

procedure TMaxLogicStrUtilsTests.FastComparer_DictionaryIntegration;
var
  lDict: TDictionary<string, Integer>;
  lValue: Integer;
begin
  lDict := TDictionary<string, Integer>.Create(TFastCaseAwareComparer.OrdinalIgnoreCase);
  try
    lDict.Add('Header', 1);
    lDict.Add('token', 2);

    Assert.IsTrue(lDict.TryGetValue('HEADER', lValue));
    Assert.AreEqual(1, lValue);
    Assert.IsTrue(lDict.TryGetValue('TOKEN', lValue));
    Assert.AreEqual(2, lValue);

    lDict['HEADER'] := 10;
    Assert.IsTrue(lDict.TryGetValue('header', lValue));
    Assert.AreEqual(10, lValue);
  finally
    lDict.Free;
  end;

  lDict := TDictionary<string, Integer>.Create(TFastCaseAwareComparer.Ordinal);
  try
    lDict.Add('Key', 1);
    Assert.IsFalse(lDict.ContainsKey('key'));
    lDict.Add('key', 2);
    Assert.AreEqual(2, lDict['key']);
  finally
    lDict.Free;
  end;
end;

procedure TMaxLogicStrUtilsTests.FastComparer_HashBoundaryCoverage;
var
  lComparer: IEqualityComparer<string>;
  s15, s16, s17: string;
  h15a, h15b, h16, h17: Integer;
begin
  lComparer := TFastCaseAwareComparer.Ordinal;
  s15 := StringOfChar('a', 15);
  s16 := StringOfChar('a', 16);
  s17 := StringOfChar('a', 17);

  h15a := lComparer.GetHashCode(s15);
  h15b := lComparer.GetHashCode(s15);
  h16 := lComparer.GetHashCode(s16);
  h17 := lComparer.GetHashCode(s17);

  Assert.AreEqual(h15a, h15b, 'Hash must be stable for identical input');
  Assert.AreNotEqual(h15a, h16, 'Different lengths around 16-byte boundary must hash differently');
  Assert.AreNotEqual(h16, h17, 'Slightly longer inputs must hash differently');

  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCase;
  Assert.AreEqual(
    lComparer.GetHashCode(StringOfChar('A', 32)),
    lComparer.GetHashCode(StringOfChar('a', 32)),
    'Case folding should not alter 4-lane hashing determinism');
end;

procedure TMaxLogicStrUtilsTests.FastComparer_VeryLargeInput;
const
  cLength = 200000;
var
  lComparerCI, lComparerOrd: IEqualityComparer<string>;
  lBuilder: TStringBuilder;
  lLarge, lLargeUpper: string;
  i: Integer;
begin
  lBuilder := TStringBuilder.Create(cLength);
  try
    for i := 0 to Pred(cLength) do
      if (i and 1) = 0 then
        lBuilder.Append(Char(Ord('a') + (i mod 26)))
      else
        lBuilder.Append(Char($03B1 + (i mod 4))); // α..δ
    lLarge := lBuilder.ToString;
  finally
    lBuilder.Free;
  end;
  lLargeUpper := lLarge.ToUpper;

  lComparerCI := TFastCaseAwareComparer.OrdinalIgnoreCase;
  Assert.IsTrue(lComparerCI.Equals(lLarge, lLargeUpper));
  Assert.AreEqual(lComparerCI.GetHashCode(lLarge), lComparerCI.GetHashCode(lLargeUpper));

  lComparerOrd := TFastCaseAwareComparer.Ordinal;
  Assert.IsFalse(lComparerOrd.Equals(lLarge, lLargeUpper));
end;

procedure TMaxLogicStrUtilsTests.Utf8Comparer_OrdinalBytesAndDictionary;
var
  lComparer: IEqualityComparer<UTF8String>;
  g: TGarbos;
  lDictionary: TDictionary<UTF8String, Integer>;
  lKey, lOther, lCopy: UTF8String;
begin
  lComparer := TFastCaseAwareComparer.OrdinalUtf8;
  lKey := UTF8String('Grüße'#0'東京😀');
  lOther := UTF8String('grüße'#0'東京😀');
  lCopy := Copy(lKey, 1, Length(lKey));
  Assert.IsTrue(lComparer.Equals(lKey, lCopy));
  Assert.AreEqual(lComparer.GetHashCode(lKey), lComparer.GetHashCode(lCopy));
  Assert.IsFalse(lComparer.Equals(lKey, lOther));
  Assert.IsFalse(lComparer.Equals(lKey, UTF8String('Grüße')));
  Assert.IsTrue(lComparer.Equals('', ''));
  Assert.IsFalse(lComparer.Equals('', lKey));
  // Ordinal comparison preserves bytes even for malformed UTF-8.
  SetLength(lKey, 1);
  lKey[1] := AnsiChar($FF);
  SetLength(lOther, 1);
  lOther[1] := AnsiChar($FE);
  Assert.IsFalse(lComparer.Equals(lKey, lOther));
  GC(lDictionary, TDictionary<UTF8String, Integer>.Create(lComparer), g);
  lDictionary.Add(lKey, 17);
  Assert.AreEqual(17, lDictionary[lKey]);
  Assert.IsFalse(lDictionary.ContainsKey(lOther));
end;

procedure TMaxLogicStrUtilsTests.Utf8Comparer_IgnoreCaseUnicodeAndDictionary;
var
  lComparer: IEqualityComparer<UTF8String>;
  lUnicodeComparer: IEqualityComparer<string>;
  g: TGarbos;
  lDictionary: TDictionary<UTF8String, Integer>;
  lLeft, lRight: TArray<string>;
  lKey, lOther: UTF8String;
  i: Integer;
begin
  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCaseUtf8;
  lUnicodeComparer := TFastCaseAwareComparer.OrdinalIgnoreCase;
  lLeft := ['', 'Key42', 'Grüße', 'ſ', 'ı', 'σ', 'Ж', '東京😀',
    'ab'#0'cd', 'ß', 'é', '𐐀'];
  lRight := ['', 'kEY42', 'GRÜßE', 'S', 'I', 'ς', 'ж', '東京😀',
    'AB'#0'CD', 'SS', 'é', '𐐨'];
  GC(lDictionary, TDictionary<UTF8String, Integer>.Create(lComparer), g);
  for i := 0 to High(lLeft) do
  begin
    lKey := UTF8String(lLeft[i]);
    lOther := UTF8String(lRight[i]);
    Assert.AreEqual(lUnicodeComparer.Equals(lLeft[i], lRight[i]),
      lComparer.Equals(lKey, lOther), 'Case-fold parity at ' + IntToStr(i));
    lDictionary.Clear;
    lDictionary.Add(lKey, i);
    if lUnicodeComparer.Equals(lLeft[i], lRight[i]) then
    begin
      Assert.AreEqual(lComparer.GetHashCode(lKey), lComparer.GetHashCode(lOther));
      Assert.AreEqual(i, lDictionary[lOther]);
    end else begin
      Assert.IsFalse(lDictionary.ContainsKey(lOther));
    end;
  end;
  Assert.IsFalse(lComparer.Equals(UTF8String('ab'#0'cd'), UTF8String('ab'#0'ce')));
  Assert.IsFalse(lComparer.Equals('', UTF8String('x')));
end;

procedure TMaxLogicStrUtilsTests.Utf8Comparer_HashBoundaries;
var
  lComparer: IEqualityComparer<UTF8String>;
  lLeft, lRight: UTF8String;
  lSuffix: Char;
  i: Integer;
begin
  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCaseUtf8;
  for lSuffix in string('sµıȿſ') do
    for i := 0 to 65 do
    begin
      lLeft := UTF8String(StringOfChar('a', i) + lSuffix);
      lRight := UTF8String(StringOfChar('A', i) + lSuffix.ToUpper);
      Assert.IsTrue(lComparer.Equals(lLeft, lRight));
      Assert.AreEqual(lComparer.GetHashCode(lLeft), lComparer.GetHashCode(lRight));
    end;
  lComparer := TFastCaseAwareComparer.OrdinalUtf8;
  for i := 0 to 65 do
  begin
    lLeft := UTF8String(StringOfChar('a', i));
    lRight := Copy(lLeft, 1, Length(lLeft));
    Assert.IsTrue(lComparer.Equals(lLeft, lRight));
    Assert.AreEqual(lComparer.GetHashCode(lLeft), lComparer.GetHashCode(lRight));
  end;
end;

procedure TMaxLogicStrUtilsTests.Utf8FilterEx_NativeSyntax;
var
  lFilter: TFilterExUtf8;
begin
  lFilter := TFilterExUtf8.Create('grüße 東京 !pdf *txt');
  Assert.IsTrue(lFilter.Matches(UTF8String('GRÜßE 東京 file.TXT')));
  Assert.IsFalse(lFilter.Matches(UTF8String('GRÜßE 東京 file.pdf.txt')));
  lFilter := TFilterExUtf8.Create('"grün blau"|東京');
  Assert.IsTrue(lFilter.Matches(UTF8String('prefix GRÜN BLAU suffix')));
  Assert.IsTrue(lFilter.Matches(UTF8String('東京')));
  Assert.IsFalse(lFilter.Matches(UTF8String('grün other blau')));
  lFilter := TFilterExUtf8.Create('ä* *東京');
  Assert.IsTrue(lFilter.Matches(UTF8String('Äx 東京')));
  Assert.IsFalse(lFilter.Matches(UTF8String('xÄ 東京')));
  lFilter := TFilterExUtf8.Create('*grün*');
  Assert.IsTrue(lFilter.Matches(UTF8String('xGRÜNy')));
  lFilter := TFilterExUtf8.Create('ä?');
  Assert.IsTrue(lFilter.Matches(UTF8String('Ä😀')));
  Assert.IsFalse(lFilter.Matches(UTF8String('Ä😀x')));
  lFilter := TFilterExUtf8.Create('');
  Assert.IsTrue(lFilter.Matches(''));
end;

procedure TMaxLogicStrUtilsTests.Utf8Masks_UnicodeAndEmptyFilters;
var
  lText, lPattern: UTF8String;
  lFilters: TArray<UTF8String>;
begin
  lText := UTF8String('Grün.txt');
  lPattern := UTF8String('Gr?n.txt');
  Assert.IsTrue(StringMatches(lText, lPattern));
  lPattern := UTF8String('gr?n.TXT');
  Assert.IsFalse(StringMatches(lText, lPattern));
  Assert.IsTrue(StringMatches(lText, lPattern, False));
  lFilters := [];
  Assert.IsTrue(MatchesFilter(lText, lFilters));
  lFilters := [UTF8String('*.pdf'), UTF8String('Gr?n.txt')];
  Assert.IsTrue(MatchesFilter(lText, lFilters));
  lFilters := [UTF8String('*.pdf')];
  Assert.IsFalse(MatchesFilter(lText, lFilters));
end;

procedure TMaxLogicStrUtilsTests.Utf8_NativePathsDoNotAllocate;
var
  lComparer: IEqualityComparer<UTF8String>;
  lFilter: TFilterExUtf8;
  lText, lOther, lMask: UTF8String;
  lMasks: TArray<UTF8String>;
  lMatch: Boolean;
  lHash: Integer;
  lCount: NativeInt;
begin
  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCaseUtf8;
  lText := UTF8String('Grüße 東京😀 file.txt');
  lOther := UTF8String('GRÜßE 東京😀 FILE.TXT');
  lMask := UTF8String('Grüße*?.txt');
  lMasks := [UTF8String('*.pdf'), lMask];
  lFilter := TFilterExUtf8.Create(UTF8String('grüße 東京 !pdf *txt'));
  lComparer.GetHashCode(lText);
  lComparer.Equals(lText, lOther);
  lFilter.Matches(lText);
  StringMatches(lText, lMask, False);
  lCount := CountUtf8Allocations(
    procedure
    begin
      lHash := lComparer.GetHashCode(lText);
      lMatch := lComparer.Equals(lText, lOther) and lFilter.Matches(lText) and
        StringMatches(lText, lMask, False) and MatchesFilter(lText, lMasks);
    end);
  Assert.IsTrue(lMatch);
  Assert.AreEqual(lComparer.GetHashCode(lText), lHash);
  Assert.AreEqual(NativeInt(0), lCount, 'Prepared UTF-8 matching and comparison must allocate nothing');
end;

procedure TMaxLogicStrUtilsTests.Utf8_Performance;
const
  cIterations = 4000;
  cSamples = 9;
var
  lComparer: IEqualityComparer<UTF8String>;
  lFilter: TFilterExUtf8;
  lTexts: TArray<UTF8String>;
  lTimes: TArray<Int64>;
  lWatch: TStopwatch;
  lMatches, lHash, i, j, n: Integer;
begin
  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCaseUtf8;
  lFilter := TFilterExUtf8.Create(UTF8String('grüße 東京 !pdf *txt'));
  lTexts := [UTF8String('Grüße 東京 short.txt'),
    UTF8String('GRÜßE 東京 ' + StringOfChar('x', 256) + '.TXT'),
    UTF8String('other text.pdf')];
  SetLength(lTimes, cSamples);
  lHash := 0;
  for n := 0 to cSamples do
  begin
    lMatches := 0;
    lWatch := TStopwatch.StartNew;
    for i := 1 to cIterations do
      for j := 0 to High(lTexts) do
      begin
        if lFilter.Matches(lTexts[j]) then
          Inc(lMatches);
        lHash := lHash xor lComparer.GetHashCode(lTexts[j]);
      end;
    if n > 0 then
      lTimes[n - 1] := lWatch.ElapsedTicks;
    Assert.AreEqual(cIterations * 2, lMatches);
  end;
  TArray.Sort<Int64>(lTimes);
  LogPerfMessage(Format('UTF8 batch: median=%.0f us, p95=%.0f us, min=%.0f us, max=%.0f us; hash=%d',
    [TicksToMicroseconds(lTimes[cSamples div 2]), TicksToMicroseconds(lTimes[cSamples - 1]),
     TicksToMicroseconds(lTimes[0]), TicksToMicroseconds(lTimes[cSamples - 1]), lHash]));
end;

procedure TMaxLogicStrUtilsTests.Utf8_MaskScalarsSetsAndBacktracking;
var
  lFilter: TFilterExUtf8;
begin
  Assert.IsTrue(StringMatches(UTF8String('😀'), UTF8String('?')));
  Assert.IsFalse(StringMatches(UTF8String('😀'), UTF8String('??')));
  Assert.IsFalse(StringMatches(UTF8String(''), UTF8String('*?')));
  Assert.IsTrue(StringMatches(UTF8String(#10), UTF8String('?')));
  Assert.IsTrue(StringMatches(UTF8String('abcabcz'), UTF8String('*ab?z')));
  Assert.IsFalse(StringMatches(UTF8String('abcabcz'), UTF8String('*ab?x')));
  Assert.IsTrue(StringMatches(UTF8String('a😀b東京c'), UTF8String('*a*b??c')));
  Assert.IsTrue(StringMatches(UTF8String(string('ö')), UTF8String('[ä-ö]')));
  Assert.IsTrue(StringMatches(UTF8String('Öx'), UTF8String('[ä-ö]?'), False));
  Assert.IsFalse(StringMatches(UTF8String('Öx'), UTF8String('[ä-ö]?'), True));
  Assert.IsTrue(StringMatches(UTF8String('😀'), UTF8String('[😀-🙏]')));
  Assert.IsTrue(StringMatches(UTF8String('9'), UTF8String('[!a-z]'), False));
  Assert.IsFalse(StringMatches(UTF8String('A'), UTF8String('[!a-z]'), False));
  Assert.IsTrue(StringMatches(UTF8String('a'#0'b'), UTF8String('a?b')));
  Assert.WillRaise(
    procedure
    begin
      StringMatches(UTF8String(''), UTF8String('[abc'));
    end, EMaskException);
  Assert.WillRaise(
    procedure
    begin
      StringMatches(UTF8String('x'), UTF8String('[z-a]'));
    end, EMaskException);
  Assert.WillRaise(
    procedure
    begin
      StringMatches(UTF8String('x'), UTF8String('[]'));
    end, EMaskException);
  lFilter := TFilterExUtf8.Create('*[!0-9].txt');
  Assert.IsTrue(lFilter.Matches(UTF8String('fileö.txt')), 'Prepared negated character set');
  Assert.IsFalse(lFilter.Matches(UTF8String('file9.txt')));
end;

procedure TMaxLogicStrUtilsTests.Utf8_MalformedBytesStayDistinct;
var
  lComparer: IEqualityComparer<UTF8String>;
  lLeft, lRight: UTF8String;
  lFilter: TFilterExUtf8;
begin
  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCaseUtf8;
  SetLength(lLeft, 1);
  SetLength(lRight, 1);
  for var i := $80 to $FF do
  begin
    lLeft[1] := AnsiChar(i);
    lRight[1] := AnsiChar(i xor 1);
    Assert.IsFalse(lComparer.Equals(lLeft, lRight));
    Assert.IsTrue(StringMatches(lLeft, UTF8String('?')));
    Assert.IsTrue(StringMatches(lLeft, lLeft));
    lFilter := TFilterExUtf8.Create(UTF8String('*') + lLeft);
    Assert.IsTrue(lFilter.Matches(UTF8String('x') + lLeft));
    Assert.IsFalse(lFilter.Matches(UTF8String('x') + lRight));
  end;
  SetLength(lLeft, 3);
  lLeft[1] := AnsiChar($ED);
  lLeft[2] := AnsiChar($A0);
  lLeft[3] := AnsiChar($80);
  Assert.IsTrue(StringMatches(lLeft, UTF8String('???')));
  Assert.IsFalse(StringMatches(lLeft, UTF8String('?')));
  lRight := UTF8String(string('�'));
  Assert.IsFalse(lComparer.Equals(lLeft, lRight));
  lLeft[1] := AnsiChar($E0);
  lLeft[2] := AnsiChar($80);
  Assert.IsTrue(StringMatches(lLeft, UTF8String('???')));
  SetLength(lLeft, 4);
  lLeft[1] := AnsiChar($F4);
  lLeft[2] := AnsiChar($8F);
  lLeft[3] := AnsiChar($BF);
  lLeft[4] := AnsiChar($BF);
  Assert.IsTrue(StringMatches(lLeft, UTF8String('?')));
  lRight := Copy(lLeft, 1, 4);
  Assert.AreEqual(lComparer.GetHashCode(lLeft), lComparer.GetHashCode(lRight));
  lFilter := TFilterExUtf8.Create(UTF8String('*') + lLeft);
  Assert.IsTrue(lFilter.Matches(UTF8String('x') + lLeft));
  lLeft[2] := AnsiChar($90);
  Assert.IsTrue(StringMatches(lLeft, UTF8String('????')));
  Assert.IsFalse(lComparer.Equals(lLeft, lRight));
end;

procedure TMaxLogicStrUtilsTests.Utf8_FilterParsingAndRecordCopies;
var
  lFilter, lCopy: TFilterExUtf8;
begin
  lFilter := Default(TFilterExUtf8);
  Assert.IsTrue(lFilter.Matches('anything'));
  lFilter := TFilterExUtf8.Create('  "a b"|東京  !pdf '#9);
  Assert.IsTrue(lFilter.Matches('a b.txt'));
  Assert.IsTrue(lFilter.Matches(UTF8String('東京.txt')));
  Assert.IsFalse(lFilter.Matches('a b.pdf'));
  lCopy := lFilter;
  lFilter := TFilterExUtf8.Create('other');
  Assert.IsFalse(lFilter.Matches('a b.txt'));
  Assert.IsTrue(lCopy.Matches('a b.txt'));
  lFilter := TFilterExUtf8.Create('"a""b"');
  Assert.IsTrue(lFilter.Matches('a"b'));
  lFilter := TFilterExUtf8.Create('"x|y"');
  Assert.IsTrue(lFilter.Matches('x|y'));
  Assert.IsFalse(lFilter.Matches('x'));
  lFilter := TFilterExUtf8.Create('*');
  Assert.IsTrue(lFilter.Matches(''));
  lFilter := TFilterExUtf8.Create('**');
  Assert.IsFalse(lFilter.Matches(''));
  lFilter := TFilterExUtf8.Create(UTF8String('a'#0'b*'));
  Assert.IsTrue(lFilter.Matches(UTF8String('A'#0'Btail')));
  Assert.IsFalse(lFilter.Matches(UTF8String('A'#0'Ctail')));
end;

procedure TMaxLogicStrUtilsTests.Utf8_ComparerAllBmpCaseMappings;
var
  lComparer: IEqualityComparer<UTF8String>;
  lLeft, lRight: UTF8String;
  c: Char;
begin
  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCaseUtf8;
  for var i := 0 to $FFFF do
  begin
    if (i >= $D800) and (i <= $DFFF) then
      Continue;
    c := Char(i);
    lLeft := UTF8String(string(c));
    lRight := UTF8String(string(c.ToUpper));
    Assert.IsTrue(lComparer.Equals(lLeft, lRight));
    Assert.AreEqual(lComparer.GetHashCode(lLeft), lComparer.GetHashCode(lRight));
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMaxLogicStrUtilsTests);

end.
