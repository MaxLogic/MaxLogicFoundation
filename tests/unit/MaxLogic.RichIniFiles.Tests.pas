unit MaxLogic.RichIniFiles.Tests;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TRichIniFilesTests = class
  public
    [Test] procedure DefaultOptions_HaveExpectedDefaults;
    [Test] procedure Create_EnsuresCommentPrefixesAndDelimiter;
    [Test] procedure LoadFromFile_ParsesBasicDocument;
    [Test] procedure ReadString_LastWinsAcrossDuplicates;
    [Test] procedure ReadSection_CollapsesKeysWithOrderByLastOccurrence;
    [Test] procedure ReadSectionValues_ProvidesCollapsedPairs;
    [Test] procedure ReadSections_FirstAppearanceOrder;
    [Test] procedure ReadAllKeyValues_ReturnsAllOccurrences;
    [Test] procedure KeyCount_AndLastKeyIndex_TrackDuplicates;
    [Test] procedure CaseInsensitive_ReadsHonorNormalization;
    [Test] procedure WriteString_EditLastOccurrence_Canonicalizes;
    [Test] procedure WriteString_AppendsAndCreatesSection;
    [Test] procedure WriteStringIndexed_EditsSpecificOccurrence;
    [Test] procedure AppendKey_ReturnsExpectedOccurrenceIndex;
    [Test] procedure DeleteKey_RemovesAllOccurrencesAndOwnedComments;
    [Test] procedure DeleteKeyIndexed_RemovesOnlyRequestedOccurrence;
    [Test] procedure EraseSection_RemovesAllOccurrences;
    [Test] procedure WriteIntegerAndBool_RoundTrip;
    [Test] procedure ReadComment_ReturnsOwnedComments;
    [Test] procedure WriteComment_ReplacesExistingOwnedBlock;
    [Test] procedure PurgeComments_RemovesAllLines;
    [Test] procedure WriteMultilineString_EncodesValue;
    [Test] procedure ReadMultilineString_DecodesStoredValue;
    [Test] procedure ConsolidateSection_MergesIntoFirstBlock;
    [Test] procedure ConsolidateAll_MergesEverySection;
    [Test] procedure SaveToFile_RoundTripPreservesRawContent;
    [Test] procedure SaveToFile_CanonicalizesModifiedLines;
    [Test] procedure SaveToFile_RespectsEncodingAndNewlineOptions;
    [Test] procedure SaveToFile_ResetsDirtyAndState;
    [Test] procedure CommentOwnership_AttachToPrev_OwnsAboveKey;
    [Test] procedure CommentOwnership_None_LeavesOrphans;
    [Test] procedure CreateFromStrings_ParsesInputAndMaintainsState;
    [Test] procedure SaveToFile_AnsiEncoding_NoBom;
    [Test] procedure SaveToFile_CustomEncodingWithBom;
    [Test] procedure SaveToFile_BpAsSourceWithoutSource_WritesNoBom;
    [Test] procedure SaveToFile_RespectsExplicitNewlineOnFreshDocument;
    [Test] procedure ConsolidateSection_GlobalSectionCollapsesDuplicates;
    [Test] procedure SaveToFile_BpAsSourcePreservesLoadedBom;
    [Test] procedure SaveToFile_PreservesInputNewlinesAfterEdit;
    [Test] procedure DeleteKey_AttachToPrev_RemovesOwnedComment;
    [Test] procedure WriteComment_UsesFirstConfiguredPrefix;
  end;

implementation

uses
  System.IOUtils,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  MaxLogic.RichIniFile;

function LoadIniFromText(const aContent: string; const aOptions: TRichIniOptions): TRichIniFile;
var
  lTempFile: string;
  lOptions: TRichIniOptions;
begin
  lOptions := aOptions;
  lTempFile := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
  TFile.WriteAllText(lTempFile, aContent, TEncoding.UTF8);
  Result := TRichIniFile.Create('', lOptions);
  try
    try
      Result.LoadFromFile(lTempFile);
    finally
      if TFile.Exists(lTempFile) then
        TFile.Delete(lTempFile);
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ TRichIniFilesTests }

procedure TRichIniFilesTests.Create_EnsuresCommentPrefixesAndDelimiter;
var
  lOptions: TRichIniOptions;
  lFile: TRichIniFile;
begin
  lOptions := DefaultRichIniOptions;
  SetLength(lOptions.CommentPrefixes, 0);
  lOptions.KeyValueDelimiter := #0;
  lFile := TRichIniFile.Create('', lOptions);
  try
    Assert.AreEqual(3, Length(lFile.Options.CommentPrefixes));
    Assert.AreEqual(';', lFile.Options.CommentPrefixes[0]);
    Assert.AreEqual('=', lFile.Options.KeyValueDelimiter);
  finally
    lFile.Free;
  end;
end;

procedure TRichIniFilesTests.DefaultOptions_HaveExpectedDefaults;
var
  lOptions: TRichIniOptions;
begin
  lOptions := DefaultRichIniOptions;
  Assert.AreEqual(Integer(eoAutoDetect), Integer(lOptions.LoadEncoding));
  Assert.AreEqual(Integer(eoUTF8), Integer(lOptions.SaveEncoding));
  Assert.IsTrue(lOptions.CustomEncoding = nil);
  Assert.AreEqual(Integer(bpAsSource), Integer(lOptions.BomPolicy));
  Assert.AreEqual(Integer(nlPlatform), Integer(lOptions.NewlineMode));
  Assert.AreEqual(Integer(csCaseSensitive), Integer(lOptions.CaseSensitivity));
  Assert.AreEqual(Integer(mbTreatAsKey), Integer(lOptions.AcceptMissingBracket));
  Assert.AreEqual(Integer(coAttachToNext), Integer(lOptions.CommentOwnership));
  Assert.AreEqual(3, Length(lOptions.CommentPrefixes));
  Assert.AreEqual(';', lOptions.CommentPrefixes[0]);
  Assert.AreEqual('#', lOptions.CommentPrefixes[1]);
  Assert.AreEqual('//', lOptions.CommentPrefixes[2]);
  Assert.AreEqual('=', lOptions.KeyValueDelimiter);
end;

procedure TRichIniFilesTests.LoadFromFile_ParsesBasicDocument;
var
  lTempFile: string;
  lOptions: TRichIniOptions;
  lFile: TRichIniFile;
  lContent: string;
begin
  lTempFile := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
  lContent :=
    '; top comment' + sLineBreak +
    '[Main]' + sLineBreak +
    'Key=Value';
  TFile.WriteAllText(lTempFile, lContent, TEncoding.UTF8);
  lOptions := DefaultRichIniOptions;
  lFile := TRichIniFile.Create('', lOptions);
  try
    lFile.LoadFromFile(lTempFile);
    Assert.AreEqual(lTempFile, lFile.FileName);
    Assert.AreEqual(3, lFile.LineCount);
    Assert.AreEqual(2, lFile.SectionBlockCount); // global section + [Main]
    Assert.IsFalse(lFile.Dirty);
  finally
    lFile.Free;
    TFile.Delete(lTempFile);
  end;
end;

procedure TRichIniFilesTests.ReadString_LastWinsAcrossDuplicates;
const
  cContent =
    'RootKey=42' + sLineBreak +
    '[Main]' + sLineBreak +
    'Value=1' + sLineBreak +
    'Value=2' + sLineBreak +
    '[Main]' + sLineBreak +
    'Value=3';
var
  lIni: TRichIniFile;
  lOptions: TRichIniOptions;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  try
    Assert.AreEqual(6, lIni.LineCount);
    Assert.AreEqual('42', lIni.ReadString('', 'RootKey', 'fallback'));
    Assert.AreEqual('3', lIni.ReadString('Main', 'Value', 'fallback'));
    Assert.AreEqual('fallback', lIni.ReadString('Main', 'Missing', 'fallback'));
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.ReadSection_CollapsesKeysWithOrderByLastOccurrence;
const
  cContent =
    '[Main]' + sLineBreak +
    'Alpha=1' + sLineBreak +
    'Beta=2' + sLineBreak +
    'Alpha=3' + sLineBreak +
    '[Main]' + sLineBreak +
    'Gamma=4' + sLineBreak +
    'Beta=5';
var
  lIni: TRichIniFile;
  lSL: TStringList;
  lOptions: TRichIniOptions;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  lSL := TStringList.Create;
  try
    lIni.ReadSection('Main', lSL);
    Assert.AreEqual(3, lSL.Count, 'Actual: ' + lSL.CommaText);
    Assert.AreEqual('Alpha', lSL[0]);
    Assert.AreEqual('Gamma', lSL[1]);
    Assert.AreEqual('Beta', lSL[2]);
  finally
    lSL.Free;
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.ReadSectionValues_ProvidesCollapsedPairs;
const
  cContent =
    'Root=global' + sLineBreak +
    '[Main]' + sLineBreak +
    'A=1' + sLineBreak +
    'B=2' + sLineBreak +
    'A=3' + sLineBreak +
    '[Main]' + sLineBreak +
    'C=4';
var
  lIni: TRichIniFile;
  lSL: TStringList;
  lOptions: TRichIniOptions;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  lSL := TStringList.Create;
  try
    lIni.ReadSectionValues('Main', lSL);
    Assert.AreEqual(3, lSL.Count, 'Actual: ' + lSL.CommaText);
    Assert.AreEqual('B=2', lSL[0]);
    Assert.AreEqual('A=3', lSL[1]);
    Assert.AreEqual('C=4', lSL[2]);
  finally
    lSL.Free;
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.ReadSections_FirstAppearanceOrder;
const
  cContent =
    '[Main]' + sLineBreak +
    'A=1' + sLineBreak +
    '[Other]' + sLineBreak +
    'B=2' + sLineBreak +
    '[Main]' + sLineBreak +
    'C=3' + sLineBreak +
    '[Settings]' + sLineBreak +
    'D=4';
var
  lIni: TRichIniFile;
  lSL: TStringList;
  lOptions: TRichIniOptions;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  lSL := TStringList.Create;
  try
    lIni.ReadSections(lSL);
    Assert.AreEqual(3, lSL.Count, 'Actual: ' + lSL.CommaText);
    Assert.AreEqual('Main', lSL[0]);
    Assert.AreEqual('Other', lSL[1]);
    Assert.AreEqual('Settings', lSL[2]);
  finally
    lSL.Free;
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.ReadAllKeyValues_ReturnsAllOccurrences;
const
  cContent =
    '[Main]' + sLineBreak +
    'Value=1' + sLineBreak +
    'Value=2' + sLineBreak +
    '[Main]' + sLineBreak +
    'Value=3';
var
  lIni: TRichIniFile;
  lOptions: TRichIniOptions;
  lValues: TArray<string>;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  try
    lIni.ReadAllKeyValues('Main', 'Value', lValues);
    Assert.AreEqual(3, Length(lValues));
    Assert.AreEqual('1', lValues[0]);
    Assert.AreEqual('2', lValues[1]);
    Assert.AreEqual('3', lValues[2]);
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.KeyCount_AndLastKeyIndex_TrackDuplicates;
const
  cContent =
    '[Main]' + sLineBreak +
    'Value=1' + sLineBreak +
    'Value=2' + sLineBreak +
    '[Main]' + sLineBreak +
    'Value=3';
var
  lIni: TRichIniFile;
  lOptions: TRichIniOptions;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  try
    Assert.AreEqual(3, lIni.SectionBlockCount);
    Assert.AreEqual(3, lIni.KeyCount('Main', 'Value'));
    Assert.AreEqual(2, lIni.LastKeyIndex('Main', 'Value'));
    Assert.AreEqual(-1, lIni.LastKeyIndex('Main', 'Missing'));
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.CaseInsensitive_ReadsHonorNormalization;
const
  cContent =
    '[Mixed]' + sLineBreak +
    'NaMe=Value';
var
  lIni: TRichIniFile;
  lOptions: TRichIniOptions;
  lValues: TArray<string>;
begin
  lOptions := DefaultRichIniOptions;
  lOptions.CaseSensitivity := csCaseInsensitive;
  lIni := LoadIniFromText(cContent, lOptions);
  try
    Assert.AreEqual('Value', lIni.ReadString('mixed', 'name', 'fallback'));
    Assert.AreEqual(1, lIni.KeyCount('MIXED', 'NAME'));
    lIni.ReadAllKeyValues('mixed', 'NAME', lValues);
    Assert.AreEqual(1, Length(lValues));
    Assert.AreEqual('Value', lValues[0]);
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.WriteString_EditLastOccurrence_Canonicalizes;
const
  cContent =
    '[Main]' + sLineBreak +
    'Value = 1' + sLineBreak +
    'Value = 2';
var
  lIni: TRichIniFile;
  lOptions: TRichIniOptions;
  lValues: TArray<string>;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  try
    lIni.WriteString('Main', 'Value', '42');
    Assert.IsTrue(lIni.Dirty);
    lIni.ReadAllKeyValues('Main', 'Value', lValues);
    Assert.AreEqual(2, Length(lValues));
    Assert.AreEqual('1', lValues[0]);
    Assert.AreEqual('42', lValues[1]);
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.WriteString_AppendsAndCreatesSection;
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lSections: TStringList;
begin
  lOptions := DefaultRichIniOptions;
  lIni := TRichIniFile.Create('', lOptions);
  try
    lIni.WriteString('Main', 'Alpha', '1');
    Assert.AreEqual('1', lIni.ReadString('Main', 'Alpha', 'fallback'));
    Assert.AreEqual(2, lIni.SectionBlockCount);

    lIni.WriteString('Main', 'Beta', '2');
    lIni.WriteString('Secondary', 'Gamma', '3');

    lSections := TStringList.Create;
    try
      lIni.ReadSections(lSections);
      Assert.AreEqual(2, lSections.Count);
      Assert.AreEqual('Main', lSections[0]);
      Assert.AreEqual('Secondary', lSections[1]);
    finally
      lSections.Free;
    end;
    Assert.IsTrue(lIni.Dirty);
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.WriteStringIndexed_EditsSpecificOccurrence;
const
  cContent =
    '[Main]' + sLineBreak +
    'Value=1' + sLineBreak +
    'Value=2' + sLineBreak +
    'Value=3';
var
  lIni: TRichIniFile;
  lOptions: TRichIniOptions;
  lValues: TArray<string>;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  try
    lIni.WriteString('Main', 'Value', '88', 1);
    lIni.ReadAllKeyValues('Main', 'Value', lValues);
    Assert.AreEqual(3, Length(lValues));
    Assert.AreEqual('1', lValues[0]);
    Assert.AreEqual('88', lValues[1]);
    Assert.AreEqual('3', lValues[2]);
    Assert.IsTrue(lIni.Dirty);
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.AppendKey_ReturnsExpectedOccurrenceIndex;
const
  cContent =
    '[Main]' + sLineBreak +
    'Value=1' + sLineBreak +
    'Value=2' + sLineBreak +
    'Value=3';
var
  lIni: TRichIniFile;
  lOptions: TRichIniOptions;
  lValues: TArray<string>;
  lIndex: Integer;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  try
    lIndex := lIni.AppendKey('Main', 'Value', '4');
    Assert.AreEqual(3, lIndex);
    lIni.ReadAllKeyValues('Main', 'Value', lValues);
    Assert.AreEqual(4, Length(lValues));
    Assert.AreEqual('4', lValues[3]);
    Assert.AreEqual(4, lIni.KeyCount('Main', 'Value'));
    Assert.AreEqual(3, lIni.LastKeyIndex('Main', 'Value'));
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.DeleteKey_RemovesAllOccurrencesAndOwnedComments;
const
  cContent =
    '[Main]' + sLineBreak +
    '; owned comment' + sLineBreak +
    'Value=1' + sLineBreak +
    'Value=2';
var
  lIni: TRichIniFile;
  lOptions: TRichIniOptions;
  lSections: TStringList;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  lSections := TStringList.Create;
  try
    Assert.AreEqual(4, lIni.LineCount);
    lIni.DeleteKey('Main', 'Value');
    Assert.AreEqual(1, lIni.LineCount);
    Assert.AreEqual(0, lIni.KeyCount('Main', 'Value'));
    lIni.ReadSection('Main', lSections);
    Assert.AreEqual(0, lSections.Count);
    Assert.IsTrue(lIni.Dirty);
  finally
    lSections.Free;
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.DeleteKeyIndexed_RemovesOnlyRequestedOccurrence;
const
  cContent =
    '[Main]' + sLineBreak +
    'Value=1' + sLineBreak +
    'Value=2' + sLineBreak +
    'Value=3';
var
  lIni: TRichIniFile;
  lOptions: TRichIniOptions;
  lValues: TArray<string>;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  try
    lIni.DeleteKey('Main', 'Value', 1);
    lIni.ReadAllKeyValues('Main', 'Value', lValues);
    Assert.AreEqual(2, Length(lValues));
    Assert.AreEqual('1', lValues[0]);
    Assert.AreEqual('3', lValues[1]);
    Assert.AreEqual(2, lIni.KeyCount('Main', 'Value'));
    Assert.AreEqual(1, lIni.LastKeyIndex('Main', 'Value'));
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.EraseSection_RemovesAllOccurrences;
const
  cContent =
    '[Main]' + sLineBreak +
    'Value=1' + sLineBreak +
    '[Other]' + sLineBreak +
    'Foo=Bar' + sLineBreak +
    '[Main]' + sLineBreak +
    'Value=2';
var
  lIni: TRichIniFile;
  lOptions: TRichIniOptions;
  lSections: TStringList;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  lSections := TStringList.Create;
  try
    lIni.EraseSection('Main');
    lIni.ReadSections(lSections);
    Assert.AreEqual(1, lSections.Count);
    Assert.AreEqual('Other', lSections[0]);
    Assert.AreEqual('fallback', lIni.ReadString('Main', 'Value', 'fallback'));
    Assert.IsTrue(lIni.Dirty);
    Assert.AreEqual(2, lIni.SectionBlockCount);
  finally
    lSections.Free;
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.WriteIntegerAndBool_RoundTrip;
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
begin
  lOptions := DefaultRichIniOptions;
  lIni := TRichIniFile.Create('', lOptions);
  try
    lIni.WriteInteger('Main', 'Number', 123);
    lIni.WriteBool('Main', 'Enabled', True);
    Assert.AreEqual(123, lIni.ReadInteger('Main', 'Number', 0));
    Assert.IsTrue(lIni.ReadBool('Main', 'Enabled', False));
    Assert.AreEqual(2, lIni.KeyCount('Main', 'Number') + lIni.KeyCount('Main', 'Enabled'));
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.ReadComment_ReturnsOwnedComments;
const
  cContent =
    '[Main]' + sLineBreak +
    '; first' + sLineBreak +
    '; second' + sLineBreak +
    'Value=42';
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  try
    Assert.AreEqual('first' + sLineBreak + 'second', lIni.ReadComment('Main', 'Value'));
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.WriteComment_ReplacesExistingOwnedBlock;
const
  cContent =
    '[Main]' + sLineBreak +
    '; old comment' + sLineBreak +
    'Value=1';
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  try
    lIni.WriteComment('Main', 'Value', 'updated' + sLineBreak + 'block');
    Assert.AreEqual('updated' + sLineBreak + 'block', lIni.ReadComment('Main', 'Value'));
    Assert.AreEqual(4, lIni.LineCount);
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.PurgeComments_RemovesAllLines;
const
  cContent =
    '[Main]' + sLineBreak +
    '; removable' + sLineBreak +
    'Value=1';
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  try
    lIni.PurgeComments;
    Assert.AreEqual('', lIni.ReadComment('Main', 'Value'));
    Assert.AreEqual(2, lIni.LineCount);
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.WriteMultilineString_EncodesValue;
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lValue: string;
begin
  lOptions := DefaultRichIniOptions;
  lIni := TRichIniFile.Create('', lOptions);
  try
    lValue := 'Hello' + sLineBreak + 'World';
    lIni.WriteMultilineString('Main', 'Greeting', lValue);
    Assert.AreEqual('Hello\nWorld', lIni.ReadString('Main', 'Greeting', ''));
    Assert.AreEqual(lValue, lIni.ReadMultilineString('Main', 'Greeting', 'fallback'));
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.ReadMultilineString_DecodesStoredValue;
const
  cContent =
    '[Main]' + sLineBreak +
    'Value=Line1\nLine2\\nLiteral';
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lExpected: string;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  try
    lExpected := 'Line1' + sLineBreak + 'Line2\nLiteral';
    Assert.AreEqual('Line1\nLine2\\nLiteral', lIni.ReadString('Main', 'Value', ''));
    Assert.AreEqual(lExpected, lIni.ReadMultilineString('Main', 'Value', 'fallback'));
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.ConsolidateSection_MergesIntoFirstBlock;
const
  cContent =
    '[Main]' + sLineBreak +
    '; original' + sLineBreak +
    'Alpha=1' + sLineBreak +
    'Beta=5' + sLineBreak +
    '[Main]' + sLineBreak +
    '; last comment' + sLineBreak +
    'Alpha=2' + sLineBreak +
    'Beta=3';
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lValues: TStringList;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  lValues := TStringList.Create;
  try
    lIni.ConsolidateSection('Main');
    lIni.ReadSectionValues('Main', lValues);
    Assert.AreEqual(2, lValues.Count);
    Assert.AreEqual('Alpha=2', lValues[0]);
    Assert.AreEqual('Beta=3', lValues[1]);
    Assert.AreEqual('last comment', lIni.ReadComment('Main', 'Alpha'));
    Assert.AreEqual(1, lIni.KeyCount('Main', 'Alpha'));
    Assert.AreEqual(1, lIni.KeyCount('Main', 'Beta'));
  finally
    lValues.Free;
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.ConsolidateAll_MergesEverySection;
const
  cContent =
    '[Main]' + sLineBreak +
    'A=1' + sLineBreak +
    '[Main]' + sLineBreak +
    'A=2' + sLineBreak +
    '[Other]' + sLineBreak +
    'B=1' + sLineBreak +
    '[Other]' + sLineBreak +
    'B=2';
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lSections: TStringList;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  lSections := TStringList.Create;
  try
    lIni.ConsolidateAll;
    Assert.AreEqual('2', lIni.ReadString('Main', 'A', ''));
    Assert.AreEqual(1, lIni.KeyCount('Main', 'A'));
    Assert.AreEqual('2', lIni.ReadString('Other', 'B', ''));
    Assert.AreEqual(1, lIni.KeyCount('Other', 'B'));
    lIni.ReadSections(lSections);
    Assert.AreEqual(2, lSections.Count);
  finally
    lSections.Free;
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.SaveToFile_RoundTripPreservesRawContent;
const
  cContent =
    '; leading comment' + sLineBreak +
    '[Main]' + sLineBreak +
    'Alpha = 1' + sLineBreak +
    '' + sLineBreak +
    'Beta= 2';
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lSourceFile: string;
  lCopyFile: string;
  lOriginal: TBytes;
  lSaved: TBytes;
  i: Integer;
begin
  lSourceFile := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
  lCopyFile := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
  TFile.WriteAllText(lSourceFile, cContent, TEncoding.UTF8);
  lOptions := DefaultRichIniOptions;
  lIni := TRichIniFile.Create('', lOptions);
  try
    lIni.LoadFromFile(lSourceFile);
    lIni.SaveToFile(lCopyFile);
    lOriginal := TFile.ReadAllBytes(lSourceFile);
    lSaved := TFile.ReadAllBytes(lCopyFile);
    Assert.AreEqual(Length(lOriginal), Length(lSaved));
    for i := 0 to Length(lOriginal) - 1 do
      Assert.AreEqual(Integer(lOriginal[i]), Integer(lSaved[i]), Format('Byte mismatch at %d', [i]));
  finally
    lIni.Free;
    if TFile.Exists(lSourceFile) then
      TFile.Delete(lSourceFile);
    if TFile.Exists(lCopyFile) then
      TFile.Delete(lCopyFile);
  end;
end;

procedure TRichIniFilesTests.SaveToFile_CanonicalizesModifiedLines;
const
  cContent =
    '[Main]' + sLineBreak +
    'Alpha = 1';
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lTempFile: string;
  lText: string;
begin
  lTempFile := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  try
    lIni.WriteString('Main', 'Alpha', '42');
    lIni.SaveToFile(lTempFile);
    lText := TFile.ReadAllText(lTempFile, TEncoding.UTF8);
    Assert.AreEqual('[Main]' + sLineBreak + 'Alpha=42', lText);
  finally
    lIni.Free;
    if TFile.Exists(lTempFile) then
      TFile.Delete(lTempFile);
  end;
end;

procedure TRichIniFilesTests.SaveToFile_RespectsEncodingAndNewlineOptions;
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lTempFile: string;
  lBytes: TBytes;
  lText: string;
begin
  lOptions := DefaultRichIniOptions;
  lOptions.SaveEncoding := eoUTF8;
  lOptions.BomPolicy := bpForce;
  lOptions.NewlineMode := nlUnixLF;
  lIni := TRichIniFile.Create('', lOptions);
  lTempFile := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
  try
    lIni.WriteString('Main', 'Line', '1');
    lIni.WriteString('Main', 'Next', '2');
    lIni.SaveToFile(lTempFile);
    lBytes := TFile.ReadAllBytes(lTempFile);
    Assert.IsTrue((Length(lBytes) >= 3) and (lBytes[0] = $EF) and (lBytes[1] = $BB) and (lBytes[2] = $BF));
    lText := TEncoding.UTF8.GetString(lBytes, 3, Length(lBytes) - 3);
    Assert.AreEqual(0, Pos(#13, lText));
    Assert.AreEqual('[Main]'#10'Line=1'#10'Next=2', lText);
  finally
    lIni.Free;
    if TFile.Exists(lTempFile) then
      TFile.Delete(lTempFile);
  end;
end;

procedure TRichIniFilesTests.SaveToFile_ResetsDirtyAndState;
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lTempFile: string;
begin
  lOptions := DefaultRichIniOptions;
  lIni := TRichIniFile.Create('', lOptions);
  lTempFile := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
  try
    lIni.WriteString('Main', 'Alpha', '1');
    Assert.IsTrue(lIni.Dirty);
    lIni.SaveToFile(lTempFile);
    Assert.IsFalse(lIni.Dirty);
    Assert.AreEqual(lTempFile, lIni.FileName);
    lIni.WriteString('Main', 'Beta', '2');
    lIni.SaveToFile;
    Assert.AreEqual('2', lIni.ReadString('Main', 'Beta', 'missing'));
  finally
    lIni.Free;
    if TFile.Exists(lTempFile) then
      TFile.Delete(lTempFile);
  end;
end;

procedure TRichIniFilesTests.CommentOwnership_AttachToPrev_OwnsAboveKey;
const
  cContent =
    '[Main]' + sLineBreak +
    'Key=Value';
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lTempFile: string;
  lText: string;
begin
  lOptions := DefaultRichIniOptions;
  lOptions.CommentOwnership := coAttachToPrev;
  lIni := LoadIniFromText(cContent, lOptions);
  try
    lIni.WriteComment('Main', 'Key', 'line1' + sLineBreak + 'line2');
    Assert.AreEqual('line1' + sLineBreak + 'line2', lIni.ReadComment('Main', 'Key'));
    lTempFile := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
    try
      lIni.SaveToFile(lTempFile);
      lText := TFile.ReadAllText(lTempFile, TEncoding.UTF8);
      Assert.AreEqual('[Main]' + sLineBreak + 'Key=Value' + sLineBreak + '; line1' + sLineBreak + '; line2', lText);
    finally
      if TFile.Exists(lTempFile) then
        TFile.Delete(lTempFile);
    end;
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.CommentOwnership_None_LeavesOrphans;
const
  cContent =
    '; orphan comment' + sLineBreak +
    '[Main]' + sLineBreak +
    'Key=Value' + sLineBreak +
    '; trailing orphan';
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lTempFile: string;
  lText: string;
begin
  lOptions := DefaultRichIniOptions;
  lOptions.CommentOwnership := coNone;
  lIni := LoadIniFromText(cContent, lOptions);
  try
    Assert.AreEqual('', lIni.ReadComment('Main', 'Key'));
    lIni.WriteComment('Main', 'Key', 'new');
    Assert.AreEqual('new', lIni.ReadComment('Main', 'Key'));
    lTempFile := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
    try
      lIni.SaveToFile(lTempFile);
      lText := TFile.ReadAllText(lTempFile, TEncoding.UTF8);
    Assert.AreEqual('; orphan comment' + sLineBreak + '[Main]' + sLineBreak + '; new' + sLineBreak + 'Key=Value' + sLineBreak + '; trailing orphan',
        lText);
    finally
      if TFile.Exists(lTempFile) then
        TFile.Delete(lTempFile);
    end;
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.CreateFromStrings_ParsesInputAndMaintainsState;
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
begin
  lOptions := DefaultRichIniOptions;
  lIni := TRichIniFile.CreateFromStrings(TArray<string>.Create('Alpha=1', '', '[Main]', 'Key=Value'), lOptions);
  try
    Assert.IsFalse(lIni.Dirty);
    Assert.AreEqual(4, lIni.LineCount);
    Assert.AreEqual(2, lIni.SectionBlockCount);
    Assert.AreEqual('1', lIni.ReadString('', 'Alpha', 'fallback'));
    Assert.AreEqual('Value', lIni.ReadString('Main', 'Key', 'fallback'));
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.SaveToFile_AnsiEncoding_NoBom;
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lTempFile: string;
  lBytes: TBytes;
  lReload: TRichIniFile;
  lReloadOptions: TRichIniOptions;
begin
  lOptions := DefaultRichIniOptions;
  lOptions.SaveEncoding := eoAnsi;
  lOptions.BomPolicy := bpForce;
  lIni := TRichIniFile.Create('', lOptions);
  lTempFile := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
  try
    lIni.WriteString('Main', 'Value', 'Ä');
    lIni.SaveToFile(lTempFile);
    lBytes := TFile.ReadAllBytes(lTempFile);
    Assert.IsTrue((Length(lBytes) > 0) and (not ((Length(lBytes) >= 3) and (lBytes[0] = $EF) and (lBytes[1] = $BB) and (lBytes[2] = $BF))));
    lReloadOptions := DefaultRichIniOptions;
    lReload := TRichIniFile.Create('', lReloadOptions);
    try
      lReload.LoadFromFile(lTempFile);
      Assert.AreEqual('Ä', lReload.ReadString('Main', 'Value', 'fallback'));
    finally
      lReload.Free;
    end;
  finally
    lIni.Free;
    if TFile.Exists(lTempFile) then
      TFile.Delete(lTempFile);
  end;
end;

procedure TRichIniFilesTests.SaveToFile_CustomEncodingWithBom;
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lTempFile: string;
  lBytes: TBytes;
begin
  lOptions := DefaultRichIniOptions;
  lOptions.SaveEncoding := eoCustom;
  lOptions.CustomEncoding := TEncoding.Unicode;
  lOptions.BomPolicy := bpForce;
  lIni := TRichIniFile.Create('', lOptions);
  lTempFile := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
  try
    lIni.WriteString('Main', 'Value', 'Text');
    lIni.SaveToFile(lTempFile);
    lBytes := TFile.ReadAllBytes(lTempFile);
    Assert.IsTrue((Length(lBytes) >= 2) and (lBytes[0] = $FF) and (lBytes[1] = $FE));
  finally
    lIni.Free;
    if TFile.Exists(lTempFile) then
      TFile.Delete(lTempFile);
  end;
end;

procedure TRichIniFilesTests.SaveToFile_BpAsSourceWithoutSource_WritesNoBom;
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lTempFile: string;
  lBytes: TBytes;
begin
  lOptions := DefaultRichIniOptions;
  lOptions.BomPolicy := bpAsSource;
  lIni := TRichIniFile.Create('', lOptions);
  lTempFile := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
  try
    lIni.WriteString('Main', 'Value', '1');
    lIni.SaveToFile(lTempFile);
    lBytes := TFile.ReadAllBytes(lTempFile);
    Assert.IsTrue((Length(lBytes) > 0) and not ((Length(lBytes) >= 3) and (lBytes[0] = $EF) and (lBytes[1] = $BB) and (lBytes[2] = $BF)));
  finally
    lIni.Free;
    if TFile.Exists(lTempFile) then
      TFile.Delete(lTempFile);
  end;
end;

procedure TRichIniFilesTests.SaveToFile_RespectsExplicitNewlineOnFreshDocument;
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lTempFile: string;
  lText: string;
begin
  lOptions := DefaultRichIniOptions;
  lOptions.NewlineMode := nlWindowsCRLF;
  lIni := TRichIniFile.Create('', lOptions);
  lTempFile := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
  try
    lIni.WriteString('Main', 'A', '1');
    lIni.WriteString('Main', 'B', '2');
    lIni.SaveToFile(lTempFile);
    lText := TFile.ReadAllText(lTempFile, TEncoding.UTF8);
    Assert.AreEqual('[Main]' + #13#10 + 'A=1' + #13#10 + 'B=2', lText);
  finally
    lIni.Free;
    if TFile.Exists(lTempFile) then
      TFile.Delete(lTempFile);
  end;
end;

procedure TRichIniFilesTests.ConsolidateSection_GlobalSectionCollapsesDuplicates;
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lValues: TArray<string>;
begin
  lOptions := DefaultRichIniOptions;
  lIni := TRichIniFile.Create('', lOptions);
  try
    lIni.WriteString('', 'Alpha', '1');
    lIni.AppendKey('', 'Alpha', '2');
    lIni.AppendKey('', 'Beta', '3');
    lIni.AppendKey('', 'Beta', '4');
    lIni.ConsolidateSection('');
    Assert.AreEqual(1, lIni.KeyCount('', 'Alpha'));
    Assert.AreEqual('2', lIni.ReadString('', 'Alpha', 'fallback'));
    lIni.ReadAllKeyValues('', 'Beta', lValues);
    Assert.AreEqual(1, Length(lValues));
    Assert.AreEqual('4', lValues[0]);
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.SaveToFile_BpAsSourcePreservesLoadedBom;
var
  lFile: string;
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lBytes: TBytes;
begin
  lFile := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
  TFile.WriteAllText(lFile, '[Main]' + sLineBreak + 'Key=1', TEncoding.UTF8);
  lOptions := DefaultRichIniOptions;
  lIni := TRichIniFile.Create('', lOptions);
  try
    lIni.LoadFromFile(lFile);
    lIni.WriteString('Main', 'Key', '2');
    lIni.SaveToFile;
    lBytes := TFile.ReadAllBytes(lFile);
    Assert.IsTrue((Length(lBytes) >= 3) and (lBytes[0] = $EF) and (lBytes[1] = $BB) and (lBytes[2] = $BF));
  finally
    lIni.Free;
    if TFile.Exists(lFile) then
      TFile.Delete(lFile);
  end;
end;

procedure TRichIniFilesTests.SaveToFile_PreservesInputNewlinesAfterEdit;
var
  lFile: string;
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lText: string;
begin
  lFile := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
  TFile.WriteAllText(lFile, '[Main]' + #10 + 'Key=1', TEncoding.UTF8);
  lOptions := DefaultRichIniOptions;
  lIni := TRichIniFile.Create('', lOptions);
  try
    lIni.LoadFromFile(lFile);
    lIni.WriteString('Main', 'Key', '2');
    lIni.SaveToFile;
    lText := TFile.ReadAllText(lFile, TEncoding.UTF8);
    lText := StringReplace(lText, #13, '', [rfReplaceAll]);
    var lParts := lText.Split([#10]);
    var lFiltered := TList<string>.Create;
    try
      for var item in lParts do
        if item <> '' then
          lFiltered.Add(item);
      Assert.AreEqual(2, lFiltered.Count, 'Parts=' + String.Join(',', lFiltered.ToArray));
      Assert.AreEqual('[Main]', lFiltered[0], 'Parts=' + String.Join(',', lFiltered.ToArray));
      Assert.AreEqual('Key=2', lFiltered[1], 'Parts=' + String.Join(',', lFiltered.ToArray));
    finally
      lFiltered.Free;
    end;
  finally
    lIni.Free;
    if TFile.Exists(lFile) then
      TFile.Delete(lFile);
  end;
end;

procedure TRichIniFilesTests.DeleteKey_AttachToPrev_RemovesOwnedComment;
const
  cContent =
    '[Main]' + sLineBreak +
    '; owned comment' + sLineBreak +
    'Key=1';
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
begin
  lOptions := DefaultRichIniOptions;
  lIni := LoadIniFromText(cContent, lOptions);
  try
    lIni.DeleteKey('Main', 'Key');
    Assert.AreEqual(0, lIni.KeyCount('Main', 'Key'));
    Assert.AreEqual('', lIni.ReadComment('Main', 'Key'));
    Assert.AreEqual(1, lIni.LineCount); // section header + blank comment removed
  finally
    lIni.Free;
  end;
end;

procedure TRichIniFilesTests.WriteComment_UsesFirstConfiguredPrefix;
var
  lOptions: TRichIniOptions;
  lIni: TRichIniFile;
  lTempFile: string;
  lText: string;
begin
  lOptions := DefaultRichIniOptions;
  lOptions.CommentPrefixes := TArray<string>.Create('#', '//');
  lIni := TRichIniFile.Create('', lOptions);
  lTempFile := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
  try
    lIni.WriteString('Main', 'Key', '1');
    lIni.WriteComment('Main', 'Key', 'test');
    lIni.SaveToFile(lTempFile);
    lText := TFile.ReadAllText(lTempFile, TEncoding.UTF8);
    Assert.IsTrue(Pos('# test', lText) > 0);
  finally
    lIni.Free;
    if TFile.Exists(lTempFile) then
      TFile.Delete(lTempFile);
  end;
end;

end.
