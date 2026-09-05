unit MaxLogic.RichIniFile.Utf8.Tests;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TRichIniUtf8Tests = class
  public
    [Test] procedure Utf8Reads_DoNotAllocate;
    [Test] procedure Utf8Workload_Performance;
    [Test] procedure ScalarReads_ParityAndAllocations;
    [Test] procedure DocumentOperations_Parity;
    [Test] procedure Persistence_EncodingPolicies;
    [Test] procedure Syntax_UnicodeDelimiterAndComments;
    [Test] procedure Multiline_BytesAndEscapes;
    [Test] procedure Defaults_ErrorsAndRepeatedLoad;
    [Test] procedure CaseInsensitive_NonAsciiAndInvalidBytes;
    [Test] procedure CustomEncoding_HonorsDecoder;
  end;

implementation

uses
  System.Classes, System.Diagnostics, System.Generics.Collections, System.IOUtils, System.SysUtils, AutoFree, MaxLogic.RichIniFile, MaxLogic.RichIniFile.Utf8;

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

procedure TRichIniUtf8Tests.Utf8Reads_DoNotAllocate;
var
  g: TGarbos;
  lFile: TRichIniFileUtf8;
  lSection, lKey, lDefault, lValue, lResult: UTF8String;
  lCount: NativeInt;
begin
  lSection := UTF8String('Grüße 東京');
  lKey := UTF8String('Schlüssel😀');
  lDefault := UTF8String('missing');
  lValue := UTF8String('Wert 東京😀');
  GC(lFile, TRichIniFileUtf8.Create('', MaxLogic.RichIniFile.Utf8.DefaultRichIniOptions), g);
  lFile.WriteString(lSection, lKey, lValue);
  lResult := UTF8String(lFile.ReadString(lSection, lKey, lDefault));
  lCount := CountUtf8Allocations(
    procedure
    var
      i: Integer;
    begin
      for i := 1 to 100 do
        lResult := UTF8String(lFile.ReadString(lSection, lKey, lDefault));
    end);
  Assert.AreEqual(string(lValue), string(lResult));
  Assert.AreEqual(NativeInt(0), lCount, 'Native UTF8 reads must allocate nothing');
end;

procedure TRichIniUtf8Tests.Utf8Workload_Performance;
const
  cIterations = 50000;
var
  g: TGarbos;
  lFile: TRichIniFileUtf8;
  lOptions: TRichIniOptionsUtf8;
  lSection, lKey, lValue, lResult: UTF8String;
  lTimes: TArray<Int64>;
  lWatch: TStopwatch;
  i, j: Integer;
begin
  lSection := UTF8String('Grüße 東京');
  lKey := UTF8String('Schlüssel😀');
  lValue := UTF8String('Wert 東京😀 ' + StringOfChar('x', 256));
  lOptions := MaxLogic.RichIniFile.Utf8.DefaultRichIniOptions;
  lOptions.CaseSensitivity := csCaseInsensitive;
  GC(lFile, TRichIniFileUtf8.Create('', lOptions), g);
  lFile.WriteString(lSection, lKey, lValue);
  SetLength(lTimes, 9);
  for j := -1 to High(lTimes) do
  begin
    lWatch := TStopwatch.StartNew;
    for i := 1 to cIterations do
      lResult := UTF8String(lFile.ReadString(lSection, lKey, ''));
    lWatch.Stop;
    Assert.AreEqual(string(lValue), string(lResult));
    if j >= 0 then
      lTimes[j] := lWatch.ElapsedTicks;
  end;
  TArray.Sort<Int64>(lTimes);
  System.Writeln(Format('RichIni UTF8 caller: %d reads, us min=%.0f median=%.0f p95=%.0f max=%.0f',
    [cIterations, lTimes[0]*1E6/TStopwatch.Frequency, lTimes[4]*1E6/TStopwatch.Frequency,
     lTimes[8]*1E6/TStopwatch.Frequency, lTimes[8]*1E6/TStopwatch.Frequency]));
end;

procedure TRichIniUtf8Tests.ScalarReads_ParityAndAllocations;
const
  cDefault = -123;
var
  g: TGarbos;
  lFile: TRichIniFileUtf8;
  lText: string;
  lCases: TArray<string>;
  lCount: NativeInt;
  lResult: Integer;
  lBool: Boolean;
begin
  GC(lFile, TRichIniFileUtf8.Create('', MaxLogic.RichIniFile.Utf8.DefaultRichIniOptions), g);
  lCases := ['', '0', '-0', '+1', ' 12', #9'12', '12 ', '-', '+',
    '2147483647', '2147483648', '-2147483648', '-2147483649', '$FFFFFFFF',
    '-$FFFFFFFF', '$80000000', '-$80000000', '$100000000', '0x10', 'xFF', 'Xff',
    '0x', '$', '0Xabcdef', '00x10', '1.5', '東京', '12'#0'ignored', StringOfChar('0', 300)+'7'];
  for lText in lCases do
  begin
    lFile.WriteString('s', 'int', UTF8String(lText));
    Assert.AreEqual(StrToIntDef(lText, cDefault), lFile.ReadInteger('s', 'int', cDefault), lText);
  end;
  lFile.WriteString('s', 'int', '-2147483648');
  lFile.WriteString('s', 'bool', 'YES');
  lCount := CountUtf8Allocations(
    procedure
    begin
      lResult := lFile.ReadInteger('s', 'int', 123);
      lBool := lFile.ReadBool('s', 'bool', False);
    end);
  Assert.AreEqual(Low(Integer), lResult);
  Assert.IsTrue(lBool);
  Assert.AreEqual(NativeInt(0), lCount, 'Integer and boolean reads must avoid conversion allocations');
end;

procedure TRichIniUtf8Tests.DocumentOperations_Parity;
const
  cText = '; global'#13#10'g=0'#13#10'[Grüße 東京]'#13#10'; first'#13#10+
    'Schlüssel😀=eins'#13#10'other=zwei'#13#10'[Grüße 東京]'#13#10+
    '; last'#13#10'Schlüssel😀=drei'#13#10'OTHER=vier'#13#10'[empty]';
var
  g: TGarbos;
  lFile: TRichIniFileUtf8;
  lUnicode: MaxLogic.RichIniFile.TRichIniFile;
  lOptions: TRichIniOptionsUtf8;
  lUnicodeOptions: MaxLogic.RichIniFile.TRichIniOptions;
  lNativeList: TList<UTF8String>;
  lUnicodeList: TStringList;
  lValues: TArray<UTF8String>;
  lSection, lKey, lValue: UTF8String;
  lCase, lOwnership, i: Integer;
begin
  lSection := UTF8String('Grüße 東京');
  lKey := UTF8String('Schlüssel😀');
  GC(lNativeList, TList<UTF8String>.Create, g);
  GC(lUnicodeList, TStringList.Create, g);
  for lCase := 0 to 1 do
    for lOwnership := 0 to 2 do
    begin
      lOptions := MaxLogic.RichIniFile.Utf8.DefaultRichIniOptions;
      lUnicodeOptions := MaxLogic.RichIniFile.DefaultRichIniOptions;
      lOptions.CaseSensitivity := TCaseSensitivity(lCase);
      lUnicodeOptions.CaseSensitivity := MaxLogic.RichIniFile.TCaseSensitivity(lCase);
      lOptions.CommentOwnership := TCommentOwnership(lOwnership);
      lUnicodeOptions.CommentOwnership := MaxLogic.RichIniFile.TCommentOwnership(lOwnership);
      GC(lFile, TRichIniFileUtf8.Create('', lOptions), g);
      GC(lUnicode, MaxLogic.RichIniFile.TRichIniFile.Create('', lUnicodeOptions), g);
      lFile.LoadFromText(UTF8String(cText));
      lUnicode.LoadFromText(cText);
      Assert.AreEqual(2, lFile.KeyCount(lSection, lKey));
      Assert.AreEqual(1, lFile.LastKeyIndex(lSection, lKey));
      Assert.AreEqual('drei', string(lFile.ReadString(lSection, lKey, '')));
      lFile.ReadAllKeyValues(lSection, lKey, lValues);
      Assert.AreEqual(NativeInt(2), Length(lValues));
      Assert.AreEqual('eins', string(lValues[0]));
      Assert.AreEqual('drei', string(lValues[1]));
      lFile.WriteString(lSection, lKey, UTF8String('geändert😀'), 0);
      lUnicode.WriteString(string(lSection), string(lKey), 'geändert😀', 0);
      for i := 0 to 19 do
      begin
        lValue := UTF8String('v東京' + IntToStr(i));
        lFile.WriteString(lSection, lKey, lValue);
        lUnicode.WriteString(string(lSection), string(lKey), string(lValue));
        Assert.AreEqual(lUnicode.ReadString(string(lSection), string(lKey), ''), string(lFile.ReadString(lSection, lKey, '')));
      end;
      Assert.AreEqual(lUnicode.AppendKey(string(lSection), string(lKey), 'append'), lFile.AppendKey(lSection, lKey, 'append'));
      lFile.WriteComment(lSection, lKey, UTF8String('Kommentar😀'#13#10'東京'));
      lUnicode.WriteComment(string(lSection), string(lKey), 'Kommentar😀'#13#10'東京');
      Assert.AreEqual(lUnicode.ReadComment(string(lSection), string(lKey)), string(lFile.ReadComment(lSection, lKey)));
      lFile.DeleteKey(lSection, lKey, 0);
      lUnicode.DeleteKey(string(lSection), string(lKey), 0);
      lFile.ConsolidateAll;
      lUnicode.ConsolidateAll;
      Assert.AreEqual(1, lFile.KeyCount(lSection, lKey));
      Assert.AreEqual(lUnicode.LineCount, lFile.LineCount);
      lFile.ReadSection(lSection, lNativeList);
      lUnicode.ReadSection(string(lSection), lUnicodeList);
      Assert.AreEqual(NativeInt(lUnicodeList.Count), NativeInt(lNativeList.Count));
      for i := 0 to lNativeList.Count - 1 do
        Assert.AreEqual(lUnicodeList[i], string(lNativeList[i]));
      lFile.ReadSectionValues(lSection, lNativeList);
      lUnicode.ReadSectionValues(string(lSection), lUnicodeList);
      Assert.AreEqual(NativeInt(lUnicodeList.Count), NativeInt(lNativeList.Count));
      for i := 0 to lNativeList.Count - 1 do
        Assert.AreEqual(lUnicodeList[i], string(lNativeList[i]));
      lFile.ReadSections(lNativeList);
      lUnicode.ReadSections(lUnicodeList);
      Assert.AreEqual(NativeInt(lUnicodeList.Count), NativeInt(lNativeList.Count));
      for i := 0 to lNativeList.Count - 1 do
        Assert.AreEqual(lUnicodeList[i], string(lNativeList[i]));
      lFile.PurgeComments;
      lUnicode.PurgeComments;
      lFile.DeleteKey(lSection, lKey);
      lUnicode.DeleteKey(string(lSection), string(lKey));
      lFile.EraseSection(lSection);
      lUnicode.EraseSection(string(lSection));
      lFile.EraseSection('');
      lUnicode.EraseSection('');
      Assert.AreEqual(lUnicode.LineCount, lFile.LineCount);
      Assert.AreEqual(lUnicode.SectionBlockCount, lFile.SectionBlockCount);
      Assert.AreEqual(0, lFile.KeyCount(lSection, lKey));
    end;
end;

procedure TRichIniUtf8Tests.Persistence_EncodingPolicies;
const
  cText = '; Grüße'#10'[s]'#10'key=älter'#10;
var
  g: TGarbos;
  lFile, lReload: TRichIniFileUtf8;
  lUnicode: MaxLogic.RichIniFile.TRichIniFile;
  lOptions: TRichIniOptionsUtf8;
  lUnicodeOptions: MaxLogic.RichIniFile.TRichIniOptions;
  lNativePath, lUnicodePath: string;
  lActual, lExpected: TBytes;
  lEncoding, lBom, lNewline, i: Integer;
begin
  lNativePath := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.native.ini');
  lUnicodePath := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.unicode.ini');
  try
    for lEncoding := 0 to 3 do
      for lBom := 0 to 2 do
        for lNewline := 0 to 3 do
        begin
          lOptions := MaxLogic.RichIniFile.Utf8.DefaultRichIniOptions;
          lUnicodeOptions := MaxLogic.RichIniFile.DefaultRichIniOptions;
          lOptions.SaveEncoding := TEncodingMode(lEncoding);
          lUnicodeOptions.SaveEncoding := MaxLogic.RichIniFile.TEncodingMode(lEncoding);
          lOptions.CustomEncoding := TEncoding.Unicode;
          lUnicodeOptions.CustomEncoding := TEncoding.Unicode;
          lOptions.BomPolicy := TBomPolicy(lBom);
          lUnicodeOptions.BomPolicy := MaxLogic.RichIniFile.TBomPolicy(lBom);
          lOptions.NewlineMode := TNewlineMode(lNewline);
          lUnicodeOptions.NewlineMode := MaxLogic.RichIniFile.TNewlineMode(lNewline);
          GC(lFile, TRichIniFileUtf8.Create('', lOptions), g);
          GC(lUnicode, MaxLogic.RichIniFile.TRichIniFile.Create('', lUnicodeOptions), g);
          lFile.LoadFromText(UTF8String(cText), TEncoding.UTF8, True);
          lUnicode.LoadFromText(cText, TEncoding.UTF8, True);
          lFile.WriteString('s', 'key', UTF8String('Grüße'));
          lUnicode.WriteString('s', 'key', 'Grüße');
          lFile.SaveToFile(lNativePath);
          lUnicode.SaveToFile(lUnicodePath);
          lActual := TFile.ReadAllBytes(lNativePath);
          lExpected := TFile.ReadAllBytes(lUnicodePath);
          Assert.AreEqual(Length(lExpected), Length(lActual));
          for i := 0 to High(lActual) do
            Assert.AreEqual(lExpected[i], lActual[i]);
          Assert.IsFalse(lFile.Dirty);
          Assert.AreEqual(lNativePath, lFile.FileName);
          lOptions.LoadEncoding := lOptions.SaveEncoding;
          GC(lReload, TRichIniFileUtf8.Create('', lOptions), g);
          lReload.LoadFromFile(lNativePath);
          Assert.AreEqual('Grüße', string(lReload.ReadString('s', 'key', '')), Format('encoding=%d bom=%d newline=%d bytes=%s', [lEncoding, lBom, lNewline, string(UTF8String(lReload.ReadString('s', 'key', '')))]));
          lReload.WriteString('s', 'key', 'updated');
          lReload.UpdateFile;
          lReload.LoadFromFile(lNativePath);
          Assert.AreEqual('updated', string(lReload.ReadString('s', 'key', '')));
        end;
  finally
    if TFile.Exists(lNativePath) then
      TFile.Delete(lNativePath);
    if TFile.Exists(lUnicodePath) then
      TFile.Delete(lUnicodePath);
  end;
end;

procedure TRichIniUtf8Tests.Syntax_UnicodeDelimiterAndComments;
var
  g: TGarbos;
  lFile: TRichIniFileUtf8;
  lOptions: TRichIniOptionsUtf8;
begin
  lOptions := MaxLogic.RichIniFile.Utf8.DefaultRichIniOptions;
  lOptions.CommentPrefixes := [UTF8String('注意')];
  lOptions.KeyValueDelimiter := '§';
  lOptions.AcceptMissingBracket := TMissingBracketPolicy.mbAcceptAsSection;
  GC(lFile, TRichIniFileUtf8.Create('', lOptions), g);
  lFile.LoadFromText(UTF8String('[東京'#13#10'注意 コメント😀'#13#10'  clé § 値😀  '#13#10'plain'));
  Assert.AreEqual('値😀', string(lFile.ReadString(UTF8String('東京'), UTF8String('clé'), '')));
  Assert.AreEqual('コメント😀', string(lFile.ReadComment(UTF8String('東京'), UTF8String('clé'))));
  Assert.AreEqual('', string(lFile.ReadString(UTF8String('東京'), 'plain', 'missing')));
  lFile.WriteString(UTF8String('東京'), UTF8String('clé'), UTF8String('新しい'));
  Assert.AreEqual('新しい', string(lFile.ReadString(UTF8String('東京'), UTF8String('clé'), '')));
  lFile.LoadFromText(UTF8String('[s]'#10'key'#10'k§v'#10'空§'));
  Assert.AreEqual('v', string(lFile.ReadString('s', 'k', '')));
  Assert.AreEqual('', string(lFile.ReadString('s', UTF8String(string('空')), 'missing')));
end;

procedure TRichIniUtf8Tests.Multiline_BytesAndEscapes;
var
  g: TGarbos;
  lFile: TRichIniFileUtf8;
  lUnicode: MaxLogic.RichIniFile.TRichIniFile;
  lText: string;
  lValues: TArray<string>;
  lPath: string;
  lRaw, lLoaded: UTF8String;
  lBytes: TBytes;
  i: Integer;
begin
  GC(lFile, TRichIniFileUtf8.Create('', MaxLogic.RichIniFile.Utf8.DefaultRichIniOptions), g);
  GC(lUnicode, MaxLogic.RichIniFile.TRichIniFile.Create('', MaxLogic.RichIniFile.DefaultRichIniOptions), g);
  lValues := ['', '東京😀', 'a'#13#10'b'#10'c'#13'd', '\n\q\\end\', 'a'#0'東京', StringOfChar('x', 8192)];
  for lText in lValues do
  begin
    lFile.WriteMultilineString('s', 'k', UTF8String(lText));
    lUnicode.WriteMultilineString('s', 'k', lText);
    Assert.AreEqual(lUnicode.ReadString('s', 'k', ''), string(lFile.ReadString('s', 'k', '')));
    Assert.AreEqual(lUnicode.ReadMultilineString('s', 'k', ''), string(lFile.ReadMultilineString('s', 'k', '')));
    lFile.WriteString('s', 'k', UTF8String(lText));
    lUnicode.WriteString('s', 'k', lText);
    Assert.AreEqual(lUnicode.ReadMultilineString('s', 'k', ''), string(lFile.ReadMultilineString('s', 'k', '')));
  end;
  SetLength(lRaw, 128);
  for i := 1 to Length(lRaw) do
    lRaw[i] := AnsiChar(i + 127);
  lFile.LoadFromText(UTF8String('[s]'#10'k=') + lRaw);
  Assert.IsTrue(lRaw = lFile.ReadString('s', 'k', ''), 'raw value read length=' + IntToStr(Length(lFile.ReadString('s', 'k', ''))));
  lPath := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
  try
    lFile.SaveToFile(lPath);
    lBytes := TFile.ReadAllBytes(lPath);
    SetString(lLoaded, PAnsiChar(@lBytes[0]), Length(lBytes));
    Assert.IsTrue(lLoaded = UTF8String('[s]' + sLineBreak + 'k=') + lRaw, Format('saved raw: expected length=%d actual=%d leading=%s', [Length(UTF8String('[s]' + sLineBreak + 'k=') + lRaw), Length(lLoaded), string(Copy(lLoaded,1,10))]));
    lFile.LoadFromFile(lPath);
    Assert.IsTrue(lRaw = lFile.ReadString('s', 'k', ''), 'raw value read length=' + IntToStr(Length(lFile.ReadString('s', 'k', ''))));
  finally
    if TFile.Exists(lPath) then
      TFile.Delete(lPath);
  end;
end;

procedure TRichIniUtf8Tests.Defaults_ErrorsAndRepeatedLoad;
var
  g: TGarbos;
  lFile: TRichIniFileUtf8;
  lOptions: TRichIniOptionsUtf8;
  lValues: TArray<UTF8String>;
  lFailed: Boolean;
begin
  lOptions := MaxLogic.RichIniFile.Utf8.DefaultRichIniOptions;
  lOptions.CommentPrefixes := nil;
  lOptions.BooleanTrueValues := nil;
  lOptions.BooleanTrueValue := '';
  lOptions.BooleanFalseValue := '';
  lOptions.KeyValueDelimiter := #0;
  GC(lFile, TRichIniFileUtf8.CreateFromStrings([UTF8String('[s]'), UTF8String('k=v')], lOptions), g);
  Assert.AreEqual('v', string(lFile.ReadString('s', 'k', '')));
  Assert.AreEqual(NativeInt(3), Length(lFile.Options.CommentPrefixes));
  Assert.AreEqual(NativeInt(6), Length(lFile.Options.BooleanTrueValues));
  Assert.AreEqual('default', string(lFile.ReadString('missing', 'k', 'default')));
  Assert.AreEqual(42, lFile.ReadInteger('missing', 'k', 42));
  Assert.IsTrue(lFile.ReadBool('missing', 'k', True));
  lFile.ReadAllKeyValues('missing', 'k', lValues);
  Assert.AreEqual(NativeInt(0), Length(lValues));
  lFailed := False;
  try
    lFile.WriteString('s', 'k', 'v', 10);
  except
    on E: EArgumentOutOfRangeException do
      lFailed := True;
  end;
  Assert.IsTrue(lFailed);
  lFailed := False;
  try
    lFile.DeleteKey('s', 'k', 10);
  except
    on E: EArgumentOutOfRangeException do
      lFailed := True;
  end;
  Assert.IsTrue(lFailed);
  lFailed := False;
  try
    lFile.WriteComment('missing', 'k', 'comment');
  except
    on E: EArgumentException do
      lFailed := True;
  end;
  Assert.IsTrue(lFailed);
  lFailed := False;
  try
    lFile.SaveToFile;
  except
    on E: EInvalidOperation do
      lFailed := True;
  end;
  Assert.IsTrue(lFailed);
  lFile.LoadFromText('');
  Assert.AreEqual('missing', string(lFile.ReadString('s', 'k', 'missing')));
  Assert.IsFalse(lFile.Dirty);
  lFile.WriteInteger('s', 'int', Low(Integer));
  Assert.AreEqual(Low(Integer), lFile.ReadInteger('s', 'int', 0));
  lFile.WriteBool('s', 'bool', True);
  Assert.AreEqual('1', string(lFile.ReadString('s', 'bool', '')));
  lFile.WriteBool('s', 'bool', False);
  Assert.AreEqual('0', string(lFile.ReadString('s', 'bool', '')));
  lFile.LoadFromFile('');
  Assert.AreEqual('missing', string(lFile.ReadString('s', 'int', 'missing')));
end;

procedure TRichIniUtf8Tests.CaseInsensitive_NonAsciiAndInvalidBytes;
var
  g: TGarbos;
  lFile: TRichIniFileUtf8;
  lOptions: TRichIniOptionsUtf8;
  lKey: UTF8String;
  i: Integer;
begin
  lOptions := MaxLogic.RichIniFile.Utf8.DefaultRichIniOptions;
  lOptions.CaseSensitivity := TCaseSensitivity.csCaseInsensitive;
  lOptions.BooleanTrueValues := [UTF8String('ÄÖYES')];
  lOptions.BooleanTrueValue := UTF8String('äöyes');
  lOptions.BooleanFalseValue := 'nein';
  GC(lFile, TRichIniFileUtf8.Create('', lOptions), g);
  lFile.WriteString(UTF8String('Grüße'), UTF8String('ÖL😀'), UTF8String('äöyes'));
  Assert.IsTrue(lFile.ReadBool(UTF8String('GRÜßE'), UTF8String('öl😀'), False));
  lFile.WriteBool('s', 'b', True);
  Assert.IsTrue(lFile.ReadBool('s', 'b', False));
  lFile.WriteBool('s', 'b', False);
  Assert.AreEqual('nein', string(lFile.ReadString('s', 'b', '')));
  SetLength(lKey, 1);
  for i := 128 to 255 do
  begin
    lKey[1] := AnsiChar(i);
    lFile.WriteInteger('invalid', lKey, i);
  end;
  for i := 128 to 255 do
  begin
    lKey[1] := AnsiChar(i);
    Assert.AreEqual(i, lFile.ReadInteger('invalid', lKey, -1));
  end;
end;

procedure TRichIniUtf8Tests.CustomEncoding_HonorsDecoder;
var
  g: TGarbos;
  lEncoding: TUTF8Encoding;
  lFile: TRichIniFileUtf8;
  lOptions: TRichIniOptionsUtf8;
  lPath: string;
  lFailed: Boolean;
begin
  GC(lEncoding, TUTF8Encoding.Create(False), g);
  lOptions := MaxLogic.RichIniFile.Utf8.DefaultRichIniOptions;
  lOptions.LoadEncoding := TEncodingMode.eoCustom;
  lOptions.CustomEncoding := lEncoding;
  GC(lFile, TRichIniFileUtf8.Create('', lOptions), g);
  lPath := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.ini');
  try
    TFile.WriteAllBytes(lPath, [$C3]);
    lFailed := False;
    try
      lFile.LoadFromFile(lPath);
    except
      on E: EEncodingError do
        lFailed := True;
    end;
    Assert.IsTrue(lFailed, 'Our explicitly selected UTF8 decoder must reject incomplete input');
  finally
    if TFile.Exists(lPath) then
      TFile.Delete(lPath);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TRichIniUtf8Tests);
end.
