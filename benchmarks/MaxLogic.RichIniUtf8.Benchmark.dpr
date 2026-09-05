program MaxLogicRichIniUtf8Benchmark;

{$APPTYPE CONSOLE}

uses
  System.Classes, System.Diagnostics, System.Generics.Collections, System.IOUtils, System.SysUtils,
  AutoFree in '..\AutoFree.pas',
  MaxLogic.StrUtils in '..\MaxLogic.StrUtils.pas',
  MaxLogic.RichIniFile in '..\MaxLogic.RichIniFile.pas',
  MaxLogic.RichIniFile.Utf8 in '..\MaxLogic.RichIniFile.Utf8.pas';

type
  TOperation = (opRead, opWrite, opLoadText, opLoadFile, opSave);
  TDataset = record
    Text: UTF8String;
    Sections, Keys, Values: TArray<UTF8String>;
    InputPath, OutputPath: string;
  end;

function CreateDataset(aCount: Integer; aNonAscii: Boolean): TDataset;
var
  g: TGarbos;
  lBuilder: TStringBuilder;
  lSuffix, lSection, lKey, lValue: string;
  i: Integer;
begin
  Result := Default(TDataset);
  SetLength(Result.Sections, aCount);
  SetLength(Result.Keys, aCount);
  SetLength(Result.Values, aCount);
  if aNonAscii then
    lSuffix := 'Grüße東京😀'
  else
    lSuffix := 'text';
  GC(lBuilder, TStringBuilder.Create, g);
  for i := 0 to aCount - 1 do
  begin
    lSection := 'section' + IntToStr(i div 100) + lSuffix;
    lKey := 'key' + IntToStr(i) + lSuffix;
    lValue := 'value' + IntToStr(i) + lSuffix;
    if i mod 3 = 0 then
      lValue := lValue + StringOfChar('x', 256);
    if i mod 100 = 0 then
      lBuilder.Append('[' + lSection + ']' + #13#10);
    lBuilder.Append(lKey + '=' + lValue + #13#10);
    Result.Sections[i] := UTF8String(lSection);
    Result.Keys[i] := UTF8String(lKey);
    Result.Values[i] := UTF8String(lValue);
  end;
  Result.Text := UTF8String(lBuilder.ToString);
  Result.InputPath := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.input.ini');
  Result.OutputPath := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName + '.output.ini');
  TFile.WriteAllText(Result.InputPath, string(Result.Text), TEncoding.UTF8);
end;

function Measure(aNative: Boolean; aOperation: TOperation; aUnicode: MaxLogic.RichIniFile.TRichIniFile;
  aUtf8: TRichIniFileUtf8; const aData: TDataset): Int64;
var
  lWatch: TStopwatch;
  lResult, lExpected: UTF8String;
  lTotal: Int64;
  i, j, lCount: Integer;
begin
  lCount := Length(aData.Keys);
  lTotal := 0;
  lWatch := TStopwatch.StartNew;
  if aNative then
  begin
    case aOperation of
      opRead:
        for i := 0 to 49999 do
        begin
          j := i mod lCount;
          lResult := aUtf8.ReadString(aData.Sections[j], aData.Keys[j], '');
          Inc(lTotal, Length(lResult));
        end;
      opWrite:
        for i := 0 to 19999 do
        begin
          j := i mod lCount;
          aUtf8.WriteString(aData.Sections[j], aData.Keys[j], aData.Values[(i div lCount + i) mod 2]);
        end;
      opLoadText: aUtf8.LoadFromText(aData.Text);
      opLoadFile: aUtf8.LoadFromFile(aData.InputPath);
      opSave: aUtf8.SaveToFile(aData.OutputPath);
    end;
  end else begin
    case aOperation of
      opRead:
        for i := 0 to 49999 do
        begin
          j := i mod lCount;
          lResult := UTF8String(aUnicode.ReadString(string(aData.Sections[j]), string(aData.Keys[j]), ''));
          Inc(lTotal, Length(lResult));
        end;
      opWrite:
        for i := 0 to 19999 do
        begin
          j := i mod lCount;
          aUnicode.WriteString(string(aData.Sections[j]), string(aData.Keys[j]), string(aData.Values[(i div lCount + i) mod 2]));
        end;
      opLoadText: aUnicode.LoadFromText(string(aData.Text));
      opLoadFile: aUnicode.LoadFromFile(aData.InputPath);
      opSave: aUnicode.SaveToFile(aData.OutputPath);
    end;
  end;
  lWatch.Stop;
  Result := lWatch.ElapsedTicks;
  // Correctness is outside the timed region; every measured result remains observable.
  if aOperation = opRead then
  begin
    if lTotal = 0 then
      raise EInvalidOperation.Create('Read benchmark returned no values');
    j := 49999 mod lCount;
    if lResult <> aData.Values[j] then
      raise EInvalidOperation.Create('Read benchmark returned incorrect value');
  end else if aOperation = opWrite then
  begin
    j := 19999 mod lCount;
    lExpected := aData.Values[(19999 div lCount + 19999) mod 2];
    if aNative then
      lResult := aUtf8.ReadString(aData.Sections[j], aData.Keys[j], '')
    else
      lResult := UTF8String(aUnicode.ReadString(string(aData.Sections[j]), string(aData.Keys[j]), ''));
    if lResult <> lExpected then
      raise EInvalidOperation.Create('Write benchmark returned incorrect value');
  end else begin
    if aNative then
      lResult := aUtf8.ReadString(aData.Sections[0], aData.Keys[0], '')
    else
      lResult := UTF8String(aUnicode.ReadString(string(aData.Sections[0]), string(aData.Keys[0]), ''));
    if lResult <> aData.Values[0] then
      raise EInvalidOperation.Create('Load/save benchmark returned incorrect value');
  end;
end;

procedure Benchmark(aCount: Integer; aNonAscii, aIgnoreCase: Boolean);
const
  cNames: array[TOperation] of string = ('read_50000', 'write_20000', 'load_text', 'load_file', 'save_atomic');
  cSamples = 15;
var
  g: TGarbos;
  lUnicode: MaxLogic.RichIniFile.TRichIniFile;
  lNative: TRichIniFileUtf8;
  lUnicodeOptions: MaxLogic.RichIniFile.TRichIniOptions;
  lOptions: TRichIniOptionsUtf8;
  lData: TDataset;
  lUnicodeTimes, lNativeTimes: TArray<Int64>;
  lUnicodeTicks, lNativeTicks: Int64;
  lOperation: TOperation;
  i, j: Integer;
  lBytes: TBytes;
  lSaved: UTF8String;
begin
  lData := CreateDataset(aCount, aNonAscii);
  try
    lOptions := MaxLogic.RichIniFile.Utf8.DefaultRichIniOptions;
    lOptions.BomPolicy := TBomPolicy.bpNone;
    lOptions.NewlineMode := TNewlineMode.nlPreserveInput;
    lUnicodeOptions := MaxLogic.RichIniFile.DefaultRichIniOptions;
    lUnicodeOptions.BomPolicy := MaxLogic.RichIniFile.TBomPolicy.bpNone;
    lUnicodeOptions.NewlineMode := MaxLogic.RichIniFile.TNewlineMode.nlPreserveInput;
    if aIgnoreCase then
    begin
      lOptions.CaseSensitivity := TCaseSensitivity.csCaseInsensitive;
      lUnicodeOptions.CaseSensitivity := MaxLogic.RichIniFile.TCaseSensitivity.csCaseInsensitive;
    end;
    GC(lUnicode, MaxLogic.RichIniFile.TRichIniFile.Create('', lUnicodeOptions), g);
    GC(lNative, TRichIniFileUtf8.Create('', lOptions), g);
    SetLength(lUnicodeTimes, cSamples);
    SetLength(lNativeTimes, cSamples);
    for lOperation := Low(TOperation) to High(TOperation) do
    begin
      for i := -2 to cSamples - 1 do
      begin
        // Reset outside timing. Alternate order to reduce thermal/cache ordering bias.
        lUnicode.LoadFromText(string(lData.Text));
        lNative.LoadFromText(lData.Text);
        if Odd(i) then
        begin
          lNativeTicks := Measure(True, lOperation, lUnicode, lNative, lData);
          lUnicodeTicks := Measure(False, lOperation, lUnicode, lNative, lData);
        end else begin
          lUnicodeTicks := Measure(False, lOperation, lUnicode, lNative, lData);
          lNativeTicks := Measure(True, lOperation, lUnicode, lNative, lData);
        end;
        if lOperation = opSave then
        begin
          lBytes := TFile.ReadAllBytes(lData.OutputPath);
          SetString(lSaved, PAnsiChar(@lBytes[0]), Length(lBytes));
          // ParseText intentionally drops the final line terminator in both versions.
          if lSaved <> Copy(lData.Text, 1, Length(lData.Text)-2) then
            raise EInvalidOperation.Create('Saved bytes differ from the document');
        end;
        if i >= 0 then
        begin
          lUnicodeTimes[i] := lUnicodeTicks;
          lNativeTimes[i] := lNativeTicks;
        end;
      end;
      TArray.Sort<Int64>(lUnicodeTimes);
      TArray.Sort<Int64>(lNativeTimes);
      for j := 0 to 1 do
      begin
        if j = 0 then
          Writeln(Format('%d,%d,%d,%s,unicode,%.3f,%.3f,%.3f,%.3f',
            [aCount, Ord(aNonAscii), Ord(aIgnoreCase), cNames[lOperation],
             lUnicodeTimes[0]*1000.0/TStopwatch.Frequency, lUnicodeTimes[7]*1000.0/TStopwatch.Frequency,
             lUnicodeTimes[14]*1000.0/TStopwatch.Frequency, lUnicodeTimes[14]*1000.0/TStopwatch.Frequency], TFormatSettings.Invariant))
        else
          Writeln(Format('%d,%d,%d,%s,utf8,%.3f,%.3f,%.3f,%.3f',
            [aCount, Ord(aNonAscii), Ord(aIgnoreCase), cNames[lOperation],
             lNativeTimes[0]*1000.0/TStopwatch.Frequency, lNativeTimes[7]*1000.0/TStopwatch.Frequency,
             lNativeTimes[14]*1000.0/TStopwatch.Frequency, lNativeTimes[14]*1000.0/TStopwatch.Frequency], TFormatSettings.Invariant));
      end;
    end;
  finally
    if TFile.Exists(lData.InputPath) then
      TFile.Delete(lData.InputPath);
    if TFile.Exists(lData.OutputPath) then
      TFile.Delete(lData.OutputPath);
  end;
end;

const
  cSizes: array[0..1] of Integer = (100, 10000);
var
  lSize, lText, lCase: Integer;
begin
  ReportMemoryLeaksOnShutdown := True;
  try
    Writeln('keys,nonascii,ignorecase,operation,implementation,min_ms,median_ms,p95_ms,max_ms');
    for lSize in cSizes do
      for lText := 0 to 1 do
        for lCase := 0 to 1 do
          Benchmark(lSize, lText = 1, lCase = 1);
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
