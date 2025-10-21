program MaxLogicRichIniBenchmark;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  System.Diagnostics,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.IniFiles,
  MaxLogic.strUtils in '..\MaxLogic.strUtils.pas',
  MaxLogic.RichIniFile in '..\MaxLogic.RichIniFile.pas';

type
  TKeyInfo = record
    Section: string;
    Key: string;
  end;

  TDataset = record
    FileName: string;
    Keys: TArray<TKeyInfo>;
    Lines: TArray<string>;
  end;

  TBenchmarkResult = record
    Name: string;
    LoadMs: Double;
    ReadMs: Double;
    WriteMs: Double;
    SaveMs: Double;
    WriteSaveMs: Double;
  end;

const
  cIterations = 5;

function FormatMs(const aValue: Double): string;
begin
  Result := Format('%.3f ms', [aValue]);
end;

function PrepareDataset(const aSections, aKeysPerSection: Integer): TDataset;
var
  lSectionIndex: Integer;
  lKeyIndex: Integer;
  lKeyList: TList<TKeyInfo>;
  lKeyInfo: TKeyInfo;
  lFile: string;
  lLines: TStringList;
begin
  lFile := TPath.Combine(TPath.GetTempPath, 'richini-bench-' + TPath.GetRandomFileName + '.ini');
  lLines := TStringList.Create;
  lKeyList := TList<TKeyInfo>.Create;
  try
    for lSectionIndex := 1 to aSections do
    begin
      lLines.Add(Format('[Section%d]', [lSectionIndex]));
      for lKeyIndex := 1 to aKeysPerSection do
      begin
        lLines.Add(Format('Key%d=Value%d', [lKeyIndex, lKeyIndex]));
        lKeyInfo.Section := Format('Section%d', [lSectionIndex]);
        lKeyInfo.Key := Format('Key%d', [lKeyIndex]);
        lKeyList.Add(lKeyInfo);
      end;
      lLines.Add('; comment to preserve');
      lLines.Add('DuplicateKey=Original');
      lLines.Add('DuplicateKey=Overwrite');
      lKeyInfo.Section := Format('Section%d', [lSectionIndex]);
      lKeyInfo.Key := 'DuplicateKey';
      lKeyList.Add(lKeyInfo);
    end;
    lLines.SaveToFile(lFile, TEncoding.UTF8);
    Result.FileName := lFile;
    Result.Keys := lKeyList.ToArray;
    SetLength(Result.Lines, lLines.Count);
    for lKeyIndex := 0 to lLines.Count - 1 do
      Result.Lines[lKeyIndex] := lLines[lKeyIndex];
  finally
    lLines.Free;
    lKeyList.Free;
  end;
end;

procedure CleanupDataset(const aDataset: TDataset);
begin
  if (aDataset.FileName <> '') and TFile.Exists(aDataset.FileName) then
    TFile.Delete(aDataset.FileName);
end;

function BenchmarkRichIni(const aDataset: TDataset): TBenchmarkResult;
var
  i: Integer;
  lTotalLoad, lTotalRead, lTotalWrite, lTotalSave: Double;
  lStopwatch: TStopwatch;
  lIni: TRichIniFile;
  lOptions: TRichIniOptions;
  lSink: Integer;
  lTempFile: string;
  lKey: TKeyInfo;
begin
  Result.Name := 'TRichIniFile';
  lTotalLoad := 0;
  lTotalRead := 0;
  lTotalWrite := 0;
  lTotalSave := 0;
  lSink := 0;
  lOptions := DefaultRichIniOptions;

  for i := 1 to cIterations do
  begin
    lStopwatch := TStopwatch.StartNew;
    lIni := TRichIniFile.Create('', lOptions);
    try
      lIni.LoadFromFile(aDataset.FileName);
    finally
      lStopwatch.Stop;
      lTotalLoad := lTotalLoad + lStopwatch.Elapsed.TotalMilliseconds;
      lIni.Free;
    end;
  end;

  lIni := TRichIniFile.Create('', lOptions);
  try
    lIni.LoadFromFile(aDataset.FileName);
    for i := 1 to cIterations do
    begin
      lStopwatch := TStopwatch.StartNew;
      for lKey in aDataset.Keys do
        Inc(lSink, Length(lIni.ReadString(lKey.Section, lKey.Key, '')));
      lStopwatch.Stop;
      lTotalRead := lTotalRead + lStopwatch.Elapsed.TotalMilliseconds;
    end;

    for i := 1 to cIterations do
    begin
      lStopwatch := TStopwatch.StartNew;
      lIni.WriteString('Section1', 'Key1', 'Value' + IntToStr(i));
      lStopwatch.Stop;
      lTotalWrite := lTotalWrite + lStopwatch.Elapsed.TotalMilliseconds;

      lTempFile := TPath.Combine(TPath.GetTempPath, Format('richini-save-%d-%s.ini', [i, TPath.GetRandomFileName]));
      lStopwatch := TStopwatch.StartNew;
      lIni.SaveToFile(lTempFile);
      lStopwatch.Stop;
      lTotalSave := lTotalSave + lStopwatch.Elapsed.TotalMilliseconds;
      if TFile.Exists(lTempFile) then
        TFile.Delete(lTempFile);
    end;
  finally
    lIni.Free;
  end;

  Result.LoadMs := lTotalLoad / cIterations;
  Result.ReadMs := lTotalRead / cIterations;
  Result.WriteMs := lTotalWrite / cIterations;
  Result.SaveMs := lTotalSave / cIterations;
  Result.WriteSaveMs := Result.WriteMs + Result.SaveMs;
end;

function BenchmarkMemIni(const aDataset: TDataset): TBenchmarkResult;
var
  i: Integer;
  lTotalLoad, lTotalRead, lTotalWrite, lTotalSave: Double;
  lStopwatch: TStopwatch;
  lIni: TMemIniFile;
  lSink: Integer;
  lTempFile: string;
  lKey: TKeyInfo;
  lSections: TStringList;
begin
  Result.Name := 'TMemIniFile';
  lTotalLoad := 0;
  lTotalRead := 0;
  lTotalWrite := 0;
  lTotalSave := 0;
  lSink := 0;

  for i := 1 to cIterations do
  begin
    lStopwatch := TStopwatch.StartNew;
    lIni := TMemIniFile.Create(aDataset.FileName, TEncoding.UTF8);
    try
      lSections := TStringList.Create;
      try
        lIni.ReadSections(lSections);
      finally
        lSections.Free;
      end;
    finally
      lStopwatch.Stop;
      lTotalLoad := lTotalLoad + lStopwatch.Elapsed.TotalMilliseconds;
      lIni.Free;
    end;
  end;

  lIni := TMemIniFile.Create(aDataset.FileName, TEncoding.UTF8);
  try
    for i := 1 to cIterations do
    begin
      lStopwatch := TStopwatch.StartNew;
      for lKey in aDataset.Keys do
        Inc(lSink, Length(lIni.ReadString(lKey.Section, lKey.Key, '')));
      lStopwatch.Stop;
      lTotalRead := lTotalRead + lStopwatch.Elapsed.TotalMilliseconds;
    end;
  finally
    lIni.Free;
  end;

  for i := 1 to cIterations do
  begin
    lTempFile := TPath.Combine(TPath.GetTempPath, Format('memini-save-%d-%s.ini', [i, TPath.GetRandomFileName]));
    TFile.Copy(aDataset.FileName, lTempFile, True);
    lIni := TMemIniFile.Create(lTempFile, TEncoding.UTF8);
    try
      lStopwatch := TStopwatch.StartNew;
      lIni.WriteString('Section1', 'Key1', 'Value' + IntToStr(i));
      lStopwatch.Stop;
      lTotalWrite := lTotalWrite + lStopwatch.Elapsed.TotalMilliseconds;

      lStopwatch := TStopwatch.StartNew;
      lIni.UpdateFile;
      lStopwatch.Stop;
      lTotalSave := lTotalSave + lStopwatch.Elapsed.TotalMilliseconds;
    finally
      lIni.Free;
      if TFile.Exists(lTempFile) then
        TFile.Delete(lTempFile);
    end;
  end;

  Result.LoadMs := lTotalLoad / cIterations;
  Result.ReadMs := lTotalRead / cIterations;
  Result.WriteMs := lTotalWrite / cIterations;
  Result.SaveMs := lTotalSave / cIterations;
  Result.WriteSaveMs := Result.WriteMs + Result.SaveMs;
end;

function BenchmarkIniFile(const aDataset: TDataset): TBenchmarkResult;
var
  i: Integer;
  lTotalLoad, lTotalRead, lTotalWrite: Double;
  lStopwatch: TStopwatch;
  lIni: TIniFile;
  lSink: Integer;
  lKey: TKeyInfo;
  lSections: TStringList;
  lTempFile: string;
begin
  Result.Name := 'TIniFile';
  lTotalLoad := 0;
  lTotalRead := 0;
  lTotalWrite := 0;
  Result.SaveMs := -1; // not applicable
  lSink := 0;

  for i := 1 to cIterations do
  begin
    lStopwatch := TStopwatch.StartNew;
    lIni := TIniFile.Create(aDataset.FileName);
    try
      lSections := TStringList.Create;
      try
        lIni.ReadSections(lSections);
      finally
        lSections.Free;
      end;
    finally
      lStopwatch.Stop;
      lTotalLoad := lTotalLoad + lStopwatch.Elapsed.TotalMilliseconds;
      lIni.Free;
    end;
  end;

  lIni := TIniFile.Create(aDataset.FileName);
  try
    for i := 1 to cIterations do
    begin
      lStopwatch := TStopwatch.StartNew;
      for lKey in aDataset.Keys do
        Inc(lSink, Length(lIni.ReadString(lKey.Section, lKey.Key, '')));
      lStopwatch.Stop;
      lTotalRead := lTotalRead + lStopwatch.Elapsed.TotalMilliseconds;
    end;
  finally
    lIni.Free;
  end;

  for i := 1 to cIterations do
  begin
    lTempFile := TPath.Combine(TPath.GetTempPath, Format('rtlini-save-%d-%s.ini', [i, TPath.GetRandomFileName]));
    TFile.Copy(aDataset.FileName, lTempFile, True);
    lIni := TIniFile.Create(lTempFile);
    try
      lStopwatch := TStopwatch.StartNew;
      lIni.WriteString('Section1', 'Key1', 'Value' + IntToStr(i));
      lStopwatch.Stop;
      lTotalWrite := lTotalWrite + lStopwatch.Elapsed.TotalMilliseconds;
    finally
      lIni.Free;
      if TFile.Exists(lTempFile) then
        TFile.Delete(lTempFile);
    end;
  end;

  Result.LoadMs := lTotalLoad / cIterations;
  Result.ReadMs := lTotalRead / cIterations;
  Result.WriteMs := lTotalWrite / cIterations;
  Result.WriteSaveMs := Result.WriteMs;
  if lSink = -1 then
    Writeln('');
end;

function BenchmarkRichIniFromStrings(const aDataset: TDataset): TBenchmarkResult;
var
  i: Integer;
  lTotalLoad, lTotalRead, lTotalWrite, lTotalSave: Double;
  lStopwatch: TStopwatch;
  lIni: TRichIniFile;
  lOptions: TRichIniOptions;
  lSink: Integer;
  lTempFile: string;
  lKey: TKeyInfo;
begin
  Result.Name := 'TRichIniFile (strings)';
  lTotalLoad := 0;
  lTotalRead := 0;
  lTotalWrite := 0;
  lTotalSave := 0;
  lSink := 0;
  lOptions := DefaultRichIniOptions;

  for i := 1 to cIterations do
  begin
    lStopwatch := TStopwatch.StartNew;
    lIni := TRichIniFile.CreateFromStrings(aDataset.Lines, lOptions);
    try
      // nothing to do
    finally
      lStopwatch.Stop;
      lTotalLoad := lTotalLoad + lStopwatch.Elapsed.TotalMilliseconds;
      lIni.Free;
    end;
  end;

  lIni := TRichIniFile.CreateFromStrings(aDataset.Lines, lOptions);
  try
    for i := 1 to cIterations do
    begin
      lStopwatch := TStopwatch.StartNew;
      for lKey in aDataset.Keys do
        Inc(lSink, Length(lIni.ReadString(lKey.Section, lKey.Key, '')));
      lStopwatch.Stop;
      lTotalRead := lTotalRead + lStopwatch.Elapsed.TotalMilliseconds;
    end;

    for i := 1 to cIterations do
    begin
      lStopwatch := TStopwatch.StartNew;
      lIni.WriteString('Section1', 'Key1', 'Value' + IntToStr(i));
      lStopwatch.Stop;
      lTotalWrite := lTotalWrite + lStopwatch.Elapsed.TotalMilliseconds;

      lTempFile := TPath.Combine(TPath.GetTempPath, Format('richini-save-%d-%s.ini', [i, TPath.GetRandomFileName]));
      lStopwatch := TStopwatch.StartNew;
      lIni.SaveToFile(lTempFile);
      lStopwatch.Stop;
      lTotalSave := lTotalSave + lStopwatch.Elapsed.TotalMilliseconds;
      if TFile.Exists(lTempFile) then
        TFile.Delete(lTempFile);
    end;
  finally
    lIni.Free;
  end;

  Result.LoadMs := lTotalLoad / cIterations;
  Result.ReadMs := lTotalRead / cIterations;
  Result.WriteMs := lTotalWrite / cIterations;
  Result.SaveMs := lTotalSave / cIterations;
  Result.WriteSaveMs := Result.WriteMs + Result.SaveMs;
end;

function BenchmarkMemIniFromStrings(const aDataset: TDataset): TBenchmarkResult;
var
  i: Integer;
  lTotalLoad, lTotalRead, lTotalWrite: Double;
  lStopwatch: TStopwatch;
  lIni: TMemIniFile;
  lSink: Integer;
  lKey: TKeyInfo;
  lSource: TStringList;
  lLine: string;
begin
  Result.Name := 'TMemIniFile (strings)';
  lTotalLoad := 0;
  lTotalRead := 0;
  lTotalWrite := 0;
  lSink := 0;

  lSource := TStringList.Create;
  try
    for lLine in aDataset.Lines do
      lSource.Add(lLine);

    for i := 1 to cIterations do
    begin
      lStopwatch := TStopwatch.StartNew;
      lIni := TMemIniFile.Create('');
      try
        lIni.SetStrings(lSource);
      finally
        lStopwatch.Stop;
        lTotalLoad := lTotalLoad + lStopwatch.Elapsed.TotalMilliseconds;
        lIni.Free;
      end;
    end;

    lIni := TMemIniFile.Create('');
    try
      lIni.SetStrings(lSource);
      for i := 1 to cIterations do
      begin
        lStopwatch := TStopwatch.StartNew;
        for lKey in aDataset.Keys do
          Inc(lSink, Length(lIni.ReadString(lKey.Section, lKey.Key, '')));
        lStopwatch.Stop;
        lTotalRead := lTotalRead + lStopwatch.Elapsed.TotalMilliseconds;
      end;

      for i := 1 to cIterations do
      begin
        lStopwatch := TStopwatch.StartNew;
        lIni.WriteString('Section1', 'Key1', 'Value' + IntToStr(i));
        lStopwatch.Stop;
        lTotalWrite := lTotalWrite + lStopwatch.Elapsed.TotalMilliseconds;
      end;
    finally
      lIni.Free;
    end;
  finally
    lSource.Free;
  end;

  Result.LoadMs := lTotalLoad / cIterations;
  Result.ReadMs := lTotalRead / cIterations;
  Result.WriteMs := lTotalWrite / cIterations;
  Result.SaveMs := -1;
  Result.WriteSaveMs := Result.WriteMs;
end;

procedure PrintResults(const aTitle: string; const aResults: TArray<TBenchmarkResult>);
var
  lResult: TBenchmarkResult;
  lLoadStr, lReadStr, lWriteStr, lSaveStr, lWriteSaveStr: string;
  lLoadMaxWidth, lReadMaxWidth, lWriteMaxWidth, lSaveMaxWidth, lWriteSaveMaxWidth: Integer;
begin
  Writeln(aTitle);
  lLoadMaxWidth:= 0;
  lReadMaxWidth:= 0;
  lWriteMaxWidth := 0;
  lSaveMaxWidth:= 0;
  lWriteSaveMaxWidth := 0;

  for lResult in aResults do
  begin
    lLoadStr := FormatFloat('0.000000', lResult.LoadMs) + ' ms';
    lReadStr := FormatFloat('0.000000', lResult.ReadMs) + ' ms';
    lWriteStr := FormatFloat('0.000000', lResult.WriteMs) + ' ms';
    if lResult.SaveMs >= 0 then
      lSaveStr := FormatFloat('0.000000', lResult.SaveMs) + ' ms'
    else
      lSaveStr := '(n/a)';
    lWriteSaveStr := FormatFloat('0.000000', lResult.WriteSaveMs) + ' ms';

    if Length(lLoadStr) > lLoadMaxWidth then
      lLoadMaxWidth := Length(lLoadStr);
    if Length(lReadStr) > lReadMaxWidth then
      lReadMaxWidth := Length(lReadStr);
    if Length(lWriteStr) > lWriteMaxWidth then
      lWriteMaxWidth := Length(lWriteStr);
    if Length(lSaveStr) > lSaveMaxWidth then
      lSaveMaxWidth := Length(lSaveStr);
    if Length(lWriteSaveStr) > lWriteSaveMaxWidth then
      lWriteSaveMaxWidth := Length(lWriteSaveStr);
  end;

  Writeln('Benchmark Results (avg of ', cIterations, ' runs)');
  Writeln('----------------------------------------------');
  for lResult in aResults do
  begin
    lLoadStr := putBefore(FormatFloat('0.000000', lResult.LoadMs) + ' ms', ' ', lLoadMaxWidth);
    lReadStr := putBefore(FormatFloat('0.000000', lResult.ReadMs) + ' ms', ' ', lReadMaxWidth);
    lWriteStr := putBefore(FormatFloat('0.000000', lResult.WriteMs) + ' ms', ' ', lWriteMaxWidth);
    if lResult.SaveMs >= 0 then
      lSaveStr := putBefore(FormatFloat('0.000000', lResult.SaveMs) + ' ms', ' ', lSaveMaxWidth)
    else
      lSaveStr := putBefore('(n/a)', ' ', lSaveMaxWidth);
    lWriteSaveStr := putBefore(FormatFloat('0.000000', lResult.WriteSaveMs) + ' ms', ' ', lWriteSaveMaxWidth);

    Writeln(Format('%-15s Load: %s  Read: %s  Write: %s  Save: %s  Write+Save: %s',
      [lResult.Name, lLoadStr, lReadStr, lWriteStr, lSaveStr, lWriteSaveStr]));
  end;
  Writeln;
end;

var
  lDataset: TDataset;
  lResults: TArray<TBenchmarkResult>;
  lStringResults: TArray<TBenchmarkResult>;
begin
  try
    lDataset := PrepareDataset(200, 50);
    try
      SetLength(lResults, 3);
      lResults[0] := BenchmarkRichIni(lDataset);
      lResults[1] := BenchmarkMemIni(lDataset);
      lResults[2] := BenchmarkIniFile(lDataset);
      PrintResults('File-based dataset', lResults);

      SetLength(lStringResults, 2);
      lStringResults[0] := BenchmarkRichIniFromStrings(lDataset);
      lStringResults[1] := BenchmarkMemIniFromStrings(lDataset);
      PrintResults('TStringList-based dataset', lStringResults);
    finally
      CleanupDataset(lDataset);
    end;
    Readln;
  except
    on E: Exception do
    begin
      Writeln('Benchmark failed: ', E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;
end.
