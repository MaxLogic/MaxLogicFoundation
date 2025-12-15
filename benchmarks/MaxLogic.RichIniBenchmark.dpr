program MaxLogicRichIniBenchmark;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  System.Diagnostics,
  System.Classes,
  System.Math,
  System.IOUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
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

  TMetric = record
    MeanMs: Double;
    StdDevMs: Double;
    MedianMs: Double;
    MinMs: Double;
    MaxMs: Double;
  end;

  TBenchmarkResult = record
    Name: string;
    Load: TMetric;
    Read: TMetric;
    Write: TMetric;
    Save: TMetric;
    WriteSave: TMetric;
  end;

var
  Iterations: Integer = 15;
  WarmupIterations: Integer = 3;

function FormatMs(const aValue: Double): string;
begin
  Result := Format('%.3f ms', [aValue]);
end;

function CalcMetric(const aSamples: TArray<Double>): TMetric;
var
  lSorted: TArray<Double>;
  lCount: Integer;
  lMean: Double;
  lM2: Double;
  lDelta: Double;
  lValue: Double;
  i: Integer;
begin
  Result.MeanMs := 0;
  Result.StdDevMs := 0;
  Result.MedianMs := 0;
  Result.MinMs := 0;
  Result.MaxMs := 0;

  lCount := Length(aSamples);
  if lCount = 0 then
    Exit;

  Result.MinMs := aSamples[0];
  Result.MaxMs := aSamples[0];
  lMean := 0;
  lM2 := 0;
  for i := 0 to lCount - 1 do
  begin
    lValue := aSamples[i];
    if lValue < Result.MinMs then
      Result.MinMs := lValue;
    if lValue > Result.MaxMs then
      Result.MaxMs := lValue;

    lDelta := lValue - lMean;
    lMean := lMean + (lDelta / (i + 1));
    lM2 := lM2 + lDelta * (lValue - lMean);
  end;
  Result.MeanMs := lMean;
  if lCount > 1 then
    Result.StdDevMs := Sqrt(lM2 / (lCount - 1));

  lSorted := Copy(aSamples, 0, Length(aSamples));
  TArray.Sort<Double>(lSorted);
  if (lCount and 1) = 1 then
    Result.MedianMs := lSorted[lCount div 2]
  else
    Result.MedianMs := (lSorted[(lCount div 2) - 1] + lSorted[lCount div 2]) / 2;
end;

function GetIntParam(const aName: string; const aDefault: Integer): Integer;
var
  i: Integer;
  lKey: string;
  lValue: string;
begin
  Result := aDefault;
  lKey := '--' + aName + '=';
  for i := 1 to ParamCount do
  begin
    if SameText(Copy(ParamStr(i), 1, Length(lKey)), lKey) then
    begin
      lValue := Copy(ParamStr(i), Length(lKey) + 1, MaxInt);
      if lValue <> '' then
        Result := StrToIntDef(lValue, aDefault);
      Exit;
    end;
  end;
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
  lLoad, lRead, lWrite, lSave, lWriteSave: TArray<Double>;
  lStopwatch: TStopwatch;
  lIni: TRichIniFile;
  lOptions: TRichIniOptions;
  lSink: Integer;
  lTempFile: string;
  lKey: TKeyInfo;
begin
  Result.Name := 'TRichIniFile';
  lSink := 0;
  lOptions := DefaultRichIniOptions;

  SetLength(lLoad, Iterations);
  SetLength(lRead, Iterations);
  SetLength(lWrite, Iterations);
  SetLength(lSave, Iterations);
  SetLength(lWriteSave, Iterations);

  for i := 1 to WarmupIterations do
  begin
    lIni := TRichIniFile.Create('', lOptions);
    try
      lIni.LoadFromFile(aDataset.FileName);
    finally
      lIni.Free;
    end;
  end;

  for i := 0 to Iterations - 1 do
  begin
    lStopwatch := TStopwatch.StartNew;
    lIni := TRichIniFile.Create('', lOptions);
    try
      lIni.LoadFromFile(aDataset.FileName);
    finally
      lStopwatch.Stop;
      lLoad[i] := lStopwatch.Elapsed.TotalMilliseconds;
      lIni.Free;
    end;
  end;

  lIni := TRichIniFile.Create('', lOptions);
  try
    lIni.LoadFromFile(aDataset.FileName);
    for i := 1 to WarmupIterations do
      for lKey in aDataset.Keys do
        Inc(lSink, Length(lIni.ReadString(lKey.Section, lKey.Key, '')));

    for i := 0 to Iterations - 1 do
    begin
      lStopwatch := TStopwatch.StartNew;
      for lKey in aDataset.Keys do
        Inc(lSink, Length(lIni.ReadString(lKey.Section, lKey.Key, '')));
      lStopwatch.Stop;
      lRead[i] := lStopwatch.Elapsed.TotalMilliseconds;
    end;

    lTempFile := TPath.Combine(TPath.GetTempPath, 'richini-save-' + TPath.GetRandomFileName + '.ini');
    for i := 1 to WarmupIterations do
    begin
      lIni.WriteString('Section1', 'Key1', 'Value' + IntToStr(i));
      lIni.SaveToFile(lTempFile);
    end;

    for i := 0 to Iterations - 1 do
    begin
      lStopwatch := TStopwatch.StartNew;
      lIni.WriteString('Section1', 'Key1', 'Value' + IntToStr(i));
      lStopwatch.Stop;
      lWrite[i] := lStopwatch.Elapsed.TotalMilliseconds;

      lStopwatch := TStopwatch.StartNew;
      lIni.SaveToFile(lTempFile);
      lStopwatch.Stop;
      lSave[i] := lStopwatch.Elapsed.TotalMilliseconds;

      lWriteSave[i] := lWrite[i] + lSave[i];
    end;

    if TFile.Exists(lTempFile) then
      TFile.Delete(lTempFile);
  finally
    lIni.Free;
  end;

  Result.Load := CalcMetric(lLoad);
  Result.Read := CalcMetric(lRead);
  Result.Write := CalcMetric(lWrite);
  Result.Save := CalcMetric(lSave);
  Result.WriteSave := CalcMetric(lWriteSave);
end;

function BenchmarkMemIni(const aDataset: TDataset): TBenchmarkResult;
var
  i: Integer;
  lLoad, lRead, lWrite, lSave, lWriteSave: TArray<Double>;
  lStopwatch: TStopwatch;
  lIni: TMemIniFile;
  lSink: Integer;
  lTempFile: string;
  lKey: TKeyInfo;
  lSections: TStringList;
begin
  Result.Name := 'TMemIniFile';
  lSink := 0;

  SetLength(lLoad, Iterations);
  SetLength(lRead, Iterations);
  SetLength(lWrite, Iterations);
  SetLength(lSave, Iterations);
  SetLength(lWriteSave, Iterations);

  for i := 1 to WarmupIterations do
  begin
    lIni := TMemIniFile.Create(aDataset.FileName, TEncoding.UTF8);
    try
      lSections := TStringList.Create;
      try
        lIni.ReadSections(lSections);
      finally
        lSections.Free;
      end;
    finally
      lIni.Free;
    end;
  end;

  for i := 0 to Iterations - 1 do
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
      lLoad[i] := lStopwatch.Elapsed.TotalMilliseconds;
      lIni.Free;
    end;
  end;

  lIni := TMemIniFile.Create(aDataset.FileName, TEncoding.UTF8);
  try
    for i := 1 to WarmupIterations do
      for lKey in aDataset.Keys do
        Inc(lSink, Length(lIni.ReadString(lKey.Section, lKey.Key, '')));

    for i := 0 to Iterations - 1 do
    begin
      lStopwatch := TStopwatch.StartNew;
      for lKey in aDataset.Keys do
        Inc(lSink, Length(lIni.ReadString(lKey.Section, lKey.Key, '')));
      lStopwatch.Stop;
      lRead[i] := lStopwatch.Elapsed.TotalMilliseconds;
    end;
  finally
    lIni.Free;
  end;

  lTempFile := TPath.Combine(TPath.GetTempPath, 'memini-save-' + TPath.GetRandomFileName + '.ini');
  for i := 1 to WarmupIterations do
  begin
    TFile.Copy(aDataset.FileName, lTempFile, True);
    lIni := TMemIniFile.Create(lTempFile, TEncoding.UTF8);
    try
      lIni.WriteString('Section1', 'Key1', 'Value' + IntToStr(i));
      lIni.UpdateFile;
    finally
      lIni.Free;
    end;
  end;

  for i := 0 to Iterations - 1 do
  begin
    TFile.Copy(aDataset.FileName, lTempFile, True);
    lIni := TMemIniFile.Create(lTempFile, TEncoding.UTF8);
    try
      lStopwatch := TStopwatch.StartNew;
      lIni.WriteString('Section1', 'Key1', 'Value' + IntToStr(i + 1));
      lStopwatch.Stop;
      lWrite[i] := lStopwatch.Elapsed.TotalMilliseconds;

      lStopwatch := TStopwatch.StartNew;
      lIni.UpdateFile;
      lStopwatch.Stop;
      lSave[i] := lStopwatch.Elapsed.TotalMilliseconds;
      lWriteSave[i] := lWrite[i] + lSave[i];
    finally
      lIni.Free;
    end;
  end;

  if TFile.Exists(lTempFile) then
    TFile.Delete(lTempFile);

  Result.Load := CalcMetric(lLoad);
  Result.Read := CalcMetric(lRead);
  Result.Write := CalcMetric(lWrite);
  Result.Save := CalcMetric(lSave);
  Result.WriteSave := CalcMetric(lWriteSave);
end;

function BenchmarkIniFile(const aDataset: TDataset): TBenchmarkResult;
var
  i: Integer;
  lLoad, lRead, lWrite, lWriteSave: TArray<Double>;
  lStopwatch: TStopwatch;
  lIni: TIniFile;
  lSink: Integer;
  lKey: TKeyInfo;
  lSections: TStringList;
  lTempFile: string;
begin
  Result.Name := 'TIniFile';
  lSink := 0;

  SetLength(lLoad, Iterations);
  SetLength(lRead, Iterations);
  SetLength(lWrite, Iterations);
  SetLength(lWriteSave, Iterations);

  for i := 1 to WarmupIterations do
  begin
    lIni := TIniFile.Create(aDataset.FileName);
    try
      lSections := TStringList.Create;
      try
        lIni.ReadSections(lSections);
      finally
        lSections.Free;
      end;
    finally
      lIni.Free;
    end;
  end;

  for i := 0 to Iterations - 1 do
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
      lLoad[i] := lStopwatch.Elapsed.TotalMilliseconds;
      lIni.Free;
    end;
  end;

  lIni := TIniFile.Create(aDataset.FileName);
  try
    for i := 1 to WarmupIterations do
      for lKey in aDataset.Keys do
        Inc(lSink, Length(lIni.ReadString(lKey.Section, lKey.Key, '')));

    for i := 0 to Iterations - 1 do
    begin
      lStopwatch := TStopwatch.StartNew;
      for lKey in aDataset.Keys do
        Inc(lSink, Length(lIni.ReadString(lKey.Section, lKey.Key, '')));
      lStopwatch.Stop;
      lRead[i] := lStopwatch.Elapsed.TotalMilliseconds;
    end;
  finally
    lIni.Free;
  end;

  lTempFile := TPath.Combine(TPath.GetTempPath, 'rtlini-save-' + TPath.GetRandomFileName + '.ini');
  for i := 1 to WarmupIterations do
  begin
    TFile.Copy(aDataset.FileName, lTempFile, True);
    lIni := TIniFile.Create(lTempFile);
    try
      lIni.WriteString('Section1', 'Key1', 'Value' + IntToStr(i));
    finally
      lIni.Free;
    end;
  end;

  for i := 0 to Iterations - 1 do
  begin
    TFile.Copy(aDataset.FileName, lTempFile, True);
    lIni := TIniFile.Create(lTempFile);
    try
      lStopwatch := TStopwatch.StartNew;
      lIni.WriteString('Section1', 'Key1', 'Value' + IntToStr(i + 1));
      lStopwatch.Stop;
      lWrite[i] := lStopwatch.Elapsed.TotalMilliseconds;
      lWriteSave[i] := lWrite[i];
    finally
      lIni.Free;
    end;
  end;

  if TFile.Exists(lTempFile) then
    TFile.Delete(lTempFile);

  Result.Load := CalcMetric(lLoad);
  Result.Read := CalcMetric(lRead);
  Result.Write := CalcMetric(lWrite);
  Result.Save.MeanMs := -1;
  Result.WriteSave := CalcMetric(lWriteSave);
  if lSink = -1 then
    Writeln('');
end;

function BenchmarkRichIniFromStrings(const aDataset: TDataset): TBenchmarkResult;
var
  i: Integer;
  lLoad, lRead, lWrite, lSave, lWriteSave: TArray<Double>;
  lStopwatch: TStopwatch;
  lIni: TRichIniFile;
  lOptions: TRichIniOptions;
  lSink: Integer;
  lTempFile: string;
  lKey: TKeyInfo;
begin
  Result.Name := 'TRichIniFile (strings)';
  lSink := 0;
  lOptions := DefaultRichIniOptions;

  SetLength(lLoad, Iterations);
  SetLength(lRead, Iterations);
  SetLength(lWrite, Iterations);
  SetLength(lSave, Iterations);
  SetLength(lWriteSave, Iterations);

  for i := 1 to WarmupIterations do
  begin
    lIni := TRichIniFile.CreateFromStrings(aDataset.Lines, lOptions);
    try
      // nothing to do
    finally
      lIni.Free;
    end;
  end;

  for i := 0 to Iterations - 1 do
  begin
    lStopwatch := TStopwatch.StartNew;
    lIni := TRichIniFile.CreateFromStrings(aDataset.Lines, lOptions);
    try
      // nothing to do
    finally
      lStopwatch.Stop;
      lLoad[i] := lStopwatch.Elapsed.TotalMilliseconds;
      lIni.Free;
    end;
  end;

  lIni := TRichIniFile.CreateFromStrings(aDataset.Lines, lOptions);
  try
    for i := 1 to WarmupIterations do
      for lKey in aDataset.Keys do
        Inc(lSink, Length(lIni.ReadString(lKey.Section, lKey.Key, '')));

    for i := 0 to Iterations - 1 do
    begin
      lStopwatch := TStopwatch.StartNew;
      for lKey in aDataset.Keys do
        Inc(lSink, Length(lIni.ReadString(lKey.Section, lKey.Key, '')));
      lStopwatch.Stop;
      lRead[i] := lStopwatch.Elapsed.TotalMilliseconds;
    end;

    lTempFile := TPath.Combine(TPath.GetTempPath, 'richini-save-' + TPath.GetRandomFileName + '.ini');
    for i := 1 to WarmupIterations do
    begin
      lIni.WriteString('Section1', 'Key1', 'Value' + IntToStr(i));
      lIni.SaveToFile(lTempFile);
    end;

    for i := 0 to Iterations - 1 do
    begin
      lStopwatch := TStopwatch.StartNew;
      lIni.WriteString('Section1', 'Key1', 'Value' + IntToStr(i + 1));
      lStopwatch.Stop;
      lWrite[i] := lStopwatch.Elapsed.TotalMilliseconds;

      lStopwatch := TStopwatch.StartNew;
      lIni.SaveToFile(lTempFile);
      lStopwatch.Stop;
      lSave[i] := lStopwatch.Elapsed.TotalMilliseconds;
      lWriteSave[i] := lWrite[i] + lSave[i];
    end;

    if TFile.Exists(lTempFile) then
      TFile.Delete(lTempFile);
  finally
    lIni.Free;
  end;

  Result.Load := CalcMetric(lLoad);
  Result.Read := CalcMetric(lRead);
  Result.Write := CalcMetric(lWrite);
  Result.Save := CalcMetric(lSave);
  Result.WriteSave := CalcMetric(lWriteSave);
end;

function BenchmarkMemIniFromStrings(const aDataset: TDataset): TBenchmarkResult;
var
  i: Integer;
  lLoad, lRead, lWrite, lWriteSave: TArray<Double>;
  lStopwatch: TStopwatch;
  lIni: TMemIniFile;
  lSink: Integer;
  lKey: TKeyInfo;
  lSource: TStringList;
  lLine: string;
begin
  Result.Name := 'TMemIniFile (strings)';
  lSink := 0;

  lSource := TStringList.Create;
  try
    for lLine in aDataset.Lines do
      lSource.Add(lLine);

    SetLength(lLoad, Iterations);
    SetLength(lRead, Iterations);
    SetLength(lWrite, Iterations);
    SetLength(lWriteSave, Iterations);

    for i := 1 to WarmupIterations do
    begin
      lIni := TMemIniFile.Create('');
      try
        lIni.SetStrings(lSource);
      finally
        lIni.Free;
      end;
    end;

    for i := 0 to Iterations - 1 do
    begin
      lStopwatch := TStopwatch.StartNew;
      lIni := TMemIniFile.Create('');
      try
        lIni.SetStrings(lSource);
      finally
        lStopwatch.Stop;
        lLoad[i] := lStopwatch.Elapsed.TotalMilliseconds;
        lIni.Free;
      end;
    end;

    lIni := TMemIniFile.Create('');
    try
      lIni.SetStrings(lSource);
      for i := 1 to WarmupIterations do
        for lKey in aDataset.Keys do
          Inc(lSink, Length(lIni.ReadString(lKey.Section, lKey.Key, '')));

      for i := 0 to Iterations - 1 do
      begin
        lStopwatch := TStopwatch.StartNew;
        for lKey in aDataset.Keys do
          Inc(lSink, Length(lIni.ReadString(lKey.Section, lKey.Key, '')));
        lStopwatch.Stop;
        lRead[i] := lStopwatch.Elapsed.TotalMilliseconds;
      end;

      for i := 0 to Iterations - 1 do
      begin
        lStopwatch := TStopwatch.StartNew;
        lIni.WriteString('Section1', 'Key1', 'Value' + IntToStr(i + 1));
        lStopwatch.Stop;
        lWrite[i] := lStopwatch.Elapsed.TotalMilliseconds;
        lWriteSave[i] := lWrite[i];
      end;
    finally
      lIni.Free;
    end;
  finally
    lSource.Free;
  end;

  Result.Load := CalcMetric(lLoad);
  Result.Read := CalcMetric(lRead);
  Result.Write := CalcMetric(lWrite);
  Result.Save.MeanMs := -1;
  Result.WriteSave := CalcMetric(lWriteSave);
end;

procedure PrintResults(const aTitle: string; const aResults: TArray<TBenchmarkResult>);
var
  lResult: TBenchmarkResult;
  lLoadStr, lReadStr, lWriteStr, lSaveStr, lWriteSaveStr: string;
  lLoadMaxWidth, lReadMaxWidth, lWriteMaxWidth, lSaveMaxWidth, lWriteSaveMaxWidth: Integer;
  lSaveSdStr: string;
  lSaveMedianStr: string;
begin
  Writeln(aTitle);
  lLoadMaxWidth:= 0;
  lReadMaxWidth:= 0;
  lWriteMaxWidth := 0;
  lSaveMaxWidth:= 0;
  lWriteSaveMaxWidth := 0;

  for lResult in aResults do
  begin
    lLoadStr := FormatFloat('0.000000', lResult.Load.MeanMs) + ' ms';
    lReadStr := FormatFloat('0.000000', lResult.Read.MeanMs) + ' ms';
    lWriteStr := FormatFloat('0.000000', lResult.Write.MeanMs) + ' ms';
    if lResult.Save.MeanMs >= 0 then
      lSaveStr := FormatFloat('0.000000', lResult.Save.MeanMs) + ' ms'
    else
      lSaveStr := '(n/a)';
    lWriteSaveStr := FormatFloat('0.000000', lResult.WriteSave.MeanMs) + ' ms';

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

  Writeln('Benchmark Results (mean +/- stdev; median) of ', Iterations, ' runs; warmup=', WarmupIterations);
  Writeln('----------------------------------------------');
  for lResult in aResults do
  begin
    lLoadStr := putBefore(FormatFloat('0.000000', lResult.Load.MeanMs) + ' ms', ' ', lLoadMaxWidth);
    lReadStr := putBefore(FormatFloat('0.000000', lResult.Read.MeanMs) + ' ms', ' ', lReadMaxWidth);
    lWriteStr := putBefore(FormatFloat('0.000000', lResult.Write.MeanMs) + ' ms', ' ', lWriteMaxWidth);
    if lResult.Save.MeanMs >= 0 then
      lSaveStr := putBefore(FormatFloat('0.000000', lResult.Save.MeanMs) + ' ms', ' ', lSaveMaxWidth)
    else
      lSaveStr := putBefore('(n/a)', ' ', lSaveMaxWidth);
    lWriteSaveStr := putBefore(FormatFloat('0.000000', lResult.WriteSave.MeanMs) + ' ms', ' ', lWriteSaveMaxWidth);

    Writeln(Format('%-15s Load: %s  Read: %s  Write: %s  Save: %s  Write+Save: %s',
      [lResult.Name, lLoadStr, lReadStr, lWriteStr, lSaveStr, lWriteSaveStr]));

    if lResult.Save.MeanMs >= 0 then
      lSaveSdStr := FormatFloat('0.000000', lResult.Save.StdDevMs) + ' ms'
    else
      lSaveSdStr := '(n/a)';
    if lResult.Save.MeanMs >= 0 then
      lSaveMedianStr := FormatFloat('0.000000', lResult.Save.MedianMs) + ' ms'
    else
      lSaveMedianStr := '(n/a)';

    Writeln(Format('%-15s sd:  %s  sd:  %s  sd:  %s  sd:  %s  sd:  %s',
      ['',
       putBefore(FormatFloat('0.000000', lResult.Load.StdDevMs) + ' ms', ' ', lLoadMaxWidth),
       putBefore(FormatFloat('0.000000', lResult.Read.StdDevMs) + ' ms', ' ', lReadMaxWidth),
       putBefore(FormatFloat('0.000000', lResult.Write.StdDevMs) + ' ms', ' ', lWriteMaxWidth),
       putBefore(lSaveSdStr, ' ', lSaveMaxWidth),
       putBefore(FormatFloat('0.000000', lResult.WriteSave.StdDevMs) + ' ms', ' ', lWriteSaveMaxWidth)]));
    Writeln(Format('%-15s med: %s  med: %s  med: %s  med: %s  med: %s',
      ['',
       putBefore(FormatFloat('0.000000', lResult.Load.MedianMs) + ' ms', ' ', lLoadMaxWidth),
       putBefore(FormatFloat('0.000000', lResult.Read.MedianMs) + ' ms', ' ', lReadMaxWidth),
       putBefore(FormatFloat('0.000000', lResult.Write.MedianMs) + ' ms', ' ', lWriteMaxWidth),
       putBefore(lSaveMedianStr, ' ', lSaveMaxWidth),
       putBefore(FormatFloat('0.000000', lResult.WriteSave.MedianMs) + ' ms', ' ', lWriteSaveMaxWidth)]));
  end;
  Writeln;
end;

var
  lDataset: TDataset;
  lResults: TArray<TBenchmarkResult>;
  lStringResults: TArray<TBenchmarkResult>;
begin
  try
    Iterations := GetIntParam('iterations', Iterations);
    WarmupIterations := GetIntParam('warmup', WarmupIterations);
    if Iterations < 1 then
      Iterations := 1;
    if WarmupIterations < 0 then
      WarmupIterations := 0;

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
    // Readln;
  except
    on E: Exception do
    begin
      Writeln('Benchmark failed: ', E.ClassName, ': ', E.Message);
      //Readln;
    end;
  end;
end.
