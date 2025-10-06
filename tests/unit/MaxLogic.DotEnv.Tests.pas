unit MaxLogic.DotEnv.Tests;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.StrUtils,
  System.Generics.Collections,
  maxlogic.DotEnv;

type
  [TestFixture]
  TMaxLogicDotEnvTests = class
  strict private
    function MakeTempDir(const ASubFolder: string = ''): string;
    procedure RemoveDirRecursive(const APath: string);
    procedure WriteText(const AFile, AContent: string);
    function BuildPath(const ABase: string; const Parts: array of string): string;
    function CollectValue(const DotEnv: TDotEnv; const Key: string): string;
  public
    [Test] procedure Interpolation_Simple;
    [Test] procedure Interpolation_Default;
    [Test] procedure InlineComment_Ignored;
    [Test] procedure StrictUndefined_Raises;
    [Test] procedure Expressions_Arithmetic;
    [Test] procedure Expressions_JoinEnv;
    [Test] procedure Expressions_Boolean;
    [Test] procedure Builtins_CurrentFileAndDir;
    [Test] procedure Builtins_AppPaths;
    [Test] procedure Includes_Override;
    [Test] procedure Includes_Cycle;
    [Test] procedure Layered_LocalOverridesBase;
    [Test] procedure MultiLine_TripleQuotes;
    [Test] procedure MultiLine_Heredoc;
    [Test] procedure CommandSubstitution_Enabled;
    [Test] procedure CommandSubstitution_Disabled;
    [Test] procedure ProcessEnv_DoNotOverrideExisting;
    [Test] procedure Search_ParentsRespectsDepth;
    [Test] procedure Search_XdgHome;
    [Test] procedure Search_WindowsProfile;
    [Test] procedure Search_HomeOverridesByLayer;
    [Test] procedure Security_OversizeFile;
    [Test] procedure Security_SecretPermissions;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows;
{$ELSE}
  Posix.Stdlib,
  Posix.SysStat,
  Posix.Unistd,
  Posix.SysTypes;
{$ENDIF}

const
  ONE_MB = 1024 * 1024;

{ TMaxLogicDotEnvTests }

function TMaxLogicDotEnvTests.BuildPath(const ABase: string; const Parts: array of string): string;
var
  Part: string;
begin
  Result := ABase;
  for Part in Parts do
    Result := TPath.Combine(Result, Part);
end;

function TMaxLogicDotEnvTests.CollectValue(const DotEnv: TDotEnv; const Key: string): string;
begin
  Result := '';
  DotEnv.TryGetValue(Key, Result);
end;

function TMaxLogicDotEnvTests.MakeTempDir(const ASubFolder: string): string;
var
  GUIDStr: string;
  Base: string;
begin
  Base := TPath.GetTempPath;
  GUIDStr := TGUID.NewGuid.ToString.Replace('{', '').Replace('}', '');
  if ASubFolder <> '' then
    Result := BuildPath(Base, [ASubFolder + '-' + GUIDStr])
  else
    Result := BuildPath(Base, ['DotEnv-' + GUIDStr]);
  TDirectory.CreateDirectory(Result);
end;

procedure TMaxLogicDotEnvTests.RemoveDirRecursive(const APath: string);
begin
  if TDirectory.Exists(APath) then
    TDirectory.Delete(APath, True);
end;

procedure TMaxLogicDotEnvTests.WriteText(const AFile, AContent: string);
begin
  TDirectory.CreateDirectory(TPath.GetDirectoryName(AFile));
  TFile.WriteAllText(AFile, AContent, TEncoding.UTF8);
end;

procedure TMaxLogicDotEnvTests.Interpolation_Simple;
var
  TempDir, EnvFile, Expected: string;
  DotEnv: TDotEnv;
  Value: string;
begin
  TempDir := MakeTempDir('interp');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'A=1'#10'B=${A}'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.IsTrue(DotEnv.TryGetValue('B', Value));
      Assert.AreEqual('1', Value);
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Interpolation_Default;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('default');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'VALUE=${MISSING:-fallback}'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('fallback', CollectValue(DotEnv, 'VALUE'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.InlineComment_Ignored;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('inline');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'PORT=8080 # dev server'#10'# trailing comment'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('8080', CollectValue(DotEnv, 'PORT'));
      Assert.IsFalse(DotEnv.TryGetValue('#', EnvFile), 'Inline comment should not produce a key');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.StrictUndefined_Raises;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  Found: Boolean;
  Err: TDotEnvError;
begin
  TempDir := MakeTempDir('strict');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'VALUE=${UNSET}'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], [TDotEnvOption.StrictUndefined]);
      Errors := DotEnv.GetErrors;
      Found := False;
      for Err in Errors do
        if Err.Kind = TDotEnvErrorKind.DekStrict then
        begin
          Found := True;
          Break;
        end;
      Assert.IsTrue(Found, 'Strict undefined error expected');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Expressions_Arithmetic;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('expr-arith');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'VALUE=$[ 2 + 2 * 3 ]'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('8', CollectValue(DotEnv, 'VALUE'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Expressions_JoinEnv;
var
  TempDir, EnvFile, Expected: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('expr-join');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'ENV=dev'#10'HOST=$[ join(APP_DIR, ''configs'', env(''ENV'')) ]'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Expected := ExcludeTrailingPathDelimiter(TPath.GetDirectoryName(TPath.GetFullPath(ParamStr(0))));
      Expected := TPath.Combine(Expected, 'configs');
      Expected := TPath.Combine(Expected, 'dev');
      Expected := TPath.GetFullPath(Expected);
      Assert.AreEqual(Expected, CollectValue(DotEnv, 'HOST'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Expressions_Boolean;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('expr-bool');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'VALUE=$[ (1 < 2) && (''a'' == ''a'') ]'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('true', CollectValue(DotEnv, 'VALUE'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Builtins_CurrentFileAndDir;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
  Value: string;
begin
  TempDir := MakeTempDir('builtins');
  try
    EnvFile := BuildPath(TempDir, ['config.env']);
    WriteText(EnvFile, 'HERE=${CUR_DIR}'#10'FILE=${CUR_FILE}'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Value := CollectValue(DotEnv, 'HERE');
      Assert.AreEqual(TPath.GetDirectoryName(TPath.GetFullPath(EnvFile)), Value);
      Value := CollectValue(DotEnv, 'FILE');
      Assert.AreEqual(TPath.GetFullPath(EnvFile), Value);
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Builtins_AppPaths;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
  Exe, Dir: string;
begin
  TempDir := MakeTempDir('builtins-app');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'EXE=${APP_EXE}'#10'DIR=${APP_DIR}'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Exe := CollectValue(DotEnv, 'EXE');
      Dir := CollectValue(DotEnv, 'DIR');
      Assert.AreEqual(TPath.GetFullPath(ParamStr(0)), Exe);
      Assert.AreEqual(ExcludeTrailingPathDelimiter(TPath.GetDirectoryName(TPath.GetFullPath(ParamStr(0)))), Dir);
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Includes_Override;
var
  TempDir, ParentEnv, ChildEnv: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('include');
  try
    ParentEnv := BuildPath(TempDir, ['main.env']);
    ChildEnv := BuildPath(TempDir, ['child.env']);
    WriteText(ParentEnv, '#include "child.env"'#10'X=2'#10);
    WriteText(ChildEnv, 'X=1'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([ParentEnv], []);
      Assert.AreEqual('2', CollectValue(DotEnv, 'X'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Includes_Cycle;
var
  TempDir, AFile, BFile: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  HasCycle: Boolean;
  Err: TDotEnvError;
begin
  TempDir := MakeTempDir('cycle');
  try
    AFile := BuildPath(TempDir, ['a.env']);
    BFile := BuildPath(TempDir, ['b.env']);
    WriteText(AFile, '#include "b.env"'#10);
    WriteText(BFile, '#include "a.env"'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([AFile], []);
      Errors := DotEnv.GetErrors;
      HasCycle := False;
      for Err in Errors do
        if Err.Kind = TDotEnvErrorKind.DekCycle then
        begin
          HasCycle := True;
          Break;
        end;
      Assert.IsTrue(HasCycle, 'Cycle detection expected');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Layered_LocalOverridesBase;
var
  TempDir: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('layered');
  try
    WriteText(BuildPath(TempDir, ['.env']), 'K=base'#10);
    WriteText(BuildPath(TempDir, ['.env.local']), 'K=local'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadLayered(TempDir, []);
      Assert.AreEqual('local', CollectValue(DotEnv, 'K'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.MultiLine_TripleQuotes;
const
  ValueText = 'line1'#10'line2';
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('multiline');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'LICENSE="""'#10'line1'#10'line2'#10'"""'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual(ValueText, CollectValue(DotEnv, 'LICENSE'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.MultiLine_Heredoc;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('heredoc');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'TOKEN<<END'#10'line1'#10'line2'#10'END'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('line1'#10'line2', CollectValue(DotEnv, 'TOKEN'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.CommandSubstitution_Enabled;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
  Value: string;
begin
  TempDir := MakeTempDir('cmd');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'USER=$(echo dotenv)'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], [TDotEnvOption.AllowCommandSubst]);
      Value := CollectValue(DotEnv, 'USER');
      Assert.IsTrue(Value.StartsWith('dotenv', True), 'Command substitution expected');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.CommandSubstitution_Disabled;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  Err: TDotEnvError;
  HasWarning: Boolean;
begin
  TempDir := MakeTempDir('cmd-disabled');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'USER=$(echo dotenv)'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Errors := DotEnv.GetErrors;
      HasWarning := False;
      for Err in Errors do
        if Err.Kind = TDotEnvErrorKind.DekCommand then
        begin
          HasWarning := True;
          Break;
        end;
      Assert.IsTrue(HasWarning, 'Command warning expected');
      Assert.AreEqual('$(echo dotenv)', CollectValue(DotEnv, 'USER'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.ProcessEnv_DoNotOverrideExisting;
const
  EnvName = 'MAXLOGIC_EXISTING';
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
  Original, Value: string;
begin
  TempDir := MakeTempDir('process');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, EnvName + '=file'#10);
    Original := GetEnvironmentVariable(EnvName);
    {$IFDEF MSWINDOWS}
    SetEnvironmentVariable(PChar(EnvName), PChar('process'));
    {$ELSE}
    setenv(PAnsiChar(AnsiString(EnvName)), PAnsiChar(AnsiString('process')), 1);
    {$ENDIF}
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], [TDotEnvOption.DoNotOverrideExisting]);
      Assert.IsFalse(DotEnv.TryGetValue(EnvName, Value));
      Assert.AreEqual('process', GetEnvironmentVariable(EnvName));
    finally
      DotEnv.Free;
{$IFDEF MSWINDOWS}
      if Original = '' then
        SetEnvironmentVariable(PChar(EnvName), nil)
      else
        SetEnvironmentVariable(PChar(EnvName), PChar(Original));
{$ELSE}
      if Original = '' then
        unsetenv(PAnsiChar(AnsiString(EnvName)))
      else
        setenv(PAnsiChar(AnsiString(EnvName)), PAnsiChar(AnsiString(Original)), 1);
      {$ENDIF}
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Search_ParentsRespectsDepth;
var
  Root, Child, Deeper, Value: string;
  DotEnv: TDotEnv;
begin
  Root := MakeTempDir('parents');
  try
    WriteText(BuildPath(Root, ['.env']), 'TOP=value'#10);
    Child := BuildPath(Root, ['a', 'b', 'c', 'd']);
    TDirectory.CreateDirectory(Child);

    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadLayered(Child, [TDotEnvOption.SearchParents]);
      Assert.AreEqual('value', CollectValue(DotEnv, 'TOP'));
    finally
      DotEnv.Free;
    end;

    Deeper := BuildPath(Root, ['a', 'b', 'c', 'd', 'e', 'f']);
    TDirectory.CreateDirectory(Deeper);

    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadLayered(Deeper, [TDotEnvOption.SearchParents]);
      Assert.IsFalse(DotEnv.TryGetValue('TOP', Value));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(Root);
  end;
end;

procedure TMaxLogicDotEnvTests.Search_XdgHome;
{$IFNDEF MSWINDOWS}
var
  TempBase, ConfigHome: string;
  DotEnv: TDotEnv;
  SavedHome, SavedDirs: string;
begin
  TempBase := MakeTempDir('xdg');
  try
    ConfigHome := BuildPath(TempBase, ['xdg', 'maxlogic']);
    WriteText(BuildPath(ConfigHome, ['.env']), 'VALUE=xdg'#10);
    SavedHome := GetEnvironmentVariable('XDG_CONFIG_HOME');
    SavedDirs := GetEnvironmentVariable('XDG_CONFIG_DIRS');
    setenv(PAnsiChar(AnsiString('XDG_CONFIG_HOME')),
      PAnsiChar(AnsiString(ExcludeTrailingPathDelimiter(BuildPath(TempBase, ['xdg'])))), 1);
    setenv(PAnsiChar(AnsiString('XDG_CONFIG_DIRS')), PAnsiChar(AnsiString('')), 1);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadLayered(BuildPath(TempBase, ['proj']), [TDotEnvOption.SearchXDG]);
      Assert.AreEqual('xdg', CollectValue(DotEnv, 'VALUE'));
    finally
      DotEnv.Free;
    if SavedHome = '' then
      unsetenv(PAnsiChar(AnsiString('XDG_CONFIG_HOME')))
    else
      setenv(PAnsiChar(AnsiString('XDG_CONFIG_HOME')), PAnsiChar(AnsiString(SavedHome)), 1);
    if SavedDirs = '' then
      unsetenv(PAnsiChar(AnsiString('XDG_CONFIG_DIRS')))
    else
      setenv(PAnsiChar(AnsiString('XDG_CONFIG_DIRS')), PAnsiChar(AnsiString(SavedDirs)), 1);
    end;
  finally
    RemoveDirRecursive(TempBase);
  end;
end;
{$ELSE}
begin
  Assert.Pass('XDG search applies only to POSIX systems');
end;
{$ENDIF}

procedure TMaxLogicDotEnvTests.Search_WindowsProfile;
{$IFDEF MSWINDOWS}
var
  TempBase, AppDataPath: string;
  DotEnv: TDotEnv;
  SavedAppData: string;
begin
  TempBase := MakeTempDir('winprofile');
  try
    AppDataPath := BuildPath(TempBase, ['AppData']);
    WriteText(BuildPath(AppDataPath, ['MaxLogic', '.env']), 'VALUE=appdata'#10);
    SavedAppData := GetEnvironmentVariable('APPDATA');
    SetEnvironmentVariable('APPDATA', PChar(AppDataPath));
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadLayered(BuildPath(TempBase, ['project']), [TDotEnvOption.SearchWindowsProfile]);
      Assert.AreEqual('appdata', CollectValue(DotEnv, 'VALUE'));
    finally
      DotEnv.Free;
      if SavedAppData = '' then
        SetEnvironmentVariable('APPDATA', nil)
      else
        SetEnvironmentVariable('APPDATA', PChar(SavedAppData));
    end;
  finally
    RemoveDirRecursive(TempBase);
  end;
end;
{$ELSE}
begin
  Assert.Pass('Windows profile search applies only on Windows');
end;
{$ENDIF}

procedure TMaxLogicDotEnvTests.Search_HomeOverridesByLayer;
var
  TempBase, HomeDir: string;
  DotEnv: TDotEnv;
  SavedHome: string;
begin
  TempBase := MakeTempDir('home');
  try
    HomeDir := BuildPath(TempBase, ['home']);
    WriteText(BuildPath(HomeDir, ['.env']), 'VALUE=home'#10);
    WriteText(BuildPath(TempBase, ['project', '.env.local']), 'VALUE=project'#10);
    SavedHome := GetEnvironmentVariable('HOME');
    {$IFDEF MSWINDOWS}
    SetEnvironmentVariable('HOME', PChar(HomeDir));
    {$ELSE}
    setenv(PAnsiChar(AnsiString('HOME')), PAnsiChar(AnsiString(HomeDir)), 1);
    {$ENDIF}
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadLayered(BuildPath(TempBase, ['project']), [TDotEnvOption.SearchUserHome]);
      Assert.AreEqual('project', CollectValue(DotEnv, 'VALUE'));
    finally
      DotEnv.Free;
      {$IFDEF MSWINDOWS}
      if SavedHome = '' then
        SetEnvironmentVariable('HOME', nil)
      else
        SetEnvironmentVariable('HOME', PChar(SavedHome));
      {$ELSE}
      if SavedHome = '' then
        unsetenv(PAnsiChar(AnsiString('HOME')))
      else
        setenv(PAnsiChar(AnsiString('HOME')), PAnsiChar(AnsiString(SavedHome)), 1);
      {$ENDIF}
    end;
  finally
    RemoveDirRecursive(TempBase);
  end;
end;

procedure TMaxLogicDotEnvTests.Security_OversizeFile;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
  Oversize: string;
  Errors: TArray<TDotEnvError>;
  Err: TDotEnvError;
  Found: Boolean;
begin
  TempDir := MakeTempDir('oversize');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    Oversize := StringOfChar('A', ONE_MB + 10);
    WriteText(EnvFile, Oversize);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Errors := DotEnv.GetErrors;
      Found := False;
      for Err in Errors do
        if Err.Kind = TDotEnvErrorKind.DekSecurity then
        begin
          Found := True;
          Break;
        end;
      Assert.IsTrue(Found, 'Oversize security error expected');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Security_SecretPermissions;
{$IFNDEF MSWINDOWS}
var
  TempDir, SecretFile: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  Err: TDotEnvError;
  HasSecurityError: Boolean;
begin
  TempDir := MakeTempDir('secret');
  try
    SecretFile := BuildPath(TempDir, ['.env.secret']);
    WriteText(SecretFile, 'KEY=secret'#10);
    // world-readable
    fpchmod(PAnsiChar(AnsiString(SecretFile)), S_IRUSR or S_IWUSR or S_IRGRP);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadLayered(TempDir, []);
      Errors := DotEnv.GetErrors;
      HasSecurityError := False;
      for Err in Errors do
        if Err.Kind = TDotEnvErrorKind.DekSecurity then
        begin
          HasSecurityError := True;
          Break;
        end;
      Assert.IsTrue(HasSecurityError, 'Secret permissions error expected');
    finally
      DotEnv.Free;
    end;
    fpchmod(PAnsiChar(AnsiString(SecretFile)), S_IRUSR or S_IWUSR);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadLayered(TempDir, []);
      Assert.AreEqual('secret', CollectValue(DotEnv, 'KEY'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;
{$ELSE}
begin
  Assert.Pass('POSIX-only permission check');
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TMaxLogicDotEnvTests);

end.
