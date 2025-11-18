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
    function DumpDiag(const DotEnv: TDotEnv): string;
  public
    [Test] procedure Interpolation_Simple;
    [Test] procedure Interpolation_Default;
    [Test] procedure InlineComment_Ignored;
    [Test] procedure StrictUndefined_Raises;
    [Test] procedure Expressions_Arithmetic;
    [Test] procedure Expressions_JoinEnv;
    [Test] procedure Expressions_Boolean;
    [Test] procedure Expressions_UnknownFunction_Error;
    [Test] procedure Expressions_DivisionByZero_Error;
    [Test] procedure Expressions_NestedSquareBrackets;
    [Test] procedure Expressions_StringEscapes_SingleQuotes;
    [Test] procedure Expressions_Upper_Lower_Trim;
    [Test] procedure Builtins_CurrentFileAndDir;
    [Test] procedure Builtins_AppPaths;
    [Test] procedure Builtins_DirSep_AppPaths;
    [Test] procedure Builtins_CurDir_File_Includes;
    [Test] procedure Builtins_Expand_WithoutSourceFile_CurDirAndCurFile;
    [Test] procedure Builtins_ProcessEnv_Overrides_Builtins;
    [Test] procedure Builtins_OnResolveSymbol_DoesNotOverride_Builtins;
    [Test] procedure Includes_Override;
    [Test] procedure Includes_Cycle;
    [Test] procedure Layered_LocalOverridesBase;
    [Test] procedure MultiLine_TripleQuotes;
    [Test] procedure MultiLine_Heredoc;
    [Test] procedure CommandSubstitution_Enabled;
    [Test] procedure CommandSubstitution_Disabled;
    [Test] procedure ProcessEnv_DoNotOverrideExisting;
    [Test] procedure ProcessEnv_EmptyValues_TreatedAsUnset;
    [Test] procedure ProcessEnv_OnResolveSymbol_Order;
    [Test] procedure Search_ParentsRespectsDepth;
    [Test] procedure Search_Default_IncludesParents;
    [Test] procedure Search_Parents_OptOut_ZeroDepth;
    [Test] procedure Search_XdgHome;
    [Test] procedure Search_WindowsProfile;
    [Test] procedure Search_HomeOverridesByLayer;
    [Test] procedure Security_OversizeFile;
    [Test] procedure Security_SecretPermissions;
    [Test] procedure Evaluation_AfterMerge_CrossRoot_Dependency;
    [Test] procedure Evaluation_Streaming_CrossRoot_Dependency;
    [Test] procedure Evaluation_CycleDetected;
    [Test] procedure Unquoted_WindowsPath_DoubleBackslash_Preserves;
    [Test] procedure Unquoted_WindowsPath_SingleBackslash_TabPitfall;
    [Test] procedure EscapedDollar_DeferredExpand;
    [Test] procedure DoNotOverrideExisting_AfterMerge_UsesProcessEnvInDerived;
    [Test] procedure Includes_Optional_Missing_NoError;
    [Test] procedure CommandSubstitution_Windows_QuotedPathWithSpaces;
    [Test] procedure CommandSubstitution_Windows_QuotedPathWithSpaces_NonZeroExit;
    [Test] procedure Security_SecretPermissions_CaseInsensitive;
    [Test] procedure Keys_CaseInsensitive_Merge_LastWins;
    [Test] procedure Keys_CaseSensitive_Option_Overrides;
    [Test] procedure Keys_CaseSensitivity_DefaultPlatform;
    [Test] procedure Keys_CaseSensitivity_And_Collisions;

    // Value syntax
    [Test] procedure Value_Unquoted_CommentEscapes;
    [Test] procedure Value_Unquoted_Semicolon_NotCommentInline;
    [Test] procedure Value_Unquoted_TrailingWhitespace;
    [Test] procedure Value_Unquoted_LineContinuation;
    [Test] procedure Value_DoubleQuoted_Escapes;
    [Test] procedure Value_TripleQuoted_LeadingNewline_NoInitialNewline;
    [Test] procedure Value_Heredoc_Expansion_DefaultsAndStrict;
    [Test] procedure EscapedDollar_MiddleOfString_DeferredExpand;

    // Error reporting
    [Test] procedure Error_Parse_InvalidKey_Continue;
    [Test] procedure Error_Parse_MissingClosingBrace;
    [Test] procedure Error_Parse_UnterminatedExpression;
    [Test] procedure Include_InvalidDirective_MissingQuotes;

    // Includes
    [Test] procedure Includes_RelativeTraversal_FromSubdir;
    [Test] procedure Includes_OptionalPresent_LoadsAndOverrides;
    [Test] procedure Includes_Cycle_ThreeFiles_NoValues;

    // Layered precedence
    [Test] procedure Layered_SecretOverridesLocal;
    [Test] procedure Layered_SecretMixedCase_OverridesLocal_POSIX;

    // Evaluation modes
    [Test] procedure Evaluation_AfterMerge_ForwardReference_SingleFile;
    [Test] procedure Evaluation_Streaming_ForwardReference_SingleFile;
    [Test] procedure Evaluation_Streaming_ForwardReference_StrictUndefined_Fatal;

    // Search roots and custom roots
    [Test] procedure Search_XdgDirs_Multiple_OrderAndLoad;
    [Test] procedure Search_CustomRoots_Precision_And_Order;
    [Test] procedure Search_CustomNamespace_OverridesDefault;

    // API consistency
    [Test] procedure API_AsDictionary_PreservesOriginalCasing;

    // Command substitution edge cases
    [Test] procedure CommandSubstitution_NestedParentheses;
    [Test] procedure CommandSubstitution_Disabled_LiteralInMiddle;
    [Test] procedure CommandSubstitution_Windows_UTF8BOM_Batch;

    // Built-ins
    [Test] procedure Builtins_Expand_WithSourceFile_UsesProvidedPath;
    [Test] procedure OnResolveSymbol_Receives_FilePath;

    // Process env semantics
    [Test] procedure DoNotOverrideExisting_Streaming_UsesProcessEnvInDerived;

    // Expressions
    [Test] procedure Expressions_Modulo_And_ModuloByZero_Error;
    [Test] procedure Expressions_UnaryMinus_And_StringConcat;
    [Test] procedure Expressions_PathFunctions;
    [Test] procedure Expressions_Join_NoArgs_Error;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows
{$ELSE}
  Posix.Stdlib, Posix.Stdio, Posix.SysStat, Posix.SysTypes, Posix.Unistd
{$ENDIF}
  ;

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

function TMaxLogicDotEnvTests.DumpDiag(const DotEnv: TDotEnv): string;
var
  SB: TStringBuilder;
  Errs: TArray<TDotEnvError>;
  E: TDotEnvError;
  Tr: TArray<string>;
  S: string;

  function ErrKindToText(const K: TDotEnvErrorKind): string;
  begin
    case K of
      TDotEnvErrorKind.DekParse:    Result := 'Parse';
      TDotEnvErrorKind.DekStrict:   Result := 'Strict';
      TDotEnvErrorKind.DekCycle:    Result := 'Cycle';
      TDotEnvErrorKind.DekIO:       Result := 'IO';
      TDotEnvErrorKind.DekSecurity: Result := 'Security';
      TDotEnvErrorKind.DekCommand:  Result := 'Command';
      TDotEnvErrorKind.DekWarning:  Result := 'Warning';
    else
      Result := 'Unknown';
    end;
  end;
begin
  SB := TStringBuilder.Create;
  try
    Tr := DotEnv.GetTrace;
    SB.AppendLine('Trace:');
    for S in Tr do
      SB.AppendLine('  ' + S);

    Errs := DotEnv.GetErrors;
    if Length(Errs) > 0 then
    begin
      SB.AppendLine('Errors:');
      for E in Errs do
        SB.AppendLine(Format('  [%s] %s(%d:%d): %s (fatal=%s)',
          [ErrKindToText(E.Kind), E.FilePath, E.Line, E.Column, E.Message, BoolToStr(E.Fatal, True)]));
    end;

    Result := SB.ToString;
  finally
    SB.Free;
  end;
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

procedure TMaxLogicDotEnvTests.Expressions_UnknownFunction_Error;
var
  TempDir, EnvFile, Dummy: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  HasParseFatal: Boolean;
  Err: TDotEnvError;
begin
  TempDir := MakeTempDir('expr-unknown');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'BAD=$[ nope(1) ]'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      // Key should not be stored when expression contains unknown function
      Assert.IsFalse(DotEnv.TryGetValue('BAD', Dummy), 'Unknown function must prevent storing the key');
      // DekParse fatal should be recorded
      Errors := DotEnv.GetErrors;
      HasParseFatal := False;
      for Err in Errors do
        if (Err.Kind = TDotEnvErrorKind.DekParse) and Err.Fatal then
        begin
          HasParseFatal := True;
          Break;
        end;
      Assert.IsTrue(HasParseFatal, 'DekParse fatal expected for unknown function');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Expressions_DivisionByZero_Error;
var
  TempDir, EnvFile, Dummy: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  HasParseFatal: Boolean;
  Err: TDotEnvError;
begin
  TempDir := MakeTempDir('expr-div-zero');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'BAD=$[ 1 / 0 ]'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      // Key should not be stored when division by zero occurs
      Assert.IsFalse(DotEnv.TryGetValue('BAD', Dummy), 'Division by zero must prevent storing the key');
      // DekParse fatal with a helpful message expected
      Errors := DotEnv.GetErrors;
      HasParseFatal := False;
      for Err in Errors do
        if (Err.Kind = TDotEnvErrorKind.DekParse) and Err.Fatal and
           ContainsText(Err.Message, 'division by zero') then
        begin
          HasParseFatal := True;
          Break;
        end;
      Assert.IsTrue(HasParseFatal, 'DekParse fatal "Division by zero" expected');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Expressions_NestedSquareBrackets;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('expr-nested');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'VAL=$[ 1 + $[ 2 + 3 ] ]'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('6', CollectValue(DotEnv, 'VAL'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Expressions_StringEscapes_SingleQuotes;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('expr-str-esc');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'S=$[ ''a''''b'' ]'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('a''b', CollectValue(DotEnv, 'S'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Expressions_Upper_Lower_Trim;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('expr-up-low-trim');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile,
      'UP=$[ upper(''MiXeD'') ]'#10 +
      'LOW=$[ lower(''MiXeD'') ]'#10 +
      'TRIM=$[ trim(''  spaced  '') ]'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('MIXED', CollectValue(DotEnv, 'UP'));
      Assert.AreEqual('mixed', CollectValue(DotEnv, 'LOW'));
      Assert.AreEqual('spaced', CollectValue(DotEnv, 'TRIM'));
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

procedure TMaxLogicDotEnvTests.Builtins_DirSep_AppPaths;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
  Sep, CombUnq, CombQuot, Expected, AppDir: string;
begin
  TempDir := MakeTempDir('builtins-dirsep');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile,
      'SEP=${DIR_SEP}'#10 +
      'COMB_UNQ=${APP_DIR}${DIR_SEP}configs'#10 +
      'COMB_QUOT="${APP_DIR}${DIR_SEP}configs"'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      // DIR_SEP validation
      Sep := CollectValue(DotEnv, 'SEP');
      Assert.AreEqual(string(TPath.DirectorySeparatorChar), Sep, 'DIR_SEP builtin failed');

      // Combine APP_DIR + DIR_SEP + 'configs' in unquoted and quoted contexts
      AppDir := ExcludeTrailingPathDelimiter(TPath.GetDirectoryName(TPath.GetFullPath(ParamStr(0))));
      Expected := TPath.Combine(AppDir, 'configs');

      CombUnq := CollectValue(DotEnv, 'COMB_UNQ');
      CombQuot := CollectValue(DotEnv, 'COMB_QUOT');
      Assert.AreEqual(Expected, CombUnq, 'Unquoted combination failed');
      Assert.AreEqual(Expected, CombQuot, 'Quoted combination failed');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Builtins_CurDir_File_Includes;
var
  TempDir, SubDir, MainEnv, ChildEnv, IncludeSpec: string;
  DotEnv: TDotEnv;
  ExpectedDir, ExpectedFile: string;
begin
  TempDir := MakeTempDir('curdir-include');
  try
    SubDir := BuildPath(TempDir, ['sub']);
    TDirectory.CreateDirectory(SubDir);
    MainEnv := BuildPath(TempDir, ['main.env']);
    ChildEnv := BuildPath(SubDir, ['child.env']);
    IncludeSpec := 'sub' + TPath.DirectorySeparatorChar + 'child.env';
    WriteText(MainEnv, '#include "' + IncludeSpec + '"'#10);
    WriteText(ChildEnv, 'DIR=${CUR_DIR}'#10'FILE=${CUR_FILE}'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([MainEnv], []);
      ExpectedDir := TPath.GetDirectoryName(TPath.GetFullPath(ChildEnv));
      ExpectedFile := TPath.GetFullPath(ChildEnv);
      Assert.AreEqual(ExpectedDir, CollectValue(DotEnv, 'DIR'));
      Assert.AreEqual(ExpectedFile, CollectValue(DotEnv, 'FILE'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Builtins_Expand_WithoutSourceFile_CurDirAndCurFile;
var
  DotEnv: TDotEnv;
  Cwd: string;
begin
  DotEnv := TDotEnv.Create;
  try
    Cwd := TDirectory.GetCurrentDirectory;
    // CUR_DIR falls back to process current directory when no source file is provided
    Assert.AreEqual(Cwd, DotEnv.Expand('${CUR_DIR}'));
    // CUR_FILE is empty when not evaluating within a file
    Assert.AreEqual('', DotEnv.Expand('${CUR_FILE}'));
    // APP_EXE/APP_DIR remain available even without a source file
    Assert.AreEqual(TPath.GetFullPath(ParamStr(0)), DotEnv.Expand('${APP_EXE}'));
    Assert.AreEqual(
      ExcludeTrailingPathDelimiter(TPath.GetDirectoryName(TPath.GetFullPath(ParamStr(0)))),
      DotEnv.Expand('${APP_DIR}')
    );
  finally
    DotEnv.Free;
  end;
end;

procedure TMaxLogicDotEnvTests.Builtins_ProcessEnv_Overrides_Builtins;
var
  TempDir, EnvFile, Original: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('builtins-env-override');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'VAL=${APP_DIR}'#10);
    Original := GetEnvironmentVariable('APP_DIR');
  {$IFDEF MSWINDOWS}
    SetEnvironmentVariable(PChar('APP_DIR'), PChar('ENV_OVERRIDE'));
  {$ELSE}
    setenv(PAnsiChar(AnsiString('APP_DIR')), PAnsiChar(AnsiString('ENV_OVERRIDE')), 1);
  {$ENDIF}
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('ENV_OVERRIDE', CollectValue(DotEnv, 'VAL'),
        'Process env should take precedence over built-in APP_DIR');
    finally
      DotEnv.Free;
    end;
  finally
  {$IFDEF MSWINDOWS}
    if Original = '' then
      SetEnvironmentVariable(PChar('APP_DIR'), nil)
    else
      SetEnvironmentVariable(PChar('APP_DIR'), PChar(Original));
  {$ELSE}
    if Original = '' then
      unsetenv(PAnsiChar(AnsiString('APP_DIR')))
    else
      setenv(PAnsiChar(AnsiString('APP_DIR')), PAnsiChar(AnsiString(Original)), 1);
  {$ENDIF}
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Builtins_OnResolveSymbol_DoesNotOverride_Builtins;
var
  TempDir, EnvFile, Saved: string;
  DotEnv: TDotEnv;
  HadSaved: Boolean;
  ActualExe: string;
begin
  HadSaved := False; // avoid E1036 if an exception occurs before assignment
  Saved := '';
  TempDir := MakeTempDir('builtins-onresolve');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'VAL=${APP_EXE}'#10);

    // Ensure process env doesn't overshadow built-in for this test
    Saved := GetEnvironmentVariable('APP_EXE');
    HadSaved := Saved <> '';
  {$IFDEF MSWINDOWS}
    if HadSaved then
      SetEnvironmentVariable(PChar('APP_EXE'), nil);
  {$ELSE}
    if HadSaved then
      unsetenv(PAnsiChar(AnsiString('APP_EXE')));
  {$ENDIF}

    DotEnv := TDotEnv.Create;
    try
      DotEnv.OnResolveSymbol :=
        function(const AName, AFilePath: string): string
        begin
          if SameText(AName, 'APP_EXE') then
            Result := 'OVERRIDE'
          else
            Result := '';
        end;
      DotEnv.LoadFiles([EnvFile], []);
      ActualExe := TPath.GetFullPath(ParamStr(0));
      Assert.AreEqual(ActualExe, CollectValue(DotEnv, 'VAL'),
        'Built-ins must take precedence over OnResolveSymbol');
    finally
      DotEnv.Free;
    end;
  finally
  {$IFDEF MSWINDOWS}
    if HadSaved then
      SetEnvironmentVariable(PChar('APP_EXE'), PChar(Saved));
  {$ELSE}
    if HadSaved then
      setenv(PAnsiChar(AnsiString('APP_EXE')), PAnsiChar(AnsiString(Saved)), 1);
  {$ENDIF}
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
      Assert.AreEqual('$(echo dotenv)', CollectValue(DotEnv, 'USER'),
        'Command disabled: literal $(...) expected.' + sLineBreak + DumpDiag(DotEnv));
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

procedure TMaxLogicDotEnvTests.ProcessEnv_EmptyValues_TreatedAsUnset;
const
  EnvName = 'DL_ENV_EMPTY_TEST';
  KWithDefault = 'DL_ENV_FALLBACK_A';
  KStrict = 'DL_ENV_STRICT_B';
var
  TempDir, EnvFile, OriginalEnv, OriginalA, V: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  Err: TDotEnvError;
  HasStrict: Boolean;
begin
  TempDir := MakeTempDir('env-empty');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    // A uses default fallback, B has no default (should trigger StrictUndefined)
    WriteText(EnvFile,
      KWithDefault + '=${' + EnvName + ':-fallback}'#10 +
      KStrict      + '=${' + EnvName + '}'#10);

    // Save originals
    OriginalEnv := GetEnvironmentVariable(EnvName);
    OriginalA := GetEnvironmentVariable(KWithDefault);

  {$IFDEF MSWINDOWS}
    // Set the process env var to empty (treated as unset)
    SetEnvironmentVariable(PChar(EnvName), PChar(''));
  {$ELSE}
    setenv(PAnsiChar(AnsiString(EnvName)), PAnsiChar(AnsiString('')), 1);
  {$ENDIF}

    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], [TDotEnvOption.StrictUndefined]);

      // A should pick fallback because empty env is treated as unset
      Assert.AreEqual('fallback', CollectValue(DotEnv, KWithDefault));

      // B should not be stored; DekStrict fatal should be recorded
      Assert.IsFalse(DotEnv.TryGetValue(KStrict, V),
        'B should not be stored when StrictUndefined and env is empty');
      Errors := DotEnv.GetErrors;
      HasStrict := False;
      for Err in Errors do
        if Err.Kind = TDotEnvErrorKind.DekStrict then
        begin
          HasStrict := True;
          Break;
        end;
      Assert.IsTrue(HasStrict, 'DekStrict fatal expected for empty env without default');
    finally
      DotEnv.Free;
    end;
  finally
  {$IFDEF MSWINDOWS}
    if OriginalEnv = '' then
      SetEnvironmentVariable(PChar(EnvName), nil)
    else
      SetEnvironmentVariable(PChar(EnvName), PChar(OriginalEnv));

    if OriginalA = '' then
      SetEnvironmentVariable(PChar(KWithDefault), nil)
    else
      SetEnvironmentVariable(PChar(KWithDefault), PChar(OriginalA));
  {$ELSE}
    if OriginalEnv = '' then
      unsetenv(PAnsiChar(AnsiString(EnvName)))
    else
      setenv(PAnsiChar(AnsiString(EnvName)), PAnsiChar(AnsiString(OriginalEnv)), 1);

    if OriginalA = '' then
      unsetenv(PAnsiChar(AnsiString(KWithDefault)))
    else
      setenv(PAnsiChar(AnsiString(KWithDefault)), PAnsiChar(AnsiString(OriginalA)), 1);
  {$ENDIF}
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.ProcessEnv_OnResolveSymbol_Order;
const
  EnvName = 'DL_ENV_ORD_PROC';
var
  TempDir, EnvFile, Original, ExpectedCurDir: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('env-onresolve-order');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    // K: stored; DER: deferred (post-eval winner should see K's final value)
    // FROMENV: process env should take precedence over OnResolveSymbol
    // BUILT: built-in should take precedence over OnResolveSymbol
    // ONLYONS: fallback to OnResolveSymbol (no other source)
    WriteText(EnvFile,
      'K=file'#10 +
      'DER=${K}'#10 +
      'FROMENV=${' + EnvName + '}'#10 +
      'BUILT=${CUR_DIR}'#10 +
      'ONLYONS=${NO_SUCH}'#10);

    // Prepare process env
    Original := GetEnvironmentVariable(EnvName);
  {$IFDEF MSWINDOWS}
    SetEnvironmentVariable(PChar(EnvName), PChar('proc'));
  {$ELSE}
    setenv(PAnsiChar(AnsiString(EnvName)), PAnsiChar(AnsiString('proc')), 1);
  {$ENDIF}

    DotEnv := TDotEnv.Create;
    try
      // Lowest tier hook: should not override higher tiers
      DotEnv.OnResolveSymbol :=
        function(const AName, AFilePath: string): string
        begin
          Result := 'ONS';
        end;

      DotEnv.LoadFiles([EnvFile], []);
      // stored > everything else
      Assert.AreEqual('file', CollectValue(DotEnv, 'K'));
      // deferred winners (post-eval) > env/builtins/onresolve
      Assert.AreEqual('file', CollectValue(DotEnv, 'DER'));
      // process env > built-ins > onresolve
      Assert.AreEqual('proc', CollectValue(DotEnv, 'FROMENV'));
      // built-ins > onresolve
      ExpectedCurDir := TPath.GetDirectoryName(TPath.GetFullPath(EnvFile));
      Assert.AreEqual(ExpectedCurDir, CollectValue(DotEnv, 'BUILT'));
      // onresolve used only when all other tiers fail
      Assert.AreEqual('ONS', CollectValue(DotEnv, 'ONLYONS'));
    finally
      DotEnv.Free;
    end;
  finally
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

    DotEnv := TDotEnv.Create(1024 * 1024, 4);
    try
      DotEnv.LoadLayered(Child, []);
      Assert.AreEqual('value', CollectValue(DotEnv, 'TOP'));
    finally
      DotEnv.Free;
    end;

    Deeper := BuildPath(Root, ['a', 'b', 'c', 'd', 'e', 'f']);
    TDirectory.CreateDirectory(Deeper);

    DotEnv := TDotEnv.Create(1024 * 1024, 4);
    try
      DotEnv.LoadLayered(Deeper, []);
      Assert.IsFalse(DotEnv.TryGetValue('TOP', Value));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(Root);
  end;
end;

procedure TMaxLogicDotEnvTests.Search_Default_IncludesParents;
var
  ParentDir, ChildDir: string;
  DotEnv: TDotEnv;
begin
  ParentDir := MakeTempDir('parents-default');
  try
    // parent has .env; child has none
    WriteText(BuildPath(ParentDir, ['.env']), 'P=1'#10);
    ChildDir := BuildPath(ParentDir, ['child']);
    TDirectory.CreateDirectory(ChildDir);

    DotEnv := TDotEnv.Create; // default: parent search enabled
    try
      DotEnv.LoadLayered(ChildDir, []);
      Assert.AreEqual('1', CollectValue(DotEnv, 'P'), 'Parent .env should be included by default');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(ParentDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Search_Parents_OptOut_ZeroDepth;
var
  ParentDir, ChildDir: string;
  DotEnv: TDotEnv;
  Value: string;
  Tr: TArray<string>;
  S, ParentEnvFile: string;
  HasParent: Boolean;
begin
  ParentDir := MakeTempDir('parents-optout');
  try
    // parent has .env, child has none
    WriteText(BuildPath(ParentDir, ['.env']), 'KEY=parent'#10);
    ChildDir := BuildPath(ParentDir, ['child']);
    TDirectory.CreateDirectory(ChildDir);

    DotEnv := TDotEnv.Create(1024 * 1024, 0); // zero depth: no parent traversal
    try
      DotEnv.LoadLayered(ChildDir, []);
      // KEY should not be visible from parent
      Assert.IsFalse(DotEnv.TryGetValue('KEY', Value), 'KEY should not be loaded when parent depth is 0');

      // Trace should not include parent root files
      Tr := DotEnv.GetTrace;
      ParentEnvFile := TPath.GetFullPath(BuildPath(ParentDir, ['.env']));
      HasParent := False;
      for S in Tr do
        if S.ToLower.Contains(ParentEnvFile.ToLower) then
        begin
          HasParent := True;
          Break;
        end;
      Assert.IsFalse(HasParent, 'Trace should not include parent root files when depth=0');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(ParentDir);
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
    WriteText(BuildPath(AppDataPath, ['maxlogic', '.env']), 'VALUE=appdata'#10);
    SavedAppData := GetEnvironmentVariable('APPDATA');
    SetEnvironmentVariable(PChar('APPDATA'), PChar(AppDataPath));
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadLayered(BuildPath(TempBase, ['project']), [TDotEnvOption.SearchWindowsProfile]);
      Assert.AreEqual('appdata', CollectValue(DotEnv, 'VALUE'));
    finally
      DotEnv.Free;
      if SavedAppData = '' then
        SetEnvironmentVariable(PChar('APPDATA'), nil)
      else
        SetEnvironmentVariable(PChar('APPDATA'), PChar(SavedAppData));
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
    SetEnvironmentVariable(PChar('HOME'), PChar(HomeDir));
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
        SetEnvironmentVariable(PChar('HOME'), nil)
      else
        SetEnvironmentVariable(PChar('HOME'), PChar(SavedHome));
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
    chmod(PAnsiChar(AnsiString(SecretFile)), S_IRUSR or S_IWUSR or S_IRGRP);
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
    chmod(PAnsiChar(AnsiString(SecretFile)), S_IRUSR or S_IWUSR);
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

procedure TMaxLogicDotEnvTests.Evaluation_AfterMerge_CrossRoot_Dependency;
var
  Root, Child: string;
  DotEnv: TDotEnv;
begin
  Root := MakeTempDir('eval-merge');
  try
    // parent .env defines KEY1 and derives KEY2 from KEY1
    WriteText(BuildPath(Root, ['.env']), 'KEY1=parent'#10'KEY2=${KEY1}'#10);
    // base (child) overrides only KEY1
    Child := BuildPath(Root, ['child']);
    TDirectory.CreateDirectory(Child);
    WriteText(BuildPath(Child, ['.env']), 'KEY1=base'#10);

    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadLayered(Child, []);
      Assert.AreEqual('base', CollectValue(DotEnv, 'KEY1'));
      Assert.AreEqual('base', CollectValue(DotEnv, 'KEY2')); // derived sees final winner
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(Root);
  end;
end;

procedure TMaxLogicDotEnvTests.Evaluation_Streaming_CrossRoot_Dependency;
var
  Root, Child: string;
  DotEnv: TDotEnv;
begin
  Root := MakeTempDir('eval-stream');
  try
    WriteText(BuildPath(Root, ['.env']), 'KEY1=parent'#10'KEY2=${KEY1}'#10);
    Child := BuildPath(Root, ['child']);
    TDirectory.CreateDirectory(Child);
    WriteText(BuildPath(Child, ['.env']), 'KEY1=base'#10);

    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadLayered(Child, [TDotEnvOption.StreamingEvaluation]);
      Assert.AreEqual('base', CollectValue(DotEnv, 'KEY1'));
      Assert.AreEqual('parent', CollectValue(DotEnv, 'KEY2')); // computed earlier in parent
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(Root);
  end;
end;

procedure TMaxLogicDotEnvTests.Evaluation_CycleDetected;
var
  TempDir: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  Err: TDotEnvError;
  HasCycle: Boolean;
  Dummy: string;
begin
  TempDir := MakeTempDir('eval-cycle');
  try
    // A and B reference each other
    WriteText(BuildPath(TempDir, ['.env']), 'A=${B}'#10'B=${A}'#10);

    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadLayered(TempDir, []);
      Errors := DotEnv.GetErrors;
      HasCycle := False;
      for Err in Errors do
        if Err.Kind = TDotEnvErrorKind.DekCycle then
        begin
          HasCycle := True;
          Break;
        end;
      Assert.IsTrue(HasCycle, 'Cycle detection error expected');
      Assert.IsFalse(DotEnv.TryGetValue('A', Dummy), 'A should not be resolved on cycle');
      Assert.IsFalse(DotEnv.TryGetValue('B', Dummy), 'B should not be resolved on cycle');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

{$IFDEF MSWINDOWS}
procedure TMaxLogicDotEnvTests.Unquoted_WindowsPath_DoubleBackslash_Preserves;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('path-ddb');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    // double backslash preserves a single backslash
    WriteText(EnvFile, 'P=C:\\temp'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('C:\temp', CollectValue(DotEnv, 'P'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Unquoted_WindowsPath_SingleBackslash_TabPitfall;
var
  TempDir, EnvFile, V: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('path-tab');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    // single \t in unquoted is a TAB per unquoted-escape rules
    WriteText(EnvFile, 'P=C:\temp'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      V := CollectValue(DotEnv, 'P');
      Assert.AreEqual('C:' + #9 + 'emp', V);
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;
{$ELSE}
procedure TMaxLogicDotEnvTests.Unquoted_WindowsPath_DoubleBackslash_Preserves;
begin
  Assert.Pass('Windows-specific test');
end;

procedure TMaxLogicDotEnvTests.Unquoted_WindowsPath_SingleBackslash_TabPitfall;
begin
  Assert.Pass('Windows-specific test');
end;
{$ENDIF}

procedure TMaxLogicDotEnvTests.EscapedDollar_DeferredExpand;
var
  TempDir, EnvFile, Lit, Expanded: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('escaped-dollar');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'A=one'#10'B=$${A}'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Lit := CollectValue(DotEnv, 'B');
      Assert.AreEqual('${A}', Lit, 'Literal ${A} expected from $$ escape');
      Expanded := DotEnv.Expand(Lit);
      Assert.AreEqual('one', Expanded, 'Deferred expansion should resolve with final map');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.DoNotOverrideExisting_AfterMerge_UsesProcessEnvInDerived;
const
  K = 'DL_EAM_EXISTING';
var
  TempDir, EnvFile, Original, V: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('eam-existing');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    // File sets both the base key and a derived key
    WriteText(EnvFile, K + '=base'#10'DER=${' + K + '}'#10);
    Original := GetEnvironmentVariable(K);
  {$IFDEF MSWINDOWS}
    SetEnvironmentVariable(PChar(K), PChar('proc'));
  {$ELSE}
    setenv(PAnsiChar(AnsiString(K)), PAnsiChar(AnsiString('proc')), 1);
  {$ENDIF}
    DotEnv := TDotEnv.Create;
    try
      // Default is evaluate-after-merge
      DotEnv.LoadFiles([EnvFile], [TDotEnvOption.DoNotOverrideExisting]);
      // The base key should not be stored/overridden
      Assert.IsFalse(DotEnv.TryGetValue(K, V), 'Key should not be stored when DoNotOverrideExisting');
      // The derived key should resolve using the process env value
      Assert.AreEqual('proc', CollectValue(DotEnv, 'DER'));
    finally
      DotEnv.Free;
    end;
  finally
  {$IFDEF MSWINDOWS}
    if Original = '' then
      SetEnvironmentVariable(PChar(K), nil)
    else
      SetEnvironmentVariable(PChar(K), PChar(Original));
  {$ELSE}
    if Original = '' then
      unsetenv(PAnsiChar(AnsiString(K)))
    else
      setenv(PAnsiChar(AnsiString(K)), PAnsiChar(AnsiString(Original)), 1);
  {$ENDIF}
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Includes_Optional_Missing_NoError;
var
  TempDir, MainEnv, MissingResolved: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  Tr: TArray<string>;
  HasIO: Boolean;
  HasMissingTrace: Boolean;
  S: string;
  Err: TDotEnvError;
begin
  TempDir := MakeTempDir('include-opt');
  try
    MainEnv := BuildPath(TempDir, ['main.env']);
    // optional include + a regular assignment
    WriteText(MainEnv, '#include_if_exists "missing.env"'#10'K=ok'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([MainEnv], []);
      // No DekIO error expected
      Errors := DotEnv.GetErrors;
      HasIO := False;
      for Err in Errors do
        if Err.Kind = TDotEnvErrorKind.DekIO then
        begin
          HasIO := True;
          Break;
        end;
      Assert.IsFalse(HasIO, 'No IO error expected for #include_if_exists');
      // Trace should show "missing: <resolved>"
      MissingResolved := TPath.GetFullPath(BuildPath(TPath.GetDirectoryName(MainEnv), ['missing.env']));
      Tr := DotEnv.GetTrace;
      HasMissingTrace := False;
      for S in Tr do
        if S.StartsWith('missing: ', True) and S.ToLower.Contains(MissingResolved.ToLower) then
        begin
          HasMissingTrace := True;
          Break;
        end;
      Assert.IsTrue(HasMissingTrace, 'Trace should include missing include file');
      // Regular assignment still loaded
      Assert.AreEqual('ok', CollectValue(DotEnv, 'K'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

{$IFDEF MSWINDOWS}
procedure TMaxLogicDotEnvTests.CommandSubstitution_Windows_QuotedPathWithSpaces;
var
  TempDir, SpaceDir, ToolPath, EnvFile, EscToolPath: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('cmd-quote');
  try
    SpaceDir := BuildPath(TempDir, ['My App']);
    TDirectory.CreateDirectory(SpaceDir);
    ToolPath := BuildPath(SpaceDir, ['tool.bat']);
    // simple echo
    WriteText(ToolPath, '@echo quoted'#10);
    // need double backslashes in unquoted .env to preserve paths
    EscToolPath := StringReplace(ToolPath, '\', '\\', [rfReplaceAll]);
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'OUT=$("' + EscToolPath + '")'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], [TDotEnvOption.AllowCommandSubst]);
      Assert.AreEqual('quoted', CollectValue(DotEnv, 'OUT'),
        'Quoted path with spaces should execute and capture stdout.' + sLineBreak + DumpDiag(DotEnv));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.CommandSubstitution_Windows_QuotedPathWithSpaces_NonZeroExit;
var
  TempDir, SpaceDir, FailPath, EnvFile, EscFailPath: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  HasCmdErr: Boolean;
  Err: TDotEnvError;
begin
  TempDir := MakeTempDir('cmd-quote-fail');
  try
    SpaceDir := BuildPath(TempDir, ['My App']);
    TDirectory.CreateDirectory(SpaceDir);
    FailPath := BuildPath(SpaceDir, ['fail.bat']);
    // non-zero exit code
    WriteText(FailPath, '@exit /B 7'#10);
    EscFailPath := StringReplace(FailPath, '\', '\\', [rfReplaceAll]);
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'OUT=$("' + EscFailPath + '")'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], [TDotEnvOption.AllowCommandSubst]);
      // value should be empty and DekCommand error recorded
      Assert.AreEqual('', CollectValue(DotEnv, 'OUT'),
        'Non-zero exit should yield empty value.' + sLineBreak + DumpDiag(DotEnv));
      Errors := DotEnv.GetErrors;
      HasCmdErr := False;
      for Err in Errors do
        if Err.Kind = TDotEnvErrorKind.DekCommand then
        begin
          HasCmdErr := True;
          Break;
        end;
      Assert.IsTrue(HasCmdErr, 'DekCommand error expected for non-zero exit');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;
{$ELSE}
procedure TMaxLogicDotEnvTests.CommandSubstitution_Windows_QuotedPathWithSpaces;
begin
  Assert.Pass('Windows-specific test');
end;
procedure TMaxLogicDotEnvTests.CommandSubstitution_Windows_QuotedPathWithSpaces_NonZeroExit;
begin
  Assert.Pass('Windows-specific test');
end;
{$ENDIF}

procedure TMaxLogicDotEnvTests.Security_SecretPermissions_CaseInsensitive;
{$IFNDEF MSWINDOWS}
var
  TempDir, SecretFile: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  Err: TDotEnvError;
  HasSecurityError: Boolean;
begin
  TempDir := MakeTempDir('secret-ci');
  try
    // mixed-case suffix should still be treated as secret
    SecretFile := BuildPath(TempDir, ['.env.SeCrEt']);
    WriteText(SecretFile, 'MM=ok'#10);
    // world-readable -> should trigger security error
    chmod(PAnsiChar(AnsiString(SecretFile)), S_IRUSR or S_IWUSR or S_IRGRP);
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
      Assert.IsTrue(HasSecurityError, 'Secret permissions error expected (case-insensitive suffix)');
    finally
      DotEnv.Free;
    end;
    // Fix permissions to 600 and ensure it loads
    chmod(PAnsiChar(AnsiString(SecretFile)), S_IRUSR or S_IWUSR);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadLayered(TempDir, []);
      Assert.AreEqual('ok', CollectValue(DotEnv, 'MM'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;
{$ELSE}
begin
  Assert.Pass('POSIX-only test');
end;
{$ENDIF}

procedure TMaxLogicDotEnvTests.Keys_CaseInsensitive_Merge_LastWins;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('keys-ci-lastwins');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'k=1'#10'K=2'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
    {$IFDEF MSWINDOWS}
      // Windows default: case-insensitive keys; last assignment wins
      Assert.AreEqual('2', CollectValue(DotEnv, 'k'));
      Assert.AreEqual('2', CollectValue(DotEnv, 'K'));
    {$ELSE}
      // POSIX default: case-sensitive; this scenario is not applicable here
      Assert.Pass('POSIX default is case-sensitive; merge-last-wins (case-insensitive) applies on Windows');
    {$ENDIF}
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Keys_CaseSensitive_Option_Overrides;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('keys-case-option');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'k=1'#10'K=2'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], [TDotEnvOption.CaseSensitiveKeys]);
      Assert.AreEqual('1', CollectValue(DotEnv, 'k'), 'lowercase key should be distinct');
      Assert.AreEqual('2', CollectValue(DotEnv, 'K'), 'uppercase key should be distinct');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Keys_CaseSensitivity_DefaultPlatform;
var
  TempDir, EnvFile, V: string;
  DotEnv: TDotEnv;
  HasUpper: Boolean;
begin
  TempDir := MakeTempDir('keys-case-default');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'k=1'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      // always present via original casing
      Assert.AreEqual('1', CollectValue(DotEnv, 'k'));
      // platform-dependent behavior for different casing
      HasUpper := DotEnv.TryGetValue('K', V);
    {$IFDEF MSWINDOWS}
      Assert.IsTrue(HasUpper, 'Windows: keys are case-insensitive by default');
      Assert.AreEqual('1', V);
    {$ELSE}
      Assert.IsFalse(HasUpper, 'POSIX: keys are case-sensitive by default');
    {$ENDIF}
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Keys_CaseSensitivity_And_Collisions;
var
  TempDir, EnvFile, K1, K2: string;
  DotEnv: TDotEnv;
  Dict: TDictionary<string, string>;
  Keys: TArray<string>;
begin
  TempDir := MakeTempDir('keys-collisions');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    K1 := 'foo';
    K2 := 'FOO';
    // colliding keys differ only by case; second one wins where case-insensitive
    WriteText(EnvFile, K1 + '=1'#10 + K2 + '=2'#10);

    // 1) Default platform behavior
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      // last assignment should win
      Assert.AreEqual('2', CollectValue(DotEnv, K2));
    {$IFDEF MSWINDOWS}
      // case-insensitive default: both casings resolve to the same winner; dict has no duplicates
      Assert.AreEqual('2', CollectValue(DotEnv, K1));
      Dict := DotEnv.AsDictionary;
      try
        Assert.AreEqual(1, Dict.Count, 'Case-insensitive map must not contain duplicate keys');
        Keys := Dict.Keys.ToArray;
        Assert.AreEqual(K2, Keys[0], 'Winner casing should be preserved (last assignment)');
      finally
        Dict.Free;
      end;
    {$ELSE}
      // POSIX default: case-sensitive; both entries present distinctly
      Assert.AreEqual('1', CollectValue(DotEnv, K1));
      Dict := DotEnv.AsDictionary;
      try
        Assert.AreEqual(2, Dict.Count, 'Case-sensitive map should contain both casings');
      finally
        Dict.Free;
      end;
    {$ENDIF}
    finally
      DotEnv.Free;
    end;

    // 2) Explicit CaseSensitiveKeys option: always distinct
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], [TDotEnvOption.CaseSensitiveKeys]);
      Dict := DotEnv.AsDictionary;
      try
        Assert.AreEqual(2, Dict.Count, 'CaseSensitiveKeys: both casings must be distinct');
        Assert.AreEqual('1', CollectValue(DotEnv, K1));
        Assert.AreEqual('2', CollectValue(DotEnv, K2));
      finally
        Dict.Free;
      end;
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Value_Unquoted_CommentEscapes;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('val-001');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'A=foo\#bar'#10'B=foo\;bar'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('foo#bar', CollectValue(DotEnv, 'A'));
      Assert.AreEqual('foo;bar', CollectValue(DotEnv, 'B'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Value_Unquoted_Semicolon_NotCommentInline;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('val-002');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'A=http://x;param'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('http://x;param', CollectValue(DotEnv, 'A'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Value_Unquoted_TrailingWhitespace;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('val-003');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    // A trims trailing spaces; B keeps escaped trailing space
    WriteText(EnvFile, 'A=foo  '#10'B=foo\ '#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('foo', CollectValue(DotEnv, 'A'));
      Assert.AreEqual('foo ', CollectValue(DotEnv, 'B'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Value_Unquoted_LineContinuation;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('val-004');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'A=one\'#10'newline two'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('one'#10'newline two', CollectValue(DotEnv, 'A'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Value_DoubleQuoted_Escapes;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('val-005');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'A="x\n\$y\\z"'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('x'#10'$y\z', CollectValue(DotEnv, 'A'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Value_TripleQuoted_LeadingNewline_NoInitialNewline;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('val-007');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    // No newline after opening token -> preserve text as-is
    WriteText(EnvFile, 'A="""text"""'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('text', CollectValue(DotEnv, 'A'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Value_Heredoc_Expansion_DefaultsAndStrict;
var
  TempDir, EnvFile, V: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  HasStrict: Boolean;
  Err: TDotEnvError;
begin
  TempDir := MakeTempDir('val-008');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile,
      'HD1<<END'#10 +
      'one ${MISSING:-X} two $[1 + 1]'#10 +
      'END'#10 +
      'HD2<<E'#10 +
      '${UNSET}'#10 +
      'E'#10);

    // Non-strict: expansion + default
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('one X two 2', CollectValue(DotEnv, 'HD1'));
    finally
      DotEnv.Free;
    end;

    // StrictUndefined: HD2 should not be stored and strict error recorded
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], [TDotEnvOption.StrictUndefined]);
      Assert.IsFalse(DotEnv.TryGetValue('HD2', V), 'HD2 must not be stored under StrictUndefined');
      HasStrict := False;
      Errors := DotEnv.GetErrors;
      for Err in Errors do
        if Err.Kind = TDotEnvErrorKind.DekStrict then
        begin
          HasStrict := True;
          Break;
        end;
      Assert.IsTrue(HasStrict, 'StrictUndefined error expected for HD2');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.EscapedDollar_MiddleOfString_DeferredExpand;
var
  TempDir, EnvFile, Lit, Expanded: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('val-009');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'A=one'#10'B=pre $${A} post'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Lit := CollectValue(DotEnv, 'B');
      Assert.AreEqual('pre ${A} post', Lit);
      Expanded := DotEnv.Expand(Lit);
      Assert.AreEqual('pre one post', Expanded);
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Error_Parse_InvalidKey_Continue;
var
  TempDir, EnvFile, Dummy: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  HasParse: Boolean;
  Err: TDotEnvError;
begin
  TempDir := MakeTempDir('err-001');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, '1BAD=val'#10'GOOD=ok'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Errors := DotEnv.GetErrors;
      HasParse := False;
      for Err in Errors do
        if (Err.Kind = TDotEnvErrorKind.DekParse) and Err.Fatal then
        begin
          HasParse := True;
          Break;
        end;
      Assert.IsTrue(HasParse, 'DekParse fatal expected for invalid key');
      Assert.AreEqual('ok', CollectValue(DotEnv, 'GOOD'),
        'Subsequent valid line should still load');
      Assert.IsFalse(DotEnv.TryGetValue('1BAD', Dummy));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Error_Parse_MissingClosingBrace;
var
  TempDir, EnvFile, Dummy: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  Found: Boolean;
  Err: TDotEnvError;
begin
  TempDir := MakeTempDir('err-002');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'A=${NAME'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.IsFalse(DotEnv.TryGetValue('A', Dummy));
      Found := False;
      Errors := DotEnv.GetErrors;
      for Err in Errors do
        if (Err.Kind = TDotEnvErrorKind.DekParse) and Err.Fatal and
           ContainsText(Err.Message, 'Missing closing brace') then
        begin
          Found := True;
          Break;
        end;
      Assert.IsTrue(Found, 'Missing closing brace parse error expected');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Error_Parse_UnterminatedExpression;
var
  TempDir, EnvFile, Dummy: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  Found: Boolean;
  Err: TDotEnvError;
begin
  TempDir := MakeTempDir('err-003');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'A=$[ 1 + 2'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.IsFalse(DotEnv.TryGetValue('A', Dummy));
      Found := False;
      Errors := DotEnv.GetErrors;
      for Err in Errors do
        if (Err.Kind = TDotEnvErrorKind.DekParse) and Err.Fatal and
           ContainsText(Err.Message, 'Unterminated expression') then
        begin
          Found := True;
          Break;
        end;
      Assert.IsTrue(Found, 'Unterminated expression parse error expected');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Include_InvalidDirective_MissingQuotes;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  Found: Boolean;
  Err: TDotEnvError;
begin
  TempDir := MakeTempDir('err-004-incdir');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, '#include missing-quotes'#10'K=1'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Found := False;
      Errors := DotEnv.GetErrors;
      for Err in Errors do
        if (Err.Kind = TDotEnvErrorKind.DekParse) and
           ContainsText(Err.Message, 'Include directive requires quoted path') then
        begin
          Found := True;
          Break;
        end;
      Assert.IsTrue(Found, 'Invalid include directive error expected');
      Assert.AreEqual('1', CollectValue(DotEnv, 'K'), 'Subsequent assignment should still load');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Includes_RelativeTraversal_FromSubdir;
var
  TempDir, ParentDir, SubDir, MainEnv, SecretEnv: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('inc-rel');
  try
    ParentDir := BuildPath(TempDir, ['parent']);
    SubDir := BuildPath(ParentDir, ['sub']);
    TDirectory.CreateDirectory(SubDir);
    SecretEnv := BuildPath(ParentDir, ['secrets.env']);
    WriteText(SecretEnv, 'X=1'#10'DIR=${CUR_DIR}'#10'FILE=${CUR_FILE}'#10);
    MainEnv := BuildPath(SubDir, ['main.env']);
    WriteText(MainEnv, '#include "../secrets.env"'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([MainEnv], []);
      Assert.AreEqual('1', CollectValue(DotEnv, 'X'));
      Assert.AreEqual(TPath.GetDirectoryName(TPath.GetFullPath(SecretEnv)), CollectValue(DotEnv, 'DIR'));
      Assert.AreEqual(TPath.GetFullPath(SecretEnv), CollectValue(DotEnv, 'FILE'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Includes_OptionalPresent_LoadsAndOverrides;
var
  TempDir, MainEnv, ChildEnv: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('inc-opt-present');
  try
    MainEnv := BuildPath(TempDir, ['main.env']);
    ChildEnv := BuildPath(TempDir, ['child.env']);
    WriteText(MainEnv, 'X=base'#10'#include_if_exists "child.env"'#10);
    WriteText(ChildEnv, 'X=inc'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([MainEnv], []);
      Assert.AreEqual('inc', CollectValue(DotEnv, 'X'), 'Included file should override earlier value');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Includes_Cycle_ThreeFiles_NoValues;
var
  TempDir, AFile, BFile, CFile: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  HasCycle: Boolean;
  Err: TDotEnvError;
  Dict: TDictionary<string, string>;
begin
  TempDir := MakeTempDir('inc-cycle3');
  try
    AFile := BuildPath(TempDir, ['a.env']);
    BFile := BuildPath(TempDir, ['b.env']);
    CFile := BuildPath(TempDir, ['c.env']);
    WriteText(AFile, '#include "b.env"'#10);
    WriteText(BFile, '#include "c.env"'#10);
    WriteText(CFile, '#include "a.env"'#10);
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
      Assert.IsTrue(HasCycle, 'Three-file cycle detection expected');
      Dict := DotEnv.AsDictionary;
      try
        Assert.AreEqual(0, Dict.Count, 'No values should be stored for a pure include cycle');
      finally
        Dict.Free;
      end;
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Layered_SecretOverridesLocal;
var
  TempDir: string;
  DotEnv: TDotEnv;
  SecretPath: string;
begin
  TempDir := MakeTempDir('layer-secret');
  try
    WriteText(BuildPath(TempDir, ['.env']), 'K=base'#10);
    WriteText(BuildPath(TempDir, ['.env.local']), 'K=local'#10);
    SecretPath := BuildPath(TempDir, ['.env.secret']);
    WriteText(SecretPath, 'K=secret'#10);
  {$IFNDEF MSWINDOWS}
    chmod(PAnsiChar(AnsiString(SecretPath)), S_IRUSR or S_IWUSR);
  {$ENDIF}
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadLayered(TempDir, []);
      Assert.AreEqual('secret', CollectValue(DotEnv, 'K'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Layered_SecretMixedCase_OverridesLocal_POSIX;
{$IFNDEF MSWINDOWS}
var
  TempDir, SecretPath: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('layer-secret-ci');
  try
    WriteText(BuildPath(TempDir, ['.env']), 'K=base'#10);
    WriteText(BuildPath(TempDir, ['.env.local']), 'K=local'#10);
    SecretPath := BuildPath(TempDir, ['.env.SeCrEt']);
    WriteText(SecretPath, 'K=secretmix'#10);
    chmod(PAnsiChar(AnsiString(SecretPath)), S_IRUSR or S_IWUSR);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadLayered(TempDir, []);
      Assert.AreEqual('secretmix', CollectValue(DotEnv, 'K'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;
{$ELSE}
begin
  Assert.Pass('POSIX-only test');
end;
{$ENDIF}

procedure TMaxLogicDotEnvTests.Evaluation_AfterMerge_ForwardReference_SingleFile;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('eval-fwd-merge');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'B=${A}'#10'A=base'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('base', CollectValue(DotEnv, 'A'));
      Assert.AreEqual('base', CollectValue(DotEnv, 'B'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Evaluation_Streaming_ForwardReference_SingleFile;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('eval-fwd-stream');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'B=${A}'#10'A=base'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], [TDotEnvOption.StreamingEvaluation]);
      Assert.AreEqual('base', CollectValue(DotEnv, 'A'));
      Assert.AreEqual('', CollectValue(DotEnv, 'B'), 'Streaming: forward ref should be empty (no default)');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Evaluation_Streaming_ForwardReference_StrictUndefined_Fatal;
var
  TempDir, EnvFile, Dummy: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  HasStrict: Boolean;
  Err: TDotEnvError;
begin
  TempDir := MakeTempDir('eval-fwd-stream-strict');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'B=${A}'#10'A=base'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], [TDotEnvOption.StreamingEvaluation, TDotEnvOption.StrictUndefined]);
      // B should not be stored
      Assert.IsFalse(DotEnv.TryGetValue('B', Dummy));
      HasStrict := False;
      Errors := DotEnv.GetErrors;
      for Err in Errors do
        if Err.Kind = TDotEnvErrorKind.DekStrict then
        begin
          HasStrict := True;
          Break;
        end;
      Assert.IsTrue(HasStrict, 'Strict error expected in streaming forward-ref');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Search_XdgDirs_Multiple_OrderAndLoad;
{$IFNDEF MSWINDOWS}
var
  TempBase, Dir1, Dir2, ML1, ML2: string;
  DotEnv: TDotEnv;
  SavedHome, SavedDirs, SavedXdgHome: string;
begin
  TempBase := MakeTempDir('xdg-multi');
  try
    Dir1 := BuildPath(TempBase, ['xdg1']);
    Dir2 := BuildPath(TempBase, ['xdg2']);
    ML1 := BuildPath(Dir1, ['maxlogic']);
    ML2 := BuildPath(Dir2, ['maxlogic']);
    TDirectory.CreateDirectory(ML1);
    TDirectory.CreateDirectory(ML2);
    WriteText(BuildPath(ML1, ['.env']), 'K=one'#10);
    WriteText(BuildPath(ML2, ['.env']), 'K=two'#10);

    // Isolate XDG settings
    SavedXdgHome := GetEnvironmentVariable('XDG_CONFIG_HOME');
    SavedDirs := GetEnvironmentVariable('XDG_CONFIG_DIRS');
    SavedHome := GetEnvironmentVariable('HOME');

    setenv(PAnsiChar(AnsiString('XDG_CONFIG_HOME')), PAnsiChar(AnsiString(BuildPath(TempBase, ['nohome']))), 1);
    setenv(PAnsiChar(AnsiString('HOME')), PAnsiChar(AnsiString(BuildPath(TempBase, ['home']))), 1);
    setenv(PAnsiChar(AnsiString('XDG_CONFIG_DIRS')),
      PAnsiChar(AnsiString(ExcludeTrailingPathDelimiter(Dir1) + ':' + ExcludeTrailingPathDelimiter(Dir2))), 1);

    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadLayered(BuildPath(TempBase, ['proj']), [TDotEnvOption.SearchXDG]);
      // Earlier dir in XDG_CONFIG_DIRS should win
      Assert.AreEqual('one', CollectValue(DotEnv, 'K'));
    finally
      DotEnv.Free;
    end;
  finally
    if SavedXdgHome = '' then unsetenv(PAnsiChar(AnsiString('XDG_CONFIG_HOME')))
    else setenv(PAnsiChar(AnsiString('XDG_CONFIG_HOME')), PAnsiChar(AnsiString(SavedXdgHome)), 1);
    if SavedDirs = '' then unsetenv(PAnsiChar(AnsiString('XDG_CONFIG_DIRS')))
    else setenv(PAnsiChar(AnsiString('XDG_CONFIG_DIRS')), PAnsiChar(AnsiString(SavedDirs)), 1);
    if SavedHome = '' then unsetenv(PAnsiChar(AnsiString('HOME')))
    else setenv(PAnsiChar(AnsiString('HOME')), PAnsiChar(AnsiString(SavedHome)), 1);
    RemoveDirRecursive(TempBase);
  end;
end;
{$ELSE}
begin
  Assert.Pass('POSIX-only test');
end;
{$ENDIF}

procedure TMaxLogicDotEnvTests.Search_CustomRoots_Precision_And_Order;
var
  TempBase, RootA, RootB: string;
  DotEnv: TDotEnv;
  Roots: TArray<TSearchRoot>;
  Tr: TArray<string>;
  ConsideredCount: Integer;
  S: string;
begin
  TempBase := MakeTempDir('custom-roots');
  try
    RootA := BuildPath(TempBase, ['A']);
    RootB := BuildPath(TempBase, ['B']);
    TDirectory.CreateDirectory(RootA);
    TDirectory.CreateDirectory(RootB);
    WriteText(BuildPath(RootA, ['.env']), 'K=fromA'#10);
    WriteText(BuildPath(RootB, ['.env']), 'K=fromB'#10);

    SetLength(Roots, 2);
    Roots[0].Kind := TSearchRootKind.srCustom; Roots[0].Path := RootA;
    Roots[1].Kind := TSearchRootKind.srCustom; Roots[1].Path := RootB;

    DotEnv := TDotEnv.Create;
    try
      DotEnv.SetSearchRoots(Roots);
      DotEnv.LoadLayered(BuildPath(TempBase, ['proj']), []);
      // Earlier custom root wins
      Assert.AreEqual('fromA', CollectValue(DotEnv, 'K'));
      // Only 2 roots × 3 layer files considered
      ConsideredCount := 0;
      Tr := DotEnv.GetTrace;
      for S in Tr do
        if S.StartsWith('considered: ', True) then
          Inc(ConsideredCount);
      Assert.AreEqual(6, ConsideredCount);
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempBase);
  end;
end;

procedure TMaxLogicDotEnvTests.Search_CustomNamespace_OverridesDefault;
const
  CustomNs = 'acme';
var
  TempBase, ProjectDir: string;
  DotEnv: TDotEnv;
{$IFDEF MSWINDOWS}
  AppDataPath, CustomDir, SavedAppData: string;
{$ELSE}
  HomeDir, CustomDir, SavedHome: string;
{$ENDIF}
begin
  TempBase := MakeTempDir('custom-namespace');
  try
    ProjectDir := BuildPath(TempBase, ['project']);
    TDirectory.CreateDirectory(ProjectDir);
{$IFDEF MSWINDOWS}
    AppDataPath := BuildPath(TempBase, ['AppData']);
    CustomDir := BuildPath(AppDataPath, [CustomNs]);
    TDirectory.CreateDirectory(CustomDir);
    WriteText(BuildPath(CustomDir, ['.env']), 'VALUE=custom'#10);
    SavedAppData := GetEnvironmentVariable('APPDATA');
    SetEnvironmentVariable(PChar('APPDATA'), PChar(AppDataPath));
    DotEnv := TDotEnv.Create;
    try
      DotEnv.EnvironmentNamespace := CustomNs;
      DotEnv.LoadLayered(ProjectDir, [TDotEnvOption.SearchWindowsProfile]);
      Assert.AreEqual('custom', CollectValue(DotEnv, 'VALUE'));
    finally
      DotEnv.Free;
      if SavedAppData = '' then
        SetEnvironmentVariable(PChar('APPDATA'), nil)
      else
        SetEnvironmentVariable(PChar('APPDATA'), PChar(SavedAppData));
    end;
{$ELSE}
    HomeDir := BuildPath(TempBase, ['home']);
    CustomDir := BuildPath(HomeDir, ['.config', CustomNs]);
    TDirectory.CreateDirectory(CustomDir);
    WriteText(BuildPath(CustomDir, ['.env']), 'VALUE=custom'#10);
    SavedHome := GetEnvironmentVariable('HOME');
    setenv(PAnsiChar(AnsiString('HOME')), PAnsiChar(AnsiString(HomeDir)), 1);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.EnvironmentNamespace := CustomNs;
      DotEnv.LoadLayered(ProjectDir, [TDotEnvOption.SearchUserHome]);
      Assert.AreEqual('custom', CollectValue(DotEnv, 'VALUE'));
    finally
      DotEnv.Free;
      if SavedHome = '' then
        unsetenv(PAnsiChar(AnsiString('HOME')))
      else
        setenv(PAnsiChar(AnsiString('HOME')), PAnsiChar(AnsiString(SavedHome)), 1);
    end;
{$ENDIF}
  finally
    RemoveDirRecursive(TempBase);
  end;
end;

procedure TMaxLogicDotEnvTests.API_AsDictionary_PreservesOriginalCasing;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
  D: TDictionary<string, string>;
  Keys: TArray<string>;
begin
  TempDir := MakeTempDir('api-asdict');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'KeyName=1'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      D := DotEnv.AsDictionary;
      try
        Keys := D.Keys.ToArray;
        Assert.AreEqual(1, Length(Keys));
        Assert.AreEqual('KeyName', Keys[0], 'AsDictionary must preserve original casing');
      finally
        D.Free;
      end;
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.CommandSubstitution_NestedParentheses;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('cmd-nested');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'OUT=$(echo a (b) c)'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], [TDotEnvOption.AllowCommandSubst]);
      Assert.AreEqual('a (b) c', CollectValue(DotEnv, 'OUT'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.CommandSubstitution_Disabled_LiteralInMiddle;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('cmd-disabled-mid');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'A=pre $(echo x) post'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []); // disabled
      Assert.AreEqual('pre $(echo x) post', CollectValue(DotEnv, 'A'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.CommandSubstitution_Windows_UTF8BOM_Batch;
{$IFDEF MSWINDOWS}
var
  TempDir, ToolPath, EnvFile, EscToolPath: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('cmd-bom');
  try
    ToolPath := BuildPath(TempDir, ['tool.bat']);
    // write with UTF-8 BOM (WriteText uses TEncoding.UTF8 which includes BOM)
    WriteText(ToolPath, '@echo bomok'#10);
    EscToolPath := StringReplace(ToolPath, '\', '\\', [rfReplaceAll]);
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'OUT=$("' + EscToolPath + '")'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], [TDotEnvOption.AllowCommandSubst]);
      Assert.AreEqual('bomok', CollectValue(DotEnv, 'OUT'),
        'UTF-8 BOM batch should execute via BOM-stripped temp copy');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;
{$ELSE}
begin
  Assert.Pass('Windows-specific test');
end;
{$ENDIF}

procedure TMaxLogicDotEnvTests.Builtins_Expand_WithSourceFile_UsesProvidedPath;
var
  TempDir, FilePath, DirExpected, FileExpected: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('exp-src-file');
  try
    FilePath := BuildPath(TempDir, ['sub', 'conf.env']);
    TDirectory.CreateDirectory(TPath.GetDirectoryName(FilePath));
    FilePath := TPath.GetFullPath(FilePath);
    DirExpected := TPath.GetDirectoryName(FilePath);
    FileExpected := FilePath;

    DotEnv := TDotEnv.Create;
    try
      Assert.AreEqual(DirExpected, DotEnv.Expand('${CUR_DIR}', FilePath));
      Assert.AreEqual(FileExpected, DotEnv.Expand('${CUR_FILE}', FilePath));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.OnResolveSymbol_Receives_FilePath;
var
  TempDir, EnvFile, SeenFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('onresolve-file');
  try
    EnvFile := BuildPath(TempDir, ['main.env']);
    WriteText(EnvFile, 'X=${NO_SUCH}'#10);
    SeenFile := '';
    DotEnv := TDotEnv.Create;
    try
      DotEnv.OnResolveSymbol :=
        function(const AName, AFilePath: string): string
        begin
          SeenFile := AFilePath;
          if SameText(AName, 'NO_SUCH') then
            Result := 'OK'
          else
            Result := '';
        end;
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('OK', CollectValue(DotEnv, 'X'));
      Assert.AreEqual(TPath.GetFullPath(EnvFile), SeenFile, 'OnResolveSymbol should receive current source file path');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.DoNotOverrideExisting_Streaming_UsesProcessEnvInDerived;
const
  K = 'DL_ESTR';
var
  TempDir, EnvFile, Original, V: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('estr');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    // base key and derived key
    WriteText(EnvFile, K + '=base'#10'DER=${' + K + '}'#10);
    Original := GetEnvironmentVariable(K);
  {$IFDEF MSWINDOWS}
    SetEnvironmentVariable(PChar(K), PChar('proc'));
  {$ELSE}
    setenv(PAnsiChar(AnsiString(K)), PAnsiChar(AnsiString('proc')), 1);
  {$ENDIF}
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], [TDotEnvOption.DoNotOverrideExisting, TDotEnvOption.StreamingEvaluation]);
      Assert.IsFalse(DotEnv.TryGetValue(K, V), 'Base key should not be stored when DoNotOverrideExisting');
      Assert.AreEqual('proc', CollectValue(DotEnv, 'DER'), 'Derived should read process env during streaming');
    finally
      DotEnv.Free;
    end;
  finally
  {$IFDEF MSWINDOWS}
    if Original = '' then
      SetEnvironmentVariable(PChar(K), nil)
    else
      SetEnvironmentVariable(PChar(K), PChar(Original));
  {$ELSE}
    if Original = '' then
      unsetenv(PAnsiChar(AnsiString(K)))
    else
      setenv(PAnsiChar(AnsiString(K)), PAnsiChar(AnsiString(Original)), 1);
  {$ENDIF}
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Expressions_Modulo_And_ModuloByZero_Error;
var
  TempDir, EnvFile, Dummy: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  HasModuloZero: Boolean;
  Err: TDotEnvError;
begin
  TempDir := MakeTempDir('expr-mod');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'A=$[ 5 % 2 ]'#10'B=$[ 1 % 0 ]'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('1', CollectValue(DotEnv, 'A'));
      Assert.IsFalse(DotEnv.TryGetValue('B', Dummy), 'Modulo by zero must prevent storing the key');
      Errors := DotEnv.GetErrors;
      HasModuloZero := False;
      for Err in Errors do
        if (Err.Kind = TDotEnvErrorKind.DekParse) and Err.Fatal and
           ContainsText(Err.Message, 'Modulo by zero') then
        begin
          HasModuloZero := True;
          Break;
        end;
      Assert.IsTrue(HasModuloZero, 'DekParse fatal "Modulo by zero" expected');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Expressions_UnaryMinus_And_StringConcat;
var
  TempDir, EnvFile: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('expr-unary-concat');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile,
      'A=$[ -5 + 2 ]'#10 +
      'B=$[ ''a'' + ''b'' ]'#10 +
      'C=$[ ''foo'' + 1 ]'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.AreEqual('-3', CollectValue(DotEnv, 'A'));
      Assert.AreEqual('ab', CollectValue(DotEnv, 'B'));
      Assert.AreEqual('foo1', CollectValue(DotEnv, 'C'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Expressions_PathFunctions;
var
  TempDir, SomePath, Rel, ExpectedDir, ExpectedBase, ExpectedAbs: string;
  DotEnv: TDotEnv;
begin
  TempDir := MakeTempDir('expr-paths');
  try
    SomePath := BuildPath(TempDir, ['folder', 'file.txt']);
    TDirectory.CreateDirectory(TPath.GetDirectoryName(SomePath));
    ExpectedDir := TPath.GetDirectoryName(SomePath);
    ExpectedBase := TPath.GetFileName(SomePath);
    {$IFDEF MSWINDOWS}
    Rel := '.\x\..\y';
    {$ELSE}
    Rel := './x/../y';
    {$ENDIF}
    ExpectedAbs := TPath.GetFullPath(Rel);

    // Note: expression string literals use single quotes; embed using '' in .env text.
    WriteText(BuildPath(TempDir, ['.env']),
      'DIR=$[ dirname(''' + SomePath + ''') ]'#10 +
      'BASE=$[ basename(''' + SomePath + ''') ]'#10 +
      'ABS=$[ abspath(''' + Rel + ''') ]'#10);

    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([BuildPath(TempDir, ['.env'])], []);
      Assert.AreEqual(ExpectedDir, CollectValue(DotEnv, 'DIR'));
      Assert.AreEqual(ExpectedBase, CollectValue(DotEnv, 'BASE'));
      Assert.AreEqual(ExpectedAbs, CollectValue(DotEnv, 'ABS'));
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

procedure TMaxLogicDotEnvTests.Expressions_Join_NoArgs_Error;
var
  TempDir, EnvFile, Dummy: string;
  DotEnv: TDotEnv;
  Errors: TArray<TDotEnvError>;
  Found: Boolean;
  Err: TDotEnvError;
begin
  TempDir := MakeTempDir('expr-join-noargs');
  try
    EnvFile := BuildPath(TempDir, ['.env']);
    WriteText(EnvFile, 'BAD=$[ join() ]'#10);
    DotEnv := TDotEnv.Create;
    try
      DotEnv.LoadFiles([EnvFile], []);
      Assert.IsFalse(DotEnv.TryGetValue('BAD', Dummy));
      Errors := DotEnv.GetErrors;
      Found := False;
      for Err in Errors do
        if (Err.Kind = TDotEnvErrorKind.DekParse) and Err.Fatal and
           ContainsText(Err.Message, 'join requires arguments') then
        begin
          Found := True;
          Break;
        end;
      Assert.IsTrue(Found, 'DekParse fatal "join requires arguments" expected');
    finally
      DotEnv.Free;
    end;
  finally
    RemoveDirRecursive(TempDir);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMaxLogicDotEnvTests);

end.
