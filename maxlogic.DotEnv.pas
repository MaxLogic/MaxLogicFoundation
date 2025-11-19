unit maxlogic.DotEnv;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults;

const
  MAX_DOTENV_FILE_SIZE = 1024 * 1024; // 1 MB cap

  DEFAULT_PARENT_DEPTH = 30;

 type
  TDotEnvOption = (
    DoNotOverrideExisting,    // Do not store keys already present in the process environment.
    CaseSensitiveKeys,        // Force case-sensitive key comparisons, even on Windows.
    StrictUndefined,          // Treat missing variables (without defaults) as fatal errors.
    AllowCommandSubst,        // Enable $(...) command execution during value expansion.
    SearchUserHome,           // Search the user home and ~/.config/<namespace> roots.
    SearchXDG,                // Search XDG config locations (POSIX) using the namespace.
    SearchWindowsProfile,     // Search %APPDATA%\<namespace>.
    StreamingEvaluation       // Evaluate values as files are read instead of after merge.
  );

  TDotEnvOptions = set of TDotEnvOption;

  TSearchRootKind = (srExplicit, srCWD, srParents, srHome, srXDG, srWinProfile, srCustom);

  TSearchRoot = record
    Kind: TSearchRootKind;
    Path: string;
  end;

  TOnResolveSymbol = reference to function(const aName, aFilePath: string): string;

  TDotEnvErrorKind = (
    DekParse,
    DekStrict,
    DekCycle,
    DekIO,
    DekSecurity,
    DekCommand,
    DekWarning
  );

  TDotEnvError = record
    FilePath: string;
    Line: Integer;
    Column: Integer;
    Kind: TDotEnvErrorKind;
    Message: string;
    Fatal: Boolean;
  end;

  TDotEnv = class
  private
    type
      TStoredValue = record
        Key: string;
        Value: string;
        SourceFile: string;
        Line: Integer;
        RootIndex: Integer;
        Sequence: Int64;
      end;

      TRawAssignment = record
        Key: string;
        RawValue: string;
        FilePath: string;
        Line: Integer;
        RootIndex: Integer;
        FileOrder: Integer;
      end;

      TKeyComparer = class(TInterfacedObject, IEqualityComparer<string>)
      private
        FComparer: IEqualityComparer<string>;
        FCaseSensitive: Boolean;
      public
        constructor Create(aCaseSensitive: Boolean);
        function Equals(const aLeft, aRight: string): Boolean;
        function GetHashCode(const aValue: string): Integer;
      end;

      TLayeredFile = record
        Path: string;
        RootIndex: Integer;
      end;

      TExprValueKind = (evUndefined, evString, evNumber, evBoolean);

      TExprValue = record
        Kind: TExprValueKind;
        StrValue: string;
        NumValue: Double;
        BoolValue: Boolean;
        class function FromString(const S: string): TExprValue; static;
        class function FromNumber(const N: Double): TExprValue; static;
        class function FromBoolean(const B: Boolean): TExprValue; static;
        function ToStringValue: string;
        function ToNumber: Double;
        function ToBoolean: Boolean;
      end;

      TExprParser = class
      private
        FOwner: TDotEnv;
        FText: string;
        FPos: Integer;
        FSourceFile: string;
        FLine: Integer;
        function PeekChar: Char;
        function ConsumeChar: Char;
        procedure SkipWhitespace;
        function MatchToken(const aToken: string): Boolean;
        function ParseExpression: TExprValue;
        function ParseOr: TExprValue;
        function ParseAnd: TExprValue;
        function ParseCompare: TExprValue;
        function ParseAdd: TExprValue;
        function ParseMul: TExprValue;
        function ParseUnary: TExprValue;
        function ParsePrimary: TExprValue;
        function ParseIdentifier: string;
        function ParseStringLiteral: TExprValue;
        function ParseNumber: TExprValue;
        function EvaluateFunction(const aName: string): TExprValue;
        function ResolveIdentifier(const aName: string): TExprValue;
      public
        constructor Create(aOwner: TDotEnv; const aText, aSourceFile: string; aLine: Integer);
        function Evaluate: TExprValue;
      end;

    var
      FValues: TDictionary<string, TStoredValue>;
      FErrors: TList<TDotEnvError>;
      FTrace: TList<string>;
      FInitialEnv: TDictionary<string, Byte>;
      FCurrentOptions: TDotEnvOptions;
      FSequence: Int64;
      FCustomRoots: TArray<TSearchRoot>;
      FHasCustomRoots: Boolean;
      FOnResolveSymbol: TOnResolveSymbol;
      FCaseSensitiveCache: Boolean;
      FIncludeStack: TStack<string>;
      FMaxFileSize: Int64;
      FParentDepth: Integer;
      FDeferred: TDictionary<string, TRawAssignment>;
      FEvalStack: THashSet<string>;
      FDeferredActive: Boolean;
      FEnvironmentNamespace: string;

    procedure InitDefaults;
    procedure ResetState;
    function DetermineCaseSensitive: Boolean;
    procedure EnsureValuesDictionary;
    function NormalizeKey(const aKey: string): string;
    procedure SetEnvironmentNamespace(const aValue: string);

    procedure AddError(const aFilePath: string; aLine, aColumn: Integer; aKind: TDotEnvErrorKind;
      const aMsg: string; aFatal: Boolean);
    procedure AddTrace(const aMsg: string);

    procedure EnsureInitialEnv;
    procedure CaptureInitialEnv;
    function ProcessEnvHasKey(const aKey: string): Boolean;

    function ParseFile(const aFilePath: string; aRootIndex, aFileOrder: Integer;
      aEntries: TList<TRawAssignment>): Boolean;
    procedure ParseContent(const aFilePath: string; aRootIndex, aFileOrder: Integer;
      const aContent: string; aEntries: TList<TRawAssignment>);
    function ParseKeyValue(const aFilePath: string; const aLine: string; aLineNumber: Integer;
      aRootIndex, aFileOrder: Integer; out aAssignment: TRawAssignment): Boolean;
    function ParseValue(const aFilePath: string; var aLineIndex: Integer;
      const aLines: TArray<string>; const aInitialRemainder: string; aLineNumber: Integer): string;
    function ParseDoubleQuoted(const aFilePath: string; const aFragment: string;
      aLineNumber: Integer): string;
    function ParseSingleQuoted(const aFilePath: string; const aFragment: string;
      aLineNumber: Integer): string;
    function ParseTripleQuoted(const aFilePath: string; var aLineIndex: Integer;
      const aLines: TArray<string>; const aFragment: string; const aQuoteToken: string;
      aLineNumber: Integer): string;
    function ParseHeredoc(const aFilePath: string; var aLineIndex: Integer;
      const aLines: TArray<string>; const aMarker: string; aLineNumber: Integer): string;
    function ParseUnquoted(const aFilePath: string; var aLineIndex: Integer;
      const aLines: TArray<string>; const aFragment: string; aLineNumber: Integer): string;

    function ExpandValue(const aRawValue, aSourceFile: string; aLine: Integer): string;
    function ExpandVariable(const aSource, aSourceFile: string; aLine: Integer; var aIndex: Integer): string;
    function ExpandExpression(const aSource, aSourceFile: string; aLine: Integer; var aIndex: Integer): string;
    function ExpandCommand(const aSource, aSourceFile: string; aLine: Integer; var aIndex: Integer): string;
    function ResolveVariable(const aName, aSourceFile: string; aLine: Integer;
      const aDefaultValue: string; aHasDefault: Boolean; out aFailed: Boolean): string;
    function ResolveSymbol(const aName, aSourceFile: string; out aValue: string): Boolean;
    function ResolveBuiltIn(const aName, aSourceFile: string; out aValue: string): Boolean;

    procedure StoreValue(const aAssignment: TRawAssignment; const aExpandedValue: string);
    procedure ApplyToProcessEnv(const aKey, aValue: string);
    function ExecuteCommand(const aCmd: string; out aOutput: string; out aErrMsg: string): Boolean;

    procedure CollectFile(const aFilePath: string; aRootIndex, aFileOrder: Integer;
      aEntries: TList<TRawAssignment>);
    function CollectLayeredFiles(const aBaseDir: string; const aOptions: TDotEnvOptions): TArray<TLayeredFile>;
    function ExpandRoots(const aBaseDir: string; const aOptions: TDotEnvOptions): TArray<TSearchRoot>;
    function HandleIncludeLine(const aCurrentLine, aFilePath: string; aLineNumber: Integer;
      aRootIndex, aFileOrder: Integer; aEntries: TList<TRawAssignment>): Boolean;
    function ResolveIncludePath(const aBaseFile, aTarget: string): string;
    function IsPathOnStack(const aPath: string): Boolean;
    function CheckSecretPermissions(const aFilePath: string): Boolean;
    procedure EvaluateAfterMergeEntries(const aEntries: TList<TRawAssignment>);
    procedure EvaluateDeferredKey(const aNormalizedKey: string);
  public
    /// <summary>Creates a dot-env loader with default file-size cap and parent search depth.</summary>
    constructor Create; overload;
    /// <summary>Creates a dot-env loader with custom file-size and parent-depth settings.</summary>
    /// <param name="aMaxFileSize">Maximum number of bytes allowed per .env file.</param>
    /// <param name="aParentDepth">Maximum number of parent directories to crawl when searching.</param>
    constructor Create(aMaxFileSize: Int64; aParentDepth: Integer); overload;
    /// <summary>Releases all resources held by the loader.</summary>
    destructor Destroy; override;

    /// <summary>Loads layered .env files rooted at <paramref name="aBaseDir"/>.</summary>
    /// <param name="aBaseDir">Directory whose layered files (.env, .env.local, .env.secret) form the highest-precedence root.</param>
    /// <param name="aOptions">Flags controlling search roots, evaluation mode, and safety features.</param>
    procedure LoadLayered(const aBaseDir: string; const aOptions: TDotEnvOptions = []);
    /// <summary>Loads the specified .env files in order without performing root expansion.</summary>
    /// <param name="aFiles">Explicit file paths to load.</param>
    /// <param name="aOptions">Flags controlling evaluation mode and safety features.</param>
    procedure LoadFiles(const aFiles: array of string; const aOptions: TDotEnvOptions = []);

    /// <summary>Overrides the automatically computed search roots used by <see cref="LoadLayered" />.</summary>
    /// <param name="aRoots">Exact root definitions in precedence order.</param>
    procedure SetSearchRoots(const aRoots: TArray<TSearchRoot>);
    /// <summary>Returns the deterministic set of roots that would be considered for the given base directory.</summary>
    /// <param name="aBaseDir">Directory used as the project root.</param>
    /// <param name="aOptions">Search flags that influence which auxiliary roots are added.</param>
    class function DefaultSearchRoots(const aBaseDir: string; aOptions: TDotEnvOptions): TArray<TSearchRoot>;

    /// <summary>Retrieves a value by key if it was stored during the last load.</summary>
    /// <param name="aKey">Logical key to read.</param>
    /// <param name="aValue">Receives the stored value when the function returns True.</param>
    function TryGetValue(const aKey: string; out aValue: string): Boolean;
    /// <summary>Retrieves a value by key, returning <paramref name="aDefaultValue"/> when not present.</summary>
    function GetValue(const aKey: string; const aDefaultValue: string = ''): string;
    /// <summary>Returns a snapshot dictionary of all stored key/value pairs (preserving winner casing).</summary>
    function AsDictionary: TDictionary<string, string>;
    /// <summary>Expands a string as though it were loaded from a file, honoring built-ins and process values.</summary>
    /// <param name="aText">The text to expand.</param>
    /// <param name="aSourceFilePath">Optional file path used for CUR_DIR/CUR_FILE evaluation.</param>
    function Expand(const aText: string; const aSourceFilePath: string = ''): string;

    /// <summary>Returns the list of structured errors emitted during the last load.</summary>
    function GetErrors: TArray<TDotEnvError>;
    /// <summary>Returns the chronological trace describing which files were considered/loaded/skipped.</summary>
    function GetTrace: TArray<string>;

    /// <summary>Callback invoked when no stored value, process environment variable, or built-in matches a symbol.</summary>
    property OnResolveSymbol: TOnResolveSymbol read FOnResolveSymbol write FOnResolveSymbol;
    /// <summary>Namespace appended to shared search roots (e.g., ~/.config/&lt;namespace&gt;); set to empty to disable.</summary>
    property EnvironmentNamespace: string read FEnvironmentNamespace write SetEnvironmentNamespace;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ELSE}
  Posix.Stdlib, Posix.Stdio, Posix.SysStat, Posix.Unistd, Posix.Errno, Posix.SysWait,
{$ENDIF}
  System.StrUtils, System.Character, System.IOUtils, System.Math,
  maxlogic.ioUtils;

const
  LAYER_FILES: array[0..2] of string = ('.env', '.env.local', '.env.secret');

function StripTrailingWhitespaceWithEscape(var aText: string): Boolean;
var
  I: Integer;
  lKeep: Boolean;
  lRemoved: Boolean;
begin
  lRemoved := False;
  I := Length(aText);
  while I > 0 do
  begin
    if not CharInSet(aText[I], [' ', #9]) then
      Break;
    lKeep := (I > 1) and (aText[I - 1] = '\');
    if lKeep then
    begin
      Delete(aText, I - 1, 1);
      Dec(I);
    end
    else
    begin
      Delete(aText, I, 1);
      lRemoved := True;
    end;
    Dec(I);
  end;
  Result := lRemoved;
end;

function StripCommentPreservingEscape(const aText: string): string;
var
  I: Integer;
  Ch, lNextCh: Char;
  lBuilder: TStringBuilder;
begin
  lBuilder := TStringBuilder.Create;
  try
    I := 1;
    while I <= Length(aText) do
    begin
      Ch := aText[I];
      if (Ch = '\') and (I < Length(aText)) then
      begin
        lNextCh := aText[I + 1];
        // Only consume backslash when escaping comment characters
        if (lNextCh = '#') or (lNextCh = ';') then
        begin
          lBuilder.Append(lNextCh);
          Inc(I, 2);
          Continue;
        end
        else
        begin
          lBuilder.Append('\');
          Inc(I);
          Continue;
        end;
      end;

      if (Ch = '#') or (Ch = ';') then
      begin
        if (I = 1) or CharInSet(aText[I - 1], [' ', #9]) then
          Break;
      end;

      lBuilder.Append(Ch);
      Inc(I);
    end;
    Result := lBuilder.ToString;
  finally
    lBuilder.Free;
  end;
end;

function DecodeEscapes(const aText: string): string;
var
  I: Integer;
  Ch: Char;
  lBuilder: TStringBuilder;
begin
  lBuilder := TStringBuilder.Create;
  try
    I := 1;
    while I <= Length(aText) do
    begin
      Ch := aText[I];
      if (Ch = '\') and (I < Length(aText)) then
      begin
        Inc(I);
        case aText[I] of
          'n': lBuilder.Append(#10);
          'r': lBuilder.Append(#13);
          't': lBuilder.Append(#9);
          '$': lBuilder.Append('$$'); // ensure literal $ survives expansion
          '\': lBuilder.Append('\');
        else
          // Unknown escape: preserve backslash and char
          lBuilder.Append('\');
          lBuilder.Append(aText[I]);
        end;
      end
      else
        lBuilder.Append(Ch);
      Inc(I);
    end;
    Result := lBuilder.ToString;
  finally
    lBuilder.Free;
  end;
end;

function IsWindowsPlatform: Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := True;
{$ELSE}
  Result := False;
{$ENDIF}
end;

{ TDotEnv.TExprValue }

class function TDotEnv.TExprValue.FromBoolean(const B: Boolean): TExprValue;
begin
  Result.Kind := TExprValueKind.evBoolean;
  Result.BoolValue := B;
  Result.NumValue := Ord(B);
  if B then
    Result.StrValue := 'true'
  else
    Result.StrValue := 'false';
end;

class function TDotEnv.TExprValue.FromNumber(const N: Double): TExprValue;
begin
  Result.Kind := TExprValueKind.evNumber;
  Result.NumValue := N;
  Result.BoolValue := not SameValue(N, 0);
  Result.StrValue := FloatToStr(N, TFormatSettings.Invariant);
end;

class function TDotEnv.TExprValue.FromString(const S: string): TExprValue;
begin
  Result.Kind := TExprValueKind.evString;
  Result.StrValue := S;
  Result.BoolValue := not S.IsEmpty;
  Result.NumValue := 0;
end;

function TDotEnv.TExprValue.ToBoolean: Boolean;
var
  lValue: string;
begin
  case Kind of
    TExprValueKind.evBoolean:
      Exit(BoolValue);
    TExprValueKind.evNumber:
      Exit(not SameValue(NumValue, 0));
    TExprValueKind.evString:
      begin
        lValue := StrValue.ToLower;
        if (lValue = 'true') or (lValue = '1') then
          Exit(True)
        else if (lValue = 'false') or (lValue = '0') or (lValue = '') then
          Exit(False)
        else
          raise Exception.CreateFmt('Cannot convert "%s" to boolean', [StrValue]);
      end;
  end;
  Result := False;
end;

function TDotEnv.TExprValue.ToNumber: Double;
var
  lValue: Double;
begin
  case Kind of
    TExprValueKind.evNumber:
      Exit(NumValue);
    TExprValueKind.evBoolean:
      if BoolValue then
        Exit(1)
      else
        Exit(0);
    TExprValueKind.evString:
      begin
        if TryStrToFloat(StrValue, lValue, TFormatSettings.Invariant) then
          Exit(lValue)
        else
          raise Exception.CreateFmt('Cannot convert "%s" to number', [StrValue]);
      end;
  end;
  raise Exception.Create('Numeric value expected');
end;

function TDotEnv.TExprValue.ToStringValue: string;
begin
  case Kind of
    TExprValueKind.evString:
      Result := StrValue;
    TExprValueKind.evBoolean:
      if BoolValue then
        Result := 'true'
      else
        Result := 'false';
    TExprValueKind.evNumber:
      Result := FloatToStr(NumValue, TFormatSettings.Invariant);
  else
    Result := '';
  end;
end;

{ TDotEnv.TExprParser }

constructor TDotEnv.TExprParser.Create(aOwner: TDotEnv; const aText, aSourceFile: string; aLine: Integer);
begin
  inherited Create;
  FOwner := aOwner;
  FText := aText;
  FSourceFile := aSourceFile;
  FLine := aLine;
  FPos := 1;
end;

function TDotEnv.TExprParser.PeekChar: Char;
begin
  if FPos <= Length(FText) then
    Result := FText[FPos]
  else
    Result := #0;
end;

function TDotEnv.TExprParser.ConsumeChar: Char;
begin
  Result := PeekChar;
  Inc(FPos);
end;

procedure TDotEnv.TExprParser.SkipWhitespace;
begin
  while (FPos <= Length(FText)) and FText[FPos].IsWhiteSpace do
    Inc(FPos);
end;

function TDotEnv.TExprParser.MatchToken(const aToken: string): Boolean;
begin
  if (aToken = '') or (FPos + Length(aToken) - 1 > Length(FText)) then
    Exit(False);
  if Copy(FText, FPos, Length(aToken)) = aToken then
  begin
    Inc(FPos, Length(aToken));
    Exit(True);
  end;
  Result := False;
end;

function TDotEnv.TExprParser.ParseIdentifier: string;
var
  lStartPos: Integer;
begin
  SkipWhitespace;
  lStartPos := FPos;
  if (FPos <= Length(FText)) and (FText[FPos].IsLetter or (FText[FPos] = '_')) then
  begin
    Inc(FPos);
    while (FPos <= Length(FText)) and (FText[FPos].IsLetterOrDigit or (FText[FPos] = '_')) do
      Inc(FPos);
  end;
  Result := Copy(FText, lStartPos, FPos - lStartPos);
  if Result = '' then
    raise Exception.Create('Identifier expected');
end;

function TDotEnv.TExprParser.ParseStringLiteral: TExprValue;
var
  lBuffer: TStringBuilder;
  Ch: Char;
begin
  SkipWhitespace;
  if ConsumeChar <> '''' then
    raise Exception.Create('String literal expected');
  lBuffer := TStringBuilder.Create;
  try
    while FPos <= Length(FText) do
    begin
      Ch := ConsumeChar;
      if Ch = '''' then
      begin
        if (FPos <= Length(FText)) and (FText[FPos] = '''') then
        begin
          ConsumeChar;
          lBuffer.Append('''');
        end
        else
          Break;
      end
      else
        lBuffer.Append(Ch);
    end;
    Result := TExprValue.FromString(lBuffer.ToString);
  finally
    lBuffer.Free;
  end;
end;

function TDotEnv.TExprParser.ParseNumber: TExprValue;
var
  lStartPos: Integer;
  lToken: string;
  lValue: Double;
begin
  SkipWhitespace;
  lStartPos := FPos;
  while (FPos <= Length(FText)) and FText[FPos].IsDigit do
    Inc(FPos);
  if (FPos <= Length(FText)) and (FText[FPos] = '.') then
  begin
    Inc(FPos);
    while (FPos <= Length(FText)) and FText[FPos].IsDigit do
      Inc(FPos);
  end;
  lToken := Copy(FText, lStartPos, FPos - lStartPos);
  if lToken = '' then
    raise Exception.Create('Number expected');
  if not TryStrToFloat(lToken, lValue, TFormatSettings.Invariant) then
    raise Exception.CreateFmt('Invalid number %s', [lToken]);
  Result := TExprValue.FromNumber(lValue);
end;

function TDotEnv.TExprParser.ResolveIdentifier(const aName: string): TExprValue;
var
  lValue: string;
begin
  if SameText(aName, 'true') then
    Exit(TExprValue.FromBoolean(True));
  if SameText(aName, 'false') then
    Exit(TExprValue.FromBoolean(False));
  if SameText(aName, 'null') then
    Exit(TExprValue.FromString(''));
  if FOwner.ResolveSymbol(aName, FSourceFile, lValue) then
    Exit(TExprValue.FromString(lValue));
  if TDotEnvOption.StrictUndefined in FOwner.FCurrentOptions then
    FOwner.AddError(FSourceFile, FLine, 1, TDotEnvErrorKind.DekStrict,
      Format('Undefined identifier %s', [aName]), True);
  Result := TExprValue.FromString('');
end;

function TDotEnv.TExprParser.EvaluateFunction(const aName: string): TExprValue;
var
  lArgs: TArray<TExprValue>;
  lArgIndex: Integer;
  lPathValue: string;
  lArgList: TList<TExprValue>;
begin
  if not MatchToken('(') then
    raise Exception.Create('Expected (');
  lArgList := TList<TExprValue>.Create;
  try
    SkipWhitespace;
    if PeekChar <> ')' then
    begin
      repeat
        lArgList.Add(ParseExpression);
        SkipWhitespace;
      until not MatchToken(',');
    end;
    if not MatchToken(')') then
      raise Exception.Create('Expected )');
    lArgs := lArgList.ToArray;
  finally
    lArgList.Free;
  end;

  if SameText(aName, 'env') then
  begin
    if Length(lArgs) <> 1 then
      raise Exception.Create('env(name) expects one argument');
    Exit(ResolveIdentifier(lArgs[0].ToStringValue));
  end;

  if SameText(aName, 'join') then
  begin
    if Length(lArgs) = 0 then
      raise Exception.Create('join requires arguments');
    lPathValue := lArgs[0].ToStringValue;
    for lArgIndex := 1 to High(lArgs) do
      lPathValue := TPath.Combine(lPathValue, lArgs[lArgIndex].ToStringValue);
    lPathValue := TPath.GetFullPath(lPathValue);
    Exit(TExprValue.FromString(lPathValue));
  end;

  if SameText(aName, 'dirname') then
  begin
    if Length(lArgs) <> 1 then
      raise Exception.Create('dirname expects one argument');
    Exit(TExprValue.FromString(TPath.GetDirectoryName(lArgs[0].ToStringValue)));
  end;

  if SameText(aName, 'basename') then
  begin
    if Length(lArgs) <> 1 then
      raise Exception.Create('basename expects one argument');
    Exit(TExprValue.FromString(TPath.GetFileName(lArgs[0].ToStringValue)));
  end;

  if SameText(aName, 'abspath') then
  begin
    if Length(lArgs) <> 1 then
      raise Exception.Create('abspath expects one argument');
    Exit(TExprValue.FromString(TPath.GetFullPath(lArgs[0].ToStringValue)));
  end;

  if SameText(aName, 'upper') then
  begin
    if Length(lArgs) <> 1 then
      raise Exception.Create('upper expects one argument');
    Exit(TExprValue.FromString(lArgs[0].ToStringValue.ToUpper));
  end;

  if SameText(aName, 'lower') then
  begin
    if Length(lArgs) <> 1 then
      raise Exception.Create('lower expects one argument');
    Exit(TExprValue.FromString(lArgs[0].ToStringValue.ToLower));
  end;

  if SameText(aName, 'trim') then
  begin
    if Length(lArgs) <> 1 then
      raise Exception.Create('trim expects one argument');
    Exit(TExprValue.FromString(lArgs[0].ToStringValue.Trim));
  end;

  raise Exception.CreateFmt('Unknown function %s', [aName]);
end;

function TDotEnv.TExprParser.ParsePrimary: TExprValue;
var
  lIdent: string;
  Ch: Char;
begin
  SkipWhitespace;
  Ch := PeekChar;
  case Ch of
    '(':
      begin
        ConsumeChar;
        Result := ParseExpression;
        SkipWhitespace;
        if not MatchToken(')') then
          raise Exception.Create('Expected )');
      end;
    '''':
      Result := ParseStringLiteral;
    '0' .. '9':
      Result := ParseNumber;
    '-', '+':
      begin
        ConsumeChar;
        Result := ParseNumber;
        if Ch = '-' then
          Result := TExprValue.FromNumber(-Result.ToNumber);
      end;
  else
    begin
      lIdent := ParseIdentifier;
      SkipWhitespace;
      if PeekChar = '(' then
        Result := EvaluateFunction(lIdent)
      else
        Result := ResolveIdentifier(lIdent);
    end;
  end;
end;

function TDotEnv.TExprParser.ParseUnary: TExprValue;
begin
  SkipWhitespace;
  if MatchToken('!') then
    Exit(TExprValue.FromBoolean(not ParseUnary.ToBoolean));
  if MatchToken('-') then
    Exit(TExprValue.FromNumber(-ParseUnary.ToNumber));
  Result := ParsePrimary;
end;

function TDotEnv.TExprParser.ParseMul: TExprValue;
var
  lLeft, lRight: TExprValue;
  Op: Char;
begin
  lLeft := ParseUnary;
  while True do
  begin
    SkipWhitespace;
    Op := PeekChar;
    case Op of
      '*', '/', '%':
        begin
          ConsumeChar;
          lRight := ParseUnary;
          case Op of
            '*': lLeft := TExprValue.FromNumber(lLeft.ToNumber * lRight.ToNumber);
            '/':
              begin
                if SameValue(lRight.ToNumber, 0) then
                  raise Exception.Create('Division by zero');
                lLeft := TExprValue.FromNumber(lLeft.ToNumber / lRight.ToNumber);
              end;
            '%':
              begin
                if SameValue(lRight.ToNumber, 0) then
                  raise Exception.Create('Modulo by zero');
                lLeft := TExprValue.FromNumber(
                  lLeft.ToNumber - Trunc(lLeft.ToNumber / lRight.ToNumber) * lRight.ToNumber
                );
              end;
          end;
        end;
    else
      Exit(lLeft);
    end;
  end;
end;

function TDotEnv.TExprParser.ParseAdd: TExprValue;
var
  lLeft, lRight: TExprValue;
  Op: Char;
begin
  lLeft := ParseMul;
  while True do
  begin
    SkipWhitespace;
    Op := PeekChar;
    case Op of
      '+', '-':
        begin
          ConsumeChar;
          lRight := ParseMul;
          if Op = '+' then
          begin
            if (lLeft.Kind = TExprValueKind.evString) or (lRight.Kind = TExprValueKind.evString) then
              lLeft := TExprValue.FromString(lLeft.ToStringValue + lRight.ToStringValue)
            else
              lLeft := TExprValue.FromNumber(lLeft.ToNumber + lRight.ToNumber);
          end
          else
            lLeft := TExprValue.FromNumber(lLeft.ToNumber - lRight.ToNumber);
        end;
    else
      Exit(lLeft);
    end;
  end;
end;

function TDotEnv.TExprParser.ParseCompare: TExprValue;
var
  lLeft: TExprValue;
begin
  lLeft := ParseAdd;
  while True do
  begin
    SkipWhitespace;
    if MatchToken('==') then
      lLeft := TExprValue.FromBoolean(lLeft.ToStringValue = ParseAdd.ToStringValue)
    else if MatchToken('!=') then
      lLeft := TExprValue.FromBoolean(lLeft.ToStringValue <> ParseAdd.ToStringValue)
    else if MatchToken('<=') then
      lLeft := TExprValue.FromBoolean(lLeft.ToNumber <= ParseAdd.ToNumber)
    else if MatchToken('>=') then
      lLeft := TExprValue.FromBoolean(lLeft.ToNumber >= ParseAdd.ToNumber)
    else if MatchToken('<') then
      lLeft := TExprValue.FromBoolean(lLeft.ToNumber < ParseAdd.ToNumber)
    else if MatchToken('>') then
      lLeft := TExprValue.FromBoolean(lLeft.ToNumber > ParseAdd.ToNumber)
    else
      Exit(lLeft);
  end;
end;

function TDotEnv.TExprParser.ParseAnd: TExprValue;
var
  lLeft, lRight: TExprValue;
begin
  lLeft := ParseCompare;
  while True do
  begin
    SkipWhitespace;
    if MatchToken('&&') then
    begin
      lRight := ParseCompare;
      lLeft := TExprValue.FromBoolean(lLeft.ToBoolean and lRight.ToBoolean);
    end
    else
      Exit(lLeft);
  end;
end;

function TDotEnv.TExprParser.ParseOr: TExprValue;
var
  lLeft, lRight: TExprValue;
begin
  lLeft := ParseAnd;
  while True do
  begin
    SkipWhitespace;
    if MatchToken('||') then
    begin
      lRight := ParseAnd;
      lLeft := TExprValue.FromBoolean(lLeft.ToBoolean or lRight.ToBoolean);
    end
    else
      Exit(lLeft);
  end;
end;

function TDotEnv.TExprParser.ParseExpression: TExprValue;
begin
  Result := ParseOr;
end;

function TDotEnv.TExprParser.Evaluate: TExprValue;
begin
  Result := ParseExpression;
  SkipWhitespace;
  if FPos <= Length(FText) then
    raise Exception.CreateFmt('Unexpected token at position %d', [FPos]);
end;

{ TDotEnv.TKeyComparer }

constructor TDotEnv.TKeyComparer.Create(aCaseSensitive: Boolean);
begin
  inherited Create;
  FCaseSensitive := aCaseSensitive;
  // Use default ordinal comparer; handle case-insensitive logic in Equals/GetHashCode
  FComparer := TEqualityComparer<string>.Default;
end;

function TDotEnv.TKeyComparer.Equals(const aLeft, aRight: string): Boolean;
begin
  if FCaseSensitive then
    Result := FComparer.Equals(aLeft, aRight)
  else
    Result := SameText(aLeft, aRight);
end;

function TDotEnv.TKeyComparer.GetHashCode(const aValue: string): Integer;
begin
  if FCaseSensitive then
    Result := FComparer.GetHashCode(aValue)
  else
    // Hash on a normalized (lowercased) value to stay consistent with SameText equality
    Result := TEqualityComparer<string>.Default.GetHashCode(aValue.ToLower);
end;

{ TDotEnv }

constructor TDotEnv.Create;
begin
  inherited Create;
  InitDefaults;
end;

constructor TDotEnv.Create(aMaxFileSize: Int64; aParentDepth: Integer);
begin
  inherited Create;
  InitDefaults;
  if aMaxFileSize > 0 then
    FMaxFileSize := aMaxFileSize;
  if aParentDepth >= 0 then
    FParentDepth := aParentDepth;
end;

destructor TDotEnv.Destroy;
begin
  FValues.Free;
  FErrors.Free;
  FTrace.Free;
  FInitialEnv.Free;
  FIncludeStack.Free;
  FEvalStack.Free;
  inherited;
end;

procedure TDotEnv.InitDefaults;
begin
  FErrors := TList<TDotEnvError>.Create;
  FTrace := TList<string>.Create;
  FSequence := 0;
  FCaseSensitiveCache := True;
  FIncludeStack := TStack<string>.Create;
  FMaxFileSize := MAX_DOTENV_FILE_SIZE;
  FParentDepth := DEFAULT_PARENT_DEPTH;
  FEnvironmentNamespace := 'maxlogic';
end;

procedure TDotEnv.ResetState;
begin
  FErrors.Clear;
  FTrace.Clear;
  if FIncludeStack <> nil then
    FIncludeStack.Clear;
  // reset deferred evaluation state between loads
  FDeferredActive := False;
  FDeferred := nil;
  if FEvalStack <> nil then
    FEvalStack.Clear;
end;

function TDotEnv.DetermineCaseSensitive: Boolean;
begin
  if TDotEnvOption.CaseSensitiveKeys in FCurrentOptions then
    Exit(True);
  if IsWindowsPlatform then
    Result := False
  else
    Result := True;
end;

procedure TDotEnv.EnsureValuesDictionary;
var
  lDesired: Boolean;
  lNewDict: TDictionary<string, TStoredValue>;
  lPair: TPair<string, TStoredValue>;
begin
  lDesired := DetermineCaseSensitive;
  if FValues = nil then
  begin
    FValues := TDictionary<string, TStoredValue>.Create(TKeyComparer.Create(lDesired));
    FCaseSensitiveCache := lDesired;
    Exit;
  end;
  if lDesired <> FCaseSensitiveCache then
  begin
    lNewDict := TDictionary<string, TStoredValue>.Create(TKeyComparer.Create(lDesired));
    for lPair in FValues do
      lNewDict.AddOrSetValue(NormalizeKey(lPair.Value.Key), lPair.Value);
    FValues.Free;
    FValues := lNewDict;
    FCaseSensitiveCache := lDesired;
  end;
end;

function TDotEnv.NormalizeKey(const aKey: string): string;
begin
  Result := aKey;
end;

procedure TDotEnv.AddError(const aFilePath: string; aLine, aColumn: Integer; aKind: TDotEnvErrorKind;
  const aMsg: string; aFatal: Boolean);
var
  lErr: TDotEnvError;
begin
  lErr.FilePath := aFilePath;
  lErr.Line := aLine;
  lErr.Column := aColumn;
  lErr.Kind := aKind;
  lErr.Message := aMsg;
  lErr.Fatal := aFatal;
  FErrors.Add(lErr);
end;

procedure TDotEnv.AddTrace(const aMsg: string);
begin
  FTrace.Add(aMsg);
end;

procedure TDotEnv.EnsureInitialEnv;
begin
  if FInitialEnv = nil then
    CaptureInitialEnv;
end;

procedure TDotEnv.CaptureInitialEnv;
var
  lComparer: IEqualityComparer<string>;
{$IFDEF MSWINDOWS}
  lBlock: PWideChar;
  lPtr: PWideChar;
  lEntry: string;
  lEqPos: Integer;
{$ELSE}
  lEnv: PPAnsiChar;
  lEntry: string;
  lEqPos: Integer;
{$ENDIF}
begin
  lComparer := TKeyComparer.Create(not IsWindowsPlatform);
  FInitialEnv := TDictionary<string, Byte>.Create(lComparer);
{$IFDEF MSWINDOWS}
  lBlock := GetEnvironmentStringsW;
  if lBlock = nil then
    Exit;
  try
    lPtr := lBlock;
    while lPtr^ <> #0 do
    begin
      lEntry := lPtr;
      lEqPos := Pos('=', lEntry);
      if lEqPos > 1 then
        FInitialEnv.AddOrSetValue(Copy(lEntry, 1, lEqPos - 1), 1);
      Inc(lPtr, lstrlenW(lPtr) + 1);
    end;
  finally
    FreeEnvironmentStringsW(lBlock);
  end;
{$ELSE}
  lEnv := environ;
  while (lEnv <> nil) and (lEnv^ <> nil) do
  begin
    lEntry := string(lEnv^);
    lEqPos := Pos('=', lEntry);
    if lEqPos > 1 then
      FInitialEnv.AddOrSetValue(Copy(lEntry, 1, lEqPos - 1), 1);
    Inc(lEnv);
  end;
{$ENDIF}
end;

function TDotEnv.ProcessEnvHasKey(const aKey: string): Boolean;
var
  lLookup: string;
begin
  EnsureInitialEnv;
  if FInitialEnv = nil then
    Exit(False);
  lLookup := NormalizeKey(aKey);
  Result := FInitialEnv.ContainsKey(lLookup);
end;

function TDotEnv.TryGetValue(const aKey: string; out aValue: string): Boolean;
var
  lStored: TStoredValue;
begin
  EnsureValuesDictionary;
  Result := FValues.TryGetValue(NormalizeKey(aKey), lStored);
  if Result then
    aValue := lStored.Value
  else
    aValue := '';
end;

function TDotEnv.GetValue(const aKey: string; const aDefaultValue: string): string;
begin
  if not TryGetValue(aKey, Result) then
    Result := aDefaultValue;
end;

function TDotEnv.AsDictionary: TDictionary<string, string>;
var
  lDict: TDictionary<string, string>;
  lPair: TPair<string, TStoredValue>;
begin
  EnsureValuesDictionary;
  lDict := TDictionary<string, string>.Create(TKeyComparer.Create(DetermineCaseSensitive));
  for lPair in FValues do
    lDict.AddOrSetValue(lPair.Value.Key, lPair.Value.Value);
  Result := lDict;
end;

function TDotEnv.GetErrors: TArray<TDotEnvError>;
begin
  Result := FErrors.ToArray;
end;

function TDotEnv.GetTrace: TArray<string>;
begin
  Result := FTrace.ToArray;
end;

function TDotEnv.ResolveBuiltIn(const aName, aSourceFile: string; out aValue: string): Boolean;
var
  lDir: string;
begin
  if SameText(aName, 'CUR_DIR') then
  begin
    if aSourceFile <> '' then
      lDir := TPath.GetDirectoryName(aSourceFile)
    else
      lDir := TDirectory.GetCurrentDirectory;
    aValue := lDir;
    Exit(True);
  end;
  if SameText(aName, 'CUR_FILE') then
  begin
    aValue := aSourceFile;
    Exit(True);
  end;
  if SameText(aName, 'DIR_SEP') then
  begin
    aValue := string(TPath.DirectorySeparatorChar);
    Exit(True);
  end;
  if SameText(aName, 'APP_EXE') then
  begin
    aValue := TPath.GetFullPath(ParamStr(0));
    Exit(True);
  end;
  if SameText(aName, 'APP_DIR') then
  begin
    aValue := ExcludeTrailingPathDelimiter(TPath.GetDirectoryName(TPath.GetFullPath(ParamStr(0))));
    Exit(True);
  end;
  Result := False;
end;

function TDotEnv.ResolveSymbol(const aName, aSourceFile: string; out aValue: string): Boolean;
var
  lStored: TStoredValue;
  lNormal: string;
begin
  EnsureValuesDictionary;
  lNormal := NormalizeKey(aName);
  if FValues.TryGetValue(lNormal, lStored) then
  begin
    aValue := lStored.Value;
    Exit(True);
  end;
  // When DoNotOverrideExisting is set, prefer a non-empty process value
  if TDotEnvOption.DoNotOverrideExisting in FCurrentOptions then
  begin
    aValue := GetEnvironmentVariable(aName);
    if aValue <> '' then
      Exit(True);
  end;
  if FDeferredActive and (FDeferred <> nil) then
  begin
    if FDeferred.ContainsKey(lNormal) then
      EvaluateDeferredKey(lNormal);
    if FValues.TryGetValue(lNormal, lStored) then
    begin
      aValue := lStored.Value;
      Exit(True);
    end;
  end;
  aValue := GetEnvironmentVariable(aName);
  if aValue <> '' then
    Exit(True);
  if ResolveBuiltIn(aName, aSourceFile, aValue) then
    Exit(True);
  if Assigned(FOnResolveSymbol) then
  begin
    aValue := FOnResolveSymbol(aName, aSourceFile);
    if aValue <> '' then
      Exit(True);
  end;
  Result := False;
end;

function TDotEnv.ResolveVariable(const aName, aSourceFile: string; aLine: Integer;
  const aDefaultValue: string; aHasDefault: Boolean; out aFailed: Boolean): string;
begin
  aFailed := False;
  if ResolveSymbol(aName, aSourceFile, Result) then
    Exit;
  if aHasDefault then
  begin
    Result := aDefaultValue;
    Exit;
  end;
  if TDotEnvOption.StrictUndefined in FCurrentOptions then
  begin
    aFailed := True;
    AddError(aSourceFile, aLine, 1, TDotEnvErrorKind.DekStrict,
      Format('Undefined variable %s (no default). Use ${%s:-fallback} to provide a default.',
      [aName, aName]), True);
  end;
  Result := '';
end;

function IsNameStartChar(const aC: Char): Boolean;
begin
  Result := aC.IsLetter or (aC = '_');
end;

function IsNameChar(const aC: Char): Boolean;
begin
  Result := aC.IsLetterOrDigit or (aC = '_');
end;

function TDotEnv.ExpandVariable(const aSource, aSourceFile: string; aLine: Integer;
  var aIndex: Integer): string;
var
  lStartPos, lEndPos: Integer;
  lNamePart: string;
  lDefaultValue: string;
  lHasDefault: Boolean;
  lDefaultPos: Integer;
  lFailed: Boolean;
begin
  Result := '';
  lHasDefault := False;
  lDefaultValue := '';
  if aIndex > Length(aSource) then
    Exit;
  if aSource[aIndex] = '{' then
  begin
    lStartPos := aIndex + 1;
    lEndPos := lStartPos;
    while (lEndPos <= Length(aSource)) and (aSource[lEndPos] <> '}') do
      Inc(lEndPos);
    if lEndPos > Length(aSource) then
    begin
      AddError(aSourceFile, aLine, 1, TDotEnvErrorKind.DekParse,
        'Missing closing brace in variable reference', True);
      aIndex := Length(aSource) + 1;
      Exit('');
    end;
    lNamePart := Copy(aSource, lStartPos, lEndPos - lStartPos);
    aIndex := lEndPos + 1;
  end
  else
  begin
    lStartPos := aIndex;
    if not ((lStartPos <= Length(aSource)) and IsNameStartChar(aSource[lStartPos])) then
    begin
      Result := '$' + aSource[aIndex];
      Inc(aIndex);
      Exit;
    end;
    lEndPos := lStartPos;
    Inc(lEndPos);
    while (lEndPos <= Length(aSource)) and IsNameChar(aSource[lEndPos]) do
      Inc(lEndPos);
    lNamePart := Copy(aSource, lStartPos, lEndPos - lStartPos);
    aIndex := lEndPos;
  end;
  lDefaultPos := Pos(':-', lNamePart);
  if lDefaultPos > 0 then
  begin
    lDefaultValue := Copy(lNamePart, lDefaultPos + 2, MaxInt);
    lNamePart := Copy(lNamePart, 1, lDefaultPos - 1);
    lHasDefault := True;
  end;
  lNamePart := Trim(lNamePart);
  Result := ResolveVariable(lNamePart, aSourceFile, aLine, lDefaultValue, lHasDefault, lFailed);
end;

function TDotEnv.ExpandExpression(const aSource, aSourceFile: string; aLine: Integer;
  var aIndex: Integer): string;
var
  lStartPos: Integer;
  lDepth: Integer;
  lExprText: string;
  lParser: TExprParser;
  lValue: TExprValue;
begin
  Result := '';
  if (aIndex > Length(aSource)) or (aSource[aIndex] <> '[') then
    Exit;
  Inc(aIndex);
  lStartPos := aIndex;
  lDepth := 1;
  while (aIndex <= Length(aSource)) and (lDepth > 0) do
  begin
    if aSource[aIndex] = '[' then
      Inc(lDepth)
    else if aSource[aIndex] = ']' then
    begin
      Dec(lDepth);
      if lDepth = 0 then
        Break;
    end;
    Inc(aIndex);
  end;
  if lDepth <> 0 then
  begin
    AddError(aSourceFile, aLine, 1, TDotEnvErrorKind.DekParse, 'Unterminated expression', True);
    aIndex := Length(aSource) + 1;
    Exit;
  end;
  lExprText := Copy(aSource, lStartPos, aIndex - lStartPos);
  Inc(aIndex); // skip closing ]
  // Pre-expand nested constructs (e.g., inner $[ ... ]) before parsing the expression
  lExprText := ExpandValue(lExprText, aSourceFile, aLine);
  lParser := TExprParser.Create(Self, lExprText, aSourceFile, aLine);
  try
    try
      lValue := lParser.Evaluate;
      Result := lValue.ToStringValue;
    except
      on E: Exception do
      begin
        AddError(aSourceFile, aLine, 1, TDotEnvErrorKind.DekParse,
          Format('Expression error: %s', [E.Message]), True);
        Result := '';
      end;
    end;
  finally
    lParser.Free;
  end;
end;

function TDotEnv.ExpandCommand(const aSource, aSourceFile: string; aLine: Integer;
  var aIndex: Integer): string;
var
  lStartPos: Integer;
  lDepth: Integer;
  lCmdText: string;
  lOutput: string;
  lErrMsg: string;
begin
  Result := '';
  if (aIndex > Length(aSource)) or (aSource[aIndex] <> '(') then
    Exit;
  if not (TDotEnvOption.AllowCommandSubst in FCurrentOptions) then
  begin
    // consume command but return literal
    lStartPos := aIndex;
    lDepth := 1;
    Inc(aIndex);
    while (aIndex <= Length(aSource)) and (lDepth > 0) do
    begin
      if aSource[aIndex] = '(' then
        Inc(lDepth)
      else if aSource[aIndex] = ')' then
      begin
        Dec(lDepth);
        if lDepth = 0 then
          Break;
      end;
      Inc(aIndex);
    end;
    if lDepth <> 0 then
    begin
      AddError(aSourceFile, aLine, 1, TDotEnvErrorKind.DekParse,
        'Unterminated command substitution', True);
      aIndex := Length(aSource) + 1;
      Exit;
    end;
    lCmdText := Copy(aSource, lStartPos, aIndex - lStartPos + 1);
    Inc(aIndex);
    AddError(aSourceFile, aLine, 1, TDotEnvErrorKind.DekCommand,
      'Command substitution disabled by configuration', False);
    Result := '$' + lCmdText;
    Exit;
  end;

  Inc(aIndex);
  lStartPos := aIndex;
  lDepth := 1;
  while (aIndex <= Length(aSource)) and (lDepth > 0) do
  begin
    if aSource[aIndex] = '(' then
      Inc(lDepth)
    else if aSource[aIndex] = ')' then
    begin
      Dec(lDepth);
      if lDepth = 0 then
        Break;
    end;
    Inc(aIndex);
  end;
  if lDepth <> 0 then
  begin
    AddError(aSourceFile, aLine, 1, TDotEnvErrorKind.DekParse,
      'Unterminated command substitution', True);
    aIndex := Length(aSource) + 1;
    Exit;
  end;
  lCmdText := Copy(aSource, lStartPos, aIndex - lStartPos);
  Inc(aIndex);
  if ExecuteCommand(lCmdText, lOutput, lErrMsg) then
    Result := lOutput.Trim
  else
  begin
    AddError(aSourceFile, aLine, 1, TDotEnvErrorKind.DekCommand, lErrMsg, False);
    Result := '';
  end;
end;

function TDotEnv.ExpandValue(const aRawValue, aSourceFile: string; aLine: Integer): string;
var
  lSB: TStringBuilder;
  I: Integer;
  Ch: Char;
begin
  lSB := TStringBuilder.Create;
  try
    I := 1;
    while I <= Length(aRawValue) do
    begin
      Ch := aRawValue[I];
      if Ch = '$' then
      begin
        Inc(I);
        if I > Length(aRawValue) then
        begin
          lSB.Append('$');
          Break;
        end;
        case aRawValue[I] of
          '{', '_', 'A' .. 'Z', 'a' .. 'z':
            lSB.Append(ExpandVariable(aRawValue, aSourceFile, aLine, I));
          '[':
            lSB.Append(ExpandExpression(aRawValue, aSourceFile, aLine, I));
          '(':
            lSB.Append(ExpandCommand(aRawValue, aSourceFile, aLine, I));
          '$':
            begin
              lSB.Append('$');
              Inc(I);
            end;
        else
          lSB.Append('$');
          lSB.Append(aRawValue[I]);
          Inc(I);
        end;
      end
      else
      begin
        lSB.Append(Ch);
        Inc(I);
      end;
    end;
    Result := lSB.ToString;
  finally
    lSB.Free;
  end;
end;

function TDotEnv.Expand(const aText: string; const aSourceFilePath: string): string;
begin
  Result := ExpandValue(aText, aSourceFilePath, 0);
end;

procedure TDotEnv.StoreValue(const aAssignment: TRawAssignment; const aExpandedValue: string);
var
  lNormal: string;
  lStored: TStoredValue;
  lSkipOverride: Boolean;
begin
  EnsureValuesDictionary;
  lNormal := NormalizeKey(aAssignment.Key);
  lSkipOverride := False;
  if (TDotEnvOption.DoNotOverrideExisting in FCurrentOptions) and
     (not FValues.ContainsKey(lNormal)) and ProcessEnvHasKey(aAssignment.Key) then
    lSkipOverride := True;
  if lSkipOverride then
  begin
    AddTrace(Format('skip existing env: %s', [aAssignment.Key]));
    Exit;
  end;
  lStored.Key := aAssignment.Key;
  lStored.Value := aExpandedValue;
  lStored.SourceFile := aAssignment.FilePath;
  lStored.Line := aAssignment.Line;
  lStored.RootIndex := aAssignment.RootIndex;
  lStored.Sequence := FSequence;
  Inc(FSequence);
  FValues.AddOrSetValue(lNormal, lStored);
  // do not export to process environment (prevents cross-test/state leakage)
  // ApplyToProcessEnv(aAssignment.Key, aExpandedValue);
end;

procedure TDotEnv.ApplyToProcessEnv(const aKey, aValue: string);
begin
{$IFDEF MSWINDOWS}
  SetEnvironmentVariable(PChar(aKey), PChar(aValue));
{$ELSE}
  setenv(PAnsiChar(AnsiString(aKey)), PAnsiChar(AnsiString(aValue)), 1);
{$ENDIF}
end;

procedure TDotEnv.EvaluateDeferredKey(const aNormalizedKey: string);
var
  A: TRawAssignment;
  lExpanded: string;
  lErrBefore, lErrAfter: Integer;
  i: Integer;
  lHasNewFatal: Boolean;
begin
  if not FDeferredActive or (FDeferred = nil) then
    Exit;
  if not FDeferred.TryGetValue(aNormalizedKey, A) then
    Exit; // already evaluated or not a winner
  if (FEvalStack <> nil) and FEvalStack.Contains(aNormalizedKey) then
  begin
    AddError(A.FilePath, A.Line, 1, TDotEnvErrorKind.DekCycle,
      Format('Cyclic reference for "%s"', [A.Key]), True);
    // break the cycle; do not attempt to store a value
    FDeferred.Remove(aNormalizedKey);
    Exit;
  end;
  if FEvalStack = nil then
    FEvalStack := THashSet<string>.Create(TKeyComparer.Create(DetermineCaseSensitive));
  FEvalStack.Add(aNormalizedKey);
  lErrBefore := FErrors.Count;
  try
    lExpanded := ExpandValue(A.RawValue, A.FilePath, A.Line);
  finally
    FEvalStack.Remove(aNormalizedKey);
  end;
  lErrAfter := FErrors.Count;
  // Detect only newly-added fatal errors during this evaluation
  lHasNewFatal := False;
  for i := lErrBefore to lErrAfter - 1 do
    if FErrors[i].Fatal then
    begin
      lHasNewFatal := True;
      Break;
    end;

  FDeferred.Remove(aNormalizedKey);
  if not lHasNewFatal then
    StoreValue(A, lExpanded)
  else
    AddTrace(Format('skip store due to fatal errors: %s', [A.Key]));
end;

procedure TDotEnv.EvaluateAfterMergeEntries(const aEntries: TList<TRawAssignment>);
var
  lWinners: TDictionary<string, TRawAssignment>;
  lEntry: TRawAssignment;
  lKeys: TArray<string>;
  K: string;
  lKeyList: TList<string>;
begin
  EnsureValuesDictionary;
  lWinners := TDictionary<string, TRawAssignment>.Create(TKeyComparer.Create(DetermineCaseSensitive));
  try
    // last-wins by precedence implied by aEntries order
    for lEntry in aEntries do
      lWinners.AddOrSetValue(NormalizeKey(lEntry.Key), lEntry);

    FDeferredActive := True;
    FEvalStack := THashSet<string>.Create(TKeyComparer.Create(DetermineCaseSensitive));
    FDeferred := lWinners;
    try
      // snapshot keys since EvaluateDeferredKey mutates FDeferred/lWinners
      lKeyList := TList<string>.Create;
      try
        for K in lWinners.Keys do
          lKeyList.Add(K);
        lKeys := lKeyList.ToArray;
      finally
        lKeyList.Free;
      end;
      for K in lKeys do
        EvaluateDeferredKey(K);
    finally
      FEvalStack.Free;
      FEvalStack := nil;
      FDeferred := nil;
      FDeferredActive := False;
    end;
  finally
    lWinners.Free;
  end;
end;

function TDotEnv.ExecuteCommand(const aCmd: string; out aOutput: string; out aErrMsg: string): Boolean;
{$IFDEF MSWINDOWS}
var
  lReadPipe, lWritePipe: THandle;
  lSecurity: TSecurityAttributes;
  lStartInfo: TStartupInfo;
  lProcInfo: TProcessInformation;
  lBuffer: array[0..2047] of AnsiChar;
  lBytesRead: Cardinal;
  lChunk: AnsiString;
  lCmdLine: string;
  lRaw: string;
  lPayload: string;
  lInner: string;
  lTempScript: string;
  lNeedDeleteTemp: Boolean;
  lFSrc, lFDst: TFileStream;
  lFirst3: array[0..2] of Byte;
  lReadCount, N: Integer;
  lBuf: TBytes;
  lComSpec: string;
  lExitCode: Cardinal;
  lGuid: TGUID;
{$ELSE}
var
  lPipe: PIOFile;
  lBuffer: array[0..1023] of AnsiChar;
  lLinePtr: PAnsiChar;
  lStatus: Integer;
  {$IF DECLARED(WIFEXITED)}
  lExitCode: Integer;
  {$IFEND}
  lAccum: AnsiString;
{$ENDIF}
begin
  // Execute external command, capture stdout; implementation varies by platform
  aOutput := '';
  aErrMsg := '';
  Result := False;
{$IFDEF MSWINDOWS}
  lNeedDeleteTemp := False;
  lSecurity := Default(TSecurityAttributes);
  lSecurity.nLength := SizeOf(lSecurity);
  lSecurity.bInheritHandle := True;
  if not CreatePipe(lReadPipe, lWritePipe, @lSecurity, 0) then
  begin
    aErrMsg := SysErrorMessage(GetLastError);
    Exit(False);
  end;
  try
    if not SetHandleInformation(lReadPipe, HANDLE_FLAG_INHERIT, 0) then
    begin
      aErrMsg := SysErrorMessage(GetLastError);
      Exit(False);
    end;
    lStartInfo := Default(TStartupInfo);
    lStartInfo.cb := SizeOf(lStartInfo);
    lStartInfo.dwFlags := STARTF_USESTDHANDLES;
    lStartInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
    lStartInfo.hStdOutput := lWritePipe;
    lStartInfo.hStdError := lWritePipe;
    lProcInfo := Default(TProcessInformation);

    // Resolve command processor
    lComSpec := GetEnvironmentVariable('COMSPEC');
    if lComSpec = '' then
      lComSpec := 'cmd.exe';

    // Normalize payload
    lRaw := Trim(aCmd);
    lPayload := lRaw;

    // If payload is a single quoted path to a .bat/.cmd file, and it has a UTF-8 BOM,
    // create a temporary copy without BOM to avoid cmd.exe parse failures.
    if (lPayload <> '') and (lPayload[1] = '"') and (lPayload[Length(lPayload)] = '"') then
    begin
      lInner := Copy(lPayload, 2, Length(lPayload) - 2);
      if (Pos('"', lInner) = 0) and TFile.Exists(lInner) then
      begin
        if SameText(TPath.GetExtension(lInner), '.bat') or SameText(TPath.GetExtension(lInner), '.cmd') then
        begin
          lFSrc := TFileStream.Create(lInner, fmOpenRead or fmShareDenyWrite);
          try
            lReadCount := lFSrc.Read(lFirst3, 3);
            if (lReadCount = 3) and (lFirst3[0] = $EF) and (lFirst3[1] = $BB) and (lFirst3[2] = $BF) then
            begin
              CreateGUID(lGuid);
              lTempScript := TPath.Combine(TPath.GetTempPath,
                GUIDToString(lGuid).Replace('{','').Replace('}','') + TPath.GetExtension(lInner));
              lFDst := TFileStream.Create(lTempScript, fmCreate);
              try
                SetLength(lBuf, 64 * 1024);
                while True do
                begin
                  N := lFSrc.Read(lBuf[0], Length(lBuf));
                  if N <= 0 then Break;
                  lFDst.Write(lBuf[0], N);
                end;
              finally
                lFDst.Free;
              end;
              lPayload := '"' + lTempScript + '"';
              lNeedDeleteTemp := True;
            end;
          finally
            lFSrc.Free;
          end;
        end;
      end;
    end;

    // Build the final command line for cmd.exe
    if (lPayload <> '') and (lPayload[1] = '"') and (lPayload[Length(lPayload)] = '"') then
      lCmdLine := '/S /C ' + lPayload
    else
      lCmdLine := '/S /C "' + lPayload + '"';
    AddTrace('exec: ' + lCmdLine);
    UniqueString(lCmdLine); // ensure mutable buffer for CreateProcess
    if not CreateProcess(PChar(lComSpec), PChar(lCmdLine), nil, nil, True,
      CREATE_NO_WINDOW, nil, nil, lStartInfo, lProcInfo) then
    begin
      aErrMsg := SysErrorMessage(GetLastError);
      Exit(False);
    end;
    CloseHandle(lWritePipe);
    lWritePipe := 0;
    try
      repeat
        if not ReadFile(lReadPipe, lBuffer, SizeOf(lBuffer), lBytesRead, nil) then
          Break;
        if lBytesRead = 0 then
          Break;
        SetString(lChunk, PAnsiChar(@lBuffer[0]), lBytesRead);
        aOutput := aOutput + string(lChunk);
      until False;
      WaitForSingleObject(lProcInfo.hProcess, INFINITE);
      if not GetExitCodeProcess(lProcInfo.hProcess, lExitCode) then
        lExitCode := 1;
      if lExitCode <> 0 then
      begin
        aErrMsg := Format('Command exited with code %d', [lExitCode]);
        Result := False;
      end
      else
        Result := True;
    finally
      CloseHandle(lProcInfo.hThread);
      CloseHandle(lProcInfo.hProcess);
      if lNeedDeleteTemp and (lTempScript <> '') then
      begin
        try
          TFile.Delete(lTempScript);
        except
          // ignore cleanup failures
        end;
      end;
    end;
  finally
    if lWritePipe <> 0 then
      CloseHandle(lWritePipe);
    CloseHandle(lReadPipe);
  end;
{$ELSE}
  lAccum := '';
  lPipe := popen(PAnsiChar(AnsiString(aCmd)), PAnsiChar(AnsiString('r')));
  if lPipe = nil then
  begin
    aErrMsg := 'Failed to execute command';
    Exit(False);
  end;
  try
    while True do
    begin
      lLinePtr := fgets(@lBuffer[0], SizeOf(lBuffer), lPipe);
      if lLinePtr = nil then
        Break;
      lAccum := lAccum + AnsiString(lLinePtr);
    end;
  finally
    lStatus := pclose(lPipe);
  end;
  aOutput := string(lAccum);
  // Interpret exit status: prefer sys/wait helpers when available; otherwise fall back to raw Status.
  {$IF DECLARED(WIFEXITED)}
  if WIFEXITED(lStatus) then
    lExitCode := WEXITSTATUS(lStatus)
  else
    lExitCode := 1;
  Result := (lExitCode = 0);
  if not Result then
    aErrMsg := Format('Command exited with code %d', [lExitCode]);
  {$ELSE}
  Result := (lStatus = 0);
  if not Result then
    aErrMsg := Format('Command exited with code %d', [lStatus]);
  {$IFEND}
{$ENDIF}
end;

procedure TDotEnv.CollectFile(const aFilePath: string; aRootIndex, aFileOrder: Integer;
  aEntries: TList<TRawAssignment>);
var
  lLoaded: Boolean;
  lPathToUse: string;
  lDir: string;
  lCandidate: string;
  lItems: TArray<string>;
begin
  AddTrace('considered: ' + aFilePath);
  lPathToUse := aFilePath;

  // POSIX: allow case-insensitive match for ".env.secret"
  if not TFile.Exists(lPathToUse) and SameText(TPath.GetFileName(lPathToUse), '.env.secret') then
  begin
    lDir := TPath.GetDirectoryName(lPathToUse);
    try
      lItems := TDirectory.GetFiles(lDir);
      for lCandidate in lItems do
        if SameText(TPath.GetFileName(lCandidate), '.env.secret') then
        begin
          lPathToUse := lCandidate;
          Break;
        end;
    except
      // ignore directory listing failures; fall back to original path
    end;
  end;

  if TFile.Exists(lPathToUse) then
  begin
    lLoaded := ParseFile(lPathToUse, aRootIndex, aFileOrder, aEntries);
    if lLoaded then
      AddTrace('loaded: ' + lPathToUse)
    else
      AddTrace('ignored: ' + lPathToUse);
  end
  else
    AddTrace('missing: ' + lPathToUse);
end;

function TDotEnv.ParseFile(const aFilePath: string; aRootIndex, aFileOrder: Integer;
  aEntries: TList<TRawAssignment>): Boolean;
var
  lContent: string;
  lSize: Int64;
  lNormalPath: string;
  lFS: TFileStream;
begin
  Result := False;
  lNormalPath := TPath.GetFullPath(aFilePath);
  if lNormalPath = '' then
    Exit;
  if IsPathOnStack(lNormalPath) then
  begin
    AddError(lNormalPath, 0, 0, TDotEnvErrorKind.DekCycle,
      Format('Include cycle detected involving %s', [lNormalPath]), True);
    Exit;
  end;
  if not CheckSecretPermissions(lNormalPath) then
    Exit;
  FIncludeStack.Push(lNormalPath);
  try
    try
      if not TFile.Exists(lNormalPath) then
        Exit;
      lFS := TFileStream.Create(lNormalPath, fmOpenRead or fmShareDenyWrite);
      try
        lSize := lFS.Size;
      finally
        lFS.Free;
      end;
      if lSize > FMaxFileSize then
      begin
        AddError(lNormalPath, 0, 0, TDotEnvErrorKind.DekSecurity,
          Format('File %s exceeds maximum size', [lNormalPath]), True);
        Exit;
      end;
      lContent := TFile.ReadAllText(lNormalPath, TEncoding.UTF8);
    except
      on E: Exception do
      begin
        AddError(lNormalPath, 0, 0, TDotEnvErrorKind.DekIO, E.Message, True);
        Exit;
      end;
    end;
    lContent := StringReplace(lContent, #13#10, #10, [rfReplaceAll]);
    lContent := StringReplace(lContent, #13, #10, [rfReplaceAll]);
    ParseContent(lNormalPath, aRootIndex, aFileOrder, lContent, aEntries);
    Result := True;
  finally
    FIncludeStack.Pop;
  end;
end;

function TDotEnv.ParseDoubleQuoted(const aFilePath: string; const aFragment: string;
  aLineNumber: Integer): string;
var
  lBuffer: TStringBuilder;
  I, lLen: Integer;
  Ch: Char;
begin
  lBuffer := TStringBuilder.Create;
  try
    lLen := Length(aFragment);
    I := 2;
    while I <= lLen do
    begin
      Ch := aFragment[I];
      if Ch = '"' then
        Break;
      if (Ch = '\') and (I < lLen) then
      begin
        Inc(I);
        case aFragment[I] of
          'n': lBuffer.Append(#10);
          'r': lBuffer.Append(#13);
          't': lBuffer.Append(#9);
          '"': lBuffer.Append('"');
          '\': lBuffer.Append('\');
          '$': lBuffer.Append('$$'); // ensure literal $ survives expansion
        else
          lBuffer.Append(aFragment[I]);
        end;
      end
      else
        lBuffer.Append(Ch);
      Inc(I);
    end;
    if (I > lLen) or (aFragment[I] <> '"') then
      AddError(aFilePath, aLineNumber, 1, TDotEnvErrorKind.DekParse, 'Unterminated double quote', True);
    Result := lBuffer.ToString;
  finally
    lBuffer.Free;
  end;
end;

function TDotEnv.ParseSingleQuoted(const aFilePath: string; const aFragment: string;
  aLineNumber: Integer): string;
var
  lBuffer: TStringBuilder;
  I, lLen: Integer;
  Ch: Char;
begin
  lBuffer := TStringBuilder.Create;
  try
    lLen := Length(aFragment);
    I := 2;
    while I <= lLen do
    begin
      Ch := aFragment[I];
      if Ch = '''' then
      begin
        if (I < lLen) and (aFragment[I + 1] = '''') then
        begin
          lBuffer.Append('''');
          Inc(I);
        end
        else
          Break;
      end
      else
        lBuffer.Append(Ch);
      Inc(I);
    end;
    if (I > lLen) or (aFragment[I] <> '''') then
      AddError(aFilePath, aLineNumber, 1, TDotEnvErrorKind.DekParse, 'Unterminated single quote', True);
    Result := lBuffer.ToString;
  finally
    lBuffer.Free;
  end;
end;

function TDotEnv.ParseTripleQuoted(const aFilePath: string; var aLineIndex: Integer;
  const aLines: TArray<string>; const aFragment: string; const aQuoteToken: string;
  aLineNumber: Integer): string;
var
  lBuffer: TStringBuilder;
  lCurrent: string;
  lDelim: string;
  lPosClose: Integer;
  lFound: Boolean;
begin
  lBuffer := TStringBuilder.Create;
  try
    lDelim := aQuoteToken;
    lCurrent := Copy(aFragment, Length(lDelim) + 1, MaxInt);
    if (lCurrent <> '') and (lCurrent[1] = #10) then
      Delete(lCurrent, 1, 1);
    lFound := False;
    while True do
    begin
      lPosClose := Pos(lDelim, lCurrent);
      if lPosClose > 0 then
      begin
        if lPosClose > 1 then
        begin
          if lBuffer.Length > 0 then
            lBuffer.Append(#10);
          lBuffer.Append(Copy(lCurrent, 1, lPosClose - 1));
        end;
        lFound := True;
        Break;
      end;
      if lBuffer.Length > 0 then
        lBuffer.Append(#10);
      lBuffer.Append(lCurrent);
      if aLineIndex >= High(aLines) then
        Break;
      Inc(aLineIndex);
      lCurrent := aLines[aLineIndex];
    end;
    if not lFound then
      AddError(aFilePath, aLineNumber, 1, TDotEnvErrorKind.DekParse, 'Unterminated triple quote', True);
    Result := lBuffer.ToString;
  finally
    lBuffer.Free;
  end;
end;

function TDotEnv.ParseHeredoc(const aFilePath: string; var aLineIndex: Integer;
  const aLines: TArray<string>; const aMarker: string; aLineNumber: Integer): string;
var
  lBuffer: TStringBuilder;
  lCurrent: string;
  lFound: Boolean;
begin
  lBuffer := TStringBuilder.Create;
  try
    lFound := False;
    while aLineIndex < High(aLines) do
    begin
      Inc(aLineIndex);
      lCurrent := aLines[aLineIndex];
      if lCurrent = aMarker then
      begin
        lFound := True;
        Break;
      end;
      if lBuffer.Length > 0 then
        lBuffer.Append(#10);
      lBuffer.Append(lCurrent);
    end;
    if not lFound then
      AddError(aFilePath, aLineNumber, 1, TDotEnvErrorKind.DekParse, 'Unterminated heredoc', True);
    Result := lBuffer.ToString;
  finally
    lBuffer.Free;
  end;
end;

function TDotEnv.ParseUnquoted(const aFilePath: string; var aLineIndex: Integer;
  const aLines: TArray<string>; const aFragment: string; aLineNumber: Integer): string;
var
  lBuffer: TStringBuilder;
  lCurrent: string;
  lSegment: string;
  lContinueLine: Boolean;
begin
  lBuffer := TStringBuilder.Create;
  try
    lCurrent := aFragment;
    repeat
      lSegment := StripCommentPreservingEscape(lCurrent);
      StripTrailingWhitespaceWithEscape(lSegment);
      lContinueLine := False;
      if (Length(lSegment) > 0) and (lSegment[Length(lSegment)] = '\') then
      begin
        Delete(lSegment, Length(lSegment), 1);
        lContinueLine := True;
      end;
      lSegment := DecodeEscapes(lSegment);
      lBuffer.Append(lSegment);
      if lContinueLine then
      begin
        lBuffer.Append(#10);
        if aLineIndex >= High(aLines) then
        begin
          AddError(aFilePath, aLineNumber, 1, TDotEnvErrorKind.DekParse,
            'Line continuation reached end of file', True);
          Break;
        end;
        Inc(aLineIndex);
        lCurrent := aLines[aLineIndex];
      end;
    until not lContinueLine;
    Result := lBuffer.ToString;
  finally
    lBuffer.Free;
  end;
end;

function TDotEnv.ParseValue(const aFilePath: string; var aLineIndex: Integer;
  const aLines: TArray<string>; const aInitialRemainder: string; aLineNumber: Integer): string;
var
  lFragment: string;
  lMarker: string;
  lTripleDouble: string;
  lTripleSingle: string;
begin
  lFragment := aInitialRemainder;
  if lFragment = '' then
    Exit('');
  lTripleDouble := StringOfChar('"', 3);
  lTripleSingle := StringOfChar(#39, 3);
  if Length(lFragment) >= 3 then
  begin
    if Copy(lFragment, 1, 3) = lTripleDouble then
      Exit(ParseTripleQuoted(aFilePath, aLineIndex, aLines, lFragment, lTripleDouble, aLineNumber));
    if Copy(lFragment, 1, 3) = lTripleSingle then
      Exit(ParseTripleQuoted(aFilePath, aLineIndex, aLines, lFragment, lTripleSingle, aLineNumber));
  end;
  if (Length(lFragment) >= 2) and (Copy(lFragment, 1, 2) = '<<') then
  begin
    lMarker := Trim(Copy(lFragment, 3, MaxInt));
    if lMarker = '' then
    begin
      AddError(aFilePath, aLineNumber, 1, TDotEnvErrorKind.DekParse, 'Heredoc marker expected', True);
      Exit('');
    end;
    Exit(ParseHeredoc(aFilePath, aLineIndex, aLines, lMarker, aLineNumber));
  end;
  if lFragment[1] = '"' then
    Exit(ParseDoubleQuoted(aFilePath, lFragment, aLineNumber));
  if lFragment[1] = '''' then
    Exit(ParseSingleQuoted(aFilePath, lFragment, aLineNumber));
  Result := ParseUnquoted(aFilePath, aLineIndex, aLines, lFragment, aLineNumber);
end;

function TDotEnv.ParseKeyValue(const aFilePath: string; const aLine: string; aLineNumber: Integer;
  aRootIndex, aFileOrder: Integer; out aAssignment: TRawAssignment): Boolean;
var
  lLen: Integer;
  I: Integer;
  lKeyStart, lKeyEnd: Integer;
  lDelim: Char;
  lRemainder: string;
  lTrimmed: string;
begin
  Result := False;
  lTrimmed := Trim(aLine);
  if lTrimmed = '' then
    Exit(False);
  if lTrimmed[1] in ['#', ';'] then
    Exit(False);

  lLen := Length(aLine);
  I := 1;
  while (I <= lLen) and (aLine[I] in [' ', #9]) do
    Inc(I);
  if (I > lLen) or not IsNameStartChar(aLine[I]) then
  begin
    AddError(aFilePath, aLineNumber, 1, TDotEnvErrorKind.DekParse, 'Invalid key', True);
    Exit(False);
  end;
  lKeyStart := I;
  Inc(I);
  while (I <= lLen) and IsNameChar(aLine[I]) do
    Inc(I);
  lKeyEnd := I - 1;
  while (I <= lLen) and (aLine[I] in [' ', #9]) do
    Inc(I);
  if I > lLen then
  begin
    AddError(aFilePath, aLineNumber, 1, TDotEnvErrorKind.DekParse, 'Expected delimiter', True);
    Exit(False);
  end;
  lDelim := aLine[I];
  if (lDelim = '<') and (I < lLen) and (aLine[I + 1] = '<') then
  begin
    lRemainder := Copy(aLine, I, MaxInt);
  end
  else
  begin
    if not (lDelim in ['=', ':']) then
    begin
      AddError(aFilePath, aLineNumber, 1, TDotEnvErrorKind.DekParse, 'Expected = or :', True);
      Exit(False);
    end;
    Inc(I);
    while (I <= lLen) and (aLine[I] in [' ', #9]) do
      Inc(I);
    lRemainder := Copy(aLine, I, MaxInt);
  end;

  aAssignment.Key := Copy(aLine, lKeyStart, lKeyEnd - lKeyStart + 1);
  aAssignment.RawValue := lRemainder;
  aAssignment.FilePath := aFilePath;
  aAssignment.Line := aLineNumber;
  aAssignment.RootIndex := aRootIndex;
  aAssignment.FileOrder := aFileOrder;
  Result := True;
end;

procedure TDotEnv.ParseContent(const aFilePath: string; aRootIndex, aFileOrder: Integer;
  const aContent: string; aEntries: TList<TRawAssignment>);
var
  lLines: TArray<string>;
  lLineIndex: Integer;
  lLine: string;
  lAssignment: TRawAssignment;
  lValue: string;
begin
  lLines := aContent.Split([#10]);
  lLineIndex := 0;
  while lLineIndex <= High(lLines) do
  begin
    lLine := lLines[lLineIndex];
    if HandleIncludeLine(lLine, aFilePath, lLineIndex + 1, aRootIndex, aFileOrder, aEntries) then
    begin
      Inc(lLineIndex);
      Continue;
    end;
    if ParseKeyValue(aFilePath, lLine, lLineIndex + 1, aRootIndex, aFileOrder, lAssignment) then
    begin
      lValue := ParseValue(aFilePath, lLineIndex, lLines, lAssignment.RawValue, lAssignment.Line);
      lAssignment.RawValue := lValue;
      aEntries.Add(lAssignment);
    end;
    Inc(lLineIndex);
  end;
end;

procedure TDotEnv.LoadFiles(const aFiles: array of string; const aOptions: TDotEnvOptions);
var
  lEntries: TList<TRawAssignment>;
  I: Integer;
  lFilePath: string;
  lEntry: TRawAssignment;
  lValue: string;
  lFileOrder: Integer;
  j: Integer;
  lErrStart: Integer;
  lHasFatal: Boolean;
begin
  FCurrentOptions := aOptions;
  ResetState;
  EnsureValuesDictionary;
  lEntries := TList<TRawAssignment>.Create;
  try
    lFileOrder := 0;
    for I := Low(aFiles) to High(aFiles) do
    begin
      lFilePath := TPath.GetFullPath(aFiles[I]);
      CollectFile(lFilePath, 0, lFileOrder, lEntries);
      Inc(lFileOrder);
    end;
    if TDotEnvOption.StreamingEvaluation in FCurrentOptions then
    begin
      for I := 0 to lEntries.Count - 1 do
      begin
        lEntry := lEntries[I];
        lErrStart := FErrors.Count;
        lValue := ExpandValue(lEntry.RawValue, lEntry.FilePath, lEntry.Line);
        lHasFatal := False;
        for j := lErrStart to FErrors.Count - 1 do
          if FErrors[j].Fatal then
          begin
            lHasFatal := True;
            Break;
          end;
        if not lHasFatal then
          StoreValue(lEntry, lValue)
        else
          AddTrace(Format('skip store due to fatal errors: %s (%s:%d)',
            [lEntry.Key, lEntry.FilePath, lEntry.Line]));
      end;
    end
    else
      EvaluateAfterMergeEntries(lEntries);
  finally
    lEntries.Free;
  end;
end;

procedure TDotEnv.SetSearchRoots(const aRoots: TArray<TSearchRoot>);
begin
  FCustomRoots := Copy(aRoots, 0, Length(aRoots));
  FHasCustomRoots := Length(FCustomRoots) > 0;
end;

procedure TDotEnv.SetEnvironmentNamespace(const aValue: string);
begin
  FEnvironmentNamespace := aValue.Trim;
end;

function TDotEnv.ExpandRoots(const aBaseDir: string; const aOptions: TDotEnvOptions): TArray<TSearchRoot>;
var
  lRoots: TList<TSearchRoot>;
  lSeen: TDictionary<string, Byte>;
  lBasePath: string;
  lCurrent: string;
  lParentPath: string;
  lDepth: Integer;
  lHomeDir: string;
  lNamespace: string;
{$IFNDEF MSWINDOWS}
  lXdgHome: string;
  lXdgDirs: string;
  lParts: TArray<string>;
  lDirItem: string;
{$ENDIF}

  function NormalizePathKey(const aPath: string): string;
  begin
    Result := TPath.GetFullPath(aPath);
  end;

  function AppendNamespace(const aBase: string): string;
  begin
    if lNamespace = '' then
      Exit(aBase);
    Result := TPath.Combine(aBase, lNamespace);
  end;

  procedure AddRoot(const aKind: TSearchRootKind; const aPath: string);
  var
    lRootItem: TSearchRoot;
    lKeyPath: string;
  begin
    if aPath = '' then
      Exit;
    lKeyPath := NormalizePathKey(aPath);
    if lKeyPath = '' then
      Exit;
    if not lSeen.ContainsKey(lKeyPath) then
    begin
      lRootItem.Kind := aKind;
      lRootItem.Path := TPath.GetFullPath(aPath);
      lRoots.Add(lRootItem);
      lSeen.AddOrSetValue(lKeyPath, 1);
    end;
  end;

begin
  if FHasCustomRoots then
  begin
    Result := Copy(FCustomRoots, 0, Length(FCustomRoots));
    Exit;
  end;

  lNamespace := FEnvironmentNamespace;

  lRoots := TList<TSearchRoot>.Create;
  lSeen := TDictionary<string, Byte>.Create(TKeyComparer.Create(not IsWindowsPlatform));
  try
    lBasePath := aBaseDir;
    if lBasePath = '' then
      lBasePath := TDirectory.GetCurrentDirectory;
    lBasePath := TPath.GetFullPath(lBasePath);
    AddRoot(TSearchRootKind.srCWD, lBasePath);

    if FParentDepth > 0 then
    begin
      lCurrent := lBasePath;
      for lDepth := 1 to FParentDepth do
      begin
        lParentPath := TPath.GetDirectoryName(unSlash(lCurrent));
        if (lParentPath = '') or MaxLogic.ioUtils.SamePath(lParentPath, lCurrent) then
          Break;
        AddRoot(TSearchRootKind.srParents, lParentPath);
        lCurrent := lParentPath;
      end;
    end;

{$IFNDEF MSWINDOWS}
    if TDotEnvOption.SearchXDG in aOptions then
    begin
      lHomeDir := TPath.GetHomePath;
      lXdgHome := GetEnvironmentVariable('XDG_CONFIG_HOME');
      if (lXdgHome = '') and (lHomeDir <> '') then
        lXdgHome := TPath.Combine(lHomeDir, '.config');
      if lXdgHome <> '' then
        AddRoot(TSearchRootKind.srXDG, AppendNamespace(lXdgHome));
      lXdgDirs := GetEnvironmentVariable('XDG_CONFIG_DIRS');
      if lXdgDirs = '' then
        lXdgDirs := '/etc/xdg';
      lParts := lXdgDirs.Split([':']);
      for lDirItem in lParts do
        if lDirItem.Trim <> '' then
          AddRoot(TSearchRootKind.srXDG, AppendNamespace(lDirItem.Trim));
    end;
{$ENDIF}

    if TDotEnvOption.SearchUserHome in aOptions then
    begin
      lHomeDir := TPath.GetHomePath;
      if lHomeDir <> '' then
      begin
        AddRoot(TSearchRootKind.srHome, lHomeDir);
        AddRoot(TSearchRootKind.srHome, AppendNamespace(TPath.Combine(lHomeDir, '.config')));
      end;
    end;

{$IFDEF MSWINDOWS}
    if TDotEnvOption.SearchWindowsProfile in aOptions then
    begin
      lParentPath := GetEnvironmentVariable('APPDATA');
      if lParentPath <> '' then
        AddRoot(TSearchRootKind.srWinProfile, AppendNamespace(lParentPath));
    end;
{$ENDIF}

    Result := lRoots.ToArray;
  finally
    lRoots.Free;
    lSeen.Free;
  end;
end;

function TDotEnv.CollectLayeredFiles(const aBaseDir: string; const aOptions: TDotEnvOptions): TArray<TLayeredFile>;
var
  lRoots: TArray<TSearchRoot>;
  lRootIdx: Integer;
  lLayerIdx: Integer;
  lFiles: TList<TLayeredFile>;
  lInfo: TLayeredFile;
begin
  lRoots := ExpandRoots(aBaseDir, aOptions);
  lFiles := TList<TLayeredFile>.Create;
  try
    for lRootIdx := High(lRoots) downto Low(lRoots) do
    begin
      for lLayerIdx := Low(LAYER_FILES) to High(LAYER_FILES) do
      begin
        lInfo.Path := TPath.Combine(lRoots[lRootIdx].Path, LAYER_FILES[lLayerIdx]);
        lInfo.RootIndex := lRootIdx;
        lFiles.Add(lInfo);
      end;
    end;
    Result := lFiles.ToArray;
  finally
    lFiles.Free;
  end;
end;

function TDotEnv.HandleIncludeLine(const aCurrentLine, aFilePath: string; aLineNumber: Integer;
  aRootIndex, aFileOrder: Integer; aEntries: TList<TRawAssignment>): Boolean;
var
  lTrimmed: string;
  lOptional: Boolean;
  lQuoteStart, lQuoteEnd: Integer;
  lIncludePath: string;
  lResolved: string;
begin
  lTrimmed := TrimLeft(aCurrentLine);
  lOptional := False;
  if StartsText('#include_if_exists', lTrimmed) then
    lOptional := True
  else if not StartsText('#include', lTrimmed) then
    Exit(False);

  lQuoteStart := Pos('"', lTrimmed);
  lQuoteEnd := LastDelimiter('"', lTrimmed);
  if (lQuoteStart = 0) or (lQuoteEnd <= lQuoteStart) then
  begin
    AddError(aFilePath, aLineNumber, 1, TDotEnvErrorKind.DekParse,
      'Include directive requires quoted path', True);
    Exit(True);
  end;
  lIncludePath := Copy(lTrimmed, lQuoteStart + 1, lQuoteEnd - lQuoteStart - 1);
  lResolved := ResolveIncludePath(aFilePath, lIncludePath);
  if lResolved = '' then
  begin
    AddError(aFilePath, aLineNumber, 1, TDotEnvErrorKind.DekIO,
      Format('Unable to resolve include target %s', [lIncludePath]), True);
    Exit(True);
  end;

  AddTrace('considered: ' + lResolved);
  if not TFile.Exists(lResolved) then
  begin
    AddTrace('missing: ' + lResolved);
    if not lOptional then
      AddError(aFilePath, aLineNumber, 1, TDotEnvErrorKind.DekIO,
        Format('Included file not found: %s', [lResolved]), True);
    Exit(True);
  end;

  if ParseFile(lResolved, aRootIndex, aFileOrder, aEntries) then
    AddTrace('loaded: ' + lResolved)
  else
    AddTrace('ignored: ' + lResolved);
  Result := True;
end;

function TDotEnv.ResolveIncludePath(const aBaseFile, aTarget: string): string;
var
  lBaseDir: string;
  lCombined: string;
begin
  Result := '';
  if aTarget = '' then
    Exit;
  if not TPath.IsPathRooted(aTarget) then
  begin
    if aBaseFile <> '' then
      lBaseDir := TPath.GetDirectoryName(aBaseFile)
    else
      lBaseDir := TDirectory.GetCurrentDirectory;
    lCombined := TPath.Combine(lBaseDir, aTarget);
  end
  else
    lCombined := aTarget;
  try
    Result := TPath.GetFullPath(lCombined);
  except
    Result := '';
  end;
end;

function TDotEnv.IsPathOnStack(const aPath: string): Boolean;
var
  lItem: string;
begin
  Result := False;
  if FIncludeStack = nil then
    Exit(False);
  for lItem in FIncludeStack do
    if SameFileName(lItem, aPath) then
      Exit(True);
  Exit(False);
end;

function TDotEnv.CheckSecretPermissions(const aFilePath: string): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := True;
end;
{$ELSE}
var
  lStatBuf: stat;
  lFileNameOnly: string;
begin
  Result := True;
  lFileNameOnly := TPath.GetFileName(aFilePath);
  if not SameText(lFileNameOnly, '.env.secret') then
    Exit(True);

  if stat(PAnsiChar(AnsiString(aFilePath)), lStatBuf) <> 0 then
  begin
    AddError(aFilePath, 0, 0, TDotEnvErrorKind.DekIO,
      SysErrorMessage(errno), True);
    Exit(False);
  end;
  if (lStatBuf.st_mode and (S_IRWXG or S_IRWXO)) <> 0 then
  begin
    AddError(aFilePath, 0, 0, TDotEnvErrorKind.DekSecurity,
      'Secret files must be 0600 (owner read/write only)', True);
    Exit(False);
  end;
end;
{$ENDIF}

procedure TDotEnv.LoadLayered(const aBaseDir: string; const aOptions: TDotEnvOptions);
var
  lEntries: TList<TRawAssignment>;
  lFileInfos: TArray<TLayeredFile>;
  lInfo: TLayeredFile;
  lEntry: TRawAssignment;
  lValue: string;
  lFileOrder: Integer;
  j: Integer;
  lErrStart: Integer;
  lHasFatal: Boolean;
begin
  FCurrentOptions := aOptions;
  ResetState;
  EnsureValuesDictionary;
  lEntries := TList<TRawAssignment>.Create;
  try
    lFileInfos := CollectLayeredFiles(aBaseDir, aOptions);
    lFileOrder := 0;
    for lInfo in lFileInfos do
    begin
      CollectFile(lInfo.Path, lInfo.RootIndex, lFileOrder, lEntries);
      Inc(lFileOrder);
    end;
    if TDotEnvOption.StreamingEvaluation in FCurrentOptions then
    begin
      for lFileOrder := 0 to lEntries.Count - 1 do
      begin
        lEntry := lEntries[lFileOrder];
        lErrStart := FErrors.Count;
        lValue := ExpandValue(lEntry.RawValue, lEntry.FilePath, lEntry.Line);
        lHasFatal := False;
        for j := lErrStart to FErrors.Count - 1 do
          if FErrors[j].Fatal then
          begin
            lHasFatal := True;
            Break;
          end;
        if not lHasFatal then
          StoreValue(lEntry, lValue)
        else
          AddTrace(Format('skip store due to fatal errors: %s (%s:%d)',
            [lEntry.Key, lEntry.FilePath, lEntry.Line]));
      end;
    end
    else
      EvaluateAfterMergeEntries(lEntries);
  finally
    lEntries.Free;
  end;
end;

class function TDotEnv.DefaultSearchRoots(const aBaseDir: string; aOptions: TDotEnvOptions): TArray<TSearchRoot>;
var
  lInst: TDotEnv;
begin
  lInst := TDotEnv.Create;
  try
    Result := lInst.ExpandRoots(aBaseDir, aOptions);
  finally
    lInst.Free;
  end;
end;

end.
