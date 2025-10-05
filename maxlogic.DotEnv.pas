unit maxlogic.DotEnv;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

const
  MAX_DOTENV_FILE_SIZE = 1024 * 1024; // 1 MB cap

  DEFAULT_PARENT_DEPTH = 4;

 type
  TDotEnvOption = (
    DoNotOverrideExisting,
    CaseSensitiveKeys,
    StrictUndefined,
    AllowCommandSubst,
    SearchParents,
    SearchUserHome,
    SearchXDG,
    SearchWindowsProfile
  );

  TDotEnvOptions = set of TDotEnvOption;

  TSearchRootKind = (srExplicit, srCWD, srParents, srHome, srXDG, srWinProfile, srCustom);

  TSearchRoot = record
    Kind: TSearchRootKind;
    Path: string;
    MaxDepth: Integer;
  end;

  TOnResolveSymbol = reference to function(const AName, AFilePath: string): string;

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

  TDotEnv = class sealed
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
        FCaseSensitive: Boolean;
      public
        constructor Create(ACaseSensitive: Boolean);
        function Equals(const Left, Right: string): Boolean;
        function GetHashCode(const Value: string): Integer;
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
        function MatchToken(const Token: string): Boolean;
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
        function EvaluateFunction(const Name: string): TExprValue;
        function ResolveIdentifier(const Name: string): TExprValue;
      public
        constructor Create(AOwner: TDotEnv; const Text, SourceFile: string; Line: Integer);
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

    procedure ResetState;
    function DetermineCaseSensitive: Boolean;
    procedure EnsureValuesDictionary;
    function NormalizeKey(const Key: string): string;

    procedure AddError(const FilePath: string; Line, Column: Integer; Kind: TDotEnvErrorKind;
      const Msg: string; Fatal: Boolean);
    procedure AddTrace(const Msg: string);

    procedure EnsureInitialEnv;
    procedure CaptureInitialEnv;
    function ProcessEnvHasKey(const Key: string): Boolean;

    function ParseFile(const FilePath: string; RootIndex, FileOrder: Integer;
      Entries: TList<TRawAssignment>): Boolean;
    procedure ParseContent(const FilePath: string; RootIndex, FileOrder: Integer;
      const Content: string; Entries: TList<TRawAssignment>);
    function ParseKeyValue(const FilePath: string; const Line: string; LineNumber: Integer;
      RootIndex, FileOrder: Integer; out Assignment: TRawAssignment): Boolean;
    function ParseValue(const FilePath: string; var LineIndex: Integer;
      const Lines: TArray<string>; const InitialRemainder: string; LineNumber: Integer): string;
    function ParseDoubleQuoted(const FilePath: string; const Fragment: string;
      LineNumber: Integer): string;
    function ParseSingleQuoted(const FilePath: string; const Fragment: string;
      LineNumber: Integer): string;
    function ParseTripleQuoted(const FilePath: string; var LineIndex: Integer;
      const Lines: TArray<string>; const Fragment: string; const QuoteToken: string;
      LineNumber: Integer): string;
    function ParseHeredoc(const FilePath: string; var LineIndex: Integer;
      const Lines: TArray<string>; const Marker: string; LineNumber: Integer): string;
    function ParseUnquoted(const FilePath: string; var LineIndex: Integer;
      const Lines: TArray<string>; const Fragment: string; LineNumber: Integer): string;

    function ExpandValue(const RawValue, SourceFile: string; Line: Integer): string;
    function ExpandVariable(const Source, SourceFile: string; Line: Integer; var Index: Integer): string;
    function ExpandExpression(const Source, SourceFile: string; Line: Integer; var Index: Integer): string;
    function ExpandCommand(const Source, SourceFile: string; Line: Integer; var Index: Integer): string;
    function ResolveVariable(const Name, SourceFile: string; Line: Integer;
      const DefaultValue: string; HasDefault: Boolean; out Failed: Boolean): string;
    function ResolveSymbol(const Name, SourceFile: string; out Value: string): Boolean;
    function ResolveBuiltIn(const Name, SourceFile: string; out Value: string): Boolean;

    procedure StoreValue(const Assignment: TRawAssignment; const ExpandedValue: string);
    procedure ApplyToProcessEnv(const Key, Value: string);
    function ExecuteCommand(const Cmd: string; out Output: string; out ErrMsg: string): Boolean;

    procedure CollectFile(const FilePath: string; RootIndex, FileOrder: Integer;
      Entries: TList<TRawAssignment>);
    function CollectLayeredFiles(const ABaseDir: string; const AOptions: TDotEnvOptions): TArray<TLayeredFile>;
    function ExpandRoots(const BaseDir: string; const AOptions: TDotEnvOptions): TArray<TSearchRoot>;
    function HandleIncludeLine(const CurrentLine, FilePath: string; LineNumber: Integer;
      RootIndex, FileOrder: Integer; Entries: TList<TRawAssignment>): Boolean;
    function ResolveIncludePath(const BaseFile, Target: string): string;
    function IsPathOnStack(const Path: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadLayered(const ABaseDir: string; const AOptions: TDotEnvOptions = []);
    procedure LoadFiles(const AFiles: array of string; const AOptions: TDotEnvOptions = []);

    procedure SetSearchRoots(const ARoots: TArray<TSearchRoot>);
    class function DefaultSearchRoots(const ABaseDir: string; AOptions: TDotEnvOptions): TArray<TSearchRoot>;

    function TryGetValue(const Key: string; out Value: string): Boolean;
    function GetValue(const Key: string; const DefaultValue: string = ''): string;
    function AsDictionary: TDictionary<string, string>;
    function Expand(const S: string; const SourceFilePath: string = ''): string;

    function GetErrors: TArray<TDotEnvError>;
    function GetTrace: TArray<string>;

    property OnResolveSymbol: TOnResolveSymbol read FOnResolveSymbol write FOnResolveSymbol;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ELSE}
  Posix.Stdlib,
  Posix.Stdio,
{$ENDIF}
  System.StrUtils,
  System.Character,
  System.Hash,
  System.IOUtils,
  System.Math;

const
  LAYER_FILES: array[0..2] of string = ('.env', '.env.local', '.env.secret');

function StripTrailingWhitespaceWithEscape(var S: string): Boolean;
var
  I: Integer;
  Keep: Boolean;
  Removed: Boolean;
begin
  Removed := False;
  I := Length(S);
  while I > 0 do
  begin
    if not CharInSet(S[I], [' ', #9]) then
      Break;
    Keep := (I > 1) and (S[I - 1] = '\');
    if Keep then
    begin
      Delete(S, I - 1, 1);
      Dec(I);
    end
    else
    begin
      Delete(S, I, 1);
      Removed := True;
    end;
    Dec(I);
  end;
  Result := Removed;
end;

function StripCommentPreservingEscape(const S: string): string;
var
  I: Integer;
  Escape: Boolean;
  Ch: Char;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    Escape := False;
    for I := 1 to Length(S) do
    begin
      Ch := S[I];
      if Escape then
      begin
        Builder.Append(Ch);
        Escape := False;
        Continue;
      end;
      if Ch = '\' then
      begin
        Escape := True;
        Continue;
      end;
      if (Ch = '#') or (Ch = ';') then
      begin
        if (I = 1) or CharInSet(S[I - 1], [' ', #9]) then
          Break;
      end;
      Builder.Append(Ch);
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function DecodeEscapes(const S: string): string;
var
  I: Integer;
  Ch: Char;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    I := 1;
    while I <= Length(S) do
    begin
      Ch := S[I];
      if (Ch = '\') and (I < Length(S)) then
      begin
        Inc(I);
        Builder.Append(S[I]);
      end
      else
        Builder.Append(Ch);
      Inc(I);
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
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
  Result.Kind := evBoolean;
  Result.BoolValue := B;
  Result.NumValue := Ord(B);
  if B then
    Result.StrValue := 'true'
  else
    Result.StrValue := 'false';
end;

class function TDotEnv.TExprValue.FromNumber(const N: Double): TExprValue;
begin
  Result.Kind := evNumber;
  Result.NumValue := N;
  Result.BoolValue := not SameValue(N, 0);
  Result.StrValue := FloatToStr(N, TFormatSettings.Invariant);
end;

class function TDotEnv.TExprValue.FromString(const S: string): TExprValue;
begin
  Result.Kind := evString;
  Result.StrValue := S;
  Result.BoolValue := not S.IsEmpty;
  Result.NumValue := 0;
end;

function TDotEnv.TExprValue.ToBoolean: Boolean;
var
  LValue: string;
begin
  case Kind of
    evBoolean:
      Exit(BoolValue);
    evNumber:
      Exit(not SameValue(NumValue, 0));
    evString:
      begin
        LValue := StrValue.ToLower;
        if (LValue = 'true') or (LValue = '1') then
          Exit(True)
        else if (LValue = 'false') or (LValue = '0') or (LValue = '') then
          Exit(False)
        else
          raise Exception.CreateFmt('Cannot convert "%s" to boolean', [StrValue]);
      end;
  end;
  Result := False;
end;

function TDotEnv.TExprValue.ToNumber: Double;
var
  Value: Double;
begin
  case Kind of
    evNumber:
      Exit(NumValue);
    evBoolean:
      if BoolValue then
        Exit(1)
      else
        Exit(0);
    evString:
      begin
        if TryStrToFloat(StrValue, Value, TFormatSettings.Invariant) then
          Exit(Value)
        else
          raise Exception.CreateFmt('Cannot convert "%s" to number', [StrValue]);
      end;
  end;
  raise Exception.Create('Numeric value expected');
end;

function TDotEnv.TExprValue.ToStringValue: string;
begin
  case Kind of
    evString:
      Result := StrValue;
    evBoolean:
      if BoolValue then
        Result := 'true'
      else
        Result := 'false';
    evNumber:
      Result := FloatToStr(NumValue, TFormatSettings.Invariant);
  else
    Result := '';
  end;
end;

{ TDotEnv.TExprParser }

constructor TDotEnv.TExprParser.Create(AOwner: TDotEnv; const Text, SourceFile: string; Line: Integer);
begin
  inherited Create;
  FOwner := AOwner;
  FText := Text;
  FSourceFile := SourceFile;
  FLine := Line;
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

function TDotEnv.TExprParser.MatchToken(const Token: string): Boolean;
begin
  if (Token = '') or (FPos + Length(Token) - 1 > Length(FText)) then
    Exit(False);
  if Copy(FText, FPos, Length(Token)) = Token then
  begin
    Inc(FPos, Length(Token));
    Exit(True);
  end;
  Result := False;
end;

function TDotEnv.TExprParser.ParseIdentifier: string;
var
  StartPos: Integer;
begin
  SkipWhitespace;
  StartPos := FPos;
  if (FPos <= Length(FText)) and (FText[FPos].IsLetter or (FText[FPos] = '_')) then
  begin
    Inc(FPos);
    while (FPos <= Length(FText)) and (FText[FPos].IsLetterOrDigit or (FText[FPos] = '_')) do
      Inc(FPos);
  end;
  Result := Copy(FText, StartPos, FPos - StartPos);
  if Result = '' then
    raise Exception.Create('Identifier expected');
end;

function TDotEnv.TExprParser.ParseStringLiteral: TExprValue;
var
  Buffer: TStringBuilder;
  Ch: Char;
begin
  SkipWhitespace;
  if ConsumeChar <> '''' then
    raise Exception.Create('String literal expected');
  Buffer := TStringBuilder.Create;
  try
    while FPos <= Length(FText) do
    begin
      Ch := ConsumeChar;
      if Ch = '''' then
      begin
        if (FPos <= Length(FText)) and (FText[FPos] = '''') then
        begin
          ConsumeChar;
          Buffer.Append('''');
        end
        else
          Break;
      end
      else
        Buffer.Append(Ch);
    end;
    Result := TExprValue.FromString(Buffer.ToString);
  finally
    Buffer.Free;
  end;
end;

function TDotEnv.TExprParser.ParseNumber: TExprValue;
var
  StartPos: Integer;
  Token: string;
  Value: Double;
begin
  SkipWhitespace;
  StartPos := FPos;
  while (FPos <= Length(FText)) and FText[FPos].IsDigit do
    Inc(FPos);
  if (FPos <= Length(FText)) and (FText[FPos] = '.') then
  begin
    Inc(FPos);
    while (FPos <= Length(FText)) and FText[FPos].IsDigit do
      Inc(FPos);
  end;
  Token := Copy(FText, StartPos, FPos - StartPos);
  if Token = '' then
    raise Exception.Create('Number expected');
  if not TryStrToFloat(Token, Value, TFormatSettings.Invariant) then
    raise Exception.CreateFmt('Invalid number %s', [Token]);
  Result := TExprValue.FromNumber(Value);
end;

function TDotEnv.TExprParser.ResolveIdentifier(const Name: string): TExprValue;
var
  Value: string;
begin
  if SameText(Name, 'true') then
    Exit(TExprValue.FromBoolean(True));
  if SameText(Name, 'false') then
    Exit(TExprValue.FromBoolean(False));
  if SameText(Name, 'null') then
    Exit(TExprValue.FromString(''));
  if FOwner.ResolveSymbol(Name, FSourceFile, Value) then
    Exit(TExprValue.FromString(Value));
  if TDotEnvOption.StrictUndefined in FOwner.FCurrentOptions then
    FOwner.AddError(FSourceFile, FLine, 1, TDotEnvErrorKind.DekStrict,
      Format('Undefined identifier %s', [Name]), True);
  Result := TExprValue.FromString('');
end;

function TDotEnv.TExprParser.EvaluateFunction(const Name: string): TExprValue;
var
  Args: TArray<TExprValue>;
  ArgIndex: Integer;
  PathValue: string;
begin
  if not MatchToken('(') then
    raise Exception.Create('Expected (');
  Args := [];
  SkipWhitespace;
  if PeekChar <> ')' then
  begin
    repeat
      Args := Args + [ParseExpression];
      SkipWhitespace;
    until not MatchToken(',');
  end;
  if not MatchToken(')') then
    raise Exception.Create('Expected )');

  if SameText(Name, 'env') then
  begin
    if Length(Args) <> 1 then
      raise Exception.Create('env(name) expects one argument');
    Exit(ResolveIdentifier(Args[0].ToStringValue));
  end;

  if SameText(Name, 'join') then
  begin
    if Length(Args) = 0 then
      raise Exception.Create('join requires arguments');
    PathValue := Args[0].ToStringValue;
    for ArgIndex := 1 to High(Args) do
      PathValue := TPath.Combine(PathValue, Args[ArgIndex].ToStringValue);
    PathValue := TPath.GetFullPath(PathValue);
    Exit(TExprValue.FromString(PathValue));
  end;

  if SameText(Name, 'dirname') then
  begin
    if Length(Args) <> 1 then
      raise Exception.Create('dirname expects one argument');
    Exit(TExprValue.FromString(TPath.GetDirectoryName(Args[0].ToStringValue)));
  end;

  if SameText(Name, 'basename') then
  begin
    if Length(Args) <> 1 then
      raise Exception.Create('basename expects one argument');
    Exit(TExprValue.FromString(TPath.GetFileName(Args[0].ToStringValue)));
  end;

  if SameText(Name, 'abspath') then
  begin
    if Length(Args) <> 1 then
      raise Exception.Create('abspath expects one argument');
    Exit(TExprValue.FromString(TPath.GetFullPath(Args[0].ToStringValue)));
  end;

  if SameText(Name, 'upper') then
  begin
    if Length(Args) <> 1 then
      raise Exception.Create('upper expects one argument');
    Exit(TExprValue.FromString(Args[0].ToStringValue.ToUpper));
  end;

  if SameText(Name, 'lower') then
  begin
    if Length(Args) <> 1 then
      raise Exception.Create('lower expects one argument');
    Exit(TExprValue.FromString(Args[0].ToStringValue.ToLower));
  end;

  if SameText(Name, 'trim') then
  begin
    if Length(Args) <> 1 then
      raise Exception.Create('trim expects one argument');
    Exit(TExprValue.FromString(Args[0].ToStringValue.Trim));
  end;

  raise Exception.CreateFmt('Unknown function %s', [Name]);
end;

function TDotEnv.TExprParser.ParsePrimary: TExprValue;
var
  Ident: string;
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
      Ident := ParseIdentifier;
      SkipWhitespace;
      if PeekChar = '(' then
        Result := EvaluateFunction(Ident)
      else
        Result := ResolveIdentifier(Ident);
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
  Left, Right: TExprValue;
  Op: Char;
begin
  Left := ParseUnary;
  while True do
  begin
    SkipWhitespace;
    Op := PeekChar;
    case Op of
      '*', '/', '%':
        begin
          ConsumeChar;
          Right := ParseUnary;
          case Op of
            '*': Left := TExprValue.FromNumber(Left.ToNumber * Right.ToNumber);
            '/': Left := TExprValue.FromNumber(Left.ToNumber / Right.ToNumber);
            '%': Left := TExprValue.FromNumber(System.Math.FMod(Left.ToNumber, Right.ToNumber));
          end;
        end;
    else
      Exit(Left);
    end;
  end;
end;

function TDotEnv.TExprParser.ParseAdd: TExprValue;
var
  Left, Right: TExprValue;
  Op: Char;
begin
  Left := ParseMul;
  while True do
  begin
    SkipWhitespace;
    Op := PeekChar;
    case Op of
      '+', '-':
        begin
          ConsumeChar;
          Right := ParseMul;
          if Op = '+' then
          begin
            if (Left.Kind = evString) or (Right.Kind = evString) then
              Left := TExprValue.FromString(Left.ToStringValue + Right.ToStringValue)
            else
              Left := TExprValue.FromNumber(Left.ToNumber + Right.ToNumber);
          end
          else
            Left := TExprValue.FromNumber(Left.ToNumber - Right.ToNumber);
        end;
    else
      Exit(Left);
    end;
  end;
end;

function TDotEnv.TExprParser.ParseCompare: TExprValue;
var
  Left, Right: TExprValue;
begin
  Left := ParseAdd;
  while True do
  begin
    SkipWhitespace;
    if MatchToken('==') then
      Left := TExprValue.FromBoolean(Left.ToStringValue = ParseAdd.ToStringValue)
    else if MatchToken('!=') then
      Left := TExprValue.FromBoolean(Left.ToStringValue <> ParseAdd.ToStringValue)
    else if MatchToken('<=') then
      Left := TExprValue.FromBoolean(Left.ToNumber <= ParseAdd.ToNumber)
    else if MatchToken('>=') then
      Left := TExprValue.FromBoolean(Left.ToNumber >= ParseAdd.ToNumber)
    else if MatchToken('<') then
      Left := TExprValue.FromBoolean(Left.ToNumber < ParseAdd.ToNumber)
    else if MatchToken('>') then
      Left := TExprValue.FromBoolean(Left.ToNumber > ParseAdd.ToNumber)
    else
      Exit(Left);
  end;
end;

function TDotEnv.TExprParser.ParseAnd: TExprValue;
var
  Left, Right: TExprValue;
begin
  Left := ParseCompare;
  while True do
  begin
    SkipWhitespace;
    if MatchToken('&&') then
    begin
      Right := ParseCompare;
      Left := TExprValue.FromBoolean(Left.ToBoolean and Right.ToBoolean);
    end
    else
      Exit(Left);
  end;
end;

function TDotEnv.TExprParser.ParseOr: TExprValue;
var
  Left, Right: TExprValue;
begin
  Left := ParseAnd;
  while True do
  begin
    SkipWhitespace;
    if MatchToken('||') then
    begin
      Right := ParseAnd;
      Left := TExprValue.FromBoolean(Left.ToBoolean or Right.ToBoolean);
    end
    else
      Exit(Left);
  end;
end;

function TDotEnv.TExprParser.ParseExpression: TExprValue;
begin
  Result := ParseOr;
end;

function TDotEnv.TExprParser.Evaluate: TExprValue;
begin
  Result := ParseExpression;
end;

{ TDotEnv.TKeyComparer }

constructor TDotEnv.TKeyComparer.Create(ACaseSensitive: Boolean);
begin
  inherited Create;
  FCaseSensitive := ACaseSensitive;
end;

function TDotEnv.TKeyComparer.Equals(const Left, Right: string): Boolean;
begin
  if FCaseSensitive then
    Result := Left = Right
  else
    Result := SameText(Left, Right);
end;

function TDotEnv.TKeyComparer.GetHashCode(const Value: string): Integer;
var
  Work: string;
begin
  if FCaseSensitive then
    Work := Value
  else
    Work := AnsiUpperCase(Value);
  Result := THashBobJenkins.GetHashValue(Work, 0);
end;

{ TDotEnv }

constructor TDotEnv.Create;
begin
  inherited Create;
  FErrors := TList<TDotEnvError>.Create;
  FTrace := TList<string>.Create;
  FSequence := 0;
  FCaseSensitiveCache := True;
  FIncludeStack := TStack<string>.Create;
end;

destructor TDotEnv.Destroy;
begin
  FValues.Free;
  FErrors.Free;
  FTrace.Free;
  FInitialEnv.Free;
  FIncludeStack.Free;
  inherited;
end;

procedure TDotEnv.ResetState;
begin
  FErrors.Clear;
  FTrace.Clear;
  if FIncludeStack <> nil then
    FIncludeStack.Clear;
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
  Desired: Boolean;
  NewDict: TDictionary<string, TStoredValue>;
  Pair: TPair<string, TStoredValue>;
begin
  Desired := DetermineCaseSensitive;
  if FValues = nil then
  begin
    FValues := TDictionary<string, TStoredValue>.Create(TKeyComparer.Create(Desired));
    FCaseSensitiveCache := Desired;
    Exit;
  end;
  if Desired <> FCaseSensitiveCache then
  begin
    NewDict := TDictionary<string, TStoredValue>.Create(TKeyComparer.Create(Desired));
    for Pair in FValues do
      NewDict.AddOrSetValue(NormalizeKey(Pair.Value.Key), Pair.Value);
    FValues.Free;
    FValues := NewDict;
    FCaseSensitiveCache := Desired;
  end;
end;

function TDotEnv.NormalizeKey(const Key: string): string;
begin
  if DetermineCaseSensitive then
    Result := Key
  else
    Result := AnsiUpperCase(Key);
end;

procedure TDotEnv.AddError(const FilePath: string; Line, Column: Integer; Kind: TDotEnvErrorKind;
  const Msg: string; Fatal: Boolean);
var
  Err: TDotEnvError;
begin
  Err.FilePath := FilePath;
  Err.Line := Line;
  Err.Column := Column;
  Err.Kind := Kind;
  Err.Message := Msg;
  Err.Fatal := Fatal;
  FErrors.Add(Err);
end;

procedure TDotEnv.AddTrace(const Msg: string);
begin
  FTrace.Add(Msg);
end;

procedure TDotEnv.EnsureInitialEnv;
begin
  if FInitialEnv = nil then
    CaptureInitialEnv;
end;

procedure TDotEnv.CaptureInitialEnv;
var
  Comparer: IEqualityComparer<string>;
{$IFDEF MSWINDOWS}
  Block: PWideChar;
  Ptr: PWideChar;
  Entry: string;
  EqPos: Integer;
{$ELSE}
  Env: PPAnsiChar;
  Entry: string;
  EqPos: Integer;
{$ENDIF}
begin
  Comparer := TKeyComparer.Create(False);
  FInitialEnv := TDictionary<string, Byte>.Create(Comparer);
{$IFDEF MSWINDOWS}
  Block := GetEnvironmentStringsW;
  if Block = nil then
    Exit;
  try
    Ptr := Block;
    while Ptr^ <> #0 do
    begin
      Entry := Ptr;
      EqPos := Pos('=', Entry);
      if EqPos > 1 then
        FInitialEnv.AddOrSetValue(AnsiUpperCase(Copy(Entry, 1, EqPos - 1)), 1);
      Inc(Ptr, Length(Ptr) + 1);
    end;
  finally
    FreeEnvironmentStringsW(Block);
  end;
{$ELSE}
  Env := environ;
  while (Env <> nil) and (Env^ <> nil) do
  begin
    Entry := string(Env^);
    EqPos := Pos('=', Entry);
    if EqPos > 1 then
      FInitialEnv.AddOrSetValue(Copy(Entry, 1, EqPos - 1), 1);
    Inc(Env);
  end;
{$ENDIF}
end;

function TDotEnv.ProcessEnvHasKey(const Key: string): Boolean;
var
  Lookup: string;
begin
  EnsureInitialEnv;
  if FInitialEnv = nil then
    Exit(False);
  Lookup := NormalizeKey(Key);
  Result := FInitialEnv.ContainsKey(Lookup);
end;

function TDotEnv.TryGetValue(const Key: string; out Value: string): Boolean;
var
  Stored: TStoredValue;
begin
  EnsureValuesDictionary;
  Result := FValues.TryGetValue(NormalizeKey(Key), Stored);
  if Result then
    Value := Stored.Value
  else
    Value := '';
end;

function TDotEnv.GetValue(const Key: string; const DefaultValue: string): string;
begin
  if not TryGetValue(Key, Result) then
    Result := DefaultValue;
end;

function TDotEnv.AsDictionary: TDictionary<string, string>;
var
  Dict: TDictionary<string, string>;
  Pair: TPair<string, TStoredValue>;
begin
  EnsureValuesDictionary;
  Dict := TDictionary<string, string>.Create(TKeyComparer.Create(DetermineCaseSensitive));
  for Pair in FValues do
    Dict.AddOrSetValue(Pair.Value.Key, Pair.Value.Value);
  Result := Dict;
end;

function TDotEnv.GetErrors: TArray<TDotEnvError>;
begin
  Result := FErrors.ToArray;
end;

function TDotEnv.GetTrace: TArray<string>;
begin
  Result := FTrace.ToArray;
end;

function TDotEnv.ResolveBuiltIn(const Name, SourceFile: string; out Value: string): Boolean;
var
  Dir: string;
begin
  if SameText(Name, 'CUR_DIR') then
  begin
    if SourceFile <> '' then
      Dir := TPath.GetDirectoryName(SourceFile)
    else
      Dir := TDirectory.GetCurrentDirectory;
    Value := Dir;
    Exit(True);
  end;
  if SameText(Name, 'CUR_FILE') then
  begin
    Value := SourceFile;
    Exit(True);
  end;
  if SameText(Name, 'DIR_SEP') then
  begin
    Value := string(TPath.DirectorySeparatorChar);
    Exit(True);
  end;
  if SameText(Name, 'APP_EXE') then
  begin
    Value := TPath.GetFullPath(ParamStr(0));
    Exit(True);
  end;
  if SameText(Name, 'APP_DIR') then
  begin
    Value := ExcludeTrailingPathDelimiter(TPath.GetDirectoryName(TPath.GetFullPath(ParamStr(0))));
    Exit(True);
  end;
  Result := False;
end;

function TDotEnv.ResolveSymbol(const Name, SourceFile: string; out Value: string): Boolean;
var
  Stored: TStoredValue;
  Normal: string;
begin
  EnsureValuesDictionary;
  Normal := NormalizeKey(Name);
  if FValues.TryGetValue(Normal, Stored) then
  begin
    Value := Stored.Value;
    Exit(True);
  end;
  Value := GetEnvironmentVariable(Name);
  if Value <> '' then
    Exit(True);
  if ResolveBuiltIn(Name, SourceFile, Value) then
    Exit(True);
  if Assigned(FOnResolveSymbol) then
  begin
    Value := FOnResolveSymbol(Name, SourceFile);
    if Value <> '' then
      Exit(True);
  end;
  Result := False;
end;

function TDotEnv.ResolveVariable(const Name, SourceFile: string; Line: Integer;
  const DefaultValue: string; HasDefault: Boolean; out Failed: Boolean): string;
begin
  Failed := False;
  if ResolveSymbol(Name, SourceFile, Result) then
    Exit;
  if HasDefault then
  begin
    Result := DefaultValue;
    Exit;
  end;
  if TDotEnvOption.StrictUndefined in FCurrentOptions then
  begin
    Failed := True;
    AddError(SourceFile, Line, 1, TDotEnvErrorKind.DekStrict,
      Format('Undefined variable %s', [Name]), True);
  end;
  Result := '';
end;

function IsNameStartChar(const C: Char): Boolean;
begin
  Result := C.IsLetter or (C = '_');
end;

function IsNameChar(const C: Char): Boolean;
begin
  Result := C.IsLetterOrDigit or (C = '_');
end;

function TDotEnv.ExpandVariable(const Source, SourceFile: string; Line: Integer;
  var Index: Integer): string;
var
  StartPos, EndPos: Integer;
  NamePart: string;
  DefaultValue: string;
  HasDefault: Boolean;
  DefaultPos: Integer;
  Failed: Boolean;
begin
  Result := '';
  HasDefault := False;
  if Index > Length(Source) then
    Exit;
  if Source[Index] = '{' then
  begin
    StartPos := Index + 1;
    EndPos := StartPos;
    while (EndPos <= Length(Source)) and (Source[EndPos] <> '}') do
      Inc(EndPos);
    if EndPos > Length(Source) then
    begin
      AddError(SourceFile, Line, 1, TDotEnvErrorKind.DekParse,
        'Missing closing brace in variable reference', True);
      Index := Length(Source) + 1;
      Exit('');
    end;
    NamePart := Copy(Source, StartPos, EndPos - StartPos);
    Index := EndPos + 1;
  end
  else
  begin
    StartPos := Index;
    if not ((StartPos <= Length(Source)) and IsNameStartChar(Source[StartPos])) then
    begin
      Result := '$' + Source[Index];
      Inc(Index);
      Exit;
    end;
    EndPos := StartPos;
    Inc(EndPos);
    while (EndPos <= Length(Source)) and IsNameChar(Source[EndPos]) do
      Inc(EndPos);
    NamePart := Copy(Source, StartPos, EndPos - StartPos);
    Index := EndPos;
  end;
  DefaultPos := Pos(':-', NamePart);
  if DefaultPos > 0 then
  begin
    DefaultValue := Copy(NamePart, DefaultPos + 2, MaxInt);
    NamePart := Copy(NamePart, 1, DefaultPos - 1);
    HasDefault := True;
  end;
  NamePart := Trim(NamePart);
  Result := ResolveVariable(NamePart, SourceFile, Line, DefaultValue, HasDefault, Failed);
end;

function TDotEnv.ExpandExpression(const Source, SourceFile: string; Line: Integer;
  var Index: Integer): string;
var
  StartPos: Integer;
  Depth: Integer;
  ExprText: string;
  Parser: TExprParser;
  Value: TExprValue;
begin
  Result := '';
  if (Index > Length(Source)) or (Source[Index] <> '[') then
    Exit;
  Inc(Index);
  StartPos := Index;
  Depth := 1;
  while (Index <= Length(Source)) and (Depth > 0) do
  begin
    if Source[Index] = '[' then
      Inc(Depth)
    else if Source[Index] = ']' then
    begin
      Dec(Depth);
      if Depth = 0 then
        Break;
    end;
    Inc(Index);
  end;
  if Depth <> 0 then
  begin
    AddError(SourceFile, Line, 1, TDotEnvErrorKind.DekParse, 'Unterminated expression', True);
    Index := Length(Source) + 1;
    Exit;
  end;
  ExprText := Copy(Source, StartPos, Index - StartPos);
  Inc(Index); // skip closing ]
  Parser := TExprParser.Create(Self, ExprText, SourceFile, Line);
  try
    try
      Value := Parser.Evaluate;
      Result := Value.ToStringValue;
    except
      on E: Exception do
      begin
        AddError(SourceFile, Line, 1, TDotEnvErrorKind.DekParse,
          Format('Expression error: %s', [E.Message]), True);
        Result := '';
      end;
    end;
  finally
    Parser.Free;
  end;
end;

function TDotEnv.ExpandCommand(const Source, SourceFile: string; Line: Integer;
  var Index: Integer): string;
var
  StartPos: Integer;
  Depth: Integer;
  CmdText: string;
  Output: string;
  ErrMsg: string;
begin
  Result := '';
  if (Index > Length(Source)) or (Source[Index] <> '(') then
    Exit;
  if not (TDotEnvOption.AllowCommandSubst in FCurrentOptions) then
  begin
    // consume command but return literal
    StartPos := Index;
    Depth := 1;
    Inc(Index);
    while (Index <= Length(Source)) and (Depth > 0) do
    begin
      if Source[Index] = '(' then
        Inc(Depth)
      else if Source[Index] = ')' then
      begin
        Dec(Depth);
        if Depth = 0 then
          Break;
      end;
      Inc(Index);
    end;
    if Depth <> 0 then
    begin
      AddError(SourceFile, Line, 1, TDotEnvErrorKind.DekParse,
        'Unterminated command substitution', True);
      Index := Length(Source) + 1;
      Exit;
    end;
    CmdText := Copy(Source, StartPos, Index - StartPos + 1);
    Inc(Index);
    AddError(SourceFile, Line, 1, TDotEnvErrorKind.DekCommand,
      'Command substitution disabled by configuration', False);
    Result := '$' + CmdText;
    Exit;
  end;

  Inc(Index);
  StartPos := Index;
  Depth := 1;
  while (Index <= Length(Source)) and (Depth > 0) do
  begin
    if Source[Index] = '(' then
      Inc(Depth)
    else if Source[Index] = ')' then
    begin
      Dec(Depth);
      if Depth = 0 then
        Break;
    end;
    Inc(Index);
  end;
  if Depth <> 0 then
  begin
    AddError(SourceFile, Line, 1, TDotEnvErrorKind.DekParse,
      'Unterminated command substitution', True);
    Index := Length(Source) + 1;
    Exit;
  end;
  CmdText := Copy(Source, StartPos, Index - StartPos);
  Inc(Index);
  if ExecuteCommand(CmdText, Output, ErrMsg) then
    Result := Output.Trim
  else
  begin
    AddError(SourceFile, Line, 1, TDotEnvErrorKind.DekCommand, ErrMsg, False);
    Result := '';
  end;
end;

function TDotEnv.ExpandValue(const RawValue, SourceFile: string; Line: Integer): string;
var
  SB: TStringBuilder;
  I: Integer;
  Ch: Char;
begin
  SB := TStringBuilder.Create;
  try
    I := 1;
    while I <= Length(RawValue) do
    begin
      Ch := RawValue[I];
      if Ch = '$' then
      begin
        Inc(I);
        if I > Length(RawValue) then
        begin
          SB.Append('$');
          Break;
        end;
        case RawValue[I] of
          '{', '_', 'A' .. 'Z', 'a' .. 'z':
            SB.Append(ExpandVariable(RawValue, SourceFile, Line, I));
          '[':
            SB.Append(ExpandExpression(RawValue, SourceFile, Line, I));
          '(':
            SB.Append(ExpandCommand(RawValue, SourceFile, Line, I));
          '$':
            begin
              SB.Append('$');
              Inc(I);
            end;
        else
          SB.Append('$');
          SB.Append(RawValue[I]);
          Inc(I);
        end;
      end
      else
      begin
        SB.Append(Ch);
        Inc(I);
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TDotEnv.Expand(const S: string; const SourceFilePath: string): string;
begin
  Result := ExpandValue(S, SourceFilePath, 0);
end;

procedure TDotEnv.StoreValue(const Assignment: TRawAssignment; const ExpandedValue: string);
var
  Normal: string;
  Stored: TStoredValue;
  SkipOverride: Boolean;
begin
  EnsureValuesDictionary;
  Normal := NormalizeKey(Assignment.Key);
  SkipOverride := False;
  if (TDotEnvOption.DoNotOverrideExisting in FCurrentOptions) and
     (not FValues.ContainsKey(Normal)) and ProcessEnvHasKey(Assignment.Key) then
    SkipOverride := True;
  if SkipOverride then
  begin
    AddTrace(Format('skip existing env: %s', [Assignment.Key]));
    Exit;
  end;
  Stored.Key := Assignment.Key;
  Stored.Value := ExpandedValue;
  Stored.SourceFile := Assignment.FilePath;
  Stored.Line := Assignment.Line;
  Stored.RootIndex := Assignment.RootIndex;
  Stored.Sequence := FSequence;
  Inc(FSequence);
  FValues.AddOrSetValue(Normal, Stored);
  ApplyToProcessEnv(Assignment.Key, ExpandedValue);
end;

procedure TDotEnv.ApplyToProcessEnv(const Key, Value: string);
begin
{$IFDEF MSWINDOWS}
  SetEnvironmentVariable(PChar(Key), PChar(Value));
{$ELSE}
  setenv(PAnsiChar(AnsiString(Key)), PAnsiChar(AnsiString(Value)), 1);
{$ENDIF}
end;

function TDotEnv.ExecuteCommand(const Cmd: string; out Output: string; out ErrMsg: string): Boolean;
{$IFDEF MSWINDOWS}
var
  ReadPipe, WritePipe: THandle;
  Security: TSecurityAttributes;
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  Buffer: array[0..2047] of AnsiChar;
  BytesRead: Cardinal;
  Chunk: AnsiString;
  CmdLine: string;
  ExitCode: Cardinal;
{$ELSE}
var
  Pipe: Pointer;
  Buffer: array[0..1023] of AnsiChar;
  LinePtr: PAnsiChar;
  Status: Integer;
  Accum: AnsiString;
{$ENDIF}
begin
  Output := '';
  ErrMsg := '';
{$IFDEF MSWINDOWS}
  ZeroMemory(@Security, SizeOf(Security));
  Security.nLength := SizeOf(Security);
  Security.bInheritHandle := True;
  if not CreatePipe(ReadPipe, WritePipe, @Security, 0) then
  begin
    ErrMsg := SysErrorMessage(GetLastError);
    Exit(False);
  end;
  try
    if not SetHandleInformation(ReadPipe, HANDLE_FLAG_INHERIT, 0) then
    begin
      ErrMsg := SysErrorMessage(GetLastError);
      Exit(False);
    end;
    FillChar(StartInfo, SizeOf(StartInfo), 0);
    StartInfo.cb := SizeOf(StartInfo);
    StartInfo.dwFlags := STARTF_USESTDHANDLES;
    StartInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
    StartInfo.hStdOutput := WritePipe;
    StartInfo.hStdError := WritePipe;
    FillChar(ProcInfo, SizeOf(ProcInfo), 0);
    CmdLine := 'cmd.exe /C ' + Cmd;
    if not CreateProcess(nil, PChar(CmdLine), nil, nil, True,
      CREATE_NO_WINDOW, nil, nil, StartInfo, ProcInfo) then
    begin
      ErrMsg := SysErrorMessage(GetLastError);
      Exit(False);
    end;
    CloseHandle(WritePipe);
    WritePipe := 0;
    try
      repeat
        if not ReadFile(ReadPipe, Buffer, SizeOf(Buffer), BytesRead, nil) then
          Break;
        if BytesRead = 0 then
          Break;
        SetString(Chunk, PAnsiChar(@Buffer[0]), BytesRead);
        Output := Output + string(Chunk);
      until False;
      WaitForSingleObject(ProcInfo.hProcess, INFINITE);
      if not GetExitCodeProcess(ProcInfo.hProcess, ExitCode) then
        ExitCode := 1;
      if ExitCode <> 0 then
      begin
        ErrMsg := Format('Command exited with code %d', [ExitCode]);
        Result := False;
      end
      else
        Result := True;
    finally
      CloseHandle(ProcInfo.hThread);
      CloseHandle(ProcInfo.hProcess);
    end;
  finally
    if WritePipe <> 0 then
      CloseHandle(WritePipe);
    CloseHandle(ReadPipe);
  end;
{$ELSE}
  Pipe := popen(PAnsiChar(AnsiString(Cmd)), PAnsiChar(AnsiString('r')));
  if Pipe = nil then
  begin
    ErrMsg := 'Failed to execute command';
    Exit(False);
  end;
  try
    while True do
    begin
      LinePtr := fgets(@Buffer[0], SizeOf(Buffer), Pipe);
      if LinePtr = nil then
        Break;
      Accum := Accum + AnsiString(LinePtr);
    end;
  finally
    Status := pclose(Pipe);
  end;
  Output := string(Accum);
  if Status <> 0 then
  begin
    ErrMsg := Format('Command exited with status %d', [Status]);
    Result := False;
  end
  else
    Result := True;
{$ENDIF}
  Output := Output.Trim;
end;

procedure TDotEnv.CollectFile(const FilePath: string; RootIndex, FileOrder: Integer;
  Entries: TList<TRawAssignment>);
var
  Loaded: Boolean;
begin
  AddTrace('considered: ' + FilePath);
  if TFile.Exists(FilePath) then
  begin
    Loaded := ParseFile(FilePath, RootIndex, FileOrder, Entries);
    if Loaded then
      AddTrace('loaded: ' + FilePath)
    else
      AddTrace('ignored: ' + FilePath);
  end
  else
    AddTrace('missing: ' + FilePath);
end;

function TDotEnv.ParseFile(const FilePath: string; RootIndex, FileOrder: Integer;
  Entries: TList<TRawAssignment>): Boolean;
var
  Content: string;
  Size: Int64;
  NormalPath: string;
begin
  Result := False;
  NormalPath := TPath.GetFullPath(FilePath);
  if NormalPath = '' then
    Exit;
  if IsPathOnStack(NormalPath) then
  begin
    AddError(NormalPath, 0, 0, TDotEnvErrorKind.DekCycle,
      Format('Include cycle detected involving %s', [NormalPath]), True);
    Exit;
  end;
  FIncludeStack.Push(NormalPath);
  try
    try
      if not TFile.Exists(NormalPath) then
        Exit;
      Size := TFile.GetSize(NormalPath);
      if Size > MAX_DOTENV_FILE_SIZE then
      begin
        AddError(NormalPath, 0, 0, TDotEnvErrorKind.DekSecurity,
          Format('File %s exceeds maximum size', [NormalPath]), True);
        Exit;
      end;
      Content := TFile.ReadAllText(NormalPath, TEncoding.UTF8);
    except
      on E: Exception do
      begin
        AddError(NormalPath, 0, 0, TDotEnvErrorKind.DekIO, E.Message, True);
        Exit;
      end;
    end;
    Content := StringReplace(Content, #13#10, #10, [rfReplaceAll]);
    Content := StringReplace(Content, #13, #10, [rfReplaceAll]);
    ParseContent(NormalPath, RootIndex, FileOrder, Content, Entries);
    Result := True;
  finally
    FIncludeStack.Pop;
  end;
end;

function TDotEnv.ParseDoubleQuoted(const FilePath: string; const Fragment: string;
  LineNumber: Integer): string;
var
  Buffer: TStringBuilder;
  I, Len: Integer;
  Ch: Char;
begin
  Buffer := TStringBuilder.Create;
  try
    Len := Length(Fragment);
    I := 2;
    while I <= Len do
    begin
      Ch := Fragment[I];
      if Ch = '"' then
        Break;
      if (Ch = '\') and (I < Len) then
      begin
        Inc(I);
        case Fragment[I] of
          'n': Buffer.Append(#10);
          'r': Buffer.Append(#13);
          't': Buffer.Append(#9);
          '"': Buffer.Append('"');
          '\': Buffer.Append('\');
          '$': Buffer.Append('$');
        else
          Buffer.Append(Fragment[I]);
        end;
      end
      else
        Buffer.Append(Ch);
      Inc(I);
    end;
    if (I > Len) or (Fragment[I] <> '"') then
      AddError(FilePath, LineNumber, 1, TDotEnvErrorKind.DekParse, 'Unterminated double quote', True);
    Result := Buffer.ToString;
  finally
    Buffer.Free;
  end;
end;

function TDotEnv.ParseSingleQuoted(const FilePath: string; const Fragment: string;
  LineNumber: Integer): string;
var
  Buffer: TStringBuilder;
  I, Len: Integer;
  Ch: Char;
begin
  Buffer := TStringBuilder.Create;
  try
    Len := Length(Fragment);
    I := 2;
    while I <= Len do
    begin
      Ch := Fragment[I];
      if Ch = '''' then
      begin
        if (I < Len) and (Fragment[I + 1] = '''') then
        begin
          Buffer.Append('''');
          Inc(I);
        end
        else
          Break;
      end
      else
        Buffer.Append(Ch);
      Inc(I);
    end;
    if (I > Len) or (Fragment[I] <> '''') then
      AddError(FilePath, LineNumber, 1, TDotEnvErrorKind.DekParse, 'Unterminated single quote', True);
    Result := Buffer.ToString;
  finally
    Buffer.Free;
  end;
end;

function TDotEnv.ParseTripleQuoted(const FilePath: string; var LineIndex: Integer;
  const Lines: TArray<string>; const Fragment: string; const QuoteToken: string;
  LineNumber: Integer): string;
var
  Buffer: TStringBuilder;
  Current: string;
  Delim: string;
  PosClose: Integer;
  Found: Boolean;
begin
  Buffer := TStringBuilder.Create;
  try
    Delim := QuoteToken;
    Current := Copy(Fragment, Length(Delim) + 1, MaxInt);
    Found := False;
    while True do
    begin
      PosClose := Pos(Delim, Current);
      if PosClose > 0 then
      begin
        Buffer.Append(Copy(Current, 1, PosClose - 1));
        Found := True;
        Break;
      end;
      Buffer.Append(Current);
      if LineIndex >= High(Lines) then
        Break;
      Buffer.Append(#10);
      Inc(LineIndex);
      Current := Lines[LineIndex];
    end;
    if not Found then
      AddError(FilePath, LineNumber, 1, TDotEnvErrorKind.DekParse, 'Unterminated triple quote', True);
    Result := Buffer.ToString;
  finally
    Buffer.Free;
  end;
end;

function TDotEnv.ParseHeredoc(const FilePath: string; var LineIndex: Integer;
  const Lines: TArray<string>; const Marker: string; LineNumber: Integer): string;
var
  Buffer: TStringBuilder;
  Current: string;
  Found: Boolean;
begin
  Buffer := TStringBuilder.Create;
  try
    Found := False;
    while LineIndex < High(Lines) do
    begin
      Inc(LineIndex);
      Current := Lines[LineIndex];
      if Current = Marker then
      begin
        Found := True;
        Break;
      end;
      if Buffer.Length > 0 then
        Buffer.Append(#10);
      Buffer.Append(Current);
    end;
    if not Found then
      AddError(FilePath, LineNumber, 1, TDotEnvErrorKind.DekParse, 'Unterminated heredoc', True);
    Result := Buffer.ToString;
  finally
    Buffer.Free;
  end;
end;

function TDotEnv.ParseUnquoted(const FilePath: string; var LineIndex: Integer;
  const Lines: TArray<string>; const Fragment: string; LineNumber: Integer): string;
var
  Buffer: TStringBuilder;
  Current: string;
  Segment: string;
  ContinueLine: Boolean;
begin
  Buffer := TStringBuilder.Create;
  try
    Current := Fragment;
    repeat
      Segment := StripCommentPreservingEscape(Current);
      StripTrailingWhitespaceWithEscape(Segment);
      ContinueLine := False;
      if (Length(Segment) > 0) and (Segment[Length(Segment)] = '\') then
      begin
        Delete(Segment, Length(Segment), 1);
        ContinueLine := True;
      end;
      Segment := DecodeEscapes(Segment);
      Buffer.Append(Segment);
      if ContinueLine then
      begin
        Buffer.Append(#10);
        if LineIndex >= High(Lines) then
        begin
          AddError(FilePath, LineNumber, 1, TDotEnvErrorKind.DekParse,
            'Line continuation reached end of file', True);
          Break;
        end;
        Inc(LineIndex);
        Current := Lines[LineIndex];
      end;
    until not ContinueLine;
    Result := Buffer.ToString;
  finally
    Buffer.Free;
  end;
end;

function TDotEnv.ParseValue(const FilePath: string; var LineIndex: Integer;
  const Lines: TArray<string>; const InitialRemainder: string; LineNumber: Integer): string;
var
  Fragment: string;
  Marker: string;
  TripleDouble: string;
  TripleSingle: string;
begin
  Fragment := InitialRemainder;
  if Fragment = '' then
    Exit('');
  TripleDouble := StringOfChar('"', 3);
  TripleSingle := StringOfChar(#39, 3);
  if Length(Fragment) >= 3 then
  begin
    if Copy(Fragment, 1, 3) = TripleDouble then
      Exit(ParseTripleQuoted(FilePath, LineIndex, Lines, Fragment, TripleDouble, LineNumber));
    if Copy(Fragment, 1, 3) = TripleSingle then
      Exit(ParseTripleQuoted(FilePath, LineIndex, Lines, Fragment, TripleSingle, LineNumber));
  end;
  if (Length(Fragment) >= 2) and (Copy(Fragment, 1, 2) = '<<') then
  begin
    Marker := Trim(Copy(Fragment, 3, MaxInt));
    if Marker = '' then
    begin
      AddError(FilePath, LineNumber, 1, TDotEnvErrorKind.DekParse, 'Heredoc marker expected', True);
      Exit('');
    end;
    Exit(ParseHeredoc(FilePath, LineIndex, Lines, Marker, LineNumber));
  end;
  if Fragment[1] = '"' then
    Exit(ParseDoubleQuoted(FilePath, Fragment, LineNumber));
  if Fragment[1] = '''' then
    Exit(ParseSingleQuoted(FilePath, Fragment, LineNumber));
  Result := ParseUnquoted(FilePath, LineIndex, Lines, Fragment, LineNumber);
end;

function TDotEnv.ParseKeyValue(const FilePath: string; const Line: string; LineNumber: Integer;
  RootIndex, FileOrder: Integer; out Assignment: TRawAssignment): Boolean;
var
  Len: Integer;
  I: Integer;
  KeyStart, KeyEnd: Integer;
  Delim: Char;
  Remainder: string;
  Trimmed: string;
begin
  Result := False;
  Trimmed := Trim(Line);
  if Trimmed = '' then
    Exit(False);
  if Trimmed[1] in ['#', ';'] then
    Exit(False);

  Len := Length(Line);
  I := 1;
  while (I <= Len) and (Line[I] in [' ', #9]) do
    Inc(I);
  if (I > Len) or not IsNameStartChar(Line[I]) then
  begin
    AddError(FilePath, LineNumber, 1, TDotEnvErrorKind.DekParse, 'Invalid key', True);
    Exit(False);
  end;
  KeyStart := I;
  Inc(I);
  while (I <= Len) and IsNameChar(Line[I]) do
    Inc(I);
  KeyEnd := I - 1;
  while (I <= Len) and (Line[I] in [' ', #9]) do
    Inc(I);
  if I > Len then
  begin
    AddError(FilePath, LineNumber, 1, TDotEnvErrorKind.DekParse, 'Expected delimiter', True);
    Exit(False);
  end;
  Delim := Line[I];
  if not (Delim in ['=', ':']) then
  begin
    AddError(FilePath, LineNumber, 1, TDotEnvErrorKind.DekParse, 'Expected = or :', True);
    Exit(False);
  end;
  Inc(I);
  while (I <= Len) and (Line[I] in [' ', #9]) do
    Inc(I);
  Remainder := Copy(Line, I, MaxInt);

  Assignment.Key := Copy(Line, KeyStart, KeyEnd - KeyStart + 1);
  Assignment.RawValue := Remainder;
  Assignment.FilePath := FilePath;
  Assignment.Line := LineNumber;
  Assignment.RootIndex := RootIndex;
  Assignment.FileOrder := FileOrder;
  Result := True;
end;

procedure TDotEnv.ParseContent(const FilePath: string; RootIndex, FileOrder: Integer;
  const Content: string; Entries: TList<TRawAssignment>);
var
  Lines: TArray<string>;
  LineIndex: Integer;
  Line: string;
  Assignment: TRawAssignment;
  Value: string;
begin
  Lines := Content.Split([#10]);
  LineIndex := 0;
  while LineIndex <= High(Lines) do
  begin
    Line := Lines[LineIndex];
    if HandleIncludeLine(Line, FilePath, LineIndex + 1, RootIndex, FileOrder, Entries) then
    begin
      Inc(LineIndex);
      Continue;
    end;
    if ParseKeyValue(FilePath, Line, LineIndex + 1, RootIndex, FileOrder, Assignment) then
    begin
      Value := ParseValue(FilePath, LineIndex, Lines, Assignment.RawValue, Assignment.Line);
      Assignment.RawValue := Value;
      Entries.Add(Assignment);
    end;
    Inc(LineIndex);
  end;
end;

procedure TDotEnv.LoadFiles(const AFiles: array of string; const AOptions: TDotEnvOptions);
var
  Entries: TList<TRawAssignment>;
  I: Integer;
  FilePath: string;
  Entry: TRawAssignment;
  Value: string;
  FileOrder: Integer;
begin
  FCurrentOptions := AOptions;
  ResetState;
  EnsureValuesDictionary;
  Entries := TList<TRawAssignment>.Create;
  try
    FileOrder := 0;
    for I := Low(AFiles) to High(AFiles) do
    begin
      FilePath := TPath.GetFullPath(AFiles[I]);
      CollectFile(FilePath, 0, FileOrder, Entries);
      Inc(FileOrder);
    end;
    for Entry in Entries do
    begin
      Value := ExpandValue(Entry.RawValue, Entry.FilePath, Entry.Line);
      StoreValue(Entry, Value);
    end;
  finally
    Entries.Free;
  end;
end;

procedure TDotEnv.SetSearchRoots(const ARoots: TArray<TSearchRoot>);
begin
  FCustomRoots := Copy(ARoots);
  FHasCustomRoots := Length(FCustomRoots) > 0;
end;

function TDotEnv.ExpandRoots(const BaseDir: string; const AOptions: TDotEnvOptions): TArray<TSearchRoot>;
var
  Roots: TList<TSearchRoot>;
  Seen: TDictionary<string, Byte>;
  BasePath: string;
  Current: string;
  ParentPath: string;
  Depth: Integer;
  HomeDir: string;
{$IFNDEF MSWINDOWS}
  XdgHome: string;
  XdgDirs: string;
  Parts: TArray<string>;
  DirItem: string;
{$ENDIF}

  function NormalizeKey(const Path: string): string;
  var
    FullPath: string;
  begin
    FullPath := TPath.GetFullPath(Path);
    if IsWindowsPlatform then
      Result := AnsiUpperCase(FullPath)
    else
      Result := FullPath;
  end;

  procedure AddRoot(const Kind: TSearchRootKind; const Path: string; DepthValue: Integer = 0);
  var
    RootItem: TSearchRoot;
    KeyPath: string;
  begin
    if Path = '' then
      Exit;
    KeyPath := NormalizeKey(Path);
    if KeyPath = '' then
      Exit;
    if not Seen.ContainsKey(KeyPath) then
    begin
      RootItem.Kind := Kind;
      RootItem.Path := TPath.GetFullPath(Path);
      RootItem.MaxDepth := DepthValue;
      Roots.Add(RootItem);
      Seen.AddOrSetValue(KeyPath, 1);
    end;
  end;

begin
  if FHasCustomRoots then
    Exit(Copy(FCustomRoots));

  Roots := TList<TSearchRoot>.Create;
  Seen := TDictionary<string, Byte>.Create(TKeyComparer.Create(True));
  try
    BasePath := BaseDir;
    if BasePath = '' then
      BasePath := TDirectory.GetCurrentDirectory;
    BasePath := TPath.GetFullPath(BasePath);
    AddRoot(srCWD, BasePath, 0);

    if TDotEnvOption.SearchParents in AOptions then
    begin
      Current := BasePath;
      for Depth := 1 to DEFAULT_PARENT_DEPTH do
      begin
        ParentPath := TPath.GetDirectoryName(Current);
        if (ParentPath = '') or SameFileName(ParentPath, Current) then
          Break;
        AddRoot(srParents, ParentPath, Depth);
        Current := ParentPath;
      end;
    end;

{$IFNDEF MSWINDOWS}
    if TDotEnvOption.SearchXDG in AOptions then
    begin
      HomeDir := TPath.GetHomePath;
      XdgHome := GetEnvironmentVariable('XDG_CONFIG_HOME');
      if (XdgHome = '') and (HomeDir <> '') then
        XdgHome := TPath.Combine(HomeDir, '.config');
      if XdgHome <> '' then
        AddRoot(srXDG, TPath.Combine(XdgHome, 'maxlogic'));
      XdgDirs := GetEnvironmentVariable('XDG_CONFIG_DIRS');
      if XdgDirs = '' then
        XdgDirs := '/etc/xdg';
      Parts := XdgDirs.Split([':']);
      for DirItem in Parts do
        if DirItem.Trim <> '' then
          AddRoot(srXDG, TPath.Combine(DirItem.Trim, 'maxlogic'));
    end;
{$ENDIF}

    if TDotEnvOption.SearchUserHome in AOptions then
    begin
      HomeDir := TPath.GetHomePath;
      if HomeDir <> '' then
      begin
        AddRoot(srHome, HomeDir, 0);
        AddRoot(srHome, TPath.Combine(TPath.Combine(HomeDir, '.config'), 'maxlogic'), 0);
      end;
    end;

{$IFDEF MSWINDOWS}
    if TDotEnvOption.SearchWindowsProfile in AOptions then
    begin
      ParentPath := GetEnvironmentVariable('APPDATA');
      if ParentPath <> '' then
        AddRoot(srWinProfile, TPath.Combine(ParentPath, 'MaxLogic'), 0);
    end;
{$ENDIF}

    Result := Roots.ToArray;
  finally
    Roots.Free;
    Seen.Free;
  end;
end;

function TDotEnv.CollectLayeredFiles(const ABaseDir: string; const AOptions: TDotEnvOptions): TArray<TLayeredFile>;
var
  Roots: TArray<TSearchRoot>;
  RootIdx: Integer;
  LayerIdx: Integer;
  Files: TList<TLayeredFile>;
  Info: TLayeredFile;
begin
  Roots := ExpandRoots(ABaseDir, AOptions);
  Files := TList<TLayeredFile>.Create;
  try
    for RootIdx := High(Roots) downto Low(Roots) do
    begin
      for LayerIdx := Low(LAYER_FILES) to High(LAYER_FILES) do
      begin
        Info.Path := TPath.Combine(Roots[RootIdx].Path, LAYER_FILES[LayerIdx]);
        Info.RootIndex := RootIdx;
        Files.Add(Info);
      end;
    end;
    Result := Files.ToArray;
  finally
    Files.Free;
  end;
end;

function TDotEnv.HandleIncludeLine(const CurrentLine, FilePath: string; LineNumber: Integer;
  RootIndex, FileOrder: Integer; Entries: TList<TRawAssignment>): Boolean;
var
  Trimmed: string;
  Optional: Boolean;
  QuoteStart, QuoteEnd: Integer;
  IncludePath: string;
  Resolved: string;
begin
  Trimmed := TrimLeft(CurrentLine);
  Optional := False;
  if StartsText('#include_if_exists', Trimmed) then
    Optional := True
  else if not StartsText('#include', Trimmed) then
    Exit(False);

  QuoteStart := Pos('"', Trimmed);
  QuoteEnd := LastDelimiter('"', Trimmed);
  if (QuoteStart = 0) or (QuoteEnd <= QuoteStart) then
  begin
    AddError(FilePath, LineNumber, 1, TDotEnvErrorKind.DekParse,
      'Include directive requires quoted path', True);
    Exit(True);
  end;
  IncludePath := Copy(Trimmed, QuoteStart + 1, QuoteEnd - QuoteStart - 1);
  Resolved := ResolveIncludePath(FilePath, IncludePath);
  if Resolved = '' then
  begin
    AddError(FilePath, LineNumber, 1, TDotEnvErrorKind.DekIO,
      Format('Unable to resolve include target %s', [IncludePath]), True);
    Exit(True);
  end;

  AddTrace('considered: ' + Resolved);
  if not TFile.Exists(Resolved) then
  begin
    AddTrace('missing: ' + Resolved);
    if not Optional then
      AddError(FilePath, LineNumber, 1, TDotEnvErrorKind.DekIO,
        Format('Included file not found: %s', [Resolved]), True);
    Exit(True);
  end;

  if ParseFile(Resolved, RootIndex, FileOrder, Entries) then
    AddTrace('loaded: ' + Resolved)
  else
    AddTrace('ignored: ' + Resolved);
  Result := True;
end;

function TDotEnv.ResolveIncludePath(const BaseFile, Target: string): string;
var
  BaseDir: string;
  Combined: string;
begin
  Result := '';
  if Target = '' then
    Exit;
  if TPath.IsRelativePath(Target) then
  begin
    if BaseFile <> '' then
      BaseDir := TPath.GetDirectoryName(BaseFile)
    else
      BaseDir := TDirectory.GetCurrentDirectory;
    Combined := TPath.Combine(BaseDir, Target);
  end
  else
    Combined := Target;
  try
    Result := TPath.GetFullPath(Combined);
  except
    Result := '';
  end;
end;

function TDotEnv.IsPathOnStack(const Path: string): Boolean;
var
  Item: string;
begin
  Result := False;
  if FIncludeStack = nil then
    Exit(False);
  for Item in FIncludeStack do
    if SameFileName(Item, Path) then
      Exit(True);
end;

procedure TDotEnv.LoadLayered(const ABaseDir: string; const AOptions: TDotEnvOptions);
var
  Entries: TList<TRawAssignment>;
  FileInfos: TArray<TLayeredFile>;
  Info: TLayeredFile;
  Entry: TRawAssignment;
  Value: string;
  FileOrder: Integer;
begin
  FCurrentOptions := AOptions;
  ResetState;
  EnsureValuesDictionary;
  Entries := TList<TRawAssignment>.Create;
  try
    FileInfos := CollectLayeredFiles(ABaseDir, AOptions);
    FileOrder := 0;
    for Info in FileInfos do
    begin
      CollectFile(Info.Path, Info.RootIndex, FileOrder, Entries);
      Inc(FileOrder);
    end;
    for Entry in Entries do
    begin
      Value := ExpandValue(Entry.RawValue, Entry.FilePath, Entry.Line);
      StoreValue(Entry, Value);
    end;
  finally
    Entries.Free;
  end;
end;

class function TDotEnv.DefaultSearchRoots(const ABaseDir: string; AOptions: TDotEnvOptions): TArray<TSearchRoot>;
var
  Inst: TDotEnv;
begin
  Inst := TDotEnv.Create;
  try
    Result := Inst.ExpandRoots(ABaseDir, AOptions);
  finally
    Inst.Free;
  end;
end;

end.
