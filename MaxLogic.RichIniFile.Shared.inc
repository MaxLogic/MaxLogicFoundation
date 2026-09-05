interface

uses
  System.Classes, System.Generics.Collections, System.Generics.Defaults, System.IniFiles, System.SysUtils;

type
  {$IFDEF RICHINI_UTF8}
  TRichIniString = UTF8String;
  TRichIniStrings = TList<UTF8String>;
  {$ELSE}
  TRichIniString = UnicodeString;
  TRichIniStrings = TStrings;
  {$ENDIF}

  TEncodingMode = (
    eoAutoDetect,
    eoUTF8,
    eoAnsi,
    eoCustom
  );

  TBomPolicy = (
    bpAsSource,
    bpNone,
    bpForce
  );

  TNewlineMode = (
    nlPreserveInput,
    nlPlatform,
    nlWindowsCRLF,
    nlUnixLF
  );

  TCommentOwnership = (
    coNone,
    coAttachToNext,
    coAttachToPrev
  );

  TMissingBracketPolicy = (
    mbAcceptAsSection,
    mbTreatAsKey
  );

  TCaseSensitivity = (
    csCaseSensitive,
    csCaseInsensitive
  );

  TRichIniOptions = record
    LoadEncoding: TEncodingMode;
    SaveEncoding: TEncodingMode;
    CustomEncoding: TEncoding;
    BomPolicy: TBomPolicy;
    NewlineMode: TNewlineMode;
    CaseSensitivity: TCaseSensitivity;
    AcceptMissingBracket: TMissingBracketPolicy;
    CommentPrefixes: TArray<TRichIniString>;
    CommentOwnership: TCommentOwnership;
    KeyValueDelimiter: Char;
    BooleanTrueValues: TArray<TRichIniString>;
    BooleanTrueValue: TRichIniString;
    BooleanFalseValue: TRichIniString;
  end;

function DefaultRichIniOptions: TRichIniOptions;

type
  TRichIniLineKind = (
    lkSectionHeader,
    lkKeyValue,
    lkComment,
    lkBlank,
    lkUnparsed
  );

  TRichIniLine = class;
  TRichIniCommentLine = class;
  TRichIniKeyLine = class;
  TRichIniSectionBlock = class;

  TRichIniLine = class
  private
    fKind: TRichIniLineKind;
    fRawText: TRichIniString;
    fModified: Boolean;
    fSectionBlock: TRichIniSectionBlock;
    fOwnedComments: TObjectList<TRichIniCommentLine>;
    function GetOwnedComments: TObjectList<TRichIniCommentLine>;
  public
    constructor Create(const aKind: TRichIniLineKind; const aRawText: TRichIniString);
    destructor Destroy; override;
    procedure AttachComment(aComment: TRichIniCommentLine);
    procedure DetachComment(aComment: TRichIniCommentLine);
    procedure ClearOwnedComments;
    function OwnedCommentCount: Integer;
    property Kind: TRichIniLineKind read fKind;
    property RawText: TRichIniString read fRawText write fRawText;
    property Modified: Boolean read fModified write fModified;
    property SectionBlock: TRichIniSectionBlock read fSectionBlock write fSectionBlock;
    property OwnedComments: TObjectList<TRichIniCommentLine> read GetOwnedComments;
  end;

  TRichIniCommentLine = class(TRichIniLine)
  private
    fLeadingWhitespace: TRichIniString;
    fPrefix: TRichIniString;
    fText: TRichIniString;
    fOwnerLine: TRichIniLine;
  public
    constructor Create(const aRawText, aLeadingWhitespace, aPrefix, aCommentText: TRichIniString);
    property LeadingWhitespace: TRichIniString read fLeadingWhitespace write fLeadingWhitespace;
    property Prefix: TRichIniString read fPrefix write fPrefix;
    property CommentText: TRichIniString read fText write fText;
    property OwnerLine: TRichIniLine read fOwnerLine write fOwnerLine;
  end;

  TRichIniSectionLine = class(TRichIniLine)
  private
    fName: TRichIniString;
  public
    constructor Create(const aName, aRawText: TRichIniString);
    property Name: TRichIniString read fName write fName;
  end;

  TRichIniKeyLine = class(TRichIniLine)
  private
    fKey: TRichIniString;
    fValue: TRichIniString;
    fDelimiter: Char;
    fLookupKey: TRichIniString;
  public
    constructor Create(const aKey, aValue, aLookupKey: TRichIniString; const aDelimiter: Char; const aRawText: TRichIniString);
    property Key: TRichIniString read fKey write fKey;
    property Value: TRichIniString read fValue write fValue;
    property Delimiter: Char read fDelimiter write fDelimiter;
    property LookupKey: TRichIniString read fLookupKey write fLookupKey;
  end;

  TRichIniSectionBlock = class
  private
    fName: TRichIniString;
    fLookupName: TRichIniString;
    fNoHeader: Boolean;
    fHeaderLine: TRichIniSectionLine;
    fKeyMap: TObjectDictionary<TRichIniString, TObjectList<TRichIniKeyLine>>;
    fKeyOrder: TList<TRichIniKeyLine>;
    constructor CreateWithComparer(const aName, aLookupName: TRichIniString; const aNoHeader: Boolean;
      const aComparer: IEqualityComparer<TRichIniString>);
  public
    constructor Create(const aName, aLookupName: TRichIniString; const aNoHeader: Boolean);
    destructor Destroy; override;
    procedure AddKeyLine(aLine: TRichIniKeyLine);
    procedure RemoveKeyLine(aLine: TRichIniKeyLine);
    function KeyLines(const aToken: TRichIniString): TObjectList<TRichIniKeyLine>;
    property KeyOrder: TList<TRichIniKeyLine> read fKeyOrder;
    property Name: TRichIniString read fName write fName;
    property LookupName: TRichIniString read fLookupName write fLookupName;
    property NoHeader: Boolean read fNoHeader write fNoHeader;
    property HeaderLine: TRichIniSectionLine read fHeaderLine write fHeaderLine;
  end;

  TRichIniFile = class({$IFDEF RICHINI_UTF8}TObject{$ELSE}TCustomIniFile{$ENDIF})
  private type
    TRichIniParseState = record
      CurrentBlock: TRichIniSectionBlock;
      PendingComments: TList<TRichIniCommentLine>;
      LastContentLine: TRichIniLine;
    end;
  private
    fFileName: string;
    fOptions: TRichIniOptions;
    fDelimiterText: TRichIniString;
    fLines: TObjectList<TRichIniLine>;
    fSections: TObjectList<TRichIniSectionBlock>;
    fSectionMap: TObjectDictionary<TRichIniString, TObjectList<TRichIniSectionBlock>>;
    fComparer: IEqualityComparer<TRichIniString>;
    fDirty: Boolean;
    fDetectedNewline: TRichIniString;
    fSourceEncoding: TEncoding;
    fSourceBom: Boolean;
    fHasSource: Boolean;
    function CurrentComparer: IEqualityComparer<TRichIniString>; inline;
    function TokensEqual(const aLeft, aRight: TRichIniString): Boolean; inline;
    procedure EnsureOptionsDefaults;
    procedure ClearDocument;
    procedure CreateGlobalSection;
    function CreateSectionBlock(const aSection: TRichIniString; const aNoHeader: Boolean): TRichIniSectionBlock;
    function GetOrCreateSectionList(const aToken: TRichIniString): TObjectList<TRichIniSectionBlock>;
    function SectionToken(const aSection: TRichIniString): TRichIniString; inline;
    function KeyToken(const aKey: TRichIniString): TRichIniString; inline;
    procedure AddLineToDocument(aLine: TRichIniLine);
    procedure AttachPendingComments(const aOwner: TRichIniLine; const aPending: TList<TRichIniCommentLine>);
    procedure ProcessLine(const aLine: TRichIniString; var aState: TRichIniParseState);
    procedure ParseLines(const aLines: TArray<TRichIniString>);
    procedure ParseText(const aText: TRichIniString);
    function DetectNewline(const aText: TRichIniString): TRichIniString;
    function ResolveLoadEncoding(const aBuffer: TBytes; out aEncoding: TEncoding; out aHasBom: Boolean): TRichIniString;
    function PlatformNewline: TRichIniString;
    function ResolveNewline: TRichIniString;
    function FindSectionList(const aSection: TRichIniString): TObjectList<TRichIniSectionBlock>;
    function FindLastKeyLine(const aSection, aKey: TRichIniString): TRichIniKeyLine;
    function BuildKeyRawText(const aKey, aValue: TRichIniString): TRichIniString;
    function EnsureSectionBlockForWrite(const aSection: TRichIniString): TRichIniSectionBlock;
    function GetSectionInsertIndex(const aBlock: TRichIniSectionBlock): Integer;
    function FindKeyLineByIndex(const aSection, aKey: TRichIniString; aKeyIndex: Integer): TRichIniKeyLine;
    procedure RemoveOwnedComments(aLine: TRichIniLine);
    procedure DeleteKeyLineInstance(aLine: TRichIniKeyLine);
    function DefaultCommentPrefix: TRichIniString;
    function IsTrueBooleanValue(const aValue: TRichIniString): Boolean;
    function SplitToLines(const aText: TRichIniString): TArray<TRichIniString>;
    function CollectCommentText(const aLine: TRichIniLine): TRichIniString;
    function EncodeMultilineValue(const aValue: TRichIniString): TRichIniString;
    function DecodeMultilineValue(const aValue: TRichIniString): TRichIniString;
    procedure ResolveSaveEncoding(out aEncoding: TEncoding; out aWriteBom: Boolean);

    function RenderLine(const aLine: TRichIniLine): TRichIniString;
    function CreateTempFileName(const aTarget: string): string;
    procedure ReplaceFileAtomic(const aSource, aDestination: string);
  protected
    function GetLineCount: Integer;
    function GetSectionBlockCount: Integer;
  public
    constructor Create(const aFileName: string; const aOptions: TRichIniOptions); reintroduce; overload;
    constructor CreateFromStrings(const aLines: TArray<TRichIniString>; const aOptions: TRichIniOptions); overload;
    destructor Destroy; override;

    procedure LoadFromText(const aText: TRichIniString; const aSourceEncoding: TEncoding = nil; aHasBom: Boolean = False);
    procedure LoadFromFile(const aFileName: string);
    procedure SaveToFile(const aFileName: string = '');
    procedure UpdateFile; {$IFNDEF RICHINI_UTF8}override;{$ENDIF}

    function ReadString(const aSection, aKey, aDefault: TRichIniString): TRichIniString; {$IFNDEF RICHINI_UTF8}override;{$ENDIF}
    procedure WriteString(const aSection, aKey, aValue: TRichIniString); overload; {$IFNDEF RICHINI_UTF8}override;{$ENDIF}
    procedure WriteString(const aSection, aKey, aValue: TRichIniString; aKeyIndex: Integer); reintroduce; overload;
    function ReadInteger(const aSection, aKey: TRichIniString; aDefault: Integer): Integer; {$IFNDEF RICHINI_UTF8}override;{$ENDIF}
    procedure WriteInteger(const aSection, aKey: TRichIniString; aValue: Integer); {$IFNDEF RICHINI_UTF8}override;{$ENDIF}
    function ReadBool(const aSection, aKey: TRichIniString; aDefault: Boolean): Boolean; {$IFNDEF RICHINI_UTF8}override;{$ENDIF}
    procedure WriteBool(const aSection, aKey: TRichIniString; aValue: Boolean); {$IFNDEF RICHINI_UTF8}override;{$ENDIF}
    procedure DeleteKey(const aSection, aKey: TRichIniString); overload; {$IFNDEF RICHINI_UTF8}override;{$ENDIF}
    procedure DeleteKey(const aSection, aKey: TRichIniString; aKeyIndex: Integer); reintroduce; overload;
    procedure EraseSection(const aSection: TRichIniString); {$IFNDEF RICHINI_UTF8}override;{$ENDIF}
    procedure ReadSection(const aSection: TRichIniString; aStrings: TRichIniStrings); {$IFNDEF RICHINI_UTF8}override;{$ENDIF}
    procedure ReadSections(aStrings: TRichIniStrings); {$IFNDEF RICHINI_UTF8}override;{$ENDIF}
    procedure ReadSectionValues(const aSection: TRichIniString; aStrings: TRichIniStrings); {$IFNDEF RICHINI_UTF8}override;{$ENDIF}

    function AppendKey(const aSection, aKey, aValue: TRichIniString): Integer;
    function LastKeyIndex(const aSection, aKey: TRichIniString): Integer;
    function KeyCount(const aSection, aKey: TRichIniString): Integer;
    procedure ReadAllKeyValues(const aSection, aKey: TRichIniString; out aValues: TArray<TRichIniString>);

    function ReadComment(const aSection, aKey: TRichIniString): TRichIniString;
    procedure WriteComment(const aSection, aKey, aComment: TRichIniString);
    procedure PurgeComments;

    procedure ConsolidateSection(const aSection: TRichIniString);
    procedure ConsolidateAll;

    procedure WriteMultilineString(const aSection, aKey, aValueWithLineBreaks: TRichIniString);
    function ReadMultilineString(const aSection, aKey, aDefault: TRichIniString): TRichIniString;

    property FileName: string read fFileName;
    property Options: TRichIniOptions read fOptions;
    property Dirty: Boolean read fDirty;
    property LineCount: Integer read GetLineCount;
    property SectionBlockCount: Integer read GetSectionBlockCount;
  end;
{$IFDEF RICHINI_UTF8}
type
  TRichIniFileUtf8 = TRichIniFile;
  TRichIniOptionsUtf8 = TRichIniOptions;
{$ENDIF}

implementation

uses
  System.IOUtils
{$IFDEF MSWINDOWS}, Winapi.Windows{$ENDIF},
  AutoFree, MaxLogic.StrUtils;

type
  PRichIniChar = {$IFDEF RICHINI_UTF8}PAnsiChar{$ELSE}PWideChar{$ENDIF};

{$IFDEF RICHINI_UTF8}
{$I MaxLogic.RichIniFile.Utf8.Helpers.inc}
{$ELSE}
type
  TRichIniBuilder = TStringBuilder;
{$ENDIF}

const
  cDefaultCommentPrefixes: array[0..2] of TRichIniString = (';', '#', '//');
  cDefaultBooleanTrueValues: array[0..5] of TRichIniString = ('1', 'y', 'yes', 'on', 'enabled', 'true');
  cDefaultBooleanTrueValue = '1';
  cDefaultBooleanFalseValue = '0';

function DefaultRichIniOptions: TRichIniOptions;
var
  lIndex: Integer;
begin
  Result.LoadEncoding := eoAutoDetect;
  Result.SaveEncoding := eoUTF8;
  Result.CustomEncoding := nil;
  Result.BomPolicy := bpAsSource;
  Result.NewlineMode := nlPlatform;
  Result.CaseSensitivity := csCaseSensitive;
  Result.AcceptMissingBracket := mbTreatAsKey;
  Result.CommentOwnership := coAttachToNext;
  Result.KeyValueDelimiter := '=';
  SetLength(Result.CommentPrefixes, Length(cDefaultCommentPrefixes));
  for lIndex := Low(cDefaultCommentPrefixes) to High(cDefaultCommentPrefixes) do
    Result.CommentPrefixes[lIndex] := cDefaultCommentPrefixes[lIndex];
  SetLength(Result.BooleanTrueValues, Length(cDefaultBooleanTrueValues));
  for lIndex := Low(cDefaultBooleanTrueValues) to High(cDefaultBooleanTrueValues) do
    Result.BooleanTrueValues[lIndex] := cDefaultBooleanTrueValues[lIndex];
  Result.BooleanTrueValue := cDefaultBooleanTrueValue;
  Result.BooleanFalseValue := cDefaultBooleanFalseValue;
end;

{ TRichIniLine }

constructor TRichIniLine.Create(const aKind: TRichIniLineKind; const aRawText: TRichIniString);
begin
  inherited Create;
  fKind := aKind;
  fRawText := aRawText;
end;

destructor TRichIniLine.Destroy;
begin
  ClearOwnedComments;
  fOwnedComments.Free;
  inherited;
end;

procedure TRichIniLine.AttachComment(aComment: TRichIniCommentLine);
begin
  if aComment = nil then
    Exit;
  if OwnedComments.IndexOf(aComment) < 0 then
  begin
    OwnedComments.Add(aComment);
    aComment.OwnerLine := Self;
  end;
end;

procedure TRichIniLine.ClearOwnedComments;
var
  lComment: TRichIniCommentLine;
begin
  if fOwnedComments = nil then
    Exit;
  for lComment in fOwnedComments do
    lComment.OwnerLine := nil;
  fOwnedComments.Clear;
end;

procedure TRichIniLine.DetachComment(aComment: TRichIniCommentLine);
begin
  if (fOwnedComments = nil) or (aComment = nil) then
    Exit;
  if fOwnedComments.Remove(aComment) >= 0 then
    aComment.OwnerLine := nil;
end;

function TRichIniLine.GetOwnedComments: TObjectList<TRichIniCommentLine>;
begin
  if fOwnedComments = nil then
    fOwnedComments := TObjectList<TRichIniCommentLine>.Create(False);
  Result := fOwnedComments;
end;

function TRichIniLine.OwnedCommentCount: Integer;
begin
  if fOwnedComments = nil then
    Result := 0
  else
    Result := fOwnedComments.Count;
end;

{ TRichIniCommentLine }

constructor TRichIniCommentLine.Create(const aRawText, aLeadingWhitespace, aPrefix, aCommentText: TRichIniString);
begin
  inherited Create(lkComment, aRawText);
  fLeadingWhitespace := aLeadingWhitespace;
  fPrefix := aPrefix;
  fText := aCommentText;
end;

{ TRichIniSectionLine }

constructor TRichIniSectionLine.Create(const aName, aRawText: TRichIniString);
begin
  inherited Create(lkSectionHeader, aRawText);
  fName := aName;
end;

{ TRichIniKeyLine }

constructor TRichIniKeyLine.Create(const aKey, aValue, aLookupKey: TRichIniString; const aDelimiter: Char;
  const aRawText: TRichIniString);
begin
  inherited Create(lkKeyValue, aRawText);
  fKey := aKey;
  fValue := aValue;
  fLookupKey := aLookupKey;
  fDelimiter := aDelimiter;
end;

{ TRichIniSectionBlock }

constructor TRichIniSectionBlock.Create(const aName, aLookupName: TRichIniString; const aNoHeader: Boolean);
begin
  CreateWithComparer(aName, aLookupName, aNoHeader, TFastCaseAwareComparer.{$IFDEF RICHINI_UTF8}OrdinalUtf8{$ELSE}Ordinal{$ENDIF});
end;

constructor TRichIniSectionBlock.CreateWithComparer(const aName, aLookupName: TRichIniString; const aNoHeader: Boolean;
  const aComparer: IEqualityComparer<TRichIniString>);
begin
  inherited Create;
  fName := aName;
  fLookupName := aLookupName;
  fNoHeader := aNoHeader;
  fKeyMap := TObjectDictionary<TRichIniString, TObjectList<TRichIniKeyLine>>.Create([doOwnsValues], aComparer);
  fKeyOrder := TList<TRichIniKeyLine>.Create;
end;

destructor TRichIniSectionBlock.Destroy;
begin
  fKeyOrder.Free;
  fKeyMap.Free;
  inherited;
end;

procedure TRichIniSectionBlock.AddKeyLine(aLine: TRichIniKeyLine);
var
  lList: TObjectList<TRichIniKeyLine>;
begin
  if not fKeyMap.TryGetValue(aLine.LookupKey, lList) then
  begin
    lList := TObjectList<TRichIniKeyLine>.Create(False);
    fKeyMap.Add(aLine.LookupKey, lList);
  end;
  lList.Add(aLine);
  fKeyOrder.Add(aLine);
end;

function TRichIniSectionBlock.KeyLines(const aToken: TRichIniString): TObjectList<TRichIniKeyLine>;
begin
  if not fKeyMap.TryGetValue(aToken, Result) then
    Result := nil;
end;

procedure TRichIniSectionBlock.RemoveKeyLine(aLine: TRichIniKeyLine);
var
  lList: TObjectList<TRichIniKeyLine>;
begin
  if not fKeyMap.TryGetValue(aLine.LookupKey, lList) then
    Exit;
  lList.Remove(aLine);
  if lList.Count = 0 then
    fKeyMap.Remove(aLine.LookupKey);
  fKeyOrder.Remove(aLine);
end;

{ TRichIniFile }

constructor TRichIniFile.Create(const aFileName: string; const aOptions: TRichIniOptions);
begin
  inherited Create{$IFNDEF RICHINI_UTF8}(aFileName){$ENDIF};
  fFileName := aFileName;
  fOptions := aOptions;
  EnsureOptionsDefaults;
  fComparer := nil;
  fLines := TObjectList<TRichIniLine>.Create(True);
  fSections := TObjectList<TRichIniSectionBlock>.Create(True);
  fSectionMap := TObjectDictionary<TRichIniString, TObjectList<TRichIniSectionBlock>>.Create([doOwnsValues], CurrentComparer);
  ClearDocument;
end;

constructor TRichIniFile.CreateFromStrings(const aLines: TArray<TRichIniString>; const aOptions: TRichIniOptions);
begin
  inherited Create{$IFNDEF RICHINI_UTF8}(''){$ENDIF};
  fFileName := '';
  fOptions := aOptions;
  EnsureOptionsDefaults;
  fComparer := nil;
  fLines := TObjectList<TRichIniLine>.Create(True);
  fSections := TObjectList<TRichIniSectionBlock>.Create(True);
  fSectionMap := TObjectDictionary<TRichIniString, TObjectList<TRichIniSectionBlock>>.Create([doOwnsValues], CurrentComparer);
  ClearDocument;
  fHasSource := False;
  fSourceEncoding := TEncoding.UTF8;
  fSourceBom := False;
  fDetectedNewline := PlatformNewline;
  ParseLines(aLines);
  fSourceEncoding := TEncoding.UTF8;
  fSourceBom := False;
  fHasSource := True;
  fDirty := False;
end;

destructor TRichIniFile.Destroy;
begin
  fSectionMap.Free;
  fSections.Free;
  fLines.Free;
  inherited;
end;

procedure TRichIniFile.AddLineToDocument(aLine: TRichIniLine);
begin
  if aLine = nil then
    Exit;
  fLines.Add(aLine);
end;

procedure TRichIniFile.AttachPendingComments(const aOwner: TRichIniLine; const aPending: TList<TRichIniCommentLine>);
var
  lComment: TRichIniCommentLine;
begin
  if (aOwner = nil) or (aPending = nil) or (aPending.Count = 0) then
    Exit;
  for lComment in aPending do
    aOwner.AttachComment(lComment);
  aPending.Clear;
end;

procedure TRichIniFile.ProcessLine(const aLine: TRichIniString; var aState: TRichIniParseState);
var
  lLineLen: Integer;
  lP: PRichIniChar;
  lTrimStart: Integer;
  lTrimEnd: Integer;
  lTrimLength: Integer;
  lPrefix: TRichIniString;
  lPrefixLen: Integer;
  lCommentTextStart: Integer;
  lLeadingWs: TRichIniString;
  lCommentText: TRichIniString;
  lComment: TRichIniCommentLine;
  lSectionName: TRichIniString;
  lSectionStart: Integer;
  lSectionEnd: Integer;
  lClosingPos: Integer;
  lSectionLine: TRichIniSectionLine;
  lDelimiterPos: Integer;
  lKeyStart: Integer;
  lKeyEnd: Integer;
  lValueStart: Integer;
  lValueEnd: Integer;
  lKeyText: TRichIniString;
  lValueText: TRichIniString;
  lKeyLine: TRichIniKeyLine;
  lLine: TRichIniLine;
  i: Integer;
begin
  lLineLen := Length(aLine);
  lP := PRichIniChar(aLine);
  lTrimStart := 1;
  while (lTrimStart <= lLineLen) and ((lP[lTrimStart - 1] = ' ') or (lP[lTrimStart - 1] = #9)) do
    Inc(lTrimStart);
  if lTrimStart > lLineLen then
  begin
    lLine := TRichIniLine.Create(lkBlank, aLine);
    lLine.SectionBlock := aState.CurrentBlock;
    AddLineToDocument(lLine);
    Exit;
  end;

  lTrimEnd := lLineLen;
  while (lTrimEnd >= lTrimStart) and ((lP[lTrimEnd - 1] = ' ') or (lP[lTrimEnd - 1] = #9)) do
    Dec(lTrimEnd);
  lTrimLength := lTrimEnd - lTrimStart + 1;

  for i := 0 to High(fOptions.CommentPrefixes) do
  begin
    lPrefix := fOptions.CommentPrefixes[i];
    lPrefixLen := Length(lPrefix);
    if (lPrefixLen > 0) and (lTrimLength >= lPrefixLen) and
       (lP[lTrimStart - 1] = lPrefix[1]) and
       {$IFDEF RICHINI_UTF8}
       CompareMem(lP + (lTrimStart - 1), PRichIniChar(lPrefix), lPrefixLen) then
       {$ELSE}
       (StrLComp(lP + (lTrimStart - 1), PRichIniChar(lPrefix), lPrefixLen) = 0) then
       {$ENDIF}
    begin
      lCommentTextStart := lTrimStart + lPrefixLen;
      while (lCommentTextStart <= lLineLen) and ((lP[lCommentTextStart - 1] = ' ') or (lP[lCommentTextStart - 1] = #9)) do
        Inc(lCommentTextStart);
      if lCommentTextStart <= lLineLen then
        SetString(lCommentText, lP + (lCommentTextStart - 1), lLineLen - lCommentTextStart + 1)
      else
        lCommentText := '';
      if lTrimStart > 1 then
      begin
        SetString(lLeadingWs, lP, lTrimStart - 1);
        lComment := TRichIniCommentLine.Create(aLine, lLeadingWs, lPrefix, lCommentText);
      end
      else
        lComment := TRichIniCommentLine.Create(aLine, '', lPrefix, lCommentText);
      lComment.SectionBlock := aState.CurrentBlock;
      AddLineToDocument(lComment);
      case fOptions.CommentOwnership of
        coAttachToNext:
          aState.PendingComments.Add(lComment);
        coAttachToPrev:
          if aState.LastContentLine <> nil then
            aState.LastContentLine.AttachComment(lComment);
      end;
      Exit;
    end;
  end;

  if lP[lTrimStart - 1] = '[' then
  begin
    lClosingPos := 0;
    for i := lTrimStart + 1 to lLineLen do
      if lP[i - 1] = ']' then
      begin
        lClosingPos := i;
        Break;
      end;
    if (lClosingPos > lTrimStart) or
       ((lClosingPos = 0) and (fOptions.AcceptMissingBracket = mbAcceptAsSection)) then
    begin
      lSectionStart := lTrimStart + 1;
      if lClosingPos > lTrimStart then
        lSectionEnd := lClosingPos - 1
      else
        lSectionEnd := lLineLen;
      while (lSectionStart <= lSectionEnd) and ((lP[lSectionStart - 1] = ' ') or (lP[lSectionStart - 1] = #9)) do
        Inc(lSectionStart);
      while (lSectionEnd >= lSectionStart) and ((lP[lSectionEnd - 1] = ' ') or (lP[lSectionEnd - 1] = #9)) do
        Dec(lSectionEnd);
      if lSectionStart <= lSectionEnd then
        SetString(lSectionName, lP + (lSectionStart - 1), lSectionEnd - lSectionStart + 1)
      else
        lSectionName := '';
      aState.CurrentBlock := CreateSectionBlock(lSectionName, False);
      lSectionLine := TRichIniSectionLine.Create(lSectionName, aLine);
      lSectionLine.SectionBlock := aState.CurrentBlock;
      aState.CurrentBlock.HeaderLine := lSectionLine;
      AddLineToDocument(lSectionLine);
      AttachPendingComments(lSectionLine, aState.PendingComments);
      aState.LastContentLine := lSectionLine;
      Exit;
    end;
  end;

  lDelimiterPos := 0;
  for i := lTrimStart to lLineLen do
    if (lP[i - 1] = fDelimiterText[1]) and
       (i + Length(fDelimiterText) - 1 <= lLineLen) and
       ((Length(fDelimiterText) = 1) or
        CompareMem(lP + i - 1, PRichIniChar(fDelimiterText), Length(fDelimiterText) * SizeOf(lP^))) then
    begin
      lDelimiterPos := i;
      Break;
    end;

  if lDelimiterPos > 0 then
    lKeyEnd := lDelimiterPos - 1
  else
    lKeyEnd := lTrimEnd;
  lKeyStart := lTrimStart;
  while (lKeyStart <= lKeyEnd) and ((lP[lKeyStart - 1] = ' ') or (lP[lKeyStart - 1] = #9)) do
    Inc(lKeyStart);
  while (lKeyEnd >= lKeyStart) and ((lP[lKeyEnd - 1] = ' ') or (lP[lKeyEnd - 1] = #9)) do
    Dec(lKeyEnd);
  if lKeyStart <= lKeyEnd then
    SetString(lKeyText, lP + (lKeyStart - 1), lKeyEnd - lKeyStart + 1)
  else
    lKeyText := '';

  if lDelimiterPos > 0 then
  begin
    lValueStart := lDelimiterPos + Length(fDelimiterText);
    while (lValueStart <= lLineLen) and ((lP[lValueStart - 1] = ' ') or (lP[lValueStart - 1] = #9)) do
      Inc(lValueStart);
    lValueEnd := lLineLen;
    while (lValueEnd >= lValueStart) and ((lP[lValueEnd - 1] = ' ') or (lP[lValueEnd - 1] = #9)) do
      Dec(lValueEnd);
    if lValueStart <= lValueEnd then
      SetString(lValueText, lP + (lValueStart - 1), lValueEnd - lValueStart + 1)
    else
      lValueText := '';
  end else
    lValueText := '';

  lKeyLine := TRichIniKeyLine.Create(lKeyText, lValueText, KeyToken(lKeyText), fOptions.KeyValueDelimiter, aLine);
  lKeyLine.SectionBlock := aState.CurrentBlock;
  aState.CurrentBlock.AddKeyLine(lKeyLine);
  AddLineToDocument(lKeyLine);
  AttachPendingComments(lKeyLine, aState.PendingComments);
  aState.LastContentLine := lKeyLine;
end;

procedure TRichIniFile.ParseLines(const aLines: TArray<TRichIniString>);
var
  lState: TRichIniParseState;
  i: Integer;
begin
  if fSections.Count = 0 then
    CreateGlobalSection;

  if (fLines <> nil) and (fLines.Capacity < fLines.Count + Length(aLines)) then
    fLines.Capacity := fLines.Count + Length(aLines);

  lState.CurrentBlock := fSections[0];
  lState.PendingComments := TList<TRichIniCommentLine>.Create;
  try
    lState.LastContentLine := nil;
    for i := 0 to High(aLines) do
      ProcessLine(aLines[i], lState);
  finally
    lState.PendingComments.Free;
  end;
end;

procedure TRichIniFile.ClearDocument;
begin
  if fLines <> nil then
    fLines.Clear;
  if fSections <> nil then
    fSections.Clear;
  if fSectionMap <> nil then
    fSectionMap.Clear;
  fDirty := False;
  fDetectedNewline := '';
  fSourceEncoding := nil;
  fSourceBom := False;
  fHasSource := False;
  if fSections <> nil then
    CreateGlobalSection;
end;

procedure TRichIniFile.CreateGlobalSection;
begin
  CreateSectionBlock('', True);
end;

function TRichIniFile.CreateSectionBlock(const aSection: TRichIniString; const aNoHeader: Boolean): TRichIniSectionBlock;
var
  lToken: TRichIniString;
begin
  lToken := SectionToken(aSection);
  Result := TRichIniSectionBlock.CreateWithComparer(aSection, lToken, aNoHeader, CurrentComparer);
  fSections.Add(Result);
  GetOrCreateSectionList(lToken).Add(Result);
end;

function TRichIniFile.DetectNewline(const aText: TRichIniString): TRichIniString;
var
  i: Integer;
begin
  for i := 1 to Length(aText) do
  begin
    if aText[i] = #13 then
    begin
      if (i < Length(aText)) and (aText[i + 1] = #10) then
        Exit(#13#10)
      else
        Exit(#13);
    end;
    if aText[i] = #10 then
      Exit(#10);
  end;
  Result := '';
end;

procedure TRichIniFile.EnsureOptionsDefaults;
var
  lIndex: Integer;
begin
  if Length(fOptions.CommentPrefixes) = 0 then
  begin
    SetLength(fOptions.CommentPrefixes, Length(cDefaultCommentPrefixes));
    for lIndex := Low(cDefaultCommentPrefixes) to High(cDefaultCommentPrefixes) do
      fOptions.CommentPrefixes[lIndex] := cDefaultCommentPrefixes[lIndex];
  end;
  if fOptions.KeyValueDelimiter = #0 then
    fOptions.KeyValueDelimiter := '=';
  fDelimiterText := TRichIniString(string(fOptions.KeyValueDelimiter));
  if Length(fOptions.BooleanTrueValues) = 0 then
  begin
    SetLength(fOptions.BooleanTrueValues, Length(cDefaultBooleanTrueValues));
    for lIndex := Low(cDefaultBooleanTrueValues) to High(cDefaultBooleanTrueValues) do
      fOptions.BooleanTrueValues[lIndex] := cDefaultBooleanTrueValues[lIndex];
  end;
  if fOptions.BooleanTrueValue = '' then
    fOptions.BooleanTrueValue := cDefaultBooleanTrueValue;
  if fOptions.BooleanFalseValue = '' then
    fOptions.BooleanFalseValue := cDefaultBooleanFalseValue;
end;

function TRichIniFile.SectionToken(const aSection: TRichIniString): TRichIniString;
begin
  Result := aSection;
end;

function TRichIniFile.KeyToken(const aKey: TRichIniString): TRichIniString;
begin
  Result := aKey;
end;

function TRichIniFile.GetLineCount: Integer;
begin
  if fLines <> nil then
    Result := fLines.Count
  else
    Result := 0;
end;

function TRichIniFile.GetOrCreateSectionList(const aToken: TRichIniString): TObjectList<TRichIniSectionBlock>;
begin
  if not fSectionMap.TryGetValue(aToken, Result) then
  begin
    Result := TObjectList<TRichIniSectionBlock>.Create(False);
    fSectionMap.Add(aToken, Result);
  end;
end;

function TRichIniFile.GetSectionBlockCount: Integer;
begin
  if fSections <> nil then
    Result := fSections.Count
  else
    Result := 0;
end;

function TRichIniFile.CurrentComparer: IEqualityComparer<TRichIniString>;
begin
  if fComparer = nil then
  begin
    if fOptions.CaseSensitivity = csCaseInsensitive then
      fComparer := TFastCaseAwareComparer.{$IFDEF RICHINI_UTF8}OrdinalIgnoreCaseUtf8{$ELSE}OrdinalIgnoreCase{$ENDIF}
    else
      fComparer := TFastCaseAwareComparer.{$IFDEF RICHINI_UTF8}OrdinalUtf8{$ELSE}Ordinal{$ENDIF};
  end;
  Result := fComparer;
end;

function TRichIniFile.TokensEqual(const aLeft, aRight: TRichIniString): Boolean;
begin
  Result := CurrentComparer.Equals(aLeft, aRight);
end;

function TRichIniFile.FindSectionList(const aSection: TRichIniString): TObjectList<TRichIniSectionBlock>;
var
  lToken: TRichIniString;
begin
  lToken := SectionToken(aSection);
  if not fSectionMap.TryGetValue(lToken, Result) then
    Result := nil;
end;

function TRichIniFile.FindLastKeyLine(const aSection, aKey: TRichIniString): TRichIniKeyLine;
var
  lKeyList: TObjectList<TRichIniKeyLine>;
  lSectionList: TObjectList<TRichIniSectionBlock>;
  lKeyToken: TRichIniString;
  i: Integer;
  lBlock: TRichIniSectionBlock;
begin
  Result := nil;
  if fLines = nil then
    Exit;
  lSectionList := FindSectionList(aSection);
  if lSectionList = nil then
    Exit;
  lKeyToken := KeyToken(aKey);
  for i := lSectionList.Count - 1 downto 0 do
  begin
    lBlock := lSectionList[i];
    lKeyList := lBlock.KeyLines(lKeyToken);
    if (lKeyList <> nil) and (lKeyList.Count > 0) then
    begin
      Result := lKeyList[lKeyList.Count - 1];
      Exit;
    end;
  end;
end;

function TRichIniFile.BuildKeyRawText(const aKey, aValue: TRichIniString): TRichIniString;
begin
  Result := aKey + fDelimiterText + aValue;
end;

function TRichIniFile.EnsureSectionBlockForWrite(const aSection: TRichIniString): TRichIniSectionBlock;
var
  lList: TObjectList<TRichIniSectionBlock>;
  lHeaderLine: TRichIniSectionLine;
begin
  lList := FindSectionList(aSection);
  if (lList <> nil) and (lList.Count > 0) then
    Exit(lList[lList.Count - 1]);

  if aSection = '' then
  begin
    Result := CreateSectionBlock('', True);
    Exit;
  end;

  Result := CreateSectionBlock(aSection, False);
  lHeaderLine := TRichIniSectionLine.Create(aSection, '[' + aSection + ']');
  lHeaderLine.SectionBlock := Result;
  lHeaderLine.Modified := True;
  Result.HeaderLine := lHeaderLine;
  fLines.Add(lHeaderLine);
  fDirty := True;
end;

function TRichIniFile.GetSectionInsertIndex(const aBlock: TRichIniSectionBlock): Integer;
var
  i: Integer;
  lHeaderIndex: Integer;
begin
  Result := fLines.Count;
  if aBlock = nil then
    Exit;

  for i := fLines.Count - 1 downto 0 do
    if fLines[i].SectionBlock = aBlock then
      Exit(i + 1);

  if (aBlock.HeaderLine <> nil) then
  begin
    lHeaderIndex := fLines.IndexOf(aBlock.HeaderLine);
    if lHeaderIndex >= 0 then
      Exit(lHeaderIndex + 1);
  end;

  if aBlock.NoHeader then
    Exit(0);
end;

function TRichIniFile.FindKeyLineByIndex(const aSection, aKey: TRichIniString; aKeyIndex: Integer): TRichIniKeyLine;
var
  lCount: Integer;
  lLine: TRichIniLine;
  lSectionToken: TRichIniString;
  lKeyToken: TRichIniString;
begin
  Result := nil;
  if aKeyIndex < 0 then
    Exit;

  lCount := -1;
  lSectionToken := SectionToken(aSection);
  lKeyToken := KeyToken(aKey);
  for lLine in fLines do
  begin
    if (lLine.Kind <> lkKeyValue) or (lLine.SectionBlock = nil) then
      Continue;
    if TokensEqual(lLine.SectionBlock.LookupName, lSectionToken) and
       TokensEqual(TRichIniKeyLine(lLine).LookupKey, lKeyToken) then
    begin
      Inc(lCount);
      if lCount = aKeyIndex then
      begin
        Result := TRichIniKeyLine(lLine);
        Exit;
      end;
    end;
  end;
end;

procedure TRichIniFile.RemoveOwnedComments(aLine: TRichIniLine);
var
  lComment: TRichIniCommentLine;
begin
  if (aLine = nil) or (aLine.OwnedCommentCount = 0) then
    Exit;
  while aLine.OwnedCommentCount > 0 do
  begin
    lComment := aLine.OwnedComments[aLine.OwnedCommentCount - 1];
    aLine.DetachComment(lComment);
    if fLines.IndexOf(lComment) >= 0 then
    begin
      fLines.Remove(lComment);
      fDirty := True;
    end;
  end;
end;

procedure TRichIniFile.DeleteKeyLineInstance(aLine: TRichIniKeyLine);
var
  lBlock: TRichIniSectionBlock;
begin
  if aLine = nil then
    Exit;
  RemoveOwnedComments(aLine);
  lBlock := aLine.SectionBlock;
  if lBlock <> nil then
    lBlock.RemoveKeyLine(aLine);
  if fLines.IndexOf(aLine) >= 0 then
    fLines.Remove(aLine);
  fDirty := True;
end;

function TRichIniFile.DefaultCommentPrefix: TRichIniString;
begin
  if Length(fOptions.CommentPrefixes) > 0 then
    Result := fOptions.CommentPrefixes[0]
  else
    Result := ';';
end;

function TRichIniFile.IsTrueBooleanValue(const aValue: TRichIniString): Boolean;
var
  lValue: TRichIniString;
  lTrueValue: TRichIniString;
begin
  lValue := {$IFDEF RICHINI_UTF8}IniTrim{$ELSE}Trim{$ENDIF}(aValue);
  for lTrueValue in fOptions.BooleanTrueValues do
    if {$IFDEF RICHINI_UTF8}IniSameText{$ELSE}SameText{$ENDIF}(lValue, lTrueValue) then
      Exit(True);
  Result := False;
end;

function TRichIniFile.SplitToLines(const aText: TRichIniString): TArray<TRichIniString>;
var
  lNormalized: TRichIniString;
  lList: TList<TRichIniString>;
  lStart: Integer;
  i: Integer;
begin
  lNormalized := {$IFDEF RICHINI_UTF8}IniReplace{$ELSE}StringReplace{$ENDIF}(aText, sLineBreak, #10, [rfReplaceAll]);
  lNormalized := {$IFDEF RICHINI_UTF8}IniReplace{$ELSE}StringReplace{$ENDIF}(lNormalized, #13, #10, [rfReplaceAll]);
  lList := TList<TRichIniString>.Create;
  try
    lStart := 1;
    for i := 1 to Length(lNormalized) do
      if lNormalized[i] = #10 then
      begin
        lList.Add(Copy(lNormalized, lStart, i - lStart));
        lStart := i + 1;
      end;
    lList.Add(Copy(lNormalized, lStart, Length(lNormalized) - lStart + 1));
    Result := lList.ToArray;
  finally
    lList.Free;
  end;
end;

function TRichIniFile.CollectCommentText(const aLine: TRichIniLine): TRichIniString;
var
  lBuilder: TRichIniBuilder;
  i: Integer;
begin
  if (aLine = nil) or (aLine.OwnedCommentCount = 0) then
    Exit('');
  lBuilder := TRichIniBuilder.Create;
  try
    for i := 0 to aLine.OwnedComments.Count - 1 do
    begin
      if i > 0 then
        lBuilder.Append(sLineBreak);
      lBuilder.Append(TRichIniCommentLine(aLine.OwnedComments[i]).CommentText);
    end;
    Result := lBuilder.ToString;
  finally
    lBuilder.Free;
  end;
end;

function TRichIniFile.EncodeMultilineValue(const aValue: TRichIniString): TRichIniString;
var
  lBuilder: TRichIniBuilder;
  i: Integer;
begin
  lBuilder := TRichIniBuilder.Create;
  try
    i := 1;
    while i <= Length(aValue) do
    begin
      case aValue[i] of
        #13:
          begin
            if (i < Length(aValue)) and (aValue[i + 1] = #10) then
              Inc(i);
            lBuilder.Append('\n');
          end;
        #10:
          lBuilder.Append('\n');
        '\':
          lBuilder.Append('\\');
      else
        lBuilder.Append(aValue[i]);
      end;
      Inc(i);
    end;
    Result := lBuilder.ToString;
  finally
    lBuilder.Free;
  end;
end;

function TRichIniFile.DecodeMultilineValue(const aValue: TRichIniString): TRichIniString;
var
  lBuilder: TRichIniBuilder;
  i: Integer;
begin
  lBuilder := TRichIniBuilder.Create;
  try
    i := 1;
    while i <= Length(aValue) do
    begin
      if (aValue[i] = '\') and (i < Length(aValue)) then
      begin
        case aValue[i + 1] of
          'n':
            begin
              lBuilder.Append(sLineBreak);
              Inc(i);
            end;
          '\':
            begin
              lBuilder.Append('\');
              Inc(i);
            end;
        else
          lBuilder.Append(aValue[i + 1]);
          Inc(i);
        end;
      end else
        lBuilder.Append(aValue[i]);
      Inc(i);
    end;
    Result := lBuilder.ToString;
  finally
    lBuilder.Free;
  end;
end;

procedure TRichIniFile.ResolveSaveEncoding(out aEncoding: TEncoding; out aWriteBom: Boolean);
begin
  case fOptions.SaveEncoding of
    eoUTF8:
      aEncoding := TEncoding.UTF8;
    eoAnsi:
      aEncoding := TEncoding.ANSI;
    eoCustom:
      begin
        if fOptions.CustomEncoding = nil then
          raise EArgumentException.Create('CustomEncoding must be assigned when SaveEncoding = eoCustom');
        aEncoding := fOptions.CustomEncoding;
      end;
  else
    begin
      if fHasSource and (fSourceEncoding <> nil) then
        aEncoding := fSourceEncoding
      else
        aEncoding := TEncoding.UTF8;
    end;
  end;

  case fOptions.BomPolicy of
    bpNone:
      aWriteBom := False;
    bpForce:
      aWriteBom := True;
  else
    begin
      if fHasSource then
        aWriteBom := fSourceBom
      else
        aWriteBom := False;
    end;
  end;

  if aEncoding = TEncoding.ANSI then
    aWriteBom := False;
end;

function TRichIniFile.CreateTempFileName(const aTarget: string): string;
var
  lDir: string;
  lGuid: TGuid;
  lName: string;
begin
  lDir := TPath.GetDirectoryName(aTarget);
  if lDir = '' then
    lDir := TDirectory.GetCurrentDirectory;
  CreateGuid(lGuid);
  lName := GuidToString(lGuid);
  lName := StringReplace(lName, '{', '', [rfReplaceAll]);
  lName := StringReplace(lName, '}', '', [rfReplaceAll]);
  lName := StringReplace(lName, '-', '', [rfReplaceAll]);
  Result := TPath.Combine(lDir, Format('.~%s.%s.tmp', [TPath.GetFileName(aTarget), lName]));
  while TFile.Exists(Result) do
  begin
    lName := lName + '_';
    Result := TPath.Combine(lDir, Format('.~%s.%s.tmp', [TPath.GetFileName(aTarget), lName]));
  end;
end;

procedure TRichIniFile.ReplaceFileAtomic(const aSource, aDestination: string);
begin
  {$IFDEF MSWINDOWS}
  if not MoveFileEx(PChar(aSource), PChar(aDestination), MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH) then
    RaiseLastOSError;
  {$ELSE}
  if TFile.Exists(aDestination) then
    TFile.Delete(aDestination);
  TFile.Move(aSource, aDestination);
  {$ENDIF}
end;

procedure TRichIniFile.LoadFromFile(const aFileName: string);
var
  lBuffer: TBytes;
  lEncoding: TEncoding;
  lText: TRichIniString;
  lHasBom: Boolean;
begin
  fFileName := aFileName;
  if (aFileName = '') or not TFile.Exists(aFileName) then
  begin
    ClearDocument;
    Exit;
  end;

  lBuffer := TFile.ReadAllBytes(aFileName);
  lText := ResolveLoadEncoding(lBuffer, lEncoding, lHasBom);
  LoadFromText(lText, lEncoding, lHasBom);
end;

procedure TRichIniFile.LoadFromText(const aText: TRichIniString; const aSourceEncoding: TEncoding; aHasBom: Boolean);
begin
  ClearDocument;
  fDetectedNewline := DetectNewline(aText);
  ParseText(aText);
  if aSourceEncoding <> nil then
    fSourceEncoding := aSourceEncoding
  else
    fSourceEncoding := TEncoding.UTF8;
  fSourceBom := aHasBom;
  fHasSource := True;
  fDirty := False;
end;

procedure TRichIniFile.ParseText(const aText: TRichIniString);
var
  lLength: Integer;
  lIndex: Integer;
  lLineStart: Integer;
  lLineText: TRichIniString;
  lEstimatedLines: Integer;
  lSampleLen: Integer;
  lSampleLines: Integer;
  lState: TRichIniParseState;

begin
  if fSections.Count = 0 then
    CreateGlobalSection;
  lState.CurrentBlock := fSections[0];
  lState.PendingComments := TList<TRichIniCommentLine>.Create;
  try
    lState.LastContentLine := nil;
    lLength := Length(aText);
    lEstimatedLines := 0;
    if lLength > 0 then
    begin
      lSampleLen := lLength;
      if lSampleLen > 4096 then
        lSampleLen := 4096;
      lSampleLines := 1;
      lIndex := 1;
      while lIndex <= lSampleLen do
      begin
        if aText[lIndex] = #10 then
          Inc(lSampleLines)
        else if aText[lIndex] = #13 then
        begin
          Inc(lSampleLines);
          if (lIndex < lSampleLen) and (aText[lIndex + 1] = #10) then
            Inc(lIndex);
        end;
        Inc(lIndex);
      end;
      if lSampleLen < lLength then
        lEstimatedLines := ((Int64(lLength) * lSampleLines) div lSampleLen) + 1
      else
        lEstimatedLines := lSampleLines;
    end;
    if (lEstimatedLines > 0) and (fLines.Capacity < fLines.Count + lEstimatedLines) then
      fLines.Capacity := fLines.Count + lEstimatedLines;

    lLineStart := 1;
    lIndex := 1;
    while lIndex <= lLength do
    begin
      if (aText[lIndex] = #13) or (aText[lIndex] = #10) then
      begin
        lLineText := Copy(aText, lLineStart, lIndex - lLineStart);
        if (aText[lIndex] = #13) and (lIndex < lLength) and (aText[lIndex + 1] = #10) then
          Inc(lIndex);
        Inc(lIndex);
        lLineStart := lIndex;
        ProcessLine(lLineText, lState);
      end else
        Inc(lIndex);
    end;
    if lLineStart <= lLength + 1 then
    begin
      lLineText := Copy(aText, lLineStart, lLength - lLineStart + 1);
      if (lLineText <> '') or (lLength = 0) then
        ProcessLine(lLineText, lState);
    end;
  finally
    lState.PendingComments.Free;
  end;
end;

function TRichIniFile.PlatformNewline: TRichIniString;
begin
  {$IFDEF MSWINDOWS}
  Result := sLineBreak;
  {$ELSE}
  Result := #10;
  {$ENDIF}
end;

function DetectPreambleOffset(const aBytes, aPreamble: TBytes): Integer;
var
  i: Integer;
begin
  Result := 0;
  if Length(aPreamble) = 0 then
    Exit;
  if Length(aBytes) < Length(aPreamble) then
    Exit;
  for i := 0 to Length(aPreamble) - 1 do
    if aBytes[i] <> aPreamble[i] then
      Exit;
  Result := Length(aPreamble);
end;

function TRichIniFile.ResolveLoadEncoding(const aBuffer: TBytes; out aEncoding: TEncoding; out aHasBom: Boolean): TRichIniString;
var
  lBytes: TBytes;
  lDetectedEncoding: TEncoding;
  lBomLen: Integer;
  lPreamble: TBytes;
  lOffset: Integer;


begin
  Result := '';
  lBytes := aBuffer;
  lDetectedEncoding := nil;
  // UTF8 documents without a BOM must not pass through the system ANSI code page.
  lBomLen := TEncoding.GetBufferEncoding(lBytes, lDetectedEncoding{$IFDEF RICHINI_UTF8}, TEncoding.UTF8{$ENDIF});
  aHasBom := lBomLen > 0;
  case fOptions.LoadEncoding of
    eoUTF8:
      begin
        aEncoding := TEncoding.UTF8;
        lPreamble := aEncoding.GetPreamble;
        lOffset := DetectPreambleOffset(lBytes, lPreamble);
        aHasBom := lOffset > 0;
        Result := {$IFDEF RICHINI_UTF8}IniDecode(aEncoding, lBytes, lOffset, Length(lBytes) - lOffset){$ELSE}aEncoding.GetString(lBytes, lOffset, Length(lBytes) - lOffset){$ENDIF};
      end;
    eoAnsi:
      begin
        aEncoding := TEncoding.ANSI;
        aHasBom := False;
        Result := {$IFDEF RICHINI_UTF8}IniDecode(aEncoding, lBytes){$ELSE}aEncoding.GetString(lBytes){$ENDIF};
      end;
    eoCustom:
      begin
        if fOptions.CustomEncoding = nil then
          raise EArgumentException.Create('CustomEncoding must be assigned when LoadEncoding = eoCustom');
        aEncoding := fOptions.CustomEncoding;
        lPreamble := aEncoding.GetPreamble;
        lOffset := DetectPreambleOffset(lBytes, lPreamble);
        aHasBom := lOffset > 0;
        Result := TRichIniString(aEncoding.GetString(lBytes, lOffset, Length(lBytes) - lOffset));
      end;
  else
    begin
      if lDetectedEncoding <> nil then
        aEncoding := lDetectedEncoding
      else
        aEncoding := TEncoding.UTF8;
      lOffset := lBomLen;
      try
        Result := {$IFDEF RICHINI_UTF8}IniDecode(aEncoding, lBytes, lOffset, Length(lBytes) - lOffset){$ELSE}aEncoding.GetString(lBytes, lOffset, Length(lBytes) - lOffset){$ENDIF};
      except
        on E: EEncodingError do
        begin
          aEncoding := TEncoding.ANSI;
          Result := {$IFDEF RICHINI_UTF8}IniDecode(aEncoding, lBytes){$ELSE}aEncoding.GetString(lBytes){$ENDIF};
          aHasBom := False;
        end;
      end;
    end;
  end;
end;

function TRichIniFile.ResolveNewline: TRichIniString;
begin
  case fOptions.NewlineMode of
    nlPreserveInput:
      if fDetectedNewline <> '' then
        Result := fDetectedNewline
      else
        Result := PlatformNewline;
    nlPlatform:
      Result := PlatformNewline;
    nlWindowsCRLF:
      Result := #13#10;
    nlUnixLF:
      Result := #10;
  else
    Result := PlatformNewline;
  end;
end;

function TRichIniFile.RenderLine(const aLine: TRichIniLine): TRichIniString;
var
  lSection: TRichIniSectionLine;
  lKey: TRichIniKeyLine;
  lComment: TRichIniCommentLine;
begin
  if not aLine.Modified then
    Exit(aLine.RawText);
  case aLine.Kind of
    lkSectionHeader:
      begin
        lSection := TRichIniSectionLine(aLine);
        Result := '[' + lSection.Name + ']';
      end;
    lkKeyValue:
      begin
        lKey := TRichIniKeyLine(aLine);
        Result := BuildKeyRawText(lKey.Key, lKey.Value);
        lKey.Delimiter := fOptions.KeyValueDelimiter;
      end;
    lkComment:
      begin
        lComment := TRichIniCommentLine(aLine);
        Result := lComment.LeadingWhitespace + lComment.Prefix;
        if lComment.CommentText <> '' then
          Result := Result + ' ' + lComment.CommentText;
      end;
  else
    Result := aLine.RawText;
  end;
  aLine.RawText := Result;
end;


procedure TRichIniFile.SaveToFile(const aFileName: string);
var
  lTargetFile: string;
  lTempFile: string;
  lEncoding: TEncoding;
  lWriteBom: Boolean;
  lNewline: TRichIniString;
  lBuilder: TRichIniBuilder;
  lLine: TRichIniLine;
  lRendered: TRichIniString;
  i: Integer;
  lContent: TRichIniString;
  lStream: TFileStream;
  lPreamble: TBytes;
  lBytes: TBytes;
  lDirectory: string;
  lCapacity: Integer;


begin
  if aFileName <> '' then
    lTargetFile := aFileName
  else
    lTargetFile := fFileName;
  if lTargetFile = '' then
    raise EInvalidOperation.Create('No target file specified for SaveToFile');

  ResolveSaveEncoding(lEncoding, lWriteBom);
  lNewline := ResolveNewline;

  lCapacity := 0;
  for lLine in fLines do
    Inc(lCapacity, Length(lLine.RawText));
  if fLines.Count > 1 then
    Inc(lCapacity, (fLines.Count - 1) * Length(lNewline));
  if lCapacity < 0 then
    lCapacity := 0;

  lBuilder := TRichIniBuilder.Create(lCapacity);
  try
    for i := 0 to fLines.Count - 1 do
    begin
      lLine := fLines[i];
      lRendered := RenderLine(lLine);
      if i > 0 then
        lBuilder.Append(lNewline);
      lBuilder.Append(lRendered);
      lLine.Modified := False;
    end;
    lContent := lBuilder.ToString;
  finally
    lBuilder.Free;
  end;

  lDirectory := TPath.GetDirectoryName(lTargetFile);
  if lDirectory <> '' then
    ForceDirectories(lDirectory);
  lTempFile := CreateTempFileName(lTargetFile);

  try
    lStream := TFileStream.Create(lTempFile, fmCreate or fmShareExclusive);
    try
      if lWriteBom then
      begin
        lPreamble := lEncoding.GetPreamble;
        if Length(lPreamble) > 0 then
          lStream.WriteBuffer(lPreamble[0], Length(lPreamble));
      end;
      if lContent <> '' then
      begin
        {$IFDEF RICHINI_UTF8}
        if (lEncoding = TEncoding.UTF8) and (fOptions.SaveEncoding <> eoCustom) then
          lStream.WriteBuffer(PAnsiChar(lContent)^, Length(lContent))
        else
        begin
          lBytes := lEncoding.GetBytes(string(lContent));
          if Length(lBytes) > 0 then
            lStream.WriteBuffer(lBytes[0], Length(lBytes));
        end;
        {$ELSE}
        lBytes := lEncoding.GetBytes(lContent);
        if Length(lBytes) > 0 then
          lStream.WriteBuffer(lBytes[0], Length(lBytes));
        {$ENDIF}
      end;
    finally
      lStream.Free;
    end;

    ReplaceFileAtomic(lTempFile, lTargetFile);
  except
    on E: Exception do
    begin
      if TFile.Exists(lTempFile) then
        TFile.Delete(lTempFile);
      raise;
    end;
  end;

  fFileName := lTargetFile;
  fSourceEncoding := lEncoding;
  fSourceBom := lWriteBom;
  fDetectedNewline := lNewline;
  fHasSource := True;
  fDirty := False;
end;

function TRichIniFile.AppendKey(const aSection, aKey, aValue: TRichIniString): Integer;
var
  lBlock: TRichIniSectionBlock;
  lInsertIndex: Integer;
  lLine: TRichIniKeyLine;
begin
  Result := KeyCount(aSection, aKey);
  lBlock := EnsureSectionBlockForWrite(aSection);
  lInsertIndex := GetSectionInsertIndex(lBlock);

  lLine := TRichIniKeyLine.Create(aKey, aValue, KeyToken(aKey), fOptions.KeyValueDelimiter, BuildKeyRawText(aKey, aValue));
  lLine.SectionBlock := lBlock;
  lLine.Modified := True;

  if (lInsertIndex < 0) or (lInsertIndex > fLines.Count) then
    raise EInvalidOperation.CreateFmt('AppendKey insert index out of range (%d) for count %d', [lInsertIndex, fLines.Count]);
  fLines.Insert(lInsertIndex, lLine);
  lBlock.AddKeyLine(lLine);
  fDirty := True;
end;

procedure TRichIniFile.ConsolidateAll;
var
  lSeen: TDictionary<TRichIniString, Boolean>;
  lNames: TList<TRichIniString>;
  lBlock: TRichIniSectionBlock;
  lName: TRichIniString;
begin
  lSeen := TDictionary<TRichIniString, Boolean>.Create(CurrentComparer);
  lNames := TList<TRichIniString>.Create;
  try
    for lBlock in fSections do
    begin
      if lBlock = nil then
        Continue;
      if lSeen.ContainsKey(lBlock.LookupName) then
        Continue;
      lSeen.Add(lBlock.LookupName, True);
      lNames.Add(lBlock.Name);
    end;
    for lName in lNames do
      ConsolidateSection(lName);
  finally
    lNames.Free;
    lSeen.Free;
  end;
end;

procedure TRichIniFile.ConsolidateSection(const aSection: TRichIniString);
var
  lList: TObjectList<TRichIniSectionBlock>;
  lPrimary: TRichIniSectionBlock;
  lOrder: TList<TRichIniString>;
  lIndexMap: TDictionary<TRichIniString, Integer>;
  lKeyText: TDictionary<TRichIniString, TRichIniString>;
  lValueMap: TDictionary<TRichIniString, TRichIniString>;
  lCommentMap: TDictionary<TRichIniString, TRichIniString>;
  lLine: TRichIniLine;
  lToken: TRichIniString;
  lIdx: Integer;
  i: Integer;
  lBlocksToRemove: TList<TRichIniSectionBlock>;
  lBlock: TRichIniSectionBlock;
  lBlockList: TObjectList<TRichIniSectionBlock>;
  lComment: TRichIniString;
  lSectionToken: TRichIniString;
begin
  lList := FindSectionList(aSection);
  if (lList = nil) or (lList.Count = 0) then
    Exit;

  lPrimary := lList[0];

  lOrder := TList<TRichIniString>.Create;
  lIndexMap := TDictionary<TRichIniString, Integer>.Create(CurrentComparer);
  lKeyText := TDictionary<TRichIniString, TRichIniString>.Create(CurrentComparer);
  lValueMap := TDictionary<TRichIniString, TRichIniString>.Create(CurrentComparer);
  lCommentMap := TDictionary<TRichIniString, TRichIniString>.Create(CurrentComparer);
  lBlocksToRemove := TList<TRichIniSectionBlock>.Create;
  lSectionToken := SectionToken(aSection);
  try
    for i := 1 to lList.Count - 1 do
      lBlocksToRemove.Add(lList[i]);

    for lLine in fLines do
    begin
      if (lLine.Kind <> lkKeyValue) or (lLine.SectionBlock = nil) or
         (not TokensEqual(lLine.SectionBlock.LookupName, lSectionToken)) then
        Continue;
      lToken := TRichIniKeyLine(lLine).LookupKey;
      if lIndexMap.TryGetValue(lToken, lIdx) then
      begin
        lOrder.Delete(lIdx);
        lIndexMap.Remove(lToken);
        for i := lIdx to lOrder.Count - 1 do
          lIndexMap.AddOrSetValue(lOrder[i], i);
      end;
      lOrder.Add(lToken);
      lIndexMap.AddOrSetValue(lToken, lOrder.Count - 1);
      lKeyText.AddOrSetValue(lToken, TRichIniKeyLine(lLine).Key);
      lValueMap.AddOrSetValue(lToken, TRichIniKeyLine(lLine).Value);
      lCommentMap.AddOrSetValue(lToken, CollectCommentText(lLine));
    end;
    i := fLines.Count - 1;
    while i >= 0 do
    begin
      if i >= fLines.Count then
      begin
        Dec(i);
        Continue;
      end;
      lLine := fLines[i];
      if (lLine.SectionBlock = nil) or (not TokensEqual(lLine.SectionBlock.LookupName, lSectionToken)) then
      begin
        Dec(i);
        Continue;
      end;
      if (lLine.Kind = lkSectionHeader) and (lLine.SectionBlock = lPrimary) then
      begin
        Dec(i);
        Continue;
      end;
      if lLine.Kind = lkKeyValue then
        DeleteKeyLineInstance(TRichIniKeyLine(lLine))
      else
      begin
        if lLine.Kind = lkComment then
        begin
          if (lLine is TRichIniCommentLine) and (TRichIniCommentLine(lLine).OwnerLine <> nil) then
            TRichIniCommentLine(lLine).OwnerLine.DetachComment(TRichIniCommentLine(lLine));
        end else
          lLine.ClearOwnedComments;
        fLines.Delete(i);
        fDirty := True;
      end;
      Dec(i);
    end;
    for lBlock in lBlocksToRemove do
    begin
      if fSectionMap.TryGetValue(lBlock.LookupName, lBlockList) then
      begin
        lBlockList.Remove(lBlock);
        if lBlockList.Count = 0 then
          fSectionMap.Remove(lBlock.LookupName);
      end;
      fSections.Remove(lBlock);
    end;

    for lToken in lOrder do
    begin
      AppendKey(aSection, lKeyText[lToken], lValueMap[lToken]);
      lComment := lCommentMap[lToken];
      if lComment <> '' then
        WriteComment(aSection, lKeyText[lToken], lComment);
    end;
  finally
    lBlocksToRemove.Free;
    lCommentMap.Free;
    lValueMap.Free;
    lKeyText.Free;
    lIndexMap.Free;
    lOrder.Free;
  end;
end;

procedure TRichIniFile.DeleteKey(const aSection, aKey: TRichIniString);
var
  lLine: TRichIniLine;
  lMatches: TList<TRichIniKeyLine>;
  lSectionToken: TRichIniString;
  lKeyToken: TRichIniString;
begin
  lMatches := TList<TRichIniKeyLine>.Create;
  try
    lSectionToken := SectionToken(aSection);
    lKeyToken := KeyToken(aKey);
    for lLine in fLines do
    begin
      if (lLine.Kind = lkKeyValue) and (lLine.SectionBlock <> nil) and
         TokensEqual(lLine.SectionBlock.LookupName, lSectionToken) and
         TokensEqual(TRichIniKeyLine(lLine).LookupKey, lKeyToken) then
        lMatches.Add(TRichIniKeyLine(lLine));
    end;
    for lLine in lMatches do
      DeleteKeyLineInstance(TRichIniKeyLine(lLine));
  finally
    lMatches.Free;
  end;
end;

procedure TRichIniFile.DeleteKey(const aSection, aKey: TRichIniString; aKeyIndex: Integer);
var
  lLine: TRichIniKeyLine;
begin
  lLine := FindKeyLineByIndex(aSection, aKey, aKeyIndex);
  if lLine = nil then
    raise EArgumentOutOfRangeException.CreateFmt('Key "%s" in section "%s" does not have occurrence index %d.',
      [aKey, aSection, aKeyIndex]);
  DeleteKeyLineInstance(lLine);
end;

procedure TRichIniFile.EraseSection(const aSection: TRichIniString);
var
  lBlocks: TList<TRichIniSectionBlock>;
  lBlock: TRichIniSectionBlock;
  lLine: TRichIniLine;
  lList: TObjectList<TRichIniSectionBlock>;
  lLinesToRemove: TList<TRichIniLine>;
  lSectionToken: TRichIniString;
begin
  lBlocks := TList<TRichIniSectionBlock>.Create;
  try
    lSectionToken := SectionToken(aSection);
    for lBlock in fSections do
      if (lBlock <> nil) and TokensEqual(lBlock.LookupName, lSectionToken) then
        lBlocks.Add(lBlock);

    if lBlocks.Count = 0 then
      Exit;

    for lBlock in lBlocks do
    begin
      lLinesToRemove := TList<TRichIniLine>.Create;
      try
        for lLine in fLines do
          if lLine.SectionBlock = lBlock then
            lLinesToRemove.Add(lLine);

        for lLine in lLinesToRemove do
        begin
          if fLines.IndexOf(lLine) < 0 then
            Continue;
          if lLine.Kind = lkKeyValue then
            DeleteKeyLineInstance(TRichIniKeyLine(lLine))
          else
          begin
            fLines.Remove(lLine);
            fDirty := True;
          end;
        end;
      finally
        lLinesToRemove.Free;
      end;

      if fSectionMap.TryGetValue(lBlock.LookupName, lList) then
      begin
        lList.Remove(lBlock);
        if lList.Count = 0 then
          fSectionMap.Remove(lBlock.LookupName);
      end;
      fSections.Remove(lBlock);
      fDirty := True;
    end;

    if (aSection = '') and (FindSectionList('') = nil) then
      CreateGlobalSection;
  finally
    lBlocks.Free;
  end;
end;

function TRichIniFile.KeyCount(const aSection, aKey: TRichIniString): Integer;
var
  lList: TObjectList<TRichIniSectionBlock>;
  lBlock: TRichIniSectionBlock;
  lKeyList: TObjectList<TRichIniKeyLine>;
  lKeyToken: TRichIniString;
begin
  Result := 0;
  lList := FindSectionList(aSection);
  if lList = nil then
    Exit;
  lKeyToken := KeyToken(aKey);
  for lBlock in lList do
  begin
    lKeyList := lBlock.KeyLines(lKeyToken);
    if lKeyList <> nil then
      Inc(Result, lKeyList.Count);
  end;
end;

function TRichIniFile.LastKeyIndex(const aSection, aKey: TRichIniString): Integer;
begin
  Result := KeyCount(aSection, aKey) - 1;
end;

procedure TRichIniFile.PurgeComments;
var
  i: Integer;
  lLine: TRichIniLine;
  lComment: TRichIniCommentLine;
  lRemoved: Boolean;
begin
  lRemoved := False;
  for i := fLines.Count - 1 downto 0 do
  begin
    lLine := fLines[i];
    if lLine.Kind = lkComment then
    begin
      if lLine is TRichIniCommentLine then
      begin
        lComment := TRichIniCommentLine(lLine);
        if lComment.OwnerLine <> nil then
          lComment.OwnerLine.DetachComment(lComment);
      end;
      fLines.Delete(i);
      lRemoved := True;
    end else
      lLine.ClearOwnedComments;
  end;
  if lRemoved then
    fDirty := True;
end;

function TRichIniFile.ReadBool(const aSection, aKey: TRichIniString; aDefault: Boolean): Boolean;
var
  lLine: TRichIniKeyLine;
begin
  lLine := FindLastKeyLine(aSection, aKey);
  if lLine = nil then
    Exit(aDefault);
  Result := IsTrueBooleanValue(lLine.Value);
end;

function TRichIniFile.ReadComment(const aSection, aKey: TRichIniString): TRichIniString;
var
  lLine: TRichIniKeyLine;
begin
  lLine := FindLastKeyLine(aSection, aKey);
  if lLine = nil then
    Exit('');
  Result := CollectCommentText(lLine);
end;

function TRichIniFile.ReadInteger(const aSection, aKey: TRichIniString; aDefault: Integer): Integer;
var
  lLine: TRichIniKeyLine;
begin
  lLine := FindLastKeyLine(aSection, aKey);
  if lLine = nil then
    Exit(aDefault);
  Result := {$IFDEF RICHINI_UTF8}IniStrToIntDef{$ELSE}StrToIntDef{$ENDIF}(lLine.Value, aDefault);
end;

procedure TRichIniFile.ReadSection(const aSection: TRichIniString; aStrings: TRichIniStrings);
var
  lOrder: TList<TRichIniString>;
  lIndexMap: TDictionary<TRichIniString, Integer>;
  lKeyLine: TRichIniKeyLine;
  lMap: TDictionary<TRichIniString, TRichIniKeyLine>;
  lIdx: Integer;
  lToken: TRichIniString;
  j: Integer;
  lSectionList: TObjectList<TRichIniSectionBlock>;
  lBlock: TRichIniSectionBlock;
begin
  if aStrings = nil then
    Exit;
  {$IFNDEF RICHINI_UTF8}
  aStrings.BeginUpdate;
  try
  {$ENDIF}
    aStrings.Clear;
    if fLines = nil then
      Exit;
    lOrder := TList<TRichIniString>.Create;
    lIndexMap := TDictionary<TRichIniString, Integer>.Create(CurrentComparer);
    lMap := TDictionary<TRichIniString, TRichIniKeyLine>.Create(CurrentComparer);
    try
      lSectionList := FindSectionList(aSection);
      if lSectionList <> nil then
        for lBlock in lSectionList do
          for lKeyLine in lBlock.KeyOrder do
          begin
            lToken := lKeyLine.LookupKey;
            if lIndexMap.TryGetValue(lToken, lIdx) then
            begin
              lOrder.Delete(lIdx);
              lIndexMap.Remove(lToken);
              for j := lIdx to lOrder.Count - 1 do
                lIndexMap.AddOrSetValue(lOrder[j], j);
            end;
            lOrder.Add(lToken);
            lIndexMap.AddOrSetValue(lToken, lOrder.Count - 1);
            lMap.AddOrSetValue(lToken, lKeyLine);
          end;
      for lToken in lOrder do
        aStrings.Add(lMap[lToken].Key);
    finally
      lMap.Free;
      lIndexMap.Free;
      lOrder.Free;
    end;
  {$IFNDEF RICHINI_UTF8}
  finally
    aStrings.EndUpdate;
  end;
  {$ENDIF}
end;

procedure TRichIniFile.ReadSections(aStrings: TRichIniStrings);
var
  lSeen: TDictionary<TRichIniString, Boolean>;
  lBlock: TRichIniSectionBlock;
begin
  if aStrings = nil then
    Exit;
  {$IFNDEF RICHINI_UTF8}
  aStrings.BeginUpdate;
  try
  {$ENDIF}
    aStrings.Clear;
    lSeen := TDictionary<TRichIniString, Boolean>.Create(CurrentComparer);
    try
      for lBlock in fSections do
      begin
        if (lBlock = nil) or (lBlock.Name = '') then
          Continue;
        if lSeen.ContainsKey(lBlock.LookupName) then
          Continue;
        aStrings.Add(lBlock.Name);
        lSeen.Add(lBlock.LookupName, True);
      end;
    finally
      lSeen.Free;
    end;
  {$IFNDEF RICHINI_UTF8}
  finally
    aStrings.EndUpdate;
  end;
  {$ENDIF}
end;

procedure TRichIniFile.ReadSectionValues(const aSection: TRichIniString; aStrings: TRichIniStrings);
var
  lOrder: TList<TRichIniString>;
  lIndexMap: TDictionary<TRichIniString, Integer>;
  lKeyLine: TRichIniKeyLine;
  lMap: TDictionary<TRichIniString, TRichIniKeyLine>;
  lIdx: Integer;
  lToken: TRichIniString;
  j: Integer;
  lSectionList: TObjectList<TRichIniSectionBlock>;
  lBlock: TRichIniSectionBlock;
begin
  if aStrings = nil then
    Exit;
  {$IFNDEF RICHINI_UTF8}
  aStrings.BeginUpdate;
  try
  {$ENDIF}
    aStrings.Clear;
    if fLines = nil then
      Exit;
    lOrder := TList<TRichIniString>.Create;
    lIndexMap := TDictionary<TRichIniString, Integer>.Create(CurrentComparer);
    lMap := TDictionary<TRichIniString, TRichIniKeyLine>.Create(CurrentComparer);
    try
      lSectionList := FindSectionList(aSection);
      if lSectionList <> nil then
        for lBlock in lSectionList do
          for lKeyLine in lBlock.KeyOrder do
          begin
            lToken := lKeyLine.LookupKey;
            if lIndexMap.TryGetValue(lToken, lIdx) then
            begin
              lOrder.Delete(lIdx);
              lIndexMap.Remove(lToken);
              for j := lIdx to lOrder.Count - 1 do
                lIndexMap.AddOrSetValue(lOrder[j], j);
            end;
            lOrder.Add(lToken);
            lIndexMap.AddOrSetValue(lToken, lOrder.Count - 1);
            lMap.AddOrSetValue(lToken, lKeyLine);
          end;
      for lToken in lOrder do
      begin
        lKeyLine := lMap[lToken];
        aStrings.Add(lKeyLine.Key + '=' + lKeyLine.Value);
      end;
    finally
      lMap.Free;
      lIndexMap.Free;
      lOrder.Free;
    end;
  {$IFNDEF RICHINI_UTF8}
  finally
    aStrings.EndUpdate;
  end;
  {$ENDIF}
end;

function TRichIniFile.ReadString(const aSection, aKey, aDefault: TRichIniString): TRichIniString;
var
  lLine: TRichIniKeyLine;
begin
  lLine := FindLastKeyLine(aSection, aKey);
  if lLine = nil then
    Result := aDefault
  else
    Result := lLine.Value;
end;

procedure TRichIniFile.ReadAllKeyValues(const aSection, aKey: TRichIniString; out aValues: TArray<TRichIniString>);
var
  lValues: TList<TRichIniString>;
  lSectionList: TObjectList<TRichIniSectionBlock>;
  lBlock: TRichIniSectionBlock;
  lKeyLine: TRichIniKeyLine;
  lKeyToken: TRichIniString;
begin
  lValues := TList<TRichIniString>.Create;
  try
    lSectionList := FindSectionList(aSection);
    lKeyToken := KeyToken(aKey);
    if lSectionList <> nil then
      for lBlock in lSectionList do
        for lKeyLine in lBlock.KeyOrder do
          if TokensEqual(lKeyLine.LookupKey, lKeyToken) then
            lValues.Add(lKeyLine.Value);
    aValues := lValues.ToArray;
  finally
    lValues.Free;
  end;
end;

function TRichIniFile.ReadMultilineString(const aSection, aKey, aDefault: TRichIniString): TRichIniString;
var
  lLine: TRichIniKeyLine;
begin
  lLine := FindLastKeyLine(aSection, aKey);
  if lLine = nil then
    Exit(aDefault);
  Result := DecodeMultilineValue(lLine.Value);
end;

procedure TRichIniFile.WriteBool(const aSection, aKey: TRichIniString; aValue: Boolean);
begin
  if aValue then
    WriteString(aSection, aKey, fOptions.BooleanTrueValue)
  else
    WriteString(aSection, aKey, fOptions.BooleanFalseValue);
end;

procedure TRichIniFile.UpdateFile;
begin
  SaveToFile;
end;

function NormalizeComment(const aValue: TRichIniString): TRichIniString;
begin
  Result := {$IFDEF RICHINI_UTF8}IniReplace{$ELSE}StringReplace{$ENDIF}(aValue, sLineBreak, #10, [rfReplaceAll]);
  Result := {$IFDEF RICHINI_UTF8}IniReplace{$ELSE}StringReplace{$ENDIF}(Result, #13, #10, [rfReplaceAll]);
end;

procedure TRichIniFile.WriteComment(const aSection, aKey, aComment: TRichIniString);
var
  lLine: TRichIniKeyLine;
  lPrefix: TRichIniString;
  lLines: TArray<TRichIniString>;
  lCommentLine: TRichIniCommentLine;
  lRaw: TRichIniString;
  lInsertIndex: Integer;
  lLineText: TRichIniString;
  lIdx: Integer;
  lExisting: TRichIniString;
  lExistingNorm: TRichIniString;
  lNewNorm: TRichIniString;


begin
  lLine := FindLastKeyLine(aSection, aKey);
  if lLine = nil then
    raise EArgumentException.CreateFmt('Key "%s" not found in section "%s".', [aKey, aSection]);

  lExisting := CollectCommentText(lLine);
  lExistingNorm := NormalizeComment(lExisting);
  lNewNorm := NormalizeComment(aComment);

  if (aComment <> '') and (lExistingNorm = lNewNorm) then
    Exit;

  RemoveOwnedComments(lLine);

  if aComment = '' then
    Exit;

  lPrefix := DefaultCommentPrefix;
  lLines := SplitToLines(aComment);

  lInsertIndex := fLines.IndexOf(lLine);
  if lInsertIndex < 0 then
    lInsertIndex := fLines.Count;

  if fOptions.CommentOwnership = coAttachToPrev then
    Inc(lInsertIndex);

  for lIdx := 0 to Length(lLines) - 1 do
  begin
    lLineText := lLines[lIdx];
    lRaw := lPrefix;
    if lLineText <> '' then
      lRaw := lRaw + ' ' + lLineText;
    lCommentLine := TRichIniCommentLine.Create(lRaw, '', lPrefix, lLineText);
    lCommentLine.SectionBlock := lLine.SectionBlock;
    lCommentLine.Modified := True;
    lLine.AttachComment(lCommentLine);
    if (lInsertIndex < 0) or (lInsertIndex > fLines.Count) then
      raise EInvalidOperation.CreateFmt('WriteComment insert index out of range (%d) for count %d', [lInsertIndex, fLines.Count]);
    fLines.Insert(lInsertIndex, lCommentLine);
    Inc(lInsertIndex);
  end;
  fDirty := True;
end;

procedure TRichIniFile.WriteInteger(const aSection, aKey: TRichIniString; aValue: Integer);
begin
  WriteString(aSection, aKey, {$IFDEF RICHINI_UTF8}IniIntToStr{$ELSE}IntToStr{$ENDIF}(aValue));
end;

procedure TRichIniFile.WriteMultilineString(const aSection, aKey, aValueWithLineBreaks: TRichIniString);
begin
  WriteString(aSection, aKey, EncodeMultilineValue(aValueWithLineBreaks));
end;

procedure TRichIniFile.WriteString(const aSection, aKey, aValue: TRichIniString);
var
  lLine: TRichIniKeyLine;
  lRaw: TRichIniString;
begin
  lLine := FindLastKeyLine(aSection, aKey);
  if lLine <> nil then
  begin
    lRaw := BuildKeyRawText(lLine.Key, aValue);
    if (lLine.Value = aValue) and (lLine.Delimiter = fOptions.KeyValueDelimiter) and
       (lLine.RawText = lRaw) then
      Exit;
    lLine.Value := aValue;
    lLine.Delimiter := fOptions.KeyValueDelimiter;
    lLine.RawText := lRaw;
    lLine.LookupKey := KeyToken(lLine.Key);
    lLine.Modified := True;
    fDirty := True;
    Exit;
  end;

  AppendKey(aSection, aKey, aValue);
end;

procedure TRichIniFile.WriteString(const aSection, aKey, aValue: TRichIniString; aKeyIndex: Integer);
var
  lLine: TRichIniKeyLine;
  lRaw: TRichIniString;
begin
  lLine := FindKeyLineByIndex(aSection, aKey, aKeyIndex);
  if lLine = nil then
    raise EArgumentOutOfRangeException.CreateFmt('Key "%s" in section "%s" does not have occurrence index %d.',
      [aKey, aSection, aKeyIndex]);
  lRaw := BuildKeyRawText(lLine.Key, aValue);
  if (lLine.Value = aValue) and (lLine.Delimiter = fOptions.KeyValueDelimiter) and
     (lLine.RawText = lRaw) then
    Exit;
  lLine.Value := aValue;
  lLine.Delimiter := fOptions.KeyValueDelimiter;
  lLine.RawText := lRaw;
  lLine.LookupKey := KeyToken(lLine.Key);
  lLine.Modified := True;
  fDirty := True;
end;

end.
