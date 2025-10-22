unit MaxLogic.RichIniFile;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils;

type
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
    CommentPrefixes: TArray<string>;
    CommentOwnership: TCommentOwnership;
    KeyValueDelimiter: Char;
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
    fRawText: string;
    fModified: Boolean;
    fSectionBlock: TRichIniSectionBlock;
    fOwnedComments: TObjectList<TRichIniCommentLine>;
    function GetOwnedComments: TObjectList<TRichIniCommentLine>;
  public
    constructor Create(const aKind: TRichIniLineKind; const aRawText: string);
    destructor Destroy; override;
    procedure AttachComment(aComment: TRichIniCommentLine);
    procedure DetachComment(aComment: TRichIniCommentLine);
    procedure ClearOwnedComments;
    function OwnedCommentCount: Integer;
    property Kind: TRichIniLineKind read fKind;
    property RawText: string read fRawText write fRawText;
    property Modified: Boolean read fModified write fModified;
    property SectionBlock: TRichIniSectionBlock read fSectionBlock write fSectionBlock;
    property OwnedComments: TObjectList<TRichIniCommentLine> read GetOwnedComments;
  end;

  TRichIniCommentLine = class(TRichIniLine)
  private
    fLeadingWhitespace: string;
    fPrefix: string;
    fText: string;
    fOwnerLine: TRichIniLine;
  public
    constructor Create(const aRawText, aLeadingWhitespace, aPrefix, aCommentText: string);
    property LeadingWhitespace: string read fLeadingWhitespace write fLeadingWhitespace;
    property Prefix: string read fPrefix write fPrefix;
    property CommentText: string read fText write fText;
    property OwnerLine: TRichIniLine read fOwnerLine write fOwnerLine;
  end;

  TRichIniSectionLine = class(TRichIniLine)
  private
    fName: string;
  public
    constructor Create(const aName, aRawText: string);
    property Name: string read fName write fName;
  end;

  TRichIniKeyLine = class(TRichIniLine)
  private
    fKey: string;
    fValue: string;
    fDelimiter: Char;
    fLookupKey: string;
  public
    constructor Create(const aKey, aValue, aLookupKey: string; const aDelimiter: Char; const aRawText: string);
    property Key: string read fKey write fKey;
    property Value: string read fValue write fValue;
    property Delimiter: Char read fDelimiter write fDelimiter;
    property LookupKey: string read fLookupKey write fLookupKey;
  end;

  TRichIniSectionBlock = class
  private
    fName: string;
    fLookupName: string;
    fNoHeader: Boolean;
    fHeaderLine: TRichIniSectionLine;
    fKeyMap: TObjectDictionary<string, TObjectList<TRichIniKeyLine>>;
    fKeyOrder: TList<TRichIniKeyLine>;
  public
    constructor Create(const aName, aLookupName: string; const aNoHeader: Boolean);
    destructor Destroy; override;
    procedure AddKeyLine(aLine: TRichIniKeyLine);
    procedure RemoveKeyLine(aLine: TRichIniKeyLine);
    function KeyLines(const aToken: string): TObjectList<TRichIniKeyLine>;
    property KeyOrder: TList<TRichIniKeyLine> read fKeyOrder;
    property Name: string read fName write fName;
    property LookupName: string read fLookupName write fLookupName;
    property NoHeader: Boolean read fNoHeader write fNoHeader;
    property HeaderLine: TRichIniSectionLine read fHeaderLine write fHeaderLine;
  end;

type
  TRichIniFile = class
  private
    fFileName: string;
    fOptions: TRichIniOptions;
    fLines: TObjectList<TRichIniLine>;
    fSections: TObjectList<TRichIniSectionBlock>;
    fSectionMap: TObjectDictionary<string, TObjectList<TRichIniSectionBlock>>;
    fDirty: Boolean;
    fDetectedNewline: string;
    fSourceEncoding: TEncoding;
    fSourceBom: Boolean;
    fHasSource: Boolean;
    procedure EnsureOptionsDefaults;
    procedure ClearDocument;
    procedure CreateGlobalSection;
    function CreateSectionBlock(const aSection: string; const aNoHeader: Boolean): TRichIniSectionBlock;
    function GetOrCreateSectionList(const aToken: string): TObjectList<TRichIniSectionBlock>;
    function SectionToken(const aSection: string): string; inline;
    function KeyToken(const aKey: string): string; inline;
    procedure AddLineToDocument(aLine: TRichIniLine);
    procedure AttachPendingComments(const aOwner: TRichIniLine; const aPending: TList<TRichIniCommentLine>);
    procedure ParseText(const aText: string);
    function DetectNewline(const aText: string): string;
    function ResolveLoadEncoding(const aBuffer: TBytes; out aEncoding: TEncoding; out aHasBom: Boolean): string;
    function PlatformNewline: string;
    function ResolveNewline: string;
    function FindSectionList(const aSection: string): TObjectList<TRichIniSectionBlock>;
    function FindLastKeyLine(const aSection, aKey: string): TRichIniKeyLine;
    function BuildKeyRawText(const aKey, aValue: string): string;
    function EnsureSectionBlockForWrite(const aSection: string): TRichIniSectionBlock;
    function GetSectionInsertIndex(const aBlock: TRichIniSectionBlock): Integer;
    function FindKeyLineByIndex(const aSection, aKey: string; aKeyIndex: Integer): TRichIniKeyLine;
    procedure RemoveOwnedComments(aLine: TRichIniLine);
    procedure DeleteKeyLineInstance(aLine: TRichIniKeyLine);
    function DefaultCommentPrefix: string;
    function SplitToLines(const aText: string): TArray<string>;
    function CollectCommentText(const aLine: TRichIniLine): string;
    function EncodeMultilineValue(const aValue: string): string;
    function DecodeMultilineValue(const aValue: string): string;
    procedure ResolveSaveEncoding(out aEncoding: TEncoding; out aWriteBom: Boolean);
    function CreateTempFileName(const aTarget: string): string;
    procedure ReplaceFileAtomic(const aSource, aDestination: string);
    class procedure RaiseNotImplemented(const aMethod: string); static;
  protected
    function GetLineCount: Integer;
    function GetSectionBlockCount: Integer;
  public
    constructor Create(const aFileName: string; const aOptions: TRichIniOptions);
    constructor CreateFromStrings(const aLines: TArray<string>; const aOptions: TRichIniOptions);
    destructor Destroy; override;

    procedure LoadFromFile(const aFileName: string);
    procedure SaveToFile(const aFileName: string = '');

    function ReadString(const aSection, aKey, aDefault: string): string;
    procedure WriteString(const aSection, aKey, aValue: string); overload;
    procedure WriteString(const aSection, aKey, aValue: string; aKeyIndex: Integer); overload;
    function ReadInteger(const aSection, aKey: string; aDefault: Integer): Integer;
    procedure WriteInteger(const aSection, aKey: string; aValue: Integer);
    function ReadBool(const aSection, aKey: string; aDefault: Boolean): Boolean;
    procedure WriteBool(const aSection, aKey: string; aValue: Boolean);
    procedure DeleteKey(const aSection, aKey: string); overload;
    procedure DeleteKey(const aSection, aKey: string; aKeyIndex: Integer); overload;
    procedure EraseSection(const aSection: string);
    procedure ReadSection(const aSection: string; aStrings: TStrings);
    procedure ReadSections(aStrings: TStrings);
    procedure ReadSectionValues(const aSection: string; aStrings: TStrings);

    function AppendKey(const aSection, aKey, aValue: string): Integer;
    function LastKeyIndex(const aSection, aKey: string): Integer;
    function KeyCount(const aSection, aKey: string): Integer;
    procedure ReadAllKeyValues(const aSection, aKey: string; out aValues: TArray<string>);

    function ReadComment(const aSection, aKey: string): string;
    procedure WriteComment(const aSection, aKey, aComment: string);
    procedure PurgeComments;

    procedure ConsolidateSection(const aSection: string);
    procedure ConsolidateAll;

    procedure WriteMultilineString(const aSection, aKey, aValueWithLineBreaks: string);
    function ReadMultilineString(const aSection, aKey, aDefault: string): string;

    property FileName: string read fFileName;
    property Options: TRichIniOptions read fOptions;
    property Dirty: Boolean read fDirty;
    property LineCount: Integer read GetLineCount;
    property SectionBlockCount: Integer read GetSectionBlockCount;
  end;

implementation

uses
  System.IOUtils
  {$IFDEF MSWINDOWS}, Winapi.Windows{$ENDIF};

const
  cDefaultCommentPrefixes: array[0..2] of string = (';', '#', '//');

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
end;

{ TRichIniLine }

constructor TRichIniLine.Create(const aKind: TRichIniLineKind; const aRawText: string);
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

constructor TRichIniCommentLine.Create(const aRawText, aLeadingWhitespace, aPrefix, aCommentText: string);
begin
  inherited Create(lkComment, aRawText);
  fLeadingWhitespace := aLeadingWhitespace;
  fPrefix := aPrefix;
  fText := aCommentText;
end;

{ TRichIniSectionLine }

constructor TRichIniSectionLine.Create(const aName, aRawText: string);
begin
  inherited Create(lkSectionHeader, aRawText);
  fName := aName;
end;

{ TRichIniKeyLine }

constructor TRichIniKeyLine.Create(const aKey, aValue, aLookupKey: string; const aDelimiter: Char;
  const aRawText: string);
begin
  inherited Create(lkKeyValue, aRawText);
  fKey := aKey;
  fValue := aValue;
  fLookupKey := aLookupKey;
  fDelimiter := aDelimiter;
end;

{ TRichIniSectionBlock }

constructor TRichIniSectionBlock.Create(const aName, aLookupName: string; const aNoHeader: Boolean);
begin
  inherited Create;
  fName := aName;
  fLookupName := aLookupName;
  fNoHeader := aNoHeader;
  fKeyMap := TObjectDictionary<string, TObjectList<TRichIniKeyLine>>.Create([doOwnsValues]);
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

function TRichIniSectionBlock.KeyLines(const aToken: string): TObjectList<TRichIniKeyLine>;
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
  inherited Create;
  fFileName := aFileName;
  fOptions := aOptions;
  EnsureOptionsDefaults;
  fLines := TObjectList<TRichIniLine>.Create(True);
  fSections := TObjectList<TRichIniSectionBlock>.Create(True);
  fSectionMap := TObjectDictionary<string, TObjectList<TRichIniSectionBlock>>.Create([doOwnsValues]);
  ClearDocument;
end;

constructor TRichIniFile.CreateFromStrings(const aLines: TArray<string>; const aOptions: TRichIniOptions);
var
  lText: string;
  i: Integer;
begin
  inherited Create;
  fFileName := '';
  fOptions := aOptions;
  EnsureOptionsDefaults;
  fLines := TObjectList<TRichIniLine>.Create(True);
  fSections := TObjectList<TRichIniSectionBlock>.Create(True);
  fSectionMap := TObjectDictionary<string, TObjectList<TRichIniSectionBlock>>.Create([doOwnsValues]);
  ClearDocument;
  fHasSource := False;
  fSourceEncoding := TEncoding.UTF8;
  fSourceBom := False;
  fDetectedNewline := '';
  lText := '';
  for i := 0 to High(aLines) do
  begin
    if i > 0 then
      lText := lText + sLineBreak;
    lText := lText + aLines[i];
  end;
  ParseText(lText);
  fDetectedNewline := DetectNewline(lText);
  if fDetectedNewline = '' then
    fDetectedNewline := PlatformNewline;
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

function TRichIniFile.CreateSectionBlock(const aSection: string; const aNoHeader: Boolean): TRichIniSectionBlock;
var
  lToken: string;
begin
  lToken := SectionToken(aSection);
  Result := TRichIniSectionBlock.Create(aSection, lToken, aNoHeader);
  fSections.Add(Result);
  GetOrCreateSectionList(lToken).Add(Result);
end;

function TRichIniFile.DetectNewline(const aText: string): string;
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
end;

function TRichIniFile.SectionToken(const aSection: string): string;
begin
  if fOptions.CaseSensitivity = csCaseInsensitive then
    Result := AnsiLowerCase(aSection)
  else
    Result := aSection;
end;

function TRichIniFile.KeyToken(const aKey: string): string;
begin
  if fOptions.CaseSensitivity = csCaseInsensitive then
    Result := AnsiLowerCase(aKey)
  else
    Result := aKey;
end;

function TRichIniFile.GetLineCount: Integer;
begin
  if fLines <> nil then
    Result := fLines.Count
  else
    Result := 0;
end;

function TRichIniFile.GetOrCreateSectionList(const aToken: string): TObjectList<TRichIniSectionBlock>;
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

function TRichIniFile.FindSectionList(const aSection: string): TObjectList<TRichIniSectionBlock>;
var
  lToken: string;
begin
  lToken := SectionToken(aSection);
  if not fSectionMap.TryGetValue(lToken, Result) then
    Result := nil;
end;

function TRichIniFile.FindLastKeyLine(const aSection, aKey: string): TRichIniKeyLine;
var
  lKeyList: TObjectList<TRichIniKeyLine>;
  lSectionList: TObjectList<TRichIniSectionBlock>;
  lKeyToken: string;
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

function TRichIniFile.BuildKeyRawText(const aKey, aValue: string): string;
begin
  Result := aKey + string(fOptions.KeyValueDelimiter) + aValue;
end;

function TRichIniFile.EnsureSectionBlockForWrite(const aSection: string): TRichIniSectionBlock;
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

function TRichIniFile.FindKeyLineByIndex(const aSection, aKey: string; aKeyIndex: Integer): TRichIniKeyLine;
var
  lCount: Integer;
  lLine: TRichIniLine;
  lSectionToken: string;
  lKeyToken: string;
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
    if (lLine.SectionBlock.LookupName = lSectionToken) and
       (TRichIniKeyLine(lLine).LookupKey = lKeyToken) then
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

function TRichIniFile.DefaultCommentPrefix: string;
begin
  if Length(fOptions.CommentPrefixes) > 0 then
    Result := fOptions.CommentPrefixes[0]
  else
    Result := ';';
end;

function TRichIniFile.SplitToLines(const aText: string): TArray<string>;
var
  lNormalized: string;
  lList: TList<string>;
  lStart: Integer;
  i: Integer;
begin
  lNormalized := StringReplace(aText, sLineBreak, #10, [rfReplaceAll]);
  lNormalized := StringReplace(lNormalized, #13, #10, [rfReplaceAll]);
  lList := TList<string>.Create;
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

function TRichIniFile.CollectCommentText(const aLine: TRichIniLine): string;
var
  lBuilder: TStringBuilder;
  i: Integer;
begin
  if (aLine = nil) or (aLine.OwnedCommentCount = 0) then
    Exit('');
  lBuilder := TStringBuilder.Create;
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

function TRichIniFile.EncodeMultilineValue(const aValue: string): string;
var
  lBuilder: TStringBuilder;
  i: Integer;
begin
  lBuilder := TStringBuilder.Create;
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

function TRichIniFile.DecodeMultilineValue(const aValue: string): string;
var
  lBuilder: TStringBuilder;
  i: Integer;
begin
  lBuilder := TStringBuilder.Create;
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
  lText: string;
  lHasBom: Boolean;
begin
  fFileName := aFileName;
  ClearDocument;
  if (aFileName = '') or not TFile.Exists(aFileName) then
    Exit;

  lBuffer := TFile.ReadAllBytes(aFileName);
  lText := ResolveLoadEncoding(lBuffer, lEncoding, lHasBom);
  fDetectedNewline := DetectNewline(lText);
  ParseText(lText);
  fSourceEncoding := lEncoding;
  fSourceBom := lHasBom;
  fHasSource := True;
  fDirty := False;
end;

procedure TRichIniFile.ParseText(const aText: string);
var
  lCurrentBlock: TRichIniSectionBlock;
  lPendingComments: TList<TRichIniCommentLine>;
  lLastContentLine: TRichIniLine;
  lLength: Integer;
  lIndex: Integer;
  lLineStart: Integer;
  lLineText: string;
  lEstimatedLines: Integer;

  procedure ProcessLine(const aLine: string);
  var
    lLineLen: Integer;
    lTrimStart: Integer;
    lTrimEnd: Integer;
    lTrimLength: Integer;
    lLeadingWhitespace: string;
    lPrefix: string;
    lPrefixLen: Integer;
    lCommentTextStart: Integer;
    lCommentText: string;
    lComment: TRichIniCommentLine;
    lSectionName: string;
    lSectionStart: Integer;
    lSectionEnd: Integer;
    lClosingPos: Integer;
    lSectionLine: TRichIniSectionLine;
    lDelimiterPos: Integer;
    lKeyStart: Integer;
    lKeyEnd: Integer;
    lValueStart: Integer;
    lValueEnd: Integer;
    lKeyText: string;
    lValueText: string;
    lKeyLine: TRichIniKeyLine;
    lLine: TRichIniLine;
    i: Integer;
  begin
    lLineLen := Length(aLine);
    lTrimStart := 1;
    while (lTrimStart <= lLineLen) and CharInSet(aLine[lTrimStart], [' ', #9]) do
      Inc(lTrimStart);
    if lTrimStart > lLineLen then
    begin
      lLine := TRichIniLine.Create(lkBlank, aLine);
      lLine.SectionBlock := lCurrentBlock;
      AddLineToDocument(lLine);
      Exit;
    end;

    lTrimEnd := lLineLen;
    while (lTrimEnd >= lTrimStart) and CharInSet(aLine[lTrimEnd], [' ', #9]) do
      Dec(lTrimEnd);
    lTrimLength := lTrimEnd - lTrimStart + 1;
    lLeadingWhitespace := Copy(aLine, 1, lTrimStart - 1);

    for i := 0 to High(fOptions.CommentPrefixes) do
    begin
      lPrefix := fOptions.CommentPrefixes[i];
      lPrefixLen := Length(lPrefix);
      if (lPrefixLen > 0) and (lTrimLength >= lPrefixLen) and
         (StrLComp(PChar(@aLine[lTrimStart]), PChar(lPrefix), lPrefixLen) = 0) then
      begin
        lCommentTextStart := lTrimStart + lPrefixLen;
        while (lCommentTextStart <= lLineLen) and CharInSet(aLine[lCommentTextStart], [' ', #9]) do
          Inc(lCommentTextStart);
        if lCommentTextStart <= lLineLen then
          lCommentText := Copy(aLine, lCommentTextStart, lLineLen - lCommentTextStart + 1)
        else
          lCommentText := '';
        lComment := TRichIniCommentLine.Create(aLine, lLeadingWhitespace, lPrefix, lCommentText);
        lComment.SectionBlock := lCurrentBlock;
        AddLineToDocument(lComment);
        case fOptions.CommentOwnership of
          coAttachToNext:
            lPendingComments.Add(lComment);
          coAttachToPrev:
            if lLastContentLine <> nil then
              lLastContentLine.AttachComment(lComment);
        end;
        Exit;
      end;
    end;

    if aLine[lTrimStart] = '[' then
    begin
      lClosingPos := 0;
      for i := lTrimStart + 1 to lLineLen do
        if aLine[i] = ']' then
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
        while (lSectionStart <= lSectionEnd) and CharInSet(aLine[lSectionStart], [' ', #9]) do
          Inc(lSectionStart);
        while (lSectionEnd >= lSectionStart) and CharInSet(aLine[lSectionEnd], [' ', #9]) do
          Dec(lSectionEnd);
        if lSectionStart <= lSectionEnd then
          lSectionName := Copy(aLine, lSectionStart, lSectionEnd - lSectionStart + 1)
        else
          lSectionName := '';
        lCurrentBlock := CreateSectionBlock(lSectionName, False);
        lSectionLine := TRichIniSectionLine.Create(lSectionName, aLine);
        lSectionLine.SectionBlock := lCurrentBlock;
        lCurrentBlock.HeaderLine := lSectionLine;
        AddLineToDocument(lSectionLine);
        AttachPendingComments(lSectionLine, lPendingComments);
        lLastContentLine := lSectionLine;
        Exit;
      end;
    end;

    lDelimiterPos := 0;
    for i := lTrimStart to lLineLen do
      if aLine[i] = fOptions.KeyValueDelimiter then
      begin
        lDelimiterPos := i;
        Break;
      end;

    if lDelimiterPos > 0 then
      lKeyEnd := lDelimiterPos - 1
    else
      lKeyEnd := lTrimEnd;
    lKeyStart := lTrimStart;
    while (lKeyStart <= lKeyEnd) and CharInSet(aLine[lKeyStart], [' ', #9]) do
      Inc(lKeyStart);
    while (lKeyEnd >= lKeyStart) and CharInSet(aLine[lKeyEnd], [' ', #9]) do
      Dec(lKeyEnd);
    if lKeyStart <= lKeyEnd then
      lKeyText := Copy(aLine, lKeyStart, lKeyEnd - lKeyStart + 1)
    else
      lKeyText := '';

    if lDelimiterPos > 0 then
    begin
      lValueStart := lDelimiterPos + 1;
      while (lValueStart <= lLineLen) and CharInSet(aLine[lValueStart], [' ', #9]) do
        Inc(lValueStart);
      lValueEnd := lLineLen;
      while (lValueEnd >= lValueStart) and CharInSet(aLine[lValueEnd], [' ', #9]) do
        Dec(lValueEnd);
      if lValueStart <= lValueEnd then
        lValueText := Copy(aLine, lValueStart, lValueEnd - lValueStart + 1)
      else
        lValueText := '';
    end else
      lValueText := '';

    lKeyLine := TRichIniKeyLine.Create(lKeyText, lValueText, KeyToken(lKeyText), fOptions.KeyValueDelimiter, aLine);
    lKeyLine.SectionBlock := lCurrentBlock;
    lCurrentBlock.AddKeyLine(lKeyLine);
    AddLineToDocument(lKeyLine);
    AttachPendingComments(lKeyLine, lPendingComments);
    lLastContentLine := lKeyLine;
  end;

begin
  if fSections.Count = 0 then
    CreateGlobalSection;
  lCurrentBlock := fSections[0];
  lPendingComments := TList<TRichIniCommentLine>.Create;
  try
    lLastContentLine := nil;
    lLength := Length(aText);
    lEstimatedLines := 0;
    if lLength > 0 then
    begin
      lEstimatedLines := 1;
      lIndex := 1;
      while lIndex <= lLength do
      begin
        if aText[lIndex] = #10 then
          Inc(lEstimatedLines)
        else if aText[lIndex] = #13 then
        begin
          Inc(lEstimatedLines);
          if (lIndex < lLength) and (aText[lIndex + 1] = #10) then
            Inc(lIndex);
        end;
        Inc(lIndex);
      end;
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
        ProcessLine(lLineText);
      end else
        Inc(lIndex);
    end;
    if lLineStart <= lLength + 1 then
    begin
      lLineText := Copy(aText, lLineStart, lLength - lLineStart + 1);
      if (lLineText <> '') or (lLength = 0) then
        ProcessLine(lLineText);
    end;
  finally
    lPendingComments.Free;
  end;
end;

function TRichIniFile.PlatformNewline: string;
begin
  {$IFDEF MSWINDOWS}
  Result := sLineBreak;
  {$ELSE}
  Result := #10;
  {$ENDIF}
end;

function TRichIniFile.ResolveLoadEncoding(const aBuffer: TBytes; out aEncoding: TEncoding; out aHasBom: Boolean): string;
var
  lBytes: TBytes;
  lDetectedEncoding: TEncoding;
  lBomLen: Integer;
  lPreamble: TBytes;
  lOffset: Integer;

  function DetectPreambleOffset(const aPreamble: TBytes): Integer;
  var
    i: Integer;
  begin
    Result := 0;
    if Length(aPreamble) = 0 then
      Exit;
    if Length(lBytes) < Length(aPreamble) then
      Exit;
    for i := 0 to Length(aPreamble) - 1 do
      if lBytes[i] <> aPreamble[i] then
        Exit;
    Result := Length(aPreamble);
  end;
begin
  Result := '';
  lBytes := aBuffer;
  lDetectedEncoding := nil;
  lBomLen := TEncoding.GetBufferEncoding(lBytes, lDetectedEncoding);
  aHasBom := lBomLen > 0;
  case fOptions.LoadEncoding of
    eoUTF8:
      begin
        aEncoding := TEncoding.UTF8;
        lPreamble := aEncoding.GetPreamble;
        lOffset := DetectPreambleOffset(lPreamble);
        aHasBom := lOffset > 0;
        Result := aEncoding.GetString(lBytes, lOffset, Length(lBytes) - lOffset);
      end;
    eoAnsi:
      begin
        aEncoding := TEncoding.ANSI;
        aHasBom := False;
        Result := aEncoding.GetString(lBytes);
      end;
    eoCustom:
      begin
        if fOptions.CustomEncoding = nil then
          raise EArgumentException.Create('CustomEncoding must be assigned when LoadEncoding = eoCustom');
        aEncoding := fOptions.CustomEncoding;
        lPreamble := aEncoding.GetPreamble;
        lOffset := DetectPreambleOffset(lPreamble);
        aHasBom := lOffset > 0;
        Result := aEncoding.GetString(lBytes, lOffset, Length(lBytes) - lOffset);
      end;
  else
    begin
      if lDetectedEncoding <> nil then
        aEncoding := lDetectedEncoding
      else
        aEncoding := TEncoding.UTF8;
      lOffset := lBomLen;
      try
        Result := aEncoding.GetString(lBytes, lOffset, Length(lBytes) - lOffset);
      except
        on E: EEncodingError do
        begin
          aEncoding := TEncoding.ANSI;
          Result := aEncoding.GetString(lBytes);
          aHasBom := False;
        end;
      end;
    end;
  end;
end;

function TRichIniFile.ResolveNewline: string;
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

class procedure TRichIniFile.RaiseNotImplemented(const aMethod: string);
begin
  raise EInvalidOperation.CreateFmt('%s is not implemented yet.', [aMethod]);
end;

procedure TRichIniFile.SaveToFile(const aFileName: string);
var
  lTargetFile: string;
  lTempFile: string;
  lEncoding: TEncoding;
  lWriteBom: Boolean;
  lNewline: string;
  lBuilder: TStringBuilder;
  lLine: TRichIniLine;
  lRendered: string;
  i: Integer;
  lContent: string;
  lStream: TFileStream;
  lPreamble: TBytes;
  lBytes: TBytes;
  lDirectory: string;

  function RenderLine(const aLine: TRichIniLine): string;
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

begin
  if aFileName <> '' then
    lTargetFile := aFileName
  else
    lTargetFile := fFileName;
  if lTargetFile = '' then
    raise EInvalidOperation.Create('No target file specified for SaveToFile');

  ResolveSaveEncoding(lEncoding, lWriteBom);
  lNewline := ResolveNewline;

  lBuilder := TStringBuilder.Create;
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
        lBytes := lEncoding.GetBytes(lContent);
        if Length(lBytes) > 0 then
          lStream.WriteBuffer(lBytes[0], Length(lBytes));
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

function TRichIniFile.AppendKey(const aSection, aKey, aValue: string): Integer;
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
  lSeen: TDictionary<string, Boolean>;
  lNames: TList<string>;
  lBlock: TRichIniSectionBlock;
  lName: string;
begin
  lSeen := TDictionary<string, Boolean>.Create;
  lNames := TList<string>.Create;
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

procedure TRichIniFile.ConsolidateSection(const aSection: string);
var
  lList: TObjectList<TRichIniSectionBlock>;
  lPrimary: TRichIniSectionBlock;
  lOrder: TList<string>;
  lIndexMap: TDictionary<string, Integer>;
  lKeyText: TDictionary<string, string>;
  lValueMap: TDictionary<string, string>;
  lCommentMap: TDictionary<string, string>;
  lLine: TRichIniLine;
  lToken: string;
  lIdx: Integer;
  i: Integer;
  lBlocksToRemove: TList<TRichIniSectionBlock>;
  lBlock: TRichIniSectionBlock;
  lBlockList: TObjectList<TRichIniSectionBlock>;
  lComment: string;
  lSectionToken: string;
begin
  lList := FindSectionList(aSection);
  if (lList = nil) or (lList.Count = 0) then
    Exit;

  lPrimary := lList[0];

  lOrder := TList<string>.Create;
  lIndexMap := TDictionary<string, Integer>.Create;
  lKeyText := TDictionary<string, string>.Create;
  lValueMap := TDictionary<string, string>.Create;
  lCommentMap := TDictionary<string, string>.Create;
  lBlocksToRemove := TList<TRichIniSectionBlock>.Create;
  lSectionToken := SectionToken(aSection);
  try
    for i := 1 to lList.Count - 1 do
      lBlocksToRemove.Add(lList[i]);

    for lLine in fLines do
    begin
      if (lLine.Kind <> lkKeyValue) or (lLine.SectionBlock = nil) or
         (lLine.SectionBlock.LookupName <> lSectionToken) then
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
      if (lLine.SectionBlock = nil) or (lLine.SectionBlock.LookupName <> lSectionToken) then
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

procedure TRichIniFile.DeleteKey(const aSection, aKey: string);
var
  lLine: TRichIniLine;
  lMatches: TList<TRichIniKeyLine>;
  lSectionToken: string;
  lKeyToken: string;
begin
  lMatches := TList<TRichIniKeyLine>.Create;
  try
    lSectionToken := SectionToken(aSection);
    lKeyToken := KeyToken(aKey);
    for lLine in fLines do
    begin
      if (lLine.Kind = lkKeyValue) and (lLine.SectionBlock <> nil) and
         (lLine.SectionBlock.LookupName = lSectionToken) and
         (TRichIniKeyLine(lLine).LookupKey = lKeyToken) then
        lMatches.Add(TRichIniKeyLine(lLine));
    end;
    for lLine in lMatches do
      DeleteKeyLineInstance(TRichIniKeyLine(lLine));
  finally
    lMatches.Free;
  end;
end;

procedure TRichIniFile.DeleteKey(const aSection, aKey: string; aKeyIndex: Integer);
var
  lLine: TRichIniKeyLine;
begin
  lLine := FindKeyLineByIndex(aSection, aKey, aKeyIndex);
  if lLine = nil then
    raise EArgumentOutOfRangeException.CreateFmt('Key "%s" in section "%s" does not have occurrence index %d.',
      [aKey, aSection, aKeyIndex]);
  DeleteKeyLineInstance(lLine);
end;

procedure TRichIniFile.EraseSection(const aSection: string);
var
  lBlocks: TList<TRichIniSectionBlock>;
  lBlock: TRichIniSectionBlock;
  lLine: TRichIniLine;
  lList: TObjectList<TRichIniSectionBlock>;
  lLinesToRemove: TList<TRichIniLine>;
  lSectionToken: string;
begin
  lBlocks := TList<TRichIniSectionBlock>.Create;
  try
    lSectionToken := SectionToken(aSection);
    for lBlock in fSections do
      if (lBlock <> nil) and (lBlock.LookupName = lSectionToken) then
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

function TRichIniFile.KeyCount(const aSection, aKey: string): Integer;
var
  lList: TObjectList<TRichIniSectionBlock>;
  lBlock: TRichIniSectionBlock;
  lKeyList: TObjectList<TRichIniKeyLine>;
  lKeyToken: string;
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

function TRichIniFile.LastKeyIndex(const aSection, aKey: string): Integer;
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

function TRichIniFile.ReadBool(const aSection, aKey: string; aDefault: Boolean): Boolean;
begin
  Result := StrToBoolDef(ReadString(aSection, aKey, BoolToStr(aDefault, True)), aDefault);
end;

function TRichIniFile.ReadComment(const aSection, aKey: string): string;
var
  lLine: TRichIniKeyLine;
begin
  lLine := FindLastKeyLine(aSection, aKey);
  if lLine = nil then
    Exit('');
  Result := CollectCommentText(lLine);
end;

function TRichIniFile.ReadInteger(const aSection, aKey: string; aDefault: Integer): Integer;
begin
  Result := StrToIntDef(ReadString(aSection, aKey, IntToStr(aDefault)), aDefault);
end;

procedure TRichIniFile.ReadSection(const aSection: string; aStrings: TStrings);
var
  lOrder: TList<string>;
  lIndexMap: TDictionary<string, Integer>;
  lKeyLine: TRichIniKeyLine;
  lMap: TDictionary<string, TRichIniKeyLine>;
  lIdx: Integer;
  lToken: string;
  j: Integer;
  lSectionList: TObjectList<TRichIniSectionBlock>;
  lBlock: TRichIniSectionBlock;
begin
  if aStrings = nil then
    Exit;
  aStrings.BeginUpdate;
  try
    aStrings.Clear;
    if fLines = nil then
      Exit;
    lOrder := TList<string>.Create;
    lIndexMap := TDictionary<string, Integer>.Create;
    lMap := TDictionary<string, TRichIniKeyLine>.Create;
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
  finally
    aStrings.EndUpdate;
  end;
end;

procedure TRichIniFile.ReadSections(aStrings: TStrings);
var
  lSeen: TDictionary<string, Boolean>;
  lBlock: TRichIniSectionBlock;
begin
  if aStrings = nil then
    Exit;
  aStrings.BeginUpdate;
  try
    aStrings.Clear;
    lSeen := TDictionary<string, Boolean>.Create;
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
  finally
    aStrings.EndUpdate;
  end;
end;

procedure TRichIniFile.ReadSectionValues(const aSection: string; aStrings: TStrings);
var
  lOrder: TList<string>;
  lIndexMap: TDictionary<string, Integer>;
  lKeyLine: TRichIniKeyLine;
  lMap: TDictionary<string, TRichIniKeyLine>;
  lIdx: Integer;
  lToken: string;
  j: Integer;
  lSectionList: TObjectList<TRichIniSectionBlock>;
  lBlock: TRichIniSectionBlock;
begin
  if aStrings = nil then
    Exit;
  aStrings.BeginUpdate;
  try
    aStrings.Clear;
    if fLines = nil then
      Exit;
    lOrder := TList<string>.Create;
    lIndexMap := TDictionary<string, Integer>.Create;
    lMap := TDictionary<string, TRichIniKeyLine>.Create;
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
  finally
    aStrings.EndUpdate;
  end;
end;

function TRichIniFile.ReadString(const aSection, aKey, aDefault: string): string;
var
  lLine: TRichIniKeyLine;
begin
  lLine := FindLastKeyLine(aSection, aKey);
  if lLine = nil then
    Result := aDefault
  else
    Result := lLine.Value;
end;

procedure TRichIniFile.ReadAllKeyValues(const aSection, aKey: string; out aValues: TArray<string>);
var
  lValues: TList<string>;
  lSectionList: TObjectList<TRichIniSectionBlock>;
  lBlock: TRichIniSectionBlock;
  lKeyLine: TRichIniKeyLine;
  lKeyToken: string;
begin
  lValues := TList<string>.Create;
  try
    lSectionList := FindSectionList(aSection);
    lKeyToken := KeyToken(aKey);
    if lSectionList <> nil then
      for lBlock in lSectionList do
        for lKeyLine in lBlock.KeyOrder do
          if lKeyLine.LookupKey = lKeyToken then
            lValues.Add(lKeyLine.Value);
    aValues := lValues.ToArray;
  finally
    lValues.Free;
  end;
end;

function TRichIniFile.ReadMultilineString(const aSection, aKey, aDefault: string): string;
var
  lLine: TRichIniKeyLine;
begin
  lLine := FindLastKeyLine(aSection, aKey);
  if lLine = nil then
    Exit(aDefault);
  Result := DecodeMultilineValue(lLine.Value);
end;

procedure TRichIniFile.WriteBool(const aSection, aKey: string; aValue: Boolean);
begin
  WriteString(aSection, aKey, BoolToStr(aValue, True));
end;

procedure TRichIniFile.WriteComment(const aSection, aKey, aComment: string);
var
  lLine: TRichIniKeyLine;
  lPrefix: string;
  lLines: TArray<string>;
  lCommentLine: TRichIniCommentLine;
  lRaw: string;
  lInsertIndex: Integer;
  lLineText: string;
  lIdx: Integer;
  lExisting: string;
  lExistingNorm: string;
  lNewNorm: string;

  function NormalizeComment(const aValue: string): string;
  begin
    Result := StringReplace(aValue, sLineBreak, #10, [rfReplaceAll]);
    Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
  end;
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

procedure TRichIniFile.WriteInteger(const aSection, aKey: string; aValue: Integer);
begin
  WriteString(aSection, aKey, IntToStr(aValue));
end;

procedure TRichIniFile.WriteMultilineString(const aSection, aKey, aValueWithLineBreaks: string);
begin
  WriteString(aSection, aKey, EncodeMultilineValue(aValueWithLineBreaks));
end;

procedure TRichIniFile.WriteString(const aSection, aKey, aValue: string);
var
  lLine: TRichIniKeyLine;
  lRaw: string;
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

procedure TRichIniFile.WriteString(const aSection, aKey, aValue: string; aKeyIndex: Integer);
var
  lLine: TRichIniKeyLine;
  lRaw: string;
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
