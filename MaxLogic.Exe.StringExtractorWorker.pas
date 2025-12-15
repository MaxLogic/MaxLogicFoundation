unit MaxLogic.Exe.StringExtractorWorker;

interface

uses
  windows, classes, SysUtils,
  Messages, Controls, Forms, Dialogs,
  ExeImage,
  ComCtrls,
  RXMisc,
  HexDump,
  XPMan,
  // PiotrLib,,
  // LingoMaxDictionary,
  MaxLogic.DFM, rxtypes;

const
  CR = #13 + #10;

Type
{$IFNDEF UNICODE}
  UnicodeString = WideString;
{$ENDIF}

  TStringElement = class(TObject)
  private
    FName: string;
    FValue: string;
    procedure SetName(const Value: string);
    procedure SetValue(const Value: string);

  public
    constructor Create;
    destructor Destroy; override;

    property Name: string read FName write SetName;
    Property Value: string read FValue write SetValue;
  end;

  TStringExtractor = class(TObject)
  private
    fExeImage: TExeImage;
    fForms: TList;
    fStrings: TList;
    fFileName: string;
    fExcludeList: TStringList;

    procedure LoadResources(ResList: TResourceList);
    procedure AddStringTable(Str: UnicodeString);
    function GetDFMObject(index: integer): TDFMObject;
    function GetFormsCount: integer;
    function IsOnExcludeList(const PropName: string): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Extract(const FileName: string);
    procedure Clear;
    procedure LoadExcludeList(const FileName: string);

    property Forms[index: integer]: TDFMObject read GetDFMObject;
    Property FormsCount: integer read GetFormsCount;
  end;

implementation

uses
  maxLogic.ioUtils, ioutils, masks;

Function eTime: string;
begin
  result := FormatDateTime('yyyy"-"mm"-"dd" "hh":"nn":"ss', now);
end;

{ TStringElement }

constructor TStringElement.Create;
begin
  inherited;

end;

destructor TStringElement.Destroy;
begin

  inherited;
end;

procedure TStringElement.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TStringElement.SetValue(const Value: string);
begin
  FValue := Value;
end;
{ TStringExtractor }

procedure TStringExtractor.AddStringTable(Str: UnicodeString);
var
  i: integer;
  res: TStringElement;
  NumberTable, Value: UnicodeString;
begin
  i := Pos(',', Str);
  NumberTable := copy(Str, 1, i - 1);
  Value := copy(Str, i + 1, length(Str));
  Value := Trim(Value);

  // remove " from the start and the end
  delete(Value, 1, 1);
  delete(Value, length(Value), 1);

  res := TStringElement.Create;
  res.Name := NumberTable;
  res.Value := Value;
  fStrings.add(res);
end;

procedure TStringExtractor.Clear;
VAR
  X: integer;
begin
  for X := 0 to fForms.Count - 1 do
    TObject(Forms[X]).Free;
  fForms.Clear;

  for X := 0 to fStrings.Count - 1 do
    TObject(fStrings[X]).Free;
  fStrings.Clear;

  if Assigned(fExeImage) then
    FreeAndNIL(fExeImage);
end;

constructor TStringExtractor.Create;
begin
  inherited;
  fExeImage := NIL;
  fExcludeList := TStringList.Create;
  fForms := TList.Create;
  fStrings := TList.Create;
end;



destructor TStringExtractor.Destroy;
begin
  Clear;
  fExcludeList.Free;
  fForms.Free;
  fStrings.Free;
  inherited;
end;

procedure TStringExtractor.Extract(const FileName: string);
var
  ResList: TResourceList;
begin
  Clear;
  fFileName := FileName;

  fExeImage := TExeImage.CreateImage(NIL, FileName);
  ResList := fExeImage.Resources;
  LoadResources(ResList);
end;

function TStringExtractor.GetDFMObject(index: integer): TDFMObject;
begin
  result := fForms[index];
end;

function TStringExtractor.GetFormsCount: integer;
begin
  result := fForms.Count
end;



function TStringExtractor.IsOnExcludeList(const PropName: string): Boolean;
var
  X: integer;
  s: string;
begin
  result := false;
  s := Lowercase(PropName);
  for X := 0 to fExcludeList.Count - 1 do
  begin
    if masks.MatchesMask(s, fExcludeList[X]) then
    begin
      result := True;
      Break;
    end;

  end;

end;

procedure TStringExtractor.LoadExcludeList(const FileName: string);
var
  X: integer;
begin
  fExcludeList.Clear;
  fExcludeList.loadFromFile(FileName);
  for X := 0 to fExcludeList.Count - 1 do
    fExcludeList[X] := Lowercase(fExcludeList[X]);

end;

procedure TStringExtractor.LoadResources(ResList: TResourceList);
var
  ResItem: TResourceItem;
  i, J: integer;
  ResStr: TStringList;
  H: THandle;
  MM: TMemoryStream;
  res: TResourceStream;
  DFM: TDFMObject;
begin
  H := LoadLibrary(PChar(fFileName));
  ResStr := TStringList.Create;

  for i := 0 to ResList.Count - 1 do
  begin
    case ResList[i].ResType of
      rtString:
        begin
          ResItem := TResourceItem(ResList[i]);
          ResStr.Assign(ResItem);
          for J := 0 to ResStr.Count - 1 do
            AddStringTable(ResStr[J]);
        end;
      rtRCData:
        begin
          try
            res := TResourceStream.Create(H, ResList[i].Name, RT_RCDATA);
            DFM := TDFMObject.Create;
            try
              DFM.LoadFromStream(res);
              fForms.add(DFM);
            except
              DFM.Free;
            end;
            res.Free;
          except
            // do nothing
          end;
        end;

    end; // case

    if ResList[i].IsList then
      LoadResources(ResList[i].List);

  end; // for reslist
  ResStr.Free;
end;













end.
