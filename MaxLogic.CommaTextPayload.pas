unit MaxLogic.CommaTextPayload;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, system.Generics.Defaults;

type
  /// <summary>
  /// Managed record for fast and clean handling of key=value comma-delimited text using TDictionary.
  /// ATTENTION: Case-insensitive keys.
  /// </summary>
  TCommaTextPayload = record
  private
    type
      iDictWrapper = interface
        ['{34265137-1F93-4509-8A52-CAB468DA4998}']
        function GetDict: TDictionary<string, string>;
      end;
      TDictWrapper = class(TInterfacedObject, iDictWrapper)
      public
        Dict: TDictionary<string, string>;
        constructor Create;
        destructor Destroy; override;
        function GetDict: TDictionary<string, string>;
      end;
  private
    fDictRef: iDictWrapper;
    function GetDict: TDictionary<string, string>; inline;
    function GetValue(const aKey: string): string; inline; inline;
    procedure SetValue(const aKey, aValue: string); inline; inline;
    function GetCount: Integer; inline; inline;
    function GetCommaText: string; inline;
    procedure SetCommaText(const aText: string); inline;
  public
    class function Create: TCommaTextPayload; static;
    function Clone: TCommaTextPayload;
    procedure Clear; inline;
    function TryGetValue(const aKey: string; out aValue: string): Boolean; inline; inline;

    property Values[const aKey: string]: string read GetValue write SetValue; default;
    property Count: Integer read GetCount;
    property CommaText: string read GetCommaText write SetCommaText;
  end;

implementation

{ TCommaTextPayload.TDictWrapper }

constructor TCommaTextPayload.TDictWrapper.Create;
begin
  inherited Create;
  Dict := TDictionary<string, string>.Create(TIStringComparer.Ordinal);
end;

destructor TCommaTextPayload.TDictWrapper.Destroy;
begin
  Dict.Free;
  inherited;
end;

function TCommaTextPayload.TDictWrapper.GetDict: TDictionary<string, string>;
begin
  Result:= Dict;
end;

{ TCommaTextPayload }

class function TCommaTextPayload.Create: TCommaTextPayload;
begin
  Result.fDictRef := TDictWrapper.Create;
end;

function TCommaTextPayload.Clone: TCommaTextPayload;
var
  lPair: TPair<string, string>;
begin
  Result := TCommaTextPayload.Create;
  for lPair in GetDict do
    Result.GetDict.AddOrSetValue(lPair.Key, lPair.Value);
end;

procedure TCommaTextPayload.Clear;
begin
  GetDict.Clear;
end;

function TCommaTextPayload.GetDict: TDictionary<string, string>;
begin
  if not Assigned(fDictRef) then
    fDictRef := TDictWrapper.Create;
  Result := fDictRef.GetDict;
end;

function TCommaTextPayload.GetValue(const aKey: string): string;
begin
  GetDict.TryGetValue(aKey, Result);
end;

procedure TCommaTextPayload.SetValue(const aKey, aValue: string);
begin
  GetDict.AddOrSetValue(aKey, aValue);
end;

function TCommaTextPayload.GetCount: Integer;
begin
  Result := GetDict.Count;
end;

function TCommaTextPayload.TryGetValue(const aKey: string; out aValue: string): Boolean;
begin
  Result := GetDict.TryGetValue(aKey, aValue);
end;

function TCommaTextPayload.GetCommaText: string;
var
  lList: TStringList;
  lPair: TPair<string, string>;
begin
  lList := TStringList.Create;
  try
    lList.StrictDelimiter := True;

    for lPair in GetDict do
      lList.Values[lPair.Key] := lPair.Value;

    Result := lList.CommaText;
  finally
    lList.Free;
  end;
end;

procedure TCommaTextPayload.SetCommaText(const aText: string);
var
  lList: TStringList;
  x: Integer;
begin
  GetDict.Clear;

  lList := TStringList.Create;
  try
    lList.StrictDelimiter := True;
    lList.CommaText := aText;

    for x := 0 to lList.Count - 1 do
      GetDict.AddOrSetValue(lList.Names[x], lList.ValueFromIndex[x]);
  finally
    lList.Free;
  end;
end;

end.

