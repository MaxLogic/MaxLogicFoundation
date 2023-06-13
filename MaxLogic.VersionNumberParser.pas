unit MaxLogic.VersionNumberParser;

interface

uses
  sysUtils, classes;

Type
  TVersion = record
  private
    function GetBuild: Integer;
    function GetHasBuild: Boolean;
    function GetHasMajor: Boolean;
    function GetHasMinor: Boolean;
    function GetHasRelease: Boolean;
    function GetMajor: Integer;
    function GetMinor: Integer;
    function GetRelease: Integer;
    function GetText: String;
    procedure SetBuild(const Value: Integer);
    procedure SetMajor(const Value: Integer);
    procedure SetMinor(const Value: Integer);
    procedure SetRelease(const Value: Integer);
    procedure SetText(const Value: String);
    function GetFullText: String;
  public
    Data: array [0 .. 3] of Integer;

    // returns true if HasMajor and HasMinor, Release and Build are optional
    Function Valid: Boolean; overload;
    // checks any amount of version parts. 0=Major, 1=Major+Minor, 2=Major+Minor+Release and 3=all
    Function Valid(aCount: Integer): Boolean; overload;

    // sets all values to -1
    Procedure Clear;
    Function ParsePostgresVersionString(const aVersionString: String): Boolean;

    Function IsGreaterOrEqualThen(const v: TVersion): Boolean;
    Function IsLessThen(const v: TVersion): Boolean;

    property Text: String read GetText write SetText;

    { property: FullText
      as text, but it always outputs all 4 parts of the version string. in other words all -1 will be treated as 0
      that helps if we want to store the version in a db or as a part of the file name.
      so we will evade the situation where we sometimes have the version like "1.0" and sometimes as "1.0.0" or "1.0.0.0"
      sometimes have the version like "1.0" and sometimes as "1.0.0" or "1.0.0.0" }
    property FullText: String read GetFullText;

    // instead of accessing the arraym you can access the date like this
    property Major: Integer read GetMajor write SetMajor;
    property Minor: Integer read GetMinor write SetMinor;
    property Release: Integer read GetRelease write SetRelease;
    property Build: Integer read GetBuild write SetBuild;

    // -1 means no valid entry exists
    property HasMajor: Boolean read GetHasMajor;
    property HasMinor: Boolean read GetHasMinor;
    property HasRelease: Boolean read GetHasRelease;
    property HasBuild: Boolean read GetHasBuild;
  end;

  /// <summary>
  /// This class helps to use version ranges.
  /// it is simply a minVersion-maxVersion notation, so separated by a "-"
  /// like: 1.1.9-5.1
  /// </summary>
  TVersionRange = record
  private
    function GetText: String;
    procedure SetText(const Value: String);
  public
    MinVersion: TVersion;
    MaxVersion: TVersion;

    Function InRange(const aVersion: TVersion): Boolean;

    Property Text: String read GetText write SetText;
  end;

implementation

uses
  strUtils;

{ TVersion }

procedure TVersion.Clear;
begin

end;

function TVersion.GetBuild: Integer;
begin
  Result := Data[3]
end;

function TVersion.GetFullText: String;
var
  x: Integer;
  sData: array of String;
begin
  setLength(sData, length(Data));
  for x := 0 to length(Data) - 1 do
  begin
    if Data[x] < 0 then
      sData[x] := '0'
    else
      sData[x] := IntToStr(Data[x]);
  end;
  Result := String.Join('.', sData);
end;

function TVersion.GetHasBuild: Boolean;
begin
  Result := GetBuild <> -1;
end;

function TVersion.GetHasMajor: Boolean;
begin
  Result := Major <> -1;
end;

function TVersion.GetHasMinor: Boolean;
begin
  Result := Minor <> -1;
end;

function TVersion.GetHasRelease: Boolean;
begin
  Result := Release <> -1;
end;

function TVersion.GetMajor: Integer;
begin
  Result := Data[0];;
end;

function TVersion.GetMinor: Integer;
begin
  Result := Data[1];
end;

function TVersion.GetRelease: Integer;
begin
  Result := Data[2];
end;

function TVersion.GetText: String;
var
  x: Integer;
begin
  Result := '';
  for x := 3 downto 0 do
  begin
    if (Data[x] <> -1) or (Result <> '') then
    begin
      if Result <> '' then
        Result := '.' + Result;
      if Data[x] = -1 then
        Result := '0' + Result
      else
        Result := Data[x].ToString + Result;
    end;
  end;
end;

function TVersion.IsGreaterOrEqualThen(const v: TVersion): Boolean;
var
  x: Integer;
begin
  for x := 0 to 3 do
    // -1 means null/nil/invalid, so we can not really test with anything
    if (Data[x] <> -1) and (v.Data[x] <> -1) then
    begin
      if Data[x] > v.Data[x] then
        Exit(True)
      else if Data[x] < v.Data[x] then
        Exit(False);
    end;

  Result := True; // all are equal
end;

function TVersion.IsLessThen(const v: TVersion): Boolean;
var
  x: Integer;
begin
  for x := 0 to 3 do
    // -1 means null/nil/invalid, so we can not really test with anything
    if (Data[x] <> -1) and (v.Data[x] <> -1) then
    begin
      if Data[x] < v.Data[x] then
        Exit(True)
      else if Data[x] > v.Data[x] then
        Exit(False);
    end;

  Result := False; // all are equal
end;

function TVersion.ParsePostgresVersionString(
  const aVersionString: String): Boolean;
var
  ar: TArray<String>;
begin
  Clear;
  Result := False;

  // looks like this:
  // "PostgreSQL 9.3.5, compiled by Visual C++ build 1600, 32-bit"

  ar := aVersionString.Split([' ', ','], TStringSplitOptions.ExcludeEmpty);
  if length(ar) >= 2 then
  begin
    self.Text := ar[1];
    Result := self.Valid;
  end;
end;

procedure TVersion.SetBuild(const Value: Integer);
begin
  Data[3] := Value;
end;

procedure TVersion.SetMajor(const Value: Integer);
begin
  Data[0] := Value;
end;

procedure TVersion.SetMinor(const Value: Integer);
begin
  Data[1] := Value;
end;

procedure TVersion.SetRelease(const Value: Integer);
begin
  Data[2] := Value;
end;

procedure TVersion.SetText(const Value: String);
var
  ar: TArray<String>;
  x: Integer;
begin
  ar := Value.Split(['.', ' '], TStringSplitOptions.ExcludeEmpty);
  setLength(ar, 4);
  for x := 0 to 3 do
    Data[x] := strToIntDef(ar[x], -1);
end;

function TVersion.Valid(aCount: Integer): Boolean;
var
  x: Integer;
begin
  Result := True;
  if aCount < 0 then
    aCount := 0
  else if aCount > 3 then
    aCount := 3;

  for x := 0 to aCount do
    if Data[x] = -1 then
      Exit(False);
end;

function TVersion.Valid: Boolean;
begin
  Result := HasMajor and HasMinor;
end;

{ TVersionRange }

function TVersionRange.GetText: String;
begin
  if MinVersion.Valid then
    Result := MinVersion.Text
  else
    Result := '*';

  if MaxVersion.Valid then
    Result := Result + '-' + MaxVersion.Text;
end;

function TVersionRange.InRange(const aVersion: TVersion): Boolean;
begin
  Result := True;

  if MinVersion.Valid then
    if aVersion.IsLessThen(MinVersion) then
      Exit(False);

  if MaxVersion.Valid then
    if MaxVersion.IsLessThen(aVersion) then
      Exit(False);
end;

procedure TVersionRange.SetText(const Value: String);
var
  ar: TArray<String>;
begin
  ar := Value.Split(['-'], 2);
  setLength(ar, 2);
  MinVersion.Text := ar[0];
  MaxVersion.Text := ar[1];
end;

end.
