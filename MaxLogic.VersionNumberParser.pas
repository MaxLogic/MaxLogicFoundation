unit MaxLogic.VersionNumberParser;

interface

uses
  sysUtils, classes;

Type
  TVersion = record
  private
    function GetBuild: Integer;
    function GetHasBuild: Boolean;
    function GetHasMajor: Boolean ;
    function GetHasMinor: Boolean ;
    function GetHasRelease: Boolean ;
    function GetMajor: Integer;
    function GetMinor: Integer;
    function GetRelease: Integer;
    function GetText: String;
    procedure SetBuild(const Value: Integer);
    procedure SetMajor(const Value: Integer);
    procedure SetMinor(const Value: Integer);
    procedure SetRelease(const Value: Integer);
    procedure SetText(const Value: String);
  public
    Data: array[0..3] of Integer;

    // returns true if HasMajor and HasMinor, Release and Build are optional
    Function Valid: Boolean; overload;
    // checks any amount of version parts. 0=Major, 1=Major+Minor, 2=Major+Minor+Release and 3=all
    Function Valid(aCount: Integer): Boolean; overload;

    // sets all values to -1
    Procedure Clear;
    Function ParsePostgresVersionString(const aVersionString: String): Boolean;

    Function IsGreaterOrEqualThen(const v:TVersion): Boolean;
    Function IsLessThen(const v:TVersion): Boolean;

    property Text: String read GetText write SetText;

    // instead of accessing the arraym you can access the date like this
    property Major: Integer read GetMajor write SetMajor;
    property Minor : Integer read GetMinor write SetMinor;
    property Release : Integer read GetRelease write SetRelease;
    property Build : Integer read GetBuild write SetBuild;

    // -1 means no valid entry exists
    property HasMajor : Boolean  read GetHasMajor ;
    property HasMinor : Boolean  read GetHasMinor ;
    property HasRelease : Boolean read GetHasRelease ;
    property HasBuild : Boolean read GetHasBuild ;
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
  Result:= data[3]
end;

function TVersion.GetHasBuild: Boolean;
begin
  Result:= GetBuild <>-1;
end;

function TVersion.GetHasMajor: Boolean ;
begin
  Result:= Major <> -1;
end;

function TVersion.GetHasMinor: Boolean ;
begin
  Result:= Minor <> -1;
end;

function TVersion.GetHasRelease: Boolean ;
begin
  Result:= Release <> -1;
end;

function TVersion.GetMajor: Integer;
begin
  Result:= data[0];;
end;

function TVersion.GetMinor: Integer;
begin
  Result:= data[1];
end;

function TVersion.GetRelease: Integer;
begin
  Result:= data[2];
end;

function TVersion.GetText: String;
var
  x: Integer;
begin
  Result:= '';
  for x := 3 downto 0 do
  begin
    if (data[x]<>-1) or (Result <> '') then
    begin
      if Result <> '' then
        Result := '.' + Result;
      if data[x] = -1 then
        Result := '0' + Result
      else
        Result:= data[x].ToString + Result;
    end;
  end;
end;

function TVersion.IsGreaterOrEqualThen(const v: TVersion): Boolean;
var
  x: Integer;
begin
  for x := 0 to 3 do
    // -1 means null/nil/invalid, so we can not really test with anything
    if (data[x]<> -1) and (v.data[x]<>-1) then
    begin
      if data[x]> v.Data[x] then
        Exit(True)
      else if data[x]< v.Data[x] then
        Exit(False);
    end;

  Result:= True; // all are equal
end;

function TVersion.IsLessThen(const v: TVersion): Boolean;
var
  x: Integer;
begin
  for x := 0 to 3 do
    // -1 means null/nil/invalid, so we can not really test with anything
    if (data[x]<> -1) and (v.data[x]<>-1) then
    begin
      if data[x]< v.Data[x] then
        Exit(True)
      else if data[x]> v.Data[x] then
        Exit(False);
    end;

  Result:= False; // all are equal
end;

function TVersion.ParsePostgresVersionString(
  const aVersionString: String): Boolean;
var
  ar: TArray<String>;
  i: integer;
begin
  Clear;
  Result:= false;

  // looks like this:
  // "PostgreSQL 9.3.5, compiled by Visual C++ build 1600, 32-bit"

  ar := aVersionString.Split([' ', ','], TStringSplitOptions.ExcludeEmpty);
  if length(ar) >= 2 then
  begin
    self.Text := ar[1];
    result := self.Valid;
  end;
end;

procedure TVersion.SetBuild(const Value: Integer);
begin
  data[3]:= value;
end;









procedure TVersion.SetMajor(const Value: Integer);
begin
  data[0]:= value;
end;

procedure TVersion.SetMinor(const Value: Integer);
begin
data[1]:= value;
end;

procedure TVersion.SetRelease(const Value: Integer);
begin
  data[2]:= value;
end;

procedure TVersion.SetText(const Value: String);
var
  ar: TArray<String>;
  x: Integer;
begin
  ar:= value.Split(['.', ' '], TStringSplitOptions.ExcludeEmpty);
  SetLength(ar, 4);
  for x := 0 to 3 do
    data[x] := strToIntDef(ar[x], -1);
end;

function TVersion.Valid(aCount: Integer): Boolean;
var
  x: Integer;
begin
  Result:= True;
  if aCount< 0 then
    aCount:= 0
  else if aCount> 3 then
    aCount:= 3;

  for x := 0 to aCount do
    if Data[x] = -1 then
      Exit(false);
end;

function TVersion.Valid: Boolean;
begin
  result:= HasMajor and HasMinor;
end;

end.
