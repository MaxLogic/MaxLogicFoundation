﻿unit maxLogic.CmdLineParams;

{ a simple helper to allow access to the command line parameters with the ability to use it in unit testing }

interface

uses
  System.SysUtils, System.Classes, generics.collections;

type

  TSwitchPrefix = (spDash, spSlash, spDoubleDash);
  TSwitchPrefixes = set of TSwitchPrefix;

  iCmdLineparams = interface
    ['{5091EFAD-8AB6-4A68-A7B3-1C00055CD993}']

    procedure SetSwitchPrefixes(const Value: TSwitchPrefixes);
    function GetSwitchPrefixes: TSwitchPrefixes;

    /// <summary>
    /// Gets the total number of command line parameters stored.
    /// </summary>
    function GetCount: integer;
    /// <summary>
    /// Gets the list of command line parameters as a TStringList.
    /// </summary>
    function GetParamList: TStringList;
    /// <summary>
    /// Parses a command line string and populates the internal list.
    /// Handles quoted parameters using TStringList capabilities.
    /// </summary>
    procedure BuildFromString(const aCmdLineParams: string);

    /// <summary>
    /// Finds a command line switch parameter index.
    /// </summary>
    /// <param name="aSwitch">The switch name to find (without prefix).</param>
    /// <param name="aIgnoreCase">True to ignore case during comparison.</param>
    /// <param name="aIndex">Output: The index of the found parameter in the list.</param>
    /// <returns>True if the switch parameter is found, False otherwise.</returns>
    function find(const aSwitch: string; aIgnoreCase: boolean; out aIndex: integer): boolean; overload;

    /// <summary>
    /// Finds if a command line switch parameter exists using specified prefixes.
    /// </summary>
    function find(const aSwitch: string; aIgnoreCase: boolean): boolean; overload;

    /// <summary>
    /// Finds if a command line switch exists using default prefixes and case-insensitivity.
    /// </summary>
    function find(const aSwitch: string): boolean; overload;

    /// <summary>
    /// Finds a command line switch and attempts to extract its value.
    /// Value can be separated by ':', '=', or be the next parameter (if it doesn't look like another switch).
    /// Default is case-insensitive search.
    /// </summary>
    /// <param name="aSwitch">The switch name to find (without prefix).</param>
    /// <param name="aValue">Output: The extracted value associated with the switch.</param>
    /// <param name="aIgnoreCase">True to ignore case during comparison (default True).</param>
    /// <returns>True if the switch is found (regardless of value), False otherwise. aValue is only guaranteed to be valid if Result is True and a value could be extracted.</returns>
    function find(const aSwitch: string; var aValue: string; aIgnoreCase: boolean = True): boolean; overload;
    function find(const aSwitchAndAliases: TArray<string>; var aValue: string; aIgnoreCase: boolean = True): boolean; overload;

    function has(const aSwitchNames: array of string; aIgnoreCase: boolean = True): boolean;


    property Count: integer read GetCount;
    property Params: TStringList read GetParamList;
    property SwitchPrefixes: TSwitchPrefixes read GetSwitchPrefixes write SetSwitchPrefixes;
  end;


  /// <summary>
  /// Implements iCmdLineParams for parsing and accessing command line arguments.
  /// </summary>
  TCmdLineParams = class(TInterfacedObject, iCmdLineparams)
  private
    // key is the switch name without the switch prefix char and without an appended value
    fOrgCaseDic: TDictionary<string, integer>;
    fLowerCaseDic: TDictionary<string, integer>;
    fParams: TStringList;
    fSwitchPrefixes: TSwitchPrefixes;

    procedure RebuildDic;
    function IsSwitch(const aCmdParam: String; out aParamWithoutPrefix: String): Boolean; overload;
    function IsSwitch(const aCmdParam: String): Boolean; overload;
    function GetCount: integer;
    function GetParamList: TStringList;
    procedure SetSwitchPrefixes(const Value: TSwitchPrefixes);
    function GetSwitchPrefixes: TSwitchPrefixes;
  public
    constructor Create;
    destructor Destroy; override;

    procedure BuildFromString(const aCmdLineParams: string);
    function find(const aSwitch: string; aIgnoreCase: boolean; out aIndex: integer): boolean; overload;
    function find(const aSwitch: string; aIgnoreCase: boolean): boolean; overload;
    function find(const aSwitch: string): boolean; overload;
    function find(const aSwitch: string; var aValue: string; aIgnoreCase: boolean = True): boolean; overload;
    function find(const aSwitchAndAliases: TArray<string>; var aValue: string; aIgnoreCase: boolean = True): boolean; overload;

    function has(const aSwitchNames: array of string; aIgnoreCase: boolean = True): boolean; overload;


    property Count: integer read GetCount;
    property Params: TStringList read GetParamList;
    property SwitchPrefixes: TSwitchPrefixes read GetSwitchPrefixes write SetSwitchPrefixes;
  end;

function maxCmdLineParams: iCmdLineparams;

implementation

uses
  maxLogic.StrUtils, system.StrUtils;

var
  GlobalMaxCmdLineParams: iCmdLineparams = nil;

function maxCmdLineParams: iCmdLineparams;
begin
  if GlobalMaxCmdLineParams <> nil then
    Exit(GlobalMaxCmdLineParams);
  Result:= TCmdLineparams.Create;

  // simple test agains thread races
  if GlobalMaxCmdLineParams <> nil then
    Exit(GlobalMaxCmdLineParams);
  GlobalMaxCmdLineParams := Result;
end;

{ TCmdLineParams }

procedure TCmdLineParams.BuildFromString(const aCmdLineParams: string);
begin
  // TStringList handles quoted parameters correctly when StrictDelimiter is true.
  fParams.StrictDelimiter := True;
  fParams.Delimiter := ' ';
  fParams.DelimitedText := aCmdLineParams;
  RebuildDic;
end;

constructor TCmdLineParams.Create;
begin
  inherited Create;
  {$IFDEF MSWINDOWS}
  Self.fSwitchPrefixes:= [spDash, spDoubleDash, spSlash];
  {$ELSE}
  Self.fSwitchPrefixes:= [spDash, spDoubleDash];
  {$ENDIF}
  fOrgCaseDic := TDictionary<string, integer>.Create;
  fLowerCaseDic := TDictionary<string, integer>.Create;

  fParams := TStringList.Create;
  fParams.StrictDelimiter := True;
  fParams.Delimiter := ' ';

  for var i := 1 to System.ParamCount do
    fParams.add(System.ParamStr(i));

  RebuildDic;
end;

destructor TCmdLineParams.Destroy;
begin
  fParams.Free;
  fOrgCaseDic.Free;
  fLowerCaseDic.Free;
  inherited;
end;

function TCmdLineParams.find(const aSwitchAndAliases: TArray<string>; var aValue: string; aIgnoreCase: boolean = True): boolean; 
begin
  for var n in aSwitchAndAliases do
    if find(n, aValue, aIgnoreCase) then
      Exit(True);
  aValue:= '';
  Result:= False;
end;

function TCmdLineParams.find(const aSwitch: string; aIgnoreCase: boolean): boolean;
var
  i: integer;
begin
  Result := find(aSwitch, aIgnoreCase, i);
end;

function TCmdLineParams.find(const aSwitch: string): boolean;
var
  i: integer;
begin
  Result := find(aSwitch, True, i);
end;

function TCmdLineParams.find(const aSwitch: string; var aValue: string; aIgnoreCase: boolean): boolean;
var
  i: integer;
  s: string;
  lLeft, lRight: string;
  lPotentialValue: string;
begin
  Result := find(aSwitch, aIgnoreCase, i);
  aValue := '';

  if not Result then
    exit;

  s := fParams[i];

  // Check for value separated by ':' or '=' within the same parameter string 's'
  if maxLogic.StrUtils.SplitInHalfBy(s, ':', lLeft, lRight) or maxLogic.StrUtils.SplitInHalfBy(s, '=', lLeft, lRight) then
  begin
    aValue := lRight;
    exit;
  end;

  // If no separator, check the *next* parameter
  if (i + 1) < fParams.Count then
  begin
    lPotentialValue := fParams[i + 1];
    // Check if the next parameter looks like another switch
    if IsSwitch(lPotentialValue) then
    begin
      // Next item looks like a switch, so current switch has no value.
    end else begin
      // Treat the next parameter as the value
      aValue := lPotentialValue;
    end;
  end;
end;

// Core implementation to find the index of a switch parameter using dictionary lookups only
function TCmdLineParams.find(const aSwitch: string; aIgnoreCase: boolean; out aIndex: integer): boolean;
var
  lDic: TDictionary<string, integer>;
  lEffectiveSwitch: string;
begin
  Result := False;
  aIndex := -1;

  if aIgnoreCase then
  begin
    lDic := fLowerCaseDic;
    lEffectiveSwitch := aSwitch.ToLower;
  end
  else
  begin
    lDic := fOrgCaseDic;
    lEffectiveSwitch := aSwitch;
  end;

  if lDic.TryGetValue(lEffectiveSwitch, aIndex) then
    exit(True);

  // Switch not found by any means
  Result := False;
end;

function TCmdLineParams.has(const aSwitchNames: array of string; aIgnoreCase: boolean): boolean;
begin
  for var n in aSwitchNames do
    if find(n, aIgnoreCase) then
      Exit(True);
  Result := False;
end;

function TCmdLineParams.IsSwitch(const aCmdParam: String): Boolean;
var
  s: String;
begin
  Result:= IsSwitch(aCmdParam, s);
end;

function TCmdLineParams.IsSwitch(const aCmdParam: String;
  out aParamWithoutPrefix: String): Boolean;
var
  sp: TSwitchPrefix;
begin
  Result := False;
  if aCmdParam = '' then
    exit;


  if startsStr('--', aCmdParam) then
    sp:= TSwitchPrefix.spDoubleDash
  else if startsStr('-', aCmdParam) then
    sp:= TSwitchPrefix.spDash
  else if startsStr('/', aCmdParam) then
    sp:= TSwitchPrefix.spSlash
  else
    Exit(False);

  if sp in Self.fSwitchPrefixes then
  begin
    Result:= True;
    if sp = TSwitchPrefix.spDoubleDash then
      aParamWithoutPrefix:= Copy(aCmdParam, 3, Length(aCmdParam)-2)
    else
      aParamWithoutPrefix:= Copy(aCmdParam, 2, Length(aCmdParam)-1);
  end;
end;

function TCmdLineParams.GetCount: integer;
begin
  Result := fParams.Count;
end;

function TCmdLineParams.GetParamList: TStringList;
begin
  Result := fParams;
end;

function TCmdLineParams.GetSwitchPrefixes: TSwitchPrefixes;
begin
  Result:= fSwitchPrefixes;
end;

procedure TCmdLineParams.RebuildDic;
var
  s: string;
  lLowerParam, lLeft, lRight: string;
begin
  fOrgCaseDic.Clear;
  fLowerCaseDic.Clear;

  for var X := 0 to fParams.Count - 1 do
  begin
    if not IsSwitch(fParams[X], s) then
      Continue;

    // store switch without value in case of a ":" or '=" value separation
    if maxLogic.StrUtils.SplitInHalfBy(s, '=', lLeft, lRight) or maxLogic.StrUtils.SplitInHalfBy(s, ':', lLeft, lRight) then
      s := lLeft;

    s := s.Trim;
    if s = '' then
      Continue;

    if not fOrgCaseDic.ContainsKey(s) then
      fOrgCaseDic.add(s, X);

    lLowerParam := s.ToLower;
    if not fLowerCaseDic.ContainsKey(lLowerParam) then
      fLowerCaseDic.add(lLowerParam, X);
  end;
  fOrgCaseDic.TrimExcess;
  fLowerCaseDic.TrimExcess;
end;

procedure TCmdLineParams.SetSwitchPrefixes(const Value: TSwitchPrefixes);
begin
  fSwitchPrefixes := Value;
  RebuildDic;
end;

initialization

finalization
  GlobalMaxCmdLineParams:= nil;

end.

