unit MaxLogic.Utils;

// Scope: tiny, dependency-free, pure helpers that don’t clearly belong elsewhere.

interface

uses
  System.SysUtils, system.classes;

{ ===== Boolean ===== }
function b2s(const aValue: Boolean; const aTrueText: string = 'True'; const aFalseText: string = 'False'): string; inline;
function b2i(const aValue: Boolean): Integer; inline;

{$IFNDEF FPC}
function GetTickCount64: uInt64; inline;
{$ENDIF}

function isFloat(const aText: String): Boolean;

implementation

uses
  MaxLogic.strutils, system.Math;

function b2s(const aValue: Boolean; const aTrueText: string; const aFalseText: string): string;
begin
  if aValue then
    Result := aTrueText
  else
    Result := aFalseText;
end;

function b2i(const aValue: Boolean): Integer;
begin
  if aValue then
    Result := 1
  else
    Result:= 0;
end;

{$IFNDEF FPC}
function GetTickCount64: uInt64; inline;
begin
  Result:= TThread.GetTickCount64;
end;
{$ENDIF}

function IsFloat(const aText: string): Boolean;
var
  lValue: Double;
  lFs: TFormatSettings;
begin
  lFs := TFormatSettings.Invariant; // standard, predictable baseline :contentReference[oaicite:0]{index=0}
  lFs.ThousandSeparator := #0;

  lFs.DecimalSeparator := '.';
  Result := TryStrToFloat(aText, lValue, lFs); // overload with explicit format settings :contentReference[oaicite:1]{index=1}

  if not Result then
  begin
    lFs.DecimalSeparator := ',';
    Result := TryStrToFloat(aText, lValue, lFs);
  end;
end;

end.
