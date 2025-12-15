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

implementation

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

end.
