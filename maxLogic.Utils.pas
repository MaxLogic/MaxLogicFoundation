unit MaxLogic.Utils;

// Scope: tiny, dependency-free, pure helpers that don’t clearly belong elsewhere.

interface

uses
  System.SysUtils;

{ ===== Boolean ===== }
function b2s(const aValue: Boolean; const aTrueText: string = 'True'; const aFalseText: string = 'False'): string; inline;
function b2i(const aValue: Boolean): Integer; inline;

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

end.
