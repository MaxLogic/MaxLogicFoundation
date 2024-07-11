unit maxLogic.IfThenEx;

interface

uses
  System.SysUtils, System.DateUtils, System.StrUtils;

function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: Integer): Integer; overload;
function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: Double): Double; overload;
function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: TDateTime): TDateTime; overload;
function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: Boolean): Boolean; overload;
function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: Int64): Int64; overload;
function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: Single): Single; overload;
function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: Cardinal): Cardinal; overload;
function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: Word): Word; overload;
function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: Byte): Byte; overload;

implementation

function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: Integer): Integer;
begin
  if aCondition then
    Result := aTrueResult
  else
    Result := aFalseResult;
end;

function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: Double): Double;
begin
  if aCondition then
    Result := aTrueResult
  else
    Result := aFalseResult;
end;

function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: TDateTime): TDateTime;
begin
  if aCondition then
    Result := aTrueResult
  else
    Result := aFalseResult;
end;

function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: Boolean): Boolean;
begin
  if aCondition then
    Result := aTrueResult
  else
    Result := aFalseResult;
end;

function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: Int64): Int64;
begin
  if aCondition then
    Result := aTrueResult
  else
    Result := aFalseResult;
end;

function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: Single): Single;
begin
  if aCondition then
    Result := aTrueResult
  else
    Result := aFalseResult;
end;

function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: Cardinal): Cardinal;
begin
  if aCondition then
    Result := aTrueResult
  else
    Result := aFalseResult;
end;

function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: Word): Word;
begin
  if aCondition then
    Result := aTrueResult
  else
    Result := aFalseResult;
end;

function IfThen(aCondition: Boolean; aTrueResult, aFalseResult: Byte): Byte;
begin
  if aCondition then
    Result := aTrueResult
  else
    Result := aFalseResult;
end;



end.

