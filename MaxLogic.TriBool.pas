unit MaxLogic.TriBool;

{
  inspired by:
  https://stackoverflow.com/questions/19536331/three-valued-logic-in-delphi

  Some sample usage:

  var
  x: Double;
  tb1, tb2: TTriBool;

  tb1 := True;
  tb2 := x>3.0;
  Writeln((tb1 or tb2).ToString);

  tb1 := False;
  tb2.Value := tbUnknown;
  Writeln((tb1 or tb2).ToString);

  which outputs:

  True
  Unknown



}

interface

type
  TTriBool = record
  public
    type
    TTriBoolEnum = (tbUnknown, tbFalse, tbTrue);
  public
    Value: TTriBoolEnum;
  public
    class operator Implicit(const Value: Boolean): TTriBool;
    class operator Implicit(const Value: TTriBoolEnum): TTriBool;
    class operator Implicit(const Value: TTriBool): TTriBoolEnum;
    class operator Equal(const lhs, rhs: TTriBool): Boolean;
    class operator LogicalOr(const lhs, rhs: TTriBool): TTriBool;
    function ToString: string;
  end;

implementation

class operator TTriBool.Implicit(const Value: Boolean): TTriBool;
begin
  if Value then
    Result.Value := tbTrue
  else
    Result.Value := tbFalse;
end;

class operator TTriBool.Implicit(const Value: TTriBoolEnum): TTriBool;
begin
  Result.Value := Value;
end;

class operator TTriBool.Implicit(const Value: TTriBool): TTriBoolEnum;
begin
  Result := Value.Value;
end;

class operator TTriBool.Equal(const lhs, rhs: TTriBool): Boolean;
begin
  Result := lhs.Value = rhs.Value;
end;

class operator TTriBool.LogicalOr(const lhs, rhs: TTriBool): TTriBool;
begin
  if (lhs.Value = tbTrue) or (rhs.Value = tbTrue) then
    Result := tbTrue
  else if (lhs.Value = tbFalse) and (rhs.Value = tbFalse) then
    Result := tbFalse
  else
    Result := tbUnknown;
end;

function TTriBool.ToString: string;
begin
  case Value of
    tbFalse:
      Result := 'False';
    tbTrue:
      Result := 'True';
    // tbUnknown:
  else
    Result := 'Unknown';
  end;
end;

end.
