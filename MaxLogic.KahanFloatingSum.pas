unit MaxLogic.KahanFloatingSum;

{ Inspired by:
  http://stackoverflow.com/questions/6699066/in-which-order-should-floats-be-added-to-get-the-most-precise-result#comment7935766_6699451 }

interface

uses
  System.SysUtils, System.Classes;

type
  TKahan = record
  private
    fCompensation: Double; // Tracks the small compensation value
    fSum: Double; // Accumulated sum
  public
    // Initializes fields when the record is created
    class operator Initialize(out ADest: TKahan);

    // Add a value to the sum with Kahan compensation
    procedure Add(const AValue: Double);

    // Initialize the sum with a first value
    procedure AddFirst(const AValue: Double);

    // Public property to expose the accumulated sum
    property Sum: Double read fSum;
  end;

implementation

{ TKahan }

class operator TKahan.Initialize(out ADest: TKahan);
begin
  // Ensure fields are initialized to zero
  ADest.fCompensation := 0.0;
  ADest.fSum := 0.0;
end;

procedure TKahan.Add(const AValue: Double);
var
  lTempSum, lCompensatedValue: Double;
begin
  lCompensatedValue := AValue - fCompensation;
  lTempSum := fSum + lCompensatedValue;
  fCompensation := (lTempSum - fSum) - lCompensatedValue;
  fSum := lTempSum;
end;

procedure TKahan.AddFirst(const AValue: Double);
begin
  fSum := AValue;
  fCompensation := 0.0;
end;

end.

