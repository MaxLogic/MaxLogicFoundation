unit CancelToken;

interface

uses
  SysUtils, Classes;

Type
  iCancelToken = Interface
    procedure Cancel;
    function GetCanceled: Boolean;
    property Canceled: Boolean read GetCanceled;
  end;

  TCancelToken = class(TInterfacedObject, iCancelToken)
  private
    fCanceled: Boolean;
    function GetCanceled: Boolean;
  public
    procedure Cancel;
    property Canceled: Boolean read GetCanceled;
  end;

implementation

{ TCancelToken }

procedure TCancelToken.Cancel;
begin
  fCanceled := True;
end;

function TCancelToken.GetCanceled: Boolean;
begin
  Result := fCanceled;
end;

end.
