unit CancelToken;

interface

uses
  {$IFDEF FPC}
  Classes, SysUtils;
  {$ELSE}
  System.Classes, System.SyncObjs, System.SysUtils;
  {$ENDIF}

Type
  iCancelToken = Interface
    procedure Cancel;
    function GetCanceled: Boolean;
    property Canceled: Boolean read GetCanceled;
  end;

  TCancelToken = class(TInterfacedObject, iCancelToken)
  private
    fCanceled: Integer;
    function GetCanceled: Boolean;
  public
    procedure Cancel;
    property Canceled: Boolean read GetCanceled;
  end;

implementation

{ TCancelToken }

procedure TCancelToken.Cancel;
begin
  {$IFDEF FPC}
  System.InterlockedExchange(fCanceled, 1);
  {$ELSE}
  TInterlocked.Exchange(fCanceled, 1);
  {$ENDIF}
end;

function TCancelToken.GetCanceled: Boolean;
begin
  {$IFDEF FPC}
  Result := System.InterlockedCompareExchange(fCanceled, 0, 0) <> 0;
  {$ELSE}
  Result := TInterlocked.CompareExchange(fCanceled, 0, 0) <> 0;
  {$ENDIF}
end;

end.
