unit MaxLogic.ComGuard;

interface

type
  /// <summary>
  ///  Ensures COM is initialised *once* per thread and automatically
  ///  uninitialised on thread shutdown (even in thread-pool workers).
  /// </summary>
  TComGuard = record
  public
    class procedure Ensure; static;
  end;

implementation

uses
  Winapi.ActiveX, System.SysUtils;

threadvar
  _GuardCookie: IInterface;  // lives in TLS: one copy per thread

type
  TUninit = class(TInterfacedObject)
  public
    destructor Destroy; override;
  end;

destructor TUninit.Destroy;
begin
  CoUninitialize;
  inherited;
end;

class procedure TComGuard.Ensure;
var
  hr: HRESULT;
begin
  if Assigned(_GuardCookie) then
    Exit; // this thread is already COM-ready

  hr := CoInitializeEx(nil, COINIT_MULTITHREADED);
  case hr of
    S_OK, S_FALSE: _GuardCookie := TUninit.Create;         // balance with CoUninitialize
    {RPC_E_CHANGED_MODE:
      raise Exception.Create('Thread already initialised with a different COM apartment model');
  else
    OleCheck(hr);  // re-raise any other failure}
  end;
end;

end.
