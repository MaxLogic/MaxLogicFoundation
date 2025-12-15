unit maxlogic.fpc.compatibility;

{$I fpc_delphimode.inc}

interface

uses
  SysUtils, Classes
  {$IFDEF FPC}, syncobjs{$ENDIF};

{$IFNDEF FPC}
type
  QWord = uInt64;
{$ENDIF}

{$REGION 'FPC_TYPES'}
{$IFDEF FPC}
  {$if (FPC_FULLVERSION >= 30301)}
    // FPC 3.3.1+ : supports "reference to"
    type
      TProc = reference to procedure;

      // FPC does not allow overloading type identifiers by arity -> use distinct names
      TProc1<T> = reference to procedure (const A1: T);
      TProc2<T1,T2> = reference to procedure (const A1: T1; const A2: T2);
  {$else}
    // FPC ≤ 3.2.x : no "reference to"; use nested procvars to allow captures
    type
      TProc = procedure is nested;

      // Same naming rule: distinct identifiers for different arities
      TProc1<T> = procedure (const A1: T) is nested;
      TProc2<T1,T2> = procedure (const A1: T1; const A2: T2) is nested;
  {$endif}

  TMLMonitorObject = class
  private
    fCS: TRTLCriticalSection;
    fEvent: TEvent;
    fWaiters: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property CS: TRTLCriticalSection read fCS;
    property Event: TEvent read fEvent;
    property Waiters: Integer read fWaiters write fWaiters;
  end;

  TMonitor = class
  public
    class procedure Enter(aObj: TObject); static;
    class procedure Exit(aObj: TObject); static;
    class procedure Pulse(aObj: TObject); static;
    class procedure PulseAll(aObj: TObject); static;
    class procedure Wait(aObj: TObject); overload; static;
    class function Wait(aObj: TObject; aTimeout: Cardinal): Boolean; overload; static;
  end;
{$ENDIF}
{$ENDREGION 'FPC_TYPES'}

{$REGION 'FPC_TPath_substitution}
{$IFDEF FPC}
type
  TPath = class
  class function GetHomePath: String;
    class function Combine(const a, b: String): String;
  end;
{$ENDIF}
{$ENDREGION 'FPC_TPath_substitution}


implementation

{$REGION 'FPC_TPath_substitution}
{$IFDEF FPC}
class function TPath.GetHomePath: String;
var
  lHome: string;
begin
  {$IFDEF MSWINDOWS}
  lHome := GetEnvironmentVariable('LOCALAPPDATA');
  if lHome = '' then
    lHome := GetEnvironmentVariable('USERPROFILE');
  {$ELSE}
  // Prefer XDG if present, then HOME
  lHome := GetEnvironmentVariable('XDG_STATE_HOME');
  if lHome = '' then
    lHome := GetEnvironmentVariable('XDG_DATA_HOME');
  if lHome = '' then
    lHome := GetEnvironmentVariable('HOME');
  {$ENDIF}
  if lHome = '' then
    lHome := GetTempDir;
  Result := IncludeTrailingPathDelimiter(lHome);
end;

class function TPath.Combine(const a, b: String): String;
begin
  Result := IncludeTrailingPathDelimiter(a) + b;
end;
{$ENDIF}
{$ENDREGION 'FPC_TPath_substitution}

{$IFDEF FPC}
constructor TMLMonitorObject.Create;
begin
  inherited Create;
  InitCriticalSection(fCS);
  fEvent := TEvent.Create(nil, False, False, '');
  fWaiters := 0;
end;

destructor TMLMonitorObject.Destroy;
begin
  fEvent.Free;
  DoneCriticalSection(fCS);
  inherited Destroy;
end;

class procedure TMonitor.Enter(aObj: TObject);
begin
  EnterCriticalSection(TMLMonitorObject(aObj).fCS);
end;

class procedure TMonitor.Exit(aObj: TObject);
begin
  LeaveCriticalSection(TMLMonitorObject(aObj).fCS);
end;

class procedure TMonitor.Pulse(aObj: TObject);
begin
  if TMLMonitorObject(aObj).fWaiters > 0 then
    TMLMonitorObject(aObj).fEvent.SetEvent;
end;

class procedure TMonitor.PulseAll(aObj: TObject);
var
  i: Integer;
begin
  for i := 1 to TMLMonitorObject(aObj).fWaiters do
    TMLMonitorObject(aObj).fEvent.SetEvent;
end;

class procedure TMonitor.Wait(aObj: TObject);
begin
  Inc(TMLMonitorObject(aObj).fWaiters);
  TMLMonitorObject(aObj).fEvent.ResetEvent;
  LeaveCriticalSection(TMLMonitorObject(aObj).fCS);
  TMLMonitorObject(aObj).fEvent.WaitFor(INFINITE);
  EnterCriticalSection(TMLMonitorObject(aObj).fCS);
  Dec(TMLMonitorObject(aObj).fWaiters);
end;

class function TMonitor.Wait(aObj: TObject; aTimeout: Cardinal): Boolean;
begin
  Inc(TMLMonitorObject(aObj).fWaiters);
  RTLEventResetEvent(TMLMonitorObject(aObj).fEvent);
  LeaveCriticalSection(TMLMonitorObject(aObj).fCS);
  RTLEventWaitFor(TMLMonitorObject(aObj).fEvent, aTimeout);
  EnterCriticalSection(TMLMonitorObject(aObj).fCS);
  Dec(TMLMonitorObject(aObj).fWaiters);
  Result := True;
end;
{$ENDIF}

end.

