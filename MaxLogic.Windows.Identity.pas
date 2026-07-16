unit MaxLogic.Windows.Identity;

interface

type
  TWindowsBootIdentitySource = (
    wbisUnavailable,
    wbisWindowsRecorded,
    wbisApproximate);

  TWindowsBootIdentity = record
    UtcMilliseconds: Int64;
    Source: TWindowsBootIdentitySource;
  end;

/// <summary>
/// Returns only an already resolved identity and never starts WMI work.
/// </summary>
function TryGetCachedWindowsBootIdentity(
  out aIdentity: TWindowsBootIdentity): Boolean;
/// <summary>
/// Starts at most one background WMI lookup and waits for no more than 1000 ms.
/// The caller must be an owning background workflow, never a VCL event handler.
/// Pass aRetryUnavailable only when that workflow explicitly owns a retry.
/// </summary>
function TryGetWindowsBootIdentity(
  const aTimeoutMilliseconds: Cardinal;
  const aRetryUnavailable: Boolean;
  out aIdentity: TWindowsBootIdentity): Boolean;
/// <summary>
/// Returns process creation time from GetProcessTimes or False when unavailable.
/// </summary>
function TryGetProcessStartedAtUtcMilliseconds(
  const aProcessId: Cardinal;
  out aStartedAtUtcMilliseconds: Int64): Boolean;

{$IFDEF UNITTESTS}
type
  TWindowsBootIdentityQuery = reference to function(
    const aTimeoutMilliseconds: Cardinal;
    out aIdentity: TWindowsBootIdentity): Boolean;

procedure ResetWindowsBootIdentityCacheForTesting;
procedure SetWindowsBootIdentityQueryForTesting(
  const aQuery: TWindowsBootIdentityQuery);
{$ENDIF}

implementation

uses
  System.Classes,
  System.DateUtils,
  System.SyncObjs,
  System.SysUtils,
  System.Variants
  {$IFDEF MSWINDOWS},
  Winapi.ActiveX,
  Winapi.Wbem,
  Winapi.Windows
  {$ENDIF};

const
  cMaximumLookupMilliseconds = 1000;
  cUnavailableCacheMilliseconds = 60000;
  cProcessQueryLimitedInformation = $1000;
  cRpcAuthnLevelDefault = 0;
  cRpcAuthnLevelCall = 3;
  cRpcAuthnWinNt = 10;
  cRpcAuthzNone = 0;
  cRpcImpLevelImpersonate = 3;
  cEoacNone = 0;

type
  {$IFDEF UNITTESTS}
  TBootIdentityQueryMethod = TWindowsBootIdentityQuery;
  {$ELSE}
  TBootIdentityQueryMethod = reference to function(
    const aTimeoutMilliseconds: Cardinal;
    out aIdentity: TWindowsBootIdentity): Boolean;
  {$ENDIF}

  IBootIdentityLookup = interface
    ['{74B79B06-7161-4AE9-B035-53981A16F8D0}']
    function TryGetResult(out aIdentity: TWindowsBootIdentity): Boolean;
    function WaitFor(const aTimeoutMilliseconds: Cardinal): TWaitResult;
    procedure ExecuteQuery;
  end;

  TBootIdentityCacheDecision = (
    bicdAvailable,
    bicdUnavailable,
    bicdLookup);

  TBootIdentityLookup = class(TInterfacedObject, IBootIdentityLookup)
  private
    fDone: TEvent;
    fIdentity: TWindowsBootIdentity;
    fQuery: TBootIdentityQueryMethod;
    fSucceeded: Boolean;
    fTimeoutMilliseconds: Cardinal;
  public
    constructor Create(
      const aQuery: TBootIdentityQueryMethod;
      const aTimeoutMilliseconds: Cardinal);
    destructor Destroy; override;
    procedure ExecuteQuery;
    function TryGetResult(out aIdentity: TWindowsBootIdentity): Boolean;
    function WaitFor(const aTimeoutMilliseconds: Cardinal): TWaitResult;
  end;

var
  gCacheLock: TCriticalSection;
  gCachedIdentity: TWindowsBootIdentity;
  gFailureCachedAt: UInt64;
  gHasCachedIdentity: Boolean;
  gHasCachedFailure: Boolean;
  gLookup: IBootIdentityLookup;
  gQuery: TBootIdentityQueryMethod;

function TryParseDmtfDateTime(
  const aValue: string;
  out aUtcMilliseconds: Int64): Boolean;
var
  lIsoDateTime: string;
  lOffsetMinutes: Integer;
  lUtcDateTime: TDateTime;
begin
  Result := False;
  aUtcMilliseconds := 0;
  if (Length(aValue) < 25) or
    (aValue[15] <> '.') or
    (not CharInSet(aValue[22], ['+', '-'])) or
    (not TryStrToInt(Copy(aValue, 23, 3), lOffsetMinutes)) then
    Exit;
  lIsoDateTime := Format(
    '%s-%s-%sT%s:%s:%s.%s%s%.2d:%.2d',
    [
      Copy(aValue, 1, 4),
      Copy(aValue, 5, 2),
      Copy(aValue, 7, 2),
      Copy(aValue, 9, 2),
      Copy(aValue, 11, 2),
      Copy(aValue, 13, 2),
      Copy(aValue, 16, 3),
      aValue[22],
      lOffsetMinutes div 60,
      lOffsetMinutes mod 60
    ]);
  if not TryISO8601ToDate(lIsoDateTime, lUtcDateTime, True) then
    Exit;
  aUtcMilliseconds :=
    (DateTimeToUnix(lUtcDateTime, True) * 1000) +
    MilliSecondOf(lUtcDateTime);
  Result := aUtcMilliseconds > 0;
end;

{$IFDEF MSWINDOWS}
function InitializeWmiCom(out aShouldUninitialize: Boolean): Boolean;
var
  lHResult: HRESULT;
begin
  aShouldUninitialize := False;
  lHResult := CoInitializeEx(nil, COINIT_MULTITHREADED);
  if lHResult in [S_OK, S_FALSE] then
    aShouldUninitialize := True
  else if lHResult <> RPC_E_CHANGED_MODE then
    Exit(False);
  lHResult := CoInitializeSecurity(
    nil,
    -1,
    nil,
    nil,
    cRpcAuthnLevelDefault,
    cRpcImpLevelImpersonate,
    nil,
    cEoacNone,
    nil);
  Result := Succeeded(lHResult) or (lHResult = RPC_E_TOO_LATE);
  if (not Result) and aShouldUninitialize then
  begin
    CoUninitialize;
    aShouldUninitialize := False;
  end;
end;

function TryConnectWmi(
  out aLocator: IWbemLocator;
  out aServices: IWbemServices): Boolean;
var
  lHResult: HRESULT;
begin
  aLocator := nil;
  aServices := nil;
  lHResult := CoCreateInstance(
    CLSID_WbemLocator,
    nil,
    CLSCTX_INPROC_SERVER,
    IID_IWbemLocator,
    aLocator);
  if Failed(lHResult) then
    Exit(False);
  lHResult := aLocator.ConnectServer(
    'ROOT\CIMV2',
    '',
    '',
    '',
    0,
    '',
    nil,
    aServices);
  if Failed(lHResult) then
    Exit(False);
  lHResult := CoSetProxyBlanket(
    aServices,
    cRpcAuthnWinNt,
    cRpcAuthzNone,
    nil,
    cRpcAuthnLevelCall,
    cRpcImpLevelImpersonate,
    nil,
    cEoacNone);
  Result := Succeeded(lHResult);
end;

function TryReadWmiBootDateTime(
  const aServices: IWbemServices;
  const aTimeoutMilliseconds: Cardinal;
  out aValue: string): Boolean;
var
  lEnumerator: IEnumWbemClassObject;
  lHResult: HRESULT;
  lObject: IWbemClassObject;
  lPropertyName: WideString;
  lReturned: Cardinal;
  lValue: OleVariant;
begin
  aValue := '';
  lHResult := aServices.ExecQuery(
    'WQL',
    'SELECT LastBootUpTime FROM Win32_OperatingSystem',
    WBEM_FLAG_FORWARD_ONLY or WBEM_FLAG_RETURN_IMMEDIATELY,
    nil,
    lEnumerator);
  if Failed(lHResult) then
    Exit(False);
  lHResult := lEnumerator.Next(
    aTimeoutMilliseconds,
    1,
    lObject,
    lReturned);
  if Failed(lHResult) or (lReturned <> 1) then
    Exit(False);
  lPropertyName := 'LastBootUpTime';
  lHResult := lObject.Get(
    PWideChar(lPropertyName),
    0,
    lValue,
    nil,
    nil);
  Result :=
    Succeeded(lHResult) and
    (not VarIsNull(lValue)) and
    (not VarIsEmpty(lValue));
  if Result then
    aValue := VarToWideStr(lValue);
end;
{$ENDIF}

function QueryWindowsRecordedBootIdentity(
  const aTimeoutMilliseconds: Cardinal;
  out aIdentity: TWindowsBootIdentity): Boolean;
{$IFDEF MSWINDOWS}
var
  lBootDateTime: string;
  lComInitialized: Boolean;
  lLocator: IWbemLocator;
  lServices: IWbemServices;
  lUtcMilliseconds: Int64;
begin
  Result := False;
  aIdentity := Default(TWindowsBootIdentity);
  if not InitializeWmiCom(lComInitialized) then
    Exit;
  try
    if not TryConnectWmi(lLocator, lServices) then
      Exit;
    if not TryReadWmiBootDateTime(
      lServices,
      aTimeoutMilliseconds,
      lBootDateTime) then
      Exit;
    if not TryParseDmtfDateTime(
      lBootDateTime,
      lUtcMilliseconds) then
      Exit;
    aIdentity.UtcMilliseconds := lUtcMilliseconds;
    aIdentity.Source := wbisWindowsRecorded;
    Result := True;
  finally
    lServices := nil;
    lLocator := nil;
    if lComInitialized then
      CoUninitialize;
  end;
end;
{$ELSE}
begin
  aIdentity := Default(TWindowsBootIdentity);
  Result := False;
end;
{$ENDIF}

function StartBootIdentityLookup(
  const aQuery: TBootIdentityQueryMethod;
  const aTimeoutMilliseconds: Cardinal): IBootIdentityLookup;
var
  lLookup: IBootIdentityLookup;
  lThread: TThread;
begin
  lLookup := TBootIdentityLookup.Create(aQuery, aTimeoutMilliseconds);
  lThread := TThread.CreateAnonymousThread(
    procedure
    begin
      lLookup.ExecuteQuery;
    end);
  lThread.FreeOnTerminate := True;
  lThread.Start;
  Result := lLookup;
end;

constructor TBootIdentityLookup.Create(
  const aQuery: TBootIdentityQueryMethod;
  const aTimeoutMilliseconds: Cardinal);
begin
  inherited Create;
  fDone := TEvent.Create(nil, True, False, '');
  fQuery := aQuery;
  fTimeoutMilliseconds := aTimeoutMilliseconds;
end;

destructor TBootIdentityLookup.Destroy;
begin
  fDone.Free;
  inherited;
end;

procedure TBootIdentityLookup.ExecuteQuery;
begin
  try
    fSucceeded := fQuery(fTimeoutMilliseconds, fIdentity);
  except
    // The Try API reports expected WMI/COM lookup failures as unavailable.
    fIdentity := Default(TWindowsBootIdentity);
    fSucceeded := False;
  end;
  fDone.SetEvent;
end;

function TBootIdentityLookup.TryGetResult(
  out aIdentity: TWindowsBootIdentity): Boolean;
begin
  aIdentity := fIdentity;
  Result := fSucceeded;
end;

function TBootIdentityLookup.WaitFor(
  const aTimeoutMilliseconds: Cardinal): TWaitResult;
begin
  Result := fDone.WaitFor(aTimeoutMilliseconds);
end;

function TryGetCachedWindowsBootIdentity(
  out aIdentity: TWindowsBootIdentity): Boolean;
begin
  aIdentity := Default(TWindowsBootIdentity);
  gCacheLock.Acquire;
  try
    Result := gHasCachedIdentity;
    if Result then
      aIdentity := gCachedIdentity;
  finally
    gCacheLock.Release;
  end;
end;

function GetBootIdentityCacheDecision(
  const aTimeoutMilliseconds: Cardinal;
  const aRetryUnavailable: Boolean;
  out aIdentity: TWindowsBootIdentity;
  out aLookup: IBootIdentityLookup): TBootIdentityCacheDecision;
var
  lFailureAge: UInt64;
  lNow: UInt64;
  lQuery: TBootIdentityQueryMethod;
begin
  aIdentity := Default(TWindowsBootIdentity);
  aLookup := nil;
  gCacheLock.Acquire;
  try
    if gHasCachedIdentity then
    begin
      aIdentity := gCachedIdentity;
      Exit(bicdAvailable);
    end;
    lNow := TThread.GetTickCount64;
    lFailureAge := lNow - gFailureCachedAt;
    if gHasCachedFailure and
      (lFailureAge < cUnavailableCacheMilliseconds) and
      (not aRetryUnavailable) then
      Exit(bicdUnavailable);
    if gLookup = nil then
    begin
      lQuery := gQuery;
      gLookup := StartBootIdentityLookup(lQuery, aTimeoutMilliseconds);
    end;
    aLookup := gLookup;
    Result := bicdLookup;
  finally
    gCacheLock.Release;
  end;
end;

procedure CacheBootIdentityTimeout(const aLookup: IBootIdentityLookup);
begin
  gCacheLock.Acquire;
  try
    if gLookup = aLookup then
    begin
      gFailureCachedAt := TThread.GetTickCount64;
      gHasCachedFailure := True;
    end;
  finally
    gCacheLock.Release;
  end;
end;

function PublishBootIdentityResult(
  const aLookup: IBootIdentityLookup;
  const aLookupIdentity: TWindowsBootIdentity;
  const aLookupSucceeded: Boolean;
  out aIdentity: TWindowsBootIdentity): Boolean;
begin
  aIdentity := Default(TWindowsBootIdentity);
  gCacheLock.Acquire;
  try
    if gLookup <> aLookup then
    begin
      Result := gHasCachedIdentity;
      if Result then
        aIdentity := gCachedIdentity;
      Exit;
    end;
    gLookup := nil;
    Result := aLookupSucceeded;
    if Result then
    begin
      gCachedIdentity := aLookupIdentity;
      gHasCachedIdentity := True;
      gFailureCachedAt := 0;
      gHasCachedFailure := False;
      aIdentity := gCachedIdentity;
    end
    else
    begin
      gFailureCachedAt := TThread.GetTickCount64;
      gHasCachedFailure := True;
    end;
  finally
    gCacheLock.Release;
  end;
end;

function TryGetWindowsBootIdentity(
  const aTimeoutMilliseconds: Cardinal;
  const aRetryUnavailable: Boolean;
  out aIdentity: TWindowsBootIdentity): Boolean;
var
  lDecision: TBootIdentityCacheDecision;
  lLookup: IBootIdentityLookup;
  lLookupIdentity: TWindowsBootIdentity;
  lLookupSucceeded: Boolean;
  lTimeoutMilliseconds: Cardinal;
begin
  aIdentity := Default(TWindowsBootIdentity);
  lTimeoutMilliseconds := aTimeoutMilliseconds;
  if lTimeoutMilliseconds > cMaximumLookupMilliseconds then
    lTimeoutMilliseconds := cMaximumLookupMilliseconds;
  lDecision := GetBootIdentityCacheDecision(
    lTimeoutMilliseconds,
    aRetryUnavailable,
    aIdentity,
    lLookup);
  if lDecision = bicdAvailable then
    Exit(True);
  if lDecision = bicdUnavailable then
    Exit(False);
  if lLookup.WaitFor(lTimeoutMilliseconds) <> wrSignaled then
  begin
    CacheBootIdentityTimeout(lLookup);
    Exit(False);
  end;
  lLookupSucceeded :=
    lLookup.TryGetResult(lLookupIdentity) and
    (lLookupIdentity.Source <> wbisUnavailable) and
    (lLookupIdentity.UtcMilliseconds > 0);
  Result := PublishBootIdentityResult(
    lLookup,
    lLookupIdentity,
    lLookupSucceeded,
    aIdentity);
end;

{$IFDEF MSWINDOWS}
function TryGetProcessCreationFileTime(
  const aProcessId: Cardinal;
  out aCreationFileTime: TFileTime): Boolean;
var
  lExitFileTime: TFileTime;
  lKernelFileTime: TFileTime;
  lOwnHandle: Boolean;
  lProcess: THandle;
  lUserFileTime: TFileTime;
begin
  lOwnHandle := aProcessId <> Winapi.Windows.GetCurrentProcessId;
  if lOwnHandle then
  begin
    lProcess := OpenProcess(
      cProcessQueryLimitedInformation,
      False,
      aProcessId);
    if lProcess = 0 then
      lProcess := OpenProcess(PROCESS_QUERY_INFORMATION, False, aProcessId);
  end
  else
    lProcess := Winapi.Windows.GetCurrentProcess;
  if lProcess = 0 then
    Exit(False);
  try
    Result := GetProcessTimes(
      lProcess,
      aCreationFileTime,
      lExitFileTime,
      lKernelFileTime,
      lUserFileTime);
  finally
    if lOwnHandle then
      CloseHandle(lProcess);
  end;
end;
{$ENDIF}

function TryGetProcessStartedAtUtcMilliseconds(
  const aProcessId: Cardinal;
  out aStartedAtUtcMilliseconds: Int64): Boolean;
{$IFDEF MSWINDOWS}
var
  lCreationDateTime: TDateTime;
  lCreationFileTime: TFileTime;
  lSystemTime: TSystemTime;
begin
  aStartedAtUtcMilliseconds := 0;
  if not TryGetProcessCreationFileTime(aProcessId, lCreationFileTime) then
    Exit(False);
  if not FileTimeToSystemTime(lCreationFileTime, lSystemTime) then
    Exit(False);
  lCreationDateTime := SystemTimeToDateTime(lSystemTime);
  aStartedAtUtcMilliseconds :=
    (DateTimeToUnix(lCreationDateTime, True) * 1000) +
    lSystemTime.wMilliseconds;
  Result := aStartedAtUtcMilliseconds > 0;
end;
{$ELSE}
begin
  aStartedAtUtcMilliseconds := 0;
  Result := False;
end;
{$ENDIF}

{$IFDEF UNITTESTS}
procedure ResetWindowsBootIdentityCacheForTesting;
begin
  gCacheLock.Acquire;
  try
    gCachedIdentity := Default(TWindowsBootIdentity);
    gFailureCachedAt := 0;
    gHasCachedIdentity := False;
    gHasCachedFailure := False;
    gLookup := nil;
  finally
    gCacheLock.Release;
  end;
end;

procedure SetWindowsBootIdentityQueryForTesting(
  const aQuery: TWindowsBootIdentityQuery);
begin
  gCacheLock.Acquire;
  try
    if Assigned(aQuery) then
      gQuery := aQuery
    else
      gQuery := QueryWindowsRecordedBootIdentity;
  finally
    gCacheLock.Release;
  end;
end;
{$ENDIF}

initialization
  gCacheLock := TCriticalSection.Create;
  gQuery := QueryWindowsRecordedBootIdentity;

finalization
  gLookup := nil;
  gQuery := nil;
  gCacheLock.Free;

end.
