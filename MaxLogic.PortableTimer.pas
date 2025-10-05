unit MaxLogic.PortableTimer;

{$I fpc_delphimode.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  maxSignal,                    // iSignal / TSignal
  maxlogic.fpc.compatibility,   // CS & helpers
  CancelToken;                  // iCancelToken / TCancelToken

type
  TTimerMode = (tmFixedDelay, tmFixedRate);

  // Non-visual, cross-platform threaded timer.
  // Fires OnTimer on its own worker thread.
  TThreadedTimer = class(TComponent)
  private
    type
      TTimerThread = class(TThread)
      private
        fOwner: TObject;            // backref, used only for callback target
        fWake: iSignal;             // auto-reset event (wake/sleep)
        fCancel: iCancelToken;      // cooperative cancellation
      protected
        procedure Execute; override;
      public
        constructor Create(aOwner: TObject; const aWake: iSignal; const aCancel: iCancelToken);
      end;
  private
    fIntervalMs: Cardinal;
    fMode: TTimerMode;
    fEnabled: Boolean;
    fOnTimer: TNotifyEvent;
    fLock: TCriticalSection;
    fThread: TTimerThread;
    fWake: iSignal;
    fCancel: iCancelToken;

    // user data
    fTag: NativeInt;
    fDataPtr: Pointer;
    fDataIntf: IInterface;

    function  GetInterval: Cardinal;
    procedure SetInterval(const aValue: Cardinal);
    procedure SetEnabled(const aValue: Boolean);
    procedure SetOnTimer(const aValue: TNotifyEvent);
    function  GetMode: TTimerMode;
    procedure SetMode(const aValue: TTimerMode);
    function  GetTag: NativeInt;
    procedure SetTag(const aValue: NativeInt);
    function  GetDataPtr: Pointer;
    procedure SetDataPtr(const aValue: Pointer);
    function  GetDataIntf: IInterface;
    procedure SetDataIntf(const aValue: IInterface);

    procedure Kick; inline;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // milliseconds, coerced to >=1
    property Interval: Cardinal read GetInterval write SetInterval;

    // run state
    property Enabled: Boolean read fEnabled write SetEnabled;

    // timer cadence
    property SchedulingMode: TTimerMode read GetMode write SetMode; // tmFixedDelay/tmFixedRate

    // callback
    property OnTimer: TNotifyEvent read fOnTimer write SetOnTimer;

    // read-only exposures
    property CancelToken: iCancelToken read fCancel;

    // user payloads
    property Tag: NativeInt read GetTag write SetTag;
    property DataPtr: Pointer read GetDataPtr write SetDataPtr;
    property DataIntf: IInterface read GetDataIntf write SetDataIntf;
  end;


  TPortableTimer = class(TThreadedTimer)
  public
    constructor Create; reintroduce;
    procedure Start(const aIntervalMs: Cardinal);
    procedure Stop;
  end;

implementation

{ TThreadedTimer.TTimerThread }

constructor TThreadedTimer.TTimerThread.Create(aOwner: TObject; const aWake: iSignal; const aCancel: iCancelToken);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  fOwner  := aOwner;
  fWake   := aWake;
  fCancel := aCancel;
end;

procedure TThreadedTimer.TTimerThread.Execute;
var
  lHandler: TNotifyEvent;
  lInterval: Cardinal;
  lEnabled: Boolean;
  lMode: TTimerMode;
  lRes: TWaitResult;
  lOwner: TThreadedTimer;

  function NowMs: QWord; inline;
  begin
    Result := GetTickCount64;
  end;

  procedure SnapshotState;
  begin
    lOwner.fLock.Acquire;
    try
      lEnabled  := lOwner.fEnabled;
      lInterval := lOwner.fIntervalMs;
      lHandler  := lOwner.fOnTimer;
      lMode     := lOwner.fMode;
    finally
      lOwner.fLock.Release;
    end;
    if lInterval = 0 then
      lInterval := 1;
  end;

var
  lNextDue: QWord;
  lNow: QWord;
  lWait: Cardinal;
begin
  lOwner := TThreadedTimer(fOwner);

  // initial snapshot & next due time
  SnapshotState;
  lNextDue := NowMs + lInterval;

  while (not fCancel.Canceled) and (not Terminated) do
  begin
    // if disabled or no handler, idle with a longer wait but still wake-able
    if (not lEnabled) or (not Assigned(lHandler)) then
    begin
      lRes := lOwner.fWake.WaitForSignaled(1000);
      if fCancel.Canceled or Terminated then Break;
      // state might have changed; refresh and set next due relative to now
      SnapshotState;
      lNextDue := NowMs + lInterval;
      Continue;
    end;

    // compute remaining time until next due (for fixed-rate alignment)
    lNow := NowMs;
    if lNow >= lNextDue then
      lWait := 0
    else
    begin
      // clamp to Cardinal range for WaitForSignaled
      if (lNextDue - lNow) > High(Cardinal) then
        lWait := High(Cardinal)
      else
        lWait := Cardinal(lNextDue - lNow);
    end;

    lRes := lOwner.fWake.WaitForSignaled(lWait);
    if fCancel.Canceled or Terminated then Break;

    // refresh state after the wait (OnTimer could have been reassigned)
    SnapshotState;

    // Time to fire if we actually timed out (i.e., reached due time)
    if (lRes = wrTimeout) and lEnabled and Assigned(lHandler) then
    begin
      try
        lHandler(lOwner);
      except
        // swallow/log to keep the timer alive
      end;

      // schedule next tick based on selected mode
      if lMode = tmFixedDelay then
        lNextDue := NowMs + lInterval            // fixed delay after completion
      else
      begin
        // fixed rate: keep cadence relative to prior scheduled time
        // advance by one or more intervals to avoid drift if we overran
        repeat
          lNextDue := lNextDue + lInterval;
        until lNextDue > NowMs;
      end;
    end
    else
    begin
      // woke early (Kick or state change) -> recompute next due conservatively
      // so changes apply promptly
      lNextDue := NowMs + lInterval;
    end;
  end;
end;

{ TThreadedTimer }

constructor TThreadedTimer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fLock := TCriticalSection.Create;
  fWake := TSignal.Create(True{auto-reset});
  fCancel := TCancelToken.Create;
  fIntervalMs := 1000;
  fMode := tmFixedDelay;
  fEnabled := False;

  fTag := 0;
  fDataPtr := nil;
  fDataIntf := nil;

  fThread := TTimerThread.Create(Self, fWake, fCancel);
end;

destructor TThreadedTimer.Destroy;
var
  lThread: TTimerThread;
  lIsSelf: Boolean;
begin
  // Detach local reference to the thread under lock
  fLock.Acquire;
  try
    lThread := fThread;
    fThread := nil;
  finally
    fLock.Release;
  end;

  if lThread <> nil then
  begin
    // cooperatively cancel & wake
    fCancel.Cancel;
    fWake.SetSignaled;

    lIsSelf := (TThread.CurrentThread.ThreadID = lThread.ThreadID);
    if lIsSelf then
    begin
      // Freeing from inside OnTimer: let the worker free itself
      lThread.FreeOnTerminate := True;
      // no WaitFor on self-thread
    end else begin
      lThread.WaitFor;   // deterministic shutdown (Delphi/FPC)
      lThread.Free;
    end;
  end;

  fWake := nil;   // release signal
  fCancel := nil; // release token
  fLock.Free;
  inherited Destroy;
end;

function TThreadedTimer.GetInterval: Cardinal;
begin
  fLock.Acquire;
  try
    Result := fIntervalMs;
  finally
    fLock.Release;
  end;
end;

procedure TThreadedTimer.SetInterval(const aValue: Cardinal);
var
  lValue: Cardinal;
begin
  lValue := aValue;
  if lValue = 0 then
    lValue := 1;

  fLock.Acquire;
  try
    if fIntervalMs = lValue then Exit;
    fIntervalMs := lValue;
  finally
    fLock.Release;
  end;
  Kick; // wake so the new interval applies immediately
end;

procedure TThreadedTimer.SetEnabled(const aValue: Boolean);
begin
  fLock.Acquire;
  try
    if fEnabled = aValue then Exit;
    fEnabled := aValue;
  finally
    fLock.Release;
  end;
  Kick; // wake to apply state change now
end;

function TThreadedTimer.GetMode: TTimerMode;
begin
  fLock.Acquire;
  try
    Result := fMode;
  finally
    fLock.Release;
  end;
end;

procedure TThreadedTimer.SetMode(const aValue: TTimerMode);
begin
  fLock.Acquire;
  try
    if fMode = aValue then Exit;
    fMode := aValue;
  finally
    fLock.Release;
  end;
  Kick; // re-align cadence immediately
end;

procedure TThreadedTimer.SetOnTimer(const aValue: TNotifyEvent);
begin
  fLock.Acquire;
  try
    fOnTimer := aValue;
  finally
    fLock.Release;
  end;
  Kick; // wake so the new handler is seen immediately
end;

function TThreadedTimer.GetTag: NativeInt;
begin
  fLock.Acquire;
  try
    Result := fTag;
  finally
    fLock.Release;
  end;
end;

procedure TThreadedTimer.SetTag(const aValue: NativeInt);
begin
  fLock.Acquire;
  try
    fTag := aValue;
  finally
    fLock.Release;
  end;
end;

function TThreadedTimer.GetDataPtr: Pointer;
begin
  fLock.Acquire;
  try
    Result := fDataPtr;
  finally
    fLock.Release;
  end;
end;

procedure TThreadedTimer.SetDataPtr(const aValue: Pointer);
begin
  fLock.Acquire;
  try
    fDataPtr := aValue;
  finally
    fLock.Release;
  end;
end;

function TThreadedTimer.GetDataIntf: IInterface;
begin
  fLock.Acquire;
  try
    Result := fDataIntf;
  finally
    fLock.Release;
  end;
end;

procedure TThreadedTimer.SetDataIntf(const aValue: IInterface);
begin
  fLock.Acquire;
  try
    fDataIntf := aValue; // interface refcounting handled automatically
  finally
    fLock.Release;
  end;
  // no Kick needed
end;

procedure TThreadedTimer.Kick;
begin
  if fWake <> nil then
    fWake.SetSignaled;
end;

{ TPortableTimer }

constructor TPortableTimer.Create;
begin
  inherited Create(nil);
end;

procedure TPortableTimer.Start(const aIntervalMs: Cardinal);
begin
  Interval := aIntervalMs;
  Enabled := True;
end;

procedure TPortableTimer.Stop;
begin
  Enabled := False;
end;

end.
