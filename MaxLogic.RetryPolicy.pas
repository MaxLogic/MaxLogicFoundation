unit MaxLogic.RetryPolicy;

interface

uses
  System.SysUtils, System.Classes, System.Threading, System.Math;

type
  // Forward declarations
  TRetryPolicy = class;
  ERetryCancelledException = class(Exception);
  ERetryFailedException = class(Exception);

  /// <summary>
  /// Defines how a callback should be invoked, especially in asynchronous scenarios.
  /// </summary>
  TCallbackSync = (
    /// <summary>The callback is executed directly on the current thread (background thread for async).</summary>
    csCurrentThread,
    /// <summary>The callback is marshaled to the main thread via TThread.Synchronize (blocking).</summary>
    csSynchronize,
    /// <summary>The callback is marshaled to the main thread via TThread.ForceQueue (non-blocking).</summary>
    csForceQueue
  );

  // Private implementation detail. Not for public use.
  TRetryOptions = record
  private
    class function GetDefault: TRetryOptions; static;
  public
    RetryCount: Integer;
    InitialDelay: Cardinal;
    MaxDelay: Cardinal;
    DelayIncrement: Cardinal;
    BackoffFactor: Double;
    RaiseOnFailure: Boolean;
    OnSuccess: TThreadProcedure;
    OnSuccessSync: TCallbackSync;
    OnFailure: TProc<Exception>;
    OnFailureSync: TCallbackSync;
    OnRetry: TProc<Cardinal, Cardinal>; // Retry No., Delay Used
    OnRetrySync: TCallbackSync;
  end;

  // Private implementation detail. Manages a single execution run.
  TRetryRunner = class
  private
    fPolicy: TRetryPolicy;
    fOptions: TRetryOptions;
    fAction: TFunc<Boolean>;
    procedure DoInvoke(aProc: TThreadProcedure; aSync: TCallbackSync);
    procedure InvokeFailureProc(aProc: TProc<Exception>; aException: Exception; aSync: TCallbackSync);
    procedure InvokeRetryProc(aProc: TProc<Cardinal, Cardinal>; aRetryNum, aDelay: Cardinal; aSync: TCallbackSync);
    function CalculateDelay(aRetryNum: Cardinal): Cardinal;
    procedure RunSync;
  public
    constructor Create(aPolicy: TRetryPolicy; const aOptions: TRetryOptions; const aAction: TFunc<Boolean>);
    class procedure ExecuteSync(aPolicy: TRetryPolicy; const aOptions: TRetryOptions; const aAction: TFunc<Boolean>);
    class function ExecuteAsync(aPolicy: TRetryPolicy; const aOptions: TRetryOptions; const aAction: TFunc<Boolean>): ITask;
  end;

  /// <summary>
  /// Creates and configures a retry policy for executing actions that may fail.
  /// This class uses a fluent interface and is the sole entry point for the library.
  /// </summary>
  TRetryPolicy = class
  private
    fOptions: TRetryOptions;
    fCancelled: Boolean;
    class function WrapProc(aProc: TProc): TFunc<Boolean>; overload;
    class function WrapProc<T>(aProc: TProc<T>; aValue: T): TFunc<Boolean>; overload;
  public
    constructor Create;

    /// <summary>Sets the number of times to retry after an initial failure. 0 means no retries.</summary>
    function WithRetries(aCount: Integer): TRetryPolicy;

    /// <summary>Sets the initial delay in milliseconds before the first retry.</summary>
    function WithInitialDelay(aMilliseconds: Cardinal): TRetryPolicy;

    /// <summary>Sets a linear increment for the delay after each attempt.</summary>
    function WithDelayIncrement(aMilliseconds: Cardinal): TRetryPolicy;

    /// <summary>Sets an exponential backoff factor for the delay. Overrides linear increment.</summary>
    function WithExponentialBackoff(aFactor: Double = 1.5): TRetryPolicy;

    /// <summary>Sets the maximum possible delay between retries.</summary>
    function WithMaxDelay(aMilliseconds: Cardinal): TRetryPolicy;

    /// <summary>Determines if an exception is raised on final failure. Default is True.</summary>
    function RaiseOnFinalFailure(aValue: Boolean): TRetryPolicy;

    /// <summary>A callback to execute on success.</summary>
    function OnSuccess(aProc: TThreadProcedure; aSync: TCallbackSync = csCurrentThread): TRetryPolicy;

    /// <summary>A callback to execute on final failure. Receives the last exception.</summary>
    function OnFailure(aProc: TProc<Exception>; aSync: TCallbackSync = csCurrentThread): TRetryPolicy;

    /// <summary>A callback executed before each retry attempt.</summary>
    function OnRetry(aProc: TProc<Cardinal, Cardinal>; aSync: TCallbackSync = csCurrentThread): TRetryPolicy;

    /// <summary>Signals the running policy to terminate before the next attempt.</summary>
    procedure Cancel;
    /// <summary>Indicates if the Cancel method has been called.</summary>
    property IsCancelled: Boolean read fCancelled;

    { Synchronous Execution }
    procedure Execute(aAction: TFunc<Boolean>); overload;
    procedure Execute(aProc: TProc); overload;
    procedure Execute<T>(aProc: TProc<T>; aValue: T); overload;

    { Asynchronous Execution }
    function ExecuteAsync(aAction: TFunc<Boolean>): ITask; overload;
    function ExecuteAsync(aProc: TProc): ITask; overload;
    function ExecuteAsync<T>(aProc: TProc<T>; aValue: T): ITask; overload;
  end;

// quick access methods
procedure Retry(aAction: TFunc<Boolean>; aRetryCount: Cardinal = 3; aDelay: Cardinal = 100); overload;
procedure Retry(aProc: TProc; aRetryCount: Cardinal = 3; aDelay: Cardinal = 100); overload;
procedure Retry<T>(aProc: TProc<T>; aValue: T; aRetryCount: Cardinal = 3; aDelay: Cardinal = 100); overload;

implementation

uses
  autoFree;

procedure Retry(aAction: TFunc<Boolean>; aRetryCount: Cardinal = 3; aDelay: Cardinal = 100); overload;
var
  lPolicy: TRetryPolicy;
begin
  gc(lPolicy, TRetryPolicy.Create);
  lPolicy.WithRetries(aRetryCount)
    .WithInitialDelay(aDelay)
    .Execute(aAction);
end;
procedure Retry(aProc: TProc; aRetryCount: Cardinal = 3; aDelay: Cardinal = 100); overload;
begin
  Retry(
    TRetryPolicy.WrapProc(aProc),
    aRetryCount, aDelay);
end;

procedure Retry<T>(aProc: TProc<T>; aValue: T; aRetryCount: Cardinal = 3; aDelay: Cardinal = 100); overload;
begin
  Retry(
    TRetryPolicy.WrapProc<T>(aProc, aValue),
    aRetryCount, aDelay);
end;

{ TRetryOptions }

class function TRetryOptions.GetDefault: TRetryOptions;
begin
  Result.RetryCount := 2; // Default to 2 retries (3 total attempts)
  Result.InitialDelay := 100;
  Result.MaxDelay := 0;
  Result.DelayIncrement := 0;
  Result.BackoffFactor := 0;
  Result.RaiseOnFailure := True;
  Result.OnSuccess := nil;
  Result.OnSuccessSync := csCurrentThread;
  Result.OnFailure := nil;
  Result.OnFailureSync := csCurrentThread;
  Result.OnRetry := nil;
  Result.OnRetrySync := csCurrentThread;
end;

{ TRetryRunner }

constructor TRetryRunner.Create(aPolicy: TRetryPolicy; const aOptions: TRetryOptions; const aAction: TFunc<Boolean>);
begin
  inherited Create;
  fPolicy := aPolicy;
  fOptions := aOptions;
  fAction := aAction;
end;

class procedure TRetryRunner.ExecuteSync(aPolicy: TRetryPolicy; const aOptions: TRetryOptions; const aAction: TFunc<Boolean>);
var
  lRunner: TRetryRunner;
begin
  lRunner := TRetryRunner.Create(aPolicy, aOptions, aAction);
  try
    lRunner.RunSync;
  finally
    lRunner.Free;
  end;
end;

class function TRetryRunner.ExecuteAsync(-aPolicy: TRetryPolicy; const aOptions: TRetryOptions; const aAction: TFunc<Boolean>): ITask;
begin
  Result := TTask.Run(
    procedure
    var
      lRunner: TRetryRunner;
    begin
      lRunner := TRetryRunner.Create(aPolicy, aOptions, aAction);
      try
        lRunner.RunSync;
      finally
        lRunner.Free;
      end;
    end);
end;

// Centralized marshalling logic to avoid repetition.
procedure TRetryRunner.DoInvoke(aProc: TThreadProcedure; aSync: TCallbackSync);
begin
  case aSync of
    csCurrentThread: aProc();
    csSynchronize: TThread.Synchronize(nil, aProc);
    csForceQueue: TThread.ForceQueue(nil, aProc);
  end;
end;

procedure TRetryRunner.InvokeFailureProc(aProc: TProc<Exception>; aException: Exception; aSync: TCallbackSync);
begin
  if Assigned(aProc) then
  begin
    // Wrap the call in a parameterless procedure to satisfy Synchronize/ForceQueue.
    DoInvoke(procedure
      begin
        aProc(aException);
      end, aSync);
  end;
end;

procedure TRetryRunner.InvokeRetryProc(aProc: TProc<Cardinal, Cardinal>; aRetryNum, aDelay: Cardinal; aSync: TCallbackSync);
begin
  if Assigned(aProc) then
  begin
    // Wrap the call in a parameterless procedure.
    DoInvoke(procedure
      begin
        aProc(aRetryNum, aDelay);
      end, aSync);
  end;
end;

procedure TRetryRunner.RunSync;
var
  lLastException: Exception;
  lAttempt: Cardinal;
  lDelay: Cardinal;
begin
  lLastException := nil;

  for lAttempt := 0 to Cardinal(fOptions.RetryCount) do
  begin
    if fPolicy.IsCancelled then
    begin
      lLastException := ERetryCancelledException.Create('Retry policy was cancelled.');
      Break;
    end;

    // On subsequent attempts (retries), handle the pre-retry logic
    if lAttempt > 0 then
    begin
      lDelay := CalculateDelay(lAttempt);
      InvokeRetryProc(fOptions.OnRetry, lAttempt, lDelay, fOptions.OnRetrySync);

      if lDelay > 0 then
        TThread.Sleep(lDelay); // Use TThread.Sleep for clarity.

      // Check for cancellation again after sleeping
      if fPolicy.IsCancelled then
      begin
        lLastException := ERetryCancelledException.Create('Retry policy was cancelled.');
        Break;
      end;
    end;

    try
      if fAction() then
      begin
        if assigned(fOptions.OnSuccess) then
          DoInvoke(fOptions.OnSuccess, fOptions.OnSuccessSync);
        lLastException := nil; // Clear any previous exception on success.
        Exit; // Success.
      end
      else
      begin
        // The action returned false, which is a failure condition.
        // We create a generic exception if one wasn't already thrown.
        if not Assigned(lLastException) then
          lLastException := ERetryFailedException.Create('Action returned false.');
      end;
    except
      on E: Exception do
        lLastException := E;
    end;
  end;

  // If we've exited the loop, all retries have failed or were cancelled.
  InvokeFailureProc(fOptions.OnFailure, lLastException, fOptions.OnFailureSync);

  if fOptions.RaiseOnFailure then
  begin
    if Assigned(lLastException) then
      raise lLastException;

    // This case should only be hit if the action returns false and no exception is ever assigned.
    raise ERetryFailedException.Create('Action failed to return success after all retries.');
  end;
end;

function TRetryRunner.CalculateDelay(aRetryNum: Cardinal): Cardinal;
begin
  if fOptions.BackoffFactor > 1.0 then
    Result := Round(fOptions.InitialDelay * Power(fOptions.BackoffFactor, aRetryNum - 1))
  else
    Result := fOptions.InitialDelay + (fOptions.DelayIncrement * (aRetryNum - 1));

  if (fOptions.MaxDelay > 0) and (Result > fOptions.MaxDelay) then
    Result := fOptions.MaxDelay;
end;

{ TRetryPolicy }

constructor TRetryPolicy.Create;
begin
  inherited Create;
  fOptions := TRetryOptions.GetDefault;
  fCancelled := False;
end;

class function TRetryPolicy.New: TRetryPolicy;
begin
  Result := TRetryPolicy.Create;
end;

procedure TRetryPolicy.Cancel;
begin
  fCancelled := True;
end;

class function TRetryPolicy.WrapProc(aProc: TProc): TFunc<Boolean>;
begin
  Result := function: Boolean
    begin
      aProc();
      Result := True;
    end;
end;

class function TRetryPolicy.WrapProc<T>(aProc: TProc<T>; aValue: T): TFunc<Boolean>;
begin
  Result := function: Boolean
    begin
      aProc(aValue);
      Result := True;
    end;
end;

function TRetryPolicy.WithRetries(aCount: Integer): TRetryPolicy;
begin
  Result := Self;
  if aCount < 0 then
    fOptions.RetryCount := 0
  else
    fOptions.RetryCount := aCount;
end;

function TRetryPolicy.WithInitialDelay(aMilliseconds: Cardinal): TRetryPolicy;
begin
  Result := Self;
  fOptions.InitialDelay := aMilliseconds;
end;

function TRetryPolicy.WithDelayIncrement(aMilliseconds: Cardinal): TRetryPolicy;
begin
  Result := Self;
  fOptions.DelayIncrement := aMilliseconds;
end;

function TRetryPolicy.WithExponentialBackoff(aFactor: Double): TRetryPolicy;
begin
  Result := Self;
  fOptions.BackoffFactor := aFactor;
end;

function TRetryPolicy.WithMaxDelay(aMilliseconds: Cardinal): TRetryPolicy;
begin
  Result := Self;
  fOptions.MaxDelay := aMilliseconds;
end;

function TRetryPolicy.RaiseOnFinalFailure(aValue: Boolean): TRetryPolicy;
begin
  Result := Self;
  fOptions.RaiseOnFailure := aValue;
end;

function TRetryPolicy.OnSuccess(aProc: TThreadProcedure; aSync: TCallbackSync): TRetryPolicy;
begin
  Result := Self;
  fOptions.OnSuccess := aProc;
  fOptions.OnSuccessSync := aSync;
end;

function TRetryPolicy.OnFailure(aProc: TProc<Exception>; aSync: TCallbackSync): TRetryPolicy;
begin
  Result := Self;
  fOptions.OnFailure := aProc;
  fOptions.OnFailureSync := aSync;
end;

function TRetryPolicy.OnRetry(aProc: TProc<Cardinal, Cardinal>; aSync: TCallbackSync): TRetryPolicy;
begin
  Result := Self;
  fOptions.OnRetry := aProc;
  fOptions.OnRetrySync := aSync;
end;

procedure TRetryPolicy.Execute(aAction: TFunc<Boolean>);
begin
  TRetryRunner.ExecuteSync(Self, fOptions, aAction);
end;

procedure TRetryPolicy.Execute(aProc: TProc);
begin
  TRetryRunner.ExecuteSync(Self, fOptions, WrapProc(aProc));
end;

procedure TRetryPolicy.Execute<T>(aProc: TProc<T>; aValue: T);
begin
  TRetryRunner.ExecuteSync(Self, fOptions, Self.WrapProc<T>(aProc, aValue));
end;

function TRetryPolicy.ExecuteAsync(aAction: TFunc<Boolean>): ITask;
begin
  Result := TRetryRunner.ExecuteAsync(Self, fOptions, aAction);
end;

function TRetryPolicy.ExecuteAsync(aProc: TProc): ITask;
begin
  Result := TRetryRunner.ExecuteAsync(Self, fOptions, WrapProc(aProc));
end;

function TRetryPolicy.ExecuteAsync<T>(aProc: TProc<T>; aValue: T): ITask;
begin
  Result := TRetryRunner.ExecuteAsync(Self, fOptions, Self.WrapProc<T>(aProc, aValue));
end;

end.

