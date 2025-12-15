unit maxThreadedLoop;

{ Version: 1.1

  Description: a threaded timer }

interface

uses
  windows, classes, sysUtils, generics.collections, maxAsync, diagnostics;

type
  TThreadedLoop = class
  private
    fAsync: iAsync;
    fStopWatch: TStopWatch;
    fSignal: iSignal;
    fStopped: boolean;
    fMillisecondsSinceLastLoop: TSafeValue<int64>;
    fInterval: integer;
    function GetMillisecondsSinceLastLoop: int64;
    procedure run;
  protected
    fSec: TFixedCriticalSection;
    property isStopped:boolean read fStopped;

    // all 3 are called in the thread context
    procedure Loopbegin; virtual; Abstract;
    procedure LoopIteration(var Quit: boolean); virtual; Abstract;
    procedure LoopEnd; virtual; Abstract;

  public
    constructor Create(Interval: integer);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property MillisecondsSinceLastLoop: int64 read GetMillisecondsSinceLastLoop;
  end;

implementation


{ TThreadedLoop }

constructor TThreadedLoop.Create(Interval: integer);

begin
  inherited Create;
  fStopWatch := TStopWatch.startNew;
  fInterval := Interval;
  fSec := TFixedCriticalSection.Create;
  fMillisecondsSinceLastLoop := TSafeValue<int64>.New;
  fSignal := TSignal.Create;
end;

destructor TThreadedLoop.Destroy;
begin
  Stop;
  fSec.free;
  inherited;
end;

function TThreadedLoop.GetMillisecondsSinceLastLoop: int64;
begin
  result := fStopWatch.elapsedmilliseconds - fMillisecondsSinceLastLoop.value;
end;

procedure TThreadedLoop.run;
var
  et: int64;
  Quit: boolean;
  ms: int64;
begin
  Quit := false;
  Loopbegin;
  try
    while not fStopped do
    begin
      et := fStopWatch.elapsedmilliseconds;
      fMillisecondsSinceLastLoop.value := et;

      LoopIteration(Quit);
      if Quit then
        break;

      // to call the next loop exactly on the interval - the required time in the loop
      ms := fStopWatch.elapsedmilliseconds - et;
      if ms < fInterval then
        fSignal.waitforsignaled(fInterval - ms);
    end;
  finally
    LoopEnd;
  end;
end;

procedure TThreadedLoop.Start;
begin
  Stop;

  fAsync := maxAsync.simpleAsyncCall(
    procedure
    begin
      run;
    end, ClassName + '.loop');
end;

procedure TThreadedLoop.Stop;
begin
  if fAsync <> nil then
  begin
    fStopped := true;
    fSignal.SetSignaled;
    fAsync.waitfor;
    fAsync := nil;
  end;
end;

end.
