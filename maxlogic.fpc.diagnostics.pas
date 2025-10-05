unit maxlogic.fpc.diagnostics;

{$I fpc_delphimode.inc}

interface

uses
{$IFDEF FPC}
  {$IFDEF MSWINDOWS}
  Windows
  {$ELSE}
  Unix
  {$ENDIF}
{$ELSE}
  System.Diagnostics
{$ENDIF};

{$IFDEF FPC}

type
  TStopwatch = record
  private
    FStartTicks: QWord;
    FElapsedTicks: QWord;
    FRunning: Boolean;
    function GetElapsedTicks: Int64; inline;
    function GetElapsedMilliseconds: Int64; inline;
    class function InternalGetTimeStamp: QWord; static;
  public
    class function StartNew: TStopwatch; static;
    procedure Start; inline;
    procedure Stop; inline;
    procedure Reset; inline;
    procedure Restart; inline;
    property ElapsedTicks: Int64 read GetElapsedTicks;
    property ElapsedMilliseconds: Int64 read GetElapsedMilliseconds;
    class function Frequency: Int64; static;
    class function IsHighResolution: Boolean; static;
    class function GetTimeStamp: QWord; static;
  end;

{$ELSE}

type
  TStopwatch = System.Diagnostics.TStopwatch;

{$ENDIF}

implementation

{$IFDEF FPC}

class function TStopwatch.InternalGetTimeStamp: QWord;
var
{$IFDEF MSWINDOWS}
  c: Int64;
{$ELSE}
  tv: TTimeVal;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  QueryPerformanceCounter(c);
  Result := QWord(c);
{$ELSE}
  FpGetTimeOfDay(@tv, nil);
  Result := QWord(tv.tv_sec) * 1000000 + tv.tv_usec;
{$ENDIF}
end;

class function TStopwatch.GetTimeStamp: QWord;
begin
  Result := InternalGetTimeStamp;
end;

class function TStopwatch.Frequency: Int64;
{$IFDEF MSWINDOWS}
var
  f: Int64;
begin
  QueryPerformanceFrequency(f);
  Result := f;
end;
{$ELSE}
begin
  Result := 1000000;
end;
{$ENDIF}

class function TStopwatch.IsHighResolution: Boolean;
begin
  Result := True;
end;

class function TStopwatch.StartNew: TStopwatch;
begin
  Result.Reset;
  Result.Start;
end;

procedure TStopwatch.Start;
begin
  if not FRunning then
  begin
    FStartTicks := InternalGetTimeStamp;
    FRunning := True;
  end;
end;

procedure TStopwatch.Stop;
begin
  if FRunning then
  begin
    FElapsedTicks := FElapsedTicks + (InternalGetTimeStamp - FStartTicks);
    FRunning := False;
  end;
end;

procedure TStopwatch.Reset;
begin
  FStartTicks := 0;
  FElapsedTicks := 0;
  FRunning := False;
end;

procedure TStopwatch.Restart;
begin
  Reset;
  Start;
end;

function TStopwatch.GetElapsedTicks: Int64;
begin
  if FRunning then
    Result := FElapsedTicks + (InternalGetTimeStamp - FStartTicks)
  else
    Result := FElapsedTicks;
end;

function TStopwatch.GetElapsedMilliseconds: Int64;
begin
  Result := (GetElapsedTicks * 1000) div Frequency;
end;

{$ENDIF}

end.
