unit maxCallMeLater;

{ Prepare a callback that runs after some time.
  Current impl: VCL TTimer on the main thread.

  Version: 1.6
  History:
  2025-09-22: use CancelToken
  2018-05-17: avoid double-destroy when owner frees us
  2016-11-15: TTimer needs an owner to prevent execution of "dead" code
  2015-01-19: thread-safe
  Attention: After the TCallMeLaterData is executed it will be freed automatically }

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, ExtCtrls, // trimmed: removed unused units
  maxAsync,
  CancelToken;

type
  TCallMeProc = reference to procedure;

  TCallMeLaterData = class(TComponent)
  private
    fTimer: TTimer;
    fProc: TCallMeProc;
    fCallMeAfterMiliseconds: DWORD; // keep original public API spelling
    fWasDestroyedInfo: iCancelToken;

    procedure SetAProc(const aValue: TCallMeProc);
    procedure SetCallMeAfterMiliseconds(const aValue: DWORD);

    procedure TimerOnTimer(Sender: TObject);
    procedure RunInTimerMode;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure Run;

    property CallMeAfterMiliseconds: DWORD read fCallMeAfterMiliseconds write SetCallMeAfterMiliseconds;
    property Proc: TCallMeProc read fProc write SetAProc;
  end;

{ aOwner is recommended so that if the owner is destroyed before the timer fires,
  we won't execute "dead" code. }
procedure CallmeLater(aProc: TCallMeProc; Miliseconds: DWORD; aOwner: TComponent);

implementation

uses
  system.math;

procedure CallmeLater(aProc: TCallMeProc; Miliseconds: DWORD; aOwner: TComponent);
var
  lData: TCallMeLaterData;
begin
  lData := TCallMeLaterData.Create(aOwner);
  lData.CallMeAfterMiliseconds := Miliseconds;
  lData.Proc := aProc;
  lData.Run;
end;

{ TCallMeLaterData }

constructor TCallMeLaterData.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fWasDestroyedInfo := TCancelToken.Create;
end;

destructor TCallMeLaterData.Destroy;
begin
  if Assigned(fTimer) then
  begin
    fTimer.OnTimer := nil;
    fTimer.Enabled := False;
  end;
  if (fWasDestroyedInfo <> nil) and not fWasDestroyedInfo.Canceled then
    fWasDestroyedInfo.Cancel;
  inherited;
end;

procedure TCallMeLaterData.Run;
var
  lWasDestroyed: iCancelToken;
begin
  if maxAsync.InsideMainThread then
    RunInTimerMode
  else
  begin
    lWasDestroyed:= fWasDestroyedInfo;
    maxAsync.SyncVCLCall(
      procedure
      begin
        if not lWasDestroyed.Canceled then
          RunInTimerMode;
      end,
      True);
  end;
end;

procedure TCallMeLaterData.RunInTimerMode;
var
  lInterval: Cardinal;
begin
  // already destroyed?
  if (fWasDestroyedInfo <> nil) and fWasDestroyedInfo.Canceled then
    Exit;

  if fTimer = nil then
    fTimer := TTimer.Create(Self);

  // Clamp 0 to 1 to avoid aggressive WM_TIMER behavior.
  lInterval := Max(1, fCallMeAfterMiliseconds);

  fTimer.Enabled := False;
  fTimer.Interval := lInterval;
  fTimer.OnTimer := TimerOnTimer;
  fTimer.Enabled := True;
end;

procedure TCallMeLaterData.SetAProc(const aValue: TCallMeProc);
begin
  fProc := aValue;
end;

procedure TCallMeLaterData.SetCallMeAfterMiliseconds(const aValue: DWORD);
begin
  fCallMeAfterMiliseconds := aValue;
end;

procedure TCallMeLaterData.TimerOnTimer(Sender: TObject);
var
  lWasDestroyedInfo: iCancelToken;
begin
  // It can happen that invoking Proc destroys this object via the owner.
  // Use a cancel token snapshot to detect that scenario.
  lWasDestroyedInfo := fWasDestroyedInfo;

  try
    if Assigned(fTimer) then
      fTimer.Enabled := False;

    if Assigned(fProc) then
      fProc();
  finally
    // If we weren't destroyed during Proc(), self-destroy now (one-shot).
    if (lWasDestroyedInfo <> nil) and not lWasDestroyedInfo.Canceled then
      Self.Free;
  end;
end;

end.
