unit maxCallMeLater;

{ This allows you to prepare special Callback that are just called after some time
  the current implementation works on timers, but the next step will be to implement asyncCalls as well

  Version: 1.4
  History:
  2018-05-17: fix: if the timer was destroyed by its owner it will not be destroyed again...
  2016-11-15: TTimer needs a owner to prevent execution of "dead" code
  2015-01-19: is now thread safe

  Attention: After the TCallMeLaterData is executed it will be freed automatically }
interface

uses
  windows, classes, sysUtils, forms, controls, stdctrls, extCtrls, messages, ComCtrls, generics.collections, maxAsync;

type
  TCallMeProc = reference to procedure;

  TCallMeLaterData = class(TComponent)
  private
    fTimer: TTimer;
    FProc: TCallMeProc;
    FCallMeAfterMiliseconds: dword;
    fWasDestroyedInfo: ^boolean;
    procedure SetaProc(const Value: TCallMeProc);
    procedure SetCallMeAfterMiliseconds(const Value: dword);

    procedure TimerOnTimer(sender: Tobject);
    Procedure RunInTimerMode;
  public
    constructor Create(aOwner: TComponent); override;
    Destructor Destroy; override;

    Procedure run;

    property CallMeAfterMiliseconds: dword read FCallMeAfterMiliseconds write SetCallMeAfterMiliseconds;
    property Proc: TCallMeProc read FProc write SetaProc;
  end;

  // a owner is required to kill the execution in case the parent is destroyed!
Procedure CallmeLater(aProc: TCallMeProc; Miliseconds: dword; aOwner: TComponent);

implementation


Procedure CallmeLater(aProc: TCallMeProc; Miliseconds: dword; aOwner: TComponent);
var
  cd: TCallMeLaterData;
begin
  cd := TCallMeLaterData.Create(aOwner);
  cd.CallMeAfterMiliseconds := Miliseconds;
  cd.Proc := aProc;
  cd.run;
end;

{ TCallMeLaterData }

constructor TCallMeLaterData.Create;
begin
  inherited Create(aOwner);

end;

destructor TCallMeLaterData.Destroy;
begin
  if fWasDestroyedInfo <> nil then
    fWasDestroyedInfo^ := true;
  inherited;
end;

procedure TCallMeLaterData.run;
begin
  if maxAsync.InsideMainThread then
    RunInTimerMode
  else
  begin
    maxAsync.SyncVCLCall(
      procedure
      begin
        RunInTimerMode
      end, true);
  end;
end;

procedure TCallMeLaterData.RunInTimerMode;
begin
  if not Assigned(fTimer) then
    fTimer := TTimer.Create(self);

  fTimer.interval := CallMeAfterMiliseconds;
  fTimer.OnTimer := TimerOnTimer;
  fTimer.enabled := true;

end;

procedure TCallMeLaterData.SetaProc(

  const
  Value:
  TCallMeProc);
begin
  FProc := Value;
end;

procedure TCallMeLaterData.SetCallMeAfterMiliseconds(

  const
  Value:
  dword);
begin
  FCallMeAfterMiliseconds := Value;
end;

procedure TCallMeLaterData.TimerOnTimer(sender: Tobject);
var
  WasDestroyedInfo: boolean;
begin
  { NOTE:
    it can happen that the calling of the timer will destroy the owner of this object...
    in that case it is bad to destroy it again right?
    But to know if it was destroyed or not, we need some kind of notification.
    So I use a private pointer to a boolean, that will be set to true in the destructor (only if it points to something of course
    and in this method we will test if it was actually set or not, if not, then we can free this object here }
  WasDestroyedInfo := false;
  fWasDestroyedInfo := @WasDestroyedInfo;

  try
    fTimer.enabled := false;
    if Assigned(FProc) then
      FProc();
  finally
    if not WasDestroyedInfo then
    begin
      fWasDestroyedInfo := nil;
      self.free;
    end;
  end;
end;

end.
