unit maxLogic.vcl.ControlDrawingDisabler;

{ Version: 1.02 }
interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, Messages;

type

  // when adding or moving many controls on a form it is best to disable its drawing and realigning first, this class  are for that purpose
  // NOTE: it implements a counter, so you just need to ensure you call unLockDrawing for every call to LockDrawing
  // Threading note: no threading allowed, always run from the main vcl thread

  TControlDrawingDisabler = class
  private type
      TDrawingDisablerFlag = class(TComponent)
      private
        Count: integer;
      end;
  private
    class function GetCounter(aControl: TWinControl; aCanCreate: boolean = True): TDrawingDisablerFlag;

    // returns the value after the modification
    class function incCounter(aControl: TWinControl): integer;
    class function decCounter(aControl: TWinControl): integer;
  public
    class procedure LockDrawing(aControl: TWinControl);
    class procedure UnLockDrawing(aControl: TWinControl);
    class function islocked(aControl: TWinControl): boolean;
  end;

implementation

class procedure TControlDrawingDisabler.LockDrawing(aControl: TWinControl);
begin
  if assigned(aControl)
    and (not (csDestroyingHandle in aControl.ControlState))
    and (not (csDestroying in aControl.ComponentState))
    and (aControl.HandleAllocated)
    and (incCounter(aControl) = 1) then
  begin
    SendMessage(aControl.Handle, WM_SETREDRAW, integer(False), 0);
    aControl.DisableAlign;
  end;
end;

class procedure TControlDrawingDisabler.UnLockDrawing(aControl: TWinControl);
begin
  if assigned(aControl)
    and (not (csDestroyingHandle in aControl.ControlState))
    and (not (csDestroying in aControl.ComponentState))
    and (aControl.HandleAllocated)
    and (decCounter(aControl) = 0) then
  begin
    aControl.EnableAlign;
    SendMessage(aControl.Handle, WM_SETREDRAW, integer(True), 0);
    aControl.Invalidate;
    RedrawWindow(aControl.Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);
  end;
end;

class function TControlDrawingDisabler.GetCounter(aControl: TWinControl; aCanCreate: boolean): TDrawingDisablerFlag;
begin
  Result := nil;
  // most likely it is the last component in the list...
  for var X := aControl.ComponentCount - 1 downto 0 do
    if aControl.Components[X] is TDrawingDisablerFlag then
    begin
      exit(aControl.Components[X] as TDrawingDisablerFlag);
    end;
  // if we are here... nothing was found, so create a new one
  if aCanCreate then
    Result := TDrawingDisablerFlag.Create(aControl);
end;

class function TControlDrawingDisabler.incCounter(aControl: TWinControl): integer;
var
  lCounter: TDrawingDisablerFlag;
begin
  lCounter := GetCounter(aControl);
  Inc(lCounter.Count);
  Result := lCounter.Count;
end;

class function TControlDrawingDisabler.islocked(
  aControl: TWinControl): boolean;
var
  lCounter: TDrawingDisablerFlag;
begin
  Result := False;
  if assigned(aControl)
    and (not (csDestroyingHandle in aControl.ControlState))
    and (not (csDestroying in aControl.ComponentState))
    and (aControl.HandleAllocated) then
  begin
    lCounter := GetCounter(aControl);
    Result := assigned(lCounter)
      and (lCounter.Count > 0);
  end;
end;

class function TControlDrawingDisabler.decCounter(aControl: TWinControl): integer;
var
  lCounter: TDrawingDisablerFlag;
begin
  lCounter := GetCounter(aControl);
  Dec(lCounter.Count);
  Result := lCounter.Count;
end;

end.

