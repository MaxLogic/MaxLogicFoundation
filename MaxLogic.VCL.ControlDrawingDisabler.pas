Unit MaxLogic.VCL.ControlDrawingDisabler;

{ Version: 1.02 }
Interface

Uses
  windows, sysUtils, classes, controls, StdCtrls, messages;

Type

  // when adding or moving many controls on a form it is best to disable its drawing and realigning first, this class  are for that purpose
  // NOTE: it implements a counter, so you just need to ensure you call unLockDrawing for every call to LockDrawing
  // Threading note: no threading allowed, always run from the main vcl thread

  TControlDrawingDisabler = Class
  Private Type
    TDrawingDisablerFlag = Class(TComponent)
    Private
      Count: Integer;
    End;
  Private
    Class Function getOrCreateCounter(aControl: TWinControl)
      : TDrawingDisablerFlag;

    // returns the value after the modification
    Class Function incCounter(aControl: TWinControl): Integer;
    Class Function decCounter(aControl: TWinControl): Integer;
  Public
    Class Procedure LockDrawing(aControl: TWinControl);
    Class Procedure UnLockDrawing(aControl: TWinControl);
  End;

Implementation

Class Procedure TControlDrawingDisabler.LockDrawing(aControl: TWinControl);
Begin

  If assigned(aControl)
    And (Not (csDestroyingHandle In aControl.ControlState))
    And (Not (csDestroying In aControl.ComponentState))
    And (aControl.HandleAllocated)
    And (incCounter(aControl) = 1)
  Then
  Begin
    SendMessage(aControl.Handle, WM_SETREDRAW, Integer(False), 0);
    aControl.DisableAlign;
  End;
End;

Class Procedure TControlDrawingDisabler.UnLockDrawing(aControl: TWinControl);
Begin
  If assigned(aControl)
    And (Not (csDestroyingHandle In aControl.ControlState))
    And (Not (csDestroying In aControl.ComponentState))
    And (aControl.HandleAllocated)
    And (decCounter(aControl) = 0)
  Then
  Begin
    aControl.EnableAlign;
    SendMessage(aControl.Handle, WM_SETREDRAW, Integer(true), 0);
    aControl.invalidate;
    RedrawWindow(aControl.Handle, Nil, 0, RDW_INVALIDATE Or RDW_UPDATENOW Or RDW_ALLCHILDREN);
  End;
End;

Class Function TControlDrawingDisabler.getOrCreateCounter(aControl: TWinControl)
  : TDrawingDisablerFlag;
Var
  x: Integer;
Begin
  // most likely it is the last component in the list...
  For x := aControl.ComponentCount - 1 Downto 0 Do
    If aControl.Components[x] Is TDrawingDisablerFlag Then
    Begin
      exit(aControl.Components[x] As TDrawingDisablerFlag);
    End;
  // if we are here... nothing was found, so create a new one
  result := TDrawingDisablerFlag.Create(aControl);
End;

Class Function TControlDrawingDisabler.incCounter
  (aControl: TWinControl): Integer;
Var
  Counter: TDrawingDisablerFlag;
Begin
  Counter := getOrCreateCounter(aControl);
  inc(Counter.Count);
  result := Counter.Count;
End;

Class Function TControlDrawingDisabler.decCounter
  (aControl: TWinControl): Integer;
Var
  Counter: TDrawingDisablerFlag;
Begin
  Counter := getOrCreateCounter(aControl);
  dec(Counter.Count);
  result := Counter.Count;
End;

End.
