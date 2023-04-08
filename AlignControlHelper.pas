Unit AlignControlHelper;

{ Version: 1.5 }

Interface

Uses
  windows, sysUtils, controls, ExtCtrls;

Procedure AutoSizeHeight(aControl: TWinControl);
Procedure DeleteAllEmptyRows(aGrid: TGridPanel; aResetGridHeight: boolean = true);
Procedure AdjustTabOrderForControlsInGridPanel(aGrid: TGridPanel);
Function GetControlheightInclMargins(aControl: TControl): integer;
Function GetControlWidthInclMargins(aControl: TControl): integer;
Procedure AlignVerticalCenter(aBase, aMoveThis: TWinControl);
Procedure AlignVerticalBottom(aBase, aMoveThis: TWinControl);

Implementation

Uses
  Math;

Procedure AutoSizeHeight(aControl: TWinControl);
Var
  x: integer;
  h: integer;
  cc: TWinControl;
Begin
  h := 0;
  For x := 0 To aControl.ControlCount - 1 Do
  Begin
    If Not(aControl.controls[x] Is TWinControl) Then
      Continue;

    cc := aControl.controls[x] As TWinControl;
    If cc.visible Then
      h := max(h, cc.top + cc.Height);
  End;
  If h <> 0 Then
    aControl.ClientHeight := h;
End;

Procedure DeleteAllEmptyRows(aGrid: TGridPanel; aResetGridHeight: boolean = true);
Var
  i: integer;
  ci: TControlItem;
  Rowheight: integer;
  IsEmpty: boolean;
  c: integer;
  r: integer;
  CellItem: TCellItem;
  h: integer;
  wc: TControl;
  RealHeight: integer;
Begin
  RealHeight := 0;
  For r := aGrid.RowCollection.Count - 1 Downto 0 Do
  Begin
      Rowheight := 0;
    IsEmpty := true;

    For c := 0 To aGrid.ColumnCollection.Count - 1 Do
    Begin
        wc := aGrid.ControlCollection.controls[c, r];
      If wc = NIL Then
        Continue;

      If wc.visible Then
      Begin
          IsEmpty := false;
        h := wc.Height;
        If wc.alignwithMargins Then
          h := h + wc.Margins.top + wc.Margins.bottom;

        Rowheight := max(Rowheight, h);
      End
      Else
      Begin
          wc.parent := aGrid.parent;
        aGrid.ControlCollection.RemoveControl(wc);
      End;
    End;

    If IsEmpty Then
    Begin
        Rowheight := 0;
    End;
    aGrid.RowCollection.Items[r].SizeStyle := ssAbsolute;
    aGrid.RowCollection.Items[r].Value := Rowheight;

    Inc(RealHeight, Rowheight);
  End;
  If aResetGridHeight Then
    aGrid.ClientHeight := RealHeight;
End;

Procedure AdjustTabOrderForControlsInGridPanel(aGrid: TGridPanel);
Var
  i: integer;
  ci: TControlItem;
  c: integer;
  r: integer;
  CellItem: TCellItem;
  wc: TControl;
  tab: integer;
Begin
  tab := 0;
  For r := 0 To aGrid.RowCollection.Count - 1 Do
    For c := 0 To aGrid.ColumnCollection.Count - 1 Do
    Begin
        wc := aGrid.ControlCollection.controls[c, r];
      If (wc = NIL) Or (Not(wc Is TWinControl)) Then
        Continue;

      (wc As TWinControl).TabOrder := tab;
      Inc(tab);
    End;
End;

Function GetControlheightInclMargins(aControl: TControl): integer;
Begin
  result := aControl.Height;
  If aControl.alignwithMargins Then
    result := result + aControl.Margins.bottom + aControl.Margins.top;
End;

Function GetControlWidthInclMargins(aControl: TControl): integer;
Begin
  result := aControl.width;
  If aControl.alignwithMargins Then
    result := result + aControl.Margins.Left + aControl.Margins.Right;
End;

Procedure AlignVerticalCenter(aBase, aMoveThis: TWinControl);
Begin
  aMoveThis.top :=
    aBase.top +
    round(aBase.Height / 2 - aMoveThis.Height / 2);
End;

Procedure AlignVerticalBottom(aBase, aMoveThis: TWinControl);
Begin
  aMoveThis.top :=
    (aBase.top + aBase.Height) -
    aMoveThis.Height;
End;

End.
