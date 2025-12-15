Unit AdvGridHelper;
{ Version: 2.1 }

Interface

Uses
  winApi.windows, system.SysUtils, system.classes, AdvGrid, AdvGridWorkbook, vcl.Graphics, vcl.forms,
  vcl.ExtCtrls, generics.collections;

Type
  TGridAdjuster = Class(TObject)
  Private
    pnlHrdFont, NrpnlHrdFont: TForm;

    ws: Array Of Array Of integer;
    cw: Array Of integer;
    Grid: TadvStringGrid;
    FPreventShrinkingOfColumns: boolean;
    Procedure CalcBaseWidths;
    Procedure UpdateMergedColWidths;
    Procedure ApplyColWidths;
    Procedure Log(Const msg: String; ident: integer = 0);
    Procedure SetPreventShrinkingOfColumns(Const Value: boolean);
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure UpdateColWidth(aGrid: TadvStringGrid);

    Property PreventShrinkingOfColumns: boolean Read FPreventShrinkingOfColumns Write SetPreventShrinkingOfColumns;
  End;

Procedure SaveColWidths(Grid: TadvStringGrid; Const aFilename: String);
  OVERLOAD;
Procedure LoadColWidths(Grid: TadvStringGrid; Const aFilename: String);
  OVERLOAD;

Procedure SaveColWidths(Grid: TAdvGridWorkbook;
  Const aFilename: String); OVERLOAD;
Procedure LoadColWidths(Grid: TAdvGridWorkbook;
  Const aFilename: String); OVERLOAD;

Procedure GetSelectedRows(Grid: TadvStringGrid; RowIndices: TList<integer>);
Procedure ResizeColumns(Grid: TadvStringGrid; PreventShrinkingOfColumns: boolean = false);

Implementation

Uses
  IniFiles, BaseGrid, math, MaxLogic.HtmlUtils;

CONST
  CELL_MARGINS = 10;

Var
  WidthCache_hdr: TStringList = NIL;
  WidthCache_nrm: TStringList = NIL;

Procedure SaveColWidths(Grid: TadvStringGrid; Const aFilename: String);
Var
  ini: TiniFile;
  x: integer;
  sec: String;
Begin
  sec := Grid.owner.name + '.' + Grid.name;
  ini := TiniFile.Create(aFilename);
  For x := 0 To Grid.colCount - 1 Do
    ini.WriteInteger(sec, 'Col_' + IntToStr(x), Grid.colwidths[x]);
  ini.Free;
End;

Procedure LoadColWidths(Grid: TadvStringGrid; Const aFilename: String);
Var
  ini: TiniFile;
  x: integer;
  sec: String;
Begin
  sec := Grid.owner.name + '.' + Grid.name;
  ini := TiniFile.Create(aFilename);
  For x := 0 To Grid.colCount - 1 Do
    Grid.colwidths[x] := ini.ReadInteger(sec, 'Col_' + IntToStr(x),
      Grid.colwidths[x]);
  ini.Free;
End;

Procedure SaveColWidths(Grid: TAdvGridWorkbook; Const aFilename: String);
Begin
  If Assigned(Grid.Grid) Then
    SaveColWidths(Grid.Grid, aFilename)
End;

Procedure LoadColWidths(Grid: TAdvGridWorkbook; Const aFilename: String);
Begin
  If Assigned(Grid.Grid) Then
    SaveColWidths(Grid.Grid, aFilename)
End;

{ TGridAdjuster }

Procedure TGridAdjuster.ApplyColWidths;
Var
  c: integer;
Begin
  For c := 0 To Grid.colCount - 1 Do
  Begin
    If Not PreventShrinkingOfColumns Then
      Grid.colwidths[c] := cw[c]
    Else If Grid.colwidths[c] < cw[c] Then
      Grid.colwidths[c] := cw[c];
  End;
End;

Procedure TGridAdjuster.CalcBaseWidths;
Var
  c, r, w: integer;
Begin
  For c := 0 To Grid.colCount - 1 Do
  Begin
    w := 0;
    For r := 0 To Grid.rowCount - 1 Do
      If Grid.CellSpan(c, r).x = 0 Then
        w := Max(w, ws[c, r]);
    cw[c] := w;
  End;
End;

Constructor TGridAdjuster.Create;
Begin
  Inherited;

  pnlHrdFont := TForm.Create(Nil);
  NrpnlHrdFont := TForm.Create(Nil);

End;

Destructor TGridAdjuster.Destroy;
Begin
  pnlHrdFont.Free;
  NrpnlHrdFont.Free;
  Inherited;
End;

Procedure TGridAdjuster.Log;
Begin
  // logging is disabled
End;

Procedure TGridAdjuster.SetPreventShrinkingOfColumns(Const Value: boolean);
Begin
  FPreventShrinkingOfColumns := Value;
End;

Procedure TGridAdjuster.UpdateColWidth;
Var

  aCol: integer;
  s: String;
  r, c, w: integer;
  hdr, nrm: TCanvas;
  CacheIndex: integer;
Begin
  Log('Enter - TGridAdjuster.UpdateColWidth', 1);
  Grid := aGrid;

  hdr := pnlHrdFont.canvas;
  nrm := pnlHrdFont.canvas;

  nrm.font.assign(Grid.font);
  hdr.font.assign(Grid.fixedfont);

  setLength(cw, Grid.colCount);
  setLength(ws, Grid.colCount, Grid.rowCount);

  For r := 0 To Grid.rowCount - 1 Do
    For c := 0 To Grid.colCount - 1 Do
    Begin
      s := Grid.cells[c, r];
      If Grid.EnableHTML Then
        s := HtmlToText(s);

      ws[c, r] := 0;
      If s <> '' Then
      Begin
        // is fixed cell
        If (r < Grid.FixedRows) Or (c < Grid.FixedCols) Then
        Begin
          If WidthCache_hdr.find(s, CacheIndex) Then
            w := integer(WidthCache_hdr.objects[CacheIndex])
          Else
          Begin
            w := hdr.TextWidth(s);
            WidthCache_hdr.addObject(s, pointer(w));
          End;
        End
        Else
        Begin
          If WidthCache_nrm.find(s, CacheIndex) Then
            w := integer(WidthCache_nrm.objects[CacheIndex])
          Else
          Begin
            w := nrm.TextWidth(s);
            WidthCache_nrm.addObject(s, pointer(w));
          End;
        End;
        ws[c, r] := w + CELL_MARGINS;
      End;
    End;

  CalcBaseWidths;
  UpdateMergedColWidths;
  ApplyColWidths;

  setLength(ws, 0, 0);
  setLength(cw, 0);

  Log('Leave - TGridAdjuster.UpdateColWidth');
End;

Function RoundUp(Const d: double): integer;
Begin
  Result := Trunc(d);
  If frac(d) > 0 Then
    Result := Result + 1;
End;

Procedure TGridAdjuster.UpdateMergedColWidths;
Var
  iDiff: integer;
  diff: double;
  i: integer;
  c, r, w: integer;
  cp: TCellProperties;
Begin
  For c := 0 To Grid.colCount - 1 Do
  Begin
    For r := Grid.rowCount - 1 Downto 0 Do
      If Grid.CellSpan(c, r).x > 0 Then
      Begin
        cp := Grid.CellProperties[c, r];
        If (cp.CellSpanX > 0) And cp.IsBaseCell Then
        Begin
          w := 0;
          For i := 0 To cp.CellSpanX Do
            inc(w, cw[i + c]);

          If ws[c, r] > w Then
          Begin
            diff := ws[c, r] - w;
            diff := diff / (cp.CellSpanX + 1);
            iDiff := RoundUp(diff);
            For i := 0 To cp.CellSpanX Do
              cw[i + c] := cw[i + c] + iDiff;
          End;

        End;
      End;
  End;
End;

Procedure GetSelectedRows(Grid: TadvStringGrid; RowIndices: TList<integer>);
Var
  x: integer;
  i: integer;
Begin
  For x := 0 To Grid.SelectedRowCount - 1 Do
  Begin
    i := Grid.SelectedRow[x];
    RowIndices.add(i);
  End;
End;

Procedure ResizeColumns(Grid: TadvStringGrid; PreventShrinkingOfColumns: boolean = false);
Var
  a: TGridAdjuster;
Begin
  a := TGridAdjuster.Create;
  a.PreventShrinkingOfColumns := PreventShrinkingOfColumns;
  a.UpdateColWidth(Grid);
  a.Free;
End;

INITIALIZATION

WidthCache_hdr := TStringList.Create;
WidthCache_hdr.sorted := True;

WidthCache_nrm := TStringList.Create;
WidthCache_nrm.sorted := True;

FINALIZATION

If Assigned(WidthCache_hdr) Then
  FreeAndNIL(WidthCache_hdr);
If Assigned(WidthCache_nrm) Then
  FreeAndNIL(WidthCache_nrm);

End.
