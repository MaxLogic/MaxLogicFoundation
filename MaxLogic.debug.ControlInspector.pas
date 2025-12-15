unit MaxLogic.Debug.ControlInspector;

{
  TDebugControlInspector — Runtime Control & Data-Binding Inspector (VCL)

  Purpose:
    Designed to speed up work in unfamiliar or legacy codebases. It lets you
    jump from a control visible at runtime back to its owning form and understand
    how data-aware controls are wired to datasets.

  Usage:
    Hover over a control and press Ctrl+Shift+Q to open a context menu. From there
    you can copy the control hierarchy, dataset details (fields / SQL / params),
    selected DBGrid row metadata, and lookup-dataset information.

  Output:
    Writes a readable report to the clipboard; if the clipboard is locked, a UTF-8
    temp file is created and opened automatically.

  Integration:
    Call TDebugControlInspector.Inject(MainForm) once at startup, or drop the
    component onto a form. It hooks non-invasively and cleans up on destroy.
}

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, System.Variants, System.Rtti, System.Generics.Collections,
  Data.DB, Vcl.Controls, Vcl.DBGrids, Vcl.Forms, Vcl.Menus, MaxLogic.RttiHelper;

type
  // Customization hooks:
  //  - Lookup handler: return a dataset related to the control (e.g. lookup/list/aux DS) or nil if not applicable.
  //  - Info handler: append any extra diagnostic lines about the control to a TStrings destination.
  TLookupDataSetHandler = reference to function(aControl: TControl): TDataSet;
  TControlInfoHandler   = reference to procedure(aControl: TControl; aDest: TStrings);

  TDebugControlInspector = class(TComponent)
  private const
    cSep = ';';
  private
    class var fScreenOnFormChangeCaptured: Boolean;
    class var fLookupHandlers: TDictionary<TClass, TLookupDataSetHandler>;
    class var fInfoHandlers:   TDictionary<TClass, TControlInfoHandler>;
    class constructor CreateClass;

  private
    fOrgFormKeyDown: TKeyEvent;
    fMouseOverControl: TWinControl;
    fDSForControlUnderMouse: TDataSet;
    fDSLookupForControlUnderMouse: TDataSet;
    fPopUp: TPopupMenu;
    fmiControl: TMenuItem;
    fmiDBEdit: TMenuItem;
    fmiDBGrid: TMenuItem;
    fmiDataSet: TMenuItem;
    fmiLookupDataSet: TMenuItem;
    fRTTIHelper: TRTTIHelper;
    fOrgOnScreenActiveFormChanged: TNotifyEvent;

    // helpers
    function MakeFieldLogEntry(aField: TField): string;
    class procedure SafeWriteToClipBoard(const aText: string);
    procedure Exec(const aFileName: string);

    // key handling / popup
    procedure FormKeyDown(aSender: TObject; var aKey: Word; aShift: TShiftState);
    procedure DisplayPopUpForControlUnderMouse;
    procedure BuildPopup;
    function  addMenuItem(const aCaption: string; aOnClick: TNotifyEvent; aParentMenuItem: TMenuItem = nil): TMenuItem;

    // menu handlers
    procedure miControlClick(aSender: TObject);
    procedure miDBEditClick(aSender: TObject);
    procedure miDBGridClick(aSender: TObject);
    procedure miDataSetClick(aSender: TObject);
    procedure miLookupDataSetClick(aSender: TObject);

    // injection / lifecycle
    procedure RegisterHotKey(aForm: TForm);
    procedure ScreenOnActivFormChanged(aSender: TObject);

    // copy helpers
    procedure DoCopyDataSetDetailsToClipBoard(aDataSet: TDataSet; aOnlyCurrentRecord: Boolean);
    procedure DoCopySelectedRowInGridDetailsToClipBoard(aGrid: TDBGrid);

    // dataset reflection
    function GetDSVariableInfo(aDataSet: TDataSet): string;
    function GetDSSQLText(aDataSet: TDataSet): string;

    // lookup detection (generic)
    function TryGetLookupDataSetViaRTTI(aControl: TControl; out aLookupDataSet: TDataSet): Boolean;
    function TryGetLookupDataSetFromPropertyPath(const aObject: TObject; const aPath: string; out aLookupDataSet: TDataSet): Boolean;
    function TryGetDataSourceProperty(const aObject: TObject; const aPropName: string; out aDataSet: TDataSet): Boolean;

    // handler resolution
    class function TryLookupHandler(aControl: TControl; out aHandler: TLookupDataSetHandler): Boolean; static;
    class function TryInfoHandler(aControl: TControl; out aHandler: TControlInfoHandler): Boolean; static;

    // binding/info
    procedure AppendCommonBindingInfo(aControl: TControl; aDest: TStrings);
  public
    constructor Create(aOwner: TComponent = nil); override;
    destructor Destroy; override;

    class procedure CopySelectedRowInGridDetailsToClipBoard(aGrid: TDBGrid);
    class procedure CopyDataSetDetailsToClipBoard(aDataSet: TDataSet; aOnlyCurrentRecord: Boolean);
    class procedure CopyControlHirarchyToClipBoard(aControl: TControl);

    class procedure Inject(aMainForm: TForm);

    // Public registration API for custom component handlers
    class procedure RegisterLookupHandler(aControlClass: TClass; const aHandler: TLookupDataSetHandler);
    class procedure UnregisterLookupHandler(aControlClass: TClass);
    class procedure RegisterInfoHandler(aControlClass: TClass; const aHandler: TControlInfoHandler);
    class procedure UnregisterInfoHandler(aControlClass: TClass);
  end;

implementation

uses
  AutoFree, AutoHourGlass, System.StrUtils, System.IOUtils, Winapi.ShellAPI, ClipBrd;

{ TDebugControlInspector }

class constructor TDebugControlInspector.CreateClass;
begin
  fScreenOnFormChangeCaptured := False;
  fLookupHandlers := TDictionary<TClass, TLookupDataSetHandler>.Create;
  fInfoHandlers   := TDictionary<TClass, TControlInfoHandler>.Create;
end;

constructor TDebugControlInspector.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  if Assigned(aOwner) then
    if aOwner is TForm then
      RegisterHotKey(aOwner as TForm);

  fRTTIHelper := TRTTIHelper.Create;
end;

destructor TDebugControlInspector.Destroy;
begin
  fRTTIHelper.Free;

  if Assigned(fOrgFormKeyDown) then
    if Assigned(Owner) then
      (Owner as TForm).OnKeyDown := fOrgFormKeyDown;

  if Assigned(fOrgOnScreenActiveFormChanged) then
  begin
    fScreenOnFormChangeCaptured := False;
    if Assigned(Screen) then
      Screen.OnActiveFormChange := fOrgOnScreenActiveFormChanged;
  end;

  inherited;
end;

class procedure TDebugControlInspector.CopySelectedRowInGridDetailsToClipBoard(aGrid: TDBGrid);
var
  lLogger: TDebugControlInspector;
begin
  gc(lLogger, TDebugControlInspector.Create);
  lLogger.DoCopySelectedRowInGridDetailsToClipBoard(aGrid);
end;

procedure TDebugControlInspector.DoCopySelectedRowInGridDetailsToClipBoard(aGrid: TDBGrid);
var
  lList: TStringList;
  i: Integer; // <=2 chars: allowed without prefix
begin
  gc(lList, TStringList.Create);

  lList.Add(aGrid.Name + ': ' + aGrid.ClassName + sLineBreak +
    'ColCount: ' + IntToStr(aGrid.Columns.Count) + sLineBreak +
    'DataSource: ' + IfThen(Assigned(aGrid.DataSource), aGrid.DataSource.Name, '<nil>') + sLineBreak +
    'DataSet: ' + IfThen(Assigned(aGrid.DataSource) and Assigned(aGrid.DataSource.DataSet), aGrid.DataSource.DataSet.Name, '<nil>'));

  for i := 0 to aGrid.Columns.Count - 1 do
  begin
    lList.Add(sLineBreak + '#' + IntToStr(i));
    if Assigned(aGrid.Columns[i].Field) then
      lList.Add(MakeFieldLogEntry(aGrid.Columns[i].Field))
    else
      lList.Add('Field: <nil>');
  end;

  SafeWriteToClipBoard(lList.Text);
end;

function TDebugControlInspector.MakeFieldLogEntry(aField: TField): string;
begin
  Result :=
    'FieldName: ' + aField.FieldName + sLineBreak +
    'Component name: ' + aField.Name + sLineBreak +
    'FieldClass: ' + aField.ClassName + sLineBreak +
    'DisplayLabel: ' + aField.DisplayLabel + sLineBreak +
    'AsString: ' + aField.AsString + sLineBreak +
    'DisplayText: ' + aField.DisplayText + sLineBreak +
    'Visible: ' + BoolToStr(aField.Visible, True);
end;

class procedure TDebugControlInspector.CopyDataSetDetailsToClipBoard(aDataSet: TDataSet; aOnlyCurrentRecord: Boolean);
var
  lLogger: TDebugControlInspector;
begin
  gc(lLogger, TDebugControlInspector.Create);
  lLogger.DoCopyDataSetDetailsToClipBoard(aDataSet, aOnlyCurrentRecord);
end;

procedure TDebugControlInspector.DoCopyDataSetDetailsToClipBoard(aDataSet: TDataSet; aOnlyCurrentRecord: Boolean);
var
  lList: TStringList;
  i: Integer;
  s: string; // <=2 chars: allowed without prefix
begin
  if not aOnlyCurrentRecord then
    raise Exception.Create('not yet implemmented!');

  gc(lList, TStringList.Create);

  lList.Add('DS Name: ' + aDataSet.Name + sLineBreak +
    'ClassName: ' + aDataSet.ClassName + sLineBreak +
    'RecordCount: ' + IntToStr(aDataSet.RecordCount) + sLineBreak +
    'FieldCount: ' + IntToStr(aDataSet.FieldCount) + sLineBreak);

  for i := 0 to aDataSet.Fields.Count - 1 do
  begin
    lList.Add(sLineBreak + '#' + IntToStr(i));
    lList.Add(MakeFieldLogEntry(aDataSet.Fields[i]));
  end;

  // SQL (if any)
  s := GetDSSQLText(aDataSet);
  if s <> '' then
    lList.Add(sLineBreak + '==SQL==' + sLineBreak + s + sLineBreak + '==/SQL==' + sLineBreak);

  // Variables/Params (best-effort across dataset classes)
  lList.Add(
    sLineBreak + '==Variables==' + sLineBreak +
    GetDSVariableInfo(aDataSet) +
    '==/Variables==' + sLineBreak
  );

  SafeWriteToClipBoard(lList.Text);
end;

function TDebugControlInspector.GetDSSQLText(aDataSet: TDataSet): string;
var
  lCtx: TRttiContext;
  lTyp: TRttiType;
  lProp: TRttiProperty;
  lObj: TObject;
  lStrings: TStrings;
begin
  Result := '';
  lCtx := TRttiContext.Create;
  try
    lTyp := lCtx.GetType(aDataSet.ClassType);

    // Try property 'SQL' as TStrings or string
    lProp := lTyp.GetProperty('SQL');
    if Assigned(lProp) then
    begin
      if lProp.PropertyType.IsInstance then
      begin
        lObj := lProp.GetValue(aDataSet).AsObject;
        if (lObj is TStrings) then
        begin
          lStrings := TStrings(lObj);
          Exit(lStrings.Text);
        end;
      end else
      if lProp.PropertyType.TypeKind in [tkUString, tkString, tkWString, tkLString] then
        Exit(lProp.GetValue(aDataSet).AsString);
    end;
  finally
    lCtx.Free;
  end;
end;

function TDebugControlInspector.GetDSVariableInfo(aDataSet: TDataSet): string;
var
  lCtx: TRttiContext;
  lTyp: TRttiType;

  function TryReadInt(const aPropName: string; out aValue: Integer): Boolean;
  var
    lProp: TRttiProperty;
  begin
    lProp := lTyp.GetProperty(aPropName);
    Result := Assigned(lProp) and (lProp.PropertyType.TypeKind in [tkInteger, tkInt64]);
    if Result then
      aValue := lProp.GetValue(aDataSet).AsInteger;
  end;

  function TryInvokeString(const aMethod: string; const aIndex: Integer; out aValue: string): Boolean;
  var
    lM: TRttiMethod;
    lVal: TValue;
  begin
    lM := lTyp.GetMethod(aMethod);
    Result := Assigned(lM);
    if Result then
    begin
      lVal := lM.Invoke(aDataSet, [aIndex]);
      Result := lVal.Kind in [tkUString, tkString];
      if Result then
        aValue := lVal.AsString;
    end;
  end;

  function TryInvokeVariant(const aMethod: string; const aIndex: Integer; out aValue: Variant): Boolean;
  var
    lM: TRttiMethod;
    lVal: TValue;
  begin
    lM := lTyp.GetMethod(aMethod);
    Result := Assigned(lM);
    if Result then
    begin
      lVal := lM.Invoke(aDataSet, [aIndex]);
      aValue := lVal.AsVariant;
    end;
  end;

var
  s: string; // <=2 chars: allowed without prefix
  lCount, i: Integer;
  lName: string;
  v: Variant;

  // Params fallback
  lProp: TRttiProperty;
  lObj: TObject;
  lParams: TParams;
  p: TParam;
begin
  lCtx := TRttiContext.Create;
  try
    lTyp := lCtx.GetType(aDataSet.ClassType);
    Result := '';

    // Prefer a "Variables" API if exposed by the dataset
    if TryReadInt('VariableCount', lCount) and (lCount > 0) then
    begin
      for i := 0 to lCount - 1 do
      begin
        if not TryInvokeString('VariableName', i, lName) then
          lName := '#' + IntToStr(i);
        if not TryInvokeVariant('GetVariable', i, v) then
          v := Null;

        Result := Result +
          '#' + IntToStr(i) + sLineBreak +
          'Name: ' + lName + sLineBreak +
          'Value: ' + VarToStrDef(v, '<NULL>') + sLineBreak + sLineBreak;
      end;
      Exit;
    end;

    // Fallback: Params collection (e.g., TFDQuery, TSQLQuery, etc.)
    lProp := lTyp.GetProperty('Params');
    if Assigned(lProp) and lProp.PropertyType.IsInstance then
    begin
      lObj := lProp.GetValue(aDataSet).AsObject;
      if lObj is TParams then
      begin
        lParams := TParams(lObj);
        s := 'ParamCount: ' + IntToStr(lParams.Count) + sLineBreak;
        for i := 0 to lParams.Count - 1 do
        begin
          p := lParams[i];
          s := s +
            '#' + IntToStr(i) + sLineBreak +
            'Name: ' + p.Name + sLineBreak +
            'Value: ' + VarToStrDef(p.Value, '<NULL>') + sLineBreak + sLineBreak;
        end;
        Exit(s);
      end;
    end;

    // Nothing recognized
    Result := '<no variables/params interface detected>' + sLineBreak;
  finally
    lCtx.Free;
  end;
end;

class procedure TDebugControlInspector.SafeWriteToClipBoard(const aText: string);
var
  lNumOfTries: Integer;
  lSuccess: Boolean;
  lFn: string;
begin
  lNumOfTries := 1;
  lSuccess := False;

  repeat
    try
      Clipboard.AsText := aText;
      lSuccess := True;
    except
      Sleep(lNumOfTries * 50);
      Inc(lNumOfTries);
    end;
  until (lNumOfTries > 5) or lSuccess;

  if not lSuccess then
  begin
    lFn := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'clipBoardFailure.txt';
    TFile.WriteAllText(lFn, aText, TEncoding.UTF8);
    ShellExecute(Application.Handle, 'open', PChar(lFn), '', '', SW_SHOWNORMAL);
  end;
end;

procedure TDebugControlInspector.Exec(const aFileName: string);
begin
  ShellExecute(Application.Handle, 'open', PChar(aFileName), '', '', SW_SHOWNORMAL);
end;

class procedure TDebugControlInspector.CopyControlHirarchyToClipBoard(aControl: TControl);
var
  s: string; // <=2 chars: allowed without prefix
begin
  s := '';
  repeat
    s := aControl.Name + ': ' + aControl.ClassName + ' (' + aControl.UnitName + '.pas)' + sLineBreak + s;
    aControl := aControl.Parent;
  until aControl = nil;

  SafeWriteToClipBoard(Trim(s));
end;

procedure TDebugControlInspector.RegisterHotKey(aForm: TForm);
begin
  fOrgFormKeyDown := aForm.OnKeyDown;
  aForm.OnKeyDown := FormKeyDown;
  aForm.KeyPreview := True;
end;

procedure TDebugControlInspector.FormKeyDown(aSender: TObject; var aKey: Word; aShift: TShiftState);
var
  lHandled: Boolean;
begin
  lHandled := False;
  if (ssCtrl in aShift) and (ssShift in aShift) then
  begin
    if Char(aKey) in ['Q', 'q'] then
    begin
      lHandled := True;
      fMouseOverControl := FindVCLWindow(Mouse.CursorPos);
      if fMouseOverControl <> nil then
        DisplayPopUpForControlUnderMouse;
    end;
  end;

  if not lHandled then
    if Assigned(fOrgFormKeyDown) then
      fOrgFormKeyDown(aSender, aKey, aShift);
end;

procedure TDebugControlInspector.DisplayPopUpForControlUnderMouse;
var
  lLookupHandler: TLookupDataSetHandler;
begin
  if not Assigned(fPopUp) then
    BuildPopup;

  fDSForControlUnderMouse := nil;
  fDSLookupForControlUnderMouse := nil;

  // primary DS from control (via RTTI, not TRTTIHelper.GetDataSetFromControl)
  TryGetDataSourceProperty(fMouseOverControl, 'DataSource', fDSForControlUnderMouse);
  if (fDSForControlUnderMouse = nil) and Assigned(fMouseOverControl.Parent) then
    TryGetDataSourceProperty(fMouseOverControl.Parent, 'DataSource', fDSForControlUnderMouse);

  // try common lookup DS properties first
  if not Assigned(fDSLookupForControlUnderMouse) then
  begin
    if not TryGetDataSourceProperty(fMouseOverControl, 'LookupDataSource', fDSLookupForControlUnderMouse) then
      if not TryGetDataSourceProperty(fMouseOverControl, 'LookupSource', fDSLookupForControlUnderMouse) then
        TryGetDataSourceProperty(fMouseOverControl, 'ListSource', fDSLookupForControlUnderMouse);
  end;

  // parent fallback for lookup props
  if not Assigned(fDSLookupForControlUnderMouse) and Assigned(fMouseOverControl.Parent) then
  begin
    if not TryGetDataSourceProperty(fMouseOverControl.Parent, 'LookupDataSource', fDSLookupForControlUnderMouse) then
      if not TryGetDataSourceProperty(fMouseOverControl.Parent, 'LookupSource', fDSLookupForControlUnderMouse) then
        TryGetDataSourceProperty(fMouseOverControl.Parent, 'ListSource', fDSLookupForControlUnderMouse);
  end;

  // generic RTTI-based lookup discovery (nested paths, etc.)
  if not Assigned(fDSLookupForControlUnderMouse) then
    TryGetLookupDataSetViaRTTI(fMouseOverControl, fDSLookupForControlUnderMouse);

  if not Assigned(fDSLookupForControlUnderMouse) and Assigned(fMouseOverControl.Parent) then
    TryGetLookupDataSetViaRTTI(fMouseOverControl.Parent, fDSLookupForControlUnderMouse);

  // user-registered handler (control first, then parent)
  if not Assigned(fDSLookupForControlUnderMouse) and TryLookupHandler(fMouseOverControl, lLookupHandler) then
    fDSLookupForControlUnderMouse := lLookupHandler(fMouseOverControl);

  if not Assigned(fDSLookupForControlUnderMouse) and Assigned(fMouseOverControl.Parent) then
    if TryLookupHandler(fMouseOverControl.Parent, lLookupHandler) then
      fDSLookupForControlUnderMouse := lLookupHandler(fMouseOverControl.Parent);

  // menu visibility
  fmiDBGrid.Visible := (fMouseOverControl is TDBGrid);
  fmiDBEdit.Visible :=
    (fDSForControlUnderMouse <> nil) and
    (fRTTIHelper.ReadProperty(fMouseOverControl, 'Datafield') <> '');
  fmiDataSet.Visible := (fDSForControlUnderMouse <> nil);
  fmiLookupDataSet.Visible := (fDSLookupForControlUnderMouse <> nil);

  with Mouse.CursorPos do
    fPopUp.Popup(X, Y);
end;
procedure TDebugControlInspector.BuildPopup;
begin
  fPopUp := TPopupMenu.Create(Self);
  fPopUp.AutoHotkeys := maManual;
  fmiControl := addMenuItem('Control: copy hirarchy to clipboard', miControlClick);
  fmiDBEdit := addMenuItem('DBEdit: copy details to clipboard', miDBEditClick);
  fmiDBGrid := addMenuItem('DBGrid: copy details to clipboard', miDBGridClick);
  fmiDataSet := addMenuItem('DataSet: copy details to clipboard', miDataSetClick);
  fmiLookupDataSet := addMenuItem('LookUp-DataSet: copy details to clipboard', miLookupDataSetClick);
end;

function TDebugControlInspector.addMenuItem(const aCaption: string; aOnClick: TNotifyEvent; aParentMenuItem: TMenuItem): TMenuItem;
begin
  Result := fPopUp.CreateMenuItem;
  Result.Caption := aCaption;
  Result.OnClick := aOnClick;
  Result.Visible := True;
  fPopUp.Items.Add(Result);
end;

procedure TDebugControlInspector.miControlClick(aSender: TObject);
begin
  CopyControlHirarchyToClipBoard(fMouseOverControl);
end;

procedure TDebugControlInspector.miDBEditClick(aSender: TObject);
var
  s: string; // <=2 chars: allowed without prefix
  lFieldName: string;
  lInfoHandler: TControlInfoHandler;
  lDS: TDataSet;
  lInfoStrings: TStringList;
begin
  lFieldName := fRTTIHelper.ReadProperty(fMouseOverControl, 'Datafield');

  s :=
    'Control.Name: ' + fMouseOverControl.Name + sLineBreak +
    'Control.ClassName: ' + fMouseOverControl.ClassName + sLineBreak +
    'Control.FieldName: ' + lFieldName + sLineBreak;

  if fDSForControlUnderMouse = nil then
    s := s + 'DataSet: nil'
  else
  begin
    s := s + 'DataSet: ' + fDSForControlUnderMouse.Name + sLineBreak;

    s := s + 'Field details:' + sLineBreak +
      MakeFieldLogEntry(fDSForControlUnderMouse.FieldByName(lFieldName)) + sLineBreak +
      '-----' + sLineBreak;
  end;

  if fDSLookupForControlUnderMouse <> nil then
    s := s + 'Lookup-DataSet: ' + fDSLookupForControlUnderMouse.Name + sLineBreak;

  // Append common binding/lookup properties via RTTI (KeyField/ListField/Lookup* etc.)
  gc(lInfoStrings, TStringList.Create);
  AppendCommonBindingInfo(fMouseOverControl, lInfoStrings);

  // Allow user-registered info handlers to add more details (control, then parent)
  if TryInfoHandler(fMouseOverControl, lInfoHandler) then
    lInfoHandler(fMouseOverControl, lInfoStrings);

  if (lInfoStrings.Count = 0) and Assigned(fMouseOverControl.Parent) then
  begin
    AppendCommonBindingInfo(fMouseOverControl.Parent, lInfoStrings);
    if TryInfoHandler(fMouseOverControl.Parent, lInfoHandler) then
      lInfoHandler(fMouseOverControl.Parent, lInfoStrings);
  end;

  if lInfoStrings.Count > 0 then
  begin
    s := s + 'Additional control binding info:' + sLineBreak;
    s := s + lInfoStrings.Text;
  end;

  SafeWriteToClipBoard(s);
end;

procedure TDebugControlInspector.miDBGridClick(aSender: TObject);
begin
  CopySelectedRowInGridDetailsToClipBoard(fMouseOverControl as TDBGrid);
end;

procedure TDebugControlInspector.miDataSetClick(aSender: TObject);
begin
  CopyDataSetDetailsToClipBoard(fDSForControlUnderMouse, True);
end;

class procedure TDebugControlInspector.Inject(aMainForm: TForm);
var
  lInst: TDebugControlInspector;
begin
  if fScreenOnFormChangeCaptured then
    Exit;

  lInst := TDebugControlInspector.Create(aMainForm);
  lInst.fOrgOnScreenActiveFormChanged := Screen.OnActiveFormChange;
  Screen.OnActiveFormChange := lInst.ScreenOnActivFormChanged;
  fScreenOnFormChangeCaptured := True;
end;

procedure TDebugControlInspector.ScreenOnActivFormChanged(aSender: TObject);
var
  lForm: TForm;
  i: Integer;
  lFound: Boolean;
  lInst: TDebugControlInspector;
begin
  lForm := Screen.ActiveForm;
  if Assigned(lForm) then
  begin
    // does form already own a TDebugControlInspector?
    lFound := False;
    for i := 0 to lForm.ComponentCount - 1 do
      if lForm.Components[i] is TDebugControlInspector then begin lFound := True; Break; end;

    if lFound then
      Exit;

    lInst := TDebugControlInspector.Create(lForm);
    // constructor hooks the hotkey
  end;
end;

procedure TDebugControlInspector.miLookupDataSetClick(aSender: TObject);
begin
  CopyDataSetDetailsToClipBoard(fDSLookupForControlUnderMouse, True);
end;

{ ===== Generic lookup detection via RTTI ===== }

function TDebugControlInspector.TryGetLookupDataSetViaRTTI(aControl: TControl; out aLookupDataSet: TDataSet): Boolean;
const
  // single-level data source property candidates commonly used by VCL/3rd-party controls
  C_SourceProps: array[0..3] of string = ('LookupDataSource', 'LookupSource', 'ListSource', 'DataSource');
  // nested property paths (object.property)
  C_NestedPaths: array[0..2] of string = ('DBLookupComboBox.LookupSource', 'LookupComboBox.LookupSource', 'Editor.LookupSource');
var
  prop: string; // <=2 chars: allowed without prefix
begin
  aLookupDataSet := nil;

  // try direct properties that return TDataSource
  for prop in C_SourceProps do
    if TryGetDataSourceProperty(aControl, prop, aLookupDataSet) and Assigned(aLookupDataSet) then
      Exit(True);

  // try common nested holders (e.g. a wrapper exposing an inner DBLookupComboBox)
  for prop in C_NestedPaths do
    if TryGetLookupDataSetFromPropertyPath(aControl, prop, aLookupDataSet) and Assigned(aLookupDataSet) then
      Exit(True);

  Result := Assigned(aLookupDataSet);
end;

function TDebugControlInspector.TryGetLookupDataSetFromPropertyPath(const aObject: TObject; const aPath: string; out aLookupDataSet: TDataSet): Boolean;
var
  lCtx: TRttiContext;
  lVal: TValue;
  lCurObj: TObject;
  lTyp: TRttiType;
  lPart: string;
  parts: TArray<string>;
  i: Integer;
  lProp: TRttiProperty;
  lDS: TDataSource;
begin
  aLookupDataSet := nil;
  Result := False;

  if (aObject = nil) or (aPath = '') then
    Exit;

  lCtx := TRttiContext.Create;
  try
    lCurObj := aObject;
    parts := aPath.Split(['.']);
    for i := 0 to High(parts) do
    begin
      lTyp := lCtx.GetType(lCurObj.ClassType);
      lPart := parts[i];

      lProp := lTyp.GetProperty(lPart);
      if not Assigned(lProp) then
        Exit(False);

      lVal := lProp.GetValue(lCurObj);
      if i < High(parts) then
      begin
        if not lVal.IsObject then
          Exit(False);
        lCurObj := lVal.AsObject;
        if lCurObj = nil then
          Exit(False);
      end else
      begin
        // last step: expect a TDataSource and return its DataSet
        if lVal.IsObject and (lVal.AsObject is TDataSource) then
        begin
          lDS := TDataSource(lVal.AsObject);
          if Assigned(lDS) then
            aLookupDataSet := lDS.DataSet;
          Result := Assigned(aLookupDataSet);
        end;
      end;
    end;
  finally
    lCtx.Free;
  end;
end;

function TDebugControlInspector.TryGetDataSourceProperty(const aObject: TObject; const aPropName: string; out aDataSet: TDataSet): Boolean;
var
  lCtx: TRttiContext;
  lTyp: TRttiType;
  lProp: TRttiProperty;
  lVal: TValue;
  lDS: TDataSource;
begin
  aDataSet := nil;
  Result := False;

  if aObject = nil then
    Exit;

  lCtx := TRttiContext.Create;
  try
    lTyp := lCtx.GetType(aObject.ClassType);
    lProp := lTyp.GetProperty(aPropName);
    if not Assigned(lProp) then
      Exit;

    lVal := lProp.GetValue(aObject);
    if lVal.IsObject and (lVal.AsObject is TDataSource) then
    begin
      lDS := TDataSource(lVal.AsObject);
      if Assigned(lDS) then
        aDataSet := lDS.DataSet;
      Result := Assigned(aDataSet);
    end;
  finally
    lCtx.Free;
  end;
end;

procedure TDebugControlInspector.AppendCommonBindingInfo(aControl: TControl; aDest: TStrings);
  procedure AppendIfStringProp(const aPropName, aLabel: string);
  var
    lVal: string;
  begin
    lVal := fRTTIHelper.ReadProperty(aControl, aPropName);
    if lVal <> '' then
      aDest.Add(aLabel + ': ' + lVal);
  end;

  procedure AppendIfDSProp(const aPropName, aLabel: string);
  var
    lDS: TDataSource;
    lDataSet: TDataSet;
  begin
    if TryGetDataSourceProperty(aControl, aPropName, lDataSet) and Assigned(lDataSet) then
      aDest.Add(aLabel + ': ' + lDataSet.Name)
    else
    begin
      // also try reading the DS component name directly for clarity (best-effort)
      // FIX: TRTTIHelper.ReadObjectProperty has only (Instance, PropName)
      lDS := TDataSource(fRTTIHelper.ReadObjectProperty(aControl, aPropName));
      if Assigned(lDS) then
        aDest.Add(aLabel + ': ' + lDS.Name);
    end;
  end;
begin
  // common data-aware bits
  AppendIfStringProp('DataField', 'DataField');
  AppendIfDSProp('DataSource', 'DataSource');

  // VCL DBLookup controls usually expose KeyField/ListField/ListSource
  AppendIfStringProp('KeyField', 'KeyField');
  AppendIfStringProp('ListField', 'ListField');
  AppendIfDSProp('ListSource', 'ListSource');

  // Popular 3rd-party naming
  AppendIfStringProp('LookupField', 'LookupField');
  AppendIfStringProp('LookupDisplay', 'LookupDisplay');
  AppendIfStringProp('LookupFormat', 'LookupFormat');
  AppendIfDSProp('LookupSource', 'LookupSource');
  AppendIfDSProp('LookupDataSource', 'LookupDataSource');

  // nested common wrappers
  // (will appear if the wrapper exposes an inner control with those props)
  // handled by TryGetLookupDataSetFromPropertyPath in discovery stage
end;

{ ===== Handler resolution & registration ===== }

class function TDebugControlInspector.TryLookupHandler(aControl: TControl; out aHandler: TLookupDataSetHandler): Boolean;
var
  lClass: TClass;
begin
  Result := False;
  aHandler := nil;

  lClass := aControl.ClassType;
  while (lClass <> nil) and (lClass <> TControl) do
  begin
    if fLookupHandlers.TryGetValue(lClass, aHandler) then
      Exit(True);
    lClass := lClass.ClassParent;
  end;
end;

class function TDebugControlInspector.TryInfoHandler(aControl: TControl; out aHandler: TControlInfoHandler): Boolean;
var
  lClass: TClass;
begin
  Result := False;
  aHandler := nil;

  lClass := aControl.ClassType;
  while (lClass <> nil) and (lClass <> TControl) do
  begin
    if fInfoHandlers.TryGetValue(lClass, aHandler) then
      Exit(True);
    lClass := lClass.ClassParent;
  end;
end;

class procedure TDebugControlInspector.RegisterLookupHandler(aControlClass: TClass; const aHandler: TLookupDataSetHandler);
begin
  if aControlClass = nil then
    Exit;

  fLookupHandlers.AddOrSetValue(aControlClass, aHandler);
end;

class procedure TDebugControlInspector.UnregisterLookupHandler(aControlClass: TClass);
begin
  if aControlClass = nil then
    Exit;

  fLookupHandlers.Remove(aControlClass);
end;

class procedure TDebugControlInspector.RegisterInfoHandler(aControlClass: TClass; const aHandler: TControlInfoHandler);
begin
  if aControlClass = nil then
    Exit;

  fInfoHandlers.AddOrSetValue(aControlClass, aHandler);
end;

class procedure TDebugControlInspector.UnregisterInfoHandler(aControlClass: TClass);
begin
  if aControlClass = nil then
    Exit;

  fInfoHandlers.Remove(aControlClass);
end;

end.

