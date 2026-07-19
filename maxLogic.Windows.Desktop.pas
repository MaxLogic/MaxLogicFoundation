unit maxLogic.Windows.Desktop;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,
  Vcl.Graphics;

type
  TWndList = TList<HWND>;

function CopyIconFromWindowHandle(const aHandle: THandle; const aIcon: TIcon): Boolean;
function ForceForegroundWindow(const aWnd: THandle): Boolean;
function GetFileName(const aWnd: HWND): string;
function GetWinCaption(const aWnd: HWND): string;
procedure GetWndList(const aList: TWndList);
function IsWndValid(const aWnd: HWND): Boolean;
procedure PrintWindow(const aWnd: HWND; const aBitmap: TBitmap);
function RetrieveAppUserModelID(const aWnd: HWND): string;
function RetrieveCommandLine(const aPid: Cardinal): string;
function RetrievePID(const aWnd: HWND): Cardinal;
function RetrieveRelaunchCommand(const aWnd: HWND): string;

implementation

uses
  System.SysUtils, System.Variants, System.Win.ComObj,
  Winapi.ActiveX, Winapi.Messages, Winapi.PropSys;

const
  cPROCESS_QUERY_LIMITED_INFORMATION = $1000;
  cGCLP_HICON = -14;
  cRPC_C_AUTHN_LEVEL_DEFAULT = 0;
  cRPC_C_IMP_LEVEL_IMPERSONATE = 3;
  cEOAC_NONE = 0;

var
  glProgramManager: HWND = 0;
  glTaskBarWnd: HWND = 0;
  glSysFader: HWND = 0;
  glTooltipsClass32: HWND = 0;

function QueryFullProcessImageName(hProcess: THandle; dwFlags: DWORD; lpExeName: PChar; var lpdwSize: DWORD): BOOL;
  stdcall; external kernel32 name 'QueryFullProcessImageNameW';

function IsIgnoredWindow(const aWnd: HWND): Boolean;
begin
  Result :=
    (aWnd = 0) or
    (aWnd = glProgramManager) or
    (aWnd = glTaskBarWnd) or
    (aWnd = glSysFader) or
    (aWnd = glTooltipsClass32);
end;

function IsWndValid(const aWnd: HWND): Boolean;
begin
  Result :=
    (not IsIgnoredWindow(aWnd)) and
    IsWindow(aWnd) and
    IsWindowVisible(aWnd) and
    (not SameText(Copy(GetWinCaption(aWnd), 1, 3), 'GDI'));
end;

function WndListEnumWinProc(aWnd: HWND; aParam: LPARAM): BOOL; stdcall;
var
  lWndList: TWndList;
begin
  lWndList := TWndList(aParam);
  if Assigned(lWndList) and IsWndValid(aWnd) then
    lWndList.Add(aWnd);
  Result := True;
end;

procedure GetWndList(const aList: TWndList);
begin
  if not Assigned(aList) then
    Exit;

  EnumWindows(@WndListEnumWinProc, LPARAM(aList));
end;

function GetWinCaption(const aWnd: HWND): string;
var
  lLen: Integer;
  lWritten: Integer;
begin
  Result := '';
  if aWnd = 0 then
    Exit;

  lLen := GetWindowTextLength(aWnd);
  if lLen <= 0 then
    Exit;

  SetLength(Result, lLen + 1);
  lWritten := GetWindowText(aWnd, PChar(Result), lLen + 1);
  SetLength(Result, lWritten);
  Result := Trim(Result);
end;

function RetrievePID(const aWnd: HWND): Cardinal;
var
  lPid: DWORD;
begin
  Result := 0;
  if aWnd = 0 then
    Exit;

  lPid := 0;
  GetWindowThreadProcessId(aWnd, @lPid);
  Result := lPid;
end;

function GetProcessImageFileName(const aPid: Cardinal): string;
var
  lBuffer: array[0..MAX_PATH * 4 - 1] of Char;
  lProcess: THandle;
  lSize: DWORD;
begin
  Result := '';
  if aPid = 0 then
    Exit;

  lProcess := OpenProcess(cPROCESS_QUERY_LIMITED_INFORMATION or PROCESS_QUERY_INFORMATION, False, aPid);
  if lProcess = 0 then
    Exit;
  try
    lSize := Length(lBuffer);
    if QueryFullProcessImageName(lProcess, 0, lBuffer, lSize) then
      SetString(Result, lBuffer, lSize);
  finally
    CloseHandle(lProcess);
  end;
end;

function GetFileName(const aWnd: HWND): string;
begin
  Result := GetProcessImageFileName(RetrievePID(aWnd));
end;

procedure PrintWindow(const aWnd: HWND; const aBitmap: TBitmap);
var
  lBitmap: HBITMAP;
  lDC: HDC;
  lMemDC: HDC;
  lOldObject: HGDIOBJ;
  lRect: TRect;
begin
  if (aWnd = 0) or (not Assigned(aBitmap)) then
    Exit;

  if not GetWindowRect(aWnd, lRect) then
    Exit;

  lDC := GetDC(aWnd);
  if lDC = 0 then
    Exit;
  try
    lMemDC := CreateCompatibleDC(lDC);
    if lMemDC = 0 then
      Exit;
    try
      lBitmap := CreateCompatibleBitmap(lDC, lRect.Width, lRect.Height);
      if lBitmap = 0 then
        Exit;
      lOldObject := SelectObject(lMemDC, lBitmap);
      try
        SendMessage(aWnd, WM_PRINT, WPARAM(lMemDC), PRF_CHILDREN or PRF_CLIENT or PRF_ERASEBKGND or PRF_NONCLIENT or
          PRF_OWNED);
      finally
        SelectObject(lMemDC, lOldObject);
      end;
      aBitmap.Handle := lBitmap;
    finally
      DeleteDC(lMemDC);
    end;
  finally
    ReleaseDC(aWnd, lDC);
  end;
end;

function CopyIconFromWindowHandle(const aHandle: THandle; const aIcon: TIcon): Boolean;
var
  lIcon: HICON;
begin
  Result := False;
  if (aHandle = 0) or (not Assigned(aIcon)) then
    Exit;

  lIcon := HICON(GetClassLongPtr(aHandle, cGCLP_HICON));
  if lIcon = 0 then
    lIcon := HICON(SendMessage(aHandle, WM_GETICON, ICON_BIG, 0));
  if lIcon = 0 then
    lIcon := HICON(SendMessage(aHandle, WM_GETICON, ICON_SMALL, 0));
  if lIcon = 0 then
    Exit;

  aIcon.Handle := CopyIcon(lIcon);
  Result := aIcon.Handle <> 0;
end;

function ForceForegroundWindow(const aWnd: THandle): Boolean;
begin
  if IsIconic(aWnd) then
    ShowWindow(aWnd, SW_RESTORE);

  SetActiveWindow(aWnd);
  SetForegroundWindow(aWnd);
  Result := GetForegroundWindow = aWnd;
end;

procedure InitializeComForCurrentThread(out aShouldUninitialize: Boolean);
var
  lResult: HRESULT;
begin
  aShouldUninitialize := False;
  lResult := CoInitializeEx(nil, COINIT_MULTITHREADED);
  if (lResult = S_OK) or (lResult = S_FALSE) then
    aShouldUninitialize := True;
end;

function RetrieveCommandLine(const aPid: Cardinal): string;
var
  lComInited: Boolean;
  lEnum: IEnumVariant;
  lFetched: ULONG;
  lItem: OleVariant;
  lObjSet: OleVariant;
  lServices: OleVariant;
  lWql: string;
  lLocator: OleVariant;
begin
  Result := '';
  if aPid = 0 then
    Exit;

  InitializeComForCurrentThread(lComInited);
  try
    CoInitializeSecurity(nil, -1, nil, nil, cRPC_C_AUTHN_LEVEL_DEFAULT, cRPC_C_IMP_LEVEL_IMPERSONATE, nil, cEOAC_NONE,
      nil);
    lLocator := CreateOleObject('WbemScripting.SWbemLocator');
    lServices := lLocator.ConnectServer('.', 'root\cimv2');
    lWql := Format('SELECT CommandLine FROM Win32_Process WHERE ProcessId=%d', [aPid]);
    lObjSet := lServices.ExecQuery(lWql, 'WQL', 0);
    lEnum := IUnknown(lObjSet._NewEnum) as IEnumVariant;
    lItem := Unassigned;
    lFetched := 0;

    if lEnum.Next(1, lItem, lFetched) = S_OK then
    begin
      if not VarIsNull(lItem.CommandLine) then
        Result := VarToStr(lItem.CommandLine);
    end;
  finally
    if lComInited then
      CoUninitialize;
  end;
end;

function SHGetPropertyStoreForWindow(hwnd: HWND; const riid: TGUID; out ppv): HRESULT; stdcall;
  external 'shell32.dll';

function PSGetPropertyKeyFromName(pszName: PWideChar; out pkey: PROPERTYKEY): HRESULT; stdcall;
  external 'propsys.dll';

function PropVariantToStringAlloc(const propvar: PROPVARIANT; out ppszOut: PWideChar): HRESULT; stdcall;
  external 'propsys.dll';

function GetWindowPropertyString(const aWnd: HWND; const aCanonicalName: string): string;
var
  lHr: HRESULT;
  lKey: PROPERTYKEY;
  lOutStr: PWideChar;
  lPropVariant: PROPVARIANT;
  lStore: IPropertyStore;
begin
  Result := '';
  if aWnd = 0 then
    Exit;

  lStore := nil;
  if Failed(SHGetPropertyStoreForWindow(aWnd, IPropertyStore, lStore)) then
    Exit;
  if Failed(PSGetPropertyKeyFromName(PWideChar(aCanonicalName), lKey)) then
    Exit;

  lPropVariant := Default(PROPVARIANT);
  lHr := lStore.GetValue(lKey, lPropVariant);
  if Failed(lHr) then
    Exit;

  lOutStr := nil;
  try
    lHr := PropVariantToStringAlloc(lPropVariant, lOutStr);
    if Succeeded(lHr) and (lOutStr <> nil) then
      Result := lOutStr;
  finally
    if lOutStr <> nil then
      CoTaskMemFree(lOutStr);
    PropVariantClear(lPropVariant);
  end;
end;

function RetrieveAppUserModelID(const aWnd: HWND): string;
begin
  Result := GetWindowPropertyString(aWnd, 'System.AppUserModel.ID');
end;

function RetrieveRelaunchCommand(const aWnd: HWND): string;
begin
  Result := GetWindowPropertyString(aWnd, 'System.AppUserModel.RelaunchCommand');
end;

initialization
  glProgramManager := FindWindow('Progman', nil);
  glTaskBarWnd := FindWindow('Shell_TrayWnd', nil);
  glSysFader := FindWindow('SysFader', nil);
  glTooltipsClass32 := FindWindow('Tooltips_class32', nil);

end.
