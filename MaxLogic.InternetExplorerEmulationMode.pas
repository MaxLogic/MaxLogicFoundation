unit MaxLogic.InternetExplorerEmulationMode;

{Version: 1.1}

interface

uses
  windows, classes, sysUtils, system.types;


// based on http://www.cyotek.com/blog/configuring-the-emulation-mode-of-an-internet-explorer-webbrowser-control
// and https://weblog.west-wind.com/posts/2011/may/21/web-browser-control-specifying-the-ie-version
Type
  TInternetExplorerEmulationMode = Class
  Public Const
    ie11_enforced = 11001; // Internet Explorer 11. Webpages are displayed in IE11 edge mode, regardless of the !DOCTYPE directive.
    ie11 = 11000; // IE11. Webpages containing standards-based !DOCTYPE directives are displayed in IE11 edge mode. Default value for IE11.
    ie10_enforced = 10001; // Internet Explorer 10. Webpages are displayed in IE10 Standards mode, regardless of the !DOCTYPE directive.
    ie10 = 10000;
    // Internet Explorer 10. Webpages containing standards-based !DOCTYPE directives are displayed in IE10 Standards mode. Default value for Internet Explorer 10.
    ie9_enforced = 9999; // Windows Internet Explorer 9. Webpages are displayed in IE9 Standards mode, regardless of the !DOCTYPE directive.
    ie9 = 9000;
    // Internet Explorer 9. Webpages containing standards-based !DOCTYPE directives are displayed in IE9 mode. Default value for Internet Explorer 9.
    ie8_enforced = 8888; // Webpages are displayed in IE8 Standards mode, regardless of the !DOCTYPE directive.
    ie8 = 8000; // Webpages containing standards-based !DOCTYPE directives are displayed in IE8 mode. Default value for Internet Explorer 8
    ie7 = 7000;
    // Webpages containing standards-based !DOCTYPE directives are displayed in IE7 Standards mode. Default value for applications hosting the WebBrowser Control.

    // returns  only the major version, 0on failure
    Class Function GetInstalledInternetExplorerMajorVersion: dword;
    // returns 0 if failed
    Class Function IEMajorVersionToEmulationFlag(IEMajorVersion: dword; UseEnforcedMode: boolean = True; FailOnNewerVersion: boolean = false): dword;
    // value should be one of the ieX constants above. ForExe should be the name of the exe to be registered, please note, if leaving blank, the current exe will be used
    Class Function SetWebBrowserEmulationMode(Value: dword; forExe: String = ''): boolean;
    // returns 0 if failed or if nothing is set yet
    Class Function GetWebBrowserEmulationMode(forExe: String = ''): dword;
    // returns 0 if nothing was written, otherwise the emulation mode constant
    Class Function SetNewestEmulationMode(forExe: String = ''): dword;

    Class   Procedure SetMixed32And64bitTabs;
  End;


implementation

uses
  Registry, strUtils;
{ TInternetExplorerEmulationMode }

Class
  Function TInternetExplorerEmulationMode.GetInstalledInternetExplorerMajorVersion: dword;
Var
  reg: TRegistry;
  s: String;
  p: TStringDynArray;
Begin
  result := 0;

  reg := TRegistry.Create;
  Try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    If reg.openKeyReadOnly('SOFTWARE\Microsoft\Internet Explorer') Then
    Begin
      // Older versions of IE used the Version value, while newer versions use svcVersion. In either case, this value contains the version string.
      If reg.ValueExists('svcVersion') Then
        s := reg.readString('svcVersion')
      Else If reg.ValueExists('Version') Then
        s := reg.readString('Version')
      Else
        s := '';

      If s <> '' Then
      Begin
          p := strUtils.SplitString(s, '.');
        If Length(p) >= 1 Then
          result := StrToIntDef(p[0], 0);
      End;
    End;
  Except
    // do nothing
  End;
  reg.Free;
End;

Class
  Function TInternetExplorerEmulationMode.SetWebBrowserEmulationMode(Value: dword; forExe: String = ''): boolean;
Var
  reg: TRegistry;
Begin
  result := false;

  If Value = 0 Then
    Exit;

  If forExe = '' Then
    forExe := ExtractFileName(ParamStr(0));
  // Setting the emulation version is very straightforward - add a value to the registry in the below key containing the name of your executable file and a value from the ieX constants

  // (or HKEY_CURRENT_USER)
  reg := TRegistry.Create;
  Try
    reg.RootKey := HKEY_CURRENT_USER;
    If reg.openKey('SOFTWARE\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION', True) Then
    Begin
        reg.WriteInteger(forExe, Value);
      result := True;
    End;
  Except
    // do nothing
  End;
  reg.Free;
End;


                                                                 // seee https://docs.microsoft.com/en-US/troubleshoot/browsers/32-bit-browser-applications-not-working-as-expected
// you need to set
// HKEY_CURRENT_USER\Software\Microsoft\Internet Explorer\Main\TabProcGrowth
// to a non 0 value
// reason: on a windows 64bit machne, the IE Tabs run in x64 - always
// so 32bit plugins can not be loaded
Class   Procedure TInternetExplorerEmulationMode.SetMixed32And64bitTabs;
Var
  reg: TRegistry;
Begin
  reg := TRegistry.Create;
  Try
    reg.RootKey := HKEY_CURRENT_USER;
    If reg.openKey('Software\Microsoft\Internet Explorer\Main', True) Then
        reg.WriteInteger('TabProcGrowth', 1);
  Except
    // do nothing
  End;
  reg.Free;
End;

Class
  Function TInternetExplorerEmulationMode.GetWebBrowserEmulationMode(forExe: String): dword;
Var
  reg: TRegistry;
Begin
  result := 0;

  If forExe = '' Then
    forExe := ExtractFileName(ParamStr(0));

  // (or HKEY_CURRENT_USER)
  reg := TRegistry.Create;
  Try
    reg.RootKey := HKEY_CURRENT_USER;
    If reg.openKeyReadOnly('SOFTWARE\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION') Then
      If reg.ValueExists(forExe) Then
        result := reg.ReadInteger(forExe);
  Except
    // do nothing
  End;
  reg.Free;

End;

Class
  Function TInternetExplorerEmulationMode.IEMajorVersionToEmulationFlag(IEMajorVersion: dword; UseEnforcedMode: boolean = True;
FailOnNewerVersion: boolean = false): dword;

  Function ef(EnforcedValue, NormalValue: dword): dword;
  Begin
    If UseEnforcedMode Then
      result := EnforcedValue
    Else
      result := NormalValue;
  End;

Begin
  If IEMajorVersion > 11 Then
  Begin
    If FailOnNewerVersion Then
      Exit(0);

    Exit(ef(ie11_enforced, ie11));
  End;

  Case IEMajorVersion Of
    11:
      result := ef(ie11_enforced, ie11);
    10:
      result := ef(ie10_enforced, ie10);
    9:
      result := ef(ie9_enforced, ie9);
    8:
      result := ef(ie8_enforced, ie8);
    7:
      result := ie7;

  Else
    result := 0;
  End;
End;

Class
  Function TInternetExplorerEmulationMode.SetNewestEmulationMode(forExe: String): dword;
Var
  ieEmulation, ieMajor: dword;
Begin
  result := 0;

  ieMajor := TInternetExplorerEmulationMode.GetInstalledInternetExplorerMajorVersion;
  ieEmulation := IEMajorVersionToEmulationFlag(ieMajor);
  If ieEmulation = 0 Then
    Exit;

  If GetWebBrowserEmulationMode(forExe) <> ieEmulation Then
    If SetWebBrowserEmulationMode(ieEmulation, forExe) Then
      result := ieEmulation;
End;


Initialization

TInternetExplorerEmulationMode.SetNewestEmulationMode;

end.
