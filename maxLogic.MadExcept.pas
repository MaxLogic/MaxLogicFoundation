unit maxLogic.madExcept;

interface

uses
  madExcept,
  winApi.Windows, System.SysUtils, generics.collections, System.Classes;

procedure AdjustMadExcept(const aLogdir: string);

procedure SetUpWebUpload(const aUrl, aTenant: String; aDisableOtherSendBugRportMethods: Boolean = True);

// --- smtp specific
procedure SetUpSmtp(const aServer, aUsername, aPassword: string; aPort: integer);
procedure SetBugReportMailRecipient(const amailAddres: string);
// ---

procedure AddFileToMadExcept(const FileName: string);
// thread safe
procedure AddFieldToBugReportHeader(const aName, aValue: string);

procedure UpdateMailSubject(const ErrorMessage: string);
procedure handleException(const exceptIntf: IMEException; var handled: boolean);

procedure MadExceptOnBeforeSendMail(Action: TExceptAction;
  const exceptIntf: IMEException;
  var handled: boolean);

function GetExceptionStackTrace(aException: Exception): string;

implementation

uses
  maxLogic.MadExceptStrings,
  maxLogic.IOUtils, System.syncObjs, madStackTrace;

var
  fBuildInfo: string = '';
  fBugReportMailRecipient: string = 'pawel@maxlogic.eu';

  glAddFieldToBugReportHeaderList: TStringList;
  glAddFieldToBugReportHeaderCS: TCriticalSection;

var glLastBugReport: string;

function GetExceptionStackTrace(aException: Exception): string;
begin
  Result := madExcept.GetCrashStackTrace;
end;

procedure MadExceptOnBeforeSendMail(Action: TExceptAction;
  const exceptIntf: IMEException;
  var handled: boolean);
var
  s: string;
begin
  if Action = eaSendBugReport then
  begin
    // In some situations you might want to check whether the current exception is already stored somewhere.
    // Comparing exceptions is not too easy, though. Especially comparing callstacks is tough.
    // So madExcept offers you some help by adding a "CallstackCrc" property to the "IMEException" interface. It consists of 3 dwords.
    // The first dword designates the exception location only.
    // The second dword is a crc of the callstack of the crashed thread.
    // The third dword is a crc of the callstack of all delphi threads.
    // Please note that this information is only available after all the callstacks were fully calculated.
    // So asking this property before the "epCompleteReport" phase doesn't make too much sense.
    // property IMEException.CallstackCrc [index: integer] : dword;

    s := '[ID:' +
      IntTohex(exceptIntf.CallstackCrc[0], 1) + '.' +
      IntTohex(exceptIntf.CallstackCrc[1], 1) + '] ErrorMessage: ' +
      exceptIntf.ExceptMessage;

    UpdateMailSubject(s);
  end;
end;

procedure handleException(const exceptIntf: IMEException; var handled: boolean);
var
  X: integer;
  n, v: string;
begin
  // UpdateMailSubject(exceptIntf.ExceptMessage);
  exceptIntf.BugReportHeader.lock;
  try
    glAddFieldToBugReportHeaderCS.enter;
    try
      for X := 0 to glAddFieldToBugReportHeaderList.Count - 1 do
      begin
        n := glAddFieldToBugReportHeaderList.names[X];
        v := glAddFieldToBugReportHeaderList.ValueFromIndex[X];
        if n <> '' then
          exceptIntf.BugReportHeader[n] := v;
      end;

    finally
      glAddFieldToBugReportHeaderCS.Leave;
    end;
  finally
    exceptIntf.BugReportHeader.unLock;
  end;
end;

procedure SetUpWebUpload(const aUrl, aTenant: String; aDisableOtherSendBugRportMethods: Boolean = True);
var
  mcfg: IMEModuleSettings;
begin
  mcfg := MESettings;

  // use web upload only
  if aDisableOtherSendBugRportMethods then
  begin
    mcfg.MailViaMapi := False;
    mcfg.MailViaMailto := False;
    mcfg.MailAsSmtpServer := False;
    mcfg.MailAsSmtpClient := False;
    mcfg.MailViaMapi := False;
    mcfg.MailViaMailto := False;
  end;

  mcfg.UploadToCustomScript := true;
  mcfg.HttpServer:= aUrl;
  mcfg.HttpSsl:= True;
  // Default port (0) means auto-select based on HttpSsl (443 for HTTPS, 80 for HTTP)
  mcfg.HttpPort := 0;

  // You might need to specify a custom field name if your PHP expects it explicitly
  // By default, madExcept sends the bug report in the "bugreport" field
  mcfg.AdditionalFields['BugReportField'] := 'bugreport';

  // Optional: add extra form fields if needed
  // mcfg.AdditionalFields['username'] := 'exampleuser';
  if aTenant<>'' then
    mcfg.AdditionalFields['customerIdentifier'] := aTenant;
end;

procedure SetUpSmtp(const aServer, aUsername, aPassword: string; aPort: integer);
var
  mcfg: IMEModuleSettings;
begin
  mcfg := MESettings;

  // use local mail client
  // mcfg.MailViaMapi := true;
  // mcfg.MailViaMailto := False;
  // mcfg.MailAsSmtpServer := False;

  // smtp client
  mcfg.MailAsSmtpClient := True;
  mcfg.SmtpServer := aServer; // 'mail.maxlogic.eu';
  mcfg.SmtpPort := aPort; // 465;
  mcfg.SmtpAccount := aUsername; // 'bugreport@maxlogic.eu';
  mcfg.SmtpPassword := aPassword;
  mcfg.SmtpSsl := True;
  mcfg.SmtpTls := False;
end;

procedure SetBugReportMailRecipient(const amailAddres: string);
var
  mcfg: IMEModuleSettings;
begin
  fBugReportMailRecipient := amailAddres;

  mcfg := MESettings;
  mcfg.MailAddr := fBugReportMailRecipient;
end;

procedure AdjustMadExcept(const aLogdir: string);
var
  mcfg: IMEModuleSettings;
  ZipFileName: string;
  s: string;
begin

  madExcept.RegisterExceptActionHandler(MadExceptOnBeforeSendMail, stDontSync);

  ZipFileName := 'Log.zip';
  mcfg := MESettings;
  // reload resource strings
  try
    mcfg.Reload;
  except
    // do nothing
  end;
  // CollectStrings(mcfg);
  Translate(mcfg);

  // Message box kind//  TMEShowSetting = (ssFullBox, ssAssistant, ssDetailBox, ssSimpleBox, ssNothing);
  // mcfg.Filter1ShowSetting := ssDetailBox;
  // mcfg.Filter2ShowSetting := ssDetailBox;

  mcfg.BugReportFile := aLogdir + 'bugreport.log';
  mcfg.BugReportZip := ZipFileName;
  // mcfg.ScreenShotDepth := 4;
  // mcfg.ScreenShotAppOnly := true;
  // mcfg.ScreenShotSendAs := 'Screen.jpg';
  // mcfg.ScreenShotZip := 'screen.zip';

  // mcfg.Enabled := True;
  // mcfg.CheckFileCrc := True;
  mcfg.AutoSave := True;
  mcfg.AutoClipboard := FindWindow('TAppBuilder', nil) > 0; // is delphi running
  mcfg.ShowPleaseWaitBox := True;

  // Send mail Options
  fBuildInfo := Trim(ExtractFileName(maxLogic.IOUtils.GetCurrentDLLName) + ' ' +
    maxLogic.IOUtils.GetBuildInfo);
  mcfg.MailSubject := 'Bugreport - ' + fBuildInfo;
  mcfg.MailAddr := fBugReportMailRecipient;

  { // upload to web server
    property IMESettings.UploadToFogBugz      : boolean;    // create FogBogz bug report
    property IMESettings.UploadToBugZilla     : boolean;    // create BugZilla bug report
    property IMESettings.UploadToMantis       : boolean;    // create Mantis bug report
    property IMESettings.UploadToCustomScript : boolean;    // upload to HTTP custom script
    property IMESettings.HttpServer           : string;     // FogBugz/BugZilla/Mantis/script url
    property IMESettings.HttpSsl              : boolean;    // use SSL connection
    property IMESettings.HttpPort             : dword;      // connection port
    property IMESettings.HttpAccount          : string;     // http auth user name (optional)
    property IMESettings.HttpPassword         : string;     // http auth password  (optional)
    property IMESettings.BugTrackerAccount    : string;     // FogBugz/BugZilla/Mantis account
    property IMESettings.BugTrackerPassword   : string;     // FogBugz/BugZilla/Mantis password
    property IMESettings.BugTrackerProject    : string;     // FogBugz/BugZilla/Mantis project name
    property IMESettings.BugTrackerArea       : string;     // FogBugz/BugZilla/Mantis area name
    property IMESettings.BugTrackerAssignTo   : string;     // FogBugz/BugZilla/Mantis assign to
  }

  mcfg := nil;
end;

var
  glSyncMadExceptFileAdder: TCriticalSection = nil;
  glMadExceptAttachments: TDictionary<string, BYTE> = nil;

procedure AddFileToMadExcept(const FileName: string);
var
  mcfg: IMEModuleSettings;
  Attachments: IMEAttachments;
  ZipFileName: string;
  X: integer;
  found: boolean;
  LoFn: string;
begin
  if glSyncMadExceptFileAdder = nil then
    glSyncMadExceptFileAdder := TCriticalSection.Create;
  glSyncMadExceptFileAdder.enter;
  try
    if glMadExceptAttachments = nil then
      glMadExceptAttachments := TDictionary<string, BYTE>.Create;

    LoFn := AnsiLowerCase(FileName);
    found := glMadExceptAttachments.ContainsKey(LoFn);
    if not found then
    begin
      glMadExceptAttachments.Add(LoFn, 0);

      mcfg := MESettings;

      // mcfg.Filter1ShowSetting := ssDetailBox;
      // mcfg.Filter2ShowSetting := ssDetailBox;

      ZipFileName := mcfg.BugReportZip;
      Attachments := mcfg.AdditionalAttachments;

      Attachments.Add(FileName,
        ExtractFileName(FileName),
        ZipFileName,
        '');

      Attachments := nil;
      mcfg := nil;
    end;
  finally
    glSyncMadExceptFileAdder.Leave;
  end;
end;

procedure UpdateMailSubject(const ErrorMessage: string);
var
  mcfg: IMEModuleSettings;
begin
  mcfg := MESettings;
  mcfg.MailSubject := 'Bugreport - ' + fBuildInfo + ' - ' + ErrorMessage;
end;

procedure AddFieldToBugReportHeader(const aName, aValue: string);
begin
  glAddFieldToBugReportHeaderCS.enter;
  try
    glAddFieldToBugReportHeaderList.Values[aName] := aValue;
  finally
    glAddFieldToBugReportHeaderCS.Leave;
  end;
end;

initialization
  // hide some common leaks that we can not change...
  HideLeak('TJvCustomComboEdit.DefaultImages');

  glAddFieldToBugReportHeaderList := TStringList.Create;
  glAddFieldToBugReportHeaderCS := TCriticalSection.Create;

  {$IFDEF Win32}
  glAddFieldToBugReportHeaderList.Values['Bitness'] := 'Win32';
  {$ENDIF}

  {$IFDEF win64}
  glAddFieldToBugReportHeaderList.Values['Bitness'] := 'Win64';
  {$ENDIF}

finalization

  FreeAndNil(glSyncMadExceptFileAdder);
  FreeAndNil(glMadExceptAttachments);
  FreeAndNil(glAddFieldToBugReportHeaderList);
  FreeAndNil(glAddFieldToBugReportHeaderCS);

end.

