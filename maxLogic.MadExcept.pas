Unit MaxLogic.MadExcept;

Interface

Uses
  MadExcept,
  Windows, sysUtils, generics.collections, classes;

Procedure AdjustMadExcept(Const aLogdir: String);
Procedure SetUpSmtp(Const aServer, aUsername, aPassword: String; aPort: Integer);
Procedure SetBugReportMailRecipient(Const amailAddres: String);
Procedure AddFileToMadExcept(Const Filename: String);
// thread safe
Procedure AddFieldToBugReportHeader(Const aName, aValue: String);

Procedure UpdateMailSubject(Const ErrorMessage: String);
Procedure handleException(Const exceptIntf: IMEException; Var handled: boolean);

Procedure MadExceptOnBeforeSendMail(Action: TExceptAction;
  Const exceptIntf: IMEException;
  Var handled: boolean);

Implementation

Uses
  MaxLogic.MadExceptStrings,
  MaxLogic.ioUtils, maxAsync, syncObjs;

Var
  fBuildInfo: String = '';
  fBugReportMailRecipient: String = 'pawel@maxlogic.eu';

  AddFieldToBugReportHeaderList: TStringList;
  AddFieldToBugReportHeaderCS: TCriticalSection;

Procedure MadExceptOnBeforeSendMail(Action: TExceptAction;
  Const exceptIntf: IMEException;
  Var handled: boolean);
Var
  s: String;
Begin
  If Action = eaSendBugReport Then
  Begin
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
      IntToHex(exceptIntf.CallstackCrc[0], 1) + '.' +
      IntToHex(exceptIntf.CallstackCrc[1], 1) + '] ErrorMessage: ' +
      exceptIntf.ExceptMessage;

    UpdateMailSubject(s);
  End;
End;

Procedure handleException(Const exceptIntf: IMEException; Var handled: boolean);
Var
  x: Integer;
  n, v: String;
Begin
  // UpdateMailSubject(exceptIntf.ExceptMessage);
  exceptIntf.BugReportHeader.Lock;
  Try
    AddFieldToBugReportHeaderCS.enter;
    Try
      For x := 0 To AddFieldToBugReportHeaderList.Count - 1 Do
      Begin
        n := AddFieldToBugReportHeaderList.names[x];
        v := AddFieldToBugReportHeaderList.ValueFromIndex[x];
        If n <> '' Then
          exceptIntf.BugReportHeader[n] := v;
      End;

    Finally
      AddFieldToBugReportHeaderCS.Leave;
    End;
  Finally
    exceptIntf.BugReportHeader.unLock;
  End;
End;

Procedure SetUpSmtp(Const aServer, aUsername, aPassword: String; aPort: Integer);
Var
  mcfg: IMEModuleSettings;
Begin
  mcfg := MESettings;

  // use local mail client
  // mcfg.MailViaMapi := true;
  // mcfg.MailViaMailto := False;
  // mcfg.MailAsSmtpServer := False;

  // smtp client
  mcfg.MailAsSmtpClient := true;
  mcfg.SmtpServer := aServer; // 'mail.maxlogic.eu';
  mcfg.SmtpPort := aPort; // 465;
  mcfg.SmtpAccount := aUsername; // 'bugreport@maxlogic.eu';
  mcfg.SmtpPassword := aPassword;
  mcfg.SmtpSsl := true;
  mcfg.SmtpTls := False;
End;

Procedure SetBugReportMailRecipient(Const amailAddres: String);
Var
  mcfg: IMEModuleSettings;
Begin
  fBugReportMailRecipient := amailAddres;

  mcfg := MESettings;
  mcfg.MailAddr := fBugReportMailRecipient;
End;

Procedure AdjustMadExcept(Const aLogdir: String);
Var
  mcfg: IMEModuleSettings;
  ZipFileName: String;
  s: String;
Begin
   madexcept.RegisterExceptionHandler(handleException, stDontSync, epQuickFiltering);
  MadExcept.RegisterExceptActionHandler(MadExceptOnBeforeSendMail, stDontSync);

  ZipFileName := 'Log.zip';
  mcfg := MESettings;
  // reload resource strings
  Try
    mcfg.Reload;
  Except
    // do nothing
  End;
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
  mcfg.AutoSave := true;
  mcfg.AutoClipboard := FindWindow('TAppBuilder', Nil) > 0; // is delphi running
  mcfg.ShowPleaseWaitBox := true;

  // Send mail Options
  fBuildInfo := Trim(ExtractFilename(MaxLogic.ioUtils.GetCurrentDLLName) + ' ' +
    MaxLogic.ioUtils.GetBuildInfo);
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

  mcfg := Nil;
End;

Var
  glSyncMadExceptFileAdder: TFixedCriticalSection = Nil;
  glMadExceptAttachments: TDictionary<String, byte> = Nil;

Procedure AddFileToMadExcept(Const Filename: String);
Var
  mcfg: IMEModuleSettings;
  Attachments: IMEAttachments;
  ZipFileName: String;
  x: Integer;
  found: boolean;
  LoFn: String;
Begin
  If glSyncMadExceptFileAdder = Nil Then
    glSyncMadExceptFileAdder := TFixedCriticalSection.Create;
  glSyncMadExceptFileAdder.enter;
  Try
    If glMadExceptAttachments = Nil Then
      glMadExceptAttachments := TDictionary<String, byte>.Create;

    LoFn := AnsiLowerCase(Filename);
    found := glMadExceptAttachments.ContainsKey(LoFn);
    If Not found Then
    Begin
      glMadExceptAttachments.add(LoFn, 0);

      mcfg := MESettings;

      // mcfg.Filter1ShowSetting := ssDetailBox;
      // mcfg.Filter2ShowSetting := ssDetailBox;

      ZipFileName := mcfg.BugReportZip;
      Attachments := mcfg.AdditionalAttachments;

      Attachments.add(Filename,
        ExtractFilename(Filename),
        ZipFileName,
        '');

      Attachments := Nil;
      mcfg := Nil;
    End;
  Finally
    glSyncMadExceptFileAdder.Leave;
  End;
End;

Procedure UpdateMailSubject(Const ErrorMessage: String);
Var
  mcfg: IMEModuleSettings;
Begin
  mcfg := MESettings;
  mcfg.MailSubject := 'Bugreport - ' + fBuildInfo + ' - ' + ErrorMessage;
End;

Procedure AddFieldToBugReportHeader(Const aName, aValue: String);
Begin
  AddFieldToBugReportHeaderCS.enter;
  Try
    AddFieldToBugReportHeaderList.Values[aName] := aValue;
  Finally
    AddFieldToBugReportHeaderCS.Leave;
  End;
End;

Initialization

// This function hides all leaks in unit "initialization" sections.
// It's ok to use this for EXEs, but not recommended for DLLs.
// HideInitializationLeaks;

// hide some common leaks that we can not change...
HideLeak('TJvCustomComboEdit.DefaultImages');

AddFieldToBugReportHeaderList := TStringList.Create;
AddFieldToBugReportHeaderCS := TCriticalSection.Create;

{$IFDEF Win32}
AddFieldToBugReportHeaderList .values['Bitness'] := 'Win32';
{$ENDIF}

{$IFDEF Win64}
AddFieldToBugReportHeaderList .values['Bitness'] := 'Win64';
{$ENDIF}

Finalization

If glSyncMadExceptFileAdder <> Nil Then
Begin
  glSyncMadExceptFileAdder.Free;
  glSyncMadExceptFileAdder := Nil;
End;
If assigned(glMadExceptAttachments) Then
Begin
  glMadExceptAttachments.Free;
  glMadExceptAttachments := Nil;
End;

AddFieldToBugReportHeaderList.Free;
AddFieldToBugReportHeaderCS.Free;

End.
