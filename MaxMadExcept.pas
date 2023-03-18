unit MaxMadExcept;

interface
uses
  windows, SysUtils, madExcept, classes;


procedure AdjustMadExcept(const afLogdir : string);
Procedure AddFileToMadExcept(const FileName : String);
procedure Translate(mcfg : IMEModuleSettings);

implementation
uses
  bsUTILS, pawel1, GlobalConst, iniFiles, Forms, dialogs;



procedure Translate(mcfg : IMEModuleSettings);
begin
//  with mcfg do
//  begin

//
//
//   CUSTOM STRINGS
//
//   except box
//  property IMESettings.TitleBar           : string;  // title bar
//  property IMESettings.ExceptMsg          : string;  // exception message
//  property IMESettings.FrozenMsg          : string;  // frozen message
//  property IMESettings.BitFaultMsg        : string;  // bit fault message
//
//   except box - button captions
//  property IMESettings.SendBtnCaption     : string;
//  property IMESettings.SaveBtnCaption     : string;
//  property IMESettings.PrintBtnCaption    : string;
//  property IMESettings.ShowBtnCaption     : string;
//  property IMESettings.ContinueBtnCaption : string;
//  property IMESettings.RestartBtnCaption  : string;
//  property IMESettings.CloseBtnCaption    : string;
//
//   message box - button captions
//  property IMESettings.OkBtnCaption       : string;
//  property IMESettings.DetailsBtnCaption  : string;
//
//   please wait box - strings
//  property IMESettings.PleaseWaitTitle    : string;
//  property IMESettings.PleaseWaitText     : string;


//property IMESettings.MailSubject        : string;  // mail subject
//property IMESettings.MailBody           : string;  // mail body
//property IMESettings.SendBoxTitle       : string;  // send box title
//property IMESettings.PrepareAttachMsg   : string;  // prepare attachments
//property IMESettings.MxLookupMsg        : string;  // mx lookup
//property IMESettings.ConnectMsg         : string;  // server connect
//property IMESettings.AuthMsg            : string;  // authentication
//property IMESettings.SendMailMsg        : string;  // send mail
//property IMESettings.FieldsMsg          : string;  // set http fields
//property IMESettings.SendAttachMsg      : string;  // sending attachments
//property IMESettings.SendFinalizeMsg    : string;  // finalization
//property IMESettings.SendFailureMsg     : string;  // failure message

  
//  end;
end;


procedure AdjustMadExcept(const afLogdir : string);
var
  l : TStringList;
  ini : TiniFile;
  UploadURL : string;
  mcfg : IMEModuleSettings;
  Attachments : IMEAttachments ;
  ZipFileName : string;

  procedure Add(const LogName : String);
  begin
    Attachments.Add(afLogdir + LogName,
                              LogName,
                              ZipFileName,
                              '');
  end;

begin

  ZipFileName := 'Log.zip';
  mcfg := MESettings;
  Translate(mcfg);

  mcfg.BugReportFile := afLogdir + 'bugreport.log';
  mcfg.BugReportZip := ZipFileName;
  mcfg.ScreenShotDepth := 4;
  mcfg.ScreenShotAppOnly := True;
  mcfg.ScreenShotSendAs := 'Screen.jpg';
  mcfg.ScreenShotZip          := 'screen.zip';




//   mcfg.CheckFileCrc := True;
  mcfg.AutoSave           := True;
  mcfg.AutoClipboard      := pawel1.IsDelphiRunning;
  mcfg.ShowPleaseWaitBox  := True;




  // Send mail Options
  mcfg.AutoSend           := True;
//  mcfg.AutoSendPrgrBox    := True;

  mcfg.MailAddr := 'info@maxlogic.eu';
  // mcfg.MailFrom := 'info@maxlogic.eu';
  mcfg.MailSubject := 'BugReport - '+ExtractFileName(Application.ExeName);

  mcfg.MailAsSmtpClient  := True;
  mcfg.SmtpServer := 'mail.maxlogic.eu';
  mcfg.SmtpPort := 26;
  mcfg.SmtpAccount := 'bugreport@maxlogic.eu';
  mcfg.SmtpPassword := 'BugReportAbc123';


//
//  mcfg.MailAsSmtpServer   := false;
//  mcfg.MailViaMailto     := false;
//  mcfg.MailViaMapi       := false;
//    mcfg.UploadViaHttp     := false;

  mcfg.HideUglyItems := True;



  attachments := mcfg.AdditionalAttachments;
//  add('ORAuto-Trunc-Strings.log');


  attachments := NIL;
  mcfg := NIL;
end;




Procedure AddFileToMadExcept(const FileName : String);
var
  mcfg : IMEModuleSettings;
  Attachments : IMEAttachments ;
  ZipFileName : string;
begin
  mcfg := MESettings;
  ZipFileName := mcfg.BugReportZip;
  attachments := mcfg.AdditionalAttachments;

  Attachments.Add(FileName,
        ExtractFileName(FileName),
        ZipFileName,
        '');

  attachments := NIL;
  mcfg := NIL;

end;




end.
