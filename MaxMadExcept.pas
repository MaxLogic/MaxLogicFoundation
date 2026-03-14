unit MaxMadExcept;

interface

uses
  madExcept,
  System.SysUtils, system.Classes, generics.collections, system.syncObjs;

procedure AdjustMadExcept(const aBugReportDir: String);
procedure AddFileToMadExcept(const FileName: string);

procedure UpdateMailSubject(const ErrorMessage: string);
procedure handleException(const exceptIntf: IMEException; var handled: boolean);
procedure MadExceptOnBeforeSendMail(Action: TExceptAction;
  const exceptIntf: IMEException;
  var handled: boolean);

implementation

uses
  maxLogic.IOUtils, maxLogic.madExcept, maxAsync,
  MaxLogic.MadExceptStrings, autoFree, system.ioUtils;

var
  fBuildInfo: string = '';

resourcestring
cMStr_MailBody =
    'Error: %exceptMsg%' + '%lf%' +
    'Exe: %appname%' + '%lf%' +
    'Version: %appversion%' + '%lf%' +
    'ErrorDetails: %errorDetails%';


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
begin
  UpdateMailSubject(exceptIntf.ExceptMessage);
end;

procedure AdjustMadExcept(const aBugReportDir: String);
var
  mcfg: IMEModuleSettings;
  lSettings: IMESettings;
  lAttachments: IMEAttachments;
  LZipFileName: string;

  procedure Add(const aFileName: string);
  begin
    lAttachments.Add(aFileName,
      extractFileName(aFileName),
      LZipFileName,
      '');
  end;

var
  s: string;
  lTranslations: TStringList;
begin
  // madexcept.RegisterExceptionHandler(handleException, stDontSync, epQuickFiltering);
  madExcept.RegisterExceptActionHandler(MadExceptOnBeforeSendMail, stDontSync);

  LZipFileName := 'Log.zip';
  mcfg := MESettings;
  lSettings := MESettings();

  mcfg.BugReportFile := TPath.Combine(aBugReportDir, 'bugreport.Log');
  mcfg.BugReportZip := LZipFileName;
  //mcfg.ScreenShotDepth := 4;
  mcfg.ScreenShotAppOnly := True;
  mcfg.ScreenShotSendAs := 'Screen.jpg';
  mcfg.ScreenShotZip := 'screen.zip';

  // mcfg.Enabled := True;
  // mcfg.CheckFileCrc := True;
  mcfg.AutoSave := True;
  // mcfg.AutoClipboard := pawel1.IsDelphiRunning;
  mcfg.ShowPleaseWaitBox := True;

  // Send mail Options
  fBuildInfo := Trim(ExtractFileName(ParamStr(0)) + ' ' +
    maxLogic.IOUtils.GetBuildInfo);
  mcfg.MailSubject := 'Bugreport - ' + fBuildInfo;
  mcfg.AutoSend := True;

  maxLogic.madExcept.SetUpWebUpload('https://maxlogic.eu/bugreport_mailer/bugreport_mailer.php', 'maxlogic');

  mcfg.HideUglyItems := True;
  lAttachments := mcfg.AdditionalAttachments;
  // add('somefile');

  gc(lTranslations, TStringList.Create);
  lTranslations.Values['MailBody']:= cMStr_MailBody ;
  MaxLogic.MadExceptStrings.Translate(mcfg, lTranslations);

  lAttachments := nil;
  mcfg := nil;
end;

var
  glSyncMadExceptFileAdder: TFixedCriticalSection = nil;
  glMadExceptAttachments: TDictionary<string, BYTE> = nil;

procedure AddFileToMadExcept(const FileName: string);
var
  mcfg: IMEModuleSettings;
  Attachments: IMEAttachments;
  ZipFileName: string;
  X: integer;
  found: boolean;
  LoFn: string;
  lCS: TFixedCriticalSection;
  lDic: TDictionary<string, BYTE>;
begin
  // locking free create of critical section
  if glSyncMadExceptFileAdder = nil then
  begin
    lCS := TFixedCriticalSection.Create;
    if TInterlocked.CompareExchange(Pointer(glSyncMadExceptFileAdder), Pointer(lCS), nil) <> nil then
      lCS.Free;
  end;
  // locking free create of dictionary
  if glMadExceptAttachments = nil then
  begin
    lDic := TDictionary<string, BYTE>.Create;
    if TInterlocked.CompareExchange(Pointer(glMadExceptAttachments), Pointer(lDic), nil) <> nil then
      lDic.Free;
  end;

  LoFn := AnsiLowerCase(FileName);
  found := glMadExceptAttachments.ContainsKey(LoFn);
  if not found then
    glSyncMadExceptFileAdder.enter;
  begin
    try
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
end;

procedure UpdateMailSubject(const ErrorMessage: string);
var
  mcfg: IMEModuleSettings;
begin
  mcfg := MESettings;
  mcfg.MailSubject := 'Bugreport - ' + fBuildInfo + ' - ' + ErrorMessage;
end;

initialization

  // This function hides all leaks in unit "initialization" sections.
  // It's ok to use this for EXEs, but not recommended for DLLs.
  // HideInitializationLeaks;

  // hide some common leaks that we can not change...
  HideLeak('TJvCustomComboEdit.DefaultImages');

finalization

  if glSyncMadExceptFileAdder <> nil then
  begin
    glSyncMadExceptFileAdder.Free;
    glSyncMadExceptFileAdder := nil;
  end;
  if assigned(glMadExceptAttachments) then
  begin
    glMadExceptAttachments.Free;
    glMadExceptAttachments := nil;
  end;

end.


