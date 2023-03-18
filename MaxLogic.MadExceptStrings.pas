Unit MaxLogic.MadExceptStrings;
// see:
// http://help.madshi.net/madExceptSettings10.htm

Interface

Uses
madExcept,
  sysUtils, classes;


// this will use the resource strings from this unit
Procedure Translate(mcfg: IMEModuleSettings); Overload;
// this will use a string list to set up the strings
Procedure Translate(mcfg: IMEModuleSettings; l: TStringList; Const cLineBreakSubstitute: String = '\n'); Overload;
// just a helper function to get the strings out of the madExcept settings dialog into resourcestrings
Procedure CollectStrings(mcfg: IMEModuleSettings; l: TStringList; Const cLineBreakSubstitute: String = '\n');



Implementation

Uses
  MaxLogic.ioUtils;

Resourcestring
  // cMStr_TitleBar -'KFZMeister.exe';
  cMStr_ExceptMsg = 'In der Anwendung trat ein Fehler auf:' + slineBreak + '%exceptMsg%';
  cMStr_FrozenMsg = 'Die Anwendung scheint nicht mehr zu reagieren.';
  cMStr_BitFaultMsg = 'Die Datei "%exeName%" scheint defekt zu sein!';
  cMStr_SendBtnCaption = 'Fehlerbericht senden';
  cMStr_SaveBtnCaption = 'Fehlerbericht speichern';
  cMStr_PrintBtnCaption = 'Fehlerbericht drucken';
  cMStr_ShowBtnCaption = 'Fehlerbericht anzeigen';
  cMStr_ContinueBtnCaption = 'Anwendung fortfahren';
  cMStr_RestartBtnCaption = 'Anwendung neu starten';
  cMStr_CloseBtnCaption = 'Anwendung schlieﬂen';
  cMStr_OkBtnCaption = '&OK';
  cMStr_DetailsBtnCaption = '&Details';
  cMStr_PleaseWaitTitle = 'Information';
  cMStr_PleaseWaitText = 'Bitte einen Moment warten...';
  cMStr_BugTrackerTitle = '%exeName%, %exceptMsg%';
  cMStr_BugTrackerDescription = 'Fehler-Details:' + slineBreak + '%errorDetails%';
  cMStr_MailSubject = 'Fehlerbericht';
  cMStr_MailBody = 'Der Fehlerbericht befindet sich im Anhang' + '%lf%' +
    'Fehler: %exceptMsg%' + '%lf%' +
    'Exe: %appname%' + '%lf%' +
    'Version: %appversion%' + '%lf%' +
    'ErrorDetails: %errorDetails%';

  cMStr_SendBoxTitle = 'Sende Fehlerbericht...';
  cMStr_PrepareAttachMsg = 'Anh‰nge werden vorbereitet...';
  cMStr_MxLookupMsg = 'Suche Mail Server...';
  cMStr_ConnectMsg = 'Verbinde mit Server...';
  cMStr_SendMailMsg = 'Sende Mail...';
  cMStr_FieldsMsg = 'Setze Felder...';
  cMStr_SendAttachMsg = 'Sende Anh‰nge...';
  cMStr_SendFinalizeMsg = 'Beenden...';
  cMStr_SendFailureMsg = 'Entschuldigung, das Senden des Fehlerberichts schlug fehl.';




Procedure Translate(mcfg: IMEModuleSettings);
Var
  exeName: String;
  Function injectExename(Const s: String): String;
  Begin
    result := s;
    // result:=format(s,[exename]);
  End;

Begin
  exeName := ExtractFileName(MaxLogic.ioUtils.GetCurrentDLLName);

  With mcfg Do
  Begin
    // CUSTOM STRINGS

    // except box
    // TitleBar              := cMStr_TitleBar;
    // exception message
    ExceptMsg := cMStr_ExceptMsg;
    // frozen message
    FrozenMsg := cMStr_FrozenMsg;
    // bit fault message
    BitFaultMsg := injectExename(cMStr_BitFaultMsg);

    // except box = button captions
    SendBtnCaption := cMStr_SendBtnCaption;
    SaveBtnCaption := cMStr_SaveBtnCaption;
    PrintBtnCaption := cMStr_PrintBtnCaption;
    ShowBtnCaption := cMStr_ShowBtnCaption;
    ContinueBtnCaption := cMStr_ContinueBtnCaption;
    RestartBtnCaption := cMStr_RestartBtnCaption;
    CloseBtnCaption := cMStr_CloseBtnCaption;

    // message box = button captions
    OkBtnCaption := cMStr_OkBtnCaption;
    DetailsBtnCaption := cMStr_DetailsBtnCaption;

    // please wait box = strings
    PleaseWaitTitle := cMStr_PleaseWaitTitle;
    PleaseWaitText := cMStr_PleaseWaitText;

    // send
    BugTrackerTitle := injectExename(cMStr_BugTrackerTitle);
    BugTrackerDescription := cMStr_BugTrackerDescription;
    MailSubject := cMStr_MailSubject;
    MailBody := cMStr_MailBody;
    SendBoxTitle := cMStr_SendBoxTitle;
    // prepare attachments
    PrepareAttachMsg := cMStr_PrepareAttachMsg;
    MxLookupMsg := cMStr_MxLookupMsg;
    // server connect
    ConnectMsg := cMStr_ConnectMsg;
    // send mail
    SendMailMsg := cMStr_SendMailMsg;
    FieldsMsg := cMStr_FieldsMsg;
    SendAttachMsg := cMStr_SendAttachMsg;
    SendFinalizeMsg := cMStr_SendFinalizeMsg;
    SendFailureMsg := cMStr_SendFailureMsg;

  End;
End;

Procedure CollectStrings(mcfg: IMEModuleSettings; l: TStringList; Const cLineBreakSubstitute: String = '\n');

  Procedure Add(Const n, v: String);
  Begin
    l.Add(Trim(n) + '=' +
      StringReplace(v, slineBreak, cLineBreakSubstitute, [rfReplaceAll, rfIgnorecase]));
  End;

Begin

  With mcfg Do
  Begin
    // CUSTOM STRINGS

    // except box
    Add('TitleBar             ', TitleBar);
    // exception message
    Add('ExceptMsg            ', ExceptMsg);
    // frozen message
    Add('FrozenMsg            ', FrozenMsg);
    // bit fault message
    Add('BitFaultMsg          ', BitFaultMsg);

    // except box = button captions
    Add('SendBtnCaption       ', SendBtnCaption);
    Add('SaveBtnCaption       ', SaveBtnCaption);
    Add('PrintBtnCaption      ', PrintBtnCaption);
    Add('ShowBtnCaption       ', ShowBtnCaption);
    Add('ContinueBtnCaption   ', ContinueBtnCaption);
    Add('RestartBtnCaption    ', RestartBtnCaption);
    Add('CloseBtnCaption      ', CloseBtnCaption);

    // message box = button captions
    Add('OkBtnCaption         ', OkBtnCaption);
    Add('DetailsBtnCaption    ', DetailsBtnCaption);

    // please wait box = strings
    Add('PleaseWaitTitle', PleaseWaitTitle);
    Add('PleaseWaitText', PleaseWaitText);

    // send
    Add('BugTrackerTitle', BugTrackerTitle);
    Add('BugTrackerDescription', BugTrackerDescription);
    Add('MailSubject', MailSubject);
    Add('MailBody', MailBody);
    Add('SendBoxTitle', SendBoxTitle);
    // prepare attachments
    Add('PrepareAttachMsg', PrepareAttachMsg);
    Add('MxLookupMsg', MxLookupMsg);
    // server connect
    Add('ConnectMsg', ConnectMsg);
    // send mail
    Add('SendMailMsg', SendMailMsg);
    Add('FieldsMsg', FieldsMsg);
    Add('SendAttachMsg', SendAttachMsg);
    Add('SendFinalizeMsg', SendFinalizeMsg);
    Add('SendFailureMsg', SendFailureMsg);

  End;

End;

Procedure Translate(mcfg: IMEModuleSettings; l: TStringList; Const cLineBreakSubstitute: String = '\n');
  Function read(Const aKey, aDefault: String): String;
  Var
    v: String;
  Begin
    v := l.Values[Trim(aKey)];
    If v <> '' Then
    Begin
      v := StringReplace(v, cLineBreakSubstitute, slineBreak, [rfReplaceAll, rfIgnorecase]);
      result := v;
    End
    Else
      result := aDefault
  End;

Begin

  With mcfg Do
  Begin
    // CUSTOM STRINGS

    // except box
    TitleBar := Read('TitleBar             ', TitleBar);

    // exception message
    ExceptMsg := Read('ExceptMsg            ', ExceptMsg);

    // frozen message
    FrozenMsg := Read('FrozenMsg            ', FrozenMsg);

    // bit fault message
    BitFaultMsg := Read('BitFaultMsg          ', BitFaultMsg);

    // except box = button captions
    SendBtnCaption := Read('SendBtnCaption       ', SendBtnCaption);
    SaveBtnCaption := Read('SaveBtnCaption       ', SaveBtnCaption);
    PrintBtnCaption := Read('PrintBtnCaption      ', PrintBtnCaption);
    ShowBtnCaption := Read('ShowBtnCaption       ', ShowBtnCaption);
    ContinueBtnCaption := Read('ContinueBtnCaption   ', ContinueBtnCaption);
    RestartBtnCaption := Read('RestartBtnCaption    ', RestartBtnCaption);
    CloseBtnCaption := Read('CloseBtnCaption      ', CloseBtnCaption);

    // message box = button captions
    OkBtnCaption := Read('OkBtnCaption         ', OkBtnCaption);
    DetailsBtnCaption := Read('DetailsBtnCaption    ', DetailsBtnCaption);

    // please wait box = strings
    PleaseWaitTitle := Read('PleaseWaitTitle', PleaseWaitTitle);
    PleaseWaitText := Read('PleaseWaitText', PleaseWaitText);

    // send
    BugTrackerTitle := Read('BugTrackerTitle', BugTrackerTitle);
    BugTrackerDescription := Read('BugTrackerDescription', BugTrackerDescription);
    MailSubject := Read('MailSubject', MailSubject);
    MailBody := Read('MailBody', MailBody);
    SendBoxTitle := Read('SendBoxTitle', SendBoxTitle);

    // prepare attachments
    PrepareAttachMsg := Read('PrepareAttachMsg', PrepareAttachMsg);
    MxLookupMsg := Read('MxLookupMsg', MxLookupMsg);

    // server connect
    ConnectMsg := Read('ConnectMsg', ConnectMsg);

    // send mail
    SendMailMsg := Read('SendMailMsg', SendMailMsg);
    FieldsMsg := Read('FieldsMsg', FieldsMsg);
    SendAttachMsg := Read('SendAttachMsg', SendAttachMsg);
    SendFinalizeMsg := Read('SendFinalizeMsg', SendFinalizeMsg);
    SendFailureMsg := Read('SendFailureMsg', SendFailureMsg);

  End;
End;


End.
