Unit MaxLogic.Logger;

{ .$DEFINE DISABLE_PRLOGGER }
{
  Version: 1.1
  History
  2023-03-24: add possibility to disable json output
  add madStackTrace and auto CallStack to errors of type ERROR
  added MaxLogFileAgeInDays and deleting old log files
}
(*
  JSON has become the de facto standard format for capturing and storing log data - and for good reason. Structured logging with JSON reduces the cost and complexity of extracting valuable insights from log data.30 lis 2021
  this unit is based on the LoggerPro but it stores the data as json entries
  That allows us to include many properties into the log and still keep it as a simple one-line-per-log-entry file that can be interpreted both by humans and computers
  The log will have the following format depending on the format type:
  ofkJson:
  '[%{DATETIME}] [%{LEVEL}] %{JsonMESSAGE}';
  ofkPlainText:
  '[%{DATETIME}] [%{LEVEL}] [...other tags...] %{Message}
  ofkPlainOnNextLine:
  '[%{DATETIME}] [%{LEVEL}] [...other tags...]
  %{Message}

  ATTENTION:
  in a DLL you have to manually free the TMaxLog object and
  call LoggerPro.GlobalLogger.ReleaseGlobalLogger; before the process terminates.
*)
Interface

Uses
  {$IFDEF MSWINDOWS}
  winapi.windows,
  {$ENDIF}
  system.sysUtils, system.classes, system.Diagnostics,
  MaxLogic.FastList, syncObjs,
  LoggerPro, // LoggerPro core
  LoggerPro.FileAppender, // File appender
  LoggerPro.OutputDebugStringAppender; // OutputDebugString appender

Type
  // forward declarations
  TMaxLog = Class;
  TLogEntry = Class;
  TDoLogOnRelease = Class;
  TLogType = LoggerPro.TLogType;
  TOutputFormatKind = (ofkJson, ofkPlainText, ofkPlainOnNextLine);

  { this class stores the data for a log entry
    some properties are mandatory, like the ThreadId }
  iLogEntry = Interface
    ['{AAB78F7A-4526-4BAF-B1B7-C73CB23AC63D}']
    // will write a tag "MSG" with that value
    Function msg(Const aMsg: String): iLogEntry;
    // ATTENTION: aTagName must be a simple ascii text, something that will not need any encoding to use as a json key name
    // the value can be any text, it will be later encoded to be a nice json value string
    // this method returns self, so it can be chained to use in fluid interfaces
    Function put(Const aTagName, aTagValue: String): iLogEntry; Overload;
    Function put(Const aTagName: String; aTagValue: Integer): iLogEntry; Overload;
    // stores the date as ISO8601
    Function put(Const aTagName: String; Const aTagValue: TDateTime): iLogEntry; Overload;
    Function put(Const aTagName: String; Const aTagValue: Double): iLogEntry; Overload;
    { often we want to know where our log entry is coming from,
      in VCL applications this is often a TComponent, this class helps to write the Component name, class and owner (and its owner) to the log
      NOTE: this stores the component under the "COMP" key }
    Function putComponent(aComponent: TComponent): iLogEntry;
    Procedure SetLogType(Const Value: TLogType);
    Function GetLogType: TLogType;
    Property LogType: TLogType Read GetLogType Write SetLogType;
    // properly combined and encoded text that will go to the log file
    Function Text(aKind: TOutputFormatKind): String;
    {$IFDEF MadExcept}
    // if madExcept is used and the logType is error then this will hold the call stack
    // you can also disable this using iMaxLog.AutoStackTraceForErrors:= false
    procedure SetCallStack(const Value: String);
    function GetCallStack: String;
    property CallStack: String read GetCallStack write SetCallStack;
    {$ENDIF}
  End;

  TLogEntry = Class(TInterfacedObject, iLogEntry)
  Private
    // tags are the message parts that will form the json object
    fTags: MaxLogic.FastList.TSortedList<String, String>;
    fThreadId: String;
    fLogType: TLogType;
    {$IFDEF MadExcept}
    FCallStack: String;
    {$ENDIF}
    Procedure Append(Var aText: String; Const aTagName, aTagValue: String);
    Procedure CaptureThreadId;
    Procedure SetLogType(Const Value: TLogType);
    Function GetLogType: TLogType;
    {$IFDEF MadExcept}
    procedure SetCallStack(const Value: String);
    function GetCallStack: String;
    {$ENDIF}
  Public
    Constructor Create;
    Destructor Destroy; Override;
    // will write a tag "MSG" with that value
    Function msg(Const aMsg: String): iLogEntry;
    // ATTENTION: aTagName must be a simple ascii text, something that will not need any encoding to use as a json key name
    // the value can be any text, it will be later encoded to be a nice json value string
    // this method returns self, so it can be chained to use in fluid interfaces
    Function put(Const aTagName, aTagValue: String): iLogEntry; Overload;
    Function put(Const aTagName: String; aTagValue: Integer): iLogEntry; Overload;
    Function put(Const aTagName: String; Const aTagValue: Int64): iLogEntry; Overload;
    // stores the date as ISO8601
    Function put(Const aTagName: String; Const aTagValue: TDateTime): iLogEntry; Overload;
    Function put(Const aTagName: String; Const aTagValue: Double): iLogEntry; Overload;
    { often we want to know where our log entry is coming from,
      in VCL applications this is often a TComponent, this class helps to write the Component name, class and owner (and its owner) to the log
      NOTE: this stores the component under the "COMP" key }
    Function putComponent(aComponent: TComponent): iLogEntry;
    // properly combined and encoded text that will go to the log file
    // ATTENTION: this is also the time the "MsSinceAppStart " timestamp will be written to the text
    Function Text(aKind: TOutputFormatKind): String;
    // default is etInfo
    // NOTE this is secondary to the maxLog.log() method
    Property LogType: TLogType Read GetLogType Write SetLogType;

    {$IFDEF MadExcept}
    // if madExcept is used and the logType is error then this will hold the call stack
    // you can also disable this using iMaxLog.AutoStackTraceForErrors:= false
    property CallStack: String read GetCallStack write SetCallStack;
    {$ENDIF}
  End;

  iMaxLog = Interface
    ['{28201268-1C2A-4102-9BFB-1E81BB3B1826}']
    Procedure SetDefaultTag(Const Value: String);
    Procedure SetLogWriter(Const Value: ILogWriter);
    Function GetLogWriter: ILogWriter;
    Function GetDefaultTag: String;
    // creates a new logEntry
    // NOTE: that doesnt write it to anywhere
    Function LogENtry(Const aMsg: String = ''): iLogEntry;
    // will create two entries into the log
    // the first with the EnterProc tag and the second, as soon as the interface goes out of scope, with the ExitProc tag and a additional Duration (in ms) tag
    // ATTENTION note that delphi 10.4 changed the life span of interfaced objects... they go out of scope as soon as the begin end in which it was declared goes out of scope!
    Function LogEnterExitProc(Const aProcName: String; aLogType: TLogType = TLogType.Debug): iLogEntry;
    Procedure add(Const aMsg: String; aLogType: TLogType = TLogType.Info); Overload;
    Procedure add(aMsg: iLogEntry); Overload;
    // as above but override the LogType
    Procedure add(aMsg: iLogEntry; aLogType: TLogType); Overload;
    // will accept the message as is
    Procedure addRaw(Const aJsonLogEntryText: String; aLogType: TLogType);
    // simplify logging add
    Procedure Info(Const aMsg: String); Overload;
    Procedure Info(aMsg: iLogEntry); Overload;
    Procedure Warn(Const aMsg: String); Overload;
    Procedure Warn(aMsg: iLogEntry); Overload;
    Procedure Error(Const aMsg: String); Overload;
    Procedure Error(aMsg: iLogEntry); Overload;
    Procedure Debug(Const aMsg: String); Overload;
    Procedure Debug(aMsg: iLogEntry); Overload;
    // the the loggerPro saves a sufix to the file names, this is this Tag here
    // Default is Main
    Property DefaultTag: String Read GetDefaultTag Write SetDefaultTag;
    // raw access to the underlying logger
    // if you do not like the predefined one, you always can reassign this value here
    Property LogWriter: ILogWriter Read GetLogWriter Write SetLogWriter;
    function GetOutputFormatKind: TOutputFormatKind;
    procedure SetOutputFormatKind(const Value: TOutputFormatKind);
    property OutputFormatKind: TOutputFormatKind read GetOutputFormatKind write SetOutputFormatKind;
    procedure SetAutoStackTraceForErrors(const Value: Boolean);
    function GetAutoStackTraceForErrors: Boolean;
    property AutoStackTraceForErrors: Boolean read GetAutoStackTraceForErrors write SetAutoStackTraceForErrors;
    function GetLogDir: String;
    property LogDir: String read GetLogDir;
    // set this to auto-delete files older then n days
    // NOTE: the log file usually has a pid in its name, that will be replaced by a * wildcard while deleting
    procedure SetMaxLogFileAgeInDays(const Value: Integer);
    function GetMaxLogFileAgeInDays: Integer;
    property MaxLogFileAgeInDays: Integer read GetMaxLogFileAgeInDays write SetMaxLogFileAgeInDays;
  End;

  TMaxLog = Class(TInterfacedObject, iMaxLog)
  Private
    FDefaultTag: String;
    FLogWriter: ILogWriter;
    fLogDir: String;
    fCs: TCriticalSection;
    fOutputFormatKind: TOutputFormatKind;
    FAutoStackTraceForErrors: Boolean;
    FMaxLogFileAgeInDays: Integer;
    fLastCheckOldLogFilesTimeStamp: TDateTime;
    fFileMaskForDeletingOldLogFiles: String;
    Procedure SetDefaultTag(Const Value: String);
    Procedure SetLogWriter(Const Value: ILogWriter);
    Function GetLogWriter: ILogWriter;
    Function GetDefaultTag: String;
    function GetOutputFormatKind: TOutputFormatKind;
    procedure SetOutputFormatKind(const Value: TOutputFormatKind);
    procedure SetAutoStackTraceForErrors(const Value: Boolean);
    function GetAutoStackTraceForErrors: Boolean;
    function GetLogDir: String;
    procedure SetMaxLogFileAgeInDays(const Value: Integer);
    function GetMaxLogFileAgeInDays: Integer;
    procedure CheckDeleteOldLogFiles;
  Public
    // the final file name will be
    // aLogDir + Format(aLogFileNameFormat, [lModuleName, aFileNumber, aTag]);
    // note, if LogDir is empty, the the default will be {GetInstallDir}\Logs
    Constructor Create(aMaxFileCount: Integer = 10;
      aMaxFileSizeInKb: Integer = 50 * 1024;
      Const aLogDir: String = '';
      Const aLogFileNameFormat: String = '%s.%2.2d.%s.log');
    Destructor Destroy; Override;
    // creates a new logEntry
    // NOTE: that doesnt write it to anywhere
    Function LogENtry(Const aMsg: String = ''): iLogEntry;
    // will create two entries into the log
    // the first with the EnterProc tag and the second, as soon as the interface goes out of scope, with the ExitProc tag and a additional Duration (in ms) tag
    // ATTENTION note that delphi 10.4 changed the life span of interfaced objects... they go out of scope as soon as the begin end in which it was declared goes out of scope!
    Function LogEnterExitProc(Const aProcName: String; aLogType: TLogType = TLogType.Debug): iLogEntry;
    Procedure add(Const aMsg: String; aLogType: TLogType = TLogType.Info); Overload;
    Procedure add(aMsg: iLogEntry); Overload;
    // as above but override the LogType
    Procedure add(aMsg: iLogEntry; aLogType: TLogType); Overload;
    // will accept the message as is
    Procedure addRaw(Const aJsonLogEntryText: String; aLogType: TLogType);
    // simplify logging add
    Procedure Info(Const aMsg: String); Overload;
    Procedure Info(aMsg: iLogEntry); Overload;
    Procedure Warn(Const aMsg: String); Overload;
    Procedure Warn(aMsg: iLogEntry); Overload;
    Procedure Error(Const aMsg: String); Overload;
    Procedure Error(aMsg: iLogEntry); Overload;
    Procedure Debug(Const aMsg: String); Overload;
    Procedure Debug(aMsg: iLogEntry); Overload;
    // the the loggerPro saves a sufix to the file names, this is this Tag here
    // Default is Main
    Property DefaultTag: String Read GetDefaultTag Write SetDefaultTag;
    property OutputFormatKind: TOutputFormatKind read GetOutputFormatKind write SetOutputFormatKind;
    property AutoStackTraceForErrors: Boolean read GetAutoStackTraceForErrors write SetAutoStackTraceForErrors;
    // raw access to the underlying logger
    // if you do not like the predefined one, you always can reassign this value here
    Property LogWriter: ILogWriter Read GetLogWriter Write SetLogWriter;
    property LogDir: String read GetLogDir;
    // set this to auto-delete files older then n days
    // NOTE: the log file usually has a pid in its name, that will be replaced by a * wildcard while deleting
    property MaxLogFileAgeInDays: Integer read GetMaxLogFileAgeInDays write SetMaxLogFileAgeInDays;
  End;

  TDoLogOnRelease = Class(TLogEntry)
  Private
    fStopWatch: TStopWatch;
    fMaxLog: iMaxLog;
  Public
    Constructor Create(aMaxLog: iMaxLog);
    Destructor Destroy; Override;
  End;

Var
  glMainVclThreadId: TThreadID;
  glPid: Int64;
  glPidAsHex: String = '(undefined)'; // this will spare us the conversion each time we have a logEntry to create
  GlobalAppStartTimer: TStopWatch; // will be started in the initialization section
  // default global maxLog instance, will be auto created on first start up
  // but you should call SetGlobalMaxLog yourself
Function maxLog: iMaxLog;
Procedure SetGlobalMaxLog(aInstance: iMaxLog);
// test if the global instance is already created
Function GlobalMaxLogInstanceCreated: Boolean;
// will free the global instance
Procedure ShutDownMaxLog;
// ToDo: maybe look for a faster alternative
Function JsonEncode(Const RawText: String): String; inline;

Implementation

Uses
  {$IFDEF madExcept}
  madStackTrace,
  {$ENDIF}
  dateUtils, strUtils, system.Json,
  idGlobal, ioUtils,
  MaxLogic.ioUtils,
  system.Threading, system.Masks;

const
  cTagMsg = 'MSG';
  {$IFDEF MadExcept}
  CCallStackTag = 'CallStack';
  {$ENDIF}

Var
  glFormatSettings: TFormatSettings;
  glCs: TCriticalSection;
  glDefaultMaxLog: iMaxLog;

Function GlobalMaxLogInstanceCreated: Boolean;
begin
  Result := assigned(glDefaultMaxLog);
end;

Function maxLog: iMaxLog;
Begin
  If glDefaultMaxLog <> Nil Then
    Result := glDefaultMaxLog
  Else
  Begin
    // just in case the initialiation section of this unit was not yet called...
    If Not assigned(glCs) Then
    Begin
      Var
      lCs := TCriticalSection.Create;
      If TInterlocked.CompareExchange(pointer(glCs), pointer(lCs), Nil) <> Nil Then
        lCs.Free;
    End;
    glCs.Enter;
    Try
      If glDefaultMaxLog <> Nil Then
        Result := glDefaultMaxLog
      Else
      Begin
        glDefaultMaxLog := TMaxLog.Create;
        Result := glDefaultMaxLog;
      End;
    Finally
      glCs.Leave;
    End;
  End;
End;

Procedure SetGlobalMaxLog(aInstance: iMaxLog);
Var
  lCs: TCriticalSection;
Begin
  // just in case the initialiation section of this unit was not yet called...
  If Not assigned(glCs) Then
  Begin
    lCs := TCriticalSection.Create;
    If TInterlocked.CompareExchange(pointer(glCs), pointer(lCs), Nil) <> Nil Then
      lCs.Free;
  End;
  glCs.Enter;
  Try
    glDefaultMaxLog := aInstance;
  Finally
    glCs.Leave;
  End;
End;

Procedure ShutDownMaxLog;
Begin
  glDefaultMaxLog := NIL;
End;

Function JsonEncode(Const RawText: String): String;
Begin
  With TJsonString.Create(RawText) Do
  Begin
    Result := ToJson;
    Free;
  End;
End;

{ TLogEntry }
Procedure TLogEntry.Append(Var aText: String; Const aTagName, aTagValue: String);
Var
  s: String;
Begin
  If aTagValue = '' Then
    Exit;
  s := '"' + aTagName + '":' + JsonEncode(aTagValue);
  If aText = '' Then
    aText := s
  Else
    aText := aText + ', ' + s;
End;

Procedure TLogEntry.CaptureThreadId;
Var
  tid: TThreadID;
Begin
  If fThreadId = '' Then
  Begin
    {$IFDEF MSWINDOWS}
    tid := GetCurrentThreadId;
    {$ELSE}
    tid := TThread.CurrentThread.ThreadID;
    {$ENDIF}
    If tid = glMainVclThreadId Then
      fThreadId := 'Main'
    Else
    Begin
      // ToDo: maybe we can get a list of thread names from somewhere?
      fThreadId := IntToHex(tid, 1);
    End;
  End;
End;

Constructor TLogEntry.Create;
Begin
  Inherited;
  fTags := TSortedList<String, String>.Create;
  fTags.Capacity := 10;
  CaptureThreadId;
  self.fLogType := TLogType.Info;
End;

Destructor TLogEntry.Destroy;
Begin
  Inherited;
End;

{$IFDEF MadExcept}

function TLogEntry.GetCallStack: String;
begin
  Result := FCallStack;
end;
{$ENDIF}


Function TLogEntry.GetLogType: TLogType;
Begin
  Result := fLogType;
End;

Function TLogEntry.msg(Const aMsg: String): iLogEntry;
Begin
  Result := put(cTagMsg, aMsg);
End;

Function TLogEntry.put(Const aTagName, aTagValue: String): iLogEntry;
Begin
  fTags.addOrSet(aTagValue, aTagName);
  Result := self;
End;

Function TLogEntry.put(Const aTagName: String; aTagValue: Integer): iLogEntry;
Begin
  Result := put(aTagName, aTagValue.ToString);
End;

Function TLogEntry.put(Const aTagName: String; Const aTagValue: Double): iLogEntry;
Var
  s: String;
Begin
  s := FloatToStr(aTagValue, glFormatSettings);
  Result := put(aTagName, s);
End;

Function TLogEntry.put(Const aTagName: String; Const aTagValue: TDateTime): iLogEntry;
Var
  s: String;
Begin
  s := dateUtils.DateToISO8601(aTagValue);
  Result := put(aTagName, s);
End;

Function TLogEntry.put(Const aTagName: String; Const aTagValue: Int64): iLogEntry;
Begin
  Result := put(aTagName, aTagValue.ToString);
End;

Function TLogEntry.putComponent(aComponent: TComponent): iLogEntry;
Var
  s: String;
  Procedure prepend(Const aValue: String);
  Begin
    If s = '' Then
      s := aValue
    Else
      s := s + '.' + aValue;
  End;

Begin
  Repeat
    If (aComponent = Nil) And (s = '') Then
      prepend('<NIL>')
    Else
    Begin
      prepend(
        ifThen(aComponent.Name <> '', aComponent.Name, '$' + IntToHex(NativeInt(aComponent), 1)) +
          '(' + aComponent.ClassName + ')');
      aComponent := aComponent.Owner;
    End;
  Until aComponent = Nil;
  Result := put('COMP', s);
End;

{$IFDEF MadExcept}

procedure TLogEntry.SetCallStack(const Value: String);
begin
  FCallStack := Value;
end;
{$ENDIF}


Procedure TLogEntry.SetLogType(Const Value: TLogType);
Begin
  fLogType := Value;
End;

Function TLogEntry.Text(aKind: TOutputFormatKind): String;
Var
  x, i: Integer;
  pid: Int64;
Begin
  Result := '';
  put('TID', fThreadId);
  put('PID', glPidAsHex);
  pid := idGlobal.CurrentProcessId;
  If pid <> glPid Then
    put('fork-PID', IntToHex(pid, 1));
  put('MsSinceAppStart', GlobalAppStartTimer.ElapsedMilliseconds);
  if aKind = ofkJson then
  begin
    For x := 0 To fTags.count - 1 Do
      Append(Result, fTags.items[x].iid, fTags.items[x].data);

    {$IFDEF MadExcept}
    // in case of errors
    // add CallStack always as the last item
    if FCallStack <> '' then
      Append(Result, CCallStackTag, FCallStack);
    {$ENDIF}
    Result := '{' + Result + '}'
  end else begin
    For x := 0 To fTags.count - 1 Do
      if not(cTagMsg = fTags.items[x].iid) then
        Result := Result + '[' +
          fTags.items[x].iid + ': ' + fTags.items[x].data + '] ';
    if fTags.find(cTagMsg, i) then
    begin
      if aKind = ofkPlainOnNextLine then
        Result := Trim(Result) + sLineBreak + fTags[i]
      else
        Result := Result + fTags[i];
    end;

    {$IFDEF MadExcept}
    if CallStack <> '' then
      Result := Result + sLineBreak +
        CCallStackTag + sLineBreak +
        CallStack;
    {$ENDIF}
  end;
End;

{ TMaxLog }
Procedure TMaxLog.Error(Const aMsg: String);
Begin
  add(aMsg, TLogType.Error);
End;

Procedure TMaxLog.Info(Const aMsg: String);
Begin
  add(aMsg, TLogType.Info);
End;

Function TMaxLog.LogENtry(Const aMsg: String): iLogEntry;
Begin
  Result := TLogEntry.Create;
  If aMsg <> '' Then
    Result.msg(aMsg);
End;

procedure TMaxLog.SetAutoStackTraceForErrors(const Value: Boolean);
begin
  FAutoStackTraceForErrors := Value;
end;

Procedure TMaxLog.SetDefaultTag(Const Value: String);
Begin
  FDefaultTag := Value;
End;

Procedure TMaxLog.SetLogWriter(Const Value: ILogWriter);
Begin
  FLogWriter := Value;
End;

procedure TMaxLog.SetMaxLogFileAgeInDays(const Value: Integer);
begin
  FMaxLogFileAgeInDays := Value;
end;

procedure TMaxLog.SetOutputFormatKind(const Value: TOutputFormatKind);
begin
  fOutputFormatKind := Value;
end;

Procedure TMaxLog.add(aMsg: iLogEntry; aLogType: TLogType);
Begin
  if FAutoStackTraceForErrors and
    (aLogType = TLogType.Error) then
  begin
    {$IFDEF MadExcept}
    aMsg.CallStack :=
      StackTrace(
      false, // ide ugly
      True, // showRelativeAddrs : boolean        = false;
      True // showRelativeLines : boolean        = false;
      );
    {$ENDIF}
  end;
  addRaw(aMsg.Text(OutputFormatKind), aLogType);
End;

Procedure TMaxLog.CheckDeleteOldLogFiles;
var
  lPattern, lLogDir: String;
  dt: TDateTime;
  lMaxAgeInDays: Integer;
begin
  if FMaxLogFileAgeInDays <= 0 then
    Exit;
  if dateUtils.HoursBetween(now, fLastCheckOldLogFilesTimeStamp) < 24 then
    Exit;
  dt := now;
  TInterlocked.Exchange(Double(fLastCheckOldLogFilesTimeStamp), Double(dt));
  // capture local variables
  lLogDir := self.fLogDir;
  lPattern := fFileMaskForDeletingOldLogFiles;
  lMaxAgeInDays := FMaxLogFileAgeInDays;
  // run async
  TTask.Create(
      procedure
    var
      fn: String;
      fileDt: TDateTime;
    begin
      Exit;
      for fn in TDirectory.GetFiles(lLogDir, lPattern) do
      begin
        try
          fileDt := TFile.GetLastWriteTime(fn);
          if dateUtils.HoursBetween(now, fileDt) > 24 * lMaxAgeInDays then
            TFile.Delete(fn);
        except
          // do nothing
        end;
      end;
    end).Start;
end;

Procedure TMaxLog.addRaw(Const aJsonLogEntryText: String; aLogType: TLogType);
Begin
  CheckDeleteOldLogFiles;
  {$IFNDEF DISABLE_PRLOGGER }
  If LogWriter <> Nil Then
    self.LogWriter.Log(aLogType, aJsonLogEntryText, FDefaultTag);
  {$ELSE}
  // mostly for debuging a super simple log writing
  var f: textfile;
  var
  fn := self.fLogDir + changeFileExt(extractFilename(paramStr(0)), '-pid-' + IntToHex(glPid, 1) + '.log');
  system.assign(f, fn);
  try
    if not fileExists(fn) then
      rewrite(f)
    else
      Append(f);
    system.WriteLn(f, aJsonLogEntryText);
    system.Flush(f);
  finally
    CloseFile(f);
  end;
  {$ENDIF}
End;

Constructor TMaxLog.Create;
Var
  fa: TLoggerProFileAppender;
  pid: Int64;
Begin
  Inherited Create;
  fOutputFormatKind := ofkJson;
  FAutoStackTraceForErrors := True;
  fCs := TCriticalSection.Create;
  fLogDir := aLogDir;
  If fLogDir = '' Then
    fLogDir := ExtractFilePath(MaxLogic.ioUtils.GetCurrentDLLName) + 'logs'
  Else
    fLogDir := ExpandFileName(fLogDir);
  fLogDir := IncludeTrailingPathDelimiter(fLogDir);
  pid := idGlobal.CurrentProcessId;
  fFileMaskForDeletingOldLogFiles :=
    changeFileExt(aLogFileNameFormat, '*.log');
  fFileMaskForDeletingOldLogFiles := StringReplace(fFileMaskForDeletingOldLogFiles, '%s', '*', [rfReplaceAll, rfIgnoreCase]);
  fFileMaskForDeletingOldLogFiles := StringReplace(fFileMaskForDeletingOldLogFiles, '%d', '*', [rfReplaceAll, rfIgnoreCase]);
  fFileMaskForDeletingOldLogFiles := StringReplace(fFileMaskForDeletingOldLogFiles, '%2.2d', '*', [rfReplaceAll, rfIgnoreCase]);
  fFileMaskForDeletingOldLogFiles := StringReplace(fFileMaskForDeletingOldLogFiles, '*.*', '*', [rfReplaceAll, rfIgnoreCase]);
  fFileMaskForDeletingOldLogFiles := StringReplace(fFileMaskForDeletingOldLogFiles, '**', '*', [rfReplaceAll, rfIgnoreCase]);
  {$IFNDEF DISABLE_PRLOGGER}
  fa := TLoggerProFileAppender.Create(aMaxFileCount, aMaxFileSizeInKb,
    fLogDir, [],
  // ChangeFileExt(aLogFileNameFormat, '-pid-' + IntToHex(pid, 1) + '.log'),
  aLogFileNameFormat,
    '[%0:s] [%2:s] %3:s',
    TEncoding.UTF8);
  FLogWriter := BuildLogWriter([
    fa
    // TLoggerProOutputDebugStringAppender.Create
    ]);
  {$ENDIF}
  FDefaultTag := 'Main-pid-' + IntToHex(pid, 1);
End;

Procedure TMaxLog.add(aMsg: iLogEntry);
Begin
  add(aMsg, aMsg.LogType);
End;

Procedure TMaxLog.add(Const aMsg: String; aLogType: TLogType);
Begin
  add(LogENtry(aMsg), aLogType);
End;

Procedure TMaxLog.Debug(Const aMsg: String);
Begin
  add(aMsg, TLogType.Debug);
End;

Procedure TMaxLog.Warn(Const aMsg: String);
Begin
  add(aMsg, TLogType.Warning);
End;

Function TMaxLog.LogEnterExitProc(Const aProcName: String;
aLogType: TLogType = TLogType.Debug): iLogEntry;
Var
  obj: TDoLogOnRelease;
  msg: TLogEntry;
Begin
  msg := TLogEntry.Create;
  msg.put('EnterProc', aProcName);
  self.add(msg, aLogType);
  obj := TDoLogOnRelease.Create(self);
  obj.put('ExitProc', aProcName);
  obj.LogType := aLogType;
  Result := obj;
End;

Procedure TMaxLog.Debug(aMsg: iLogEntry);
Begin
  add(aMsg, TLogType.Debug);
End;

Destructor TMaxLog.Destroy;
Var
  l: ILogWriter;
Begin
  If FLogWriter <> Nil Then
  Begin
    // While FLogWriter.AppendersCount <> 0 Do
    // FLogWriter.DelAppender(FLogWriter.Appenders[0]);
    FLogWriter := Nil;
  End;
  fCs.Free;
  Inherited;
End;

Procedure TMaxLog.Error(aMsg: iLogEntry);
Begin
  add(aMsg, TLogType.Error);
End;

function TMaxLog.GetAutoStackTraceForErrors: Boolean;
begin
  Result := FAutoStackTraceForErrors;
end;

Function TMaxLog.GetDefaultTag: String;
Begin
  Result := FDefaultTag;
End;

function TMaxLog.GetLogDir: String;
begin
  Result := fLogDir;
end;

Function TMaxLog.GetLogWriter: ILogWriter;
Begin
  Result := FLogWriter;
End;

function TMaxLog.GetMaxLogFileAgeInDays: Integer;
begin
  Result := FMaxLogFileAgeInDays;
end;

function TMaxLog.GetOutputFormatKind: TOutputFormatKind;
begin
  Result := fOutputFormatKind;
end;

Procedure TMaxLog.Info(aMsg: iLogEntry);
Begin
  add(aMsg, TLogType.Info);
End;

Procedure TMaxLog.Warn(aMsg: iLogEntry);
Begin
  add(aMsg, TLogType.Warning);
End;

{ TDoLogOnRelease }
Constructor TDoLogOnRelease.Create;
Begin
  Inherited Create;
  fStopWatch := TStopWatch.StartNew;
  fMaxLog := aMaxLog;
End;

Destructor TDoLogOnRelease.Destroy;
Begin
  put('Duration', fStopWatch.ElapsedMilliseconds.ToString);
  If fMaxLog <> Nil Then
    fMaxLog.addRaw(self.Text(fMaxLog.OutputFormatKind), self.LogType);
  Inherited;
End;

Initialization

GlobalAppStartTimer := TStopWatch.StartNew;
glFormatSettings := TFormatSettings.Create;
glFormatSettings.decimalSeparator := '.';
{$IFDEF MSWINDOWS}
glMainVclThreadId := GetCurrentThreadId;
{$ELSE}
glMainVclThreadId := TThread.CurrentThread.ThreadID;
{$ENDIF}
glPid := idGlobal.CurrentProcessId;
glPidAsHex := IntToHex(glPid, 1);
If Not assigned(glCs) Then
Begin
  Var
  lCs := TCriticalSection.Create;
  If TInterlocked.CompareExchange(pointer(glCs), pointer(lCs), Nil) <> Nil Then
    lCs.Free;
End;

Finalization

ShutDownMaxLog;
If assigned(glCs) Then
Begin
  glCs.Free;
  glCs := Nil;
End;

End.
