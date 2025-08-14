Unit MaxLogic.Logger;

//{$DEFINE DISABLE_PRLOGGER} // Keep this commented unless you specifically want to disable LoggerPro
{
  Version: 1.2
  History
  2024-07-28: Added CloneWithTag for creating logger instances with different tags sharing the core writer.
                      Fixed bug in CheckDeleteOldLogFiles task.
                      Applied coding style and naming conventions.
                      Removed unused fCs field.
                      Added XML documentation stubs.
                      Added missing Int64 overload for put method.
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
  system.sysUtils, system.classes, system.Diagnostics, system.Threading,
  syncObjs, system.Masks, system.Json, system.DateUtils, system.StrUtils,
  {$IFNDEF DISABLE_PRLOGGER }
  LoggerPro, LoggerPro.FileAppender, LoggerPro.OutputDebugStringAppender,
  LoggerPro.GlobalLogger, LoggerPro.Proxy, LoggerPro.Renderers,
  {$ENDIF}
  MaxLogic.FastList, idGlobal, MaxLogic.ioUtils;

Type
  // Forward declarations
  TMaxLog = Class;
  TLogEntry = Class;
  TDoLogOnRelease = Class;

  {$IFNDEF DISABLE_PRLOGGER }
  TLogType = LoggerPro.TLogType;
  {$ELSE}
  TLogType = (Debug = 0, Info, Warning, Error);
  {$ENDIF}

  TOutputFormatKind = (ofkJson, ofkPlainText, ofkPlainOnNextLine);

  /// <summary>
  /// Interface for a single log entry, allowing fluent addition of tagged data.
  /// </summary>
  iLogEntry = Interface
    ['{AAB78F7A-4526-4BAF-B1B7-C73CB23AC63D}']
    /// <summary>Sets the main message tag ("MSG").</summary>
    Function msg(Const aMsg: String): iLogEntry;
    /// <summary>Adds a string tag. Tag names should be simple ASCII.</summary>
    Function put(Const aTagName, aTagValue: String): iLogEntry; Overload;
    /// <summary>Adds an integer tag.</summary>
    Function put(Const aTagName: String; aTagValue: Integer): iLogEntry; Overload;
    /// <summary>Adds an Int64 tag.</summary>
    Function put(Const aTagName: String; Const aTagValue: Int64): iLogEntry; Overload;
    /// <summary>Adds a TDateTime tag (formatted as ISO8601).</summary>
    Function put(Const aTagName: String; Const aTagValue: TDateTime): iLogEntry; Overload;
    /// <summary>Adds a floating-point tag.</summary>
    Function put(Const aTagName: String; Const aTagValue: Double): iLogEntry; Overload;
    /// <summary>Adds component hierarchy information under the "COMP" tag.</summary>
    Function putComponent(aComponent: TComponent): iLogEntry;
    /// <summary>Sets the severity level for this specific log entry.</summary>
    Procedure SetLogType(Const Value: TLogType);
    /// <summary>Gets the severity level for this specific log entry.</summary>
    Function GetLogType: TLogType;
    /// <summary>Gets or sets the severity level for this specific log entry.</summary>
    Property LogType: TLogType Read GetLogType Write SetLogType;
    /// <summary>Formats the log entry into a string based on the specified kind.</summary>
    Function Text(aKind: TOutputFormatKind): String;
    {$IFDEF MadExcept}
    /// <summary>Sets the call stack string (used for errors with MadExcept).</summary>
    procedure SetCallStack(const Value: String);
    /// <summary>Gets the call stack string.</summary>
    function GetCallStack: String;
    /// <summary>Gets or sets the call stack (relevant for errors when MadExcept is enabled).</summary>
    property CallStack: String read GetCallStack write SetCallStack;
    {$ENDIF}
  End;

  /// <summary>
  /// Interface for the main logger functionality.
  /// </summary>
  iMaxLog = Interface
    ['{28201268-1C2A-4102-9BFB-1E81BB3B1826}']

    procedure CheckDeleteOldLogFiles;

    /// <summary>Sets the default tag used when logging messages.</summary>
    Procedure SetDefaultTag(Const Value: String);
    /// <summary>Gets the default tag used when logging messages.</summary>
    Function GetDefaultTag: String;
    {$IFNDEF DISABLE_PRLOGGER }
    /// <summary>Sets the underlying LoggerPro writer instance.</summary>
    Procedure SetLogWriter(Const Value: ILogWriter);
    /// <summary>Gets the underlying LoggerPro writer instance.</summary>
    Function GetLogWriter: ILogWriter;
    {$ENDIF}
    /// <summary>Creates a new, empty log entry object.</summary>
    Function LogENtry(Const aMsg: String = ''): iLogEntry;
    /// <summary>Logs procedure entry and automatically logs exit with duration when the result goes out of scope.</summary>
    Function LogEnterExitProc(Const aProcName: String; aLogType: TLogType = TLogType.Debug): iLogEntry;
    /// <summary>Adds a simple string message with a specified log level.</summary>
    Procedure add(Const aMsg: String; aLogType: TLogType = TLogType.Info); Overload;
    /// <summary>Adds a pre-built log entry using its own log level.</summary>
    Procedure add(aMsg: iLogEntry); Overload;
    /// <summary>Adds a pre-built log entry, overriding its log level.</summary>
    Procedure add(aMsg: iLogEntry; aLogType: TLogType); Overload;
    /// <summary>Adds a raw, pre-formatted log string.</summary>
    Procedure addRaw(Const aJsonLogEntryText: String; aLogType: TLogType);
    // lock/Unlock to protect the LoggerPro writer access
    procedure Lock;
    procedure UnLock;

    /// <summary>Logs an informational message (string).</summary>
    Procedure Info(Const aMsg: String); Overload;
    /// <summary>Logs an informational message (log entry).</summary>
    Procedure Info(aMsg: iLogEntry); Overload;
    /// <summary>Logs a warning message (string).</summary>
    Procedure Warn(Const aMsg: String); Overload;
    /// <summary>Logs a warning message (log entry).</summary>
    Procedure Warn(aMsg: iLogEntry); Overload;
    /// <summary>Logs an error message (string).</summary>
    Procedure Error(Const aMsg: String); Overload;
    // Best source: crash-time stack (must be called inside the same `except` block)
    // lTrace := madExcept.GetCrashStackTrace;
    Procedure Error(Const aMsg, aCallStackTrace: String); overload;
    /// <summary>Logs an error message (log entry).</summary>
    Procedure Error(aMsg: iLogEntry); Overload;
    /// <summary>Logs a debug message (string).</summary>
    Procedure Debug(Const aMsg: String); Overload;
    /// <summary>Logs a debug message (log entry).</summary>
    Procedure Debug(aMsg: iLogEntry); Overload;
    /// <summary>Gets the output format kind (JSON, PlainText, etc.).</summary>
    function GetOutputFormatKind: TOutputFormatKind;
    /// <summary>Sets the output format kind.</summary>
    procedure SetOutputFormatKind(const Value: TOutputFormatKind);
    /// <summary>Gets whether to automatically add stack traces for errors (requires MadExcept).</summary>
    function GetAutoStackTraceForErrors: Boolean;
    /// <summary>Sets whether to automatically add stack traces for errors.</summary>
    procedure SetAutoStackTraceForErrors(const Value: Boolean);
    /// <summary>Gets the directory where log files are stored.</summary>
    function GetLogDir: String;
    /// <summary>Gets the maximum age in days for log files before deletion.</summary>
    function GetMaxLogFileAgeInDays: Integer;
    /// <summary>Sets the maximum age in days for log files before deletion (0 disables).</summary>
    procedure SetMaxLogFileAgeInDays(const Value: Integer);
    /// <summary>Creates a new logger instance that shares the same configuration and underlying writer but uses a different default tag.</summary>
    Function CloneWithTag(Const aNewTag: String): iMaxLog;
    // releases internal loggerPro Writer
    procedure ShutDown;

    /// <summary>Gets or sets the default tag associated with log messages from this instance.</summary>
    Property DefaultTag: String Read GetDefaultTag Write SetDefaultTag;
    {$IFNDEF DISABLE_PRLOGGER }
    /// <summary>Gets or sets the raw underlying LoggerPro writer instance.</summary>
    Property LogWriter: ILogWriter Read GetLogWriter Write SetLogWriter;
    {$ENDIF}
    /// <summary>Gets or sets the format for log output.</summary>
    property OutputFormatKind: TOutputFormatKind read GetOutputFormatKind write SetOutputFormatKind;
    /// <summary>Gets or sets whether stack traces are automatically included for error logs (requires MadExcept).</summary>
    property AutoStackTraceForErrors: Boolean read GetAutoStackTraceForErrors write SetAutoStackTraceForErrors;
    /// <summary>Gets the configured log directory.</summary>
    property LogDir: String read GetLogDir;
    /// <summary>Gets or sets the maximum age (in days) of log files to keep.</summary>
    property MaxLogFileAgeInDays: Integer read GetMaxLogFileAgeInDays write SetMaxLogFileAgeInDays;
  End;

  /// <summary>
  /// Concrete implementation of iLogEntry.
  /// </summary>
  TLogEntry = Class(TInterfacedObject, iLogEntry)
  Private
    // Tags are the message parts that will form the json object
    fTags: MaxLogic.FastList.TSortedList<String, String>;
    fThreadId: String;
    fLogType: TLogType;
    {$IFDEF MadExcept}
    fCallStack: String;
    {$ENDIF}
    Procedure Append(Var aText: String; Const aTagName, aTagValue: String);
    Procedure CaptureThreadId;
    // iLogEntry implementations
    Procedure SetLogType(Const Value: TLogType);
    Function GetLogType: TLogType;
    {$IFDEF MadExcept}
    procedure SetCallStack(const Value: String);
    function GetCallStack: String;
    {$ENDIF}
  Public
    constructor Create;
    destructor Destroy; override;
    // iLogEntry implementations
    Function msg(Const aMsg: String): iLogEntry;
    Function put(Const aTagName, aTagValue: String): iLogEntry; Overload;
    Function put(Const aTagName: String; aTagValue: Integer): iLogEntry; Overload;
    Function put(Const aTagName: String; Const aTagValue: Int64): iLogEntry; Overload;
    Function put(Const aTagName: String; Const aTagValue: TDateTime): iLogEntry; Overload;
    Function put(Const aTagName: String; Const aTagValue: Double): iLogEntry; Overload;
    Function putComponent(aComponent: TComponent): iLogEntry;
    Function Text(aKind: TOutputFormatKind): String;
  End;

  /// <summary>
  /// Concrete implementation of iMaxLog. Manages log settings and interacts with LoggerPro or basic file writing.
  /// </summary>
  TMaxLog = Class(TInterfacedObject, iMaxLog)
  Private
    fDefaultTag: String;
    {$IFNDEF DISABLE_PRLOGGER }
    fLogWriter: ILogWriter;
    {$ENDIF}
    fLogDir: String;
    fOutputFormatKind: TOutputFormatKind;
    fAutoStackTraceForErrors: Boolean;
    fMaxLogFileAgeInDays: Integer;
    fLastCheckOldLogFilesTimeStamp: TDateTime;
    fFileMaskForDeletingOldLogFiles: String;
    fParentInstance: iMaxLog;
    // Private constructor for cloning
    constructor CreateClone(aSource: TMaxLog; const aNewTag: string);
    // iMaxLog private implementations
    Procedure SetDefaultTag(Const Value: String);
    {$IFNDEF DISABLE_PRLOGGER }
    Procedure SetLogWriter(Const Value: ILogWriter);
    Function GetLogWriter: ILogWriter;
    {$ENDIF}
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
    // The final file name will be: aLogDir + Format(aLogFileNameFormat, [lModuleName, aFileNumber, aTag, PID]);
    // Note: If aLogDir is empty, the default will be {AppDir}\Logs
    constructor Create(aMaxFileCount: Integer = 10;
      aMaxFileSizeInKb: Integer = 50 * 1024;
      Const aLogDir: String = '';
      Const aLogFileNameFormat: String
        {$IFNDEF DISABLE_PRLOGGER}
        = TLoggerProFileAppender.DEFAULT_FILENAME_FORMAT_WITH_PID
        {$ELSE}
        = ''
        {$ENDIF}); overload;
    destructor Destroy; override;

    // iMaxLog public implementations
    Function LogENtry(Const aMsg: String = ''): iLogEntry;
    Function LogEnterExitProc(Const aProcName: String; aLogType: TLogType = TLogType.Debug): iLogEntry;
    Procedure add(Const aMsg: String; aLogType: TLogType = TLogType.Info); Overload;
    Procedure add(aMsg: iLogEntry); Overload;
    Procedure add(aMsg: iLogEntry; aLogType: TLogType); Overload;
    Procedure addRaw(Const aJsonLogEntryText: String; aLogType: TLogType);
    // lock/Unlock to protect the LoggerPro writer access
    procedure Lock;
    procedure UnLock;
    Procedure Info(Const aMsg: String); Overload;
    Procedure Info(aMsg: iLogEntry); Overload;
    Procedure Warn(Const aMsg: String); Overload;
    Procedure Warn(aMsg: iLogEntry); Overload;
    Procedure Error(Const aMsg: String); Overload;
    // Best source: crash-time stack (must be called inside the same `except` block)
    // lTrace := madExcept.GetCrashStackTrace;
    Procedure Error(Const aMsg, aCallStackTrace: String); overload;
    Procedure Error(aMsg: iLogEntry); Overload;
    Procedure Debug(Const aMsg: String); Overload;
    Procedure Debug(aMsg: iLogEntry); Overload;
    Function CloneWithTag(Const aNewTag: String): iMaxLog;
    // releases internal loggerPro Writer
    procedure ShutDown;

    // Properties defined in iMaxLog are implemented via the private getters/setters
  End;

  /// <summary>
  /// Helper class used by LogEnterExitProc to automatically log procedure exit and duration.
  /// </summary>
  TDoLogOnRelease = Class(TLogEntry)
  Private
    fStopWatch: TStopWatch;
    fMaxLog: iMaxLog; // Keep reference to the logger that created it
  Public
    constructor Create(aMaxLog: iMaxLog);
    destructor Destroy; override;
  End;

Var
  glMainVclThreadId: TThreadID; // ID of the main VCL/Application thread

  {$IFDEF MsWindows} // PID is constant on Windows
  glPid: Int64;
  glPidAsHex: String = ''; // Cache PID hex string
  {$ENDIF}

  GlobalAppStartTimer: TStopWatch; // Timer started at application launch

  // Default global iMaxLog instance, auto-created on first use
Function maxLog: iMaxLog;
// Explicitly set the global iMaxLog instance
Procedure SetGlobalMaxLog(aInstance: iMaxLog);
// Check if the global instance has been created
Function GlobalMaxLogInstanceCreated: Boolean;
// Release the global logger instance and resources
Procedure ShutDownMaxLog;
// Helper to encode string for JSON
Function JsonEncode(Const RawText: String): String; inline;

{$IFDEF MsWindows}
// Get current process ID (Windows)
Function GetPID: Cardinal;
{$ENDIF}
// Get current process ID as a hex string
Function GetPidAsHex: String;

Implementation

Uses
  {$IFDEF madExcept}
  madStackTrace, // For capturing stack traces on error
  {$ENDIF}
  {$IFDEF LINUX}
  // Linux specific units if needed, e.g., for process/thread info
  Posix.Stdlib, Posix.SysStat, Posix.SysTypes, Posix.Unistd, Posix.Signal, Posix.Fcntl,
  {$ENDIF}
  system.IOUtils; // Added for TDirectory, TFile, TPath

const
  cTagMsg = 'MSG'; // Standard tag name for the main log message
  {$IFDEF MadExcept}
  cCallStackTag = 'CallStack'; // Standard tag name for stack traces
  {$ENDIF}

Var
  glFormatSettings: TFormatSettings; // For consistent float/date formatting
  glCs: TCriticalSection;           // Protects global logger instance creation
  glDefaultMaxLog: iMaxLog;         // The global logger instance

// -----------------------------------------------------------------------------
// Global Logger Management
// -----------------------------------------------------------------------------

Function GlobalMaxLogInstanceCreated: Boolean;
begin
  Result := Assigned(glDefaultMaxLog);
end;

Function maxLog: iMaxLog;
begin
  // Fast path: Instance already exists
  if Assigned(glDefaultMaxLog) then
  begin
    Result := glDefaultMaxLog;
    Exit;
  end;

  // Ensure critical section is created (thread-safe)
  if not Assigned(glCs) then
  begin
    var lCs := TCriticalSection.Create;
    if TInterlocked.CompareExchange(Pointer(glCs), Pointer(lCs), nil) <> nil then
      lCs.Free; // Didn't win the race, free the extra CS
  end;

  // Slow path: Create instance within critical section
  glCs.Enter;
  try
    // Double-check pattern
    if Assigned(glDefaultMaxLog) then
      Result := glDefaultMaxLog
    else
    begin
      glDefaultMaxLog := TMaxLog.Create; // Create with default settings
      Result := glDefaultMaxLog;
    end;
  finally
    glCs.Leave;
  end;
end;

Procedure SetGlobalMaxLog(aInstance: iMaxLog);
begin
  // Ensure critical section is created (thread-safe)
  if not Assigned(glCs) then
  begin
    var lCs := TCriticalSection.Create;
    if TInterlocked.CompareExchange(Pointer(glCs), Pointer(lCs), nil) <> nil then
      lCs.Free;
  end;

  glCs.Enter;
  try
    glDefaultMaxLog := aInstance; // Assign the provided instance
  finally
    glCs.Leave;
  end;
end;

Procedure ShutDownMaxLog;
begin
  if Assigned(glDefaultMaxLog) then
  begin
    if assigned(glDefaultMaxLog) then
      glDefaultMaxLog.ShutDown;
    glDefaultMaxLog := nil; // Release our reference
    {$IFNDEF DISABLE_PRLOGGER}
    // Signal LoggerPro to shut down its threads and release resources
    LoggerPro.GlobalLogger.ReleaseGlobalLogger;

    // Give LoggerPro's background threads some time to finish writing
    // Note: This might be fragile. LoggerPro should ideally block until done.
    Sleep(10); // Initial small delay
    // CheckSynchronize waits for pending main thread messages, often used
    // to allow background thread sync calls to complete.
    while System.Classes.CheckSynchronize(50) do // Check every 50ms
      Sleep(10); // Short sleep between checks
    {$ENDIF}
  end;
end;

// -----------------------------------------------------------------------------
// Process ID Helpers
// -----------------------------------------------------------------------------

{$IFDEF MsWindows}
Function GetPID: Cardinal;
begin
  // Cache PID on Windows as it doesn't change
  if glPid <> 0 then
    Exit(glPid);

  Result := GetCurrentProcessId;
  glPid := Result; // Store for next time
end;
{$ENDIF}

Function GetPidAsHex: String;
var
  lPid: Int64;
begin
  {$IFDEF MsWindows}
  // Use cached hex string on Windows if available
  if glPidAsHex <> '' then
    Exit(glPidAsHex);
  {$ENDIF}

  lPid := idGlobal.CurrentProcessId; // Get current PID (works cross-platform)
  Result := IntToHex(lPid, 1); // Convert to hex (minimum 1 digit)

  {$IFDEF MsWindows}
  // Cache the result on Windows
  glPidAsHex := Result;
  {$ENDIF}
end;

// -----------------------------------------------------------------------------
// JSON Encoding Helper
// -----------------------------------------------------------------------------

Function JsonEncode(Const RawText: String): String;
var
  lJsonStr: TJsonString;
begin
  // Use System.Json for robust JSON string encoding
  lJsonStr := TJsonString.Create(RawText);
  try
    Result := lJsonStr.ToJson;
  finally
    lJsonStr.Free; // Ensure TJsonString is freed
  end;
end;

// -----------------------------------------------------------------------------
// TLogEntry Implementation
// -----------------------------------------------------------------------------

constructor TLogEntry.Create;
begin
  inherited Create;
  // Initialize the sorted list for tags with a reasonable default capacity
  fTags := TSortedList<String, String>.Create;
  fTags.Capacity := 10;
  CaptureThreadId; // Record the creating thread's ID/name
  fLogType := TLogType.Info; // Default log level
end;

destructor TLogEntry.Destroy;
begin
  // TSortedList will be freed automatically by TInterfacedObject mechanism
  // if fTags holds the only reference (which it should)
  inherited;
end;

Procedure TLogEntry.Append(Var aText: String; Const aTagName, aTagValue: String);
var
  s: String;
begin
  // Don't append empty values
  if aTagValue = '' then
    Exit;

  // Format as "TagName": "EncodedValue"
  s := '"' + aTagName + '":' + JsonEncode(aTagValue);

  // Append with comma separator if needed
  if aText = '' then
    aText := s
  else
    aText := aText + ', ' + s;
end;

Procedure TLogEntry.CaptureThreadId;
var
  tid: TThreadID;
begin
  // Only capture if not already done
  if fThreadId = '' then
  begin
    {$IFDEF MSWINDOWS}
    tid := GetCurrentThreadId;
    {$ELSE}
    tid := TThread.CurrentThread.ThreadID; // Use TThread for cross-platform
    {$ENDIF}

    // Identify the main thread
    if tid = glMainVclThreadId then
      fThreadId := 'Main'
    else
    begin
      // ToDo: Consider integrating with TThread.Name for more descriptive IDs
      fThreadId := IntToHex(tid, 1); // Use Hex ID for other threads
    end;
  end;
end;

{$IFDEF MadExcept}
function TLogEntry.GetCallStack: String;
begin
  Result := fCallStack;
end;

procedure TLogEntry.SetCallStack(const Value: String);
begin
  fCallStack := Value;
end;
{$ENDIF}

Function TLogEntry.GetLogType: TLogType;
begin
  Result := fLogType;
end;

Procedure TLogEntry.SetLogType(Const Value: TLogType);
begin
  fLogType := Value;
end;

Function TLogEntry.msg(Const aMsg: String): iLogEntry;
begin
  Result := put(cTagMsg, aMsg); // Use the constant for the message tag
end;

Function TLogEntry.put(Const aTagName, aTagValue: String): iLogEntry;
begin
  fTags.addOrSet(aTagValue, aTagName); // Add or update the tag
  Result := Self; // Return self for fluent interface chaining
end;

Function TLogEntry.put(Const aTagName: String; aTagValue: Integer): iLogEntry;
begin
  Result := put(aTagName, aTagValue.ToString); // Convert integer to string
end;

Function TLogEntry.put(Const aTagName: String; Const aTagValue: Int64): iLogEntry;
begin
  Result := put(aTagName, aTagValue.ToString); // Convert Int64 to string
end;

Function TLogEntry.put(Const aTagName: String; Const aTagValue: Double): iLogEntry;
var
  s: String;
begin
  // Use global format settings for consistent float formatting (e.g., decimal point)
  s := FloatToStr(aTagValue, glFormatSettings);
  Result := put(aTagName, s);
end;

Function TLogEntry.put(Const aTagName: String; Const aTagValue: TDateTime): iLogEntry;
var
  s: String;
begin
  // Use ISO8601 format for dates, which is standard and unambiguous
  s := system.DateUtils.DateToISO8601(aTagValue, True); // Include time zone info
  Result := put(aTagName, s);
end;

Function TLogEntry.putComponent(aComponent: TComponent): iLogEntry;
var
  s: String;
  procedure PrependName(Const aValue: String);
  begin
    if s = '' then
      s := aValue
    else
      s := s + '.' + aValue; // Build hierarchy string like Owner.Child.Grandchild
  end;

begin
  s := ''; // Initialize hierarchy string
  repeat
    if (aComponent = nil) then
    begin
      if s = '' then // Handle case where input component is nil
        PrependName('<NIL>');
      Break; // Exit loop once owner chain reaches nil
    end
    else
    begin
      // Prepend "Name(ClassName)" or "$Address(ClassName)" if no name
      PrependName(
        IfThen(aComponent.Name <> '', aComponent.Name, '$' + IntToHex(NativeInt(aComponent), 1)) +
        '(' + aComponent.ClassName + ')'
      );
      aComponent := aComponent.Owner; // Move up the owner chain
    end;
  until False; // Loop controlled by break condition

  Result := put('COMP', s); // Add the hierarchy under the "COMP" tag
end;

Function TLogEntry.Text(aKind: TOutputFormatKind): String;
var
  x, i: Integer;
  lPid: Int64; // Local variable for current PID check
  lTagValue: String;
  lTagName: String;
begin
  Result := '';
  // Add standard metadata tags common to all formats
  put('TID', fThreadId);
  put('PID', GetPidAsHex);

  // Check if process forked (PID changed since startup - relevant on Linux)
  lPid := idGlobal.CurrentProcessId;
  {$IFDEF MSWINDOWS}
  if lPid <> glPid then // Compare with cached startup PID on Windows
    put('fork-PID', IntToHex(lPid, 1));
  {$ELSE}
  // On Linux, we don't cache glPid, so this check isn't meaningful in the same way.
  // You might need a different mechanism if fork detection is critical on Linux.
  {$ENDIF}
  put('MsSinceAppStart', GlobalAppStartTimer.ElapsedMilliseconds); // Add uptime

  case aKind of
    ofkJson:
    begin
      // Build JSON object: { "Tag1": "Value1", "Tag2": "Value2", ... }
      for x := 0 to fTags.Count - 1 do
      begin
        lTagName := fTags.Items[x].iid;
        lTagValue := fTags.Items[x].data;
        Append(Result, lTagName, lTagValue); // Append "tag": "value"
      end;

      {$IFDEF MadExcept}
      // Add call stack last in JSON if available
      if fCallStack <> '' then
        Append(Result, cCallStackTag, fCallStack);
      {$ENDIF}
      Result := '{' + Result + '}'; // Wrap in braces
    end;
    ofkPlainText, ofkPlainOnNextLine:
    begin
      // Build plain text: [TAG1: Value1] [TAG2: Value2] ... Message
      for x := 0 to fTags.Count - 1 do
      begin
        lTagName := fTags.Items[x].iid;
        // Skip the main message tag here; handle it separately
        if SameText(lTagName, cTagMsg) then // Case-insensitive compare
          Continue;
        lTagValue := fTags.Items[x].data;
        Result := Result + '[' + lTagName + ': ' + lTagValue + '] ';
      end;

      // Find and append the main message
      if fTags.Find(cTagMsg, i) then
      begin
        if aKind = ofkPlainOnNextLine then
          Result := Trim(Result) + sLineBreak + fTags[i] // Message on new line
        else
          Result := Result + fTags[i]; // Message on same line
      end;

      {$IFDEF MadExcept}
      // Append call stack on new lines if available
      if fCallStack <> '' then
      begin
        Result := Result + sLineBreak +
                  cCallStackTag + ':' + sLineBreak + // Add a label for the stack
                  fCallStack;
      end;
      {$ENDIF}
    end;
  else // Should not happen
    Result := 'Unknown OutputFormatKind';
  end;
end;

// -----------------------------------------------------------------------------
// TMaxLog Implementation
// -----------------------------------------------------------------------------

// Main constructor
constructor TMaxLog.Create(aMaxFileCount: Integer; aMaxFileSizeInKb: Integer;
  const aLogDir: String; const aLogFileNameFormat: String);
var
{$IFNDEF DISABLE_PRLOGGER }
  lFileAppender: TLoggerProFileAppender;
{$ENDIF}
  lPid: Int64;
  lModuleName: string; // For potential use in filename format
begin
  inherited Create;
  fOutputFormatKind := ofkJson; // Default to JSON format
  fAutoStackTraceForErrors := True; // Default to adding stack traces for errors

  // Determine and normalize the log directory
  if aLogDir = '' then
    // Default to 'Logs' subdirectory relative to the executable/DLL
    fLogDir := IncludeTrailingPathDelimiter(ExtractFilePath(MaxLogic.ioUtils.GetCurrentDLLName) + 'logs')
  else
    // Expand potential relative paths or environment variables
    fLogDir := IncludeTrailingPathDelimiter(ExpandFileName(aLogDir));

  // Ensure the log directory exists
  ForceDirectories(fLogDir);

  lPid := idGlobal.CurrentProcessId;
  fDefaultTag := 'Main'; // Default tag, can be overridden

  // Calculate the file mask for deleting old log files based on the format string
  fFileMaskForDeletingOldLogFiles := ChangeFileExt(aLogFileNameFormat, '*.log');
  // Replace format specifiers with wildcards
  lModuleName := TPath.GetFileNameWithoutExtension(MaxLogic.ioUtils.GetCurrentDLLName); // Get module name for replacements
  fFileMaskForDeletingOldLogFiles := StringReplace(fFileMaskForDeletingOldLogFiles, '%s', lModuleName, [rfIgnoreCase]); // Assume first %s is module
  fFileMaskForDeletingOldLogFiles := StringReplace(fFileMaskForDeletingOldLogFiles, '%s', '*', [rfReplaceAll, rfIgnoreCase]); // Replace remaining %s with *
  fFileMaskForDeletingOldLogFiles := StringReplace(fFileMaskForDeletingOldLogFiles, '%d', '*', [rfReplaceAll, rfIgnoreCase]);
  fFileMaskForDeletingOldLogFiles := StringReplace(fFileMaskForDeletingOldLogFiles, '%2.2d', '*', [rfReplaceAll, rfIgnoreCase]);
  fFileMaskForDeletingOldLogFiles := StringReplace(fFileMaskForDeletingOldLogFiles, '*.*', '*', [rfReplaceAll, rfIgnoreCase]); // Collapse wildcards
  fFileMaskForDeletingOldLogFiles := StringReplace(fFileMaskForDeletingOldLogFiles, '**', '*', [rfReplaceAll, rfIgnoreCase]); // Collapse wildcards

  {$IFNDEF DISABLE_PRLOGGER}
  // Create the LoggerPro file appender
  lFileAppender := TLoggerProFileAppender.Create(
    aMaxFileCount,          // Max number of log files
    aMaxFileSizeInKb,       // Max size per file
    fLogDir,                // Log directory
    aLogFileNameFormat,     // Filename pattern
    TLogItemRendererNoTag.Create, // Use a renderer that omits the tag (we put it in the message)
    TEncoding.UTF8          // Use UTF-8 encoding
    );
  // Build the log writer with the file appender
  // You could add other appenders here, like TLoggerProOutputDebugStringAppender.Create
  fLogWriter := BuildLogWriter([lFileAppender]);
  {$ENDIF}

  // Initialize other fields
  fMaxLogFileAgeInDays := 0; // Default: Don't delete old logs
  fLastCheckOldLogFilesTimeStamp := 0; // Initialize timestamp for log deletion check
end;

// Cloning constructor (private)
constructor TMaxLog.CreateClone(aSource: TMaxLog; const aNewTag: string);
begin
  inherited Create;
  // Copy configuration and shared resources from the source instance
  fDefaultTag := aNewTag; // Set the new tag
  fLogDir := aSource.fLogDir;
  fOutputFormatKind := aSource.fOutputFormatKind;
  fAutoStackTraceForErrors := aSource.fAutoStackTraceForErrors;

  fMaxLogFileAgeInDays := 0; // disable deleting of old log files, only the main instace should do that
  fParentInstance:= aSource;
end;

destructor TMaxLog.Destroy;
begin
  ShutDown;
  inherited;
end;

procedure TMaxLog.Error(const aMsg, aCallStackTrace: String);
var
  lMsg: iLogEntry;
begin
  lMsg:= LogENtry(aMsg);
  lMsg.CallStack := aCallStackTrace;
  Add(lMsg, TLogType.Error);
end;

// iMaxLog Interface Method Implementations

function TMaxLog.GetLogDir: String;
begin
  Result := fLogDir;
end;

procedure TMaxLog.SetMaxLogFileAgeInDays(const Value: Integer);
begin
  // Basic validation could be added (e.g., Value >= 0)
  fMaxLogFileAgeInDays := Value;
  // Reset timestamp to force check soon if age limit was just set
  if Value > 0 then
    fLastCheckOldLogFilesTimeStamp := 0;
end;

function TMaxLog.GetMaxLogFileAgeInDays: Integer;
begin
  Result := fMaxLogFileAgeInDays;
end;

procedure TMaxLog.SetAutoStackTraceForErrors(const Value: Boolean);
begin
  fAutoStackTraceForErrors := Value;
end;

function TMaxLog.GetAutoStackTraceForErrors: Boolean;
begin
  Result := fAutoStackTraceForErrors;
end;

procedure TMaxLog.SetOutputFormatKind(const Value: TOutputFormatKind);
begin
  fOutputFormatKind := Value;
end;

procedure TMaxLog.ShutDown;
begin
  {$IFNDEF DISABLE_PRLOGGER }
  if fLogWriter = nil then
    Exit;
  try
    Lock;
    try
      fLogWriter:= nil;
    finally
      UnLock;
    end;
  Except
    // do nothing
  end;
  {$ENDIF}
end;

procedure TMaxLog.UnLock;
begin
  system.TMonitor.Exit(Self);
end;

function TMaxLog.GetOutputFormatKind: TOutputFormatKind;
begin
  Result := fOutputFormatKind;
end;

Procedure TMaxLog.SetDefaultTag(Const Value: String);
begin
  fDefaultTag := Value;
end;

Function TMaxLog.GetDefaultTag: String;
begin
  Result := fDefaultTag;
end;

{$IFNDEF DISABLE_PRLOGGER }
Procedure TMaxLog.SetLogWriter(Const Value: ILogWriter);
begin
  Lock;
  try
    fLogWriter := Value; // Allow replacing the writer
  finally
    UnLock;
  end;
end;

Function TMaxLog.GetLogWriter: ILogWriter;
begin
  Result := fLogWriter;
end;
{$ENDIF}

Function TMaxLog.CloneWithTag(Const aNewTag: String): iMaxLog;
begin
  // Use the private cloning constructor
  Result := TMaxLog.CreateClone(Self, aNewTag);
end;

Function TMaxLog.LogENtry(Const aMsg: String): iLogEntry;
begin
  // Create a new log entry object
  Result := TLogEntry.Create;
  // Optionally add the initial message
  if aMsg <> '' then
    Result.msg(aMsg);
end;

procedure TMaxLog.Lock;
begin
  system.TMonitor.Enter(Self);
end;

Function TMaxLog.LogEnterExitProc(Const aProcName: String;
  aLogType: TLogType): iLogEntry;
var
  lEntryHelper: TDoLogOnRelease; // Use specific type
  lMsgEntry: TLogEntry; // Use specific type
begin
  // Log the entry message immediately
  lMsgEntry := TLogEntry.Create;
  lMsgEntry.put('EnterProc', aProcName);
  Self.add(lMsgEntry, aLogType); // Use Self to call the instance's add method

  // Create the helper object that logs on destruction
  lEntryHelper := TDoLogOnRelease.Create(Self); // Pass Self (iMaxLog)
  lEntryHelper.put('ExitProc', aProcName); // Set the exit message tag
  lEntryHelper.fLogType := aLogType; // Set the log level for the exit message
  Result := lEntryHelper; // Return the helper interface
end;

Procedure TMaxLog.addRaw(Const aJsonLogEntryText: String; aLogType: TLogType);
begin
  // Check if it's time to delete old log files
  CheckDeleteOldLogFiles;

  {$IFNDEF DISABLE_PRLOGGER }
  // Use LoggerPro if enabled
  Lock;
  try
    if Assigned(fLogWriter) then
      fLogWriter.Log(aLogType, aJsonLogEntryText, fDefaultTag)
    else if assigned(fParentInstance) then
    begin
      fParentInstance.Lock;
      try
        if Assigned(fParentInstance.LogWriter) then
          fParentInstance.LogWriter.Log(aLogType, aJsonLogEntryText, fDefaultTag);
      finally
        fParentInstance.UnLock;
      end;
    end;
  finally
    UnLock;
  end;
  {$ELSE}
  // Simple file logging if LoggerPro is disabled
  var f: TextFile;
  var fn := Self.fLogDir + ChangeFileExt(ExtractFileName(MaxLogic.ioUtils.GetCurrentDLLName), '.log');
  // Note: Simplified filename without PID/Tag if LoggerPro is disabled
  System.Assign(f, fn);
  try
    if TFile.Exists(fn) then
      System.Append(f) // Append if file exists
    else
      System.Rewrite(f); // Create new file otherwise
    System.WriteLn(f, aJsonLogEntryText); // Write the log line
    System.Flush(f); // Ensure it's written to disk
  finally
    System.CloseFile(f); // Close the file handle
  end;
  {$ENDIF}
end;

Procedure TMaxLog.add(aMsg: iLogEntry; aLogType: TLogType);
begin
  // Automatically add stack trace for errors if enabled and MadExcept is defined
  if fAutoStackTraceForErrors and (aLogType = TLogType.Error) then
  begin
    {$IFDEF MadExcept}
    if (aMsg.CallStack = '') then
    begin
      aMsg.CallStack := MadStackTrace.StackTrace;
    end;
    {$ENDIF}
  end;
  // Set the log type on the entry before formatting
  aMsg.LogType := aLogType;
  // Format the entry and pass it to addRaw
  addRaw(aMsg.Text(fOutputFormatKind), aLogType);
end;

Procedure TMaxLog.add(aMsg: iLogEntry);
begin
  // Add the entry using its own stored log level
  add(aMsg, aMsg.LogType);
end;

Procedure TMaxLog.add(Const aMsg: String; aLogType: TLogType);
begin
  // Convenience overload: create entry, set message, add it
  add(LogENtry(aMsg), aLogType);
end;

// --- Simplified Logging Methods ---

Procedure TMaxLog.Info(Const aMsg: String);
begin
  add(aMsg, TLogType.Info);
end;

Procedure TMaxLog.Info(aMsg: iLogEntry);
begin
  add(aMsg, TLogType.Info);
end;

Procedure TMaxLog.Warn(Const aMsg: String);
begin
  add(aMsg, TLogType.Warning);
end;

Procedure TMaxLog.Warn(aMsg: iLogEntry);
begin
  add(aMsg, TLogType.Warning);
end;

Procedure TMaxLog.Error(Const aMsg: String);
begin
  add(aMsg, TLogType.Error);
end;

Procedure TMaxLog.Error(aMsg: iLogEntry);
begin
  add(aMsg, TLogType.Error);
end;

Procedure TMaxLog.Debug(Const aMsg: String);
begin
  add(aMsg, TLogType.Debug);
end;

Procedure TMaxLog.Debug(aMsg: iLogEntry);
begin
  add(aMsg, TLogType.Debug);
end;

// --- Log File Maintenance ---

Procedure TMaxLog.CheckDeleteOldLogFiles;
var
  lPattern: String;
  lLogDir: String;
  dtNow: TDateTime;
  lMaxAgeInDays: Integer;
  lLastCheck: TDateTime;
  lInt64: Int64;
  lProc: TProc;
begin
  if fParentInstance <> nil then
    fParentInstance.CheckDeleteOldLogFiles;

  // Exit if deletion is disabled
  if fMaxLogFileAgeInDays <= 0 then
    Exit;

  dtNow := Now;
  lInt64:= TInterlocked.Read( pInt64(@fLastCheckOldLogFilesTimeStamp)^ );
  lLastCheck := pDateTime( @lInt64)^; // Read volatile field; reminder: TDateTime is a Double



  // Check only once per day (approximately)
  if (lLastCheck > 0) and (system.DateUtils.HoursBetween(dtNow, lLastCheck) < 24) then
    Exit;

  // Attempt to update the timestamp using interlocked exchange for thread safety
  if TInterlocked.CompareExchange(Double(fLastCheckOldLogFilesTimeStamp), Double(dtNow), Double(lLastCheck)) <> Double(lLastCheck) then
    Exit; // Another thread is already checking, bail out

  // Capture local variables for the async task
  lLogDir := Self.fLogDir;
  lPattern := Self.fFileMaskForDeletingOldLogFiles;
  lMaxAgeInDays := Self.fMaxLogFileAgeInDays;

  // Run the deletion check asynchronously
  lProc:= (procedure
    var
      fn: String;
      lFileDt: TDateTime;
      lFiles: TArray<String>;
    begin
      try
        lFiles := TDirectory.GetFiles(lLogDir, lPattern);
        for fn in lFiles do
        begin
          try
            lFileDt := TFile.GetLastWriteTime(fn);
            // Check age based on hours for slightly more precision than DaysBetween
            if system.DateUtils.HoursBetween(Now, lFileDt) > (24 * lMaxAgeInDays) then
              DeleteFile(fn); // deleteFIle() does not raise an exception, but TFile.Delete() does... so we prefer DeleteFile here
          except
            // Ignore errors deleting single files (e.g., permission issues, file locked)
            // Consider logging this error to the logger itself if important
          end;
        end;
      except
         // Ignore errors getting the file list (e.g., directory not found)
      end;
    end);
  System.Threading.TTask.Run(lProc);
end;

// -----------------------------------------------------------------------------
// TDoLogOnRelease Implementation
// -----------------------------------------------------------------------------

constructor TDoLogOnRelease.Create(aMaxLog: iMaxLog);
begin
  inherited Create; // Calls TLogEntry.Create
  fMaxLog := aMaxLog; // Store the logger instance that created it
  fStopWatch := TStopWatch.StartNew; // Start timing
end;

destructor TDoLogOnRelease.Destroy;
var
  lElapsedMs: Int64;
begin
  lElapsedMs := fStopWatch.ElapsedMilliseconds;
  // Add the duration tag before formatting
  put('Duration', lElapsedMs.ToString + ' ms');

  // Use the stored logger instance to write the exit message
  if Assigned(fMaxLog) then
  begin
    // Format the message using the logger's settings and log it
    fMaxLog.addRaw(Self.Text(fMaxLog.OutputFormatKind), Self.fLogType);
  end;

  inherited;
end;

// -----------------------------------------------------------------------------
// Unit Initialization / Finalization
// -----------------------------------------------------------------------------

Initialization
  // Initialize global stopwatch for uptime calculation
  GlobalAppStartTimer := TStopWatch.StartNew;
  // Create global format settings for consistent number/date formatting
  glFormatSettings := TFormatSettings.Create;
  glFormatSettings.DecimalSeparator := '.'; // Use period for decimal separator

  // Record the main thread ID for identification in logs
  {$IFDEF MSWINDOWS}
  glMainVclThreadId := GetCurrentThreadId;
  glPid := GetPID; // Pre-cache PID on Windows
  glPidAsHex := GetPidAsHex; // Pre-cache hex PID on Windows
  {$ELSE}
  glMainVclThreadId := TThread.CurrentThread.ThreadID;
  // PID caching is not done for Linux here
  {$ENDIF}

  // Critical section for global logger creation is created on demand in maxLog function

Finalization
  // Clean up global logger instance and associated resources
  ShutDownMaxLog;

  // Free the global critical section if it was created
  if Assigned(glCs) then
  begin
    FreeAndNil(glCs);
  end;

end.
