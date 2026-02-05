unit maxLogAsyncWriter;

{$REGION 'Version History'}
{ copyright Pawel Piotrowski
  Description: this is a class that helps using old style log writing f existing legacy applications

  Version: 2.11
  History:
  2025-11-17: added global logger
  2024-10-10: Linux compatible
  2018-28: all logs are now written to the blog format, which is a encrypted binary log format
  2018-10-10: rebuild to prevent deadlocks that seemed to occur with the previous version
  2017-05-16: the object is created only if it is needed
  2017-01-20: some small improvements in the robustness. the
  TFileHolder is now responsible for writing, the StreamWriter and the FileStream are now strict private
  the communication is now a bit differently.
  the locking is now done via TMonitor
  The Signals are gone
  2016-09-13: the instance is now freed in the finalization section, no longer in the class destructor, this caused problems when using this class in a dll
  2016-02-12: minor adjustments
  2014--12-10: some bugfixes
  2014-12-04: ClassDestructor was not called as long as the thread was working... but it was working as long as the class was not destroyed... now we check for application.terminated to make sure the application is quiting
  2014-11-10: Added TmaxLogAsyncWriter.FetchAll
  2014-06-05: some minor fixes
  2014-05-15: use TStream instead of Memory maped files. Yes it is a bit slower, but this way we cut down a lot of memory overhead.
  2014-05-13: Limited the pagesize to 1 MB.
  2014-04-26 - extended the delay to 300 ms
  2014-03-20: first implementation }
{$ENDREGION}

{$IFDEF DEBUG}
// this will force the log entries to be written immediately
// this will make the app slower
{$DEFINE FORCE_FULL_FLUSH}
{$ENDIF}

interface

uses
  {$IFDEF madExcept}
  madExcept,
  {$ENDIF}
  {$IFDEF MsWindows}
  Windows,
  {$ENDIF}
  Classes, SysUtils, maxAsync, generics.collections,
  synCommons, SynCrypto,
  diagnostics;

type
  // forward declaration
  TMaxLogAsyncWriter = class;

  TLogItem = record
  public
    msg: string;
    FileName: string;
    TimeStamp: Int64;
    DateTime: TDateTime;
    CreatePlainTextLogFile: boolean;
    ThreadId: dword;
    ThreadName: string;
    class function Create: TLogItem; static;
    function estimateInMemorySize: dword;
  end;

  TItems = TArray<TLogItem>;

  TCompressMethod = (cmNone = 0, cmSynLZ1 = 1, cmSynLZ2 = 2);

  TBinaryLogWriter = class
  public

    // sometimes we want explicitly to keep the old logs.... here is how you can do it
    class var DisableOldLogPurging: boolean;
      // completly disable writing of the binary log
    class var Disabled: boolean;
  private
    const
      cMaxBLogSize = 1024 * 1024 * 10;
      cMaxOldFileCount = 10;
  private
    fLogPart: integer;
    fBLog: TStream;
    fKey: TSHA256Digest;
    fKeyInitialized: boolean;
    fChunk: TMemoryStream;
    fOlderLogFiles: TStringList;
    fParent: TMaxLogAsyncWriter;

    procedure writeBLogHeader;

    procedure Write(const Items: TArray<TLogItem>);
    procedure OpenBLogFile(const aDir: string);
    class procedure Writestring(const Stream: TStream; const s: string);
    procedure initKey;

    // after the file gets too big, we will rename it, read it, compress it, and write it again
    procedure MoveAndZipBLog;
  public
    constructor Create(aParent: TMaxLogAsyncWriter);
    destructor Destroy; override;
  end;

  TBlogArchiveRepacker = class
  private
    fLogPart: integer;
    fOld, fNew, fChunk: TMemoryStream;
    fFilename: string;
    fKey: TSHA256Digest;
    fStopWatch: TStopWatch;
    procedure flushChunk;
    procedure addRecompressSummary;
  public
    constructor Create(const aFileName: string; const aKey: TSHA256Digest; aLogPart: integer);
    destructor Destroy; override;

    procedure Execute;
  end;

  TOnLogCallbackProc = reference to procedure(const aMsg, aLogFileName: string; aCreatePlainTextLogFile: boolean = True);
  TMaxLogAsyncWriter = class
  private const
      mb = 1024 * 1024;
      cMaxLogItemBufferSize = 10 * mb;
      cLAZYDELAY = 300;
      cWaitForFileTimeOut = 1000;
      cMaxFileOpenTime = 10000;

  private type
      TFileHolder = class
      strict private
        fFilename: string;
        fFile: TFileStream;
        fWriter: TStreamWriter;
        fLastAccessTimeStamp: Int64;
        procedure Open;
        procedure Close;
      public
        DicKey: string;
        constructor Create(aFileName: string);
        destructor Destroy; override;

        procedure Write(const s: string);
        // returns true if the file was closed
        function CheckLastAccessAndCloseIfNeeded: boolean;
      end;
  private
    fBLogDir: string;
    fOnLogCallbacks: TDictionary<string, TOnLogCallbackProc>;
    fAsync: iAsync;
    fSignalNewItemAvailable, fSignalAllItemsProcesed: iSignal;
    fTerminated: boolean;
    fItems: TList<TLogItem>;
    fItemsSize: dword;
    fdic: TObjectDictionary<string, TFileHolder>;
    fBLogWriter: TBinaryLogWriter;

    procedure asyncprocess;
    procedure compact(var Items: TItems);
    function RetrieveItems(out Items: TItems): boolean;
    procedure OpenFile(FileName: string; var fh: TFileHolder);
    procedure AddLogItem(const LogItem: TLogItem);
    procedure CloseFiles(IgnoreDelay: boolean = False);
    procedure lock;
    procedure unlock;

  private
    fLogAsyncWriter: TMaxLogAsyncWriter;
  public
    constructor Create(const aLogDir: string);
    destructor Destroy; override;

    procedure AddToLogFile(const aText: string; const FileName: string; CreatePlainTextLogFile: boolean = True);
    procedure AddToLogFileAndWaitTilWritingComplete(const aText: string; const FileName: string; CreatePlainTextLogFile: boolean = True);
    // forces all files to be written and closed; it will wait until all files are really closed
    procedure FetchAll;

    // if there is already a callback with tht name, it will be overriden
    procedure RegisterOnLogCallback(const AName: string; aOnLogCallbackProc: TOnLogCallbackProc);
    procedure UnRegisterOnLogCallback(const AName: string);
    property BLogDir: string read fBLogDir;
  end;

// if the logger is already created, but with a different log dir, then the old one will be freed and a new one will be created
function InitGlobalLogger(const aLogDir: string): TMaxLogAsyncWriter;
procedure FreeGlobalLogger;
// if the Globa Logger was not yet initialized, and the log file name has a full path, then a new globalLogger will be created.
// if not Global Logger is present, the function returns false
function AddToLogFile(const aMsg, aLogFileName: string; aCreatePlainTextLogFile: boolean = True): boolean;
// returns false if the logger is not created yet
function FlushGlobalLogger: boolean;

implementation

uses
  {$IFDEF MsWindows}
  maxLogic.Windows,
  {$ENDIF}
  AppWideStopWatch,
  maxCryptoHelper.Decryptor, System.IOUtils, bsUtils, System.zlib, SynLZ, System.SyncObjs, System.Math,
  maxLogic.IOUtils, maxLogic.StrUtils,
  System.StrUtils;

var
  fglGlobalLogger: TMaxLogAsyncWriter = nil;

function InitGlobalLogger(const aLogDir: string): TMaxLogAsyncWriter;
var
  lDir, lCurDir: string;
begin
  lDir := slash(ExpandFileName(aLogDir));
  if aLogDir = '' then
    raise Exception.Create('maxLogAsyncWriter.InitGlobalLogger: LogDir can not be empty');
  if fglGlobalLogger <> nil then
  begin
    lCurDir := slash(ExpandFileName(fglGlobalLogger.BLogDir));
    if not SameText(lDir, lCurDir) then
      FreeAndNil(fglGlobalLogger);
  end;

  if fglGlobalLogger = nil then
    fglGlobalLogger := TMaxLogAsyncWriter.Create(aLogDir);

  Result := fglGlobalLogger;
end;

procedure FreeGlobalLogger;
begin
  FreeAndNil(fglGlobalLogger);
end;

function AddToLogFile(const aMsg, aLogFileName: string; aCreatePlainTextLogFile: boolean = True): boolean;
begin
  if not assigned(fglGlobalLogger) then
    if ExtractFilePath(aLogFileName) <> '' then
      InitGlobalLogger(ExtractFilePath(aLogFileName));

  if not assigned(fglGlobalLogger) then
    exit(False);

  fglGlobalLogger.AddToLogFile(aMsg, aLogFileName, aCreatePlainTextLogFile);
  Result := True;
end;

function FlushGlobalLogger: boolean;
begin
  if not assigned(fglGlobalLogger) then
    exit(False);

  fglGlobalLogger.FetchAll;
  Result := True;
end;

{ TmaxLogAsyncWriter }

procedure TMaxLogAsyncWriter.AddToLogFile(const aText, FileName: string; CreatePlainTextLogFile: boolean);
var
  LogItem: TLogItem;
begin
  LogItem := TLogItem.Create;
  LogItem.msg := aText;
  LogItem.FileName := FileName;
  LogItem.CreatePlainTextLogFile := CreatePlainTextLogFile;
  if CreatePlainTextLogFile then
    if ExtractFilePath(LogItem.FileName) = '' then
      LogItem.FileName := TPath.Combine(self.BLogDir, LogItem.FileName);

  AddLogItem(LogItem);

  for var lProc in fOnLogCallbacks.Values.ToArray do
  begin
    if assigned(lProc) then
      lProc(aText, FileName, CreatePlainTextLogFile);
  end;
end;

procedure TMaxLogAsyncWriter.AddToLogFileAndWaitTilWritingComplete(const aText, FileName: string; CreatePlainTextLogFile: boolean = True);
begin
  AddToLogFile(aText, FileName, CreatePlainTextLogFile);
  FetchAll;
end;

procedure TMaxLogAsyncWriter.asyncprocess;
var
  LogItem: TLogItem;
  Items: TItems;
  fh: TFileHolder;
begin
  while True do
  begin
    RetrieveItems(Items);
    if length(Items) = 0 then
    begin
      if fTerminated then
        break;

      fSignalAllItemsProcesed.setSignaled;
      fSignalNewItemAvailable.waitforSignaled; //(cLAZYDELAY);
      Continue; // restart the loop and get the new items to process
    end;

    if not TBinaryLogWriter.Disabled and assigned(fBLogWriter) then
      fBLogWriter.Write(Items);

    compact(Items);

    for LogItem in Items do
    begin
      OpenFile(LogItem.FileName, fh);
      fh.Write(LogItem.msg);
    end;
  end; // while

  fSignalAllItemsProcesed.setSignaled;
end;

procedure TMaxLogAsyncWriter.CloseFiles;
var
  fh: TFileHolder;
  ToDel: TList<string>;
  s: string;
begin
  ToDel := TList<string>.Create;
  try
    if IgnoreDelay then
      fdic.Clear
    else
    begin
      for fh in fdic.Values do
      begin
        if fh.CheckLastAccessAndCloseIfNeeded then
          ToDel.Add(fh.DicKey);
      end;

      for s in ToDel do
        fdic.Remove(s);
    end;
  finally
    ToDel.Free;
  end;
end;

constructor TMaxLogAsyncWriter.Create;
begin
  inherited Create;
  fOnLogCallbacks := TDictionary<string, TOnLogCallbackProc>.Create;
  fBLogDir := aLogDir;

  if not TBinaryLogWriter.Disabled then
    fBLogWriter := TBinaryLogWriter.Create(self);

  fSignalNewItemAvailable := tSignal.Create;
  fSignalNewItemAvailable.setNonsignaled;

  fSignalAllItemsProcesed := tSignal.Create;
  fSignalAllItemsProcesed.setSignaled;

  fdic := TObjectDictionary<string, TFileHolder>.Create([doOwnsValues]);
  fItems := TList<TLogItem>.Create;
  fAsync := maxAsync.SimpleAsyncCall(
    procedure
    begin
      asyncprocess;
    end
    , classname);
end;

destructor TMaxLogAsyncWriter.Destroy;
var
  LogItem: TLogItem;
begin
  fTerminated := True;
  // wake up
  fSignalNewItemAvailable.setSignaled;
  fAsync.WaitFor;

  CloseFiles(True);
  fdic.Free;
  fItems.Free;

  if assigned(fBLogWriter) then
    fBLogWriter.Free;
  FreeAndNil(fOnLogCallbacks);
  inherited;
end;

procedure TMaxLogAsyncWriter.FetchAll;
begin
  fSignalAllItemsProcesed.setNonsignaled;
  fSignalNewItemAvailable.setSignaled;
  fSignalAllItemsProcesed.waitforSignaled;
end;

procedure TMaxLogAsyncWriter.OpenFile(FileName: string;

  var
  fh:
  TFileHolder);
var
  fn: string;
begin
  FileName := ExpandFileName(FileName);
  fn := AnsiLowercase(FileName);

  if not fdic.TryGetValue(fn, fh) then
  begin
    fh := TFileHolder.Create(FileName);
    fh.DicKey := fn;
    fdic.Add(fn, fh);
  end;

end;

procedure TMaxLogAsyncWriter.RegisterOnLogCallback(const AName: string;
  aOnLogCallbackProc: TOnLogCallbackProc);
begin
  if assigned(aOnLogCallbackProc) then
    fOnLogCallbacks.AddOrSetValue(AName, aOnLogCallbackProc);
end;

procedure TMaxLogAsyncWriter.AddLogItem(const LogItem: TLogItem);
var
  Path: string;
begin
  lock;
  try
    fItems.Add(LogItem);
    fItemsSize := fItemsSize + LogItem.estimateInMemorySize;

    fSignalAllItemsProcesed.setNonsignaled;
    fSignalNewItemAvailable.setSignaled;
  finally
    unlock;
  end;

  // prevent memory overflow
  while (fItemsSize > cMaxLogItemBufferSize) and (not fTerminated) do
    fSignalAllItemsProcesed.waitforSignaled(10);

  {$IFDEF FORCE_FULL_FLUSH}
  fSignalAllItemsProcesed.waitforSignaled;
  {$ENDIF}
end;

function TMaxLogAsyncWriter.RetrieveItems(
  out Items: TItems): boolean;
begin
  Result := False;
  lock;
  try
    Items := fItems.ToArray;
    fItems.Clear;
    fItemsSize := 0;
    Result := length(Items) <> 0;
    fSignalNewItemAvailable.setNonsignaled;
  finally
    unlock;
  end;
end;

procedure TMaxLogAsyncWriter.lock;
begin
  TMonitor.Enter(self);
end;

procedure TMaxLogAsyncWriter.unlock;
begin
  TMonitor.exit(self);
end;

procedure TMaxLogAsyncWriter.UnRegisterOnLogCallback(const AName: string);
begin
  fOnLogCallbacks.Remove(AName);
end;

procedure TMaxLogAsyncWriter.compact;
var
  dic: TDictionary<string, integer>;
  Item: TLogItem;
  s: string;
  newCount, i, X: integer;
  newItems: TItems;
begin
  newCount := 0;
  SetLength(newItems, length(Items));
  dic := nil;

  for X := 0 to length(Items) - 1 do
    if Items[X].CreatePlainTextLogFile then
    begin
      if dic = nil then
        dic := TDictionary<string, integer>.Create(length(Items));

      Items[X].FileName := ExpandFileName(Items[X].FileName);
      s := AnsiLowercase(Items[X].FileName);
      if dic.TryGetValue(s, i) then
        newItems[i].msg := newItems[i].msg + sLineBreak + Items[X].msg
      else
      begin
        newItems[newCount] := Items[X];
        dic.Add(s, newCount);
        Inc(newCount);
      end;
    end;

  // trim
  if newCount <> length(newItems) then
    SetLength(newItems, newCount);

  Items := newItems;
  if dic <> nil then
    dic.Free;
end;

{ TmaxLogAsyncWriter.TFileHolder }

procedure TMaxLogAsyncWriter.TFileHolder.Close;
begin
  if assigned(fWriter) then
    FreeAndNil(fWriter);
  if assigned(fFile) then
    FreeAndNil(fFile);
end;

constructor TMaxLogAsyncWriter.TFileHolder.Create(aFileName: string);
begin
  inherited Create;

  fFilename := aFileName;
end;

destructor TMaxLogAsyncWriter.TFileHolder.Destroy;
begin
  Close;

  inherited;

end;

procedure TMaxLogAsyncWriter.TFileHolder.Open;
var
  hFile: THandle;
  st: TStopWatch;
begin
  if assigned(fWriter) then
    exit;

  ForceDirectories(ExtractFilePath(fFilename));
  st := TStopWatch.startNew;

  repeat
    hFile := CreateFile(
      { In the ANSI version of this function, the name is limited to MAX_PATH characters. To extend this limit to 32,767 wide characters, call the Unicode version of the function and prepend "\\?\" to the path. For more information, see Naming Files, Paths, and Namespaces. }
      PChar('\\?\' + fFilename),
      GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE,
      nil,
      OPEN_ALWAYS,
      FILE_ATTRIBUTE_NORMAL, 0);
  until (hFile <> INVALID_HANDLE_VALUE) or (st.ElapsedMilliseconds > cWaitForFileTimeOut);

  if hFile = INVALID_HANDLE_VALUE then
  begin
    // logging is not essential, just turn it off in that case
    {$IFDEF MsWindows}
    if maxLogic.Windows.IsDelphiRunning then
      raise Exception.Create(SysErrorMessage(GetLastError) + '; FileName: ' + fFilename + '; ElapsedTime: ' + IntToStr(st.ElapsedMilliseconds) + ' ms')
    else
      {$ENDIF}
      exit;
  end else begin
    fFile := TFileStream.Create(hFile);

    fFile.Position := fFile.Size;
    fWriter := TStreamWriter.Create(fFile, TEncoding.UTF8);
  end;
end;

procedure TMaxLogAsyncWriter.TFileHolder.Write(const s: string);
begin
  fLastAccessTimeStamp := AppStopWatch.ElapsedMilliseconds;
  if not assigned(fWriter) then
    Open;
  if assigned(fWriter) then
    fWriter.WriteLine(s);
end;

function TMaxLogAsyncWriter.TFileHolder.CheckLastAccessAndCloseIfNeeded: boolean;
begin
  Result := True;

  // no need to perform TMonitor.enter if there is nothing to close
  if not assigned(fWriter) then
    exit;

  if AppStopWatch.ElapsedMilliseconds - fLastAccessTimeStamp > TMaxLogAsyncWriter.cMaxFileOpenTime then
  begin
    Close;
    Result := True;
  end
  else
    Result := False;

end;

{ TBinaryLogWriter }

constructor TBinaryLogWriter.Create;
begin
  inherited Create;
  fParent := aParent;
  fOlderLogFiles := TStringList.Create;
  fChunk := TMemoryStream.Create;
  fChunk.Size := 1024;
end;

destructor TBinaryLogWriter.Destroy;
begin
  FreeAndNil(fOlderLogFiles);
  FreeAndNil(fBLog);
  FreeAndNil(fChunk);
  inherited;
end;

procedure TBinaryLogWriter.Write(const Items: TArray<TLogItem>);
var
  dir: string;
  Size: dword;
  fileNamesNoPath: array of string;
  rs, S2: rawByteString;
  X: integer;
  c: TAESCFB;
begin
  if length(Items) = 0 then
    exit;

  if not assigned(fBLog) then
  begin
    dir := fParent.fBLogDir;
    OpenBLogFile(dir);
    // if the funktion above failed...
    if not assigned(fBLog) then
      exit;
  end;

  SetLength(fileNamesNoPath, length(Items));

  // estimate buffer size
  Size := 0;
  for X := 0 to length(Items) - 1 do
  begin
    fileNamesNoPath[X] := ExtractFileName(Items[X].FileName);
    Inc(Size,
      SizeOf(Int64) +
      SizeOf(TDateTime) +
      SizeOf(dword) +
      length(Items[X].msg) * 2 +
      length(fileNamesNoPath[X]) * 2 +
      // thread id
      SizeOf(dword) * 2
      );
  end;

  // actually it will be a bit more then we need,
  // but, it is better to reserve more memory then to precise count the number of bytes needed for each string above
  if fChunk.Size < Size then
    fChunk.Size := Size;

  // write the data
  fChunk.Position := 0;
  for X := 0 to length(Items) - 1 do
  begin
    fChunk.writeBuffer(Items[X].TimeStamp, SizeOf(Int64));
    fChunk.writeBuffer(Items[X].DateTime, SizeOf(TDateTime));

    Writestring(fChunk, fileNamesNoPath[X]);
    Writestring(fChunk, Items[X].msg);

    fChunk.writeBuffer(Items[X].ThreadId, SizeOf(dword));
    Writestring(fChunk, Items[X].ThreadName);
  end;

  // get the bytes out of the stream
  Size := fChunk.Position;
  fChunk.Position := 0;
  SetLength(rs, Size);
  fChunk.readbuffer(rs[1], Size);

  // now compress
  SetLength(S2, SynLZcompressdestlen(length(rs)));
  Size := SynLZcompress1(pAnsiChar(rs), length(rs), pAnsiChar(S2));
  SetLength(S2, Size);

  // encrypt
  c := TAESCFB.Create(fKey);
  rs := c.encryptPKCS7(S2, True);
  c.Free;

  Size := length(rs);
  fBLog.writeBuffer(Size, 4);
  if Size <> 0 then
    fBLog.writeBuffer(rs[1], Size);

  if fBLog.Size > cMaxBLogSize then
  begin
    fBLog.Free;
    fBLog := nil;
    MoveAndZipBLog;
  end;

end;

procedure TBinaryLogWriter.writeBLogHeader;
var
  Version: dword;
  b: BYTE;
begin
  Version := 4;
  fBLog.writeBuffer(Version, SizeOf(dword));
  fBLog.writeBuffer(System.MainThreadID, SizeOf(dword));
  b := BYTE(cmSynLZ1);
  fBLog.writeBuffer(b, 1);
end;

procedure TBinaryLogWriter.OpenBLogFile(const aDir: string);
var
  FileName: string;
  hFile: THandle;
  st: TStopWatch;
begin
  if assigned(fBLog) then
    exit;

  initKey;

  // now open the file
  ForceDirectories(aDir);
  FileName := aDir + '.all.bLog';
  st := TStopWatch.startNew;
  repeat
    hFile := CreateFile(
      { In the ANSI version of this function, the name is limited to MAX_PATH characters. To extend this limit to 32,767 wide characters, call the Unicode version of the function and prepend "\\?\" to the path. For more information, see Naming Files, Paths, and Namespaces. }
      PChar('\\?\' + FileName),
      GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE,
      nil,
      OPEN_ALWAYS,
      FILE_ATTRIBUTE_NORMAL, 0);
  until (hFile <> INVALID_HANDLE_VALUE) or (st.ElapsedMilliseconds > TMaxLogAsyncWriter.cWaitForFileTimeOut);

  if hFile = INVALID_HANDLE_VALUE then
  begin
    // logging is not essential, just turn it off in that case
    {$IFDEF MsWindows}
    if maxLogic.Windows.IsDelphiRunning then
      raise Exception.Create(SysErrorMessage(GetLastError) + '; FileName: ' + FileName + '; ElapsedTime: ' + IntToStr(st.ElapsedMilliseconds) + ' ms')
    else
      {$ENDIF}
      exit;
  end else begin
    fBLog := TFileStream.Create(hFile);
    if fBLog.Size <> 0 then
      fBLog.Position := fBLog.Size
    else
    begin
      writeBLogHeader;
    end;
  end;
end;

class procedure TBinaryLogWriter.Writestring(const Stream: TStream;
  const s: string);
var
  b: TBytes;
  Size: dword;
begin
  b := TEncoding.UTF8.GetBytes(s);

  Size := length(b);
  Stream.writeBuffer(Size, 4);
  if Size <> 0 then
    Stream.writeBuffer(b[0], Size);
end;

procedure TBinaryLogWriter.initKey;
var
  b, outData: TBytes;
begin
  if fKeyInitialized then
    exit;
  fKeyInitialized := True;

  // init aes key
  // 230 bytes
  b := [$8C, $53, $05, $53, $2D, $C2, $C1, $3C, $47, $B5, $E6, $4E, $50, $CC, $9F, $12, $3F, $F6, $2F, $6F, $7C, $15, $38, $2E, $42, $96, $38, $B5, $F9, $7B, $3A, $5B, $8F, $74, $AE, $33, $AE, $B7, $74, $C2
    , $79, $D6, $42, $57, $40, $3A, $33, $D0, $D5, $37, $87, $58, $DC, $65, $78, $E0, $7C, $BB, $90, $B3, $94, $1F, $9D, $51, $C0, $35, $B1, $24, $FD, $63, $4A, $AA, $DE, $A6, $45, $4D, $AD, $17, $12, $78, $B5
    , $77, $A4, $C6, $11, $CB, $78, $5E, $0D, $9F, $EA, $99, $34, $FC, $DB, $07, $87, $0C, $A3, $F8, $2D, $6C, $4B, $70, $4E, $74, $28, $ED, $28, $CD, $61, $A6, $78, $DD, $71, $9C, $45, $84, $AF, $40, $83, $24
    , $EF, $76, $B4, $14, $CA, $B1, $63, $1C, $A0, $B6, $D7, $F6, $5A, $A0, $A6, $6C, $4B, $61, $2F, $B4, $24, $0F, $8D, $7D, $0B, $A3, $49, $D3, $91, $F9, $DD, $1F, $78, $D2, $D4, $7C, $29, $A6, $F7, $BC, $E5
    , $5B, $85, $17, $82, $68, $12, $25, $00, $17, $6C, $E0, $16, $5D, $83, $E9, $6A, $27, $7B, $0C, $39, $4F, $FB, $96, $4C, $7D, $6D, $56, $62, $6E, $C6, $99, $EC, $EA, $9B, $07, $44, $DF, $45, $B3, $96, $DF
    , $21, $8D, $E1, $09, $C9, $87, $6F, $E1, $3A, $46, $FB, $52, $13, $33, $3B, $A7, $12, $9A, $AB, $56, $7F, $35, $14, $9E, $9D, $75];

  if maxCryptoHelper.Decryptor.decrypt(b, outData) then
  begin
    move(outData[0], fKey, 32);
  end else begin
    raise Exception.Create('Corruption in ' + classname);
    halt;
  end;

  // burn sensible data after usage:
  fillchar(outData[0], length(outData), 0);

end;

procedure TBinaryLogWriter.MoveAndZipBLog;
var
  fn1, fn2: string;
  lKey: TSHA256Digest;
begin
  fn1 := fParent.fBLogDir + '.all.bLog';
  fn2 := fParent.fBLogDir + '.all(' + FormatDateTime('yyyy"-"mm"-"dd"_"hh";"nn";"ss"."zzz', now) + ').bLog';
  TFile.move(fn1, fn2);

  fOlderLogFiles.Add(fn2);
  if not DisableOldLogPurging then
    while fOlderLogFiles.Count > Max(2, cMaxOldFileCount) do
    begin
      // actually we will not delete the first log file, as it mostly contains some special start up informations.
      fn1 := fOlderLogFiles[1];
      try
        if TFile.Exists(fn1) then
          TFile.delete(fn1);
      except
        // do nothing
      end;
      fOlderLogFiles.delete(1);
    end;

  lKey := fKey;
  maxAsync.SimpleAsyncCall(
    procedure
    var
      repacker: TBlogArchiveRepacker;
    begin
      repacker := TBlogArchiveRepacker.Create(fn2, lKey, fLogPart);
      TInterlocked.increment(fLogPart);
      try
        repacker.Execute;
      finally
        repacker.Free;
      end;
    end, 'repackOldArchive');
end;

{ TLogItem }

class
  function TLogItem.Create: TLogItem;
  begin
    Result := default(TLogItem);
    Result.TimeStamp := AppStopWatch.ElapsedMilliseconds;
    Result.DateTime := now();

    Result.ThreadId := TThread.CurrentThread.ThreadID;
    Result.ThreadName := TmaxAsyncGlobal.GetThreadName(Result.ThreadId);
  end;

  function TLogItem.estimateInMemorySize: dword;
  begin
    Result :=
      length(msg) * 2 +
      length(FileName) * 2 +
      SizeOf(TimeStamp) +
      SizeOf(DateTime) +
      SizeOf(CreatePlainTextLogFile) +
      SizeOf(ThreadId) +
      length(ThreadName) * 2;
  end;

  { TBlogArchiveRepacker }

  procedure TBlogArchiveRepacker.addRecompressSummary;
  const
    mb = 1024 * 1024;
  var
    i64: Int64;
    dt: TDateTime;
    s: string;
  begin
    s := 'orgSize: ' + IntToStr(fOld.Size) + ' b' + CR +
      'Size (MB): ' + fStr(fOld.Size / mb) + CR + CR +
      'Part#: ' + IntToStr(fLogPart);

    i64 := AppStopWatch.ElapsedMilliseconds;
    fChunk.writeBuffer(i64, SizeOf(Int64));
    dt := now;
    fChunk.writeBuffer(dt, SizeOf(TDateTime));

    TBinaryLogWriter.Writestring(fChunk, classname);
    TBinaryLogWriter.Writestring(fChunk, s);

    fChunk.writeBuffer(System.MainThreadID, SizeOf(dword));
    TBinaryLogWriter.Writestring(fChunk, '');
  end;

  constructor TBlogArchiveRepacker.Create(const aFileName: string;
    const aKey: TSHA256Digest; aLogPart: integer);
  begin
    inherited Create;
    fLogPart := aLogPart;
    fFilename := aFileName;
    fKey := aKey;
    fOld := TMemoryStream.Create;
    fNew := TMemoryStream.Create;
    fChunk := TMemoryStream.Create;
  end;

  destructor TBlogArchiveRepacker.Destroy;
  begin
    fNew.Free;
    fOld.Free;
    fChunk.Free;
    inherited;
  end;

  procedure TBlogArchiveRepacker.Execute;
  const
    mb = 1024 * 1024;
  var
    Version: dword;
    S1, S2: rawByteString;
    Size: dword;
    c: TAESCFB;
    cm: TCompressMethod;
    b: BYTE;
  begin
    fStopWatch := TStopWatch.startNew;
    fOld.LoadFromFile(fFilename);

    // reserve some memory up front
    fNew.Size := fOld.Size;
    fChunk.Size := fOld.Size;

    // write header
    Version := 4; // the one that has compression enabled
    fNew.writeBuffer(Version, SizeOf(dword));
    fNew.writeBuffer(System.MainThreadID, SizeOf(dword));
    b := BYTE(cmSynLZ2);
    fNew.writeBuffer(b, 1);

    // skip the header in the old file
    fOld.Position := SizeOf(dword) * 2;
    fOld.readbuffer(cm, 1);

    // now read all chunks and decrypt them
    // store them all in one big chunk
    while fOld.Position + 4 < fOld.Size do
    begin
      fOld.readbuffer(Size, 4);
      // make sure we do not go out of the file...
      if fOld.Position + Size > fOld.Size then
        exit; // something went wrong... let the orginal file be as it was...

      if Size <> 0 then
      begin
        SetLength(S2, Size);
        fOld.readbuffer(S2[1], Size);

        // decrypt the chunk
        c := TAESCFB.Create(fKey);
        try
          S1 := c.DecryptPKCS7(S2, True);
        except
          c.Free;
          exit; // something went wrong... leave the orginal file as it were. no repacking this time.
        end;
        c.Free;

        // decompress the chunk
        case cm of
          cmNone:
            S2 := S1;
          cmSynLZ1:
            begin
              SetLength(S2, SynLZdecompressdestlen(pAnsiChar(S1)));
              Size := SynLZdecompress1(pAnsiChar(S1), length(S1), pAnsiChar(S2));
              SetLength(S2, Size);
            end;
          cmSynLZ2:
            begin
              SetLength(S2, SynLZdecompressdestlen(pAnsiChar(S1)));
              Size := SynLZdecompress2(pAnsiChar(S1), length(S1), pAnsiChar(S2));
              SetLength(S2, Size);
            end;
        end;

        // write it to the one big chunk
        fChunk.writeBuffer(S2[1], length(S2));

        if fChunk.Size > 50 * mb then
          flushChunk;
      end;
    end;

    addRecompressSummary;
    flushChunk;

    fNew.Size := fNew.Position; // truncate
    // if the orginal was better compressed... then no need to overwrite it
    if fNew.Size < fOld.Size then
      fNew.SaveToFile(fFilename);
  end;

  procedure TBlogArchiveRepacker.flushChunk;
  var
    S1, S2: rawByteString;
    Size: dword;
    c: TAESCFB;
  begin
    // chunk contains now all the decrypted log items from the
    fChunk.Size := fChunk.Position; // trim it
    SetLength(S1, fChunk.Size);
    fChunk.Position := 0;
    fChunk.readbuffer(S1[1], fChunk.Size);
    fChunk.Size := 0; // free memory

    // now compress
    SetLength(S2, SynLZcompressdestlen(length(S1)));
    Size := SynLZcompress2(pAnsiChar(S1), length(S1), pAnsiChar(S2));
    SetLength(S2, Size);

    // now encrypt
    c := TAESCFB.Create(fKey);
    S1 := c.encryptPKCS7(S2, True);
    c.Free;

    Size := length(S1);
    fNew.writeBuffer(Size, 4);
    if Size <> 0 then
      fNew.writeBuffer(S1[1], Size);
  end;

initialization

finalization
  FreeGlobalLogger;

end.

