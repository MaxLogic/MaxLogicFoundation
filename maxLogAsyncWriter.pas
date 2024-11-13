Unit maxLogAsyncWriter;

{ copyright Pawel Piotrowski
  Description: this is a class that helps using old style log writing f existing legacy applications

  Version: 2.10
  History:
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

Interface

Uses
  {$IFDEF madExcept}
  madExcept,
  {$ENDIF}
  {$IFDEF MsWindows}
  windows,
  {$ENDIF}
  classes, sysUtils, maxAsync, generics.collections,
  synCommons, SynCrypto,
  diagnostics;

Type
  // forward declaration
  TMaxLogAsyncWriter = Class;

  TLogItem = Record
  Public
    Msg: String;
    Filename: String;
    TimeStamp: Int64;
    DateTime: TDateTime;
    CreatePlainTextLogFile: boolean;
    ThreadId: DWORD;
    ThreadName: String;
    Class Function Create: TLogItem; Static;
    Function estimateInMemorySize: DWORD;
  End;

  TItems = TArray<TLogItem>;

  TCompressMethod = (cmNone = 0, cmSynLZ1 = 1, cmSynLZ2 = 2);

  TBinaryLogWriter = Class
  Public

    // sometimes we want explicitly to keep the old logs.... here is how you can do it
    Class Var DisableOldLogPurging: boolean;
    // completly disable writing of the binary log
    Class Var Disabled: boolean;
  Private
    Const
    cMaxBLogSize = 1024 * 1024 * 10;
    cMaxOldFileCount = 10;
  Private
    fLogPart: integer;
    fBLog: TStream;
    fKey: TSHA256Digest;
    fKeyInitialized: boolean;
    fChunk: TMemoryStream;
    fOlderLogFiles: TStringList;

    Procedure writeBLogHeader;

    Procedure write(Const items: TArray<TLogItem>);
    Procedure OpenBLogFile(Const adir: String);
    Class Procedure writeString(Const stream: TStream; Const s: String);
    Procedure initKey;

    // after the file gets too big, we will rename it, read it, compress it, and write it again
    Procedure MoveAndZipBLog;
  Public
    Constructor Create;
    Destructor destroy; Override;
  End;

  TBlogArchiveRepacker = Class
  Private
    fLogPart: integer;
    fOld, fNew, fChunk: TMemoryStream;
    fFileName: String;
    fKey: TSHA256Digest;
    fStopWatch: TStopWatch;
    Procedure flushChunk;
    Procedure addRecompressSummary;
  Public
    Constructor Create(Const aFileName: String; Const aKey: TSHA256Digest; aLogPart: integer);
    Destructor destroy; Override;

    Procedure execute;
  End;

  TMaxLogAsyncWriter = Class
  Private Const
    mb = 1024 * 1024;
    cMaxLogItemBufferSize = 10 * mb;
    cLAZYDELAY = 300;
    cWaitForFileTimeOut = 1000;
    cMaxFileOpenTime = 10000;

  Private Type
    TFileHolder = Class
    Strict Private
      fFileName: String;
      fFile: TFileStream;
      fWriter: TStreamWriter;
      fLastAccessTimeStamp: Int64;
      Procedure Open;
      Procedure Close;
    Public
      DicKey: String;
      Constructor Create(aFileName: String);
      Destructor destroy; Override;

      Procedure write(Const s: String);
      // returns true if the file was closed
      Function CheckLastAccessAndCloseIfNeeded: boolean;
    End;
  Private
    Class Var fBLogDir: String;
  Private
    fAsync: iAsync;
    fSignalNewItemAvailable, fSignalAllItemsProcesed: iSignal;
    fTerminated: boolean;
    fItems: TList<TLogItem>;
    fItemsSize: DWORD;
    fdic: TObjectDictionary<String, TFileHolder>;
    fBLogWriter: TBinaryLogWriter;

    Procedure asyncprocess;
    Procedure compact(Var items: TItems);
    Function retriveItems(Out items: TItems): boolean;
    Procedure OpenFile(Filename: String; Var fh: TFileHolder);
    Procedure DoAddToLogFile(Const aText, Filename: String; CreatePlainTextLogFile: boolean);
    Procedure AddLogItem(Const LogItem: TLogItem);
    Procedure CloseFiles(IgnoreDelay: boolean = false);
    Procedure DoFetchAll;
    Procedure lock;
    Procedure unLock;

  Private // class methods
    Class Var
      fLogAsyncWriter: TMaxLogAsyncWriter;
    Class Constructor CreateClass;
    Class Function GetSingeltonInstance: TMaxLogAsyncWriter; Static;
    Class Function GetBLogDir: String; Static;
    Class Procedure SetBlogDir(Const Value: String); Static;
  Public
    Constructor Create;
    Destructor destroy; Override;

    Class Procedure AddToLogFile(Const aText: String; Const Filename: String; CreatePlainTextLogFile: boolean = true);
    Class Procedure AddToLogFileAndWaitTilWritingComplete(Const aText: String; Const Filename: String; CreatePlainTextLogFile: boolean = true);
    // forces all files to be written and closed; it will wait until all files are really closed
    Class Procedure FetchAll;

  Public
    Class Property BLogDir: String Read GetBLogDir Write SetBlogDir;
  End;

  TEnterLeaveLog = Class(TInterfacedObject)
  Private
    fTimeStamp: Int64;
    fDescription: String;
    fLogFile: String;
    fCreatePlainTextLogFile: boolean;
    Function getElapsedSufix: String;
  Public
    Destructor destroy; Override;

    { will add an log enty and after the interface get released a second one,
      Note, a "enter" / "Leave" prefix will be added before the aDescription.
      OnLeave a timestamp will be added as a sufix like "[%d ms]"
      Note: if the logName doesn't ave a path or extension, then aCreatePlainTextLogFile will be set to false anyway }
    Class Function enter(Const aDescription, aLogFileName: String;
      Const aFirstEntryOnlyExtraText: String = '';
      aCreatePlainTextLogFile: boolean = true): iInterface;
  End;

Implementation

Uses
  // mmSystem,
  AppWideStopWatch, pawel1,
  maxCryptoHelper.Decryptor, ioUtils, bsutils, zlib, SynLZ;

{ TmaxLogAsyncWriter }

Class Procedure TMaxLogAsyncWriter.AddToLogFile(Const aText, Filename: String; CreatePlainTextLogFile: boolean = true);
Begin
  GetSingeltonInstance.DoAddToLogFile(aText, Filename, CreatePlainTextLogFile);
End;

Procedure TMaxLogAsyncWriter.DoAddToLogFile(Const aText, Filename: String; CreatePlainTextLogFile: boolean);
Var
  LogItem: TLogItem;
Begin
  LogItem := TLogItem.Create;
  LogItem.Msg := aText;
  LogItem.Filename := Filename;
  LogItem.CreatePlainTextLogFile := CreatePlainTextLogFile;

  AddLogItem(LogItem);
End;

Class Procedure TMaxLogAsyncWriter.AddToLogFileAndWaitTilWritingComplete(Const aText, Filename: String; CreatePlainTextLogFile: boolean = true);
Begin
  AddToLogFile(aText, Filename, CreatePlainTextLogFile);
  FetchAll;
End;

Procedure TMaxLogAsyncWriter.asyncprocess;
Var
  LogItem: TLogItem;
  items: TItems;
  fh: TFileHolder;
Begin
  While true Do
  Begin
    fSignalAllItemsProcesed.setNonsignaled;

    retriveItems(items);
    If length(items) = 0 Then
    Begin
      // CloseFiles;
      If fTerminated Then
        break;

      fSignalAllItemsProcesed.setSignaled;
      fSignalNewItemAvailable.waitforSignaled(cLAZYDELAY);
      continue; // restart the loop and get the new items to process
    End;

    If Not TBinaryLogWriter.Disabled And assigned(fBLogWriter) Then
      fBLogWriter.write(items);

    compact(items);

    For LogItem In items Do
    Begin
      OpenFile(LogItem.Filename, fh);
      fh.write(LogItem.Msg);
    End;

    fSignalAllItemsProcesed.setSignaled;

  End; // while

  fSignalAllItemsProcesed.setSignaled;
End;

Procedure TMaxLogAsyncWriter.CloseFiles;
Var
  fh: TFileHolder;
  ToDel: TList<String>;
  s: String;
Begin
  ToDel := TList<String>.Create;
  Try
    If IgnoreDelay Then
      fdic.clear
    Else
    Begin
      For fh In fdic.values Do
      Begin
        If fh.CheckLastAccessAndCloseIfNeeded Then
          ToDel.add(fh.DicKey);
      End;

      For s In ToDel Do
        fdic.remove(s);
    End;
  Finally
    ToDel.free;
  End;
End;

Constructor TMaxLogAsyncWriter.Create;
Begin
  Inherited;
  If Not TBinaryLogWriter.Disabled Then
    fBLogWriter := TBinaryLogWriter.Create;

  fSignalNewItemAvailable := tSignal.Create;
  fSignalNewItemAvailable.setNonsignaled;

  fSignalAllItemsProcesed := tSignal.Create;
  fSignalAllItemsProcesed.setSignaled;

  fdic := TObjectDictionary<String, TFileHolder>.Create([doOwnsValues]);
  fItems := TList<TLogItem>.Create;
  fAsync := maxAsync.SimpleAsyncCall(
    Procedure
    Begin
      asyncprocess;
    End
    , classname);
End;

Class Constructor TMaxLogAsyncWriter.CreateClass;
Begin
  // GetSingeltonInstance;
End;

Destructor TMaxLogAsyncWriter.destroy;
Var
  LogItem: TLogItem;
Begin
  fTerminated := true;
  // wake up
  fSignalNewItemAvailable.setSignaled;
  fAsync.waitFor;

  CloseFiles(true);
  fdic.free;
  fItems.free;

  If assigned(fBLogWriter) Then
    fBLogWriter.free;

  Inherited;
End;

Class Procedure TMaxLogAsyncWriter.FetchAll;
Begin
  GetSingeltonInstance.DoFetchAll;
End;

Procedure TMaxLogAsyncWriter.DoFetchAll;
Begin
  fSignalAllItemsProcesed.setNonsignaled;
  fSignalNewItemAvailable.setSignaled;
  fSignalAllItemsProcesed.waitforSignaled;
End;

Class Function TMaxLogAsyncWriter.GetSingeltonInstance: TMaxLogAsyncWriter;
Begin
  If Not assigned(fLogAsyncWriter) Then
    fLogAsyncWriter := TMaxLogAsyncWriter.Create;

  result := fLogAsyncWriter;
End;

Procedure TMaxLogAsyncWriter.OpenFile(Filename: String;

Var
  fh:
  TFileHolder);
Var
  fn: String;
Begin
  Filename := expandfilename(Filename);
  fn := AnsiLowercase(Filename);

  If Not fdic.TryGetValue(fn, fh) Then
  Begin
    fh := TFileHolder.Create(Filename);
    fh.DicKey := fn;
    fdic.add(fn, fh);
  End;

End;

Procedure TMaxLogAsyncWriter.AddLogItem(Const LogItem: TLogItem);
Var
  path: String;
Begin
  lock;
  Try
    fItems.add(LogItem);
    fItemsSize := fItemsSize + LogItem.estimateInMemorySize;
    path := extractFilePath(LogItem.Filename);
    fSignalAllItemsProcesed.setNonsignaled;

    // do not write items if they are not yet full set up
    If path <> '' Then
    Begin
      If fBLogDir = '' Then
        fBLogDir := path;

      fSignalNewItemAvailable.setSignaled;
    End;
  Finally
    unLock;
  End;

  // prevent memory overflow
  While (fItemsSize > cMaxLogItemBufferSize) And (Not fTerminated) Do
    fSignalAllItemsProcesed.waitforSignaled(10);
End;

Function TMaxLogAsyncWriter.retriveItems(
  Out items: TItems): boolean;
Begin
  result := false;
  lock;
  Try
    // before retriving date, we need to property set the log dir...
    If fBLogDir <> '' Then
    Begin
      items := fItems.toArray;
      fItems.clear;
      fItemsSize := 0;
      result := length(items) <> 0;
      fSignalNewItemAvailable.setNonsignaled;
    End;
  Finally
    unLock;
  End;
End;

Procedure TMaxLogAsyncWriter.lock;
Begin
  TMonitor.enter(self);
End;

Procedure TMaxLogAsyncWriter.unLock;
Begin
  TMonitor.exit(self);
End;

Procedure TMaxLogAsyncWriter.compact;
Var
  dic: TDictionary<String, integer>;
  item: TLogItem;
  s: String;
  newCount, i, x: integer;
  newItems: TItems;
Begin
  newCount := 0;
  setLength(newItems, length(items));
  dic := Nil;

  For x := 0 To length(items) - 1 Do
    If items[x].CreatePlainTextLogFile Then
    Begin
      If dic = Nil Then
        dic := TDictionary<String, integer>.Create(length(items));

      items[x].Filename := expandfilename(items[x].Filename);
      s := AnsiLowercase(items[x].Filename);
      If dic.TryGetValue(s, i) Then
        newItems[i].Msg := newItems[i].Msg + sLineBreak + items[x].Msg
      Else
      Begin
        newItems[newCount] := items[x];
        dic.add(s, newCount);
        inc(newCount);
      End;
    End;

  // trim
  If newCount <> length(newItems) Then
    setLength(newItems, newCount);

  items := newItems;
  If dic <> Nil Then
    dic.free;
End;

Class Function TMaxLogAsyncWriter.GetBLogDir: String;
Begin
  result := fBLogDir;
End;

Class Procedure TMaxLogAsyncWriter.SetBlogDir(Const Value: String);
Var
  nDir: String;
  f1, f2: String;
Begin
  GetSingeltonInstance.lock;
  Try
    If Not sameText(GetSingeltonInstance.fBLogDir, nDir) Then
    Begin
      If assigned(GetSingeltonInstance.fBLogWriter) Then
      Begin
        GetSingeltonInstance.fBLogWriter.free;
        GetSingeltonInstance.fBLogWriter := Nil;

        f1 := fBLogDir + '.all.bLog';
        f2 := nDir + '.all.bLog';
        If Not fileExists(f2) Then
          TFile.move(f1, f2);
      End;

      fBLogDir := nDir;
    End;

  Finally
    GetSingeltonInstance.unLock;
  End;

End;

{ TmaxLogAsyncWriter.TFileHolder }

Procedure TMaxLogAsyncWriter.TFileHolder.Close;
Begin
  If assigned(fWriter) Then
    FreeAndNIL(fWriter);
  If assigned(fFile) Then
    FreeAndNIL(fFile);
End;

Constructor TMaxLogAsyncWriter.TFileHolder.Create(aFileName: String);
Begin
  Inherited Create;

  fFileName := aFileName;
End;

Destructor TMaxLogAsyncWriter.TFileHolder.destroy;
Begin
  Close;

  Inherited;

End;

Procedure TMaxLogAsyncWriter.TFileHolder.Open;
Var
  hFile: THandle;
  st: TStopWatch;
Begin
  If assigned(fWriter) Then
    exit;

  forceDirectories(extractFilePath(fFileName));
  st := TStopWatch.startNew;

  Repeat
    hFile := CreateFile(
    { In the ANSI version of this function, the name is limited to MAX_PATH characters. To extend this limit to 32,767 wide characters, call the Unicode version of the function and prepend "\\?\" to the path. For more information, see Naming Files, Paths, and Namespaces. }
    PChar('\\?\' + fFileName),
      GENERIC_READ Or GENERIC_WRITE,
      FILE_SHARE_READ Or FILE_SHARE_WRITE,
      Nil,
      OPEN_ALWAYS,
      FILE_ATTRIBUTE_NORMAL, 0);
  Until (hFile <> INVALID_HANDLE_VALUE) Or (st.elapsedMilliseconds > cWaitForFileTimeOut);

  If hFile = INVALID_HANDLE_VALUE Then
  Begin
    // logging is not essential, just turn it off in that case
    If pawel1.IsDelphiRunning Then
      Raise Exception.Create(SysErrorMessage(GetLastError) + '; FileName: ' + fFileName + '; ElapsedTime: ' + IntToStr(st.elapsedMilliseconds) + ' ms')
    Else
      exit;
  End Else Begin
    fFile := TFileStream.Create(hFile);

    fFile.position := fFile.size;
    fWriter := TStreamWriter.Create(fFile, TENcoding.utf8);
  End;
End;

Procedure TMaxLogAsyncWriter.TFileHolder.write( Const s: String);
Begin
  fLastAccessTimeStamp := AppStopWatch.elapsedMilliseconds;
  If Not assigned(fWriter) Then
    Open;
  If assigned(fWriter) Then
    fWriter.WriteLine(s);
End;

Function TMaxLogAsyncWriter.TFileHolder.CheckLastAccessAndCloseIfNeeded: boolean;
Begin
  result := true;

  // no need to perform TMonitor.enter if there is nothing to close
  If Not assigned(fWriter) Then
    exit;

  If AppStopWatch.elapsedMilliseconds - fLastAccessTimeStamp > TMaxLogAsyncWriter.cMaxFileOpenTime Then
  Begin
    Close;
    result := true;
  End
  Else
    result := false;

End;

{ TBinaryLogWriter }

Constructor TBinaryLogWriter.Create;
Begin
  Inherited Create;
  fOlderLogFiles := TStringList.Create;
  fChunk := TMemoryStream.Create;
  fChunk.size := 1024;
End;

Destructor TBinaryLogWriter.destroy;
Begin
  fOlderLogFiles.free;
  If assigned(fBLog) Then
  Begin
    fBLog.free;
    fBLog := Nil;
  End;
  Inherited;
End;

Procedure TBinaryLogWriter.write(Const items: TArray<TLogItem>);
Var
  dir: String;
  size: DWORD;
  fileNamesNoPath: Array Of String;
  rs, s2: rawByteString;
  x: integer;
  c: TAESCFB;
Begin
  If length(items) = 0 Then
    exit;

  If Not assigned(fBLog) Then
  Begin
    dir := TMaxLogAsyncWriter.fBLogDir;
    OpenBLogFile(dir);
    // if the funktion above failed...
    If Not assigned(fBLog) Then
      exit;
  End;

  setLength(fileNamesNoPath, length(items));

  // estimate buffer size
  size := 0;
  For x := 0 To length(items) - 1 Do
  Begin
    fileNamesNoPath[x] := extractFileName(items[x].Filename);
    inc(size,
      SizeOf(Int64) +
      SizeOf(TDateTime) +
      SizeOf(DWORD) +
      length(items[x].Msg) * 2 +
      length(fileNamesNoPath[x]) * 2 +
    // thread id
      SizeOf(DWORD) * 2
      );
  End;

  // actually it will be a bit more then we need,
  // but, it is better to reserve more memory then to precise count the number of bytes needed for each string above
  If fChunk.size < size Then
    fChunk.size := size;

  // write the data
  fChunk.position := 0;
  For x := 0 To length(items) - 1 Do
  Begin
    fChunk.writeBuffer(items[x].TimeStamp, SizeOf(Int64));
    fChunk.writeBuffer(items[x].DateTime, SizeOf(TDateTime));

    writeString(fChunk, fileNamesNoPath[x]);
    writeString(fChunk, items[x].Msg);

    fChunk.writeBuffer(items[x].ThreadId, SizeOf(DWORD));
    writeString(fChunk, items[x].ThreadName);
  End;

  // get the bytes out of the stream
  size := fChunk.position;
  fChunk.position := 0;
  setLength(rs, size);
  fChunk.ReadBuffer(rs[1], size);

  // now compress
  setLength(s2, SynLZcompressdestlen(length(rs)));
  size := SynLZcompress1(pAnsiChar(rs), length(rs), pAnsiChar(s2));
  setLength(s2, size);

  // encrypt
  c := TAESCFB.Create(fKey);
  rs := c.encryptPKCS7(s2, true);
  c.free;

  size := length(rs);
  fBLog.writeBuffer(size, 4);
  If size <> 0 Then
    fBLog.writeBuffer(rs[1], size);

  If fBLog.size > cMaxBLogSize Then
  Begin
    fBLog.free;
    fBLog := Nil;
    MoveAndZipBLog;
  End;

End;

Procedure TBinaryLogWriter.writeBLogHeader;
Var
  version: DWORD;
  b: byte;
Begin
  version := 4;
  fBLog.writeBuffer(version, SizeOf(DWORD));
  fBLog.writeBuffer(System.MainThreadID, SizeOf(DWORD));
  b := byte(cmSynLZ1);
  fBLog.writeBuffer(b, 1);
End;

Procedure TBinaryLogWriter.OpenBLogFile(Const adir: String);
Var
  Filename: String;
  hFile: THandle;
  st: TStopWatch;
Begin
  If assigned(fBLog) Then
    exit;

  initKey;

  // now open the file
  forceDirectories(adir);
  Filename := adir + '.all.bLog';
  st := TStopWatch.startNew;
  Repeat
    hFile := CreateFile(
    { In the ANSI version of this function, the name is limited to MAX_PATH characters. To extend this limit to 32,767 wide characters, call the Unicode version of the function and prepend "\\?\" to the path. For more information, see Naming Files, Paths, and Namespaces. }
    PChar('\\?\' + Filename),
      GENERIC_READ Or GENERIC_WRITE,
      FILE_SHARE_READ Or FILE_SHARE_WRITE,
      Nil,
      OPEN_ALWAYS,
      FILE_ATTRIBUTE_NORMAL, 0);
  Until (hFile <> INVALID_HANDLE_VALUE) Or (st.elapsedMilliseconds > TMaxLogAsyncWriter.cWaitForFileTimeOut);

  If hFile = INVALID_HANDLE_VALUE Then
  Begin
    // logging is not essential, just turn it off in that case
    If pawel1.IsDelphiRunning Then
      Raise Exception.Create(SysErrorMessage(GetLastError) + '; FileName: ' + Filename + '; ElapsedTime: ' + IntToStr(st.elapsedMilliseconds) + ' ms')
    Else
      exit;
  End Else Begin
    fBLog := TFileStream.Create(hFile);
    If fBLog.size <> 0 Then
      fBLog.position := fBLog.size
    Else
    Begin
      writeBLogHeader;
    End;
  End;
End;

Class Procedure TBinaryLogWriter.writeString(Const stream: TStream;
Const s: String);
Var
  b: Tbytes;
  size: DWORD;
Begin
  b := TENcoding.utf8.getBytes(s);

  size := length(b);
  stream.writeBuffer(size, 4);
  If size <> 0 Then
    stream.writeBuffer(b[0], size);
End;

Procedure TBinaryLogWriter.initKey;
Var
  b, outData: Tbytes;
Begin
  If fKeyInitialized Then
    exit;
  fKeyInitialized := true;

  // init aes key
  // 230 bytes
  b := [$8C, $53, $05, $53, $2D, $C2, $C1, $3C, $47, $B5, $E6, $4E, $50, $CC, $9F, $12, $3F, $F6, $2F, $6F, $7C, $15, $38, $2E, $42, $96, $38, $B5, $F9, $7B, $3A, $5B, $8F, $74, $AE, $33, $AE, $B7, $74, $C2
    , $79, $D6, $42, $57, $40, $3A, $33, $D0, $D5, $37, $87, $58, $DC, $65, $78, $E0, $7C, $BB, $90, $B3, $94, $1F, $9D, $51, $C0, $35, $B1, $24, $FD, $63, $4A, $AA, $DE, $A6, $45, $4D, $AD, $17, $12, $78, $B5
    , $77, $A4, $C6, $11, $CB, $78, $5E, $0D, $9F, $EA, $99, $34, $FC, $DB, $07, $87, $0C, $A3, $F8, $2D, $6C, $4B, $70, $4E, $74, $28, $ED, $28, $CD, $61, $A6, $78, $DD, $71, $9C, $45, $84, $AF, $40, $83, $24
    , $EF, $76, $B4, $14, $CA, $B1, $63, $1C, $A0, $B6, $D7, $F6, $5A, $A0, $A6, $6C, $4B, $61, $2F, $B4, $24, $0F, $8D, $7D, $0B, $A3, $49, $D3, $91, $F9, $DD, $1F, $78, $D2, $D4, $7C, $29, $A6, $F7, $BC, $E5
    , $5B, $85, $17, $82, $68, $12, $25, $00, $17, $6C, $E0, $16, $5D, $83, $E9, $6A, $27, $7B, $0C, $39, $4F, $FB, $96, $4C, $7D, $6D, $56, $62, $6E, $C6, $99, $EC, $EA, $9B, $07, $44, $DF, $45, $B3, $96, $DF
    , $21, $8D, $E1, $09, $C9, $87, $6F, $E1, $3A, $46, $FB, $52, $13, $33, $3B, $A7, $12, $9A, $AB, $56, $7F, $35, $14, $9E, $9D, $75];

  If maxCryptoHelper.Decryptor.decrypt(b, outData) Then
  Begin
    move(outData[0], fKey, 32);
  End Else Begin
    Raise Exception.Create('Corruption in ' + classname);
    halt;
  End;

  // burn sensible data after usage:
  FillChar(outData[0], length(outData), 0);

End;

Procedure TBinaryLogWriter.MoveAndZipBLog;
Var
  f1, f2: String;
  lKey: TSHA256Digest;
Begin
  f1 := TMaxLogAsyncWriter.fBLogDir + '.all.bLog';
  f2 := TMaxLogAsyncWriter.fBLogDir + '.all(' + formatDateTime('yyyy"-"mm"-"dd"_"hh";"nn";"ss"."zzz', now) + ').bLog';
  TFile.move(f1, f2);

  fOlderLogFiles.add(f2);
  If Not DisableOldLogPurging Then
    While fOlderLogFiles.Count > cMaxOldFileCount Do
    Begin
      // actually we will not delete the first log file, as it mostly contains some special start up informations.
      f1 := fOlderLogFiles[1];
      Try
        If TFile.Exists(f1) Then
          TFile.Delete(f1);
      Except
        // do nothing
      End;
      fOlderLogFiles.Delete(1);
    End;

  lKey := fKey;
  maxAsync.SimpleAsyncCall(
    Procedure
    Var
      repacker: TBlogArchiveRepacker;
    Begin
      repacker := TBlogArchiveRepacker.Create(f2, lKey, fLogPart);
      inc(fLogPart);
      Try
        repacker.execute;
      Finally
        repacker.free;
      End;
    End, 'repackOldArchive');

End;

{ TLogItem }

Class
  Function TLogItem.Create: TLogItem;
Begin
  result := Default (TLogItem);
  result.TimeStamp := AppStopWatch.elapsedMilliseconds;
  result.DateTime := now();

  result.ThreadId := GetCurrentThreadId();
  result.ThreadName := TmaxAsyncGlobal.GetThreadName(result.ThreadId);
End;

Function TLogItem.estimateInMemorySize: DWORD;
Begin
  result :=
    length(Msg) * 2 +
    length(Filename) * 2 +
    SizeOf(TimeStamp) +
    SizeOf(DateTime) +
    SizeOf(CreatePlainTextLogFile) +
    SizeOf(ThreadId) +
    length(ThreadName) * 2;
End;

{ TBlogArchiveRepacker }

Procedure TBlogArchiveRepacker.addRecompressSummary;
Var
  i64: Int64;
  dt: TDateTime;
  s: String;
Begin
  s := 'orgSize: ' + IntToStr(fOld.size) + ' b' + cr +
    'Size (MB): ' + fstr(fOld.size / mb) + cr + cr +
    'Part#: ' + IntToStr(fLogPart);

  i64 := AppStopWatch.elapsedMilliseconds;
  fChunk.writeBuffer(i64, SizeOf(Int64));
  dt := now;
  fChunk.writeBuffer(dt, SizeOf(TDateTime));

  TBinaryLogWriter.writeString(fChunk, classname);
  TBinaryLogWriter.writeString(fChunk, s);

  fChunk.writeBuffer(System.MainThreadID, SizeOf(DWORD));
  TBinaryLogWriter.writeString(fChunk, '');
End;

Constructor TBlogArchiveRepacker.Create(Const aFileName: String;
Const aKey: TSHA256Digest; aLogPart: integer);
Begin
  Inherited Create;
  fLogPart := aLogPart;
  fFileName := aFileName;
  fKey := aKey;
  fOld := TMemoryStream.Create;
  fNew := TMemoryStream.Create;
  fChunk := TMemoryStream.Create;
End;

Destructor TBlogArchiveRepacker.destroy;
Begin
  fNew.free;
  fOld.free;
  fChunk.free;
  Inherited;
End;

Procedure TBlogArchiveRepacker.execute;
Var
  version: DWORD;
  s1, s2: rawByteString;
  size: DWORD;
  c: TAESCFB;
  cm: TCompressMethod;
  b: byte;
Begin
  fStopWatch := TStopWatch.startNew;
  fOld.LoadFromFile(fFileName);

  // reserve some memory up front
  fNew.size := fOld.size;
  fChunk.size := fOld.size;

  // write header
  version := 4; // the one that has compression enabled
  fNew.writeBuffer(version, SizeOf(DWORD));
  fNew.writeBuffer(System.MainThreadID, SizeOf(DWORD));
  b := byte(cmSynLZ2);
  fNew.writeBuffer(b, 1);

  // skip the header in the old file
  fOld.position := SizeOf(DWORD) * 2;
  fOld.ReadBuffer(cm, 1);

  // now read all chunks and decrypt them
  // store them all in one big chunk
  While fOld.position + 4 < fOld.size Do
  Begin
    fOld.ReadBuffer(size, 4);
    // make sure we do not go out of the file...
    If fOld.position + size > fOld.size Then
      break;

    If size <> 0 Then
    Begin
      setLength(s2, size);
      fOld.ReadBuffer(s2[1], size);

      // decrypt the chunk
      c := TAESCFB.Create(fKey);
      s1 := c.DecryptPKCS7(s2, true);
      c.free;

      // decompress the chunk
      Case cm Of
        cmNone:
          s2 := s1;
        cmSynLZ1:
          Begin
            setLength(s2, SynLZdecompressdestlen(pAnsiChar(s1)));
            size := SynLZdecompress1(pAnsiChar(s1), length(s1), pAnsiChar(s2));
            setLength(s2, size);
          End;
        cmSynLZ2:
          Begin
            setLength(s2, SynLZdecompressdestlen(pAnsiChar(s1)));
            size := SynLZdecompress2(pAnsiChar(s1), length(s1), pAnsiChar(s2));
            setLength(s2, size);
          End;
      End;

      // write it to the one big chunk
      fChunk.writeBuffer(s2[1], length(s2));

      If fChunk.size > 50 * mb Then
        flushChunk;
    End;
  End;

  addRecompressSummary;
  flushChunk;

  fNew.size := fNew.position; // truncate
  If fNew.size < fOld.size Then
    fNew.SaveToFile(fFileName);
End;

Procedure TBlogArchiveRepacker.flushChunk;
Var
  s1, s2: rawByteString;
  size: DWORD;
  c: TAESCFB;
Begin
  // chunk contains now all the decrypted log items from the
  fChunk.size := fChunk.position; // trim it
  setLength(s1, fChunk.size);
  fChunk.position := 0;
  fChunk.ReadBuffer(s1[1], fChunk.size);
  fChunk.size := 0; // free memory

  // now compress
  setLength(s2, SynLZcompressdestlen(length(s1)));
  size := SynLZcompress2(pAnsiChar(s1), length(s1), pAnsiChar(s2));
  setLength(s2, size);

  // now encrypt
  c := TAESCFB.Create(fKey);
  s1 := c.encryptPKCS7(s2, true);
  c.free;

  size := length(s1);
  fNew.writeBuffer(size, 4);
  If size <> 0 Then
    fNew.writeBuffer(s1[1], size);
End;

{ TEnterLeaveLog }

Destructor TEnterLeaveLog.destroy;
Begin
  TMaxLogAsyncWriter.AddToLogFile('Leave ' + fDescription + getElapsedSufix,
    self.fLogFile,
    fCreatePlainTextLogFile);

  Inherited;
End;

Class Function TEnterLeaveLog.enter(Const aDescription,
  aLogFileName: String;
Const aFirstEntryOnlyExtraText: String = '';
aCreatePlainTextLogFile: boolean = true): iInterface;
Var
  l: TEnterLeaveLog;
  s: String;
Begin
  l := TEnterLeaveLog.Create;
  l.fLogFile := aLogFileName;
  l.fDescription := aDescription;
  l.fCreatePlainTextLogFile := aCreatePlainTextLogFile;
  l.fTimeStamp := AppWideStopWatch.AppStopWatch.elapsedMilliseconds;

  If (aLogFileName = '') OR (extractFileExt(aLogFileName) = '') Then
    l.fCreatePlainTextLogFile := false;

  s := 'Enter ' + l.fDescription;
  If aFirstEntryOnlyExtraText <> '' Then
    s := s + ' ' + aFirstEntryOnlyExtraText;

  TMaxLogAsyncWriter.AddToLogFile(s,
    l.fLogFile,
    l.fCreatePlainTextLogFile);

  result := l;
End;

Function TEnterLeaveLog.getElapsedSufix: String;
Begin
  result := format(' [%d ms]', [AppStopWatch.elapsedMilliseconds - self.fTimeStamp]);
End;

Initialization

  {$IFDEF madExcept}
  HideLeak ('TBinaryLogWriter.Create');
  // this is a singelton and is destroyed in the finalisation section
  {$ENDIF}


Finalization

If TMaxLogAsyncWriter.fLogAsyncWriter <> Nil Then
Begin
  TMaxLogAsyncWriter.fLogAsyncWriter.free;
  TMaxLogAsyncWriter.fLogAsyncWriter := Nil;
End;

End.
