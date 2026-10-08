Unit MaxLogic.FileMutex;

Interface

Uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  sysUtils, classes;

Type
  /// <summary>
  /// you can use THandleStream to read/write from/to the underlying file on Windows.
  /// On POSIX the handle is read-only when the file is not writable for us; the lock never needs write access.
  /// </summary>
  TFileMutex = Class
  Private
    FFileHandle: THandle;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Function Open(Const aFileName: String; aTimeOutInMs: Integer): Boolean;
    Procedure Close;

    Property Handle: THandle Read FFileHandle;
  End;

Implementation

Uses
  {$IFDEF POSIX}
  Posix.Base, Posix.Errno, Posix.Fcntl,
  {$ENDIF}
  ioUtils, diagnostics, math;

{$IFDEF POSIX}
Const
  // flock() is the only lock on POSIX: it is what legacy tdbengine_7 uses, it excludes other open file
  // descriptions (so threads of one process exclude each other) and the kernel drops it on process death.
  // No fcntl share-mode lock on top: drvfs/9p and NFS emulate flock with the same lock as fcntl, so a
  // second lock on the same descriptor would conflict with itself there and never be acquired.
  cLockExclusive = 2;
  cLockNonBlocking = 4;
  cLockUnlock = 8;

Function Cflock(fd: Integer; operation: Integer): Integer; Cdecl; External libc Name _PU + 'flock';

Function TryLockHandle(aHandle: THandle): Boolean;
Begin
  Result := Cflock(Integer(aHandle), cLockExclusive Or cLockNonBlocking) = 0;
End;

Procedure UnlockHandle(aHandle: THandle);
Begin
  Cflock(Integer(aHandle), cLockUnlock);
End;

// Opens the lock file without ever truncating it; every descriptor is close-on-exec so child processes
// never inherit the lock. O_CREAT is only used for a file that does not exist yet.
Function OpenLockFile(Const aFileName: String): THandle;
Const
  {$IF Defined(LINUX) or Defined(ANDROID)}
  cOpenCloseOnExec = $80000; // O_CLOEXEC; the Delphi Linux RTL does not declare it
  {$ELSEIF Declared(O_CLOEXEC)}
  cOpenCloseOnExec = O_CLOEXEC;
  {$ELSE}
  {$MESSAGE FATAL 'MaxLogic.FileMutex: O_CLOEXEC is unknown for this POSIX target'}
  {$IFEND}
  cCreateMode = 438; // 0666 octal: the umask decides, as for legacy tdbengine_7
Var
  lAttempt, lFd: Integer;
  lPath: MarshaledAString;
  M: TMarshaller;
Begin
  Result := INVALID_HANDLE_VALUE;
  lPath := M.AsAnsi(aFileName, CP_UTF8).ToPointer;
  For lAttempt := 1 To 2 Do
  Begin
    // 1. Read-write first: NFS emulates flock with fcntl locks, which need a descriptor open for writing.
    lFd := __open(lPath, O_RDWR Or cOpenCloseOnExec, 0);
    If lFd <> -1 Then
      Exit(THandle(lFd));
    Case errno Of
      EACCES, EROFS:
        Begin
          // 2. Not writable for us (another user's 0644 file, read-only mount). flock works on a read-only
          // descriptor; legacy tdbengine_7 opens existing semaphore files read-only too.
          lFd := __open(lPath, O_RDONLY Or cOpenCloseOnExec, 0);
          If lFd <> -1 Then
            Result := THandle(lFd);
          Exit;
        End;
      ENOENT:
        Begin
          // 3. Missing: create it exclusively. O_EXCL keeps O_CREAT away from existing files, which
          // fs.protected_regular rejects for another user's file in a sticky directory.
          lFd := __open(lPath, O_RDWR Or O_CREAT Or O_EXCL Or cOpenCloseOnExec, cCreateMode);
          If lFd <> -1 Then
            Exit(THandle(lFd));
          // EEXIST: another process created it first; start over once with step 1. Anything else fails.
          If errno <> EEXIST Then
            Exit;
        End;
    Else
      Exit;
    End;
  End;
End;
{$ENDIF}

Function TFileMutex.Open(Const aFileName: String; aTimeOutInMs: Integer): Boolean;
Var
  st: TStopWatch;
  sleepMs: Integer;
Begin
  Close;
  st := TStopWatch.startNew;
  Repeat
    {$IFDEF POSIX}
    FFileHandle := OpenLockFile(aFileName);
    If (FFileHandle <> INVALID_HANDLE_VALUE) And Not TryLockHandle(FFileHandle) Then
    Begin
      FileClose(FFileHandle);
      FFileHandle := INVALID_HANDLE_VALUE;
    End;
    {$ELSE}
    If TFile.Exists(aFileName) Then
      FFileHandle := FileOpen(aFileName, fmOpenReadWrite OR fmShareExclusive)
    else begin
      // chmod is as octal value
      FFileHandle := FileCreate(aFileName, fmOpenReadWrite Or fmShareExclusive, 420);
    end;
    {$ENDIF}

    // we do not spin forever, the cpu would not like that. Rather go to sleep after some time
    If (FFileHandle = INVALID_HANDLE_VALUE) And (st.ElapsedMilliseconds < aTimeOutInMs) And (st.ElapsedMilliseconds > 30) Then
    Begin
      sleepMs := min(5, (aTimeOutInMs - st.ElapsedMilliseconds) - 1);
      If sleepMs > 0 Then
        sleep(sleepMs);
    End;

    until (FFileHandle <> INVALID_HANDLE_VALUE) or (st.ElapsedMilliseconds >= aTimeOutInMs);

  Result := (FFileHandle <> INVALID_HANDLE_VALUE);
End;

Destructor TFileMutex.Destroy;
Begin
  Close;
  Inherited;
End;

Constructor TFileMutex.Create;
Begin
  Inherited Create;
  FFileHandle:= INVALID_HANDLE_VALUE;
End;

Procedure TFileMutex.Close;
Begin
  if fFileHandle <> INVALID_HANDLE_VALUE Then
  Begin
    {$IFDEF POSIX}
    UnlockHandle(FFileHandle);
    {$ENDIF}
    FileClose(FFileHandle);
    FFileHandle := INVALID_HANDLE_VALUE;
  End;
End;

End.
