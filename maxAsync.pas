Unit maxAsync;

{ .$DEFINE DEBUG_MAXASYNC }

{ .$DEFINE ASYNC_LOOP_USES_SINGLETHREAD }// for debuging

{ SOME OTHER USEFUL CONDITIONALS:
  NeverUninstall;NeverSleepOnMMThreadContention;UseSwitchToThread;UseReleaseStack   v }

// forward unhandled exceptions to main thread?
{$IFDEF MSWINDOWS}
{$DEFINE FORWARD_EXCEPTIONS_TO_MAIN_THREAD}
{$ENDIF}

// TThread.ForceQueueis not avaiable in XE8.. not sure when it was introduced?
{$IF CompilerVersion <= 29.0}
{$DEFINE ForceQueueNotAvailable}
{$ENDIF}


{
  Version: 2.30

  History
  2023-06-28: moved iSignal to its own unit: maxSignal
  2022-01-30: Linux compatibility
  2021-06-12: TAsyncTimer implementation
  2020-02-27: iAsync has now access to the thread priority
  2019-10-05: removed tmonitor in favor of TFixedRtlCriticalSection
  2018-11-26: added thread safe messageDlg method
  2018-10-06: fix small memory leach when closing the application
  2018-10-30: the SimpleAsyncCall has now a syncedAfterDone parameter that is just a proc you can pass that will be called after the thread went of. Not it will happen even if the initial thread proc raises an exception
  2018-10-29: you can now retrive the task name using a threadId: TmaxAsyncGlobal.GetThreadName
  2018-04-30" added iUserData and TUserData
  2018-02-16: TasyncLoop: some class methods added
  2017-06-18: better compatibility with win64
  2017-05-16: bugfix: when the application terminated and there were still some threads running, it could happen that the global thread list was freed but the thread triesd to access it anyway.
  2017-04-28: fix: the global thread wait list was destroyed before all threads finished working. So when they tried to add themselves to the global wait list, it failed. now they check if the global wait list is still alive
  2017-01-20: when reusing a thread the default values are now reseted in the TThreadData.SetDefaultValues method
  2016-12-10: removed TmaxPool added TAsyncCollectionProcessor
  2016-12-08: added tAsyncLoop TmaxAsyncGlobal
  2016-09-11: added TSafeValue which encapsulates the value, to allow read/write in a thread safe manner
  2014-11-10: Exception handling: better synchronization
  2014-08-10: minor refactorings
  2014-07-07: fixed iAsync.MsgWaitFor. There was a problem while handing exceptions and waiting without blocking the gui
  2014-05-27: added a timeout for the MsgWaitForSingleObject function
  2014-05-25: added AutoReset as a param to creating TSignal.
  2014-05-24: some more functions for the iSignal
  2013-10-26: Removed the MasterThread, reducing managing overheat.
  2013-06-25:
  * Exceptions are now passed over to the main thread
  * fixed minor memory leaks
  2013-05-25: added a function Pool.shutdown, in addition, the master pool thread is created only if needed.
  2013-05-24: ensure compatibility with delphi 2010
  2013-05-05:
  Fix: the imaxPool.msgWaitforAll and imaxPool.WaitForAll had an small error. it could happen, that the task was already deleted from the list, during it was accessed. Now this can no more happen, as we do clone the list, just before we do wait. The funktion takes also into account newly created tasks.
  2013-04-26:
  - iAsync.Restart changed to iAsync.WakeUp, this one do no longer wait for the previos call to be finished, it just sets the signal to re awake the procedure
  2013-04-25:
  * fix in the function TmaxPool.WaitFor(var events: array of THandle; Milliseconds: dword = infinite; DoProcessMessages: boolean = false): boolean; method, it was possible to reduce teh TimeOut below 0 }
Interface

Uses
  {$IFDEF MSWINDOWS}
  Windows, Forms, Messages, System.UITypes, dialogs,
  {$ENDIF}
  {$IFDEF LINUX}
  posix.sysTypes,
  posix.pthread,
  {$ENDIF}
  Classes, SysUtils, SyncObjs,
  generics.defaults, generics.Collections,
  maxSignal;

Type
  {$IFNDEF MSWINDOWS}
  dword = Cardinal;
  {$ENDIF}
  // forward declaration
  TmaxThread = Class;

  TSignal = maxSignal.TSignal;
  iSignal = maxSignal.iSignal;

  (* *
    iAsync = iInterface;
    TAsyncLoop = class;
    TAsyncCollectionProcessor = class;
    iSignal = iInterface;
    {$IFDEF MSWINDOWS}
    TWaiter = class;
    {$ENDIF}
    iCriticalSection = iInterface;
    // do not use directly:
    TmaxThread = class;
    iThreadData = iInterface;
    TThreadData = class;
    TmaxAsyncGlobal = class;
    tInterfacedCriticalSection = class;
    * *)

  iAsync = Interface
    ['{41564891-4A25-464E-B756-B6EFD50063E4}']
    Procedure WaitFor;
    {$IFDEF MsWindows}
    Procedure MsgWaitFor;
    {$ENDIF}
    Function Finished: boolean;
    // After execution, the thread is still available, so you do not need to re-create it. The easiest way is just to WakeUp it. You can run it either with a new procedure to be called or with the same as before
    Procedure WakeUp(aProc: TThreadProcedure; Const TaskName: String); Overload;
    Procedure WakeUp; Overload;

    {$IFDEF MSWINDOWS}
    Function GetThreadPriority: TThreadPriority;
    Procedure SetThreadPriority(Const Value: TThreadPriority);
    Property Priority: TThreadPriority Read GetThreadPriority Write SetThreadPriority;
    {$ENDIF}
  End;

  iThreadData = Interface
    ['{CC30782E-30D8-48E4-B3C6-33383C37C3BA}']
    Function GetFinished: boolean;
    Function GetFullThreadId: String;
    Procedure SetThreadId(const aId: TThreadId);
    Function GetWaiting: boolean;
    Procedure SetFinished(Const Value: boolean);
    Procedure SetWaiting(Const Value: boolean);
    Procedure SetTerminated(Const Value: boolean);
    Function GetTerminated: boolean;
    Function GetReadySignal: iSignal;
    Procedure SetThread(Const Value: TmaxThread);
    Function GetThread: TmaxThread;

    Function GetWakeSignal: iSignal;
    Function GetKeepAlive: boolean;
    Function GetProc: TThreadProcedure;
    Function GetTaskName: String;
    Procedure SetKeepAlive(Const Value: boolean);
    Procedure SetThreadToTerminated;
    Procedure SetProc(Const Value: TThreadProcedure);
    Procedure SetTaskName(Const Value: String);
    Function GetStartSignal: iSignal;

    Procedure SetDefaultValues;

    Property WakeUpSignal: iSignal Read GetWakeSignal;
    Property Proc: TThreadProcedure Read GetProc Write SetProc;
    Property KeepAlive: boolean Read GetKeepAlive Write SetKeepAlive;
    Property TaskName: String Read GetTaskName Write SetTaskName;

    Property StartSignal: iSignal Read GetStartSignal;
    Property Finished: boolean Read GetFinished Write SetFinished;
    Property Waiting: boolean Read GetWaiting Write SetWaiting;
    Property Terminated: boolean Read GetTerminated Write SetTerminated;
    Property ReadySignal: iSignal Read GetReadySignal;

    // direct access
    Property Thread: TmaxThread Read GetThread Write SetThread;
  End;

  TDummy = Record
    Case byte Of
      0:
        (byteArray: Array [0 .. 95] Of byte;);
      1:
        (integerArray: Array [0 .. 23] Of integer;);
      2:
        (int64Array: Array [0 .. 11] Of int64;);
      3:
        (FloatArray: Array [0 .. 11] Of double;);
  End;

  { http://delphitools.info/2011/11/30/fixing-tcriticalsection/
    TFixedCriticalSection (along with TMonitor*) suffers from a severe design flaw in which entering/leaving different TFixedCriticalSection instances can end up serializing your threads, and the whole can even end up performing worse than if your threads had been serialized.
    This is because it’s a small, dynamically allocated object, so several TFixedCriticalSection instances can end up in the same CPU cache line, and when that happens, you’ll have cache conflicts aplenty between the cores running the threads.
    How severe can that be? Well, it depends on how many cores you have, but the more cores you have, the more severe it can get. On a quad core, a bad case of contention can easily result in a 200% slowdown on top of the serialization. And it won’t always be reproducible, since it’s related to dynamic memory allocation.
    There is thankfully a simple fix for that, use TFixedCriticalSection: }

  TFixedCriticalSection = Class(TCriticalSection)
  Public
    Dummy: TDummy
    End;

    // prefered is still the TRTLCriticalSection, or better, the TFixedRtlCriticalSection
    {$IFDEF MSWINDOWS}
    TRTLCriticalSectionHelper = Record Helper
    For TRTLCriticalSection
      Public
      Class
    Function Create: TRTLCriticalSection;
    Static;
  Public
    Procedure Enter; Inline;
    Procedure Leave; Inline;
    // just an alias
    Procedure Exit; Inline;
    Procedure free; Inline;
  End;
  {$ENDIF}
  {$IFDEF MSWINDOWS}

  TFixedRtlCriticalSection = Record
  Public
    // you can access it directly if needed
    CriticalSection: TRTLCriticalSection;
    // see TFixedCriticalSection for details
    Dummy: TDummy;

    Class Function Create: TFixedRtlCriticalSection; Static; Inline;

    Procedure Enter; Inline;
    Procedure Leave; Inline;
    // just an alias
    Procedure Exit; Inline;
    Procedure free; Inline;
  End;
  {$ENDIF}

  TObjectWithCriticalSection = Class
  Protected
    {$IFDEF MSWINDOWS}
    fCriticalSection: TFixedRtlCriticalSection;
    {$ELSE}
    fCriticalSection: TFixedCriticalSection;
    {$ENDIF}
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure lock; Inline;
    Procedure unLock; Inline;
  End;

  iCriticalSection = Interface
    ['{C906B8AD-2DCE-4E8B-9047-1B49FDCA5A98}']
    Procedure Enter;
    Procedure Leave;
  End;

  tInterfacedCriticalSection = Class(tInterfacedObject, iCriticalSection)
  Private
    fSec: TFixedCriticalSection;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure Enter;
    Procedure Leave;
  End;

  TThreadData = Class(tInterfacedObject, iThreadData)
  Private
    fFinished: boolean;
    fWaiting: boolean;
    fThreadId: TThreadId;
    {$IFDEF MSWINDOWS}
    fThreadHandle: THandle;
    {$ENDIF}
    fThread: TmaxThread;
    fReadySignal: iSignal;
    FWakeSignal: iSignal;
    fTaskName: String;
    fKeepAlive: boolean;
    FProc: TThreadProcedure;
    FTerminated: boolean;
    fStartSignal: iSignal;

    Function GetThread: TmaxThread;
    Function GetFullThreadId: String;
    Procedure SetThreadId(const aId: TThreadId);
    Procedure SetThread(Const Value: TmaxThread);
    Function GetFinished: boolean;
    Function GetWaiting: boolean;
    Procedure SetFinished(Const Value: boolean);
    Procedure SetWaiting(Const Value: boolean);
    Function GetReadySignal: iSignal;
    Function GetWakeSignal: iSignal;

    Function GetKeepAlive: boolean;
    Function GetProc: TThreadProcedure;
    Function GetTaskName: String;
    Procedure SetKeepAlive(Const Value: boolean);
    Procedure SetThreadToTerminated;
    Procedure SetProc(Const Value: TThreadProcedure);
    Procedure SetTaskName(Const Value: String);

    Procedure SetTerminated(Const Value: boolean);
    Function GetTerminated: boolean;
    Function GetStartSignal: iSignal;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure SetDefaultValues;

    Property Thread: TmaxThread Read GetThread Write SetThread;
    Property StartSignal: iSignal Read GetStartSignal;
    Property Finished: boolean Read GetFinished Write SetFinished;
    Property Waiting: boolean Read GetWaiting Write SetWaiting;
    Property Terminated: boolean Read GetTerminated Write SetTerminated;
    Property ReadySignal: iSignal Read GetReadySignal;
    Property WakeUpSignal: iSignal Read GetWakeSignal;

    Property Proc: TThreadProcedure Read GetProc Write SetProc;
    Property KeepAlive: boolean Read GetKeepAlive Write SetKeepAlive;
    Property TaskName: String Read GetTaskName Write SetTaskName;
  End;

  iAsyncIntern = Interface
    ['{6C221063-5386-4C57-A645-B6E63DB04F1C}']
    Function GetThreadData: iThreadData;
  End;

  TmaxAsync = Class(tInterfacedObject, iAsync, iAsyncIntern)
  Private
    fThreadData: iThreadData;
    Function GetThreadData: iThreadData;
    {$IFDEF MSWINDOWS}
    Function GetThreadPriority: TThreadPriority;
    Procedure SetThreadPriority(Const Value: TThreadPriority);
    {$ENDIF}
  Public
    Constructor Create;
    Destructor Destroy; Override;
    {$IFDEF MsWindows}
    Procedure MsgWaitFor;
    {$ENDIF}
    Procedure WaitFor;
    Function Finished: boolean;
    Procedure WakeUp(aProc: TThreadProcedure; Const TaskName: String); Overload;
    Procedure WakeUp; Overload;
    {$IFDEF MSWINDOWS}
    Property Priority: TThreadPriority Read GetThreadPriority Write SetThreadPriority;
    {$ENDIF}
  End;

  TmaxThread = Class(TThread)
  Private
    fThreadData: iThreadData;
    fName: String;

    Procedure HandleException;
    Class Procedure DoSync(aProc: TThreadProcedure; DoWait: boolean);
  Protected
    Procedure Execute; Override;
  Public
    Destructor Destroy; Override;
  End;

  TAsyncEnumProc = reference To Procedure(CurIndex: integer);
  TAsyncEnumProcWithCancel = reference To Procedure(CurIndex: integer; Var aCancel: boolean);

  // iterates through a loop. It is a one time shoot, create it, run it and free it afterwards
  TAsyncLoop = Class
  Private
    fReadySignal: iSignal;
    fMax: integer;
    fCurIndex: integer;
    FProc: TAsyncEnumProc;
    FOnFinished: TThreadProcedure;
    FSimultanousThreadCount: integer;
    fActivThreadCount: integer;
    fThreads: Array Of iAsync;

    Procedure SetOnFinished(Const Value: TThreadProcedure);
    Procedure SetSimultanousThreadCount(Const Value: integer);
    Procedure asyncLoopIteration;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure Execute(aMin, aMax: integer; aProc: TAsyncEnumProc);
    Procedure WaitFor;
    Procedure Cancel;

    // simplyfied call.  If ThreadCount is 0 (or less), then the default value will be used
    Class Procedure RunAndWait(Const aMin, aMax: integer; aProc: TAsyncEnumProcWithCancel; ThreadCount: integer = 0);
    Class Procedure Run(Const aMin, aMax: integer; aProc: TAsyncEnumProcWithCancel; OnDone: TProc; ThreadCount: integer = 0);

    // set before running the loop
    // finished will be executed in the thread context of the last active thread, so be aware to perform a sync if needed
    Property OnFinished: TThreadProcedure Read FOnFinished Write SetOnFinished;
    // default is the number of CPU cores, *4 - 2 seting it after running the loop will have no effects
    Property SimultanousThreadCount: integer Read FSimultanousThreadCount Write SetSimultanousThreadCount;
  End;

  TAsyncCollectionProcessorProc<T> = reference To Procedure(Const item: T);
  // this will work in paralel on the items in the collection
TAsyncCollectionProcessor < T >= Class
  Private Type
  TThreadHolder = Record
  Id: int64;
async:
iAsync;
End;
Private
  fReadySignal: iSignal;
  fItems: TQueue<T>;
  FSimultanousThreadCount: integer;
  FOnFinished: TThreadProcedure;
  FProc: TAsyncCollectionProcessorProc<T>;

  fThreads: TList<TThreadHolder>;
  fNextThreadId: int64;
  {$IFDEF MSWINDOWS}
  fCriticalSection: TFixedRtlCriticalSection;
  {$ELSE}
  fCriticalSection: TFixedCriticalSection;
  {$ENDIF}
  Procedure SetOnFinished(Const Value: TThreadProcedure);
  Procedure SetProc(Const Value: TAsyncCollectionProcessorProc<T>);
  Procedure SetSimultanousThreadCount(Const Value: integer);
  Procedure RestartThreads;
  Function CreateThreadHolder: TThreadHolder;
  Procedure AsyncProcessItem(const ThreadId: int64);
Public
  Constructor Create;
  Destructor Destroy;
  Override;

  Procedure Add(Const item: T);
  Overload;
  Procedure Add(Const Items: TArray<T>);
  Overload;
  Procedure Add(Const Items: TList<T>);
  Overload;
  Procedure ClearItems;

  Procedure WaitFor;

  // set before running the loop
  Property Proc: TAsyncCollectionProcessorProc<T> Read FProc Write SetProc;
  // finished will be executed in the thread context of the last active thread, so be aware to perform a sync if needed
  // NOTE: this callback is called after the last item is processed. So if you do not stop the processor, and add more items later, this event may be triggered multiple times
  Property OnFinished: TThreadProcedure Read FOnFinished Write SetOnFinished;
  // default is the number of CPU cores, *4 - 2 . if the system is already running, you can only increase the number of threads used, a decrease will be applied only after a restart.
  Property SimultanousThreadCount: integer Read FSimultanousThreadCount Write SetSimultanousThreadCount;
  End;

  TFindInSortedList < T >= Class
    Private
    fList: TList<T>;
  fComparer:
    iComparer<T>;
Public
  Constructor Create(aList: TList<T>; aComparer: iComparer<T>);
  Function Find(Const Value: T; Out Index: integer): boolean;
  End;

  TSafeValue<T: Record > = Record
    Private
    fValue: T;
  fSec: iCriticalSection;

  Function GetValue: T;
  Procedure Setvalue(Const aValue: T);
  Procedure EnsureInit;
  Inline;
Public
  Property Value: T Read GetValue Write Setvalue;
  Class
  Function New: TSafeValue<T>;
  Static;
  End;

  iUserData < T >= Interface
    ['{77527854-F405-4CBD-8FAA-B2ED87C6E2E1}']
  Procedure Setvalue(Const aValue: T);
  Function GetValue: T;
  Property Value: T Read GetValue Write Setvalue;
  End;

  // a very small lock artifact, use carefuly
  TLockFreeLock = Record
    Private
    fLocked: integer;
Public
  Procedure lock;
  Inline;
  Procedure unLock;
  Inline;
  End;

  TUserData < T >= Class(tInterfacedObject, iUserData<T>)
    Private
    fValue: T;
  fLock: TLockFreeLock;
  Procedure Setvalue(Const aValue: T);
  Function GetValue: T;
Public
  Constructor Create(Const aValue: T);
  Property Value: T Read GetValue Write Setvalue;
  End;

  TmaxAsyncGlobal = Class
    Strict Private Class

Var
  FNumberOfProcessors: dword;
  fWaitingThreadDataList: TList<iThreadData>;
  fAllThreads: TList<TThreadData>;
  {$IFDEF MSWINDOWS}
  fCS: TRTLCriticalSection;
  {$ELSE}
  fCS: TCriticalSection;
  {$ENDIF}
  fGlobalThreadListDestroyed: boolean;
Private
  Class Constructor CreateClass;
  Class Destructor Destroyclass;
  Class
  Procedure ReleaseThreadData(Const aThreadData: iThreadData);
  Class
  Function GetThreadData: iThreadData;
  Class
  Procedure AddToWaiting(ThreadData: iThreadData);

  Class
  Procedure addToAllThreadList(aData: TThreadData);
  Class
  Procedure removeFromAllThreadList(aData: TThreadData);
Public
  Class Property NumberOfProcessors: dword Read FNumberOfProcessors;
  Class
  Function GetThreadName(const aThreadId: TThreadId): String;
  End;

  {$IFDEF MSWINDOWS}
  // helps to wait for multiple signas or multiple async Items
  TWaiter = Class
    Public
  // returns false if the timeout is expired, otherwise true
  // ProcessMessages will be set to false if we are not in the main VCL Thread
    Class Function WaitFor(Const async: TArray<iAsync>; Milliseconds: dword = infinite; DoProcessMessages: boolean = false): boolean;
  Overload;
  Class Function WaitFor(Const Signals: TArray<iSignal>; Milliseconds: dword = infinite; DoProcessMessages: boolean = false): boolean;
  Overload;
  Class Function WaitFor(Const aEvents: TArray<TEvent>; Milliseconds: dword = infinite; DoProcessMessages: boolean = false): boolean;
  Overload;
  End;
  {$ENDIF}
  IReaderWriterLock = Interface
    ['{044BC96C-BBD4-4B75-9487-048257D27679}']
  Procedure BeginRead;
  Procedure EndRead;
  Procedure BeginWrite;
  Procedure EndWrite;
  End;

  TAtomic = Record
  /// <summary>
  /// Assures {$A8} or {$A+} state
  /// </summary>
  /// <seealso href="https://stackoverflow.com/questions/829235/ifopt-a4">
  /// {$IFOPT A4}?
  /// </seealso>
    Class Constructor Create;
  End;

  TAsyncTimer = Class
    Private
    fAsync: iAsync;
  fSignal: iSignal;
  FProc: TProc;
  FEnabled: boolean;
  FInterval: integer;
  FTerminated: boolean;
  Procedure asyncLoop;
  Procedure SetEnabled(Const Value: boolean);
  Procedure SetInterval(Const Value: integer);
Public
  Constructor Create(aProc: TProc; aInterval: integer; aEnabled: boolean = True);
  Destructor Destroy;
  Override;
  Property Enabled: boolean Read FEnabled Write SetEnabled;
  Property Interval: integer Read FInterval Write SetInterval;
  End;

  Function SimpleAsyncCall(aProc: TThreadProcedure;
    Const TaskName: String = '';
    SyncedAfterDone: TThreadProcedure = Nil
    {$IFDEF MSWINDOWS}
    ; aThreadPriority: TThreadPriority = tpNormal
    {$ENDIF}
    ): iAsync;

  { WARNING:
    The sync code can only be executed when the main thread reaches a "safe" state, which, in a typical VCL application, is the message loop.
    Now imagine, the main thread is waiting... in that case it will never  execute the sync code.
    The main thread will wait for the thread to finish and the thread will wait for the main thread to sync. }
  Procedure SyncVCLCall(aProc: TThreadProcedure; DoWait: boolean = false);

  // this function will tell us if we are in the main thread or not
  Function InsideMainThread: boolean;
  inline;

  {$IFDEF MSWINDOWS}
  Procedure MsgWaitForSingleObject(Handle: THandle; TimeOut: dword = infinite);
  inline;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Function MessageDlg(Const Msg: String; DlgType: TMsgDlgType;
    Buttons: TMsgDlgButtons; HelpCtx: Longint): integer;
  Function MessageDlgThreadSafe(Const Msg: String; DlgType: TMsgDlgType;
    Buttons: TMsgDlgButtons; HelpCtx: Longint): integer;
  {$ENDIF}

  // how to kill a thread:
  // TerminateThread(fThread.Handle, 1);
Implementation

Uses
  {$IFDEF madExcept}madExcept, {$ENDIF}
  {$IFDEF DEBUG_MAXASYNC}ClipBrd, {$ENDIF}
  {$IFDEF MSWINDOWS}
  mmSystem, ActiveX,
  {$ENDIF}
  math, Diagnostics;

{$IFDEF MSWINDOWS}


Function MessageDlg(Const Msg: String; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): integer;
Begin
  result := MessageDlgThreadSafe(Msg, DlgType, Buttons, HelpCtx)
End;
{$ENDIF}

{$IFDEF MSWINDOWS}


Function MessageDlgThreadSafe(Const Msg: String; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): integer;
Var
  lResult: integer;
Begin
  If InsideMainThread Then
    result := dialogs.MessageDlg(Msg, DlgType, Buttons, HelpCtx)
  Else
  Begin
    lResult := 0;
    SyncVCLCall(
        Procedure
      Begin
        lResult := dialogs.MessageDlg(Msg, DlgType, Buttons, HelpCtx);
      End, True);
    result := lResult;
  End;
End;
{$ENDIF}


Function SimpleAsyncCall(aProc: TThreadProcedure; Const TaskName: String = ''; SyncedAfterDone: TThreadProcedure = Nil
{$IFDEF MSWINDOWS}
  ; aThreadPriority: TThreadPriority = tpNormal
{$ENDIF}
): iAsync;

  Function merge: TThreadProcedure;
  Begin
    result := (
      Procedure
      Begin
        Try
          aProc();
        Finally
          SyncVCLCall(
              Procedure
            Begin
              SyncedAfterDone();
            End, false);
        End;
      End);
  End;

Var
  async: TmaxAsync;
Begin
  async := TmaxAsync.Create;
  {$IFDEF MSWINDOWS}
  async.Priority := aThreadPriority;
  {$ENDIF}
  result := async;
  If Not assigned(SyncedAfterDone) Then
    async.fThreadData.Proc := aProc
  Else
    async.fThreadData.Proc := merge();

  async.fThreadData.TaskName := TaskName;
  async.fThreadData.StartSignal.SetSignaled;
End;

Destructor TmaxThread.Destroy;
Begin
  fThreadData.Waiting := false;
  fThreadData.Terminated := True;
  fThreadData.Finished := True;
  fThreadData.ReadySignal.SetSignaled;
  fThreadData := NIL;
  Inherited;
End;

Class Procedure TmaxThread.DoSync(aProc: TThreadProcedure; DoWait: boolean);
Var
  Signal: iSignal;
  MyProc: TThreadProcedure;
Begin
  If InsideMainThread Then
  Begin
    If DoWait Then
      aProc()
    Else begin
      {$IFDEF ForceQueueNotAvailable}
      TThread.Queue(Nil, aProc);
      {$ELSE}
      TThread.ForceQueue(Nil, aProc);
      {$ENDIF}
    end;

  End
  Else If Not DoWait Then
    {$IFDEF ForceQueueNotAvailable}
    TThread.Queue(Nil, aProc)
    {$ELSE}
    TThread.ForceQueue(Nil, aProc)
    {$ENDIF}
  Else
  Begin
    Signal := TSignal.Create;
    Signal.SetNonSignaled;

    MyProc := Procedure
      Begin
        Try
          aProc();
        Finally
          // prevent a dead lock in case of a exception
          Signal.SetSignaled;
        End;
      End;

    {$IFDEF ForceQueueNotAvailable}
    TThread.Queue(Nil, MyProc);
    {$ELSE}
    TThread.ForceQueue(Nil, MyProc);
    {$ENDIF}
    Signal.WaitForSignaled;
    Signal := NIL;
  End;
End;

Procedure TmaxThread.Execute;
Var
  CoInitialized: boolean;
  sName: String;
Begin
  fThreadData.StartSignal.WaitForSignaled;

  {$IFDEF MSWINDOWS}
  CoInitialized := (CoInitialize(Nil) In [S_OK, S_FALSE]);
  {$ENDIF}
  fThreadData.SetThreadId(
  {$IFDEF MSWINDOWS}
  GetCurrentThreadId
  {$ELSE}
  TThread.CurrentThread.ThreadId
  {$ENDIF}
  );
  {$IFDEF DEBUG_MAXASYNC}
  OutputDebugString(pwidechar('maxAsync: starting Thread:' + fThreadData.GetFullThreadId));
  {$ENDIF}
  Try
    Repeat
      Repeat
        If Terminated Then
          break;

        {$IFDEF DEBUG_MAXASYNC}
        OutputDebugString(pwidechar('maxAsync: running loop Thread:' + fThreadData.GetFullThreadId));
        {$ENDIF}
        fThreadData.ReadySignal.SetNonSignaled;
        fThreadData.WakeUpSignal.SetNonSignaled;

        fThreadData.Waiting := false;
        fThreadData.Finished := false;

        sName := ClassName + '.' + fThreadData.TaskName;
        fName := sName;
        {$IFDEF DEBUG_MAXASYNC}
        OutputDebugString(pwidechar('maxAsync: naming Thread:' + fThreadData.GetFullThreadId + ' >> ' + sName));
        {$ENDIF}
        NameThreadForDebugging(ansiString(sName));

        {$IFDEF madExcept}
        madExcept.NameThread(GetCurrentThreadId, sName);
        {$ENDIF}
        If fThreadData.Proc <> Nil Then
        Begin
          Try
            fThreadData.Proc();
          Except
            HandleException;
          End;
        End
        Else
        Begin
          {$IFDEF DEBUG_MAXASYNC}
          OutputDebugString(pwidechar('maxAsync: no proc to run for Thread:' + fThreadData.GetFullThreadId));
          {$ENDIF}
        End;

        fThreadData.Finished := True;
        fThreadData.ReadySignal.SetSignaled;

        If (Not fThreadData.KeepAlive) Or Terminated Then
          break;

        fThreadData.Waiting := True;

        {$IFDEF DEBUG_MAXASYNC}
        OutputDebugString(pwidechar('maxAsync: before wait for awake signal Thread:' + fThreadData.GetFullThreadId));
        {$ENDIF}
        fThreadData.WakeUpSignal.WaitForSignaled;
        {$IFDEF DEBUG_MAXASYNC}
        OutputDebugString(pwidechar('maxAsync: after wait for awake signal Thread:' + fThreadData.GetFullThreadId));
        {$ENDIF}
      Until Terminated Or (Not fThreadData.KeepAlive);

      If Not Terminated Then
      Begin
        fThreadData.StartSignal.SetNonSignaled;
        TmaxAsyncGlobal.AddToWaiting(fThreadData);
        fThreadData.StartSignal.WaitForSignaled;
      End;

    Until Terminated;

  Finally
    {$IFDEF DEBUG_MAXASYNC}
    OutputDebugString(pwidechar('maxAsync: exit Thread:' + fThreadData.GetFullThreadId));
    {$ENDIF}
    fThreadData.Waiting := false;
    fThreadData.Finished := True;
    fThreadData.Terminated := True;
    fThreadData.ReadySignal.SetSignaled;

    {$IFDEF MSWINDOWS}
    If CoInitialized Then
      CoUninitialize;
    {$ENDIF}
  End;
End;

Procedure TmaxThread.HandleException;
Var
  aException: Exception;
Begin
  aException := Exception(ExceptObject);

  // Don't show EAbort messages
  If Not(aException Is EAbort) Then
    {$IFDEF FORWARD_EXCEPTIONS_TO_MAIN_THREAD}
    synchronize(
      Procedure
      Begin
        aException.Message := aException.Message + '; Thread.Name: ' + fName;
        If aException Is Exception Then
          Application.ShowException(aException)
        Else
          SysUtils.ShowException(aException, Nil);
      End);
  {$ENDIF}

End;

{$IFDEF MSWINDOWS}


Procedure MsgWaitForSingleObject(Handle: THandle; TimeOut: dword = infinite);
Begin
  maxSignal.MsgWaitForSingleObject(Handle, TimeOut);
End; // MsgWaitForSingleObject
{$ENDIF}


Function InsideMainThread: boolean;
Begin
  result := maxSignal.InsideMainThread;
End;

{ TSimpleAsync }

Constructor TmaxAsync.Create;
Begin
  Inherited Create;
  fThreadData := TmaxAsyncGlobal.GetThreadData;
End;

Destructor TmaxAsync.Destroy;
Begin
  fThreadData.KeepAlive := false;
  fThreadData.WakeUpSignal.SetSignaled;

  fThreadData := NIL;
  Inherited;
End;

Function TmaxAsync.Finished: boolean;
Begin
  result := fThreadData.Finished;
End;

{$IFDEF MSWindows}


Procedure TmaxAsync.SetThreadPriority(Const Value: TThreadPriority);
Begin
  self.fThreadData.Thread.Priority := Value;
End;
{$ENDIF}


Procedure TmaxAsync.WakeUp;
Begin
  If fThreadData.Terminated Then
    WakeUp(fThreadData.Proc, fThreadData.TaskName)
  Else
  Begin
    fThreadData.Finished := false;
    fThreadData.ReadySignal.SetNonSignaled;
    fThreadData.WakeUpSignal.SetSignaled;
  End;
End;

Procedure TmaxAsync.WakeUp(aProc: TThreadProcedure; Const TaskName: String);
Begin
  fThreadData.Proc := aProc;
  fThreadData.TaskName := TaskName;
  fThreadData.Finished := false;

  fThreadData.ReadySignal.SetNonSignaled;
  fThreadData.WakeUpSignal.SetSignaled;
End;

Procedure TmaxAsync.WaitFor;
Begin
  // if not fThreadData.Finished then
  // if fThreadData.Thread <> nil then
  fThreadData.ReadySignal.WaitForSignaled;
  // fThreadData.Thread.WaitFor;
End;

{ TTHreadData }

Constructor TThreadData.Create;
Begin
  Inherited Create;

  TmaxAsyncGlobal.addToAllThreadList(self);

  fStartSignal := TSignal.Create;
  fReadySignal := TSignal.Create;
  FWakeSignal := TSignal.Create;

  SetDefaultValues;

  fThread := TmaxThread.Create(True);
  {$IFDEF MSWINDOWS}
  fThreadHandle := fThread.Handle;
  {$ENDIF}
  fThreadId := 0;
  fThread.fThreadData := self;
  fThread.FreeOnterminate := True;

  fThread.Start;

End;

Destructor TThreadData.Destroy;
Begin
  TmaxAsyncGlobal.removeFromAllThreadList(self);

  fReadySignal := NIL;
  FWakeSignal := NIL;
  Inherited;
End;

Function TThreadData.GetFinished: boolean;
Begin
  result := fFinished;
End;

Function TThreadData.GetKeepAlive: boolean;
Begin
  result := fKeepAlive;
End;

Function TThreadData.GetProc: TThreadProcedure;
Begin
  result := FProc;
End;

Function TThreadData.GetReadySignal: iSignal;
Begin
  result := fReadySignal;
End;

Function TThreadData.GetTaskName: String;
Begin
  result := fTaskName;
End;

Function TThreadData.GetTerminated: boolean;
Begin
  result := FTerminated;
End;

Function TThreadData.GetThread: TmaxThread;
Begin
  result := fThread;
End;

Function TThreadData.GetFullThreadId: String;
Begin
  result := 'ID: ' + IntToStr(fThreadId) + ' ($' + IntToHex(fThreadId, 1)
  {$IFDEF MSWINDOWS}
  + ' Handle: ' +
    IntToStr(fThreadHandle) + ' ($' +
    IntToHex(fThreadHandle, 1) + ')';
  {$ENDIF}
End;

Function TThreadData.GetWaiting: boolean;
Begin
  result := fWaiting;
End;

Function TThreadData.GetWakeSignal: iSignal;
Begin
  result := FWakeSignal;
End;

Procedure TThreadData.SetFinished(Const Value: boolean);
Begin
  fFinished := Value;
End;

Procedure TThreadData.SetKeepAlive(Const Value: boolean);
Begin
  fKeepAlive := Value;
End;

Procedure TThreadData.SetProc(Const Value: TThreadProcedure);
Begin
  FProc := Value;
End;

Procedure TThreadData.SetTaskName(Const Value: String);
Begin
  fTaskName := Value;
End;

Procedure TThreadData.SetTerminated(Const Value: boolean);
Begin
  FTerminated := Value;
End;

Procedure TThreadData.SetThread(Const Value: TmaxThread);
Begin
  fThread := Value
End;

Procedure TThreadData.SetThreadId(const aId: TThreadId);
Begin
  fThreadId := aId;
End;

Procedure TThreadData.SetWaiting(Const Value: boolean);
Begin
  fWaiting := Value;
End;

Procedure SyncVCLCall(aProc: TThreadProcedure; DoWait: boolean);
Begin
  If InsideMainThread Then
    aProc()
  Else
    TmaxThread.DoSync(aProc, DoWait)
End;

Function TThreadData.GetStartSignal: iSignal;
Begin
  result := fStartSignal;
End;

Procedure TThreadData.SetThreadToTerminated;
Begin
  fThread.terminate;
End;

Procedure TThreadData.SetDefaultValues;
Begin
  fTaskName := '';
  fKeepAlive := True;
  fFinished := false;
  fWaiting := false;
  FProc := Nil;

  fStartSignal.SetNonSignaled;
  fReadySignal.SetNonSignaled;
  FWakeSignal.SetNonSignaled;

  {$IFDEF MSWINDOWS}
  If assigned(fThread) Then
    self.fThread.Priority := TThreadPriority.tpNormal;
  {$ENDIF}
End;

{ TFindInSortedList<T> }

Constructor TFindInSortedList<T>.Create(aList: TList<T>;
aComparer:
  iComparer<T>);
Begin
  Inherited Create;
  fList := aList;
  fComparer := aComparer;

End;

Function TFindInSortedList<T>.Find(
Const
  Value:
  T;
Out index: integer): boolean;

Var
  L, H, i, C: integer;
Begin
  result := false;
  L := 0;
  H := fList.count - 1;
  While L <= H Do
  Begin
    i := (L + H) Shr 1;
    C := fComparer.compare(fList[i], Value);
    If C < 0 Then
      L := i + 1
    Else
    Begin
      H := i - 1;

      If C = 0 Then
      Begin
        result := True;
        Index := i;
        Exit;
      End;
    End;
  End;
  Index := L;
End;

{ TSafeValue<T> }

Procedure TSafeValue<T>.EnsureInit;
Begin
  If fSec = Nil Then
    fSec := tInterfacedCriticalSection.Create;
End;

Function TSafeValue<T>.GetValue: T;
Begin
  EnsureInit;
  fSec.Enter;
  Try
    result := fValue;
  Finally
    fSec.Leave;
  End;
End;

Class Function TSafeValue<T>.New: TSafeValue<T>;
Begin
  result := Default (TSafeValue<T>);
  result.EnsureInit;
End;

Procedure TSafeValue<T>.Setvalue(Const aValue: T);
Begin
  EnsureInit;
  fSec.Enter;
  Try
    fValue := aValue;
  Finally
    fSec.Leave;
  End;
End;

{ tInterfacedCriticalSection }

Constructor tInterfacedCriticalSection.Create;
Begin
  Inherited Create;
  fSec := TFixedCriticalSection.Create;
End;

Destructor tInterfacedCriticalSection.Destroy;
Begin
  fSec.free;
  Inherited;
End;

Procedure tInterfacedCriticalSection.Enter;
Begin
  fSec.Enter;
End;

Procedure tInterfacedCriticalSection.Leave;
Begin
  fSec.Leave;
End;

{ TAsyncLoop }

Constructor TAsyncLoop.Create;
Begin
  Inherited Create;
  FSimultanousThreadCount := TmaxAsyncGlobal.NumberOfProcessors * 4 - 2 { for the main VCL Thread };
  fReadySignal := TSignal.Create;
  fReadySignal.SetSignaled;
End;

Procedure TAsyncLoop.Execute(aMin, aMax: integer; aProc: TAsyncEnumProc);
Var
  x: integer;
Begin
  // prevent double executions
  WaitFor;
  fReadySignal.SetNonSignaled;

  {$IFDEF ASYNC_LOOP_USES_SINGLETHREAD}
  self.SimultanousThreadCount := 1;
  {$ENDIF}
  SetLength(fThreads, self.SimultanousThreadCount);

  fCurIndex := aMin;
  fMax := aMax;
  FProc := aProc;

  fActivThreadCount := Length(fThreads);

  For x := 0 To Length(fThreads) - 1 Do
    fThreads[x] := SimpleAsyncCall(
      Procedure
      Begin
        asyncLoopIteration;
      End, ClassName + IntToStr(x));

End;

Class Procedure TAsyncLoop.Run(Const aMin, aMax: integer; aProc: TAsyncEnumProcWithCancel; OnDone: TProc; ThreadCount: integer);
Var
  loop: TAsyncLoop;
Begin
  loop := TAsyncLoop.Create;
  If ThreadCount <> 0 Then
    loop.SimultanousThreadCount := ThreadCount;

  loop.OnFinished :=
      Procedure
    Begin
      loop.free;
      OnDone;
    End;

  loop.Execute(aMin, aMax,
    Procedure(i: integer)
    Var
      aCancel: boolean;
    Begin
      aCancel := false;
      aProc(i, aCancel);
      If aCancel Then
        loop.Cancel;
    End);

End;

Class Procedure TAsyncLoop.RunAndWait(Const aMin, aMax: integer; aProc: TAsyncEnumProcWithCancel; ThreadCount: integer);
Var
  loop: TAsyncLoop;
Begin
  loop := TAsyncLoop.Create;
  If ThreadCount <> 0 Then
    loop.SimultanousThreadCount := ThreadCount;

  loop.Execute(aMin, aMax,
    Procedure(i: integer)
    Var
      aCancel: boolean;
    Begin
      aCancel := false;
      aProc(i, aCancel);
      If aCancel Then
        loop.Cancel;
    End);

  loop.WaitFor;
  loop.free;
End;

Procedure TAsyncLoop.WaitFor;
Begin
  fReadySignal.WaitForSignaled;
End;

Procedure TAsyncLoop.SetOnFinished(

Const
  Value:
  TThreadProcedure);
Begin
  FOnFinished := Value;
End;

Procedure TAsyncLoop.SetSimultanousThreadCount(

Const
  Value:
  integer);
Begin
  FSimultanousThreadCount := Value;
End;

Destructor TAsyncLoop.Destroy;
Var
  x: integer;
Begin
  WaitFor;
  For x := 0 To Length(fThreads) - 1 Do
    If fThreads[x] <> Nil Then
    Begin
      fThreads[x].WaitFor;
      fThreads[x] := Nil;
    End;
  fThreads := Nil;
  fReadySignal := Nil;
  Inherited;
End;

Procedure TAsyncLoop.asyncLoopIteration;
Var
  index: integer;
  CallFinished: boolean;
Begin
  Repeat
    CallFinished := false;
    Index := TInterlocked.Increment(fCurIndex) - 1;
    If Index > fMax Then
      If TInterlocked.Decrement(fActivThreadCount) = 0 Then
        CallFinished := True;

    If CallFinished Then
    Begin
      fReadySignal.SetSignaled;
      If assigned(FOnFinished) Then
        FOnFinished();
    End
    Else If Index <= fMax Then
    Begin
      FProc(Index);
    End;
  Until Index > fMax;
End;

Procedure TAsyncLoop.Cancel;
Begin
  TInterlocked.Add(fCurIndex, fMax + 1);
End;

{ TmaxAsyncGlobal }

Class Constructor TmaxAsyncGlobal.CreateClass;
Begin
  {$IFDEF MSWINDOWS}
  InitializeCriticalSection(fCS);
  {$ELSE}
  fCS := TCriticalSection.Create;
  {$ENDIF}
  {$IFDEF madExcept}
  HideLeak(@fCS);
  {$ENDIF}
  FNumberOfProcessors := TThread.ProcessorCount;
  fWaitingThreadDataList := Nil;
  fAllThreads := TList<TThreadData>.Create;
End;

Class Destructor TmaxAsyncGlobal.Destroyclass;
Var
  x: integer;
Begin
  fCS.Enter;
  Try
    fGlobalThreadListDestroyed := True;
    If assigned(fWaitingThreadDataList) Then
    Begin
      For x := 0 To fWaitingThreadDataList.count - 1 Do
      Begin
        ReleaseThreadData(fWaitingThreadDataList[x]);
        fWaitingThreadDataList[x] := NIL;
      End;
      fWaitingThreadDataList.clear;
      fWaitingThreadDataList.free;
      fWaitingThreadDataList := Nil;
    End;

    fAllThreads.free;
  Finally
    fCS.Leave;
  End;
  // leave it on...
  // deleteCriticalSection(fCS);
End;

Class Function TmaxAsyncGlobal.GetThreadData: iThreadData;
Var
  i: integer;
Begin
  result := Nil;
  fCS.Enter;
  Try
    // if there are some threads waiting... use them... otherwise we will need to create a new one
    If (assigned(fWaitingThreadDataList)) And (fWaitingThreadDataList.count <> 0) Then
    Begin
      i := fWaitingThreadDataList.count - 1;
      result := fWaitingThreadDataList[i];
      fWaitingThreadDataList[i] := Nil;
      fWaitingThreadDataList.Delete(i);

      result.SetDefaultValues;
    End;
  Finally
    fCS.Leave;
  End;

  If result = Nil Then
    result := TThreadData.Create;
End;

Class Procedure TmaxAsyncGlobal.AddToWaiting(ThreadData: iThreadData);
Begin
  fCS.Enter;
  Try
    If fGlobalThreadListDestroyed Then
    Begin
      ReleaseThreadData(ThreadData);
      ThreadData := Nil;
    End
    Else
    Begin
      If Not assigned(fWaitingThreadDataList) Then
        fWaitingThreadDataList := TList<iThreadData>.Create;
      fWaitingThreadDataList.Add(ThreadData);
    End;
  Finally
    fCS.Leave;
  End;
End;

Class Procedure TmaxAsyncGlobal.ReleaseThreadData(Const aThreadData: iThreadData);
Begin
  aThreadData.SetThreadToTerminated;
  aThreadData.KeepAlive := false;
  aThreadData.WakeUpSignal.SetSignaled;
  aThreadData.StartSignal.SetSignaled;
End;

Class Procedure TmaxAsyncGlobal.addToAllThreadList(aData: TThreadData);
Begin
  If fGlobalThreadListDestroyed Then
    Exit;

  fCS.Enter;
  Try
    If Not fGlobalThreadListDestroyed Then
      fAllThreads.Add(aData);

  Finally
    fCS.Leave;
  End;
End;

Class Procedure TmaxAsyncGlobal.removeFromAllThreadList(aData: TThreadData);
Var
  i: integer;
Begin
  If fGlobalThreadListDestroyed Then
    Exit;

  fCS.Enter;
  Try
    If Not fGlobalThreadListDestroyed Then
    Begin
      i := fAllThreads.indexOf(aData);
      If i <> -1 Then
        fAllThreads.Delete(i);
    End;
  Finally
    fCS.Leave;
  End;
End;

Class Function TmaxAsyncGlobal.GetThreadName(const aThreadId: TThreadId): String;
Var
  x: integer;
Begin
  result := '';
  If fGlobalThreadListDestroyed Then
    Exit;

  If aThreadId = MainThreadID Then
    Exit('MainVclThread');

  fCS.Enter;
  Try
    If Not fGlobalThreadListDestroyed Then
    Begin
      For x := 0 To fAllThreads.count - 1 Do
        If fAllThreads[x].fThreadId = aThreadId Then
        Begin
          result := fAllThreads[x].TaskName;
          break;
        End;
    End;
  Finally
    fCS.Leave;
  End;
End;

{ TWaiter }

{$IFDEF MSWINDOWS}


Class Function TWaiter.WaitFor(Const async: TArray<iAsync>; Milliseconds: dword = infinite; DoProcessMessages: boolean = false): boolean;
Var
  events: TArray<TEvent>;
  x: integer;
Begin
  SetLength(events, Length(async));
  For x := 0 To Length(async) - 1 Do
    events[x] := (async[x] As iAsyncIntern).GetThreadData.ReadySignal.Event;

  result := WaitFor(events, Milliseconds, DoProcessMessages);
End;

Class Function TWaiter.WaitFor(Const Signals: TArray<iSignal>; Milliseconds: dword; DoProcessMessages: boolean): boolean;
Var
  events: TArray<TEvent>;
  x: integer;
Begin
  SetLength(events, Length(Signals));
  For x := 0 To Length(Signals) - 1 Do
    events[x] := Signals[x].Event;

  result := WaitFor(events, Milliseconds, DoProcessMessages);
End;

Class Function TWaiter.WaitFor(Const aEvents: TArray<TEvent>; Milliseconds: dword; DoProcessMessages: boolean): boolean;
CONST
  MAXCOUNT = Windows.MAXIMUM_WAIT_OBJECTS - 1;

  Procedure Delete(Var a: TArray<THandle>; index, count: integer);
  Var
    ItemsOnTheRight, len: integer;
  Begin
    len := Length(a);
    ItemsOnTheRight := len - ((Index + count));
    If ItemsOnTheRight > 0 Then
    Begin
      Move(
      a[Index + count],
        a[Index],
        SizeOf(THandle) * ItemsOnTheRight);
    End;

    SetLength(a, len - count);
  End;

Var
  x: integer;
  count: dword;
  SignalState: dword;
  StartTime: dword;
  orgTimeOutMilliseconds: dword;
  events: TArray<THandle>;
  st: TStopWatch;
Begin
  If DoProcessMessages And (Not InsideMainThread) Then
    DoProcessMessages := false;

  orgTimeOutMilliseconds := Milliseconds;
  st := TStopWatch.StartNew;

  count := Length(aEvents);
  If count = 0 Then
    Exit(True);

  SetLength(events, count);
  Move(aEvents[0], events[0], count * SizeOf(THandle));

  While Length(events) > 0 Do
  Begin
    count := Min(MAXCOUNT, Length(events));

    If Not DoProcessMessages Then
    Begin
      SignalState := WaitForMultipleObjects(count, @events[0], True, Milliseconds);
      If SignalState In [WAIT_OBJECT_0 .. WAIT_OBJECT_0 + count - 1] Then
        Delete(events, 0, count);

    End
    Else
    Begin

      SignalState := MsgWaitForMultipleObjects(count, events[0], false, Milliseconds, QS_ALLINPUT);
      // GUI message
      If SignalState = WAIT_OBJECT_0 + count Then
        Application.ProcessMessages
      Else If SignalState In [WAIT_OBJECT_0 .. WAIT_OBJECT_0 + count - 1] Then
        Delete(events, SignalState - WAIT_OBJECT_0, 1);
    End;

    // check timeout
    If (Milliseconds <> infinite) And (Length(events) <> 0) Then
    Begin
      // if the timeOut is set, and is due, then exit
      If (orgTimeOutMilliseconds <= st.ElapsedMilliseconds) Then
      Begin
        events := Nil;
        Exit(false);
      End;

      // now calculate the new timeout for the WaitForMultipleObjects proc, take the time already passed into account, but wait at least 1 millisecond
      Milliseconds := Max(1, orgTimeOutMilliseconds - (st.ElapsedMilliseconds));
    End;

    // end of while loop
  End;
  result := True;
End;
{$ENDIF}

{ TAsyncCollectionProcessor<T> }

Constructor TAsyncCollectionProcessor<T>.Create;
Begin
  Inherited Create;
  {$IFDEF MSWINDOWS}
  fCriticalSection := TFixedRtlCriticalSection.Create;
  {$ELSE} !
    fCriticalSection := TFixedCriticalSection.Create;
  {$ENDIF}
  fItems := TQueue<T>.Create;

  fThreads := TList<TThreadHolder>.Create;

  FSimultanousThreadCount := TmaxAsyncGlobal.NumberOfProcessors * 4 - 2 { for the main VCL Thread };
  fReadySignal := TSignal.Create;
  fReadySignal.SetSignaled;
End;

Destructor TAsyncCollectionProcessor<T>.Destroy;
Begin
  ClearItems;
  WaitFor;
  fItems.free;
  fThreads.free;
  fCriticalSection.free;
  Inherited;
End;

Procedure TAsyncCollectionProcessor<T>.Add(Const Items: TList<T>);
Var
  item: T;
Begin
  fCriticalSection.Enter;
  Try
    For item In Items Do
      fItems.enQueue(item);
  Finally
    fCriticalSection.Leave;
  End;
  RestartThreads;

End;

Procedure TAsyncCollectionProcessor<T>.Add(Const Items: TArray<T>);
Var
  item: T;
Begin
  fCriticalSection.Enter;
  Try
    For item In Items Do
      fItems.enQueue(item);
  Finally
    fCriticalSection.Leave;
  End;
  RestartThreads;
End;

Procedure TAsyncCollectionProcessor<T>.Add(Const item: T);
Begin
  fCriticalSection.Enter;
  Try
    fItems.enQueue(item);
  Finally
    fCriticalSection.Leave;
  End;
  RestartThreads;
End;

Procedure TAsyncCollectionProcessor<T>.ClearItems;
Begin
  fCriticalSection.Enter;
  Try
    fItems.clear;
  Finally
    fCriticalSection.Leave;
  End;
End;

Procedure TAsyncCollectionProcessor<T>.SetOnFinished(Const Value: TThreadProcedure);
Begin
  FOnFinished := Value;
End;

Procedure TAsyncCollectionProcessor<T>.SetProc(Const Value: TAsyncCollectionProcessorProc<T>);
Begin
  FProc := Value;
End;

Procedure TAsyncCollectionProcessor<T>.SetSimultanousThreadCount(Const Value: integer);
Begin
  If FSimultanousThreadCount <> Value Then
  Begin
    FSimultanousThreadCount := Value;
    RestartThreads;
  End;
End;

Procedure TAsyncCollectionProcessor<T>.WaitFor;
Begin
  fReadySignal.WaitForSignaled;
End;

Procedure TAsyncCollectionProcessor<T>.RestartThreads;
Begin
  fReadySignal.SetNonSignaled;
  fCriticalSection.Enter;
  Try
    While fThreads.count < FSimultanousThreadCount Do
      fThreads.Add(CreateThreadHolder);

  Finally
    fCriticalSection.Leave;
  End;
End;

Function TAsyncCollectionProcessor<T>.CreateThreadHolder: TThreadHolder;
Var
  Id: int64;
Begin
  // this is already in a critical section, no need to lock it again
  result := Default (TThreadHolder);
  {$R-}{$Q-}
  Id := TInterlocked.Increment(fNextThreadId);
  result.Id := Id;
  result.async := SimpleAsyncCall(
    Procedure
    Begin
      AsyncProcessItem(Id);
    End, ClassName + 'Worker')
End;

Procedure TAsyncCollectionProcessor<T>.AsyncProcessItem(const ThreadId: int64);
Var
  x: integer;
  item: T;
  IsLast, NoItem: boolean;
Begin
  Repeat
    IsLast := false;
    NoItem := false;
    Item:= default(T); // prevent 'variable might not be initialized' compiler warning

    fCriticalSection.Enter;
    Try
      If fItems.count <> 0 Then
      begin
        item := fItems.dequeue;
        NoItem := False;
      end else begin
        NoItem := True;

        For x := 0 To fThreads.count - 1 Do
          If fThreads[x].Id = ThreadId Then
          Begin
            fThreads.Delete(x);
            break;
          End;
        IsLast := fThreads.count = 0
      End;

    Finally
      fCriticalSection.Leave;
    End;

    If NoItem And IsLast Then
    Begin
      Try
        If assigned(FOnFinished) Then
          FOnFinished();
      Finally
        fReadySignal.SetSignaled;
      End;
    end Else If Not NoItem Then
      FProc(item);
  Until NoItem;
End;

Function TmaxAsync.GetThreadData: iThreadData;
Begin
  result := fThreadData;
End;

{$IFDEF MSWINDOWS}


Function TmaxAsync.GetThreadPriority: TThreadPriority;
Begin
  result := self.fThreadData.Thread.Priority;
End;

{$IFDEF MsWIndows}


procedure TmaxAsync.MsgWaitFor;
begin
  While Not fThreadData.Finished Do
    fThreadData.ReadySignal.MsgWaitForSignaled;
end;
{$ENDIF}

{$ENDIF}
{ TUserData }

Constructor TUserData<T>.Create(Const aValue: T);
Begin
  Inherited Create;
  fValue := aValue;
  fLock := Default (TLockFreeLock);
End;

Function TUserData<T>.GetValue: T;
Begin
  fLock.lock;
  Try
    result := fValue;
  Finally
    fLock.unLock;
  End;
End;

Procedure TUserData<T>.Setvalue(Const aValue: T);
Begin
  fLock.lock;
  Try
    fValue := aValue;
  Finally
    fLock.unLock;
  End;
End;

{ TRTLCriticalSectionHelper }
{$IFDEF MSWINDOWS}


Class Function TRTLCriticalSectionHelper.Create: TRTLCriticalSection;
Begin
  InitializeCriticalSection(result);
End;

Procedure TRTLCriticalSectionHelper.Enter;
Begin
  EnterCriticalSection(self);
End;

Procedure TRTLCriticalSectionHelper.Exit;
Begin
  LeaveCriticalSection(self);
End;

Procedure TRTLCriticalSectionHelper.free;
Begin
  DeleteCriticalSection(self);
End;

Procedure TRTLCriticalSectionHelper.Leave;
Begin
  LeaveCriticalSection(self);
End;
{$ENDIF}

{ TObjectWithCriticalSection }

Constructor TObjectWithCriticalSection.Create;
Begin
  Inherited Create;
  {$IFDEF MSWINDOWS}
  fCriticalSection := TFixedRtlCriticalSection.Create;
  {$ELSE}
  fCriticalSection := TFixedCriticalSection.Create;
  {$ENDIF}
End;

Destructor TObjectWithCriticalSection.Destroy;
Begin
  fCriticalSection.free;
  Inherited;
End;

Procedure TObjectWithCriticalSection.lock;
Begin
  fCriticalSection.Enter;
End;

Procedure TObjectWithCriticalSection.unLock;
Begin
  fCriticalSection.Leave;
End;

{ TFixedRtlCriticalSection }

{$IFDEF MSWINDOWS}


Class Function TFixedRtlCriticalSection.Create: TFixedRtlCriticalSection;
Begin
  result.Dummy := Default (TDummy);
  result.CriticalSection := TRTLCriticalSection.Create;
End;

Procedure TFixedRtlCriticalSection.Enter;
Begin
  CriticalSection.Enter;
End;

Procedure TFixedRtlCriticalSection.Exit;
Begin
  CriticalSection.Leave;
End;

Procedure TFixedRtlCriticalSection.free;
Begin
  CriticalSection.free;
End;

Procedure TFixedRtlCriticalSection.Leave;
Begin
  CriticalSection.Leave;
End;
{$ENDIF}

{ TLockFreeLock }

Procedure TLockFreeLock.lock;
Var
  Succeeded: boolean;
Begin
  Repeat
    TInterlocked.CompareExchange(fLocked, 1, 0, Succeeded);
  Until Succeeded;
End;

Procedure TLockFreeLock.unLock;
Begin
  // if we acquired the lock, then we do not need to wait or test the value
  TInterlocked.Exchange(fLocked, 0);
End;

{ TAtomic }

Class Constructor TAtomic.Create;
Type
  TTestRec = Record
    a: byte;
    B: int64;
  End;

Begin
  // In the {$A8} or {$A+} state, fields in record types that are declared without the packed modifier and fields in class structures are aligned on quadword boundaries.
  {$IF SIZEOF(TTestRec) <> 16}
  'App must be compiled in A8 mode'
  {$IFEND}
End;

{ TAsyncTimer }

Procedure TAsyncTimer.SetEnabled(Const Value: boolean);
Begin
  If FEnabled <> Value Then
  Begin
    FEnabled := Value;
    If Value Then
      fSignal.SetSignaled;
  End;
End;

Constructor TAsyncTimer.Create(aProc: TProc; aInterval: integer; aEnabled: boolean);
Begin
  Inherited Create;
  FInterval := aInterval;
  fSignal := TSignal.Create;
  fSignal.SetNonSignaled;
  FProc := aProc;
  FEnabled := aEnabled;
  fAsync := SimpleAsyncCall(asyncLoop)
End;

Destructor TAsyncTimer.Destroy;
Begin
  FEnabled := false;
  FTerminated := True;
  fSignal.SetSignaled;
  fAsync.WaitFor;
  Inherited;
End;

Procedure TAsyncTimer.asyncLoop;
Begin
  Repeat
    fSignal.WaitForSignaled(FInterval);
    If FEnabled Then
      FProc();
    fSignal.SetNonSignaled;

  Until FTerminated;
End;

Procedure TAsyncTimer.SetInterval(Const Value: integer);
Begin
  FInterval := Value;
  fSignal.SetSignaled;
End;

Initialization

{$IFDEF MadExcept}
madExcept.NameThread(GetCurrentThreadId, 'MainVclThread');
{$ENDIF}
System.NeverSleepOnMMThreadContention := True;

End.
