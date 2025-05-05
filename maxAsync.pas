unit maxAsync;

{ .$DEFINE DEBUG_MAXASYNC }

{ .$DEFINE ASYNC_LOOP_USES_SINGLETHREAD } // for debuging

{$IFDEF MsWindows}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{ SOME OTHER USEFUL CONDITIONALS:
  NeverUninstall;NeverSleepOnMMThreadContention;UseSwitchToThread;UseReleaseStack   v }

// forward unhandled exceptions to main thread?
{$IFDEF MsWindows}{$IFNDEF CONSOLE}
{$DEFINE FORWARD_EXCEPTIONS_TO_MAIN_THREAD}
{$ENDIF}{$ENDIF}

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
interface

uses
  {$IFDEF MsWindows}
  winApi.Windows, Winapi.Messages,
  {$IFNDEF CONSOLE}
  Forms, Dialogs, System.UITypes,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF LINUX}
  posix.sysTypes,
  posix.pthread,
  {$ENDIF}
  Classes, SysUtils, SyncObjs,
  generics.defaults, generics.collections,
  maxSignal;

type
  {$IFNDEF MSWINDOWS}
  dword = Cardinal;
  {$ENDIF}
  // forward declaration
  TmaxThread = class;

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

  iAsync = interface
    ['{41564891-4A25-464E-B756-B6EFD50063E4}']
    procedure WaitFor;
    {$IFDEF MsWindows}
    procedure MsgWaitFor;
    {$ENDIF}
    function Finished: boolean;
    // After execution, the thread is still available, so you do not need to re-create it. The easiest way is just to WakeUp it. You can run it either with a new procedure to be called or with the same as before
    procedure WakeUp(aProc: TThreadProcedure; const TaskName: string); overload;
    procedure WakeUp; overload;

    {$IFDEF MsWindows}
    function GetThreadPriority: TThreadPriority;
    procedure SetThreadPriority(const Value: TThreadPriority);
    property Priority: TThreadPriority read GetThreadPriority write SetThreadPriority;
    {$ENDIF}
  end;

  iThreadData = interface
    ['{CC30782E-30D8-48E4-B3C6-33383C37C3BA}']
    function GetFinished: boolean;
    function GetFullThreadId: string;
    procedure SetThreadId(const aId: TThreadId);
    function GetWaiting: boolean;
    procedure SetFinished(const Value: boolean);
    procedure SetWaiting(const Value: boolean);
    procedure SetTerminated(const Value: boolean);
    function GetTerminated: boolean;
    function GetReadySignal: iSignal;
    procedure SetThread(const Value: TmaxThread);
    function GetThread: TmaxThread;

    function GetWakeSignal: iSignal;
    function GetKeepAlive: boolean;
    function GetProc: TThreadProcedure;
    function GetTaskName: string;
    procedure SetKeepAlive(const Value: boolean);
    procedure SetThreadToTerminated;
    procedure SetProc(const Value: TThreadProcedure);
    procedure SetTaskName(const Value: string);
    function GetStartSignal: iSignal;

    procedure SetDefaultValues;

    property WakeUpSignal: iSignal read GetWakeSignal;
    property Proc: TThreadProcedure read GetProc write SetProc;
    property KeepAlive: boolean read GetKeepAlive write SetKeepAlive;
    property TaskName: string read GetTaskName write SetTaskName;

    property StartSignal: iSignal read GetStartSignal;
    property Finished: boolean read GetFinished write SetFinished;
    property Waiting: boolean read GetWaiting write SetWaiting;
    property Terminated: boolean read GetTerminated write SetTerminated;
    property ReadySignal: iSignal read GetReadySignal;

    // direct access
    property Thread: TmaxThread read GetThread write SetThread;
  end;

  TDummy = record
    case byte of
      0:
      (byteArray: array[0..95] of byte; );
      1:
      (integerArray: array[0..23] of integer; );
      2:
      (int64Array: array[0..11] of int64; );
      3:
      (FloatArray: array[0..11] of double; );
  end;

  { http://delphitools.info/2011/11/30/fixing-tcriticalsection/
    TFixedCriticalSection (along with TMonitor*) suffers from a severe design flaw in which entering/leaving different TFixedCriticalSection instances can end up serializing your threads, and the whole can even end up performing worse than if your threads had been serialized.
    This is because it�s a small, dynamically allocated object, so several TFixedCriticalSection instances can end up in the same CPU cache line, and when that happens, you�ll have cache conflicts aplenty between the cores running the threads.
    How severe can that be? Well, it depends on how many cores you have, but the more cores you have, the more severe it can get. On a quad core, a bad case of contention can easily result in a 200% slowdown on top of the serialization. And it won�t always be reproducible, since it�s related to dynamic memory allocation.
    There is thankfully a simple fix for that, use TFixedCriticalSection: }

  TFixedCriticalSection = class(TCriticalSection)
  public
    Dummy: TDummy
  end;

  // prefered is still the TRTLCriticalSection, or better, the TFixedRtlCriticalSection
  {$IFDEF MsWindows}
  TRTLCriticalSectionHelper = record helper
    for TRTLCriticalSection
      Public
      class
      function Create: TRTLCriticalSection;
        static;
    public
      procedure Enter; inline;
      procedure Leave; inline;
      // just an alias
      procedure exit; inline;
      procedure Free; inline;
    end;
    {$ENDIF}
    {$IFDEF MsWindows}

    TFixedRtlCriticalSection = record
    public
      // you can access it directly if needed
      CriticalSection: TRTLCriticalSection;
      // see TFixedCriticalSection for details
      Dummy: TDummy;

      class function Create: TFixedRtlCriticalSection; static; inline;

      procedure Enter; inline;
      procedure Leave; inline;
      // just an alias
      procedure exit; inline;
      procedure Free; inline;
    end;
    {$ENDIF}

    TObjectWithCriticalSection = class
    protected
      {$IFDEF MsWindows}
      fCriticalSection: TFixedRtlCriticalSection;
      {$ELSE}
      fCriticalSection: TFixedCriticalSection;
      {$ENDIF}
    public
      constructor Create;
      destructor Destroy; override;

      procedure lock; inline;
      procedure unLock; inline;
    end;

    iCriticalSection = interface
      ['{C906B8AD-2DCE-4E8B-9047-1B49FDCA5A98}']
      procedure Enter;
      procedure Leave;
    end;

    tInterfacedCriticalSection = class(TInterfacedObject, iCriticalSection)
    private
      fSec: TFixedCriticalSection;
    public
      constructor Create;
      destructor Destroy; override;

      procedure Enter;
      procedure Leave;
    end;

    TThreadData = class(TInterfacedObject, iThreadData)
    private
      fFinished: boolean;
      fWaiting: boolean;
      fThreadId: TThreadId;
      {$IFDEF MsWindows}
      fThreadHandle: THandle;
      {$ENDIF}
      fThread: TmaxThread;
      fReadySignal: iSignal;
      FWakeSignal: iSignal;
      fTaskName: string;
      fKeepAlive: boolean;
      FProc: TThreadProcedure;
      FTerminated: boolean;
      fStartSignal: iSignal;

      function GetThread: TmaxThread;
      function GetFullThreadId: string;
      procedure SetThreadId(const aId: TThreadId);
      procedure SetThread(const Value: TmaxThread);
      function GetFinished: boolean;
      function GetWaiting: boolean;
      procedure SetFinished(const Value: boolean);
      procedure SetWaiting(const Value: boolean);
      function GetReadySignal: iSignal;
      function GetWakeSignal: iSignal;

      function GetKeepAlive: boolean;
      function GetProc: TThreadProcedure;
      function GetTaskName: string;
      procedure SetKeepAlive(const Value: boolean);
      procedure SetThreadToTerminated;
      procedure SetProc(const Value: TThreadProcedure);
      procedure SetTaskName(const Value: string);

      procedure SetTerminated(const Value: boolean);
      function GetTerminated: boolean;
      function GetStartSignal: iSignal;
    public
      constructor Create;
      destructor Destroy; override;

      procedure SetDefaultValues;

      property Thread: TmaxThread read GetThread write SetThread;
      property StartSignal: iSignal read GetStartSignal;
      property Finished: boolean read GetFinished write SetFinished;
      property Waiting: boolean read GetWaiting write SetWaiting;
      property Terminated: boolean read GetTerminated write SetTerminated;
      property ReadySignal: iSignal read GetReadySignal;
      property WakeUpSignal: iSignal read GetWakeSignal;

      property Proc: TThreadProcedure read GetProc write SetProc;
      property KeepAlive: boolean read GetKeepAlive write SetKeepAlive;
      property TaskName: string read GetTaskName write SetTaskName;
    end;

    iAsyncIntern = interface
      ['{6C221063-5386-4C57-A645-B6E63DB04F1C}']
      function GetThreadData: iThreadData;
    end;

    TmaxAsync = class(TInterfacedObject, iAsync, iAsyncIntern)
    private
      fThreadData: iThreadData;
      function GetThreadData: iThreadData;
      {$IFDEF MsWindows}
      function GetThreadPriority: TThreadPriority;
      procedure SetThreadPriority(const Value: TThreadPriority);
      {$ENDIF}
    public
      constructor Create;
      destructor Destroy; override;
      {$IFDEF MsWindows}
      procedure MsgWaitFor;
      {$ENDIF}
      procedure WaitFor;
      function Finished: boolean;
      procedure WakeUp(aProc: TThreadProcedure; const TaskName: string); overload;
      procedure WakeUp; overload;
      {$IFDEF MsWindows}
      property Priority: TThreadPriority read GetThreadPriority write SetThreadPriority;
      {$ENDIF}
    end;

    TmaxThread = class(TThread)
    private
      fThreadData: iThreadData;
      fName: string;

      procedure HandleException;
      class procedure DoSync(aProc: TThreadProcedure; DoWait: boolean);
    protected
      procedure Execute; override;
    public
      destructor Destroy; override;
    end;

    TAsyncEnumProc = reference to procedure(CurIndex: integer);
    TAsyncEnumProcWithCancel = reference to procedure(CurIndex: integer; var aCancel: boolean);

    // iterates through a loop. It is a one time shoot, create it, run it and free it afterwards
    TAsyncLoop = class
    private
      fReadySignal: iSignal;
      fMax: integer;
      fCurIndex: integer;
      FProc: TAsyncEnumProc;
      FOnFinished: TThreadProcedure;
      FSimultanousThreadCount: integer;
      fActivThreadCount: integer;
      fThreads: array of iAsync;

      procedure SetOnFinished(const Value: TThreadProcedure);
      procedure SetSimultanousThreadCount(const Value: integer);
      procedure asyncLoopIteration;
    public
      constructor Create;
      destructor Destroy; override;

      procedure Execute(aMin, aMax: integer; aProc: TAsyncEnumProc);
      procedure WaitFor;
      procedure Cancel;

      // simplyfied call.  If ThreadCount is 0 (or less), then the default value will be used
      class procedure RunAndWait(const aMin, aMax: integer; aProc: TAsyncEnumProcWithCancel; ThreadCount: integer = 0);
      class procedure Run(const aMin, aMax: integer; aProc: TAsyncEnumProcWithCancel; OnDone: TProc; ThreadCount: integer = 0);

      // set before running the loop
      // finished will be executed in the thread context of the last active thread, so be aware to perform a sync if needed
      property OnFinished: TThreadProcedure read FOnFinished write SetOnFinished;
      // default is the number of CPU cores, *4 - 2 seting it after running the loop will have no effects
      property SimultanousThreadCount: integer read FSimultanousThreadCount write SetSimultanousThreadCount;
    end;

    TAsyncCollectionProcessorProc<t> = reference to procedure(const Item: t);
    // this will work in paralel on the items in the collection
    TAsyncCollectionProcessor<t >= class
    private type
        TThreadHolder = record
          Id: int64;
          async:
          iAsync;
        end;
    private
      fReadySignal: iSignal;
      fItems: TQueue<t>;
      FSimultanousThreadCount: integer;
      FOnFinished: TThreadProcedure;
      FProc: TAsyncCollectionProcessorProc<t>;

      fThreads: TList<TThreadHolder>;
      fNextThreadId: int64;
      {$IFDEF MsWindows}
      fCriticalSection: TFixedRtlCriticalSection;
      {$ELSE}
      fCriticalSection: TFixedCriticalSection;
      {$ENDIF}
      procedure SetOnFinished(const Value: TThreadProcedure);
      procedure SetProc(const Value: TAsyncCollectionProcessorProc<t>);
      procedure SetSimultanousThreadCount(const Value: integer);
      procedure RestartThreads;
      function CreateThreadHolder: TThreadHolder;
      procedure AsyncProcessItem(const ThreadId: int64);
    public
      constructor Create;
      destructor Destroy;
        override;

      procedure add(const Item: t);
        overload;
      procedure add(const Items: TArray<t>);
        overload;
      procedure add(const Items: TList<t>);
        overload;
      procedure ClearItems;

      procedure WaitFor;

      // set before running the loop
      property Proc: TAsyncCollectionProcessorProc<t>read FProc write SetProc;
      // finished will be executed in the thread context of the last active thread, so be aware to perform a sync if needed
      // NOTE: this callback is called after the last item is processed. So if you do not stop the processor, and add more items later, this event may be triggered multiple times
      property OnFinished: TThreadProcedure read FOnFinished write SetOnFinished;
      // default is the number of CPU cores, *4 - 2 . if the system is already running, you can only increase the number of threads used, a decrease will be applied only after a restart.
      property SimultanousThreadCount: integer read FSimultanousThreadCount write SetSimultanousThreadCount;
    end;

    TFindInSortedList<t >= class
    private
      fList: TList<t>;
      fComparer:
      iComparer<t>;
    public
      constructor Create(aList: TList<t>; aComparer: iComparer<t>);
      function find(const Value: t; out Index: integer): boolean;
    end;

    TSafeValue<t: record > = record
    private
      fValue: t;
      fSec: iCriticalSection;

      function getValue: t;
      procedure Setvalue(const aValue: t);
      procedure EnsureInit;
        inline;
    public
      property Value: t read getValue write Setvalue;
      class
        function new: TSafeValue<t>;
          static;
      end;

    iUserData<t >= interface
      ['{77527854-F405-4CBD-8FAA-B2ED87C6E2E1}']
procedure Setvalue(const aValue: t);
function getValue: t;
property Value: t read getValue write Setvalue;
    end;

    // a very small lock artifact, use carefuly
    TLockFreeLock = record
    private
      fLocked: integer;
    public
      procedure lock;
        inline;
      procedure unLock;
        inline;
    end;

    TUserData<t >= class(TInterfacedObject, iUserData<t>)
    private
      fValue: t;
      fLock: TLockFreeLock;
      procedure Setvalue(const aValue: t);
      function getValue: t;
    public
      constructor Create(const aValue: t);
      property Value: t read getValue write Setvalue;
    end;

    TmaxAsyncGlobal = class
    strict private
      class var FNumberOfProcessors: dword;
      class var fWaitingThreadDataList: TList<iThreadData>;
      class var fAllThreads: TList<TThreadData>;
        {$IFDEF MsWindows}
      class var fCS: TRTLCriticalSection;
        {$ELSE}
      class var fCS: TCriticalSection;
        {$ENDIF}
      class var fGlobalThreadListDestroyed: boolean;
    private
      class constructor CreateClass;
      class destructor Destroyclass;
      class procedure ReleaseThreadData(const aThreadData: iThreadData);
      class function GetThreadData: iThreadData;
      class procedure AddToWaiting(ThreadData: iThreadData);

      class procedure addToAllThreadList(aData: TThreadData);
      class procedure removeFromAllThreadList(aData: TThreadData);
    public
      class property NumberOfProcessors: dword read FNumberOfProcessors;
      class function GetThreadName(const aThreadId: TThreadId): string;
    end;

    {$IFDEF MsWindows}{$IFNDEF CONSOLE}
    // helps to wait for multiple signas or multiple async Items
    TWaiter = class
    public
      // returns false if the timeout is expired, otherwise true
      // ProcessMessages will be set to false if we are not in the main VCL Thread
      class function WaitFor(const async: TArray<iAsync>; Milliseconds: dword = infinite; DoProcessMessages: boolean = False): boolean; overload;
      class function WaitFor(const Signals: TArray<iSignal>; Milliseconds: dword = infinite; DoProcessMessages: boolean = False): boolean; overload;
      class function WaitFor(const aEvents: TArray<TEvent>; Milliseconds: dword = infinite; DoProcessMessages: boolean = False): boolean; overload;
    end;
    {$ENDIF}{$ENDIF}

    IReaderWriterLock = interface
      ['{044BC96C-BBD4-4B75-9487-048257D27679}']
      procedure BeginRead;
      procedure EndRead;
      procedure BeginWrite;
      procedure EndWrite;
    end;

    TAtomic = record
      /// <summary>
      /// Assures {$A8} or {$A+} state
      /// </summary>
      /// <seealso href="https://stackoverflow.com/questions/829235/ifopt-a4">
      /// {$IFOPT A4}?
      /// </seealso>
      class constructor Create;
    end;

    TAsyncTimer = class
    private
      fAsync: iAsync;
      fSignal: iSignal;
      FProc: TProc;
      FEnabled: boolean;
      FInterval: integer;
      FTerminated: boolean;
      procedure asyncLoop;
      procedure SetEnabled(const Value: boolean);
      procedure SetInterval(const Value: integer);
    public
      constructor Create(aProc: TProc; aInterval: integer; aEnabled: boolean = True);
      destructor Destroy;
        override;
      property Enabled: boolean read FEnabled write SetEnabled;
      property Interval: integer read FInterval write SetInterval;
    end;

function SimpleAsyncCall(aProc: TThreadProcedure;
  const TaskName: string = '';
  SyncedAfterDone: TThreadProcedure = nil
  {$IFDEF MsWindows}
  ; aThreadPriority: TThreadPriority = tpNormal
  {$ENDIF}
  ): iAsync;

{ WARNING:
  The sync code can only be executed when the main thread reaches a "safe" state, which, in a typical VCL application, is the message loop.
  Now imagine, the main thread is waiting... in that case it will never  execute the sync code.
  The main thread will wait for the thread to finish and the thread will wait for the main thread to sync. }
procedure syncVclCall(aProc: TThreadProcedure; DoWait: boolean = False);

// this function will tell us if we are in the main thread or not
function InsideMainThread: boolean;
  inline;

{$IFDEF MsWindows}
procedure MsgWaitForSingleObject(Handle: THandle; TimeOut: dword = infinite);
  inline;
{$ENDIF}

{$IFDEF MsWindows}{$IFNDEF CONSOLE}
function MessageDlg(const msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): integer;
function MessageDlgThreadSafe(const msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): integer;
{$ENDIF}{$ENDIF}

// how to kill a thread:
// TerminateThread(fThread.Handle, 1);
implementation

uses
  {$IFDEF madExcept}madExcept, {$ENDIF}
  {$IFDEF DEBUG_MAXASYNC}ClipBrd, {$ENDIF}
  {$IFDEF MsWindows}
  Winapi.mmSystem, Winapi.ActiveX,
  {$ENDIF}
  system.Math, system.diagnostics;

{$IFDEF MsWindows}{$IFNDEF CONSOLE}

function MessageDlg(const msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): integer;
begin
  Result := MessageDlgThreadSafe(msg, DlgType, Buttons, HelpCtx)
end;
{$ENDIF}{$ENDIF}

{$IFDEF MsWindows}{$IFNDEF CONSOLE}

function MessageDlgThreadSafe(const msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): integer;
var
  lResult: integer;
begin
  if InsideMainThread then
    Result := Dialogs.MessageDlg(msg, DlgType, Buttons, HelpCtx)
  else
  begin
    lResult := 0;
    syncVclCall(
      procedure
      begin
        lResult := Dialogs.MessageDlg(msg, DlgType, Buttons, HelpCtx);
      end, True);
    Result := lResult;
  end;
end;
{$ENDIF}{$ENDIF}

function SimpleAsyncCall(aProc: TThreadProcedure; const TaskName: string = ''; SyncedAfterDone: TThreadProcedure = nil
  {$IFDEF MsWindows}
  ; aThreadPriority: TThreadPriority = tpNormal
  {$ENDIF}
  ): iAsync;

  function merge: TThreadProcedure;
  begin
    Result := (
      procedure
      begin
        try
          aProc();
        finally
          syncVclCall(
            procedure
            begin
              SyncedAfterDone();
            end, False);
        end;
      end);
  end;

var
  async: TmaxAsync;
begin
  async := TmaxAsync.Create;
  {$IFDEF MsWindows}
  async.Priority := aThreadPriority;
  {$ENDIF}
  Result := async;
  if not assigned(SyncedAfterDone) then
    async.fThreadData.Proc := aProc
  else
    async.fThreadData.Proc := merge();

  async.fThreadData.TaskName := TaskName;
  async.fThreadData.StartSignal.SetSignaled;
end;

destructor TmaxThread.Destroy;
begin
  fThreadData.Waiting := False;
  fThreadData.Terminated := True;
  fThreadData.Finished := True;
  fThreadData.ReadySignal.SetSignaled;
  fThreadData := nil;
  inherited;
end;

class procedure TmaxThread.DoSync(aProc: TThreadProcedure; DoWait: boolean);
var
  Signal: iSignal;
  MyProc: TThreadProcedure;
begin
  if InsideMainThread then
  begin
    if DoWait then
      aProc()
    else
    begin
      {$IFDEF ForceQueueNotAvailable}
      TThread.Queue(nil, aProc);
      {$ELSE}
      TThread.ForceQueue(nil, aProc);
      {$ENDIF}
    end;

  end
  else if not DoWait then
    {$IFDEF ForceQueueNotAvailable}
    TThread.Queue(nil, aProc)
      {$ELSE}
    TThread.ForceQueue(nil, aProc)
      {$ENDIF}
  else
  begin
    Signal := TSignal.Create;
    Signal.SetNonSignaled;

    MyProc := procedure
    begin
      try
        aProc();
      finally
        // prevent a dead lock in case of a exception
        Signal.SetSignaled;
      end;
    end;

    {$IFDEF ForceQueueNotAvailable}
    TThread.Queue(nil, MyProc);
    {$ELSE}
    TThread.ForceQueue(nil, MyProc);
    {$ENDIF}
    Signal.WaitForSignaled;
    Signal := nil;
  end;
end;

procedure TmaxThread.Execute;
var
  CoInitialized: boolean;
  sName: string;
begin
  fThreadData.StartSignal.WaitForSignaled;

  {$IFDEF MsWindows}
  CoInitialized := (CoInitialize(nil) in [S_OK, S_FALSE]);
  {$ENDIF}
  fThreadData.SetThreadId(
    {$IFDEF MsWindows}
    GetCurrentThreadId
    {$ELSE}
    TThread.CurrentThread.ThreadId
    {$ENDIF}
    );
  {$IFDEF DEBUG_MAXASYNC}
  OutputDebugString(pwidechar('maxAsync: starting Thread:' + fThreadData.GetFullThreadId));
  {$ENDIF}
  try
    repeat
      repeat
        if Terminated then
          break;

        {$IFDEF DEBUG_MAXASYNC}
        OutputDebugString(pwidechar('maxAsync: running loop Thread:' + fThreadData.GetFullThreadId));
        {$ENDIF}
        fThreadData.ReadySignal.SetNonSignaled;
        fThreadData.WakeUpSignal.SetNonSignaled;

        fThreadData.Waiting := False;
        fThreadData.Finished := False;

        sName := classname + '.' + fThreadData.TaskName;
        fName := sName;
        {$IFDEF DEBUG_MAXASYNC}
        OutputDebugString(pwidechar('maxAsync: naming Thread:' + fThreadData.GetFullThreadId + ' >> ' + sName));
        {$ENDIF}
        NameThreadForDebugging(ansiString(sName));

        {$IFDEF madExcept}
        madExcept.NameThread(GetCurrentThreadId, sName);
        {$ENDIF}
        if fThreadData.Proc <> nil then
        begin
          try
            fThreadData.Proc();
          except
            HandleException;
          end;
        end
        else
        begin
          {$IFDEF DEBUG_MAXASYNC}
          OutputDebugString(pwidechar('maxAsync: no proc to run for Thread:' + fThreadData.GetFullThreadId));
          {$ENDIF}
        end;

        fThreadData.Finished := True;
        fThreadData.ReadySignal.SetSignaled;

        if (not fThreadData.KeepAlive) or Terminated then
          break;

        fThreadData.Waiting := True;

        {$IFDEF DEBUG_MAXASYNC}
        OutputDebugString(pwidechar('maxAsync: before wait for awake signal Thread:' + fThreadData.GetFullThreadId));
        {$ENDIF}
        fThreadData.WakeUpSignal.WaitForSignaled;
        {$IFDEF DEBUG_MAXASYNC}
        OutputDebugString(pwidechar('maxAsync: after wait for awake signal Thread:' + fThreadData.GetFullThreadId));
        {$ENDIF}
      until Terminated or (not fThreadData.KeepAlive);

      if not Terminated then
      begin
        fThreadData.StartSignal.SetNonSignaled;
        TmaxAsyncGlobal.AddToWaiting(fThreadData);
        fThreadData.StartSignal.WaitForSignaled;
      end;

    until Terminated;

  finally
    {$IFDEF DEBUG_MAXASYNC}
    OutputDebugString(pwidechar('maxAsync: exit Thread:' + fThreadData.GetFullThreadId));
    {$ENDIF}
    fThreadData.Waiting := False;
    fThreadData.Finished := True;
    fThreadData.Terminated := True;
    fThreadData.ReadySignal.SetSignaled;

    {$IFDEF MsWindows}
    if CoInitialized then
      CoUninitialize;
    {$ENDIF}
  end;
end;

procedure TmaxThread.HandleException;
var
  aException: Exception;
begin
  aException := Exception(ExceptObject);

  // Don't show EAbort messages
  if not (aException is EAbort) then
    {$IFDEF FORWARD_EXCEPTIONS_TO_MAIN_THREAD}
    synchronize(
      procedure
      begin
        aException.Message := aException.Message + '; Thread.Name: ' + fName;
        if aException is Exception then
          application.ShowException(aException)
        else
          SysUtils.ShowException(aException, nil);
      end);
  {$ENDIF}

end;

{$IFDEF MsWindows}

procedure MsgWaitForSingleObject(Handle: THandle; TimeOut: dword = infinite);
begin
  maxSignal.MsgWaitForSingleObject(Handle, TimeOut);
end; // MsgWaitForSingleObject
{$ENDIF}

function InsideMainThread: boolean;
begin
  Result := maxSignal.InsideMainThread;
end;

{ TSimpleAsync }

constructor TmaxAsync.Create;
begin
  inherited Create;
  fThreadData := TmaxAsyncGlobal.GetThreadData;
end;

destructor TmaxAsync.Destroy;
begin
  fThreadData.KeepAlive := False;
  fThreadData.WakeUpSignal.SetSignaled;

  fThreadData := nil;
  inherited;
end;

function TmaxAsync.Finished: boolean;
begin
  Result := fThreadData.Finished;
end;

{$IFDEF MsWindows}

procedure TmaxAsync.SetThreadPriority(const Value: TThreadPriority);
begin
  self.fThreadData.Thread.Priority := Value;
end;
{$ENDIF}

procedure TmaxAsync.WakeUp;
begin
  if fThreadData.Terminated then
    WakeUp(fThreadData.Proc, fThreadData.TaskName)
  else
  begin
    fThreadData.Finished := False;
    fThreadData.ReadySignal.SetNonSignaled;
    fThreadData.WakeUpSignal.SetSignaled;
  end;
end;

procedure TmaxAsync.WakeUp(aProc: TThreadProcedure; const TaskName: string);
begin
  fThreadData.Proc := aProc;
  fThreadData.TaskName := TaskName;
  fThreadData.Finished := False;

  fThreadData.ReadySignal.SetNonSignaled;
  fThreadData.WakeUpSignal.SetSignaled;
end;

procedure TmaxAsync.WaitFor;
begin
  // if not fThreadData.Finished then
  // if fThreadData.Thread <> nil then
  fThreadData.ReadySignal.WaitForSignaled;
  // fThreadData.Thread.WaitFor;
end;

{ TTHreadData }

constructor TThreadData.Create;
begin
  inherited Create;

  TmaxAsyncGlobal.addToAllThreadList(self);

  fStartSignal := TSignal.Create;
  fReadySignal := TSignal.Create;
  FWakeSignal := TSignal.Create;

  SetDefaultValues;

  fThread := TmaxThread.Create(True);
  {$IFDEF MsWindows}
  fThreadHandle := fThread.Handle;
  {$ENDIF}
  fThreadId := 0;
  fThread.fThreadData := self;
  fThread.FreeOnterminate := True;

  fThread.start;

end;

destructor TThreadData.Destroy;
begin
  TmaxAsyncGlobal.removeFromAllThreadList(self);

  fReadySignal := nil;
  FWakeSignal := nil;
  inherited;
end;

function TThreadData.GetFinished: boolean;
begin
  Result := fFinished;
end;

function TThreadData.GetKeepAlive: boolean;
begin
  Result := fKeepAlive;
end;

function TThreadData.GetProc: TThreadProcedure;
begin
  Result := FProc;
end;

function TThreadData.GetReadySignal: iSignal;
begin
  Result := fReadySignal;
end;

function TThreadData.GetTaskName: string;
begin
  Result := fTaskName;
end;

function TThreadData.GetTerminated: boolean;
begin
  Result := FTerminated;
end;

function TThreadData.GetThread: TmaxThread;
begin
  Result := fThread;
end;

function TThreadData.GetFullThreadId: string;
begin
  Result := 'ID: ' + IntToStr(fThreadId) + ' ($' + IntTohex(fThreadId, 1)
    {$IFDEF MsWindows}
  + ' Handle: ' +
    IntToStr(fThreadHandle) + ' ($' +
    IntTohex(fThreadHandle, 1) + ')';
  {$ENDIF}
end;

function TThreadData.GetWaiting: boolean;
begin
  Result := fWaiting;
end;

function TThreadData.GetWakeSignal: iSignal;
begin
  Result := FWakeSignal;
end;

procedure TThreadData.SetFinished(const Value: boolean);
begin
  fFinished := Value;
end;

procedure TThreadData.SetKeepAlive(const Value: boolean);
begin
  fKeepAlive := Value;
end;

procedure TThreadData.SetProc(const Value: TThreadProcedure);
begin
  FProc := Value;
end;

procedure TThreadData.SetTaskName(const Value: string);
begin
  fTaskName := Value;
end;

procedure TThreadData.SetTerminated(const Value: boolean);
begin
  FTerminated := Value;
end;

procedure TThreadData.SetThread(const Value: TmaxThread);
begin
  fThread := Value
end;

procedure TThreadData.SetThreadId(const aId: TThreadId);
begin
  fThreadId := aId;
end;

procedure TThreadData.SetWaiting(const Value: boolean);
begin
  fWaiting := Value;
end;

procedure syncVclCall(aProc: TThreadProcedure; DoWait: boolean);
begin
  if InsideMainThread then
    aProc()
  else
    TmaxThread.DoSync(aProc, DoWait)
end;

function TThreadData.GetStartSignal: iSignal;
begin
  Result := fStartSignal;
end;

procedure TThreadData.SetThreadToTerminated;
begin
  fThread.terminate;
end;

procedure TThreadData.SetDefaultValues;
begin
  fTaskName := '';
  fKeepAlive := True;
  fFinished := False;
  fWaiting := False;
  FProc := nil;

  fStartSignal.SetNonSignaled;
  fReadySignal.SetNonSignaled;
  FWakeSignal.SetNonSignaled;

  {$IFDEF MsWindows}
  if assigned(fThread) then
    self.fThread.Priority := TThreadPriority.tpNormal;
  {$ENDIF}
end;

{ TFindInSortedList<T> }

constructor TFindInSortedList<t>.Create(aList: TList<t>;
  aComparer:
  iComparer<t>);
begin
  inherited Create;
  fList := aList;
  fComparer := aComparer;

end;

function TFindInSortedList<t>.find(
  const
  Value:
  t;
  out Index: integer): boolean;

var
  l, H, i, c: integer;
begin
  Result := False;
  l := 0;
  H := fList.Count - 1;
  while l <= H do
  begin
    i := (l + H) shr 1;
    c := fComparer.compare(fList[i], Value);
    if c < 0 then
      l := i + 1
    else
    begin
      H := i - 1;

      if c = 0 then
      begin
        Result := True;
        Index := i;
        exit;
      end;
    end;
  end;
  Index := l;
end;

{ TSafeValue<T> }

procedure TSafeValue<t>.EnsureInit;
begin
  if fSec = nil then
    fSec := tInterfacedCriticalSection.Create;
end;

function TSafeValue<t>.getValue: t;
begin
  EnsureInit;
  fSec.Enter;
  try
    Result := fValue;
  finally
    fSec.Leave;
  end;
end;

class function TSafeValue<t>.new: TSafeValue<t>;
begin
  Result := default(TSafeValue<t>);
  Result.EnsureInit;
end;

procedure TSafeValue<t>.Setvalue(const aValue: t);
begin
  EnsureInit;
  fSec.Enter;
  try
    fValue := aValue;
  finally
    fSec.Leave;
  end;
end;

{ tInterfacedCriticalSection }

constructor tInterfacedCriticalSection.Create;
begin
  inherited Create;
  fSec := TFixedCriticalSection.Create;
end;

destructor tInterfacedCriticalSection.Destroy;
begin
  fSec.Free;
  inherited;
end;

procedure tInterfacedCriticalSection.Enter;
begin
  fSec.Enter;
end;

procedure tInterfacedCriticalSection.Leave;
begin
  fSec.Leave;
end;

{ TAsyncLoop }

constructor TAsyncLoop.Create;
begin
  inherited Create;
  FSimultanousThreadCount := TmaxAsyncGlobal.NumberOfProcessors * 4 - 2 { for the main VCL Thread };
  fReadySignal := TSignal.Create;
  fReadySignal.SetSignaled;
end;

procedure TAsyncLoop.Execute(aMin, aMax: integer; aProc: TAsyncEnumProc);
var
  X: integer;
begin
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

  fActivThreadCount := length(fThreads);

  for X := 0 to length(fThreads) - 1 do
    fThreads[X] := SimpleAsyncCall(
      procedure
      begin
        asyncLoopIteration;
      end, classname + IntToStr(X));

end;

class procedure TAsyncLoop.Run(const aMin, aMax: integer; aProc: TAsyncEnumProcWithCancel; OnDone: TProc; ThreadCount: integer);
var
  loop: TAsyncLoop;
begin
  loop := TAsyncLoop.Create;
  if ThreadCount <> 0 then
    loop.SimultanousThreadCount := ThreadCount;

  loop.OnFinished :=
    procedure
  begin
    loop.Free;
    OnDone;
  end;

  loop.Execute(aMin, aMax,
    procedure(i: integer)
    var
      aCancel: boolean;
    begin
      aCancel := False;
      aProc(i, aCancel);
      if aCancel then
        loop.Cancel;
    end);

end;

class procedure TAsyncLoop.RunAndWait(const aMin, aMax: integer; aProc: TAsyncEnumProcWithCancel; ThreadCount: integer);
var
  loop: TAsyncLoop;
begin
  loop := TAsyncLoop.Create;
  if ThreadCount <> 0 then
    loop.SimultanousThreadCount := ThreadCount;

  loop.Execute(aMin, aMax,
    procedure(i: integer)
    var
      aCancel: boolean;
    begin
      aCancel := False;
      aProc(i, aCancel);
      if aCancel then
        loop.Cancel;
    end);

  loop.WaitFor;
  loop.Free;
end;

procedure TAsyncLoop.WaitFor;
begin
  fReadySignal.WaitForSignaled;
end;

procedure TAsyncLoop.SetOnFinished(

  const
  Value:
  TThreadProcedure);
begin
  FOnFinished := Value;
end;

procedure TAsyncLoop.SetSimultanousThreadCount(

  const
  Value:
  integer);
begin
  FSimultanousThreadCount := Value;
end;

destructor TAsyncLoop.Destroy;
var
  X: integer;
begin
  WaitFor;
  for X := 0 to length(fThreads) - 1 do
    if fThreads[X] <> nil then
    begin
      fThreads[X].WaitFor;
      fThreads[X] := nil;
    end;
  fThreads := nil;
  fReadySignal := nil;
  inherited;
end;

procedure TAsyncLoop.asyncLoopIteration;
var
  Index: integer;
  CallFinished: boolean;
begin
  repeat
    CallFinished := False;
    Index := TInterlocked.Increment(fCurIndex) - 1;
    if Index > fMax then
      if TInterlocked.Decrement(fActivThreadCount) = 0 then
        CallFinished := True;

    if CallFinished then
    begin
      fReadySignal.SetSignaled;
      if assigned(FOnFinished) then
        FOnFinished();
    end
    else if Index <= fMax then
    begin
      FProc(Index);
    end;
  until Index > fMax;
end;

procedure TAsyncLoop.Cancel;
begin
  TInterlocked.add(fCurIndex, fMax + 1);
end;

{ TmaxAsyncGlobal }

class constructor TmaxAsyncGlobal.CreateClass;
begin
  {$IFDEF MsWindows}
  InitializeCriticalSection(fCS);
  {$ELSE}
  fCS := TCriticalSection.Create;
  {$ENDIF}
  {$IFDEF madExcept}
  HideLeak(@fCS);
  {$ENDIF}
  FNumberOfProcessors := TThread.ProcessorCount;
  fWaitingThreadDataList := nil;
  fAllThreads := TList<TThreadData>.Create;
end;

class destructor TmaxAsyncGlobal.Destroyclass;
var
  X: integer;
begin
  fCS.Enter;
  try
    fGlobalThreadListDestroyed := True;
    if assigned(fWaitingThreadDataList) then
    begin
      for X := 0 to fWaitingThreadDataList.Count - 1 do
      begin
        ReleaseThreadData(fWaitingThreadDataList[X]);
        fWaitingThreadDataList[X] := nil;
      end;
      fWaitingThreadDataList.Clear;
      fWaitingThreadDataList.Free;
      fWaitingThreadDataList := nil;
    end;

    fAllThreads.Free;
  finally
    fCS.Leave;
  end;
  // leave it on...
  // deleteCriticalSection(fCS);
end;

class function TmaxAsyncGlobal.GetThreadData: iThreadData;
var
  i: integer;
begin
  Result := nil;
  fCS.Enter;
  try
    // if there are some threads waiting... use them... otherwise we will need to create a new one
    if (assigned(fWaitingThreadDataList)) and (fWaitingThreadDataList.Count <> 0) then
    begin
      i := fWaitingThreadDataList.Count - 1;
      Result := fWaitingThreadDataList[i];
      fWaitingThreadDataList[i] := nil;
      fWaitingThreadDataList.Delete(i);

      Result.SetDefaultValues;
    end;
  finally
    fCS.Leave;
  end;

  if Result = nil then
    Result := TThreadData.Create;
end;

class procedure TmaxAsyncGlobal.AddToWaiting(ThreadData: iThreadData);
begin
  fCS.Enter;
  try
    if fGlobalThreadListDestroyed then
    begin
      ReleaseThreadData(ThreadData);
      ThreadData := nil;
    end
    else
    begin
      if not assigned(fWaitingThreadDataList) then
        fWaitingThreadDataList := TList<iThreadData>.Create;
      fWaitingThreadDataList.add(ThreadData);
    end;
  finally
    fCS.Leave;
  end;
end;

class procedure TmaxAsyncGlobal.ReleaseThreadData(const aThreadData: iThreadData);
begin
  aThreadData.SetThreadToTerminated;
  aThreadData.KeepAlive := False;
  aThreadData.WakeUpSignal.SetSignaled;
  aThreadData.StartSignal.SetSignaled;
end;

class procedure TmaxAsyncGlobal.addToAllThreadList(aData: TThreadData);
begin
  if fGlobalThreadListDestroyed then
    exit;

  fCS.Enter;
  try
    if not fGlobalThreadListDestroyed then
      fAllThreads.add(aData);

  finally
    fCS.Leave;
  end;
end;

class procedure TmaxAsyncGlobal.removeFromAllThreadList(aData: TThreadData);
var
  i: integer;
begin
  if fGlobalThreadListDestroyed then
    exit;

  fCS.Enter;
  try
    if not fGlobalThreadListDestroyed then
    begin
      i := fAllThreads.IndexOf(aData);
      if i <> -1 then
        fAllThreads.Delete(i);
    end;
  finally
    fCS.Leave;
  end;
end;

class function TmaxAsyncGlobal.GetThreadName(const aThreadId: TThreadId): string;
var
  X: integer;
begin
  Result := '';
  if fGlobalThreadListDestroyed then
    exit;

  if aThreadId = MainThreadID then
    exit('MainVclThread');

  fCS.Enter;
  try
    if not fGlobalThreadListDestroyed then
    begin
      for X := 0 to fAllThreads.Count - 1 do
        if fAllThreads[X].fThreadId = aThreadId then
        begin
          Result := fAllThreads[X].TaskName;
          break;
        end;
    end;
  finally
    fCS.Leave;
  end;
end;

{ TWaiter }

{$IFDEF MsWindows}{$IFNDEF CONSOLE}

class function TWaiter.WaitFor(const async: TArray<iAsync>; Milliseconds: dword = infinite; DoProcessMessages: boolean = False): boolean;
var
  events: TArray<TEvent>;
  X: integer;
begin
  SetLength(events, length(async));
  for X := 0 to length(async) - 1 do
    events[X] := (async[X] as iAsyncIntern).GetThreadData.ReadySignal.Event;

  Result := WaitFor(events, Milliseconds, DoProcessMessages);
end;

class function TWaiter.WaitFor(const Signals: TArray<iSignal>; Milliseconds: dword; DoProcessMessages: boolean): boolean;
var
  events: TArray<TEvent>;
  X: integer;
begin
  SetLength(events, length(Signals));
  for X := 0 to length(Signals) - 1 do
    events[X] := Signals[X].Event;

  Result := WaitFor(events, Milliseconds, DoProcessMessages);
end;

class function TWaiter.WaitFor(const aEvents: TArray<TEvent>; Milliseconds: dword; DoProcessMessages: boolean): boolean;
const
  MAXCOUNT = Winapi.Windows.MAXIMUM_WAIT_OBJECTS - 1;

  procedure Delete(var a: TArray<THandle>; Index, Count: integer);
  var
    ItemsOnTheRight, len: integer;
  begin
    len := length(a);
    ItemsOnTheRight := len - ((Index + Count));
    if ItemsOnTheRight > 0 then
    begin
      move(
        a[Index + Count],
        a[Index],
        SizeOf(THandle) * ItemsOnTheRight);
    end;

    SetLength(a, len - Count);
  end;

var
  X: integer;
  Count: dword;
  SignalState: dword;
  StartTime: dword;
  orgTimeOutMilliseconds: dword;
  events: TArray<THandle>;
  st: TStopWatch;
begin
  if DoProcessMessages and (not InsideMainThread) then
    DoProcessMessages := False;

  orgTimeOutMilliseconds := Milliseconds;
  st := TStopWatch.startNew;

  Count := length(aEvents);
  if Count = 0 then
    exit(True);

  SetLength(events, Count);
  move(aEvents[0], events[0], Count * SizeOf(THandle));

  while length(events) > 0 do
  begin
    Count := Min(MAXCOUNT, length(events));

    if not DoProcessMessages then
    begin
      SignalState := WaitForMultipleObjects(Count, @events[0], True, Milliseconds);
      if SignalState in [WAIT_OBJECT_0..WAIT_OBJECT_0 + Count - 1] then
        Delete(events, 0, Count);

    end
    else
    begin

      SignalState := MsgWaitForMultipleObjects(Count, events[0], False, Milliseconds, QS_ALLINPUT);
      // GUI message
      if SignalState = WAIT_OBJECT_0 + Count then
        application.ProcessMessages
      else if SignalState in [WAIT_OBJECT_0..WAIT_OBJECT_0 + Count - 1] then
        Delete(events, SignalState - WAIT_OBJECT_0, 1);
    end;

    // check timeout
    if (Milliseconds <> infinite) and (length(events) <> 0) then
    begin
      // if the timeOut is set, and is due, then exit
      if (orgTimeOutMilliseconds <= st.ElapsedMilliseconds) then
      begin
        events := nil;
        exit(False);
      end;

      // now calculate the new timeout for the WaitForMultipleObjects proc, take the time already passed into account, but wait at least 1 millisecond
      Milliseconds := Max(1, orgTimeOutMilliseconds - (st.ElapsedMilliseconds));
    end;

    // end of while loop
  end;
  Result := True;
end;
{$ENDIF}{$ENDIF}

{ TAsyncCollectionProcessor<T> }

constructor TAsyncCollectionProcessor<t>.Create;
begin
  inherited Create;
  {$IFDEF MsWindows}
  fCriticalSection := TFixedRtlCriticalSection.Create;
  {$ELSE}
  fCriticalSection := TFixedCriticalSection.Create;
  {$ENDIF}
  fItems := TQueue<t>.Create;

  fThreads := TList<TThreadHolder>.Create;

  FSimultanousThreadCount := TmaxAsyncGlobal.NumberOfProcessors * 4 - 2 { for the main VCL Thread };
  fReadySignal := TSignal.Create;
  fReadySignal.SetSignaled;
end;

destructor TAsyncCollectionProcessor<t>.Destroy;
begin
  ClearItems;
  WaitFor;
  fItems.Free;
  fThreads.Free;
  fCriticalSection.Free;
  inherited;
end;

procedure TAsyncCollectionProcessor<t>.add(const Items: TList<t>);
var
  Item: t;
begin
  fCriticalSection.Enter;
  try
    for Item in Items do
      fItems.enQueue(Item);
  finally
    fCriticalSection.Leave;
  end;
  RestartThreads;

end;

procedure TAsyncCollectionProcessor<t>.add(const Items: TArray<t>);
var
  Item: t;
begin
  fCriticalSection.Enter;
  try
    for Item in Items do
      fItems.enQueue(Item);
  finally
    fCriticalSection.Leave;
  end;
  RestartThreads;
end;

procedure TAsyncCollectionProcessor<t>.add(const Item: t);
begin
  fCriticalSection.Enter;
  try
    fItems.enQueue(Item);
  finally
    fCriticalSection.Leave;
  end;
  RestartThreads;
end;

procedure TAsyncCollectionProcessor<t>.ClearItems;
begin
  fCriticalSection.Enter;
  try
    fItems.Clear;
  finally
    fCriticalSection.Leave;
  end;
end;

procedure TAsyncCollectionProcessor<t>.SetOnFinished(const Value: TThreadProcedure);
begin
  FOnFinished := Value;
end;

procedure TAsyncCollectionProcessor<t>.SetProc(const Value: TAsyncCollectionProcessorProc<t>);
begin
  FProc := Value;
end;

procedure TAsyncCollectionProcessor<t>.SetSimultanousThreadCount(const Value: integer);
begin
  if FSimultanousThreadCount <> Value then
  begin
    FSimultanousThreadCount := Value;
    RestartThreads;
  end;
end;

procedure TAsyncCollectionProcessor<t>.WaitFor;
begin
  fReadySignal.WaitForSignaled;
end;

procedure TAsyncCollectionProcessor<t>.RestartThreads;
begin
  fReadySignal.SetNonSignaled;
  fCriticalSection.Enter;
  try
    while fThreads.Count < FSimultanousThreadCount do
      fThreads.add(CreateThreadHolder);

  finally
    fCriticalSection.Leave;
  end;
end;

function TAsyncCollectionProcessor<t>.CreateThreadHolder: TThreadHolder;
var
  Id: int64;
begin
  // this is already in a critical section, no need to lock it again
  Result := default(TThreadHolder);
  {$R-}{$Q-}
  Id := TInterlocked.Increment(fNextThreadId);
  Result.Id := Id;
  Result.async := SimpleAsyncCall(
    procedure
    begin
      AsyncProcessItem(Id);
    end, classname + 'Worker')
end;

procedure TAsyncCollectionProcessor<t>.AsyncProcessItem(const ThreadId: int64);
var
  X: integer;
  Item: t;
  IsLast, NoItem: boolean;
begin
  repeat
    IsLast := False;
    NoItem := False;
    Item := default(t); // prevent 'variable might not be initialized' compiler warning

    fCriticalSection.Enter;
    try
      if fItems.Count <> 0 then
      begin
        Item := fItems.dequeue;
        NoItem := False;
      end
      else
      begin
        NoItem := True;

        for X := 0 to fThreads.Count - 1 do
          if fThreads[X].Id = ThreadId then
          begin
            fThreads.Delete(X);
            break;
          end;
        IsLast := fThreads.Count = 0
      end;

    finally
      fCriticalSection.Leave;
    end;

    if NoItem and IsLast then
    begin
      try
        if assigned(FOnFinished) then
          FOnFinished();
      finally
        fReadySignal.SetSignaled;
      end;
    end
    else if not NoItem then
      FProc(Item);
  until NoItem;
end;

function TmaxAsync.GetThreadData: iThreadData;
begin
  Result := fThreadData;
end;

{$IFDEF MsWindows}

function TmaxAsync.GetThreadPriority: TThreadPriority;
begin
  Result := self.fThreadData.Thread.Priority;
end;

{$IFDEF MsWindows}

procedure TmaxAsync.MsgWaitFor;
begin
  while not fThreadData.Finished do
    fThreadData.ReadySignal.MsgWaitForSignaled;
end;
{$ENDIF}

{$ENDIF}
{ TUserData }

constructor TUserData<t>.Create(const aValue: t);
begin
  inherited Create;
  fValue := aValue;
  fLock := default(TLockFreeLock);
end;

function TUserData<t>.getValue: t;
begin
  fLock.lock;
  try
    Result := fValue;
  finally
    fLock.unLock;
  end;
end;

procedure TUserData<t>.Setvalue(const aValue: t);
begin
  fLock.lock;
  try
    fValue := aValue;
  finally
    fLock.unLock;
  end;
end;

{ TRTLCriticalSectionHelper }
{$IFDEF MsWindows}

class function TRTLCriticalSectionHelper.Create: TRTLCriticalSection;
begin
  InitializeCriticalSection(Result);
end;

procedure TRTLCriticalSectionHelper.Enter;
begin
  EnterCriticalSection(self);
end;

procedure TRTLCriticalSectionHelper.exit;
begin
  LeaveCriticalSection(self);
end;

procedure TRTLCriticalSectionHelper.Free;
begin
  DeleteCriticalSection(self);
end;

procedure TRTLCriticalSectionHelper.Leave;
begin
  LeaveCriticalSection(self);
end;
{$ENDIF}

{ TObjectWithCriticalSection }

constructor TObjectWithCriticalSection.Create;
begin
  inherited Create;
  {$IFDEF MsWindows}
  fCriticalSection := TFixedRtlCriticalSection.Create;
  {$ELSE}
  fCriticalSection := TFixedCriticalSection.Create;
  {$ENDIF}
end;

destructor TObjectWithCriticalSection.Destroy;
begin
  fCriticalSection.Free;
  inherited;
end;

procedure TObjectWithCriticalSection.lock;
begin
  fCriticalSection.Enter;
end;

procedure TObjectWithCriticalSection.unLock;
begin
  fCriticalSection.Leave;
end;

{ TFixedRtlCriticalSection }

{$IFDEF MsWindows}

class function TFixedRtlCriticalSection.Create: TFixedRtlCriticalSection;
begin
  Result.Dummy := default(TDummy);
  Result.CriticalSection := TRTLCriticalSection.Create;
end;

procedure TFixedRtlCriticalSection.Enter;
begin
  CriticalSection.Enter;
end;

procedure TFixedRtlCriticalSection.exit;
begin
  CriticalSection.Leave;
end;

procedure TFixedRtlCriticalSection.Free;
begin
  CriticalSection.Free;
end;

procedure TFixedRtlCriticalSection.Leave;
begin
  CriticalSection.Leave;
end;
{$ENDIF}

{ TLockFreeLock }

procedure TLockFreeLock.lock;
var
  Succeeded: boolean;
begin
  repeat
    TInterlocked.CompareExchange(fLocked, 1, 0, Succeeded);
  until Succeeded;
end;

procedure TLockFreeLock.unLock;
begin
  // if we acquired the lock, then we do not need to wait or test the value
  TInterlocked.Exchange(fLocked, 0);
end;

{ TAtomic }

class constructor TAtomic.Create;
type
  TTestRec = record
    a: byte;
    b: int64;
  end;

begin
  // In the {$A8} or {$A+} state, fields in record types that are declared without the packed modifier and fields in class structures are aligned on quadword boundaries.
  {$IF SIZEOF(TTestRec) <> 16}
  'App must be compiled in A8 mode'
    {$IFEND}
end;

{ TAsyncTimer }

procedure TAsyncTimer.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if Value then
      fSignal.SetSignaled;
  end;
end;

constructor TAsyncTimer.Create(aProc: TProc; aInterval: integer; aEnabled: boolean);
begin
  inherited Create;
  FInterval := aInterval;
  fSignal := TSignal.Create;
  fSignal.SetNonSignaled;
  FProc := aProc;
  FEnabled := aEnabled;
  fAsync := SimpleAsyncCall(asyncLoop)
end;

destructor TAsyncTimer.Destroy;
begin
  FEnabled := False;
  FTerminated := True;
  fSignal.SetSignaled;
  fAsync.WaitFor;
  inherited;
end;

procedure TAsyncTimer.asyncLoop;
begin
  repeat
    fSignal.WaitForSignaled(FInterval);
    if FEnabled then
      FProc();
    fSignal.SetNonSignaled;

  until FTerminated;
end;

procedure TAsyncTimer.SetInterval(const Value: integer);
begin
  FInterval := Value;
  fSignal.SetSignaled;
end;

initialization

  {$IFDEF madExcept}
  madExcept.NameThread(GetCurrentThreadId, 'MainVclThread');
  {$ENDIF}
  System.NeverSleepOnMMThreadContention := True;

end.

