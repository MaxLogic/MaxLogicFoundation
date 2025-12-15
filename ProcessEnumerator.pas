unit ProcessEnumerator;
{
Example of usage:


procedure SetProcessList(var List: TStringList; ShowAll: Boolean);
var
Processes: TProcessList;
Process : TProcess;
i, j : Integer;
begin
try
Processes := TProcessList.Create;
for i:= 0 to Processes.Count -1 do
begin
Process := Processes[i];
if ShowAll then
begin // incl. DLL's ...
List.Add(Process.FileName) ;
for j:=0 to Process.Count-1 do
begin
List.Add(Process.Modules[j].FileName + ':' + Process.Modules[j].FilePath) ;
end;
end
else
begin
List.Add(Process.FileName + ':' + Process.FilePath);
end;
end;
finally
Processes.Free;
end;
end;


}
interface

uses
  Windows, Classes, SysUtils, ComCtrls, Dialogs, controls, messages;

type
  // NT
  PHInst = ^HInst;

  TQueryFullProcessImageName = function(hProcess: THandle; flags: dword; Filename: pAnsiChar; var BufferLen: dword): boolean;stdcall;
  TEnumProcesses = function(pidList : PInteger; cb : integer;
    var cbNeeded : integer) : boolean; stdcall;
  TEnumProcessModules = function(hProcess : THandle; moduleList : PHInst; cb : integer;
    var cbNeeded : integer) : boolean; stdcall;
  TGetModuleBaseName = function(hProcess : THandle; module : HInst; BaseName : PAnsiChar;
    size : integer) : integer; stdcall;
  TGetModuleFileName = function(hProcess : THandle; module : HInst; FileName : PAnsiChar;
    size : integer) : integer; stdcall;

{ Describes an entry from a list that enumerates the processes residing in
the system address space when a snapshot was taken }
  TProcessEntry32 = packed record
    dwSize :     DWORD;
    cntUsage :   DWORD;
    th32ProcessID : DWORD; // this process
    th32DefaultHeapID : DWORD;
    th32ModuleID : DWORD; // associated exe
    cntThreads : DWORD;
    th32ParentProcessID : DWORD; // This process's parent process
    pcPriClassBase : longint;    // Base priority of process's threads
    dwFlags :    DWORD;
    szExeFile :  array[0..MAX_PATH - 1] of Ansichar;// path
  end;

{ Describes an entry from a list that enumerates the modules used by a
specified process }
  TModuleEntry32 = packed record
    dwSize :      DWORD;
    th32ModuleID : DWORD;  // This module
    th32ProcessID : DWORD; // owning process
    GlblcntUsage : DWORD;  // Global usage count on the module
    ProccntUsage : DWORD;  // Module usage count in th32ProcessID's context
    modBaseAddr : PBYTE;   // Base address of module in th32ProcessID's context
    modBaseSize : DWORD;   // Size in bytes of module starting at modBaseAddr
    hModule :     HMODULE; // The hModule in th32ProcessID's context
    szModule :    array[0..255] of Ansichar;
    szExePath :   array[0..MAX_PATH - 1] of ansichar;
  end;

  // 9x
  TCreateToolhelp32Snapshot = function(dwFlags, th32ProcessID : DWORD) : THandle; stdcall;
  TProcess32First = function(hSnapshot : THandle; var lppe : TProcessEntry32) : BOOL; stdcall;
  TProcess32Next = function(hSnapshot : THandle; var lppe : TProcessEntry32) : BOOL; stdcall;
  TModule32First = function(hSnapshot : THandle; var lpme : TModuleEntry32) : BOOL; stdcall;
  TModule32Next = function(hSnapshot : THandle; var lpme : TModuleEntry32) : BOOL; stdcall;

  { Information holder for a module attached to a Process }
  TModule = class(TObject)
  private
    sFileName : string;
    sFilePath : string;
  public
    constructor Create(ModuleName : string);
    property FileName : string Read sFileName;
    property FilePath : string Read sFilePath;
  end;

  { Information holder a Process }
  TProcess = class(TObject)
  private
    hProcess :   THandle;
    ProcessID :  DWord;
    ModuleList : TList;
    sFileName :  string;
    sFilePath :  string;
  protected
    procedure EnumModules95;
    procedure EnumModulesNT;
    function GetCount : integer;
    function GetModule(Index : integer) : TModule;
    procedure SetPriority(NewPriority : integer);
    function GetPriority : integer;
    procedure Clear;
  public
    constructor Create;
    constructor Create9x(ProcessEntry32 : TProcessEntry32);
    constructor CreateNT(NewProccessID : DWord);
    destructor Destroy; override;
    procedure Refresh;
    function KillProcess : boolean;
    property Count : integer Read GetCount;
    property Modules[Index : integer] : TModule Read GetModule; default;
    property FileName : string Read sFileName;
    property FilePath : string Read sFilePath;
    property Priority : integer Read GetPriority Write SetPriority;
    property ID : DWord Read ProcessID;
  end;

  { Holder for a list of Processes }
  TProcessList = class(TObject)
  private
    ProcessList : TList;
  protected
    procedure EnumProcesses95;
    procedure EnumProcessesNT;
    function GetCount : integer;
    function GetProcess(Index : integer) : TProcess;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Refresh;
    procedure Clear;

    function IsRunning(const FileName : string) : Boolean;overload;
    function IsRunning(const FileName : string; out ExeProcessId : dword) : Boolean;overload;

    property Count : integer Read GetCount;
    property Process[Index : integer] : TProcess Read GetProcess; default;
  end;

const
  TH32CS_SNAPHEAPLIST = $00000001;
  TH32CS_SNAPPROCESS  = $00000002;
  TH32CS_SNAPTHREAD   = $00000004;
  TH32CS_SNAPMODULE   = $00000008;
  TH32CS_SNAPALL      = TH32CS_SNAPHEAPLIST or TH32CS_SNAPPROCESS or TH32CS_SNAPTHREAD or
    TH32CS_SNAPMODULE;
  PROCESS_TERMINATE   = $0001;
  PROCESS_VM_READ     = $0010;
  PROCESS_SET_INFORMATION = $0200;
  PROCESS_QUERY_INFORMATION = $0400;

var
  { Windows NT Method Pointers }
  hPSAPI : THandle = 0;
  EnumProcessModules : TEnumProcessModules;
  GetModuleBaseName : TGetModuleBaseName;
  GetModuleFileName : TGetModuleFileName;
  EnumProcesses : TEnumProcesses;
  QueryFullProcessImageName : TQueryFullProcessImageName;
  { Windows 9x Method Pointers }

  CreateToolhelp32Snapshot : TCreateToolhelp32Snapshot;
  Process32First : TProcess32First;
  Process32Next : TProcess32Next;
  Module32First : TModule32First;
  Module32Next : TModule32Next;

implementation



{
Name: ValidProcessID
Description: Checks a ProcessID for validity (It correctly identifies a
valid running process).
Parameters: ProcessID, The ID of the process to validate
Return : If the function returns true the ProcessID is Valid.
if the function returns false the ProcessID is Invalid.
}
function ValidProcessID(ProcessID : DWord) : boolean;
var
  hProcess : THandle;
begin
  if ProcessID = 0 then
    Result := false
  else begin
    hProcess := OpenProcess(PROCESS_QUERY_INFORMATION, False, ProcessID);
    Result   := hProcess <> 0; { If hProcess is = 0 the OpenProcess Failed }
    CloseHandle(hProcess);
  end;
end;

{
Name: KernelLoaded
Description: Retieves the method pointers (if not already loaded) for
the Win9x ToolHelp Methods in Kernel32.DLL. This function
will always fail on Windows NT.
Return : If the function returns true the ToolHelp methods of Kernel32.DLL
where correctly loaded.
if the function returns false the methods were not loaded.
}
function KernelLoaded : boolean;
var
  hLib : THandle;
begin
  Result := False;
  hLib   := GetModuleHandle(kernel32);
  if hLib <> 0 then
  begin
  @QueryFullProcessImageName := GetProcAddress(hLib, 'QueryFullProcessImageName');
       @CreateToolhelp32Snapshot :=
      GetProcAddress(hLib, 'CreateToolhelp32Snapshot');
    @Process32First := GetProcAddress(hLib, 'Process32First');
    @Module32First := GetProcAddress(hLib, 'Module32First');
    @Module32Next := GetProcAddress(hLib, 'Module32Next');
    @Process32Next := GetProcAddress(hLib, 'Process32Next');
    Result := (@CreateToolhelp32Snapshot <> nil) and (@Process32First <> nil) and
      (@Module32First <> nil) and (@Module32Next <> nil) and (@Process32Next <> nil);
  end;
end;

{
Name: PSAPILoaded
Description: Retieves the method pointers (if not already loaded) for
the enumeration Methods in PSAPI.DLL. This function
will always fail on Windows 9x.
Return : If the function returns true the enumerate methods of PSAPI.DLL
where correctly loaded.
if the function returns false the methods were not loaded.
}
function PSAPILoaded : boolean;
begin
  Result := True;
  if hPSAPI = 0 then
  begin
    hPSAPI := LoadLibrary('PSAPI.dll');
    if hPSAPI < 32 then
    begin
      hPSAPI := 0;
      raise Exception.Create('Error Loading PSAPI.DLL, ' + SysErrorMessage(GetLastError));
    end;
    @EnumProcesses := GetProcAddress(hPSAPI, 'EnumProcesses');
    @EnumProcessModules := GetProcAddress(hPSAPI, 'EnumProcessModules');
    @GetModuleBaseName := GetProcAddress(hPSAPI, 'GetModuleBaseNameA');
    @GetModuleFileName := GetProcAddress(hPSAPI, 'GetModuleFileNameExA');
    Result := (@EnumProcesses <> nil) and (@EnumProcessModules <> nil) and
      (@GetModuleBaseName <> nil) and (@GetModuleFileName <> nil);
  end;
end;

{
Name: TProcessList.Clear
Description: Clears and frees all TProcess objects.
}
procedure TProcessList.Clear;
var
  Counter : integer;
begin
  for Counter := ProcessList.Count - 1 downto 0 do
    TObject(ProcessList[counter]).Free;
  ProcessList.Clear;
end;

{
Name: TProcessList.Create
Description: Creates TProcessList object and fills the list with
all running processes.
}
constructor TProcessList.Create;
begin
inherited create;
  ProcessList := TList.Create;
  Refresh;
end;

{
Name: TProcessList.Destroy
Description: Destorys the TProcessList object and frees all memory and
objects
in th list.
}
destructor TProcessList.Destroy;
begin
  Clear;
  ProcessList.Free;
  inherited;
end;

{
Name: TProcessList.EnumProcesses95
Description: Enumerates all running processes and modules. Fills the list
with the TProcess objects.
}
procedure TProcessList.EnumProcesses95;
var
  hSnapshot :      THandle;
  ProcessEntry :   TProcessEntry32;
  ProcessNotDone : BOOL;
begin
  hSnapshot := 0;
  if KernelLoaded then
  begin
    try
      // Grab a snapshot of all running processes
      hSnapshot      := CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0);
      ProcessEntry.dwSize := sizeof(TProcessEntry32);
      // Get the information on the first process in the SnapShot
      ProcessNotDone := Process32First(hSnapshot, ProcessEntry);
      // Keep stepping through all processes until finished
      while ProcessNotDone do
      begin
        // Add Process to List
        ProcessList.Add(TProcess.Create9x(ProcessEntry));
        // Get information on next process
        ProcessNotDone := Process32Next(hSnapshot, ProcessEntry);
      end;
    finally
      // Ensure handle to snapshot is closed to prevent a memory leak
      CloseHandle(hSnapshot);
    end;
  end;
end;

{
Name: TProcessList.EnumProcessesNT
Description: Enumerates all running processes and modules. Fills the list
with the TProcess objects.
}
procedure TProcessList.EnumProcessesNT;
var
  pid: dword;
  p: Pointer;
  aProcesses : array[0..102400] of DWord;
  cProcesses : DWord;
  cbNeeded, I : integer;
  cb : integer;
begin
  // Check PSAPI.DLL is loaded
  if PSAPILoaded then
  begin
    // Fill aProcesses array with all ProcessID's
    EnumProcesses(@aProcesses[0], SizeOf(aProcesses), cbNeeded);
    // Calculate the actual number of ProcessID's, Remembering that CbNeeded
    // holds the size in bytes required to hold all ProcessID's. Because a
    // Process ID is 4 Bytes in size we can Div cbNeeded by 4 to get the
    // actual count of processes.
    // Loop through the returned ProcessID's and create a TProcess Object for
    // each one. Check the ID for validity first as NT seems to return invalid
    // ProcessID's sometimes
    for I := 0 to (cbNeeded div 4) do
    if i < length(aProcesses) then
                                begin
      pid := aProcesses[I];
      if ValidProcessID(pid) then
      begin
        Try
          p  := TProcess.CreateNT(pid);
          ProcessList.Add(p);
        Except

        End;
      end;
    end;
  end;
end;

{
Name: TProcessList.GetCount
Description: Retrieves the number of TProcess objects in the list
Return : Number of TProcess Objects.
}
function TProcessList.GetCount : integer;
begin
  Result := ProcessList.Count;
end;

{
Name: TProcessList.GetCount
Description: Retrieves a TProcess object from the List.
Parameter: Index, The index of the required TProcess Object.
Return : The requested TProcess Objects or nil if an invalid
index was requested.
}
function TProcessList.GetProcess(Index : integer) : TProcess;
begin
  Result := nil;
  if (Index >= 0) and (Index <= ProcessList.Count) then
    Result := TProcess(ProcessList[Index]);
end;

function TProcessList.IsRunning(const FileName: string): Boolean;
var
  d : dword;
begin
  Result := IsRunning(FileName, d)
end;

function TProcessList.IsRunning(const FileName : string; out ExeProcessId : dword) :
Boolean;
var
  Process : TProcess;
  i, j : Integer;
  ExePath, Exename : string;
begin
  Result := false;
  Exepath := AnsiUpperCase(ExtractFilepath(FileName));
  ExeName := AnsiUpperCase(ExtractFileName(FileName));


    for i:= 0 to Self.Count -1 do
    begin
      Process := self.Process[i];
      if ExeName = AnsiUpperCase(Process.FileName) then
        if ExePath = AnsiUpperCase(Process.FilePath) then
        begin
          ExeProcessId := Process.ProcessID;
          Result := True;
          Break;
        end;
    end;
end;

{
Name: TProcessList.Refresh
Description: Clear and the re-enumurate al processes. Any existing
references to an TProcess objects will become invalid.
This procedure will call the correct enumeration method
depending on the operating system.
}
procedure TProcessList.Refresh;
begin
  Clear;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    EnumProcessesNT
  else
    EnumProcesses95;
end;

{ TProcess }

{
Name: TProcess.Create
Description: Raises and exception as the function should not be
called. use CreateNT or Create9x instead.
Return : nil.
}
constructor TProcess.Create;
begin
  raise Exception.Create('Do not call create, use Create9x or CreateNT');
end;

{
Name: TProcess.CreateNT
Description: Creates a TProcess object and enumerates all attached modules.
Use this constructor only while running on Windows NT.
Return : TProcess Object.
}
constructor TProcess.CreateNT(NewProccessID : DWord);
var
  cbNeeded :  integer;
  TheModule : THandle;
  ModName :   array[0..MAX_PATH + 1] of ansichar;
  len: dword;
  ansi: ansistring;
begin
inherited create;

  ModuleList := TList.Create;
  ProcessID  := NewProccessID;
  hProcess   := OpenProcess(PROCESS_QUERY_INFORMATION {OR PROCESS_VM_READ}, False, ProcessID);
  if hProcess = 0 then
  begin
    raise Exception.Create('Bad Process ID');
  end;

  len := MAX_PATH;
  SetLength(ansi, len+1);
  if @QueryFullProcessImageName <> NIL then
                                       if QueryFullProcessImageName(hProcess, 0, pAnsiChar(ansi), len) then
  begin
  inc(len, 10);
  SetLength(ansi, len);
    QueryFullProcessImageName(hProcess, 0, pAnsiChar(ansi), len) ;
    SetLength(ansi, len);
  end;

  { Extract information on this process }

  if EnumProcessModules(hProcess, @TheModule, SizeOf(TheModule), cbNeeded) then
  begin
    GetModuleFileName(hProcess, TheModule, ModName, SizeOf(ModName));
    sFileName := ExtractFileName(ModName);
    sFilePath := ExtractFilePath(ModName);
  end;
  Refresh;
end;

{
Name: TProcess.Create9x
Description: Creates a TProcess object and enumerates all attached modules.
Use this constructor only while running on Windows 9x.
Return : TProcess Object.
}
constructor TProcess.Create9x(ProcessEntry32 : TProcessEntry32);
begin
  ModuleList := TList.Create;
  ProcessID  := ProcessEntry32.th32ProcessID;
  sFileName  := ExtractFileName(ProcessEntry32.szExeFile);
  sFilePath  := ExtractFilePath(ProcessEntry32.szExeFile);
  hProcess   := OpenProcess(PROCESS_QUERY_INFORMATION + PROCESS_VM_READ, False, ProcessID);
  if hProcess = 0 then
    raise Exception.Create('Bad Process ID');
  Refresh;
end;

{
Name: TProcess.Destroy
Description: Destorys the TProcess object and frees all memory and objects
in the list.
}
destructor TProcess.Destroy;
begin
  if hProcess <> 0 then
    CloseHandle(hProcess);
  Clear;
  ModuleList.Free;
  inherited;
end;

{
Name: TProcess.EnumModules95
Description: Enumerates all modules associated with this process.
Fills the list with the TModule objects.
}
procedure TProcess.EnumModules95;
var
  hSnapshot :     THandle;
  ModuleEntry :   TModuleEntry32;
  ModuleNotDone : BOOL;
begin
  hSnapshot := 0;
  if KernelLoaded then
  begin
    try
      // Grab a snapshot of all running processes
      hSnapshot     := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, ProcessID);
      // Get the information on the first module in the current process
      ModuleEntry.dwSize := SizeOf(TModuleEntry32);
      ModuleNotDone := Module32First(hSnapshot, ModuleEntry);
      // Keep stepping through all modules until finished
      while ModuleNotDone do
      begin
        // Add the module to the list             szExePat h)
        ModuleList.Add(TModule.Create(ModuleEntry.szExePath));
        // Get information on next module
        ModuleNotDone := Module32Next(hSnapshot, ModuleEntry);
      end;
    finally
      // Ensure handle to snapshot is closed to prevent a memory leak
      CloseHandle(hSnapshot);
    end;
  end;
end;

{
Name: TProcess.EnumModulesNT
Description: Enumerates all modules associated with this process.
Fills the list with the TModule objects.
}
procedure TProcess.EnumModulesNT;
var
  cbNeeded : integer;
  cb :   integer;
  ModName : array[0..MAX_PATH + 1] of Ansichar;
  I :    integer;
  aMod : array[0..1024] of DWord;
begin
  // Fill aMod array with all Module Handles
  if EnumProcessModules(hProcess, @aMod[0], SizeOf(aMod), cbNeeded) then
  begin
  // Calculate the actual number of Module handles, Remembering that CbNeeded
  // holds the size in bytes required to hold all handles. Because a
  // handle is 4 Bytes in size we can Div cbNeeded by 4 to get the actual
  // count of Modules. And Loop through all Modules.
  for I := 0 to (cbNeeded div 4) do
  if i < length(amod) then
  begin
  try


    if GetModuleFileName(hProcess, aMod[I], ModName, SizeOf(ModName)) > 0 then
      ModuleList.Add(TModule.Create(ModName));
  except
sleep(0);
  end;
  end;
  end;
end;

{
Name: TProcess.GetCount
Description: Retrieves the number of TModule objects in the list
Return : Number of TModule Objects.
}
function TProcess.GetCount : integer;
begin
  Result := ModuleList.Count;
end;

{
Name: TProcess.GetModule
Description: Retrieves a TModule object from the List.
Parameter: Index, The index of the required TModule Object.
Return : The requested TModule Objects or nil if an invalid
index was requested.
}
function TProcess.GetModule(Index : integer) : TModule;
begin
  Result := nil;
  if (Index >= 0) and (Index <= ModuleList.Count) then
    Result := TModule(ModuleList[Index]);
end;

{
Name: TProcess.Clear
Description: Clears and frees all TModule objects.
}
procedure TProcess.Clear;
var
  Counter : integer;
begin
  for Counter := ModuleList.Count - 1 downto 0 do
    TObject(ModuleList[counter]).Free;
  ModuleList.Clear;
end;

{
Name: TProcess.Refresh
Description: Clear and the re-enumurate all modules. Any existing
references to an TModule objects will become invalid.
This procedure will call the correct enumeration method
depending on the operating system.
}
procedure TProcess.Refresh;
begin
  Clear;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    EnumModulesNT
  else
    EnumModules95;
end;

{
Name: TProcess.SetPriority
Description: Changes the Priority of the Process to NewPriority.
Parameter: NewPriority, The Priority value to set the process to.
}
procedure TProcess.SetPriority(NewPriority : integer);
var
  ExitCode :      DWORD;
  ProcessHandle : THandle;
begin
ProcessHandle := 0;
  try
    // Get a handle to the process with the required access rights
    ProcessHandle := OpenProcess(PROCESS_SET_INFORMATION, False, ProcessID);
    if ProcessHandle <> 0 then
      // Set the priority level
      SetPriorityClass(ProcessHandle, NewPriority);
    ;
  finally
  if ProcessHandle <> 0 then
      CloseHandle(ProcessHandle);
  end;
end;

{
Name: TProcess.GetPriority
Description: Gets the Priority level of the Process.
Result: The Priority value to of the process.
}
function TProcess.GetPriority : integer;
begin
  Result := GetPriorityClass(hProcess);
end;

{
Name: TProcess.KillProcess
Description: Performs a Terminate on the Process. This will force an
immediate termination, without loadded DLL's being unloaded
memory may not be freed and other genral havoc
Result: True is the process was successfully terminated.
}
function TProcess.KillProcess : boolean;
var
  ExitCode :      DWORD;
  ProcessHandle : THandle;
begin
  ProcessHandle := 0;
  Result := False;
  try
    // Get a handle to the process with the required access rights
    ProcessHandle := OpenProcess(PROCESS_TERMINATE, False, ProcessID);
    if ProcessHandle <> 0 then
      Result := TerminateProcess(ProcessHandle, 0);
  finally
  if ProcessHandle <> 0  then
      CloseHandle(ProcessHandle);
  end;
end;

{ TModule }

{
Name: TModule.Create
Description: Creates a TModule object.
Return : TModule Object.
}
constructor TModule.Create(ModuleName : string);
begin
  sFileName := ExtractFileName(ModuleName);
  sFilePath := ExtractFilePath(ModuleName);
end;

end.
