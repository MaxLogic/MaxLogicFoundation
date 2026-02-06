unit MaxLogic.Process;

interface

uses
  System.SysUtils,
  System.Types;

type
  TPid = Cardinal;
  TPidArray = TArray<TPid>;

  TCpuUsage = record
    /// <summary>
    /// CPU usage normalized to total system capacity (0..100 typically).
    /// Formula: SingleCorePercent / CPUCount
    /// </summary>
    OverallPercent: Double;

    /// <summary>
    /// CPU usage in "single-core percent".
    /// 100 = fully using one core; 200 = fully using two cores, etc.
    /// </summary>
    SingleCorePercent: Double;
  end;

function CurrentProcessId: TPid;
function CurrentThreadId: TPid;

function KillProcess(aPid: TPid): Boolean; overload;
function KillProcess(const aNamePattern: string): Integer; overload; // returns killed count

function FindPid(const aFileNamePattern: string): TPidArray;

function CpuUsage(aPid: TPid): TCpuUsage; overload;
function CpuUsage: TCpuUsage; overload;

function RamUsageBytes(aPid: TPid): UInt64; overload;
function RamUsageBytes: UInt64; overload;

implementation

uses

  {$IF Defined(MSWINDOWS)}
  Winapi.Windows,
  Winapi.TlHelp32,
  Winapi.PsAPI,
  {$ENDIF}
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.Masks;

{$IF Defined(MSWINDOWS) and not Declared(PROCESS_QUERY_LIMITED_INFORMATION)}
const
  // Vista+ access right. Some Delphi Winapi.Windows variants do not declare it.
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;
{$IFEND}

{$IF Defined(POSIX)}
uses
  Posix.Unistd,
  Posix.Signal,
  Posix.SysTypes;
{$ENDIF}

{$IF Defined(MACOS)}
// macOS libproc / proc_pid_rusage
type
  pid_t = Integer;

const
  RUSAGE_INFO_V2 = 2;

type
  // Minimal subset we need (layout must match; this is a best-effort binding)
  // Fields based on Apple docs + known struct layout from libproc bindings.
  rusage_info_v2 = record
    ri_uuid: array[0..15] of Byte;
    ri_user_time: UInt64;
    ri_system_time: UInt64;
    ri_pkg_idle_wkups: UInt64;
    ri_interrupt_wkups: UInt64;
    ri_pageins: UInt64;
    ri_wired_size: UInt64;
    ri_resident_size: UInt64;
    ri_phys_footprint: UInt64;
    // remaining fields exist, but we don't need them here
    _reserved: array[0..15] of UInt64;
  end;

function proc_listpids(aType: Cardinal; aTypeInfo: Cardinal; aBuffer: Pointer; aBufferSize: Integer): Integer; cdecl;
  external '/usr/lib/libproc.dylib' name 'proc_listpids';

function proc_pidpath(aPid: pid_t; aBuffer: Pointer; aBufferSize: UInt32): Integer; cdecl;
  external '/usr/lib/libproc.dylib' name 'proc_pidpath';

function proc_pid_rusage(aPid: pid_t; aFlavor: Integer; aBuffer: Pointer): Integer; cdecl;
  external '/usr/lib/libproc.dylib' name 'proc_pid_rusage';

const
  PROC_ALL_PIDS = 1;
{$ENDIF}

type
  TProcCpuSample = record
    WallStamp: Int64;     // TStopwatch.GetTimeStamp
    ProcTime100ns: UInt64; // process CPU time converted to 100ns units (Windows native; Linux/mac converted)
    Initialized: Boolean;
  end;

var
  gCpuSamples: TDictionary<TPid, TProcCpuSample>;
  gCpuSamplesLock: TObject;

function CpuCountSafe: Integer;
begin
  // System.CPUCount exists, but TThread.ProcessorCount is also fine.
  // We use CPUCount to match RTL notion.
  Result := System.CPUCount;
  if Result < 1 then
    Result := 1;
end;

function FileNameMatchesPattern(const aFullPathOrName, aPattern: string): Boolean;
var
  lName: string;
begin
  // Pattern is intended to match a filename (not full path), but we support both.
  lName := ExtractFileName(aFullPathOrName);
  Result :=
    MatchesMask(lName, aPattern) or
    MatchesMask(aFullPathOrName, aPattern);
end;

{$IF Defined(MSWINDOWS)}
function WinProcessCpuTime100ns(aPid: TPid; out aProcTime100ns: UInt64): Boolean;
var
  lProc: THandle;
  lOwnProcessHandle: Boolean;
  lCreateFT, lExitFT, lKernelFT, lUserFT: TFileTime;
  lKernel, lUser: UInt64;
begin
  Result := False;
  aProcTime100ns := 0;

  lOwnProcessHandle := False;
  if aPid = Winapi.Windows.GetCurrentProcessId then
  begin
    lProc := Winapi.Windows.GetCurrentProcess;
    lOwnProcessHandle := True;
  end
  else
  begin
    // Some Windows/API combinations require PROCESS_QUERY_INFORMATION for GetProcessTimes.
    lProc := OpenProcess(PROCESS_QUERY_INFORMATION, False, aPid);
    if lProc = 0 then
      lProc := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, aPid);
  end;

  if lProc = 0 then
    Exit;
  try
    if not GetProcessTimes(lProc, lCreateFT, lExitFT, lKernelFT, lUserFT) then
      Exit;

    lKernel := UInt64(lKernelFT.dwLowDateTime) or (UInt64(lKernelFT.dwHighDateTime) shl 32);
    lUser := UInt64(lUserFT.dwLowDateTime) or (UInt64(lUserFT.dwHighDateTime) shl 32);

    aProcTime100ns := lKernel + lUser; // FILETIME is 100ns units
    Result := True;
  finally
    if not lOwnProcessHandle then
      CloseHandle(lProc);
  end;
end;

function WinRamUsageBytes(aPid: TPid; out aBytes: UInt64): Boolean;
var
  lProc: THandle;
  lCounters: PROCESS_MEMORY_COUNTERS_EX;
begin
  Result := False;
  aBytes := 0;

  lProc := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION or PROCESS_VM_READ, False, aPid);
  if lProc = 0 then
    Exit;

  try
    FillChar(lCounters, SizeOf(lCounters), 0);
    lCounters.cb := SizeOf(lCounters);

    if not GetProcessMemoryInfo(lProc, @lCounters, SizeOf(lCounters)) then
      Exit;

    // WorkingSetSize ~= RSS (what most people mean by "RAM usage")
    aBytes := lCounters.WorkingSetSize;
    Result := True;
  finally
    CloseHandle(lProc);
  end;
end;

function WinFindPids(const aPattern: string): TPidArray;
var
  lSnap: THandle;
  lPe: TProcessEntry32;
  lList: TList<TPid>;
begin
  lList := TList<TPid>.Create;
  try
    lSnap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if lSnap = INVALID_HANDLE_VALUE then
      Exit(nil);
    try
      FillChar(lPe, SizeOf(lPe), 0);
      lPe.dwSize := SizeOf(lPe);

      if Process32First(lSnap, lPe) then
      begin
        repeat
          if FileNameMatchesPattern(lPe.szExeFile, aPattern) then
            lList.Add(lPe.th32ProcessID);
        until not Process32Next(lSnap, lPe);
      end;
    finally
      CloseHandle(lSnap);
    end;

    Result := lList.ToArray;
  finally
    lList.Free;
  end;
end;

{$ENDIF}

{$IF Defined(POSIX) and (not Defined(MACOS))} // Linux/Android: procfs approach

function ReadAllTextUtf8(const aFileName: string; out aText: string): Boolean;
var
  lBytes: TBytes;
begin
  Result := False;
  aText := '';
  try
    lBytes := TFile.ReadAllBytes(aFileName);
    aText := TEncoding.UTF8.GetString(lBytes);
    Result := True;
  except
    // ignore
  end;
end;

function LinuxProcCpuTime100ns(aPid: TPid; out aProcTime100ns: UInt64): Boolean;
var
  lStat: string;
  lOpenParen, lCloseParen: Integer;
  lAfter: string;
  lParts: TArray<string>;
  lUTime, lSTime: UInt64;
  lHz: Int64;
  lTicks: UInt64;
  lSeconds: Double;
begin
  Result := False;
  aProcTime100ns := 0;

  if not ReadAllTextUtf8(Format('/proc/%d/stat', [aPid]), lStat) then
    Exit;

  // /proc/<pid>/stat has comm in parentheses which may include spaces.
  lOpenParen := lStat.IndexOf('(');
  lCloseParen := lStat.LastIndexOf(')');
  if (lOpenParen < 0) or (lCloseParen < 0) or (lCloseParen <= lOpenParen) then
    Exit;

  lAfter := Trim(lStat.Substring(lCloseParen + 1));
  lParts := lAfter.Split([' '], TStringSplitOptions.ExcludeEmpty);

  // After ')' the fields continue; utime/stime are fields 14/15 overall.
  // In lParts, index shifts: field 3 ("state") is lParts[0].
  // So utime (14) => lParts[11], stime (15) => lParts[12].
  if Length(lParts) < 13 then
    Exit;

  lUTime := StrToUInt64Def(lParts[11], 0);
  lSTime := StrToUInt64Def(lParts[12], 0);
  lTicks := lUTime + lSTime;

  lHz := sysconf(_SC_CLK_TCK);
  if lHz <= 0 then
    Exit;

  lSeconds := lTicks / lHz;
  aProcTime100ns := UInt64(Round(lSeconds * 10000000.0)); // seconds -> 100ns
  Result := True;
end;

function LinuxRamUsageBytes(aPid: TPid; out aBytes: UInt64): Boolean;
var
  lStatus: string;
  lLines: TArray<string>;
  lLine: string;
  lKb: UInt64;
  lP: Integer;
begin
  Result := False;
  aBytes := 0;

  if not ReadAllTextUtf8(Format('/proc/%d/status', [aPid]), lStatus) then
    Exit;

  lLines := lStatus.Split([sLineBreak]);
  for lLine in lLines do
  begin
    if lLine.StartsWith('VmRSS:', True) then
    begin
      // VmRSS:   12345 kB
      lP := lLine.IndexOf(':');
      if lP >= 0 then
      begin
        lKb := StrToUInt64Def(Trim(StringReplace(lLine.Substring(lP + 1), 'kB', '', [rfIgnoreCase])), 0);
        aBytes := lKb * 1024;
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function LinuxExePathOfPid(aPid: TPid): string;
var
  lBuf: array[0..4095] of AnsiChar;
  lLen: ssize_t;
  lLink: AnsiString;
begin
  Result := '';
  lLink := AnsiString(Format('/proc/%d/exe', [aPid]));
  FillChar(lBuf, SizeOf(lBuf), 0);
  lLen := readlink(PAnsiChar(lLink), @lBuf[0], SizeOf(lBuf) - 1);
  if lLen > 0 then
    Result := string(AnsiString(Copy(lBuf, 0, lLen)));
end;

function LinuxFindPids(const aPattern: string): TPidArray;
var
  lDir: TSearchRec;
  lPid: TPid;
  lPath: string;
  lList: TList<TPid>;
begin
  lList := TList<TPid>.Create;
  try
    if FindFirst('/proc/*', faDirectory, lDir) = 0 then
    try
      repeat
        if (lDir.Attr and faDirectory) <> 0 then
        begin
          if TryStrToUInt(lDir.Name, lPid) then
          begin
            lPath := LinuxExePathOfPid(lPid);
            if (lPath <> '') and FileNameMatchesPattern(lPath, aPattern) then
              lList.Add(lPid);
          end;
        end;
      until FindNext(lDir) <> 0;
    finally
      FindClose(lDir);
    end;

    Result := lList.ToArray;
  finally
    lList.Free;
  end;
end;

{$ENDIF}

{$IF Defined(MACOS)}

function MacProcessCpuTime100ns(aPid: TPid; out aProcTime100ns: UInt64): Boolean;
var
  lInfo: rusage_info_v2;
  lRes: Integer;
  lNs: UInt64;
begin
  Result := False;
  aProcTime100ns := 0;

  FillChar(lInfo, SizeOf(lInfo), 0);
  lRes := proc_pid_rusage(pid_t(aPid), RUSAGE_INFO_V2, @lInfo);
  if lRes <> 0 then
    Exit;

  // ri_user_time / ri_system_time are documented as time values (commonly nanoseconds).
  // We treat them as nanoseconds and convert to 100ns units.
  lNs := lInfo.ri_user_time + lInfo.ri_system_time;
  aProcTime100ns := lNs div 100; // 1 * 100ns = 100ns = 0.1us; 1ns => /100 for 100ns units
  Result := True;
end;

function MacRamUsageBytes(aPid: TPid; out aBytes: UInt64): Boolean;
var
  lInfo: rusage_info_v2;
  lRes: Integer;
begin
  Result := False;
  aBytes := 0;

  FillChar(lInfo, SizeOf(lInfo), 0);
  lRes := proc_pid_rusage(pid_t(aPid), RUSAGE_INFO_V2, @lInfo);
  if lRes <> 0 then
    Exit;

  aBytes := lInfo.ri_resident_size;
  Result := True;
end;

function MacPidPath(aPid: TPid): string;
var
  lBuf: array[0..4095] of AnsiChar;
  lLen: Integer;
begin
  Result := '';
  FillChar(lBuf, SizeOf(lBuf), 0);
  lLen := proc_pidpath(pid_t(aPid), @lBuf[0], SizeOf(lBuf));
  if lLen > 0 then
    Result := string(AnsiString(PAnsiChar(@lBuf[0])));
end;

function MacFindPids(const aPattern: string): TPidArray;
var
  lBufBytes: Integer;
  lCountBytes: Integer;
  lPids: TArray<Integer>;
  i, lCount: Integer;
  lPid: TPid;
  lPath: string;
  lList: TList<TPid>;
begin
  lList := TList<TPid>.Create;
  try
    // First call with nil to get required bytes (often works; if not, we grow).
    lBufBytes := 4096 * SizeOf(Integer);
    SetLength(lPids, lBufBytes div SizeOf(Integer));

    while True do
    begin
      lCountBytes := proc_listpids(PROC_ALL_PIDS, 0, @lPids[0], lBufBytes);
      if lCountBytes <= lBufBytes then
        Break;

      lBufBytes := lCountBytes;
      SetLength(lPids, lBufBytes div SizeOf(Integer));
    end;

    lCount := lCountBytes div SizeOf(Integer);
    for i := 0 to lCount - 1 do
    begin
      if lPids[i] <= 0 then
        Continue;

      lPid := TPid(lPids[i]);
      lPath := MacPidPath(lPid);
      if (lPath <> '') and FileNameMatchesPattern(lPath, aPattern) then
        lList.Add(lPid);
    end;

    Result := lList.ToArray;
  finally
    lList.Free;
  end;
end;

{$ENDIF}

function CurrentProcessId: TPid;
begin
{$IF Defined(MSWINDOWS)}
  Result := Winapi.Windows.GetCurrentProcessId;
{$ELSEIF Defined(POSIX)}
  Result := TPid(Posix.Unistd.getpid);
{$ELSE}
  Result := 0;
{$ENDIF}
end;

function CurrentThreadId: TPid;
begin
{$IF Defined(MSWINDOWS)}
  Result := Winapi.Windows.GetCurrentThreadId;
{$ELSE}
  // POSIX doesn't have a portable numeric thread id that matches OS tools.
  // Returning 0 is safer than pretending.
  Result := 0;
{$ENDIF}
end;

function FindPid(const aFileNamePattern: string): TPidArray;
begin
{$IF Defined(MSWINDOWS)}
  Result := WinFindPids(aFileNamePattern);
{$ELSEIF Defined(MACOS)}
  Result := MacFindPids(aFileNamePattern);
{$ELSEIF Defined(POSIX)}
  Result := LinuxFindPids(aFileNamePattern);
{$ELSE}
  Result := nil;
{$ENDIF}
end;

function KillProcess(aPid: TPid): Boolean;
{$IF Defined(MSWINDOWS)}
var
  lProc: THandle;
{$ENDIF}
begin
  Result := False;

{$IF Defined(MSWINDOWS)}
  lProc := OpenProcess(PROCESS_TERMINATE, False, aPid);
  if lProc = 0 then
    Exit;

  try
    Result := TerminateProcess(lProc, 1);
  finally
    CloseHandle(lProc);
  end;

{$ELSEIF Defined(POSIX)}
  // First try SIGTERM; if you want SIGKILL escalation, add a second step.
  Result := Posix.Signal.kill(pid_t(aPid), SIGTERM) = 0;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function KillProcess(const aNamePattern: string): Integer;
var
  lPids: TPidArray;
  lPid: TPid;
begin
  Result := 0;
  lPids := FindPid(aNamePattern);
  for lPid in lPids do
    if KillProcess(lPid) then
      Inc(Result);
end;

function ReadProcessCpuTime100ns(aPid: TPid; out aTime100ns: UInt64): Boolean;
begin
{$IF Defined(MSWINDOWS)}
  Result := WinProcessCpuTime100ns(aPid, aTime100ns);
{$ELSEIF Defined(MACOS)}
  Result := MacProcessCpuTime100ns(aPid, aTime100ns);
{$ELSEIF Defined(POSIX)}
  Result := LinuxProcCpuTime100ns(aPid, aTime100ns);
{$ELSE}
  aTime100ns := 0;
  Result := False;
{$ENDIF}
end;

function CpuUsage(aPid: TPid): TCpuUsage;
var
  lNowWall: Int64;
  lNowProc: UInt64;
  lPrev: TProcCpuSample;
  lDeltaWallSec: Double;
  lDeltaProcSec: Double;
  lCpuCount: Integer;
  lSingle: Double;
begin
  Result.OverallPercent := 0;
  Result.SingleCorePercent := 0;

  if not ReadProcessCpuTime100ns(aPid, lNowProc) then
    Exit;

  lNowWall := TStopwatch.GetTimeStamp;

  TMonitor.Enter(gCpuSamplesLock);
  try
    if (not gCpuSamples.TryGetValue(aPid, lPrev)) or (not lPrev.Initialized) then
    begin
      lPrev.Initialized := True;
      lPrev.WallStamp := lNowWall;
      lPrev.ProcTime100ns := lNowProc;
      gCpuSamples.AddOrSetValue(aPid, lPrev);
      Exit; // first sample => 0%
    end;

    lDeltaWallSec := (lNowWall - lPrev.WallStamp) / TStopwatch.Frequency;
    lDeltaProcSec := (Int64(lNowProc) - Int64(lPrev.ProcTime100ns)) / 10000000.0;

    lPrev.WallStamp := lNowWall;
    lPrev.ProcTime100ns := lNowProc;
    lPrev.Initialized := True;
    gCpuSamples.AddOrSetValue(aPid, lPrev);
  finally
    TMonitor.Exit(gCpuSamplesLock);
  end;

  if lDeltaWallSec <= 0 then
    Exit;
  if lDeltaProcSec < 0 then
    Exit;

  lSingle := (lDeltaProcSec / lDeltaWallSec) * 100.0;
  lCpuCount := CpuCountSafe;

  Result.SingleCorePercent := lSingle;
  Result.OverallPercent := lSingle / lCpuCount;
end;

function CpuUsage: TCpuUsage;
begin
  Result := CpuUsage(CurrentProcessId);
end;

function RamUsageBytes(aPid: TPid): UInt64;
{$IF Defined(MSWINDOWS)}
var
  lBytes: UInt64;
{$ENDIF}
{$IF Defined(POSIX) and (not Defined(MACOS))}
var
  lBytes: UInt64;
{$ENDIF}
{$IF Defined(MACOS)}
var
  lBytes: UInt64;
{$ENDIF}
begin
{$IF Defined(MSWINDOWS)}
  if WinRamUsageBytes(aPid, lBytes) then
    Exit(lBytes);
  Exit(0);

{$ELSEIF Defined(MACOS)}
  if MacRamUsageBytes(aPid, lBytes) then
    Exit(lBytes);
  Exit(0);

{$ELSEIF Defined(POSIX)}
  if LinuxRamUsageBytes(aPid, lBytes) then
    Exit(lBytes);
  Exit(0);

{$ELSE}
  Result := 0;
{$ENDIF}
end;

function RamUsageBytes: UInt64;
begin
  Result := RamUsageBytes(CurrentProcessId);
end;

initialization
  gCpuSamples := TDictionary<TPid, TProcCpuSample>.Create;
  gCpuSamplesLock := TObject.Create;

finalization
  gCpuSamples.Free;
  gCpuSamplesLock.Free;

end.
