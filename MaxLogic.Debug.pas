unit MaxLogic.Debug;

interface

uses
  System.SysUtils;

function GetCurMemoryUsageStr: string;
/// <summary>
/// Returns a human-readable string with current memory usage based on the Delphi memory manager
/// and (on Windows) additional process memory information.
/// Cross-platform; on non-Windows platforms only Delphi memory manager data is used.
/// </summary>

function CurrentMemoryUsage: Int64;
/// <summary>
/// Returns bytes allocated according to Delphi's memory manager state
/// (small, medium and large blocks).
/// Cross-platform.
/// </summary>

function CurrentMemoryUsageViaProcessManager: Cardinal;
/// <summary>
/// On Windows: returns the working set size (in bytes) using GetProcessMemoryInfo.
/// On non-Windows: falls back to CurrentMemoryUsage (truncated to Cardinal).
/// </summary>

function GetUsedVirtualMemory: Cardinal;
/// <summary>
/// On Windows: returns used virtual memory in bytes via GlobalMemoryStatusEx.
/// On non-Windows: falls back to CurrentMemoryUsage (truncated to Cardinal).
/// </summary>

function IsDelphiRunning: Boolean;
/// <summary>
/// On Windows: returns True if a Delphi IDE instance (TAppBuilder) is detected.
/// On non-Windows: always returns False.
/// </summary>

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.PsAPI,
  {$ENDIF}
  System.Math;

function CurrentMemoryUsage: Int64;
var
  lState: TMemoryManagerState;
  lSmallBlock: TSmallBlockTypeState;
begin
  GetMemoryManagerState(lState);

  Result :=
    lState.TotalAllocatedMediumBlockSize +
    lState.TotalAllocatedLargeBlockSize;

  for lSmallBlock in lState.SmallBlockTypeStates do
    Result := Result + (lSmallBlock.UseableBlockSize * lSmallBlock.AllocatedBlockCount);
end;

function CurrentMemoryUsageViaProcessManager: Cardinal;
{$IFDEF MSWINDOWS}
var
  lPMC: TProcessMemoryCounters;
begin
  Result := 0;
  ZeroMemory(@lPMC, SizeOf(lPMC));
  lPMC.cb := SizeOf(lPMC);

  if GetProcessMemoryInfo(GetCurrentProcess, @lPMC, SizeOf(lPMC)) then
    Result := lPMC.WorkingSetSize
  else
    RaiseLastOSError;
end;
{$ELSE}
begin
  // Cross-platform fallback: use Delphi memory manager information.
  // NOTE: This is not the same as OS working set, but gives a useful approximation.
  Result := Cardinal(CurrentMemoryUsage);
end;
{$ENDIF}

function GetUsedVirtualMemory: Cardinal;
{$IFDEF MSWINDOWS}
var
  lStatusEx: TMemoryStatusEx;
begin
  Result:= 0;
  ZeroMemory(@lStatusEx, SizeOf(lStatusEx));
  lStatusEx.dwLength := SizeOf(lStatusEx);

  if GlobalMemoryStatusEx(lStatusEx) then
    Result := Cardinal(lStatusEx.ullTotalVirtual - lStatusEx.ullAvailVirtual)
  else
    RaiseLastOSError;
end;
{$ELSE}
begin
  // No simple cross-platform OS-level virtual memory query.
  // Use Delphi memory manager as a reasonable approximation.
  Result := Cardinal(CurrentMemoryUsage);
end;
{$ENDIF}

function GetCurMemoryUsageStr: string;
var
  lCurBytes: Int64;
  lReservedBytes: UInt64;
  lMaxBytes: UInt64;
  lCurMB: Double;
  lReservedMB: Double;
  lMaxMB: Double;
  lMemInfoAvailable: Boolean;
  {$IFDEF MSWINDOWS}
  lPMC: TProcessMemoryCounters;
  {$ENDIF}
begin
  lCurBytes := CurrentMemoryUsage;
  lMemInfoAvailable := False;
  lReservedBytes := 0;
  lMaxBytes := 0;

  {$IFDEF MSWINDOWS}
  ZeroMemory(@lPMC, SizeOf(lPMC));
  lPMC.cb := SizeOf(lPMC);

  if GetProcessMemoryInfo(GetCurrentProcess, @lPMC, SizeOf(lPMC)) then
  begin
    lMemInfoAvailable := True;
    lReservedBytes := lPMC.WorkingSetSize;
    lMaxBytes := lPMC.PeakWorkingSetSize;
  end;
  {$ENDIF}

  lCurMB := lCurBytes / (1024.0 * 1024.0);

  if lMemInfoAvailable then
  begin
    lReservedMB := lReservedBytes / (1024.0 * 1024.0);
    lMaxMB := lMaxBytes / (1024.0 * 1024.0);

    Result := Format(
      'Memory usage: %.2f MB (reserved: %.2f MB, max: %.2f MB)',
      [lCurMB, lReservedMB, lMaxMB]
    );
  end
  else
  begin
    // Cross-platform / non-Windows or failure to query process memory info.
    Result := Format(
      'Memory usage: %.2f MB (process OS metrics not available)',
      [lCurMB]
    );
  end;
end;

function IsDelphiRunning: Boolean;
{$IFDEF MSWINDOWS}
begin
  // Delphi IDE main window class name is TAppBuilder
  Result := FindWindow('TAppBuilder', nil) <> 0;
end;
{$ELSE}
begin
  // No Delphi IDE on non-Windows platforms
  Result := False;
end;
{$ENDIF}

end.

