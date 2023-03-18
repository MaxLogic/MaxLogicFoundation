Unit MaxLogic.MemoryUsageInfo;

Interface

Uses
  windows, sysUtils, classes, Winapi.PsAPI;

  // FastMM4 memory info
Function CurrentMemoryUsage: int64;
Function GetCurMemoryUsageStr: String;
Function fstr(Const d: double; vs: byte = 2; ns: byte = 2): String;

Implementation

Function CurrentMemoryUsage: int64;
Var
  st: TMemoryManagerState;
  sb: TSmallBlockTypeState;
Begin
  GetMemoryManagerState(st);
  Result := st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
  For sb In st.SmallBlockTypeStates Do
    Result := Result + sb.UseableBlockSize * sb.AllocatedBlockCount;
End;



Function GetCurMemoryUsageStr: String;
Var
  Cur, reserved, max: int64;
  s: String;
  pmc: TProcessMemoryCounters;
Begin
  Cur := CurrentMemoryUsage;
  If GetProcessMemoryInfo(GetCurrentProcess, @pmc, sizeOf(pmc)) Then
  Begin
      max := pmc.PeakWorkingSetSize;
    reserved := pmc.WorkingSetSize;
  End
  Else
  Begin
      max := 0;
    reserved := 0;
  End;
  s := Format('Memory usage: %s MB by FastMM4 (reserved: %s, max: %s MB)',
    [fstr(Cur / 1024 / 1024), fstr(reserved / 1024 / 1024),
    fstr(max / 1024 / 1024)]);

  Result := s;
End;


Function fstr(Const d: double; vs: byte = 2; ns: byte = 2): String;
Var
  s: String;
Begin
  s := '0.' + StringOfChar('0', ns);
  Result := FormatFloat(s, d);
End;

end.