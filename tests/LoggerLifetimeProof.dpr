program LoggerLifetimeProof;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MaxLogic.Logger in '..\MaxLogic.Logger.pas';

// Keep this probe in its own process: unrelated worker allocations would invalidate heap deltas.
function AllocatedBlocks: Int64;
var
  lState: TMemoryManagerState;
  i: Integer;
begin
  GetMemoryManagerState(lState);
  Result := Int64(lState.AllocatedMediumBlockCount) + lState.AllocatedLargeBlockCount;
  for i := Low(lState.SmallBlockTypeStates) to High(lState.SmallBlockTypeStates) do
    Inc(Result, lState.SmallBlockTypeStates[i].AllocatedBlockCount);
end;

procedure ExerciseEntries(const aCount: Integer; const aTagged: Boolean);
var
  i: Integer;
  lEntry: iLogEntry;
begin
  for i := 1 to aCount do
  begin
    lEntry := TLogEntry.Create;
    if aTagged then
      lEntry.put('diagnostic', IntToStr(i));
    lEntry := nil;
  end;
end;

procedure RunProof;
const
  cCount = 100;
var
  lBefore, lEmptyAfter, lTaggedAfter: Int64;
begin
  ExerciseEntries(1, True);
  lBefore := AllocatedBlocks;
  ExerciseEntries(cCount, False);
  lEmptyAfter := AllocatedBlocks;
  ExerciseEntries(cCount, True);
  lTaggedAfter := AllocatedBlocks;
  Writeln('entries_per_batch=', cCount);
  Writeln('empty_live_allocation_delta=', lEmptyAfter - lBefore);
  Writeln('tagged_live_allocation_delta=', lTaggedAfter - lEmptyAfter);
  if (lEmptyAfter <> lBefore) or (lTaggedAfter <> lEmptyAfter) then
    ExitCode := 1;
end;

begin
  RunProof;
end.
