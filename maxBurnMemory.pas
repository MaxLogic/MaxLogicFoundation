unit maxBurnMemory;

{
  a simple helper to clean up memory filling it with zeroes

  version: 1.2
}
interface

uses
  sysUtils, classes;

procedure burn(var value: string); overload; inline;
procedure burn(var value: rawByteString); overload; inline;
procedure burn(var value: tBytes); overload; inline;
procedure burn(var value: double); overload; inline;
procedure burn(var value: integer); overload; inline;
procedure burn(stream: TStream); overload;

procedure burnMem(value: pointer; Size: integer); inline;

implementation

uses
  math;

procedure burn(stream: TStream);
const
  blockSize = 1024;
var
  b: tBytes;
  bytesLeft, len: int64;
begin
  bytesLeft := stream.Size;
  SetLength(b, min(blockSize, stream.Size));
  burn(b);

  stream.position := 0;
  while bytesLeft <> 0 do
  begin
    len := min(length(b), bytesLeft);
    stream.writeBuffer(b[0], len);
    dec(bytesLeft, len);
  end;
end;

procedure burn(var value: string);
begin
  if length(value) <> 0 then
    burnMem(@value[1], length(value) * SizeOf(value[1]));
end;

procedure burn(var value: rawByteString);
begin
  if length(value) <> 0 then
    burnMem(@value[1], length(value));
end;

procedure burn(var value: tBytes);
begin
  if length(value) <> 0 then
    burnMem(@value[0], length(value));
end;

procedure burn(var value: double);
begin
  burnMem(@value, SizeOf(value));
end;

procedure burn(var value: integer);
begin
  burnMem(@value, SizeOf(value));
end;

procedure burnMem(value: pointer; Size: integer); inline;
begin
  system.fillChar(value^, Size, 0);
end;

end.
