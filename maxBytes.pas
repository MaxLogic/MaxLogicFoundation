unit maxBytes;

interface
uses
sysUtils;

Type
  TBytesHelper=record
public
  Offset: Integer;
  Bytes: TBytes;
    Procedure Write(const Buffer; Size: integer); inline;
    procedure Read(var Buffer; Size: Integer); inline;
        class operator Initialize (out Dest: TBytesHelper);
  end;


implementation

  { TBytesHelper }

procedure TBytesHelper.Write(const Buffer; Size: integer);
begin
Move(buffer, bytes[offset], size);
inc(offset, size);
end;

procedure TBytesHelper.Read(var Buffer; Size: Integer);
begin
Move(bytes[offset], buffer, size);
inc(offset, size);
end;

class operator TBytesHelper.Initialize(out Dest: TBytesHelper);
begin
Dest.offset:= 0;
dest.Bytes:= nil;
end;


end.
