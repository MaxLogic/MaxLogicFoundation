unit MaxLogic.MemoryBuffer;

{

  This utility allows safe and efficient handling of dynamic memory, providing capabilities to write various data types to a contiguous memory block,
  automatically expanding the buffer as needed.

  It offers methods to write common data types (such as integers, doubles, and strings) and read them back, handling the required buffer expansion
  seamlessly. The `FreeMemoryOnDestroy` property allows flexibility in managing memory lifetimes.

}

interface

uses
  system.SysUtils, system.Classes;

type
  iMemoryBuffer = Interface
    // Private methods to set properties and manage buffer expansion

    // Sets the capacity of the memory buffer. If the specified value is less than the current position or size,
    // the position and size will be adjusted to fit within the new capacity.
    procedure SetCapacity(const Value: nativeUInt);

    // Sets the current position within the buffer. Expands the buffer capacity if the new position exceeds the current capacity.
    procedure SetPosition(const Value: nativeUInt);

    // Sets the buffer increment size, which determines the amount by which the buffer expands when needed.
    procedure SetBufferIncrementSize(const Value: nativeUInt);

    // Ensures the buffer has sufficient capacity for a given required size.
    // If the current capacity is insufficient, it will be increased to accommodate the required size plus the buffer increment.
    procedure EnsureBufferSize(aRequiredSize: nativeUInt);

    // Sets the size of the buffer, which reflects the amount of data written. Expands the capacity if necessary.
    procedure SetSize(const Value: nativeUInt);

    // Controls whether the memory buffer should be freed automatically when the instance is destroyed.
    procedure SetFreeMemoryOnDestroy(const Value: Boolean);

    // Property getter methods
    function GetCapacity: nativeUInt;
    function GetMemory: pByte;
    function GetPosition: nativeUInt;
    function GetSize: nativeUInt;
    function GetFreeMemoryOnDestroy: Boolean;
    function GetBufferIncrementSize: nativeUInt;

    // Public properties to access and control buffer settings

    // Specifies by how much the buffer should increment each time it expands. This helps to avoid frequent reallocations.
    property BufferIncrementSize: nativeUInt read GetBufferIncrementSize write SetBufferIncrementSize;

    // Direct pointer to the underlying memory block. This allows raw access to the buffer's contents if needed.
    property Memory: pByte read GetMemory;

    // Total allocated capacity of the memory buffer. Setting this property can expand or shrink the buffer.
    // If reduced below the current position or size, both will be adjusted to fit within the new capacity.
    property Capacity: nativeUInt read GetCapacity write SetCapacity;

    // The current read/write position within the buffer. When set beyond the current size,
    // it automatically increases the size to match. Expands the buffer if needed.
    property Position: nativeUInt read GetPosition write SetPosition;

    // Represents the amount of data currently stored in the buffer. Expands capacity if set beyond the current capacity.
    // Reflects the maximum position that has been written to, but does not shrink automatically.
    property Size: nativeUInt read GetSize write SetSize;

    // Controls whether the allocated memory buffer should be freed when the class instance is destroyed.
    // Set this to `False` if the memory buffer should be retained after the instance is released.
    property FreeMemoryOnDestroy: Boolean read GetFreeMemoryOnDestroy write SetFreeMemoryOnDestroy;

    // Methods to read and write data to the buffer

    // Writes a specified buffer to the memory block at the current position. Expands capacity as needed.
    // Increments the position and updates the size if required.
    procedure WriteBuffer(const aBuffer; aSize: nativeInt);

    // Reads data from the buffer into a specified variable. Reads up to `aSize` bytes, or until the end of the buffer.
    // Returns the number of bytes actually read.
    function ReadBuffer(var aBuffer; aSize: Integer): Integer;

    // Convenience methods for writing specific data types to the buffer

    // Writes an integer value to the buffer at the current position.
    procedure Write(aValue: Integer); overload;

    // Writes a double value to the buffer at the current position.
    procedure Write(const aValue: double); overload;

    // Writes an ANSI string to the buffer at the current position. Does not store the length of the string.
    procedure Write(const aValue: AnsiString); overload;

    // Writes a byte array (TBytes) to the buffer at the current position.
    procedure Write(const aValue: TBytes); overload;

    // Reads an integer value from the current position in the buffer.
    function ReadInt32: int32;

    // Reads a double value from the current position in the buffer.
    function ReadDouble: double;

    // Reads an ANSI string of a specified length from the current position in the buffer.
    function ReadAnsiString(aSize: Integer): AnsiString;

    // Reads a specified number of bytes from the buffer and returns them as a byte array (TBytes).
    function ReadBytes(aSize: Integer): TBytes;

    // Special methods for handling UTF-8 strings

    // Writes a UTF-8 encoded string to the buffer. First writes the length of the string as a 4-byte integer,
    // followed by the UTF-8 encoded bytes of the string.
    procedure WriteUtf8String(const aValue: String); overload;

    // Reads a UTF-8 encoded string from the buffer. Expects the string length as a 4-byte integer,
    // followed by the UTF-8 encoded bytes of the string.
    function ReadUtf8String: String;

    // Memory management methods

    // Resets the buffer by setting the position and size to 0. Does not free the memory.
    Procedure Clear;

    // Releases the allocated memory buffer, setting capacity to zero.
    // If `FreeMemoryOnDestroy` is set to `False`, this does nothing.
    procedure ReleaseMemory;

    // Adjusts the capacity to match the current size, discarding any unused memory after the written buffer.
    procedure TrimMemory;

    // Creates a copy of the memory buffer with the same data. The cloned instance will have its own memory allocation.
    function Clone: iMemoryBuffer;
  end;

  TMemoryBlock = class(TInterfacedObject, iMemoryBuffer)
  private
    // Total allocated capacity of the memory buffer, in bytes.
    fCapacity: nativeUInt;

    // Current read/write position within the buffer. Represents the next byte location for reading or writing.
    fPosition: nativeUInt;

    // Pointer to the beginning of the allocated memory block.
    fMemory: pByte;

    // Specifies the amount by which the buffer should increment each time it expands. This helps avoid frequent reallocations.
    FBufferIncrementSize: nativeUInt;

    // Represents the amount of data currently stored in the buffer. Reflects the maximum position that has been written to.
    fSize: nativeUInt;

    // Controls whether the allocated memory buffer should be freed when the class instance is destroyed.
    FFreeMemoryOnDestroy: Boolean;

    // Sets the total capacity of the memory buffer. Expands or reduces the buffer size as needed.
    // If reduced below the current position or size, both will be adjusted to fit within the new capacity.
    procedure SetCapacity(const Value: nativeUInt);

    // Sets the current read/write position within the buffer.
    // Expands the buffer capacity if the new position exceeds the current capacity.
    procedure SetPosition(const Value: nativeUInt);

    // Ensures that the buffer has sufficient capacity for a given required size.
    // If the current capacity is insufficient, the buffer expands to accommodate the required size plus the buffer increment.
    procedure EnsureBufferSize(aRequiredSize: nativeUInt);

    // Sets the buffer increment size, which defines the amount by which the buffer should grow during expansions.
    procedure SetBufferIncrementSize(const Value: nativeUInt);

    // Sets the buffer size to reflect the amount of data stored. Expands capacity if necessary.
    procedure SetSize(const Value: nativeUInt);

    // Controls whether the memory buffer should be freed automatically when the instance is destroyed.
    procedure SetFreeMemoryOnDestroy(const Value: Boolean);

    // Property getter methods for private fields
    function GetCapacity: nativeUInt;
    function GetMemory: pByte;
    function GetPosition: nativeUInt;
    function GetSize: nativeUInt;
    function GetFreeMemoryOnDestroy: Boolean;
    function GetBufferIncrementSize: nativeUInt;

  public
    // Specifies by how much the buffer should increment each time it expands, helping to reduce the frequency of reallocations.
    property BufferIncrementSize: nativeUInt read GetBufferIncrementSize write SetBufferIncrementSize;

    // Direct pointer to the underlying memory block, providing raw access to the buffer contents.
    property Memory: pByte read GetMemory;

    // Total allocated capacity of the memory buffer, in bytes. Setting this property can expand or shrink the buffer.
    // If reduced below the current position or size, both are adjusted to fit within the new capacity.
    property Capacity: nativeUInt read GetCapacity write SetCapacity;

    // The current read/write position within the buffer. Expands the buffer if set beyond the current capacity.
    property Position: nativeUInt read GetPosition write SetPosition;

    // Represents the amount of data currently stored in the buffer. Expands capacity if set beyond the current capacity.
    // Reflects the maximum position that has been written to.
    property Size: nativeUInt read GetSize write SetSize;

    // Controls whether the memory buffer should be freed when the instance is destroyed.
    // Set to `False` if the buffer should be retained after the instance is destroyed.
    property FreeMemoryOnDestroy: Boolean read GetFreeMemoryOnDestroy write SetFreeMemoryOnDestroy;

    // Initializes the memory buffer with a specified initial size and buffer increment.
    // `aInitialSize` sets the initial capacity and size of the memory block.
    // `aBufferIncrements` defines the increment size for expansions.
    constructor Create(aInitialSize: nativeUInt = 0; aBufferIncrements: nativeUInt = 16 * 1024);

    // Frees the allocated memory if `FreeMemoryOnDestroy` is `True` and cleans up the instance.
    destructor Destroy; override;

    // Writes a specified buffer to the memory block at the current position, expanding the buffer as needed.
    // Advances the position and updates the size if necessary.
    procedure WriteBuffer(const aBuffer; aSize: nativeInt);

    // Reads data from the buffer into a specified variable. Reads up to `aSize` bytes or until the end of the buffer.
    // Returns the actual number of bytes read.
    function ReadBuffer(var aBuffer; aSize: Integer): Integer;

    // Writes an integer value to the buffer at the current position.
    procedure Write(aValue: Integer); overload;

    // Writes a double value to the buffer at the current position.
    procedure Write(const aValue: double); overload;

    // Writes an ANSI string to the buffer at the current position, without storing the string length.
    procedure Write(const aValue: AnsiString); overload;

    // Writes a byte array (TBytes) to the buffer at the current position.
    procedure Write(const aValue: TBytes); overload;

    // Reads a 32-bit integer value from the buffer at the current position.
    function ReadInt32: int32;

    // Reads a double value from the buffer at the current position.
    function ReadDouble: double;

    // Reads an ANSI string of a specified length from the buffer at the current position.
    function ReadAnsiString(aSize: Integer): AnsiString;

    // Reads a specified number of bytes from the buffer, returning them as a byte array (TBytes).
    function ReadBytes(aSize: Integer): TBytes;

    // Writes a UTF-8 encoded string to the buffer, preceded by a 4-byte integer indicating the string length.
    procedure WriteUtf8String(const aValue: String); overload;

    // Reads a UTF-8 encoded string from the buffer, expecting a 4-byte integer for the length followed by the UTF-8 encoded bytes.
    function ReadUtf8String: String;

    // Resets the buffer's position and size to 0 without freeing memory.
    // Use `ReleaseMemory` to release the memory block if needed.
    Procedure Clear;

    // Releases the allocated memory buffer, resetting `Capacity`, `Position`, and `Size` to 0.
    // Does nothing if `FreeMemoryOnDestroy` is `False`.
    procedure ReleaseMemory;

    // Adjusts the buffer's capacity to match the current size, discarding any unused memory after the data.
    procedure TrimMemory;

    // Creates a deep copy of the memory buffer with the same data. The cloned instance will have its own allocated memory.
    function Clone: iMemoryBuffer;
  end;

implementation

uses
  system.math;

{ TMemoryBlock }

procedure TMemoryBlock.Clear;
begin
  fPosition := 0;
  fSize := 0;
end;

function TMemoryBlock.Clone: iMemoryBuffer;
var
  lNew: TMemoryBlock;
begin
  // note: the constructor initializes capacity and size by the initial size given
  lNew := TMemoryBlock.Create(Self.Size, Self.BufferIncrementSize);
  move(Self.fMemory^, lNew.fMemory^, Self.Size);
  lNew.fPosition := Self.fPosition;
  Result := lNew;

end;

constructor TMemoryBlock.Create(aInitialSize,
  aBufferIncrements: nativeUInt);
begin
  inherited Create;
  FFreeMemoryOnDestroy := True;
  FBufferIncrementSize := aBufferIncrements;
  Capacity := aInitialSize;
  Size := aInitialSize;
end;

destructor TMemoryBlock.Destroy;
begin
  if Self.FFreeMemoryOnDestroy then
    ReleaseMemory;

  inherited;
end;

procedure TMemoryBlock.EnsureBufferSize(aRequiredSize: nativeUInt);
begin
  if aRequiredSize >= fCapacity then
    Capacity := (aRequiredSize + Self.BufferIncrementSize); // set the new size to be the required size + the Buffer inc value, this way we will have room for more memory movements later on.
end;

function TMemoryBlock.GetBufferIncrementSize: nativeUInt;
begin
  Result := FBufferIncrementSize;
end;

function TMemoryBlock.GetCapacity: nativeUInt;
begin
  Result := fCapacity;
end;

function TMemoryBlock.GetMemory: pByte;
begin
  Result := fMemory;
end;

function TMemoryBlock.GetFreeMemoryOnDestroy: Boolean;
begin
  Result := FFreeMemoryOnDestroy;
end;

function TMemoryBlock.GetPosition: nativeUInt;
begin
  Result := fPosition;
end;

function TMemoryBlock.GetSize: nativeUInt;
begin
  Result := fSize;
end;

function TMemoryBlock.ReadAnsiString(aSize: Integer): AnsiString;
begin
  if aSize <= 0 then
    Exit('');

  if fPosition + aSize > fSize then
    raise EReadError.Create('Read beyond buffer size');
  setLength(Result, aSize);
  ReadBuffer(Result[1], aSize);
end;

function TMemoryBlock.ReadBuffer(var aBuffer; aSize: Integer): Integer;
var
  pb: pByte;
begin
  Result := min(aSize, fSize - fPosition);
  pb := fMemory;
  Inc(pb, fPosition);
  move(pb^, aBuffer, Result);
  Inc(fPosition, Result);
end;

function TMemoryBlock.ReadBytes(aSize: Integer): TBytes;
begin
  if aSize <= 0 then
    Exit(nil);
  if fPosition + aSize > fSize then
    raise EReadError.Create('Read beyond buffer size');
  setLength(Result, aSize);
  ReadBuffer(Result[0], aSize);
end;

function TMemoryBlock.ReadDouble: double;
begin
  if fPosition + SizeOf(Result) > fSize then
    raise EReadError.Create('Read beyond buffer size');
  ReadBuffer(Result, SizeOf(Result));
end;

function TMemoryBlock.ReadInt32: int32;
begin
  if fPosition + SizeOf(Result) > fSize then
    raise EReadError.Create('Read beyond buffer size');
  ReadBuffer(Result, SizeOf(Result));
end;

function TMemoryBlock.ReadUtf8String: String;
var
  lBytes: TBytes;
begin
  lBytes := ReadBytes(ReadInt32);
  Result := TEncoding.utf8.GetString(lBytes);
end;

procedure TMemoryBlock.ReleaseMemory;
begin
  Capacity := 0; // frees memory and sets size and position also to 0
end;

procedure TMemoryBlock.SetBufferIncrementSize(const Value: nativeUInt);
begin
  FBufferIncrementSize := Value;
end;

procedure TMemoryBlock.SetCapacity(const Value: nativeUInt);
begin
  if fCapacity = Value then
    Exit;

  fCapacity := Value;
  ReallocMem(fMemory, fCapacity);

  // Adjust position and size if capacity has been reduced
  if fCapacity < fPosition then
    fPosition := fCapacity;
  if fCapacity < fSize then
    fSize := fCapacity;
end;

procedure TMemoryBlock.SetFreeMemoryOnDestroy(const Value: Boolean);
begin
  FFreeMemoryOnDestroy := Value;
end;

procedure TMemoryBlock.SetPosition(const Value: nativeUInt);
begin
  fPosition := Value;
  fSize := max(fSize, fPosition);
  EnsureBufferSize(fPosition);
end;

procedure TMemoryBlock.SetSize(const Value: nativeUInt);
begin
  fSize := Value;
  if fSize > fCapacity then
    EnsureBufferSize(fSize);
end;

procedure TMemoryBlock.TrimMemory;
begin
  Capacity := Size;
end;

procedure TMemoryBlock.Write(aValue: Integer);
begin
  WriteBuffer(aValue, SizeOf(aValue));
end;

procedure TMemoryBlock.Write(const aValue: double);
begin
  WriteBuffer(aValue, SizeOf(aValue));
end;

procedure TMemoryBlock.Write(const aValue: AnsiString);
begin
  if aValue <> '' then
    WriteBuffer(aValue[1], Length(aValue));
end;

procedure TMemoryBlock.Write(const aValue: TBytes);
begin
  if Length(aValue) <> 0 then
    WriteBuffer(aValue[0], Length(aValue))
end;

procedure TMemoryBlock.WriteUtf8String(const aValue: String);
var
  lBytes: TBytes;
  lLen: int32;
begin
  if Length(aValue) > High(int32) then
    raise EInvalidOperation.Create('String is too long to write.');

  lBytes := TEncoding.utf8.GetBytes(aValue);
  lLen := Length(lBytes);
  Write(lLen);
  if lLen > 0 then
    Write(lBytes);
end;

procedure TMemoryBlock.WriteBuffer(const aBuffer; aSize: nativeInt);
var
  pb: pByte;
begin
  EnsureBufferSize(fPosition + aSize);
  pb := fMemory;
  Inc(pb, fPosition);
  move(aBuffer, pb^, aSize);
  Inc(fPosition, aSize);
  fSize := max(fSize, fPosition);
end;

end.
