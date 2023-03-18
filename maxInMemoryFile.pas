unit maxInMemoryFile;

{ Author: pawel Piotrowski
  Copyright 2010 MaxLogic, Pawe³ Piotrowski
  Version: 1.8
  History:
  2019-06-18: added the option to create auto delete temp files for the TMMFStream class
  2017-11-14: some improvements for x64
  2017-01-31: TInmemoryFile maps the real size, of the file, in case of the file already exists
  2014-03-20: fixed a bug when creating a file
  2014-03-17:
  * there was a bug when the file was empty (size 0) and opened for read only access
  * the expansion of the file on disk ist now more radical, but it will decrease remapping of the file again and again overand over again
  2014-03-15: added TMMStream - use Memory mapped files like streams

  NOTE about InterOp:
  64-bit versions of Windows use 32-bit handles for interoperability. When sharing a handle between 32-bit and 64-bit applications, only the lower 32 bits are significant,
  so it is safe to truncate the handle (when passing it from 64-bit to 32-bit) or sign-extend the handle (when passing it from 32-bit to 64-bit).
  Handles that can be shared include
  handles to user objects such as windows (HWND), handles to GDI objects such as pens and brushes (HBRUSH and HPEN), and handles to named objects such as mutexes, semaphores, and file handles.

}
interface

uses
  Windows,
  SysUtils, classes;

type

  { define a in memory file that can be acessed across multiple processes
    NOTE: no real on disc file is created }
  { RESIZE INFO:
    "If an application specifies a size for the file mapping object that is larger than the size of the actual named file on disk and if the page protection allows write access (that is, the flProtect parameter specifies PAGE_READWRITE or PAGE_EXECUTE_READWRITE), then the file on disk is increased to match the specified size of the file mapping object. If the file is extended, the contents of the file between the old end of the file and the new end of the file are not guaranteed to be zero; the behavior is defined by the file system" }
  TInMemoryFile = class
  private
    fMappingHandle: THandle;
    fMappingName: string;
    fMemory: pointer;
    fSize: Cardinal;
    fIsnew: boolean;
    fMaxSize: Cardinal;
    Procedure MapFile(h: THandle);
  public
    // aMaxSize: you can define how big the allocation might get
    // From MSDN: The size of the file mapping object that you select controls how far into the file you can "see" with memory mapping. If you create a file mapping object that is 500 Kb in size, you have access only to the first 500 Kb of the file, regardless of the size of the file. Since it does not cost you any system resources to create a larger file mapping object, create a file mapping object that is the size of the file (set the dwMaximumSizeHigh and dwMaximumSizeLow parameters of CreateFileMapping both to zero) even if you do not expect to view the entire file. The cost in system resources comes in creating the views and accessing them.
    constructor Create(const aName: string; const aSize: Cardinal; aMaxSize: Cardinal = 0);
    Destructor Destroy; override;

    // if you specified a MaxSize bigger then the initial size, you can actually increase the size here
    // returns the new allocated size
    function ReSize(const NewSize: Cardinal): Cardinal;

    property Memory: pointer read fMemory;
    // NOTE: might be biger then you declared.
    // Either there was already a InMemoryFile created or most likey:
    // the system has a     allocation granularity, the size will approach that
    // To obtain the memory allocation granularity of the system, use the GetSystemInfo function, which fills in the members of a SYSTEM_INFO structure. }
    property Size: Cardinal read fSize;
    property MaxSize: Cardinal read fMaxSize;
    property IsNew: boolean read fIsnew;
    property Name: string read fMappingName;
  end;

  TMMFFileMode = (mmfRead, mmfReadWrite, mmfCreate, mffTempFile);
  TMMFOptimizationMode = (moNormal, moSequentialScan, moRandomAccess);

  TMMFStream = class(TStream)
  private
    fOpenPageIndex: Cardinal;
    fOpenPageSize: Cardinal;
    fOpenPageCapacity: Cardinal;
    fOpenpageOffset: int64;
    fView: pointer;

    fPosition: int64;
    hFile: THandle;
    hFileMap: THandle;
    fFileName: string;
    fProcessId: THandle;
    fPageSize: Cardinal;

    fFileSize: int64;
    fFileCapacity: int64;

    fReadOnly: boolean;
    fFileMode: TMMFFileMode;

    function OpenFileHandle(OptimizationMode: TMMFOptimizationMode): boolean;
    Function OpenMapping: boolean;
    procedure CloseMapping;

    procedure ClosePage;
    function GetEof: boolean;
  protected
    function GetSize: int64; override;
    procedure SetSize(NewSize: Longint); overload; override;
    procedure SetSize(const NewSize: int64); overload; override;
  public
    Constructor Create(const aFileName: string; FileMode: TMMFFileMode = mmfRead; PageSize: Cardinal = 50 * 1024 * 1024;
      OptimizationMode: TMMFOptimizationMode = moNormal);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function OpenPage(index: Cardinal): boolean;

    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; overload; override;


    property Size: int64 read fFileSize write SetSize;
    property Eof: boolean read GetEof;

    property PageSize: Cardinal read fOpenPageSize;
    property Pagecapacity: Cardinal read fOpenPageCapacity;
    property OpenpageOffset: int64 read fOpenpageOffset;
    property OpenPageView: pointer read fView;
    property PageIndex: Cardinal read fOpenPageIndex;
  end;

//function GetFileSizeEx(hFile: THandle; lpFileSize: PLargeInteger): BOOL; stdcall; external kernel32 name 'GetFileSizeEx';

implementation

uses
  System.RTLConsts;

{ TInMemoryFile }

Constructor TInMemoryFile.Create;
begin
  inherited Create;

  fIsnew := false;
  fMappingName := aName;
  fSize := aSize;
  fMaxSize := aMaxSize;
  if fSize > fMaxSize then
    fMaxSize := fSize;

  // If the object exists before the function call, the function returns a handle to the existing object (with its current size, not the specified size), and GetLastError returns ERROR_ALREADY_EXISTS.
  // If the function fails, the return value is NULL. To get extended error information, call GetLastError.
  // The size of the file mapping object that you select controls how far into the file you can "see" with memory mapping. If you create a file mapping object that is 500 Kb in size, you have access only to the first 500 Kb of the file, regardless of the size of the file. Since it does not cost you any system resources to create a larger file mapping object, create a file mapping object that is the size of the file (set the dwMaximumSizeHigh and dwMaximumSizeLow parameters of CreateFileMapping both to zero) even if you do not expect to view the entire file. The cost in system resources comes in creating the views and accessing them.
  fMappingHandle := CreateFileMapping(INVALID_HANDLE_VALUE,
    nil,
    PAGE_READWRITE,
    0, // int64rec(fSize).hi,
    fMaxSize,
    PChar(fMappingName));

  if fMappingHandle = 0 then
  begin
      RaiseLastOSError;
    exit;
  end;
  fIsnew := GetLastError <> ERROR_ALREADY_EXISTS;
  MapFile(fMappingHandle);
end; (* RestoreIfRunning *)

Destructor TInMemoryFile.Destroy;
begin
  if fMemory <> NIL then
    UnmapViewOfFile(fMemory);
  if fMappingHandle <> 0 then
    CloseHandle(fMappingHandle);

  inherited;
end;

function TInMemoryFile.ReSize(const NewSize: Cardinal): Cardinal;
begin
  if fSize = NewSize then
    exit(fSize);
  fSize := NewSize;
  if fSize > fMaxSize then
    fSize := fMaxSize;

  UnmapViewOfFile(fMemory);
  fMemory := nil;

  MapFile(self.fMappingHandle);
  result := fSize;
end;

procedure TInMemoryFile.MapFile(h: THandle);
var
  info: TMemoryBasicInformation;
begin
  if fMemory <> NIL then
  begin
      UnmapViewOfFile(fMemory);
    fMemory := nil;
  end;

  fMemory := MapViewOfFile(h,
    FILE_MAP_ALL_ACCESS,

    // A high-order Cardinal of the file offset where the view begins.
    0,
    // The number of bytes of a file mapping to map to the view. All bytes must be within the maximum size specified by CreateFileMapping. If this parameter is 0 (zero), the mapping extends from the specified offset to the end of the file mapping.
    0,

    // The number of bytes of a file mapping to map to the view.
    // All bytes must be within the maximum size specified by CreateFileMapping.
    // If this parameter is 0 (zero), the mapping extends from the specified offset to the end of the file mapping.
    fSize);

  VirtualQuery(fMemory, info, SizeOf(TMemoryBasicInformation));
  fSize := info.RegionSize;
end;

{ TMMFStream }

constructor TMMFStream.Create;
var
  hiSize: Cardinal;
  loSize: Cardinal;
  SysInfo: TSystemInfo;
begin
  inherited Create;

  { A low-order Cardinal of the file offset where the view is to begin. The combination of the high and low offsets must specify an offset within the file mapping. They must also match the memory allocation granularity of the system. That is, the offset must be a multiple of the allocation granularity. To obtain the memory allocation granularity of the system, use the GetSystemInfo function, which fills in the members of a SYSTEM_INFO structure. }
  GetSystemInfo(SysInfo);
  fPageSize := (PageSize div SysInfo.dwAllocationGranularity) * SysInfo.dwAllocationGranularity;
  if fPageSize = 0 then
    fPageSize := SysInfo.dwAllocationGranularity;

  fProcessId := GetCurrentProcessID;
  hFile := INVALID_HANDLE_VALUE;

  fFileName := aFileName;
  fFileMode := FileMode;
  fReadOnly := FileMode = mmfRead;

  if OpenFileHandle(OptimizationMode) then
  begin
      loSize := Windows.GetFileSize(hFile, @hiSize);
    fFileSize := (hiSize shl 32) + loSize;
    fFileCapacity := fFileSize;

    OpenPage(0);
  end;
end;

destructor TMMFStream.Destroy;
var
  loSize, hiSize: Cardinal;
  actualSize: int64;
begin
  CloseMapping;

  if hFile <> INVALID_HANDLE_VALUE then
  begin
    // trim excess
    if not fReadOnly then
    begin
        loSize := Windows.GetFileSize(hFile, @hiSize);
      actualSize := hiSize shl 32 + loSize;
      if fFileSize <> actualSize then
        SetSize(fFileSize);
    end;

    CloseHandle(hFile);
    hFile := 0;
  end;

  inherited;
end;

function TMMFStream.GetEof: boolean;
begin
result:=position>=size;
end;

function TMMFStream.GetSize: int64;
begin
  result := fFileSize;
end;

function TMMFStream.OpenMapping;
const
  sep = '-';
var
  flProtect: Cardinal;
  fn: string;
begin
  result := false;

  if fReadOnly then
    flProtect := PAGE_READONLY
  else
    flProtect := PAGE_READWRITE;

  fn := classname + sep + IntToStr(integer(self)) + sep + IntToStr(integer(hFile)) + sep +
    IntToStr(fProcessId);

  hFileMap := CreateFileMapping(
    hFile,
    nil,
    flProtect,
    int64rec(fFileCapacity).hi,
    int64rec(fFileCapacity).lo,
    PChar(fn));

  if (hFileMap = 0) then
  begin
      RaiseLastOSError;
  end
  else
    result := True;
end;

function TMMFStream.OpenFileHandle(OptimizationMode: TMMFOptimizationMode): boolean;
var
  dwDesiredAccess: Cardinal;
  dwCreationDisposition: Cardinal;
  attr: Cardinal;
  dwShareMode: Cardinal;
begin
  attr := FILE_ATTRIBUTE_NORMAL;
  if OptimizationMode = moRandomAccess then
    attr := attr or FILE_FLAG_RANDOM_ACCESS
  else if OptimizationMode = moSequentialScan then
    attr := attr or FILE_FLAG_SEQUENTIAL_SCAN;

  if fReadOnly then
    dwShareMode := FILE_SHARE_READ
  else
    dwShareMode := 0;

  case fFileMode of
    mmfRead:
      dwCreationDisposition := OPEN_EXISTING;
    mmfCreate:
      dwCreationDisposition := CREATE_ALWAYS;
    mffTempFile:
      begin
          dwCreationDisposition := CREATE_ALWAYS;
        attr := attr or FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE;
      end;
  else
    // mmfReadWrite:
    dwCreationDisposition := OPEN_ALWAYS;
  end;

  dwDesiredAccess := generic_read;
  if not fReadOnly then
    dwDesiredAccess := dwDesiredAccess or generic_write;

  { Open the file }
  hFile := CreateFile(
    { In the ANSI version of this function, the name is limited to MAX_PATH characters. To extend this limit to 32,767 wide characters, call the Unicode version of the function and prepend "\\?\" to the path. For more information, see Naming Files, Paths, and Namespaces. }
    PChar(fFileName),
    dwDesiredAccess,
    // pass 0 - to disable any access (read or write)
    // or pass D to allow read access
    dwShareMode,
    nil,
    dwCreationDisposition,
    attr, 0);

  if hFile = INVALID_HANDLE_VALUE then
    raise EFCreateError.CreateResFmt(@SFCreateErrorEx, [ExpandFileName(fFileName), SysErrorMessage(GetLastError)])
  ELSE
    result := True;
end;

function TMMFStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  i: int64;
begin
  i := Offset;
  result := Seek(i, Origin);
end;

function TMMFStream.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  case Origin of
    soBeginning:
      fPosition := Offset;
    soCurrent:
      Inc(fPosition, Offset);
    soEnd:
      fPosition := fFileSize + Offset;
  end;
  result := fPosition;
end;


procedure TMMFStream.SetSize(NewSize: Longint);
var
  i: int64;
begin
  i := NewSize;
  SetSize(i);
end;

function TMMFStream.Write(const Buffer; Count: Longint): Longint;
var
  BytesLeft: integer;
  BytesAvailableOnThisPage: integer;
  pb: pByte;
begin
  if (fView = nil) or (fPosition < fOpenpageOffset) or (fPosition >= fOpenpageOffset + fOpenPageCapacity) then
    if not OpenPage(trunc(fPosition / fPageSize)) then
      exit(0);

  BytesLeft := Count;
  BytesAvailableOnThisPage := (fOpenpageOffset + fOpenPageCapacity) - fPosition;

  if BytesLeft < BytesAvailableOnThisPage then
    result := BytesLeft
  else
    result := BytesAvailableOnThisPage;

  pb := fView;
  Inc(pb, fPosition - fOpenpageOffset);
  Move(Buffer, pb^, result);

  dec(BytesLeft, result);
  Inc(fPosition, result);
  if fPosition > fFileSize then
    fFileSize := fPosition;

  if BytesLeft <> 0 then
  begin
      pb := @Buffer;
    Inc(pb, result);
    result := result + write(pb^, BytesLeft);
  end;
end;

function TMMFStream.OpenPage(index: Cardinal): boolean;
var
  dwDesiredAccess,
    Offset: int64;
  dwNumberOfBytesToMap: Cardinal;
begin
  result := false;
  ClosePage;

  Offset := index * fPageSize;
  dwNumberOfBytesToMap := fPageSize;

  if Offset + dwNumberOfBytesToMap > fFileCapacity then
  begin
    if fReadOnly then
    begin
      if fFileCapacity > Offset then
        dwNumberOfBytesToMap := fFileCapacity - Offset
      else
        exit(false);
    end
    else
    begin
        CloseMapping;
      if fFileCapacity < fPageSize then
        fFileCapacity := Offset + dwNumberOfBytesToMap
      else
      begin
        repeat
          // double the size
          fFileCapacity := fFileCapacity + fFileCapacity;
        until (fFileCapacity > Offset + dwNumberOfBytesToMap);
      end;
      FileSeek(hFile, fFileCapacity, ord(soBeginning));
      Win32Check(SetEndOfFile(hFile));
    end;
  end;
  if dwNumberOfBytesToMap = 0 then
    exit(false);

  if hFileMap = 0 then
    if not OpenMapping then
      exit(false);

  fOpenPageCapacity := dwNumberOfBytesToMap;
  if Offset + dwNumberOfBytesToMap > fFileSize then
    fOpenPageSize := fFileSize - Offset
  else
    fOpenPageSize := fOpenPageCapacity;
  fOpenPageIndex := index;
  fOpenpageOffset := Offset;

  if not fReadOnly then
    dwDesiredAccess := FILE_MAP_WRITE
  else
    dwDesiredAccess := FILE_MAP_READ;

  fView := MapViewOfFile(
    hFileMap,
    dwDesiredAccess,
    int64rec(Offset).hi,
    int64rec(Offset).lo,
    dwNumberOfBytesToMap);

  if (fView = nil) then
    RaiseLastOSError
  else
    result := True;
end;

function TMMFStream.Read(var Buffer; Count: Longint): Longint;
var
  BytesLeft: integer;
  BytesAvailableOnThisPage: integer;
  pb: pByte;
begin
  result := 0;

  if fPosition >= fFileSize then
    exit;

  if (fView = nil) or (fPosition < fOpenpageOffset) or (fPosition >= fOpenpageOffset + fOpenPageSize) then
    if not OpenPage(trunc(fPosition / fPageSize)) then
      exit(0);

  // if fPosition + Count > fFileSize then
  // Count := fFileSize - fPosition;

  BytesLeft := Count;
  BytesAvailableOnThisPage := (fOpenpageOffset + fOpenPageSize) - fPosition;

  if BytesLeft < BytesAvailableOnThisPage then
    result := BytesLeft
  else
    result := BytesAvailableOnThisPage;

  pb := fView;
  Inc(pb, fPosition - fOpenpageOffset);
  Move(pb^, Buffer, result);

  dec(BytesLeft, result);
  Inc(fPosition, result);
  // this should occure quite rare, we can afford a recursive call
  if BytesLeft <> 0 then
  begin
      pb := @Buffer;
    Inc(pb, result);
    result := result + read(pb^, BytesLeft);
  end;

end;

procedure TMMFStream.SetSize(const NewSize: int64);
begin
  CloseMapping;

  fFileSize := NewSize;
  fFileCapacity := NewSize;

  FileSeek(hFile, NewSize, ord(soBeginning));
  Win32Check(SetEndOfFile(hFile));
end;

procedure TMMFStream.CloseMapping;
begin
  ClosePage;
  if hFileMap <> 0 then
  begin
      CloseHandle(hFileMap);
    hFileMap := 0;
  end;
end;

procedure TMMFStream.ClosePage;
begin
  if fView <> nil then
  begin
      UnmapViewOfFile(fView);
    fView := nil;
    fOpenPageSize := 0;
    fOpenPageCapacity := 0;
    fOpenpageOffset := 0;
  end;

end;

initialization

// TMMFStream.SelfTest;

end.
