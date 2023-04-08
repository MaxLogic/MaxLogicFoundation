unit HdSN;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, CheckLst;

const
  IDE_ID_FUNCTION = $EC; // Returns ID sector for ATA.
  CAP_IDE_ID_FUNCTION = 1; // ATAPI ID command supported
  IDENTIFY_BUFFER_SIZE = 512;
  DFP_RECEIVE_DRIVE_DATA = $0007C088;

type
  USHORT = Word;
  TIdBuffer = array [0 .. IDENTIFY_BUFFER_SIZE - 1] of Byte;

  TGetVersionOutParams = packed record
    bVersion: Byte; // Binary driver version.
    bRevision: Byte; // Binary driver revision.
    bReserved: Byte; // Not used.
    bIDEDeviceMap: Byte; // Bit map of IDE devices.
    fCapabilities: DWORD; // Bit mask of driver capabilities.
    dwReserved: Array [0 .. 3] of DWORD; // For future use.
  end;

  TIdSector = packed record
    wGenConfig: USHORT;
    wNumCyls: USHORT;
    wReserved: USHORT;
    wNumHeads: USHORT;
    wBytesPerTrack: USHORT;
    wBytesPerSector: USHORT;
    wSectorsPerTrack: USHORT;
    wVendorUnique: Array [0 .. 2] of USHORT;
    sSerialNumber: Array [0 .. 19] of CHAR;
    wBufferType: USHORT;
    wBufferSize: USHORT;
    wECCSize: USHORT;
    sFirmwareRev: Array [0 .. 7] of CHAR;
    sModelNumber: Array [0 .. 39] of CHAR;
    wMoreVendorUnique: USHORT;
    wDoubleWordIO: USHORT;
    wCapabilities: USHORT;
    wReserved1: USHORT;
    wPIOTiming: USHORT;
    wDMATiming: USHORT;
    wBS: USHORT;
    wNumCurrentCyls: USHORT;
    wNumCurrentHeads: USHORT;
    wNumCurrentSectorsPerTrack: USHORT;
    ulCurrentSectorCapacity: ULONG;
    wMultSectorStuff: USHORT;
    ulTotalAddressableSectors: ULONG;
    wSingleWordDMA: USHORT;
    wMultiWordDMA: USHORT;
    bReserved: Array [0 .. 127] of Byte;
  end;

  TDriverStatus = packed record
    bDriverError: Byte; // Error code from driver, or 0 if no error.
    bIDEStatus: Byte; // Contents of IDE Error register. Only valid when bDriverError is SMART_IDE_ERROR.
    bReserved: Array [0 .. 1] of Byte; // Reserved for future expansion.
    dwReserved: Array [0 .. 1] of DWORD; // Reserved for future expansion.
  end;

  TSendCmdOutParams = packed record
    cBufferSize: DWORD; // Size of bBuffer in bytes
    DriverStatus: TDriverStatus; // Driver status structure.
    bBuffer: Array [0 .. 0] of Byte; // Buffer of arbitrary length in which to store the data read from the drive.
  end;

  TIDERegs = packed record
    bFeaturesReg: Byte; // Used for specifying SMART "commands".
    bSectorCountReg: Byte; // IDE sector count register
    bSectorNumberReg: Byte; // IDE sector number register
    bCylLowReg: Byte; // IDE low order cylinder value
    bCylHighReg: Byte; // IDE high order cylinder value
    bDriveHeadReg: Byte; // IDE drive/head register
    bCommandReg: Byte; // Actual IDE command.
    bReserved: Byte; // reserved for future use.  Must be zero.
  end;

  TSendCmdInParams = packed record
    cBufferSize: DWORD; // Buffer size in bytes
    irDriveRegs: TIDERegs; // Structure with drive register values.
    bDriveNumber: Byte; // Physical drive number to send command to (0,1,2,3).
    bReserved: Array [0 .. 2] of Byte; // Reserved for future expansion.
    dwReserved: Array [0 .. 3] of DWORD; // For future use.
    bBuffer: Array [0 .. 0] of Byte; // Input buffer.
  end;

  // Get first IDE harddisk serial number
function GetIdeSerialNumber: String;

function Physical_SN(drive: CHAR): Int64;
function GetVolSN(aVolumn: CHAR): string;
function Get_HDD_Capacity_Bytes(drive: CHAR): Int64;
Function IsFixedDrive(drive: CHAR): Boolean;

var
  aIdBuffer: TIdBuffer;
  Id_Sector: TIdSector absolute aIdBuffer;

function DirectIdentify(I: DWORD): Boolean;

implementation

var
  OSVersionInfo: TOSVersionInfo;

function GetPhysicalDriveHandle(DriveNum: Byte; DesireAccess: ACCESS_MASK): THandle;
var
  S: string;
begin
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  if OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then // Windows NT, Windows 2000
  begin
    Str(DriveNum, S); // avoid SysUtils
    Result := CreateFile(PChar('\\.\PhysicalDrive' + S), DesireAccess, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  end
  else // Windows 95 OSR2, Windows 98
    Result := CreateFile('\\.\SMARTVSD', 0, 0, nil, CREATE_NEW, 0, 0);
{$IFDEF debug}
  if Result = INVALID_HANDLE_VALUE then
    OutputDebugString(PChar('Error on CreateFile: ' + SysErrorMessage(GetLastError)));
{$ENDIF}
end;

function SmartIdentifyDirect(hDevice: THandle; bDriveNum: Byte; bIDCmd: Byte; var IdSector: TIdSector; var IdSectorSize: LongInt): BOOL;
const
  BufferSize = SizeOf(TSendCmdOutParams) + IDENTIFY_BUFFER_SIZE - 1;
var
  SCIP: TSendCmdInParams;
  Buffer: Array [0 .. BufferSize - 1] of Byte;
  SCOP: TSendCmdOutParams absolute Buffer;
  dwBytesReturned: DWORD;
begin
  FillChar(SCIP, SizeOf(TSendCmdInParams) - 1, #0);
  FillChar(Buffer, BufferSize, #0);
  dwBytesReturned := 0;
  IdSectorSize := 0;
  // Set up data structures for IDENTIFY command.
  with SCIP do
  begin
    cBufferSize := IDENTIFY_BUFFER_SIZE;
    bDriveNumber := bDriveNum;
    with irDriveRegs do
    begin
      bFeaturesReg := 0;
      bSectorCountReg := 1;
      bSectorNumberReg := 1;
      bCylLowReg := 0;
      bCylHighReg := 0;
      bDriveHeadReg := $A0 or ((bDriveNum and 1) shl 4);
      bCommandReg := bIDCmd; // The command can either be IDE identify or ATAPI identify.
    end;
  end;
  Result := DeviceIoControl(hDevice, DFP_RECEIVE_DRIVE_DATA, @SCIP, SizeOf(TSendCmdInParams) - 1, @SCOP, BufferSize, dwBytesReturned, nil);
  if Result then
  begin
    IdSectorSize := dwBytesReturned - SizeOf(TSendCmdOutParams) + 1;
    if IdSectorSize <= 0 then
      IdSectorSize := 0
    else
      System.Move(SCOP.bBuffer, IdSector, IdSectorSize);
  end;
end;

function DirectIdentify(I: DWORD): Boolean;
var
  B: Boolean;
  hDevice: THandle;
  nIdSectorSize: LongInt;
begin
  B := False;
  FillChar(aIdBuffer, SizeOf(aIdBuffer), #0);
  try
    hDevice := GetPhysicalDriveHandle(I, GENERIC_READ or GENERIC_WRITE);
    try
{$IFDEF debug}
      OutputDebugString(PChar('GetPhysicalDriveHandle return ' + IntToHex(hDevice, 8)));
{$ENDIF}
      if (hDevice <> INVALID_HANDLE_VALUE) then
        try
          B := SmartIdentifyDirect(hDevice, 0, IDE_ID_FUNCTION, Id_Sector, nIdSectorSize);
        finally
{$IFDEF debug}
          OutputDebugString('PrintIdSectorInfo end');
{$ENDIF}
        end;
    finally
      Result := B;
      CloseHandle(hDevice);
    end;
  except
    Result := False;
  end;
end;

function Hex_To_Dec(S: string): Int64;
var
  C: CHAR;
  J: Integer;
  II: Int64;
  IH: Int64;
begin
  IH := 0;
  try
    II := 1;
    while (S <> '') do
    begin
      C := S[Length(S)];
      if (C in ['0' .. '9']) then
        J := StrToInt(C)
      else
        J := Ord(C) - 55;
      IH := IH + II * J;
      Delete(S, Length(S), 1);
      II := II * 16;
    end;
  finally
    Result := IH;
  end;
end;

procedure ChangeByteOrder(var Data; Size: Integer);
var
  ptr: PChar;
  I: Integer;
  C: CHAR;
begin
  ptr := @Data;
  for I := 0 to (Size shr 1) - 1 do
  begin
    C := ptr^;
    ptr^ := (ptr + 1)^;
    (ptr + 1)^ := C;
    Inc(ptr, 2);
  end;
end;

function Physical_SN(drive: CHAR): Int64;
type
  TacOutBuffer = array [0 .. 40] of CHAR;
var
  Id: Integer;
  I: Integer;
  IH: Int64;
  acOutBuffer: TacOutBuffer;
  T: string;
begin
  Id := Ord(AnsiUpperCase(drive)[1]) - 64;
  Id := 0;

  IH := 0;
  try
    DirectIdentify(Id);
    with Id_Sector do
    begin
      ChangeByteOrder(sSerialNumber, SizeOf(sSerialNumber));
      acOutBuffer[SizeOf(sSerialNumber)] := #0;
      StrLCopy(acOutBuffer, sSerialNumber, SizeOf(sSerialNumber));
      I := 0;
      T := '';
      while (acOutBuffer[I] in [#32 .. #127]) do
      begin
        T := T + acOutBuffer[I];
        Inc(I);
      end;
      T := Trim(T);
      IH := Hex_To_Dec(T);
    end;
  finally
    Result := IH;
  end;
end;



// --------------------

function GetVolSN(aVolumn: CHAR): string;
// need admin rights
var
  Volumn: string;
  sVolName, sFName: Array [0 .. 20] of CHAR;
  dwVolSN, dwMaxFNameLen, dwFlag: DWORD;
begin
  Volumn := AnsiUpperCase(aVolumn)[1] + ':\';
  Result := ' Nothing Found !';

  if GetVolumeInformation(PChar(Volumn), sVolName, 20,
    @dwVolSN, dwMaxFNameLen, dwFlag, sFName, 20) then
    Result := IntToHex(dwVolSN, 8);
end;

function Get_HDD_Capacity_Bytes(drive: CHAR): Int64;
var
  I: Int64;
  W: Word;
begin
  W := Ord(AnsiUpperCase(drive)[1]) - 64;

  I := 0;
  try
    try
      I := DiskSize(W);
    except
      I := 0;
    end;
  finally
    Result := I;
  end;
end;

Function IsFixedDrive(drive: CHAR): Boolean;
var
  S: string;
begin
  S := AnsiUpperCase(drive) + ':\';
  Result := GetDriveType(PChar(S)) = DRIVE_FIXED
end;

// Get first IDE harddisk serial number
function GetIdeSerialNumber: String;
const
  IDENTIFY_BUFFER_SIZE = 512;
type
  TIDERegs = packed record
    bFeaturesReg: Byte; // Used for specifying SMART "commands".
    bSectorCountReg: Byte; // IDE sector count register
    bSectorNumberReg: Byte; // IDE sector number register
    bCylLowReg: Byte; // IDE low order cylinder value
    bCylHighReg: Byte; // IDE high order cylinder value
    bDriveHeadReg: Byte; // IDE drive/head register
    bCommandReg: Byte; // Actual IDE command.
    bReserved: Byte; // reserved for future use.  Must be zero.
  end;

  TSendCmdInParams = packed record
    // Buffer size in bytes
    cBufferSize: DWORD;
    // Structure with drive register values.
    irDriveRegs: TIDERegs;
    // Physical drive number to send command to (0,1,2,3).
    bDriveNumber: Byte;
    bReserved: Array [0 .. 2] of Byte;
    dwReserved: Array [0 .. 3] of DWORD;
    bBuffer: Array [0 .. 0] of Byte; // Input buffer.
  end;

  TIdSector = packed record
    wGenConfig: Word;
    wNumCyls: Word;
    wReserved: Word;
    wNumHeads: Word;
    wBytesPerTrack: Word;
    wBytesPerSector: Word;
    wSectorsPerTrack: Word;
    wVendorUnique: Array [0 .. 2] of Word;
    sSerialNumber: Array [0 .. 19] of CHAR;
    wBufferType: Word;
    wBufferSize: Word;
    wECCSize: Word;
    sFirmwareRev: Array [0 .. 7] of CHAR;
    sModelNumber: Array [0 .. 39] of CHAR;
    wMoreVendorUnique: Word;
    wDoubleWordIO: Word;
    wCapabilities: Word;
    wReserved1: Word;
    wPIOTiming: Word;
    wDMATiming: Word;
    wBS: Word;
    wNumCurrentCyls: Word;
    wNumCurrentHeads: Word;
    wNumCurrentSectorsPerTrack: Word;
    ulCurrentSectorCapacity: DWORD;
    wMultSectorStuff: Word;
    ulTotalAddressableSectors: DWORD;
    wSingleWordDMA: Word;
    wMultiWordDMA: Word;
    bReserved: Array [0 .. 127] of Byte;
  end;

  PIdSector = ^TIdSector;

  TDriverStatus = packed record
    // Error code from driver, or 0 if no error.
    bDriverError: Byte;
    // Contents of IDE Error register. Only valid when bDriverError is SMART_IDE_ERROR.
    bIDEStatus: Byte;
    bReserved: Array [0 .. 1] of Byte;
    dwReserved: Array [0 .. 1] of DWORD;
  end;

  TSendCmdOutParams = packed record
    // Size of bBuffer in bytes
    cBufferSize: DWORD;
    // Driver status structure.
    DriverStatus: TDriverStatus;
    // Buffer of arbitrary length in which to store the data read from the drive.
    bBuffer: Array [0 .. 0] of Byte;
  end;

var
  hDevice: THandle;
  cbBytesReturned: DWORD;
  ptr: PChar;
  SCIP: TSendCmdInParams;
  aIdOutCmd: Array [0 .. (SizeOf(TSendCmdOutParams) + IDENTIFY_BUFFER_SIZE - 1) - 1] of Byte;
  IdOutCmd: TSendCmdOutParams;
  procedure ChangeByteOrder(var Data; Size: Integer);
  var
    ptr: PChar;
    I: Integer;
    C: CHAR;
  begin
    ptr := @Data;
    for I := 0 to (Size shr 1) - 1 do
    begin
      C := ptr^;
      ptr^ := (ptr + 1)^;
      (ptr + 1)^ := C;
      Inc(ptr, 2);
    end;
  end;

begin
  Result := ''; // return empty string on error
  if SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT then // Windows NT, Windows 2000
  begin
    // warning! change name for other drives: ex.: second drive '\\.\PhysicalDrive1\'
    hDevice := CreateFile('\\.\PhysicalDrive0', GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  end
  else // Version Windows 95 OSR2, Windows 98
    hDevice := CreateFile('\\.\SMARTVSD', 0, 0, nil, CREATE_NEW, 0, 0);
  if hDevice = INVALID_HANDLE_VALUE then
    Exit;
  try
    FillChar(SCIP, SizeOf(TSendCmdInParams) - 1, #0);
    FillChar(aIdOutCmd, SizeOf(aIdOutCmd), #0);
    cbBytesReturned := 0;
    // Set up data structures for IDENTIFY command.
    with SCIP do
    begin
      cBufferSize := IDENTIFY_BUFFER_SIZE;
      // bDriveNumber := 0;
      with irDriveRegs do
      begin
        bSectorCountReg := 1;
        bSectorNumberReg := 1;
        // if Win32Platform=VER_PLATFORM_WIN32_NT then bDriveHeadReg := $A0
        // else bDriveHeadReg := $A0 or ((bDriveNum and 1) shl 4);
        bDriveHeadReg := $A0;
        bCommandReg := $EC;
      end;
    end;
    if not DeviceIoControl(hDevice, $0007C088, @SCIP, SizeOf(TSendCmdInParams) - 1,
      @aIdOutCmd, SizeOf(aIdOutCmd), cbBytesReturned, nil) then
      Exit;
  finally
    CloseHandle(hDevice);
  end;
  with PIdSector(@IdOutCmd.bBuffer)^ do
  begin
    ChangeByteOrder(sSerialNumber, SizeOf(sSerialNumber));
    (PChar(@sSerialNumber) + SizeOf(sSerialNumber))^ := #0;
    Result := PChar(@sSerialNumber);
  end;
end;

end.
