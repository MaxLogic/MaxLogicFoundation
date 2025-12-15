unit FuncReplacement;

{$I FuncReplacement.inc}

interface

uses
  Classes, Dialogs, SyncObjs;

type
  // Dump of original func, pointers to Original & New funcs
  TFuncReplacement = class
  private
    OrigDump: packed array [0 .. 4] of byte;
    OrigFunc, MyFunc: pointer;
    fReplaced: boolean; // Is func replaced now

    procedure SetReplaced(aReplaced: boolean);
  public
    constructor Create(aOrigFunc, aMyFunc: pointer);
    destructor Destroy; override;
    property Replaced: boolean read fReplaced write SetReplaced;
  end;

function GetActualAddr(Proc: pointer): pointer;
function IsWin9xDebugThunk(AnAddr: pointer): boolean; // extract from JclPeImage.pas

type
  TSaveRedir = packed record
    Addr: pointer;
    Bytes: array [0 .. 4] of byte;
  end;

  PSaveRedir = ^TSaveRedir;

  /// Example: Replace Application.MessageBox with your own.
  { S: TSaveRedir;

    initialization
    RedirectCall(@TApplication.MessageBox, @MyNewMessageBox, @S); }

procedure RedirectCall(FromAddr, ToAddr: pointer; SaveRedir: PSaveRedir);
procedure UndoRedirectCall(const SaveRedir: TSaveRedir);

implementation

uses
  Windows, SysUtils, TypInfo, Forms
{$IFDEF D7} , uLegacyCode {$ENDIF}
{$IFNDEF D5} , StrUtils {$ENDIF};

type
  PWin9xDebugThunk = ^TWin9xDebugThunk;

  TWin9xDebugThunk = packed record
    PUSH: byte; // PUSH instruction opcode ($68)
    Addr: pointer; // The actual address of the DLL routine
    JMP: byte; // JMP instruction opcode ($E9)
    Rel: integer; // Relative displacement (a Kernel32 address)
  end;

  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;

  TAbsoluteIndirectJmp = packed record
    OpCode: Word;
    Addr: PPointer;
  end;

function IsWin9xDebugThunk(AnAddr: pointer): boolean; // extract from JclPeImage.pas
{ -> EAX: AnAddr }
asm
  TEST EAX, EAX
  JZ  @@NoThunk
  CMP BYTE PTR [EAX].TWin9xDebugThunk.PUSH, $68
  JNE @@NoThunk
  CMP BYTE PTR [EAX].TWin9xDebugThunk.JMP, $E9
  JNE @@NoThunk
  XOR EAX, EAX
  MOV AL, 1
  JMP @@exit
@@NoThunk:
  XOR EAX, EAX
@@exit:
end;

function GetActualAddr(Proc: pointer): pointer;
begin
  if Proc <> nil then
  begin
    if (SysUtils.Win32Platform <> VER_PLATFORM_WIN32_NT) and IsWin9xDebugThunk(Proc) then
      Proc := PWin9xDebugThunk(Proc).Addr;
    if (PAbsoluteIndirectJmp(Proc).OpCode = $25FF) then // JMP mem32
      Result := PAbsoluteIndirectJmp(Proc).Addr^
    else
      Result := Proc;
  end
  else
    Result := nil;
end;

{ TFuncReplacement }

constructor TFuncReplacement.Create(aOrigFunc, aMyFunc: pointer);
var
  OldProtect: cardinal;
begin
  OrigFunc := GetActualAddr(aOrigFunc);
  MyFunc := aMyFunc;
  move(OrigFunc^, OrigDump[0], 5);
  VirtualProtect(OrigFunc, 5, PAGE_EXECUTE_READWRITE, @OldProtect);
end;

destructor TFuncReplacement.Destroy;
begin
  SetReplaced(false);
  inherited;
end;

procedure TFuncReplacement.SetReplaced(aReplaced: boolean);
var
  Offset: integer;
begin
  if aReplaced = fReplaced then
    exit;
  if aReplaced then
  begin // Set MyFunc
    Offset := integer(MyFunc) - integer(OrigFunc) - 5;
    byte(OrigFunc^) := $E9;
    move(Offset, pointer(cardinal(OrigFunc) + 1)^, 4);
  end
  else // Set OrigFunc
    move(OrigDump[0], OrigFunc^, 5);
  fReplaced := aReplaced;
end;


// simpler method

procedure RedirectCall(FromAddr, ToAddr: pointer; SaveRedir: PSaveRedir);
var
  OldProtect: cardinal;
  NewCode: packed record
    JMP: byte;
  Distance: integer;
end;
begin
  if not VirtualProtect(FromAddr, 5, PAGE_EXECUTE_READWRITE, OldProtect) then
    RaiseLastWin32Error;
  if Assigned(SaveRedir) then
  begin
    SaveRedir^.Addr := FromAddr;
    move(FromAddr^, SaveRedir^.Bytes, 5);
  end;
  NewCode.JMP := $E9;
  NewCode.Distance := integer(ToAddr) - integer(FromAddr) - 5;
  move(NewCode, FromAddr^, 5);
  if not VirtualProtect(FromAddr, 5, OldProtect, OldProtect) then
    RaiseLastWin32Error;
end;

procedure UndoRedirectCall(const SaveRedir: TSaveRedir);
var
  OldProtect: cardinal;
begin
  if not VirtualProtect(SaveRedir.Addr, 5, PAGE_EXECUTE_READWRITE, OldProtect) then
    RaiseLastWin32Error;
  move(SaveRedir.Bytes, SaveRedir.Addr^, 5);
  if not VirtualProtect(SaveRedir.Addr, 5, OldProtect, OldProtect) then
    RaiseLastWin32Error;
end;

end.
