Unit MaxLogic.Linux.CLI;

Interface

Uses
  System.SysUtils,
  // set the `UseWSL` conditional on windows to enforce a emulation using the windows subsystem for linux on windows
  {$IF Defined(MsWindows) AND Defined(UseWSL)}
  MaxLogic.ioUtils,
  {$ELSE}
  Posix.Base, Posix.Fcntl,
  {$IFEND}
  System.Classes,
  System.Generics.Collections;

Type
  TStrProc = TProc<String>;

/// <summary>
/// executes a linux command.
/// it will retrive the terminal output and call the aBufferReady each time we have a chunk of data
/// attention: THE LINES WILL END WITH #10, SO YOU MIGHT WANT TO CALL tRIMlEFT ON THE RESULT
/// Returns the exit code or -1 if the process failed
/// </summary>
function LinuxCmd(Const aCommand: String; aBufferReady: TStrProc; aBufferSize: integer = 512 * 1024): integer; overload;

/// <summary>
/// a simplified version of the above.
/// best SUITED FOR COMMANDS THAT RETURN A SINGLE LINE AS OUTPUT
/// Please note, that this might not be suiteable for commands that will retrive large amount of text
/// attention: A right trim will be performed on the result to remove the trailing line break
/// </summary>
Function LinuxCmd(Const aCommand: String): String; Overload;
Function LinuxCmd(Const aCommand: String; out aExitCode: integer): String; Overload;


// following are internal methods. They could be in the implementation section but might be used somewhere else too. so I decided to put them here
{$IF NOT(Defined(MsWindows) AND Defined(UseWSL))}
Type
  TStreamHandle = pointer;

/// <summary>
/// Man Page: http://man7.org/linux/man-pages/man3/fgets.3p.html
/// </summary>
Function fgets(buffer: pointer; size: int32; Stream: TStreamHandle): pointer; Cdecl; External libc Name _PU + 'fgets';

/// <summary>
/// Man Page: http://man7.org/linux/man-pages/man3/popen.3.html
/// </summary>
Function popen(Const command: MarshaledAString; Const _type: MarshaledAString): TStreamHandle; Cdecl; External libc Name _PU + 'popen';

/// <summary>
/// Man Page: http://man7.org/linux/man-pages/man3/pclose.3p.html
/// </summary>
Function pclose(filehandle: TStreamHandle): int32; Cdecl; External libc Name _PU + 'pclose';

/// <summary>
/// Utility function to return a buffer of ASCII-Z data as a string.
/// </summary>
Function BufferToString(buffer: pointer; MaxSize: uint32): String;
{$IFEND}


Implementation

Uses
  StrUtils;

{$IF NOT(Defined(MsWindows) AND Defined(UseWSL))}

Function BufferToString(buffer: pointer; MaxSize: uint32): String;
Var
  cursor: ^uint8;
  EndOfBuffer: nativeuint;
Begin
  Result := '';
  If Not assigned(buffer) Then
    exit;

  cursor := buffer;
  EndOfBuffer := nativeuint(cursor) + MaxSize;

  While (nativeuint(cursor) < EndOfBuffer) And (cursor^ <> 0) Do
  Begin
    Result := Result + chr(cursor^);
    cursor := pointer(succ(nativeuint(cursor)));
  End;
End;

function LinuxCmd(Const aCommand: String; aBufferReady: TStrProc; aBufferSize: integer = 512 * 1024): integer;
Var
  Handle: TStreamHandle;
  Data: Array Of uint8;
  s: String;
  ansi: AnsiString;
Begin
  Result := -1;
  SetLength(Data, aBufferSize);
  ansi := AnsiString(aCommand);

  Handle := popen(pAnsiChar(ansi), 'r');
  If Handle = nil Then
  Begin
    raise Exception.Create('Failed to execute command: ' + aCommand);
    exit;
  end;
  Try
    While fgets(@Data[0], Sizeof(Data), Handle) <> Nil Do
    Begin
      s := BufferToString(@Data[0], Sizeof(Data));
      If assigned(aBufferReady) Then
        aBufferReady(s);
    End;
  Finally
    // pclose(handle); returns a value that if the process exited normally the top 8 bits are the exit code. So quick and dirty: ExitCode := pclose(handle) div 256; See: stackoverflow.com/questions/15058876
    Result := pclose(Handle);
    if Result <> -1 then
      Result := Result div 256;
  End;
End;

{$ELSE}
function LinuxCmd(Const aCommand: String; aBufferReady: TStrProc; aBufferSize: integer = 512 * 1024): integer; overload;
begin
  MaxLogic.ioUtils.ExecuteFile(
    'bash ' + AnsiQuotedStr(aCommand, '`'),
    '', // working dir
    Result,
    // stdOut
    aBufferReady,

    // errOut
     nil, // the linuxCmd does not capture the ErrOut at all... so let us be consequent and do not capture it as well
    True);
end;
{$IFEND}


Function LinuxCmd(Const aCommand: String): String;
var
  lExitCode: integer;
begin
  Result := LinuxCmd(aCommand, lExitCode);
end;

Function LinuxCmd(Const aCommand: String; out aExitCode: integer): String;
Var
  s: String;
Begin
  s := '';

  aExitCode := LinuxCmd(aCommand,
      procedure(aText: String)
    Begin
      s := s + aText;
    End);

  Result := TrimRight(s);
End;

End.
