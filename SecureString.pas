unit SecureString;

{ see https://medium.com/@svanas/creating-a-securestring-type-for-delphi-part-2-42e8a1762c3b
  ISecureString
  Created by Stefan van As <dutchdelphidude@gmail.com>

  Short intro
  sensitive strings (such as credit card or passwords, for example) are never really zero’ed out from memory,
  this can be a problem (for example: when your process memory is written to a hibernation file or a page file or a memory dump file).
  Zero’ing out the bytes that a Delphi string variable is pointing to isn’t all that difficult:
  But wait. It is important not to zero a string out until the reference count is 0 (zero). Otherwise, we might mistakenly zero out other string variables that are pointing to the same bytes in the memory, leading to confusing problems that are difficult to debug.
  How do we extract the reference count? Marco Cantù’s whitepaper on Delphi and Unicode tells us a lot about the internal structure of strings. It turns out that the reference count is actually stored with the string, before the actual text and before the position the string variable points to. The offset is -8 for the reference count:
  But the Delphi run-time library does not tell us when a string is about to get released from memory. There is no event. No message. No destructor.
  What we can do is “box” our strings inside another reference-counted type.
  aND THIS IS WHAT THIS CLASS IS ABOUT
  DISCLAIMER
  This software is provided 'as is' with no explicit or implied warranties
  in respect of its properties, including, but not limited to, correctness
  and/or fitness for purpose.
  ****************************************************************************
}



interface

type
  ISecureString = interface
    function Data: string;
    function Length: Integer;
  end;

  { When you have a password or a credit card, you should convert such strings to ISecureString at the earliest convenience in your project. Here is the ISecureString factory for that:
    note:
ATTENTION: you should zero the passed string after this calll}
function NewSecureString(const S: string): ISecureString;
procedure ZeroString(var S: String);

implementation

uses
  Windows;

{ TSecureString }

type
  TSecureString = class(TInterfacedObject, ISecureString)
  strict private
    FData: string;
  public
    constructor Create(const Value: string);
    destructor Destroy; override;
    function Data: string;
    function Length: Integer;
  end;

constructor TSecureString.Create(const Value: string);
begin
  inherited Create;
  system.SetString(FData, PChar(Value), system.Length(Value));
end;

destructor TSecureString.Destroy;
var
  I: Integer;
begin
  if system.Length(FData) > 0 then
  begin
    I := PInteger(PByte(FData) - 8)^;
    if (I > -1) and (I < 2) then
      ZeroMemory(Pointer(FData), system.Length(FData) * SizeOf(Char));
  end;
  inherited Destroy;
end;

function TSecureString.Data: string;
begin
  Result := FData;
end;

function TSecureString.Length: Integer;
begin
  Result := system.Length(FData);
end;

function NewSecureString(const S: string): ISecureString;
var
  I: Integer;
begin
  Result := TSecureString.Create(S);
  if system.Length(S) > 0 then
  begin
    // I := PInteger(PByte(S) - 8)^;
    // if (I > -1) and (I < 2) then
    ZeroMemory(Pointer(S), system.Length(S) * SizeOf(Char));
  end;
end;

procedure ZeroString(var S: String);
begin
  if Length(S) <> 0 then
    ZeroMemory(@S[1], system.Length(S) * SizeOf(Char));
end;

end.
