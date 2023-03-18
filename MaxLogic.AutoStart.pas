unit MaxLogic.AutoStart;

interface

uses
  windows, classes, sysUtils;

// adds application.exeName to autostart
procedure AddToAutoStart(const aParams: string = ''); Overload;
procedure AddToAutoStart(const aTitle, aFilename: string;
  const aParams: string = ''); Overload;

procedure RemoveFromAutoStart; Overload;
procedure RemoveFromAutoStart(const aFilename: string); Overload;

function isInAutoStart: boolean; Overload;
function isInAutoStart(aFilename: string): boolean; Overload;

implementation

uses
  registry, forms, strUtils;

procedure AddToAutoStart(const aParams: string = '');
var
  Title, Filename: string;
begin
  if Application.Title <> '' then
    Title := Application.Title
  else
    Title := ChangeFileExt(ExtractFilename(Application.ExeName), '');
  Filename := Application.ExeName;

  AddToAutoStart(Title, Filename, aParams);
end;

procedure AddToAutoStart(const aTitle, aFilename: string;
  const aParams: string = '');
var
  KeyName, Value: string;
  reg: TRegistry;
begin
  if aFilename = '' then
    Exit;
  if aTitle = '' then
    KeyName := ExtractFilename(aFilename)
  else
    KeyName := aTitle;

  if aFilename[1] <> '"' then
    Value := '"' + aFilename
  else
    Value := aFilename;
  if Value[Length(Value)] <> '"' then
    Value := Value + '"';

  if aParams <> '' then
  begin
    if aParams[1] <> ' ' then
      Value := Value + ' ' + aParams
    else
      Value := Value + aParams;
  end;

  reg := TRegistry.Create;
  // HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Run
  reg.rootkey := HKEY_CURRENT_USER;
  reg.openkey('\Software\Microsoft\Windows\CurrentVersion\Run', true);

  if (not reg.ValueExists(KeyName)) or (reg.readstring(KeyName) <> Value) then
    reg.WriteString(KeyName, Value);
  reg.Free;

end;

procedure RemoveFromAutoStart;
begin
  RemoveFromAutoStart(Application.ExeName);
end;

procedure RemoveFromAutoStart(const aFilename: string);
var
  reg: TRegistry;
  s: string;
  l: TStringList;
  x: Integer;
begin
  reg := TRegistry.Create;
  // HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Run
  reg.rootkey := HKEY_CURRENT_USER;
  reg.openkey('\Software\Microsoft\Windows\CurrentVersion\Run', true);
  l := TStringList.Create;
  reg.GetValueNames(l);

  for x := 0 to l.Count - 1 do
  begin
    s := Trim(reg.readstring(l[x]));
    if s = '' then
      continue;

    if s[1] = '"' then
      s := AnsiDequotedStr(s, '"');

    if startsText(aFilename, s) then // remember, there might be some command line params attached, so just test the start of this string
      reg.DeleteValue(l[x]);
  end;
  l.Free;
  reg.Free;
end;

function isInAutoStart: boolean;
begin
  result := isInAutoStart(Application.ExeName);
end;

function isInAutoStart(aFilename: string): boolean;
var
  reg: TRegistry;
  s: string;
  l: TStringList;
  x: Integer;
begin
  result := False;
  reg := TRegistry.Create;
  // HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Run
  reg.rootkey := HKEY_CURRENT_USER;
  reg.openkey('\Software\Microsoft\Windows\CurrentVersion\Run', true);
  l := TStringList.Create;
  reg.GetValueNames(l);

  for x := 0 to l.Count - 1 do
  begin
    s := Trim(reg.readstring(l[x]));
    if s = '' then
      continue;

    if s[1] = '"' then
      s := AnsiDequotedStr(s, '"');

    if startsText(aFilename, s) then // remember, there might be some command line params attached, so just test the start of this string
    begin
      result := true;
      Break;
    end;
  end;
  l.Free;
  reg.Free;

end;

end.

