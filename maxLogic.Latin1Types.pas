unit maxLogic.Latin1Types;

interface

uses
  classes, sysUtils;

type
  Latin1String = type AnsiString(28591); // codepage 28591 = ISO-8859-1
  Win1252String = type AnsiString(1252); // codepage 1252 = Windows 1252
var
  Latin1Encoding, Win1252Encoding : TEncoding;

implementation

{$IFDEF madExcept}
uses
  madExcept;
{$ENDIF}

initialization
  Latin1Encoding := TEncoding.GetEncoding('ISO-8859-1');
  Win1252Encoding := TEncoding.GetEncoding('Windows-1252');

  {$IFDEF madExcept}
  // in case madExcept exits before the finalization of this unit
  HideLeak(Latin1Encoding);
  HideLeak(Win1252Encoding);
  {$ENDIF}
Finalization
  FreeAndNil(Latin1Encoding);
  FreeAndNil(Win1252Encoding);

end.
