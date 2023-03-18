unit maxLogic.Latin1Types;

interface

uses
  windows, classes, sysUtils;

type
  Latin1String = type AnsiString(28591); // codepage 28591 = ISO-8859-1
var
  Latin1Encoding: TEncoding;

implementation

initialization
  Latin1Encoding := TEncoding.GetEncoding('ISO-8859-1');

end.
