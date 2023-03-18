Unit AutoHourGlass;

{ Copyright: Pawel Piotrowski; MaxLogic; www.maxlogic.eu
  License: free to use; no warranty at all; use as is

  How to use:

  var
  Glass : IInterface;
  begin
  Glass := AutoHourGlass.MakeCHG;
  ....
  end;

  thats alll

}
Interface

Uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics;

Type
  TAutoHourGlass = Class(TInterfacedObject)
  Private
    fCursor: TCursor;
    fCaptured: Boolean;
  Public
    Constructor Create;
    Destructor Destroy; Override;

  End;

Function MakeCHG: IInterface;

Implementation

Function MakeCHG: IInterface;
Begin
  Result := TAutoHourGlass.Create;
End;

{ TAutoHourGlass }

Constructor TAutoHourGlass.Create;
Begin
  Inherited;
  If screen.cursor <> crHourGlass Then
  Begin
      fCaptured := True;
    fCursor := screen.cursor;
    screen.cursor := crHourGlass;
  End;
End;

Destructor TAutoHourGlass.Destroy;
Begin
  If fCaptured Then
    screen.cursor := fCursor;
  Inherited;
End;

End.
