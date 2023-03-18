unit maxLogic.AnonymousProcToEvent;

{ This simple unit will allow us to assign anonymous methods to normal TNotifyEvent based events. }

interface

uses
  classes, sysUtils;

type
  { use like this:
    button1.onClick:=TAnonymousProcToEventHandler.new(button1, procedure begin DoSomething(); end); }
  TAnonymousProcToEventHandler = class(TComponent)
  private
    fProc: TProc;
    procedure event(sender: Tobject);
  public
    class function new(aParent: TComponent; aProc: TProc): TNotifyEvent;
  end;

implementation

{ TAnonymousProcToEventHandler }

procedure TAnonymousProcToEventHandler.event(sender: Tobject);
begin
  if assigned(fProc) then
    fProc();
end;

class function TAnonymousProcToEventHandler.new(aParent: TComponent; aProc: TProc): TNotifyEvent;
var
  p: TAnonymousProcToEventHandler;
begin
  p := TAnonymousProcToEventHandler.create(aParent);
  p.fProc := aProc;
  result := p.event;
end;

end.


