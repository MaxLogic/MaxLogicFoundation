unit MaxLogic.RawInput;

interface

{
  Version: 1
}

uses
  windows, classes, sysUtils, controls, types, messages,
  CnRawInput, diagnostics, forms, maxWndSubClassing;

Type
  TRawMouseEvent = procedure(sender: Tobject; const RawMouse: RawMouse; FromMouse: THandle) of object;

  TmaxRawInput = class
  private
    FOnRawKeyDown: TOnRawKeyDown;
    FOnRawKeyUp: TOnRawKeyUp;
    fLastMouseMessage,
      fLastKeyboardMessage: TStopWatch;
    FOnAnyKeyOrMessageEvent: TThreadProcedure;
    FOnRawMouseEvent: TRawMouseEvent;
    fForm: TForm;
    fWndSubClass: TWndSubClass;
    fData:TBytes;
    procedure SetOnAnyKeyOrMessageEvent(const Value: TThreadProcedure);
    procedure SetOnRawMouseEvent(const Value: TRawMouseEvent);
    function GetElapsedMillisecondsSinceLastKeyEvent: int64;
    function GetElapsedMillisecondsSinceLastMouseEvent: int64;
    function GetElapsedMillisecondsSinceLastKeyOrMouseEvent: int64;
    procedure StartUp;
  protected
    procedure WMInput(var Message: TMessage; var PrevendDefaultHandler: boolean); // message WM_INPUT;
  public
    constructor Create;
    destructor Destroy; override;

    property OnRawKeyDown: TOnRawKeyDown read FOnRawKeyDown write FOnRawKeyDown;
    property OnRawKeyUp: TOnRawKeyUp read FOnRawKeyUp write FOnRawKeyUp;
    // see details of the mouse item here: https://msdn.microsoft.com/en-us/library/windows/desktop/ms645578.aspx
    property OnRawMouseEvent: TRawMouseEvent read FOnRawMouseEvent write SetOnRawMouseEvent;
    // if you just need to know if there was a key r mouse event. just like a ping to let you know
    property OnAnyKeyOrMessageEvent: TThreadProcedure read FOnAnyKeyOrMessageEvent write SetOnAnyKeyOrMessageEvent;

    property ElapsedMillisecondsSinceLastKeyEvent: int64 read GetElapsedMillisecondsSinceLastKeyEvent;
    property ElapsedMillisecondsSinceLastMouseEvent: int64 read GetElapsedMillisecondsSinceLastMouseEvent;
    property ElapsedMillisecondsSinceLastKeyOrMouseEvent: int64 read GetElapsedMillisecondsSinceLastKeyOrMouseEvent;

  end;

implementation

uses
  math;

constructor TmaxRawInput.Create;
begin
  inherited;
  fForm := TForm.Create(nil);
  fWndSubClass := TWndSubClass.Create(fForm);
  fWndSubClass.OnMessage := self.WMInput;
  fWndSubClass.HookedWnd := fForm.handle;

  fLastMouseMessage := TStopWatch.startnew;
  fLastKeyboardMessage := TStopWatch.startnew;

  StartUp;
end;

procedure TmaxRawInput.StartUp;
var
  RID: array [0 .. 1] of TRawInputDevice;
begin
  inherited;

  // In this sample, an application wants raw input from the keyboard and mouse but wants to ignore legacy window messages (which would come from the same keyboard and mouse).
  RID[0].usUsagePage := $01;
  RID[0].usUsage := $02;
  RID[0].dwFlags := RIDEV_INPUTSINK;
  RID[0].hwndTarget := fForm.handle;

  RID[1].usUsagePage := $01;
  RID[1].usUsage := $06;
  RID[1].dwFlags := RIDEV_INPUTSINK;
  RID[1].hwndTarget := fForm.handle;

  if not RegisterRawInputDevices(@RID, 2, sizeof(RID[0])) then
    raise Exception.Create('RegisterRawInputDevices error!');
end;

procedure TmaxRawInput.WMInput(var Message: TMessage; var PrevendDefaultHandler: boolean);
var
  Ri: tagRAWINPUT;
  Size: UINT;
begin
  if (Message.Msg = WM_INPUT) then
  // if (Message.WParam and $FF) in [RIM_INPUT, RIM_INPUTSINK] then
  begin

  // get size
    Size := sizeof(tagRAWINPUT);
  GetRawInputData(
  HRAWINPUT(Message.LParam),
  RID_INPUT, nil, Size, sizeof(RAWINPUTHEADER));

  if size<>0 then
begin
if size>length(fData) then
    SetLength(fdata, Size);


{  ri:=default(tagRAWINPUT);
    Ri.header.dwSize := sizeof(RAWINPUTHEADER);

    Move(ri, data[0],
    min(sizeOf(ri), length(data)));}




    if GetRawInputData(
    HRAWINPUT(Message.LParam),
    RID_INPUT, @fdata[0], Size, sizeof(RAWINPUTHEADER)) = Size then
    begin
      move(fdata[0], ri,
          min(sizeOf(ri), size));

      if (Ri.header.dwType = RIM_TYPEKEYBOARD) then
      begin
        case Ri.keyboard.Message of
          WM_KEYDOWN, WM_KEYUP:
            begin
              fLastKeyboardMessage := TStopWatch.startnew;
              if assigned(FOnAnyKeyOrMessageEvent) then
                FOnAnyKeyOrMessageEvent();
            end;
        end;

        if (Ri.keyboard.Message = WM_KEYDOWN) and assigned(FOnRawKeyDown) then
          FOnRawKeyDown(self, Ri.keyboard.VKey, Ri.header.hDevice)
        else if (Ri.keyboard.Message = WM_KEYUP) and assigned(FOnRawKeyUp) then
          FOnRawKeyUp(self, Ri.keyboard.VKey, Ri.header.hDevice);

      end else
        if (Ri.header.dwType = RIM_TYPEMOUSE) then
      begin
        fLastMouseMessage := TStopWatch.startnew;

        if assigned(FOnAnyKeyOrMessageEvent) then
          FOnAnyKeyOrMessageEvent();

        if assigned(FOnRawMouseEvent) then
          FOnRawMouseEvent(self, Ri.mouse, Ri.header.hDevice);

      end;
    end;
end;

  end;
end;

destructor TmaxRawInput.Destroy;
begin

  fWndSubClass.free;
  fForm.free;
  inherited;
end;

procedure TmaxRawInput.SetOnAnyKeyOrMessageEvent(const Value: TThreadProcedure);
begin
  FOnAnyKeyOrMessageEvent := Value;
end;

procedure TmaxRawInput.SetOnRawMouseEvent(const Value: TRawMouseEvent);
begin
  FOnRawMouseEvent := Value;
end;

function TmaxRawInput.GetElapsedMillisecondsSinceLastKeyEvent: int64;
begin
  result := fLastKeyboardMessage.ElapsedMilliseconds
end;

function TmaxRawInput.GetElapsedMillisecondsSinceLastMouseEvent: int64;
begin
  result := fLastMouseMessage.ElapsedMilliseconds;
end;

function TmaxRawInput.GetElapsedMillisecondsSinceLastKeyOrMouseEvent: int64;
begin
  result := min(ElapsedMillisecondsSinceLastKeyEvent,
    ElapsedMillisecondsSinceLastMouseEvent);
end;

end.
