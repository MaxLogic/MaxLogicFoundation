unit maxDDE;

interface

uses
  windows, Classes, SysUtils, DdeMan, DDEml, generics.collections;

type
  TDDEMode = (dmClient, dmServer);
  TOnDDEMessage = procedure(sender: Tobject; const aItem, aMessage: string) of object;
  TOnDDEMessageLines = procedure(sender: Tobject; const aItem: string; aMessage: TStrings) of object;

  TSimpleDDE = class
  private
    FOwner: TComponent;
    FOnMessage: TOnDDEMessage;
    FTimeOut: integer;
    FDDEServer: TDdeServerConv;
    FDDEClient: TDdeClientConv;

    FDDEServerItems: TObjectDictionary<string, TDdeServerItem>;
    FDDEClientItems: TObjectDictionary<string, TDdeClientItem>;

    FOnOpen: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnExecuteMacro: TMacroEvent;
    FConnected: Boolean;
    FOnMessageLines: TOnDDEMessageLines;
    procedure SetOnMessage(const Value: TOnDDEMessage);
    procedure SetTimeOut(const Value: integer);
    procedure OnServerItemChanged(sender: Tobject);
    procedure OnClientItemChanged(sender: Tobject);
    procedure SetOnOpen(const Value: TNotifyEvent);
    procedure SetOnClose(const Value: TNotifyEvent);
    procedure DoOnOpen(sender: Tobject);
    procedure DoOnClose(sender: Tobject);
    procedure SetOnExecuteMacro(const Value: TMacroEvent);
    function Get_LastError: string;
    procedure SetOnMessageLines(const Value: TOnDDEMessageLines);
  public
    constructor Create(Owner: TComponent);
    destructor Destroy; override;

    function Connect(const aService, aTopic: string; Mode: TDDEMode): Boolean;
    procedure Disconnect;

    function OpenItemLink(const aItemName: string): Boolean;
    function CloseItemLink(const aItemName: String): Boolean;

    procedure ItemChange(const aItemName: AnsiString; aItemData: AnsiString); overload;
    procedure ItemChange(const aItemName: String; aItemData: TStrings); overload;

    function ExecuteMacro(const aMacro: AnsiString): Boolean;
    function PokeData(const aItem, aMessage: string): Boolean;

    property Connected: Boolean read FConnected;
    property LastError: string read Get_LastError;
    property TimeOut: integer read FTimeOut write SetTimeOut;
    Property OnMessage: TOnDDEMessage read FOnMessage write SetOnMessage;
    Property OnMessageLines: TOnDDEMessageLines read FOnMessageLines write SetOnMessageLines;
    property OnOpen: TNotifyEvent read FOnOpen write SetOnOpen;
    property OnClose: TNotifyEvent read FOnClose write SetOnClose;
    property OnExecuteMacro: TMacroEvent read FOnExecuteMacro write SetOnExecuteMacro;

    property DDECLient: TDdeClientConv read FDDEClient;
  end;

implementation

{ TSimpleDDE }

function TSimpleDDE.CloseItemLink(const aItemName: String): Boolean;
begin
  Result := false;
  if assigned(FDDEClient) then
  begin
    if FDDEClientItems.containskey(aItemName) then
    begin
      FDDEClientItems.Remove(aItemName);
      Result := true;
    end;
  end;
  if assigned(FDDEServer) then
  begin
    if FDDEServerItems.containskey(aItemName) then
    begin
      FDDEServerItems.Remove(aItemName);
      Result := true;
    end;
  end;
end;

function TSimpleDDE.Connect(const aService, aTopic: string;
  Mode: TDDEMode): Boolean;
begin
  Result := false;
  Disconnect;
  if Mode = dmClient then
  begin
    FDDEClient := TDdeClientConv.Create(FOwner);
    FDDEClient.ConnectMode := ddeAutomatic;
    FDDEClient.OnOpen := DoOnOpen;
    FDDEClient.OnClose := DoOnClose;
    FDDEClient.FormatChars := true;
    Result := FDDEClient.SetLink(aService, aTopic);
    if not Result then
      Exit;

  end
  else
    if Mode = dmServer then
  begin
    FDDEServer := TDdeServerConv.Create(FOwner);
    FDDEServer.Name := aTopic;
    FDDEServer.OnExecuteMacro := FOnExecuteMacro;
    FDDEServer.OnOpen := DoOnOpen;
    FDDEServer.OnClose := DoOnClose;
    Result := true;
  end;
end;

constructor TSimpleDDE.Create(Owner: TComponent);
begin
  FDDEServerItems := TObjectDictionary<string, TDdeServerItem>.Create([doOwnsValues]);
  FDDEClientItems := TObjectDictionary<string, TDdeClientItem>.Create([doOwnsValues]);
  FOwner := Owner;
  FDDEServer := nil;
  FDDEClient := nil;
  FConnected := false;
end;

destructor TSimpleDDE.Destroy;
begin
  Disconnect;
  FDDEClientItems.free;
  FDDEServerItems.free;
  inherited;
end;

procedure TSimpleDDE.Disconnect;
var
  di: TDdeClientItem;
begin
  for di in FDDEClientItems.values do
  begin
    di.OnChange := nil;
  end;
  FDDEClientItems.clear;

  if assigned(FDDEServer) then
    FreeAndNil(FDDEServer);
  if assigned(FDDEClient) then
    FreeAndNil(FDDEClient);

  FConnected := false;
end;

procedure TSimpleDDE.DoOnClose(sender: Tobject);
begin
  FConnected := false;
  if assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TSimpleDDE.DoOnOpen(sender: Tobject);
begin
  FConnected := true;
  if assigned(FOnOpen) then
    FOnOpen(Self);
end;

function TSimpleDDE.ExecuteMacro(const aMacro: AnsiString): Boolean;
begin
  if assigned(FDDEClient) then
    Result := FDDEClient.ExecuteMacro(PAnsiChar(aMacro), false)
  else
    raise Exception.Create('dde Execute Makro is available only for DDE Clients');
end;

function TSimpleDDE.Get_LastError: string;
begin
  Result := SysErrorMessage(GetLastError);
end;

procedure TSimpleDDE.ItemChange(const aItemName: String; aItemData: TStrings);
var
  s: string;
  cmd: pchar;
  DDEData: hDDEData;
  hszItem: HSZ;
begin
  if FDDEServer <> nil then
  begin
    hszItem := DdeCreateStringHandle(ddeMgr.DdeInstId, pchar(aItemName), CP_WINANSI);

    s := Trim(aItemData.text) + #0;
    DDEData := DdeCreateDataHandle(ddeMgr.DdeInstId, pchar(s), Length(s),
      0, hszItem, CF_TEXT, 0);

    FDDEServer.ExecuteMacro(DDEData);
  end
  else if assigned(FDDEClient) then
    FDDEClient.PokeDataLines(aItemName, aItemData);
end;

procedure TSimpleDDE.ItemChange(const aItemName: AnsiString; aItemData: AnsiString);
var
  item: TDdeServerItem;
begin
  if assigned(FDDEClient) then
    FDDEClient.PokeData(aItemName, PAnsiChar(aItemData))
  else
    if assigned(FDDEServer) then
  begin
    if FDDEServerItems.trygetvalue(aItemName, item) then
      item.text := aItemData
    else
    begin
      raise Exception.Create('There is not DDEServerItem with the name : "' + aItemName + '"');
    end;
  end;
end;

procedure TSimpleDDE.OnClientItemChanged(sender: Tobject);
var
  DDEServerItem: TDdeServerItem;
begin
  DDEServerItem := sender as TDdeServerItem;

  if assigned(FOnMessage) and assigned(DDEServerItem) then
    FOnMessage(Self, DDEServerItem.Name, DDEServerItem.lines.text);

  if assigned(FOnMessageLines) and assigned(DDEServerItem) then
    FOnMessageLines(Self, DDEServerItem.Name, DDEServerItem.lines);
end;

procedure TSimpleDDE.OnServerItemChanged(sender: Tobject);
var
  DDEClientItem: TDdeClientItem;
begin
  DDEClientItem := sender as TDdeClientItem;

  if assigned(FOnMessage) and assigned(DDEClientItem) then
    FOnMessage(Self, DDEClientItem.DdeItem, TrimRight(DDEClientItem.lines.text));
  if assigned(FOnMessageLines) and assigned(DDEClientItem) then
    FOnMessageLines(Self, DDEClientItem.DdeItem, DDEClientItem.lines);
end;

function TSimpleDDE.OpenItemLink(const aItemName: string): Boolean;
var
  DDEClientItem: TDdeClientItem;
  DDEServerItem: TDdeServerItem;
begin
  CloseItemLink(aItemName);

  try
    Result := false;
    if assigned(FDDEClient) then
    begin
      DDEClientItem := TDdeClientItem.Create(FOwner);
      DDEClientItem.DdeConv := FDDEClient;
      DDEClientItem.DdeItem := aItemName;
      DDEClientItem.OnChange := OnServerItemChanged;

      FDDEClientItems.add(aItemName, DDEClientItem);
      Result := true;
    end;
    if assigned(FDDEServer) then
    begin
      DDEServerItem := TDdeServerItem.Create(FOwner);
      DDEServerItem.Name := aItemName;

      DDEServerItem.ServerConv := FDDEServer;
      DDEServerItem.OnChange := OnClientItemChanged;
      FDDEServerItems.add(aItemName, DDEServerItem);
      Result := true;
    end;
  except
    Result := false;
  end;
end;

function TSimpleDDE.PokeData(const aItem, aMessage: string): Boolean;
var
  ansi: AnsiString;
begin
  if assigned(FDDEClient) then
  begin
    ansi := string(aMessage);
    Result := FDDEClient.PokeData(aItem, PAnsiChar(ansi));
  end
  else
    raise Exception.Create('PokeData is available only for a DDE Client');
end;

procedure TSimpleDDE.SetOnClose(const Value: TNotifyEvent);
begin
  FOnClose := Value;
end;

procedure TSimpleDDE.SetOnExecuteMacro(const Value: TMacroEvent);
begin
  FOnExecuteMacro := Value;
end;

procedure TSimpleDDE.SetOnMessage(const Value: TOnDDEMessage);
begin
  FOnMessage := Value;
end;

procedure TSimpleDDE.SetOnMessageLines(const Value: TOnDDEMessageLines);
begin
  FOnMessageLines := Value;
end;

procedure TSimpleDDE.SetOnOpen(const Value: TNotifyEvent);
begin
  FOnOpen := Value;
end;

procedure TSimpleDDE.SetTimeOut(const Value: integer);
begin
  FTimeOut := Value;
end;

end.
