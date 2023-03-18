Unit maxInterOpCommunication;
{$WARN UNIT_PLATFORM OFF}

{ Version: 2.3
  History
  2017-11-14: some improvements
  2014-11-02: small bugfixes
  2013-06-03: mesh net implementation. meaning each one can talk to each other

  NOTE about InterOp:
  64-bit versions of Windows use 32-bit handles for interoperability. When sharing a handle between 32-bit and 64-bit applications, only the lower 32 bits are significant,
  so it is safe to truncate the handle (when passing it from 64-bit to 32-bit) or sign-extend the handle (when passing it from 32-bit to 64-bit).
  Handles that can be shared include
  handles to user objects such as windows (HWND), handles to GDI objects such as pens and brushes (HBRUSH and HPEN), and handles to named objects such as mutexes, semaphores, and file handles.
}

Interface

Uses
  windows, classes, sysUtils, forms, maxInMemoryFile, utsWndSubClassing, messages, SyncObjs, generics.collections;

Type
{$IFNDEF UNICODE}
  Unicodestring = WideString;
{$ENDIF}
  TIOString = ShortString;
  TOnRequestEvent = Procedure(SenderHandle: THandle; Const CallCaption: TIOString; RequestParams, Responseparams: TMemoryStream) Of Object;
  TOnMessageEvent = Procedure(SenderHandle: THandle; Const CallCaption: TIOString; MessageParams: TMemoryStream) Of Object;

  TCustomInterOpCom = Class
  Private
    fForm: TForm;
    fSubClassing: TWndSubClass;
    fSec: TCriticalSection;
    FInterOpId: TIOString;
    fRequestIdCounter: dword;
    // when a request / message is send it is added here, so it doesn't go out of scope. The other side processes them and sends a confirmation back, so we can delete the data from here
    fMessagesWaitingForDeletion: TObjectDictionary<String, TObject>;
    FOnRequest: TOnRequestEvent;
    FOnMessage: TOnMessageEvent;
    FDisableSendMessage: Boolean;
    fPartyInfo: TInMemoryFile;

    Procedure FreeResponseData(RequesterHandle: THandle; RequesterRequestId: dword);
    Procedure FreeMessageData(RequesterHandle: THandle; RequesterRequestId: dword);
    Function GetRequestId: dword;

    Procedure Processmessage(Var Msg: TMessage; Var PrevendDefaultHandler: Boolean);
    Procedure SetOnRequest(Const Value: TOnRequestEvent);
    Procedure FreeStoredMemoryFiles(Const Name: String);
    Procedure SetOnMessage(Const Value: TOnMessageEvent);
    Procedure SetDisableSendMessage(Const Value: Boolean);

    Procedure RegisterParty;
    Procedure UnRegisterParty;

  Protected
    Procedure InternProcessmessage(Var Msg: TMessage; Var PrevendDefaultHandler: Boolean); Virtual;
    Function ProcessRequest(RequesterHandle: THandle; RequestId: dword): Boolean; Virtual;
    Procedure CallOnRequestEvent(SenderHandle: THandle; Const CallCaption: TIOString; RequestParams, Responseparams: TMemoryStream);
    Procedure CallOnMessageEvent(SenderHandle: THandle; Const CallCaption: TIOString; MessageParams: TMemoryStream);
    Procedure Receivemessage(SenderHandle: THandle; MessageId: dword);
  Protected

    // this one will call the other party and will wait until the message is processed and we got an response. All the params will be returned in the ResponseParams. one string is one parameter.
    Procedure DoRequestEx(Const CallCaption: TIOString; RequestParams, Responseparams: TMemoryStream; TargetHandle: THandle);

    Class Function InterOpTargetIsValid(TargetHandle: THandle): Boolean;
  Public
    Constructor Create(Const aInterOpId: TIOString);
    Destructor Destroy; Override;

    Class Function FilenameToId(Const Filename: String): String;
    Class Function GetInterOpIdFromHandle(Handle: THandle; Out InterOpId: TIOString): Boolean;

    // this one will Post the data, but will not wait until it is processed
    Procedure PostMessage(Const CallCaption: TIOString; MessageParams: TMemoryStream; TargetHandle: THandle); Overload;
    Procedure PostMessage(Const CallCaption: TIOString; Const MessageParams: String; TargetHandle: THandle); Overload;

    Property InterOpId: TIOString Read FInterOpId;

    // those 2 events are called when a request or message arrives
    // requests must be answered, messages are just a one way sending
    Property OnRequest: TOnRequestEvent Read FOnRequest Write SetOnRequest;
    Property OnMessage: TOnMessageEvent Read FOnMessage Write SetOnMessage;

    // sometimes we can not use window.sendmessage, as the window is already blocked or do process a message. Then we need to relly on windows.postmessage instead. This is a bit less sucure, as we do not know if our request was received, and we need to wait for the response by checking its state.
    Property DisableSendMessage: Boolean Read FDisableSendMessage Write SetDisableSendMessage;
  End;

  TOnConnectedWithNode = Procedure(Sender: TObject; Const NodeInterOpId: TIOString) Of Object;

  TInterOpNode = Class(TCustomInterOpCom)
  Private
    fCancel: Boolean;
    fServerheader: TInMemoryFile;
    fConnectedNodeHandles: TStringList;
    FOnConnectedWithNode: TOnConnectedWithNode;
    FOnDisconnectedWithNode: TOnConnectedWithNode;

    // creates a global entry so the server can be found and connected to
    Procedure RegisterServer;
    Procedure UnRegisterServer;

    // a foreign client has connected to us, add it to the list
    Procedure AfterClientConnected(ClientHandle: THandle);
    Procedure AfterClientDisconnected(ClientHandle: wParam);

    Procedure SetOnConnectedWithNode(Const Value: TOnConnectedWithNode);
    Procedure SetOnDisconnectedWithNode(Const Value: TOnConnectedWithNode);
    Class Constructor CreateClass;
  Protected
    Procedure InternProcessmessage(Var Msg: TMessage; Var PrevendDefaultHandler: Boolean); Override;
  Public
    Constructor Create(Const InterOpId: TIOString);
    Destructor Destroy; Override;

    // connect to an other server. if an ExeFilename is provided, and the foreign Node is not yet running, this function will try to start it
    Function ConnectTo(Const TargetInterOpId: TIOString; TimeOut: dword = 5000; Const ExeFileName: String = ''; Const ExeParams: String = ''): Boolean;
    Procedure DisconnectFrom(Const TargetInterOpId: TIOString);
    Procedure DisconnectFromAll;
    Function NodeIsOnline(Const TargetInterOpId: TIOString): Boolean;
    Class Function GetHandleFromInterOpId(Const aInterOpId: TIOString; Out Handle: THandle): Boolean;
    Class Function isOnline(Const aInterOpId: TIOString): Boolean;

    Function DoRequest(Const TargetInterOpId, CallCaption: TIOString; RequestParams, Responseparams: TMemoryStream): Boolean; Overload;
    Function DoRequest(Const TargetInterOpId, CallCaption: TIOString; Var RequestParams, Responseparams: String): Boolean; Overload;
    // you cann call the methods connectTo with a 0 timeout, in this case you need to cancel manually by using this function
    Procedure cancel;

    Property OnConnectedWithNode: TOnConnectedWithNode Read FOnConnectedWithNode Write SetOnConnectedWithNode;
    Property OnDisconnectedWithNode: TOnConnectedWithNode Read FOnDisconnectedWithNode Write SetOnDisconnectedWithNode;
  End;

  // some helper functions
Procedure WriteStringToMemoryStream(ms: TMemoryStream; Const str: Unicodestring);
Function ReadStringFromMemoryStream(ms: TMemoryStream): Unicodestring;
Procedure Exec(Const Filename: String; Const Parameter: String = '';
StartDir: String = '');

Implementation

uses
  shellAPI;

Const
  cRequestPrefix = 'maxInterOp-Request-';
  cResponsePrefix = 'maxInterOp-Response-';
  cMessagePrefix = 'maxInterOp-Message-';
  cInterOpPartyPrefix = 'maxInterOpParty-';

  cHeaderSufix = '-Header';
  cDataSufix = '-Data';

Var
  maxInterOp_Magic: TGUID;

Const
  // messages
  WM_REGISTERCLIENT = WM_USER + 1;
  // w = client handle
  WM_UNREGISTERCLIENT = WM_USER + 2;
  // w = client handle
  WM_REQUEST = WM_USER + 3;
  // w = sender handle, l = mmf id
  WM_FREE_RESPONSEDATA = WM_USER + 4;
  // w = requester handle , l = requester request id
  WM_POSTMESSAGE = WM_USER + 5;
  WM_FREE_MessageDATA = WM_USER + 6;

Type
  pInterOpPartyInfo = ^TInterOpPartyInfo;

  TInterOpPartyInfo = Packed Record
    magic: TGUID;
    Handle: uInt64;
    InterOpId: TIOString;
    ProcessId: uInt64;
    IsRunning: Boolean;
  End;

  pServerHeader = ^TServerheader;

  TServerheader = Packed Record
    magic: TGUID;
    ServerHandle: uInt64;
    ServerIsRunning: Boolean;
  End;

  pRequestHeader = ^TRequestHeader;

  TRequestHeader = Packed Record
    magic: TGUID;
    SenderHandle: uInt64;
    MMFIdOnSender: dword;
    CallCaption: TIOString;
    DataSize: dword;
  End;

  pResponseHeader = ^TResponseHeader;

  TResponseHeader = Packed Record
    magic: TGUID;
    SenderHandle: uInt64;
    ResponseToRequestId: dword;
    RequesterHandle: uInt64;
    CallCaption: TIOString;
    ResponseDataSize: dword;
    IsReady: Boolean;
  End;

  pMessageHeader = ^TMessageHeader;

  TMessageHeader = Packed Record
    magic: TGUID;
    SenderHandle: uInt64;
    SenderMessageId: dword;
    CallCaption: TIOString;
    DataSize: dword;
  End;



Procedure Exec( Const Filename: String; Const Parameter: String = ''; StartDir: String = '');
Begin
  If StartDir = '' Then
    StartDir := ExtractFilePath(Filename);
  ShellExecute(0, Nil, PChar(Filename), PChar(Parameter), PChar(StartDir),
    SW_NORMAL);
End;

  { TInterOpNode }
Class Constructor TInterOpNode.CreateClass;
Begin
  maxInterOp_Magic := StringToGuid('{151B9872-12EB-4A19-8C26-1C753FA8BC48}');
End;

Function TInterOpNode.DoRequest(Const TargetInterOpId, CallCaption: TIOString; RequestParams, Responseparams: TMemoryStream): Boolean;
Var
  Handle: THandle;
Begin
  result := false;
  If GetHandleFromInterOpId(TargetInterOpId, Handle) Then
    If self.InterOpTargetIsValid(Handle) Then
    Begin
        DoRequestEx(CallCaption, RequestParams, Responseparams, Handle);
      result := true;
    End;
End;

Function TInterOpNode.NodeIsOnline(Const TargetInterOpId: TIOString): Boolean;
Var
  Handle: THandle;
Begin
  result := false;
  If self.GetHandleFromInterOpId(TargetInterOpId, Handle) Then
    If self.InterOpTargetIsValid(Handle) Then
      result := true;
End;

Function TInterOpNode.ConnectTo(Const TargetInterOpId: TIOString; TimeOut: dword = 5000; Const ExeFileName: String = ''; Const ExeParams: String = ''): Boolean;
Var
  Handle: THandle;
  d: dword;
Begin
  fCancel := false;
  result := false;

  If Not NodeIsOnline(TargetInterOpId) Then
  Begin
    If (ExeFileName <> '') And FileExists(ExeFileName) Then
      exec(ExeFileName, ExeParams);
  End;

  d := GetTickCount;
  While (Not NodeIsOnline(TargetInterOpId)) And (Not fCancel) Do
  Begin
      sleep(50);
    If TimeOut <> 0 Then
      If GetTickCount - d > TimeOut Then
        break;
  End;

  If self.GetHandleFromInterOpId(TargetInterOpId, Handle) Then
  Begin
      windows.PostMessage(Handle, WM_REGISTERCLIENT, self.fForm.Handle, 0);
    result := true;
  End;
End;

Procedure TInterOpNode.SetOnConnectedWithNode(Const Value: TOnConnectedWithNode);
Begin
  FOnConnectedWithNode := Value;
End;

Procedure TInterOpNode.SetOnDisconnectedWithNode(Const Value: TOnConnectedWithNode);
Begin
  FOnDisconnectedWithNode := Value;
End;

Procedure TInterOpNode.DisconnectFrom;
Var
  Handle: THandle;
  i: integer;
Begin
  If self.GetHandleFromInterOpId(TargetInterOpId, Handle) Then
    windows.PostMessage(Handle, WM_UNREGISTERCLIENT, self.fForm.Handle, 0);

  If fConnectedNodeHandles.Find(TargetInterOpId, i) Then
    fConnectedNodeHandles.Delete(i);
End;

{ TCustomInterOpCom }

Procedure TCustomInterOpCom.DoRequestEx;
Var
  RequestHeader, RequestData: TInMemoryFile;
  Req: pRequestHeader;
  RequestName: String;
  RequestId: dword;
  Responsename: String;
  ResponseHeader, ResponseData: TInMemoryFile;
  Resp: pResponseHeader;
  iRes: integer;
Begin
  ResponseHeader := NIL;
  RequestId := self.GetRequestId;
  RequestName := cRequestPrefix + IntToStr(fForm.Handle) + '-' + IntToStr(RequestId);
  Responsename := cResponsePrefix + IntToStr(fForm.Handle) + '-' + IntToStr(RequestId);
  RequestHeader := TInMemoryFile.Create(RequestName + cHeaderSufix, Sizeof(TRequestHeader));
  Req := RequestHeader.Memory;

  Req.magic := maxInterOp_Magic;
  Req.SenderHandle := fForm.Handle;
  Req.MMFIdOnSender := RequestId;
  Req.CallCaption := CallCaption;
  Req.DataSize := RequestParams.Size;

  // build request data and copy memory
  RequestData := Nil;
  If RequestParams.Size > 0 Then
  Begin
      RequestData := TInMemoryFile.Create(RequestName + cDataSufix, RequestParams.Size);
    RequestParams.Position := 0;
    RequestParams.ReadBuffer(RequestData.Memory^, RequestParams.Size);
  End;

  Responseparams.Size := 0;
  If Not FDisableSendMessage Then
    iRes := windows.SendMessage(TargetHandle, WM_REQUEST, fForm.Handle, RequestId)
  Else
  Begin
      iRes := 0;
    ResponseHeader := TInMemoryFile.Create(Responsename + cHeaderSufix, Sizeof(TResponseHeader));
    Resp := ResponseHeader.Memory;
    windows.PostMessage(TargetHandle, WM_REQUEST, fForm.Handle, RequestId);

    Repeat
      sleep(5);
      If IsEqualGUID(Resp.magic, maxInterOp_Magic) Then
        If Resp.IsReady Then
          iRes := 1;
    Until (iRes = 1) Or (Not InterOpTargetIsValid(TargetHandle));
  End;

  RequestHeader.Free;
  If Assigned(RequestData) Then
    RequestData.Free;

  If iRes = 1 Then
  Begin
    If Not Assigned(ResponseHeader) Then
      ResponseHeader := TInMemoryFile.Create(Responsename + cHeaderSufix, Sizeof(TResponseHeader));
    Resp := ResponseHeader.Memory;
    If IsEqualGUID(Resp.magic, maxInterOp_Magic) Then
      If Resp.ResponseDataSize > 0 Then
      Begin
          ResponseData := TInMemoryFile.Create(Responsename + cDataSufix, Resp.ResponseDataSize);
        Responseparams.Position := 0;
        Responseparams.WriteBuffer(ResponseData.Memory^, Resp.ResponseDataSize);
        Responseparams.Position := 0;
        ResponseData.Free;
      End;
  End;

  If Assigned(ResponseHeader) Then
    ResponseHeader.Free;

  // we can use postMessage, as we do not need to wait until the command finishes
  windows.PostMessage(TargetHandle, WM_FREE_RESPONSEDATA, fForm.Handle, RequestId)
End;

Constructor TCustomInterOpCom.Create;
Begin
  Inherited Create;
  FInterOpId := aInterOpId;

  FDisableSendMessage := false;
  fMessagesWaitingForDeletion := TObjectDictionary<String, TObject>.Create;

  fSec := TCriticalSection.Create;
  fForm := TForm.Create(Nil);
  self.fSubClassing := TWndSubClass.Create(Nil);
  fSubClassing.OnMessage := Processmessage;

  fSubClassing.HookedWnd := fForm.Handle;
  RegisterParty;
End;

Destructor TCustomInterOpCom.Destroy;
Var
  x: integer;
Begin
  UnRegisterParty;
  If Assigned(fSubClassing) Then
    FreeAndNIL(self.fSubClassing);
  If Assigned(fForm) Then
    FreeAndNIL(fForm);

  fSec.Free;

  fMessagesWaitingForDeletion.clear;
  fMessagesWaitingForDeletion.Free;

  Inherited;
End;

Procedure TCustomInterOpCom.CallOnMessageEvent(SenderHandle: THandle; Const CallCaption: TIOString; MessageParams: TMemoryStream);
Begin
  If Assigned(FOnMessage) Then
    FOnMessage(SenderHandle, CallCaption, MessageParams);
End;

Procedure TCustomInterOpCom.CallOnRequestEvent(SenderHandle: THandle; Const CallCaption: TIOString; RequestParams, Responseparams: TMemoryStream);
Begin
  If Assigned(FOnRequest) Then
    FOnRequest(SenderHandle, CallCaption, RequestParams, Responseparams);
End;

Class Function TCustomInterOpCom.GetInterOpIdFromHandle(Handle: THandle; Out InterOpId: TIOString): Boolean;
Var
  party: pInterOpPartyInfo;
  mmf: TInMemoryFile;
Begin
  result := false;

  mmf := TInMemoryFile.Create(cInterOpPartyPrefix + IntToStr(Handle), Sizeof(TInterOpPartyInfo));
  party := mmf.Memory;
  If Not mmf.isnew Then
    If party.Handle = Handle Then
      If party.magic = maxInterOp_Magic Then
      Begin
          InterOpId := party.InterOpId;
        result := true;
      End;
End;

Class Function TCustomInterOpCom.FilenameToId(Const Filename: String): String;
Begin
  result := StringReplace(Filename, '\', '_-_', [rfReplaceAll]);
  result := StringReplace(result, ':', '_-_', [rfReplaceAll]);
  result := lowercase(result);
End;

Procedure TCustomInterOpCom.FreeMessageData(RequesterHandle: THandle; RequesterRequestId: dword);
Var
  s: String;
Begin
  s := cMessagePrefix + IntToStr(RequesterHandle) + '-' + IntToStr(RequesterRequestId);
  FreeStoredMemoryFiles(s);
End;

Procedure TCustomInterOpCom.FreeResponseData(RequesterHandle: THandle; RequesterRequestId: dword);
Var
  s: String;
Begin
  s := cResponsePrefix + IntToStr(RequesterHandle) + '-' + IntToStr(RequesterRequestId);
  FreeStoredMemoryFiles(s);
End;

Procedure TCustomInterOpCom.FreeStoredMemoryFiles(Const Name: String);
Begin
  fSec.enter;
  Try
    fMessagesWaitingForDeletion.remove(Name + cHeaderSufix);
    fMessagesWaitingForDeletion.remove(Name + cDataSufix);
  Finally
    fSec.leave;
  End;
End;

Function TCustomInterOpCom.GetRequestId: dword;
Begin
  fSec.enter;
{$Q-}
  inc(fRequestIdCounter);
  result := fRequestIdCounter;
  fSec.leave;
End;

Procedure TCustomInterOpCom.Receivemessage(SenderHandle: THandle; MessageId: dword);
Var
  MessageHeader, MessageData: TInMemoryFile;
  MessageParams: TMemoryStream;
  Msg: pMessageHeader;
  MessageName: String;
  CallCaption: TIOString;
Begin
  MessageName := cMessagePrefix + IntToStr(SenderHandle) + '-' + IntToStr(MessageId);
  MessageHeader := TInMemoryFile.Create(MessageName + cHeaderSufix, Sizeof(TMessageHeader));
  Msg := MessageHeader.Memory;
  If Not IsEqualGUID(Msg.magic, maxInterOp_Magic) Then
  Begin
      MessageHeader.Free;
    Exit;
  End;
  MessageParams := TMemoryStream.Create;

  CallCaption := Msg.CallCaption;
  If Msg.DataSize > 0 Then
  Begin
      MessageData := TInMemoryFile.Create(MessageName + cDataSufix, Msg.DataSize);
    MessageParams.WriteBuffer(MessageData.Memory^, Msg.DataSize);
    MessageParams.Position := 0;
    MessageData.Free;
  End;
  MessageHeader.Free;

  CallOnMessageEvent(SenderHandle, CallCaption, MessageParams);
  MessageParams.Free;

  windows.PostMessage(SenderHandle, WM_FREE_MessageDATA, SenderHandle, MessageId)

End;

Procedure TCustomInterOpCom.RegisterParty;
Var
  party: pInterOpPartyInfo;
  ProcessId: dword;
Begin
  If Assigned(fPartyInfo) Then
    Exit;
  fSec.enter;
  Try
    fPartyInfo := TInMemoryFile.Create(cInterOpPartyPrefix + IntToStr(fForm.Handle), Sizeof(TInterOpPartyInfo));
    party := fPartyInfo.Memory;
    party.Handle := fForm.Handle;
    party.InterOpId := FInterOpId;
    party.magic := maxInterOp_Magic;
    GetWindowThreadProcessId(fForm.Handle, @ProcessId);
    party.ProcessId := ProcessId;
    party.IsRunning := true;

  Finally
    fSec.leave;
  End;
End;

Function TCustomInterOpCom.ProcessRequest(RequesterHandle: THandle; RequestId: dword): Boolean;
Var
  RequestHeader, RequestData: TInMemoryFile;
  Req: pRequestHeader;
  RequestName: String;
  Responsename: String;
  ResponseHeader, ResponseData: TInMemoryFile;
  Responseparams, RequestParams: TMemoryStream;
  CallCaption: TIOString;
  Resp: pResponseHeader;
Begin
  result := false;

  RequestName := cRequestPrefix + IntToStr(RequesterHandle) + '-' + IntToStr(RequestId);
  Responsename := cResponsePrefix + IntToStr(RequesterHandle) + '-' + IntToStr(RequestId);
  RequestHeader := TInMemoryFile.Create(RequestName + cHeaderSufix, Sizeof(TRequestHeader));
  Req := RequestHeader.Memory;
  If Not IsEqualGUID(Req.magic, maxInterOp_Magic) Then
  Begin
      RequestHeader.Free;
    Exit;
  End;
  RequestParams := TMemoryStream.Create;

  CallCaption := Req.CallCaption;
  If Req.DataSize > 0 Then
  Begin
      RequestData := TInMemoryFile.Create(RequestName + cDataSufix, Req.DataSize);
    RequestParams.WriteBuffer(RequestData.Memory^, Req.DataSize);
    RequestParams.Position := 0;
    RequestData.Free;
  End;
  RequestHeader.Free;

  Responseparams := TMemoryStream.Create;
  CallOnRequestEvent(RequesterHandle, CallCaption, RequestParams, Responseparams);
  RequestParams.Free;
  ResponseHeader := TInMemoryFile.Create(Responsename + cHeaderSufix, Sizeof(TResponseHeader));
  Resp := ResponseHeader.Memory;
  fSec.enter;
  fMessagesWaitingForDeletion.add(ResponseHeader.Name, ResponseHeader);
  fSec.leave;

  Resp.IsReady := false;
  Resp.magic := maxInterOp_Magic;
  Resp.SenderHandle := fForm.Handle;
  Resp.ResponseToRequestId := RequestId;
  Resp.RequesterHandle := RequesterHandle;
  Resp.CallCaption := CallCaption;
  Resp.ResponseDataSize := Responseparams.Size;

  If Responseparams.Size > 0 Then
  Begin
      ResponseData := TInMemoryFile.Create(Responsename + cDataSufix, Responseparams.Size);
    Responseparams.Position := 0;
    Responseparams.ReadBuffer(ResponseData.Memory^, Responseparams.Size);
    fSec.enter;
    fMessagesWaitingForDeletion.add(ResponseData.Name, ResponseData);
    fSec.leave;
  End;
  Responseparams.Free;

  Resp.IsReady := true;
  result := true;
End;

Procedure TCustomInterOpCom.InternProcessmessage(Var Msg: TMessage; Var PrevendDefaultHandler: Boolean);
Begin
  Case Msg.Msg Of
    WM_POSTMESSAGE:
      Begin
          PrevendDefaultHandler := true;
        Receivemessage(Msg.wParam, Msg.LParam)
      End;
    WM_REQUEST:
      Begin
          PrevendDefaultHandler := true;
        Msg.result := 0;
        If ProcessRequest(Msg.wParam, Msg.LParam) Then
          Msg.result := 1;
      End;

    WM_FREE_RESPONSEDATA:
      Begin
          PrevendDefaultHandler := true;
        FreeResponseData(Msg.wParam, Msg.LParam);
      End;

    WM_FREE_MessageDATA:
      Begin
          PrevendDefaultHandler := true;
        FreeMessageData(Msg.wParam, Msg.LParam);
      End;
  End;
End;

Class Function TCustomInterOpCom.InterOpTargetIsValid(TargetHandle: THandle): Boolean;
Var
  mmf: TInMemoryFile;
  party: pInterOpPartyInfo;
  ProcessId: dword;
Begin
  result := false;
  mmf := TInMemoryFile.Create(cInterOpPartyPrefix + IntToStr(TargetHandle), Sizeof(TInterOpPartyInfo));
  If Not mmf.isnew Then
  Begin
      party := mmf.Memory;
    If IsEqualGUID(party.magic, maxInterOp_Magic) Then
      If party.Handle <> 0 Then
        If party.IsRunning Then
          If windows.IsWindow(party.Handle) Then
          Begin
              GetWindowThreadProcessId(party.Handle, ProcessId);
            result := party.ProcessId = ProcessId;
          End;
  End;

  mmf.Free;

End;

Procedure TCustomInterOpCom.PostMessage(Const CallCaption: TIOString; MessageParams: TMemoryStream; TargetHandle: THandle);
Var
  MessageHeader, MessageData: TInMemoryFile;
  Msg: pMessageHeader;
  MessageName: String;
  MessageId: dword;
Begin
  MessageId := self.GetRequestId;
  MessageName := cMessagePrefix + IntToStr(fForm.Handle) + '-' + IntToStr(MessageId);
  MessageHeader := TInMemoryFile.Create(MessageName + cHeaderSufix, Sizeof(TMessageHeader));
  fSec.enter;
  fMessagesWaitingForDeletion.add(MessageHeader.Name, MessageHeader);
  fSec.leave;
  Msg := MessageHeader.Memory;

  Msg.magic := maxInterOp_Magic;
  Msg.SenderHandle := fForm.Handle;
  Msg.SenderMessageId := MessageId;
  Msg.CallCaption := CallCaption;
  Msg.DataSize := MessageParams.Size;

  // build Message data and copy memory
  MessageData := Nil;
  If MessageParams.Size > 0 Then
  Begin
      MessageData := TInMemoryFile.Create(MessageName + cDataSufix, MessageParams.Size);
    MessageParams.Position := 0;
    MessageParams.ReadBuffer(MessageData.Memory^, MessageParams.Size);
    fSec.enter;
    fMessagesWaitingForDeletion.add(MessageData.Name, MessageData);
    fSec.leave;
  End;

  windows.PostMessage(TargetHandle, WM_POSTMESSAGE, fForm.Handle, MessageId);
End;

Procedure TCustomInterOpCom.PostMessage(Const CallCaption: TIOString; Const MessageParams: String; TargetHandle: THandle);
Var
  ms: TMemoryStream;
Begin
  ms := TMemoryStream.Create;
  Try
    WriteStringToMemoryStream(ms, MessageParams);
    ms.Position := 0;

    PostMessage(CallCaption, ms, TargetHandle);
  Finally
    ms.Free;
  End;
End;

Procedure TCustomInterOpCom.Processmessage(Var Msg: TMessage; Var PrevendDefaultHandler: Boolean);
Begin
  // this way we will filter all the "normal" messages, and can set our breakpoint at the follow up method
  If Msg.Msg >= WM_USER + 1 Then
    InternProcessmessage(Msg, PrevendDefaultHandler);

End;

Procedure TCustomInterOpCom.SetDisableSendMessage(Const Value: Boolean);
Begin
  FDisableSendMessage := Value;
End;

Procedure TCustomInterOpCom.SetOnMessage(Const Value: TOnMessageEvent);
Begin
  FOnMessage := Value;
End;

Procedure TCustomInterOpCom.SetOnRequest(Const Value: TOnRequestEvent);
Begin
  FOnRequest := Value;
End;

Procedure TCustomInterOpCom.UnRegisterParty;
Var
  party: pInterOpPartyInfo;
Begin
  fSec.enter;
  Try
    If Assigned(fPartyInfo) Then
    Begin
        party := fPartyInfo.Memory;
      Zeromemory(party, Sizeof(TInterOpPartyInfo));
      FreeAndNIL(fPartyInfo);
    End;
  Finally
    fSec.leave;
  End;
End;

{ TInterOpNode }

Constructor TInterOpNode.Create(Const InterOpId: TIOString);
Begin
  Inherited Create(InterOpId);
  fConnectedNodeHandles := TStringList.Create;
  fConnectedNodeHandles.Sorted := true;

  RegisterServer;
End;

Destructor TInterOpNode.Destroy;
Begin
  UnRegisterServer;
  fConnectedNodeHandles.Free;
  Inherited;
End;

Procedure TInterOpNode.DisconnectFromAll;
Var
  x: integer;
Begin
  For x := fConnectedNodeHandles.Count - 1 Downto 0 Do
    DisconnectFrom(fConnectedNodeHandles[x]);
End;

Function TInterOpNode.DoRequest(Const TargetInterOpId, CallCaption: TIOString; Var RequestParams, Responseparams: String): Boolean;
Var
  request, response: TMemoryStream;
Begin
  request := TMemoryStream.Create;
  response := TMemoryStream.Create;

  Try
    WriteStringToMemoryStream(request, RequestParams);
    request.Position := 0;

    result := DoRequest(TargetInterOpId, CallCaption, request, response);

    response.Position := 0;
    Responseparams := ReadStringFromMemoryStream(response);

  Finally
    request.Free;
    response.Free;
  End;
End;

Class Function TInterOpNode.GetHandleFromInterOpId(Const aInterOpId: TIOString; Out Handle: THandle): Boolean;
Var
  ServerHeader: pServerHeader;
  mmf: TInMemoryFile;
Begin
  result := false;
  mmf := TInMemoryFile.Create(aInterOpId, Sizeof(TServerheader));
  ServerHeader := mmf.Memory;

  If Not mmf.isnew Then
    If ServerHeader.magic = maxInterOp_Magic Then
      If ServerHeader.ServerIsRunning Then
      Begin
          Handle := ServerHeader.ServerHandle;
        result := InterOpTargetIsValid(Handle);
      End;
  mmf.Free;
End;

Procedure TInterOpNode.InternProcessmessage(Var Msg: TMessage; Var PrevendDefaultHandler: Boolean);
Begin
  Inherited;
  If Not PrevendDefaultHandler Then
    Case Msg.Msg Of
      WM_REGISTERCLIENT:
        AfterClientConnected(Msg.wParam);
      WM_UNREGISTERCLIENT:
        AfterClientDisconnected(Msg.wParam);
    End;

End;

Class Function TInterOpNode.isOnline(Const aInterOpId: TIOString): Boolean;
Var
  h: THandle;
Begin
  result := GetHandleFromInterOpId(aInterOpId, h)

End;

Procedure TInterOpNode.AfterClientConnected(ClientHandle: THandle);
Var
  i: integer;
  s: String;
  InterOpId: TIOString;
Begin
  fSec.enter;
  Try
    If self.GetInterOpIdFromHandle(ClientHandle, InterOpId) Then
    Begin
      If Not fConnectedNodeHandles.Find(InterOpId, i) Then
      Begin
          fConnectedNodeHandles.AddObject(InterOpId, Pointer(ClientHandle));

        ConnectTo(InterOpId, 1);

        If Assigned(FOnConnectedWithNode) Then
          FOnConnectedWithNode(self, InterOpId);
      End;
    End;
  Finally
    fSec.leave;
  End;
End;

Procedure TInterOpNode.RegisterServer;
Var
  ServerHeader: pServerHeader;
Begin
  fSec.enter;
  Try
    If Not Assigned(fServerheader) Then
      fServerheader := TInMemoryFile.Create(self.FInterOpId, Sizeof(TServerheader));

    ServerHeader := fServerheader.Memory;
    ServerHeader.magic := maxInterOp_Magic;
    ServerHeader.ServerHandle := fForm.Handle;
    ServerHeader.ServerIsRunning := true;
  Finally
    fSec.leave;
  End;
End;

Procedure TInterOpNode.AfterClientDisconnected(ClientHandle: wParam);
Var
  s: String;
  i: integer;
  x: integer;
Begin
  fSec.enter;
  Try
    For x := fConnectedNodeHandles.Count - 1 Downto 0 Do
      If THandle(fConnectedNodeHandles.Objects[x]) = ClientHandle Then
      Begin
        If Assigned(FOnDisconnectedWithNode) Then
          FOnDisconnectedWithNode(self, fConnectedNodeHandles[x]);

        fConnectedNodeHandles.Delete(x);
      End;
  Finally
    fSec.leave;
  End;
End;

Procedure TInterOpNode.cancel;
Begin
  fCancel := true;
End;

Procedure TInterOpNode.UnRegisterServer;
Var
  ServerHeader: pServerHeader;
Begin
  DisconnectFromAll;
  fSec.enter;
  Try
    If Not Assigned(fServerheader) Then
      fServerheader := TInMemoryFile.Create(self.FInterOpId, Sizeof(TServerheader));

    ServerHeader := fServerheader.Memory;
    ServerHeader.ServerIsRunning := false;
    ServerHeader.ServerHandle := 0;

    FreeAndNIL(fServerheader);
  Finally
    fSec.leave;
  End;
End;

Procedure WriteStringToMemoryStream(ms: TMemoryStream; Const str: Unicodestring);
Var
  len: dword;
Begin
  len := length(str);
  ms.WriteBuffer(len, 4);
  If len > 0 Then
    ms.WriteBuffer(str[1], len * Sizeof(str[1]));
End;

Function ReadStringFromMemoryStream(ms: TMemoryStream): Unicodestring;
Var
  s: Unicodestring;
  len: dword;
Begin
  result := '';
  If ms.Size - ms.Position > 4 Then
  Begin
      ms.ReadBuffer(len, 4);
    If len > 0 Then
    Begin
        SetLength(s, len);
      ms.ReadBuffer(s[1], len * Sizeof(s[1]));
      result := s;
    End;
  End;
End;

End.
