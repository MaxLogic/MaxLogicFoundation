unit maxSoapHelper;
{ Version: 1.5
  History:
  2025-04-26: support for compressed responses
  2019-04-23: delphi 10.3 compatible
  2017-08-17: added support for custom headers
  2016-10-03: added CloneSoapObject procedure }

{$I JEDI.INC}

interface

uses
  winApi.Windows, System.Classes, System.SysUtils, Soap.InvokeRegistry,
  System.win.ComObj, xml.XMLIntf, xml.XMLDoc, Soap.OPToSOAPDomConv, Soap.SOAPHTTPTrans,
  Soap.OpConvertOptions, Soap.SOAPHTTPClient,
  generics.collections, System.Net.HttpClient, MaxLogic.ComGuard;


type
  TOnSoapMessage = procedure(Sender: TObject; Stream: TStream;
    IsRequest: boolean; const RequestTimeStamp: TDateTime) of object;

  THTTPReqRespWithMonitor = class(THTTPReqResp)
  private
    fOnSoapMessage: TOnSoapMessage;
    procedure SetOnSoapmessage(const Value: TOnSoapMessage);
  public
    constructor Create(Owner: TComponent); override;
    procedure Execute(const Request: TStream; Response: TStream);
      overload; override;

    property OnSoapMessage: TOnSoapMessage read fOnSoapMessage
      write SetOnSoapmessage;
  end;

  THeaderItem = record
    Name: string;
    Value: string;
  end;

  TSoapWithLog = class
  private

  protected
    fRequestCustomHttpHeaders: TList<THeaderItem>;
    // IsRequest = true, else false
    fLastSoapMesage: array[boolean] of String;
    fLastUrl: string;

    procedure AddRequestHttpheaderItem(const Name, Value: string);
    function GetLogFileName: string; virtual; abstract;
    procedure log(const s: string); virtual; abstract;
    function GetHttp(ConnectTimeout: integer = 30000; SendTimeout: integer = 30000; ReceiveTimeout: integer = 30000; const aName: string = ''): THTTPRIO;
    procedure BeforePost(const AHTTPReqResp: THTTPReqResp;
      {$IFDEF DELPHIX_RIO_UP      }
      Client: THTTPClient
      {$ELSE}
      AData: Pointer
      {$ENDIF}); virtual;
    procedure BeforePost2; virtual;
    procedure HttpOnSoapMessage(Sender: TObject; Stream: TStream;
      IsRequest: boolean; const RequestTimeStamp: TDateTime); virtual;
    procedure AfterRequestSaved(const FileName: string); virtual;
    procedure LogBeforeExecute(const MethodName: string; Response: TStream); virtual;
    procedure LogAfterExecute(const MethodName: string; Response: TStream);
  public
    constructor Create;
    destructor Destroy; override;
  end;

function SoapToString(Obj: TRemotable; RootNodeName: string = '';
  nodeName: string = ''; Options: TObjectConvertOptions = [ocoDontPrefixNode,
    ocoDontPutTypeAttr]): string;
function SoapToString2(Obj: TRemotable; RootNodeName: string = '';
  nodeName: string = ''): string;
procedure SaveSoapToFile(Obj: TRemotable; const FileName: string);
procedure CloneSoapObject(Source, Target: TRemotable);

implementation

uses
  System.ioUtils, winApi.Wininet, autoFree;

procedure SaveSoapToFile(Obj: TRemotable; const FileName: string);
var
  s: string;
begin
  s := SoapToString(Obj);
  TFile.WriteAllText(FileName, s, TEncoding.UTF8);
end;

function SoapToString(Obj: TRemotable; RootNodeName: string = '';
  nodeName: string = ''; Options: TObjectConvertOptions = [ocoDontPrefixNode,
    ocoDontPutTypeAttr]): string;
var
  RootNode, ResultNode: iXMLNode;
  // Converter: IObjConverter;
  converter: TSOAPDomConv;
  RefId: string;
  xml: IXMLDocument;
  SoapDomConv: TSOAPDomConv;
  ObjConvOpts: TObjectConvertOptions;
begin
  ObjConvOpts := Options;

  try

    xml := NewXMLDocument;
    if RootNodeName = '' then
      RootNodeName := Obj.classname;

    if nodeName = '' then
      nodeName := Obj.classname;

    RootNode := xml.CreateNode(RootNodeName);
    xml.DocumentElement := RootNode;

    converter := TOPToSoapDomConvert.Create(nil);

    converter.Options := converter.Options +
      [soSendUntyped, soRootRefNodesToBody, soDocument, soLiteralParams];

    converter.Options := converter.Options - [soSendMultiRefObj,
      soSendMultiRefArray];

    ResultNode := Obj.ObjectToSOAP(RootNode, RootNode, converter, nodeName, '',
      '', ObjConvOpts, RefId);

    Result := RootNode.xml;

  finally
    converter := nil;
  end;
end;

function SoapToString2(Obj: TRemotable; RootNodeName: string = '';
  nodeName: string = ''): string;
var
  // Converter: IObjConverter;
  converter: TSOAPDomConv;
  ResultNode: iXMLNode;
  RootNode: iXMLNode;
  xml: IXMLDocument;
  XMLStr: string;
  ObjConvOpts: TObjectConvertOptions;
begin
  ObjConvOpts := [ocoDontPrefixNode, ocoDontPutTypeAttr];

  xml := NewXMLDocument;
  if RootNodeName = '' then
    RootNodeName := Obj.classname;
  if nodeName = '' then
    nodeName := Obj.classname;

  RootNode := xml.CreateNode(RootNodeName);
  xml.DocumentElement := RootNode;

  converter := TSOAPDomConv.Create(nil);

  converter.Options := converter.Options + [soSendUntyped, soRootRefNodesToBody,
    soDocument, soLiteralParams];

  converter.Options := converter.Options - [soSendMultiRefObj];

  ResultNode := Obj.ObjectToSOAP(RootNode, RootNode, converter, nodeName, '',
    '', ObjConvOpts, XMLStr);

  Result := RootNode.xml;
end;

{ THTTPReqRespWithMonitor }

constructor THTTPReqRespWithMonitor.Create;
begin
  inherited;
  TComGuard.Ensure;
end;

procedure THTTPReqRespWithMonitor.Execute(const Request: TStream;
  Response: TStream);
var
  d: TDateTime;
begin
  TComGuard.Ensure;
  d := now;

  if assigned(fOnSoapMessage) then
    fOnSoapMessage(self, Request, True, d);

  inherited;

  if assigned(fOnSoapMessage) then
    fOnSoapMessage(self, Response, False, d);

end;

procedure THTTPReqRespWithMonitor.SetOnSoapmessage(const Value: TOnSoapMessage);
begin
  fOnSoapMessage := Value;
end;

procedure CloneSoapObject(Source, Target: TRemotable);
var
  converter: IObjConverter;
  ResultNode: iXMLNode;
  ParentNode: iXMLNode;
  RootNode: iXMLNode;
  xml: IXMLDocument;
  XMLStr: string;
  ObjConvOpts: TObjectConvertOptions;
begin
  ObjConvOpts := [ocoDontPrefixNode];
  xml := NewXMLDocument;
  RootNode := xml.AddChild('Root');
  ParentNode := RootNode.AddChild('Parent');

  converter := TSOAPDomConv.Create(nil);

  ResultNode := Source.ObjectToSOAP(RootNode, ParentNode, converter,
    'CopyObject', '', '', ObjConvOpts, XMLStr);

  Target.SOAPToObject(RootNode, ResultNode, converter);
end;

{ TSoapWithLog }

procedure TSoapWithLog.AddRequestHttpheaderItem(const Name, Value: string);
var
  h: THeaderItem;
begin
  h.Name := Name;
  h.Value := Value;

  // eliminate duplicates
  for var X := 0 to fRequestCustomHttpHeaders.Count - 1 do
    if Name = fRequestCustomHttpHeaders[X].Name then
    begin
      fRequestCustomHttpHeaders[X] := h;
      exit;
    end;

  // add as new
  fRequestCustomHttpHeaders.add(h);
end;

procedure TSoapWithLog.AfterRequestSaved(const FileName: string);
begin

end;

function TSoapWithLog.GetHttp(ConnectTimeout: integer = 30000; SendTimeout: integer = 30000; ReceiveTimeout: integer = 30000; const aName: string = ''): THTTPRIO;
var
  mr: THTTPReqRespWithMonitor;
begin
  TComGuard.Ensure;
  Result := THTTPRIO.Create(nil);
  Result.OnBeforeExecute := LogBeforeExecute;
  Result.OnAfterExecute := LogAfterExecute;
  mr := THTTPReqRespWithMonitor.Create(Result);
  mr.OnSoapMessage := HttpOnSoapMessage;

  mr.ConnectTimeout := ConnectTimeout;
  {$IFNDEF DELPHIX_RIO_UP      }
  mr.SendTimeout := SendTimeout;
  {$ENDIF}
  mr.ReceiveTimeout := ReceiveTimeout;

  mr.OnBeforePost := BeforePost;
  mr.AutomaticDecompression := [THTTPCompressionMethod.GZip, THTTPCompressionMethod.Deflate];

  Result.HTTPWebNode := mr;

  if aName <> '' then
    Result.HTTPWebNode.Name := aName
  else
    Result.HTTPWebNode.Name := 'HTTPWebNode2'; { do not localize }

  Result.HTTPWebNode.SetSubComponent(True);
end;


procedure TSoapWithLog.HttpOnSoapMessage(Sender: TObject; Stream: TStream;
  IsRequest: boolean; const RequestTimeStamp: TDateTime);
var
  fn, s: string;
  l: TStringList;
  ms: TMemorystream;
  i: integer;
  sTimeStamp : String;
begin
  if Stream.Size = 0 then
  begin
    Log('Stream.Size = 0');
    exit;
  end;

  sTimeStamp := FormatDateTime('yyyy"-"mm"-"dd" "hh";"nn";"ss"."zzz', RequestTimeStamp);

  i := Stream.Position;
  ms := TMemorystream.Create;
  gc(ms);
  Stream.Position := 0;
  ms.CopyFrom(Stream, Stream.Size);
  Stream.Position := i;

  ms.Position := 0;
  l := TStringList.Create;
  gc(l);
  try
    l.LoadFromStream(ms, TEncoding.UTF8);
  except
    ms.Position := 0;
    l.LoadFromStream(ms);
  end;
  ms.Position := 0;

  if IsRequest then
  if IsRequest then
    s := 'Request'
  else
    s := 'Response';

  if l.Text <> fLastSoapMesage[IsRequest] then
  begin
    fLastSoapMesage[IsRequest] := l.Text;
    fn := changeFileExt(GetLogFileName, ' (' + sTimeStamp + ') '
      + s + '.xml');

    // ensure the file does not yet exists
    s := fn;
    i := 1;
    while FileExists(fn) do
    begin
      fn := changeFileExt(s, ' (' + IntToStr(i) + ').xml');
      Inc(i);
    end;

    ms.SaveToFile(fn);
    AfterRequestSaved(fn);
    Log('Saved SOAP ' + s + ' to: "' + fn + '"'); // Use Log, include headers
  end;
end;

procedure TSoapWithLog.LogAfterExecute(const MethodName: string;
  Response: TStream);
begin
  log('rio.AfterExecute method: "' + MethodName + '"');
end;

procedure TSoapWithLog.LogBeforeExecute(const MethodName: string;
  Response: TStream);
begin
  log('rio.beforeExecute method: "' + MethodName + '"');
end;

procedure TSoapWithLog.BeforePost;
var
  {$IFNDEF DELPHIX_RIO_UP      }
  s: string;
  {$ENDIF}
  X: integer;
  n, v: string;
begin
  TComGuard.Ensure;
  fLastUrl := AHTTPReqResp.URL;



  {$IFDEF DELPHIX_RIO_UP}

  {$ENDIF}

  BeforePost2;

  {$IFNDEF DELPHIX_RIO_UP      }
  s := '';
  {$ENDIF}
  for X := 0 to fRequestCustomHttpHeaders.Count - 1 do
  begin
    n := fRequestCustomHttpHeaders[X].Name;
    v := fRequestCustomHttpHeaders[X].Value;

    {$IFDEF DELPHIX_RIO_UP      }
    Client.CustomHeaders[n] := v;
    {$ELSE}
    s := s + n;
    if v <> '' then
      s := s + ': ' + v;
    s := s + CR;
    {$ENDIF}
  end;
  {$IFNDEF DELPHIX_RIO_UP      }
  if s <> '' then
    HttpAddRequestHeaders(AData,
      // A pointer to a string variable containing the headers to append to the request.
      // Each header must be terminated by a CR/LF (carriage return/line feed) pair.
      PChar(s), length(s), HTTP_ADDREQ_FLAG_ADD
      // or HTTP_ADDREQ_FLAG_REPLACE
      );
  {$ENDIF}
end;

procedure TSoapWithLog.BeforePost2;
begin

end;

constructor TSoapWithLog.Create;
begin
  inherited Create;
  fRequestCustomHttpHeaders := TList<THeaderItem>.Create;
end;

destructor TSoapWithLog.Destroy;
begin
  fRequestCustomHttpHeaders.Free;
  inherited;
end;

end.

