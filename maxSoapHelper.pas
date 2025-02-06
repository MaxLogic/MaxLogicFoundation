unit maxSoapHelper;
{ Version: 1.5
  History:
  2019-04-23: delphi 10.3 compatible
  2017-08-17: added support for custom headers
  2016-10-03: added CloneSoapObject procedure }

{$I JEDI.INC}

interface

uses
  windows, classes, SysUtils, Soap.InvokeRegistry,
  ComObj, XMLIntf, XMLDoc, Soap.OPToSOAPDomConv, Soap.SOAPHTTPTrans,
  Soap.OpConvertOptions, Soap.SOAPHTTPClient,
  generics.collections, System.Net.HttpClient;

type
  TOnSoapMessage = procedure(sender: tobject; Stream: TStream;
    IsRequest: Boolean; const RequestTimeStamp: TDateTime) of object;

  THTTPReqRespWithMonitor = class(THTTPReqResp)
  private
    fOnSoapMessage: TOnSoapMessage;
    procedure SetOnSoapmessage(const Value: TOnSoapMessage);
  public
    procedure Execute(const Request: TStream; Response: TStream);
      overload; override;

    property OnSoapMessage: TOnSoapMessage read fOnSoapMessage
      write SetOnSoapmessage;
  end;

  THeaderItem = record
    name: string;
    Value: string;
  end;

  TSoapWithLog = class
  private

  protected
    fRequestCustomHttpHeaders: TList<THeaderItem>;
    // IsRequest = true, else false
    fLastSoapMesage: array [Boolean] of string;
    fLastUrl: String;

    procedure AddRequestHttpheaderItem(const name, Value: string);
    Function GetLogFileName: string; virtual; abstract;
    procedure Log(const s: string); virtual; abstract;
    function GetHttp(ConnectTimeout :Integer=30000;  SendTimeout :Integer=30000;  ReceiveTimeout : integer =30000; const aName:String=''): THTTPRIO;
    procedure BeforePost(const AHTTPReqResp: THTTPReqResp;
{$IFDEF DELPHIX_RIO_UP      }
      Client: THTTPClient
{$ELSE}
      AData: Pointer
{$ENDIF}); virtual;
    procedure BeforePost2; virtual;
    procedure HttpOnSoapMessage(sender: tobject; Stream: TStream;
      IsRequest: Boolean; const RequestTimeStamp: TDateTime); virtual;
    procedure AfterRequestSaved(const FileName: string); virtual;
    procedure LogBeforeExecute(const MethodName: string; Response: TStream); virtual;
    procedure LogAfterExecute(const MethodName: string; Response: TStream);
  public
    constructor Create;
    destructor Destroy; override;
  end;

Function SoapToString(obj: TRemotable; RootNodeName: string = '';
  nodeName: string = ''; Options: TObjectConvertOptions = [ocoDontPrefixNode,
  ocoDontPutTypeAttr]): string;
function SoapToString2(obj: TRemotable; RootNodeName: string = '';
  nodeName: string = ''): string;
Procedure SaveSoapToFile(obj: TRemotable; const FileName: string);
procedure CloneSoapObject(Source, Target: TRemotable);

implementation

uses
  {pawel1, }ioUtils, Wininet;

Procedure SaveSoapToFile(obj: TRemotable; const FileName: string);
var
  s: string;
begin
  s := SoapToString(obj);
  TFile.WriteAllText(FileName, s, TEncoding.UTF8);
end;

Function SoapToString(obj: TRemotable; RootNodeName: string = '';
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
      RootNodeName := obj.classname;

    if nodeName = '' then
      nodeName := obj.classname;

    RootNode := xml.CreateNode(RootNodeName);
    xml.DocumentElement := RootNode;

    converter := TOPToSoapDomConvert.Create(nil);

    converter.Options := converter.Options +
      [soSendUntyped, soRootRefNodesToBody, soDocument, soLiteralParams];

    converter.Options := converter.Options - [soSendMultiRefObj,
      soSendMultiRefArray];

    ResultNode := obj.ObjectToSOAP(RootNode, RootNode, converter, nodeName, '',
      '', ObjConvOpts, RefId);

    result := RootNode.xml;

  finally
    converter := nil;
  end;
end;

function SoapToString2(obj: TRemotable; RootNodeName: string = '';
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
    RootNodeName := obj.classname;
  if nodeName = '' then
    nodeName := obj.classname;

  RootNode := xml.CreateNode(RootNodeName);
  xml.DocumentElement := RootNode;

  converter := TSOAPDomConv.Create(NIL);

  converter.Options := converter.Options + [soSendUntyped, soRootRefNodesToBody,
    soDocument, soLiteralParams];

  converter.Options := converter.Options - [soSendMultiRefObj];

  ResultNode := obj.ObjectToSOAP(RootNode, RootNode, converter, nodeName, '',
    '', ObjConvOpts, XMLStr);

  result := RootNode.xml;
end;

{ THTTPReqRespWithMonitor }

procedure THTTPReqRespWithMonitor.Execute(const Request: TStream;
  Response: TStream);
  var
  d:TDateTime;
begin
  d := now;

  if assigned(fOnSoapMessage) then
    fOnSoapMessage(self, Request, true, d);

  inherited;

  if assigned(fOnSoapMessage) then
    fOnSoapMessage(self, Response, false, d);

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

  converter := TSOAPDomConv.Create(NIL);

  ResultNode := Source.ObjectToSOAP(RootNode, ParentNode, converter,
    'CopyObject', '', '', ObjConvOpts, XMLStr);

  Target.SOAPToObject(RootNode, ResultNode, converter);
end;

{ TSoapWithLog }

procedure TSoapWithLog.AddRequestHttpheaderItem(const name, Value: string);
var
  h: THeaderItem;
begin
  h.name := name;
  h.Value := Value;

  // eliminate duplicates
  for var x := 0 to fRequestCustomHttpHeaders.count-1 do
    if Name = fRequestCustomHttpHeaders[x].Name then
    begin
      fRequestCustomHttpHeaders[x]:= h;
      Exit;
    end;

  // add as new
  fRequestCustomHttpHeaders.Add(h);
end;

procedure TSoapWithLog.AfterRequestSaved(const FileName: string);
begin

end;

function TSoapWithLog.GetHttp(ConnectTimeout :Integer=30000;  SendTimeout :Integer=30000;  ReceiveTimeout : integer =30000; const aName:String=''): THTTPRIO;
var
  mr: THTTPReqRespWithMonitor;
begin
  result := THTTPRIO.Create(nil);
  result.OnBeforeExecute := LogBeforeExecute;
  result.OnAfterExecute := LogAfterExecute;
  mr := THTTPReqRespWithMonitor.Create(result);
  mr.OnSoapMessage := HttpOnSoapMessage;

  mr.ConnectTimeout := ConnectTimeout ;
{$IFNDEF DELPHIX_RIO_UP      }
  mr.SendTimeout := SendTimeout ;
{$ENDIF}
  mr.ReceiveTimeout := ReceiveTimeout ;

  mr.OnBeforePost := BeforePost;

  result.HTTPWebNode := mr;

  if aName<>'' then
    result.HTTPWebNode.name := aName
    else
  result.HTTPWebNode.name := 'HTTPWebNode2'; { do not localize }

  result.HTTPWebNode.SetSubComponent(true);

end;

procedure TSoapWithLog.HttpOnSoapMessage(sender: tobject; Stream: TStream;
  IsRequest: Boolean; const RequestTimeStamp: TDateTime);
var
  fn, s: string;
  l: TStringList;
  ms: TMemorystream;
  i: integer;
  sTimeStamp: String;
begin
  if Stream.Size = 0 then
    exit;

    sTimeStamp:= formatDateTime('yyyy"-"mm"-"dd" "hh";"nn";"ss"."zzz', RequestTimeStamp);

  i := Stream.position;
  ms := TMemorystream.Create;
  Stream.position := 0;
  ms.copyFrom(Stream, Stream.Size);
  Stream.position := i;

  ms.position := 0;
  l := TStringList.Create;
  try
    l.LoadFromStream(ms, TEncoding.UTF8);
  except
    ms.position := 0;
    l.LoadFromStream(ms);
  end;
  ms.position := 0;

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
      s:=fn;
      i:=1;
      while fileExists(fn) do
      begin
      fn:=changeFileExt(s, ' ('+IntToStr(i)+').xml');
      inc(i);
      end;

    ms.SaveToFile(fn);
    AfterRequestSaved(fn);
    Log('Saved SOAP ' + s + ' to: "' + fn + '"');
  end;
  ms.Free;
  l.Free;
end;

procedure TSoapWithLog.LogAfterExecute(const MethodName: string;
  Response: TStream);
begin
  Log('rio.AfterExecute method: "' + MethodName + '"');
end;

procedure TSoapWithLog.LogBeforeExecute(const MethodName: string;
  Response: TStream);
begin
  Log('rio.beforeExecute method: "' + MethodName + '"');
end;

procedure TSoapWithLog.BeforePost;
var
{$IFNDEF DELPHIX_RIO_UP      }
  s: string;
{$ENDIF}
  x: integer;
  n, v: string;
begin
  fLastUrl:= AHTTPReqResp.URL;
  BeforePost2;

{$IFNDEF DELPHIX_RIO_UP      }
  s := '';
{$ENDIF}
  for x := 0 to fRequestCustomHttpHeaders.count - 1 do
  begin
    n := fRequestCustomHttpHeaders[x].name;
    v := fRequestCustomHttpHeaders[x].Value;

{$IFDEF DELPHIX_RIO_UP      }
    Client.CustomHeaders[n] := v;
{$ELSE}
    s := s + n;
    if v <> '' then
      s := s + ': ' + v;
    s := s + cr;
{$ENDIF}
  end;
{$IFNDEF DELPHIX_RIO_UP      }
  if s <> '' then
    HttpAddRequestHeaders(AData,
      // A pointer to a string variable containing the headers to append to the request.
      // Each header must be terminated by a CR/LF (carriage return/line feed) pair.
      PChar(s), Length(s), HTTP_ADDREQ_FLAG_ADD
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
