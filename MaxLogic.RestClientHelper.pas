unit MaxLogic.RestClientHelper;

{ Version: 1.5
}

{ see:
  http://docwiki.embarcadero.com/RADStudio/Seattle/en/REST_Client_Library

  echo server:
  case fRestClient.Request.method of
  rmPOST: fRestClient.Request.Resource :='post' ;
  rmGET: fRestClient.Request.Resource := 'get';
  end;
  fRestClient.Client.baseUrl :='https://postman-echo.com';

  REMARK: headers should be added always with TRESTRequestParameterOption.poDoNotEncode flag
}
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.StrUtils, System.Variants, System.Classes,
  Generics.Collections, REST.Utils, REST.Types, REST.Client, REST.Response.Adapter, REST.Authenticator.Simple, REST.Authenticator.Basic,
  REST.Authenticator.OAuth, IPPeerClient, REST.JSON, System.JSON,
  MaxLogic.JSON, types;

Type
  TStringArray = types.TStringDynArray;

  TRESTRequestMethod = REST.Types.TRESTRequestMethod;

  TmaxRestClient = class
  private
    fRESTResponse: TRESTResponse;
    fRESTRequest: TRESTRequest;
    fRESTClient: TRESTClient;
    FOnAfterExecute: TCustomRESTRequestNotifyEvent;
    FURLAlreadyEncoded: Boolean;
    FOnBeforeExecute: TNotifyEvent;
    procedure RESTRequestAfterExecute(Sender: TCustomRESTRequest);
    procedure RESTRequestHTTPProtocolError(Sender: TCustomRESTRequest);
    procedure SetOnAfterExecute(const Value: TCustomRESTRequestNotifyEvent);
    function GetResponseJSONValueAsString: String;
    function GetJSONValue: TJsonValue;
    function StrintListToKeyValuePairs(l: TStringList): TStringArray;
    function CommaStringToKeyValuePairs(const s: String): TStringArray;
    procedure SetURLAlreadyEncoded(const Value: Boolean);
    procedure SetOnBeforeExecute(const Value: TNotifyEvent);
  public
    Constructor Create;
    Destructor Destroy; override;

    // execute/call REST. afterwards you can access the response
    procedure Call(const baseUrl, ResourceName: string; const ParamsAsKeyValuepairs: TStringArray = []; Method: TRESTRequestMethod = rmGet); overload;
    procedure Call(const ResourceName: string; const ParamsAsKeyValuepairs: TStringArray = []; Method: TRESTRequestMethod = rmGet); overload;
    // params as TStringList, where each line consists of <key>=<value> pairs
    procedure Call(const baseUrl, ResourceName: string; Params: TStringList; Method: TRESTRequestMethod = rmGet); overload;
    procedure Call(const ResourceName: string; Params: TStringList; Method: TRESTRequestMethod = rmGet); overload;
    // params as a comma separated string, where <key1>=<value1>,<key2>=<value2>
    procedure Call(const baseUrl, ResourceName: string; const aParamsAsCommaSeparatedString: String; Method: TRESTRequestMethod = rmGet); overload;
    procedure Call(const ResourceName: string; const aParamsAsCommaSeparatedString: String; Method: TRESTRequestMethod); overload;


    procedure CallJson(const BaseUrl, ResourceName: string; const aJsonPayload: String;
      const aContentType: String = 'application/json';
      Method: TRESTRequestMethod = TRESTRequestMethod.rmPOST); overload;
    procedure CallJson(const ResourceName: string; const aJsonPayload: String;
      const aContentType: String = 'application/json';
      Method: TRESTRequestMethod = TRESTRequestMethod.rmPOST); overload;

    // Retrieve some responses from the JSONValue
    Function TryGetJSONValue(const path: string; out Value: string): boolean;

    // deserializes the json response to an Object
    Function Deserialize<T: class, constructor>(): T; overload;
    // as above but not the whole response but only a sub path
    Function Deserialize<T: class, constructor>(const aPath: String): T; overload;
    // similar to the above, but deserializes an array
    Function DeserializeArrayOf<T: class, constructor>(const aPath: String = ''): TArray<T>;


    Procedure ResetRESTComponentsToDefaults(ResetTheRestClientItself: boolean = true);

    class procedure LogRequest(aRestRequest: TRESTRequest; log: TStrings); overload;
    class function LogRequest(aRestRequest: TRESTRequest): string; overload;
    class function LogResponse(Response: TRESTResponse): string;
    class procedure LogRequestparams(Params: TRESTRequestParameterList; log: TStrings);

    property Response: TRESTResponse read fRESTResponse;
    property Request: TRESTRequest read fRESTRequest;
    property Client: TRESTClient read fRESTClient;
    // sets Request.URLAlreadyEncoded to that value in the call method
    property URLAlreadyEncoded: Boolean read FURLAlreadyEncoded write SetURLAlreadyEncoded;

    property OnAfterExecute: TCustomRESTRequestNotifyEvent read FOnAfterExecute write SetOnAfterExecute;
    // shortcut to the response values
    Property JSONValueAsString: String read GetResponseJSONValueAsString;
    property JSONValue: TJsonValue read GetJSONValue;

    // the sender param in the TNotifyEvent will be the TmaxRestClient
    // this gives you the opportunity to adjust something before the actuall request.execute call
    property OnBeforeExecute: TNotifyEvent read FOnBeforeExecute write SetOnBeforeExecute;
  end;

implementation

uses
  RTTI, System.UITypes, REST.Authenticator.OAuth.WebForm.Win, ClipBrd;

{ TmaxRestClient }

Function TmaxRestClient.StrintListToKeyValuePairs(l: TStringList): TStringArray;
var
  i, x: Integer;
  n, v: String;
begin
  i := 0;
  SetLength(Result, l.Count * 2);
  for x := 0 to l.Count - 1 do
  begin
    Result[i] := l.Names[x];
    inc(i);
    Result[i] := l.ValueFromIndex[x];
    inc(i);
  end;
end;

procedure TmaxRestClient.Call(const ResourceName: string;
  Params: TStringList; Method: TRESTRequestMethod);
begin
  Call(ResourceName, StrintListToKeyValuePairs(Params), Method);
end;

procedure TmaxRestClient.Call(const baseUrl, ResourceName: string;
  Params: TStringList; Method: TRESTRequestMethod);
begin
  Call(baseUrl, ResourceName, StrintListToKeyValuePairs(Params), Method);
end;

procedure TmaxRestClient.Call(const ResourceName,
  aParamsAsCommaSeparatedString: String; Method: TRESTRequestMethod);
begin
  Call(ResourceName, CommaStringToKeyValuePairs(aParamsAsCommaSeparatedString), Method);
end;

procedure TmaxRestClient.CallJson(const BaseUrl, ResourceName,
  aJsonPayload, aContentType: String; Method: TRESTRequestMethod);
begin
  Client.BaseURL:= BaseUrl;
  CallJson(ResourceName, aJsonPayload, aContentType, Method);
end;

procedure TmaxRestClient.CallJson(const ResourceName, aJsonPayload: String;
      const aContentType: String = 'application/json';
  Method: TRESTRequestMethod = TRESTRequestMethod.rmPOST);
begin
  fRESTRequest.ResetToDefaults;
  fRESTRequest.URLAlreadyEncoded:= URLAlreadyEncoded;
  fRESTResponse.ResetToDefaults;

  self.Request.Resource := ResourceName;

  Request.Params.AddBody(aJsonPayload, aContentType);
  self.Request.Method := Method;
  if assigned(fOnBeforeExecute) then
    fOnBeforeExecute(self);

  self.Request.Execute;
end;

procedure TmaxRestClient.Call(const baseUrl, ResourceName,
  aParamsAsCommaSeparatedString: String; Method: TRESTRequestMethod);
begin
  Call(baseUrl, ResourceName, CommaStringToKeyValuePairs(aParamsAsCommaSeparatedString), Method);
end;

constructor TmaxRestClient.Create;
begin
  inherited Create;

  fRESTResponse := TRESTResponse.Create(nil);
  fRESTRequest := TRESTRequest.Create(nil);
  fRESTClient := TRESTClient.Create(nil);

  with fRESTResponse do
  begin
    Name := 'RESTResponse';
  end;

  with fRESTClient do
  begin
    Name := 'RESTClient';
    // Authenticator := OAuth1_FitBit;
    Accept := 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8';
    HandleRedirects := true;
  end;

  with fRESTRequest do
  begin
    Name := 'RESTRequest';
    Client := fRESTClient;
    Response := fRESTResponse;
    OnAfterExecute := RESTRequestAfterExecute;
    /// <summary>
    /// Specifies if Events (such as OnAfterExecute) should run in the context of the main
    /// thread (true) or in the context of an arbitrary thread - which was created by the developer or by using
    /// ExecuteAsync.
    SynchronizedEvents := False;
    OnHTTPProtocolError := RESTRequestHTTPProtocolError;
  end;

end;

function TmaxRestClient.Deserialize<T>(): T;
begin
  if Assigned(fRESTResponse.JSONValue) then
    Result := TJson.JsonToObject<T>(fRESTResponse.JSONValue as TJsonObject)
  else
    Result := Nil;
end;

function TmaxRestClient.Deserialize<T>(const aPath: String): T;
var
  jo: TJsonObject;
begin
  Result:= nil;
  if Assigned(fRESTResponse.JSONValue) then
  begin
    jo:= fRESTResponse.JSONValue as TJsonObject;
    if jo.TryGetValue<TJsonObject>(aPath, jo) then
      Result := TJson.JsonToObject<T>(jo);
  end;
end;

function TmaxRestClient.DeserializeArrayOf<T>(
  const aPath: String): TArray<T>;
var
  jo: TJsonObject;
  jv: TJsonValue;
  ja: TJsonArray;
  x: Integer;
  y: Integer;
begin
  ja:= nil;
  Result:= [];
  if Assigned(fRESTResponse.JSONValue) then
  begin
    if (aPath = '') and (fRESTResponse.JSONValue is TJsonArray) then
      ja := fRESTResponse.JSONValue as TJsonArray
    else begin
      jo:= fRESTResponse.JSONValue as TJsonObject;
      if not jo.TryGetValue<TJsonArray>(aPath, ja) then
        ja := nil;
    end;

    if assigned(ja) then
    begin
      setLength(result, ja.Count);
      for x := 0 to ja.Count-1 do
      begin
        try
          jo:= ja[x] as TJsonObject;
          Result[x] := TJson.JsonToObject<T>(jo);
        Except
          // clean up the result, otherwise what we already serialized will leak
          for y := 0 to x-1 do
          begin
            Result[y].Free;
          end;
          Result := [];
          raise;
        end;
      end;
    end;

  end;
end;

destructor TmaxRestClient.Destroy;
begin
  fRESTResponse.free;
  fRESTRequest.free;
  fRESTClient.free;
  inherited;
end;

procedure TmaxRestClient.RESTRequestAfterExecute(Sender: TCustomRESTRequest);
begin
  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(Sender);
end;

procedure TmaxRestClient.RESTRequestHTTPProtocolError(Sender: TCustomRESTRequest);
begin
  // show error
  { memo_ResponseData.Lines.Add'HttpError: '+(Sender.Response.StatusText);
    memo_ResponseData.Lines.Add(Sender.Response.Content);
  }
end;

procedure TmaxRestClient.ResetRESTComponentsToDefaults;
begin
  /// reset all of the rest-components for a complete
  /// new request
  ///
  /// --> we do not clear the private data from the
  /// individual authenticators.
  ///
  fRESTRequest.ResetToDefaults;
  if ResetTheRestClientItself then
    fRESTClient.ResetToDefaults;
  fRESTResponse.ResetToDefaults;
end;

class procedure TmaxRestClient.LogRequest(aRestRequest: TRESTRequest;
  log: TStrings);
var
  x: Integer;
begin
  log.add('GetFullRequestURL: ' + aRestRequest.GetFullRequestURL);
  log.add('BaseUrl: ' + aRestRequest.Client.baseUrl);
  log.add('Resource: ' + aRestRequest.Resource);
  log.add('FullResource: ' + aRestRequest.FullResource);
  log.add('Method: ' + TValue.From<TRESTRequestMethod>(aRestRequest.Method).ToString);

  log.add('HandleRedirects: ' + IfThen(aRestRequest.HandleRedirects, 'True', 'False'));
  log.add('AcceptEncoding: ' + aRestRequest.AcceptEncoding);
  log.add('Accept: ' + aRestRequest.Accept);
  log.add('AutoCreateParams: ' + IfThen(aRestRequest.AutoCreateParams, 'True', 'False'));

  log.add('aRestRequest.TransientParams');
  LogRequestparams(aRestRequest.TransientParams, log);
  log.add('aRestRequest.Params');
  LogRequestparams(aRestRequest.Params, log);

end;

class procedure TmaxRestClient.LogRequestparams(
  Params: TRESTRequestParameterList; log: TStrings);
var
  p: TRESTRequestParameter;
  i: Integer;
begin
  i := 0;
  for p in Params do
  begin
    inc(i);
    log.add('#' + IntToStr(i) + ' ' +
      p.ToString);
  end;
end;

function TmaxRestClient.CommaStringToKeyValuePairs(
  const s: String): TStringArray;
var
  l: TStringList;
begin
  l := TStringList.Create;
  try
    l.CommaText := s;
    Result := StrintListToKeyValuePairs(l);
  finally
    l.free;
  end;
end;

procedure TmaxRestClient.SetOnAfterExecute(
  const Value: TCustomRESTRequestNotifyEvent);
begin
  FOnAfterExecute := Value;
end;

procedure TmaxRestClient.SetOnBeforeExecute(const Value: TNotifyEvent);
begin
  FOnBeforeExecute := Value;
end;

procedure TmaxRestClient.SetURLAlreadyEncoded(const Value: Boolean);
begin
  FURLAlreadyEncoded := Value;
end;

function TmaxRestClient.TryGetJSONValue(const path: string;
  out Value: string): boolean;
begin
  Result := False;
  if Assigned(fRESTResponse.JSONValue) then
    Result := fRESTResponse.JSONValue.TryGetValue<string>(path, Value)
end;

procedure TmaxRestClient.Call(const baseUrl, ResourceName: string;
  const ParamsAsKeyValuepairs: TStringArray; Method: TRESTRequestMethod);
begin
  Client.baseUrl := baseUrl;
  Call(ResourceName, ParamsAsKeyValuepairs, Method);
end;

procedure TmaxRestClient.Call(const ResourceName: string;
  const ParamsAsKeyValuepairs: TStringArray;
  Method: TRESTRequestMethod);
var
  x, Paramcount: Integer;
begin
  fRESTRequest.ResetToDefaults;
  fRESTRequest.URLAlreadyEncoded:= URLAlreadyEncoded;
  fRESTResponse.ResetToDefaults;

  self.Request.Resource := ResourceName;

  Paramcount := Length(ParamsAsKeyValuepairs) div 2;
  for x := 0 to Paramcount - 1 do
  begin
    self.Request.Params.AddItem(
      ParamsAsKeyValuepairs[x * 2],
      ParamsAsKeyValuepairs[x * 2 + 1],
      pkGETorPOSt);
  end;

  self.Request.Method := Method;

  if assigned(fOnBeforeExecute) then
    fOnBeforeExecute(self);

  self.Request.Execute;
end;

function TmaxRestClient.GetResponseJSONValueAsString: String;
begin
  if Assigned(self.Response.JSONValue) then
    Result :=
      TJson.Format(self.Response.JSONValue)
  else
    Result := '';
end;

function TmaxRestClient.GetJSONValue: TJsonValue;
begin
  Result := self.Response.JSONValue;
end;

class function TmaxRestClient.LogResponse(Response: TRESTResponse): string;
begin
  Result :=
    'ContentEncoding (like deflate or gzip): "' + Response.ContentEncoding + '"' + sLineBreak +
    'ContentLength: ' + IntToStr(Response.ContentLength) + sLineBreak +
    'ContentType (MIME content type of response): "' + Response.ContentType + '"' + sLineBreak +
    'HTTP protocol error that occured during request "' + Response.ErrorMessage + '"' + sLineBreak +
    'HTTP response header-lines: "' + Response.Headers.text + '"' + sLineBreak +
  // the same as response.JSONValue
    'JSONText: "' + Response.JSONText + '"' + sLineBreak +
    'FullRequestURI: "' + Response.FullRequestURI + '"' + sLineBreak +
    'Server identifier, e.g. "Microsoft-IIS/7.0": "' + Response.server + '"' + sLineBreak +
    'HTTP response status code: ' + IntToStr(Response.StatusCode) + sLineBreak +
    'HTTP response status text: "' + Response.StatusText + '"' + sLineBreak +
    'String representation of response content: "' + Response.Content + '"' + sLineBreak +
    'Optional path into the JSON response content.  The path identifies a starting point for loading the JSONValue property.: "' +
    Response.RootElement + '"' + sLineBreak;

end;

class function TmaxRestClient.LogRequest(aRestRequest: TRESTRequest): string;
var
  l: TStringList;
begin
  Result := '';
  l := TStringList.Create;
  try
    LogRequest(aRestRequest, l);
    Result := l.text;

  finally
    l.free;
  end;
end;

end.
