unit maxLogic.CGIUtils;

interface

uses
  CGIApp,
  System.SysUtils, System.Classes, Web.HTTPApp,
  Grijjy.Bson, Web.CGIHTTP;

Type
  TParamKind = (pkAny, // all, in that order
    pkQuery, // from the url params
    pkCLI, // from command line
    pkPost, // from the post body
    pkPostAsJsonValue, // tries to parse the post body as json and read the value
    pkCookie,
    pkEnviroment
    );
  TParamKinds = set of TParamKind;

  TParamKindsHelper = record helper for TParamKinds
    function All: TParamKinds;
    // if pkAny is given, will call the All method to replace the given values
    procedure ResolveAny;
  end;

  TTriBool = (bUnknown, bTrue, bFalse);

  // allows for separate writing of the header and also allows to flush the buffer periodically
  // ATTENTION: do not use the contentStream property of the inherited response object
  TCGIResponseEx = class(TCGIResponse)
  private
    fBuffer: TMemoryStream;
    fHeaderWasSend: Boolean;
    fBufferClosed: Boolean;
    fFinalized: Boolean;
    FBytesSend: Int64;
    FLineBreak: AnsiString;
    FEncoding: TEncoding;
    function GetBufferOpen: Boolean;
    function GetBufferSize: Int64;
    procedure SetEncoding(const Value: TEncoding);
    procedure SetLineBreak(const Value: AnsiString);
  protected
    procedure InitResponse; override;
    // for the inherited property Content
    // note: it is better to use write/writeln if you have more data to write then just a single small string...
    function GetContent: string; override;
    procedure SetContent(const Value: string); override;
  public
    constructor Create(HTTPRequest: TWebRequest);
    destructor Destroy; override;

    // the same as FinalizeResponse
    procedure SendResponse; override;
    // tells if the response was already send and finalized
    function Sent: Boolean; override;

    // sends the header if it was not yet send
    // if aIncludeContentLength is true then the Content-Length header will be added as well
    // if the property ContentLength is <= 0, it will be set to the size of the current buffer
    Procedure SendHeader(aIncludeContentLength: Boolean);

    // sends header (without content-length) if not yet send and then the buffer
    Procedure Flush;
    Procedure ClearBuffer;
    // will flush first then set the buffer to closed
    // if needed please call ClearBuffer first
    Procedure CloseBuffer;
    // ATTENTION after a call to FinalizeResponse no more data will be send
    // if the header was not yet send, then the content-length will be set to the size of the buffer
    // but if there was already something send to the client, then the content=length will not be added to the respnse
    Procedure FinalizeResponse;

    // will write to the buffer if it is still open, otherwise directly to the client
    Procedure Write(const s: String);
    Procedure WriteLn(const s: String);

    // writes directly to the client bypassing all. so no header and no buffer will be written at all by this
    function DirectWrite(var buffer; Size: Integer): Integer; overload;
    function DirectWrite(const s: String): Integer; overload;

    // tells if the header was already send
    property HeaderSend: Boolean read fHeaderWasSend;
    // per default true, false if CloseBuffer was called
    property BufferOpen: Boolean read GetBufferOpen;
    // tells how many bytes are in the buffer
    property BufferSize: Int64 read GetBufferSize;
    // tells how many bytes were send to the http client. Does not include the header itself
    property BytesSend: Int64 read FBytesSend;

    property LineBreak: AnsiString read FLineBreak write SetLineBreak;
    property Encoding: TEncoding read FEncoding write SetEncoding;
  end;

  TCGIFactoryEx = class(TCGIFactory)
  protected
    function NewResponse(CGIRequest: TCGIRequest): TCGIResponse; override;
  end;

  TCgIUtils = class
  private
    FRequest: TWebRequest;
    FResponse: TCGIResponseEx;
    FPostBodyIsJson: TTriBool;
    FPostBodyAsJson: TGoBsonDocument;

    procedure SetRequest(const Value: TWebRequest);
    procedure SetResponse(const Value: TCGIResponseEx);
  public
    Constructor Create(aRequest: TWebRequest; aResponse: TWebResponse);
    Destructor Destroy; override;

    // for debuging, takes the most importat Request parameters and creates a plain/text http response
    Procedure RenderInfo;

    // returns true if the application was executed directly from the terminal instead beeing called by the web server
    Function NonCgiMode: Boolean;
    Procedure SetPlainTextContent;

    // a helper fnction to read the different parameter kinds
    Function GetParam(const aName: String; const aFrom: TParamKinds = [pkAny]): String; overload;
    Function GetParam(const aName: String; aFrom: TParamKinds; out aValue: String; out aFoundFrom: TParamKind): Boolean; overload;

    Function GetPostBodyAsJson(out aJson: TGoBsonDocument): Boolean;
    Function PostBodyIsJson: Boolean;

    // ATTENTION: this will start usin the ContentStream of the response!
    Procedure appendContent(const s: String);
    // writes to content
    Procedure Write(const s: String);
    Procedure WriteLn(const s: String);
    // this wil close and send the buffer, all calls to write/writeln/appendContent afterwards will be written directly to the stdOut
    // returns false if the response is not of tyle TCGIResponse
    procedure CloseBuffer;

    property Request: TWebRequest read FRequest write SetRequest;
    property Response: TCGIResponseEx read FResponse write SetResponse;
  end;

implementation

uses
  maxLogic.DateUtils, ioUtils;

{ TCgIUtils }

procedure TCgIUtils.appendContent(const s: String);
begin
  FResponse.Write(s);
end;

procedure TCgIUtils.CloseBuffer;
begin
  FResponse.CloseBuffer;
end;

constructor TCgIUtils.Create(aRequest: TWebRequest;
  aResponse: TWebResponse);
begin
  inherited Create;
  FRequest := aRequest;
  FResponse := aResponse as TCGIResponseEx;

  FPostBodyIsJson := bUnknown;
end;

destructor TCgIUtils.Destroy;
begin

  inherited;
end;

function TCgIUtils.GetParam(const aName: String; aFrom: TParamKinds;
  out aValue: String; out aFoundFrom: TParamKind): Boolean;
var
  pk: TParamKind;
begin
  Result := false;
  aValue := '';

  aFrom.ResolveAny;
  for pk in aFrom do
  begin
    case pk of
      pkQuery:
        aValue := Request.QueryFields.values[aName];
      pkCLI:
        FindCmdLineSwitch(aName, aValue, True, [clstValueNextParam]);
      pkPost:
        aValue := Request.ContentFields.values[aName];
      pkPostAsJsonValue:
        begin
          if PostBodyIsJson then
            Result := FPostBodyAsJson[aName]
        end;
      pkCookie:
        aValue := Request.CookieFields.values[aName];
      pkEnviroment:
        aValue := GetEnvironmentVariable(aName);
    end; // case

    if aValue <> '' then
    begin
      aFoundFrom := pk;
      Exit(True);
    end;
  end;
end;

function TCgIUtils.GetPostBodyAsJson(out aJson: TGoBsonDocument): Boolean;
begin
  case FPostBodyIsJson of
    bUnknown:
      begin
        if TGoBsonDocument.Tryparse(Request.Content, FPostBodyAsJson) then
        begin
          FPostBodyIsJson := bTrue;
          aJson := FPostBodyAsJson;
          Result := True;
        end else begin
          FPostBodyIsJson := bFalse;
          Result := false;
        end;
      end;
    bFalse:
      Result := false;
    bTrue:
      begin
        aJson := FPostBodyAsJson;
        Result := True;
      end;
  else
    Result := false;
  end;
end;

function TCgIUtils.GetParam(const aName: String; const aFrom: TParamKinds): String;
var
  pk: TParamKind;
begin
  GetParam(aName, aFrom, Result, pk);
end;

function TCgIUtils.NonCgiMode: Boolean;
begin
  Result := (Request.Method = '') and (Request.URL = '') and (Request.Host = '');
end;

function TCgIUtils.PostBodyIsJson: Boolean;
begin
  case FPostBodyIsJson of
    bUnknown:
      begin
        if (Request.Content <> '') AND TGoBsonDocument.Tryparse(Request.Content, FPostBodyAsJson) then
        begin
          FPostBodyIsJson := bTrue;
          Result := True;
        end else begin
          FPostBodyIsJson := bFalse;
          Result := false;
        end;
      end;
    bFalse:
      Result := false;
    bTrue:
      Result := True;
  else
    Result := false;
  end;
end;

procedure TCgIUtils.RenderInfo;
begin
  Response.ContentType := 'text/plain; charset="UTF-8"';
  WriteLn(
    'now: ' + eTime + #10 +
      '-------------------------------' + #10 +
      '||| REQUEST INFO |||' + #10 +
      'Method: ' + Request.Method + #10 +
      'URL: ' + Request.URL + #10 +
      'ProtocolVersion: ' + Request.ProtocolVersion + #10 +
      'Query: ' + Request.Query + #10 +
      'PathInfo: ' + Request.PathInfo + #10 +
      'Authorization: ' + Request.Authorization + #10 +
      'Date: ' + eTime(Request.Date) + #10 +
      'Accept: ' + Request.Accept + #10 +
      'From: ' + Request.From + #10 +
      'Host: ' + Request.Host + #10 +
      'Referer: ' + Request.Referer + #10 +
      'UserAgent: ' + Request.UserAgent + #10 +
      'ContentEncoding: ' + Request.ContentEncoding + #10 +
      'ContentType: ' + Request.ContentType + #10 +
      'ContentLength: ' + IntToStr(Request.ContentLength) + #10 +
      'ContentVersion: ' + Request.ContentVersion + #10 +
      'RemoteAddr: ' + Request.RemoteAddr + #10 +
      'RemoteHost: ' + Request.RemoteHost + #10 +
      'ScriptName: ' + Request.ScriptName + #10 +
      'RemoteIP: ' + Request.RemoteIP + #10 +
      '==ContentFields==' + #10 +
      Request.ContentFields.Text + #10 +
      '==CookieFields==' + #10 +
      Request.CookieFields.Text + #10 +
      '==QueryFields==' + #10 +
      Request.QueryFields.Text + #10);

end;

procedure TCgIUtils.SetPlainTextContent;
begin
  Response.ContentType := 'text/plain; charset="UTF-8"';
end;

procedure TCgIUtils.SetRequest(const Value: TWebRequest);
begin
  FRequest := Value;
end;

procedure TCgIUtils.SetResponse(const Value: TCGIResponseEx);
begin
  FResponse := Value;
end;

procedure TCgIUtils.Write(const s: String);
begin
  appendContent(s);
end;

procedure TCgIUtils.writeLn(const s: String);
begin
  FResponse.writeLn(s)
end;

{ TParamKindsHelper }

function TParamKindsHelper.All: TParamKinds;
var
  pk: TParamKind;
begin
  Result := [];
  for pk := Low(TParamKind) to High(TParamKind) do
    include(Result, pk);
  Exclude(Result, pkAny);
end;

procedure TParamKindsHelper.ResolveAny;
begin
  if pkAny in self then
    self := All;
end;

{ TCGIResponseEx }

procedure TCGIResponseEx.ClearBuffer;
begin
  fBuffer.Position := 0;
end;

procedure TCGIResponseEx.CloseBuffer;
begin
  Flush;
  fBufferClosed := True;
end;

constructor TCGIResponseEx.Create(HTTPRequest: TWebRequest);
begin
  inherited;
  fBuffer := TMemoryStream.Create;
  FLineBreak := sLineBreak;
  FEncoding := TEncoding.UTF8;
end;

destructor TCGIResponseEx.Destroy;
begin
  FreeAndNil(fBuffer);
  inherited;
end;

function TCGIResponseEx.DirectWrite(const s: String): Integer;
var
  bytes: TBytes;
begin
  bytes := FEncoding.getbytes(s);
  Result := length(bytes);
  if Result <> 0 then
    DirectWrite(bytes[0], length(bytes))
end;

function TCGIResponseEx.DirectWrite(var buffer; Size: Integer): Integer;
begin
  Result := HTTPRequest.WriteClient(buffer, Size);
end;

procedure TCGIResponseEx.FinalizeResponse;
begin
  SendHeader(True);
  Flush;
  fFinalized := True;
end;

procedure TCGIResponseEx.Flush;
begin
  SendHeader(false);
  DirectWrite(fBuffer.Memory^, fBuffer.Position);
  FBytesSend := BytesSend + fBuffer.Position;
  fBuffer.Position := 0;
end;

function TCGIResponseEx.GetBufferOpen: Boolean;
begin
  Result := not fBufferClosed;
end;

function TCGIResponseEx.GetBufferSize: Int64;
begin
  Result := fBuffer.Position;
end;

function TCGIResponseEx.GetContent: string;
begin
  FEncoding.getstring(TBytes(fBuffer.Memory^), 0, fBuffer.Position);
end;

procedure TCGIResponseEx.InitResponse;
begin
  inherited;

end;

procedure TCGIResponseEx.SendHeader;
var
  StatusString: string;
  Headers: string;
  I: Integer;
  x: Integer;

  procedure AddHeaderItem(const Item: string; FormatStr: string); overload;
  begin
    if Item <> '' then
      Headers := Headers + Format(FormatStr, [Item]);
  end;

begin
  if fHeaderWasSend then
    Exit;
  fHeaderWasSend := True;

  // if (HTTPRequest.ProtocolVersion <> '' )  then
  begin
    if StatusCode > 0 then
      StatusString := Format('%d %s', [StatusCode, ReasonString])
    else
      StatusString := '200 OK'; { do not localize }
    AddHeaderItem(Location, 'Location: %s' + FLineBreak); { do not localize }
    AddHeaderItem(Allow, 'Allow: %s' + FLineBreak); { do not localize }
    for I := 0 to Cookies.Count - 1 do
      AddHeaderItem(Cookies[I].HeaderValue, 'Set-Cookie: %s' + FLineBreak); { do not localize }
    AddHeaderItem(DerivedFrom, 'Derived-From: %s' + FLineBreak); { do not localize }
    if Expires > 0 then
      AddHeaderItem(Format(FormatDateTime(sDateFormat + ' "GMT"', { do not localize }
            Expires), [DayOfWeekStr(Expires), MonthStr(Expires)]), 'Expires: %s' + FLineBreak); { do not localize }

    if LastModified > 0 then
      AddHeaderItem(Format(FormatDateTime(sDateFormat +
              ' "GMT"', LastModified), [DayOfWeekStr(LastModified), { do not localize }
            MonthStr(LastModified)]), 'Last-Modified: %s' + FLineBreak); { do not localize }
    AddHeaderItem(Title, 'Title: %s' + FLineBreak); { do not localize }
    AddHeaderItem(FormatAuthenticate, 'WWW-Authenticate: %s' + FLineBreak); { do not localize }
    AddCustomHeaders(Headers);
    AddHeaderItem(ContentVersion, 'Content-Version: %s' + FLineBreak); { do not localize }
    AddHeaderItem(ContentEncoding, 'Content-Encoding: %s' + FLineBreak); { do not localize }
    AddHeaderItem(ContentType, 'Content-Type: %s' + FLineBreak); { do not localize }
    if aIncludeContentLength then
    begin
      if ContentLength <= 0 then
        ContentLength := self.BufferSize;
      AddHeaderItem(IntToStr(ContentLength), 'Content-Length: %s' + FLineBreak); { do not localize }
    end;
    Headers := Headers + 'Content:' + FLineBreak + FLineBreak; { do not localize }

    // for x := 0 to Headers.Count-1 do
    self.DirectWrite(Headers)
  end;
end;

procedure TCGIResponseEx.SendResponse;
begin
  FinalizeResponse;
end;

function TCGIResponseEx.Sent: Boolean;
begin
  Result := fFinalized;
end;

procedure TCGIResponseEx.SetContent(const Value: string);
var
  bytes: TBytes;
begin
  bytes := FEncoding.getbytes(Value);
  fBuffer.Position := 0;
  if length(bytes) <> 0 then
    fBuffer.WriteBuffer(bytes[0], length(bytes));
end;

procedure TCGIResponseEx.SetEncoding(const Value: TEncoding);
begin
  FEncoding := Value;
end;

procedure TCGIResponseEx.SetLineBreak(const Value: AnsiString);
begin
  FLineBreak := Value;
end;

procedure TCGIResponseEx.Write(const s: String);
var
  bytes: TBytes;
begin
  if fBufferClosed then
  begin
    FBytesSend := FBytesSend + DirectWrite(s);
  end else begin
    bytes := FEncoding.getbytes(s);
    if length(bytes) <> 0 then
    begin
      fBuffer.WriteBuffer(bytes[0], length(bytes));
      FBytesSend := FBytesSend + length(bytes);
    end;
  end;

end;

procedure TCGIResponseEx.writeLn(const s: String);
begin
  write(s + FLineBreak);
end;

{ TCGIFactoryEx }

function TCGIFactoryEx.NewResponse(CGIRequest: TCGIRequest): TCGIResponse;
begin
  Result := TCGIResponseEx.Create(CGIRequest);
end;

initialization

TCGIFactoryEx.Create;

end.
