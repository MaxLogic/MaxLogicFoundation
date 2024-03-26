unit maxLogic.IndyHttpHelper;

{$I JEDI.INC}

// instead of indy use the net client per default
{$IFNDEF INDYHELPER_FORCE_USE_INDY} // if your project really needs to use the old way... define this in your project
{$DEFINE UseNetClient}
{$ENDIF}


{
  Version: 1.8

  History:
  2023.10.18: swith to net client but leave the interface basically as it was to simplify upgrade of older projects

}
{
  Just a small helper to easily setup the TidHttp class
}
interface

uses
  {$IFDEF MSWINDOWS} winapi.Windows, {$ENDIF}
  system.classes, system.sysUtils,

  {$IFDEF UseNetClient}
  system.Net.HTTPClient, system.Net.URLClient,
  {$ENDIF}
  idHTTP, idComponent, idGlobal, idStream, IdMultipartFormData, IdZLibCompressorBase, IdCookieManager, IdGlobalProtocols, IdHTTPHeaderInfo, IdException, IdStack, IdCompressorZLib,
  IdSSLOpenSSLHeaders, IdSSLOpenSSL, // ssl
  IdAuthenticationNTLM, // NTLM - uses OpenSSL libraries,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdURI;

const
  cDefaultTimeOut = 10000;

type
  TCreateOption = (oSSL, oGZIP, oKeepAlive, oHandleRedirects, oCookieManager, oNoCacheControl,
    oJsonRequestContentType // will set the request.contentType to Json and the Charset to utf8
    );
  TCreateOptions = set of TCreateOption;

  TOptions = record
  private
    procedure SetTimeOut(const Value: Integer);
  public
    // sets both ReadTimeOut and ConnectTimeout
    property TimeOut: Integer write SetTimeOut;
  public
    Options: TCreateOptions;
    // actually it is read/write timeout
    ReadTimeOut: Integer;
    ConnectTimeout: Integer;

    // RETURNS THE MOST IMPORTANT AND default options
    class function New(const AdditionalOptions: TCreateOptions = []): TOptions; static;
    // AS ABOVE BUT WITH SSL ENABLED
    class function DefaultSsl(const AdditionalOptions: TCreateOptions = []): TOptions; static;
  end;

type
  TWorkKind = (kWorkBegin, kWork, kWorkEnd);
  TWorkMode = idComponent.TWorkMode;
  TProgressProc = reference to procedure(http: TIdHttp;
    akind: TWorkKind;
    aWorkMode: TWorkMode;
    const bytesProcessed, bytesMax: int64;
    var cancel: boolean);

function CreateHttp(const Options: TOptions; const ProgressProc: TProgressProc = nil): TIdHttp;
procedure assignCustomHeaders(headers: TStringList; http: TIdHttp);
// call this to make sure the CloseGracefuly error will cause no harm.
procedure WalkAroundEIdConnClosedGracefully(const proc: TProc; http: TIdHttp);

{$IFDEF MSWINDOWS}
function SslDllsPresent: boolean;
{$ENDIF}

function downloadStream(const aurl: string; postParams: TStringList; response: TStream; const ProgressProc: TProgressProc = nil;
  // TimeOuts: -1 means use the defailt from the Toptions
  ConnectTimeout: Integer = -1; ReadTimeOut: Integer = -1): boolean;

function download(const aurl: string; out response: Tbytes; const ProgressProc: TProgressProc = nil;
  // TimeOuts: -1 means use the defailt from the Toptions
  ConnectTimeout: Integer = -1; ReadTimeOut: Integer = -1): boolean; overload;

function download(const aurl: string; postParams: TStringList; out response: Tbytes; const ProgressProc: TProgressProc = nil;
  // TimeOuts: -1 means use the defailt from the Toptions
  ConnectTimeout: Integer = -1; ReadTimeOut: Integer = -1): boolean; overload;

function download(const aurl: string; postParams: TStringList; out response: string; const ProgressProc: TProgressProc = nil;
  // TimeOuts: -1 means use the defailt from the Toptions
  ConnectTimeout: Integer = -1; ReadTimeOut: Integer = -1): boolean; overload;

implementation

uses
  maxLogic.ioUtils,
  autoFree, maxLogic.AnonymousProcToEvent, strUtils;

type
  TEventToProcForIdHttp = class(TComponent)
  private
    fWorkMax: int64;
    fWorkDone: int64;
    fProc: TProgressProc;
    procedure CallProc(akind: TWorkKind; aWorkMode: TWorkMode);

    procedure OnWorkBegin(ASender: TObject; aWorkMode: TWorkMode; AWorkCountMax: int64);
    procedure OnWork(ASender: TObject; aWorkMode: TWorkMode; AWorkCount: int64);
    procedure OnWorkEnd(ASender: TObject; aWorkMode: TWorkMode);

    function SSLOnVerifyPeer(Certificate: TIdX509; AOk: boolean; ADepth, AError: Integer): boolean;
  end;

procedure TEventToProcForIdHttp.CallProc(akind: TWorkKind; aWorkMode: TWorkMode);
var
  http: TIdHttp;
  aCancel: boolean;
begin
  http := self.owner as TIdHttp;
  aCancel := False;
  fProc(http, akind, aWorkMode, fWorkDone, fWorkMax, aCancel);
  if aCancel then
    http.disconnect;
end;

procedure TEventToProcForIdHttp.OnWorkBegin(ASender: TObject; aWorkMode: TWorkMode; AWorkCountMax: int64);
begin
  fWorkMax := AWorkCountMax;
  fWorkDone := 0;
  CallProc(kWorkBegin, aWorkMode);
end;

procedure TEventToProcForIdHttp.OnWork(ASender: TObject; aWorkMode: TWorkMode; AWorkCount: int64);
begin
  fWorkDone := AWorkCount;
  CallProc(kWork, aWorkMode);
end;

procedure TEventToProcForIdHttp.OnWorkEnd(ASender: TObject; aWorkMode: TWorkMode);
begin
  CallProc(kWorkEnd, aWorkMode);
end;

function CreateHttp(const Options: TOptions; const ProgressProc: TProgressProc = nil): TIdHttp;
var
  LHTTP: TIdHttp;
  LIO: TIdSSLIOHandlerSocketOpenSSL;
  EventHandler: TEventToProcForIdHttp;
begin
  EventHandler := nil;

  // init http
  LHTTP := TIdHttp.Create;
  LHTTP.HandleRedirects := true;
  LHTTP.request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101 Firefox/47.0';

  if assigned(ProgressProc) then
  begin
    EventHandler := TEventToProcForIdHttp.Create(LHTTP);
    EventHandler.fProc := ProgressProc;
    LHTTP.OnWorkBegin := EventHandler.OnWorkBegin;
    LHTTP.OnWork := EventHandler.OnWork;
    LHTTP.OnWorkEnd := EventHandler.OnWorkEnd;
  end;

  if oGZIP in Options.Options then
    LHTTP.Compressor := TIdCompressorZLib.Create(LHTTP);

  if oCookieManager in Options.Options then
  begin
    LHTTP.CookieManager := TIdCookieManager.Create(LHTTP);
    LHTTP.AllowCookies := true;
  end;

  LHTTP.HandleRedirects := oHandleRedirects in Options.Options;

  if oKeepAlive in Options.Options then
    LHTTP.request.Connection := 'keep-alive';

  if oNoCacheControl in Options.Options then
    LHTTP.request.CacheControl := 'no-cache';

  if (oSSL in Options.Options)
  {$IFDEF MSWINDOWS}
    and SslDllsPresent
  {$ENDIF}
  then
  begin
    LIO := TIdSSLIOHandlerSocketOpenSSL.Create(LHTTP);

    if not assigned(EventHandler) then
      EventHandler := TEventToProcForIdHttp.Create(LHTTP);

    LIO.OnVerifyPeer := EventHandler.SSLOnVerifyPeer;
    LIO.SSLOptions.SSLVersions := [sslvTLSv1_1, sslvTLSv1_2];

    LHTTP.IOHandler := LIO;

    LIO.DefaultPort := 443;
    LIO.Port := 443;
    LIO.SSLOptions.Mode := sslmClient;

    LIO.PassThrough := False;
  end;

  LHTTP.ConnectTimeout := Options.ConnectTimeout;
  LHTTP.ReadTimeOut := Options.ReadTimeOut;

  // assign the on progress events
  // LHTTP.OnWork := HttpWork;
  // LHTTP.OnWorkBegin := HttpWorkBegin;

  // by default the TIdHeaderList.FoldLines property is set to True, and lines get folded on whitespace and comma characters
  // that is probably not what we want. let us turn that off
  LHTTP.request.CustomHeaders.FoldLines := False;

  if oJsonRequestContentType in Options.Options then
  begin
    LHTTP.request.contentType := 'application/json';
    LHTTP.request.CharSet := 'utf-8';
  end;

  Result := LHTTP;
end;

procedure assignCustomHeaders(headers: TStringList; http: TIdHttp);
var
  x: Integer;
  ParamName, paramValue: string;
begin
  for x := 0 to headers.Count - 1 do
  begin
    ParamName := headers.Names[x];
    paramValue := headers.ValueFromIndex[x];
    http.request.CustomHeaders.AddValue(ParamName, paramValue);
  end;
end;

procedure WalkAroundEIdConnClosedGracefully;
begin
  try
    proc();
  except
    on e: EIdConnClosedGracefully do
    begin
      http.disconnect(False);
      http.IOHandler.InputBuffer.Clear();
    end;
  end;
end;

{ TOptions }

class function TOptions.New(const AdditionalOptions: TCreateOptions): TOptions;
begin
  Result := Default (TOptions);
  Result.TimeOut := cDefaultTimeOut;
  Result.Options :=
    [oGZIP, oKeepAlive, oHandleRedirects, oCookieManager] +
    AdditionalOptions;
end;

procedure TOptions.SetTimeOut(const Value: Integer);
begin
  ReadTimeOut := Value;
  ConnectTimeout := Value;
end;

class function TOptions.DefaultSsl(const AdditionalOptions: TCreateOptions): TOptions;
begin
  Result := New(AdditionalOptions);
  Result.Options := Result.Options + [oSSL];
end;

var
  fglSslDllExists: byte = 0;
  {$IFDEF MSWINDOWS}


Function GetBuildInfo(Const Filename: String; Out v1, v2, v3, v4: word): String;
Var
  VerInfoSize: dword;
  VerInfo: Pointer;
  VerValueSize: dword;
  VerValue: PVSFixedFileInfo;
  Dummy: dword;
Begin
  VerInfoSize := GetFileVersionInfoSize(PChar(Filename), Dummy);
  If VerInfoSize = 0 Then
  Begin
    Dummy := GetLastError;

    Result := '';
    Exit;
    // ShowMessage(IntToStr(Dummy));
  End; { if }
  getmem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(Filename), 0, VerInfoSize, VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  With VerValue^ Do
  Begin
    v1 := dwFileVersionMS Shr 16;
    v2 := dwFileVersionMS And $FFFF;
    v3 := dwFileVersionLS Shr 16;
    v4 := dwFileVersionLS And $FFFF;
  End;
  freemem(VerInfo, VerInfoSize);
  Result := IntToStr(v1) + '.' + IntToStr(v2) + '.' + IntToStr(v3) + '.' +
    IntToStr(v4);
End;

function SslDllsPresent: boolean;

  function TestDll(const aName: string; const MinVersionReq: array of word): boolean;
  var
    h: THandle;
    len: Integer;
    fn: string;
    v: array [0 .. 3] of word;
  begin
    Result := False;
    h := LoadLibrary(PChar(aName));
    if h <> 0 then
    begin
      SetLength(fn, MAX_PATH);
      len := GetModuleFileName(h, PChar(fn), MAX_PATH);
      if len < Length(fn) then
        SetLength(fn, len)
      else
      begin
        if len > Length(fn) then
          SetLength(fn, len + 1);
        len := GetModuleFileName(h, PChar(fn), len);
        SetLength(fn, len);
      end;

      FreeLibrary(h);

      if FileExists(fn) then
      begin
        try
          maxLogic.ioUtils.GetBuildInfo(fn, v[0], v[1], v[2], v[3]);

          if v[0] = MinVersionReq[0] then
            if v[1] = MinVersionReq[1] then
            begin
              if v[2] > MinVersionReq[2] then
                Result := true
              else if v[2] = MinVersionReq[2] then
                if v[3] >= MinVersionReq[3] then
                  Result := true;
            end;

        except
          // do nothing, we could not compare the versions here...
        end;
      end;
    end;
  end;

begin
  {$IFNDEF MSWINDOWS}
  needs implementation !
  {$ENDIF}
  // prevent double testing
  if fglSslDllExists = 1 then
    Exit(true);

  Result := TestDll('libeay32.dll', [1, 0, 2, 18]) and
    TestDll('ssleay32.dll', [1, 0, 2, 18]);

  // cache the result
  if Result then
    fglSslDllExists := 1;
end;
{$ENDIF}


function download(const aurl: string; out response: Tbytes; const ProgressProc: TProgressProc = nil;
  // TimeOuts: -1 means use the defailt from the Toptions
  ConnectTimeout: Integer = -1; ReadTimeOut: Integer = -1): boolean;
begin
  Result := download(aurl, nil, response, ProgressProc, ConnectTimeout, ReadTimeOut);
end;

function download(const aurl: string; postParams: TStringList; out response: Tbytes; const ProgressProc: TProgressProc = nil;
  // TimeOuts: -1 means use the defailt from the Toptions
  ConnectTimeout: Integer = -1; ReadTimeOut: Integer = -1): boolean;
var
  ms: TMemoryStream;
begin
  gc2(ms, TMemoryStream.Create);

  Result := downloadStream(aurl, postParams, ms,
    ProgressProc,
    ConnectTimeout, ReadTimeOut);

  if ms.size = 0 then
    response := []
  else
  begin
    SetLength(response, ms.size);
    move(ms.memory^, response[0], ms.size);
  end;
end;

function downloadStream(const aurl: string; postParams: TStringList; response: TStream; const ProgressProc: TProgressProc = nil;
  // TimeOuts: -1 means use the defailt from the Toptions
  ConnectTimeout: Integer = -1; ReadTimeOut: Integer = -1): boolean;
var
  {$IFNDEF UseNetClient}
  http: TIdHttp;
  {$ELSE}
  http: THTTPClient;
  lHttpResult: IHTTPResponse;
  {$ENDIF}
  Options: TOptions;
  lUrl: string;
begin
  lUrl := aurl;

  // create options
  {$IFDEF UseNetClient}
  Options := TOptions.New;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  if SslDllsPresent then
    Options := TOptions.DefaultSsl()
  else
  begin
    Options := TOptions.New;
    if startsText('https://', aurl) then
      Delete(lUrl, 5, 1);
  end;
  {$ELSE}
  if startsText('https://', aurl) then
    Options := TOptions.DefaultSsl()
  else
    Options := TOptions.New;
  {$ENDIF}
  {$ENDIF}
  if ConnectTimeout >= 0 then
    Options.ConnectTimeout := ConnectTimeout;
  if ReadTimeOut >= 0 then
    Options.ReadTimeOut := ReadTimeOut;

  {$IFNDEF UseNetClient}
  gc(http, CreateHttp(Options, ProgressProc));
  {$ELSE}
  gc(http, THTTPClient.Create);
  {$ENDIF}

  try
    {$IFNDEF UseNetClient}

    if postParams = nil then
      http.get(lUrl, response)
    else
      http.post(lUrl, postParams, response);

    {$ELSE}

    // net http client version
    http.ConnectionTimeout := Options.ConnectTimeout;
    http.ResponseTimeout := Options.ReadTimeOut;
    http.SendTimeout := Options.ReadTimeOut;
    http.HandleRedirects := True;
    Http.AutomaticDecompression:= [THTTPCompressionMethod.Any];

    if postParams = nil then
      lHttpResult := http.get(lUrl, response)
    else
      lHttpResult := http.post(lUrl, postParams, response);



    {$ENDIF}

    Result := true;
  except
    on e: Exception do
    begin
      if e is EIdOSSLUnderlyingCryptoError then
        Result := downloadStream(StringReplace(lUrl, 'https://', 'http://', [rfIgnoreCase]), postParams, response)
      else
        Result := False
    end;
  end;
end;

function download(const aurl: string; postParams: TStringList; out response: string; const ProgressProc: TProgressProc = nil;
  // TimeOuts: -1 means use the defailt from the Toptions
  ConnectTimeout: Integer = -1; ReadTimeOut: Integer = -1): boolean;
var
  buffer: Tbytes;
  size: Integer;
  lEncoding: TEncoding;
begin
  Result := download(aurl, postParams, buffer, ProgressProc, ConnectTimeout, ReadTimeOut);
  if Result then
  begin

    lEncoding := nil;
    size := TEncoding.GetBufferEncoding(buffer, lEncoding);
    response := lEncoding.GetString(buffer, size, Length(buffer) - size);
  end;
end;

function TEventToProcForIdHttp.SSLOnVerifyPeer(Certificate: TIdX509;
  AOk:
    boolean;
  ADepth, AError: Integer): boolean;
begin
  Result := AOk;
  // Result := TIdX509Access(Certificate).ValidateHostname(Certificate,
  // (self.owner as TIdHttp).URL.Host) = hvrMatchFound;
end;

end.
