unit PingWorker;

interface

uses
  windows,
  iphlpapi,
  IpExport,
  Winapi.WinSock,
  classes, sysUtils,
  IdBaseComponent, IdComponent, IdRawBase, IdRawClient, IdIcmpClient;

type
  T4Bytes = packed record
    s_b1, s_b2, s_b3, s_b4: byte;
  end;

  T2Words = packed record
    s_w1, s_w2: word;
  end;

  TIPAddress = record
    case integer of
      0:
        (asBytes: T4Bytes);
      1:
        (asWords: T2Words);
      2:
        (asDWord: longword);
  end;

  tPingResult = record
    Ip: TIPAddress;
    NumResponses: dword;
    ReceivedFrom: string;
    status: dword;
    RoundTripTime: dword; // in milliseconds
  end;

  TPingWorker = class
  private
    fTimeOut: dword;
  public
    constructor Create;
    destructor destroy; override;
    // requires admin rights, so it is not the best option
    function PingWithIndy: boolean;

    function ping(const IpAddress: TIPAddress; out PingResult: tPingResult): boolean; overload;
    function ping(const aHost: Ansistring; out PingResult: tPingResult): boolean; overload;
    function ping(const Ip: dword; out PingResult: tPingResult): boolean; overload;

    function ping(const aHost: Ansistring): boolean; overload;
    function ping(const Ip: TIPAddress): boolean; overload;
    function ping(const Ip: dword): boolean; overload;

    property TimeOut: dword read fTimeOut write fTimeOut;
  end;

function IcmpCreateFile: THandle; stdcall; external 'icmp.dll';
function IcmpCloseHandle(icmpHandle: THandle): boolean; stdcall; external 'icmp.dll'
  function IcmpSendEcho
  (icmpHandle: THandle;
  DestinationAddress: TIPAddress;
  RequestData: Pointer; RequestSize: Smallint;
  RequestOptions: Pointer;
  ReplyBuffer: Pointer;
  ReplySize: dword;
  TimeOut: dword): dword; stdcall; external 'icmp.dll';

implementation

{ TPingWorker }

function TPingWorker.PingWithIndy: boolean;
var
  IdIcmpClient1: TIdIcmpClient;
begin
  result := false;

  IdIcmpClient1 := TIdIcmpClient.Create(nil);
  try
    IdIcmpClient1.Host := '8.8.8.8';
    IdIcmpClient1.ping();
    result := IdIcmpClient1.ReplyStatus.ReplyStatusType = rsEcho;
  finally
    IdIcmpClient1.free;
  end;
end;

constructor TPingWorker.Create;
begin
  inherited;

  fTimeOut := 1000;
end;

destructor TPingWorker.destroy;
begin

  inherited;
end;

function TPingWorker.ping(const Ip: dword; out PingResult: tPingResult): boolean;
var
  IpAddress: TIPAddress;
begin
  IpAddress.asDWord := Ip;
  result := ping(IpAddress, PingResult);
end;

function TPingWorker.ping(const aHost: Ansistring): boolean;
var
  PingResult: tPingResult;
begin
  result := ping(aHost, PingResult);
end;

function TPingWorker.ping(const aHost: Ansistring; out PingResult: tPingResult): boolean;
var
  IpAddress: TIPAddress;
begin
  IpAddress.asDWord := inet_addr(pAnsiChar(aHost));
  result := ping(IpAddress, PingResult);
end;

function TPingWorker.ping(const IpAddress: TIPAddress; out PingResult: tPingResult): boolean;
// Something like this crude translation from:
// https://msdn.microsoft.com/en-us/library/windows/desktop/aa366050(v=vs.85).aspx
// Should do the trick.

var
  ICMPFile: THandle;
  SendData: array [0 .. 31] of AnsiChar;
  ReplyBuffer: PICMP_ECHO_REPLY;
  ReplySize: dword;
begin
  result := false;
  PingResult := default (tPingResult);

  PingResult.Ip := IpAddress;

  SendData := 'Data Buffer';

  ICMPFile := IcmpCreateFile;
  if ICMPFile <> INVALID_HANDLE_VALUE then
    try
      ReplySize := SizeOf(ICMP_ECHO_REPLY) + SizeOf(SendData);
      GetMem(ReplyBuffer, ReplySize);
      try
        PingResult.NumResponses := IcmpSendEcho(ICMPFile, IpAddress, @SendData, SizeOf(SendData),
          nil, ReplyBuffer, ReplySize, fTimeOut);
        if (PingResult.NumResponses <> 0) then
        begin
          result := true;

          PingResult.ReceivedFrom := inet_ntoa(in_addr(ReplyBuffer.Address));
          // PAnsiChar(ReplyBuffer.Data)
          PingResult.status := ReplyBuffer.status;
          PingResult.RoundTripTime := ReplyBuffer.RoundTripTime;
        end;

      finally
        FreeMem(ReplyBuffer);
      end;
    finally
      IcmpCloseHandle(ICMPFile);
    end;
end;

function TPingWorker.ping(const Ip: TIPAddress): boolean;
var
  PingResult: tPingResult;
begin
  result := ping(Ip, PingResult);
end;

function TPingWorker.ping(const Ip: dword): boolean;
var
  PingResult: tPingResult;
begin
  result := ping(Ip, PingResult);

end;

end.
