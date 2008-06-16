{ Alexander N Zubakov  All Rights Reserved

  Modified March 2008 by Lars Olson. Aservia (web server).
  Based on nYume Server }
unit zserver; {$ifdef fpc}{$mode delphi}{$H+}{$endif}{$UNITPATH ../../main/}{$R+}
interface
uses
  pwtypes, sockets;
  
const packet_size = 56000;
      MAX_CONNECTIONS = 300;  
type
  TzServer = class(TObject)
  private
    sAddr     : TINetSockAddr;
    mainSocket: int32;
    conn  : array of int32;
    ip    : array of astr;
    ccount: int32;
    csock : int32;
    function tryBind: boo;
  public
    constructor create;
    function initConnection(const ip: astr; port: word): boo;
    function  connect: int32;
    procedure disconnect(socket_index: int32);
    procedure stop;
    procedure sWrite(s: astr);
    function  sRead: astr;
    procedure select(socket_index: int32);
    function getIp(socket_index: int32): astr;
  end;

implementation
uses
  {$ifdef unix}baseunix, unix,{$endif}
  {$ifdef windows}windows,{$endif}
  pwstrutil;

function TzServer.TryBind: boo;
begin
  result:= bind(MainSocket, saddr, SizeOf(saddr));
end;

constructor TzServer.Create;
begin
  inherited create;
  ccount := 0;
end;

{ User must call this after constructing, to bind IP and Port. 
  Returns false if problem }
function TzServer.initConnection(const ip: astr; port: word): boo;
begin
  result:= false;
  mainSocket := socket(AF_INET, SOCK_STREAM, 0);
  sAddr.family := AF_INET;
  sAddr.port   := htons(port);
  sAddr.addr   := longWord(StrToNetAddr(ip));
  if tryBind then result:= listen(mainSocket, MAX_CONNECTIONS); 
end;

function TzServer.connect;
var sock     : int32;
    sAddrSize: int32;
begin
  sAddrSize := sizeOf(sAddr);
  sock:= accept(mainSocket, sAddr, sAddrSize);
  if sock <> -1 then begin
    inc(ccount); setLength(conn, ccount); setLength(ip, ccount);
    ip[ccount-1] := netAddrToStr(in_addr(sAddr.addr));
    conn[ccount-1] := sock; 
    csock:= sock;
  end;
  result:= ccount - 1;
end;

procedure TzServer.disconnect;
begin if socket_index < ccount then closeSocket(conn[socket_index]);
end;
  
procedure TzServer.stop;
var i: cardinal;
begin
  if ccount > 0 then for i:= 0 to ccount-1 do closeSocket(conn[i]);
  shutdown(mainSocket, 2);
  closeSocket(mainSocket);
end;

procedure TzServer.select;
begin if socket_index < ccount then csock := conn[socket_index];
end;

function TzServer.getIp;
begin
  if socket_index < ccount then result := ip[socket_index] else result := '';
end;

procedure TzServer.sWrite;
var i, len: int32;
begin
  len:= length(s);
  if len < packet_size then i:= len else i:= packet_size;
  while len > 0 do begin
    send(csock, pointer(s)^, i, 0);
    delete(s, 1, i);
    len:= len - packet_size;
    if len < packet_size then i:= len;
  end;
end;
  
function TzServer.sRead;
var buf  : astr;
    count: int32;
begin
  setLength(buf, packet_size);
  count:= recv(csock, pointer(buf)^, packet_size, 0);
  setLength(buf, count);
  result:= buf;
end;  

end.
