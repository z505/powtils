{ Alexander N Zubakov  All Rights Reserved
  Modified March 2008 by Lars Olson. Aservia (web server). Based on nYume 
  License: see aservia license in docs/License.html }  

unit zserver; {$ifdef fpc}{$mode delphi}{$H+}{$endif}{$R+}
interface
uses
  pwtypes, sockets;
  
const packet_size = 56000;
      MAX_CONNECTIONS = 300;  
type
  TzServer = class(TObject)
  private
    sAddr     : TINetSockAddr;
    mainsock: int32;
    conn  : array of int32;
    ip    : array of astr;
    ccount: int32;
    csock : int32;
    function tryBind: boo;
  public
    constructor Create;
    function InitConnection(const ip: astr; port: word): boo;
    function  Connect: int32;
    procedure Disconnect(socket_index: int32);
    procedure Stop;
    procedure sWrite(s: astr);
    function  sRead: astr;
    procedure Select(socket_index: int32);
    function GetIp(socket_index: int32): astr;
  end;

implementation
uses
  {$ifdef unix}baseunix, unix,{$endif}
  {$ifdef windows}windows,{$endif}
  pwstrutil, servutil;


function TzServer.TryBind: boo;
begin
  result:= bind(mainsock, saddr, SizeOf(saddr));
end;

constructor TzServer.Create;
begin
  inherited create;
  ccount := 0;
end;

{ User must call this after constructing, to bind IP and Port. 
  Returns false if problem }
function TzServer.InitConnection(const ip: astr; port: word): boo;
begin
  result:= false;
  mainsock := Socket(AF_INET, SOCK_STREAM, 0);
  sAddr.family := AF_INET;
  sAddr.port   := Htons(port);
  sAddr.addr   := Longword(StrToNetAddr(ip));
  if TryBind then result:= Listen(mainsock, MAX_CONNECTIONS); 
end;

function TzServer.Connect;
var sock     : int32;
    sAddrSize: int32;
begin
  sAddrSize := sizeof(sAddr);
  sock:= Accept(mainsock, sAddr, sAddrSize);
  if sock > 0 then begin
    inc(ccount); Setlength(conn, ccount); Setlength(ip, ccount);
    ip[ccount-1] := NetAddrToStr(in_addr(sAddr.addr));
    conn[ccount-1] := sock; 
    csock:= sock;
  end;
  result:= ccount - 1;
end;

procedure TzServer.Disconnect;
begin 
  if socket_index < ccount then begin
    if conn[socket_index] > 0 then CloseSocket(conn[socket_index]);
  end;
end;
  
procedure TzServer.Stop;
var i: cardinal;
begin
  if ccount > 0 then for i:= 0 to ccount-1 do begin
    if conn[i] > 0 then CloseSocket(conn[i]);
  end;
  Shutdown(mainsock, 2);
  CloseSocket(mainsock);
end;

procedure TzServer.Select;
begin 
  if (socket_index < ccount) and (socket_index >= 0) then 
    csock := conn[socket_index]
  else
    Errln('Socket index out of bounds!');
end;

function TzServer.GetIp;
begin
  if socket_index < ccount then result := ip[socket_index] else result := '';
end;

(* old buggy nYume based code
procedure TzServer.sWrite;
var i, len: int32;
begin
  len:= length(s);
  if len < packet_size then i:= len else i:= packet_size;
  while len > 0 do begin
    send(csock, pchar(s)[0], i, 0);
    delete(s, 1, i);
    len:= len - packet_size;
    if len < packet_size then i:= len;
  end;
end;
*)

procedure TzServer.sWrite;
var i, len: int32;
begin
  len:= length(s);
  if len < packet_size then i:= len else i:= packet_size;
  { write data in packet_size chunks, until string is emptied to zero }
  while len > 0 do begin
    Send(csock, pchar(s)[0], i, 0);
    Delete(s, 1, i);
    len:= Length(s);
    // if smaller than packet size, then send small amount leftover
    if len < packet_size then i:= len;
  end;
end;


(* old buggy nYume based code
function TzServer.sRead;
var buf  : astr;
    count: int32;
begin
  setLength(buf, packet_size);
  count:= recv(csock, pointer(buf)^, packet_size, 0);
  setLength(buf, count);
  result:= buf;
//  uniqueString(result);  //must call uniquestring or memory leak ! 
//  buf:= '';
end;  
*)

function TzServer.sRead;
var buf  : array [0..packet_size-1] of char;
    count: int32;
begin
  Fillchar(buf, sizeof(buf), 0);
  count:= Recv(csock, buf, packet_size, 0);
  result:= buf;
  Setlength(result, count);
end;  



end.
