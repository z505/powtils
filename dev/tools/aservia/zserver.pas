{ Modified March 2008 by Lars Olson. Aservia (web server).
  Based on nYume Server 

+-----------------------------------------------+------------+
 Модуль zserver 1.1                            | 17.02.2006 
+-----------------------------------------------+------------+
 Описание:                                                  
   В модуле содержится класс для создания сервера на основе 
   TCP соединений                                           
                                                            
 Использование:                                             
   Модуль предназначен для коммерческого и некоммерческого  
   использования.                                           
   Вы имеете право использовать и/или модифицировать модуль 
   по своему усмотрению. Вы не имеете права распространять  
   этот модуль в измененном виде.                           
                                                            
 Автор:                                      
   Alexander N Zubakov                       
                                             
 © 2006 Alexander N Zubakov (original author)
 © 2008 Lars Olson (modifications)
 All Rights Reserved                                      
+------------------------------------------------------------+
}
unit zserver;
{$mode Delphi}
{$SMARTLINK ON}
{$LONGSTRINGS ON}
interface
uses
  Sockets;
  
const
  packet_size = 56000;
  
type
  TzServer = class(TObject)
  private
    sAddr     : TINetSockAddr;
    MainSocket: longint;
    conn  : array of longint;
    ip    : array of string;
    ccount: longint;
    csock : longint;
    procedure TryBind(const ip, port: string);
  public
    constructor Create(ip: string; port: word);
    function  Connect: longint;
    procedure Disconnect(socket_index: longint);
    procedure Stop;
    procedure sWrite(str: string);
    function  sRead: string;
    procedure select(socket_index: longint);
    function getip(socket_index: longint): string;
  end;

implementation
uses
  {$ifdef unix}baseunix, unix,{$endif}
  {$ifdef windows}windows,{$endif}
  pwstrutil, pwtypes;

procedure ErrHalt(const msg: string);
begin
  writeln(msg);
  halt;
end;

procedure TzServer.TryBind(const ip, port: string);
begin
  if not Bind(MainSocket, saddr, SizeOf(saddr)) then 
  begin
    // Note: his halt is currently called in the constructor, not best design. 
    // Todo: move bind() to a function that is not called by the constructor
    //       OOP bites again. Hard to clean up memory when halt is encapsulated.
    ErrHalt('Can''t connect to address or port.'+ LF+
            'The ip:port you are using is '+ip+':'+port+ LF+
           ' Tip: make sure another server is not running.'+ LF+
           ' Error # '+ {$ifdef windows}inttostr(GetLastError){$endif}
                        {$ifdef unix}inttostr(fpGetErrNo){$endif}
    ); 
    // Ugly inline ifdef is above due to FPC bug, cannot wrap in another 
    // function with fpc 2.2.0. 
    // See http://bugs.freepascal.org/view.php?id=10205
  end;
end;

constructor TzServer.Create;
begin
  inherited Create;
  ccount := 0;
  MainSocket := Socket(AF_INET, SOCK_STREAM, 0);
  saddr.Family := AF_INET;
  saddr.Port   := htons(port);
  saddr.Addr   := LongWord(StrToNetAddr(ip));
  TryBind(ip, inttostr(port));
  Listen(MainSocket, 1);
end;
  
function TzServer.Connect;
var sock     : longint;
    sAddrSize: longint;
begin
  sAddrSize := SizeOf(sAddr);
  sock := Accept(MainSocket, sAddr, sAddrSize);
  if sock <> -1 then begin
    inc(ccount); setLength(conn, ccount); setLength(ip, ccount);
    ip[ccount - 1] := NetAddrToStr(in_addr(sAddr.Addr));
    conn[ccount - 1] := sock; 
    csock := sock;
  end;
  Result := ccount - 1;
end;

procedure TzServer.Disconnect;
begin
  if socket_index < ccount then CloseSocket(conn[socket_index]);
end;
  
procedure TzServer.Stop;
var i: cardinal;
begin
  if ccount > 0 then for i := 0 to ccount - 1 do CloseSocket(conn[i]);
  Shutdown(MainSocket, 2);
  CloseSocket(MainSocket);
end;

procedure TzServer.select;
begin
  if socket_index < ccount then csock := conn[socket_index];
end;

function TzServer.getip;
begin
  if socket_index < ccount then Result := ip[socket_index] else Result := '';
end;

procedure TzServer.sWrite;
var i: cardinal;
    s: integer;
begin
  s := Length(str);
  if s < packet_size then i := s else i := packet_size;
  while s > 0 do begin
    Send(csock, pointer(str)^, i, 0);
    delete(str, 1, i);
    s := s - packet_size;
    if s < packet_size then i := s;
  end;
end;
  
function TzServer.sRead;
var
  buf  : string;
  count: integer;
begin
  setLength(buf, packet_size);
  count := Recv(csock, pointer(buf)^, packet_size, 0);
  setLength(buf, count);
  Result := buf;
end;  

end.
