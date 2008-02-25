{ Makes sockets.pp send/recv calls much easier to use 

  Author: Lars (L505)
          http://z505.com }


unit pwsock; {$mode objfpc} {$H+}

interface

uses sockets, pwhostname, sysutils;

const crlf = #13#10;

type TKiloByte = (kb1, kb2, kb3, kb4, kb5, kb10);

//type TSockInfo = TInetSockAddr;

function connectsock(website: string; port: integer; out sAddr: TInetSockAddr): integer;
function connectsock(var sAddr: TInetSockAddr): integer;
function send(const sock: integer; const pc: pchar): integer;
function send(const sock: integer; const s: string): integer;
function sendln(const sock: integer; const s: string): integer;
function sendln(const sock: integer): integer;
function recv(sock: longint; var s: string): boolean;
//procedure recv(const sock: integer; bufsz: TKiloByte; var t: text);
function closesock(sock: longint): boolean;

implementation

{ returns result as out parameter 
  returns -1 as result if problem, or the socket instance otherwise }
function connectsock(website: string; port: integer; out sAddr: TInetSockAddr): integer;
begin
  result:= Socket(AF_INET, SOCK_STREAM, 0);
  if result = -1 then exit;
//  sAddr.sin_family:= AF_INET;
  sAddr:= InetResolve(website, port);
  if not sockets.Connect(result, sAddr, sizeof(sAddr)) then begin
    result:= -1;
    exit; 
    writeln('SOCKET ERROR: ', socketerror);
  end;  
end;

function connectsock(var sAddr: TInetSockAddr): integer;
begin
//  result:= Socket(AF_INET, SOCK_STREAM, 0);

  result:= Socket(AF_INET, SOCK_STREAM, 0);
  sAddr.family:= AF_INET;
  if result = -1 then exit;
  if not sockets.Connect(result, sAddr, sizeof(sAddr)) then begin
    result:= -1;
    exit; 
  end;
end;

function closesock(sock: longint): boolean;
begin
  sockets.closesocket(sock);
end;

function send(const sock: integer; const pc: pchar): integer;
begin
  result:= sockets.send(sock, pc[0], length(pc), 0);  
end;

{ sends a string to socket, returns same as overloaded }
function send(const sock: integer; const s: string): integer;
begin
  result:= pwsock.send(sock, pchar(s));
end;

{ sends a string plus linefeed to socket, returns same as socksend }
function sendln(const sock: integer; const s: string): integer;
begin
  result:= pwsock.send(sock, pchar(s) + crlf);
end;

{ sends only a linefeed to socket, returns same as overloaded }
function sendln(const sock: integer): integer;
begin
  result:= pwsock.send(sock, crlf);
end;

{ receives socket and puts into text file 
  returns false if problem, use SocketError to check further details }
function recv(sock: longint; var s: string): boolean;
const BUFLEN = 2048;   
var Buf : array[0..BUFLEN - 1] of char;
    len, i: longint;
    closed: boolean; 
    saddr: tinetsockaddr;
begin
{
  sock:= socket(AF_INET, sock_STREAM, 0);
  saddr.sin_family:= AF_INET;
  saddr.sin_addr.s_addr:= 0;
  saddr.sin_port:= htons(80);
  writeln('DEBUG connect: ', sockets.bind(sock,SAddr,sizeof(saddr)) );  
  
  writeln('DEBUG LISTEN: ', listen(sock, 1));
  }

//  FillChar(buf,  BUFLEN*SizeOf(char), char(127));
  result:= false;
  closed:= false;
  while not closed do
  begin
    len:= sockets.recv(sock, Buf, sizeof(Buf), 0);
    writeln('SOCKET ERROR: ', socketerror);
    writeln('debug: len', len);
    if len = -1 then exit;
    if Len = 0 then closed := true 
    else 
      for i := 0 to Len - 1 do s:= s + Buf[i];
//      for i := 0 to Len - 1 do Write(t, Buf[i]);
//      for i := 0 to Len - 1 do Write(Buf[i]);
  end;
  result:= true;
end;

{ overloaded with specifiable buffer size }
procedure recv(const sock: integer; bufsz: TKiloByte; var t: text);
const KB = 1024; // KiloByte selection
var buf1k  : array[0..KB - 1] of char;
    buf2k  : array[0..(KB*2) -1] of char;
    buf3k  : array[0..(KB*3) -1] of char;
    buf4k  : array[0..(KB*4) -1] of char;
    buf5k  : array[0..(KB*5) -1] of char;
    buf10k : array[0..(KB*10) -1] of char;
    len, i: longint;
    closed: boolean;
begin
  closed:= false;
  while not closed do
  begin
    case bufsz of
      kb1:
      begin
        len:= sockets.recv(sock, buf1k, sizeof(buf1k), 0);
        if Len = 0 then Closed := True 
        else 
          for i := 0 to Len-1 do Write(t, buf1k[i]);
      end;

      kb2: ;//TODO
      kb3: ;//TODO
      kb4: ;//TODO
      kb5: ;//TODO
      kb10:;//TODO 
   end;

  end;
end;

end.

(*


*)
