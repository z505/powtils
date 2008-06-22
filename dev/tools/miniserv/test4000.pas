{ miniature http server }

{$mode objfpc}{$H+}

uses baseunix, unix, sockets, sysutils;

var sock1: longint;
    saddr: TInetSockAddr;

const SERVER_IP = '192.168.0.40';

{ convert ip address to integer }
function s2addr(s : string) : LongInt;
var  r, p: cardinal; c, i: LongInt; t : String;
begin
  r := 0;
  for i := 0 to 3 do begin
    p := pos('.', s);
    if p = 0 then p := length(s) + 1;
    if p <= 1 then exit;
    t := copy(s, 1, p - 1);
    delete(s, 1, p);
    val(t, p, c);
    if (c <> 0) or (p < 0) or (p > 255) then exit;
    r := r or p shl (i * 8);
  end;
  result:= r;
end;

procedure errln(s: string);
begin
  writeln('ERROR: ', s);
end;

function checkError: boolean;
begin
//  writeln('Checking error: ');
  result:= false;
  if socketError <> 0 then begin
    writeln('SOCKET ERROR: ', socketerror);
    case socketerror of
      EsockEACCESS:  errln('ACCESS');
      EsockEBADF:    errln('BAD F');
      EsockEFAULT:   errln('FAULT');
      EsockEINTR:    errln('IN TR');
      EsockEINVAL:   errln('IN VAL');
      EsockEMFILE:   errln('EM FILE');
      EsockEMSGSIZE: errln('E MSG SIZE');
      EsockENOBUFS:  errln('E NO BUFS');
      EsockENOTCONN: errln('E NOT CONN');
      EsockENOTSOCK: errln('E NOT SOCK');
      EsockEPROTONOSUPPORT: errln('E NOSUPPORT');
      EsockEWOULDBLOCK:     errln('E WOULD BLOCK');
    end;
    result:= true;
  end;
end;

procedure endSock;
begin
  if sock1 > 0 then begin
    shutdown(sock1, 2);
    closesocket(sock1);
  end;
end;

procedure checkErrorHalt;
begin if checkError then HALT;
end;

procedure checkErrorCleanHalt;
begin if checkError then begin endSock; Halt; end;
end;

procedure dashes; 
begin writeln('-----------------------------------------------------------------');
end;

procedure note(s: string);
begin

end;


procedure sendpage(a: longint);
var err: longint;

  procedure sendStr(s: ansistring);
  var p: pointer;
  begin
    p:= pointer(s);
    err:= send(a, p^, length(s), 0);
    if err < 1 then writeln('send() had a problem');
  end;
  
  const pagedata = 'test'#13#10;

begin
  note('Sending response');
  sendStr('HTTP/1.0 200 OK'#13#10);
//  sendStr('Date: ' {+ RFC822DateTime(Now)} + #13#10);
  sendStr('Server: Powtils Miniserv'#13#10);
  sendStr('Content-Length: '+inttostr(length(pagedata))+#13#10);
  sendStr('Connection: close'#13#10);
  sendStr('Content-Type: text/html'#13#10);
  sendStr(''#13#10);
  sendStr(pagedata);
  if socketError <> 0 then writeln('Page() socket error.');
  note('Shutting down send');
  shutdown(a, 1);
end;

function recvInfo(a: longint): boolean;
var buf: array [0..1023] of char;
    got: longint = 0;
    done: boolean = false;
    tmp: ansistring = '';
begin
  result:= false;
  while not done do begin
    fillchar(buf, length(buf), 1);
    got:= recv(a, buf, sizeof(buf), 0);
    if got > 0 then begin
      tmp:= tmp+buf;
      setlength(tmp, length(tmp)+got);
      if pos('connection: close', lowercase(tmp)) > 0  then got:= 0;
      if pos('connection: keep', lowercase(tmp)) > 0  then got:= 0;
      note('RECV: '+buf);
      result:= true;
    end;
    if got < 1 then begin
      if got = -1 then errln('GOT -1');
      done:= true;
      note('Shutting down Recv');
      shutdown(a, 0);
      break;
    end;
  end;
end;

procedure serv; var i: longint;
var asock, sz, err1: longint;
    a: TInetSockAddr;
    recvd: boolean = false;
begin
//  writeln('test');
  sz:= sizeof(a); 
  asock:= accept(sock1, a, sz);
  checkError;
  if (asock < 1) then writeln('DEBUG: ASOCK: ', asock);
  note('Accept...');
  recvd:= recvInfo(asock);
  if recvd then sendPage(asock);
  if asock > 0 then begin
    //err1:= shutdown(asock, 2);
    checkError;
    err1:= closeSocket(asock);
    checkError;
    if err1 < 0 then
      writeln('DEBUG: ERROR 1: ', err1);
  end;
end;

type tproc = procedure;

procedure loop(proc: tproc); 
var done: boolean = false;
begin 
  while not done do begin
    proc;
    //case i of 
    //  50,100,500,1000,1500,2000,2500: writeln('COUNTER: ', i); 
    //end;
    //sleep(2)
  end;
end;

procedure startSock;
var one: longint = 1;
begin
  sock1:= socket(AF_INET,SOCK_STREAM,0);
  checkErrorHalt;
  writeln('SOCK1: ', sock1);
  saddr.sin_family:=AF_INET;
  saddr.sin_port:=htons(80);
  saddr.sin_addr.s_addr:= s2addr(SERVER_IP);
  writeln('BIND: ', 
    bind(sock1, saddr, sizeof(saddr)) 
  );
  checkErrorCleanHalt;

//  setSocketOptions(sock1, SOL_SOCKET, SO_REUSEADDR,  one, sizeof(one));
  checkErrorCleanHalt;

  writeln('LISTEN: ', 
    listen(sock1, 1280)
  );
  checkErrorCleanHalt;
end;

begin
  writeln('Miniserv 0.0.1');  
  dashes;
  startSock;
  loop(@serv);        
  endSock;
  dashes;
end.