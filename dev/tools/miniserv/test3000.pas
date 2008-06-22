{ This will test "the fucking 1000" problem of LWS2 on unix }

{$mode objfpc}{$H+}

uses baseunix, unix, sockets, sysutils;

var sock1: longint;
    saddr: TInetSockAddr;
const
//  SERVER_IP = ((1 shl 24) or 127)
  SERVER_IP = '192.168.0.40';

function StrToAddr(s : string) : LongInt;
var
   r, p: cardinal; c, i: LongInt; t : String;
begin
  r := 0;
  for i := 0 to 3 do
  begin
    p := Pos('.', s);
    if p = 0 then p := Length(s) + 1;
    if p <= 1 then exit;
    t := Copy(s, 1, p - 1);
    Delete(s, 1, p);
    Val(t, p, c);
    if (c <> 0) or (p < 0) or (p > 255) then exit;
    r := r or p shl (i * 8);
  end;
  StrToAddr := r;
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
begin 
  writeln('-----------------------------------------------------------------');
end;

procedure fuck1000_1; 
var err: longint = 0; 
begin
  err:= fpSystem('df &');
  writeln('DEBUG: ERROR RETURN: ', err);
end;


procedure sendpage(a: longint);
var err: longint;

  procedure sendStr(s: ansistring);
  begin
    err:= send(a, pointer(s)^, length(s), 0);
    if err < 1 then writeln('send() had a problem');
  end;
  
  const pagedata = 'test'#13#10;

begin
  writeln('Sending response.');
  sendStr('HTTP/1.0 200 OK'#13#10);
//  sendStr('Date: ' {+ RFC822DateTime(Now)} + #13#10);
  sendStr('Server: PWU LightWebServer'#13#10);
  sendStr('Content-Type: text/html'#13#10);
  sendStr('Content-Length: '+inttostr(length(pagedata))+#13#10);
  sendStr('Connection: close'#13#10);
  sendStr(''#13#10);
  sendStr(pagedata);
  if socketError <> 0 then writeln('Page() socket error.');
  writeln('Shutting down send');
  shutdown(a, 1);

end;


procedure recvinfo(a: longint);
var buf: array [0..1023] of char;
    got: longint = 0;
    done: boolean = false;
    tmp: ansistring = '';
begin
  while not done do begin
    buf:= '';
    got:= recv(a, buf, sizeof(buf), 0);
    tmp:= tmp+buf;
    if pos('connection: close', lowercase(buf)) > 0  then got:= 0;
    if pos('connection: keep', lowercase(buf)) > 0  then got:= 0;
    if got < 1 then begin
      if got = -1 then errln('GOT -1');
      done:= true;
      writeln('Shutting down recv');
      shutdown(a, 0);
      break;
    end else
      write('RECV: ', buf);
  end;
end;

procedure fuck1000_2; var i: longint;
var asock, sz, err1, err2: longint;
    a: TInetSockAddr;
begin
//  writeln('test');
  sz:= sizeof(a); 
  asock:= accept(sock1, a, sz);
  checkError;
  Writeln('Accept...');
  recvinfo(asock);
  sendpage(asock);
  if (asock < 1) then writeln('DEBUG: ASOCK: ', asock);
  if asock > 0 then begin
    err1:= shutdown(asock, 2);
    err2:= closesocket(asock);
    if (err2 < 0) or (err1 < 0) then
      writeln('DEBUG: ERROR 1: ', err1, ' ERROR 2: ', err2);
  end;
//  sleep(3)
end;

procedure fuck1000_3;
begin
end;

procedure fuck1000_4;
begin
end;

type tproc = procedure;

procedure loop3000(proc: tproc); var i: longint; cmd: string = '';
begin 
  for i:= 1 to high(longint) do begin
    proc;
    case i of 50,100,500,1000,1500,2000,2500: writeln('COUNTER: ', i); end;
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
  saddr.sin_addr.s_addr:= StrToAddr(SERVER_IP);
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
//  writeln('FUCK 1000 TEST 1'); dashes;
//  loop3000(@Fuck1000_1);        dashes;
  writeln('FUCK 1000 TEST 2');  dashes;
  startSock;
  loop3000(@Fuck1000_2);        dashes;
  endSock;

//  writeln('FUCK 1000 TEST 3');  dashes;
//  loop3000(@Fuck1000_3);        dashes;
//  writeln('FUCK 1000 TEST 4');  dashes;
//  loop3000(@Fuck1000_4);        dashes;
end.