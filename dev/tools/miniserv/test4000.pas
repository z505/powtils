{ miniature http server }

{$mode objfpc}{$H+} {$unitpath ../../main}

uses baseunix, unix, sockets, utils, pwstrutil;

var sock1: longint;
    saddr: TInetSockAddr;

const SERVER_IP = '192.168.0.40';

procedure endSock;
begin
  if sock1 > 0 then begin
    shutdown(sock1, 2);
    closesocket(sock1);
  end;
end;

procedure checkErrorCleanHalt;
begin if checkError then begin endSock; Halt; end;
end;

procedure process(asock: longint);
var f: text;
    cmd: string;
begin
  cmd:= './req '+ i2s(asock) + ' &';
  popen(f, cmd,'R');
  if fpgeterrno <> 0 then writeln ('Error from POpen:: ', fpgeterrno);
  pclose(f);
end;

procedure serv; 
var asock, sz: longint;
    a: TInetSockAddr;
begin
//  writeln('test');
  sz:= sizeof(a); 
  asock:= accept(sock1, a, sz);
  checkError;
  if (asock < 1) then writeln('DEBUG: ASOCK: ', asock);
  note('Accept...');
  process(asock);
  closesocket(asock);
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

  writeln('SETSOCKETOPTIONS: ', 
    setSocketOptions(sock1, SOL_SOCKET, SO_REUSEADDR,  one, sizeof(one))
  );
  checkErrorCleanHalt;

  writeln('LISTEN: ', 
    listen(sock1, 3580)
  );
  checkErrorCleanHalt;
end;

begin
  dashes;
  writeln('Miniserv 0.0.1');  
  startSock;
  loop(@serv);        
  endSock;
  dashes;
end.