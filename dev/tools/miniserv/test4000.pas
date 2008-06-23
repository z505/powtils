{ miniature http server }

{$mode objfpc}{$H+} {$unitpath ../../main}

uses baseunix, unix, sockets, utils, pwstrutil, crt;

var sock1: longint;
    saddr: TInetSockAddr;

const SERVER_IP = '192.168.0.40';

procedure cleanupHalt;
begin endSock(sock1); dashes; HALT;
end;

procedure checkErrorCleanHalt;
begin if checkError then cleanupHalt; 
end;

procedure process(asock: longint);
var f: text; cmd: string; procid, closed: longint;
begin
  cmd:= './req '+ i2s(asock) {+ ' &'};
  // procid:=  popen(f, cmd,'R');
  // note('PROCESS: ', procid);
  shell(cmd);
  if fpgeterrno <> 0 then writeln ('Error from POpen:: ', fpgeterrno);
  // closed:= pclose(f);
  // note('CLOSED PROCESS RESULT: '+ i2s(closed));
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
  closeSocket(asock);
end;

procedure loop; 
var done: boolean = false;
    line: string[10];
begin 
  while not done do begin
    if keypressed then readln(line);
    if line = 'q' then cleanupHalt;
    serv;
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
  loop;        
  cleanupHalt;
end.