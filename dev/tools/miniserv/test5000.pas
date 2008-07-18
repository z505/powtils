{ miniature http server }

{$mode objfpc}{$H+} {$unitpath ../../main}

uses baseunix, unix, sockets, utils5000, pwstrutil, crt, pwtypes;

var gMainSock: TSock;
    saddr: TInetSockAddr;

const SERVER_IP = '192.168.0.40';
      SERVER_PORT = 80;

procedure CleanDie; 
begin 
  EndSock(gMainSock); Dashes; HALT;
end;

procedure CleanDieIfErr;
begin 
  if CheckErr then CleanDie; 
end;

procedure Process(AcceptSock: TSock);
var cmd: string; proc, closed: int32;
    // f: text;
begin
  cmd:= './req5000 '+ i2s(AcceptSock) + ' &';
  // procres:=  popen(f, cmd,'R');
  proc:= Shell(cmd);
  Note('PROCESS RESULT: '+ i2s(proc));
  if proc = 127 then Writeln('Error from shell: ', proc);
  if FpGetErrno <> 0 then Writeln('Error from POpen:: ', FpGetErrno);
  // closed:= pclose(f);
  // note('CLOSED PROCESS RESULT: '+ i2s(closed));
end;

procedure Serve; 
var asock, sz: int32;
    a: TInetSockAddr;
begin
//  writeln('test');
  sz:= sizeof(a); 
  asock:= accept(gMainSock, a, sz);
  CheckErr;
  if (asock < 1) then Writeln('DEBUG: ASOCK: ', asock);
  Note('Accept...');
  Process(asock);
  CloseSocket(asock);
end;

procedure Loop; 
var done: boo = false;
    ch: char; 
begin 
  while not done do begin
    if KeyPressed then begin 
      Writeln; Writeln('## HIT ENTER ##'); 
      Readln(ch); Writeln('## ENTER PRESSED ##'); 
    end;
    if ch = 'q' then CleanDie;
    Serve;
  end;
end;

procedure StartSock;
begin
  gMainSock:= Socket(AF_INET,SOCK_STREAM,0);
  DieIfErr;
  Writeln('SOCK1: ', gMainSock);
  SetAddr(saddr, SERVER_IP, SERVER_PORT);
  Writeln('BIND: ', Bind(gMainSock, saddr));
  CleanDieIfErr;
  Writeln('SET REUSE: ', SetReuseOpt(gMainSock));
  CleanDieIfErr;
  Writeln('LISTEN: ', Listen(gMainSock, 3580));
  CleanDieIfErr;
end;

begin
  Writeln('Starting....Miniserv 0.0.1...');  
  StartSock;
  Loop;        
  CleanDie;
end.