{ request process for miniature http server }

{$mode objfpc}{$H+} {$unitpath ../../main}

uses baseunix, unix, sockets, utils5000, pwstrutil, pwtypes;

const BUF_SIZE = 1024;

var 
  headers: astr = 
    'HTTP/1.0 200 OK'#13#10+
    'Server: Miniserv'#13#10+
    'Content-Length: 6'#13#10+
    'Connection: close'#13#10+
    'Content-Type: text/html'#13#10+
     #13#10;
  content: astr = 'test'#13#10;
  asock: TSock; // accepted socket handle from main process

procedure SendPage;
begin
  Note('Sending response');
  Send(asock, headers);
  //Send('Date: ' {+ RFC822DateTime(Now)} + #13#10);
  Send(asock, content);
  Note('Shutting down send');
  ShutdownSend(asock);
  //Sleep(10);
end;

function RecvInfo: boo;
var buf: array [0..BUF_SIZE-1] of char;
    got: int32 = 0;
    done: boo = false;
    tmp: astr = '';
begin
  result:= false;
  while not done do begin
    Fillchar(buf, length(buf), 1);
    got:= Recv(asock, buf);
    if got > 0 then begin
      tmp:= tmp+buf;
      Setlength(tmp, Length(tmp)+got);
      if pos('connection: close', Lowercase(tmp)) > 0  then got:= 0;
      if pos('connection: keep', Lowercase(tmp)) > 0  then got:= 0;
      Note('RECV: '+buf);
      result:= true;
    end;
    if got < 1 then begin
      if got = -1 then begin
        CheckErr('broken recv: client may have disconnected');
        result:= false;
      end;
      Note('Shutting down Recv');
      ShutdownRecv(asock);

      done:= true;
    end;
  end;
  // Sleep(10);
end;

procedure ProcessRequest;
var recvd: boo = false;
    err1: int32;
begin
  Note('Processing request...');  
  recvd:= RecvInfo;
  if recvd then SendPage;
  if asock > 0 then begin
    // err1:= ShutdownSock(asock);
    // CheckErr;
    err1:= CloseSocket(asock);
    CheckErr;
    if err1 < 0 then writeln('DEBUG: ERROR 1: ', err1);
  end;
end;


begin
  if paramcount < 1 then exit;
  asock:= s2i(paramstr(1));
  if asock < 1 then exit;
  ProcessRequest;
end.