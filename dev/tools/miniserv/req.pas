{ request process for miniature http server }

{$mode objfpc}{$H+} {$unitpath ../../main}

uses baseunix, unix, sockets, utils, pwstrutil;

var sock1: longint;
    saddr: TInetSockAddr;

procedure process(asock: longint);
  
  procedure sendPage;
  var err: longint;

    procedure sendStr(p: pointer; len: longint);
    begin
      err:= send(asock, p^, len, 0);
      if err < 1 then writeln('send() had a problem');
      checkError('sendStr()');
    end;
    
    var 
    headers: pchar = 
     'HTTP/1.0 200 OK'#13#10+
     'Server: Miniserv'#13#10+
     'Content-Length: 6'#13#10+
     'Connection: close'#13#10+
     'Content-Type: text/html'#13#10+
     ''#13#10;

    pagedata: pchar = 'test'#13#10;

  begin
    note('Sending response');
    sendStr(headers, length(headers));
  //  sendStr('Date: ' {+ RFC822DateTime(Now)} + #13#10);
    sendStr(pagedata, length(pagedata));
    checkError('Page() socket error.');
    note('Shutting down send');
    shutdown(asock, 1);
  //  sleep(10);
  end;
  
  function recvInfo: boolean;
  var buf: array [0..1023] of char;
      got: longint = 0;
      done: boolean = false;
      tmp: ansistring = '';
  begin
    result:= false;
    while not done do begin
      fillchar(buf, length(buf), 1);
      got:= recv(asock, buf, sizeof(buf), 0);
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
        shutdown(asock, 0);
        break;
      end;
    end;
  //  sleep(10);
  end;
  
  var  recvd: boolean = false;
       err1: longint;
begin
  recvd:= recvInfo;
  if recvd then sendPage;
  if asock > 0 then begin
    //err1:= shutdown(asock, 2);
    checkError;
    err1:= closeSocket(asock);
    checkError;
    if err1 < 0 then writeln('DEBUG: ERROR 1: ', err1);
  end;
end;

var param1: longint;

begin
  if paramcount < 1 then exit;
  param1:= strtoint(paramstr(1));
  if param1 < 1 then exit;
  note('Request...');  
  process(param1);
end.