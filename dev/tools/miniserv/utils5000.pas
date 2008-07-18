unit utils5000; {$mode objfpc} {$h+} {$unitpath ../../main}

interface 
uses sockets, baseunix, unix, pwtypes;

type TSock = sockets.TSocket;

procedure Errln(s: astr);
procedure Dashes; 
procedure Note(s: astr);
procedure EndSock(s: TSock);
procedure SetAddr(var saddr: TInetSockAddr; ip: astr; port: int32);
function SetReuseOpt(s: TSock): int32;
function Bind(s: TSock; saddr: TInetSockAddr): boo;
function Recv(s: TSock; var buf: array of char): int32;
function CreateIp(s: astr): int32;

procedure ShutdownSend(s: TSock);
procedure ShutdownRecv(s: TSock);
procedure ShutdownSock(s: TSock);

procedure DieIfErr; 
function CheckErr(hint: shortstring): boo;
function CheckErr: boo;

procedure Send(sock: TSock; s: shortstring);

{-----------------------------------------------------------------------------}
                              implementation
{-----------------------------------------------------------------------------}


procedure Send(sock: TSock; s: shortstring);
var err: int32; convert: astr;
begin
  convert:= ansistring(s);
  uniquestring(convert);
  err:= sockets.Send(sock, pointer(pchar(convert))^, length(s), 0);
  if err < 1 then CheckErr('broken send: client may have disconnected');
end;

function Recv(s: TSock; var buf: array of char): int32;
begin
  result:= sockets.Recv(s, buf, sizeof(buf), 0);
end;

procedure SetAddr(var saddr: TInetSockAddr; ip: astr; port: int32);
begin
  saddr.sin_family:= AF_INET;
  saddr.sin_port:= Htons(port);
  saddr.sin_addr.s_addr:= CreateIp(ip);
end;

function Bind(s: TSock; saddr: TInetSockAddr): boo;
begin
  result:= sockets.Bind(s, saddr, sizeof(saddr));
end;

function SetReuseOpt(s: TSock): int32;
var one: int32 = 1;
begin
  result:= SetSocketOptions(s, SOL_SOCKET, SO_REUSEADDR, one, sizeof(one));
end;

{ convert ip address to integer }
function CreateIp(s: astr): int32;
var  r, p: cardinal; c, i: int32; t : astr;
begin
  r:= 0;
  for i:= 0 to 3 do begin
    p:= pos('.', s);
    if p = 0 then p:= length(s) + 1;
    if p <= 1 then exit;
    t:= copy(s, 1, p - 1);
    delete(s, 1, p);
    val(t, p, c);
    if (c <> 0) or (p < 0) or (p > 255) then exit;
    r:= r or p shl (i * 8);
  end;
  result:= r;
end;

procedure ShutdownSend(s: TSock);
begin
  sockets.Shutdown(s, 1);
end;

procedure ShutdownRecv(s: TSock);
begin
  sockets.Shutdown(s, 0);
end;

procedure ShutdownSock(s: TSock);
begin
  sockets.Shutdown(s, 2);
end;


procedure Errln(s: astr); 
begin 
  writeln('ERROR: ', s); 
end;

procedure Dashes; 
begin 
  writeln('-----------------------------------------------------------------'); 
end;

procedure Note(s: astr); 
begin

end;

function CheckErr(hint: shortstring): boo;
var hintIfAny: astr = '';
begin
  Note('Checking error: ');
  // compose nice extra hint only if it was given in parameter
  if hint <> '' then hintIfAny:= ' Hint: '+ hint + LineEnding;
  result:= false;
  if SocketError <> 0 then begin
    writeln('SOCKET ERROR: ', SocketError);
    write(hintIfAny);
    case SocketError of
      EsockEACCESS:  errln('EAccess');
      EsockEBADF:    errln('EBadF');
      EsockEFAULT:   errln('EFault');
      EsockEINTR:    errln('EInTr');
      EsockEINVAL:   errln('EInVal');
      EsockEMFILE:   errln('EMFile');
      EsockEMSGSIZE: errln('EMsgSize');
      EsockENOBUFS:  errln('ENoBufs');
      EsockENOTCONN: errln('ENotConn');
      EsockENOTSOCK: errln('ENotSock');
      EsockEPROTONOSUPPORT: errln('EProtoNoSupport');
      EsockEWOULDBLOCK:     errln('EWouldBlock');
    end;
    result:= true;
  end;
end;

function CheckErr: boo;
begin
  result:= CheckErr('');
end;

procedure DieIfErr; 
begin 
  if CheckErr then halt; 
end;

{ shuts down and closes entire socket }
procedure EndSock(s: TSock);
begin
  if s > 0 then begin Shutdown(s, 2); CloseSocket(s); end;
end;

///////////////////////////////////////////////////////////////////////////////
                                   end.
///////////////////////////////////////////////////////////////////////////////
