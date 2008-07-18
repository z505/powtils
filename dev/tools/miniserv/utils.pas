unit utils; {$mode objfpc} {$h+} {$unitpath ../../main}

interface
uses sockets, baseunix, unix;

function s2addr(s : string) : LongInt;
procedure errln(s: string);
function checkError(s: shortstring): boolean;
function checkError: boolean;
procedure checkErrorHalt;
procedure dashes; 
procedure note(s: string);
procedure endSock(s: longint);
function bind(s: sock; saddr: TInetSockAddr): longint;

implementation

function bind(s: sock; saddr: TInetSockAddr): longint;
begin
  result:= bind(sock1, saddr, sizeof(saddr)) );
end;

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

procedure checkErrorHalt;
begin if checkError then HALT;
end;

procedure dashes; 
begin writeln('-----------------------------------------------------------------');
end;

procedure note(s: string);
begin

end;

procedure errln(s: string);
begin
  writeln('ERROR: ', s);
end;

function checkError(s: shortstring): boolean;
begin
//  writeln('Checking error: ');
  result:= false;
  if socketError <> 0 then begin
    writeln('SOCKET ERROR: ', socketerror, ' Note: ', s);
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

function checkError: boolean;
begin
  result:= checkerror('');
end;

procedure endSock(s: longint);
begin
  if s > 0 then begin
    shutdown(s, 2);
    closesocket(s);
  end;
end;

///////////////////////////////////////////////////////////////////////////////
                                   end.
///////////////////////////////////////////////////////////////////////////////
