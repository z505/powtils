{ more advanced sockets functions/wrappers not included currently in sockets.pp fpc RTL}

unit socketsext; {$mode objfpc} {$h+}

interface
uses 
  {$ifdef win32}winsock2{$endif}
  {$ifdef unix}baseunix{$endif} ;



type
  TFds = (fdsRead, fdsWrite, fdsExcept);

function SockSelect(sock: longint; fds: TFds; TimeoutSecs: longint): longint;

implementation

{$ifdef win32}

{  DECLARATION HINTS:

   PFDSet = ^TFDSet;
   TFDSet = packed record
     fd_count: u_int;
     fd_array: array[0..FD_SETSIZE-1] of TSocket;
   end;

 function select(nfds: Longint; readfds, writefds, exceptfds: PFDSet; timeout: PTimeVal): Longint; }

function SockSelect(sock: longint; fds: TFds; TimeoutSecs: longint): longint;
var
  t: TTimeVal;
  fdset: TFdSet; 
begin

  result:= -1;
  t.tv_sec:= TimeoutSecs;
  fdset.fd_count:= 1;
  fdset.fd_array[0]:= sock;

  case fds of
    fdsRead : result:= winsock2.select(0, @fdset, nil, nil, @t);
    fdsWrite : result:= winsock2.select(0, nil, @fdset, nil, @t);
    fdsExcept : result:= winsock2.select(0, nil, nil, @fdset, @t);
  end;

end;
{$endif}

{$ifdef unix}

{  DECLARATION HINTS:
 function fpSelect(N: cInt; readfds: pFDSet; writefds: pFDSet; exceptfds: pFDSet; TimeOut: cInt): cInt; }

function SockSelect(sock: longint; fds: TFds; TimeoutSecs: longint): longint;
var
  t: longint;
  fdset: TFdSet; 
begin

  result:= -1;
  t:= TimeoutSecs * 1000; // convert seconds to baseunix's required milliseconds amount
  fpfd_zero(FDSet);
  fpfd_set(sock,FDSet);

  case fds of
    fdsRead : result:= baseunix.fpselect(sock + 1, @fdset, nil, nil, t);
    fdsWrite : result:= baseunix.fpselect(sock + 1, nil, @fdset, nil, t);
    fdsExcept : result:= baseunix.fpselect(sock + 1, nil, nil, @fdset, t);
  end;

end;
{$endif}



end.
