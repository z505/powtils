Unit LWS2Util;

Interface

Uses
  BlckSock,
  {$IFDEF WINDOWS}
  WinSock;
  {$ELSE}
  LibC;
  {$ENDIF}

Var
  Socket : TTCPBlockSocket;

Implementation

Initialization

  hSock              := StrToInt(ParamStr(1));
  Sock               := TTCPBlockSocket.Create;
  Sock.Socket        := hSock;

Finalization

  Sock.CloseSocket;
  Sock.Free;

End.
