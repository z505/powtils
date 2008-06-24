// ~NRCOL
{$MODE DELPHI}{$R+}
Unit TCPServ;

Interface

Uses
  Classes,
  BlckSock,
  Sockets,
  {$IFDEF WINDOWS}
  WinSock;
  {$ELSE}
  LibC;
  {$ENDIF}

Type
  TTCPConnectionHandler = Procedure(hSock : TSocket);

  TTCPServer = Class
  Private
    fOnConnect : TTCPConnectionHandler;
    fIP        : String;
    fPort      : String;
    fConnCount : LongInt;
    fTimeout   : Integer;
    fLinger    : Integer;
    fSock      : TTCPBlockSocket;
  Public
    Property OnConnect : TTCPConnectionHandler Read fOnConnect Write fOnConnect;
    Property ConnCount : LongInt Read fConnCount;
    Constructor Create(IP, Port : String; CanReadTimeout, Linger : Integer);
    Destructor Destroy; Override;
    Procedure Loop;
  End;

Implementation

Constructor TTCPServer.Create(IP, Port : String; CanReadTimeout, Linger : Integer);
Begin
  Inherited Create;
  fIP        := IP;
  fPort      := Port;
  fTimeout   := CanReadTimeout;
  fLinger    := Linger;
  fConnCount := 0;
  fSock      := TTCPBlockSocket.Create;
  //  fSock.EnableReuse(true);   // synapse says it is not good practice
  fSock.CreateSocket;
  fSock.SetLinger(True, fLinger);
  fSock.Bind(fIP, fPort);
  If fSock.LastError <> 0 Then
  Begin
    WriteLn('Cant bind to ', fIP, ':', fPort, ' !');
    WriteLn(fSock.LastErrorDesc);
    Exit;
  End;
  fSock.Listen;
  If fSock.LastError <> 0 Then
  Begin
    WriteLn('Cant setup to listen !');
    WriteLn(fSock.LastErrorDesc);
    Exit;
  End;
End;

Destructor TTCPServer.Destroy;
Begin
  fSock.CloseSocket;
  fSock.Free;
  Inherited Destroy;
End;

Procedure TTCPServer.Loop;
Var          
  ClientSock : TSocket;
Begin
  If fSock.CanRead(fTimeout) Then
  Begin
    ClientSock := fSock.Accept;
    If fSock.LastError = 0 Then
    Begin
      Inc(fConnCount);
      fOnConnect(ClientSock);
      Sockets.CloseSocket(ClientSock); // needed at least for unix, otherwise file descriptors leak to 1024!
    End
    Else
    Begin
      WriteLn('Cant accept connection !');
      WriteLn(fSock.LastErrorDesc);
    End;
  End;
End;

End.
