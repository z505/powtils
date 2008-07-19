Unit HTTPServ;

Interface

Uses
  Classes,
  SysUtils,
  TCPServ,
  Process,
  StreamIO,
  {$IFDEF WINDOWS}
  WinSock;
  {$ELSE}
  Unix,
  LibC;
  {$ENDIF}

Type
  THTTPRequestThread = Class(TProcess)
  Private
    fStdIn,
    fStdOut  : Text;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Accept(hSock : TSocket);
  End;

Const
  {$IFDEF WINDOWS}
  RequestBinary = 'httprequest.exe';
  {$ELSE}
  RequestBinary = './httprequest';
  {$ENDIF}

Procedure StartServer(DocRoot, IP, Port : String; TOut, Linger, Workers : Integer);
Procedure FinishServer;
Procedure AcceptConnection(hSock : TSocket);
Procedure CheckIncomming;

Implementation

Var
  DocumentRoot : String;
  Timeout      : Integer;
  Server       : TTCPServer;
  Processes    : Array Of THTTPRequestThread;
  NextProcess  : LongWord;

Procedure StartServer(DocRoot, IP, Port : String; TOut, Linger, Workers : Integer);
Var
  Ctrl : LongWord;
Begin
  Server           := TTCPServer.Create(IP, Port, Timeout, Linger);
  Timeout          := TOut;
  DocumentRoot     := DocRoot;
  Server.OnConnect := @AcceptConnection;
  SetLength(Processes, Workers);
  For Ctrl := Low(Processes) To High(Processes) Do
    Processes[Ctrl] := THTTPRequestThread.Create;
End;

Procedure FinishServer;
Var
  Ctrl : LongWord;
Begin
  For Ctrl := Low(Processes) To High(Processes) Do
  Begin
    Processes[Ctrl].Terminate(0);
    Processes[Ctrl].Free;
  End;
  Server.Free;
End;

Procedure AcceptConnection(hSock : TSocket);
Begin
  Inc(NextProcess);
  NextProcess := NextProcess Mod Length(Processes);
  Processes[NextProcess].Accept(hSock);
End;

Procedure CheckIncomming;
Var
  Ctrl : LongWord;
Begin
  Server.Loop;
  For Ctrl := Low(Processes) To High(Processes) Do
    If Not(Processes[Ctrl].Running) Then
      Processes[Ctrl].Execute;
End;

Constructor THTTPRequestThread.Create;
Begin
  Inherited Create(Nil);
  CurrentDirectory := DocumentRoot;
  CommandLine      := RequestBinary;
  Options          := Options + [ poUsePipes ];
  Execute;
  AssignStream(fStdIn, Input);
  AssignStream(fStdOut, Output);
End;

Destructor THTTPRequestThread.Destroy;
Begin
  Try
    Close(fStdIn);
  Except
    On E: Exception Do ;
  End;
  Try
    Close(fStdOut);
  Except
    On E: Exception Do ;
  End;
  Inherited Destroy;
End;

Procedure THTTPRequestThread.Accept(hSock : TSocket);
Begin
  WriteLn(fStdIn, hSock);
End;

End.
