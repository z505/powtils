Unit HTTPServ;

Interface

Uses
  Classes,
  SysUtils,
  TCPServ,
  URLEnc,
  {$IFDEF WINDOWS}
  Process,
  WinSock;
  {$ELSE}
  Unix,
  LibC;
  {$ENDIF}

Type
  THTTPRequestThread = Class
  Private
    fProcess : TProcess;
  Public
    Constructor Create(hSock: tSocket);
    Destructor Destroy; Override;
    Function IsRunning: Boolean;
  End;

Const
  EnvVarFmt     = '%s=%s';
  {$IFDEF WINDOWS}
  RequestBinary = 'httprequest.exe %d %s %d';
  {$ELSE}
  RequestBinary = './httprequest %d %s %d';
  {$ENDIF}

Var
  DocumentRoot : String;
  Timeout      : Integer;
  Server       : TTCPServer;

Procedure StartServer(DocRoot, IP, Port : String; TOut, Linger : Integer);
Procedure FinishServer;
Procedure AcceptConnection(hSock : TSocket);
Procedure CheckFinished;
Procedure CheckIncomming;

Implementation

Var
  Processes : Array Of THTTPRequestThread;

Procedure StartServer(DocRoot, IP, Port : String; TOut, Linger : Integer);
Begin
  Server           := TTCPServer.Create(IP, Port, Timeout, Linger);
  Timeout          := TOut;
  DocumentRoot     := DocRoot;
  Server.OnConnect := @AcceptConnection;
End;

Procedure FinishServer;
Begin
  While Length(Processes) > 0 Do
    CheckFinished;
  Server.Free;
End;

Procedure AcceptConnection(hSock : TSocket);
Begin
  SetLength(Processes, Length(Processes) + 1);
  Processes[High(Processes)] := THTTPRequestThread.Create(hSock);
End;

Procedure FreeProcess(Idx : LongInt);
Begin
  Processes[Idx].Free;
  Processes[Idx] := Processes[High(Processes)];
  SetLength(Processes, Length(Processes) - 1);
End;

Procedure CheckFinished;
Var
  Ctrl : LongInt;
Begin
  Ctrl := Low(Processes);
  While Ctrl <= High(Processes) Do
  Begin
    If Not(Processes[Ctrl].IsRunning) Then
    Begin
      FreeProcess(Ctrl);
      Exit;
    End
    Else
      Inc(Ctrl);
  End;
End;

Procedure CheckIncomming;
Begin
  Server.Loop;
End;

Constructor THTTPRequestThread.Create(hSock: tSocket);
Begin
  Inherited Create;
  fProcess                  := TProcess.Create(Nil);
  fProcess.CurrentDirectory := DocumentRoot;
  fProcess.CommandLine      := Format(RequestBinary, [ hSock, URLEncode(DocumentRoot), Timeout ]);
  fProcess.Options          := fProcess.Options;
  fProcess.Execute;
End;

Destructor THTTPRequestThread.Destroy;
Begin
  fProcess.Free;
  Inherited Destroy;
End;

Function THTTPRequestThread.IsRunning: Boolean;
Begin
  IsRunning := fProcess.Running;
End;

End.
