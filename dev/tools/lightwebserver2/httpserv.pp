Unit HTTPServ;

Interface

Uses
  Classes,
  SysUtils,
  TCPServ,
  {$IFDEF WINDOWS}
  Process,
  WinSock;
  {$ELSE}
  Unix,
  LibC;
  {$ENDIF}

{$IFDEF WINDOWS}
Type
  THTTPRequestThread = Class
  Private
    fProcess : TProcess;
  Public
    Constructor Create(hSock: tSocket);
    Destructor Destroy; Override;
    Function IsRunning: Boolean;
  End;
{$ENDIF}

Const
  EnvVarFmt     = '%s=%s';
  {$IFDEF WINDOWS}
  RequestBinary = 'httprequest.exe %d';
  {$ELSE}
  RequestBinary = './httprequest %d &';
  {$ENDIF}

Var
  DocumentRoot : String;
  Server       : TTCPServer;

Procedure StartServer(IP, Port : String; Timeout, Linger : Integer);
Procedure FinishServer;
Procedure AddRunTimeVar(Name, Value : String);
Procedure AcceptConnection(hSock : TSocket);
Procedure CheckFinished;
Procedure CheckIncomming;

Implementation

Var
{$IFDEF WINDOWS}
  Processes      : Array Of THTTPRequestThread;
{$ENDIF}
  RunTimeOptions : Array Of Record
    Name,
    Value : String;
  End;

Procedure StartServer(IP, Port : String; Timeout, Linger : Integer);
Begin
  Server := TTCPServer.Create(IP, Port, Timeout, Linger);
  Server.OnConnect := @AcceptConnection;
End;

Procedure FinishServer;
Begin
  {$IFDEF WINDOWS}
  While Length(Processes) > 0 Do
    CheckFinished;
  {$ENDIF}
  Server.Free;
End;

Procedure AddRunTimeVar(Name, Value : String);
Begin
  SetLength(RunTimeOptions, Length(RunTimeOptions) + 1);
  RunTimeOptions[High(RunTimeOptions)].Name  := Name;
  RunTimeOptions[High(RunTimeOptions)].Value := Value;
End;

Procedure AcceptConnection(hSock : TSocket);
Begin
  {$IFDEF WINDOWS}
  SetLength(Processes, Length(Processes) + 1);
  Processes[High(Processes)] := THTTPRequestThread.Create(hSock);
  {$ELSE}
  fpSystem(Format(RequestBinary, [ hSock ]));
  {$ENDIF}
End;

{$IFDEF WINDOWS}
Procedure FreeProcess(Idx : LongInt);
Begin
  Processes[Idx].Free;
  Processes[Idx] := Processes[High(Processes)];
  SetLength(Processes, Length(Processes) - 1);
End;
{$ENDIF}

Procedure CheckFinished;
Var
  Ctrl : LongInt;
Begin
  {$IFDEF WINDOWS}
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
  {$ENDIF}
End;

Procedure CheckIncomming;
Begin
  Server.Loop;
End;

{$IFDEF WINDOWS}
Constructor THTTPRequestThread.Create(hSock: tSocket);
Var
  Ctrl : Integer;
Begin
  Inherited Create;
  fProcess                  := TProcess.Create(Nil);
  fProcess.CurrentDirectory := DocumentRoot;
  fProcess.CommandLine      := Format(RequestBinary, [ hSock ]);
  fProcess.Options          := fProcess.Options;
  For Ctrl := Low(RunTimeOptions) To High(RunTimeOptions) Do
    fProcess.Environment.Add(
      Format(EnvVarFmt, [ RunTimeOptions[Ctrl].Name, RunTimeOptions[Ctrl].Value ])
    );
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
{$ENDIF}

End.
