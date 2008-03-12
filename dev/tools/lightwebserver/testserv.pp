// ~NRCOL
{$MODE DELPHI}
Uses
  HttpServ,
  Classes,
  Sysutils,
  Synautil,
  BreakTokens;

Var
  MyPWUWebServer : THTTPServerThread;
  Command        : String;
  CommandLine    : TTokenList;
  DocRoot        : String;
  IP             : String;
  Port           : String;
  Timeout        : LongWord;
  ThreadTimeout  : Int64;
  MaxThreads     : LongWord;

Procedure Start;
Begin
  MyPWUWebServer := THTTPServerThread.Create(DocRoot, IP, Port, TimeOut, ThreadTimeout, MaxThreads);
  // Register CGI applications here
  // Then resume the thread
  MyPWUWebServer.Resume;
  WriteLn('Accepting connections. Type help for instructions.');
End;

Procedure Stop;
Begin
  WriteLn('No incomming connections will be accepted now.');
  MyPWUWebServer.WaitForFinish;
  WriteLn('Waiting for pending threads: ', MyPWUWebServer.RunningChilds,
  ' threads pending - ', TimeToStr(Now));
  Repeat Until MyPWUWebServer.RunningChilds = 0;
  WriteLn('Finished waiting for threads: ', TimeToStr(Now));
  FreeAndNil(MyPWUWebServer);
End;

Procedure Restart;
Begin
  Stop;
  WriteLn('Server restarting...');
  Sleep(10000);
  Start;
End;

Procedure ShowHelp;
Begin
  If CommandLine[1] = '' Then
  Begin
    WriteLn;
    WriteLn('List of Server Commands : ');
    WriteLn;
    WriteLn('quit    : Finishes the server.');
    WriteLn('help    : Shows this help screen.');
    WriteLn('threads : Shows the number of running threads.');
    WriteLn('start   : Makes the server start accepting requests.');
    WriteLn('stop    : Makes the server stop accepting requests.');
    WriteLn('set     : Changes miscellaneous options.');
    WriteLn('show    : Shows current config.');
    WriteLn('restart : Restarts the server.');
    WriteLn;
  End;
  If CommandLine[1] = 'quit' Then
    WriteLn('quit : Finishes the server, waits for all pending threads to finish.');
  If CommandLine[1] = 'help' Then
    WriteLn('help    : Shows this help screen.');
  If CommandLine[1] = 'threads' Then
    WriteLn('threads : Shows the number of running threads.');
  If CommandLine[1] = 'start' Then
    WriteLn('start   : Makes the server start accepting requests. The server starts accepting requests automatically when invoked from the command line. This command should be used if the server was previously stopped by the user.');
  If CommandLine[1] = 'stop' Then
    WriteLn('stop    : Makes the server stop accepting requests.');
  If CommandLine[1] = 'set' Then
  Begin
    WriteLn;
    WriteLn('List of Server Set Commands : ');
    WriteLn;
    WriteLn('rootdir       : Sets the server root dir, where it will look for documents.');
    WriteLn('ip            : Sets the server BIND ip.');
    WriteLn('port          : Sets the server port number.');
    WriteLn('timeout       : Sets the server read timeout of socks (ms).');
    WriteLn('threadtimeout : Sets the server threads maximum running time (ms).');
    WriteLn('maxthreads    : Sets the server maximium number of concurrent threads.');
    WriteLn;
    WriteLn('All those settings can only take effect after restarting the server.');
    WriteLn;
  End;
  If CommandLine[1] = 'show' Then
    WriteLn('show    : Shows current config.');
  If CommandLine[1] = 'restart' Then
    WriteLn('restart : Restarts the server. Makes config changes take effect.');
End;

Begin
  WriteLn('PWU LightWebServer - Small Web Server for intranet/tool/instrumentation.');
  WriteLn('See License for details.');
  If ParamStr(1) <> '' Then
    IP := ParamStr(1)
  Else
    IP := '0.0.0.0';
  If ParamStr(2) <> '' Then
    Port := ParamStr(2)
  Else
    Port := '80';
  If ParamStr(3) <> '' Then
    DocRoot := ParamStr(3)
  Else
    DocRoot := './';
  If ParamStr(4) <> '' Then
    Timeout := StrToInt(ParamStr(4))
  Else
    Timeout := 1000;
  If ParamStr(5) <> '' Then
    ThreadTimeout := StrToInt(ParamStr(5))
  Else
    ThreadTimeout := 1000;
  If ParamStr(6) <> '' Then
    MaxThreads := StrToInt(ParamStr(6))
  Else
    MaxThreads := 65536;
  Start;
  Repeat
    Write('Server command: ');
    ReadLn(Command);
    CommandLine := BreakApart(Command);
    If CommandLine[0] = 'help' Then
      ShowHelp;
    If CommandLine[0] = 'threads' Then
      WriteLn('Currently there are ', MyPWUWebServer.RunningChilds, ' running.');
    If CommandLine[0] = 'start' Then
    Begin
      WriteLn('Server will start accepting requests now.');
      MyPWUWebServer.Activate;
    End;
    If CommandLine[0] = 'stop' Then
    Begin
      WriteLn('Server will stop accepting requests now.');
      MyPWUWebServer.DeActivate;
    End;
    If CommandLine[0] = 'set' Then
    Begin
      If CommandLine[1] = 'rootdir' Then
        DocRoot := CommandLine[2];
      If CommandLine[1] = 'ip' Then
        IP := CommandLine[2];
      If CommandLine[1] = 'port' Then
        Port := CommandLine[2];
      If CommandLine[1] = 'timeout' Then
        Timeout := StrToInt(CommandLine[2]);
      If CommandLine[1] = 'threadtimeout' Then
        ThreadTimeout := StrToInt(CommandLine[2]);
      If CommandLine[1] = 'maxthreads' Then
        MaxThreads := StrToInt(CommandLine[2]);
    End;
    If CommandLine[0] = 'show' Then
    Begin
      WriteLn;
      WriteLn('Document root  : ', DocRoot);
      WriteLn('IP             : ', IP);
      WriteLn('Port           : ', Port);
      WriteLn('Timeout        : ', Timeout);
      WriteLn('Thread timeout : ', ThreadTimeout);
      WriteLn('Max threads    : ', MaxThreads);
      WriteLn;
    End;
    If CommandLine[0] = 'restart' Then
      Restart;
    If CommandLine[0] = 'white' Then
      MyPWUWebServer.WhiteListed(CommandLine[1]);
    If CommandLine[0] = 'cgi' Then
      MyPWUWebServer.CGIValid(CommandLine[1]);
    If CommandLine[0] = 'load' Then
    Begin
      If CommandLine[1] = 'white' Then
        MyPWUWebServer.WhiteListedFromFile(CommandLine[2]);
      If CommandLine[1] = 'cgi' Then
        MyPWUWebServer.CGIValidFromFile(CommandLine[2]);
    End;
  Until CommandLine[0] = 'quit';
  Stop;
End.
