// ~NRCOL
{$MODE DELPHI}
Uses
  HttpServ,
  Classes,
  BreakTokens,
  Sysutils,
  Synautil;

Type
  TMyHTTPServerThread = Class(THTTPServerThread)
  Public
    Procedure SendIndex(Headers: TStringList;
    Var ICGIText, OCGIText : Text; ICGIStream, OCGIStream : TStream);
    Constructor Create;
  End;

Procedure TMyHTTPServerThread.SendIndex(Headers: TStringList;
  Var ICGIText, OCGIText : Text; ICGIStream, OCGIStream : TStream);
Begin
  WriteLn(OCGIText, 'Content-type: text/html');
  WriteLn(OCGIText, '');
  WriteLn(OCGIText, '<html>');
  WriteLn(OCGIText, '<head>');
  WriteLn(OCGIText, '<title>PWU LightWebServer example</title>');
  WriteLn(OCGIText, '</head>');
  WriteLn(OCGIText, '<body bgcolor="#000000" text="#FFFFFF">');
  WriteLn(OCGIText, '<img src="logo.png"/><br/>');
  WriteLn(OCGIText, '<strong>Congratulations</strong>');
  WriteLn(OCGIText, '<hr/>');
  WriteLn(OCGIText, 'Your PWU LightWebServer based application is running.<br/>');
  WriteLn(OCGIText, '<hr/>');
  WriteLn(OCGIText, 'Generated ' + RFC822DateTime(Now) + ' by PWU LightWebServer.');
  WriteLn(OCGIText, '</body>');
  WriteLn(OCGIText, '</html>');
End;

Constructor TMyHTTPServerThread.Create;
Begin
  Inherited Create('./', '0.0.0.0', '80', 60000);
End;

Var
  MyPWUWebServer : TMyHTTPServerThread;
  Command        : String;

Begin
  WriteLn('PWU LightWebServer - Small Web Server for intranet/tool/instrumentation.');
  WriteLn('See License for details.');
  MyPWUWebServer := TMyHTTPServerThread.Create;
  // Register internal request handlers here
  MyPWUWebServer.RegisterReqHnd('index.html', MyPWUWebServer.SendIndex);
  // Static white listed documents here
  MyPWUWebServer.WhiteListed('logo.png');
  // And external CGI applications here
  MyPWUWebServer.CGIValid('hello.cgi');
  // Then resume the thread
  MyPWUWebServer.Resume;
  WriteLn('Accepting connections.');
  Repeat
    Write('Server command: ');
    ReadLn(Command);
  Until Command = 'quit';
  WriteLn('No incomming connections will be accepted now.');
  MyPWUWebServer.WaitForFinish;
  WriteLn('Waiting for pending threads: ', MyPWUWebServer.RunningChilds,
  ' threads pending - ', TimeToStr(Now));
  Repeat
  Until MyPWUWebServer.RunningChilds <= 0;
  WriteLn('Finished waiting for threads: ', TimeToStr(Now));
  FreeAndNil(MyPWUWebServer);
End.
