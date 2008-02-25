// ~NRCOL
{$MODE DELPHI}{$R+}
Unit HTTPServ;

Interface

Uses
  {$IFDEF UNIX}
  CThreads,
  {$ENDIF}
  Classes,
  BlckSock,
  {$IFDEF WINDOWS}
  WinSock,
  {$ELSE}
  LibC,
  {$ENDIF}
  Synautil,
  SysUtils,
  SSL_OpenSSL,
  BreakTokens,
  StreamIO,
  MIMETable,
  Process,
  Syncobjs,
  ThreadPool;

Type
  // All errors reported
  ESockError = Class(Exception);

  // Request handlers must follow this format
  TRequestHandler = Procedure(Headers: TStringList;
  Var ICGIText, OCGIText : Text; ICGIStream, OCGIStream : TStream) Of Object;

  TRequestHandlerListEntry = Record
    Name : String;
    Handler : TRequestHandler;
  End;

  TRequestHandlerList = Class
  Private
    fBuffer : Array Of TRequestHandlerListEntry;
  Public
    Procedure AddHandler(Name : String; Handler : TRequestHandler);
    Function GetHandler(Name : String): TRequestHandler;
    Function IndexOf(Name : String): LongInt;
  End;

  THTTPServerThread = Class(TThread)
  Private
    fRequestHandlers : TRequestHandlerList;
    fDocRoot         : String;          // Root of all documents (appended)
    fTimeout         : LongInt;          // Read timeout
    fIP              : String;          // Server assigned IP
    fPort            : String;          // Server assigned Port
    fRunningThreads  : Longint;          // Number of running threads
    fAcceptClients   : Boolean;          // Weather the server should accept
                                         // incomming connections
    fGlobalFinished  : Boolean;          // Weather the server will stop and
                                         // exit after processing the last
                                         // pending transaction
    fCritical        : TCriticalSection; // Critical section handler
    fThreadPool      : TThreadPool;      // Request threads pool
    fNextThreadID    : LongWord;         // Next Thread ID
    fThreadTimeout   : Int64;            // Thread max time in milliseconds
    fMaxThreads      : LongWord;         // Maximium number of concurrent threads

    // Calls a specific request handler named Name
    Procedure CallReqHnd(Name : AnsiString; Headers: TStringList;
    Var ICGIText, OCGIText : Text; ICGIStream, OCGIStream : TStream);
  Protected

    Procedure DefaultReqHnd(Headers: TStringList;
    Var ICGIText, OCGIText : Text; ICGIStream, OCGIStream : TStream);

    Procedure SendBinaryBuffer(Headers : TStringList;
    Var ICGIText, OCGIText : Text; ICGIStream, OCGIStream: TStream);

    Procedure CallCGI(Headers : TStringList;
    Var ICGIText, OCGIText : Text; ICGIStream, OCGIStream: TStream);

  Public
    // Public properties
    Property DocRootDir    : String Read fDocRoot;
    Property Timeout       : LongInt Read fTimeout;
    Property ThreadPool    : TThreadPool Read fThreadPool;
    Property ThreadTimeout : Int64 Read fThreadTimeout;

    // Registers a handler. It must follow TRequestHandler format
    // Name is the unqualified filename, do not append directory
    // because directories are ignored from requests.
    // If you register a handler named 'default' it will takeover
    // the default file not found screen.
    Procedure RegisterReqHnd(Name : AnsiString; Hnd : TRequestHandler);

    // Setups a server at IP and Port, DocRoot is where documents/cgi apps
    // are stored. Password is used to allow remote server shutdown.
    Constructor Create(DocRoot, IP, Port: String; TOut : LongInt;
    TTOut : Int64; MaxThreads : LongWord);

    // The real server loop and destroyer
    Procedure Execute; Override;
    Destructor Destroy; Override;

    // Whitelists a static document that will be accessed from the web
    Procedure WhiteListed(FileName : String);

    // Loads a file with a whitelist of all files that can be accessed
    Procedure WhiteListedFromFile(FileName : String);

    // Adds a whitelisted CGI application
    Procedure CGIValid(FileName : String);

    // Loads a file with a whitelist of all CGI applications that can be accessed
    Procedure CGIValidFromFile(FileName : String);

    // Increments the number of running child threads
    Procedure IncreaseThreads;

    // Decrements the number of running child threads
    Procedure DecreaseThreads;

    // Returns TRUE if there is running child threads
    Function RunningThreads: Boolean;

    // Makes the server start accepting connections
    Procedure Activate;

    // Makes the server stop accepting connections
    Procedure DeActivate;

    // Waits for child count to reach 0 and then exit thread
    Procedure WaitForFinish;

    // Returns the current number of running child threads
    Function RunningChilds: LongInt;

    // Returns weather the server is active or not
    Function Active: Boolean;

    // Returns weather the server is going to finish
    Function Finish: Boolean;

  End;

  THTTPRequestThread = Class(TThread)
  Private
    fSock    : TTCPBlockSocket;
    fOwner   : THTTPServerThread;
    Headers  : TStringList;
    IIStream : TMemoryStream;
    OOStream : TMemoryStream;
    IIText   : Text;
    OOText   : Text;
    fSelfID  : LongWord;
  Public
    Constructor Create(AOwner : THTTPServerThread; hSock: tSocket; SelfID : LongWord);
    Destructor Destroy; Override;
    Procedure Execute; Override;
  End;

Implementation

Procedure TRequestHandlerList.AddHandler(Name : String; Handler : TRequestHandler);
Begin
  SetLength(fBuffer, Length(fBuffer) + 1);
  fBuffer[High(fBuffer)].Name := Name;
  fBuffer[High(fBuffer)].Handler := Handler;
End;

Function TRequestHandlerList.GetHandler(Name : String): TRequestHandler;
Var
  Ctrl : LongInt;
Begin
  Ctrl := IndexOf(Name);
  If Ctrl >= 0 Then
    GetHandler := fBuffer[Ctrl].Handler
  Else
    GetHandler := Nil;
End;

Function TRequestHandlerList.IndexOf(Name : String): LongInt;
Var
  Ctrl : LongInt;
Begin
  IndexOf := -1;
  For Ctrl := Low(fBuffer) To High(fBuffer) Do
    If fBuffer[Ctrl].Name = Name Then
    Begin
      IndexOf := Ctrl;
      Exit;
    End;
End;

{ Internal request handlers, dont use then ! }
Procedure THTTPServerThread.DefaultReqHnd(Headers: TStringList;
  Var ICGIText, OCGIText : Text; ICGIStream, OCGIStream : TStream);
Begin
  // Debug WriteLn('Sending default error screen.');
  WriteLn(OCGIText, 'Content-type: text/html');
  WriteLn(OCGIText, '');
  WriteLn(OCGIText, '<html>');
  WriteLn(OCGIText, '<head>');
  WriteLn(OCGIText, '<title>Error 404</title>');
  WriteLn(OCGIText, '</head>');
  WriteLn(OCGIText, '<body>');
  WriteLn(OCGIText, '<strong>Error 404</strong><hr>');
  WriteLn(OCGIText, 'The document you requested cannot be found on this server');
  WriteLn(OCGIText, 'or you are not authorized to access it.<br>');
  WriteLn(OCGIText, 'We are really sorry, if this document is indeed a valid resource');
  WriteLn(OCGIText, 'you should ask the server admin to whitelist it.<br>');
  WriteLn(OCGIText, '<hr>');
  WriteLn(OCGIText, 'Generated ', RFC822DateTime(Now), ' by PWU LightWebServer');
  WriteLn(OCGIText, '</body>');
  WriteLn(OCGIText, '</html>');
End;

Procedure THTTPServerThread.SendBinaryBuffer(Headers : TStringList;
  Var ICGIText, OCGIText : Text; ICGIStream, OCGIStream: TStream);
Var
  Source : File;
  Buffer : PChar;
Begin
  // Debug WriteLn('File effectively sent : ', ExpandFileName(fDocRoot + UnQuote(Headers.Values['x-PWU-FileName'])));
  If FileExists(ExpandFileName(fDocRoot + UnQuote(Headers.Values['x-PWU-FileName']))) Then
  Begin
    WriteLn(OCGIText, 'Content-type: ', UnQuote(Headers.Values['x-PWU-MIMEType']));
    WriteLn(OCGIText, '');
    Assign(Source, ExpandFileName(fDocRoot + UnQuote(Headers.Values['x-PWU-FileName'])));
    Reset(Source, 1);
    GetMem(Buffer, FileSize(Source));
    BlockRead(Source, Buffer^, FileSize(Source));
    OCGIStream.Write(Buffer^, FileSize(Source));
    FreeMem(Buffer, FileSize(Source));
    Close(Source);
  End
  Else
    DefaultReqHnd(Headers, ICGIText, OCGIText, ICGIStream, OCGIStream);
End;

Procedure THTTPServerThread.CallCGI(Headers : TStringList;
  Var ICGIText, OCGIText : Text; ICGIStream, OCGIStream: TStream);
Var
  SP : TProcess; 
  M  : TMemoryStream;
  BytesRead,
  N  : LongInt;
Const
   READ_BYTES = 1024;
Begin
  // Debug WriteLn('Calling CGI Process : ', ExpandFileName(fDocRoot + UnQuote(Headers.Values['x-PWU-FileName'])));
  If FileExists(ExpandFileName(fDocRoot + UnQuote(Headers.Values['x-PWU-FileName']))) Then
  Begin
    M                   := TMemoryStream.Create;
    BytesRead           := 0;
    SP                  := TProcess.Create(Nil);
    SP.CurrentDirectory := fDocRoot;
    SP.CommandLine      := ExpandFileName(fDocRoot + UnQuote(Headers.Values['x-PWU-FileName']));
    SP.Environment.Add('SERVER_SOFTWARE=PWULightWebServer');
    SP.Environment.Add('REQUEST_METHOD=' + UnQuote(Headers.Values['x-PWU-Method']));
    SP.Environment.Add('CONTENT_LENGTH=' + UnQuote(Headers.Values['Content-length']));
    SP.Environment.Add('CONTENT_TYPE=' + UnQuote(Headers.Values['Content-type']));
    SP.Environment.Add('QUERY_STRING=' + UnQuote(Headers.Values['x-PWU-Parameters']));
    SP.Environment.Add('HTTP_COOKIE=' + UnQuote(Headers.Values['HTTP-Cookie']));
    SP.Environment.Add('HTTP_ACCEPT_ENCODING=' + UnQuote(Headers.Values['HTTP-Accept-Encoding']));
    SP.Environment.Add('PATH=' + fDocRoot);
    SP.CurrentDirectory := fDocRoot;
    SP.Options          := SP.Options + [poUsePipes];
    SP.Execute;
    SP.Input.CopyFrom(ICGIStream, ICGIStream.Size);
    While SP.Running Do
    Begin
      // make room
      M.SetSize(BytesRead + READ_BYTES);
      // try to read
      N := SP.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
      If N > 0 Then 
      Begin
        Inc(BytesRead, N);
        // Debug Write('.')
      End
      Else 
      Begin
        // no data, wait 100 ms
        Sleep(1);
      End;
    End;
    // read last part
    Repeat
      // make room
      M.SetSize(BytesRead + READ_BYTES);
      // try to read it
      N := SP.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
      If N > 0 Then 
         Begin
        Inc(BytesRead, n);
        // Debug Write('.');
      End;
    Until N <= 0;
    If BytesRead > 0 Then 
    // Debug WriteLn;
    M.SetSize(BytesRead);
    OCGIStream.CopyFrom(M, M.Size);
    M.Free;
    SP.Free;
  End;
End;
{ End of internal request handlers, dont use then ! }

Procedure THTTPServerThread.CallReqHnd(Name : AnsiString; Headers: TStringList;
  Var ICGIText, OCGIText : Text; ICGIStream, OCGIStream : TStream);
Var
  Callee : TRequestHandler = nil;
Begin
  If fRequestHandlers.IndexOf(Name) < 0 Then
  Begin
     // Debug WriteLn('Handler not found, calling the default handler (error404)...');
    if fRequestHandlers.IndexOf('default') < 0 Then
      Callee := DefaultReqHnd
    Else
      Callee := fRequestHandlers.GetHandler('default');
  End
  Else
  Begin
     // Debug WriteLn('Handler found, calling it...');
    Callee := fRequestHandlers.GetHandler(Name);
  End;
  Try
    If Assigned(Callee) Then
      Callee(Headers, ICGIText, OCGIText, ICGIStream, OCGIStream);
  Except
    On E: Exception Do
      WriteLn(E.Message);
  End;
End;

Procedure THTTPServerThread.RegisterReqHnd(Name : AnsiString; Hnd : TRequestHandler);
Begin
  fRequestHandlers.AddHandler(Name, Hnd);
End;

Constructor THTTPServerThread.Create(DocRoot, IP, Port: String;
  TOut : LongInt; TTOut : Int64; MaxThreads : LongWord);
Begin
  // Debug WriteLn('Creating server thread.');
  Inherited Create(True);
  fRequestHandlers := TRequestHandlerList.Create;
  fCritical        := TCriticalSection.Create;
  fDocRoot         := DocRoot;        
  fIP              := IP;
  fPort            := Port;
  fTimeout         := TOut;
  fRunningThreads  := 0;
  fAcceptClients   := True;
  fGlobalFinished  := False;
  fThreadPool      := TThreadPool.Create;
  fThreadTimeout   := TTOut;
  fNextThreadID    := 0;
  fMaxThreads      := MaxThreads;
  FreeOnTerminate  := False;
  Priority         := tpNormal;
End;

Procedure THTTPServerThread.Execute;
Var          
  ClientSock    : TSocket;
  Sock          : TTCPBlockSocket;
Begin
  // Debug WriteLn('Server thread started.');
  Sock := TTCPBlockSocket.Create;
  With Sock Do
  Begin
    CreateSocket;
    SetLinger(True, 10);
    Bind(fIP, fPort);
    If Sock.LastError <> 0 Then
    Begin
      WriteLn(LastErrorDesc);
      WriteLn(SSL.LastErrorDesc);
      Exit;
    End;
    Listen;
    If Sock.LastError <> 0 Then
    Begin
      WriteLn(LastErrorDesc);
      WriteLn(SSL.LastErrorDesc);
      Exit;
    End;
    // Debug WriteLn('Listening.');
    Repeat
      // Debug  If CanRead(0) Then WriteLn('Incomming request, free slot ? ', (RunningChilds < fMaxThreads));
      If (CanRead(0) And Self.Active) And (RunningChilds < fMaxThreads) Then
      Begin
        ClientSock := Accept;
        If LastError = 0 Then
        Begin
          // Debug WriteLn;
          // Debug WriteLn('Creating the thread number ', fNextThreadID, ' now.');
          fThreadPool.AddThread(fNextThreadID, 
          THTTPRequestThread.Create(Self, ClientSock, fNextThreadID),
          fThreadTimeout);
          Inc(fNextThreadID);
        End;
      End
      Else
        Purge;
      fThreadPool.CheckTimeout;
    Until Finish;
    While RunningChilds > 0 Do
      fThreadPool.CheckTimeout; // Wait for pending threads
    Purge;
  End;
  Sock.Free;
  // Debug WriteLn('Server loop exited.');
End;

Destructor THTTPServerThread.Destroy;
Begin
  // Debug WriteLn('Freeing server.');
  fRequestHandlers.Free;
  fCritical.Free;
  fThreadPool.Free;
  Inherited Destroy;
End;    

Procedure THTTPServerThread.WhiteListed(FileName : String);
Begin
  RegisterReqHnd(FileName, SendBinaryBuffer);
End;

Procedure THTTPServerThread.WhiteListedFromFile(FileName : String);
Var
  TheList : TStringList;
  Ctrl : DWord;
Begin
  TheList := TStringList.Create;
  TheList.LoadFromFile(FileName);
  If TheList.Count > 0 Then
    For Ctrl := 0 To TheList.Count - 1 Do
      RegisterReqHnd(TheList[Ctrl], SendBinaryBuffer);
  TheList.Free;    
End;

Procedure THTTPServerThread.CGIValid(FileName : String);
Begin
  RegisterReqHnd(FileName, Self.CallCGI);
End;

Procedure THTTPServerThread.CGIValidFromFile(FileName : String);
Var
  TheList : TStringList;
  Ctrl : DWord;
Begin
  TheList := TStringList.Create;
  TheList.LoadFromFile(FileName);
  If TheList.Count > 0 Then
    For Ctrl := 0 To TheList.Count - 1 Do
      RegisterReqHnd(TheList[Ctrl], CallCGI);
  TheList.Free;    
End;

Procedure THTTPServerThread.IncreaseThreads;
Begin
  InterlockedIncrement(fRunningThreads);
End;

Procedure THTTPServerThread.DecreaseThreads;
Begin
  InterlockedDecrement(fRunningThreads);
End;

Function THTTPServerThread.RunningThreads: Boolean;
Begin
  fCritical.Enter;
  RunningThreads := fRunningThreads > 0;
  fCritical.Leave;
End;

Procedure THTTPServerThread.Activate;
Begin
  fCritical.Enter;
  fAcceptClients := True;
  fCritical.Leave;
End;

Procedure THTTPServerThread.DeActivate;
Begin
  fCritical.Enter;
  fAcceptClients := False;
  fCritical.Leave;
End;

Procedure THTTPServerThread.WaitForFinish;
Begin
  fCritical.Enter;
  fAcceptClients := False;
  fGlobalFinished := True;
  fCritical.Leave;
End;

Function THTTPServerThread.RunningChilds: LongInt;
Begin
  fCritical.Enter;
  RunningChilds := fRunningThreads;
  fCritical.Leave;
End;

Function THTTPServerThread.Active: Boolean;
Begin
  fCritical.Enter;
  Active := fAcceptClients;
  fCritical.Leave;
End;

Function THTTPServerThread.Finish: Boolean;
Begin
  fCritical.Enter;
  Finish := fGlobalFinished;
  fCritical.Leave;
End;
    
Constructor THTTPRequestThread.Create(AOwner : THTTPServerThread; hSock: tSocket; SelfID : LongWord);
Begin
  Inherited Create(True);
  // Debug WriteLn('Thread starting, setting up buffers...');
  fSock              := TTCPBlockSocket.Create;
  fSock.Socket       := hSock;
  fOwner             := AOwner;
  fOwner.IncreaseThreads;
  Headers            := TStringList.Create;
  Headers.Duplicates := dupIgnore;
  Headers.Sorted     := False;
  Headers.QuoteChar  := #00;
  Headers.Delimiter  := ' ';
  IIStream           := TMemoryStream.Create;
  OOStream           := TMemoryStream.Create;
  // Debug WriteLn('Assigning CGI streams.');
  AssignStream(IIText, IIStream);
  AssignStream(OOText, OOStream);
  Reset(IIText);
  Rewrite(OOText);
  FreeOnTerminate    := True;
  Priority           := tpNormal;
  fSelfID            := SelfID;
End;

Destructor THTTPRequestThread.Destroy;
Begin
  // Debug WriteLn('Freeing resources.');
  IIStream.Free;
  OOStream.Free;
  Close(IIText);
  Close(OOText);
  Headers.Free;
  fOwner.DecreaseThreads;
  fOwner.ThreadPool.DelThread(fSelfID);
  fSock.Free;
  Inherited Destroy;
End;

Procedure THTTPRequestThread.Execute;
Var
  Buffer : AnsiString;
  Tokens : TTokenList;
  DecURI : TDecodedURI;
  Ctrl   : LongWord;

  Function CreateHeaderLine(Name, Value : String): String;
  Begin
    CreateHeaderLine := Name + '="' + Value + '"';
  End;

  Procedure SafeAddHeader(Name, Value : String);
  Var
    Idx : Int64;
  Begin
    // Debug WriteLn(Name, ': ', Value);
    Idx := Headers.IndexOf(CreateHeaderLine(Name, Value));
    If Idx < 0 Then
      Headers.Add(CreateHeaderLine(Name, Value))
    Else
    Begin
      Headers.Delete(Idx);
      Headers.Add(CreateHeaderLine(Name, Value));
    End;
  End;

  Function CorrectPath: String;
  Begin
    CorrectPath :=
      StringReplace(
        StringReplace(
          ExtractFilePath(
            ExpandFileName(
              fOwner.DocRootDir + UnQuote(Headers.Values['x-PWU-Document'])
            )
          ),
          '\\', '\', [rfReplaceAll]
        ),
        '//', '/', [rfReplaceAll]
      );
  End;

Begin
  Try
    // Debug WriteLn('Checking for the possibility of HTTPS connection...');
    If FileExists(fOwner.DocRootDir + 'certificate.pem') And FileExists(fOwner.DocRootDir + 'privatekey.pem') Then
    Begin
      // Debug WriteLn('Certificate files found, enabling HTTPS.');
      fSock.SSL.CertificateFile := fOwner.DocRootDir + 'certificate.pem';
      fSock.SSL.PrivateKeyFile := fOwner.DocRootDir + 'privatekey.pem';
      fSock.SSLAcceptConnection;
      If fSock.LastError <> 0 Then
        Raise ESockError.Create('Low level socket error.');
    End;

    // Debug WriteLn('Receiving request.');
    Buffer  := fSock.RecvString(fOwner.Timeout);
    // Debug WriteLn(Buffer, ' from ', fSock.GetRemoteSinIP);
    If fSock.LastError <> 0 Then
      Raise ESockError.Create('Low level socket error.');
    If Buffer = '' Then
      Raise ESockError.Create('Empty request.');
    Tokens := BreakApart(Buffer);
    DecURI := DecodeURI(Tokens[1]);

    // Debug WriteLn('Receiving and preparing headers.');
    While Buffer <> '' Do
    Begin
      If fSock.LastError <> 0 Then
        Raise ESockError.Create('Low level socket error.');
      Buffer := fSock.RecvString(fOwner.Timeout);
      If Buffer <> '' Then
        SafeAddHeader(DecodeHeader_Label(Buffer), DecodeHeader_Value(Buffer));
    End;

    // Debug WriteLn('Building internal headers...');
    SafeAddHeader('x-PWU-Method',     Tokens[0]);
    SafeAddHeader('x-PWU-URI',        Tokens[1]);
    SafeAddHeader('x-PWU-Protocol',   Tokens[2]);
    SafeAddHeader('x-PWU-Document',   DecURI.Document);
    SafeAddHeader('x-PWU-Parameters', DecURI.Parameters);
    SafeAddHeader('x-PWU-Remote',     fSock.GetRemoteSinIP);
    SafeAddHeader('x-PWU-FileName',   ExtractFileName(UnQuote(Headers.Values['x-PWU-Document'])));
    SafeAddHeader('x-PWU-FilePath',   CorrectPath);
    SafeAddHeader('x-PWU-FileExt',    RemoveDot(ExtractFileExt(UnQuote(Headers.Values['x-PWU-FileName']))));
    SafeAddHeader('x-PWU-MIMEType',   FindMIMEOf(UnQuote(Headers.Values['x-PWU-FileExt'])));

    // Debug WriteLn('Checking for incomming (POST) data.');
    If Tokens[0] = 'POST' Then
    Begin
      // Debug WriteLn('Receiving post data.');
      fSock.RecvStreamSize(IIStream, fOwner.Timeout, StrToInt(UnQuote(Headers.Values['Content-length'])));
      IIStream.Seek(0, soFromBeginning);
      If fSock.LastError <> 0 Then
        Raise ESockError.Create('Low level socket error.');
    End;

    // Debug WriteLn('Calling the apropriate request handler.');
    fOwner.CallReqHnd(UnQuote(Headers.Values['x-PWU-FileName']), Headers, IIText, OOText, IIStream, OOStream);

    If Headers.IndexOf('x-PWU-NPH') < 0 Then
    Begin
      // Debug WriteLn('Sending response, no user non-parsed-headers.');
      fSock.SendString('HTTP/1.0 200 OK' + CR + LF);
      fSock.SendString('Content-length: ' + IntToStr(OOStream.Size) + CR + LF);
      fSock.SendString('Connection: close' + CR + LF);
      fSock.SendString('Date: ' + RFC822DateTime(Now) + CR + LF);
      fSock.SendString('Server: PWU LightWebServer' + CR + LF);
      If fSock.LastError <> 0 Then
        Raise ESockError.Create('Low level socket error.');
    End
    Else
    Begin
      // Debug WriteLn('Sending response, user non-parsed-headers present.');
      fSock.SendString('HTTP/1.0 200 OK' + CR + LF);
      For Ctrl := 0 To Headers.Count - 1 Do
        If Headers[Ctrl] <> 'x-PWU-NPH' Then
          fSock.SendString(Headers[Ctrl] + CR + LF);
      fSock.SendString('Content-length: ' + IntToStr(OOStream.Size) + CR + LF);
      fSock.SendString('Connection: close' + CR + LF);
      fSock.SendString('Date: ' + RFC822DateTime(Now) + CR + LF);
      fSock.SendString('Server: PWU LightWebServer' + CR + LF);
      If fSock.LastError <> 0 Then
        Raise ESockError.Create('Low level socket error.');
    End;

    // Debug WriteLn('Sending document.');
    fSock.SendBuffer(OOStream.Memory, OOStream.Size);
    // Debug WriteLn('Size: ', OOStream.Size);
    If fSock.LastError <> 0 Then
      Raise ESockError.Create('Low level socket error.');
    // Debug WriteLn('Thread finished.');
  Except
    On E: Exception Do
    Begin
      // Debug WriteLn('fSock     : ', fSock.LastErrorDesc);
      // Debug WriteLn('fSock.SSL : ', fSock.SSL.LastErrorDesc);
      // Debug WriteLn('Exception : ', E.Message);
      // Debug WriteLn('Thread finished due to error.');
      Exit;
    End;
  End;
End;

End.
