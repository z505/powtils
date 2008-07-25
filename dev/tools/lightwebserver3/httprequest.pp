Uses
  BreakTokens,
  Classes,
  SysUtils,
  BlckSock,
  StreamIO,
  Synautil,
  Process,
  MIMETable,
  {$IFDEF WINDOWS}
  WinSock;
  {$ELSE}
  LibC;
  {$ENDIF}

Type
  // All errors reported
  ESockError = Class(Exception);

  // Request handlers must follow this format
  TRequestHandler = Procedure(Headers: TStringList;
  Var ICGIText, OCGIText : Text; ICGIStream, OCGIStream : TStream);

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

  THTTPRequestThread = Class
  Private
    fSock    : TTCPBlockSocket;
    Headers  : TStringList;
    IIStream : TMemoryStream;
    OOStream : TMemoryStream;
    IIText   : Text;
    OOText   : Text;
  Public
    Constructor Create(hSock: tSocket);
    Destructor Destroy; Override; 
    Procedure Execute;
  End;

Var
  RequestHandlers : TRequestHandlerList;
  DocRootDir      : AnsiString;
  TimeOut         : LongInt;
  Socket          : TSocket;
  Request         : THTTPRequestThread;

{ Internal request handlers, dont use then ! }
Procedure DefaultReqHnd(Headers: TStringList;
  Var ICGIText, OCGIText : Text; ICGIStream, OCGIStream : TStream);
Begin
  // Debug WriteLn('Sending default error screen.');
  Headers.Clear;
  Headers.Add('x-PWU-NPH');
  Headers.Add('Content-type: text/html');
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

Procedure SendBinaryBuffer(Headers : TStringList;
  Var ICGIText, OCGIText : Text; ICGIStream, OCGIStream: TStream);
Var
  Source : File;
  Buffer : PChar;
Begin
  // Debug WriteLn('File effectively sent : ', ExpandFileName(DocRootDir + UnQuote(Headers.Values['x-PWU-FileName'])));
  If FileExists(ExpandFileName(DocRootDir + UnQuote(Headers.Values['x-PWU-FileName']))) Then
  Begin
    Headers.Clear;
    Headers.Add('x-PWU-NPH');
    Headers.Add('Content-type: ' + UnQuote(Headers.Values['x-PWU-MIMEType']));
    Assign(Source, ExpandFileName(DocRootDir + UnQuote(Headers.Values['x-PWU-FileName'])));
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

Procedure CallCGI(Headers : TStringList;
  Var ICGIText, OCGIText : Text; ICGIStream, OCGIStream: TStream);
Var
  SP : TProcess; 
  M  : TMemoryStream;
  LocalText : Text;
  BytesRead,
  N  : LongInt;
  Buffer : String;

Const
   READ_BYTES = 1024;

Begin
  // Debug WriteLn('Calling CGI Process : ', ExpandFileName(DocRootDir + UnQuote(Headers.Values['x-PWU-FileName'])));
  If FileExists(ExpandFileName(DocRootDir + UnQuote(Headers.Values['x-PWU-FileName']))) Then
  Begin
    M                   := TMemoryStream.Create;
    BytesRead           := 0;
    SP                  := TProcess.Create(Nil);
    SP.CurrentDirectory := DocRootDir;
    SP.CommandLine      := ExpandFileName(DocRootDir + UnQuote(Headers.Values['x-PWU-FileName']));
    SP.Environment.Add('SERVER_SOFTWARE=PWULightWebServer');
    SP.Environment.Add('REQUEST_METHOD=' + UnQuote(Headers.Values['x-PWU-Method']));
    SP.Environment.Add('CONTENT_LENGTH=' + UnQuote(Headers.Values['Content-length']));
    SP.Environment.Add('CONTENT_TYPE=' + UnQuote(Headers.Values['Content-type']));
    SP.Environment.Add('QUERY_STRING=' + UnQuote(Headers.Values['x-PWU-Parameters']));
    SP.Environment.Add('HTTP_COOKIE=' + UnQuote(Headers.Values['HTTP-Cookie']));
    SP.Environment.Add('HTTP_ACCEPT_ENCODING=' + UnQuote(Headers.Values['HTTP-Accept-Encoding']));
    SP.Environment.Add('PATH=' + DocRootDir);
    SP.CurrentDirectory := DocRootDir;
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
        Inc(BytesRead, N);
        // Debug Write('.');
      End;
    Until N <= 0;
    If BytesRead > 0 Then
    Begin
      // Debug WriteLn('Total CGI generated output : ', BytesRead);
      M.SetSize(BytesRead);
    End;
    M.Seek(0, soFromBeginning);
    AssignStream(LocalText, M);
    Reset(LocalText);
    Headers.Clear;
    Headers.Add('x-PWU-NPH');
    Repeat
      ReadLn(LocalText, Buffer);
      // Debug WriteLn('Add output header : ', Buffer);
      Headers.Add(Buffer);
    Until Eof(LocalText) Or (Buffer = '');
    If (Buffer = '') And Not(Eof(LocalText)) Then
    Begin
      ReadLn(LocalText, Buffer);
      Repeat
        WriteLn(OCGIText, Buffer);
        ReadLn(LocalText, Buffer);
      Until Eof(LocalText);
    End;
    Close(LocalText);
    M.Free;
    SP.Free;
  End;

End;
{ End of internal request handlers, dont use then ! }

Procedure CallReqHnd(Name : AnsiString; Headers: TStringList;
  Var ICGIText, OCGIText : Text; ICGIStream, OCGIStream : TStream);
Var
  Callee : TRequestHandler = nil;
Begin
  If RequestHandlers.IndexOf(Name) < 0 Then
  Begin
     // Debug WriteLn('Handler not found, calling the default handler (error404)...');
    if RequestHandlers.IndexOf('default') < 0 Then
      Callee := DefaultReqHnd
    Else
      Callee := RequestHandlers.GetHandler('default');
  End
  Else
  Begin
     // Debug WriteLn('Handler found, calling it...');
    Callee := RequestHandlers.GetHandler(Name);
  End;
  Try
    If Assigned(Callee) Then
      Callee(Headers, ICGIText, OCGIText, ICGIStream, OCGIStream);
  Except
    On E: Exception Do
      WriteLn(E.Message);
  End;
End;

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

Constructor THTTPRequestThread.Create(hSock: tSocket);
Begin
  Inherited Create;
  // Debug WriteLn('Thread starting, setting up buffers...');
  fSock              := TTCPBlockSocket.Create;
  fSock.Socket       := hSock;
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
End;

Destructor THTTPRequestThread.Destroy;
Begin
  // Debug WriteLn('Freeing resources.');
  IIStream.Free;
  OOStream.Free;
  Close(IIText);
  Close(OOText);
  Headers.Free;
  fSock.CloseSocket;
  fSock.Free;
  Inherited Destroy;
End;

Procedure THTTPRequestThread.Execute;
Var
  Buffer : AnsiString;
  Tokens : TTokenList;
  DecURI : TDecodedURI;

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
              DocRootDir + UnQuote(Headers.Values['x-PWU-Document'])
            )
          ),
          '\\', '\', [rfReplaceAll]
        ),
        '//', '/', [rfReplaceAll]
      );
  End;

  Procedure POSTOrGET;
  Var
    Ctrl : LongWord;
  Begin
    // Debug WriteLn('Checking for incomming (POST) data.');
    If Tokens[0] = 'POST' Then
    Begin
      // Debug WriteLn('Receiving post data.');
      fSock.RecvStreamSize(IIStream, Timeout, StrToInt(UnQuote(Headers.Values['Content-length'])));
      IIStream.Seek(0, soFromBeginning);
      If fSock.LastError <> 0 Then
        Raise ESockError.Create('Low level socket error.');
    End;

    // Debug WriteLn('Calling the apropriate request handler.');
    CallReqHnd(UnQuote(Headers.Values['x-PWU-FileName']), Headers, IIText, OOText, IIStream, OOStream);

    If Headers.IndexOf('x-PWU-NPH') = -1 Then
    Begin
      // Debug WriteLn('Sending response, no user non-parsed-headers.');
      fSock.SendString('HTTP/1.0 200 OK' + CR + LF);
      fSock.SendString('Content-length: ' + IntToStr(OOStream.Size) + CR + LF);
      fSock.SendString('Connection: close' + CR + LF);
      fSock.SendString('Date: ' + RFC822DateTime(Now) + CR + LF);
      fSock.SendString('Server: PWU LightWebServer' + CR + LF);
      fSock.SendString('' + CR + LF);
      If fSock.LastError <> 0 Then
        Raise ESockError.Create('Low level socket error.');
    End
    Else
    Begin
      // Debug WriteLn('Sending response, user non-parsed-headers present.');
      fSock.SendString('HTTP/1.0 200 OK' + CR + LF);
      fSock.SendString('Connection: close' + CR + LF);
      fSock.SendString('Date: ' + RFC822DateTime(Now) + CR + LF);
      fSock.SendString('Server: PWU LightWebServer' + CR + LF);
      If fSock.LastError <> 0 Then
        Raise ESockError.Create('Low level socket error.');
      OOStream.Seek(0, soFromBeginning);
      For Ctrl := 0 To Headers.Count - 1 Do
        If (Headers[Ctrl] <> '') And
          (Headers[Ctrl] <> 'x-PWU-NPH') Then
          fSock.SendString(Headers[Ctrl] + CR + LF);
      fSock.SendString('' + CR + LF);
    End;

    // Debug WriteLn('Sending document.');
    fSock.SendBuffer(OOStream.Memory, OOStream.Size);
    // Debug WriteLn('Size: ', OOStream.Size);
    If fSock.LastError <> 0 Then
      Raise ESockError.Create('Low level socket error.');
  End;

  Procedure NotImplemented;
  Begin
    // Debug WriteLn('Sending response, no user non-parsed-headers.');
    fSock.SendString('HTTP/1.0 501 Not implemented' + CR + LF);
    fSock.SendString('Date: ' + RFC822DateTime(Now) + CR + LF);
    fSock.SendString('Server: PWU LightWebServer' + CR + LF);
    fSock.SendString('Connection: close' + CR + LF);
    fSock.SendString('' + CR + LF);
    If fSock.LastError <> 0 Then
      Raise ESockError.Create('Low level socket error.');
  End;

Begin
  Try
    // Debug WriteLn('Checking for the possibility of HTTPS connection...');
    If FileExists(DocRootDir + 'certificate.pem') And FileExists(DocRootDir + 'privatekey.pem') Then
    Begin
      // Debug WriteLn('Certificate files found, enabling HTTPS.');
      fSock.SSL.CertificateFile := DocRootDir + 'certificate.pem';
      fSock.SSL.PrivateKeyFile := DocRootDir + 'privatekey.pem';
      fSock.SSLAcceptConnection;
      If fSock.LastError <> 0 Then
        Raise ESockError.Create('Low level socket error.');
    End;

    // Debug WriteLn('Receiving request.');
    Buffer  := fSock.RecvString(Timeout);
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
      Buffer := fSock.RecvString(Timeout);
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

    If (Tokens[0] = 'POST') Or (Tokens[0] = 'GET') Then
      POSTOrGET
    Else
      NotImplemented;

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

Begin
  Repeat
    ReadLn(Socket);
    Request := THTTPRequestThread.Create(Socket);
    Request.Execute;
    Request.Free;
  Until False;
End.
