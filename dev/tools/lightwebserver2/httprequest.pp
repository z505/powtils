Uses
  BreakTokens,
  SynaUtil,
  SysUtils,
  Classes,
  BlckSock,
  StreamIO,
  {$IFDEF WINDOWS}
  WinSock;
  {$ELSE}
  LibC;
  {$ENDIF}

Type
  ESockError = Class(Exception);

Var
  hSock   : TSocket;
  Sock    : TTCPBlockSocket;
  Headers : TStringList;
  IIStream: TMemoryStream;
  OOStream: TMemoryStream;
  IIText  : Text;
  OOText  : Text;
  Buffer  : AnsiString;
  Tokens  : TTokenList;
  DecURI  : TDecodedURI;
  DocRoot : String;
  Timeout : Integer;

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
            DocRoot + UnQuote(Headers.Values['x-PWU-Document'])
          )
        ),
        '\\', '\', [rfReplaceAll]
      ),
      '//', '/', [rfReplaceAll]
    );
End;

Procedure NotImplemented;
Begin
  // Debug WriteLn('Sending response, no user non-parsed-headers.');
  WriteLn('HTTP/1.0 501 Not implemented' + CR + LF);
  WriteLn('Date: ' + RFC822DateTime(Now) + CR + LF);
  WriteLn('Server: PWU LightWebServer' + CR + LF);
  WriteLn('Connection: close' + CR + LF);
  WriteLn('' + CR + LF);
  If Sock.LastError <> 0 Then
    Raise ESockError.Create('Low level socket error.');
End;

Procedure ProcessRequest;
Begin

  // Debug WriteLn('Checking for the possibility of HTTPS connection...');
  If FileExists(DocRoot + 'certificate.pem') And FileExists(DocRoot + 'privatekey.pem') Then
  Begin
    // Debug WriteLn('Certificate files found, enabling HTTPS.');
    Sock.SSL.CertificateFile := DocRoot + 'certificate.pem';
    Sock.SSL.PrivateKeyFile := DocRoot + 'privatekey.pem';
    Sock.SSLAcceptConnection;
    If Sock.LastError <> 0 Then
      Raise ESockError.Create('Low level socket error.');
  End;

  // Debug WriteLn('Receiving request.');
  Buffer  := Sock.RecvString(Timeout);
  // Debug WriteLn(Buffer, ' from ', Sock.GetRemoteSinIP);
  If Sock.LastError <> 0 Then
    Raise ESockError.Create('Low level socket error.');
  If Buffer = '' Then
    Raise ESockError.Create('Empty request.');
  Tokens := BreakApart(Buffer);
  DecURI := DecodeURI(Tokens[1]);

  // Debug WriteLn('Receiving and preparing headers.');
  While Buffer <> '' Do
  Begin
    If Sock.LastError <> 0 Then
      Raise ESockError.Create('Low level socket error.');
    Buffer := Sock.RecvString(Timeout);
    If Buffer <> '' Then
      SafeAddHeader(DecodeHeader_Label(Buffer), DecodeHeader_Value(Buffer));
  End;

  NotImplemented;

  // Debug WriteLn('Thread finished.');
End;

Begin
  Try
    hSock              := StrToInt(ParamStr(1));
    Sock               := TTCPBlockSocket.Create;
    Sock.Socket        := hSock;
    IIStream           := TMemoryStream.Create;
    OOStream           := TMemoryStream.Create;
    Headers            := TStringList.Create;
    Headers.Sorted     := True;
    Headers.Duplicates := dupIgnore;
    AssignStream(IIText, IIStream);
    AssignStream(OOText, OOStream);
    Reset(IIText);
    Rewrite(OOText);
    Close(Output);
    Close(Input);
    Output := OOText;
    Input := IIText;
    DocRoot := './';
    Timeout := 15000;
    ProcessRequest;
  Finally
    IIStream.Free;
    OOStream.Free;
    Close(IIText);
    Close(OOText);
    Headers.Free;
    Sock.CloseSocket;
    Sock.Free;
  End;
End.
