{$DEFINE LWS2 }

Unit LWS2Util;

Interface

Uses
  BreakTokens,
  Classes,
  SysUtils,
  BlckSock,
  StreamIO,
  PWURLEnc,
  {$IFDEF WINDOWS}
  WinSock;
  {$ELSE}
  LibC;
  {$ENDIF}

Var
  Sock     : TTCPBlockSocket;
  Headers  : TStringList;
  IIStream : TMemoryStream;
  OOStream : TMemoryStream;
  IIText   : Text;
  OOText   : Text;
  OldIText : Text;
  OldOText : Text;
  DocRoot  : AnsiString;
  Timeout  : Integer;

Implementation

Var
  Buffer  : AnsiString;
  Tokens  : TTokenList;
  DecURI  : TDecodedURI;

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

Procedure Cleanup;
Begin
  Sleep(Timeout);
  IIStream.Free;
  OOStream.Free;
  Close(IIText);
  Close(OOText);
  Headers.Free;
  Sock.CloseSocket;
  Sock.Free;
End;

Procedure Error(Msg : AnsiString);
Begin
  WriteLn(OldOText, Msg);
  Cleanup;
  Halt(1);
End;

Procedure Startup;
Begin

  Sock        := TTCPBlockSocket.Create;
  Sock.Socket := StrToInt(ParamStr(1));

  Headers            := TStringList.Create;
  Headers.Duplicates := dupIgnore;
  Headers.Sorted     := False;
  Headers.QuoteChar  := #00;
  Headers.Delimiter  := ' ';
  IIStream           := TMemoryStream.Create;
  OOStream           := TMemoryStream.Create;
  AssignStream(IIText, IIStream);
  AssignStream(OOText, OOStream);
  Reset(IIText);
  Rewrite(OOText);
  OldIText := Input;
  OldOText := Output;
  Input    := IIText;
  Output   := OOText;

  DocRoot := URLDecode(ParamStr(2));
  Timeout := StrToInt(ParamStr(3));

End;

Initialization

  Startup;

  If FileExists(DocRoot + 'certificate.pem') And FileExists(DocRoot + 'privatekey.pem') Then
  Begin
    Sock.SSL.CertificateFile := DocRoot + 'certificate.pem';
    Sock.SSL.PrivateKeyFile := DocRoot + 'privatekey.pem';
    Sock.SSLAcceptConnection;
    If Sock.LastError <> 0 Then
      Error(
        'Low level Sock error : ' + CR + LF +
        ' [Sock] ' + Sock.LastErrorDesc + CR + LF +
        ' [SSL] ' + Sock.SSL.LastErrorDesc
      );
  End;

  // Debug WriteLn('Receiving request.');
  Buffer  := Sock.RecvString(Timeout);
  // Debug WriteLn(Buffer, ' from ', Sock.GetRemoteSinIP);

  If Sock.LastError <> 0 Then
      Error(
        'Low level Sock error : ' + CR + LF +
        ' [Sock] ' + Sock.LastErrorDesc + CR + LF +
        ' [SSL] ' + Sock.SSL.LastErrorDesc
      );
  If Buffer = '' Then
    Error('Empty request.');
  Tokens := BreakApart(Buffer);
  DecURI := DecodeURI(Tokens[1]);

  // Debug WriteLn('Receiving and preparing headers.');
  While Buffer <> '' Do
  Begin
    If Sock.LastError <> 0 Then
      Error(
        'Low level Sock error : ' + CR + LF +
        ' [Sock] ' + Sock.LastErrorDesc + CR + LF +
        ' [SSL] ' + Sock.SSL.LastErrorDesc
      );
    Buffer := Sock.RecvString(Timeout);
    If Buffer <> '' Then
      SafeAddHeader(DecodeHeader_Label(Buffer), DecodeHeader_Value(Buffer));
  End;

  // Debug WriteLn('Building internal headers...');
  SafeAddHeader('x-PWU-Method',     Tokens[0]);
  SafeAddHeader('x-PWU-URI',        Tokens[1]);
  SafeAddHeader('x-PWU-Protocol',   Tokens[2]);
  SafeAddHeader('x-PWU-Document',   DecURI.Document);
  SafeAddHeader('x-PWU-Parameters', DecURI.Parameters);
  SafeAddHeader('x-PWU-Remote',     Sock.GetRemoteSinIP);
  SafeAddHeader('x-PWU-FileName',   ExtractFileName(UnQuote(Headers.Values['x-PWU-Document'])));
  SafeAddHeader('x-PWU-FilePath',   CorrectPath);
  SafeAddHeader('x-PWU-FileExt',    RemoveDot(ExtractFileExt(UnQuote(Headers.Values['x-PWU-FileName']))));

  // Debug WriteLn('Checking for incomming (POST) data.');
  If Tokens[0] = 'POST' Then
  Begin
    // Debug WriteLn('Receiving post data.');
    Sock.RecvStreamSize(IIStream, Timeout, StrToInt(UnQuote(Headers.Values['Content-Length'])));
    IIStream.Seek(0, soFromBeginning);
    If Sock.LastError <> 0 Then
      Error(
        'Low level Sock error : ' + CR + LF +
        ' [Sock] ' + Sock.LastErrorDesc + CR + LF +
        ' [SSL] ' + Sock.SSL.LastErrorDesc
      );
  End;

Finalization
 
  // Debug WriteLn('Sending response, and script generated document...');
  Sock.SendString('HTTP/1.0 200 OK' + CR + LF);

  // Debug WriteLn('Instructing the client to close the socket...');
  Sock.SendString('Connection: close');

  // Debug WriteLn('Sending document.');
  Sock.SendBuffer(OOStream.Memory, OOStream.Size);

  // Debug WriteLn('Size: ', OOStream.Size);
  If Sock.LastError <> 0 Then
      Error(
        'Low level Sock error : ' + CR + LF +
        ' [Sock] ' + Sock.LastErrorDesc + CR + LF +
        ' [SSL] ' + Sock.SSL.LastErrorDesc
      );

  Cleanup;

End.
