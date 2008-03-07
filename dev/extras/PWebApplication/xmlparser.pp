// ~NRCOL
Unit XMLParser;

Interface

Uses
  Classes,
  SysUtils;

Const
  ccXMLParser_BGTag = 00; // Beginning of a tag
  ccXMLParser_EDTag = 01; // Ending of a tag
  ccXMLParser_Slash = 02; // the '/' char
  ccXMLParser_Equal = 03; // The '=' char

  // More complex terminals
  ccXMLParser_Text  = 04; // Any non tagged text
  ccXMLParser_Ident = 05; // An identifier inside a tag structure
  ccXMLParser_Str   = 06; // A text inside quotes

  ccXMLParser_Excla = 07; // Adhoc to deal with those pesky new tags

Type
  EXMLParser = Class(Exception);

  TToken = Record
    Row,
    Col:     longword;
    Literal: Ansistring;
    TokenType: Byte;
  End;

  TTokenList = Array Of TToken;

  TTokenIterator = Class
  Private
    fTokenList: TTokenList;
    fPosition:  longword;
    Function GetCurrToken: Ansistring;
    Function GetCurrRow: longword;
    Function GetCurrCol: longword;
    Function GetCurrType: Byte;
  Public
    Constructor Create(Src: TTokenList);
    Function IsEOS: Boolean;
    Procedure Skip;
    Property Token: Ansistring Read GetCurrToken;
    Property Row: longword Read GetCurrRow;
    Property Col: longword Read GetCurrCol;
    Property TkType: Byte Read GetCurrType;
  End;


Function ParseXML(Source: TStream): TTokenList;

Implementation

Function ParseXML(Source: TStream): TTokenList;
Var
  NextChar: Char;
  CurrentRow, CurrentCol: longword;
  Buffer:   TTokenList;

  Function IsEOS: Boolean; // End of stream reached ?
  Begin
    IsEOS := Source.Position > (Source.Size - 1);
  End;

  Function IsAnyChar: Boolean; // Not null
  Begin
    IsAnyChar := NextChar > #00;
  End;

  Function IsStringChar: Boolean; // A valid char to be inside a string
  Begin
    IsStringChar := NextChar In [#32..#255];
  End;

  Function IsAlpha: Boolean;
  Begin
    IsAlpha := NextChar In ['a'..'z', 'A'..'Z', '_', '-'];
  End;

  Function IsAlphaNum: Boolean;
  Begin
    IsAlphaNum := IsAlpha Or (NextChar In ['0'..'9', '.']);
  End;

  Function IsWhite: Boolean;
  Begin
    IsWhite := NextChar In [' ', #13, #10];
  End;

  Procedure GetNext;
  Begin
    If Not (IsEOS) Then
      Begin
      NextChar := Char(Source.ReadByte);
      Case NextChar Of
        #13:
          Begin
          CurrentCol := 0;
          Inc(CurrentRow);
          GetNext;
          End;
        #10:
          GetNext;
        #08:
          Begin
          Inc(CurrentCol, 4);
          GetNext;
          End;
        Else
          Inc(CurrentCol, 1);
        End;
      End
    Else
    ;// Raise EXMLParser.Create('Trying to read past the end of the file.');
  End;

  Function ParseString: String;
  Var
    Temp: String;
  Begin
    GetNext;
    Temp := '';
    While (NextChar <> '"') And Not (IsEOS) Do
      Begin
      Temp := Temp + NextChar;
      GetNext;
      End;
    GetNext;
    ParseString := Temp;
  End;

  Function ParseIdent: String;
  Var
    Temp: String;
  Begin
    Temp := '';
    While IsAlphaNum And Not (IsEOS) Do
      Begin
      Temp := Temp + NextChar;
      GetNext;
      End;
    ParseIdent := Temp;
  End;

  Procedure ParseWhite;
  Begin
    While IsWhite And Not (IsEOS) Do
      GetNext;
  End;

  Function ParseText: Ansistring;
  Var
    Temp: Ansistring;
  Begin
    Temp := '';
    While (NextChar <> '<') And Not (IsEOS) Do
      Begin
      Temp := Temp + NextChar;
      GetNext;
      End;
    ParseText := Temp;
  End;

  Procedure AppendToken(Lit: String; TkType: Byte);
  Begin
    SetLength(Buffer, Length(Buffer) + 1);
    Buffer[High(Buffer)].Row     := CurrentRow;
    Buffer[High(Buffer)].Col     := CurrentCol;
    Buffer[High(Buffer)].Literal := Lit;
    Buffer[High(Buffer)].TokenType := TkType;
  End;

Begin
  NextChar   := #00;
  CurrentCol := 00;
  CurrentRow := 01;
  SetLength(Buffer, 0);
  GetNext;
  While Not (IsEOS) Do
    If NextChar = '<' Then
      Begin
      GetNext;
      If NextChar <> '!' Then
        AppendToken('<', ccXMLParser_BGTag);
      While Not (NextChar In ['<', '>']) And Not (IsEOS) Do
        Begin
        ParseWhite;
        If IsAlpha Then
          AppendToken(ParseIdent, ccXMLParser_Ident)
        Else
        If NextChar = '=' Then
          Begin
          AppendToken('=', ccXMLParser_Equal);
          GetNext;
          End
        Else
        If NextChar = '"' Then
          AppendToken(ParseString, ccXMLParser_Str)
        Else
        If NextChar = '/' Then
          Begin
          AppendToken('/', ccXMLParser_Slash);
          GetNext;
          End
        Else
        If NextChar = '!' Then
          Begin
          GetNext;
          AppendToken('<!' + ParseText, ccXMLParser_Text);
          End
        Else
        If NextChar = '>' Then
          Begin
          AppendToken('>', ccXMLParser_EDTag);
          GetNext;
          Exit;
          End
        Else
          GetNext;
        End;
      End
    Else
    If NextChar = '>' Then
      Begin
      AppendToken('>', ccXMLParser_EDTag);
      GetNext;
      End
    Else
      AppendToken(ParseText, ccXMLParser_Text);
  ParseXML := Buffer;
End;

Function TTokenIterator.GetCurrToken: Ansistring;
Begin
  GetCurrToken := fTokenList[fPosition].Literal;
End;

Function TTokenIterator.GetCurrRow: longword;
Begin
  GetCurrRow := fTokenList[fPosition].Row;
End;

Function TTokenIterator.GetCurrCol: longword;
Begin
  GetCurrCol := fTokenList[fPosition].Col;
End;

Function TTokenIterator.GetCurrType: Byte;
Begin
  GetCurrType := fTokenList[fPosition].TokenType;
End;

Constructor TTokenIterator.Create(Src: TTokenList);
Begin
  fTokenList := Src;
  fPosition  := Low(Src);
End;

Function TTokenIterator.IsEOS: Boolean;
Begin
  IsEOS := fPosition >= High(fTokenList);
End;

Procedure TTokenIterator.Skip;
Begin
  Inc(fPosition);
  If fPosition > High(fTokenList) Then
    fPosition := High(fTokenList);
End;

End.
