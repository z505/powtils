Unit BreakTokens;

Interface

Type
    TTokenList = Array[0..255] Of AnsiString;

    TDecodedURI = Record
      Document   : AnsiString;
      Parameters : AnsiString;
    End;

Function BreakApart(LineText : AnsiString): TTokenList;
Function DecodeURI(LineText : AnsiString): TDecodedURI;
Function DecodeHeader_Label(LineText : AnsiString): AnsiString;
Function DecodeHeader_Value(LineText : AnsiString): AnsiString;
Function UnQuote(Line : AnsiString): AnsiString;
Function RemoveDot(Ext : AnsiString): AnsiString;

Implementation

Function BreakApart(LineText : AnsiString): TTokenList;
Var
    Ctrl1      : Word;
    Ctrl2      : Word;
    LineTokens : TTokenList;

Begin
    For Ctrl1 := 0 To 255 Do
        LineTokens[Ctrl1] := '';
    Ctrl1 := 1;
    Ctrl2 := 0;
    If LineText <> '' Then
    Begin
        While (LineText[Ctrl1] = ' ') And (Ctrl1 <= Length(LineText)) Do
            Inc(Ctrl1);
        While Ctrl1 <= Length(LineText) Do
        Begin
            If LineText[Ctrl1] = ' ' Then
            Begin
                While (LineText[Ctrl1] = ' ') And (Ctrl1 <= Length(LineText)) Do
                    Inc(Ctrl1);
                Inc(Ctrl2);
            End;
            LineTokens[Ctrl2] := LineTokens[Ctrl2] + LineText[Ctrl1];
            Inc(Ctrl1);
        End;
    End;
    BreakApart := LineTokens;
End;

Function DecodeURI(LineText : AnsiString): TDecodedURI;
Var
  Ctrl : LongWord;
  Temp : TDecodedURI;
Begin
  Temp.Document   := '';
  Temp.Parameters := '';
  Ctrl            := 1;
  While Not(LineText[Ctrl] = '?')
  And (Ctrl <= Length(LineText)) Do
  Begin
    Temp.Document := Temp.Document + LineText[Ctrl];
    Inc(Ctrl);
  End;
  If LineText[Ctrl] = '?' Then
  Begin
    Inc(Ctrl);
    While Ctrl <= Length(LineText) Do
    Begin
      Temp.Parameters := Temp.Parameters + LineText[Ctrl];
      Inc(Ctrl);
    End;
  End;
  DecodeURI := Temp;
End;

Function DecodeHeader_Label(LineText : AnsiString): AnsiString;
Var
  Ctrl : LongWord;
  Temp : AnsiString;
Begin
  Ctrl := 1;
  Temp := '';
  While Not(LineText[Ctrl] = ':')
  And (Ctrl <= Length(LineText)) Do
  Begin
    Temp := Temp + LineText[Ctrl];
    Inc(Ctrl);
  End;
  DecodeHeader_Label := Temp;
End;

Function DecodeHeader_Value(LineText : AnsiString): AnsiString;
Var
  Ctrl : LongWord;
  Temp : AnsiString;
Begin
  Ctrl := 1;
  Temp := '';
  While Not(LineText[Ctrl] = ':')
  And (Ctrl <= Length(LineText)) Do
    Inc(Ctrl);
  If LineText[Ctrl] = ':' Then
    Inc(Ctrl);
  If LineText[Ctrl] = ' ' Then
    Inc(Ctrl);
  While Ctrl <= Length(LineText) Do
  Begin
    Temp := Temp + LineText[Ctrl];
    Inc(Ctrl);
  End;
  DecodeHeader_Value := Temp;
End;

Function UnQuote(Line : AnsiString): AnsiString;
Begin
  UnQuote := Copy(Line, 2, Length(Line) - 2);
End;

Function RemoveDot(Ext : AnsiString): AnsiString;
Begin
  RemoveDot := Copy(Ext, 2, Length(Ext) -1);
End;

End.
