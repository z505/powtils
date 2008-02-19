Unit BreakTokens;

Interface

Type
    TTokenList = Array[0..255] Of AnsiString;

Function BreakApart(LineText : AnsiString): TTokenList;
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

Function UnQuote(Line : AnsiString): AnsiString;
Begin
  UnQuote := Copy(Line, 2, Length(Line) - 2);
End;

Function RemoveDot(Ext : AnsiString): AnsiString;
Begin
  RemoveDot := Copy(Ext, 2, Length(Ext) -1);
End;

End.
