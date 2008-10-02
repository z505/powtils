Unit Scanner;

Interface

Uses Classes, SysUtils;

Type
  // All errors are reported thru this exception
  EScanner = Class(Exception);
  EPreProcess = Class(Exception);

  // This function is usefull because you can grab a file stream
  // from werever the application can provide (From a file or even
  // from memory)
  // The scanner will ask for a stream with the contents of the
  // file named FName
  TSrcCallBackFunc = Function(FName : String): TStream; // Of Object;

  TTokenKind = (
    tkUnknown,    // Unknown token while scanning whitespace
    tkWord,       // Reserved word or punctuation mark
    tkIdent,      // Identifier
    tkNumber,     // Any number, deals with $HEXA
    tkString,     // Single quote enclosed char string (Quote not included)
    tkComment,    // Any comment ('{' and '}' not included)
    tkPreCmd      // Comment that starts with '{$' (Not Included, neither trailing '}')
  );

  TPreProcessKind = (
    ppUnknown,
    ppInclude,
    ppDefine,
    ppIfDef,
    ppIfNDef,
    ppElse,
    ppEndif,
    ppSwitch
  );

  TToken = Record
    Kind    : TTokenKind;       // Kind of the token (See above)
    Value   : String;           // Literal value of it
    Row,                        // Row of the 1st char in the token
    Col     : LongInt;          // Col of the 1st char in the token
    SrcName : String;           // File name were this token was taken from
    PreKind : TPreProcessKind;  // Kind of preprocess token, if applicable
    RValue,                     // If preprocess token, its right value
    LValue  : String;           // If preprocess token, its left value
  End;

  TDefines = TStringList; // A bunch of strings that defines something

  TTokenList = Array Of TToken; // The result of a scanner pass

Function PreProcess(Name : String; Source : TTokenList;
  Defines: TDefines; CallBack: TSrcCallBackFunc): TTokenList;
Function Scanner(Name : String; sSource : TStream): TTokenList;

Implementation

Uses Source;

{$I Words.inc}

Procedure AppendToken(Var Temp : TTokenList; Value : TToken);
Begin
  SetLength(Temp, Length(Temp) + 1);
  Temp[High(Temp)] := Value;
End;

Function ReplaceByTokens(Source1, Source2: TTokenList; Pos : LongInt): TTokenList;
Var
  X, Y : LongInt;
  Temp : TTokenList;

Begin
  For X := Low(Source1) To High(Source1) Do
    If X = Pos Then
      For Y := Low(Source2) To High(Source2) Do
        AppendToken(Temp, Source2[Y])
    Else
      AppendToken(Temp, Source1[X]);
  ReplaceByTokens := Temp;
End;

Function IsDigraph(X : String): Boolean;
Var
  Y : Word;
Begin
  IsDigraph := False;
  For Y := Low(Pascal_Digraph) To High(Pascal_Digraph) Do
    If Pascal_Digraph[Y] = X Then
    Begin
      IsDigraph := True;
      Exit;
    End;
End;

Function IsGraph(X : Char): Boolean;
Var
  Y : Word;
Begin
  IsGraph := False;
  For Y := Low(Pascal_Graph) To High(Pascal_Graph) Do
    If Pascal_Graph[Y] = X Then
    Begin
      IsGraph := True;
      Exit;
    End;
End;

Function IsWord(X : String): Boolean;
Var
  Y : Word;
Begin
  IsWord := False;
  For Y := Low(Pascal_Words) To High(Pascal_Words) Do
    If Pascal_Words[Y] = X Then
    Begin
      IsWord := True;
      Exit;
    End;
End;

Function PreProcessKind(X : String): TPreProcessKind;
Begin
  PreProcessKind := ppSwitch;
  If (X = 'INCLUDE') Or (X = 'I') Then
    PreProcessKind := ppInclude;
  If (X = 'DEFINE') Or (X = 'D') Then
    PreProcessKind := ppDefine;
  If X = 'IFDEF' Then
    PreProcessKind := ppIfDef;
  If X = 'IFNDEF' Then
    PreProcessKind := ppIfNDef;
  If X = 'ELSE' Then
    PreProcessKind := ppElse;
  If X = 'ENDIF' Then
    PreProcessKind := ppEndIf;
End;

Function SplitIfPossible(X : String; Var Leftmost, Rightmost : String): Boolean;
Begin
  If Pos(' ', X) > 0 Then
  Begin
    Leftmost := UpCase(Copy(X, 1, Pos(' ', X) - 1));
    Rightmost := Copy(X, Pos(' ', X) + 1, Length(X));
    SplitIfPossible := True;
  End
  Else
  Begin
    Leftmost := UpCase(X);
    Rightmost:= '';
    SplitIfPossible := False;
  End;
End;

Function Scanner(Name : String; sSource : TStream): TTokenList;

Var
  Source  : TSource;
  Temp    : TTokenList;
  Token   : String;
  TkType  : TTokenKind;
  PreType : TPreProcessKind;
  RValue,
  LValue  : String;

  Procedure Startup;
  Begin
    Source := TSource.Create(Name, sSource);
    Source.Next;
  End;

  Procedure Run;

    Procedure ScanWhite;
    Begin
      Token := '';
      tkType := tkUnknown;
      While (Source.Current In [#08, #32, #13, #10]) And Not(Source.EOS) Do
        Source.Next;
    End;

    Procedure ScanNumber;
    Begin
      Source.Mark;
      tkType := tkNumber;
      Token := '' + Source.Current;
      If Source.Current = '$' Then
        While (Source.Next In ['0'..'9', 'A'..'F', 'a'..'f']) And Not(Source.EOS) Do
          Token := Token + Source.Current
      Else
        While (Source.Next In ['0'..'9']) And Not(Source.EOS) Do
          Token := Token + Source.Current;
    End;

    Procedure ScanString;
    Begin
      Source.Mark;
      tkType := tkString;
      While Source.Next <> '''' Do
        If Source.Current = #13 Then
          Raise EScanner.Create(
            'Error: (' +
            Source.Name + ', ' +
            IntToStr(Source.Row) + ', ' +
            IntToStr(Source.Col) +
            ') String exceeds end of line.')
        Else If Source.EOS Then
            Raise EScanner.Create(
              'Error: (' +
              Source.Name + ', ' +
              IntToStr(Source.Row) + ', ' +
              IntToStr(Source.Col) +
              ') String exceeds end of file.')
          Else
            Token := Token + Source.Current;
      Source.Next;
    End;

    Procedure ScanWord;
    Begin
      Source.Mark;
      Token := '' + Source.Current;
      While (Source.Next In ['a'..'z', 'A'..'Z', '0'..'9', '-', '_']) And
        Not(Source.EOS)  Do
        Token := Token + Source.Current;
      Token := LowerCase(Token);
      If IsWord(Token) Then
        tkType := tkWord
      Else
        tkType := tkIdent;
    End;

    Procedure ScanPunct;
    Begin
      Source.Mark;
      Token := '' + Source.Current;
      tkType := tkWord;
      If (IsDigraph(Token + Source.Next)) And Not(Source.EOS) Then
      Begin
        Token := Token + Source.Current;
        Source.Next;
      End;
    End;

    Procedure ScanComment;
    Begin
      Source.Mark;
      Token := '';
      If Source.Next = '$' Then
      Begin
        tkType := tkPreCmd;
        While Source.Next <> '}' Do
          If Source.EOS Then
            Raise EScanner.Create(
              'Error: (' +
              Source.Name + ', ' +
              IntToStr(Source.Row) + ', ' +
              IntToStr(Source.Col) +
              ') Comment exceeds end of file.')
          Else
            Token := Token + Source.Current;
        SplitIfPossible(Token, LValue, RValue);
        PreType := PreProcessKind(LValue);
        Source.Next;
      End
      Else
      Begin
        tkType := tkComment;
        While Source.Current <> '}' Do
          If Source.EOS Then
            Raise EScanner.Create(
              'Error: (' +
              Source.Name + ', ' +
              IntToStr(Source.Row) + ', ' +
              IntToStr(Source.Col) +
              ') Comment exceeds end of file.')
          Else
          Begin
            Token := Token + Source.Current;
            Source.Next;
          End;
        Source.Next;
      End;
    End;

  Begin
    While Not(Source.EOS) Do
    Begin
      PreType := ppUnknown;
      RValue  := '';
      LValue  := '';
      ScanWhite;
      If Source.Current In ['a'..'z', 'A'..'Z'] Then
        ScanWord
      Else If Source.Current In ['0'..'9'] Then
        ScanNumber
      Else If IsGraph(Source.Current) Then
        ScanPunct
      Else If Source.Current = '''' Then
        ScanString
      Else If Source.Current = '$' Then
        ScanNumber
      Else If Source.Current = '{' Then
        ScanComment
      Else
        If Not(Source.EOS) Then
          Raise EScanner.Create(
            'Error: (' +
            Source.Name + ', ' +
            IntToStr(Source.Row) + ', ' +
            IntToStr(Source.Col) +
            ') Unknown char ("' + Source.Current + '", ' +
            IntToStr(Byte(Source.Current)) + ')');
      If tkType <> tkUnknown Then
      Begin
        SetLength(Temp, Length(Temp) + 1);
        Temp[High(Temp)].Kind    := tkType;
        If tkType = tkWord Then
          Temp[High(Temp)].Value   := LowerCase(Token)
        Else
          Temp[High(Temp)].Value   := Token;
        Temp[High(Temp)].Row     := Source.Row;
        Temp[High(Temp)].Col     := Source.Col;
        Temp[High(Temp)].SrcName := Source.Name;
        Temp[High(Temp)].PreKind := PreType;
        Temp[High(Temp)].RValue  := RValue;
        Temp[High(Temp)].LValue  := LValue;
      End;
    End;
  End;

  Procedure Finish;
  Begin
    Scanner := Temp;
    Source.Free;
  End;

Begin
  Startup;
  Run;
  Finish;
End;

Function PreProcess(Name : String; Source : TTokenList;
  Defines: TDefines; CallBack: TSrcCallBackFunc): TTokenList;

Var
  Posit : LongInt;
  Temp,
  Out   : TTokenList;    

  Procedure ParseIncludes;
  Begin
    Posit := Low(Temp);
    While Posit <= High(Temp) Do
    Begin
      If (Temp[Posit].Kind = tkPreCmd) And
        (Temp[Posit].PreKind = ppInclude) Then
        Temp := ReplaceByTokens(Temp, 
        Scanner(Temp[Posit].RValue, CallBack(Temp[Posit].RValue)),
        Posit);
      Inc(Posit);
    End;
  End;

  Procedure Parse;

    Procedure ParsePreCmd; Forward;

    Procedure ParseIf;
    Var
      WichIf,
      Enabled : Boolean;
    Begin
      WichIf  := Temp[Posit].PreKind = ppIfDef;
      Enabled := Defines.IndexOf(Temp[Posit].RValue) > -1;
      Inc(Posit);
      While Not(Temp[Posit].PreKind In [ppElse, ppEndIf]) And
        (Posit <= High(Temp)) Do
      Begin
        If WichIf Then
          If Enabled Then
            If Temp[Posit].Kind = tkPreCmd Then
              ParsePreCmd
            Else
            If Temp[Posit].Kind <> tkComment Then
              AppendToken(Out, Temp[Posit])
          Else
        Else
          If Enabled Then
          Else
            If Temp[Posit].Kind = tkPreCmd Then
              ParsePreCmd
            Else
              If Temp[Posit].Kind <> tkComment Then
                AppendToken(Out, Temp[Posit]);
        Inc(Posit);
      End;
      If Temp[Posit].PreKind = ppElse Then
      Begin
        Inc(Posit);
        While (Temp[Posit].PreKind <> ppEndIf) And
          (Posit <= High(Temp)) Do
        Begin
          If WichIf Then
            If Enabled Then
            Else
              If Temp[Posit].Kind = tkPreCmd Then
                ParsePreCmd
              Else
                If Temp[Posit].Kind <> tkComment Then
                  AppendToken(Out, Temp[Posit])
          Else
            If Enabled Then
              If Temp[Posit].Kind = tkPreCmd Then
                ParsePreCmd
              Else
              If Temp[Posit].Kind <> tkComment Then
                AppendToken(Out, Temp[Posit]);
          Inc(Posit);
        End;
      End;
      If Temp[Posit].PreKind <> ppEndIf Then
        Raise EPreProcess.Create(
          'Error: (' +
          Temp[Posit].SrcName + ', ' +
          IntToStr(Temp[Posit].Row) + ', ' +
          IntToStr(Temp[Posit].Col) +
          ') Expected {$ENDIF}.');
      Inc(Posit);
    End;

    Procedure ParseDefine;
    Begin
      Defines.Add(Temp[Posit].RValue);
      Inc(Posit);
    End;

    Procedure ParsePreCmd;
    Begin
      If (Temp[Posit].PreKind = ppIfDef) Or
         (Temp[Posit].PreKind = ppIfNDef) Then
        ParseIf
      Else If Temp[Posit].PreKind = ppDefine Then
        ParseDefine;
    End;

  Begin
    Posit := Low(Temp);
    While Posit <= High(Temp) Do
      If Temp[Posit].Kind = tkPreCmd Then
        ParsePreCmd
      Else
      Begin
        If Temp[Posit].Kind <> tkComment Then
          AppendToken(Out, Temp[Posit]);
        Inc(Posit);
      End;
  End;

Begin
  Temp := Source;
  ParseIncludes;
  Parse;
  PreProcess := Out;
End;

End.
