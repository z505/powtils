Unit SyntaxTree;

Interface
Uses TokenIterator, Scanner, SysUtils, Classes, Tree, TypeConv;

Type
  TIdentifierCallBack = Function(T : TToken; Name : String): TTokenKind Of Object;

  TTokenTreeElement = Class(TTreeElement)
  Private
    fToken : TToken;
    fCallBack : TIdentifierCallBack;
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
    Procedure PropagateIdentifierCallBack(CB : TIdentifierCallBack);
    Procedure VerifyIdentifiers;
    Function EvaluateTo: TTokenKind;
    Function Generate: String;
    Property Token : TToken Read fToken;
  End;

  TTerminalSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TNumericTerminalSyntax = Class(TTerminalSyntax);
  TStringTerminalSyntax = Class(TTerminalSyntax);
  TBooleanTerminalSyntax = Class(TTerminalSyntax);
  TCharTerminalSyntax = Class(TTerminalSyntax);
  TFloatTerminalSyntax = Class(TTerminalSyntax);

  TIdentifierSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TIdentifierListSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TTypeIdentifierSyntax = Class(TIdentifierSyntax);

  TSymbolReferenceSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TMulOperatorSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TSumOperatorSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TRelOperatorSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TFactorTerminalSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;    

  TFactorSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;    

  TTermSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TPrefixedSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TUnaryMinusSyntax = Class(TPrefixedSyntax);

  TUnaryNotSyntax = Class(TPrefixedSyntax);

  TExpressionSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TExpressionListSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TVarDecSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TVarSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TStamentSyntax = Class;
  TCodeBlockSyntax = Class;

  TForFinishExpressionSyntax = Class(TExpressionSyntax);

  TIfStamentSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TForDirectionSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TForStamentSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TWhileStamentSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TRepeatStamentSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TCaseEntrySyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TCaseStamentSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TWithStamentSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TAttribSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TCallSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TStamentSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TElseStamentSyntax = Class(TStamentSyntax);

  TCodeBlockSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TVarParameterDecSyntax = Class(TVarDecSyntax);

  TParameterDecSyntax = Class(TVarDecSyntax);

  TParametersSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TFuncSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TProcSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TJavaBodySyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TJavaFuncSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TJavaProcSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

  TProgramSyntax = Class(TTokenTreeElement)
  Public
    Constructor Create(T : TTokenIterator; O : TTokenTreeElement);
  End;

Procedure RaiseError(Msg : String; Tk : TToken);

Implementation

Procedure RaiseError(Msg : String; Tk : TToken);
Begin
  Raise EParser.Create(
    'Fatal Error. ' + #13#10 +
    'File: "' +
    Tk.SrcName + '". ' + #13#10 + 'Got : "' +
    Tk.Value + '", At (' +
    IntToStr(Tk.Row) + ', ' +
    IntToStr(Tk.Col) + '): ' + #13#10 +
    Msg
  );
End;

Constructor TTokenTreeElement.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(O);
  If Assigned(T) Then
    fToken := T.Token;
End;

Procedure TTokenTreeElement.PropagateIdentifierCallBack(CB : TIdentifierCallBack);
Begin
  fCallBack := CB;
  If (Count > 0) Then
  Begin
    Start;
    Repeat
      (Child As TTokenTreeElement).PropagateIdentifierCallBack(CB);
      Next;
    Until EOE;
  End;
End;

Procedure TTokenTreeElement.VerifyIdentifiers;
Begin
  If Self Is TSymbolReferenceSyntax Then
    fCallBack(
      (Self As TSymbolReferenceSyntax).Token,
      (Self As TSymbolReferenceSyntax).Token.Value
      );
  If (Count > 0) Then
  Begin
    Start;
    Repeat
      (Child As TTokenTreeElement).VerifyIdentifiers;
      Next;
    Until EOE;
  End;
End;

Function TTokenTreeElement.EvaluateTo: TTokenKind;
Var
  Tmp1,
  Tmp2 : TTokenKind;
Begin
  If Self Is TNumericTerminalSyntax Then
    EvaluateTo := tkNumber
  Else If Self Is TStringTerminalSyntax Then
    EvaluateTo := tkString
  Else If Self Is TBooleanTerminalSyntax Then
    EvaluateTo := tkBoolean
  Else If Self Is TCharTerminalSyntax Then
    EvaluateTo := tkChar
  Else If Self Is TFloatTerminalSyntax Then
    EvaluateTo := tkFloat
  Else If Self Is TUnaryMinusSyntax Then
  Begin
    Start;
    EvaluateTo := (Child As TTokenTreeElement).EvaluateTo;
  End
  Else If Self Is TUnaryNotSyntax Then
  Begin
    Start;
    EvaluateTo := (Child As TTokenTreeElement).EvaluateTo;
  End
  Else If Self Is TExpressionSyntax Then
  Begin
    Tmp1 := tkUnknown;
    Tmp2 := tkUnknown;
    Start;
    If FindElement(TRelOperatorSyntax) Then
    Begin
      Tmp1 := (Child As TRelOperatorSyntax).EvaluateTo;
      Start;
      If FindElement(TTermSyntax) Then
        Tmp2 := (Child As TTermSyntax).EvaluateTo
      Else
        RaiseError('Internal error, cant find factor.', fToken);
      If Tmp1 <> Tmp2 Then
        RaiseError('Type mismatch (1).', fToken);
    End
    Else
    Begin
      If FindElement(TTermSyntax) Then
        Tmp1 := (Child As TTermSyntax).EvaluateTo
      Else
        RaiseError('Internal error, cant find factor.', fToken);
    End;
    EvaluateTo := Tmp1;
  End
  Else If Self Is TSymbolReferenceSyntax Then
    EvaluateTo := fCallBack(fToken, fToken.Value)
  Else If Self Is TMulOperatorSyntax Then
  Begin
    Start;
    Tmp1 := (Child As TTokenTreeElement).EvaluateTo;
    If Not(Tmp1 In [tkNumber, tkFloat]) Then
      RaiseError('Type mismatch (2).', fToken);
    EvaluateTo := Tmp1;
  End
  Else If Self Is TSumOperatorSyntax Then
  Begin
    Start;
    EvaluateTo := (Child As TTokenTreeElement).EvaluateTo;
  End
  Else If Self Is TRelOperatorSyntax Then
  Begin
    Start;
    EvaluateTo := (Child As TTokenTreeElement).EvaluateTo;
  End
  Else If Self Is TFactorTerminalSyntax Then
  Begin
    Start;
    EvaluateTo := (Child As TTokenTreeElement).EvaluateTo;
  End
  Else If Self Is TFactorSyntax Then
  Begin
    Tmp1 := tkUnknown;
    Tmp2 := tkUnknown;
    Start;
    If FindElement(TMulOperatorSyntax) Then
    Begin
      Tmp1 := (Child As TMulOperatorSyntax).EvaluateTo;
      Start;
      If FindElement(TFactorTerminalSyntax) Then
        Tmp2 := (Child As TFactorTerminalSyntax).EvaluateTo
      Else
        RaiseError('Internal error, cant find factor.', fToken);
      If Tmp1 <> Tmp2 Then
        RaiseError('Type mismatch (4).', fToken);
    End
    Else
    Begin
      If FindElement(TFactorTerminalSyntax) Then
        Tmp1 := (Child As TFactorTerminalSyntax).EvaluateTo
      Else
        RaiseError('Internal error, cant find factor.', fToken);
    End;
    EvaluateTo := Tmp1;
  End
  Else If Self Is TTermSyntax Then
  Begin
    Tmp1 := tkUnknown;
    Tmp2 := tkUnknown;
    Start;
    If FindElement(TSumOperatorSyntax) Then
    Begin
      Tmp1 := (Child As TSumOperatorSyntax).EvaluateTo;
      Start;
      If FindElement(TFactorSyntax) Then
        Tmp2 := (Child As TFactorSyntax).EvaluateTo
      Else
        RaiseError('Internal error, cant find factor.', fToken);
      If Tmp1 <> Tmp2 Then
        RaiseError('Type mismatch (5).', fToken);
    End
    Else
    Begin
      If FindElement(TFactorSyntax) Then
        Tmp1 := (Child As TFactorSyntax).EvaluateTo
      Else
        RaiseError('Internal error, cant find factor.', fToken);
    End;
    EvaluateTo := Tmp1;
  End
  Else
    EvaluateTo := tkUnknown;
End;

Function TTokenTreeElement.Generate: String;
Begin
  If Self Is TNumericTerminalSyntax Then
    Result := fToken.Value
  Else If Self Is TStringTerminalSyntax Then
    Result := '"' + fToken.Value + '"'
  Else If Self Is TBooleanTerminalSyntax Then
    If fToken.Value = 'true' Then
      Result := '1'
    Else
      Result := '0'
  Else If Self Is TCharTerminalSyntax Then
    Result := '"' + Char(StrToInt(fToken.Value)) + '"'
  Else If Self Is TFloatTerminalSyntax Then
    Result := fToken.Value
  Else If Self Is TIdentifierSyntax Then
    Result := fToken.Value
  Else If Self Is TSymbolReferenceSyntax Then
    If Count > 0 Then
      Result := fToken.Value + '.' +
        (Child As TSymbolReferenceSyntax).Generate
    Else
      Result := fToken.Value
  Else If Self Is TMulOperatorSyntax Then
  Begin
    EvaluateTo; // Just to check type mismatch;
    Result := Translate(fToken.Value) + ' ' +
      (Child As TFactorSyntax).Generate;
  End
  Else If Self Is TSumOperatorSyntax Then
  Begin
    EvaluateTo; // Just to check type mismatch;
    Result := Translate(fToken.Value) + ' ' +
      (Child As TFactorSyntax).Generate;
  End
  Else If Self Is TRelOperatorSyntax Then
  Begin
    EvaluateTo; // Just to check type mismatch;
    Result := Translate(fToken.Value) + ' ' +
      (Child As TTermSyntax).Generate;
  End
  Else If Self Is TFactorTerminalSyntax Then
  Begin
    Start;
    Result := (Child As TTokenTreeElement).Generate;
  End
  Else If Self Is TFactorSyntax Then
  Begin
    Start;
    FindElement(TFactorTerminalSyntax);
    Result := (Child As TFactorTerminalSyntax).Generate;
    Start;
    If FindElement(TMulOperatorSyntax) Then
      Result := Result + ' ' + (Child As TMulOperatorSyntax).Generate;
  End
  Else If Self Is TTermSyntax Then
  Begin
    Start;
    FindElement(TFactorSyntax);
    Result := (Child As TFactorSyntax).Generate;
    Start;
    If FindElement(TSumOperatorSyntax) Then
      Result := Result + ' ' + (Child As TSumOperatorSyntax).Generate;
  End
  Else If Self Is TUnaryMinusSyntax Then
  Begin
    Start;
    FindElement(TFactorTerminalSyntax);
    Result := '-' + (Child As TFactorTerminalSyntax).Generate
  End
  Else If Self Is TUnaryNotSyntax Then
  Begin
    Start;
    FindElement(TFactorTerminalSyntax);
    Result := '!' + (Child As TFactorTerminalSyntax).Generate
  End
  Else If Self Is TExpressionSyntax Then
  Begin
    Start;
    FindElement(TTermSyntax);
    Result := (Child As TTermSyntax).Generate;
    Start;
    If FindElement(TRelOperatorSyntax) Then
      Result := Result + ' ' + (Child As TRelOperatorSyntax).Generate;
  End
  Else If Self Is TExpressionListSyntax Then
  Begin
    Start;
    Result := (Child As TExpressionSyntax).Generate;
    Next;
    While Not(EOE) Do
    Begin
      Result := Result + ', ' + (Child As TExpressionSyntax).Generate;
      Next;
    End;
  End
  Else If Self Is TIfStamentSyntax Then
  Begin
    Start;
    FindElement(TExpressionSyntax);
    Result := 'if (' + (Child As TExpressionSyntax).Generate + ')';
    FindElement(TStamentSyntax);
    Result := Result + #13#10 +  (Child As TStamentSyntax).Generate;
    If FindElement(TElseStamentSyntax) Then
      Result := Result + 'else' + #13#10 +  
        (Child As TStamentSyntax).Generate;
  End
  Else If Self Is TForStamentSyntax Then
  Begin
    Start;
    FindElement(TIdentifierSyntax);
    Result := 'for (' + (Child As TIdentifierSyntax).Generate;
    Start;
    FindElement(TExpressionSyntax);
    Result := Result + '=' + (Child As TExpressionSyntax).Generate;
    Start;
    FindElement(TIdentifierSyntax);
    Result := Result + '; ' + (Child As TIdentifierSyntax).Generate;
    Start;
    FindElement(TForFinishExpressionSyntax);
    Result := Result + '==' + (Child As TExpressionSyntax).Generate;
    Start;
    FindElement(TIdentifierSyntax);
    Result := Result + '; ' + (Child As TIdentifierSyntax).Generate;
    Start;
    FindElement(TForDirectionSyntax);
    If (Child As TForDirectionSyntax).Token.Value = 'to' Then
      Result := Result + '++)'
    Else
      Result := Result + '--)';
    Start;
    FindElement(TStamentSyntax);
    Result := Result + #13#10 +  
      (Child As TStamentSyntax).Generate;
  End
  Else If Self Is TWhileStamentSyntax Then
  Begin
    Start;
    FindElement(TExpressionSyntax);
    Result := 'while (' + (Child As TExpressionSyntax).Generate + ')' + #13#10;
    Start;
    FindElement(TStamentSyntax);
    Result := Result +  (Child As TStamentSyntax).Generate;
  End
  Else If Self Is TRepeatStamentSyntax Then
  Begin
    Start;
    Repeat
      If Child Is TStamentSyntax Then
        Result := Result + (Child As TStamentSyntax).Generate + ';' + #13#10;
      Next;
    Until EOE;
    Start;
    FindElement(TExpressionSyntax);
    Result := 'while (!(' + (Child As TExpressionSyntax).Generate + '))' + #13#10 + '{';
    Start;
    Repeat
      If Child Is TStamentSyntax Then
        Result := Result +  (Child As TStamentSyntax).Generate + ';' + #13#10;
      Next;
    Until EOE;
    Result := Result + '};' + #13#10;
  End
  Else If Self Is TCaseEntrySyntax Then
  Begin
    Result := 'case ';
    Start;
    FindElement(TFactorTerminalSyntax);
    Result := Result + (Child As TFactorTerminalSyntax).Generate;
    Start;
    FindElement(TStamentSyntax);
    Result := Result +  (Child As TStamentSyntax).Generate;
  End
  Else If Self Is TCaseStamentSyntax Then
  Begin
    Result := 'switch (';
    Start;
    FindElement(TSymbolReferenceSyntax);
    Result := Result + (Child As TSymbolReferenceSyntax).Generate + ')' + #13#10 + '{';
    Start;
    Repeat
      If Child Is TCaseEntrySyntax Then
        Result := Result +  (Child As TCaseEntrySyntax).Generate;
      Next;
    Until EOE;
    Start;
    If FindElement(TStamentSyntax) Then
      Result := Result + 'else ' + #13#10 + (Child As TStamentSyntax).Generate;
    Result := Result + '}';
  End
  Else If Self Is TWithStamentSyntax Then
  Begin
    Result := '// Unsuported' + #13#10;
  End
  Else If Self Is TAttribSyntax Then
  Begin
    Start;
    FindElement(TIdentifierSyntax);
    Result := (Child As TIdentifierSyntax).Generate;
    Start;
    FindElement(TExpressionSyntax);
    Result := Result + ' = (' + (Child As TExpressionSyntax).Generate + ')';
  End
  Else If Self Is TCallSyntax Then
  Begin
    Start;
    FindElement(TSymbolReferenceSyntax);
    Result := (Child As TSymbolReferenceSyntax).Generate + '(';
    Start;
    If FindElement(TExpressionListSyntax) Then
      Result := Result + (Child As TExpressionListSyntax).Generate;
    Result := Result + ')';
  End
  Else If Self Is TStamentSyntax Then
  Begin
    Start;
    If Child Is TCodeBlockSyntax Then
      Result := '{' + #13#10 +  (Child As TTokenTreeElement).Generate + '}'
    Else
      Result := (Child As TTokenTreeElement).Generate;
  End
  Else If Self Is TElseStamentSyntax Then
  Begin
    Start;
    Result :=  (Child As TTokenTreeElement).Generate;
  End
  Else If Self Is TCodeBlockSyntax Then
  Begin
    Start;
    While Not(EOE) Do
    Begin
      If Result <> '' Then
        Result := Result +  (Child As TStamentSyntax).Generate + ';' + #13#10
      Else
        Result :=  (Child As TStamentSyntax).Generate + ';' + #13#10;
      Next;
    End;
  End
End;

Constructor TTerminalSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
End;

Constructor TIdentifierSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  If T.Expected(tkIdent) Then
  Begin
    Inherited Create(T, O);
    T.Next;
  End
  Else
    T.RaiseError('Expected identifier.');
End;

Constructor TIdentifierListSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  AddChild(TIdentifierSyntax.Create(T, Self));
  While T.Expected(',') Do
  Begin
    T.Next;
    AddChild(TIdentifierSyntax.Create(T, Self));
  End;
End;

Constructor TSymbolReferenceSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  If T.Expected('.') Then
  Begin
    T.Next;
    AddChild(TSymbolReferenceSyntax.Create(T, O));
  End;
End;

Constructor TMulOperatorSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  AddChild(TFactorSyntax.Create(T, O));
End;

Constructor TSumOperatorSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  AddChild(TFactorSyntax.Create(T, O));
End;

Constructor TRelOperatorSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  AddChild(TTermSyntax.Create(T, O));
End;

Constructor TFactorTerminalSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  If T.Expected(tkIdent) Then
  Begin
    T.Mark;
    T.Next;
    While Not(T.EOTk) And T.Expected('.') Do
    Begin
      T.Next;
      If T.Expected(tkIdent) Then
        T.Next
      Else
        T.RaiseError('Expected identifier after "."');
    End;
    If T.Expected('(') Then
    Begin
      T.BackTrack;
      AddChild(TCallSyntax.Create(T, O));
    End
    Else
    Begin
      T.BackTrack;
      AddChild(TSymbolReferenceSyntax.Create(T, O));
    End;
  End
  Else If T.Token.Value = '-' Then
    AddChild(TUnaryMinusSyntax.Create(T, O))
  Else If T.Token.Value = 'not' Then
    AddChild(TUnaryNotSyntax.Create(T, O))
  Else If T.Token.Value = '(' Then
  Begin
    T.Next;
    AddChild(TExpressionSyntax.Create(T, O));
    If T.Expected(')') Then
      T.Next
    Else
      T.RaiseError('Expected ")"');
  End
  Else If T.Token.Kind = tkNumber Then
    AddChild(TNumericTerminalSyntax.Create(T, O))
  Else If T.Token.Kind = tkString Then
    AddChild(TStringTerminalSyntax.Create(T, O))
  Else If T.Token.Kind = tkBoolean Then
    AddChild(TBooleanTerminalSyntax.Create(T, O))
  Else If T.Token.Kind = tkFloat Then
    AddChild(TCharTerminalSyntax.Create(T, O))
  Else If T.Token.Kind = tkChar Then
    AddChild(TFloatTerminalSyntax.Create(T, O))
  Else
    T.RaiseError('Expected identifer or literal');
End;

Constructor TFactorSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  AddChild(TFactorTerminalSyntax.Create(T, O));
  If T.IsMulOp Then
    AddChild(TMulOperatorSyntax.Create(T, O));
End;

Constructor TTermSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  AddChild(TFactorSyntax.Create(T, O));
  If T.IsSumOp Then
    AddChild(TSumOperatorSyntax.Create(T, O));
End;

Constructor TPrefixedSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  AddChild(TFactorTerminalSyntax.Create(T, O));
End;

Constructor TExpressionSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  AddChild(TTermSyntax.Create(T, O));
  If T.IsRelOp Then
    AddChild(TRelOperatorSyntax.Create(T, O));
End;

Constructor TExpressionListSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Repeat
    AddChild(TExpressionSyntax.Create(T, O));
  Until T.EOTk Or Not(T.Expected(','));
End;

Constructor TVarDecSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  AddChild(TIdentifierListSyntax.Create(T, O));
  If T.Expected(':') Then
  Begin
    T.Next;
    AddChild(TTypeIdentifierSyntax.Create(T, O));
  End
  Else
    T.RaiseError('Expected ":"');
End;

Constructor TVarSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  While Not(T.EOTk) And T.Expected(tkIdent) Do
  Begin
    AddChild(TVarDecSyntax.Create(T, Self));
    If T.Expected(';') Then
      T.Next
    Else
      T.RaiseError('Expected ";"');
  End;
End;

Constructor TIfStamentSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  AddChild(TExpressionSyntax.Create(T, O));
  If T.Expected('then') Then
    T.Next
  Else
    T.RaiseError('Expected "then"');
  AddChild(TStamentSyntax.Create(T, O));
  If T.Expected('else') Then
  Begin
    T.Next;
    AddChild(TElseStamentSyntax.Create(T, O));
  End;
End;

Constructor TForDirectionSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  If T.Expected('to') Or T.Expected('downto') Then
  Begin
    Inherited Create(T, O);
    T.Next;
  End
  Else
    T.RaiseError('Expected "to" or "downto".');
End;

Constructor TForStamentSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  AddChild(TIdentifierSyntax.Create(T, O));
  If T.Expected(':=') Then
  Begin
    T.Next;
    AddChild(TExpressionSyntax.Create(T, O));
  End
  Else
    T.RaiseError('Expected ":=".');
  If T.Expected('to') Or T.Expected('downto') Then
  Begin
    AddChild(TForDirectionSyntax.Create(T, O));
    AddChild(TForFinishExpressionSyntax.Create(T, O));
  End
  Else
    T.RaiseError('Expected "to".');
  If T.Expected('do') Then
  Begin
    T.Next;
    AddChild(TStamentSyntax.Create(T, O));
  End
  Else
    T.RaiseError('Expected "do".');
End;

Constructor TWhileStamentSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  AddChild(TExpressionSyntax.Create(T, O));
  If T.Expected('do') Then
  Begin
    T.Next;
    AddChild(TStamentSyntax.Create(T, O));
  End
  Else
    T.RaiseError('expected "do"');
End;

Constructor TRepeatStamentSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  While Not(T.Expected('until')) Do
    AddChild(TStamentSyntax.Create(T, O));
  If T.Expected('until') Then
    T.Next
  Else
    T.RaiseError('Expected "until".');
  AddChild(TExpressionSyntax.Create(T, O));
End;

Constructor TCaseEntrySyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  AddChild(TFactorTerminalSyntax.Create(T, O));
  If T.Expected(':') Then
  Begin
    T.Next;
    AddChild(TStamentSyntax.Create(T, O));
  End
  Else
    T.RaiseError('Expected ":"');
End;

Constructor TCaseStamentSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  AddChild(TSymbolReferenceSyntax.Create(T, O));
  If T.Expected('of') Then
  Begin
    T.Next;
    While Not((T.Expected('else') Or T.Expected('end')) Or T.EOTk) Do
    Begin
      AddChild(TCaseEntrySyntax.Create(T, O));
      If T.Expected(';') Then
        T.Next
      Else
        T.RaiseError('Expected ";".');
    End;
    If T.Expected('else') Then
    Begin
      T.Next;
      AddChild(TStamentSyntax.Create(T, O));
    End;
    If T.Expected('end') Then
      T.Next
    Else
      T.RaiseError('Expected "end"');
  End
  Else
    T.RaiseError('Expected "of"');
End;

Constructor TWithStamentSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  AddChild(TIdentifierSyntax.Create(T, O));
  If T.Expected('do') Then
  Begin
    T.Next;
    AddChild(TStamentSyntax.Create(T, O));
  End
  Else
    T.RaiseError('Expected "do"');
End;

Constructor TAttribSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  AddChild(TIdentifierSyntax.Create(T, O));
  If T.Expected(':=') Then
  Begin
    T.Next;
    AddChild(TExpressionSyntax.Create(T, O));
  End
  Else
    T.RaiseError('Expected ":="');
End;

Constructor TCallSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  AddChild(TSymbolReferenceSyntax.Create(T, O));
  If T.Expected('(') Then
  Begin
    T.Next;
    AddChild(TExpressionListSyntax.Create(T, O));
    If T.Expected(')') Then
      T.Next
    Else
      T.RaiseError('Expected ")"');
  End;
End;

Constructor TStamentSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  If T.Expected('if') Then
    AddChild(TIfStamentSyntax.Create(T, O))
  Else If T.Expected('for') Then
    AddChild(TForStamentSyntax.Create(T, O))
  Else If T.Expected('while') Then
    AddChild(TWhileStamentSyntax.Create(T, O))
  Else If T.Expected('repeat') Then
    AddChild(TRepeatStamentSyntax.Create(T, O))
  Else If T.Expected('case') Then
    AddChild(TCaseStamentSyntax.Create(T, O))
  Else If T.Expected('with') Then
    AddChild(TWithStamentSyntax.Create(T, O))
  Else If T.Expected('begin') Then
  Begin
    T.Next;
    AddChild(TCodeBlockSyntax.Create(T, O));
  End
  Else If T.Expected(tkIdent) Then
  Begin
    // Write('>>');
    T.Next;
    // Write('<<');
    If T.Expected(':=') Then
    Begin
      T.Back;
      AddChild(TAttribSyntax.Create(T, O));
    End
    Else
    Begin
      T.Back;
      AddChild(TCallSyntax.Create(T, O));
    End;
  End
  Else
    T.RaiseError('Expected command, attribution or call.');
End;

Constructor TCodeBlockSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  While Not(T.Expected('end')) Do
  Begin
    AddChild(TStamentSyntax.Create(T, O));
    If T.Expected(';') Then
      T.Next
    Else
      If T.Expected('end') Then
      Else
        T.RaiseError('Expected ";" or "end".');
  End;
  If T.Expected('end') Then
    T.Next
  Else
    T.RaiseError('Expected "end"');
End;

Constructor TParametersSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  Repeat
    T.Next;
    If T.Token.Value = 'var' Then
    Begin
      T.Next;
      AddChild(TVarParameterDecSyntax.Create(T, Self))
    End
    Else
      AddChild(TParameterDecSyntax.Create(T, Self));
  Until Not(T.Expected(';')) Or T.EOTk;
  If T.Expected(')') Then
    T.Next
  Else
    T.RaiseError('Expected ")"');
End;

Constructor TFuncSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  AddChild(TIdentifierSyntax.Create(T, O));
  If T.Token.Value = '(' Then
    AddChild(TParametersSyntax.Create(T, O));
  If T.Expected(':') Then
  Begin
    T.Next;
    AddChild(TTypeIdentifierSyntax.Create(T, O));
  End
  Else
    T.RaiseError('Expected ":"');
  If T.Expected(';') Then
    T.Next
  Else
    T.RaiseError('Expected ";"');
  While Not(T.Expected('begin')) Do
  Begin
    If T.Expected('var') Then
      AddChild(TVarSyntax.Create(T, O))
    Else If T.Expected('function') Then
      AddChild(TFuncSyntax.Create(T, O))
    Else If T.Expected('procedure') Then
      AddChild(TProcSyntax.Create(T, O))
    Else If T.Expected('javascript') Then
    Begin
      T.Next;
      If T.Expected('function') Then
        AddChild(TJavaFuncSyntax.Create(T, O))
      Else If T.Expected('procedure') Then
        AddChild(TJavaProcSyntax.Create(T, O))
      Else
        T.RaiseError('Expected "function" or "procedure".');
    End
    Else
      T.RaiseError('Expected Var, Function or Procedure Declaration.');
  End;
  If T.Expected('begin') Then
  Begin
    T.Next;
    AddChild(TCodeBlockSyntax.Create(T, O));
  End
  Else
    T.RaiseError('Expected "begin"');
  If T.Expected(';') Then
    T.Next
  Else
    T.RaiseError('Expected ";"');
End;

Constructor TProcSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  AddChild(TIdentifierSyntax.Create(T, O));
  If T.Token.Value = '(' Then
    AddChild(TParametersSyntax.Create(T, O));
  If T.Expected(';') Then
    T.Next
  Else
    T.RaiseError('Expected ";"');
  While Not(T.Expected('begin')) Do
  Begin
    If T.Expected('var') Then
      AddChild(TVarSyntax.Create(T, O))
    Else If T.Expected('function') Then
      AddChild(TFuncSyntax.Create(T, O))
    Else If T.Expected('procedure') Then
      AddChild(TProcSyntax.Create(T, O))
    Else If T.Expected('javascript') Then
    Begin
      T.Next;
      If T.Expected('function') Then
        AddChild(TJavaFuncSyntax.Create(T, O))
      Else If T.Expected('procedure') Then
        AddChild(TJavaProcSyntax.Create(T, O))
      Else
        T.RaiseError('Expected "function" or "procedure".');
    End
    Else
      T.RaiseError('Expected Var, Function or Procedure Declaration.');
  End;
  If T.Expected('begin') Then
  Begin
    T.Next;
    AddChild(TCodeBlockSyntax.Create(T, O));
  End
  Else
    T.RaiseError('Expected "begin"');
  If T.Expected(';') Then
    T.Next
  Else
    T.RaiseError('Expected ";"');
End;

Constructor TJavaBodySyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  If T.Expected(tkString) Then
    T.Next
  Else
    T.RaiseError('Expected string with javascript commands.');
End;

Constructor TJavaFuncSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  AddChild(TIdentifierSyntax.Create(T, O));
  If T.Token.Value = '(' Then
    AddChild(TParametersSyntax.Create(T, O));
  If T.Expected(':') Then
  Begin
    T.Next;
    AddChild(TTypeIdentifierSyntax.Create(T, O));
  End
  Else
    T.RaiseError('Expected ":"');
  If T.Expected(';') Then
    T.Next
  Else
    T.RaiseError('Expected ";"');
  While T.Expected(tkString) And Not(T.EOTk) Do
    AddChild(TJavaBodySyntax.Create(T, O));
  If T.Expected('end') Then
    T.Next
  Else
    T.RaiseError('Expected "end".');
  If T.Expected(';') Then
    T.Next
  Else
    T.RaiseError('Expected ";".');
End;

Constructor TJavaProcSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  AddChild(TIdentifierSyntax.Create(T, O));
  If T.Token.Value = '(' Then
    AddChild(TParametersSyntax.Create(T, O));
  If T.Expected(';') Then
    T.Next
  Else
    T.RaiseError('Expected ";"');
  While T.Expected(tkString) And Not(T.EOTk) Do
    AddChild(TJavaBodySyntax.Create(T, O));
  If T.Expected('end') Then
    T.Next
  Else
    T.RaiseError('Expected "end".');
  If T.Expected(';') Then
    T.Next
  Else
    T.RaiseError('Expected ";"');
End;

Constructor TProgramSyntax.Create(T : TTokenIterator; O : TTokenTreeElement);
Begin
  Inherited Create(T, O);
  If T.Expected('program') Then
    T.Next
  Else
    T.RaiseError('Expected "program"');
  AddChild(TIdentifierSyntax.Create(T, Self));
  If T.Expected(';') Then
    T.Next
  Else
    T.RaiseError('Expected ";"');
  While Not(T.Expected('begin')) Do
  Begin
    If T.Expected('var') Then
      AddChild(TVarSyntax.Create(T, O))
    Else If T.Expected('function') Then
      AddChild(TFuncSyntax.Create(T, O))
    Else If T.Expected('procedure') Then
      AddChild(TProcSyntax.Create(T, O))
    Else If T.Expected('javascript') Then
    Begin
      T.Next;
      If T.Expected('function') Then
        AddChild(TJavaFuncSyntax.Create(T, O))
      Else If T.Expected('procedure') Then
        AddChild(TJavaProcSyntax.Create(T, O))
      Else
        T.RaiseError('Expected "function" or "procedure".');
    End
    Else
      T.RaiseError('Expected Var, Function or Procedure Declaration.');
  End;
  If T.Expected('begin') Then
  Begin
    T.Next;
    AddChild(TCodeBlockSyntax.Create(T, O));
  End
  Else
    T.RaiseError('Expected "begin"');
  If T.Expected('.') Then
    T.Next
  Else
    T.RaiseError('Expected "."');
End;

End.
