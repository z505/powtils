Unit SyntaxTree;

Interface
Uses TokenIterator, Scanner, SysUtils, Classes;

Type
  TRootTreeElement = Class;
  TRootTreeElement = Class
  Private
    fToken : TToken;
    fOwner : TRootTreeElement;
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
    Property Token : TToken Read fToken;
    Property Owner : TRootTreeElement Read fOwner Write fOwner;
  End;

  TTreeElementList = Class(TRootTreeElement)
  Private
    fChilds : Array Of TRootTreeElement;
    Function GetChild(Idx : LongInt): TRootTreeElement;
    Procedure SetChild(Idx : LongInt; Child : TRootTreeElement);
    Function Count: LongInt;
  Public
    Destructor Destroy; Override;
    Procedure AddChild(Child : TRootTreeElement);
    Property Childs[Idx : LongInt]: TRootTreeElement Read GetChild Write SetChild;
  End;

  TTerminalSyntax = Class(TRootTreeElement)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TIdentifierSyntax = Class(TRootTreeElement)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TIdentifierListSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TSymbolReferenceSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TFactorSyntax = Class;
  TTermSyntax = Class;

  TMulOperatorSyntax = Class(TTerminalSyntax);

  TSumOperatorSyntax = Class(TTerminalSyntax);

  TFactorSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;    

  TTermSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TPrefixedSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TUnaryMinusSyntax = Class(TPrefixedSyntax);

  TUnaryNotSyntax = Class(TPrefixedSyntax);

  TExpressionSyntax = Class(TFactorSyntax);

  TExpressionListSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TVarDecSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TVarSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TStamentSyntax = Class;
  TCodeBlockSyntax = Class;

  TForFinishExpressionSyntax = Class(TExpressionSyntax);

  TIfStamentSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TForDirectionSyntax = Class(TRootTreeElement)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TForStamentSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TWhileStamentSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TRepeatStamentSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TCaseEntrySyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TCaseStamentSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TWithStamentSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TAttribSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TCallSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TStamentSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TElseStamentSyntax = Class(TStamentSyntax);

  TCodeBlockSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TVarParameterDecSyntax = Class(TVarDecSyntax);

  TParameterDecSyntax = Class(TVarDecSyntax);

  TParametersSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TReturnTypeIdentifierSyntax = Class(TIdentifierSyntax);

  TFuncSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TProcSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TProgramSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

Implementation

Constructor TRootTreeElement.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  If Assigned(T) Then
    fToken := T.Token;
  fOwner := O;
End;

Function TTreeElementList.GetChild(Idx : LongInt): TRootTreeElement;
Begin
  If Idx In [Low(fChilds), High(fChilds)] Then
    GetChild := fChilds[Idx]
  Else
    Raise Exception.Create('Array index out of bounds.');
End;

Procedure TTreeElementList.SetChild(Idx : LongInt; Child : TRootTreeElement);
Begin
  If Idx In [Low(fChilds), High(fChilds)] Then
    fChilds[Idx] := Child
  Else
    Raise Exception.Create('Array index out of bounds.');
End;

Function TTreeElementList.Count: LongInt;
Begin
  Count := Length(fChilds);
End;

Procedure TTreeElementList.AddChild(Child : TRootTreeElement);
Begin
  SetLength(fChilds, Length(fChilds) + 1);
  fChilds[High(fChilds)] := Child;
//  Child.Owner := Self;
End;

Destructor TTreeElementList.Destroy;
Var
  Ctrl : LongInt;
Begin
  For Ctrl := Low(fChilds) To High(fChilds) Do
    fChilds[Ctrl].Free;
  Inherited Destroy;
End;

Constructor TTerminalSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
End;

Constructor TIdentifierSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  If T.Expected(tkIdent) Then
  Begin
    Inherited Create(T, O);
    T.Next;
  End
  Else
    T.RaiseError('Expected identifier.');
End;

Constructor TIdentifierListSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Inherited Create(T, O);
  AddChild(TIdentifierSyntax.Create(T, Self));
  While T.Expected(',') Do
  Begin
    T.Next;
    AddChild(TIdentifierSyntax.Create(T, Self));
  End;
End;

Constructor TSymbolReferenceSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  If T.Expected('.') Then
  Begin
    T.Next;
    AddChild(TSymbolReferenceSyntax.Create(T, O));
  End;
End;

Constructor TFactorSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Inherited Create(T, O);
  AddChild(TTermSyntax.Create(T, O));
  If T.IsMulop Then
  Begin
    AddChild(TMulOperatorSyntax.Create(T, O));
    AddChild(TFactorSyntax.Create(T, O));
  End;
End;

Constructor TTermSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
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
  Else
    AddChild(TTerminalSyntax.Create(T, O));
  If T.IsSumOp Then
  Begin
    AddChild(TSumOperatorSyntax.Create(T, O));
    AddChild(TTermSyntax.Create(T, O));
  End;
End;

Constructor TPrefixedSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  AddChild(TTerminalSyntax.Create(T, O));
End;

Constructor TExpressionListSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Repeat
    AddChild(TExpressionSyntax.Create(T, O));
  Until T.EOTk Or Not(T.Expected(','));
End;

Constructor TVarDecSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Inherited Create(T, O);
  AddChild(TIdentifierListSyntax.Create(T, O));
  If T.Expected(':') Then
  Begin
    T.Next;
    AddChild(TIdentifierSyntax.Create(T, O));
  End
  Else
    T.RaiseError('Expected ":"');
End;

Constructor TVarSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
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

Constructor TIfStamentSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
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

Constructor TForDirectionSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  If T.Expected('to') Or T.Expected('downto') Then
  Begin
    Inherited Create(T, O);
    T.Next;
  End
  Else
    T.RaiseError('Expected "to" or "downto".');
End;

Constructor TForStamentSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
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
  If T.Expected('to') Then
  Begin
    T.Next;
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

Constructor TWhileStamentSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
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

Constructor TRepeatStamentSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
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

Constructor TCaseEntrySyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Inherited Create(T, O);
  AddChild(TTerminalSyntax.Create(T, O));
  If T.Expected(':') Then
  Begin
    T.Next;
    AddChild(TStamentSyntax.Create(T, O));
  End
  Else
    T.RaiseError('Expected ":"');
End;

Constructor TCaseStamentSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  AddChild(TIdentifierSyntax.Create(T, O));
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

Constructor TWithStamentSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
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

Constructor TAttribSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Inherited Create(T, O);
  // Write('>>>', T.Token.Value, '<<<');
  AddChild(TIdentifierSyntax.Create(T, O));
  If T.Expected(':=') Then
  Begin
    T.Next;
    AddChild(TExpressionSyntax.Create(T, O));
  End
  Else
    T.RaiseError('Expected ":="');
End;

Constructor TCallSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Inherited Create(T, O);
  AddChild(TSymbolReferenceSyntax.Create(T, O));
  If T.Expected('(') Then
  Begin
    T.Next;
    AddChild(TExpressionListSyntax.Create(T, O));
  End;
  If T.Expected(')') Then
    T.Next
  Else
    T.RaiseError('Expected ")"');
End;

Constructor TStamentSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
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

Constructor TCodeBlockSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
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

Constructor TParametersSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
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

Constructor TFuncSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  AddChild(TIdentifierSyntax.Create(T, O));
  If T.Token.Value = '(' Then
    AddChild(TParametersSyntax.Create(T, O));
  If T.Expected(':') Then
  Begin
    T.Next;
    AddChild(TReturnTypeIdentifierSyntax.Create(T, O));
  End
  Else
    T.RaiseError('Expected ":"');
  If T.Expected(';') Then
    T.Next
  Else
    T.RaiseError('Expected ";"');
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

Constructor TProcSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
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

Constructor TProgramSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
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
