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

  TFactorSyntax = Class;
  TTermSyntax = Class;

  TFactorSyntax = Class(TTreeElementList)
  Private
    fOperator    : TTerminalSyntax;
    fLeftTerm    : TTermSyntax;
    fRightFactor : TFactorSyntax;
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;    

  TTermSyntax = Class(TTreeElementList)
  Private
    fOperator  : TTerminalSyntax;
    fLeftTerm  : TRootTreeElement;
    fRightTerm : TTermSyntax;
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TUnaryMinusSyntax = Class(TRootTreeElement)
  Private
    fNegated : TTerminalSyntax;
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TUnaryNotSyntax = Class(TRootTreeElement)
  Private
    fNegated : TTerminalSyntax;
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TExpressionSyntax = Class(TRootTreeElement)
  Private
    fFactor : TFactorSyntax;
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TExpressionListSyntax = Class(TTreeElementList)
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TVarDecSyntax = Class(TRootTreeElement)
  Private
    fType      : TIdentifierSyntax;
    fVariables : TIdentifierListSyntax;
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

  TIfStamentSyntax = Class(TRootTreeElement)
  Private
    fConditional : TExpressionSyntax;
    fIfTrue      : TStamentSyntax;
    fIfFalse     : TStamentSyntax;
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

  TForStamentSyntax = Class(TRootTreeElement)
  Private
    fVariable    : TIdentifierSyntax;
    fSExpression : TExpressionSyntax;
    fDirection   : TForDirectionSyntax;
    fFExpression : TExpressionSyntax;
    fStament     : TStamentSyntax;
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TWhileStamentSyntax = Class(TRootTreeElement)
  Private
    fCondition : TExpressionSyntax;
    fStament   : TStamentSyntax;
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TRepeatStamentSyntax = Class(TRootTreeElement)
  Private
    fCondition : TExpressionSyntax;
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TCaseEntrySyntax = Class(TRootTreeElement)
  Private
    fExpression : TExpressionSyntax;
    fStament    : TStamentSyntax;
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TCaseStamentSyntax = Class(TRootTreeElement)
  Private
    fVariable    : TIdentifierSyntax;
    fElseStament : TStamentSyntax;
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TWithStamentSyntax = Class(TRootTreeElement)
  Private
    fVariable : TIdentifierSyntax;
    fStament  : TStamentSyntax;
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TAttribSyntax = Class(TRootTreeElement)
  Private
    fVariable   : TIdentifierSyntax;
    fExpression : TExpressionSyntax;
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TCallSyntax = Class(TRootTreeElement)
  Private
    fName       : TIdentifierSyntax;
    fParameters : TExpressionListSyntax;
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TStamentSyntax = Class(TRootTreeElement)
  Private
    fStament : TRootTreeElement;
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

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

  TFuncSyntax = Class(TTreeElementList)
  Private
    fName       : TIdentifierSyntax;
    fResultType : TIdentifierSyntax;
    fParameters : TParametersSyntax;
    fCodeBlock  : TCodeBlockSyntax;
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TProcSyntax = Class(TTreeElementList)
  Private
    fName       : TIdentifierSyntax;
    fParameters : TParametersSyntax;
    fCodeBlock  : TCodeBlockSyntax;
  Public
    Constructor Create(
      T : TTokenIterator;
      O : TRootTreeElement
    );
  End;

  TProgramSyntax = Class(TTreeElementList)
  Private
    fName      : TIdentifierSyntax;
    fCodeBlock : TCodeBlockSyntax;
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

Constructor TFactorSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Inherited Create(T, O);
  fLeftTerm := TTermSyntax.Create(T, O);
  If T.IsMulop Then
  Begin
    fOperator  := TTerminalSyntax.Create(T, O);
    fRightFactor := TFactorSyntax.Create(T, O);
  End
  Else
  Begin
    fOperator  := Nil;
    fRightFactor := Nil;
  End;
End;

Constructor TTermSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Inherited Create(T, O);
  If T.Expected(tkIdent) Then
  Begin
    T.Next;
    If T.Expected('(') Then
    Begin
      T.Back;
      fLeftTerm := TCallSyntax.Create(T, O);
    End
    Else
    Begin
      T.Back;
      fLeftTerm := TIdentifierSyntax.Create(T, O);
    End;
  End
  Else If T.Token.Value = '-' Then
    fLeftTerm := TUnaryMinusSyntax.Create(T, O)
  Else If T.Token.Value = 'not' Then
    fLeftTerm := TUnaryNotSyntax.Create(T, O)
  Else If T.Token.Value = '(' Then
  Begin
    T.Next;
    fLeftTerm := TExpressionSyntax.Create(T, O);
    If T.Expected(')') Then
      T.Next
    Else
      T.RaiseError('Expected ")"');
  End
  Else
    fLeftTerm := TTerminalSyntax.Create(T, O);
  If T.IsSumOp Then
  Begin
    fOperator  := TTerminalSyntax.Create(T, O);
    fRightTerm := TTermSyntax.Create(T, O);
  End
  Else
  Begin
    fOperator  := Nil;
    fRightTerm := Nil;
  End;
End;

Constructor TVarDecSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Inherited Create(T, O);
  fVariables := TIdentifierListSyntax.Create(T, O);
  If T.Expected(':') Then
  Begin
    T.Next;
    fType := TIdentifierSyntax.Create(T, O);
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

Constructor TStamentSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Var
  TempToken : TToken;
Begin
  Inherited Create(T, O);
  If T.Expected('if') Then
    fStament := TIfStamentSyntax.Create(T, O)
  Else If T.Expected('for') Then
    fStament := TForStamentSyntax.Create(T, O)
  Else If T.Expected('while') Then
    fStament := TWhileStamentSyntax.Create(T, O)
  Else If T.Expected('repeat') Then
    fStament := TRepeatStamentSyntax.Create(T, O)
  Else If T.Expected('case') Then
    fStament := TCaseStamentSyntax.Create(T, O)
  Else If T.Expected('with') Then
    fStament := TWithStamentSyntax.Create(T, O)
  Else If T.Expected('begin') Then
  Begin
    T.Next;
    fStament := TCodeBlockSyntax.Create(T, O);
  End
  Else If T.Expected(tkIdent) Then
  Begin
    T.Next;
    If T.Expected(':=') Then
    Begin
      T.Back;
      fStament := TAttribSyntax.Create(T, O);
    End
    Else
    Begin
      T.Back;
      fStament := TCallSyntax.Create(T, O);
    End;
  End;
End;

Constructor TCodeBlockSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Inherited Create(T, O);
  While Not(T.Expected('end')) Do
    AddChild(TStamentSyntax.Create(T, O));
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
  fName := TIdentifierSyntax.Create(T, O);
  If T.Token.Value = '(' Then
    fParameters := TParametersSyntax.Create(T, O);
  If T.Expected(':') Then
  Begin
    T.Next;
    fResultType := TIdentifierSyntax.Create(T, O);
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
    fCodeBlock  := TCodeBlockSyntax.Create(T, O);
  End
  Else
    T.RaiseError('Expected "begin"');
End;

Constructor TProcSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Inherited Create(T, O);
  T.Next;
  fName := TIdentifierSyntax.Create(T, O);
  If T.Token.Value = '(' Then
    fParameters := TParametersSyntax.Create(T, O);
  If T.Expected(';') Then
    T.Next
  Else
    T.RaiseError('Expected ";"');
  If T.Expected('begin') Then
  Begin
    T.Next;
    fCodeBlock  := TCodeBlockSyntax.Create(T, O);
  End
  Else
    T.RaiseError('Expected "begin"');
End;

Constructor TProgramSyntax.Create(T : TTokenIterator; O : TRootTreeElement);
Begin
  Inherited Create(T, O);
  If T.Expected('program') Then
    T.Next
  Else
    T.RaiseError('Expected "program"');
  fName := TIdentifierSyntax.Create(T, Self);
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
