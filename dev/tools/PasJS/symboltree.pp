Unit SymbolTree;

Interface

Uses Classes, Sysutils, Tree, SyntaxTree, Scanner;

Type
  ESymbol = Class(Exception);

  TPassType = (ptByValue, ptByReference);

  TSymbolTreeElement = Class;
  TSymbolTreeElement = Class(TTreeElement)
  Private
    fToken    : TToken;
    fName,
    fKind     : String;
    fPassage  : TPassType;
    fAttached : TCodeBlockSyntax;
    fVisible  : TStringList;
    fJava     : String;
  Public
    Constructor Create(T : TToken; N, K : String; O : TSymbolTreeElement); Overload;
    Constructor Create(T : TToken; N, K : String; PT : TPassType; O : TSymbolTreeElement); Overload;
    Destructor Destroy; Override;
    Procedure BuildVisibility;
    Function IdentifierCallBack(T : TToken; Name : String): TTokenKind;
    Function FindRootType(St : String): TTokenKind;
    Property Name : String Read fName Write fName;
    Property Kind : String Read fKind Write fKind;
    Property Passage : TPassType Read fPassage Write fPassage;
    Property Attached: TCodeBlockSyntax Read fAttached Write fAttached;
    Property Token: TToken Read fToken Write fToken;
    Property Visible: TStringList Read fVisible;
    Property Java: String Read fJava Write fJava;
  End;

  TSymbolVar = Class(TSymbolTreeElement);

  TSymbolParameter = Class(TSymbolTreeElement);

  TSymbolWithAttached = Class(TSymbolTreeElement)
  Public
    Constructor Create(T : TToken; Src : TTokenTreeElement; O : TSymbolTreeElement);
  End;

  TSymbolWithParameters = Class(TSymbolWithAttached)
  Public
    Constructor Create(T : TToken; Src : TTokenTreeElement; O : TSymbolTreeElement);
  End;

  TSymbolFunction = Class(TSymbolWithParameters)
  Public
    Constructor Create(T : TToken; Src : TTokenTreeElement; O : TSymbolTreeElement);
  End;
  
  TSymbolProcedure = Class(TSymbolWithParameters);

  TSymbolJavaFunction = Class(TSymbolTreeElement)
  Public
    Constructor Create(T : TToken; Src : TTokenTreeElement; O : TSymbolTreeElement);
  End;

  TSymbolJavaProcedure = Class(TSymbolTreeElement) 
  Public
    Constructor Create(T : TToken; Src : TTokenTreeElement; O : TSymbolTreeElement);
  End;
  
  TSymbolProgram = Class(TSymbolWithAttached)
  Public
    Constructor Create(Src : TTokenTreeElement);
  End;

Implementation

Constructor TSymbolTreeElement.Create(T : TToken; N, K : String; O : TSymbolTreeElement);
Begin
  Inherited Create(O);
  fToken := T;
  fName  := N;
  fKind  := K;
  fVisible := TStringList.Create;
  fVisible.Sorted := True;
  fVisible.Duplicates := DupError;
  Start;
End;

Constructor TSymbolTreeElement.Create(T : TToken; N, K : String; PT : TPassType; O : TSymbolTreeElement);
Begin
  Inherited Create(O);
  fToken   := T;
  fName    := N;
  fKind    := K;
  fPassage := PT;
  fVisible := TStringList.Create;
  fVisible.Sorted := True;
  fVisible.Duplicates := DupError;
  Start;
End;

Destructor TSymbolTreeElement.Destroy;
Begin
  fVisible.Free;
  Inherited Destroy;
End;

Procedure TSymbolTreeElement.BuildVisibility;
Begin
  If (Self Is TSymbolWithAttached) Then
  Begin
    If Assigned(Owner) Then
      fVisible.AddStrings((Owner As TSymbolTreeElement).Visible);
    Start;
    If Count > 0 Then
      Repeat
        Try
          fVisible.Objects[fVisible.Add((Child As TSymbolTreeElement).Name)] := Child;
        Except
          On E: EStringListError Do
            RaiseError('Duplicate Identifier : ', (Child As TSymbolTreeElement).Token);
        End;
        Next;
      Until EOE;
    Start;
    If Count > 0 Then
      Repeat
        If (Child Is TSymbolWithAttached) Then
          (Child As TSymbolWithAttached).BuildVisibility;
        Next;
      Until EOE;
  End;
End;

Function TSymbolTreeElement.IdentifierCallBack(T : TToken; Name : String): TTokenKind;
Begin
  If fVisible.IndexOf(Name) > -1 Then
    IdentifierCallBack :=
    FindRootType(
      (fVisible.Objects[fVisible.IndexOf(Name)] As TSymbolTreeElement).Kind
    )
  Else
    RaiseError('Unknown identifier : ' + Name, T);
End;

// TODO : Prepare to allow user defined types
Function TSymbolTreeElement.FindRootType(St : String): TTokenKind;
Begin
  If St = 'integer' Then
    FindRootType := tkNumber
  Else If St = 'float' Then
    FindRootType := tkFloat
  Else If St = 'string' Then
    FindRootType := tkString
  Else If St = 'char' Then
    FindRootType := tkChar
  Else If St = 'boolean' Then
    FindRootType := tkBoolean
  Else
    FindRootType := tkUnknown;
End;

Procedure DeclareABunch(Src : TIdentifierListSyntax; K : String; O : TSymbolTreeElement);
Begin
  Src.Start;
  Repeat
    If Src.Child Is TIdentifierSyntax Then
      O.AddChild(
        TSymbolVar.Create(
          (Src.Child As TTokenTreeElement).Token,
          (Src.Child As TTokenTreeElement).Token.Value,
          K,
          O)
        );
    Src.Next;
  Until Src.EOE;
End;

Procedure ParseVarRow(Src : TVarDecSyntax; O : TSymbolTreeElement);
Var
  TmpKind : String;
Begin
  If Src.FindElement(TTypeIdentifierSyntax) Then
    TmpKind := (Src.Child As TTokenTreeElement).Token.Value
  Else
    Raise Exception.Create('Internal error, cant find var type.');
  Src.Start;
  If Src.FindElement(TIdentifierListSyntax) Then
    DeclareABunch((Src.Child As TIdentifierListSyntax), TmpKind, O)
  Else
    Raise Exception.Create('Internal error, cant find var list.');
End;

Procedure DeclareAllVars(Src : TVarSyntax; O : TSymbolTreeElement);
Begin
  Src.Start;
  Repeat
    If Src.Child Is TVarDecSyntax Then
      ParseVarRow((Src.Child As TVarDecSyntax), O);
    Src.Next;
  Until Src.EOE;
End;

Procedure ParseSubSymbols(Src : TTokenTreeElement; O: TSymbolTreeElement);
Begin
  Src.Start;
  Repeat
    If Src.Child Is TVarSyntax Then
      DeclareAllVars((Src.Child As TVarSyntax), O)
    Else If Src.Child Is TFuncSyntax Then
      O.AddChild(TSymbolFunction.Create((Src.Child As TTokenTreeElement).Token, (Src.Child As TTokenTreeElement), O))
    Else If Src.Child Is TProcSyntax Then
      O.AddChild(TSymbolProcedure.Create((Src.Child As TTokenTreeElement).Token, (Src.Child As TTokenTreeElement), O))
    Else If Src.Child Is TJavaFuncSyntax Then
      O.AddChild(TSymbolJavaFunction.Create((Src.Child As TTokenTreeElement).Token, (Src.Child As TTokenTreeElement), O))
    Else If Src.Child Is TJavaProcSyntax Then
      O.AddChild(TSymbolJavaProcedure.Create((Src.Child As TTokenTreeElement).Token, (Src.Child As TTokenTreeElement), O));
    Src.Next;
  Until Src.EOE;
End;

Procedure DeclareABunchOfParameters(Src : TIdentifierListSyntax; K : String; O : TSymbolTreeElement; Pass : TPassType);
Begin
  Src.Start;
  Repeat
    O.AddChild(
      TSymbolParameter.Create(
        (Src.Child As TTokenTreeElement).Token,
        (Src.Child As TTokenTreeElement).Token.Value,
        K,
        Pass,
        O)
      );
    Src.Next;
  Until Src.EOE;
End;

Procedure DeclareARowOFVarParameters(Src : TVarParameterDecSyntax; O: TSymbolTreeElement);
Var
  TmpKind : String;
Begin
  If Src.FindElement(TTypeIdentifierSyntax) Then
    TmpKind := (Src.Child As TTokenTreeElement).Token.Value
  Else
    Raise Exception.Create('Internal error, cant find var type.');
  Src.Start;
  If Src.FindElement(TIdentifierListSyntax) Then
    DeclareABunchOfParameters((Src.Child As TIdentifierListSyntax), TmpKind, O, ptByReference)
  Else
    Raise Exception.Create('Internal error, cant find var list.');
End;

Procedure DeclareARowOfParameters(Src : TParameterDecSyntax; O: TSymbolTreeElement);
Var
  TmpKind : String;
Begin
  If Src.FindElement(TTypeIdentifierSyntax) Then
    TmpKind := (Src.Child As TTokenTreeElement).Token.Value
  Else
    Raise Exception.Create('Internal error, cant find var type.');
  Src.Start;
  If Src.FindElement(TIdentifierListSyntax) Then
    DeclareABunchOfParameters((Src.Child As TIdentifierListSyntax), TmpKind, O, ptByValue)
  Else
    Raise Exception.Create('Internal error, cant find var list.');
End;

Procedure DeclareParameters(Src : TTokenTreeElement; O: TSymbolTreeElement);
Begin
  Src.Start;
  Repeat
    If Src.Child Is TVarParameterDecSyntax Then
      DeclareARowOfVarParameters((Src.Child As TVarParameterDecSyntax), O)
    Else If Src.Child Is TParameterDecSyntax Then
      DeclareARowOfParameters((Src.Child As TParameterDecSyntax), O);
    Src.Next;
  Until Src.EOE;
End;

Constructor TSymbolWithAttached.Create(T : TToken; Src : TTokenTreeElement; O : TSymbolTreeElement);
Begin
  Src.Start;
  If Src.FindElement(TIdentifierSyntax) Then
    fName := (Src.Child As TTokenTreeElement).Token.Value
  Else
    Raise Exception.Create('Internal error, cant find symbol name.');
  Inherited Create((Src.Child As TTokenTreeElement).Token, fName, 'void', O);
  Src.Start;
  ParseSubSymbols((Src As TTokenTreeElement), Self);
  Src.Start;
  If Src.FindElement(TCodeBlockSyntax) Then
    Self.Attached := (Src.Child As TCodeBlockSyntax)
  Else
    Raise Exception.Create('Internal error, cant find code block for symbol.');
  (Self.Attached As TCodeBlockSyntax).PropagateIdentifierCallBack(
    Self.IdentifierCallBack
    );
End;

Constructor TSymbolWithParameters.Create(T : TToken; Src : TTokenTreeElement; O : TSymbolTreeElement);
Begin
  Inherited Create(T, Src, O);
  Src.Start;
  If Src.FindElement(TParametersSyntax) Then
      DeclareParameters((Src.Child As TTokenTreeElement), Self);
End;

Constructor TSymbolFunction.Create(T : TToken; Src : TTokenTreeElement; O : TSymbolTreeElement);
Begin
  Inherited Create(T, Src, O);
  If Src.FindElement(TTypeIdentifierSyntax) Then
    fKind := (Src.Child As TTokenTreeElement).Token.Value
  Else
    Raise Exception.Create('Internal error, cant find function return type.');
End;

Constructor TSymbolJavaFunction.Create(T : TToken; Src : TTokenTreeElement; O : TSymbolTreeElement);
Begin
  Src.Start;
  If Src.FindElement(TIdentifierSyntax) Then
    fName := (Src.Child As TTokenTreeElement).Token.Value
  Else
    Raise Exception.Create('Internal error, cant find symbol name.');
  Inherited Create((Src.Child As TTokenTreeElement).Token, fName, 'void', O);
  Src.Start;
  If Src.FindElement(TParametersSyntax) Then
      DeclareParameters((Src.Child As TTokenTreeElement), Self);
  Src.Start;
  If Src.FindElement(TTypeIdentifierSyntax) Then
    fKind := (Src.Child As TTokenTreeElement).Token.Value
  Else
    Raise Exception.Create('Internal error, cant find function return type.');
  Src.Start;
  If Not(Src.FindElement(TJavaBodySyntax)) Then
    Raise Exception.Create('Internal error, cant find javascript equivalent.');
  Src.Start;
  While Not(Src.EOE) Do
  Begin
    If Src.Child Is TJavaBodySyntax Then
      fJava := fJava + #13#10 + (Src.Child As TJavaBodySyntax).Token.Value;
    Src.Next;
  End;
End;

Constructor TSymbolJavaProcedure.Create(T : TToken; Src : TTokenTreeElement; O : TSymbolTreeElement);
Begin
  Src.Start;
  If Src.FindElement(TIdentifierSyntax) Then
    fName := (Src.Child As TTokenTreeElement).Token.Value
  Else
    Raise Exception.Create('Internal error, cant find symbol name.');
  Inherited Create((Src.Child As TTokenTreeElement).Token, fName, 'void', O);
  Src.Start;
  If Src.FindElement(TParametersSyntax) Then
      DeclareParameters((Src.Child As TTokenTreeElement), Self);
  Src.Start;
  If Not(Src.FindElement(TJavaBodySyntax)) Then
    Raise Exception.Create('Internal error, cant find javascript equivalent.');
  Src.Start;
  While Not(Src.EOE) Do
  Begin
    If Src.Child Is TJavaBodySyntax Then
      fJava := fJava + #13#10 + (Src.Child As TJavaBodySyntax).Token.Value;
    Src.Next;
  End;
End;

Constructor TSymbolProgram.Create(Src : TTokenTreeElement);
Begin
  Src.Start;
  If Src.FindElement(TIdentifierSyntax) Then
    Inherited Create((Src.Child As TTokenTreeElement).Token, Src, Nil)
  Else
    Raise Exception.Create('Internal error, cant find program name.');
End;

End.
