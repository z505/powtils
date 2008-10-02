Unit Parser;

Interface

Uses Classes, SysUtils, Scanner, TokenIterator, StreamHelp, SyntaxTree;

Function Parser(Src : TTokenIterator): TTreeElementList;

Implementation

Function Parser(Src : TTokenIterator): TTreeElementList;

  Function ParseSymbol(Src : TTokenIterator; Owner : TRootTreeElement): TRootTreeElement; Forward;

  Function ParseVarDec(Src : TTokenIterator; Owner : TRootTreeElement): TRootTreeElement;
  Begin
    ParseVarDec := TVarSyntax.Create(Src, Owner);
  End;

  Function ParseFuncDec(Src : TTokenIterator; Owner : TRootTreeElement): TRootTreeElement;
  Begin
  End;

  Function ParseProcDec(Src : TTokenIterator; Owner : TRootTreeElement): TRootTreeElement;
  Begin
  End;

  Function ParseSymbol(Src : TTokenIterator; Owner : TRootTreeElement): TRootTreeElement;
  Begin
    If Src.Token.Value = 'var' Then
      ParseSymbol := ParseVarDec(Src, Owner)
    Else If Src.Token.Value = 'procedure' Then
      ParseSymbol := ParseProcDec(Src, Owner)
    Else If Src.Token.Value = 'function' Then
      ParseSymbol := ParseFuncDec(Src, Owner)
    Else
      Src.RaiseError('Expected Var, Procedure or Function declaration.');      
  End;

Var
  RootSyntax : TTreeElementList;

Begin
  Src.Start;
  RootSyntax := TProgramSyntax.Create(Src, Nil);
  While Not(Src.EOTk Or (Src.Token.Value = 'end')) Do
    RootSyntax.AddChild(ParseSymbol(Src, RootSyntax));
  Parser := RootSyntax;
End;

End.
