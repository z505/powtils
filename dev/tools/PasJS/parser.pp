Unit Parser;

Interface

Uses TokenIterator, SyntaxTree;

Function Parser(Src : TTokenIterator): TTokenTreeElement;

Implementation

Function Parser(Src : TTokenIterator): TTokenTreeElement;
Begin
  Src.Start;
  Parser := TProgramSyntax.Create(Src, Nil);;
End;

End.
