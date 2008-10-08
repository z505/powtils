Unit CodeGen;

Interface

Uses Classes, SysUtils, Scanner, SyntaxTree, StreamHelper;

Procedure GenerateIdentifier(Dest : TStream; Ident : TIdentifierSyntax);
Procedure GenerateProgram(Dest : TStream; Prog : TProgramSyntax);

Implementation

Procedure GenerateIdentifier(Dest : TStream; Ident : TIdentifierSyntax);
Begin
  Write(Dest, Ident.Token.Value);
End;

Procedure GenerateProgram(Dest : TStream; Prog : TProgramSyntax);
Begin
  Write(Dest, '// ');
  Prog.FindElement(TIdentifierSyntax);
  GenerateIdentifier(Dest, );
  WriteLn(Dest, '');
End;

End.
