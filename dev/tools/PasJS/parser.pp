Unit Parser;

Interface

Uses Classes, SysUtils, Scanner, TokenIterator, StreamHelp;

Type
  EParser = Class(Exception);

Procedure Parser(Src : TTokenIterator; Dest : TStream);

Implementation

Procedure Parser(Src : TTokenIterator; Dest : TStream);

  Procedure EmitLn(St : String);
  Begin
    WriteLn(Dest, St);
  End;

  Procedure Emit(St : String);
  Begin
    Write(Dest, St);
  End;

  Function IsAddOp(St : String): Boolean;
  Begin
    IsAddOp := ((St = '+') Or (St = '-')) Or
      ((St = 'or') Or (St = 'xor'));
  End;

  Function IsMulOp(St : String): Boolean;
  Begin
    IsMulOp := ((St = '*') Or (St = '/')) Or
      (St = 'and');
  End;

  Function IsRelOp(St : String): Boolean;
  Begin
    IsRelOp := (St[1] In ['=', '>', '<']) Or
      ((St = '<>') Or (St = '>=')) Or
      (St = '<>'));
  End;

  Procedure RaiseError(St : String; Tk : TToken);
  Begin
    Raise EParser.Create(
      '("' +
      Tk.Value + '", "' +
      Tk.SrcName + '", ' +
      Tk.Row + ', ' +
      Tk.Col + '): ' +
      St
    );
  End;

  Procedure ParseSymbols; Foward;

  Procedure ParseVarDec;
  Begin
  End;

  Procedure ParseFuncDec;
  Begin
  End;

  Procedure ParseProcDec;
  Begin
  End;

  Procedure ParseSymbols;
  Begin
  End;

Begin
End;

End.
