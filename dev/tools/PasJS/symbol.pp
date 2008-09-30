Unit Symbol;

Interface

Uses Classes, SysUtils, StreamHelp, CodeBlocks;

Type
  TSymbolKind = (
    skVar,
    skFunc,
    skProc,
    skParam
  );

  TSymbol = Class
  Private
    fName : String;
    fKind : TSymbolKind;
  Public
    Constructor Create(N : String; K : TSymbolKind);
    Procedure CodeGen(Dest : TStream); Virtual; Abstract;
    Property Name : String Read fName;
    Property Kind : TSymbolKind Read fKind;
  End;

  TSymbolList = Array Of TSymbol;

  TVarSymbol = Class(TSymbol)
  Private
    fType : String;
  Public
    Constructor Create(N, T : String);
    Procedure CodeGen(Dest : TStream); Override;
    Property VarType : String Read fType;
  End;

  TParamSymbol = Class(TSymbol)
  Private
    fType  : String;
    fOrder : LongInt;
  Public
    Constructor Create(N, T : String);
    Procedure CodeGen(Dest : TStream); Override;
    Property VarType : String Read fType;
    Property Order   : LongInt Read fOrder;
  End;

  TFuncSymbol = Class(TSymbol)
  Private
    fType       : String;
    fCodeBlock  : TCodeBlock;
    fSymbol     : TSymbolList;
  Public
    Constructor Create(N : String);
    Procedure CodeGen(Dest : TStream); Override;
    Property VarType : String Read fType;
    Property Symbols : TSymbolList Read fSymbol Write fSymbol;
    Property Code : TCodeBlock Read fCodeBlock Write fCodeBlock;
  End;

  TProcSymbol = Class(TFuncSymbol)
  Private
    fParameters : TSymbolList;
    fCodeBlock  : TCodeBlock;
    fSymbol     : TSymbolList;
  Public
    Constructor Create(N : String);
    Procedure CodeGen(Dest : TStream); Override;
    Property Symbols : TSymbolList Read fSymbol Write fSymbol;
    Property Code : TCodeBlock Read fCodeBlock Write fCodeBlock;
  End;

Procedure AddSymbol(Var Dest : TSymbolList; Sym : TSymbol);

Implementation

Procedure AddSymbol(Var Dest : TSymbolList; Sym : TSymbol);
Begin
  SetLength(Dest, Length(Dest) + 1);
  Dest[High(Dest)] := Sym;
End;

Constructor TSymbol.Create(N : String; K : TSymbolKind);
Begin
  fName := N;
  fKind := K;
End;

Constructor TVarSymbol.Create(N, T : String);
Begin
  Inherited Create(N, skVar);
  fType := T;
End;

Procedure TVarSymbol.CodeGen(Dest : TStream);
Begin
  Write(Dest, 'var ');
  WriteLn(Dest, fName);
End;

Constructor TParamSymbol.Create(N, T : String);
Begin
  Inherited Create(N, skParam);
  fType := T;
End;

Procedure TParamSymbol.CodeGen(Dest : TStream);
Begin
  Write(Dest, fName);
End;

Constructor TFuncSymbol.Create(N : String);
Begin
  Inherited Create(N, skFunc);
End;

Procedure TFuncSymbol.CodeGen(Dest : TStream);
Begin
End;

Constructor TProcSymbol.Create(N : String);
Begin
  Inherited Create(N, skProc);
End;

Procedure TProcSymbol.CodeGen(Dest : TStream);
Begin
End;

End.
