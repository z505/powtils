Unit Expressions;

Interface

Type
  TExpressionType = (
    terLiteral,
    terReference,
    terFuncCall,
    terExpression
  );

  TExpression = Class
  Private
    fKind : TExpressionType;
  Public
    Constructor Create(K : TExpressionType);
    Procedure CodeGen(Dest : TStream); Virtual; Abstract;
    Function EvaluateTo: String; Virtual; Abstract;
    Property Kind : TExpressionType Read fKind Write fKind;
  End;

  TLiteral = Class(TExpression)
  Private
    fValue,
    fType  : String;
  Public
    Constructor Create(V, T : String);
    Procedure CodeGen(Dest : TStream); Override;
    Property Value : String Read fValue Write fValue;
    Property LType : String Read fType Write fType;
  End;

  TReference = Class(TExpression)
  Private
    fIdent : String;
  Public
    Constructor Create(Ref : String);
    Procedure CodeGen(Dest : TStream); Override;
    Property Identifier : String Read fIdent Write fIdent;
  End;

  TUnaryType = (
    utMinus,
    utNot
  );

  TUnaryOperator = Class(TExpression)
  Private
    fUnary : TUnaryType;
    fTerm  : TExpression;
  Public
    Constructor Create(UT : TUnaryType; T : TExpression);
  End;

  TUnaryMinus = Class(TUnaryOperator)
  Public
    Constructor Create(T : TExpression);
    Procedure CodeGen(Dest : TStream);
  End;

  TUnaryNot = Class(TUnaryOperator)
  Public
    Constructor Create(T : TExpression);
    Procedure CodeGen(Dest : TStream);
  End;

  TBinaryType = (
    btPlus,
    btMinus,
    btMul,
    btDiv,
    btAnd,
    btOr,
    btXOr,
    btShR,
    btShL,
    btEqual,
    btGreater,
    btSmaller,
    btGrEqual,
    btSmEqual
  End;

  TBinaryOperator = Class(TExpression)
  Private
    fBinary : TBinaryType;
    fLeft,
    fRight  : TExpression;
  Public
    Constructor Create(BT : TBinaryType; LH, RH : TExpression);
  End;

  TBinaryPlus = Class(TBinaryOperator)
  Public
    Constructor Create(LH, RH : TExpression);
    Procedure CodeGen(Dest : TStream);
  End;

  TBinaryMinus = Class(TBinaryOperator)
  Public
    Constructor Create(LH, RH : TExpression);
    Procedure CodeGen(Dest : TStream);
  End;

  TBinaryMul = Class(TBinaryOperator)
  Public
    Constructor Create(LH, RH : TExpression);
    Procedure CodeGen(Dest : TStream);
  End;

  TBinaryDiv = Class(TBinaryOperator)
  Public
    Constructor Create(LH, RH : TExpression);
    Procedure CodeGen(Dest : TStream);
  End;

  TBinaryAnd = Class(TBinaryOperator)
  Public
    Constructor Create(LH, RH : TExpression);
    Procedure CodeGen(Dest : TStream);
  End;

  TBinaryOr = Class(TBinaryOperator)
  Public
    Constructor Create(LH, RH : TExpression);
    Procedure CodeGen(Dest : TStream);
  End;

  TBinaryXOr = Class(TBinaryOperator)
  Public
    Constructor Create(LH, RH : TExpression);
    Procedure CodeGen(Dest : TStream);
  End;

  TBinaryShR = Class(TBinaryOperator)
  Public
    Constructor Create(LH, RH : TExpression);
    Procedure CodeGen(Dest : TStream);
  End;

  TBinaryShL = Class(TBinaryOperator)
  Public
    Constructor Create(LH, RH : TExpression);
    Procedure CodeGen(Dest : TStream);
  End;

Implementation

End.
