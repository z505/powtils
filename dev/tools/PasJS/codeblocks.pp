Unit CodeBlocks;

Interface

Uses
  Classes, SysUtils, StreamHelp, Expressions;

Type
  TStamentType = (
    sttNull,
    sttAttrib,
    sttCall,
    sttIf,
    sttFor,
    sttWhile,
    sttRepeat,
    sttCase,
    sttBlock
  );

  TStament = Class;

  TStamentList = Array Of TStament;

  TStament = Class
  Private
    fKind : TStamentType;
  Public
    Constructor Create(K : TStamentType);
    Procedure CodeGen(Dest : TStream); Virtual; Abstract;
    Property Kind : TStamentType Read fKind;
  End;

  TNullStament = Class(TStament)
  Private
  Public
    Constructor Create;
    Procedure CodeGen(Dest : TStream); Override;
  End;

  TAttribStament = Class(TStament)
  Private
    fIdentifier : String;
    fExpression : TExpression;
  Public
    Constructor Create(I : String; Exp : TExpression);
    Procedure CodeGen(Dest : TStream); Override;
    Property Ident : String Read fIdentifier;
    Property Expression : TExpression Read fExpression;
  End;

  TCallStament = Class(TStament)
  Private
    fIdentifier : String;
    fParameters : TExpressionList;
  Public
    Constructor Create(I : String; Params : TExpressionList);
    Procedure CodeGen(Dest : TStream); Override;
    Property Ident : String Read fIdentifier;
    Property Parameters : TExpressionList Read fParameters;
  End;
  
  TIfStament = Class(TStament)
  Private
    fConditional : TExpression;
    fIfTrue,
    fIfFalse     : TStament;
  Public
    Constructor Create;
    Procedure CodeGen(Dest : TStream); Override;
    Property Conditional : TExpression Read fConditional Write fConditional;
    Property IfTrue : TStament Read fIfTrue Write fIfTrue;
    Property IfFalse : TStament Read fIfFalse Write fIfFalse;
  End;

  TForStament = Class(TStament)
  Private
  Public
  End;

  TWhileStament = Class(TStament)
  Private
  Public
  End;

  TRepeatStament = Class(TStament)
  Private
  Public
  End;

  TCaseStament = Class(TStament)
  Private
  Public
  End;

Implementation

Constructor TStament.Create(K : TStamentType);
Begin
  fKind := K;
End;

End.
