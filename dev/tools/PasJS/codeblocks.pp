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
    sttCaseLabel,
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
    Constructor Create(Cond : TExpression; ifTrue, ifFalse : TStament);
    Procedure CodeGen(Dest : TStream); Override;
    Property Conditional : TExpression Read fConditional Write fConditional;
    Property IfTrue : TStament Read fIfTrue Write fIfTrue;
    Property IfFalse : TStament Read fIfFalse Write fIfFalse;
  End;

  TForStament = Class(TStament)
  Private
    fIdent      : String;
    fStartExpr,
    fFinishExpr : TExpression;
    fUpDown     : Boolean;
    fStament    : TStament;
  Public
    Constructor Create(Ident : String; SExpr, FExpr : TExpression;
      Dir : Boolean; Stament : TStament);
    Procedure CodeGen(Dest : TStream); Override;
    Property LoopVar : String Read fIdent Write fIdent;
    Property StartExpression : TExpression Read fStartExpr Write fStartExpr;
    Property FinishExpression : TExpression Read fFinishExpr Write fFinishExpr;
    Property Direction : Boolean Read fDirection Write fDirection;
    Property Stament : TStament Read fStament Write fStament;
  End;

  TWhileStament = Class(TStament)
  Private
    fCondition : TExpression;
    fStament   : TStament;
  Public
    Constructor Create(Cond : TExpression; Stament : TStament);
    Procedure CodeGen(Dest : TStream); Override;
    Property Condition : TExpression Read fCondition Write fCondition;
    Property Stament : TStament Read fStament Write fStament;
  End;

  TRepeatStament = Class(TStament)
  Private
    fCondition : TExpression;
    fStaments  : TStamentList;
  Public
    Constructor Create(Cond : TExpression; Staments: TStamentList);
    Procedure CodeGen(Dest : TStream); Override;
    Property Condition : TExpression Read fCondition Write fCondition;
    Property Staments : TStamentList Read fStaments Write fStaments;
  End;

  TCaseLabel = Class(TStament)
  Private
    fLabel   : String;
    fStament : TStament;
  Public
    Constructor Create(Lab : String; Stament : TStament);
    Procedure CodeGen(Dest : TStream); Override;
    Property CaseLabel : String Read fLabel Write fLabel;
    Property Stament : TStament Read fStament Write fStament;
  End;

  TCaseStament = Class(TStament)
  Private
    fIdent    : String;
    fCaseList : TStamentList;
    fCaseElse : TStament;
  Public
    Constructor Create(Ident : String; CaseList : TStamentList;
      CaseElse : TStament);
    Procedure CodeGen(Dest : TStream); Override;
    Property Ident : String Read fIdent Write fIdent;
    Property Cases : TStamentList Read fCaseList Write fCaseList;
    Property CaseElse : TStament Read fCaseElse Write fCaseElse;
  End;

  TBlockStament = Class(TStament)
  Private
    fStaments : TStamentList;
  Public
    Constructor Create(Staments : TStamentList);
    Procedure CodeGen(Dest : TStream); Override;
    Property Staments : TStamentList Read fStaments Write fStaments;
  End;

Procedure AddStament(Dest : TStamentList; Stament : TStament);

Implementation

Procedure AddStament(Dest : TStamentList; Stament : TStament);
Begin
  SetLength(Dest, Length(Dest) + 1);
  Dest[High(Dest)] := Stament;
End;

Constructor TStament.Create(K : TStamentType);
Begin
  fKind := K;
End;

Constructor TNullStament.Create;
Begin
  Inherited Create(sttNull);
End;

Procedure TNullStament.CodeGen(Dest : TStream);
Begin
  Write(Dest, '(* null stament *);');
End;

Constructor TAttribStament.Create(I : String; Exp : TExpression);
Begin
  Inherited Create(sttAttrib);
  fIdentifier := I;
  fExpression := Exp;
End;

Procedure TAttribStament.CodeGen(Dest : TStream);
Begin
  Write(Dest, fIdentifier + '=');
  fExpression.CodeGen(Dest);
  WriteLn(Dest, ';');
End;

Constructor Create(I : String; Params : TExpressionList);
Begin
  Inherited Create(sttCall);
  fIdentifier := I;
  fParameters := Params;
End;

Procedure CodeGen(Dest : TStream);
Begin
  Write(Dest, fIdentifier + '(');
  fParameters.CodeGen(Dest);
  WriteLn(');');
End;

Constructor TIfStament.Create(Cond : TExpression; ifTrue, ifFalse : TStament);
Begin
  Inherited Create(sttIf);
  fConditional := Cond;
  fIfTrue      := ifTrue;
  fIfFalse     := ifFalse;
End;

Procedure TIfStament.CodeGen(Dest : TStream); Override;
Begin
  Write(Dest, 'if (');
  fConditional.CodeGen(Dest);
  WriteLn(Dest, ')');
  fIfTrue.CodeGen(Dest);
  If Assigned(fIfFalse) Then
  Begin
    WriteLn(Dest, 'else');
    fIfFalse.CondeGen(Dest);
  End;
End;

Constructor TForStament.Create(Ident : String; SExpr, FExpr : TExpression;
  Dir : Boolean; Stament : TStament);
Begin
  Inherited Create(sttFor);
  fIdent      := Ident;
  fStartExpr  := SExpr;
  fFinishExpr := FExpr;
  fUpDown     := Dir;
  fStament    := Stament;
End;

Procedure TForStament.CodeGen(Dest : TStream); Override;
Begin
  Write(Dest, 'for(');
  Write(Dest, fIdent + '=');
  fStartExpr.CodeGen(Dest);
  Write(Dest, '; ' + fIdent + '==');
  fFinishExpr.CodeGen(Dest);
  If fUpDown Then
    Write(Dest, '; ' + fIdent + '++')
  Else
    Write(Dest, '; ' + fIdent + '--');
  WriteLn(Dest, ')');
  fStament.CodeGen(Dest);
End;

Constructor TWhileStament.Create(Cond : TExpression; Stament : TStament);
Begin
  Inherited Create(sttWhile);
  fCondition := Cond;
  fStament   := Stament;
End;

Procedure TWhileStament.CodeGen(Dest : TStream); Override;
Begin
  Write(Dest, 'while (');
  fCondition.CodeGen(Dest);
  WriteLn(Dest, ')');
  fStament.CodeGen(Dest);
End;

Constructor TRepeatStament.Create(Cond : TExpression; Staments: TStamentList);
Begin
  Inherited Create(sttRepeat);
  fCondition := Cond;
  fStaments  := Staments;
End;

Procedure TRepeatStament.CodeGen(Dest : TStream); Override;
Begin
  fStaments.CodeGen(Dest);
  WriteLn(Dest, ';');
  Write(Dest, 'while (!(');
  fCondition.CodeGen(Dest);
  WriteLn(Dest, '))');
  fStaments.CodeGen(Dest);
End;

Constructor TCaseLabel.Create(Lab : String; Stament : TStament);
Begin
  Inherited Create(sttCaseLabel);
  fLabel   := Lab;
  fStament := Stament;
End;

Procedure TCaseLabel.CodeGen(Dest : TStream);
Begin
  Write(Dest, 'case ' + fLabel + ':');
  fStament.CodeGen(Dest);
End;

Constructor TCaseStament.Create(Ident : String; CaseList : TStamentList;
  CaseElse : TStament);
Begin
  Inherited Create(sttCase);
  fIdent    := Ident;
  fCaseList := CaseList;
  fCaseElse := CaseElse;
End;

Procedure TCaseStament.CodeGen(Dest : TStream); Override;
Var
  X : LongInt;
Begin
  WriteLn(Dest, 'switch(' + fIdent + ') {');
  For X := Low(fCaseList) To High(fCaseList) Do
    fCaseList[X].CodeGen(Dest);
  If Assigned(fCaseElse) Then
  Begin
    WriteLn(Dest, 'default:');
    fCaseElse.CodeGen(Dest);
  End;
  WriteLn(Dest, '};');
End;

Constructor TBlockStament.Create(Staments : TStamentList);
Begin
  Inherited Create(sttBlock);
  fStaments := Staments;
End;

Procedure TBlockStament.CodeGen(Dest : TStream); Override;
Var
  X : LongInt;
Begin
  WriteLn(Dest, '{');
  For X := Low(fStaments) To High(fStaments) Do
    fStaments[X].CodeGen(Dest);
  WriteLn(Dest, '};');
End;

End.
