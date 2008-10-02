Unit TokenIterator;

Interface

Uses Scanner, Sysutils;

Type
  EParser = Class(Exception);

  TTokenIterator = Class
  Private
    fCurrent : Longint;
    fBuffer  : TTokenList;
    Function GetCurrentToken : TToken;
  Public
    Constructor Create(Src : TTokenList);
    Function EOTk : Boolean;
    Procedure Start;
    Procedure Next;
    Procedure Back;
    Function Expected(Tk : TTokenKind): Boolean; Overload;
    Function Expected(Tk : String): Boolean; Overload;
    Function IsSumOp: Boolean;
    Function IsMulOp: Boolean;
    Procedure RaiseError(Msg : String);
    Property Token : TToken Read GetCurrentToken;
  End;

Implementation

Function TTokenIterator.GetCurrentToken : TToken;
Begin
  GetCurrentToken := fBuffer[fCurrent];
End;

Constructor TTokenIterator.Create(Src : TTokenList);
Begin
  Inherited Create;
  fBuffer  := Src;
  fCurrent := Low(fBuffer);
End;

Function TTokenIterator.EOTk : Boolean;
Begin
  EOTk := fCurrent > High(fBuffer);
End;

Procedure TTokenIterator.Start;
Begin
  fCurrent := Low(fBuffer);
End;

Procedure TTokenIterator.Next;
Begin
  Inc(fCurrent);
End;

Procedure TTokenIterator.Back;
Begin
  Dec(fCurrent);
End;

Function TTokenIterator.Expected(Tk : TTokenKind): Boolean;
Begin
  Expected := GetCurrentToken.Kind = Tk;
End;

Function TTokenIterator.Expected(Tk : String): Boolean;
Begin
  Expected := GetCurrentToken.Value = Tk;
End;

Function TTokenIterator.IsSumOp: Boolean;
Begin
  IsSumOp :=
    ((GetCurrentToken.Value = '+') Or
    (GetCurrentToken.Value = '-')) Or
    ((GetCurrentToken.Value = 'or') Or
    (GetCurrentToken.Value = 'xor'));
End;

Function TTokenIterator.IsMulOp: Boolean;
Begin
  IsMulOp :=
    ((GetCurrentToken.Value = '*') Or
    (GetCurrentToken.Value = '/')) Or
    (GetCurrentToken.Value = 'and');
End;

Procedure TTokenIterator.RaiseError(Msg : String);
Begin
  Raise EParser.Create(
    '("' +
    GetCurrentToken.Value + '", "' +
    GetCurrentToken.SrcName + '", ' +
    IntToStr(GetCurrentToken.Row) + ', ' +
    IntToStr(GetCurrentToken.Col) + '): ' +
    Msg
  );
End;

End.
