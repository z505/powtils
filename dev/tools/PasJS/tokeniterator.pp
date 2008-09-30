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
    Function Expected(Tk : TTokenKind; Flag : Boolean): Boolean; Overload;
    Function Expected(Tk : String; Flag : Boolean): Boolean; Overload;
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

Function TTokenIterator.Expected(Tk : TTokenKind; Flag : Boolean): Boolean;
Begin
  If Flag Then
  Begin
    If GetCurrentToken.Kind = Tk Then
      
  End
  Else
    Expected := GetCurrentToken.Kind = Tk;
End;

Function TTokenIterator.Expected(Tk : String; Flag : Boolean): Boolean;
Begin
  Expected := GetCurrentToken.Value = Tk;
End;

End.
