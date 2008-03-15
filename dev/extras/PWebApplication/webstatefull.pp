Unit WebStatefull;

Interface
Uses
  Classes,
  Sysutils,
  PWMain;

Procedure NewState;

Implementation

Procedure NewState;
Var
  Next : LongWord;
  Temp : Text;
Begin
  Randomize;
  Repeat
    Next := Round(Random * $FFFFFFFF);
  Until Not(FileExists(IntToStr(Next) + '.states'));
 SetCookie('states', IntToStr(Next));
End;

End.
