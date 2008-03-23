Unit NoLoginController;

Interface

Uses
  PWVCL;

Type
  TNoLoginController = Class(TWebLoginController)
  Public
    Function Login: Boolean; Override;
    Function Logout: Boolean; Override;
  End;

Implementation

Function TNoLoginController.Login: Boolean;
Begin
  Login := False;
End;

Function TNoLoginController.Logout: Boolean;
Begin
  Logout := False;
End;

End.
