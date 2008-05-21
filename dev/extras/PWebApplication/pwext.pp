Unit PWExt;

Interface

Uses
  WebApplication,
  WebTemplate,
  WebAction,
  XMLBase,
  PWMain,
  PWVCL,
  Sysutils,
  BreakTokens,
  MD5Crypt,
  Classes;

Type
  TWebTextFileLoginController = Class(TWebLoginController)
  Private
    fPasswordFile : String;
  Public
    Function Login: Boolean; Override;
    Function Logout: Boolean; Override;
    Property Passwords: String Read fPasswordFile Write fPasswordFile;
  End;

  TWebTextFileLoginEntry = 

Implementation

Function TWebTextFileLoginController.Login: Boolean;
Var
  Handler : Text;
  Line    : AnsiString;
  Tokens  : TTokenList;

  Procedure ReadLine;
  Begin
    Repeat
      ReadLn(Handler, Line);
      Tokens := BreakApart(Line);
    Until ((Tokens[0] <> '#') And (Tokens[0] <> ''));
  End;

Begin
  Login := False;
  If (fView.Login = '') Or (fView.Password = '') Then
    Exit
  Else
  Begin
    Assign(Handler, fPasswordFile);
    Reset(Handler);
    While Not(Eof(Handler) And
      (Tokens[0] = fView.Login) And
      (Tokens[1] = MD5String(fView.Password))) Do
      ReadLine;
    If (Tokens[0] = fView.Login) And
      (Tokens[1] = MD5String(fView.Password)) Then
      Login := True;
    Close(Handler);
  End;
End;

Function TWebTextFileLoginController.Logout: Boolean;
Begin
  fView.Login := '';
  fView.Password := '';
  Logout := True;
End;

End.
