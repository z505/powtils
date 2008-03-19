// ~NRCOL

{****************************************************
 See Licence.txt for details
 Copyright(C) 2007 2008 - Jorge Aldo G. de F. Junior
 Colaborators:
 <put your name here if you improve this software>
*****************************************************}

Uses
  PWInitAll,
  PWMain,
  PWSDSSess,
  WebApplication,
  WebTemplate,
  PWVCL,
  NoLoginManager;

Var
  LoginMan : TNoLoginManager;

Begin
  SelfReference := 'test1' {$IFDEF WINDOWS} + '.exe'{$ENDIF};
  LoginMan := TNoLoginManager.Create;
  Root := TWebComponent.Create('root', 'test1', Nil);
  With TWebLoginBox.Create('dialog1', 'dialog', Root) Do
  Begin
    Caption      := 'Authentication';
    Error        := False;
    ErrorValue   := '';
    Logged       := False;
    Visible      := True;
    Active       := True;
    LoginManager := LoginMan;
  End;
  Run;
  LoginMan.Free;
  Root.Free;
End.
