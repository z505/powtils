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
  NoLoginController;

Var
  LoginMan : TNoLoginController;
  Dialog1  : TWebLoginBox;

Begin
  SelfReference := 'test1' {$IFDEF WINDOWS} + '.exe'{$ENDIF};
  Root := TWebComponent.Create('root', 'test1', Nil);
  Dialog1 := TWebLoginBox.Create('dialog1', 'dialog', Root);
  LoginMan := TNoLoginController.Create(Dialog1);
  With Dialog1 Do
  Begin
    Caption         := 'Authentication';
    Error           := False;
    ErrorValue      := '';
    Logged          := False;
    Visible         := True;
    Active          := True;
    LoginController := LoginMan;
  End;
  Run;
  LoginMan.Free;
  Root.Free;
End.
