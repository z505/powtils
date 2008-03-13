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
  PWVCL;

Var
  LoginMan : TWebLoginManager;


Begin
  SelfReference := 'test1' {$IFDEF WINDOWS} + '.exe'{$ENDIF};
  LoginMan := TWebLoginManager.Create;
  Root := TWebComponent.Create('root', 'test1', Nil);
  With TWebLoginBox.Create('dialog1', 'dialog', Root) Do
  Begin
    Visible := True;
    LoginManager := LoginMan;
  End;
  Run;
  Root.Free;
End.
