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
  Classes,
  Sysutils;

Var
  Ctrl : Byte;

Begin
  SelfReference := 'test4' {$IFDEF WINDOWS} + '.exe'{$ENDIF};
  Root := TWebComponent.Create('root', 'test4', Nil);
  With TWebPageDrawer.Create('drawer1', 'drawer', Root) Do
    Visible := True;
  For Ctrl := 1 To 10 Do
    With TWebLoginBox.Create('dialog' + IntToStr(Ctrl), 'dialog', Root.Components['drawer1']) Do
    Begin
      Caption      := 'Authentication';
      Error        := False;
      ErrorValue   := '';
      Logged       := False;
      Visible      := True;
      Active       := True;
      LoginManager := Nil;
    End;
  Run;
  Root.Free;
End.
