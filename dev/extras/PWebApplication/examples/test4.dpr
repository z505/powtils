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
  TWebPageDrawer.Create('drawer1', 'drawer', Root);
  For Ctrl := 1 To 10 Do
    With TWebComponent.Create('dialog' + IntToStr(Ctrl), 'dialog', Root.Components['drawer1']) Do
      Caption := 'dialog' + IntToStr(Ctrl);
  Run;
  Root.Free;
End.
