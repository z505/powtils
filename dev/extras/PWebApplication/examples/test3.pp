// ~NRCOL

{****************************************************
 See Licence.txt for details
 Copyright(C) 2007 2008 - Jorge Aldo G. de F. Junior
 Colaborators:
 <put your name here if you improve this software>
*****************************************************}

Uses
  PWInit,
  PWMain,
  WebApplication,
  WebTemplate,
  PWVCL,
  Classes,
  Sysutils;

Var
  Ctrl : Byte;

Begin
  SelfReference := 'test3' {$IFDEF WINDOWS} + '.exe'{$ENDIF};
  Root := TWebComponent.Create('root', 'test3', Nil);
  TWebPageFliper.Create('fliper1', 'fliper', Root);
  For Ctrl := 1 To 10 Do
    With TWebComponent.Create('dialog' + IntToStr(Ctrl), 'dialog', Root.Components['fliper1']) Do
      Caption := 'dialog' + IntToStr(Ctrl);
  Run;
  Root.Free;
End.
