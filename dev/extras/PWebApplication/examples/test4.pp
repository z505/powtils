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
  LastOne : TWebComponent;

Begin
  SelfReference := 'test4' {$IFDEF WINDOWS} + '.exe'{$ENDIF};
  Root := TWebComponent.Create('root', 'test4', Nil);
  LastOne := TWebPageDrawer.Create('drawer1', 'drawer', Root);
  Root.AddSubComponent(LastOne);
  For Ctrl := 1 To 10 Do
  Begin
    LastOne.AddSubComponent(
    TWebComponent.Create('dialog' + IntToStr(Ctrl), 'dialog', LastOne));
    LastOne.Components['dialog' + IntToStr(Ctrl)].Caption := 'dialog' + IntToStr(Ctrl);
  End;
  Run;
  Root.Free;
End.
