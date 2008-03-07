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
  SelfReference := 'test2' {$IFDEF WINDOWS} + '.exe'{$ENDIF};
  Root := TWebComponent.Create('root', 'test2', Nil);
  LastOne := TWebComponentList.Create('group1', 'group', Root);
  Root.AddSubComponent(LastOne);
  For Ctrl := 1 To 10 Do
  Begin
    Root.Components['group1'].AddSubComponent(
    TWebComponent.Create('dialog' + IntToStr(Ctrl), 'dialog', LastOne));
    Root.Components['group1'].Components['dialog' + IntToStr(Ctrl)].Condition['visible'] := True;
  End;
  Run;
  Root.Free;
End.
