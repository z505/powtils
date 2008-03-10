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
  PWVCL;

Begin
  SelfReference := 'test1' {$IFDEF WINDOWS} + '.exe'{$ENDIF};
  Root := TWebComponent.Create('root', 'test1', Nil);
  With TWebComponent.Create('dialog1', 'dialog', Root) Do
    Condition['visible'] := True;
  Run;
  Root.Free;
End.
