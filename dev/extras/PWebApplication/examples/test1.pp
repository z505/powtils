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

Var
  MyDialog : TWebEditDialog;

Begin
  WebAppInit('test1');
  MyDialog := TWebEditDialog.Create('dialog1', 'dialog', Nil);
  MyDialog.Template.Condition['visible'] := True;
  Run;
  MyDialog.Free;
  WebAppDone;
End.
