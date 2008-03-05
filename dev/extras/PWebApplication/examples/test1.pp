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
