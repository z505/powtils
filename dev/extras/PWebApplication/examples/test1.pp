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
  MyDialog := TWebEditDialog('dialog1', 'dialog', Nil);
  MyDialog.Visible := True;
  Run;
  WebAppDone;
End.
