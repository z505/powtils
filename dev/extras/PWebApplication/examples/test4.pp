Uses
  PWInit,
  PWMain,
  WebApplication,
  WebTemplate,
  PWVCL,
  Sysutils;

Var
  MyDialogArray : Array[1..10] Of TWebEditDialog;
  Ctrl : Byte;
  MyGroup : TWebPageDrawer;

Begin
  WebAppInit('test4');
  MyGroup := TWebPageDrawer.Create('drawer1', 'drawer', Nil);
  MyGroup.GarbageCollect := False;
  For Ctrl := 1 To 3 Do
  Begin
    MyDialogArray[Ctrl] := TWebEditDialog.Create('dialog' + IntToStr(Ctrl), 'dialog', MyGroup);
    MyDialogArray[Ctrl].Template.Condition['visible'] := True;
    MyGroup.AddChild('label ' + IntToStr(Ctrl), MyDialogArray[Ctrl]);
  End;
  Run;
  For Ctrl := 1 To 10 Do
    MyDialogArray[Ctrl].Free;
  MyGroup.Free;
  WebAppDone;
End.
