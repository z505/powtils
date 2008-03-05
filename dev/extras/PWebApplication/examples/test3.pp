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
  MyGroup : TWebPageFlipper;

Begin
  WebAppInit('test3');
  MyGroup := TWebPageFlipper.Create('flipper1', 'flipper', Nil);
  MyGroup.GarbageCollect := False;
  For Ctrl := 1 To 3 Do
  Begin
    MyDialogArray[Ctrl] := TWebEditDialog.Create('dialog' + IntToStr(Ctrl), 'dialog', MyGroup);
    MyGroup.AddChild('label ' + IntToStr(Ctrl), MyDialogArray[Ctrl]);
  End;
  MyGroup.Selected := 0;
  Run;
  For Ctrl := 1 To 10 Do
    MyDialogArray[Ctrl].Free;
  MyGroup.Free;
  WebAppDone;
End.
