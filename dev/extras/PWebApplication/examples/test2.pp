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
  Sysutils;

Var
  MyDialogArray : Array[1..10] Of TWebEditDialog;
  Ctrl : Byte;
  MyGroup : TWebComponentList;

Begin
  WebAppInit('test2');
  MyGroup := TWebComponentList.Create('group1', 'group', Nil);
  For Ctrl := 1 To 10 Do
  Begin
    MyDialogArray[Ctrl] := TWebEditDialog.Create('dialog' + IntToStr(Ctrl), 'dialog', MyGroup);
    MyDialogArray[Ctrl].Template.Condition['visible'] := True;
  End;
  Run;
  For Ctrl := 1 To 10 Do
    MyDialogArray[Ctrl].Free;
  MyGroup.Free;
  WebAppDone;
End.
