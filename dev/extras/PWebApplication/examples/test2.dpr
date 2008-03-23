// ~NRCOL

{****************************************************
 See Licence.txt for details
 Copyright(C) 2007 2008 - Jorge Aldo G. de F. Junior
 Colaborators:
 <put your name here if you improve this software>
*****************************************************}

Uses
  PWInitAll,
  PWMain,
  PWSDSSess,
  WebApplication,
  WebTemplate,
  PWVCL,
  Classes,
  Sysutils;

Var
  Ctrl : Byte;

Begin
  SelfReference := 'test2' {$IFDEF WINDOWS} + '.exe'{$ENDIF};
  Root := TWebComponent.Create('root', 'test2', Nil);
  With TWebComponentList.Create('group1', 'group', Root) Do
    Visible := True;
  For Ctrl := 1 To 10 Do
    With TWebLoginBox.Create('dialog' + IntToStr(Ctrl), 'dialog', Root.Components['group1']) Do
    Begin
      Caption      := 'Authentication';
      Error        := False;
      ErrorValue   := '';
      Logged       := False;
      Visible      := True;
      Active       := True;
      LoginController := Nil;
    End;
  Run;
  Root.Free;
End.
