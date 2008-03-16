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
  SelfReference := 'test3' {$IFDEF WINDOWS} + '.exe'{$ENDIF};
  Root := TWebComponent.Create('root', 'test3', Nil);
  With TWebPageFlipper.Create('flipper1', 'flipper', Root) Do
  Begin
    Visible := True;
    Selected := 0;
  End;
  For Ctrl := 1 To 10 Do
    With TWebLoginBox.Create('dialog' + IntToStr(Ctrl), 'dialog', Root.Components['flipper1']) Do
    Begin
      Caption      := 'Authentication ' + IntToStr(Ctrl);
      Error        := False;
      ErrorValue   := '';
      Logged       := False;
      Active       := True;
      LoginManager := Nil;
    End;
  Root.Components['flipper1'].Components['dialog1'].Visible := True;
  Run;
  Root.Free;
End.
