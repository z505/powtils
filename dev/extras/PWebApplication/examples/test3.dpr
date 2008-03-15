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
  With TWebPageFliper.Create('fliper1', 'fliper', Root) Do
  Begin
    Visible := True;
    Selected := 0;
  End;
  For Ctrl := 1 To 10 Do
    With TWebLoginBox.Create('dialog' + IntToStr(Ctrl), 'dialog', Root.Components['fliper1']) Do
    Begin
      Caption      := 'Authentication';
      Error        := False;
      ErrorValue   := '';
      Logged       := False;
      Active       := True;
      LoginManager := Nil;
    End;
  Root.Components['fliper1'].Components['dialog1'].Visible := True;
  Run;
  Root.Free;
End.
