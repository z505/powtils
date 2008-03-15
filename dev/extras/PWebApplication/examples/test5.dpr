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
  SelfReference := 'test5' {$IFDEF WINDOWS} + '.exe'{$ENDIF};
  Root := TWebComponent.Create('root', 'test5', Nil);
  With TWebPageScroller.Create('scroller1', 'scroller', Root) Do
    Visible := True;
  For Ctrl := 1 To 10 Do
    With TWebLoginBox.Create('dialog' + IntToStr(Ctrl), 'dialog', Root.Components['scroller1']) Do
    Begin
      Caption      := 'Authentication';
      Error        := False;
      ErrorValue   := '';
      Logged       := False;
      Active       := True;
      LoginManager := Nil;
    End;
  Root.
    Components['fliper1'].
      Components['scroller1'].
        Components['dialog1'].
          Visible := True;
  Run;
  Root.Free;
End.
