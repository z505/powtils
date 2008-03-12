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

Type
  TMagicProps = Class(TWebComponent)
  Private
    fChar    : Char;
    fString  : String;
    fInteger : Integer;
    fBool    : Boolean;
  Published
    Property Ch : Char Read fChar Write fChar;
    Property St : String Read fString Write fString;
    Property Inn : Integer Read fInteger Write fInteger;
    Property Bo : Boolean Read fBool Write fBool;
  End;

Begin
  SelfReference := 'test6' {$IFDEF WINDOWS} + '.exe'{$ENDIF};
  Root := TWebComponent.Create('root', 'test6', Nil);
  With TMagicProps.Create('magicprop1', 'magicprop', Root) Do
  Begin
    Ch := 'A';
    St := 'this is a visible property !';
    Inn := 45000;
    Bo := False;
    Visible := True;
  End;
  Run;
  Root.Free;
End.
