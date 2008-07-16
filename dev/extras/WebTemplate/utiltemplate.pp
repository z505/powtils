// ~NRCOL

{****************************************************
 See Licence.txt for details
 Copyright(C) 2007 2008 - Jorge Aldo G. de F. Junior
 Colaborators:
 <put your name here if you improve this software>
*****************************************************}

Unit UtilTemplate;

Uses
  PWMain,
  XMLBase,
  WebTemplate;

Interface

Procedure RegisterToTemplate(Template : TWebTemplate);

Implementation

Procedure TemplateIf(Caller : TXMLTag);
Begin
  If GetVar(Caller.GetAttribute('var')) = Caller.GetAttribute('value') Then
    EmitChildsIf('then')
  Else
    EmitChildsIf('else');
End;

Procedure TemplateCase(Caller : TXMLTag);
Var
  CaseFound : Boolean;
Begin
  CaseFound := False;
  Caller.First;
  While Not(Caller.EndOfChilds)  Do
  Begin
    If (Caller.Child.Name = 'when') And
      (GetVar(Caller.Child.GetAttribute('var')) = Caller.Child.GetAttribute('value')) Then
    Begin
      Child.EmitChilds;
      CaseFound := True;
    End;
    Caller.Next;
  End;
  Caller.First;
  If Not(CaseFound) Then
    While Not(Caller.EndOfChilds) Do
    Begin
      If Caller.Child.Name = 'default' Then
        Child.EmitChilds;
      Caller.Next;
    End;
End;

Procedure RegisterToTemplate(Template : TWebTemplate);
Begin
  Template.Tag['if']   := TemplateIf;
  Template.Tag['case'] := TemplateCase;
End;

End.
