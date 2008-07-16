// ~NRCOL

{****************************************************
 See Licence.txt for details
 Copyright(C) 2007 2008 - Jorge Aldo G. de F. Junior
 Colaborators:
 <put your name here if you improve this software>
*****************************************************}

Unit UtilTemplate;

Interface

Uses
  WebTemplate;

Procedure RegisterToTemplate(Template : TWebTemplate);

Implementation

Uses
  PWMain,
  XMLBase;

Procedure TemplateIf(Caller : TXMLTag);
Begin
  If GetVar(Caller.GetAttribute('var')) = Caller.GetAttribute('value') Then
    Caller.EmitChildsIf('then')
  Else
    Caller.EmitChildsIf('else');
End;

Procedure TemplateCase(Caller : TXMLTag);
Var
  CaseFound : Boolean;
Begin
  CaseFound := False;
  Caller.First;
  While Not(Caller.EndOfChilds)  Do
  Begin
    If ((Caller.Child As TXMLTag).Name = 'when') And
      (GetVar((Caller.Child As TXMLTag).GetAttribute('var')) = (Caller.Child As TXMLTag).GetAttribute('value')) Then
    Begin
      (Caller.Child As TXMLTag).EmitChilds;
      CaseFound := True;
    End;
    Caller.Next;
  End;
  Caller.First;
  If Not(CaseFound) Then
    While Not(Caller.EndOfChilds) Do
    Begin
      If (Caller.Child As TXMLTag).Name = 'default' Then
        (Caller.Child As TXMLTag).EmitChilds;
      Caller.Next;
    End;
End;

Procedure RegisterToTemplate(Template : TWebTemplate);
Begin
  Template.Tag['if']   := TemplateIf;
  Template.Tag['case'] := TemplateCase;
End;

End.
