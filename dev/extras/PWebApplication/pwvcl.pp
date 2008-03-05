// ~NRCOL
Unit PWVL;

Interface
Uses
  WebApplication,
  WebTemplate,
  XMLBase,
  PWMain,
  Sysutils,
  Classes;

Type
  TArrayGrid = Array Of Array Of String;

  // This class groups any number of input elements
  TWebDialog = Class(TWebComponent)
  Private
    fOnSubmit : TWebEvent;
    fOnCancel : TWebEvent;
    fVisible  : Boolean;
    Function DialogConditionals(Name : String): Boolean;
    Procedure DialogSubmitAsButton(Caller : TXMLTag);
    Procedure DialogCancelAsButton(Caller : TXMLTag);
    Procedure DialogForm(Caller : TXMLTag);
    Procedure DialogSubmitAct(Actions : TTokenList; Depth : LongWord);
    Procedure DialogCancelAct(Actions : TTokenList; Depth : LongWord);
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
    Property OnSubmit : TWebEvent Read fOnSubmit Write fOnSubmit;
    Property OnCancel : TWebEvent Read fOnCancel Write fOnCancel;
    Property Visible  : Boolean Read fVisible Write fVisible;
  End;

Implementation

Function TWebDialog.DialogConditionals(Name : String): Boolean;
Begin
  DialogConditionals := False;
  If Name = 'visible' Then
    DialogConditionals := fVisible;
End;

Procedure TWebDialog.DialogSubmitAsButton(Caller : TXMLTag);
Begin
  WebWrite('<button type="submit" name="action" value="' + ActionName('submit') + '">');
  Caller.EmitChilds;
  WebWrite('</button>');
End;

Procedure TWebDialog.DialogCancelAsButton(Caller : TXMLTag);
Begin
  WebWrite('<button type="submit" name="action" value="' + ActionName('cancel') + '">');
  Caller.EmitChilds;
  WebWrite('</button>');
End;

Procedure TWebDialog.DialogForm(Caller : TXMLTag);
Var
  ActAttrib : LongInt;
Begin
  ActAttrib := Caller.Attributes.IndexOfName('action');
  If ActAttrib > -1 Then
    Caller.Attributes.Delete(ActAttrib);
  Caller.Attributes.Add('action="' + SelfReference + '"');
  Caller.StartEmit;
  Caller.EmitChilds;
  Caller.EndEmit;
End;

Procedure TWebDialog.DialogSubmitAct(Actions : TTokenList; Depth : LongWord);
Begin
  If Assigned(fOnSubmit) Then
    fOnSubmit();
End;

Procedure TWebDialog.DialogCancelAct(Actions : TTokenList; Depth : LongWord);
Begin
  If Assigned(fOnCancel) Then
    fOnCancel();
End;

Constructor TWebDialog.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
  Inherited Create(Name, Tmpl, Owner);
  Template.Tag['dialog'] := Self.DialogForm;
  Template.Tag['submit'] := Self.DialogSubmitAsButton;
  Template.Tag['cancel'] := Self.DialogCancelAsButton;
  Template.Conditional   := Self.DialogConditionals;
  Actions['submit']      := Self.DialogSubmitAct;
  Actions['cancel']      := Self.DialogCancelAct;
  fVisible               := False;
End;

End.
