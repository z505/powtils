// ~NRCOL
Unit PWVCL;

Interface
Uses
  WebApplication,
  WebTemplate,
  XMLBase,
  PWMain,
  Sysutils,
  Classes;

Type
  // This class groups any number of input elements
  TWebEditDialog = Class(TWebComponent)
  Private
    fOnSubmit : TWebEvent;
    fOnCancel : TWebEvent;
    Procedure DialogSubmitAsButton(Caller : TXMLTag);
    Procedure DialogCancelAsButton(Caller : TXMLTag);
    Procedure DialogCarryOnVar(Caller : TXMLTag);
    Procedure DialogForm(Caller : TXMLTag);
    Procedure DialogSubmitAct(Actions : TTokenList; Depth : LongWord);
    Procedure DialogCancelAct(Actions : TTokenList; Depth : LongWord);
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
    Property OnSubmit : TWebEvent Read fOnSubmit Write fOnSubmit;
    Property OnCancel : TWebEvent Read fOnCancel Write fOnCancel;
  End;

  // This component is able to iterate thru its subcomponents showing the
  // ones wich have the condition 'visible' set
  TWebComponentList = Class(TWebComponent)
  Private
    fCurComponent : LongInt;
    Procedure ComponentList(Caller : TXMLTag);
    Procedure ComponentCurrent(Caller : TXMLTag);
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
  End;

  // This class shows the contents of a TStringList
  TWebMemoShow = Class(TWebComponent)
  Private
    fMemoSource : TStringList;
    fCurLine : LongInt;
    Procedure MemoShow(Caller : TXMLTag);
    Procedure MemoLine(Caller : TXMLTag);
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
    Property Memo : TStringList Read fMemoSource Write fMemoSource;
  End;

Implementation

Procedure TWebEditDialog.DialogSubmitAsButton(Caller : TXMLTag);
Begin
  WebWrite('<button type="submit" name="action" value="' + ActionName('submit') + '">');
  Caller.EmitChilds;
  WebWrite('</button>');
End;

Procedure TWebEditDialog.DialogCancelAsButton(Caller : TXMLTag);
Begin
  WebWrite('<button type="submit" name="action" value="' + ActionName('cancel') + '">');
  Caller.EmitChilds;
  WebWrite('</button>');
End;

Procedure TWebEditDialog.DialogCarryOnVar(Caller : TXMLTag);
Begin
  WebWrite('<input type="hidden" ' + Caller.Attributes.DelimitedText +
    ' value="' + GetCGIVar(
    UnQuote(Caller.Attributes.Values['name'])
    ) + '"/>');
End;

Procedure TWebEditDialog.DialogForm(Caller : TXMLTag);
Var
  ActAttrib : LongInt;
Begin
  ActAttrib := Caller.Attributes.IndexOfName('action');
  If ActAttrib > -1 Then
    Caller.Attributes.Delete(ActAttrib);
  Caller.Attributes.Add('action="' + SelfReference + '"');
  WebWrite('<form ' + Caller.Attributes.DelimitedText + '>');
  Caller.EmitChilds;
  WebWrite('</form>');
End;

Procedure TWebEditDialog.DialogSubmitAct(Actions : TTokenList; Depth : LongWord);
Begin
  If Assigned(fOnSubmit) Then
    fOnSubmit();
End;

Procedure TWebEditDialog.DialogCancelAct(Actions : TTokenList; Depth : LongWord);
Begin
  If Assigned(fOnCancel) Then
    fOnCancel();
End;

Constructor TWebEditDialog.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
  Inherited Create(Name, Tmpl, Owner);
  Template.Tag['dialog'] := Self.DialogForm;
  Template.Tag['submit'] := Self.DialogSubmitAsButton;
  Template.Tag['cancel'] := Self.DialogCancelAsButton;
  Template.Tag['carry']  := Self.DialogCarryOnVar;
  Actions['submit']      := Self.DialogSubmitAct;
  Actions['cancel']      := Self.DialogCancelAct;
End;

Procedure TWebComponentList.ComponentList(Caller : TXMLTag);
Begin
  fCurComponent := 0;
  While fCurComponent < Template.SubTemplates.Count Do
    Caller.EmitChilds;
End;

Procedure TWebComponentList.ComponentCurrent(Caller : TXMLTag);
Begin
  If fCurComponent < Template.SubTemplates.Count Then
    If Template.SubTemplates.TemplateByNumber[fCurComponent].Condition['visible'] Then
       Template.SubTemplates.TemplateByNumber[fCurComponent].LoadAndEmit;
  Inc(fCurComponent);
End;

Constructor TWebComponentList.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
  Inherited Create(Name, Tmpl, Owner);
  Template.Tag['componentlist'] := Self.ComponentList;
  Template.Tag['componentcurrent'] := Self.ComponentCurrent;
  fCurComponent := 0;
End;

Procedure TWebMemoShow.MemoShow(Caller : TXMLTag);
Begin
  fCurLine := 0;
  While fCurLine <= fMemoSource.Count Do
    Caller.EmitChilds;
End;

Procedure TWebMemoShow.MemoLine(Caller : TXMLTag);
Begin
  If fCurLine <= fMemoSource.Count Then
    WebWrite(fMemoSource[fCurLine]);
  Inc(fCurLine);
End;

Constructor TWebMemoShow.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
  Inherited Create(Name, Tmpl, Owner);
  Template.Tag['memo'] := Self.MemoShow;
  Template.Tag['line'] := Self.MemoLine;
End;

End.
