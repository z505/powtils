// ~NRCOL

{****************************************************
 See Licence.txt for details
 Copyright(C) 2007 2008 - Jorge Aldo G. de F. Junior
 Colaborators:
 <put your name here if you improve this software>
*****************************************************}

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

  // This component is a TWebComponentList that allows the user
  // to select wich one of the unique child components will be shown
  TWebPageFlipper = Class(TWebComponentList)
  Private
    fChilds   : TStringList;
    fSelected : LongInt;
    fCurLabel : LongInt;
    fGarbage  : Boolean;
    fOnSelect : TWebEvent;
    Procedure FlipperCurLabelCaption(Caller : TXMLTag);
    Procedure FlipperCurLabel(Caller : TXMLTag);
    Procedure FlipperLabels(Caller : TXMLTag);
    Procedure FlipperSelectChild(Actions : TTokenList; Depth : LongWord);
    Procedure SetSelected(S : LongInt);
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
    Destructor Destroy; Override;
    Procedure AddChild(Caption : String; Child : TWebComponent);
    Procedure FreeChilds;
    Property Selected : LongInt Read fSelected Write SetSelected;
    Property GarbageCollect : Boolean Read fGarbage Write fGarbage;
    Property OnSelect : TWebEvent Read fOnSelect Write fOnSelect;
  End;

  TWebPageDrawer = Class(TWebComponentList)
  Private
    fChilds   : TStringList;
    fCurLabel : LongInt;
    fGarbage  : Boolean;
    fOnChange : TWebEvent;
    Procedure DrawerCurLabelCaption(Caller : TXMLTag);
    Procedure DrawerCurLabel(Caller : TXMLTag);
    Procedure DrawerLabels(Caller : TXMLTag);
    Procedure DrawerToggleChild(Actions : TTokenList; Depth : LongWord);
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
    Destructor Destroy; Override;
    Procedure AddChild(Caption : String; Child : TWebComponent);
    Procedure FreeChilds;
    Property GarbageCollect : Boolean Read fGarbage Write fGarbage;
    Property OnChange : TWebEvent Read fOnChange Write fOnChange;
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
  If fCurComponent <= Template.SubTemplates.Count Then
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

Procedure TWebPageFlipper.FlipperCurLabelCaption(Caller : TXMLTag);
Begin
  If fCurLabel < fChilds.Count Then
    WebWrite(fChilds[fCurLabel]);
End;

Procedure TWebPageFlipper.FlipperCurLabel(Caller : TXMLTag);
Begin
  If (fCurLabel > -1) And (fCurLabel < fChilds.Count) Then
  Begin
    If fSelected = fCurLabel Then
    Begin
      Template.Condition['selected'] := True;
      Caller.EmitChilds;
    End
    Else
    Begin
      Template.Condition['selected'] := False;
      WebWrite('<a href="' + SelfReference + '?action=' +
      ActionName('select.' + IntToStr(fCurLabel)) +
      '">');
      Caller.EmitChilds;
      WebWrite('</a>');
    End;
  End;
  Inc(fCurLabel);
End;

Procedure TWebPageFlipper.FlipperLabels(Caller : TXMLTag);
Begin
  fCurLabel := 0;
  While fCurLabel < fChilds.Count Do
    Caller.EmitChilds;
End;

Procedure TWebPageFlipper.FlipperSelectChild(Actions : TTokenList; Depth : LongWord);
Var
  Idx : LongInt;
Begin
  Idx := StrToInt(Actions[Depth]);
  SetSelected(Idx);
  If Assigned(fOnSelect) Then
    fOnSelect();
End;

Procedure TWebPageFlipper.SetSelected(S : LongInt);
Begin
  If (S > -1) And (S < fChilds.Count) Then
  Begin
    If (fSelected > -1) And (fSelected < fChilds.Count) Then
      (fChilds.Objects[fSelected] As TWebComponent).Template.Condition['visible'] := False;
    fSelected := S;
    (fChilds.Objects[S] As TWebComponent).Template.Condition['visible'] := True;
  End;
End;

Constructor TWebPageFlipper.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
  Inherited Create(Name, Tmpl, Owner);
  fChilds := TStringList.Create;
  Template.Tag['curlabel'] := Self.FlipperCurLabel;
  Template.Tag['curlabelcaption'] := Self.FlipperCurLabelCaption;
  Template.Tag['labels'] := Self.FlipperLabels;
  Actions['select'] := Self.FlipperSelectChild;
  fCurLabel := -1;
  fSelected := -1;
  fGarbage  := False;
End;

Destructor TWebPageFlipper.Destroy;
Begin
  If fGarbage Then
    FreeChilds;
  fChilds.Free;
  Inherited Destroy;
End;

Procedure TWebPageFlipper.AddChild(Caption : String; Child : TWebComponent);
Begin
  fChilds.Objects[fChilds.Add(Caption)] := (Child As TObject);
End;

Procedure TWebPageFlipper.FreeChilds;
Var
  Ctrl : LongInt;
Begin
  For Ctrl := 0 To fChilds.Count - 1 Do
    (fChilds.Objects[Ctrl] As TWebComponent).Free;
End;

Procedure TWebPageDrawer.DrawerCurLabelCaption(Caller : TXMLTag);
Begin
  If fCurLabel < fChilds.Count Then
    WebWrite(fChilds[fCurLabel]);
End;

Procedure TWebPageDrawer.DrawerCurLabel(Caller : TXMLTag);
Begin
  If (fCurLabel > -1) And (fCurLabel < fChilds.Count) Then
  Begin
    Template.Condition['selected'] := False;
    WebWrite('<a href="' + SelfReference + '?action=' +
    ActionName('toggle.' + IntToStr(fCurLabel)) +
    '">');
    Caller.EmitChilds;
    WebWrite('</a>');
  End;
  Inc(fCurLabel);
End;

Procedure TWebPageDrawer.DrawerLabels(Caller : TXMLTag);
Begin
  fCurLabel := 0;
  While fCurLabel < fChilds.Count Do
    Caller.EmitChilds;
End;

Procedure TWebPageDrawer.DrawerToggleChild(Actions : TTokenList; Depth : LongWord);
Var
  Idx : LongInt;
Begin
  Idx := StrToInt(Actions[Depth]);
  If (Idx > -1) And (Idx < fChilds.Count) Then
    (fChilds.Objects[Idx] As TWebComponent).Template.Condition['visible'] := Not(
     (fChilds.Objects[Idx] As TWebComponent).Template.Condition['visible']);
  If Assigned(fOnChange) Then
    fOnChange();
End;

Constructor TWebPageDrawer.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
  Inherited Create(Name, Tmpl, Owner);
  fChilds := TStringList.Create;
  Template.Tag['curlabel'] := Self.DrawerCurLabel;
  Template.Tag['curlabelcaption'] := Self.DrawerCurLabelCaption;
  Template.Tag['labels'] := Self.DrawerLabels;
  Actions['toggle'] := Self.DrawerToggleChild;
  fCurLabel := -1;
  fGarbage  := False;
End;

Destructor TWebPageDrawer.Destroy;
Begin
  If fGarbage Then
    FreeChilds;
  fChilds.Free;
  Inherited Destroy;
End;

Procedure TWebPageDrawer.AddChild(Caption : String; Child : TWebComponent);
Begin
  fChilds.Objects[fChilds.Add(Caption)] := (Child As TObject);
End;

Procedure TWebPageDrawer.FreeChilds;
Var
  Ctrl : LongInt;
Begin
  For Ctrl := 0 To fChilds.Count - 1 Do
    (fChilds.Objects[Ctrl] As TWebComponent).Free;
End;

End.
