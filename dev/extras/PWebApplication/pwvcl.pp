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
  WebAction,
  XMLBase,
  PWMain,
  Sysutils,
  Classes,
  Typinfo;

Type
  // This class shows the contents of a TStringList
  TWebMemoShow = Class(TWebComponent)
  Private
    fMemoSource : TStringList;
    fCurLine : LongInt;
    fOnShowLine : TWebEvent;
  Protected
    Procedure MemoShow(Caller : TXMLTag);
    Procedure MemoLine(Caller : TXMLTag);
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
  Published
    Property Memo : TStringList Read fMemoSource Write fMemoSource;
    Property OnShowLine : TWebEvent Read fOnShowLine Write fOnShowLine;
  End;

  // This component is able to iterate thru its subcomponents showing the
  // ones wich have the condition 'visible' set
  TWebComponentList = Class(TWebComponent)
  Private
    fCurComponent : LongInt;
    fCurCaption : LongInt;
    fOnShowComponent : TWebEvent;
  Protected
    Procedure ComponentCaptionList(Caller : TXMLTag);
    Procedure ComponentCaptionCurrent(Caller : TXMLTag);
    Procedure ComponentList(Caller : TXMLTag);
    Procedure ComponentCurrent(Caller : TXMLTag);
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
  Published
    Property OnShowComponent : TWebEvent Read fOnShowComponent Write fOnShowComponent;
    Property CurComponent : LongInt Read fCurComponent;
    Property CurCaption : LongInt Read fCurCaption;
  End;

  // This component inherits TWebComponentList to allow the user
  // to change the visible state of each subcomponent
  TWebPageDrawer = Class(TWebComponentList)
  Private
    fOnChange : TWebEvent;
  Protected
    Procedure Drawer(Caller : TXMLTag);
    Procedure DrawerClick(Actions : TTokenList; Depth : LongWord);
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
  Published
    Property OnChange : TWebEvent Read fOnChange Write fOnChange;
  End;

  // This component inherits TWebComponentList to allow the user
  // to select only one of the child components that be shown
  TWebPageFliper = Class(TWebComponentList)
  Private
    fSelected : LongInt;
    fOnSelect : TWebEvent;
  Protected
    Procedure Fliper(Caller : TXMLTag);
    Procedure FliperSelect(Actions : TTokenList; Depth : LongWord);
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
  Published
    Property OnSelect : TWebEvent Read fOnSelect Write fOnSelect;
    Property Selected : LongInt Read fSelected;
  End;

  // This component inherits TWebComponentList to allow the user
  // to scroll back and forth between child components
  TWebPageScroller = Class(TWebComponentList)
  Private
    fSelected   : LongInt;
    fOnNext,
    fOnPrevious : TWebEvent;
  Protected
    Procedure ScrollerNext(Actions : TTokenList; Depth : LongWord);
    Procedure ScrollerPrevious(Actions : TTokenList; Depth : LongWord);
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
  Published
    Property OnNext : TWebEvent Read fOnNext Write fOnNext;
    Property OnPrevious : TWebEvent Read fOnPrevious Write fOnPrevious;
    Property Selected : LongInt Read fSelected;
  End;

Implementation

Procedure TWebMemoShow.MemoShow(Caller : TXMLTag);
Begin
  fCurLine := 0;
  While fCurLine <= fMemoSource.Count Do
    Caller.EmitChilds;
End;

Procedure TWebMemoShow.MemoLine(Caller : TXMLTag);
Begin
  If Assigned(fOnShowLine) Then
    fOnShowLine();
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

// TWebComponentList

Procedure TWebComponentList.ComponentCaptionList(Caller : TXMLTag);
Begin
  fCurCaption := 0;
  While fCurCaption < Count Do
    Caller.EmitChilds;
End;

Procedure TWebComponentList.ComponentCaptionCurrent(Caller : TXMLTag);
Begin
  If fCurCaption < Count Then
    WebWrite(ComponentByIndex[fCurCaption].Caption);
  Inc(fCurCaption);
End;

Procedure TWebComponentList.ComponentList(Caller : TXMLTag);
Begin
  fCurComponent := 0;
  While fCurComponent < Count Do
    Caller.EmitChilds;
End;

Procedure TWebComponentList.ComponentCurrent(Caller : TXMLTag);
Begin
  If fCurComponent < Count Then
    If ComponentByIndex[fCurComponent].Visible Then
    Begin
      SetVar('self', SelfReference);
      ComponentByIndex[fCurComponent].ExportMyProperties;
      ComponentByIndex[fCurComponent].Template.LoadAndEmit;
      If Assigned(fOnShowComponent) Then
        fOnShowComponent();
      SetVar('self', SelfReference);
    End;
  Inc(fCurComponent);
End;

Constructor TWebComponentList.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
  Inherited Create(Name, Tmpl, Owner);
  Template.Tag['components.list'] := Self.ComponentList;
  Template.Tag['components.current'] := Self.ComponentCurrent;
  Template.Tag['components.captionlist'] := Self.ComponentCaptionList;
  Template.Tag['components.caption'] := Self.ComponentCaptionCurrent;
  fCurComponent := 0;
  fCurCaption := 0;
End;

// TWebPageDrawer

Procedure TWebPageDrawer.Drawer(Caller : TXMLTag);
Begin
  If (CurComponent > -1) And (CurComponent < Count) Then
  Begin
    SetVar('component', CompleteName);
    OutF('<a href="{$self}?action={$component}.toggle.' + IntToStr(CurComponent) + '">');
    Caller.EmitChilds;
    WebWrite('</a>');
  End;
End;

Procedure TWebPageDrawer.DrawerClick(Actions : TTokenList; Depth : LongWord);
Var
  Clicked : LongInt;
Begin
  Clicked := StrToInt(Actions[Depth]);
  If (Clicked > -1) And (Clicked < Count) Then
    ComponentByIndex[Clicked].Visible := Not(ComponentByIndex[Clicked].Visible);
  If Assigned(fOnChange) Then
    fOnChange();
End;

Constructor TWebPageDrawer.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
  Inherited Create(Name, Tmpl, Owner);
  Template.Tag['drawer'] := Self.Drawer;
  Actions['toggle'] := Self.DrawerClick;
End;

// TWebPageFliper

Procedure TWebPageFliper.Fliper(Caller : TXMLTag);
Begin
  If (CurComponent > -1) And (CurComponent < Count) Then
  Begin
    SetVar('component', CompleteName);
    If fSelected <> fCurComponent Then
    Begin
      ComponentByIndex[fCurComponent].Visible := False;
      OutF('<a href="{$self}?action={$component}.select.' + IntToStr(CurComponent) + '">');
      Caller.EmitChilds;
      WebWrite('</a>');
    End
    Else
    Begin
      ComponentByIndex[CurComponent].Visible := True;
      Caller.EmitChilds;
    End;
  End;
End;

Procedure TWebPageFliper.FliperSelect(Actions : TTokenList; Depth : LongWord);
Var
  Clicked : LongInt;
Begin
  Clicked := StrToInt(Actions[Depth]);
  If (Clicked > -1) And (Clicked < Count) Then
  Begin
    If (fSelected > -1) And (fSelected < Count) Then
      ComponentByIndex[fSelected].Visible := False;
    ComponentByIndex[Clicked].Visible := True;
    fSelected := Clicked;
  End;
  If Assigned(fOnSelect) Then
    fOnSelect();
End;

Constructor TWebPageFliper.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
  Inherited Create(Name, Tmpl, Owner);
  Template.Tag['fliper'] := Self.Fliper;
  Actions['select'] := Self.FliperSelect;
End;

// TWebPageScroller

Procedure TWebPageScroller.ScrollerNext(Actions : TTokenList; Depth : LongWord);
Var
  Ctrl : LongInt;
Begin
  If Assigned(fOnNext) Then
    fOnNext();
  If Count > 0 Then
    For Ctrl := Count - 1 DownTo 0 Do
      If ComponentByIndex[Ctrl].Visible Then
        If Ctrl + 1 < Count Then
        Begin
          ComponentByIndex[Ctrl].Visible := False;
          ComponentByIndex[Ctrl + 1].Visible := True;
          fSelected := Ctrl + 1;
        End
End;

Procedure TWebPageScroller.ScrollerPrevious(Actions : TTokenList; Depth : LongWord);
Var
  Ctrl : LongInt;
Begin
  If Assigned(fOnPrevious) Then
    fOnPrevious();
  If Count > 0 Then
    For Ctrl := 0 To Count - 1 Do
      If ComponentByIndex[Ctrl].Visible Then
        If Ctrl - 1 > -1 Then
        Begin
          ComponentByIndex[Ctrl].Visible := False;
          ComponentByIndex[Ctrl - 1].Visible := True;
          fSelected := Ctrl - 1;
        End
End;

Constructor TWebPageScroller.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
  Inherited Create(Name, Tmpl, Owner);
  Actions['next'] := Self.ScrollerNext;
  Actions['previous'] := Self.ScrollerPrevious;
End;

End.
