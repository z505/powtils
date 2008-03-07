// ~NRCOL

{****************************************************
 See Licence.txt for details
 Copyright(C) 2007 2008 - Jorge Aldo G. de F. Junior
 Colaborators:
 <put your name here if you improve this software>
*****************************************************}

Unit WebApplication;

Interface
Uses
  Classes,
  PWMain,
  PWSDSSess,
	WebTemplate,
  WebAction,
  Sysutils,
  XMLBase;

Type
	TWebComponent = Class;

  TWebEvent = Procedure Of Object;

	TWebComponent = Class
	Private
    fInstanceName  : String;
    fCaption       : String;
    fOwner         : TWebComponent;
    fTemplate      : TWebTemplate;
    fActions       : TWebActionList;
    fSubComponents : TStringList;
    fConditional   : TStringList;
    fSession       : Boolean;
    Function GetComponent(Name : String): TWebComponent;
    Function GetComponentByIndex(Idx : LongInt): TWebComponent;
    Function GetCount : LongInt;
    Function GetCondition(Name : String): Boolean;
    Procedure SetCondition(Name : String; State : Boolean);
  Protected
    Procedure ConditionalTag(Caller : TXMLTag);
    Procedure SubComponent(Caller : TXMLTag);
    Procedure CarryOnVar(Caller : TXMLTag);
	Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
		Destructor Destroy; Override;
    Function CompleteName: String;
    Function ActionName(Name : String): String;
    Procedure AddSubComponent(Component : TWebComponent);
    Procedure SaveVarToSession(Name, Value: String);
    Function LoadVarFromSession(Name: String): String;
    Procedure SaveConditionToSession;
    Procedure LoadConditionFromSession;
    Procedure CascadeSaveCondition;
  Published
		Property InstanceName : String Read fInstanceName Write fInstanceName;
    Property Caption : String Read fCaption Write fCaption;
    Property Template : TWebTemplate Read fTemplate Write fTemplate;
    Property Actions : TWebActionList Read fActions Write fActions;
    Property Components[Name : String]: TWebComponent Read GetComponent;
    Property ComponentByIndex[Idx : LongInt]: TWebComponent Read GetComponentByIndex;
    Property Count : LongInt Read GetCount;
    Property Condition[Name : String]: Boolean Read GetCondition Write SetCondition;
    Property Session : Boolean Read fSession Write fSession;
	End;

ThreadVar
  SelfReference : String;
  Root          : TWebComponent;

Function UnQuote(Line : String): String;
Procedure Run;

Implementation

Function TWebComponent.GetComponent(Name : String): TWebComponent;
Var
  Ctrl : LongInt;
Begin
  Ctrl := fSubComponents.IndexOf(Name);
  If Ctrl > -1 Then
    GetComponent := (fSubComponents.Objects[Ctrl] As TWebComponent)
  Else
    GetComponent := Nil;
End;

Function TWebComponent.GetComponentByIndex(Idx : LongInt): TWebComponent;
Begin
  If (Idx > -1) And (Idx < fSubComponents.Count) Then
    GetComponentByIndex := (fSubComponents.Objects[Idx] As TWebComponent)
  Else
    GetComponentByIndex := Nil;
End;

Function TWebComponent.GetCount : LongInt;
Begin
  GetCount := fSubComponents.Count;
End;

Function TWebComponent.GetCondition(Name : String): Boolean;
Begin
  GetCondition := fConditional.IndexOf(Name) > - 1;
End;

Procedure TWebComponent.SetCondition(Name : String; State : Boolean);
Var
  Idx : LongInt;
Begin
  Idx := fConditional.IndexOf(Name);
  If State Then
    If Idx = -1 Then
      fConditional.Add(Name)
    Else
  Else
    If Idx > -1 Then
      fConditional.Delete(Idx);
End;

Procedure TWebComponent.ConditionalTag(Caller : TXMLTag);
Begin
  If ((Caller.Attributes.IndexOfName('when') > -1) And
  (fConditional.IndexOf(UnQuote(Caller.Attributes.Values['when'])) > -1)) Or
  ((Caller.Attributes.IndexOfName('notwhen') > -1) And
  (fConditional.IndexOf(UnQuote(Caller.Attributes.Values['notwhen'])) = -1)) Then
    Caller.EmitChilds;
End;

Procedure TWebComponent.SubComponent(Caller : TXMLTag);
Var
  C : TWebComponent;
Begin
  C := GetComponent(UnQuote(Caller.Attributes.Values['name']));
  If Assigned(C) Then
  Begin
    SetVar('self', SelfReference);
    SetVar('component', C.CompleteName);
    SetVar('caption', C.Caption);
    C.Template.LoadAndEmit;
    SetVar('self', SelfReference);
    SetVar('component', C.CompleteName);
    SetVar('caption', C.Caption);
  End;
End;

Procedure TWebComponent.CarryOnVar(Caller : TXMLTag);
Begin
  SetVar('carried', GetCGIVar(UnQuote(Caller.Attributes.Values['name'])));
  OutF('<input ' + Caller.Attributes.DelimitedText + '"/>');
End;

Constructor TWebComponent.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
	Inherited Create;
  fOwner         := Owner;
  fInstanceName  := Name;
  fTemplate      := TWebTemplate.Create(Tmpl + '.template.html');
  fActions       := TWebActionList.Create;
  fSubComponents := TStringList.Create;
  fConditional   := TStringList.Create;
  fSubComponents.Duplicates := dupIgnore;
  fSubComponents.Sorted := True;
  fConditional.Duplicates := dupIgnore;
  fConditional.Sorted := True;
  fTemplate.Tag['if'] := Self.ConditionalTag;
  fTemplate.Tag['component'] := Self.SubComponent;
  fTemplate.Tag['inputcarry'] := Self.CarryOnVar;
  If Assigned(Owner) Then
    fOwner.Actions[fInstanceName] := Self.fActions.CheckAction;
  LoadConditionFromSession;
End;

Destructor TWebComponent.Destroy;
Var
  Ctrl : LongInt;
Begin
  If fSubComponents.Count > 0 Then
    For Ctrl := 0 To fSubComponents.Count - 1 Do
      (fSubComponents.Objects[Ctrl] As TWebComponent).Free;
	If Assigned(fTemplate) Then
		fTemplate.Free;
  If Assigned(fActions) Then
    fActions.Free;
  If Assigned(fSubComponents) Then
    fSubComponents.Free;
  If Assigned(fConditional) Then
    fConditional.Free;
	Inherited Destroy;
End;

Function TWebComponent.CompleteName: String;
Var
  PreviousName : String;
Begin
  If Assigned(fOwner) Then
  Begin
    PreviousName := fOwner.CompleteName;
    If PreviousName <> '' Then
      CompleteName := fOwner.CompleteName + '.' + fInstanceName
    Else
      CompleteName := fInstanceName;
  End
  Else
    CompleteName := '';
End;

Function TWebComponent.ActionName(Name : String): String;
Begin
  ActionName := CompleteName + '.' + Name;
End;

Procedure TWebComponent.AddSubComponent(Component : TWebComponent);
Var
  Ctrl : LongInt;
Begin
  Ctrl := fSubComponents.IndexOf(Component.InstanceName);
  If Ctrl < 0 Then
    Ctrl := fSubComponents.Add(Component.InstanceName);
  fSubComponents.Objects[Ctrl] := (Component As TObject);
  fActions[Component.InstanceName] := Component.Actions.CheckAction;
End;

Procedure TWebComponent.SaveVarToSession(Name, Value: String);
Begin
  SetSess(CompleteName + '.' + Name, Value);
End;

Function TWebComponent.LoadVarFromSession(Name: String): String;
Begin
  LoadVarFromSession := GetSess(CompleteName + '.' + Name);
End;

Procedure TWebComponent.SaveConditionToSession;
Begin
  SetSess(CompleteName + '.conditional', fConditional.CommaText);
End;

Procedure TWebComponent.LoadConditionFromSession;
Begin
  fConditional.CommaText := GetSess(CompleteName + '.conditional');
End;

Procedure TWebComponent.CascadeSaveCondition;
Var
  Ctrl : LongInt;
Begin
  If Count > 0 Then
    For Ctrl := 0 To Count - 1 Do
      GetComponentByIndex(Ctrl).CascadeSaveCondition;
  SaveConditionToSession;
End;

Function UnQuote(Line : String): String;
Begin
  UnQuote := Copy(Line, 2, Length(Line) - 2);
End;

Procedure Run;
Var
  TheActions : TTokenList;
Begin
  If IsCGIVar('action') Then
  Begin
    TheActions := BreakApartNames(GetCGIVar('action'));
    Root.Actions.CheckAction(TheActions, 0);
  End
  Else
  Begin
    SetLength(TheActions, 1);
    TheActions[0] := 'default';
    Root.Actions.CheckAction(TheActions, 0);
  End;
  Root.CascadeSaveCondition;
  Root.Template.Load;
  Root.Template.Emit;
End;

End.
