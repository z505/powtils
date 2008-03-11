// ~NRCOL

{****************************************************
 See Licence.txt for details
 Copyright(C) 2007 2008 - Jorge Aldo G. de F. Junior
 Colaborators:
 <put your name here if you improve this software>
*****************************************************}

{$M+}
{$H+}
{$MODE DELPHI}
Unit WebApplication;

Interface
Uses
  PWInitAll,
  PWMain,
  PWSDSSess,
  Classes,
  Sysutils,
	WebTemplate,
  WebAction,
  XMLBase;

Type
	TWebComponent = Class;

  TWebEvent = Procedure Of Object;

  TWebComponent = Class(TPersistent)
	Private
    fInstanceName  : String;
    fCaption       : String;
    fOwner         : TWebComponent;
    fTemplate      : TWebTemplate;
    fActions       : TWebActionList;
    fSubComponents : TStringList;
    fVisible       : Boolean;
    Function GetComponent(Name : String): TWebComponent;
    Function GetComponentByIndex(Idx : LongInt): TWebComponent;
    Function GetCount : LongInt;
  Protected
    Procedure ConditionalTag(Caller : TXMLTag);
    Procedure SubComponent(Caller : TXMLTag);
    Procedure CarryOnVar(Caller : TXMLTag);
	Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
		Destructor Destroy; Override;
    Function CompleteName: String;
    Function ActionName(Name : String): String;
    Procedure AppendComponent(Name : String; Instance : TWebComponent);
    Property Template : TWebTemplate Read fTemplate Write fTemplate;
    Property Actions : TWebActionList Read fActions Write fActions;
    Property Components[Name : String]: TWebComponent Read GetComponent;
    Property ComponentByIndex[Idx : LongInt]: TWebComponent Read GetComponentByIndex;
    Property Count : LongInt Read GetCount;
  Published
		Property InstanceName : String Read fInstanceName Write fInstanceName;
    Property Caption : String Read fCaption Write fCaption;
    Property Owner : TWebComponent Read fOwner;
    Property Visible : Boolean Read fVisible Write fVisible;
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

Procedure TWebComponent.ConditionalTag(Caller : TXMLTag);
Begin
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
  fSubComponents.Duplicates := dupIgnore;
  fTemplate.Tag['if'] := Self.ConditionalTag;
  fTemplate.Tag['component'] := Self.SubComponent;
  fTemplate.Tag['inputcarry'] := Self.CarryOnVar;
  If Assigned(Owner) Then
  Begin
    fOwner.Actions[fInstanceName] := Self.fActions.CheckAction;
    fOwner.AppendComponent(Name, Self);
  End;
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
    CompleteName := 'root';
End;

Function TWebComponent.ActionName(Name : String): String;
Begin
  ActionName := CompleteName + '.' + Name;
End;

Procedure TWebComponent.AppendComponent(Name : String; Instance : TWebComponent);
Begin
  fSubComponents.Objects[fSubComponents.Add(Name)] := (Instance As TObject);
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
  Root.Template.Load;
  Root.Template.Emit;
End;

End.
