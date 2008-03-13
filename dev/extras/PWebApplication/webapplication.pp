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
  XMLBase,
  TypInfo;

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
    fError         : Boolean;
    fErrorValue    : String;
    Function GetComponent(Name : String): TWebComponent;
    Function GetComponentByIndex(Idx : LongInt): TWebComponent;
    Function GetCount : LongInt;
  Protected
    Procedure ConditionalTag(Caller : TXMLTag);
    Procedure SubComponent(Caller : TXMLTag);
    Procedure CarryOnVar(Caller : TXMLTag);
    Procedure ExportVars(Caller : TXMLTag);
	Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
		Destructor Destroy; Override;
    Function CompleteName: String;
    Function ActionName(Name : String): String;
    Procedure AppendComponent(Name : String; Instance : TWebComponent);
    Procedure SaveMeToStringList(Dst : TStringList);
    Procedure LoadMeFromStringList(Src : TStringList);
    Procedure CascadeSave(Dst : TStringList);
    Procedure CascadeLoad(Src : TStringList);
    Procedure ExportMyProperties;
    Property Template : TWebTemplate Read fTemplate Write fTemplate;
    Property Actions : TWebActionList Read fActions Write fActions;
    Property Components[Name : String]: TWebComponent Read GetComponent;
    Property ComponentByIndex[Idx : LongInt]: TWebComponent Read GetComponentByIndex;
    Property Count : LongInt Read GetCount;
    Property Owner : TWebComponent Read fOwner;
  Published
    Property InstanceName : String Read fInstanceName Write fInstanceName;
    Property ComponentName : String Read CompleteName;
    Property Caption : String Read fCaption Write fCaption;
    Property Visible : Boolean Read fVisible Write fVisible;
    Property Error : Boolean Read fError Write fError;
    Property ErrorValue : String Read fErrorValue Write fErrorValue;
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
Var
  Cond    : Boolean;
Begin
  Cond := False;
  If Caller.Attributes.IndexOfName('when') > -1 Then
    If Caller.Attributes.IndexOfName('notwhen') > -1 Then
      Cond :=
        Boolean(
        GetOrdProp(
        Self, UnQuote(
        Caller.Attributes.Values['when']
        )
        )) And
        Not(
        Boolean(
        GetOrdProp(
        Self, UnQuote(
        Caller.Attributes.Values['notwhen']))))
    Else
      Cond :=
        Boolean(
        GetOrdProp(
        Self, UnQuote(
        Caller.Attributes.Values['when'])))
  Else
    If Caller.Attributes.IndexOfName('notwhen') > -1 Then
      Cond :=
        Not(
        Boolean(
        GetOrdProp(
        Self, UnQuote(
        Caller.Attributes.Values['notwhen']))))
    Else
      Cond := False;
  If Cond Then
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
    If C.Visible Then
      C.Template.LoadAndEmit;
    SetVar('self', SelfReference);
  End;
End;

Procedure TWebComponent.CarryOnVar(Caller : TXMLTag);
Begin
  SetVar('carried', GetCGIVar(UnQuote(Caller.Attributes.Values['name'])));
  OutF('<input ' + Caller.Attributes.DelimitedText + '"/>');
End;

Procedure TWebComponent.ExportVars(Caller : TXMLTag);
Begin
  Self.ExportMyProperties;
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
  fTemplate.Tag['exportvars'] := Self.ExportVars;
  fError := False;
  fErrorValue := '';
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
  OCName : String;
Begin
  If Assigned(fOwner) Then
  Begin
    OCName := fOwner.CompleteName;
    If OCName <> '' Then
      CompleteName := OCName + '.' + Self.fInstanceName
    Else
      CompleteName := Self.fInstanceName;
  End
  Else
    CompleteName := '';
End;

Function TWebComponent.ActionName(Name : String): String;
Begin
  ActionName := Self.CompleteName + '.' + Name
End;

Procedure TWebComponent.AppendComponent(Name : String; Instance : TWebComponent);
Begin
  fSubComponents.Objects[fSubComponents.Add(Name)] := (Instance As TObject);
End;

Procedure TWebComponent.SaveMeToStringList(Dst : TStringList);
Var
  PLst  : PPropList;
  PTd   : PTypeData;
  Siz   : LongInt;
  Ctrl  : LongInt;
  PName : String;
  AName : String;
  Entry : String;

Begin
  PTd := GetTypeData(Self.ClassInfo);
  GetMem(PLst, PTd^.PropCount * SizeOf(Pointer));
  Siz := GetPropList(Self.ClassInfo, PLst);
  For Ctrl := 0 To Siz - 1 Do
  Begin
    PName := PLst^[Ctrl]^.Name;
    AName := Self.ActionName(PName);
    Entry := '';
    If PLst^[Ctrl]^.PropType^.Kind In [
      tkInteger, tkSet, tkBool, tkWChar, tkChar] Then
      Entry := AName + '="' + IntToStr(GetOrdProp(Self, PName)) + '"';
    If PLst^[Ctrl]^.PropType^.Kind In [
      tkSString, tkLString, tkAString, tkWString] Then
      Entry := AName + '="' + GetStrProp(Self, PName) + '"';
    If PLst^[Ctrl]^.PropType^.Kind = tkFloat Then
      If PLst^[Ctrl]^.PropType^.Name = 'TDateTime' Then
        Entry := AName + '="' + DateTimeToStr(GetFloatProp(Self, PName)) + '"'
      Else
        Entry := AName + '="' + FloatToStr(GetFloatProp(Self, PName)) + '"';
    If PLst^[Ctrl]^.PropType^.Kind = tkInt64 Then
      Entry := AName + '="' + IntToStr(GetInt64Prop(Self, PName)) + '"';
    If PLst^[Ctrl]^.PropType^.Kind =  tkEnumeration Then
      Entry := AName + '="' + GetEnumProp(Self, PName) + '"';
    If Entry <> '' Then
      Dst.Add(Entry);
  End;
  FreeMem(PLst, PTd^.PropCount * SizeOf(Pointer));
End;

Procedure TWebComponent.LoadMeFromStringList(Src : TStringList);
Var
  PLst  : PPropList;
  PTd   : PTypeData;
  Siz   : LongInt;
  Ctrl  : LongInt;
  PName : String;
  AName : String;
  Entry : LongInt;
  Value : String;

Begin
  PTd := GetTypeData(Self.ClassInfo);
  GetMem(PLst, PTd^.PropCount * SizeOf(Pointer));
  Siz := GetPropList(Self.ClassInfo, PLst);
  For Ctrl := 0 To Siz - 1 Do
  Begin
    PName := PLst^[Ctrl]^.Name;
    AName := ActionName(PName);
    Entry := Src.IndexOfName(AName);
    If Entry > -1 Then
    Begin
      Value := UnQuote(Src.Values[AName]);
      If PLst^[Ctrl]^.PropType^.Kind In [
        tkInteger, tkSet, tkBool, tkWChar, tkChar] Then
        SetOrdProp(Self, PName, StrToInt(Value));
      If PLst^[Ctrl]^.PropType^.Kind In [
        tkSString, tkLString, tkAString, tkWString] Then
        SetStrProp(Self, PName, Value);
      If PLst^[Ctrl]^.PropType^.Kind = tkFloat Then
        If PLst^[Ctrl]^.PropType^.Name = 'TDateTime' Then
          SetFloatProp(Self, PName, StrToDateTime(Value))
        Else
          SetFloatProp(Self, PName, StrToFloat(Value));
      If PLst^[Ctrl]^.PropType^.Kind = tkInt64 Then
        SetInt64Prop(Self, PName, StrToInt(Value));
      If PLst^[Ctrl]^.PropType^.Kind =  tkEnumeration Then
        SetEnumProp(Self, PName, Value);
    End;
  End;
  FreeMem(PLst, PTd^.PropCount * SizeOf(Pointer));
End;

Procedure TWebComponent.CascadeSave(Dst : TStringList);
Var
  Ctrl : LongInt;
Begin
  SaveMeToStringList(Dst);
  If GetCount > 0 Then
    For Ctrl := 0 To GetCount - 1 Do
      GetComponentByIndex(Ctrl).CascadeSave(Dst);
End;

Procedure TWebComponent.CascadeLoad(Src : TStringList);
Var
  Ctrl : LongInt;
Begin
  LoadMeFromStringList(Src);
  If GetCount > 0 Then
    For Ctrl := 0 To GetCount - 1 Do
      GetComponentByIndex(Ctrl).CascadeLoad(Src);
End;

Procedure TWebComponent.ExportMyProperties;
Var
  PLst  : PPropList;
  PTd   : PTypeData;
  Siz   : LongInt;
  Ctrl  : LongInt;
  PName : String;

Begin
  PTd := GetTypeData(Self.ClassInfo);
  GetMem(PLst, PTd^.PropCount * SizeOf(Pointer));
  Siz := GetPropList(Self.ClassInfo, PLst);
  For Ctrl := 0 To Siz - 1 Do
  Begin
    PName := PLst^[Ctrl]^.Name;
    If PLst^[Ctrl]^.PropType^.Kind In [
      tkInteger, tkSet, tkBool, tkWChar, tkChar] Then
      SetVar(PName, IntToStr(GetOrdProp(Self, PName)));
    If PLst^[Ctrl]^.PropType^.Kind In [
      tkSString, tkLString, tkAString, tkWString] Then
      SetVar(PName, GetStrProp(Self, PName));
    If PLst^[Ctrl]^.PropType^.Kind = tkFloat Then
      If PLst^[Ctrl]^.PropType^.Name = 'TDateTime' Then
        SetVar(PName, DateTimeToStr(GetFloatProp(Self, PName)))
      Else
        SetVar(PName, FloatToStr(GetFloatProp(Self, PName)));
    If PLst^[Ctrl]^.PropType^.Kind = tkInt64 Then
      SetVar(PName, IntToStr(GetInt64Prop(Self, PName)));
    If PLst^[Ctrl]^.PropType^.Kind =  tkEnumeration Then
      SetVar(PName, GetEnumProp(Self, PName));
  End;
  FreeMem(PLst, PTd^.PropCount * SizeOf(Pointer));
End;

Function UnQuote(Line : String): String;
Begin
  UnQuote := Copy(Line, 2, Length(Line) - 2);
End;

Procedure Run;
Var
  TheActions : TTokenList;
  States : TStringList;
Begin
  If FileExists(SelfReference + '.states') Then
  Begin
    States := TStringList.Create;
    States.LoadFromFile(SelfReference + '.states');
    Root.CascadeLoad(States);
    States.Free;
  End;
{  If IsSess('globalstate') Then
  Begin
    States := TStringList.Create;
    States.Text := GetSess('globalstate');
    Root.CascadeLoad(States);
    States.Free;
  End; }
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
{  States := TStringList.Create;
  SetSess('globalstate', States.Text);
  Root.CascadeSave(States);
  States.Free; }
  If Not(FileExists(SelfReference + '.states')) Then
  Begin
    States := TStringList.Create;
    Root.CascadeSave(States);
    States.SaveToFile(SelfReference + '.states');
    States.Free;
  End;
  Root.Template.Load;
  Root.Template.Emit;
End;

End.
