// ~NRCOL

{****************************************************
 See Licence.txt for details
 Copyright(C) 2007 2008 - Jorge Aldo G. de F. Junior
 Colaborators:
 <put your name here if you improve this software>
*****************************************************}

Unit WebTemplate;

Interface

Uses
  Classes,
  PWMain,
  SysUtils,
  XMLBase,
  XMLLoader,
  XMLParser;

Type
  EWebTemplate = Class(Exception);

  TWebEvent = Procedure Of Object;

	TWebTemplateList = Class;

  TWebTemplate = Class
  Protected
    fFileName:  String;
    fNameSpace: TXMLNodeList;
    fRootTag:   TXMLTagCollection;
    fLoaded:    Boolean;
    fConditional: TStringList;
    fSubTemplates : TWebTemplateList;
    Procedure ConditionalTag(Caller : TXMLTag);
    Procedure SubTmplCall(Caller : TXMLTag);
    Function GetCondition(Name : String): Boolean;
    Procedure SetCondition(Name : String; State : Boolean);
  Public
    Constructor Create(Arq: String);
    Destructor Destroy; Override;
    Procedure Emit;
    Procedure Load;
    Procedure LoadAndEmit;
		Procedure AddSubTemplate(Name : String; Template : TWebTemplate);
    Procedure ToggleCondition(Name : String);
    Property NameSpace: TXMLNodeList Read fNameSpace;
    Property Tag: TXMLNodeList Read fNameSpace;
    Property Condition[Name : String]: Boolean Read GetCondition Write SetCondition;
    Property SubTemplates: TWebTemplateList Read fSubTemplates;
  End;

	TWebTemplateList = Class
	Private
		fTemplates : TStringList;
		Function GetTemplate(Name : String): TWebTemplate;
		Procedure SetTemplate(Name : String; Template : TWebTemplate);
    Function GetTemplateByNumber(Idx: Longint): TWebTemplate;
    Function GetCount: Longint;
	Public
		Constructor Create;
		Destructor Destroy; Override;
		Property Templates[Name : String]: TWebTemplate Read GetTemplate Write SetTemplate; Default;
    Property TemplateByNumber[Idx : LongInt]: TWebTemplate Read GetTemplateByNumber;
    Property Count: LongInt Read GetCount;
	End;

Function UnQuote(Line : String): String;

Implementation

Function UnQuote(Line : String): String;
Begin
  UnQuote := Copy(Line, 2, Length(Line) - 2);
End;

Procedure TWebTemplate.ConditionalTag(Caller : TXMLTag);
Begin
  If ((Caller.Attributes.IndexOfName('when') > -1) And
  (fConditional.IndexOf(UnQuote(Caller.Attributes.Values['when'])) > -1)) Or
  ((Caller.Attributes.IndexOfName('notwhen') > -1) And
  (fConditional.IndexOf(UnQuote(Caller.Attributes.Values['notwhen'])) = -1)) Then
    Caller.EmitChilds;
End;

Procedure TWebTemplate.SubTmplCall(Caller : TXMLTag);
Begin
  fSubTemplates[UnQuote(Caller.Attributes.Values['name'])].Load;
  fSubTemplates[UnQuote(Caller.Attributes.Values['name'])].Emit;
End;

Function TWebTemplate.GetCondition(Name : String): Boolean;
Begin
  GetCondition := fConditional.IndexOf(Name) > - 1;
End;

Procedure TWebTemplate.SetCondition(Name : String; State : Boolean);
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

Constructor TWebTemplate.Create(Arq: String);
Begin
  Inherited Create;
  fFileName  := Arq;
  fLoaded    := False;
  fNameSpace := TXMLNodeList.Create;
  fNameSpace['if'] := ConditionalTag;
  fNameSpace['subtemplate'] := SubTmplCall;
  fSubTemplates := TWebTemplateList.Create;
  fConditional := TStringList.Create;
End;

Destructor TWebTemplate.Destroy;
Begin
  fConditional.Free;
  fSubTemplates.Free;
  fNameSpace.Free;
  Inherited Destroy;
End;

Procedure TWebTemplate.Load;
Var
  fSourceTokens:  TTokenList;
  fSourceStream:  TFileStream;
  fTokenIterator: TTokenIterator;
Begin
  Try
    fSourceStream := TFileStream.Create(fFileName, fmOpenRead);
    fSourceTokens := ParseXML(fSourceStream);
    fTokenIterator := TTokenIterator.Create(fSourceTokens);
    fRootTag := XMLLoad(fTokenIterator, fNameSpace);
    fLoaded  := True;
  Except
  On E: Exception Do
    WriteLn(fFileName, ': ', E.Message);
  End;
  fSourceStream.Free;
  fTokenIterator.Free;
End;

Procedure TWebTemplate.Emit;
Begin
  If fLoaded Then
    fRootTag.Emit
  Else
    Raise EWebTemplate.Create('Cannot emit until a template is loaded !');
End;

Procedure TWebTemplate.LoadAndEmit;
Begin
  Load;
  fRootTag.Emit;
End;

Procedure TWebTemplate.AddSubTemplate(Name : String; Template : TWebTemplate);
Begin
	fSubTemplates[Name] := Template;
End;

Procedure TWebTemplate.ToggleCondition(Name : String);
Var
  Idx : LongInt;
Begin
  Idx := fConditional.IndexOf(Name);
  If Idx = -1 Then
    fConditional.Add(Name)
  Else
    fConditional.Delete(Idx);
End;

Function TWebTemplateList.GetTemplate(Name : String): TWebTemplate;
Var
	Ctrl : Int64;
Begin
	Ctrl := fTemplates.IndexOf(Name);
	If Ctrl >= 0 Then
    GetTemplate := (fTemplates.Objects[Ctrl] As TWebTemplate)
	Else
	Begin
		GetTemplate := Nil;
		Raise EWebTemplate.Create('template ' + Name + ' not loaded !');
	End;
End;

Procedure TWebTemplateList.SetTemplate(Name : String; Template : TWebTemplate);
Var
	Ctrl : Int64;
Begin
	Ctrl := fTemplates.IndexOf(Name);
	If Ctrl < 0 Then
		Ctrl := fTemplates.Add(Name);
  fTemplates.Objects[Ctrl] := (Template As TObject);
End;

Function TWebTemplateList.GetTemplateByNumber(Idx: Longint): TWebTemplate;
Begin
  GetTemplateByNumber := (fTemplates.Objects[Idx] As TWebTemplate);
End;

Function TWebTemplateList.GetCount: Longint;
Begin
  GetCount := fTemplates.Count;
End;

Constructor TWebTemplateList.Create;
Begin
	Inherited Create;
	fTemplates            := TStringList.Create;
	fTemplates.Duplicates := dupIgnore;
End;

Destructor TWebTemplateList.Destroy;
Begin
  fTemplates.Free;
	Inherited Destroy;
End;

End.
