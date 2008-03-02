// ~NRCOL
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

	TWebTemplateConditional = Function(Name : String): Boolean Of Object;

	TWebTemplateList = Class;

  TWebTemplate = Class
  Protected
    fFileName:  String;
    fNameSpace: TXMLNodeList;
    fRootTag:   TXMLTagCollection;
    fLoaded:    Boolean;
		fConditional: TWebTemplateConditional;
		fSubTemplates : TWebTemplateList;
		Procedure ConditionalTag(Caller : TXMLTag);
		Procedure SubTemplateCall(Caller : TXMLTag);
  Public
    Constructor Create(Arq: String);
    Destructor Destroy; Override;
    Procedure Emit;
    Procedure Load;
    Procedure LoadAndEmit;
		Procedure AddSubTemplate(Name : String; Template : TWebTemplate);
    Property NameSpace: TXMLNodeList Read fNameSpace;
    Property Tag: TXMLNodeList Read fNameSpace;
		Property Conditional : TWebTemplateConditional Read fConditional Write fConditional;
  End;

	TWebTemplateList = Class
	Private
		fTemplates : TStringList;
		Function GetTemplate(Name : String): TWebTemplate;
		Procedure SetTemplate(Name : String; Template : TWebTemplate);
	Public
		Constructor Create;
		Destructor Destroy; Override;
		Property Templates[Name : String]: TWebTemplate Read GetTemplate Write SetTemplate; Default;
	End;

Implementation

Procedure TWebTemplate.ConditionalTag(Caller : TXMLTag);
Begin
	If Assigned(fConditional) And fConditional(Caller.Attributes.Values['when']) Then
		Caller.EmitChilds;
End;

Procedure TWebTemplate.SubTemplateCall(Caller : TXMLTag);
Begin
  fSubTemplates[Caller.Attributes.Values['name']].LoadAndEmit;
End;

Constructor TWebTemplate.Create(Arq: String);
Begin
  Inherited Create;
  fFileName  := Arq;
  fLoaded    := False;
  fNameSpace := TXMLNodeList.Create;
	fNameSpace['if'] := @ConditionalTag;
	fNameSpace['subtemplate'] := @SubTemplateCall;
End;

Destructor TWebTemplate.Destroy;
Begin
  fNameSpace.Free;
  Inherited Destroy;
End;

Procedure TWebTemplate.Load;
Var
  fSourceTokens:  TTokenList;
  fSourceStream:  TFileStream;
  fTokenIterator: TTokenIterator;
Begin
  fSourceStream := TFileStream.Create(fFileName, fmOpenRead);
  fSourceTokens := ParseXML(fSourceStream);
  fTokenIterator := TTokenIterator.Create(fSourceTokens);
  fRootTag := XMLLoad(fTokenIterator, fNameSpace);
  fLoaded  := True;
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

Function TWebTemplateList.GetTemplate(Name : String): TWebTemplate;
Var
	Ctrl : Int64;
Begin
	Ctrl := fTemplates.IndexOf(Name);
	If Ctrl >= 0 Then
		GetTemplate := TWebTemplate(fTemplates.Objects[Ctrl])
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
	fTemplates.Objects[Ctrl] := TObject(Template);
End;

Constructor TWebTemplateList.Create;
Begin
	Inherited Create;
	fTemplates            := TStringList.Create;
	fTemplates.Duplicates := dupIgnore;
	fTemplates.Sorted     := True;
End;

Destructor TWebTemplateList.Destroy;
Var
	Ctrl : LongWord;
Begin
	If fTemplates.Count > 0 Then
		For Ctrl := 0 To fTemplates.Count - 1 Do
			If Assigned(fTemplates.Objects[Ctrl]) Then
				TWebTemplate(fTemplates.Objects[Ctrl]).Free;
	fTemplates.Free;
	Inherited Destroy;
End;

End.
