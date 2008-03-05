// ~NRCOL
Unit WebApplication;

Interface
Uses
  PWMain,
	WebTemplate,
  Sysutils,
  XMLBase;

Type
	EWebActionList = Class(Exception);

  TTokenList = Array Of AnsiString;

	TWebComponent = Class;
  TWebActionList = Class;

  TWebEvent = Procedure Of Object;

  TWebActionHandler = Procedure(Actions : TTokenList; Depth : LongWord) Of Object;

	TWebActionEntry = Record
		Name    : String;
    Handler : TWebActionHandler;
	End;

  TWebActionList = Class
	Private
		fActions : Array Of TWebActionEntry;
		Function GetAction(Name : String): TWebActionHandler;
		Procedure SetAction(Name : String; Action : TWebActionHandler);
	Public
    ALName : String;
		Function Find(Name : String): Boolean;
    Procedure CheckAction(Actions : TTokenList; Depth : LongWord);
		Property Actions[Name : String]: TWebActionHandler Read GetAction Write SetAction; Default;
	End;

	TWebComponent = Class
	Private
		fInstanceName : String;
		fOwner        : TWebComponent;
		fTemplate     : TWebTemplate;
    fActions      : TWebActionList;
	Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
		Destructor Destroy; Override;
    Function CompleteActionName: String;
    Function ActionName(Name : String): String;
		Property InstanceName : String Read fInstanceName Write fInstanceName;
    Property Template     : TWebTemplate Read fTemplate Write fTemplate;
    Property Actions      : TWebActionList Read fActions Write fActions;
	End;

ThreadVar
  SelfReference : String;
  GlobalActions : TWebActionList;
  RootTemplate  : TWebTemplate;

Procedure WebAppInit(AppName : String);
Procedure WebAppDone;
Procedure Run;

Implementation

Function BreakApart(LineText : AnsiString): TTokenList;
Var
    Ctrl1      : Word;
    Ctrl2      : Word;
    LineTokens : TTokenList;
Begin
    Ctrl1 := 1;
    Ctrl2 := 0;
    If LineText <> '' Then
    Begin
        SetLength(LineTokens, 1);
        While Ctrl1 <= Length(LineText) Do
        Begin
            If LineText[Ctrl1] = '.' Then
            Begin
                SetLength(LineTokens, Length(LineTokens) + 1);
                Inc(Ctrl1);
                Inc(Ctrl2);
            End;
            LineTokens[Ctrl2] := LineTokens[Ctrl2] + LineText[Ctrl1];
            Inc(Ctrl1);
        End;
    End;
    BreakApart := LineTokens;
End;

Function TWebActionList.GetAction(Name : String): TWebActionHandler;
Var
	Ctrl  : Cardinal;
	Found : Boolean;
Begin
	Found := False;
	GetAction := Nil;
	For Ctrl := Low(fActions) To High(fActions) Do
		If fActions[Ctrl].Name = Name Then
		Begin
			GetAction := fActions[Ctrl].Handler;
			Found := True;
			Exit;
		End;
	If Not(Found) Then
		Raise EWebActionList.Create('No action handler named ' + Name + ' found !');
End;

Procedure TWebActionList.SetAction(Name : String; Action : TWebActionHandler);
Var
	Ctrl  : Cardinal;
	Found : Boolean;
Begin
	Found := False;
  If Length(fActions) > 0 Then
    For Ctrl := Low(fActions) To High(fActions) Do
      If fActions[Ctrl].Name = Name Then
      Begin
        fActions[Ctrl].Handler := Action;
        Found := True;
        Exit;
      End;
	If Not(Found) Then
	Begin
		SetLength(fActions, Length(fActions) + 1);
		fActions[High(fActions)].Name := Name;
		fActions[High(fActions)].Handler := Action;
	End;
End;

Function TWebActionList.Find(Name : String): Boolean;
Var
	Ctrl : Cardinal;
Begin
  Find := False;
  If Length(fActions) > 0 Then
    For Ctrl := Low(fActions) To High(fActions) Do
      If fActions[Ctrl].Name = Name Then
      Begin
        Find := True;
        Exit;
      End;
End;

Procedure TWebActionList.CheckAction(Actions : TTokenList; Depth : LongWord);
Var
	Ctrl : Cardinal;
Begin
  If Length(fActions) > 0 Then
    For Ctrl := Low(fActions) To High(fActions) Do
      If fActions[Ctrl].Name = Actions[Depth] Then
		  Begin
        fActions[Ctrl].Handler(Actions, Depth + 1);
			  Exit;
		  End;
End;

Constructor TWebComponent.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
	Inherited Create;
	fOwner        := Owner;
	fInstanceName := Name;
  fTemplate     := TWebTemplate.Create(Tmpl + '.tpl');
  fActions      := TWebActionList.Create;
  If Assigned(fOwner) Then
  Begin
    fOwner.Template.AddSubTemplate(fInstanceName, fTemplate);
    fOwner.Actions[fInstanceName] := Self.Actions.CheckAction;
  End
  Else
  Begin
    RootTemplate.AddSubTemplate(fInstanceName, fTemplate);
    GlobalActions.Actions[fInstanceName] := Self.Actions.CheckAction;
  End;
End;

Destructor TWebComponent.Destroy;
Begin
	If Assigned(fTemplate) Then
		fTemplate.Free;
  If Assigned(fActions) Then
    fActions.Free;
	Inherited Destroy;
End;

Function TWebComponent.CompleteActionName: String;
Begin
  If Assigned(fOwner) Then
    CompleteActionName := fOwner.CompleteActionName + fInstanceName
  Else
    CompleteActionName := fInstanceName;
End;

Function TWebComponent.ActionName(Name : String): String;
Begin
  ActionName := CompleteActionName + '.' + Name;
End;

Procedure WebAppInit(AppName : String);
Begin
  GlobalActions := TWebActionList.Create;
  RootTemplate  := TWebTemplate.Create(AppName + '.tpl');
  SelfReference := AppName {$IFDEF WINDOWS} + '.exe'{$ENDIF};
End;

Procedure WebAppDone;
Begin
  GlobalActions.Free;
  RootTemplate.Free;
End;

Procedure Run;
Var
  TheActions : TTokenList;
Begin
  If IsCGIVar('action') Then
  Begin
    TheActions := BreakApart(GetCGIVar('action'));
    GlobalActions.CheckAction(TheActions, 0);
  End
  Else
  Begin
    SetLength(TheActions, 1);
    TheActions[0] := 'default';
    GlobalActions.CheckAction(TheActions, 0);
  End;
  RootTemplate.Load;
  RootTemplate.Emit;
End;

End.
