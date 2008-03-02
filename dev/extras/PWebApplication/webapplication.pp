// ~NRCOL
Unit WebApplication;

Interface
Uses
  PWMain,
	WebTemplate,
	Sysutils;

Type
	EWebActionList = Class(Exception);

	TWebComponent = Class;

  TWebActionHandler = Procedure Of Object;

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
		Function Find(Name : String): Boolean;
    Procedure CheckAction(Name : String);
		Property Actions[Name : String]: TWebActionHandler Read GetAction Write SetAction; Default;
	End;

	TWebComponent = Class
	Private
		fInstanceName : String;
		fOwner        : TWebComponent;
		fTemplate     : TWebTemplate;
    fOnDestroy    : TWebActionHandler;
	Public
		Constructor Create(Name : String; Owner : TWebComponent);
		Destructor Destroy; Override;
    Function UniqueName(SubName : String): String;
		Property InstanceName : String Read fInstanceName Write fInstanceName;
    Property Template     : TWebTemplate Read fTemplate Write fTemplate;
    Property OnDestroy    : TWebActionHandler Read fOnDestroy Write fOnDestroy;
	End;

ThreadVar
  GlobalActions : TWebActionList;
  RootTemplate  : TWebTemplate;
  OnBeforeRun   : TWebActionHandler;
  OnAfterRun    : TWebActionHandler;

Procedure WebAppInit(AppName : String);
Procedure WebAppDone;
Procedure RegisterAction(Name : String; Handler : TWebActionHandler);
Procedure Run;

Implementation

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
	For Ctrl := Low(fActions) To High(fActions) Do
		If fActions[Ctrl].Name = Name Then
		Begin
			Find := True;
			Exit;
		End;
End;

Procedure TWebActionList.CheckAction(Name : String);
Var
	Ctrl : Cardinal;
Begin
  If Length(fActions) > 0 Then
    For Ctrl := Low(fActions) To High(fActions) Do
		  If fActions[Ctrl].Name = Name Then
		  Begin
        fActions[Ctrl].Handler();
			  Exit;
		  End;
End;

Constructor TWebComponent.Create(Name : String; Owner : TWebComponent);
Begin
	Inherited Create;
	fOwner        := Owner;
	fInstanceName := Name;
  fTemplate     := TWebTemplate.Create(fInstanceName + '.tpl');
  If Assigned(fOwner) Then
    fOwner.Template.AddSubTemplate(fInstanceName, fTemplate)
  Else
    RootTemplate.AddSubTemplate(fInstanceName, fTemplate);
End;

Destructor TWebComponent.Destroy;
Begin
	If Assigned(fTemplate) Then
		fTemplate.Free;
	If Assigned(fOnDestroy) Then
    fOnDestroy();
	Inherited Destroy;
End;

Function TWebComponent.UniqueName(SubName : String): String;
Begin
  UniqueName := Self.fInstanceName + '_' + SubName;
End;

Procedure WebAppInit(AppName : String);
Begin
  GlobalActions := TWebActionList.Create;
  RootTemplate  := TWebTemplate.Create(AppName + '.tpl');
End;

Procedure WebAppDone;
Begin
  GlobalActions.Free;
  RootTemplate.Free;
End;

Procedure RegisterAction(Name : String; Handler : TWebActionHandler);
Begin
  GlobalActions[Name] := Handler;
End;

Procedure Run;
Begin
  If Assigned(OnBeforeRun) Then
    OnBeforeRun();
  If IsCGIVar('Action') Then
    GlobalActions.CheckAction(GetCGIVar('Action'))
  Else
    GlobalActions.CheckAction('default');
  RootTemplate.Load;
  RootTemplate.Emit;
  If Assigned(OnAfterRun) Then
    OnAfterRun();
End;

End.
