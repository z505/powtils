// ~NRCOL

{****************************************************
 See Licence.txt for details
 Copyright(C) 2007 2008 - Jorge Aldo G. de F. Junior
 Colaborators:
 <put your name here if you improve this software>
*****************************************************}

Unit WebAction;

Interface
Uses
  PWMain,
  Classes,
  Sysutils;

Type
	EWebActionList = Class(Exception);

  TTokenList = Array Of AnsiString;

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

Function BreakApartNames(LineText : AnsiString): TTokenList;

Implementation

Function BreakApartNames(LineText : AnsiString): TTokenList;
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
    BreakApartNames := LineTokens;
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

End.
