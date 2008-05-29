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

Const
  ccCGIString      = 0;
  ccCGINumeric     = 1;
  ccCGIDate        = 2;
  ccCGITime        = 3;
  ccCGIDateAndTime = 4;
  ccCGIFloat       = 5;
  ccCGIInt64       = 6;
  ccCGIEnum        = 7;
  ccCGIBool        = 8;

Type
  // This class shows the contents of a TStringList
  TWebMemoShow = Class(TWebComponent)
  Private
    fMemoSource : TStringList;
    fCurLine    : LongInt;
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
    fCurComponentNum : LongInt;
    fOnShowComponent : TWebEvent;
    Function GetCurCaption: String;
    Function GetIfCurVisible: Boolean;
    Procedure SetIfCurVisible(Vis : Boolean);
  Protected
    Procedure ComponentList(Caller : TXMLTag);
    Procedure ComponentCurrent(Caller : TXMLTag);
    Procedure ComponentNext(Caller : TXMLTag);
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
  Published
    Property OnShowComponent : TWebEvent Read fOnShowComponent Write fOnShowComponent;
    Property CurComponentNum : LongInt Read fCurComponentNum;
    Property CurCaption : String Read GetCurCaption;
    Property CurVisible : Boolean Read GetIfCurVisible Write SetIfCurVisible;
  End;

  // This component inherits TWebComponentList to allow the user
  // to change the visible state of each subcomponent
  TWebPageDrawer = Class(TWebComponentList)
  Private
    fOnChange : TWebEvent;
  Protected
    Procedure DrawerClick(Actions : TTokenList; Depth : LongWord);
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
  Published
    Property OnChange : TWebEvent Read fOnChange Write fOnChange;
  End;

  // This component inherits TWebComponentList to allow the user
  // to select only one of the child components that be shown
  TWebPageFlipper = Class(TWebComponentList)
  Private
    fSelected : LongInt;
    fOnSelect : TWebEvent;
  Protected
    Procedure FlipperSelect(Actions : TTokenList; Depth : LongWord);
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
  Published
    Property OnSelect : TWebEvent Read fOnSelect Write fOnSelect;
    Property Selected : LongInt Read fSelected Write fSelected;
  End;

  // This component inherits TWebComponentList to allow the user
  // to scroll back and forth between child components
  TWebPageScroller = Class(TWebComponentList)
  Private
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
  End;

  // This component shows a frame where users can edit its properties
  TWebEditPage = Class(TWebComponent)
  Private
    fOnSubmit : TWebEvent;
    fWhiteList : Array Of Record
        Name : String;
        Kind : Byte;
      End;
    fActive : Boolean;
  Protected
    Procedure EditSubmit(Action : TTokenList; Depth : LongWord);
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
    Procedure WhiteList(Name : String; Kind : Byte);
  Published
    Property OnSubmit : TWebEvent Read fOnSubmit Write fOnSubmit;
    Property Active : Boolean Read fActive Write fActive;
  End;

  TWebLoginBox = Class;

  TWebLoginController = Class
  Private
    Function GetLogin: String;
    Function GetState: Boolean;
  Protected
    fView : TWebLoginBox;
  Public
    Constructor Create(aView : TWebLoginBox);
    Function Login: Boolean; Virtual; Abstract;
    Function Logout: Boolean; Virtual; Abstract;
    Function Change(NewPass : String): Boolean; Virtual; Abstract;
  Published
    Property LoginName : String Read GetLogin;
    Property Logged : Boolean Read GetState;
  End;

  TWebLoginBox = Class(TWebEditPage)
  Private
    fLogin,
    fPassword   : String;
    fLoginMan   : TWebLoginController;
    fLogged     : Boolean;
    fOnLogin    : TWebEvent;
    fOnLogout   : TWebEvent;
    fOnLoginErr : TWebEvent;
    Procedure DefaultLoginErr;
  Protected
    Procedure UponLogin;
    Procedure Logout(Action : TTokenList; Depth : LongWord);
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
  Published
    Property Login : String Read fLogin Write fLogin;
    Property Password : String Read fPassword Write fPassword;
    Property LoginController : TWebLoginController Read fLoginMan Write fLoginMan;
    Property Logged : Boolean Read fLogged Write fLogged;
    Property OnLogin : TWebEvent Read fOnLogin Write fOnLogin;
    Property OnLogout : TWebEvent Read fOnLogout Write fOnLogout;
    Property OnLoginErr : TWebEvent Read fOnLoginErr Write fOnLoginErr;
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

Function TWebComponentList.GetCurCaption: String;
Begin
  If fCurComponentNum < Count Then
    Result := ComponentByIndex[fCurComponentNum].Caption;
End;

Function TWebComponentList.GetIfCurVisible: Boolean;
Begin
  GetIfCurVisible := (fCurComponentNum < Count) And ComponentByIndex[fCurComponentNum].Visible;
End;

Procedure TWebComponentList.SetIfCurVisible(Vis : Boolean);
Begin
End;

Procedure TWebComponentList.ComponentList(Caller : TXMLTag);
Begin
  fCurComponentNum := 0;
  While fCurComponentNum < Count Do
    Caller.EmitChilds;
End;

Procedure TWebComponentList.ComponentCurrent(Caller : TXMLTag);
Begin
  If fCurComponentNum < Count Then
  Begin
    SetVar('self', SelfReference);
    ComponentByIndex[fCurComponentNum].ExportMyProperties;
    ComponentByIndex[fCurComponentNum].Template.LoadAndEmit;
    If Assigned(fOnShowComponent) Then
      fOnShowComponent();
    SetVar('self', SelfReference);
    ExportMyProperties;
  End;
End;

Procedure TWebComponentList.ComponentNext(Caller : TXMLTag);
Begin
  Inc(fCurComponentNum);
End;

Constructor TWebComponentList.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
  Inherited Create(Name, Tmpl, Owner);
  Template.Tag['components.list'] := Self.ComponentList;
  Template.Tag['components.current'] := Self.ComponentCurrent;
  Template.Tag['components.next'] := Self.ComponentNext;
  fCurComponentNum := 0;
End;

// TWebPageDrawer

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
  Actions['toggle'] := Self.DrawerClick;
End;

// TWebPageFlipper

Procedure TWebPageFlipper.FlipperSelect(Actions : TTokenList; Depth : LongWord);
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

Constructor TWebPageFlipper.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
  Inherited Create(Name, Tmpl, Owner);
  Actions['select'] := Self.FlipperSelect;
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
        End
End;

Constructor TWebPageScroller.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
  Inherited Create(Name, Tmpl, Owner);
  Actions['next'] := Self.ScrollerNext;
  Actions['previous'] := Self.ScrollerPrevious;
End;

// TWebEditPage

Procedure TWebEditPage.EditSubmit(Action : TTokenList; Depth : LongWord);
Var
  PLst  : PPropList;
  PTd   : PTypeData;
  Siz   : LongInt;
  Ctrl  : LongInt;
  PName : String;
  Value : String;
  Kind  : LongInt;

  Function IsWhiteListed: Boolean;
  Var
    Ctrl : LongWord;
  Begin
    Result := False;
    If Length(fWhiteList) > 0 Then
      For Ctrl := 0 To Length(fWhiteList) - 1 Do
        If fWhiteList[Ctrl].Name = PName Then
        Begin
          Result := True;
          Exit;
        End
        Else
    Else
      Result := False;
  End;

  Function GetWhiteType: LongInt;
  Var
    Ctrl : LongWord;
  Begin
    Result := -1;
    If Length(fWhiteList) > 0 Then
      For Ctrl := 0 To Length(fWhiteList) - 1 Do
        If fWhiteList[Ctrl].Name = PName Then
        Begin
          Result := fWhiteList[Ctrl].Kind;
          Exit;
        End
        Else
    Else
      Result := -1;
  End;

Begin
  If fActive Then
  Begin
    PTd := GetTypeData(Self.ClassInfo);
    GetMem(PLst, PTd^.PropCount * SizeOf(Pointer));
    Siz := GetPropList(Self.ClassInfo, PLst);
    For Ctrl := 0 To Siz - 1 Do
    Begin
      PName := PLst^[Ctrl]^.Name;
      If IsCGIVar(PName) And IsWhiteListed Then
      Begin
        Value := GetCGIVar(PName);
        Kind := GetWhiteType;
        Case Kind Of
        ccCGINumeric:
          Try
            SetOrdProp(Self, PName, StrToInt(Value));
          Except
            On E: Exception Do
            Begin
              Self.Error := True;
              Self.ErrorValue := Value + ' is not a valid integer numeric.';
            End;
          End;
        ccCGIString:
          Try
            SetStrProp(Self, PName, Value);
          Except
            On E: Exception Do
            Begin
              Self.Error := True;
              Self.ErrorValue := Value + ' is not a valid string (?).';
            End;
          End;
        ccCGIDate:
          Try
            SetFloatProp(Self, PName, StrToDate(Value))
          Except
            On E: Exception Do
            Begin
              Self.Error := True;
              Self.ErrorValue := Value + ' is not a valid date.';
            End;
          End;
        ccCGITime:
          Try
            SetFloatProp(Self, PName, StrToTime(Value))
          Except
            On E: Exception Do
            Begin
              Self.Error := True;
              Self.ErrorValue := Value + ' is not a valid time.';
            End;
          End;
        ccCGIDateAndTime:
          Try
            SetFloatProp(Self, PName, StrToDateTime(Value))
          Except
            On E: Exception Do
            Begin
              Self.Error := True;
              Self.ErrorValue := Value + ' is not a valid date and time.';
            End;
          End;
        ccCGIFloat:
          Try
            SetFloatProp(Self, PName, StrToFloat(Value));
          Except
            On E: Exception Do
            Begin
              Self.Error := True;
              Self.ErrorValue := Value + ' is not a valid floating point number.';
            End;
          End;
        ccCGIInt64:
          Try
            SetInt64Prop(Self, PName, StrToInt(Value));
          Except
            On E: Exception Do
            Begin
              Self.Error := True;
              Self.ErrorValue := Value + ' is not a valid 64bits integer.';
            End;
          End;
        ccCGIEnum:
          Try
            SetEnumProp(Self, PName, Value);
          Except
            On E: Exception Do
            Begin
              Self.Error := True;
              Self.ErrorValue := Value + ' is not a valid value for this field.';
            End;
          End;
        ccCGIBool:
          Try
            SetOrdProp(Self, PName, Ord(StrToBool(Value)));
          Except
            On E: Exception Do
            Begin
              Self.Error := True;
              Self.ErrorValue := Value + ' is not a valid boolean, must be either true or false.';
            End;
          End;
        End;
      End;
    End;
    FreeMem(PLst, PTd^.PropCount * SizeOf(Pointer));
    If Assigned(fOnSubmit) Then
      fOnSubmit();
  End;
End;

Constructor TWebEditPage.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
  Inherited Create(Name, Tmpl, Owner);
  Actions['submit'] := Self.EditSubmit;
End;

Procedure TWebEditPage.WhiteList(Name : String; Kind : Byte);
Begin
  SetLength(fWhiteList, Length(fWhiteList) + 1);
  fWhiteList[High(fWhiteList)].Name := Name;
  fWhiteList[High(fWhiteList)].Kind := Kind;
End;

// TWebLoginController

Function TWebLoginController.GetLogin: String;
Begin
  If Assigned(fView) Then
    GetLogin := fView.Login
  Else
    GetLogin := '';
End;

Function TWebLoginController.GetState: Boolean;
Begin
  If Assigned(fView) Then
    GetState := fView.Logged
  Else
    GetState := False;
End;

Constructor TWebLoginController.Create(aView : TWebLoginBox);
Begin
  Inherited Create;
  fView     := aView;
End;

// TWebLoginBox

Procedure TWebLoginBox.DefaultLoginErr;
Begin
  Error := True;
  ErrorValue := 'Login or logout not sucessfull.';
End;

Procedure TWebLoginBox.UponLogin;
Begin
  If Assigned(fLoginMan) Then
    If fLoginMan.Login Then
    Begin
      fLogged := True;
      Error := False;
      ErrorValue := '';
      If Assigned(fOnLogin) Then
        fOnLogin()
    End
    Else
    Begin
      DefaultLoginErr;
      If Assigned(fOnLoginErr) Then
        fOnLoginErr();
    End
  Else
  Begin
    Error := True;
    ErrorValue := 'No Login Manager associated.';
  End;
  Active := Not(fLogged);
End;

Procedure TWebLoginBox.Logout(Action : TTokenList; Depth : LongWord);
Begin
  If Assigned(fLoginMan) Then
    If fLoginMan.Logout Then
    Begin
      fLogged := False;
      Error := False;
      ErrorValue := '';
      If Assigned(fOnLogout) Then
        fOnLogout();
    End
    Else
    Begin
      DefaultLoginErr;
      If Assigned(fOnLoginErr) Then
        fOnLoginErr();
    End
  Else
  Begin
    Error := True;
    ErrorValue := 'No Login Manager associated.';
  End;
  Active := Not(fLogged);
  If Not(fLogged) Then
  Begin
    fLogin := '';
    fPassword := '';
  End;
End;

Constructor TWebLoginBox.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
  Inherited Create(Name, Tmpl, Owner);
  OnSubmit := Self.UponLogin;
  WhiteList('Login', ccCGIString);
  WhiteList('Password', ccCGIString);
  Actions['logout']       := Self.Logout;
End;

End.
