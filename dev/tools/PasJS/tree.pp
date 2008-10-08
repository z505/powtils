Unit Tree;

Interface

Uses Classes, SysUtils;

Type
  TLeafTreeElement = Class;

  TLeafTreeElement = Class
  Private
    fOwner : TLeafTreeElement;
  Public
    Constructor Create(O : TLeafTreeElement);
    Property Owner : TLeafTreeElement Read fOwner Write fOwner;
  End;

  TClassOfTLeafTreeElement = Class Of TLeafTreeElement;

  TTreeElement = Class(TLeafTreeElement)
  Private
    fCurrent,
    fMark : LongInt;
    fChilds : Array Of TLeafTreeElement;
    Function GetChild(Idx : LongInt): TLeafTreeElement;
    Procedure SetChild(Idx : LongInt; Chd : TLeafTreeElement);
    Function GetCurrentChild: TLeafTreeElement;
    Procedure SetCurrentChild(Chd : TLeafTreeElement);
  Public
    Constructor Create(O : TLeafTreeElement);
    Function Count: LongInt;
    Destructor Destroy; Override;
    Procedure AddChild(Child : TLeafTreeElement);
    Procedure Start;
    Procedure Next;
    Procedure Back;
    Procedure Mark;
    Procedure BackTrack;
    Function FindElement(Kind : TClassOfTLeafTreeElement): Boolean;
    Function EOE: Boolean;
    Function Last: Boolean;
    Property Childs[Idx : LongInt]: TLeafTreeElement Read GetChild Write SetChild; Default;
    Property Child: TLeafTreeElement Read GetCurrentChild Write SetCurrentChild;
  End;

Implementation

Constructor TLeafTreeElement.Create(O : TLeafTreeElement);
Begin
  Inherited Create;
  fOwner := O;
End;

Function TTreeElement.GetChild(Idx : LongInt): TLeafTreeElement;
Begin
  If (Count > 0) And (Idx In [Low(fChilds)..High(fChilds)]) Then
    GetChild := fChilds[Idx]
  Else
    Raise Exception.Create('Array index out of bounds : ' + IntToStr(Idx));
End;

Procedure TTreeElement.SetChild(Idx : LongInt; Chd : TLeafTreeElement);
Begin
  If (Count > 0) And (Idx In [Low(fChilds)..High(fChilds)]) Then
    fChilds[Idx] := Child
  Else
    Raise Exception.Create('Array index out of bounds : ' + IntToStr(Idx));
End;

Function TTreeElement.GetCurrentChild: TLeafTreeElement;
Begin
  GetCurrentChild := GetChild(fCurrent);
End;

Procedure TTreeElement.SetCurrentChild(Chd : TLeafTreeElement);
Begin
  SetChild(fCurrent, Chd);
End;

Constructor TTreeElement.Create(O : TLeafTreeElement);
Begin
  Inherited Create(O);
  fCurrent := Low(fChilds);
End;

Function TTreeElement.Count: LongInt;
Begin
  Count := Length(fChilds);
End;

Destructor TTreeElement.Destroy;
Var
  Ctrl : LongInt;
Begin
  For Ctrl := Low(fChilds) To High(fChilds) Do
    fChilds[Ctrl].Free;
  Inherited Destroy;
End;

Procedure TTreeElement.AddChild(Child : TLeafTreeElement);
Begin
  SetLength(fChilds, Length(fChilds) + 1);
  fChilds[High(fChilds)] := Child;
End;

Procedure TTreeElement.Start;
Begin
  fCurrent := Low(fChilds);
End;

Procedure TTreeElement.Next;
Begin
  Inc(fCurrent);
  If fCurrent > High(fChilds) + 1 Then
    fCurrent := High(fChilds);
End;

Procedure TTreeElement.Back;
Begin
  Dec(fCurrent);
  If fCurrent < Low(fChilds) - 1 Then
    fCurrent := Low(fChilds);
End;

Procedure TTreeElement.Mark;
Begin
  fMark := fCurrent;
End;

Procedure TTreeElement.BackTrack;
Begin
  fCurrent := fMark;
End;

Function TTreeElement.FindElement(Kind : TClassOfTLeafTreeElement): Boolean;
Var
  Ctrl : LongInt;
Begin
  FindElement := False;
  For Ctrl := Low(fChilds) To High(fChilds) Do
    If fChilds[Ctrl].ClassType = Kind Then
    Begin
      FindElement := True;
      fCurrent := Ctrl;
      Exit;
    End;
End;

Function TTreeElement.EOE: Boolean;
Begin
  EOE := fCurrent > High(fChilds);
End;

Function TTreeElement.Last: Boolean;
Begin
  Last := fCurrent >= High(fChilds);
End;

End.
