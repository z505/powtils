Unit Source;

Interface

Uses Classes;

Type
  TSource = Class
  Private
    fName    : String;
    fSource  : TStream;
    fRow,
    fCol,
    fTmpRow,
    fTmpCol  : Longint;
    fNext    : Char;
  Public
    Constructor Create(Name : String; Src : TStream);
    Procedure Mark;
    Function Next    : Char;
    Function EOS     : Boolean;
    Property Name    : String Read fName;
    Property Row     : Longint Read fRow;
    Property Col     : Longint Read fCol;
    Property Current : Char Read fNext;
  End;

Implementation

Constructor TSource.Create(Name : String; Src : TStream);
Begin
  Inherited Create;
  fSource  := Src;
  fName    := Name;
  fTmpRow  := 1;
  fTmpCol  := 0;
  fSource.Seek(0, soFromBeginning);
End;

Procedure TSource.Mark;
Begin
  fRow := fTmpRow;
  fCol := fTmpCol;
End;

Function TSource.Next : Char;
Begin
  fNext := Char(fSource.ReadByte);
  Case fNext Of
  #10 :
    Begin
      Inc(fTmpRow);
      fTmpCol := 0;
    End;
  #13 :
    Begin
    End;
  Else
      Inc(fTmpCol);
  End;
  Next := fNext;
End;

Function TSource.EOS : Boolean;
Begin
  EOS := fSource.Position >= fSource.Size;
End;

End.
