// ~NRCOL
Uses
  PWInit,
  PWMain,
  WebApplication,
  WebTemplate,
  PWVL,
  Classes,
  Sysutils;

Var
  MyWebGrid : TWebArrayGrid; 
  MyArray   : TArrayGrid;
  R, C      : Word;

Begin
  Randomize;
  SetLength(MyArray, 10);
  For R := Low(MyArray) To High(MyArray) Do
  Begin
    SetLength(MyArray[R], 10);
    For C := Low(MyArray[R]) To High(MyArray[R]) Do
      MyArray[R][C] := IntToStr(Round(Random * 100));
  End;
  WebAppInit('test');
  MyWebGrid  := TWebArrayGrid.Create('testgrid1', 'testgrid', Nil);
  MyWebGrid.Matrix  := MyArray;
  MyWebGrid.Editable := True;
  Run;
  MyWebGrid.Free;
  WebAppDone;
End.
