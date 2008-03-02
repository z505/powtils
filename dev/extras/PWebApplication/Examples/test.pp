// ~NRCOL
Uses
  PWMain,
  WebApplication,
  WebTemplate,
  PWVL,
  Classes,
  Sysutils;

Var
  MyWebGrid : TWebArrayGrid;
  MyArray   : TArrayGrid;

Begin
  WebAppInit('test');
  MyWebGrid := TWebArrayGrid.Create('testgrid', Nil);
  SetLength(MyArray, 2);
  SetLength(MyArray[0], 2);
  SetLength(MyArray[1], 2);
  MyArray[0][0] := 'X Value';
  MyArray[0][1] := 'Y Value';
  MyArray[1][0] := '0';
  MyArray[1][1] := '10';
  MyWebGrid.Matrix := MyArray;
  Run;
  WebAppDone;
End.
