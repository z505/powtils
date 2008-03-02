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

Begin
  WebWrite('');
  WebAppInit('test');
  MyWebGrid  := TWebArrayGrid.Create('testgrid1', 'testgrid', Nil);
  SetLength(MyArray, 2);
  SetLength(MyArray[0], 2);
  SetLength(MyArray[1], 2);
  MyArray[0][0] := '1';
  MyArray[0][1] := '20';
  MyArray[1][0] := '0';
  MyArray[1][1] := '10';
  MyWebGrid.Matrix  := MyArray;
  Run;
  MyWebGrid.Free;
  WebAppDone;
End.
