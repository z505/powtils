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
  MyWebMemo : TWebMemo; 
  MyLines   : TStringList;

Begin
  MyLines := TStringList.Create;
  MyLines.Add('Hello !');
  MyLines.Add('This is a web memo test !');
  MyLines.Add('You can make a flip/flop memo');
  MyLines.Add('wich is easier to read, or you');
  MyLines.Add('can put any formating you like');
  MyLines.Add('on this object template.');
  WebAppInit('test2');
  MyWebMemo  := TWebMemo.Create('testmemo1', 'testmemo', Nil);
  MyWebMemo.Lines := MyLines;
  Run;
  MyWebMemo.Free;
  MyLines.Free;
  WebAppDone;
End.
