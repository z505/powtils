Uses Scanner, Parser, Classes, Sysutils, SyntaxTree, TokenIterator;

Var
  MySourceFile : TFileStream;
  MyDefines    : TStringList;
  MySyntax     : TTreeElementList;

Function OpenNewOne(FName : String): TStream;
Begin
  OpenNewOne := TFileStream.Create(FName, fmOpenRead);
End;

Begin
  Try
    MySourceFile := TFileStream.Create(ParamStr(1), fmOpenRead);
    MyDefines    := TStringList.Create;
    MyDefines.Sorted := True;
    MyDefines.Duplicates := dupIgnore;
    MyDefines.Add('pasjs');
    MySyntax := Parser.Parser(
      TTokenIterator.Create(
        Scanner.PreProcess(
          ParamStr(1),
          Scanner.Scanner(
            ParamStr(1), MySourceFile
          ),
          MyDefines,
          @OpenNewOne
        )
      )
    );
  Except
    On E: Exception Do
    Begin
      WriteLn(E.Message);
      MySyntax.Free;
      MySourceFile.Free;
      Halt;
    End;
  End;
  MySyntax.Free;
  MySourceFile.Free;
End.

