Uses Scanner, Classes, Sysutils;

Var
  MySourceFile : TFileStream;
  MyDefines    : TStringList;

Function OpenNewOne(FName : String): TStream;
Begin
  OpenNewOne := TFileStream.Create(FName, fmOpenRead);
End;

Procedure SaveToFile(Arq : String; Src : TTokenList);
Var
  Handler : Text;
  Y : LongInt;
Begin
  Assign(Handler, Arq);
  Rewrite(Handler);
  For Y := Low(Src) To High(Src) Do
  Begin
    WriteLn(Handler, '---');
    WriteLn(Handler, Ord(Src[Y].Kind));
    WriteLn(Handler, Src[Y].Value);
    WriteLn(Handler, Src[Y].Row);
    WriteLn(Handler, Src[Y].Col);
    WriteLn(Handler, Src[Y].SrcName);
    WriteLn(Handler, Ord(Src[Y].PreKind));
    WriteLn(Handler, Src[Y].RValue);
    WriteLn(Handler, Src[Y].LValue);
  End;
  Close(Handler);
End;

Begin
  MySourceFile := TFileStream.Create(ParamStr(1), fmOpenRead);
  MyDefines    := TStringList.Create;
  MyDefines.Sorted := True;
  MyDefines.Duplicates := dupIgnore;
  MyDefines.Add('pasjs');
  SaveToFile(ParamStr(2),
    Scanner.PreProcess(ParamStr(1), 
      Scanner.Scanner(ParamStr(1), MySourceFile),
      MyDefines, @OpenNewOne)
  );
  MySourceFile.Free;
  {$IFDEF pasjs}
  WriteLn('Hello javascript !');
  {$ELSE}
  WriteLn('Hello win32 !');
  {$ENDIF}
End.

