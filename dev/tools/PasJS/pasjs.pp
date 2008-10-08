Uses Scanner, Parser, Classes, Sysutils, SyntaxTree, TokenIterator, SymbolTree;

Var
  MySourceFile : TFileStream;
  MyDefines    : TStringList;
  MySyntax     : TTokenTreeElement;
  MySymbol     : TSymbolProgram;

Function OpenNewOne(FName : String): TStream;
Begin
  OpenNewOne := TFileStream.Create(FName, fmOpenRead);
End;

Var
  Handler : Text;

Procedure DumpSymbols(Src : TSymbolTreeElement);
Var
  Buffer : Array Of String;
  X : LongInt;
Begin
  SetLength(Buffer, 0);
  If Assigned(Src.Owner) Then
  Begin
    Write(Handler, '(');
    Src.Start;
    If Src.FindElement(TSymbolParameter) Then
      Repeat
        If Src.Child Is TSymbolParameter Then
        Begin
          SetLength(Buffer, Length(Buffer) + 1);
          Buffer[High(Buffer)] := (Src.Child As TSymbolParameter).Name;
        End;
        Src.Next;
      Until Src.EOE;
    If Length(Buffer) > 0 Then
      For X := Low(Buffer) To High(Buffer) Do
        If X < High(Buffer) Then
          Write(Handler, Buffer[X], ', ')
        Else
          Write(Handler, Buffer[X]);
    WriteLn(Handler, ');');  
    WriteLn(Handler, '{');
  End;
  Src.Start;
  If Src.Count > 0 Then
  Repeat
    If Src.Child Is TSymbolVar Then
    Begin
      Write  (Handler, 'var');
      Write  (Handler, ' ' + (Src.Child As TSymbolTreeElement).Name);
      WriteLn(Handler, ';');
    End
    Else If Src.Child Is TSymbolParameter Then
    Begin
    End
    Else If Src.Child Is TSymbolFunction Then
    Begin
      Write  (Handler, 'function');
      Write  (Handler, ' ' + (Src.Child As TSymbolTreeElement).Name);
      DumpSymbols((Src.Child As TSymbolTreeElement));
    End
    Else If Src.Child Is TSymbolProcedure Then
    Begin
      Write  (Handler, 'function');
      Write  (Handler, ' ' + (Src.Child As TSymbolTreeElement).Name);
      DumpSymbols((Src.Child As TSymbolTreeElement));
    End
    Else If Src.Child Is TSymbolJavaFunction Then
    Begin
      Write  (Handler, 'function');
      Write  (Handler, ' ' + (Src.Child As TSymbolTreeElement).Name);
      DumpSymbols((Src.Child As TSymbolTreeElement));
    End
    Else If Src.Child Is TSymbolJavaProcedure Then
    Begin
      Write  (Handler, 'function');
      Write  (Handler, ' ' + (Src.Child As TSymbolTreeElement).Name);
      DumpSymbols((Src.Child As TSymbolTreeElement));
    End;
    Src.Next;
  Until Src.EOE;
  If (Src Is TSymbolFunction) Or (Src Is TSymbolProcedure) Then
    Write(Handler, (Src As TSymbolWithAttached).Attached.Generate);
  If (Src Is TSymbolJavaFunction) Then
    WriteLn(Handler, (Src As TSymbolJavaFunction).Java)
  Else If (Src Is TSymbolJavaProcedure) Then
    WriteLn(Handler, (Src As TSymbolJavaProcedure).Java);
  If Assigned(Src.Owner) Then
    WriteLn(Handler, '}');
  WriteLn(Handler, '');
End;

Begin
//  Try
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
    MySymbol := TSymbolProgram.Create(MySyntax);
    MySymbol.BuildVisibility;
    MySyntax.VerifyIdentifiers;
    // MySyntax.VerifyTypes;
    Assign(Handler, ParamStr(2));
    Rewrite(Handler);
    DumpSymbols(MySymbol);
    Close(Handler);
//  Except
//    On E: Exception Do
//    Begin
//      WriteLn(E.Message);
//      MySyntax.Free;
//      MySourceFile.Free;
//      MySymbol.Free;
//      Halt;
//    End;
//  End;
  MyDefines.Free;
  MySyntax.Free;
  MySourceFile.Free;
  MySymbol.Free;
End.

