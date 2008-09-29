Unit StreamHelp;

Interface
Uses Classes;

Procedure Write(Dest : TStream; Var Data : AnsiString);
Procedure WriteLn(Dest : TStream; Var Data : AnsiString);

Implementation

Var
  CR: AnsiString = '' + #13;
  LF: AnsiString = '' + #10;

Procedure Write(Dest : TStream; Var Data : String);
Begin
  Dest.WriteBuffer(Pointer(Data)^, Length(Data));
End;

Procedure WriteLn(Dest : TStream; Var Data : String);
Begin
  Write(Dest, Data);
  Write(Dest, CR);
  Write(Dest, LF);
End;

End.
