Uses
  CRT,
  SysUtils,
  HTTPServ;

Var
  CheckCount : Integer;
  Command    : String;

Procedure Help;
Begin
  WriteLn('Command usage: testserv docroot ip port timeout linger');
End;

Begin
  If ParamCount < 5 Then Help;

  StartServer(ParamStr(1), ParamStr(2), ParamStr(3), StrToInt(ParamStr(4)), StrToInt(ParamStr(5)));
  Repeat
    CheckIncomming;
    Inc(CheckCount);
    If (CheckCount Mod $FFFE) = 0 Then
      CheckFinished;
    If KeyPressed Then
      ReadLn(Command)
    Else
      Command := '';
  Until Command = 'quit';
  FinishServer;
End.
