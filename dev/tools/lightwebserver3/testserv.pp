Uses
  CRT,
  SysUtils,
  HTTPServ;

Var
  Command    : String;

Procedure Help;
Begin
  WriteLn('Command usage: testserv docroot ip port timeout linger workersnum');
  Halt;
End;

Begin
  If ParamCount < 6 Then Help;
  Command := '';
  StartServer(ParamStr(1), ParamStr(2), ParamStr(3), StrToInt(ParamStr(4)), StrToInt(ParamStr(5)), StrToInt(ParamStr(6)));
  Repeat
    CheckIncomming;
    If KeyPressed Then
      ReadLn(Command)
    Else
      Command := '';
  Until Command = 'quit';
  FinishServer;
End.
