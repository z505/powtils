Uses
  CRT,
  SysUtils,
  HTTPServ;

Var
  CheckCount : Integer;
  Command    : String;

Begin
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
