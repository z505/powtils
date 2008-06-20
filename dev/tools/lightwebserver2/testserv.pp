Uses
  SysUtils,
  HTTPServ;

Var
  CheckCount : Integer;

Begin
  StartServer(ParamStr(1), ParamStr(2), StrToInt(ParamStr(3)), StrToInt(ParamStr(3)));
  Repeat
    CheckIncomming;
    Inc(CheckCount);
    If (CheckCount Mod $FFFE) = 0 Then
      CheckFinished;
  Until False;
  FinishServer;
End.
