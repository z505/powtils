Program teste;

Var
  var1, var2, var3, var4, var5 : byte;
  var3, var4, var5, var6, var7 : byte;

Procedure teste(var a : integer);
Begin
  If 1 * a + 2 then
    document.writeln('teste')
  Else
    document.WriteLn('something wrong ! ifs should accept boolean expressions !')
End;

function hello: boolean;
begin
  teste := true;
end;

Begin
  teste(2);
  writeLn(hello);
End.
