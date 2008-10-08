Program teste;

Var
  var1, var2, var3, var4, var5 : byte;
  var6, var7, var8, var9, var10 : byte;

Procedure teste(var b : integer);
var c : string;
Begin
  If 2 = 2 then
    teste(1)
  Else
    teste(2);
End;

function hello: boolean;
begin
  hello := 1;
end;

Begin
  teste(2);
  teste(hello);
End.
