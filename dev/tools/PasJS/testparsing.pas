Program teste;

javascript procedure writestr(x : string);
  'document.write(x);'
end;

javascript procedure writestrln(x : string);
  'document.write(x);'
end;

javascript procedure writeint(x : integer);
  'document.write(x);'
end;

javascript procedure writeintln(x : integer);
  'document.write(x);'
end;

procedure test;
var c : integer;
  b: boolean;
Begin
  while b do
    for c := 1 to 100 do
    begin
      writeint(c);
      b := c = 10;
    end;
End;

function test2: boolean;
begin
  result := 1;
end;

Begin
End.
