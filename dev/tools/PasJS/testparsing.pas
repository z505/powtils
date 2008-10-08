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
  b: integer;
  a: integer;
Begin
  while b and c or not b do
    for c := 1 to 100 do
    begin
      writeint(c);
      b := c = 10;
    end;
  if b = c then
  repeat
    a := a + 1;
  until a = 10
  else
  repeat
    a := a - 1;
  until a = 0;
End;

function test2: boolean;
begin
  result := 1;
end;

Begin
End.
