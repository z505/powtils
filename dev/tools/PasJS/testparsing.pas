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
Begin
  for c := 1 to 100 do
    writeint(c);    
End;

Begin
End.
