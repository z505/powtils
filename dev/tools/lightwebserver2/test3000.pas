{ This will test "the fucking 1000" problem of LWS2 on unix }

{$mode objfpc}{$H+}

uses baseunix, unix;

procedure Fuck1000_1; var i: longint;
var err: longint = 0; cnt: longint = 0;
begin
  for i:= 1 to 3000 do begin
    err:= fpSystem('df &');
    inc(cnt);
    writeln('DEBUG: COUNTER: ', cnt);
    writeln('DEBUG: ERROR RETURN: ', err);
  end;
end;

procedure Fuck1000_2;
begin

end;

procedure Fuck1000_3;
begin

end;


procedure Fuck1000_4;
begin

end;



begin
  Fuck1000_1;
  Fuck1000_2;
  Fuck1000_3;
  Fuck1000_4;
end.