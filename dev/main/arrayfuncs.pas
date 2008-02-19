{ Functions to assist array work.
  by Lars (L505) }
unit ArrayFuncs; {$IFDEF FPC}{$MODE OBJFPC}{$H+}{$ENDIF}

interface

type StrArray = array of string;

function AssignArray(src: array of string): StrArray;
procedure StrArrayAdd1(var a: StrArray; s: string);

implementation

{ assigns an array to a new location (like a copy) }
function AssignArray(src: array of string): StrArray;
var
  i: integer;
begin
  SetLength (result, Length(src));
  for i:= Low(src) to High(src) do
    result[i]:= src[i];
end;


procedure StrArrayAdd1(var a: StrArray; s: string);
var len: integer;
begin
  if s = '' then exit;
  len:= length(a);
  setlength(a, len + 1);
  a[len]:= s;
end;

end.
 
