{ Functions to assist array work.
  by Lars (L505) }
unit ArrayFuncs; {$IFDEF FPC}{$MODE OBJFPC}{$H+}{$ENDIF} {$R+}

interface
uses
  pwtypes;

function AssignArray(src: array of string): AStrArray;
function AssignArray(src: str15array): str15array;
function AssignArray(src: str31array): str31array;

procedure StrArrayAdd1(var a: AStrArray; s: string);
function AssignArray(src: array of shortstring): ShortStrArray;


implementation

{ assigns an array to a new location (like a copy) }
function AssignArray(src: array of string): AStrArray;
var i: integer;
begin
  SetLength (result, Length(src));
  for i:= Low(src) to High(src) do result[i]:= src[i];
end;

function AssignArray(src: array of shortstring): ShortstrArray;
var i: integer;
begin
  SetLength (result, Length(src));
  for i:= Low(src) to High(src) do result[i]:= src[i];
end;

function AssignArray(src: str15array): str15array;
begin
  result:= src;
end;

function AssignArray(src: str31array): str31array;
begin
  result:= src;
end;

procedure StrArrayAdd1(var a: AstrArray; s: string);
var len: integer;
begin
  if s = '' then exit;
  len:= length(a);
  setlength(a, len + 1);
  a[len]:= s;
end;

end.
 
