unit PCharUtils;

{$ifdef fpc}
 {$mode objfpc} {$h+}
{$endif}

interface

procedure PCharCopy(const source: pchar; dest: pchar); overload;
procedure PcharCopy(const source: pchar; dest: pchar; count: integer); overload;
procedure PcharCat(const source: pchar; dest: pchar);

function NewPchar(const s: string; out rslt: pchar): boolean;
procedure RidPChar(rslt: pchar);

implementation

procedure PcharCopy(const source: pchar; dest: pchar); overload;
begin
  if source = nil then exit;
  if dest = nil then exit;
  move(source^, dest^, length(source) + 1);
end;

procedure PcharCopy(const source: pchar; dest: pchar; count: integer); overload;
begin
  if source = nil then exit;
  if dest = nil then exit;
  move(source^, dest^, count);
end;

{ concatenation of two pchars, memory not managed automatically }
procedure PcharCat(const source: pchar; dest: pchar);
begin
// todo: if length of pchar 0 then exit  
  if source = nil then exit;
  if dest = nil then exit;
  move(source^, dest[length(dest)], length(source) + 1);
end;


{ create a pchar copy of an ansistring (actual copy, not cast)
  returns false if memory not available or s is empty }
function NewPchar(const s: string; out rslt: pchar): boolean;
var
  L: integer;
begin
  rslt:= '';
  if s = '' then begin result:= false; exit; end;
  result:= false;
  L:= length(s);
  getmem(rslt, L + 1); // plus one for null char
  move(s[1], rslt[0], L);
  rslt[L]:= #0; // null is L since zero based
  result:= true;
end;

// dispose pchar
procedure RidPChar(rslt: pchar);
begin
  freemem(rslt);
end;


end.

