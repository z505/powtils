{ A widestring with capacity, grows in chunks. Much more efficient than
  concatenating widestrings using s + s + s style, and much easier to use
  than growing the string in chunks yourself wiht manual setlengths. This takes
  care of it for you. It can also be known as a buffered widestring.

  Could be placed into a class, I couldn't be bothered as I don't like free
  and create bloating up code, and classes can't be used on the stack. 

  Released under NRCOL public domain license.


  Lars (L505)
  http://z505.com
}


unit capwstr; {$IFDEF fpc} {$MODE objfpc} {$H+} {$ENDIF}

interface

type
  PCapWstr = ^TCapWstr;
  TCapWstr = record
    strlen: integer; // string length stored inside larger buffer area
    growby: integer; // grow the widestring in chunks
    data: widestring; // widestring data
  end;

procedure resetbuf(buf: PCapWstr); overload;
procedure resetbuf(buf: PCapWstr; growby: integer); overload;
procedure addchar(wc: widechar; buf: PCapWstr);
procedure addstring(ws: widestring; buf: PCapWstr);
procedure endupdate(buf: PCapWstr);


implementation


{ call this to reset the data structure to zero and the default growby value }
procedure resetbuf(buf: PCapWstr);
begin
  buf^.data:= '';
  buf^.strlen:= 0;
  buf^.growby:= 12800;
end;

{ same as above but with custom growby }
procedure resetbuf(buf: PCapWstr; growby: integer);
begin
  buf^.data:= '';
  buf^.strlen:= 0;
  if buf^.growby < 1 then
    buf^.growby:= 128
  else
    buf^.growby:= growby;
end;


{ private function, grows the capstring in chunks automatically only when 
  absolutely needs to }
procedure grow(buf: PCapWstr; addlen: integer);
var
  growmult: integer;
begin
  if addlen <= buf^.growby then // only grow one chunk if there is room for data incoming
    setlength(buf^.data, length(buf^.data) + buf^.growby)
  else
  begin // must grow in more than one chunk since data is too large to fit
    growmult:= (addlen div buf^.growby) + 1;   // div discards the remainder so we must add 1 to make room for that extra remainder
    setlength(buf^.data, length(buf^.data) + growmult);
  end;
end;

{ add single character to capacitance widestring }
procedure addchar(wc: widechar; buf: PCapWstr);
var
  newlen: integer;
const
  wclen = 1;
begin
  newlen:= buf^.strlen + wclen;
  if newlen > length(buf^.data) then grow(buf, wclen);
  buf^.data[newlen]:= wc; // concat
  buf^.strlen:= newlen; // string length stored, but not actual buffer length
end;

{ add existing widestring to capacitance widestring }
procedure addstring(ws: widestring; buf: PCapWstr);
var
  newlen: integer;
  wslen: integer;
  i: integer;
  oldlen: integer;
begin
  wslen:= length(ws);
  oldlen:= length(buf^.data);
  newlen:= buf^.strlen + wslen;
  if newlen > oldlen then grow(buf, wslen);
  //  debugln('debug: wslen: ' + inttostr(wslen) );

  //  move(ws[1], buf^.data[buf^.strlen + 1], wslen); // can't do this because widechars are multiple bytes
  for i:= 1 to wslen do
    buf^.data[buf^.strlen + i]:= ws[i];

  buf^.strlen:= newlen; // string length stored, but not actual buffer length
end;

{ endupdate MUST be called after doing your work with the string, this
  sets the string to the correct length (trims extra buffer spacing at end) }
procedure endupdate(buf: PCapWstr);
begin
  setlength(buf^.data, buf^.strlen);
end;

end.
