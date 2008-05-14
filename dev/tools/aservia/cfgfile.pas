{ Modified March 2008 by Lars Olson. Aservia (web server).
  Based on nYume Server }
unit cfgfile; {$MODE OBJFPC} {$SMARTLINK ON} {$H+}
interface

type
  Trec = record
    name : string;
    value: string;
  end;
    
  TArr = array of string;

  TCfgFile = class(TObject)
    private
      fname: string; fmodified: boolean; flen: integer;
      fcontent: array of Trec; 
      procedure savefile;
      function search(name: string): integer;
      function Bool2Str(value: boolean): string;
      function s2i(value: string): integer;
      function Str2Bool(value: string): boolean;
    public
      constructor Create(name: string);
      destructor  Destroy; override;
      procedure setOpt(name: string; value: string);
      procedure setOpt(name: string; value: integer);
      procedure setOpt(name: string; value: boolean);
      function getOpt(name: string; def: string = ''): string;
      function getOpt(name: string; def: integer = 0): integer;
      function getOpt(name: string; def: boolean = false): boolean;
      function Exists(name: string): boolean;
      function getAllOpts: Tarr;
  end;
    
implementation
uses pwstrutil;

constructor TCfgFile.Create(name: string = '');
var fl : text;
    tmp: string;
    p  : integer;
begin
  inherited Create;
  fname:= name;
  fmodified:= false;
  SetLength(fcontent, 0);
  flen:= 0;    
  try
    Assign(fl, fname);
    Reset(fl);
    while not eof(fl) do
    begin
      readln(fl, tmp);
      inc(flen);
      SetLength(fcontent, flen);
      p := pos('=', tmp);
      if p = 0 then begin
        fcontent[flen - 1].name:= tmp;
        fcontent[flen - 1].value:= '';
      end else begin
        fcontent[flen - 1].name:= copy(tmp, 1, p - 1);
        fcontent[flen - 1].value:= copy(tmp, p + 1, length(tmp));
      end;
    end;
    Close(fl);
  except
  end;
end;
    
destructor TCfgFile.Destroy;
begin
  if (fname <> '') and fmodified then savefile;
  inherited Destroy;
end;

procedure TCfgFile.setOpt(name: string; value: string);
var p: integer;
begin
  fmodified:= true;
  p:= search(name);
  if p >= 0 then begin
    fcontent[p].name:= Lcase(name);
    fcontent[p].value:= value;
  end else begin
    inc(flen); SetLength(fcontent, flen);
    fcontent[flen - 1].name:= Lcase(name);
    fcontent[flen - 1].value:= value;
  end;
end;
  

procedure TCfgFile.setOpt(name: string; value: integer);
var p: integer;
begin
  fmodified:= true;
  p:= search(name);
  if p >= 0 then begin
    fcontent[p].name:= Lcase(name);
    fcontent[p].value:= i2s(value);
  end else begin
    inc(flen); SetLength(fcontent, flen);
    fcontent[flen - 1].name:= Lcase(name);
    fcontent[flen - 1].value:= i2s(value);
  end;
end;
  
procedure TCfgFile.setOpt(name: string; value: boolean);
var p: integer;
begin
  fmodified := true;
  p := search(name);
  if p >= 0 then begin
    fcontent[p].name:= Lcase(name);
    fcontent[p].value:= Bool2Str(value);
  end else begin
    inc(flen); SetLength(fcontent, flen);
    fcontent[flen - 1].name:= Lcase(name);
    fcontent[flen - 1].value:= Bool2Str(value);
  end;
end;

{ value of name=value pair. If not found, return specified default param }  
function TCfgFile.getOpt(name: string; def: string = ''): string;
var p: integer;
begin
  p:= search(name);
  if p >= 0 then Result:= fcontent[p].value else Result:= def;
end;

{ overloaded }
function TCfgFile.getOpt(name: string; def: integer = 0): integer;
var p: integer;
begin
  p:= search(name);
  if p >= 0 then Result:= s2i(fcontent[p].value) else Result:= def;
end;

{ overloaded }
function TCfgFile.getOpt(name: string; def: boolean = false): boolean;
var p: integer;
begin
  p:= search(name);
  if p >= 0 then Result:= Str2Bool(fcontent[p].value) else Result:= def;
end;

  
function TCfgFile.Bool2Str(value: boolean): string;
begin
  if value then Result:= 'true' else Result:= 'false';
end;

function TCfgFile.s2i(value: string): integer;
var o: integer;
begin
  Result:= 0; val(value, Result, o);
end;

function TCfgFile.Str2Bool(value: string): boolean;
begin
  if value = 'true' then Result:= true else Result:= false;
end;
  
{ saves config }  
procedure TCfgFile.savefile;
var fl: text;
    i: integer;    
begin
  Assign(fl, fname);
  Rewrite(fl);
  if flen > 0 then for i := 0 to flen - 1 do
  begin
    writeln(fl, fcontent[i].name, '=', fcontent[i].value);
  end;
  Close(fl);
end;
  
function TCfgFile.Exists(name: string): boolean;
begin
  if search(name) >= 0 then Result:= true else Result:= false;
end;
  
function TCfgFile.getAllOpts: Tarr; 
var i: integer;
begin
  if flen > 0 then begin
    SetLength(Result, flen);
    for i := 0 to flen - 1 do Result[i]:= fcontent[i].name;
  end;
end;

function TCfgFile.search(name: string): integer;
var i: integer;
begin
  Result:= -1; i:= 0;
  while i < flen do begin
    if fcontent[i].name = Lcase(name) then begin Result:= i; i:= flen; end;
    inc(i);
  end;
end;


end.
