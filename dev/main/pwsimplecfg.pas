{ Simple config file parser spitting name/values into an stack based array 

  License: ~NRCOL (public domain)

  Authors: Lars (L505) 
           http://z505.com }

unit pwsimplecfg; {$IFDEF FPC}{$mode objfpc} {$H+}{$ENDIF} {$R+}

interface
uses 
  pwsubstr, pwfileutil, strwrap1, pwerrors, pwtypes;

const 
  MIN_CFG = 1;   // starting array element
  MAX_CFG = 100; // maximum config nameval pairs

type TNameVal = record
       name, value: astr;
     end;  

     TNameValArray = array [MIN_CFG..MAX_CFG] of TNameVal;

     TNameVals = record
       items: TNameValArray;
       len: byte;
     end;

procedure ShowConfig(const nv: TNameVals; const delim: shortstring);
procedure ShowConfig(const nv: TNameVals);
function ParseConfig(const fname: astr; var res: TNameVals): errcode;

procedure RunTest1(fname: string);


implementation

{ displays configuration data with writeln like name=val}
procedure ShowConfig(const nv: TNameVals; const delim: shortstring);
var i: byte;
begin
  if nv.len < 1 then exit;
  for i:= MIN_CFG to nv.len do
    writeln(nv.items[i].name, delim, nv.items[i].value);
end;

{ default delimiter }
procedure ShowConfig(const nv: TNameVals);
begin
  ShowConfig(nv, '=');
end;
     
{ Parse config file containing configuration name=value data, returns as
  a record array.

  Returns errors:
    OK if function was successful
    CFG_PARSE_ERR if parsing went wrong
    FILE_READ_ERR if cfg file not found
}
function ParseConfig(const fname: astr; var res: TNameVals): errcode;

  procedure add(name,value: ansistring);
  begin
    res.len:= res.len + 1;
    res.items[res.len].name:= name;
    res.items[res.len].value:= value;
  end;

var
 buff, name, value: string;
 fh: text;
 i: integer;
begin
  result:= FILE_READ_ERR;
  // exit if cfg file not found
  if fname = '' then exit;
  if not OpenFile(fh, fname, 'r') then exit;
  // Parse
  while not eof(fh) do
  begin
    readln(fh, buff);
    // skip empty lines and #comments
    if (buff = '') or (buff[1] = '#') then continue;
    i:= substrpos(buff, '=');
    // line must have equal sign if not a comment
    if i < 1 then begin
      close(fh);
      result:= CFG_PARSE_ERR;
      exit;
    end;
    name:= copy(buff, 1, i - 1);
    value:= copy(buff, i + 1, length(buff) - i);
    name:= strtrim(name);
    value:= substrstrip(strtrim(value), '"');
    value:= substrstrip(strtrim(value), '''');
    if (name = '') or (value = '') then continue;
    Add(name, value);
  end;
  close(fh);  
  result:= OK;
end;


procedure RunTest1(fname: astr);
var nv: TNameVals;
    err: errcode;
begin
  nv.len:= 0;
  err:= ParseConfig(fname, nv);
  case err of 
    FILE_READ_ERR: writeln('error: while reading config file');
    CFG_PARSE_ERR: writeln('error: while parsing config file'); 
    OK: writeln('note: config read okay');
  end;
  writeln('NUMBER OF NAME/VALUE PAIRS: ', nv.len);
  ShowConfig(nv);
  readln;
end;

end.

