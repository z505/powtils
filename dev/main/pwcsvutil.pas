{ Basic CSV functions for adding cells and new rows to a new CSV string
  which can then be save to a file 

  Note: relies on strwrap1, it is available in freepascal contributed units section 

  Excel seems to handle ,"", empty strings and "strings" fine, and it is safer to
  enclose everything in quotes instead of figuring out which fields need quotes and
  which don't - not worth the time or the bytes saved.

  Author,
  Lars (L505)
  http://z505.com  }

unit pwcsvutil; {$ifdef fpc}{$mode objfpc} {$H+}{$endif} {$IFDEF WIN32}{$DEFINE WINDOWS}{$ENDIF}

interface
uses 
  pwtypes, capstr;

type str4 = string[4];

function CsvAddCell(s: astr; var csvstr: astr): boolean; overload;
function CsvEnd(var csvstr: astr): boolean; overload;
function CsvStartNewRow(var csvstr: astr): boolean; overload;

function CsvAddCell(s: astr; var csvstr: astr; const delim, encloser: str4): boolean; overload;
function CsvStartNewRow(var csvstr: astr; const delim: str4): boolean; overload;
function CsvEnd(var csvstr: astr; const delim: str4): boolean; overload;

function CsvAddCell(s: astr; var csvstr: TCapStr; const delim, encloser: str4): boolean; overload;
function CsvEnd(var csvstr: TCapStr; const delim: str4): boolean; overload;
function CsvStartNewRow(var csvstr: TCapStr; const delim: str4): boolean; overload;

function CsvAddCell(s: astr; var csvstr: Tcapstr): boolean; overload;
function CsvEnd(var csvstr: tcapstr): boolean; overload;
function CsvStartNewRow(var csvstr: tcapstr): boolean; overload;

type
  TCsv = record
    buf: ansistring;
    encloser: string[5];
    delim: string[5];
  end;

function CsvAddCell(s: astr; var csv: TCsv): boolean; overload;
function CsvEnd(var csv: TCsv): boolean; overload;
function CsvStartNewRow(var csv: TCsv): boolean; overload;


procedure debugproc(s: astr);
var debugln: procedure (s: astr) = {$ifdef fpc}@{$endif}debugproc;



implementation

uses
  strutils,
  sysutils,
  strwrap1;


var
  CRLF: astr = #13#10; // not sure what the CSV format is on unix or MAC but for 
                         // now just implementing the MS Windows compatible CRLF

procedure debugproc(s: astr);
begin
end;

function CsvAddCell(s: astr; var csv: TCsv): boolean; overload;
begin
  result:= CsvAddCell(s, csv.buf, csv.encloser, csv.delim);
end;

function CsvEnd(var csv: TCsv): boolean; overload;
begin
  result:= CsvEnd(csv.buf, csv.delim);
end;

function CsvStartNewRow(var csv: TCsv): boolean; overload;
begin
  result:= CsvStartNewRow(csv.buf, csv.delim);
end;


{  s is the string to add, csv string is the string to add too
   S consists of an unquoted (unenclosed) string, if quotes exist in S they will
   be replaced with doubled quote (encloser). }
function CsvAddCell(s: astr; var csvstr: astr; const delim, encloser: str4): boolean;
begin
   // if '' then add "",
  result:= false;
  if s = '' then
  begin
    s:= encloser + encloser + delim; // "",
    csvstr:= csvstr + s;
    exit;
  end;
  // excel may do weird things with HTML, replace quotes first
  s:= stringreplace(s, '&quot;', '"', [rfReplaceAll]);
  // replaces " with ""
  s:= stringreplace(s, encloser, encloser + encloser, [rfReplaceAll]);

  // enclose string in Quotes (enclosers)
  s:= encloser + s + encloser;
  //Replace CrLf, i.e. Excel prefers #10
  s:= stringreplace(s, #13#10, #10, [rfReplaceAll]);
  csvstr:= csvstr + s + ',';  //"test",
end;

{ UNTESTED }
function CsvAddCell(s: astr; var f: text; const delim, encloser: str4): boolean; overload;
begin
   // if '' then add "",
  result:= false;
  if s = '' then
  begin
    s:= encloser + encloser + delim; // "",
    append(f);
    write(f, s);
    exit;
  end;
  // excel may do weird things with HTML, replace quotes first
  s:= stringreplace(s, '&quot;', '"', [rfReplaceAll]);
  // replaces " with ""
  s:= stringreplace(s, encloser, encloser + encloser, [rfReplaceAll]);

  // enclose string in Quotes (enclosers)
  s:= encloser + s + encloser;
  //Replace CrLf, i.e. Excel prefers #10
  s:= stringreplace(s, #13#10, #10, [rfReplaceAll]);
  append(f);
  write(f, s);
  write(f, ',');
end;


function CsvAddCell(s: astr; var csvstr: TCapStr; const delim, encloser: str4): boolean;
begin
   // if '' then add "",
  result:= false;
  if s = '' then
  begin
    s:= encloser + encloser + delim; // "",
    // csvstr:= csvstr + s;
    capstr.addstr(s, @csvstr);
    exit;
  end;
  // excel may do weird things with HTML, replace quotes first
  s:= stringreplace(s, '&quot;', '"', [rfReplaceAll]);
  // replaces " with ""
  s:= stringreplace(s, encloser, encloser + encloser, [rfReplaceAll]);

  // enclose string in Quotes (enclosers)
  s:= encloser + s + encloser;
  //Replace CrLf, i.e. Excel prefers #10
  s:= stringreplace(s, #13#10, #10, [rfReplaceAll]);
//  csvstr:= csvstr + s + ',';  //"test",
  capstr.addstr(s , @csvstr);
  capstr.addstr(',', @csvstr);
end;


{ adds new row to existing csv string returns false if problem }
function CsvStartNewRow(var csvstr: astr; const delim: str4): boolean;
var
  endpos: integer;
begin
  result:= false;
  if delim = '' then exit;
  endpos:= (length(csvstr) - length(delim)) + 1;
  // decide whether to replace trailing comma with carriage return
  if AnsiEndsStr(delim, csvstr) then
    system.delete(csvstr, endpos, length(delim));
  csvstr:= csvstr + CRLF;  // add carriage return to end, starting new row
  result:= true;
end;

function CsvStartNewRow(var f: text; const delim: str4): boolean; overload;
begin
//TODO
end;

{ old slower way
function CsvStartNewRow(var csvstr: TCapStr; delim: astr): boolean;
var
  endpos: integer;
begin
  result:= false;
  if delim = '' then exit;
  endpos:= (csvstr.strlen - length(delim)) + 1;
  // decide whether to replace trailing comma with carriage return
  capstr.endupdate(@csvstr); // must make this call before using string data in functions
  if AnsiEndsStr(delim, csvstr.data) then
    capstr.delete(@csvstr, endpos, length(delim));
//  csvstr:= csvstr + CRLF;  // add carriage return to end, starting new row
  capstr.addstr(CRLF, @csvstr);
  result:= true;
end;
}

function CsvStartNewRow(var csvstr: TCapStr; const delim: str4): boolean;
//var
//  endpos: integer;
begin
  result:= false;
  if delim = '' then exit;
  //endpos:= (csvstr.strlen - length(delim)) + 1;
  // decide whether to replace trailing comma with carriage return
  if EndsStr(delim, @csvstr) then
  begin
    csvstr.data[csvstr.strlen]:= CRLF[1]; // replace comma with #13
    capstr.addstr(CRLF[2], @csvstr); // add #10
  end;
  result:= true;

end;

{ this must be called after adding is complete, deletes trailing comma (delim) }
function CsvEnd(var csvstr: astr; const delim: str4): boolean;
var
  b: boolean;
  endpos: integer;
begin
  result:= false;
  if delim = '' then exit;
  if csvstr = '' then exit;
  // find location of potential trailing delimiter
  endpos:= (length(csvstr) - length(delim)) + 1;
  // delete trailing comma (delimiter)
  b:= AnsiEndsStr(delim, csvstr);
  if b = true then // delete trailing delim
    system.delete(csvstr, endpos, length(delim));
  result:= true;
end;

function CsvEnd(var f: text; const delim: str4): boolean; overload;
begin
//TODO
end;

function CsvEnd(var csvstr: TCapStr; const delim: str4): boolean;
var
  b: boolean;
  endpos: integer;
begin
  result:= false;
  if delim = '' then exit;
  if csvstr.data = '' then exit;
  // find location of potential trailing delimiter
  endpos:= (csvstr.strlen - length(delim)) + 1;
  // delete trailing comma (delimiter) 
  capstr.endupdate(@csvstr); // must make this call before using string data in functions
  b:= AnsiEndsStr(delim, csvstr.data);
  if b = true then // delete trailing delim
    capstr.delete(@csvstr, endpos, length(delim));
  capstr.endupdate(@csvstr);
  result:= true;
end;


// overloaded, with default encloser and delimiter
function CsvAddCell(s: astr; var csvstr: astr): boolean; 
begin
  result:= CsvAddCell(s, csvstr, ',', '"');
end;

// overloaded, with default delimiter
function CsvEnd(var csvstr: astr): boolean;
begin
  result:= CsvEnd(csvstr, ',');
end;

// overloaded, with default delimiter
function CsvStartNewRow(var csvstr: astr): boolean;
begin
  result:= CsvStartNewRow(csvstr, ',');
end;

// overloaded, with default encloser and delimiter
function CsvAddCell(s: astr; var csvstr: Tcapstr): boolean; 
begin
  result:= CsvAddCell(s, csvstr, ',', '"');
end;

// overloaded, with default delimiter
function CsvEnd(var csvstr: tcapstr): boolean;
begin
  result:= CsvEnd(csvstr, ',');
end;

// overloaded, with default delimiter
function CsvStartNewRow(var csvstr: tcapstr): boolean;
begin
  result:= CsvStartNewRow(csvstr, ',');
end;

end.
