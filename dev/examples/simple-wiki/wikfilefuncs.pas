{ Copyright Lars Olson  2008-2011
  Some functions for wik }


unit wikfilefuncs;  {$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF} 

interface

function StringToFile(const fname, InputStr: string): boolean;
function FileToString(const fname: string): string;
procedure SpacesToDashes(var s: string);
procedure DashesToSpaces(var s: string);

implementation

uses
  strwrap1, pwfileshare, pwsubstr;

procedure SpacesToDashes(var s: string);
begin
  s:= SubStrReplace(s,' ', '-'); 
end;

procedure DashesToSpaces(var s: string);
begin
  s:= SubStrReplace(s,'-',' '); 
end;

{ save a string to a file using file sharing mechanism }
function StringToFile(const fname, InputStr: string): boolean;
begin
  FileMarkWrite(fname);
  result:= strwrap1.strsavefile(fname, inputstr);
  FileUnMarkWrite(fname);
end;

{ load a file into a string using file sharing mechanism }
function FileToString(const fname: string): string;
var 
  k: word; // file sharing unique key
begin
  FileMarkRead(fname, k);
  result:= strwrap1.strloadfile(fname);
  FileUnMarkRead(fname, k);
end;

end.