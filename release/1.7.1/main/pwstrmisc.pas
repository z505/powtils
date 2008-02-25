{ Miscellaneous string utilities

  Author:  Lars (L505)          License: NRCOL Public Domain
           http://z505.com

}

unit strmisc;

interface

function GetMidStr(s1, s2, fromstring: string): string;

implementation
uses
 strutils;

// extract substring between two string patterns (no wildcards)
function GetMidStr(s1, s2, fromstring: string): string;
var
  pos1, pos2: integer;
begin
  pos1:= pos(s1, fromstring);
  pos2:= pos(s2, fromstring);
  result:= AnsiMidStr(fromstring, pos1 + length(s1), pos2-(pos1 + length(s1)));
end;

end.
