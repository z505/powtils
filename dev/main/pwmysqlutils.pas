unit MySqlUtils; {$ifdef fpc} {$mode objfpc} {$H+} {$endif}

interface
uses
  mysql4;

function MySQLEscape(s: string; var qmysql: TMYSQL): string;

procedure debugproc(s: string);

var // custom debugging
  debugln: procedure(s: string) = @debugproc;

implementation

procedure debugproc(s: string);
begin

end;


function MySQLEscape(s: string; var qmysql: TMYSQL): string;
var
  escaped: pchar;
  slen: longword;
  res: longword;
  newlen: longword;
begin
  result:= '';
  slen:= length(s);
  newlen:=(slen*2) + 1;
  getmem(escaped, newlen); // allocate worst case scenario
  res:= mysql_real_escape_string(@qmysql, escaped,  pchar(s), slen);
  if res > newlen then debugln('allocated pchar in mysqlEscape too small');
  result:= string(escaped); // makes copy of pchar
  freemem(escaped);
end;



end.