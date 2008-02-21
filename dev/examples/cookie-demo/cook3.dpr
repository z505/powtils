program cook2;  {$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF} {$APPTYPE CONSOLE}

uses
  pwinit,
  pwmain;

var s: string;

begin
  out('Does the cookie exist: ');
  s := getcookie('foo');
  if s = '' then 
    out('no')
  else 
  begin
    out('yes');
    out('<p>');
    out('Cookie value: ');
    out(s);
  end;
end.
