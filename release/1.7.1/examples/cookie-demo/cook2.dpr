program cook2; {$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF} {$APPTYPE CONSOLE}

uses 
  pwinit,
  pwmain;

var s: string;

begin
  unsetcookie('foo');
  out('Does the cookie still exist: ');
  s := getcookie('foo');
  if s = '' then out('no') else out('yes');
end.
