program cook;  {$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF} {$APPTYPE CONSOLE}

uses 
  pwinit,
  pwmain;

var s: string;

begin

  setcookie('foo', 'bar');
  s := getcookie('foo');
  out('What is the cookie value: ');
  out(s);
  
end.
