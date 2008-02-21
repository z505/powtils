program sess; {$ifdef FPC}{$mode objfpc}{$h+}{$endif} {$apptype console}
uses
 pwinitall, // initializes session and config file
 pwsdssess, // replace with Firebird, MySQL, if available
 pwmain;

procedure show;
begin
  setsess('foo', 'bar');
  outln('Testing session...');
  outln('Foo is: ' + getsess('foo'));  
end;

begin
  show;
  offreadln; // offline readln, only if webserver not running
end.
