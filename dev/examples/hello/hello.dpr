program hello; {$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF} {$APPTYPE CONSOLE}

uses
  pwinit,
  pwmain;

begin
  outln(' Hello! ');
  offreadln; // trick to keep console open OFFLINE only ;-)
end.

