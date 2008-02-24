{ First run the Setter program, then run this program. Note: Session programs
  are web programs... running from the command line will not produce the desired
  result (duh) since cookies/sessions require HTTP!! }
program getter; {$APPTYPE CONSOLE}

uses
  pwinit,
  pwmain,
  pwsdssess;


begin
  out('User id: ');
  outln(GetSess('USERID'));
  offreadln; // hack to keep console open, OFFLINE only
end.



