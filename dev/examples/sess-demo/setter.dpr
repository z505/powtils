{ run setter, then try getter program after }
program setter; {$APPTYPE CONSOLE}
uses
  pwinitall, // Initialize addons such as sessions and config file
  pwsdssess, // Session functions
  pwtypes, pwmain;    

begin
  setsess('USERID','MAX');
  outln('<a href="getter'+EXT+'">Click Here For Get Test</a>');
  offreadln; // hack to keep console open OFFLINE only
end.

