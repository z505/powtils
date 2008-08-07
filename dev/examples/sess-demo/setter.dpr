{ run setter, then try getter program after }
program setter; {$APPTYPE CONSOLE}
uses
  pwinitall, // Initialize addons such as sessions and config file
  pwsdssess, // Session functions
  pwtypes, pwmain;    

begin
  SetSess('USERID','MAX');
  OutLn('<a href="getter'+EXT+'">Click Here For Get Test</a>');
  OffReadLn; // hack to keep console open OFFLINE only
end.

