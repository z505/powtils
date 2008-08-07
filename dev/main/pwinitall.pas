{*******************************************************************************

                                POWTILS 

********************************************************************************

*******************************************************************************}


unit pwinitall; 

interface
uses // forces addon units to be initialized 
  pwdefaultcfg, pwsdssess ;

implementation
uses 
  pwmain;

initialization
  // initializes sessions, config, based on plugin units above, and also
  // whatever pwmain must initialize such as headers, buffer, etc
  Init;

finalization
  Fini;
end.


