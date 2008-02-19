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
  //writeln('INIT pwinitaddons');

  // initializes sessions, config, based on plugin units above, and also 
  // initializes whatever pwmain must initialize such as headers, buffer, etc
  Init;
end.


