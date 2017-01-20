unit pwinit;

interface
{ Put units like pwSdsSess and pwDefaultCfg in uses clause if you want that 
  functionality in your web program. By default pwinit uses no plugin units, 
  meaning config file, sessions, and other plugins are disabled. }

implementation
uses 
  pwmain;

initialization
  pwmain.Init;

finalization
  pwmain.Fini;

end.

{ Make different copies of this unit for different defaults. Put this file in 
  local directory of your program if you modify it, or rename different copies 
  to specific setups such as pwInitMySql or pwInitCfg if using those plugins }


{------------------------------------------------------------------------------
             ANOTHER EXAMPLE OF A CONFIGURATION IS BELOW:
 ------------------------------------------------------------------------------

unit pwInit;

interface
uses // sessions and config are plugged in
   pwSdsSess, pwDefaultCfg;

implementation
uses 
  pwmain;

initialization
  pwmain.Init;
finalization
  pwmain.Fini;
end.
}

{------------------------------------------------------------------------------
             ANOTHER EXAMPLE OF A CONFIGURATION IS BELOW:
 ------------------------------------------------------------------------------

unit pwInit;

interface
uses // just sessions, no config file (pwu_win.conf, pwu_unix.conf)
   pwSdsSess; 

implementation
uses 
  pwmain;

initialization
  pwmain.Init;
finalization
  pwmain.Fini;
end.
}


{------------------------------------------------------------------------------
             ANOTHER EXAMPLE OF A CONFIGURATION IS BELOW:
 ------------------------------------------------------------------------------


unit pwInit;

interface
uses // just config file, no sessions
   pwDefaultCfg; 

implementation
uses 
  pwmain;

initialization
  pwmain.Init;
finalization
  pwmain.Fini;
end.
}
