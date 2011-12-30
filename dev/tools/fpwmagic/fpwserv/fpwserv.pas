(****************************************************************************** 

 ******************************************************************************

  Copyright Lars Olson  2008-2011

*******************************************************************************)


program fpwserv; {$IFDEF FPC}{$MODE OBJFPC}{$H+}{$ENDIF} {$APPTYPE CONSOLE}

uses
  pwinit, pwmain, htmout;       

begin

//  WriteTopHeader;

  // must be a p URL var to continue
  if not IsCgiVar('param0') then NoPage else  Show;

//  WriteFooter;
end.
