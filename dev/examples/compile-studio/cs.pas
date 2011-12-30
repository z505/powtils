(****************************************************************************** 
   Web Compiler Studio Example
 ******************************************************************************

   Allows one to create pascal source files online, and then compile them.

   Copyright Lars Olson 2008-2011 

*******************************************************************************)


program cs; {$IFDEF FPC}{$MODE OBJFPC}{$H+}{$ENDIF} {$APPTYPE CONSOLE}

uses
  pwinit, pwmain, htmout;       

begin

  WriteTopHeader;

  // must be a p URL var to continue
  if not IsCgiVar('p') then
    NoCsPage
  else 
    ShowCs;

  WriteFooter;
end.
