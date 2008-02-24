(****************************************************************************** 
   Code Pastie
 ******************************************************************************
  Compiled against Powtils/PSP v1.6.0.2
  Allows one to create pastebin like pages. 
   
  PROGRAM WAS CREATED IN A RUSH, YEARS AGO. NOT SUPER GOOD CODE.

  Compiles with FPC, have not tested on Delphi. 

  Author: Lars aka L505
          http://z505.com   
          See LICENSE.TXT  (NRCOL public domain)
*******************************************************************************)


program pastie; {$ifdef fpc}{$MODE OBJFPC}{$H+}{$endif} {$apptype console}

uses
  pwinit, pwmain, pwenvvar, htmout;       

begin

  WriteTopHeader;
  // must be a p var to continue
  if IsPostVar('p') then 
    ShowPastie 
  else
    if IsPostVar('viewcss') then ShowPastieCssHtm
  else 
    NoPastiePage;
  WriteFooter;
end.
