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


program pastie; {$MODE OBJFPC}{$H+}

uses
  pwinit, pwmain, pwenvvar, htmout;       

begin
  // get all CGI special environment variables into global record var (we don't care that globals are evil, this is a simple CGI program)
  CGIEnvVars:= GetCGIEnvVars;
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
