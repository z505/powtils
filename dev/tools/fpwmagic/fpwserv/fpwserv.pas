(****************************************************************************** 

 ******************************************************************************

   Compiled against Powtils/PSP v1.6.x-devel.


   Author: Lars aka L505
           http://z505.com   
*******************************************************************************)


program fpwserv; {$IFDEF FPC}{$MODE OBJFPC}{$H+}{$ENDIF} {$APPTYPE CONSOLE}

uses
  pwmain,      // Powtils STATIC
  htmout;       

begin

//  WriteTopHeader;

  // must be a p URL var to continue
  if not IsCgiVar('param0') then NoPage else  Show;

//  WriteFooter;
end.
