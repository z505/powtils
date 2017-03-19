{ Static site generator for Powtils. Generates static html files, like a blog
  or wiki but super secure plain old static html pages generated, instead of
  a CGI program for serving content. CGI program is used to manage your
  content, static html files are served to the end user.

  Released under BSD or MIT license (take your pick)  }

program ss; {$IFDEF FPC}{$mode objfpc}{$H+}{$IFDEF EXTRA_SECURE} {$R+}{$Q+}{$CHECKPOINTER ON} {$ENDIF}{$ENDIF} {$IFNDEF FPC} {$apptype console} {$endif}

uses
  pwinit, // must be first unit in uses section in all 1.7.x cgi programs!
  pwmain,
  pwfileutil, // DELETE THIS FROM USES when done test
  htmout;

begin
  InitMacroVars;

  WriteTopHeader;

  // must be a p var to continue
  if not IsPostVar('p') then
    NoCmsPage
  else begin
    WriteTitle;
    ShowCms;
  end;

  WriteFooter;

end.

