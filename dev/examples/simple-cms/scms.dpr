program scms; {$IFDEF FPC}{$mode objfpc}{$H+}{$IFDEF EXTRA_SECURE} {$R+}{$Q+}{$CHECKPOINTER ON} {$ENDIF}{$ENDIF} {$IFNDEF FPC} {$apptype console} {$endif}

uses
  pwinit,
  pwmain,
  htmout;       

begin

  WriteTopHeader;

  // must be a p var to continue
  if not IsPostVar('p') then
    NoCmsPage
  else 
    ShowCms;

  WriteFooter;
end.
