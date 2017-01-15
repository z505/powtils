{H+}{$IFDEF EXTRA_SECURE} {$R+}{$Q+}{$CHECKPOINTER ON} {$ENDIF}

uses
  pwuMain,      // Pascal web unit (PWU) STATIC
  htmout;       

begin

  WriteTopHeader;

  // must be a p var to continue
  if not IsCgiVar('p') then
    NoCmsPage
  else 
    ShowCms;

  WriteFooter;
end.
