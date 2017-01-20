program scms;
{$IFDEF FPC}
  {$mode objfpc}{$H+}{$IFDEF EXTRA_SECURE} {$R+}{$Q+}{$CHECKPOINTER ON} {$ENDIF}
{$ENDIF}

uses
  pwinit,
  pwmain,
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
