{$ifndef FPC}
  {$ifdef WIN32}{$define WINDOWS}{$endif}
  {$ifdef WIN64}{$define WINDOWS}{$endif}
  {$define SYSUTILS_ON} // can't use CompactSysUtils in delphi
{$endif}

{$ifdef FPC} 
  {$mode objfpc}{$H+}
{$endif}

{$ifdef EXTRA_SECURE}
  {$R+}
  {$IFDEF FPC}{$Q+}{$CHECKPOINTER ON}{$ENDIF}
{$endif}

// force debugln if PWUDEBUG is on
{$ifdef PWUDEBUG}
  {$define DBUG_ON}
{$ENDIF}


