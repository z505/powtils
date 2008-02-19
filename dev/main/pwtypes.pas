unit pwtypes;

{$IFDEF FPC}{$MODE OBJFPC}{$H+}{$ENDIF}
{$IFDEF WIN32}{$DEFINE WINDOWS}{$ENDIF}
interface

type
  // obsolete
  TStrArray = array of string;

  // Below aliases reduce verbose line noise in long procedure declarations.
  // This goes against pascal's verbosity heritage, but extremely long 
  // declarations, (esp. ones with CONST and VAR in them) span far too wide
  // to remain redable. AStr is also safer than abstract "string" type with 
  // regards to {$H+} not being on due to a bug or programmer mistake
  astr = ansistring;
  AstrArray = array of astr;
  bln = boolean;

const // platform specific directory slash (Mac not supported yet)
  {$IFDEF UNIX}SLASH = '/';{$ENDIF}
  {$IFDEF WINDOWS}SLASH = '\';{$ENDIF}
  SLASHES = ['\', '/'];

const 
  CGI_CRLF = #13#10; // CGI uses #13#10 no matter what OS

implementation

end.

