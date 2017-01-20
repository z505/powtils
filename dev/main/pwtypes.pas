unit pwtypes;

{$I defines1.inc}

interface

type
  // Below aliases reduce verbose line noise in long procedure declarations.
  // This goes against pascal's "verbosity" heritage, but extremely long 
  // declarations, (esp. ones with CONST and VAR in them) span far too wide
  // across screens to remain readable. "AStr" is also safer than abstract 
  // "string" type, since {$H+} or {$LONGSTRINGS ON} forgotten directives can 
  // cause confusing software bugs

  astr = ansistring;
  // fast strings, power of 2 (premature optimization ;-( )
  str15 = string[15]; // 16-1 (16 based)
  str31 = string[31]; // 32-1 (32 based)
  TStrArray = array of string; // obsolete -> AstrArray is better
  AstrArray = array of astr;   // deprecated, use "AstrRay"
  AstrRay = array of astr;
  str15ray = array of str15;
  str31ray = array of str31;          
  str255ray = array of shortstring;
  int32 = longint;   // less vague than "longint", keeps declarations shorter
  boo = boolean;     // keeps procedure declarations shorter and readable 
  TByteRay = array of byte;
  PByteRay = ^TByteRay;
  TCharRay = array of char;
  PCharRay = ^TCharRay;

 {----------------- DEPRECATED -----------------------------------------------}
  num  = longint;                       // use int32
  bln = boolean;                        // use boo
  str15array = array of str15;          // use str15ray 
  str31array = array of str31;          // use str31ray
  ShortstrArray = array of shortstring; // use str255ray
 {----------------------------------------------------------------------------}

const // platform specific directory slash (old Mac not supported)
  {$ifdef UNIX}SLASH = '/';{$endif}
  {$ifdef WINDOWS}SLASH = '\';{$endif}
  SLASHES = ['\', '/'];
  {$ifdef fpc}LF = LineEnding;{$endif}
  {$ifndef fpc}LF = {$ifdef windows}#13#10{$else}#10{$endif};{$endif}
  
const
  // CGI uses #13#10 no matter what OS
  CGI_CRLF = #13#10; 
  // default program extension
  EXT={$IFDEF WINDOWS}'.exe'{$ENDIF} {$IFDEF UNIX}''{$ENDIF};

{$IFNDEF FPC} // delphi compatibility
  const
    DirectorySeparator = SLASH;  // TODO:  delphi VERSION check & KYLIX compat
    PathDelim = DirectorySeparator;
    DriveDelim = ':';          // ...
    PathSep = ';';             // .. 

  type
    UInt64 = Int64;  

    {$EXTERNALSYM INT_PTR}
    {$EXTERNALSYM UINT_PTR}
    {$EXTERNALSYM LONG_PTR}
    {$EXTERNALSYM ULONG_PTR}
    {$EXTERNALSYM DWORD_PTR}
    INT_PTR = Longint;
    UINT_PTR = LongWord;
    LONG_PTR = Longint;
    ULONG_PTR = LongWord;
    DWORD_PTR = LongWord;
    PINT_PTR = ^INT_PTR;
    PUINT_PTR = ^UINT_PTR;
    PLONG_PTR = ^LONG_PTR;
    PULONG_PTR = ^ULONG_PTR;

    PtrInt = Longint;
    PtrUInt = Longword;
    PPtrInt = ^PtrInt;
    PPtrUInt = ^PtrUInt;

    {$EXTERNALSYM SIZE_T}
    {$EXTERNALSYM SSIZE_T}
    SIZE_T = ULONG_PTR;
    SSIZE_T = LONG_PTR;
    PSIZE_T = ^SIZE_T;
    PSSIZE_T = ^SSIZE_T;

    SizeInt = SSIZE_T;
    SizeUInt = SIZE_T;
    PSizeInt = PSSIZE_T;
    PSizeUInt = PSIZE_T;
{$ENDIF}

{$IFDEF FPC}
const
  PathDelim={System.}DirectorySeparator;
  DriveDelim={System.}DriveSeparator;
  PathSep={System.}PathSeparator;
  MAX_PATH={System.}MaxPathLen;
{$ENDIF}


implementation

end.

