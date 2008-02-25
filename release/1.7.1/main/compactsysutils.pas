{
--------------------------------------------------------------------------------

 This is part of the CompactUtils project

 Instead of using this unit.. please use pwstrutil and pwfileutil wherever
 possible (and friends).

 Functions from freepascal sources, so license is RTL modified GPL.

 The goal is to pull everything from the FPC sources which can exist on it's
 own as a stand alone function with no need for initilization/finalization.
 
 This is not "compact" source code, it is a large unit. It is called "compact"
 because binary footprint of your programs that use this unit are tiny.

 Sysutils adds about 60K-90K to your program even if you use one function,
 while CompactSysUtils adds 0K and the function you use. It's smartlinking.

 This is not mainly for executable size savings like the size flamewars imply,
 but rather the point is to modularize the huge sysutils unit into maintainable
 modules - while reaping the benefit of size savings at no cost. We are not
 using Standard Pascal any more, and some people need to learn to stop creating
 unmaintainable large bloated include file *effed up* units that look
 like utter crap (fpc sources). This unit itself is huge, *only* since it is
 here to be a wrapper and is being modularized around the sub modules like
 pwstrutil, pwfileutil. It also is large ONLY for borland compatibility with
 sysutils (a sysutils emulator).

 See the CompactUtils units too for interesting KOL algorithms ported to
 Linux/BSD/Etc for web and console programming.

 If you think this unit is effed up, which it is, look into the fpc sources
 and you will see sysutils is effed up even more. i.e. what sucks doesn't suck,
 if it sucks less than something else that sucks even more ;-)

--------------------------------------------------------------------------------

 WARNING:

  BEFORE MAKING CHANGES TO THIS UNIT, NOTE WHERE YOU GOT THE FUNCTION FROM IN
  THE FPC SOURCES. FOR EXAMPLE USE THE FOLLOWING NOTATION TO ENCLOSE CHANGES:


 Win32 specific:
  -- copy pasted from somefile.pp  /win32 -------------------------------------
     CONTENT HERE
  -----------------------------------------------------------------------------

 Unix specific:
  -- copy pasted from somefile.pp  /unix -------------------------------------
     CONTENT HERE
  -----------------------------------------------------------------------------

 Not unix specific or win32 specific:
  -- copy pasted from somefile.pp  --------------------------------------------
     CONTENT HERE
  -----------------------------------------------------------------------------

  This way we can reorganize the file and find the original place that the
  source came from. 


 TODO:
  -64 bit support and/or other platform support is welcome. SVN access available
   on request! This unit has been made to work with i386/ARM so far, ,however
   general algorithms like in pwstrutil and pwfileutil may work on all platforms.

 THANKS:
  -thanks to Bernd M. for ARM patches
  -thanks to Trustmaster for thinking modularizing/unbloating the sysutils
   was not a bad idea
  -thanks to Vladimir Kladov for teaching KOL and smartlinking through KOL.PAS
  -thanks to all embedded programmers who helped improve this unit

 NO THANKS:
  -no thanks goes out to those who have always hated my non bloatware and 
   minimalism efforts 
  -RTFH: read the effing header (of this file)

 Regards,
   Lars (L505)
   http://z505.com
   And all contributors!
}


unit CompactSysUtils; {$ifdef FPC}{$mode objfpc}{$H+}{$endif}

interface
{$IFDEF WIN32}{$DEFINE WINDOWS}{$ENDIF}

uses {$IFDEF WINDOWS}windows,{$ENDIF}
  pwtypes, pwfileutil, pwstrutil;


{-- copy pasted from SYSUTILH.INC ---------------------------------------------}

type

   TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);
  
   { some helpful data types }

   THandle = System.THandle;

   TProcedure = procedure;

   TFilename = String;

   TIntegerSet = Set of 0..SizeOf(Integer)*8-1;

   LongRec = packed record
      case Integer of
        0 : (Lo,Hi : Word);
        1 : (Bytes : Array[0..3] of Byte);
   end;

   WordRec = packed record
     Lo,Hi : Byte;                          
   end;

   Int64Rec = packed record
      case integer of
        0 : (Lo,Hi : Cardinal);
        1 : (Words : Array[0..3] of Word);
        2 : (Bytes : Array[0..7] of Byte);
   end;

   PByteArray = ^TByteArray;
   TByteArray = Array[0..32767] of Byte;

   PWordarray = ^TWordArray;
   TWordArray = array[0..16383] of Word;


type
   TCreateGUIDFunc = Function(Out GUID : TGUID) : Integer;
var
   OnCreateGUID : TCreateGUIDFunc = Nil;
                   
type
  TTerminateProc = Function: Boolean;

Var
   OnShowException : Procedure (Msg : ShortString);

const
   HexDisplayPrefix : string = '$';

const
  PathDelim= DirectorySeparator;
  DriveDelim= DriveSeparator;
  PathSep= PathSeparator;
  MAX_PATH= MaxPathLen;


{------------------------------------------------------------------------------}



{$ifdef unix}
// TODO FIX FUNCTION IMPLEMENTATION FOR BELOW FUNCTIONS IN INCLUDE FILE,
// commented out declartions temporarily.
//  {$i compactsysutils_functions_interface1}
{$endif}

{$ifdef win32}
  {$i compactsysutils_functions_interface1}
{$endif}


{-- copy pasted from SYSINTH.INC ----------------------------------------------}

{ ---------------------------------------------------------------------
    Upper/lowercase translations
  ---------------------------------------------------------------------}

type
   TCaseTranslationTable = array[0..255] of char;

var
   { Tables with upper and lowercase forms of character sets.
     MUST be initialized with the correct code-pages }
   UpperCaseTable: TCaseTranslationTable;
   LowerCaseTable: TCaseTranslationTable;

{ ---------------------------------------------------------------------
    Date formatting settings
  ---------------------------------------------------------------------}

Const

   { Character to be put between date, month and year }
   DateSeparator: char = '-';

   { Format used for short date notation }
   ShortDateFormat: string = 'd/m/y';

   { Format used for long date notation }
   LongDateFormat: string = 'dd" "mmmm" "yyyy';


   { Short names of months. }
   ShortMonthNames: array[1..12] of string[128] =
     ('Jan','Feb','Mar','Apr','May','Jun',
      'Jul','Aug','Sep','Oct','Nov','Dec');

   { Long names of months. }
   LongMonthNames: array[1..12] of string[128] =
     ('January','February','March','April','May','June',
      'July','August','September','October','November','December');

   { Short names of days }
   ShortDayNames: array[1..7] of string[128] =
     ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');

   { Full names of days }
   LongDayNames: array[1..7] of string[128] =
     ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');

   { Format used for short time notation }
   ShortTimeFormat: string[128] = 'hh:nn';

   { Format used for long time notation }
   LongTimeFormat: string[128] = 'hh:nn:ss';

   { Character to be put between hours and minutes }
   TimeSeparator: char = ':';

   { String to indicate AM time when using 12 hour clock. }
   TimeAMString: string[7] = 'AM';

   { String to indicate PM time when using 12 hour clock. }
   TimePMString: string[7] = 'PM';



{ ---------------------------------------------------------------------
    Number formatting constants
  ---------------------------------------------------------------------}


  { Character that comes between integer and fractional part of a number }
  DecimalSeparator : Char = '.';

  { Character that is put every 3 numbers in a currency }
  ThousandSeparator : Char = ',';

  { Number of decimals to use when formatting a currency.  }
  CurrencyDecimals : Byte = 2;

  { Format to use when formatting currency :
    0 = $1
    1 = 1$
    2 = $ 1
    3 = 1 $
    4 = Currency string replaces decimal indicator. e.g. 1$50
   }
  CurrencyFormat : Byte = 1;

  { Same as above, only for negative currencies:
    0 = ($1)
    1 = -$1
    2 = $-1
    3 = $1-
    4 = (1$)
    5 = -1$
    6 = 1-$
    7 = 1$-
    8 = -1 $
    9 = -$ 1
    10 = $ 1-
   }
  NegCurrFormat : Byte = 5;

  { Currency notation. Default is $ for dollars. }
  CurrencyString : String[7] = '$';

type
  TSysLocale = record
    case byte of
      { win32 names }
      1 : (FarEast: boolean; MiddleEast: Boolean);
      { real meaning }
      2 : (MBCS : boolean; RightToLeft: Boolean);
  end;

var
  SysLocale : TSysLocale;


{------------------------------------------------------------------------------}

const
  DefaultTextLineBreakStyle: TTextLineBreakStyle =
    {$ifdef unix} tlbsLF {$else} {$ifdef macos} tlbsCR {$else} tlbsCRLF {$endif} {$endif} ;

function AdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string;

{-- copy/pasted DIRECTLY from freepascal sources ------------------------------}

function IntToStr(Value: integer): string;
function IntToStr(Value: Int64): string;
function IntToStr(Value: QWord): string;

function strcopy(dest,source : pchar) : pchar;
function strecopy(dest,source : pchar) : pchar;
function strlcopy(dest,source : pchar;maxlen : sizeint) : pchar;
function strend(p : pchar) : pchar;
function strcomp(str1,str2 : pchar) : longint;
function strlcomp(str1,str2 : pchar;l : sizeint) : sizeint;
function stricomp(str1,str2 : pchar) : sizeint;
function strlicomp(str1,str2 : pchar;l : sizeint) : sizeint;
function strscan(p : pchar;c : char) : pchar;
function strrscan(p : pchar;c : char) : pchar;
function strupper(p : pchar) : pchar;
function strlower(p : pchar) : pchar;

{ ansistring functions }
function StrPas(Str: PChar): string;
function StrAlloc(Size: cardinal): PChar;
function strnew(p : pchar) : pchar;
function StrPCopy(Dest: PChar; Source: string): PChar;
function StrPLCopy(Dest: PChar; Source: string; MaxLen: SizeUInt): PChar;
procedure StrDispose(Str: PChar);
function StrBufSize(Str: PChar): SizeUInt;

function strcat(dest,source : pchar) : pchar;
function strlcat(dest,source : pchar;l : SizeInt) : pchar;
function strmove(dest,source : pchar;l : SizeInt) : pchar;
function strpos(str1,str2 : pchar) : pchar;

function ExtractRelativepath (Const BaseName,DestNAme : String): String;
function IncludeTrailingPathDelimiter(Const Path : String) : String;
function IncludeTrailingBackslash(Const Path : String) : String;
function ExcludeTrailingBackslash(Const Path: string): string;
function ExcludeTrailingPathDelimiter(Const Path: string): string;
function IsPathDelimiter(Const Path: string; Index: Integer): Boolean;
Procedure DoDirSeparators (Var FileName : String);
Function SetDirSeparators (Const FileName : String) : String;
Function GetDirs (Var DirName : String; Var Dirs : Array of pchar) : Longint;

{------------------------------------------------------------------------------}


{-- obsolete functions, replace with FPC sources ------------------------------}
 {$IFNDEF CPUARM}
 (*
  //function StrLComp(const Str1, Str2: PChar; MaxLen: Cardinal): Integer; assembler;}
  function StrLen(const Str: PChar): Cardinal; assembler;
  //function StrScan(Str: PChar; Chr: Char): PChar; assembler;}
 *)
 {$ENDIF}

{------------------------------------------------------------------------------}

{--COPY/PASTED FROM FPC SOURCES: finah.inc ------------------------------------}

function ChangeFileExt(const FileName, Extension: string): string;
function ExtractFilePath(const FileName: string): string;
function ExtractFileDrive(const FileName: string): string;
function ExtractFileName(const FileName: string): string;
function ExtractFileExt(const FileName: string): string;
function ExtractFileDir(Const FileName : string): string;

{ L505: relies on dos/oldlinux which has initialization/finalization
function ExpandFileName (Const FileName : string): String;
function ExpandUNCFileName (Const FileName : string): String;
}

{Function SameFileName(const S1, S2: string): Boolean;}

{------------------------------------------------------------------------------}


{-- copy/pasted from sysstrh.inc ----------------------------------------------}


{==============================================================================}
{   standard functions                                                         }
{==============================================================================}

type
   PString = ^String;

   { For FloatToText }
   TFloatFormat = (ffGeneral, ffExponent, ffFixed, ffNumber, ffCurrency);
   TFloatValue = (fvExtended, fvCurrency, fvSingle, fvReal, fvDouble, fvComp);


   TFloatRec = Record
      Exponent: Integer;
      Negative: Boolean;
      Digits: Array[0..18] Of Char;
   End;

const
  { For floattodatetime }
  MinDateTime: TDateTime = -657434.0;      { 01/01/0100 12:00:00.000 AM }
  MaxDateTime: TDateTime =  2958465.99999; { 12/31/9999 11:59:59.999 PM }

(*
//L505 Commented out

{$if defined(FPC_HAS_TYPE_EXTENDED) or defined(FPC_HAS_TYPE_FLOAT128)}
  MinCurrency: Currency = -922337203685477.5807;
  MaxCurrency: Currency =  922337203685477.5807;
{$else}
  MinCurrency: Currency = -922337203685477.0000;
  MaxCurrency: Currency =  922337203685477.0000;
{$endif}
*)


Const
  LeadBytes: set of Char = [];
  EmptyStr : string = '';
  NullStr : PString = @EmptyStr;

  EmptyWideStr : WideString = '';
//  NullWideStr : PWideString = @EmptyWideStr;

function NewStr(const S: string): PString;
procedure DisposeStr(S: PString);
procedure AssignStr(var P: PString; const S: string);
procedure AppendStr(var Dest: String; const S: string);
function UpperCase(const s: string): string;
function LowerCase(const s: string): string; overload;
function CompareStr(const S1, S2: string): Integer;
function CompareMemRange(P1, P2: Pointer; Length: cardinal): integer;
function CompareMem(P1, P2: Pointer; Length: cardinal): Boolean;
function CompareText(const S1, S2: string): integer;
function SameText(const s1,s2:String):Boolean;

function AnsiUpperCase(const s: string): string;{$ifdef SYSUTILSINLINE}inline;{$endif}
function AnsiLowerCase(const s: string): string;{$ifdef SYSUTILSINLINE}inline;{$endif}
function AnsiCompareStr(const S1, S2: string): integer;{$ifdef SYSUTILSINLINE}inline;{$endif}
function AnsiCompareText(const S1, S2: string): integer;{$ifdef SYSUTILSINLINE}inline;{$endif}
function AnsiSameText(const s1,s2:String):Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}
function AnsiSameStr(const s1,s2:String):Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}
function AnsiStrComp(S1, S2: PChar): integer;{$ifdef SYSUTILSINLINE}inline;{$endif}
function AnsiStrIComp(S1, S2: PChar): integer;{$ifdef SYSUTILSINLINE}inline;{$endif}
function AnsiStrLComp(S1, S2: PChar; MaxLen: cardinal): integer;{$ifdef SYSUTILSINLINE}inline;{$endif}
function AnsiStrLIComp(S1, S2: PChar; MaxLen: cardinal): integer;{$ifdef SYSUTILSINLINE}inline;{$endif}
function AnsiStrLower(Str: PChar): PChar;{$ifdef SYSUTILSINLINE}inline;{$endif}
function AnsiStrUpper(Str: PChar): PChar;{$ifdef SYSUTILSINLINE}inline;{$endif}
function AnsiLastChar(const S: string): PChar;
function AnsiStrLastChar(Str: PChar): PChar;

function Trim(const S: string): string;
function TrimLeft(const S: string): string;
function TrimRight(const S: string): string;
function QuotedStr(const S: string): string;
function AnsiQuotedStr(const S: string; Quote: char): string;
function AnsiExtractQuotedStr(var Src: PChar; Quote: Char): string;
function AdjustLineBreaks(const S: string): string;

function IsValidIdent(const Ident: string): boolean;

function IntToHex(Value: integer; Digits: integer): string;
function IntToHex(Value: Int64; Digits: integer): string;
function StrToInt(const s: string): integer;
function TryStrToInt(const s: string; var i : integer) : boolean;
function StrToInt64(const s: string): int64;
function TryStrToInt64(const s: string; var i : int64) : boolean;
function StrToIntDef(const S: string; Default: integer): integer;
function StrToInt64Def(const S: string; Default: int64): int64;
function LoadStr(Ident: integer): string;
// function FmtLoadStr(Ident: integer; const Args: array of const): string;

{
//L505: commented out
Function Format (Const Fmt : String; const Args : Array of const) : String;
Function FormatBuf (Var Buffer; BufLen : Cardinal; Const Fmt; fmtLen : Cardinal; Const Args : Array of const) : Cardinal;
Function StrFmt(Buffer,Fmt : PChar; Const args: Array of const) : Pchar;
Function StrLFmt(Buffer : PCHar; Maxlen : Cardinal;Fmt : PChar; Const args: Array of const) : Pchar;
Procedure FmtStr(Var Res: String; Const Fmt : String; Const args: Array of const);                   }
Function FloatToStrF(Value: Extended; format: TFloatFormat; Precision, Digits: Integer): String;
Function FloatToStr(Value: Extended): String;
Function StrToFloat(Const S : String) : Extended;
Function StrToFloatDef(Const S: String; Const Default: Extended): Extended;
Function TryStrToFloat(Const S : String; Var Value: Single): Boolean;
Function TryStrToFloat(Const S : String; Var Value: Double): Boolean;
{$ifdef FPC_HAS_TYPE_EXTENDED}
Function TryStrToFloat(Const S : String; Var Value: Extended): Boolean;
{$endif FPC_HAS_TYPE_EXTENDED}
Function TextToFloat(Buffer: PChar; Var Value: Extended): Boolean;
Function TextToFloat(Buffer: PChar; Var Value; ValueType: TFloatValue): Boolean;
Function FloatToText(Buffer: PChar; Value: Extended; format: TFloatFormat; Precision, Digits: Integer): Longint;
{
//L505: commented out
Function FloatToDateTime (Const Value : Extended) : TDateTime;
Function FloattoCurr (Const Value : Extended) : Currency;
function TryFloatToCurr(const Value: Extended; var AResult: Currency): Boolean; }
Function CurrToStr(Value: Currency): string;
function StrToCurr(const S: string): Currency;
function TryStrToCurr(const S: string;Var Value : Currency): Boolean;
function StrToCurrDef(const S: string; Default : Currency): Currency;
function StrToBool(const S: string): Boolean;
function BoolToStr(B: Boolean): string;
function LastDelimiter(const Delimiters, S: string): Integer;
function StringReplace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string;
Function FloatToTextFmt(Buffer: PChar; Value: Extended; format: PChar): Integer;
Procedure FloatToDecimal(Var Result: TFloatRec; Value: Extended; Precision, Decimals : integer);
Function FormatFloat(Const Format : String; Value : Extended) : String;
Function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean;
function FormatCurr(const Format: string; Value: Currency): string;

function SScanf(const s: string; const fmt : string;const Pointers : array of Pointer) : Integer;

{// MBCS Functions. No MBCS yet, so mostly these are calls to the regular counterparts.}
Type
  TMbcsByteType = (mbSingleByte, mbLeadByte, mbTrailByte);

Function ByteType(const S: string; Index: Integer): TMbcsByteType;
Function StrByteType(Str: PChar; Index: Cardinal): TMbcsByteType;
Function ByteToCharLen(const S: string; MaxLen: Integer): Integer;
Function CharToByteLen(const S: string; MaxLen: Integer): Integer;
Function ByteToCharIndex(const S: string; Index: Integer): Integer;
Function StrCharLength(const Str: PChar): Integer;


const
 {$ifndef unix}
  SwitchChars = ['/','-'];
 {$else}
  SwitchChars = ['-'];
 {$endif}


Function FindCmdLineSwitch(const Switch: string; const Chars: TSysCharSet;IgnoreCase: Boolean): Boolean;
Function FindCmdLineSwitch(const Switch: string; IgnoreCase: Boolean): Boolean;
Function FindCmdLineSwitch(const Switch: string): Boolean;

function WrapText(const Line, BreakStr: string; const BreakChars: TSysCharSet;  MaxCol: Integer): string;
function WrapText(const Line: string; MaxCol: Integer): string;



{==============================================================================}
{   extra functions                                                            }
{==============================================================================}

function LeftStr(const S: string; Count: integer): string;
function RightStr(const S: string; Count: integer): string;
function BCDToInt(Value: integer): integer;


{------------------------------------------------------------------------------}

implementation
{$ifdef UNIX}uses unixutil;{$endif UNIX}




{BEGIN: WRAPPERS FOR SMARTLINKING }
// by placing below standalone functions in separate units without 
// initialization and finalization, and making wrappers for them here.. the 
// sysutils unit (currently called compactsysutils) can remain compatible with
// delphi but the separate units mean more maintainable code, and as a bonus
// smaller executable sizes when the person chooses to use those units alone


function Trim(const S: string): string;
begin
  result:= pwstrutil.trim(s);
end;

function TrimLeft(const S: string): string;
begin
  result:= pwstrutil.trimleft(s);
end;

function TrimRight(const S: string): string;
begin
  result:= pwstrutil.trimright(s);
end;

function QuotedStr(const S: string): string;
begin
  result:= pwstrutil.quotedstr(s);
end;

function AnsiQuotedStr(const S: string; Quote: char): string;
begin
  result:= pwstrutil.ansiquotedstr(s, quote);
end;

function AnsiExtractQuotedStr(var  Src: PChar; Quote: Char): string;
begin
  result:= pwstrutil.ansiextractquotedstr(src,  quote);
end;

function AdjustLineBreaks(const S: string): string;
begin
  result:= pwstrutil.adjustlinebreaks(s);
end;

function AdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string;
begin
  result:= pwstrutil.adjustlinebreaks(s, style);
end;

function IsValidIdent(const Ident: string): boolean;
begin
  result:= pwstrutil.isvalidident(ident);
end;

function WrapText(const Line, BreakStr: string; const BreakChars: TSysCharSet;  MaxCol: Integer): string;
begin
  result:= pwstrutil.wraptext(line, breakstr, breakchars, maxcol);
end;

function WrapText(const Line: string; MaxCol: Integer): string;
begin
  result:= pwstrutil.wraptext(line, maxcol);
end;

function StringReplace(const S, OldPattern, NewPattern: string;  
  Flags: TReplaceFlags): string;
begin
  result:= pwstrutil.stringreplace(s, oldpattern, newpattern, pwstrutil.TReplaceFlags(flags));
end;

function ExtractFilePath(const FileName: string): string;
begin
  result:= pwfileutil.ExtractFilePath(filename);
end;

function ExtractFileDrive(const FileName: string): string;
begin
  result:= pwfileutil.ExtractFileDrive(filename);
end;

function ExtractFileName(const FileName: string): string;
begin
  result:= pwfileutil.ExtractFileName(filename);
end;

function ExtractFileExt(const FileName: string): string;
begin
  result:= pwfileutil.ExtractFileExt(filename);
end;

function ExtractFileDir(Const FileName : string): string;
begin
  result:= pwfileutil.ExtractFileDir(filename);
end;

function StrCopy(Dest, Source:PChar): PChar;
begin
  result:= pwstrutil.strcopy(dest, source);
end; 

function StrECopy(Dest, Source: PChar): PChar;
begin
  result:= pwstrutil.strecopy(dest, source) ;
end; 

function StrLCopy(Dest,Source: PChar; MaxLen: SizeInt): PChar;
begin
  result:= pwstrutil.strlcopy(dest, source, maxlen) ;
end; 

function StrEnd(P: PChar): PChar;
begin
  result:= pwstrutil.strend(p);
end; 

function StrComp(Str1, Str2 : PChar): SizeInt;
begin
  result:= pwstrutil.strcomp(str1, str2);
end; 

function StrLComp(Str1, Str2 : PChar; L: SizeInt): SizeInt;
begin
  result:= pwstrutil.strlcomp(str1, str2, l);
end; 

function StrIComp(Str1, Str2 : PChar): SizeInt;
begin
  result:= pwstrutil.stricomp(Str1,  str2);
end; 

function StrLIComp(Str1, Str2 : PChar; L: SizeInt): SizeInt;
begin
  result:= pwstrutil.strlicomp(str1, str2, l) ;
end; 

function StrScan(P: PChar; C: Char): PChar;
begin
  result:= pwstrutil.strscan(p, c);
end; 

function StrRScan(P: PChar; C: Char): PChar;
begin
  result:= pwstrutil.strrscan(p, c);
end; 

function StrUpper(P: PChar): PChar;
begin
  result:= pwstrutil.strupper(p);
end; 

function StrLower(P: PChar): PChar;
begin
  result:= pwstrutil.strlower(p);
end; 

function NewStr(const S: string): PString;
begin
  result:= pwstrutil.newstr(s);
end; 

procedure DisposeStr(S: PString);
begin
  pwstrutil.disposestr(s);
end; 

procedure AssignStr(var P: PString; const S: string);
begin
  pwstrutil.assignstr(p, s);
end; 

procedure AppendStr(var Dest: String; const S: string);
begin
  pwstrutil.appendstr(dest, s);
end; 

function UpperCase(Const S : String) : String;
begin
  result:= pwstrutil.uppercase(s);
end; 

function Lowercase(Const S : String) : String;
begin
  result:= pwstrutil.lowercase(s);
end; 

function CompareMemRange(P1, P2: Pointer; Length: cardinal): integer;
begin
  result:= pwstrutil.comparememrange(p1,  p2, length);
end; 

function CompareStr(const S1, S2: string): Integer;
begin
  result:= pwstrutil.comparestr(s1, s2);
end; 

function ExtractRelativepath (Const BaseName,DestNAme : String): String;
begin
  result:= pwfileutil.extractrelativepath(basename, destname);
end; 

function IncludeTrailingPathDelimiter(Const Path : String) : String;
begin
  result:= pwfileutil.includetrailingpathdelimiter(path);
end; 

function IncludeTrailingBackslash(Const Path : String) : String;
begin
  result:= pwfileutil.includetrailingbackslash(path);
end; 

function ExcludeTrailingBackslash(Const Path: string): string;
begin
  result:= pwfileutil.excludetrailingbackslash(path);
end; 

function ExcludeTrailingPathDelimiter(Const Path: string): string;
begin
  result:= pwfileutil.excludetrailingpathdelimiter(path);
end; 

function IsPathDelimiter(Const Path: string; Index: Integer): Boolean;
begin
  result:= pwfileutil.ispathdelimiter(path, index);
end; 

Procedure DoDirSeparators (Var FileName : String);
begin
  pwfileutil.dodirseparators(filename);
end; 

Function SetDirSeparators (Const FileName : String) : String;
begin
  result:= pwfileutil.setdirseparators(filename);
end; 

Function GetDirs (Var DirName : String; Var Dirs : Array of pchar) : Longint;
begin
  result:= pwfileutil.getdirs(dirname, dirs);
end; 

function strcat(dest,source : pchar) : pchar;
begin
  result:= pwstrutil.strcat(dest, source);
end;

function strlcat(dest,source : pchar;l : SizeInt) : pchar;
begin
  result:= pwstrutil.strlcat(dest, source, l);
end;

function strmove(dest,source : pchar;l : SizeInt) : pchar;
begin
  result:= pwstrutil.strmove(dest, source, l);
end;

function strpos(str1,str2 : pchar) : pchar;
begin
  result:= pwstrutil.strpos(str1, str2);
end;

function StrPas(Str: PChar): string;
begin
  result:= pwstrutil.strpas(str);
end;

function StrAlloc(Size: cardinal): PChar;
begin
  result:= pwstrutil.stralloc(size);
end;

function strnew(p : pchar) : pchar;
begin
  result:= pwstrutil.strnew(p);
end;

function StrPCopy(Dest: PChar; Source: string): PChar;
begin
  result:= pwstrutil.strpcopy(dest,  source);
end;

function StrPLCopy(Dest: PChar; Source: string; MaxLen: SizeUInt): PChar;
begin
  result:= pwstrutil.strplcopy(dest, source, maxlen);
end;

procedure StrDispose(Str: PChar);
begin
  pwstrutil.strdispose(str);
end;

function StrBufSize(Str: PChar): SizeUInt;
begin
  result:= pwstrutil.strbufsize(str);
end;

function LeftStr(const S: string; Count: integer): string;
begin
  result:= pwstrutil.leftstr(s, count);
end ;

function RightStr(const S: string; Count: integer): string;
begin
  result:= pwstrutil.rightstr(s, count);
end;

function ChangeFileExt(const FileName, Extension: string): string;
begin
  result:= pwfileutil.changefileext(filename, extension);
end;

{END: WRAPPERS FOR SMARTLINKING }





{$ifdef UNIX}
{--COPY/PASTED FROM FPC SOURCES: sysutils.pp /unix ----------------------------}
Type
  ComStr  = String[255];
  PathStr = String[255];
  DirStr  = String[255];
  NameStr = String[255];
  ExtStr  = String[255];
{------------------------------------------------------------------------------}
// TODO - FIX BELOW UNIX INCLUDE FILE, UNCOMMENT WHEN DONE
//  {$i compactsysutils_unix_filefunctions_implementation.inc}
{$endif UNIX}

{$ifdef WIN32}
  {$i compactsysutils_win32_filefunctions_implementation.inc}
{$endif WIN32}



{$IFNDEF CPUARM}
  {$ASMMODE ATT}
{$ENDIF}


{-- copy/pasted from sysstr.inc -----------------------------------------------}


function CompareMem(P1, P2: Pointer; Length: cardinal): Boolean;
var i: cardinal;
begin
  Result:=True;
  I:=0;
  If (P1)<>(P2) then
    While Result and (i<Length) do
    begin
      Result:=PByte(P1)^=PByte(P2)^;
      Inc(I);
      Inc(pchar(P1));
      Inc(pchar(P2));
    end;
end;


{   CompareText compares S1 and S2, the result is the based on
    substraction of the ascii values of characters in S1 and S2
    comparison is case-insensitive
    case     result
    S1 < S2  < 0
    S1 > S2  > 0
    S1 = S2  = 0     }

function CompareText(const S1, S2: string): integer;
var
  i, count, count1, count2: integer; Chr1, Chr2: byte;
begin
  result := 0;
  Count1 := Length(S1);
  Count2 := Length(S2);
  if (Count1>Count2) then
    Count := Count2
  else
    Count := Count1;
  i := 0;
  while (result=0) and (i<count) do
    begin
    inc (i);
     Chr1 := byte(s1[i]);
     Chr2 := byte(s2[i]);
     if Chr1 in [97..122] then
       dec(Chr1,32);
     if Chr2 in [97..122] then
       dec(Chr2,32);
     result := Chr1 - Chr2;
     end ;
  if (result = 0) then
    result:=(count1-count2);
end;

function SameText(const s1,s2:String):Boolean;
begin
  Result:=CompareText(S1,S2)=0;
end;

{==============================================================================}
{   Ansi string functions                                                      }
{   these functions rely on the character set loaded by the OS                 }
{==============================================================================}

function GenericAnsiUpperCase(const s: string): string;
var len, i: integer;
begin
  len := length(s);
  SetLength(result, len);
  for i := 1 to len do
     result[i] := UpperCaseTable[ord(s[i])];
end;


function GenericAnsiLowerCase(const s: string): string;
var len, i: integer;
begin
  len := length(s);
  SetLength(result, len);
  for i := 1 to len do
     result[i] := LowerCaseTable[ord(s[i])];
end;


function GenericAnsiCompareStr(const S1, S2: string): PtrInt;
Var I, L1, L2 : SizeInt;
begin
  Result:=0;
  L1:=Length(S1);
  L2:=Length(S2);
  I:=1;
  While (Result=0) and ((I<=L1) and (I<=L2)) do
    begin
    Result:=Ord(S1[I])-Ord(S2[I]); //!! Must be replaced by ansi characters !!
    Inc(I);
    end;
  If Result=0 Then
    Result:=L1-L2;
end;

function GenericAnsiCompareText(const S1, S2: string): PtrInt;
Var I,L1,L2 : SizeInt;
begin
  Result:=0;
  L1:=Length(S1);
  L2:=Length(S2);
  I:=1;
  While (Result=0) and ((I<=L1) and (I<=L2)) do
  begin
    Result:=Ord(LowerCaseTable[Ord(S1[I])])-Ord(LowerCaseTable[Ord(S2[I])]); //!! Must be replaced by ansi characters !!
    Inc(I);
  end;
  If Result=0 Then
    Result:=L1-L2;
end;

function AnsiSameText(const s1,s2:String):Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}
begin
  AnsiSameText:=AnsiCompareText(S1,S2)=0;
end;

function AnsiSameStr(const s1,s2:String):Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}
begin
  AnsiSameStr:=AnsiCompareStr(S1,S2)=0;
end;

function GenericAnsiStrComp(S1, S2: PChar): PtrInt;
begin
  Result:=0;
  If S1=Nil then
    begin
    If S2=Nil Then Exit;
    result:=-1;
    exit;
    end;
  If S2=Nil then
    begin
    Result:=1;
    exit;
    end;
  Repeat
    Result:=Ord(S1[0])-Ord(S2[0]); //!! Must be replaced by ansi characters !!
    Inc(S1);
    Inc(S2);
  Until (Result<>0) or ((S1[0]=#0) or (S2[0]=#0))
end;


function GenericAnsiStrIComp(S1, S2: PChar): PtrInt;
begin
  Result:=0;
  If S1=Nil then
    begin
    If S2=Nil Then Exit;
    result:=-1;
    exit;
    end;
  If S2=Nil then
    begin
    Result:=1;
    exit;
    end;
  Repeat
    Result:=Ord(LowerCaseTable[Ord(S1[0])])-Ord(LowerCaseTable[Ord(S2[0])]); //!! Must be replaced by ansi characters !!
    Inc(S1);
    Inc(S2);
  Until (Result<>0) or ((S1[0]=#0) or (S2[0]=#0))
end;


function GenericAnsiStrLComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
Var I : cardinal;
begin
  Result:=0;
  If MaxLen=0 then exit;
  If S1=Nil then
    begin
    If S2=Nil Then Exit;
    result:=-1;
    exit;
    end;
  If S2=Nil then
    begin
    Result:=1;
    exit;
    end;
  I:=0;
  Repeat
    Result:=Ord(S1[0])-Ord(S2[0]); //!! Must be replaced by ansi characters !!
    Inc(S1);
    Inc(S2);
    Inc(I);
  Until (Result<>0) or ((S1[0]=#0) or (S2[0]=#0)) or (I=MaxLen)
end;


function GenericAnsiStrLIComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
Var I : cardinal;
begin
  Result:=0;
  If MaxLen=0 then exit;
  If S1=Nil then
    begin
    If S2=Nil Then Exit;
    result:=-1;
    exit;
    end;
  If S2=Nil then
    begin
    Result:=1;
    exit;
    end;
  I:=0;
  Repeat
    Result:=Ord(LowerCaseTable[Ord(S1[0])])-Ord(LowerCaseTable[Ord(S2[0])]); //!! Must be replaced by ansi characters !!
    Inc(S1);
    Inc(S2);
    Inc(I);
  Until (Result<>0) or ((S1[0]=#0) or (S2[0]=#0)) or (I=MaxLen)
end;


function GenericAnsiStrLower(Str: PChar): PChar;
begin
  result := Str;
  if Str <> Nil then
  begin
    while Str^ <> #0 do
    begin
      Str^ := LowerCaseTable[byte(Str^)];
      Str := Str + 1;
    end;
  end;
end;


function GenericAnsiStrUpper(Str: PChar): PChar;
begin
  result := Str;
  if Str <> Nil then
  begin
    while Str^ <> #0 do
    begin
      Str^ := UpperCaseTable[byte(Str^)];
      Str := Str + 1;
    end ;
  end ;
end ;

function AnsiLastChar(const S: string): PChar;
begin
  //!! No multibyte yet, so we return the last one.
  result:=StrEnd(Pchar(S));
  Dec(Result);
end ;

function AnsiStrLastChar(Str: PChar): PChar;
begin
  //!! No multibyte yet, so we return the last one.
  result:=StrEnd(Str);
  Dec(Result);
end ;

function AnsiUpperCase(const s: string): string;{$ifdef SYSUTILSINLINE}inline;{$endif}
begin
  result:=widestringmanager.UpperAnsiStringProc(s);
end;

function AnsiLowerCase(const s: string): string;{$ifdef SYSUTILSINLINE}inline;{$endif}
begin
  result:=widestringmanager.LowerAnsiStringProc(s);
end;

function AnsiCompareStr(const S1, S2: string): integer;{$ifdef SYSUTILSINLINE}inline;{$endif}
begin
  result:=widestringmanager.CompareStrAnsiStringProc(s1,s2);
end;

function AnsiCompareText(const S1, S2: string): integer;{$ifdef SYSUTILSINLINE}inline;{$endif}
begin
  result:=widestringmanager.CompareTextAnsiStringProc(s1,s2);
end;

function AnsiStrComp(S1, S2: PChar): integer;{$ifdef SYSUTILSINLINE}inline;{$endif}
begin
  result:=widestringmanager.StrCompAnsiStringProc(s1,s2);
end;

function AnsiStrIComp(S1, S2: PChar): integer;{$ifdef SYSUTILSINLINE}inline;{$endif}
begin
  result:=widestringmanager.StrICompAnsiStringProc(s1,s2);
end;

function AnsiStrLComp(S1, S2: PChar; MaxLen: cardinal): integer;{$ifdef SYSUTILSINLINE}inline;{$endif}
begin
  result:=widestringmanager.StrLCompAnsiStringProc(s1,s2,maxlen);
end;

function AnsiStrLIComp(S1, S2: PChar; MaxLen: cardinal): integer;{$ifdef SYSUTILSINLINE}inline;{$endif}
begin
  result:=widestringmanager.StrLICompAnsiStringProc(s1,s2,maxlen);
end;

function AnsiStrLower(Str: PChar): PChar;{$ifdef SYSUTILSINLINE}inline;{$endif}
begin
  result:=widestringmanager.StrLowerAnsiStringProc(Str);
end;

function AnsiStrUpper(Str: PChar): PChar;{$ifdef SYSUTILSINLINE}inline;{$endif}
begin
  result:=widestringmanager.StrUpperAnsiStringProc(Str);
end;

{==============================================================================}
{  End of Ansi functions                                                       }
{==============================================================================}



{   IntToStr returns a string representing the value of Value    }
function IntToStr(Value: integer): string;
begin
  System.Str(Value, result);
end ;

function IntToStr(Value: int64): string;
begin
  System.Str(Value, result);
end ;

function IntToStr(Value: QWord): string;
begin
  System.Str(Value, result);
end ;

{   IntToHex returns a string representing the hexadecimal value of Value   }

const
  HexDigits: array[0..15] of char = '0123456789ABCDEF';

function IntToHex(Value: integer; Digits: integer): string;
var i: integer;
begin
 SetLength(result, digits);
 for i := 0 to digits - 1 do
  begin
   result[digits - i] := HexDigits[value and 15];
   value := value shr 4;
  end ;
 while value <> 0 do begin
   result := HexDigits[value and 15] + result;
   value := value shr 4;
 end;
end ;

function IntToHex(Value: int64; Digits: integer): string;
var i: integer;
begin
 SetLength(result, digits);
 for i := 0 to digits - 1 do
  begin
   result[digits - i] := HexDigits[value and 15];
   value := value shr 4;
  end ;
 while value <> 0 do begin
   result := HexDigits[value and 15] + result;
   value := value shr 4;
 end;
end ;

function TryStrToInt(const s: string; var i : integer) : boolean;
var Error : word;
begin
  Val(s, i, Error);
  TryStrToInt:=Error=0
end;

{   StrToInt converts the string S to an integer value,
    if S does not represent a valid integer value EConvertError is raised  }
function StrToInt(const S: string): integer;
var Error: word;
begin
  Val(S, result, Error);
  if error <> 0 then result:= 0;
end;


function StrToInt64(const S: string): int64;
var Error: word;
begin
  Val(S, result, Error);
  if error <> 0 then result:= 0;
end;


function TryStrToInt64(const s: string; var i : int64) : boolean;
var
  Error : word;
begin
  Val(s, i, Error);
  TryStrToInt64:=Error=0
end;

{ StrToIntDef converts the string S to an integer value,
  Default is returned in case S does not represent a valid integer value  }
function StrToIntDef(const S: string; Default: integer): integer;
var
  Error: word;
begin
  Val(S, result, Error);
  if Error <> 0 then result := Default;
end ;

{ StrToIntDef converts the string S to an integer value,
  Default is returned in case S does not represent a valid integer value  }
function StrToInt64Def(const S: string; Default: int64): int64;
var
  Error: word;
begin
  Val(S, result, Error);
  if Error <> 0 then result := Default;
end ;


{ LoadStr returns the string resource Ident.   }
function LoadStr(Ident: integer): string;
begin
  result:='';
end ;

{ FmtLoadStr returns the string resource Ident and formats it accordingly   }
function FmtLoadStr(Ident: integer; const Args: array of const): string;
begin
  result:='';
end;

Const
  feInvalidFormat   = 1;
  feMissingArgument = 2;
  feInvalidArgIndex = 3;



function StrToFloat(Const S: String): Extended;
begin
  if not TextToFloat(Pchar(S),Result) then
    result:= 0;
//    Raise EConvertError.createfmt(SInValidFLoat,[S]);
// L505: COMMENTED OUT
end;

function StrToFloatDef(const S: string; const Default: Extended): Extended;
begin
  if not TextToFloat(PChar(S),Result,fvExtended) then
     Result:= Default;
end;

Function TextToFloat(Buffer: PChar; Var Value: Extended): Boolean;
Var
  E,P : Integer;
  S : String;
Begin
  S:=StrPas(Buffer);
  P:=Pos(DecimalSeparator,S);
  If (P<>0) Then
    S[P] := '.';
  Val(trim(S),Value,E);
  Result:=(E=0);
End;

Function TextToFloat(Buffer: PChar; Var Value; ValueType: TFloatValue): Boolean;
Var
  E,P : Integer;
  S : String;
Begin
  S:=StrPas(Buffer);
  P:=Pos(ThousandSeparator,S);
  While (P<>0) do
    begin
    Delete(S,P,1);
    P:=Pos(ThousandSeparator,S);
    end;
  P:=Pos(DecimalSeparator,S);
  If (P<>0) Then
    S[P] := '.';
  case ValueType of
    fvCurrency:
      Val(S,Currency(Value),E);
    fvExtended:
      Val(S,Extended(Value),E);
    fvDouble:
      Val(S,Double(Value),E);
    fvSingle:
      Val(S,Single(Value),E);
    fvComp:
      Val(S,Comp(Value),E);
    fvReal:
      Val(S,Real(Value),E);
  end;
  Result:=(E=0);
End;

Function TryStrToFloat(Const S : String; Var Value: Single): Boolean;
Begin
  Result := TextToFloat(PChar(S), Value, fvSingle);
End;

Function TryStrToFloat(Const S : String; Var Value: Double): Boolean;
Begin
  Result := TextToFloat(PChar(S), Value, fvDouble);
End;

{$ifdef FPC_HAS_TYPE_EXTENDED}
Function TryStrToFloat(Const S : String; Var Value: Extended): Boolean;
Begin
  Result := TextToFloat(PChar(S), Value);
End;
{$endif FPC_HAS_TYPE_EXTENDED}

Function FloatToStr(Value: Extended): String;
Begin
  Result := FloatToStrF(Value, ffGeneral, 15, 0);
End;

Function FloatToText(Buffer: PChar; Value: Extended; format: TFloatFormat; Precision, Digits: Integer): Longint;
Var
  Tmp: String[40];
Begin
  Tmp := FloatToStrF(Value, format, Precision, Digits);
  Result := Length(Tmp);
  Move(Tmp[1], Buffer[0], Result);
End;


Function FloatToStrF(Value: Extended; format: TFloatFormat; Precision, Digits: Integer): String;
Var
  P: Integer;
  Negative, TooSmall, TooLarge: Boolean;
Begin
  Case format Of

    ffGeneral:

      Begin
        If (Precision = -1) Or (Precision > 15) Then Precision := 15;
        TooSmall := (Abs(Value) < 0.00001) and (Value>0.0);
        If Not TooSmall Then
        Begin
          Str(Value:digits:precision, Result);
          P := Pos('.', Result);
          if P<>0 then
            Result[P] := DecimalSeparator;
          TooLarge := P > Precision + 1;
        End;

        If TooSmall Or TooLarge Then
          begin
          Result := FloatToStrF(Value, ffExponent, Precision, Digits);
          // Strip unneeded zeroes.
          P:=Pos('E',result)-1;
          If P<>-1 then
             While (P>1) and (Result[P]='0') do
               begin
               system.Delete(Result,P,1);
               Dec(P);
               end;
          end
        else if (P<>0) then // we have a decimalseparator
          begin
          P := Length(Result);
          While (P>0) and (Result[P] = '0') Do
            Dec(P);
          If (P>0) and (Result[P]=DecimalSeparator) Then
            Dec(P);
          SetLength(Result, P);
          end;
      End;

    ffExponent:

      Begin
        If (Precision = -1) Or (Precision > 15) Then Precision := 15;
        Str(Value:Precision + 8, Result);
        Result[3] := DecimalSeparator;
        P:=4;
        While (P>0) and (Digits < P) And (Result[Precision + 5] = '0') do
          Begin
          If P<>1 then
            system.Delete(Result, Precision + 5, 1)
          else
            system.Delete(Result, Precision + 3, 3);
          Dec(P);
          end;
        If Result[1] = ' ' Then
          System.Delete(Result, 1, 1);
      End;

    ffFixed:

      Begin
        If Digits = -1 Then Digits := 2
        Else If Digits > 18 Then Digits := 18;
        Str(Value:0:Digits, Result);
        If Result[1] = ' ' Then
          System.Delete(Result, 1, 1);
        P := Pos('.', Result);
        If P <> 0 Then Result[P] := DecimalSeparator;
      End;

    ffNumber:

      Begin
        If Digits = -1 Then Digits := 2
        Else If Digits > 15 Then Digits := 15;
        Str(Value:0:Digits, Result);
        If Result[1] = ' ' Then System.Delete(Result, 1, 1);
        P := Pos('.', Result);
        If P <> 0 Then
          Result[P] := DecimalSeparator
        else
          P := Length(Result)+1;
        Dec(P, 3);
        While (P > 1) Do
        Begin
          If Result[P - 1] <> '-' Then Insert(ThousandSeparator, Result, P);
          Dec(P, 3);
        End;
      End;

    ffCurrency:

      Begin
        If Value < 0 Then
        Begin
          Negative := True;
          Value := -Value;
        End
        Else Negative := False;

        If Digits = -1 Then Digits := CurrencyDecimals
        Else If Digits > 18 Then Digits := 18;
        Str(Value:0:Digits, Result);
        If Result[1] = ' ' Then System.Delete(Result, 1, 1);
        P := Pos('.', Result);
        If P <> 0 Then Result[P] := DecimalSeparator;
        Dec(P, 3);
        While (P > 1) Do
        Begin
          Insert(ThousandSeparator, Result, P);
          Dec(P, 3);
        End;

        If Not Negative Then
        Begin
          Case CurrencyFormat Of
            0: Result := CurrencyString + Result;
            1: Result := Result + CurrencyString;
            2: Result := CurrencyString + ' ' + Result;
            3: Result := Result + ' ' + CurrencyString;
          End
        End
        Else
        Begin
          Case NegCurrFormat Of
            0: Result := '(' + CurrencyString + Result + ')';
            1: Result := '-' + CurrencyString + Result;
            2: Result := CurrencyString + '-' + Result;
            3: Result := CurrencyString + Result + '-';
            4: Result := '(' + Result + CurrencyString + ')';
            5: Result := '-' + Result + CurrencyString;
            6: Result := Result + '-' + CurrencyString;
            7: Result := Result + CurrencyString + '-';
            8: Result := '-' + Result + ' ' + CurrencyString;
            9: Result := '-' + CurrencyString + ' ' + Result;
            10: Result := CurrencyString + ' ' + Result + '-';
          End;
        End;
      End;
  End;
End;


Function CurrToStr(Value: Currency): string;
begin
  Result:=FloatToStrF(Value,ffNumber,15,2);
end;

function StrToCurr(const S: string): Currency;
begin
  {if not} TextToFloat(PChar(S), Result, fvCurrency) {then}
//    Raise EConvertError.createfmt(SInValidFLoat,[S]);
// L505: COMMENTED OUT
end;


Function TryStrToCurr(Const S : String; Var Value: Currency): Boolean;
Begin
  Result := TextToFloat(PChar(S), Value, fvCurrency);
End;


function StrToCurrDef(const S: string; Default : Currency): Currency;
begin
  if not TextToFloat(PChar(S), Result, fvCurrency) then
    Result:=Default;
end;

function StrToBool(const S: string): Boolean;
Var
  Temp : String;
  D : Double;
  Code: word;

begin
  result:= false;
  Temp:=upcase(S);
  Val(temp,D,code);
  If Code=0 then
    Result:=(D<>0.0)
  else If Temp='TRUE' then
    result:=true;
//  All string values which are not TRUE, return false
end;

function BoolToStr(B: Boolean): string;
begin
  If B then
    Result:='TRUE'
  else
    Result:='FALSE';
end;

Function FloatToTextFmt(Buffer: PChar; Value: Extended; format: PChar): Integer;
Var
  Digits: String[40];                         { String Of Digits                 }
  Exponent: String[8];                        { Exponent strin                   }
  FmtStart, FmtStop: PChar;                   { Start And End Of relevant part   }
                                              { Of format String                 }
  ExpFmt, ExpSize: Integer;                   { Type And Length Of               }
                                              { exponential format chosen        }
  Placehold: Array[1..4] Of Integer;          { Number Of placeholders In All    }
                                              { four Sections                    }
  thousand: Boolean;                          { thousand separators?             }
  UnexpectedDigits: Integer;                  { Number Of unexpected Digits that }
                                              { have To be inserted before the   }
                                              { First placeholder.               }
  DigitExponent: Integer;                     { Exponent Of First digit In       }
                                              { Digits Array.                    }

  { Find end of format section starting at P. False, if empty }

  Function GetSectionEnd(Var P: PChar): Boolean;
  Var
    C: Char;
    SQ, DQ: Boolean;
  Begin
    Result := False;
    SQ := False;
    DQ := False;
    C := P[0];
    While (C<>#0) And ((C<>';') Or SQ Or DQ) Do
      Begin
      Result := True;
      Case C Of
        #34: If Not SQ Then DQ := Not DQ;
        #39: If Not DQ Then SQ := Not SQ;
      End;
      Inc(P);
      C := P[0];
      End;
  End;

  { Find start and end of format section to apply. If section doesn't exist,
    use section 1. If section 2 is used, the sign of value is ignored.       }

  Procedure GetSectionRange(section: Integer);
  Var
    Sec: Array[1..3] Of PChar;
    SecOk: Array[1..3] Of Boolean;
  Begin
    Sec[1] := format;
    SecOk[1] := GetSectionEnd(Sec[1]);
    If section > 1 Then
      Begin
      Sec[2] := Sec[1];
      If Sec[2][0] <> #0 Then
        Inc(Sec[2]);
      SecOk[2] := GetSectionEnd(Sec[2]);
      If section > 2 Then
        Begin
        Sec[3] := Sec[2];
        If Sec[3][0] <> #0 Then
          Inc(Sec[3]);
        SecOk[3] := GetSectionEnd(Sec[3]);
        End;
      End;
    If Not SecOk[1] Then
      FmtStart := Nil
    Else
      Begin
      If Not SecOk[section] Then
        section := 1
      Else If section = 2 Then
        Value := -Value;   { Remove sign }
      If section = 1 Then FmtStart := format Else
        Begin
        FmtStart := Sec[section - 1];
        Inc(FmtStart);
        End;
      FmtStop := Sec[section];
      End;
  End;

  { Find format section ranging from FmtStart to FmtStop. }

  Procedure GetFormatOptions;
  Var
    Fmt: PChar;
    SQ, DQ: Boolean;
    area: Integer;
  Begin
    SQ := False;
    DQ := False;
    Fmt := FmtStart;
    ExpFmt := 0;
    area := 1;
    thousand := False;
    Placehold[1] := 0;
    Placehold[2] := 0;
    Placehold[3] := 0;
    Placehold[4] := 0;
    While Fmt < FmtStop Do
      Begin
      Case Fmt[0] Of
        #34:
          Begin
          If Not SQ Then
            DQ := Not DQ;
          Inc(Fmt);
          End;
        #39:
          Begin
          If Not DQ Then
            SQ := Not SQ;
          Inc(Fmt);
          End;
      Else
        { This was 'if not SQ or DQ'. Looked wrong... }
        If Not SQ Or DQ Then
          Begin
          Case Fmt[0] Of
            '0':
              Begin
              Case area Of
                1:
                  area := 2;
                4:
                  Begin
                  area := 3;
                  Inc(Placehold[3], Placehold[4]);
                  Placehold[4] := 0;
                  End;
              End;
              Inc(Placehold[area]);
              Inc(Fmt);
              End;

            '#':
              Begin
              If area=3 Then
                area:=4;
              Inc(Placehold[area]);
              Inc(Fmt);
              End;
            '.':
              Begin
              If area<3 Then
                area:=3;
              Inc(Fmt);
              End;
            ',':
              Begin
              thousand := True;
              Inc(Fmt);
              End;
            'e', 'E':
              If ExpFmt = 0 Then
                Begin
                If (Fmt[0]='E') Then
                  ExpFmt:=1
                Else
                  ExpFmt := 3;
                Inc(Fmt);
                If (Fmt<FmtStop) Then
                  Begin
                  Case Fmt[0] Of
                    '+':
                      Begin
                      End;
                    '-':
                      Inc(ExpFmt);
                  Else
                    ExpFmt := 0;
                  End;
                  If ExpFmt <> 0 Then
                    Begin
                    Inc(Fmt);
                    ExpSize := 0;
                    While (Fmt<FmtStop) And
                          (ExpSize<4) And
                          (Fmt[0] In ['0'..'9']) Do
                      Begin
                      Inc(ExpSize);
                      Inc(Fmt);
                      End;
                    End;
                  End;
                End
              Else
                Inc(Fmt);
          Else { Case }
            Inc(Fmt);
          End; { Case }
          End; { Begin }
      End; { Case }
      End; { While .. Begin }
  End;

  Procedure FloatToStr;
  Var
    I, J, Exp, Width, Decimals, DecimalPoint, len: Integer;
  Begin
    If ExpFmt = 0 Then
      Begin
      { Fixpoint }
      Decimals:=Placehold[3]+Placehold[4];
      Width:=Placehold[1]+Placehold[2]+Decimals;
      If (Decimals=0) Then
        Str(Value:Width:0,Digits)
      Else
        Str(Value:Width+1:Decimals,Digits);
      len:=Length(Digits);
      { Find the decimal point }
      If (Decimals=0) Then
        DecimalPoint:=len+1
      Else
        DecimalPoint:=len-Decimals;
      { If value is very small, and no decimal places
        are desired, remove the leading 0.            }
      If (Abs(Value) < 1) And (Placehold[2] = 0) Then
        Begin
        If (Placehold[1]=0) Then
          Delete(Digits,DecimalPoint-1,1)
        Else
          Digits[DecimalPoint-1]:=' ';
        End;

      { Convert optional zeroes to spaces. }
      I:=len;
      J:=DecimalPoint+Placehold[3];
      While (I>J) And (Digits[I]='0') Do
        Begin
        Digits[I] := ' ';
        Dec(I);
        End;
      { If integer value and no obligatory decimal
        places, remove decimal point. }
      If (DecimalPoint < len) And (Digits[DecimalPoint + 1] = ' ') Then
          Digits[DecimalPoint] := ' ';
      { Convert spaces left from obligatory decimal point to zeroes. }
      I:=DecimalPoint-Placehold[2];
      While (I<DecimalPoint) And (Digits[I]=' ') Do
        Begin
        Digits[I] := '0';
        Inc(I);
        End;
      Exp := 0;
      End
    Else
      Begin
      { Scientific: exactly <Width> Digits With <Precision> Decimals
        And adjusted Exponent. }
      If Placehold[1]+Placehold[2]=0 Then
        Placehold[1]:=1;
      Decimals := Placehold[3] + Placehold[4];
      Width:=Placehold[1]+Placehold[2]+Decimals;
      Str(Value:Width+8,Digits);
      { Find and cut out exponent. Always the
        last 6 characters in the string.
        -> 0000E+0000                         }
      I:=Length(Digits)-5;
      Val(Copy(Digits,I+1,5),Exp,J);
      Exp:=Exp+1-(Placehold[1]+Placehold[2]);
      Delete(Digits, I, 6);
      { Str() always returns at least one digit after the decimal point.
        If we don't want it, we have to remove it. }
      If (Decimals=0) And (Placehold[1]+Placehold[2]<= 1) Then
        Begin
        If (Digits[4]>='5') Then
          Begin
          Inc(Digits[2]);
          If (Digits[2]>'9') Then
            Begin
            Digits[2] := '1';
            Inc(Exp);
            End;
          End;
        Delete(Digits, 3, 2);
        DecimalPoint := Length(Digits) + 1;
        End
      Else
        Begin
        { Move decimal point at the desired position }
        Delete(Digits, 3, 1);
        DecimalPoint:=2+Placehold[1]+Placehold[2];
        If (Decimals<>0) Then
          Insert('.',Digits,DecimalPoint);
        End;

      { Convert optional zeroes to spaces. }
      I := Length(Digits);
      J := DecimalPoint + Placehold[3];
      While (I > J) And (Digits[I] = '0') Do
        Begin
        Digits[I] := ' ';
        Dec(I);
        End;

      { If integer number and no obligatory decimal paces, remove decimal point }

      If (DecimalPoint<Length(Digits)) And
         (Digits[DecimalPoint+1]=' ') Then
          Digits[DecimalPoint]:=' ';
      If (Digits[1]=' ') Then
        Begin
        Delete(Digits, 1, 1);
        Dec(DecimalPoint);
        End;
      { Calculate exponent string }
      Str(Abs(Exp), Exponent);
      While Length(Exponent)<ExpSize Do
        Insert('0',Exponent,1);
      If Exp >= 0 Then
        Begin
        If (ExpFmt In [1,3]) Then
          Insert('+', Exponent, 1);
        End
      Else
        Insert('-',Exponent,1);
      If (ExpFmt<3) Then
        Insert('E',Exponent,1)
      Else
        Insert('e',Exponent,1);
      End;
    DigitExponent:=DecimalPoint-2;
    If (Digits[1]='-') Then
      Dec(DigitExponent);
    UnexpectedDigits:=DecimalPoint-1-(Placehold[1]+Placehold[2]);
  End;

  Function PutResult: LongInt;
  Var
    SQ, DQ: Boolean;
    Fmt, Buf: PChar;
    Dig, N: Integer;
  Begin
    SQ := False;
    DQ := False;
    Fmt := FmtStart;
    Buf := Buffer;
    Dig := 1;
    While (Fmt<FmtStop) Do
      Begin
      //Write(Fmt[0]);
      Case Fmt[0] Of
        #34:
          Begin
          If Not SQ Then
            DQ := Not DQ;
          Inc(Fmt);
          End;
        #39:
          Begin
          If Not DQ Then
            SQ := Not SQ;
          Inc(Fmt);
          End;
      Else
        If Not (SQ Or DQ) Then
          Begin
          Case Fmt[0] Of
            '0', '#', '.':
              Begin
              If (Dig=1) And (UnexpectedDigits>0) Then
                Begin
                { Everything unexpected is written before the first digit }
                For N := 1 To UnexpectedDigits Do
                  Begin
                  Buf[0] := Digits[N];
                  Inc(Buf);
                  If thousand And (Digits[N]<>'-') Then
                    Begin
                    If (DigitExponent Mod 3 = 0) And (DigitExponent>0) Then
                      Begin
                      Buf[0] := ThousandSeparator;
                      Inc(Buf);
                      End;
                    Dec(DigitExponent);
                    End;
                  End;
                Inc(Dig, UnexpectedDigits);
                End;
              If (Digits[Dig]<>' ') Then
                Begin
                If (Digits[Dig]='.') Then
                  Buf[0] := DecimalSeparator
                Else
                  Buf[0] := Digits[Dig];
                Inc(Buf);
                If thousand And (DigitExponent Mod 3 = 0) And (DigitExponent > 0) Then
                  Begin
                  Buf[0] := ThousandSeparator;
                  Inc(Buf);
                  End;
                End;
              Inc(Dig);
              Dec(DigitExponent);
              Inc(Fmt);
              End;
            'e', 'E':
              Begin
              If ExpFmt <> 0 Then
                Begin
                Inc(Fmt);
                If Fmt < FmtStop Then
                  Begin
                  If Fmt[0] In ['+', '-'] Then
                    Begin
                    Inc(Fmt, ExpSize);
                    For N:=1 To Length(Exponent) Do
                      Buf[N-1] := Exponent[N];
                    Inc(Buf,Length(Exponent));
                    ExpFmt:=0;
                    End;
                  Inc(Fmt);
                  End;
                End
              Else
                Begin
                { No legal exponential format.
                  Simply write the 'E' to the result. }
                Buf[0] := Fmt[0];
                Inc(Buf);
                Inc(Fmt);
                End;
              End;
          Else { Case }
            { Usual character }
            If (Fmt[0]<>',') Then
              Begin
              Buf[0] := Fmt[0];
              Inc(Buf);
              End;
            Inc(Fmt);
          End; { Case }
          End
        Else { IF }
          Begin
          { Character inside single or double quotes }
          Buf[0] := Fmt[0];
          Inc(Buf);
          Inc(Fmt);
          End;
      End; { Case }
    End; { While .. Begin }
    Result:=PtrInt(Buf)-PtrInt(Buffer);
  End;

Begin
  If (Value>0) Then
    GetSectionRange(1)
  Else If (Value<0) Then
    GetSectionRange(2)
  Else
    GetSectionRange(3);
  If FmtStart = Nil Then
    Begin
    Result := FloatToText(Buffer, Value, ffGeneral, 15, 4);
    End
  Else
    Begin
    GetFormatOptions;
    If (ExpFmt = 0) And (Abs(Value) >= 1E18) Then
      Result := FloatToText(Buffer, Value, ffGeneral, 15, 4)
    Else
      Begin
      FloatToStr;
      Result := PutResult;
      End;
    End;
End;


Procedure FloatToDecimal(Var Result: TFloatRec; Value: Extended; Precision, Decimals : integer);
Var
  Buffer: String[24];
  Error, N: Integer;
Begin
  Str(Value:23, Buffer);
  Result.Negative := (Buffer[1] = '-');
  Val(Copy(Buffer, 19, 5), Result.Exponent, Error);
  Inc(Result. Exponent);
  Result.Digits[0] := Buffer[2];
  Move(Buffer[4], Result.Digits[1], 14);
  If Decimals + Result.Exponent < Precision Then
    N := Decimals + Result.Exponent
  Else
    N := Precision;
  If N > 15 Then
    N := 15;
  If N = 0 Then
    Begin
    If Result.Digits[0] >= '5' Then
      Begin
      Result.Digits[0] := '1';
      Result.Digits[1] := #0;
      Inc(Result.Exponent);
      End
    Else
      Result.Digits[0] := #0;
    End
  Else If N > 0 Then
    Begin
    If Result.Digits[N] >= '5' Then
      Begin
      Repeat
        Result.Digits[N] := #0;
        Dec(N);
        Inc(Result.Digits[N]);
      Until (N = 0) Or (Result.Digits[N] < ':');
      If Result.Digits[0] = ':' Then
        Begin
        Result.Digits[0] := '1';
        Inc(Result.Exponent);
        End;
      End
    Else
      Begin
      Result.Digits[N] := '0';
      While (Result.Digits[N] = '0') And (N > -1) Do
        Begin
        Result.Digits[N] := #0;
        Dec(N);
        End;
      End;
    End
  Else
    Result.Digits[0] := #0;
  If Result.Digits[0] = #0 Then
    Begin
    Result.Exponent := 0;
    Result.Negative := False;
    End;
End;

Function FormatFloat(Const format: String; Value: Extended): String;
Var
  buf : Array[0..1024] of char;
Begin
  Buf[FloatToTextFmt(@Buf[0],Value,Pchar(Format))]:=#0;
  Result:=StrPas(@Buf);
End;

function FormatCurr(const Format: string; Value: Currency): string;
begin
  Result := FormatFloat(Format, Value);
end;


{==============================================================================}
{   extra functions                                                            }
{==============================================================================}



{    BCDToInt converts the BCD value Value to an integer   }

function BCDToInt(Value: integer): integer;
var i, j: integer;
begin
result := 0;
j := 1;
for i := 0 to SizeOf(Value) shr 1 - 1 do begin
   result := result + j * (Value and 15);
   j := j * 10;
   Value := Value shr 4;
   end ;
end ;

Function LastDelimiter(const Delimiters, S: string): Integer;
begin
  Result:=Length(S);
  While (Result>0) and (Pos(S[Result],Delimiters)=0) do
    Dec(Result);
end;


Function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean;
begin
  Result:=False;
  If (Index>0) and (Index<=Length(S)) then
    Result:=Pos(S[Index],Delimiters)<>0; // Note we don't do MBCS yet
end;

Function ByteToCharLen(const S: string; MaxLen: Integer): Integer;
begin
  Result:=Length(S);
  If Result>MaxLen then
    Result:=MaxLen;
end;

Function ByteToCharIndex(const S: string; Index: Integer): Integer;
begin
  Result:=Index;
end;


Function CharToByteLen(const S: string; MaxLen: Integer): Integer;
begin
  Result:=Length(S);
  If Result>MaxLen then
    Result:=MaxLen;
end;

Function CharToByteIndex(const S: string; Index: Integer): Integer;
begin
  Result:=Index;
end;

Function ByteType(const S: string; Index: Integer): TMbcsByteType;
begin
  Result:=mbSingleByte;
end;

Function StrByteType(Str: PChar; Index: Cardinal): TMbcsByteType;
begin
  Result:=mbSingleByte;
end;


Function StrCharLength(const Str: PChar): Integer;
begin
  result:=widestringmanager.CharLengthPCharProc(Str);
end;


Function FindCmdLineSwitch(const Switch: string; const Chars: TSysCharSet;IgnoreCase: Boolean): Boolean;
Var
  I,L : Integer;
  S,T : String;
begin
  Result:=False;
  S:=Switch;
  If IgnoreCase then
    S:=UpperCase(S);
  I:=ParamCount;
  While (Not Result) and (I>0) do
    begin
    L:=Length(Paramstr(I));
    If (L>0) and (ParamStr(I)[1] in Chars) then
      begin
      T:=Copy(ParamStr(I),2,L-1);
      If IgnoreCase then
        T:=UpperCase(T);
      Result:=S=T;
      end;
    Dec(i);
    end;
end;

Function FindCmdLineSwitch(const Switch: string; IgnoreCase: Boolean): Boolean;
begin
  Result:=FindCmdLineSwitch(Switch,SwitchChars,IgnoreCase);
end;

Function FindCmdLineSwitch(const Switch: string): Boolean;
begin
  Result:=FindCmdLineSwitch(Switch,SwitchChars,False);
end;



{
   Case Translation Tables
   Can be used in internationalization support.

   Although these tables can be obtained through system calls
   it is better to not use those, since most implementation are not 100%
   WARNING:
   before modifying a translation table make sure that the current codepage
   of the OS corresponds to the one you make changes to
}


(*
const
   { upper case translation table for character set 850 }
   CP850UCT: array[128..255] of char =
   ('€', 'š', '', '¶', 'Ž', '¶', '', '€', 'Ò', 'Ó', 'Ô', 'Ø', '×', 'Þ', 'Ž', '',
    '', '’', '’', 'â', '™', 'ã', 'ê', 'ë', 'Y', '™', 'š', '', 'œ', '', 'ž', 'Ÿ',
    'µ', 'Ö', 'à', 'é', '¥', '¥', '¦', '§', '¨', '©', 'ª', '«', '¬', '­', '®', '¯',
    '°', '±', '²', '³', '´', 'µ', '¶', '·', '¸', '¹', 'º', '»', '¼', '½', '¾', '¿',
    'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Ç', 'Ç', 'È', 'É', 'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï',
    'Ð', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', '×', 'Ø', 'Ù', 'Ú', 'Û', 'Ü', 'Ý', 'Þ', 'ß',
    'à', 'á', 'â', 'ã', 'å', 'å', 'æ', 'í', 'è', 'é', 'ê', 'ë', 'í', 'í', 'î', 'ï',
    'ð', 'ñ', 'ò', 'ó', 'ô', 'õ', 'ö', '÷', 'ø', 'ù', 'ú', 'û', 'ü', 'ý', 'þ', 'ÿ');

   { lower case translation table for character set 850 }
   CP850LCT: array[128..255] of char =
   ('‡', '', '‚', 'ƒ', '„', '…', '†', '‡', 'ˆ', '‰', 'Š', '‹', 'Œ', '', '„', '†',
    '‚', '‘', '‘', '“', '”', '•', '–', '—', '˜', '”', '', '›', 'œ', '›', 'ž', 'Ÿ',
    ' ', '¡', '¢', '£', '¤', '¤', '¦', '§', '¨', '©', 'ª', '«', '¬', '­', '®', '¯',
    '°', '±', '²', '³', '´', ' ', 'ƒ', '…', '¸', '¹', 'º', '»', '¼', '½', '¾', '¿',
    'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Æ', 'È', 'É', 'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï',
    'Ð', 'Ñ', 'ˆ', '‰', 'Š', 'Õ', '¡', 'Œ', '‹', 'Ù', 'Ú', 'Û', 'Ü', 'Ý', '', 'ß',
    '¢', 'á', '“', '•', 'ä', 'ä', 'æ', 'í', 'è', '£', '–', '—', 'ì', 'ì', 'î', 'ï',
    'ð', 'ñ', 'ò', 'ó', 'ô', 'õ', 'ö', '÷', 'ø', 'ù', 'ú', 'û', 'ü', 'ý', 'þ', 'ÿ');

   { upper case translation table for character set ISO 8859/1  Latin 1  }
   CPISO88591UCT: array[192..255] of char =
   ( #192, #193, #194, #195, #196, #197, #198, #199,
     #200, #201, #202, #203, #204, #205, #206, #207,
     #208, #209, #210, #211, #212, #213, #214, #215,
     #216, #217, #218, #219, #220, #221, #222, #223,
     #192, #193, #194, #195, #196, #197, #198, #199,
     #200, #201, #202, #203, #204, #205, #206, #207,
     #208, #209, #210, #211, #212, #213, #214, #247,
     #216, #217, #218, #219, #220, #221, #222, #89 );

   { lower case translation table for character set ISO 8859/1  Latin 1  }
   CPISO88591LCT: array[192..255] of char =
   ( #224, #225, #226, #227, #228, #229, #230, #231,
     #232, #233, #234, #235, #236, #237, #238, #239,
     #240, #241, #242, #243, #244, #245, #246, #215,
     #248, #249, #250, #251, #252, #253, #254, #223,
     #224, #225, #226, #227, #228, #229, #230, #231,
     #232, #233, #234, #235, #236, #237, #238, #239,
     #240, #241, #242, #243, #244, #245, #246, #247,
     #248, #249, #250, #251, #252, #253, #254, #255 );
*)

function sscanf(const s: string; const fmt : string;const Pointers : array of Pointer) : Integer;
var
  i,j,n,m : SizeInt;
  s1      : string;

  function GetInt : Integer;
  begin
      s1 := '';
      while (s[n] = ' ')  and (Length(s) > n) do
        inc(n);
      while (s[n] in ['0'..'9', '+', '-'])
        and (Length(s) >= n) do
        begin
          s1 := s1+s[n];
          inc(n);
        end;
      Result := Length(s1);
  end;


  function GetFloat : Integer;
  begin
      s1 := '';
      while (s[n] = ' ')  and (Length(s) > n) do
        inc(n);
      while (s[n] in ['0'..'9', '+', '-', '.', 'e', 'E'])
        and (Length(s) >= n) do
        begin
          s1 := s1+s[n];
          inc(n);
        end;
      Result := Length(s1);
  end;


  function GetString : Integer;
  begin
      s1 := '';
      while (s[n] = ' ')  and (Length(s) > n) do
        inc(n);
      while (s[n] <> ' ') and (Length(s) >= n) do
      begin
          s1 := s1+s[n];
          inc(n);
      end;
      Result := Length(s1);
  end;


  function ScanStr(c : Char) : Boolean;
  begin
      while (s[n] <> c) and (Length(s) > n) do
        inc(n);
      inc(n);
      If (n <= Length(s)) then
        Result := True
      else
        Result := False;
  end;


  function GetFmt : Integer;
  begin
      Result := -1;
      while true do
      begin

          while (fmt[m] = ' ') and (Length(fmt) > m) do
            inc(m);

          if (m >= Length(fmt)) then
            break;

          if (fmt[m] = '%') then
          begin
              inc(m);
              case fmt[m] of
                'd':
                  Result:=vtInteger;
                'f':
                  Result:=vtExtended;
                's':
                  Result:=vtString;
                'c':
                  Result:=vtChar;
//                else
//                  raise EFormatError.CreateFmt(SInvalidFormat,[fmt]);
// L505: COMMENTED OUT
              end;
              inc(m);
              break;
          end;

          if not(ScanStr(fmt[m])) then
            break;
          inc(m);
      end;
  end;


begin
  n := 1;
  m := 1;
  Result := 0;

  for i:=0 to High(Pointers) do
  begin
      j := GetFmt;
      case j of
        vtInteger :
          begin
            if GetInt>0 then
              begin
                plongint(Pointers[i])^:=StrToInt(s1);
                inc(Result);
              end
            else
              break;

          end;

        vtchar :
          begin
            if Length(s)>n then
              begin
                pchar(Pointers[i])^:=s[n];
                inc(n);
                inc(Result);
              end
            else
              break;

          end;

        vtExtended :
          begin
            if GetFloat>0 then
              begin
                pextended(Pointers[i])^:=StrToFloat(s1);
                inc(Result);
              end
            else
              break;
          end;

        vtString :
          begin
            if GetString > 0 then
              begin
                pansistring(Pointers[i])^:=s1;
                inc(Result);
              end
            else
              break;
          end;
        else
          break;
      end;
  end;
end;

{------------------------------------------------------------------------------}



{--COPY/PASTED FROM FPC SOURCES: fina.inc -------------------------------------}


(*
//L505: relies on oldlinux/dos which have initialization finalization.
function ExpandFileName (Const FileName : string): String;
Var
  S : String;
Begin
 S:=FileName;
 DoDirSeparators(S);
{$ifdef HasUnix}
  Result:=fexpand(S);
{$else}
  Result:=Dos.Fexpand(S);
{$endif}
end;

function ExpandUNCFileName (Const FileName : string): String;
begin
  Result:=ExpandFileName (FileName);
  //!! Here should follow code to replace the drive: part with UNC...
end;
{$endif}
*)



(* TODO: get working
Function GetFileHandle(var f : File):Longint;
begin
  result:=filerec(f).handle;
end;

Function GetFileHandle(var f : Text):Longint;
begin
  result:=textrec(f).handle;
end;
*)

{------------------------------------------------------------------------------}

(*// L505: COMMENTED OUT

{$ifdef fmtdebug}
Procedure Log (Const S: String);
begin
 Writeln (S);
end;
{$endif}

Procedure DoFormatError (ErrCode : Longint);
Var
  S : String;
begin
  //!! must be changed to contain format string...
  S:='';
  Case ErrCode of
   feInvalidFormat : raise EConvertError.Createfmt(SInvalidFormat,[s]);
   feMissingArgument : raise EConvertError.Createfmt(SArgumentMissing,[s]);
   feInvalidArgIndex : raise EConvertError.Createfmt(SInvalidArgIndex,[s]);
 end;
end;
*)

(*
 // L505: COMMENTED OUT
Function FloatToDateTime (Const Value : Extended) : TDateTime;
begin
  If (Value<MinDateTime) or (Value>MaxDateTime) then
    Raise EConvertError.CreateFmt (SInvalidDateTime,[Value]);
  Result:=Value;

end;


function TryFloatToCurr(const Value: Extended; var AResult: Currency): Boolean;
begin
  Result:=(Value>=MinCurrency) and (Value<=MaxCurrency);
  if Result then
    AResult := Value;
end;


function FloatToCurr(const Value: Extended): Currency;

begin
// L505: COMMENTED OUT
  {if not} TryFloatToCurr(Value, Result) {then}
//    Raise EConvertError.CreateFmt(SInvalidCurrency, [FloatToStr(Value)]);
end;
*)

(*
{$macro on}
{$define INFORMAT}
{$define TFormatString:=ansistring}
{$define TFormatChar:=char}

Function Format (Const Fmt : AnsiString; const Args : Array of const) : AnsiString;


{$undef TFormatString}
{$undef TFormatChar}
{$undef INFORMAT}
{$macro off}



Function FormatBuf (Var Buffer; BufLen : Cardinal;
                     Const Fmt; fmtLen : Cardinal;
                     Const Args : Array of const) : Cardinal;
Var S,F : String;
begin
  Setlength(F,fmtlen);
  if fmtlen > 0 then
    Move(fmt,F[1],fmtlen);
  S:=Format (F,Args);
  If Cardinal(Length(S))<Buflen then
    Result:=Length(S)
  else
    Result:=Buflen;
  Move(S[1],Buffer,Result);
end;


Procedure FmtStr(Var Res: String; Const Fmt : String; Const args: Array of const);
begin
  Res:=Format(fmt,Args);
end;

Function StrFmt(Buffer,Fmt : PChar; Const args: Array of const) : Pchar;
begin
  Buffer[FormatBuf(Buffer^,Maxint,Fmt^,strlen(fmt),args)]:=#0;
  Result:=Buffer;
end;

Function StrLFmt(Buffer : PCHar; Maxlen : Cardinal;Fmt : PChar; Const args: Array of const) : Pchar;
begin
  Buffer[FormatBuf(Buffer^,MaxLen,Fmt^,strlen(fmt),args)]:=#0;
  Result:=Buffer;
end;
*)

end.


(*


NOTE: below shared with strings unit - same whether short or ansistring

function strlen(p:pchar):sizeint;external name 'FPC_PCHAR_LENGTH';
function strcopy(dest,source : pchar) : pchar;
function strlcopy(dest,source : pchar;maxlen : SizeInt) : pchar;
function strecopy(dest,source : pchar) : pchar;
function strend(p : pchar) : pchar;
function strcat(dest,source : pchar) : pchar;
function strcomp(str1,str2 : pchar) : SizeInt;
function strlcomp(str1,str2 : pchar;l : SizeInt) : SizeInt;
function stricomp(str1,str2 : pchar) : SizeInt;
function strmove(dest,source : pchar;l : SizeInt) : pchar;
function strlcat(dest,source : pchar;l : SizeInt) : pchar;
function strscan(p : pchar;c : char) : pchar;
function strrscan(p : pchar;c : char) : pchar;
function strlower(p : pchar) : pchar;
function strupper(p : pchar) : pchar;
function strlicomp(str1,str2 : pchar;l : SizeInt) : SizeInt;
function strpos(str1,str2 : pchar) : pchar;
function strnew(p : pchar) : pchar;


NOTE: below different from strings unit - for ansistrings! or different behaviour

function StrPas(Str: PChar): string;
function StrPCopy(Dest: PChar; Source: string): PChar;
function StrPLCopy(Dest: PChar; Source: string; MaxLen: SizeUInt): PChar;
function StrAlloc(Size: cardinal): PChar;
function StrBufSize(Str: PChar): SizeUInt;
procedure StrDispose(Str: PChar);


*)
