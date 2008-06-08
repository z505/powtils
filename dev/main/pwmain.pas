(*******************************************************************************

                                POWTILS

********************************************************************************

--------------------------------------------------------------------------------
 Main Web Unit
--------------------------------------------------------------------------------
 The main unit for plain CGI programs. 

--------------------------------------------------------------------------------
 Authors/Credits
--------------------------------------------------------------------------------
 Trustmaster (Vladimir Sibirov), L505 (Lars Olson), TonyH (Anthony Henry)
 File is copyright to above authors. Legal: see Artistic License.
 Use SVN logs for minor change notes.

--------------------------------------------------------------------------------
 Verbose Debugging 
--------------------------------------------------------------------------------
 Many procedures in this unit start with {b} and end with {e}. This means debug 
 info is hidden to the right of those markers past the 80 col marker. This 
 keeps code to the left of the 80 column marker easy to read, without IFDEF's 
 causing ugly line noise in the algorithms. 
  
 Debugging conventions are as follows in major procedures of this unit:
   SomeFunc_B;  --> Begin of SomeFunc
   SomeFunc E;  --> End of SomeFunc
   SomeFunc_Xn; --> Early Exit via Exit or Label. "n" is an optional number 

 Why? The debug procedures monitor the *entire flow control* of this unit. It 
 is optional, and not linked in unless developer uses "dbug_on" define. This 
 allows you to debug a web program extensively.

 Note: Other major Powtils units should use this too, for consistency.

--------------------------------------------------------------------------------
 Special Defines 
--------------------------------------------------------------------------------
 Below defines customize behavior. Save exe size by leaving all undefined.
  dbug_on      -> detailed custom debugging if you assign debugln:= @myproc;
  GZIP_ON      -> html compression saves bandwidth, output_compression and  output_buffering must also be enabled in config for gzip to have any effect. Gzip is harder to debug at the command line though: you will see a bunch of crap ;) It also takes more CPU power.. but saves the bandwidth bill, so it is up to you.
  PWUDEBUG     -> text log file debugging, only use on localhost single visitor testing since it multiple visitors cannot write simutaneously to the same log file
  EXTRA_SECURE -> check overflows, range errors (recommended)
  SYSUTILS_ON  -> compactsysutils may cause troubles on 64 bit or other machines. If you have problems then you can define this to use Sysutils

 Using $DEFINE in this unit is not global for all units, so instead use 
 delphi project options or fpc -d option for global define across units.

 Delete all .a/.o/.ppu files in *all* directories (DELP tool) otherwise
 defines will not have effect in many cases.

********************************************************************************)

unit pwmain; {$ifdef FPC}{$GOTO ON} {$NOTES ON} {$NOTE USING STATIC WEB UNIT}{$endif}

{$I defines1.inc}

{$J+} // persistent local scope vars

interface
uses pwerrors,  pwtypes;

// for setcookie default behavior.
const FUTURE_COOKIE  = 'Mon, 01 Dec 2099 12:00:00 GMT'; //must be many years ahead
      EXPIRED_COOKIE = 'Mon, 01 Jan 2001 12:00:00 GMT'; //must be many years behind
      CASE_SENSITIVE = false;
      CASE_IGNORE = true;
      FILTER_ON = true;
      FILTER_OFF = false;
      HTM_BREAK = '<br />';
      SECURE_OFF = 0;
      SECURE_ON = 2;

// Powtils version
{$i version.inc}

type TFilterFunc = function(const s: astr): astr;

{ todo }
//type TSecurity = (sUnsafe, sTrim); 
// for GetCgiVar_S function security parameter


{--- Public Functions --------------------------------------------------------}

procedure init;

procedure offreadln;

function Lcase(const s: astr): astr;
function Ucase(const s: astr): astr;

{ Posted Variable Functions } 
// note: function names changed to 'post' in 1.7.X than old CGI
function countPostVars: longword;
function getPostVar(const name: astr): astr; overload;
function getPostVar(const name: astr; vfilter: TFilterFunc): astr; overload;
function getPostVar_S(const name: astr; security: byte): astr;
function getPostVar_SF(const name: astr; security: byte): astr;
function getPostVarAsFloat(const name: astr): double;
function getPostVarAsInt(const name: astr): int32;
function getPostVar_SafeHTML(const name: astr): astr;
function fetchPostVarName(idx: int32): astr;
function fetchPostVarVal(idx: int32): astr; overload;
function fetchPostVarVal(idx: int32; vfilter: TFilterFunc): astr; overload;
function fetchPostVarName_S(idx: int32; security: byte): astr;
function fetchPostVarVal_S(idx: int32; security: byte): astr;
function isPostVar(const name: astr): boo;

{ TODO get gGetPost var based on a valid set of characters such as A..Z, 1..9, a..z
  type TCharSet = set of char;
      function GetCgiVar(const input: astr; CharSet: TCharSet): boo;
  Use the pwstrfilter unit, with Alpha, numeric, alphanumeric sets     }

{ Abstract for url/post/get vars, macrovars, and cookies }
function countAny: longword;
function getAny(const name: astr): astr; overload;
function getAny(const name: astr; vfilter: TFilterFunc): astr; overload;
function getAny_S(const name: astr; security: byte): astr;
function getAnyAsFloat(const name: astr): double;
function getAnyAsInt(const name: astr): int32;
function isAny(const name: astr): byte; overload;

{ Cookie Functions }
function countCookies: longword;
function fetchCookieName(idx: int32): astr;
function fetchCookieVal(idx: int32): astr; overload;
function fetchCookieVal(idx: int32; cfilter: TFilterFunc): astr; overload;
function getCookie(const name: astr): astr; overload;
function getCookie(const name: astr; cfilter: TFilterFunc): astr; overload;
function getCookieAsFloat(const name: astr): double;
function getCookieAsInt(const name: astr): int32;
function isCookie(const name: astr): boo;
function setCookie(const name, value: astr): boo;
function setCookieAsFloat(const name: astr; value: double): boo;
function setCookieAsInt(const name: astr; value: int32): boo;
function setCookieEx(const name, value, path, domain, expiry: astr): boo;
function setCookieAsFloatEx(const name: astr; value: double; const path, domain, expiry: astr): boo;
function setCookieAsIntEx(const name: astr; value: int32; const path, domain, expiry: astr): boo;
function unsetCookie(const name: astr): boo;
function unsetCookieEx(const name, path, domain: astr): boo;

{ Environment Variable Functions } 
// moved to pwEnvVar.pas

{ Filtering Functions }
function filterHtml(const input: astr): astr;
function filterHtml_S(const input: astr; security: byte): astr;
function trimBadChars(const input: astr): astr;
function trimBadFile(const input: astr): astr;
function trimBadDir(const input: astr): astr;
function trimBad_S(const input: astr; security: byte): astr;

{ TODO
    function isBadFile(const input: astr): boo;
    function isBadDir(const input: astr): boo; }

{ Header Functions }
function countHeaders: longword;
function fetchHeaderName(idx: int32): astr;
function fetchHeaderVal(idx: int32): astr;
function getHeader(const name: astr): astr;
function isHeader(const name: astr): boo;
function setHeader(const name, value: astr): boo;
function unsetHeader(const name: astr): boo;
function putHeader(const header: astr): boo;

{ Output/Write Functions }
procedure out(const s: astr);
procedure outln(const s: astr); overload;
procedure outln; overload;
procedure outa(args: array of const);
procedure outlna(args: array of const);

procedure outf(const s: astr);
procedure outf(const s: astr; vfilter: TFilterFunc); overload;

procedure outff(const s: astr); 

procedure outlnf(const s: astr); overload;
procedure outlnf(const s: astr; vfilter: TFilterFunc); overload;

procedure outlnff(const s: astr);

function fileOut(const fname: astr): errcode;
function resourceOut(const fname: astr): errcode;
procedure bufferOut(Const buff; len: LongWord);

function templateOut(const fname: astr; HtmlFilter: boo): errcode; overload;
function templateOut(const fname: astr): errcode; overload;
function templateOut(const fname: astr; vfilter: TFilterFunc): errcode; overload;
function templateOut1(const fname: astr; HtmlFilter: boo): errcode; overload;
function templateRaw(const fname: astr): errcode;

{ fmt stands for format, to not conflict with sysutils Format() function }
function fmt(const s: astr): astr; overload;
function fmt(const s: astr; vfilter: TFilterFunc): astr; overload;

function fmtFilter(const s: astr): astr;

function fmt_SF(const s: astr; usefilter: boo; FilterSecurity, Trimsecurity: byte; vfilter: TFilterFunc): astr;


{ RTI Functions }
function countRtiVars: longword;
function fetchRtiName(idx: int32): astr;
function fetchRtiVal(idx: int32): astr;
function getRti(const name: astr): astr;
function getRtiAsFloat(const name: astr): double;
function getRtiAsInt(const name: astr): int32;
function isRti(const name: astr): boo;
procedure setRTI(const name, value: astr); 

{ Upload File Functions }
function fetchUpfileName(idx: int32): astr;
function getUpfileName(const name: astr): astr;
function getUpfileSize(const name: astr): int32;
function getUpfileType(const name: astr): astr;
function countUpfiles: longword;
function isUpfile(const name: astr): boo;
function saveUpfile(const name, fname: astr): boo;

{ Web Variable Functions }
function countVars: longword;
function fetchVarName(idx: int32): astr;
function fetchVarVal(idx: int32): astr; overload;
function fetchVarVal(idx: int32; vfilter: TFilterFunc): astr; overload;
function getVar(const name: astr): astr; overload;
function getVar(const name: astr; vfilter: TFilterFunc): astr; overload;
function getVar_S(const name: astr; security: byte): astr;
function getVarAsFloat(const name: astr): double;
function getVarAsInt(const name: astr): int32;
procedure setVar(const name, value: astr);
procedure setVarAsFloat(const name: astr; value: double);
procedure setVarAsInt(const name: astr; value: int32);
function isVar(const name: astr): boo;
procedure unsetVar(const name: astr);

{ Utility/Tools Functions }
function lineEndToBR(const s: astr): astr;
function randomStr(len: int32): astr;
function xorCrypt(const s: astr; key: byte): astr;

{ Config Functions }
function countCfgVars: longword;
function fetchCfgVarName(idx: int32): astr;
function fetchCfgVarVal(idx: int32): astr;
function isCfgVar(const name: astr): boo;
function setCfgVar(const name, value: astr): boo;
function getCfgVar(const name: astr): astr;

{ Error Functions }
procedure throwErr(const s: astr);
procedure throwWarn(const s: astr);  

// flags
var headers_sent: boo = false;

// END OF PUBLIC FUNCTION DECLARATIONS
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------}
{ FOR PLUGIN UNITS - TYPICAL USERS CAN IGNORE THIS PUBLIC INTERFACE           }
{-----------------------------------------------------------------------------}
{ Advanced developers use this interface to extend Powtils. i.e. custom session 
  units (mysql, firebird addons instead of SDS). Any functions in this section 
  are not for a typical developer
}

type
  // variable structure
  TWebVar = record name, value: astr; end;
  TWebVars = array of TWebVar;

  TSessUpdate = function: boo;

var
  // set this when using session plugin unit
  CustomSessUnitInit: procedure = nil;
  CustomSessUpdate: TSessUpdate = nil;
  // set this when using config plugin unit
  CustomCfgUnitInit: procedure = nil;

{ functions prefixed with "i" are internal but publically available for addons}
function iUpdateWebVar(var w: TWebVars; const name, value: astr; upcased: boo): boo;
function iAddWebCfgVar(const name, value: astr): boo;
procedure iAddWebVar(var w: TWebVars; const name, value: astr);
procedure iSetRTI(const name, value: astr);
function iCustomSessUnitSet: boo;
function iCustomCfgUnitSet: boo;

// END OF INTERNAL DECLARATIONS
{-----------------------------------------------------------------------------}

{=============================================================================}
 implementation
{=============================================================================}

uses
 {$ifdef WINDOWS}windows,{$endif} {$ifdef UNIX}baseunix,{$endif}
 {$ifdef SYSUTILS_ON}sysutils{$else}compactsysutils{$endif},
  pwstrutil,
 {$ifdef gzip_on}pwobjbuff,{$endif} // output buffer w/built in gzip
  pwnative_out, // simple writeln
  pwenvvar, pwfileutil, pwsubstr, pwurlenc, pwmimetypes, strwrap1, 
  pwdebugplugin;

{$ifdef dbug_on}
  // var debugt: text; // Debug output file (for localhost single visitor testing only!)
  var debugt: int32; 
  procedure dbugln(s: astr); begin pwdebugplugin.dbugln(debugt, s); end;
{$endif}

{ flags } 
var plugin_init_called: boo = false;    // plugin initialized flag
    cook_initialized: boo = false;      // cookie flag
    error_reporting, error_halt: boo;   // error flags
   {$ifdef gzip_on}out_buffering, out_compression: boo;{$endif} 
 
type
  // file structure
  TWebUpFile = record 
    name, filename, data, content_type: astr; 
    size: int32; 
  end;

  // uploaded files
  TWebUpFiles = array of TWebUpFile;
  // Line type for Multipart/Form-Data handling functions
  TMp_Line = array[1..6] of astr;
  // Multipart/Form-Data storage
  TMp_Form = array of astr;
  PMp_Form = ^TMp_Form;
  // type safety to prevent from being sent into the wrong AddXyz() functions
  TWebCfgVar = type TWebVar;
  // multiple config name=value pairs
  TWebCfgVars = array of TWebCfgVar;

// global variables prefixed with g
var
  gGetPost,  // GET/POST vars
  gCook,     // cookie data
  gHdr,      // headers
  gRti,      // run time information
  gVar       // macro $endif set by user via SetVar
      : TWebVars;

  gConf   : TWebCfgVars; // config data
  gUpfiles: TWebUpFiles; // file uploads storage

 {$ifdef gzip_on}outbuff: PSmartBuffer = nil;{$endif}

const
  // lower case string constants
  L_OUTPUT_BUFFERING   = 'output_buffering';
  L_OUTPUT_COMPRESSION = 'output_compression';
  L_HEADER_CHARSET     = 'header_charset';
  L_ERROR_REPORTING    = 'error_reporting';
  L_ERROR_HALT         = 'error_halt';
  L_UPLOAD_MAX_SIZE    = 'upload_max_size';
  L_SESSION_PATH       = 'session_path';
  L_SESSION_LIFE_TIME  = 'session_life_time';

  // upper case string constants
  U_HEADERS_SENT = 'HEADERS_SENT';
  U_ERRORS = 'ERRORS';
  U_FALSE = 'FALSE';

{------------------------------------------------------------------------------}
{--- SYSAPI FUNCTIONS ---------------------------------------------------------}
{------------------------------------------------------------------------------}
(* // obsolete, no need when using baseunix.pp
{$ifdef UNIX}
var
  environ: ppchar; cvar; external;

function getenv(const name: PChar): PChar; cdecl; external 'c' name 'getenv';
function setenv(const name, value: pchar; replace: int32): int32; cdecl; external 'c' name 'setenv';
function unsetenv(const name: pchar): int32; cdecl; external 'c' name 'unsetenv';
{$endif}  *)
{------------------------------------------------------------------------------}


{------------------------------------------------------------------------------}
{--- PRIVATE FUNCTIONS/PROCEDURES ---------------------------------------------}
{------------------------------------------------------------------------------}

// delphi issues
{$I delphisystemcompat.inc}

{$ifdef dbug_on} // USER MUST ASSIGN HIS OWN DEBUG PROC UNLESS PWUDEBUG DEFINED
  procedure dummydebug(s: astr); begin end;
{$endif dbug_on}

{ allows a READLN to be called in offline console, but not on the web. Use for 
  holding console open at the end of program }
procedure OffReadln; 
begin if SERV.DocRoot() = '' then begin flush(output); readln; end; 
end;

procedure ThrowNoFileErr(const fname: astr);
begin throwErr('reading file: ' + fname);
end;           

{ Send HTTP headers }
function SendHeaders: boo;
var i: int32;
{b}                                                                             begin {$ifdef dbug_on}SendHeaders_B;{$endif} 
  result:= false;
  // only send once
  if headers_sent then exit;
  // kill if mandatory Init() not called
  if not plugin_init_called then errWithHeader(MISSING_INIT_CALL_OR_UNIT);
  if {$ifndef FPC}@{$endif}customSessUpdate <> nil then customSessUpdate;
  // write headers to stdout
  if length(gHdr) > 0 then for i:= low(gHdr) to high(gHdr) do
    nativeWriteLn(gHdr[i].name + ': ' + gHdr[i].value);

  nativeWriteLn;
  // Update RTI
  headers_sent:= true;
  setRti(U_HEADERS_SENT, 'TRUE');
  result:= true;
{e}                                                                             {$ifdef dbug_on}SendHeaders E{$endif} end;


type TThrowType = (ttError, ttWarn);

{ Abstract for throwerr and throwwarn }
procedure ThrowMsg(const msg: astr; const style: TThrowType);
var i: int32;
    s: astr;
{b}                                                                             begin{$ifdef dbug_on}ThrowMsg_B;{$endif}
  // Increase ERRORS RTI
  i:= GetRtiAsInt('ERRORS');
  inc(i);
  str(i, s);
  iSetRTI('ERRORS', s);
  if not error_reporting then exit; 
  // Disable content encoding
  if IsHeader('Content-Encoding') then UnsetHeader('Content-Encoding');
  // Send headers
  if not headers_sent then SendHeaders;
  {$ifdef gzip_on} if out_buffering then FlushBuf;{$endif}
  // spit error to screen
  outln(HTM_BREAK);
  case style of
    ttError: outln('ERR: ' + Msg);
    ttWarn: outln('WARNING: ' + Msg);
  end;
  outln(HTM_BREAK);
  if error_halt then halt(0);
{e}                                                                             {$ifdef dbug_on}ThrowMsg_E;{$endif}end;

{ init default http headers on successful startup }
procedure InitHeaders;
{b}                                                                             begin{$ifdef dbug_on}InitHeaders_B;{$endif}
  setlength(gHdr, 2);
  gHdr[0].name:= 'X-Powered-By'; 
  gHdr[0].value:= 'Powtils/' + PWU_VERSION;
  gHdr[1].name:= 'Content-Type'; 
  gHdr[1].value:= 'text/html; charset=' + GetCfgVar(L_HEADER_CHARSET);
 {$ifdef gzip_on}
  if (out_buffering) and (out_compression)
    and substrexists(GetEnvVar('HTTP_ACCEPT_ENCODING'), 'gzip') 
  then begin
    setlength(gHdr,3); 
    gHdr[2].name:='Content-Encoding'; 
    gHdr[2].value:='gzip';
  end else
    out_compression:= false;
 {$endif}
{e}                                                                             {$ifdef dbug_on}InitHeaders_E;{$endif}end;

procedure InitBufAndHeaders;
begin
 {$ifdef gzip_on}
  if (out_buffering) and (out_compression) then if OutBuff = nil then
    OutBuff := new(PSmartBuffer, DynamicBuffer(8192));
 {$endif}
  InitHeaders;
end;

{ abstract }
function GetTwebvarAsFloat(const w: TWebVars; const name: astr): double;
var i: int32;
begin
  result:= 0.0;
  if length(w) = 0 then exit;
  for i:= low(w) to high(w) do if w[i].name = name then begin
    val(w[i].value, result);
    break;
  end;
end;

{ abstract }
function GetTwebvarAsInt(const w: TWebVars; const name: astr): int32;
var i: int32;
begin
  result:= 0;
  if length(w) = 0 then exit;
  for i:= low(w) to high(w) do if w[i].name = name then begin
    val(w[i].value, result);
    break;
  end;
end;

{ same as outLn but does not check for headers }
procedure outLnNoHeaders(const s: astr);
begin
 {$ifdef gzip_on}
  if out_buffering then begin
    // Append str to buffer
    OutBuff^.AppendStr(S);
    OutBuff^.AppendLineFeed;
  end else
 {$endif}
    NativeWriteLn(s);
end;

{ append single new value to WebVariables array i.e. gHdr[],  gCook[] }
procedure addWebVar(var w: TWebVars; const name, value: astr);
var len: int32;
begin
  len:= length(w); setlength(w, len+1);
  w[len].name:= name; w[len].value:= value;
end;

function isGzipFlag(s: astr): boo;
begin
  result:= false;
  if (Lcase(s) = L_OUTPUT_COMPRESSION) or (Lcase(s) = L_OUTPUT_BUFFERING) 
    then result:= true;
end;

function cfgVarToBool(const s: astr): boo;
begin if getCfgVar(s) = 'on' then result:= true else result:= false;
end;

{ merge program flags to config settings }
procedure mergeFlagsToCfg;
begin
 {$ifdef gzip_on}
  out_buffering:= CfgVarToBool(L_OUTPUT_BUFFERING);
  out_compression:= CfgVarToBool(L_OUTPUT_COMPRESSION);
 {$endif}
  error_reporting:= CfgVarToBool(L_ERROR_REPORTING);
  error_halt:= CfgVarToBool(L_ERROR_HALT);
end;

{ append single new value to WebConfiguration array i.e. gConf[] }
function addWebCfgVar(const name, value: astr): boo;
begin
  result:= false;
 {$ifndef gzip_on} // not applicable if gzip off
  if IsGzipFlag(name) then exit;
 {$endif}
  AddWebVar(TWebVars(gConf), name, value);
  MergeFlagsToCfg;
end;

{ updates value if name exists in list, returns false if no update }
function updateWebVar(var w: TWebVars; const name, value: astr; upcased: boo): boo;
var i: int32;
begin
  result:= false;
  if length(w) < 1 then exit;
  for i:= low(w) to high(w) do 
  begin
    // case Insensitive NAME
    if upcased then begin
      if Ucase(w[i].name) = Ucase(name) then begin 
        w[i].value:= value;
        result:= true;
      end;
    end else // case Sensitive NAME
      if w[i].name = name then begin 
        w[i].value:= value; 
        result:= true;
      end;
  end;
end;

{ updates value if name exists in list, returns false if no update }
function updateWebCfgVar(const name, value: astr; upcased: boo): boo;
begin
  result:= false;
  {$ifndef gzip_on}if IsGzipFlag(name) then exit;{$endif}
  result:= UpdateWebVar(TWebVars(gConf), name, value, upcased);
  if result = true then MergeFlagsToCfg;
end;

function iAddWebCfgVar(const name, value: astr): boo;
begin result:= AddWebCfgVar(name, value);
end;

function iUpdateWebVar(var w: TWebVars; const name, value: astr; upcased: boo): boo;
begin result:= UpdateWebVar(w, name, value, upcased);
end;

(*
{ updates value if name exists in list, adds one if it doesn't }
procedure PutWebVar(var w: TWebVars; const name, value: astr; upcased: boo);
var updated: boo;
begin
  updated:= UpdateWebVar(w,  name, value, upcased);
  if not updated then AddWebVar(webv, name, value);
end;
*)

{ updates value if name exists in list, adds one if it doesn't }
procedure putWebCfgVar(const name, value: astr; upcased: boo);
var updated: boo; 
{b}                                                                             begin {$ifdef dbug_on}PutWebCfgVar_B;{$endif}
  updated:= UpdateWebCfgVar(name, value, upcased);
  if not updated then AddWebCfgVar(name, value);
{e}                                                                             {$ifdef dbug_on}PutWebCfgVar_E;{$endif}end;

{ init default RTI definitions on startup }
procedure initRti; 
{b}                                                                             begin {$ifdef dbug_on}InitRTI_B;{$endif}
  setlength(gRti, 2);
  gRti[0].name:=  U_HEADERS_SENT; gRti[0].value:= U_FALSE;
  gRti[1].name:=  U_ERRORS; gRti[1].value:= '0';
{e}                                                                             {$ifdef dbug_on}InitRTI_E;{$endif}end;

{ Defaults if not using config file plugin unit }
procedure defaultCfgInit; 
{b}                                                                             begin {$ifdef dbug_on}DefaultCfgInit_B;{$endif}
  AddWebCfgVar(L_HEADER_CHARSET, 'iso-8859-1');
  AddWebCfgVar(L_ERROR_REPORTING, 'on');
  AddWebCfgVar(L_ERROR_HALT, 'off');
  AddWebCfgVar(L_UPLOAD_MAX_SIZE, '20');
 {$ifdef gzip_on}
  AddWebCfgVar(L_OUTPUT_BUFFERING, 'off');
  AddWebCfgVar(L_OUTPUT_COMPRESSION, 'off');
 {$endif}
{e}                                                                             {$ifdef dbug_on}DefaultCfgInit_E;{$endif}end;

procedure initCook;

  procedure putCookies;

    { Dump into gCook[] var }
    procedure putCookieVars(const data: astr);
    var i, len: int32; lex, name, value: astr;

     procedure addToLex;
     begin 
       setlength(lex, length(lex) + 1); lex[length(lex)]:= data[i]; inc(i);
     end;

    begin                                                                       
      // Init
      i:= 1;
      len:= length(data);
      if data[1] = '\' then inc(i);
      // Parse out
      while (i <= len) do
      begin
        // Get name
        lex:= '';
        while (i <= len) and (data[i] <> '=') do AddToLex;;
        name:= UrlDecode(lex);
        inc(i);
        // Get value
        lex:= '';
        while (i <= len) and (data[i] <> ';') do AddToLex;
        value:= UrlDecode(lex);
        AddWebVar(gCook, name, value);
        inc(i);
        // Ignore spaces
        while (i <= len) and (data[i] = ' ') do inc(i);
      end;
    end;

  var hcook: astr;
  begin
    hcook:= '';
    if IsEnvVar('HTTP_COOKIE') then begin
      hcook:= GetEnvVar('HTTP_COOKIE');
      PutCookieVars(hcook);
    end;
  end;

{b}                                                                             begin {$ifdef dbug_on}InitCook_B;{$endif}
  // only once
  if cook_initialized then exit;
  PutCookies;
  cook_initialized:= true;
{e}                                                                             {$ifdef dbug_on}InitCook_E;{$endif} end;

{$ifdef gzip_on} // flush output buffer 
function FlushBuf: boo;
{b}                                                                            begin{$ifdef dbug_on}FlushBuf_B;{$endif}
  result:= false;
  if not headers_sent then SendHeaders;
  OutBuff^.Flush;
  result:= true;
{e}                                                                            {$ifdef dbug_on}FlushBuf_E;{$endif}end;
{$endif}


{ Append into gGetPost[] var }
function AppendCgiVars(const data: astr): boo;
var
  i,
  len,
  cnt: int32;
  lex: astr;

  procedure AddToLex;
  begin
    setlength(lex, length(lex) + 1);
    lex[length(lex)]:= data[i];
    inc(i);
  end;
{b}                                                                             begin {$ifdef dbug_on}AppendCgiVars_B;{$endif}
  // Init
  result:= false; i:= 1; cnt:= length(gGetPost); len:= length(data);
  if len = 0 then begin {$ifdef dbug_on}AppendCgiVars_X1;{$endif}
                        exit;
                  end;
  if data[1] = '\' then inc(i);
  // Parse 
  while (i <= len) do
  begin
    // new item
    setlength(gGetPost, cnt+1);
    // get name
    lex:= '';
    while (i <= len) and (data[i] <> '=') do addToLex;
    gGetPost[cnt].name:= urlDecode(lex);
    inc(i);
    // Get value
    lex:= '';
    while (i <= len) and (data[i] <> '&') do AddToLex;
    gGetPost[cnt].value:= urlDecode(lex);
    inc(i); inc(cnt);
  end;
  result:= true;
{e}                                                                             {$ifdef dbug_on}AppendCgiVars_E;{$endif} end;


{ Multipart: Dump $endif from multipart/form-data into gGetPost and UpFile, splits 
  form into items }
procedure MP_FormSplit(var data: PString; const boundary: astr; var form: PMp_Form);
var separator: astr;
    ptr, len, len2: int32;
{b}                                                                             begin {$ifdef dbug_on}MP_FormSplit_B;{$endif}
  separator:= '--' + boundary + #13 + #10;
  len2:= length(separator);
  // Cut off last boundary
  len:= substrPos(data^, '--' + boundary + '--');
  data^:= copy(data^, 1, len-1);
  // Cut off first boundary
  delete(data^, 1, len2);
  while len > 0 do
  begin
    len:= length(data^);
    ptr:= substrPos(data^, separator);
    if ptr <> 0 then begin
      // Not last item
      setlength(form^, length(form^) + 1);
      form^[high(form^)]:= copy(data^, 1, ptr - 2);
      // Cut this item and next boundary
      delete(data^, 1, ptr + len2 - 1);
    end else begin
      // Last item
      setlength(form^, length(form^) + 1);
      form^[high(form^)]:= copy(data^, 1, len-1);
      break;
    end;
  end;
{e}                                                                             {$ifdef dbug_on}MP_FormSplit_E;{$endif}end;

{ Multipart: Extracts current line beginning from ptr and ending with #13#10 }
function MP_getLine(data: PString; var ptr: int32): astr;
var s: astr; 
{b}                                                                             begin {$ifdef dbug_on}MP_GetLine_B;{$endif}
  result:= '';                                                                  
  if data = nil then begin 
    {$ifdef dbug_on}MP_getLine_data_nil_X1{$endif}
    exit;
  end;
  
  repeat
    s:= copy(data^, ptr, 1);
    if (s <> #13) and (s <> #10) then result:= result + s;
    inc(ptr);
  until (s = #13) or (s = #10);

  inc(ptr);
{e}                                                                             {$ifdef dbug_on}MP_GetLine_E;{$endif}end;


{ Multipart: splits string by space. Max. result = 6 strings. }
function MP_splitLine(line: astr): TMp_Line;
var i, cnt, elem, len: int32;
    s: astr;
    quoted: boo;
{b}                                                                             begin{$ifdef dbug_on}MP_SplitLine_B;{$endif}
  for i:= 1 to 6 do result[i]:= '';
  elem:= 1;
  len:= length(line);
  quoted:= false;
  for cnt:= 1 to len do
  begin
    s:= copy(line, cnt, 1);
    if (s='"') then quoted:= not quoted; // on/off - track whether inside quotes or not
    if (s<>' ') and (s<>'=') and (s<>';') and (s<>'"') and (s<>':') then
      result[elem]:= result[elem] + s;
    if ((s=' ') or (s=';') or (s=':') or (s='=')) and quoted then 
      result[elem]:= result[elem] + s;
    if ((s=';') or (s='=') or (s=':')) and (not quoted) then
      inc(elem);
  end;
{e}                                                                             {$ifdef dbug_on}MP_SplitLine_E;{$endif}end;

{ Multipart: extracts data boundary from content-type string }
function MP_getBoundary(const content_type: astr): astr;
var len: int32;
{b}                                                                             begin{$ifdef dbug_on}MP_GetBoundry_B;{$endif}
  len:= substrpos(Content_Type, '=');
  result:= copy(content_type, len + 1, length(content_type)-len);
  if substrpos(result,'"') = 1 then result:= copy(result,2,length(result) - 2);
{e}                                                                             {$ifdef dbug_on}MP_GetBoundry_E;{$endif}end;

{ Multipart: put get/post vars }
procedure MP_putGpVars(data: PString; const content_type: astr);
var cnt, ptr, tmp, len, dpos: int32;
    buff, boundary: astr;
    line: TMp_Line;
    form: PMp_Form;
    UpIdx: int32; // current index to UpFile array
{b}                                                                             begin {$ifdef dbug_on}MP_PutGpVars_B;{$endif}
  New(form);
  boundary:= MP_getBoundary(content_type);
  MP_formSplit(data, boundary, form);
  for cnt:= low(form^) to high(form^) do
  begin
    ptr:= 1;
    len:= length(form^[cnt]);
    dpos:= substrPos(form^[cnt], #13 + #10 + #13 + #10) + 4;
    // Getting first line
    buff:= MP_getLine(@(form^[cnt]), ptr);
    // Splitting into words
    line:= MP_splitLine(buff);
    // Is it file or variable?
    if substrpos(buff, 'filename') <> 0 then
    begin
      // It is a file
      setlength(gUpFiles, length(gUpFiles) + 1);
      UpIdx:= high(gUpFiles);
      gUpFiles[UpIdx].name:= line[4];
      gUpFiles[UpIdx].filename:= line[6];
      {$ifdef dbug_on}dbugln('Upload name var: '+gUpFiles[UpIdx].name);
                      dbugln('Upload filename: '+gUpFiles[UpIdx].filename);
      {$endif}

      // Getting content type
      buff:= MP_getLine(@(form^[cnt]), ptr);
      line:= MP_splitLine(buff);
      gUpFiles[upIdx].content_type:= line[2];
      // Getting value till the end
      gUpFiles[upIdx].size:= len - dpos;
      
      // *** Make sure we have enough room to use MOVE *** (equivalent to GetMem);
      setlength(gUpFiles[upIdx].data, gUpFiles[UpIdx].size);
      
      // NO LONGER NEEDED *** gUpFiles[UpIdx].data:= copy(form^[cnt], dpos, gUpFiles[UpIdx].size);
       // ** Tonys Code     
       // *** Move is Much faster then copy especially for large strings.
      if gUpFiles[UpIdx].size > 0 then
        move(form^[cnt][dpos], gUpFiles[UpIdx].Data[1], gUpFiles[UpIdx].size);
    end else // It is a variable
    begin
      // Getting value till the end
      tmp:= len - dpos;
      AddWebVar(gGetPost, line[4], copy(form^[cnt], dpos, tmp));
    end;
  end;

  dispose(form);
{e}                                                                             {$ifdef dbug_on}MP_PutGpVars_E;{$endif}end;


{ Get/set url query string, get/post etc. }
procedure initWebData;

  procedure PutQueryString;
  var s: astr;
  begin
    s:= '';
    s:= GetEnvVar('QUERY_STRING');
    AppendCgiVars(s);
  end;

var
  upl_max_size, cont_len, cnt: int32;
  method, ctype, data: astr;
{b}                                                                             begin{$ifdef dbug_on}InitWebData_B;{$endif}
  // First get method data
  method:= GetEnvVar('REQUEST_METHOD');
  if method = 'POST' then
  begin
    // url variables persist even in a POST
    PutQueryString;
    // now retrieve and store POST data from stdin
    data:= '';
    val(GetCfgVar(L_UPLOAD_MAX_SIZE), upl_max_size);
    upl_max_size:= upl_max_size * 1048576;
    val(GetEnvVar('CONTENT_LENGTH'), cont_len);
    if cont_len > upl_max_size then cont_len:= upl_max_size;
    setlength(data, cont_len);
    for cnt:= 1 to cont_len do read(data[cnt]);
    // Depending on content type
    ctype:= GetEnvVar('CONTENT_TYPE');
    if substrpos(Lcase(ctype), 'application/x-www-form-urlencoded') > 0 then
      AppendCGIVars(data)
    else begin
      if substrpos(Lcase(ctype), 'multipart/form-data') > 0 then
        MP_PutGpVars(@data, ctype);
    end;
  end;

  if method = 'GET' then PutQueryString;
{e}                                                                             {$ifdef dbug_on}InitWebData_E;{$endif}end;


{ Sets Run Time Information variable }
procedure SetRti(const name, value: astr);
{b}                                                                             begin{$ifdef dbug_on}SetRti_B;{$endif}
  if not UpdateWebVar(gRti, name, value, CASE_IGNORE) 
    then AddWebVar(gRti, name, value);
{e}                                                                             {$ifdef dbug_on}SetRti_E;{$endif}end;


{------------------------------------------------------------------------------}


{------------------------------------------------------------------------------}
{--- PUBLIC FUNCTIONS/PROCEDURES ----------------------------------------------}
{------------------------------------------------------------------------------}

{..............................................................................}

  { iFunction (prefixed with 'i') means internal. They are for addon/extension 
    units. PUBLIC, but regular API users  SHOULD NOT USE THEM  }

  procedure iAddWebVar(var w: TWebVars; const name, value: astr);
  begin AddWebVar(w, name, value);
  end;

  procedure iSetRTI(const name, value: astr); 
  begin SetRTI(name, value); 
  end;

  { Check config plugin unit is setup properly }
  function iCustomCfgUnitSet: boo;
  begin 
    result:= false; 
    if assigned(CustomCfgUnitInit) then result:= true;
  end;

  { Check session plugin unit is setup properly }
  function iCustomSessUnitSet: boo;
  begin
    result:= false;
    if (assigned(CustomSessUnitInit)) and (assigned(CustomSessUpdate)) 
      then result:= true;
  end;

{..............................................................................}

{ wrapper for fpc/delphi lowercase function }
function Lcase(const s: astr): astr;
begin
 {$ifdef FPC}result:= system.lowercase(s);
 {$else}     result:= lowercase(s);
 {$endif}
end;

{ wrapper for fpc/delphi uppercase function }
function Ucase(const s: astr): astr;
begin
 {$ifdef FPC}result:= system.upcase(s);
 {$else}     result:= uppercase(s); 
 {$endif}
end;

{ Return number of config variables }
function CountCfgVars: longword; 
begin result:= length(gConf); 
end;

{ Indexed access to config variable }
function FetchCfgVarName(idx: int32): astr;
begin
  if (idx >= 0) and (length(gConf) > 0) 
    then result:= gConf[idx].name else result:= '';
end;

{ Indexed access to configuration variable }
function FetchCfgVarVal(idx: int32): astr;
begin
  if (idx >= 0) and (length(gConf) > 0) 
    then result:= gConf[idx].value else result:= '';
end;


{ Dynamically sets config name and value }
function SetCfgVar(const name, value: astr): boo;

  { convert string to boo, 'on' equals true }
  procedure SetFlag(var flag: boo; const value: astr);
  begin
    if Lcase(value) = 'on' then flag:= true else flag:= false;
  end;

  { convert string to coolean, then check flag: 'on' equals true }
  function FlagOn(const value: astr): boo;
  begin
    if Lcase(value) = 'on' then result:= true else result:= false;
  end;

  type TCfgKind = (ckOther, ckHeaderCharset, ckErrorReporting, ckErrorHalt, 
                   ckOutputBuffering, ckOutputCompression, ckSessionPath,
                   ckSessionLifetime, ckUploadMaxSize);
  { makes case statement possible for strings }
  function CfgKind(const s: astr): TCfgKind;
  begin
    result:= ckOther;
    if s = L_HEADER_CHARSET then result:= ckHeaderCharset else if 
       s = L_ERROR_REPORTING then result:= ckErrorReporting else if 
       s = L_ERROR_HALT then result:= ckErrorHalt else if 
     {$ifdef gzip_on} 
       s = L_OUTPUT_BUFFERING then result:= ckOutputBuffering else if 
       s = L_OUTPUT_COMPRESSION then result:= ckOutputCompression else if 
     {$endif} 
       s = L_SESSION_PATH then result:= ckSessionPath else if 
       s = L_SESSION_LIFE_TIME then result:= ckSessionLifetime else if 
       s = L_UPLOAD_MAX_SIZE then result:= ckUploadMaxSize;
  end;

  procedure BeforeExit1; 
  begin ThrowWarn('header charset can''t be set after headers sent');           {$ifdef dbug_on}SetCfgVar_X1;{$endif}
  end;

  procedure BeforeExit2;
  begin ThrowWarn('output compression needs output_buffering on');              {$ifdef dbug_on}SetCfgVar_X2;{$endif}
  end;

  procedure BeforeExit3;
  begin ThrowWarn('output compression can''t be unset after headers sent');     {$ifdef dbug_on}SetCfgVar_X3{$endif}
  end;

  procedure BeforeExit4;
  begin ThrowWarn('sess path/lifetime can''t be set after headers sent');       {$ifdef dbug_on}SetCfgVar_X4{$endif}
  end;

  procedure BeforeExit5;
  begin ThrowWarn('upload max size can''t be set after headers sent');          {$ifdef dbug_on}SetCfgVar_X5;{$endif}
  end;    

  procedure BeforeExit6;
  begin ThrowWarn('sess cfg can''t be set without session unit');               {$ifdef dbug_on}SetCfgVar_X6;{$endif}
  end;

{b}                                                                             begin {$ifdef dbug_ON}SetCfgVar_B;{$endif}
  // initialize
  result:= false; 
  // set flags
  case CfgKind(name) of
    ckHeaderCharset: 
      if headers_sent then begin BeforeExit1; exit; end;
    ckErrorReporting:  SetFlag(error_reporting, value);
    ckErrorHalt: SetFlag(error_halt,value);
   {$ifdef gzip_on}
    ckOutputBuffering: begin
      // set flags and apply checks
      if FlagOn(value) then out_buffering:= true
      else begin
        if out_buffering then FlushBuf;
        out_buffering:= false;
      end;
    end;
    ckOutputCompression: begin
      // Setting internal flag and applying checks
      if FlagOn(value) then begin
        if headers_sent then begin BeforeExit3; exit; end;
        if not out_buffering then begin BeforeExit2; exit; end;
        out_compression:= true;
      end else begin
        if headers_sent then begin BeforeExit3; exit; end;
        if out_compression then UnsetHeader('Content-Encoding');
        out_compression:= false;
      end;
    end;
   {$endif} 
    ckSessionPath, ckSessionLifetime: begin
      if not(iCustomSessUnitSet) then begin BeforeExit6; exit; end;
      if headers_sent then begin BeforeExit4; exit; end;
    end; 
    ckUploadMaxSize: if headers_sent then begin BeforeExit5; exit; end;
  end; {case}
  
  PutWebCfgVar(name, value, CASE_IGNORE);
  // headers may have been updated and gzip buffer may need to be created
  InitBufAndHeaders;
  result:= true;
{e}                                                                             {$ifdef dbug_on}SetConfigVar_E; {$endif}end;

{ Returns value of configuration variable. Case insensitive NAME search.
  todo: research if security levels can be implemented }
function GetCfgVar(const name: astr): astr;
var i: int32;
begin
  result:= '';
  if length(gConf) < 1 then exit;
  for i:= low(gConf) to high(gConf) do if Ucase(gConf[i].name) = Ucase(name) 
  then begin 
    result:= gConf[i].value; 
    break; 
  end;
end;

{ Returns number of elements in the get/post var list }
function CountPostVars: longword;
begin result:= length(gGetPost);
end;

{ Returns number of cookie variables }
function CountCookies: longword;
begin result:= length(gCook);
end;

{ Returns number of set headers }
function CountHeaders: longword;
begin result:= length(gHdr);
end;

{ Returns number of Run-Time Information variables }
function CountRTIVars: longword;
begin result:= length(gRti);
end;

{ Returns number of files uploaded }
function CountUpFiles: longword;
begin result:= length(gUpFiles);
end;

{ Returns number of all web (macro) variables }
function CountVars: longword;
begin result:= length(gVar);
end;

{ Returns number of any macro, cookie, or gGetPost variables }
function CountAny: longword;
begin result:= length(gVar) + length(gCook) + length(gGetPost);
end;

{ Replaces special chars with their HTML entities. If you are taking input on a
  guestook or forum for example.

  Default security level: 2 }
function FilterHtml(const input: astr): astr;
begin result:= FilterHtml_S(input, SECURE_ON);
end;

(* Powers the FilterHTML function, here with ability to define security level

  Secure Level X:
    For future consideration
  Secure Level 2:
    Filtering of malicious input variable injection characters. *)
function FilterHtml_S(const input: astr; security: byte): astr;
begin
  if security = SECURE_ON then
  begin
    result:= substrreplace(input, ';', '&#59;');    //sql injection semi colon
    result:= substrreplace(result, '&', '&amp;');
    result:= substrreplace(result, '#', '&#35;');   //pound sign
    result:= substrreplace(result, '"', '&quot;');  //quote
    result:= substrreplace(result, '''', '&#39;');  //single quote
    result:= substrreplace(result, '<', '&lt;');    //less than
    result:= substrreplace(result, '>', '&gt;');    //greater than
    result:= substrreplace(result, '|', '&#124;');  //pipe
    result:= substrreplace(result, '%', '&#37;');   //percent sign
    result:= substrreplace(result, '(', '&#40;');   //open bracket
    result:= substrreplace(result, ')', '&#41;');   //closed bracket
    result:= substrreplace(result, '$', '&#36;');   //dollar sign
    result:= substrreplace(result, '?', '&#63;');   //question mark
    result:= substrreplace(result, '--', '&#45;&#45;'); //sql injection double dash
    result:= substrreplace(result,  #0, '0');       //null bad, show 0 to make hackers obvious
//   Note: CSS Styles in a macro var could contain curlies { } so are not filtered    
  end;
  
end;

{ Indexed access to gGetPost variable }
function FetchPostVarName(idx: int32): astr;
begin result:= FetchPostVarName_S(idx, SECURE_ON);
end;

{ Indexed access to gGetPost variable }
function FetchPostVarVal(idx: int32): astr;
begin result:= FetchPostVarVal_S(idx, SECURE_ON);
end;

{ security 0 and custom user filter applied }
function FetchPostVarVal(idx: int32; vfilter: TFilterFunc): astr;
begin
  result:= FetchPostVarVal_S(idx, SECURE_OFF);
  if assigned(vfilter) then result:= vfilter(result);
end;

{ for DLL }
function FetchPostVarVal1(idx: int32): astr;
begin result:= FetchPostVarVal(idx);
end;

{ for DLL }
function FetchPostVarVal2(idx: int32; vfilter: TFilterFunc): astr;
begin result:= FetchPostVarVal2(idx, vfilter);
end;

{ Indexed access to Twebvars name, security specifiable }
function FetchTWebVarName_S(const w: TWebVars; idx: int32; security: byte): astr;
begin
  if (idx >= 0) and (length(w) > 0) then begin
    case Security of 
      SECURE_OFF: result:= w[idx].name;
      SECURE_ON: result:= TrimBadCHars(w[idx].name);
    end;
  end else
   result:= '';
end;

{ Indexed access to Twebvars value, security specifiable }
function FetchTWebVarVal_S(const w: TWebVars; idx: int32; security: byte): astr;
begin
  if (idx >= 0) and (length(w) > 0) then
  begin
    case Security of 
      SECURE_OFF: result:= w[idx].value;
      SECURE_ON: result:= TrimBadChars(w[idx].value);
    end;
  end else
    result:= '';
end;

{ Indexed access to gGetPost variable name, security specifiable }
function FetchPostVarName_S(idx: int32; security: byte): astr;
begin
  result:= FetchTWebVarName_S(gGetPost, idx, security);
end;

{ Indexed access to gGetPost variable value, security specifiable }
function FetchPostVarVal_S(idx: int32; security: byte): astr;
begin
  result:= FetchTWebVarVal_S(gGetPost, idx, security);
end;

{ Indexed access to cookie variable }
function FetchCookieName(idx: int32): astr;
begin
  // security off because cookies could characters developers don't want 
  // trimmed, and they may get confused if security was trimming their cookies
  result:= FetchTWebvarName_S(gCook, idx, SECURE_OFF);
end;

{ Indexed access to cookie variable }
function FetchCookieVal(idx: int32): astr;
begin
  // security off for the same reasons as FetchCookieName
  result:= FetchTWebvarVal_S(gCook, idx, SECURE_OFF);
end;

function FetchCookieVal(idx: int32; cfilter: TFilterFunc): astr;
begin
  result:= FetchCookieVal(idx);
  if assigned(cfilter) then result:= cfilter(result);
end;

{ Indexed access to header }
function FetchHeaderName(idx: int32): astr;
begin
  // security off because headers may contain characters developers don't
  // want trimmed
  result:= FetchTWebvarName_S(gHdr, idx, SECURE_OFF);
end;

{ Indexed access to header }
function FetchHeaderVal(idx: int32): astr;
begin
  // security off for same reasons as FetchHeaderName
  result:= FetchTWebvarVal_S(gHdr, idx, SECURE_OFF);
end;

{ Indexed access to RTI variable }
function FetchRtiName(idx: int32): astr;
begin
  // security off because RTI vars currently not so externally vulnerible and 
  // could  contain characters that developers don't want trimmed 
  result:= FetchTWebvarName_S(gRti, idx, SECURE_OFF);
end;

{ Indexed access to RTI variable }
function FetchRtiVal(idx: int32): astr;
begin
  // security off for same reasons as FetchRtiName
  result:= FetchTWebvarVal_S(gRti, idx, SECURE_OFF);
end;

{ Indexed access to uploaded file name }
function FetchUpFileName(idx: int32): astr;
begin
  if (idx >= 0) and (length(gUpFiles) > 0) 
    then result:= gUpFiles[idx].name else result:= '';
end;

{ Indexed access to user defined variable }
function FetchVarName(idx: int32): astr;
begin result:= FetchTWebvarName_S(gVar, idx, SECURE_OFF);
end;

{ Indexed access to user defined variable }
function FetchVarVal(idx: int32): astr;
begin result:= FetchTWebVarVal_S(gVar, idx, SECURE_OFF);
end;

function FetchVarVal(idx: int32; vfilter: TFilterFunc): astr;
begin
  result:= FetchVarVal(idx);
  if assigned(vfilter) then result:= vfilter(result);
end;

{ Fmt_SF offers the ability to specify security levels and filter
  settings, and is also used internally to power the default Format
  and FmtFilter functions. Those are the ones you use normally,
  this one is for special circumstances

  The _SF suffix means "with specifiable Security and Filter options"

  If HTMLFilter = false the Filter security is ignored and should be set
  at 0, because there is no filter security setting that applies.

  The trim security is ignored and should set at 0 when
  HTMLFilter = true,  because we can't trim the special characters
  and then try to replace them after (they would already be trimmed).
  i.e. we have to use one or the other, either replace or trim input.}

function Fmt_SF(const s: astr; usefilter: boo; FilterSecurity, Trimsecurity: byte; vfilter: TFilterFunc): astr;
const
  ID_CHARS = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_';
var
  i, len: int32;
  lex: astr;

  { appends char to result string }
  procedure AddToResult;
  begin
    setlength(result, length(result) + 1);
    result[length(result)]:= s[i];
    inc(i);
  end;

  { if escaped \$var then we must replace \ with $}
  procedure AddEscapedDollar;
  begin
    result[length(result)]:= s[i];
    inc(i);
  end;

  { appends char to lex string }
  procedure AddToLex;
  begin
    setlength(lex, length(lex) + 1);
    lex[length(lex)]:= s[i];
    inc(i);
  end;

  { evaluate macrovar to current lex }
  procedure VarToLex;
  begin
    if usefilter then begin
      case FilterSecurity of
        SECURE_OFF: lex:= GetVar_S(lex, SECURE_OFF); 
        SECURE_ON: lex:= GetVar(lex, {$ifndef FPC}@{$endif}vfilter); // apply macrovar filter
      end;
    end else
      lex:= GetVar_S(lex, TrimSecurity);
  end;

begin
  // Init
  i:= 1;
  len:= length(s);
  lex:= '';
  result:= '';
  // Parsing
  while i <= len do
  begin
    // Normal concat until chars of our attention
    while (i <= len) and (s[i] <> '$') and (s[i] <> '{') do AddToResult;
    // If encountered an indication char
    if (i <= len) and (s[i] = '$') then
    begin
      // $varname?
      // Checking if escaped
      if ((i - 1) > 0) and (s[i - 1] = '\') then AddEscapedDollar
        else
      begin
        // Getting var name
        inc(i);
        lex:= '';
        while (i <= len) and (substrpos(ID_CHARS, s[i]) > 0) do AddToLex;
        VarToLex;
        result:= result + lex;
      end;
    end else
      if (i <= len) and (s[i] = '{') then 
      begin
        // {$varname}?
        // Check if escaped or for pattern match
        if ((i - 1) > 0) and (s[i - 1] = '\') then AddToResult
        // at end of line?
        else if i = len then AddToResult
        // if not found, continue on
        else if ((i + 1) < len) and (s[i + 1] <> '$') then AddToResult
        else begin
          // There MUST be } or curly braces should be escaped
          // Getting var name till }
          i:= i + 2;
          lex:= '';
          while (i <= len) and (s[i] <> '}') do AddToLex;
          inc(i);
          VarToLex;
          result:= result + lex;
        end;
      end;
  end;
end;

{ overloaded with default macrovar filter }
function Fmt_SF(const s: astr; HTMLFilter: boo; FilterSecurity, TrimSecurity: byte): astr;
begin
  result:= Fmt_SF(s, HTMLFilter, FilterSecurity, TrimSecurity, {$ifdef fpc}@{$endif}FilterHtml);
end;


{ $macrovars replaced with ones set via SetVar, applies custom html filter to 
  macrovars being formatted, returns formatted result string}
function Fmt(const s: astr; vfilter: TFilterFunc): astr;
begin
  result:= Fmt_SF(s, true, SECURE_ON, SECURE_OFF, vfilter);
end;

{ Formats a string replacing variables as if they were macros.
  i.e. if a string contains $MyVariable it will be replaced
  This function does not filter and replace malicious/html characters, but
  rather trims (discards) them

  Default security level: 2 }
function Fmt(const s: astr): astr;
begin
  result:= Fmt_SF(s, false, SECURE_OFF, SECURE_ON);
  // Uses the following default security settings:
  //   Filter HTML: no, we are trimming
  //   Filter security: level 0, we are trimming
  //   Trim security: level 2

end;

{ Same as Fmt, but filters and replaces HTML characters with safe ones,
  as opposed to trimming and discarding them like Fmt() does.

  Default security level: 2 }
function FmtFilter(const s: astr): astr;
begin
  result:= fmt(s, {$ifdef FPC}@{$endif}FilterHtml);
end;

{ Returns value of gGetPost (GET/POST) variable. This also means your URL variables.

  Default Security level is 2. Use the _S suffix function if you do not need
  high filtering security, or you wish to implment your own filters }
function GetPostVar(const name: astr): astr;
begin
  result:= GetPostVar_S(name, SECURE_ON);
end;

{ security 0, with custom user filter in place }
function GetPostVar(const name: astr; vfilter: TFilterFunc): astr;
begin
  result:= GetPostVar_S(name, SECURE_OFF);  
  if assigned(vfilter) then result:= vfilter(result);
end;

{ for DLL }
function GetPostVar1(const name: astr; vfilter: TFilterFunc): astr;
begin
  result:= GetPostVar(name, vfilter);
end;

{ for DLL }
function GetPostVar2(const name: astr): astr;
begin
  result:= GetPostVar(name);
end;

function GetPostVar_Unsafe(const name: astr): astr;
var i: int32;
begin
  result:= '';
  if length(gGetPost) = 0 then exit;
  for i:= low(gGetPost) to high(gGetPost) do if gGetPost[i].name = name 
  then begin
    result:= gGetPost[i].value; 
    exit;
  end;
end;


(* Old way
function GetPostVar_S(const name: string; security: byte): astr;
var
  i: int32;
begin
  result:= '';
  if length(gGetPost) = 0 then exit;
  
  for i:= 0 to length(gGetPost) - 1 do if gGetPost[i].name = name then
  begin
    case security of
     //perform a trim with security 2, output result
     SECURE_ON: 
       begin
         result:= TrimBad_S(gGetPost[i].value, SECURE_ON);
         exit;
       end;
     //perform NO trim, output result
     SECURE_OFF: 
       begin 
         result:= gGetPost[i].value; 
         exit;
       end;
    end;{case}
  end;
end;
*)

{ Same as GetPostVar, but the _S suffix means you can choose the security level

  Security 0: does not automatically trim. use this when you want to implement
              your own filtering, such as when using FilterHTML
  Security 2: zeros out malicious characters}
function GetPostVar_S(const name: astr; security: byte): astr;
begin
  result:= GetPostVar_Unsafe(name);
  case security of
    // trim malicous characters
    SECURE_ON: result:= TrimBadChars(result);
    // perform NO trim, raw result
    SECURE_OFF: result:= result; 
  end;
end;

{ Returns value of gGetPost (GET/POST) variable as double precision float }
function GetPostVarAsFloat(const name: astr): double;
begin result:= GetTwebvarAsFloat(gGetPost, name);
end;

{ Returns value of gGetPost (GET/POST) variable as int32 }
function GetPostVarAsInt(const name: astr): int32;
begin result:= GetTwebvarAsInt(gGetPost, name);
end;

function GetTWebvarVal(const w: TWebVars; const name: astr): astr;
var i: int32;
begin
  result:= '';
  if length(w) < 1 then exit;
  for i:= low(w) to high(w) do if w[i].name = name then begin
    result:= w[i].value; 
    break;
  end;
end;

{ Returns value of a cookie
  todo: research if security levels can be implemented }
function GetCookie(const name: astr): astr;
begin result:= GetTWebVarVal(gCook, name);
end;

function GetCookie(const name: astr; cfilter: TFilterFunc): astr;
begin
  result:= GetCookie(name);
  if assigned(cfilter) then result:= cfilter(result);
end;

{ Returns value of a cookie as double precision float
  todo: research if security levels can be implemented }
function GetCookieAsFloat(const name: astr): double;
begin result:= GetTwebvarAsFloat(gCook, name);
end;

{ Returns value of a cookie as integer
  todo: research if security levels can be implemented }
function GetCookieAsInt(const name: astr): int32;
begin result:= GetTWebvarAsInt(gCook, name);
end;

{ Returns value part of already assigned HTTP header
  todo: research if security levels can be implemented }
function GetHeader(const name: astr): astr;
begin result:= GetTWebVarVal(gHdr, name);
end;

{ Returns value of RTI (Run Time Information) variable
  todo: research if security levels can be implemented }
function GetRti(const name: astr): astr;
begin result:= GetTWebVarVal(gRti, name);
end;

{ Returns value of RTI variable as double precision float
  todo: research if security levels can be implemented }
function GetRtiAsFloat(const name: astr): double;
begin result:= GetTwebvarAsFloat(gRti, name);
end;

{ Returns value of RTI variable as integer
  todo: research if security levels can be implemented }
function GetRtiAsInt(const name: astr): int32;
begin result:= GetTWebvarAsInt(gRti, name);
end;

{ Returns original name of the uploaded file
  todo: research if security levels can be implemented }
function GetUpFileName(const name: astr): astr; var i: int32;
begin
  result:= '';
  if length(gUpFiles) = 0 then exit;
  for i:= low(gUpFiles) to high(gUpFiles) do if gUpFiles[i].name = name then begin
    result:= gUpFiles[i].filename;
    break;
  end;
end;

{ Returns size of the uploaded file }
function GetUpFileSize(const name: astr): int32; var i: int32;
begin
  result:= 0;
  if length(gUpFiles) = 0 then exit;
  for i:= low(gUpFiles) to high(gUpFiles) do if gUpFiles[i].name = name then begin
    result:= gUpFiles[i].size;
    break;
  end;
end;

{ Returns Content-Type of the uploaded file
  todo: research if security levels can be implemented }
function GetUpFileType(const name: astr): astr; var i: int32;
begin
  result:= '';
  if length(gUpFiles) = 0 then exit;
  for i:= low(gUpFiles) to high(gUpFiles) do if gUpFiles[i].name = name then begin
    result:= gUpFiles[i].content_type;
    break;
  end;
end;

(* Powers the GetVar function. Use this function for special circumstances,
   with the ability to specify security level.

   Security level 0:
     Doesn't trim special characters. Use this if you are trimming yourself,
     or when you are using FilterHTML() function yourself

   Security level 2:
     Trims (deletes) malicious characters from variable *)
function GetVar_S(const name: astr; security: byte): astr; var i: int32;
begin
  result:= '';
  // look in vars
  if length(gVar) > 0 then
    for i:= low(gVar) to high(gVar) do if gVar[i].name = name then begin
      case security of 
        SECURE_OFF: begin result:= gVar[i].value; exit; end;
        SECURE_ON:  begin result:= TrimBadChars(gVar[i].value); exit; end;
      end;
    end;
end;

{ Returns value of any macro template variable (gVar[])
  Default security level: 2 }
function GetVar(const name: astr): astr;
begin result:= GetVar_S(name, SECURE_ON);
end;

function GetVar(const name: astr; vfilter: TFilterFunc): astr;
begin
  result:= GetVar_S(name, SECURE_OFF);
  if assigned(vfilter) then result:= vfilter(result);
end;

{ look in macrovars, posted vars, cookie vars }
function GetAny_S(const name: astr; security: byte): astr;
  
  function Check(const w: TWebVars): astr;
  var i: int32;
  begin
    result:= '';
    if length(w) < 1 then exit; 
    for i:= low(w) to high(w) do if w[i].name = name then begin
      case security of 
        SECURE_OFF: begin result:= w[i].value; exit; end;
        SECURE_ON:  begin result:= TrimBadChars(w[i].value); exit; end;
      end;
    end;
  end;

begin
  result:= '';
  result:= Check(gVar); 
  if result = '' then result:= Check(gGetPost); 
  if result = '' then result:= Check(gCook);
end;


function GetAny(const name: astr): astr;
begin result:= GetAny_S(name, SECURE_ON);
end;

{ security 0, with custom user filter in place }
function GetAny(const name: astr; vfilter: TFilterFunc): astr;
begin
  result:= GetAny_S(name, SECURE_OFF);  
  if assigned(vfilter) then result:= vfilter(result);
end;

(*
 Powers the GetCGIVar_SafeHTML function. Use this function for special
 circumstances, with the ability to specify security level.

 Security level 0:
   Doesn't filter special characters. Use this if you are filtering or
   trimming yourself, or when you are using FilterHTML

 Security level 2:
   Filters malicious characters from variable into safe html equivalents
*)
function GetPostVar_SF(const name: astr; security: byte): astr; var i: int32;
begin
  result:= '';
  // look in gGetPost vars
  if length(gGetPost) > 0 then
    for i:= low(gGetPost) to high(gGetPost) do if gGetPost[i].name = name then 
    begin
      case security of
        SECURE_OFF: begin result:= gGetPost[i].value; exit; end;
        SECURE_ON:  begin result:= FilterHtml(gGetPost[i].value); exit; end;
      end;
    end;
end;

function GetPostVar_SafeHTML(const name: astr): astr;
begin result:= GetPostVar_SF(name, SECURE_ON);
end;

{ Return value of macrovar as float (double precision) }
function GetVarAsFloat(const name: astr): double;
begin val(GetVar(name), result);
end;

{ Return float value of any macrovar, posted var, or cookie }
function GetAnyAsFloat(const name: astr): double;
begin val(GetAny(name), result);
end;

{ Return integer value of any macrovar }
function GetVarAsInt(const name: astr): int32;
begin val(GetVar(name), result);
end;

{ Return integer value of any macrovar, posted var, or cookie }
function GetAnyAsInt(const name: astr): int32;
begin val(GetAny(name), result);
end;

{ private: abstract }
function IsTwebvar(const name: astr; const w: TWebvars): boo;
var i: int32;
begin
  result:= false;
  if length(w) > 0 then for i:= low(w) to high(w) do 
    if w[i].name = name then begin result:= true; break; end;
end;

{ Tells whether a configuration variable is assigned }
function IsCfgVar(const name: astr): boo;
begin result:= IsTwebvar(name, TwebVars(gConf))
end;

{ Tells whether a gGetPost (GET/POST/URL) variable is assigned }
function IsPostVar(const name: astr): boo;
begin result:= IsTwebvar(name, gGetPost)
end;

{ Tells whether a cookie is assigned }
function IsCookie(const name: astr): boo;
begin result:= IsTwebvar(name, gCook)
end;

{ Tells if an RTI variable exists }
function IsRti(const name: astr): boo;
begin result:= IsTwebvar(name, gRti)
end;

{ Tells if a macro var exists }
function IsVar(const name: astr): boo;
begin result:= IsTwebvar(name, gVar)
end;

{ Tells if a file field is uploaded }
function IsUpFile(const name: astr): boo;
var i: int32;
begin
  result:= false;
  if length(gUpFiles) = 0 then exit;
  for i:= low(gUpFiles) to high(gUpFiles) do if gUpFiles[i].name = name then begin 
    result:= true; 
    break;
  end;
end;

{ Tells if a header is assigned, case insensitive name }
function IsHeader(const name: astr): boo;
var i: int32;
begin
  result:= false;
  if length(gHdr) = 0 then exit;
  for i:= low(gHdr) to high(gHdr) do if Ucase(gHdr[i].name) = Ucase(name) then 
  begin 
    result:= true; 
    break;
  end;
end;

{ Tells if any web var exists (macro, cookie, posted, etc) 
  NOTE: Backwards compatibility issue: DOES NOT CHECK SESSIONS IN 1.7.X }
function isAny(const name: astr): byte;
var i: int32;
begin
  result:= 0;
  // look in gVar
  if length(gVar) > 0 then
    for i:= low(gVar) to high(gVar) do if gVar[i].name = name then begin 
      result:= 1; 
      exit;
    end;
  // look in cookies
  if length(gCook) > 0 then
    for i:= low(gCook) to high(gCook) do if gCook[i].name = name then begin 
      result:= 3; 
      exit;
    end;
  // look in gGetPost gVar
  if length(gGetPost) > 0 then
    for i:= low(gGetPost) to high(gGetPost) do if gGetPost[i].name = name then begin 
      result:= 4; 
      exit;
    end;
end;


{ Replaces all end-of-line chars with <br /> tags }
function LineEndToBR(const s: astr): astr;
begin
  result:= s;
  if substrexists(result, #13#10) then 
    result:= substrreplace(result, #13#10, HTM_BREAK);
  if substrexists(result, #10) then
    result:= substrreplace(result, #10, HTM_BREAK);
end;

{ Plain text output }
procedure out(const s: astr);
begin
 {$ifdef gzip_on}
  if out_buffering then 
    OutBuff^.AppendStr(s) 
  else
 {$endif}
  begin
    if not headers_sent then SendHeaders;
    NativeWrite(s);
  end;
end;

{ Output several arguments at once, multiple types allowed }
procedure outa(args: array of const);
var i: int32;  
begin  
  if high(args) < 0 then 
    out('')  
  else 
  begin
    for i:= low(args) to high(args) do  
    begin  
      case Args[i].vtype of  
        vtinteger   : out(pwstrutil.inttostr(args[i].vInteger));  
        vtboolean   : out(booltostr(args[i].vBoolean));
        vtchar      : out(astr(args[i].vChar));  
        vtextended  : out(FormatFloat('', args[i].VExtended^));  // i.e float value
        vtString    : out(args[i].vString^);  
        vtPChar     : out(Args[i].vPChar);  
        vtAnsiString: out(AnsiString(Args[I].vAnsiString));
      else  
        {$ifdef dbug_on}outa_Unable_to_use_type_in_array_param;{$endif}
      end;  
    end;  
  end;
end;

procedure outlna(args: array of const);
begin
  outa(args);
  outln;
end;

{ Formatted output with $MacroVars. As opposed to OutFF, this function replaces
  malicious characters with zeros. It does not replace them with html entities.
  F stands for "formatted" }
procedure outf(const s: astr);
begin
  out(Fmt(s));
end;

{ overloaded, custom filter applied to macrovars }
procedure outf(const s: astr; vfilter: TFilterFunc);
var tmp: astr;
begin
  tmp:= Fmt(s, vfilter);
  out(tmp);
end;

{ Formatted output with $MacroVars. As opposed to OutF, this function filters 
  and replaces malicious characters with HTML equivilents, rather than zeroing
  them out.  FF stands for "format & filter". }
procedure outff(const s: astr);
begin
  out(FmtFilter(s));
end;

{ Plain output with line feed (no html break, just CRLF) }
procedure outln(const s: astr);
begin
  out(s + CGI_CRLF);
end;

procedure outln;
begin
  outln('');
end;

{ Formatted outln, outputs $macrovars that exist from SetVar 
  Zeros out malicious chars. F stands  for "Formatted". }
procedure outlnf(const s: astr);
begin
  outln(fmt(s));
end;

{ overloaded, custom filter applied to macrovars }
procedure outlnf(const s: astr; vfilter: TFilterFunc);
begin
  outf(s, vfilter);
  outln;
end;

{ Formatted and filtered writeln, outputs variables like macros,
  i.e. replaces $MyVar with an existing web variable, plus filters
  malicious attempts by filtering HTML special characters.
  FF stands for "Formatted and Filtered" }
procedure outlnff(const s: astr);
begin
  outff(s);
  outln;
end;

function NoHeadSentNorBuffering: boo;
begin
  result:= false;
  if (not headers_sent) {$ifdef gzip_on}and (not out_buffering){$endif} then
    result:= true;
end;


{ Plain file output - returns FILE_READ_ERR if problem, OK otherwise  }
function FileOut(const fname: astr): errcode;
var fh: text;
    s: astr;
begin
  result:= FILE_READ_ERR;
  if not FileExists_read(fname) then begin ThrowNoFileErr(fname); exit; end;
  if NoHeadSentNorBuffering then SendHeaders;
  assign(fh, fname);
  reset(fh);
  while not eof(fh) do begin readln(fh, s); OutLnNoHeaders(s); end;
  close(fh);
  result:= OK;
end;

{ private }
procedure WriteBuff(p: Pointer; len: LongWord);
begin
 {$ifdef gzip_on}
  if out_buffering then begin
    OutBuff^.AppendBuffer(p, len);
  end else
 {$endif}
    NativeWrite(p, len);
end;

{ Binary Buffer Output...UNTYPED }
procedure BufferOut(const buff; len: LongWord);
var P: pointer;
begin
  P:= @Buff;
  if NoHeadSentNorBuffering then SendHeaders;
  WriteBuff(P, len);
end;

{ Plain binary file output }
function ResourceOut(const fname: astr): errcode;
const BUFFSIZE = 16384;
var fh: file of char;
    buff: pchar;
    len: longword;
begin
  result:= FILE_READ_ERR;
  len:= 0;
  InitMimeDb; // prepare Tony's mime db unit
  if not FileExists_read(fname) then begin ThrowNoFileErr(fname); exit; end;
  GetMem(buff, BUFFSIZE);
  if (not headers_sent) then SetHeader('Content-Type', GetMimeType(fname));
  if NoHeadSentNorBuffering then SendHeaders;
  assign(fh, fname);
  reset(fh);
  while not eof(fh) do begin 
    blockread(fh, buff^, BUFFSIZE, len);  
    WriteBuff(buff, len);
  end;  
  close(fh);
  FreeMem(Buff);
  result:= ok;
end;

{ with custom filter func set by user for security on each macro var }
function TemplateOut(const fname: astr; vfilter: TFilterFunc): errcode;
var fh: text;
    s: astr;
begin
  result:= FILE_READ_ERR;
  if not strwrap1.OpenFile(fh, fname, 'r') then begin
    ThrowNoFileErr(fname);
    // cleanup note: file handle shouldn't need closing... if it wasn't opened
    exit;
  end;

  if NoHeadSentNorBuffering then SendHeaders;
  
  while not eof(fh) do begin
    readln(fh, s);
    s:= Fmt(s, vfilter); // apply custom filter to any template macro vars
    OutLnNoHeaders(s);
  end;

  close(fh);
  result:= OK;
end;

{ default templateout, applies html filter }
function TemplateOut(const fname: astr): errcode;
begin
  result:= TemplateOut(fname, true);
end;

{ Formatted file output (macro $variables). If HTMLFilter true, then malicious 
  chars from incoming variables are replaced with html entities. If false, 
  malicious chars are trimmed (deleted).

  i.e. if an incoming variable $EditInput contains malicious chars, they
  are either trimmed or they are filtered. The actual TEMPLATE html itself is 
  not filtered or trimmed, just variables being used via SetVar. Template files 
  are dynamic text files. Anything dynamic in text file format is less secure, 
  just like a PHP script is less secure than static html.}
function TemplateOut(const fname: astr; HtmlFilter: boo): errcode; 
begin
  if HtmlFilter = true then 
    result:= TemplateOut(fname, {$ifdef FPC}@{$endif}FilterHTML) 
  else 
    result:= TemplateOut(fname, nil);   
end;

{ for DLL exporting }
function TemplateOut1(const fname: astr; HtmlFilter: boo): errcode; 
begin
  result:= TemplateOut(fname, htmlfilter);
end;

{ Raw template output. Similar to TemplateOut but NO filtering or trimming
  is perfomed on the macro vars.

  Insecure, only use when you wish to output raw HTML. People can inject
  javascript into URL variables }
function TemplateRaw(const fname: astr): errcode;
begin
  result:= TemplateOut(fname, nil);
end;

{ Sets HTTP header like 'Name: Value' }
function PutHeader(const header: astr): boo;
var i: int32;
    nv: TStrArray;
begin
  result:= false;
  setlength(nv, 0);
  // Check headers
  if headers_sent then begin
    ThrowErr('Can''t put header, headers already sent');
    exit;
  end;
  // split into name=value pair
  nv:= substrsplit(header, ':');
  if length(nv) <> 2 then exit;
  nv[0]:= strtrim(nv[0]);
  nv[1]:= strtrim(nv[1]);
  // Change value if already set
  if length(gHdr) > 0 then
  for i:= low(gHdr) to high(gHdr) do if Ucase(gHdr[i].name) = Ucase(nv[0]) then
  begin
    gHdr[i].value:= nv[1];
    exit;
  end;
  // Add new header
  AddWebVar(gHdr, nv[0], nv[1]);
  result:= true;
end;

{ Generate random string of alphanumeric + '_' char, specify string length }
function RandomStr(len: int32): astr;
const PW_CHARS = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_';
var i: int32;
begin
  result:= '';
  if len < 1 then exit;
  setlength(result, len);
  for i:= 1 to len do begin randomize; result[i]:= PW_CHARS[random(62)+1]; end;
end;

{ Saves uploaded file to disk }
function SaveUpFile(const name, fname: astr): boo;
var i: int32;
    fh: TFileOfChar;
    written : boo;
{b}                                                                             begin {$ifdef dbug_on}SaveUpFile_B;{$endif}
  result:= false;
  if length(gUpFiles) = 0 then exit;
  written:= false;
  if not strwrap1.OpenFile(fh, fname, 'w') then exit;
  i:= 0;
  repeat
    if gUpFiles[i].name = name then
      if length(gUpFiles[i].Data) > 0 then begin
        blockwrite(fh, gUpFiles[i].data[1], gUpFiles[i].size);
        if ioresult = 0 then written := true;
      end;
    inc(i);
  until (i = Length(gUpFiles)) or written;
  close(fh);
  result:= written;
{e}                                                                             {$ifdef dbug_on}SaveUpFile_E{$endif}end;

{ Set a cookie }
function SetCookie(const name, value: astr): boo;
var hdrval: astr;
{b}                                                                             begin {$ifdef dbug_on}SetCookie_B;{$endif}
  result:= false;
  // Check headers
  if headers_sent then begin
    ThrowErr('Can''t set cookie, headers already sent');
    {$ifdef dbug_on}SetCookie_X1;{$endif}
    exit;
  end;

  // Change value if already exist, or add new one if not exist
  if not UpdateWebVar(gCook, name, value, CASE_SENSITIVE) then begin
    // Add new cookie
    AddWebVar(gCook, name, value);
    // Add header
    hdrval:= UrlEncode(name) + '=' + UrlEncode(value) + ';path=/;expires='+ FUTURE_COOKIE;
    AddWebVar(gHdr, 'Set-Cookie', hdrval);
  end;
  result:= true;
{e}                                                                             {$ifdef dbug_on}SetCookie_E;{$endif}end;

{ Set cookie as double precision float }
function SetCookieAsFloat(const name: astr; value: double): boo;
var s: astr;
begin 
  str(value, s);
  result:= SetCookie(name, s);
end;

{ Set cookie as integer }
function SetCookieAsInt(const name: astr; value: int32): boo;
var s: astr;
begin
  str(value, s);
  result:= SetCookie(name, s);
end;

{ Set extended cookie }
function SetCookieEx(const name, value, path, domain, expiry: astr): boo;
var headval, pathval, domainpart, expireval: astr;
    updated: boo;
{b}                                                                             begin {$ifdef dbug_on}SetCookieEx_B;{$endif}
  result:= false;
  // Check headers
  if headers_sent then begin
    ThrowErr('Can''t set cookie: headers already sent');                        {$ifdef dbug_on}SetCookieEx_X1;{$endif}
    exit;
  end;

  // Update value if name already exists, or add new one
  updated:= UpdateWebVar(gCook, name, value, CASE_SENSITIVE);
  if not updated then
  begin
    AddWebVar(gCook, name, value);
    pathval:= path;
    if pathval = '' then pathval:= '/';
    if domain = '' then domainpart:= '' else domainpart:= 'domain=' + domain + ';';
    expireval:= expiry;
    if expireval = '' then expireval:= FUTURE_COOKIE;
    // Add new header
    headval:= UrlEncode(name) + '=' + UrlEncode(value) + ';' + 
              'path=' + pathval + ';' + 
              domainpart + 
              'expires=' + expireval; // note: no trailing semi-colon
    AddWebVar(gHdr, 'Set-Cookie', headval);
  end;
  result:= true;
{e}                                                                             {$ifdef dbug_on}SetCookieEx_E;{$endif}end;

{ Sets an extended cookie as double precision float }
function SetCookieAsFloatEx(const name: astr; value: double; const path, domain, expiry: astr): boo;
var s: astr;
begin
  str(value, s);
  result:= SetCookieEx(name, s, path, domain, expiry);
end;

{ Sets an extended cookie as integer }
function SetCookieAsIntEx(const name: astr; value: int32; const path, domain, expiry: astr): boo;
var s: astr;
begin
  str(value, s);
  result:= SetCookieEx(name, s, path, domain, expiry);
end;

{ Sets HTTP header }
function SetHeader(const name, value: astr): boo;
{b}                                                                             begin{$ifdef dbug_on}SetHeader_B; {$endif}
  result:= false;
  if headers_sent then begin
    ThrowErr('Can''t set header, headers already sent');                        {$ifdef dbug_on}SetHeader_X1;{$endif}
    exit;
  end;
  // Change value if already exist or add new if not exist
  if not UpdateWebVar(gHdr, name, value, CASE_IGNORE) then begin
    AddWebVar(gHdr, name, value);
    result:= true;
  end;
{e}                                                                             {$ifdef dbug_on}SetHeader_E;{$endif}end;

(* Assigns web variable. i.e. macro variables in templates and formated output
   such as $SomeVar and {$SomeVar} *)
procedure SetVar(const name, value: astr);
{b}                                                                             begin{$ifdef dbug_on}SetVar_B; {$endif}
  // Change value if name already exist, or add new one if not exist
  if not UpdateWebVar(gVar, name, value, CASE_IGNORE) then 
    AddWebVar(gVar, name, value);
{e}                                                                             {$ifdef dbug_on}SetVar_E;{$endif}end;

{ Assigns PWU variable as double precision float }
procedure SetVarAsFloat(const name: astr; value: double);
var s: astr;
begin
  str(value, s);
  SetVar(name, s);
end;

{ Assigns macrovar as integer }
procedure SetVarAsInt(const name: astr; value: int32);
var s: astr;
begin
  str(value, s);
  SetVar(name, s);
end;

{ Throws err if error reporting is on in config settings }
procedure ThrowErr(const s: astr);
begin
  ThrowMsg(s, ttError);
end;

{ Throws warning }
procedure ThrowWarn(const s: astr);
begin
  ThrowMsg(s, ttWarn);
end;


{ Trims (deletes) all bad, unsecure characters from a string.
  i.e. hackers sometimes use pipe characters | or ../../ to try to hack the
  server. Mainly useful for trimming URL variables for malicious attempts.
  Note: see also FilterHTML, which replaces characters with real output such
        as &gt; &quot;

 Default security level: 2 }
function TrimBadChars(const input: astr): astr;
begin
  result:= TrimBad_S(input, SECURE_ON);
end;


{ Swaps all bad, unsecure characters from a string that is being used
  for filenames.  This function is not meant UTF8 or international file systems
  Replaces invalid character with ZERO, to help report malicious attempts. If 
  you see ZERO's in your files on FTP you know someone has been effing around 
  with your server trying to get into your system using special characters.  }
function TrimBadFile(const input: astr): astr;

  { first character in file name must be alphanumeric (no hyphen, space, etc) }
  procedure FirstCharTrim;
  begin
    if not (result[1] in ['a'..'z','A'..'Z','0'..'9','_','.'])
      then result[1]:= '0';
  end;

  procedure ZeroOut;
  const badchars =[
                   #0,     // NULL
                  '/',     // slashes NOT okay. local directory only!
                  '\',     // slashes NOT okay. local directory only!
                  '|',     // pipe character 
                  '#','@','$','!','%','^','&','*','=','`','?',  
                  '"',     // double quote
                  '''',    // single quote
                  '[',     // square bracket open
                  ']',     // square bracket close
                  '>',     // greater than
                  '<',     // less than
                  ',',     // comma
                   ';'     // semicolon not used in files normally
                 ];

  begin
  end;

{b}                                                                             begin{$ifdef dbug_on}TrimBadFile_B; {$endif}
  if length(input) < 1 then exit;
  //    . Dot is okay
  //    ~ Squiggly  character is okay for filenames
  result:= input;
  ZeroOut;
  FirstCharTrim;
{e}                                                                             {$ifdef dbug_on}TrimBadFile_E; {$endif}end;

type  TCharSet = set of char;

procedure ZeroOutChars(var s: astr; badchars: TCharSet); var i: int32;
begin
  if length(s) < 1 then exit;
  for i:= 1 to length(s) do if s[i] in badchars then s[i]:= '0'; 
end;

{ Trims all bad, unsecure characters from a string that is being used
  for a directory. For example, if you are opening a directory or file and
  directory, you will want only characters like A-Z, plus dots, slashes, and
  brackets, but not things like pipe characters and quotations
  This function is not meant UTF8 or international file systems
  Replaces invalid character with ZERO, to help report malicious attempts. If you see ZERO's in your files on FTP you know someone has been fucking around with your server trying to get into your system using special characters.  }
function TrimBadDir(const input: astr): astr;

  { first char in directory should be alphanumeric, dot, or slash (DIRSEP)  }
  procedure TryFirstChar;
  begin
    if length(result) < 1 then exit;
    if not (result[1] in ['a'..'z','A'..'Z','0'..'9', '.', SLASH])
      then result[1]:= '0';
  end;

  const badchars = [ {'.'   Dot okay } {'~'   Squiggly okay }
                    '|',   // pipe 
                    {$ifdef UNIX} // windows slashes on unix not accepted
                     '\',
                    {$endif}
                    {$ifdef WINDOWS} // unix slashes on windows not allowed (therefore http/ftp paths on windows not allowed in this function)
                     '/',
                    {$endif}
                     '#','@','$','!','%','^','&','*','=','`','?',
                     '"',   // double quote
                     '''',  // single quote
                     '[',   // square bracket open
                     ']',   // square bracket close
                     '>',   // greater than
                     '<',   // less than
                     ',',   // comma
                     #0,   // NULL
                     ';'   // semicolon not used in directories normally
                    ];


{b}                                                                             begin{$ifdef dbug_on}TrimBadDir_B; {$endif}
  result:= input;
  { swap bad characters with zero }
  ZeroOutChars(result, badchars);
  TryFirstChar;
{e}                                                                            {$ifdef dbug_on}TrimBadDir_E; {$endif} end;


{ Powers the TrimBadChars function. Replaces bad characters with ZERO which
  makes early problems in web application easy to spot.
  Define security level
    SECURE_ON:  Trims bad (malicious) characters
    OTHERS: future consideration}
function TrimBad_S(const input: astr; security: byte): astr;
const badchars = [#0,   // null char
                  '/','\',  // slashes 
                  '|',      // pipe 
                  '?','$','<','>','#','@','!','^','&','*','=','~','(',')',
                  '[',']', 
                  '%',';', // SQL injection 
                  '`',     // backquote
                  '"',     // double quote
                  ''''     // single quote
                 ];
{b}                                                                             begin{$ifdef dbug_on}TrimBad_S_B;{$endif}
  result:= '';
  if security = SECURE_ON then
  begin
    result:= input;
    ZeroOutChars(result, badchars);  
    result:= substrreplace(result, '--', '00'); // SQL injection double dash
  end;
{e}                                                                             {$ifdef dbug_on}TrimBad_S_E;{$endif}end;


{ Unsets a cookie }
function UnsetCookie(const name: astr): boo;
begin
  result:= UnsetCookieEx(name, '', '');
end;

{ extended }
function UnsetCookieEx(const name, path, domain: astr): boo;
var tmp: TWebVars;  
    i: int32;  
    hdrval: astr;
{b}                                                                             begin{$ifdef dbug_on}UnsetCookieEx_B;{$endif}
  result:= false;
  // header check
  if headers_sent then begin
    ThrowErr('Can''t unset cookie, headers already sent');                      {$ifdef dbug_on}UnsetCookieEx_X1;{$endif}
    exit;
  end;
  // first remove from the list
  setlength(tmp, 0);
  if length(gCook) > 0 then
  for i:= low(gCook) to high(gCook) do if gCook[i].name <> name then begin
    setlength(tmp, length(tmp) + 1);
    tmp[high(tmp)]:= gCook[i];
  end;
  // swap
  gCook:= tmp;
  // process UnsetCookie if domain & path = '' or process UnsetCookieEx if not empty
  if (domain = '') and (path = '') then 
     hdrval:= UrlEncode(name) + '=;path=/;expires=' + EXPIRED_COOKIE
  else
    hdrval:= UrlEncode(name) + '=;path='+path+';domain='+domain+';expires='+EXPIRED_COOKIE;

  AddWebVar(gHdr, 'Set-Cookie', hdrval);
  result:= true;
{e}                                                                             {$ifdef dbug_on}UnsetCookieEx_E;{$endif}end;


{ Removes HTTP header from the list }
function UnsetHeader(const name: astr): boo;
var tmp: TWebVars; 
    i: int32;
{b}                                                                             begin {$ifdef dbug_on}UnsetHeader_B;{$endif}
  result:= false;
  // check
  if headers_sent then begin
    ThrowErr('UnsetHeader: no effect, header already sent');                    {$ifdef dbug_on}UnsetHeader_X1;{$endif}
    exit;
  end;
  // first remove from list
  setlength(tmp, 0);
  if length(gHdr) > 0 then
    for i:= low(gHdr) to high(gHdr) do 
      if Ucase(gHdr[i].name) <> Ucase(name) then begin
        setlength(tmp, length(tmp)+1);
        tmp[high(tmp)]:= gHdr[i];
      end;
  // swap
  gHdr:= tmp;
  result:= true;
{e}                                                                             {$ifdef dbug_on}UnsetHeader_E;{$endif}end;


{ Removes macrovar from list }
procedure UnsetVar(const name: astr);
var tmp: TWebVars;
    i: int32;
{b}                                                                             begin {$ifdef dbug_on}UnsetVar_B;{$endif}
  setlength(tmp, 0);
  if length(gVar) > 0 then for i:= low(gVar) to high(gVar) do 
    if Ucase(gVar[i].name) <> Ucase(name) then begin
      setlength(tmp, length(tmp) + 1); 
      tmp[high(tmp)]:= gVar[i];
    end;
  // swap
  gVar:= tmp;
{e}                                                                             {$ifdef dbug_on}UnsetWebVar_E;{$endif}end;

{ Bytewise XOR encryption }
function XorCrypt(const s: astr; key: byte): astr;
var i, len: int32;
{b}                                                                             begin {$ifdef dbug_on}XorCrypt_B;{$endif}
  result:= '';
  len:= length(s);
  if len > 0 then begin
    setlength(result, len);
    for i:= 1 to len do result[i]:= chr(ord(s[i]) xor key);
  end;
{e}                                                                             {$ifdef dbug_on}XorCrypt_E;{$endif}end;

{ END OF PUBLIC FUNCTIONS/PROCEDURES                                           }
{------------------------------------------------------------------------------}


{--- INITIALIZATION/FINALIZATION ----------------------------------------------}

{$ifdef gzip_on}
// Flush and destroy buffer if output flags are on
procedure FlushDestroyOutputBuf;
begin
  if (out_buffering) and (out_compression) then dispose(OutBuff, Destroy); 
end;

 // Set content length if buffering/compression enabled
 procedure SetGzipContentLength;
 var gzipused: astr; 
 begin
   gzipused:= '';
   if (out_buffering) and (out_compression) and (not headers_sent)
     and (OutBuff^.Used > 0) then
   begin
     OutBuff^.gzip;
     str(OutBuff^.Used, gzipused);
     SetHeader('Content-Length', gzipused);
   end;
 end;
{$endif gzip_on}

{ Below Init function must be called at start of all web progams, and cannot 
  be put in pwmain initialization since it relies on other units initializing 
  first such as addon session and config plugin units }
procedure Init;
begin
 {$ifdef PWUDEBUG}
   // log file 
  pwdebugplugin.DebugInit(debugt, 'pwmain.debug.log');  
 {$endif}
  if plugin_init_called then exit;
  plugin_init_called:= true; // must be here, not at the end of this proc
  if iCustomCfgUnitSet then CustomCfgUnitInit else DefaultCfgInit;
  InitRTI;
  InitBufAndHeaders;
  InitWebData;
  // init cookies
  InitCook; 
  // init sessions
  if iCustomSessUnitSet then CustomSessUnitInit;
end;


procedure LocalFini;
begin
 {$ifdef gzip_on}
  SetGzipContentLength;
 {$endif}
  if not headers_sent then SendHeaders;
 {$ifdef gzip_on}
  FlushDestroyOutputBuf;
 {$endif}
 {$ifdef PWUDEBUG}
  pwdebugplugin.DebugFini(debugt);
 {$endif}
end;

initialization

finalization
  LocalFini;
end.
