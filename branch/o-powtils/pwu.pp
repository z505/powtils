{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

            PSP 1.5.x Merged with Pascal Web Unit project (PWU)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                       
--------------------------------------------------------------------------------
 Main Web Unit
--------------------------------------------------------------------------------

  This unit contains the main functions and procedures for web programs.

--------------------------------------------------------------------------------
 Developer Notes
--------------------------------------------------------------------------------

 PSP 1.5.x
 ---------

  [19/JAN/2006 - L505]
   - changed ifdef for gzip in finalization so that headers are sent even
     without output buffering (bugfix)

  [24/OCT/2005 - L505]

   - Changed the order of substring replace in filterHTML function so that
     pound characters get filtered first, not in the middle, which could cause
     characters to slip through as double or triple filtered.

   - Changed GetWebvar_safehtml to GetCGIvar_safehtml

 
  [23/OCT/2005 - L505]

   - Found possible memory leak/bug in sessions, added an sds free row call

  [18/OCT/2005 - L505]

   - Added TrimBadChars_file and _dir to safely trim all bad characters from
     user input when that input will be in code accessing directories or
     filenames.

  [17/OCT/2005 - L505]

   - Merged PWU with PSP
     
  [15/OCT/2005 - L505]

   - Added GetCGIVar_SafeHTML function. This is used to get variables and turn
     them into filtered html automatically. Default security level on this
     function is level 2. To bypass default security, use GetCGIVar_SF
     The _SafeHTML prefix stands for "Safe HTML". Special characters are
     filtered for you into HTML equivilents. The SF suffix stands for
     "specify filter" and security settings.

  [8/OCT/2005 - L505]

   -Added pwuEnvVar.pp unit to project which allows easy access to CGI
    environment variables, such as ScriptName, UserAgent, RemoteAddr (IP)

  [7/OCT/2005 - L505]

   -Added WebWriteLnFF, WebWriteFF, WebFormat_SF, WebWriteF_Fi, WebWriteLnF_Fi
    functions. Created a security.txt file in the /src/conf/ directory

   -Version 1.0b released:
     more functions, minor updates, and more examples included.


  [6/OCT/2005 - L505]

   -The _S suffix has been added to some functions, so we can now specify
    security we want to use. _S suffix stands for "specify security"

     - FilterHtml has 2 security levels, with defaulted at level 2. To bypass
       the default, use FilterHTML_S(input, SecureLevel)

     - GetWebVar has 2 security levels (plus level 0), defaulted at level 2.
       To bypass the default, use GetWebVar_S(input, SecureLevel)

     - TrimBadChar has have 2 security settings, defaulted at level 2. To bypass
       the default, use TrimBadChar_S(input, SecureLevel)

     - GetCGIVar has 2 security levels (plus level 0) defaulted at level 2. To
       bypass the default, use GetCGIVar_S(input, SecureLevel)


  [5/OCT/2005 -L505]

    -Modified, converted, and refactored entire PSP project into the PWU project.
     PWU is derived from the PSP project.

    -PWU Version 1.0a released: first release.
    

 PSP 1.4.3
 ---------

  -notes are missing. Please fill in from previous file or archive them.

 PSP 1.4.2
 ---------

  -notes are missing. Please fill in from previous file or archive them.
 

 PSP 1.4.1
 ---------
 
  [22.09.2005 - Trustmaster]:

   - changed unit interface, improved error reporting and made it more verbose.

  [20.09.2005 - Trustmaster]:

   - improved GZIP content encoding support.

  [07.SEP.2005 - L505]:

   - added web_trim_badchar functions

  [27.08.05 - Trustmaster]:

   - lots of debugging. But I'm not sure if that was all.

  [26.08.05 - Trustmaster]:

   - got new web unit compiled and working. 95% of code is new.


--------------------------------------------------------------------------------
  Developer Todo
--------------------------------------------------------------------------------

  [L505]:
   -Trimming null character. Is it okay to trim the null character or does
    it screw up ansistrings? Search source code for areas in question.

   -Trim functions may be moved to another unit, such as a filtering/trimming
    unit specifically for those type of functions.
    
   -security for CGI environment variables incase user injects javascript or
    other malicious junk into a HTTP_USER_AGENT string or SCRIPT_NAME
    
--------------------------------------------------------------------------------
  Authors/Credits:
--------------------------------------------------------------------------------
  -PSP Project
  -Vladimir Sibirov
  -L505 


  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
}


 {$IFDEF WIN32}{$LINKLIB kernel32}{$ENDIF}{$IFDEF UNIX}{$LINKLIB c}{$ENDIF}


// DEFAULT: ON
 {$DEFINE EXTRA_SECURE}
// take this off to save some kilobytes (kb)
// turn it on if you want strong security (check as many overflows as possible)
// DEFAULT: on


// DEFAULT: ON
 {$DEFINE GZIP_ENABLED}
// take this off to save some kilobytes (kb)
// turn it on if you plan to use GZIP in PWU.conf


// Comment the above DEFINES out and use the -d compiler
// option if you want more control
//
// -dEXTRA_SECURE FPC compiler argument
// -dGZIP_ENABLED FPC compiler argument


{$IFDEF FPC}{$H+}{$MODE OBJFPC}
  {$IFDEF EXTRA_SECURE}
   {$R+}{$Q+}{$CHECKPOINTER ON}
  {$ENDIF}
{$ENDIF}


unit pwu;

{------------------------------------------------------------------------------}
interface
{------------------------------------------------------------------------------}

const

  // Sets PWU configuration file name.
  PWU_CONFIG_PATH = 'PWU.conf';

  // Sets session file name
  PWU_SESS_PATH = 'PWUsess.sds';

  // System-wide configuration directory path on *NIX
  PWU_SYSCONF_PATH = '/etc/';

  // Supply PWU version
  PWU_VERSION = '1.5.1';


type
  StrArray = array of string;



{------------------------------------------------------------------------------}
{--- Main Procedures/Functions ------------------------------------------------}
{------------------------------------------------------------------------------}


 {-- CGI Variable Functions --}
function CountCGIVars: longword;
function GetCGIVar(const name: string): string;
function GetCGIVar_S(const name: string; const SecureLevel: integer): string;
function GetCGIVarAsFloat(const name: string): double;
function GetCGIVarAsInt(const name: string): longint;
function GetCGIVar_SafeHTML(const name: string): string;
function FetchCGIVarName(index: longword): string;
function FetchCGIVarValue(index: longword): string;
function IsCgiVar(const name: string): boolean;
function GetCGIVar_SF(const name: string; const SecureLevel: integer): string;


 {-- Cookie Functions --}
function CountCookies: longword;
function FetchCookieName(index: longword): string;
function FetchCookieValue(index: longword): string;
function GetCookie(const name: string): string;
function GetCookieAsFloat(const name: string): double;
function GetCookieAsInt(const name: string): longint;
function IsCookie(const name: string): boolean;
function SetCookie(const name, value: string): boolean;
function SetCookieAsFloat(const name: string; value: double): boolean;
function SetCookieAsInt(const name: string; value: longint): boolean;
function SetCookieEx(const name, value, path, domain, expires: string): boolean;
function SetCookieAsFloatEx(const name: string; value: double; const path, domain, expires: string): boolean;
function SetCookieAsIntEx(const name: string; value: longint; const path, domain, expires: string): boolean;
function UnsetCookie(const name: string): boolean;
function UnsetCookieEx(const name, path, domain: string): boolean;


 {-- Config Functions --}
function CountWebConfigVars: longword;
function FetchWebConfigVarName(index: longword): string;
function FetchWebConfigVarValue(index: longword): string;
function GetWebConfigVar(const name: string): string;
function IsWebConfigVar(const name: string): boolean;
function SetWebConfigVar(const name, value: string): boolean;


 {-- Environment Variable Functions --}
function CountEnvVars: longword;
function FetchEnvVarName(index: longword): string;
function FetchEnvVarValue(index: longword): string;
function GetEnvVar(const name: string): string;
function IsEnvVar(const name: string): boolean;
function SetEnvVar(const name, value: string): boolean;


 {-- Filtering Functions --}
function FilterHTML(const input: string): string;
function FilterHTML_S(const input: string; const SecureLevel: integer): string;
function TrimBadChars(const input: string): string;
function TrimBadChars_file(const input: string): string;
function TrimBadChars_dir(const input: string): string;
function TrimBadChars_S(const input: string; const SecureLevel: integer): string;


 {-- Header Functions --}
function CountWebheaders: longword;
function FetchWebHeaderName(index: longword): string;
function FetchWebHeaderValue(index: longword): string;
function GetWebHeader(const name: string): string;
function IsWebHeader(const name: string): boolean;
function SetWebHeader(const name, value: string): boolean;
function UnsetWebHeader(const name: string): boolean;
function PutWebHeader(const header: string): boolean;


 {-- Output/Write Out Functions/Procedures --}
procedure WebWrite(const str: string);
procedure WebWriteF(const str: string);
procedure WebWriteFF(const str: string);
procedure WebWriteF_Fi(const str: string; const FilterTheHTML: boolean);
procedure WebWriteLn(const str: string);
procedure WebWriteLnF(const str: string);
procedure WebWriteLnFF(const str: string);
procedure WebWriteLnF_Fi(const str: string; const FilterTheHTML: boolean );
procedure WebFileOut(const fname: string);
procedure WebResourceOut(const fname: string);
procedure WebTemplateOut(const fname: string; const FilterHTMLOption: boolean);
function WebFormat(const str: string): string;
function WebFormatAndFilter(const str: string): string;
function WebFormat_SF(const str: string;
                      const filterHTMLOption: boolean;
                      const FilterSecureLevel,
                            TrimSecureLevel: integer): string;


 {-- RTI Functions --}
function CountRTIVars: longword;
function FetchRTIName(index: longword): string;
function FetchRTIValue(index: longword): string;
function GetRTI(const name: string): string;
function GetRTIAsFloat(const name: string): double;
function GetRTIAsInt(const name: string): longint;
function IsRTI(const name: string): boolean;


 {-- Session Functions --}
function CountSessVars: longword;
function FetchSessName(index: longword): string;
function FetchSessValue(index: longword): string;
function GetSess(const name: string): string;
function GetSessAsFloat(const name: string): double;
function GetSessAsInt(const name: string): longint;
function IsSess(const name: string): boolean;
function SessDestroy: boolean;
function SetSess(const name, value: string): boolean;
function SetSessAsFloat(const name: string; value: double): boolean;
function SetSessAsInt(const name: string; value: longint): boolean;
function UnsetSess(const name: string): boolean;


 {-- Upload File Functions --}
function FetchUpfileName(index: longword): string;
function GetUpFileName(const name: string): string;
function GetUpFileSize(const name: string): longint;
function GetUpFileType(const name: string): string;
function CountUpFiles: longword;
function IsUpFile(const name: string): boolean;
function SaveUpFile(const name, fname: string): boolean;


 {-- Web Variable Functions/Procedures --}
function CountWebVars: longword;
function FetchWebVarName(index: longword): string;
function FetchWebVarValue(index: longword): string;
function GetWebVar(const name: string): string;
function GetWebVar_S(const name: string; const SecureLevel: integer): string;
function GetWebVarAsFloat(const name: string): double;
function GetWebVarAsInt(const name: string): longint;
procedure SetWebVar(const name, value: string);
procedure SetWebVarAsFloat(const name: string; value: double);
procedure SetWebVarAsInt(const name: string; value: longint);
function IsWebVar(const name: string): byte;
procedure UnsetWebVar(const name: string);


 {-- Utility/Tools Functions --}
function LineEndToBR(const str: string): string;
function RandomStr(len: longint): string;
function XORCrypt(const str: string; key: byte): string;


 {-- Error Functions --}
function ThrowWebError(const message: string): boolean;


// END OF PUBLIC FUNCTION/PROCEDURE DECLARATIONS
{------------------------------------------------------------------------------}


{------------------------------------------------------------------------------}
implementation
{------------------------------------------------------------------------------}

uses
  base64enc,
  sds,
  substrings,
  sysutils,
  urlenc
 {$IFDEF GZIP_ENABLED}
 ,gzip
 {$ENDIF};


type
  // Associative variable
  Web_TVariable = record
    name, value: string;
  end;

  // Variables associative array
  Web_TVariables = array of Web_TVariable;

  Web_PVariables = ^Web_TVariables;

type
  // Type of uploaded file
  Web_TUpFile = record
    name, filename, data, content_type: string;
    size: longint;
  end;

  // Type to store all uploaded files
  Web_TUpFiles = array of Web_TUpFile;

  // Pointer to file data
  Web_PUpFiles = ^Web_TUpFiles;


type
  // Line type for Multipart/Form-Data handling functions
  MP_Line = array[1..6] of string;

  // Multipart/Form-Data form type
  MP_Form = array of string;

  // Pointer to MP_Form
  MP_PForm = ^MP_Form;


var
  cgi,  // CGI GET/POST data
  conf, // Configuration data
  cook, // Cookie data
  env,  // Enironment data
  hdr,  // Headers
  rti,  // Run Time Information
  sess, // Session data
  vars: Web_TVariables; // PWU assign data
  upf: Web_TUpFiles;    // Uploaded files storage
  ob: string;   // Output buffer
  headers_sent, // Headers sent flag
  session_registered: boolean; // Session registered flag
 {$IFDEF GZIP_ENABLED}
  output_buffering, output_compression, // Output buffering and compression flags
 {$ENDIF}
  error_reporting, error_halt: boolean; // Error reporting flags

  //dh: text; // Debug output


procedure SetRTI(const name, value: string); forward;
function SessUpdate: boolean; forward;


{------------------------------------------------------------------------------}
{--- SYSAPI FUNCTIONS ---------------------------------------------------------}
{------------------------------------------------------------------------------}
{$IFDEF WIN32}
function GetEnvironmentStrings: pchar; stdcall; external 'kernel32' name 'GetEnvironmentStringsA';
function FreeEnvironmentStrings(p: pchar) : longbool; stdcall; external 'kernel32' name 'FreeEnvironmentStringsA';
function SetEnvironmentVariable(const lpszName, lpszValue: pchar): boolean; stdcall; external 'kernel32' name 'SetEnvironmentVariableA';
{$ENDIF}

{$IFDEF UNIX}
var environ: ppchar; cvar; external;
function getenv(const name: PChar): PChar; cdecl; external 'c' name 'getenv';
function setenv(const name, value: pchar; replace: longint): longint; cdecl; external 'c' name 'setenv';
function unsetenv(const name: pchar): longint; cdecl; external 'c' name 'unsetenv';
{$ENDIF}

{------------------------------------------------------------------------------}


{------------------------------------------------------------------------------}
{--- PRIVATE FUNCTIONS/PROCEDURES ---------------------------------------------}
{------------------------------------------------------------------------------}

// Some default header definitions on startup
procedure InitWebHeaders;
begin
  SetLength(hdr, 2);
  hdr[0].name:= 'X-Powered-By';
  hdr[0].value:= 'PWU/' + PWU_VERSION;
  hdr[1].name:= 'Content-Type';
  hdr[1].value:= 'text/html; charset=' + GetWebConfigVar('header_charset');
 {$IFDEF GZIP_ENABLED}
  if output_buffering
    and output_compression
      and substr_exists(GetEnvVar('HTTP_ACCEPT_ENCODING'), 'gzip') then
  begin
    SetLength(hdr, 3);
    hdr[2].name:= 'Content-Encoding';
    hdr[2].value:= 'gzip';
  end;
 {$ENDIF GZIP_ENABLED}
end;


// Some default RTI definitions on startup
procedure InitRTI;
begin
  SetLength(rti, 2);
  rti[0].name:= 'HEADERS_SENT';
  rti[0].value:= 'FALSE';
  rti[1].name:= 'ERRORS';
  rti[1].value:= '0';
end;


// Loads up system environment
function LoadEnvVar: boolean;
var
{$IFDEF WIN32}
    p, hp: pchar;
{$ENDIF}
{$IFDEF UNIX}
    p: ppchar;
{$ENDIF}
    s: string;
    i: longint;
begin
  result:= false;
  {$IFDEF WIN32}
  p:= GetEnvironmentStrings;
  hp:= p;
  while hp^ <> #0 do
  {$ENDIF}
  {$IFDEF UNIX}
  p:= environ;
  while (p^ <> nil) and (p^ <> #0) do
  {$ENDIF}
  begin
    {$IFDEF WIN32}s:= AnsiString(hp);{$ENDIF}
    {$IFDEF UNIX}s:= AnsiString(p^);{$ENDIF}
    i:= substr_pos(s, '=');
    SetLength(env, length(env) + 1);
    // Parsing as name=value
    env[length(env) - 1].name:= upcase(copy(s, 1, i - 1));
    env[length(env) - 1].value:= copy(s, i + 1, length(s) - i);
    // Next entry
    {$IFDEF WIN32}hp:= hp + strlen(hp) + 1;{$ENDIF}
    {$IFDEF UNIX}inc(p);{$ENDIF}
  end;
  {$IFDEF WIN32}
  FreeEnvironmentStrings(p);
  {$ENDIF}
  result:= true;
end;


// Finds the configuration file and parses it into conf
function ParseWebConfig: boolean;
var
  CONFIG_PATH,
  SYS_PATH,
  buff,
  name,
  value: string;
  fh: text;
  i: longint;
const
  {$IFDEF WIN32}
  PrevDir = '..\';   //previous directory
  {$ENDIF}
  {$IFDEF UNIX}
  PrevDir = '../';
  {$ENDIF}

begin
  result:= false;
  // Need path to System directory
  {$IFDEF WIN32}
  sys_path:= GetEnvVar('WINDIR') + '\';
  {$ENDIF}
  {$IFDEF UNIX}
  sys_path:= PWU_SYSCONF_PATH;
  {$ENDIF}

  // First search for config file in current directory
  if FileExists(PWU_CONFIG_PATH) then
    CONFIG_PATH:= PWU_CONFIG_PATH
    // Try the parent directory
  else
    if FileExists(PrevDir{../} + PWU_CONFIG_PATH) then
      CONFIG_PATH:= PrevDir{../} + PWU_CONFIG_PATH
  // Try system-wide
  else
    if FileExists(sys_path + PWU_CONFIG_PATH) then
      CONFIG_PATH:= sys_path + PWU_CONFIG_PATH
  // We won't go further without the config file
  else
    exit(false);


  // Opening
  assign(fh, CONFIG_PATH);
  reset(fh);

  // Parsing
  while not eof(fh) do
  begin
    readln(fh, buff);
    // Emtpy lines are ignored
    if buff = '' then continue;
    // All comment lines start with # only
    if buff[1] = '#' then continue;
    i:= substr_pos(buff, '=');
    name:= copy(buff, 1, i - 1);
    value:= copy(buff, i + 1, length(buff) - i);
    name:= str_trim(name);
    value:= substr_strip(str_trim(value), '"');
    if (name = '') or (value = '') then continue;
    SetLength(conf, length(conf) + 1);
    conf[length(conf) - 1].name:= name;
    conf[length(conf) - 1].value:= value;
  end;
  close(fh);

  // Setting program flags
  {$IFDEF GZIP_ENABLED}
  if lowercase(GetWebConfigVar('output_buffering')) = 'on' then
    output_buffering:= true else output_buffering:= false;
  if lowercase(GetWebConfigVar('output_compression')) = 'on' then
    output_compression:= true else output_compression:= false;
  {$ENDIF}

  if lowercase(GetWebConfigVar('error_reporting')) = 'on' then
    error_reporting:= true else error_reporting:= false;
  if lowercase(GetWebConfigVar('error_halt')) = 'on' then
    error_halt:= true else error_halt:= false;

  // Done
  result:= true;
end;



// Sends HTTP headers
function SendWebHeaders: boolean;
var
  i: longword;
begin
  result:= false;
  // Check if sent
  if headers_sent then exit(false);
  // Update session
  SessUpdate;
  // Send
  if length(hdr) > 0 then
  for i:= 0 to length(hdr) - 1 do
    writeln(hdr[i].name + ': ' + hdr[i].value);
  writeln;
  // Update RTI
  headers_sent:= true;
  SetRTI('HEADERS_SENT', 'TRUE');
  // Done
  result:= true;
end;




// Flushes output buffer
function FlushBuffer: boolean;
begin
  result:= false;
  // Check if headers are sent
  if not headers_sent then SendWebHeaders;
  // Flushing
  write(ob);
  // Done
  result:= true;
end;




// Dumps vars into cgi
function PutCGIVars(const data: string): boolean;
var
  i,
  len,
  cnt: longword;
  lex: string;
begin
  // Init
  result:= false;
  i:= 1;
  cnt:= 0;
  len:= length(data);
  if len = 0 then exit(true);
  if data[1] = '\' then inc(i);
  // Parse out
  while (i <= len) do
  begin
    // New item
    SetLength(cgi, cnt + 1);
    // Getting name
    lex:= '';
    while (i <= len) and (data[i] <> '=') do
    begin
      SetLength(lex, length(lex) + 1);
      lex[length(lex)]:= data[i];
      inc(i);
    end;
    cgi[cnt].name:= url_decode(lex);
    inc(i);
    // Getting value
    lex:= '';
    while (i <= len) and (data[i] <> '&') do
    begin
      SetLength(lex, length(lex) + 1);
      lex[length(lex)]:= data[i];
      inc(i);
    end;
    cgi[cnt].value:= url_decode(lex);
    inc(i);
    // Increasing counter
    inc(cnt);
  end;
  // Done
  result:= true;
end;


// Dumps vars from multipart/form-data into cgi and upf
// OLD NOTE: Left as-is in 1.3.3 (it must be totally rewritten otherwise)
procedure MP_FormSplit(var data: PString; const boundary: string; var form: MP_PForm);
// Splits the form into items
var
  separator: string;
  ptr,
  len,
  len2: longint;
begin
  separator:= '--' + boundary + #13 + #10;
  len2:= length(separator);
  // Cutting off last boundary
  len:= substr_pos(data^, '--' + boundary + '--');
  data^:= copy(data^, 1, len-1);
  // Cutting off first boundary
  delete(data^, 1, len2);
  while len > 0 do
  begin
    len:= length(data^);
    ptr:= substr_pos(data^, separator);
    if ptr <> 0 then
    begin
      // Not last item
      SetLength(form^, length(form^) + 1);
      form^[length(form^) - 1]:= copy(data^, 1, ptr - 2);
      // Cutting this item and next boundary
      delete(data^, 1, ptr + len2 - 1);
    end
      else
    begin
      // Last item
      SetLength(form^, length(form^) + 1);
      form^[length(form^) - 1]:= copy(data^, 1, len-1);
      break;
    end;
  end;
end;


// Extracts current line beginning from ptr and ending with #13#10
function MP_GetLine(data: PString; var ptr: longint): string;
var
  s: string;
begin
  result:= '';
  repeat
    s:= copy(data^, ptr, 1);
    if (s <> #13) and (s <> #10) then result:= result + s;
    inc(ptr);
  until (s = #13) or (s = #10);
  inc(ptr);
end;


// Splits string by space. Max. result = 6 strings.
function MP_SplitLine(line: string): MP_Line;
var
  cnt, elem, len: integer;
  s: string;
  quoted: boolean;
begin
  result[1]:= '';
  result[2]:= '';
  result[3]:= '';
  result[4]:= '';
  result[5]:= '';
  result[6]:= '';
  elem:= 1;
  len:= length(line);
  quoted:= false;
  cnt:= 1;
  for cnt:= 1 to len do
  begin
    s:= copy(line, cnt, 1);
    if (s='"') and (not quoted) then quoted:= true;
    if (s='"') and quoted then quoted:= false;
    if (s<>' ') and (s<>'=') and (s<>';') and (s<>'"') and (s<>':') then result[elem]:= result[elem] + s;
    if ((s=' ') or (s=';') or (s=':') or (s='=')) and quoted then result[elem]:= result[elem] + s;
    if ((s=';') or (s='=') or (s=':')) and (not quoted) then inc(elem);
  end;
end;


// Extracts data boundary from content-type string
function MP_GetBoundary(const content_type: string): string;
var
  len: integer;
begin
  len:= substr_pos(Content_Type, '=');
  result:= copy(content_type, len + 1, length(content_type)-len);
  if substr_pos(result, '"') = 1 then result:= copy(result, 2, length(result) - 2);
end;

procedure PutCGImpVars(data: PString; const content_type: string);
var
  cnt,
  ptr,
  tmp,
  len,
  dpos: longint;
  buff, boundary: string;
  line: MP_Line;
  form: MP_PForm;
begin
  New(form);
  boundary:= MP_GetBoundary(content_type);
  MP_FormSplit(data, boundary, form);
  for cnt:= 0 to (length(form^) - 1) do
  begin
    ptr:= 1;
    len:= length(form^[cnt]);
    dpos:= substr_pos(form^[cnt], #13 + #10 + #13 + #10) + 4;
    // Getting first line
    buff:= MP_GetLine(@(form^[cnt]), ptr);
    // Splitting into words
    line:= MP_SplitLine(buff);
    // Is it file or variable?
    if substr_pos(buff, 'filename') <> 0 then
    begin
      // It is a file
      SetLength(upf, length(upf) + 1);
      upf[length(upf) - 1].name:= line[4];
      upf[length(upf) - 1].filename:= line[6];
      // Getting content type
      buff:= MP_GetLine(@(form^[cnt]), ptr);
      line:= MP_SplitLine(buff);
      upf[length(upf) - 1].content_type:= line[2];
      // Getting value till the end
      upf[length(upf) - 1].size:= len - dpos;
      upf[length(upf) - 1].data:= copy(form^[cnt], dpos, upf[length(upf) - 1].size);
    end
    else
    begin
      // It is a variable
      SetLength(cgi, length(cgi) + 1);
      cgi[length(cgi) - 1].name:= line[4];
      // Getting value till the end
      tmp:= len - dpos;
      cgi[length(cgi) - 1].value:= copy(form^[cnt], dpos, tmp);
    end;
  end;
  Dispose(form);
end;


// Dumps vars into cook
function PutCookieVars(const data: string): boolean;
var i, len, cnt: longword;
   lex: string;
begin
  // Init
  result:= false;
  i:= 1;
  cnt:= 0;
  len:= length(data);
  if data[1] = '\' then inc(i);
  // Parse out
  while (i <= len) do
  begin
    // New item
    SetLength(cook, cnt + 1);
    // Getting name
    lex:= '';
    while (i <= len) and (data[i] <> '=') do
    begin
      SetLength(lex, length(lex) + 1);
      lex[length(lex)]:= data[i];
      inc(i);
    end;
    cook[cnt].name:= url_decode(lex);
    inc(i);
    // Getting value
    lex:= '';
    while (i <= len) and (data[i] <> ';') do
    begin
      SetLength(lex, length(lex) + 1);
      lex[length(lex)]:= data[i];
      inc(i);
    end;
    cook[cnt].value:= url_decode(lex);
    inc(i);
    // Ignoring spaces
    while (i <= len) and (data[i] = ' ') do
      inc(i);
    // Increasing counter
    inc(cnt);
  end;
  // Done
  result:= true;
end;


// Dumps vars into sess
function PutSessVars(const data: string): boolean;
var
  i,
  len,
  cnt: longword;
  lex: string;
begin
  // Init
  result:= false;
  i:= 1;
  cnt:= 0;
  len:= length(data);
  // Parse out
  while (i <= len) do
  begin
    // New item
    SetLength(sess, cnt + 1);
    // Getting name
    lex:= '';

    while (i <= len) and (data[i] <> '=') do
    begin
      SetLength(lex, length(lex) + 1);
      lex[length(lex)]:= data[i];
      inc(i);
    end;
    sess[cnt].name:= url_decode(lex);
    inc(i);
    // Getting value
    lex:= '';

    while (i <= len) and (data[i] <> ';') do
    begin
      SetLength(lex, length(lex) + 1);
      lex[length(lex)]:= data[i];
      inc(i);
    end;

    sess[cnt].value:= url_decode(lex);
    inc(i);
    // Increasing counter
    inc(cnt);
  end;
  // Done
  result:= true;
end;


// Session garbage collector
function SessGC: boolean;
var
  sess_table,
  sess_lim: string;
  res: SDS_Result;
  sess_time: longword;
  limdate: TDateTime;
begin
  result:= false;
  // Checking
  sess_table:= GetWebConfigVar('session_path');
  if not FileExists(sess_table) then
  begin
    // Searching in system temp
    if FileExists({$IFDEF WIN32}GetEnvVar('WINDIR') + '\'{$ENDIF}{$IFDEF UNIX}'/tmp/'{$ENDIF}+ PWU_SESS_PATH) then
      sess_table:= {$IFDEF WIN32}GetEnvVar('WINDIR') + '\'{$ENDIF}{$IFDEF UNIX}'/tmp/'{$ENDIF}+ PWU_SESS_PATH
    else
      exit(false);
  end;
  
  // Checking lifetime in minutes
  val(GetWebConfigVar('session_life_time'), sess_time);
  if sess_time = 0 then
    exit(true);
  limdate:= now - (sess_time / 1440);
  sess_lim:= FormatDateTime('yyyy-mm-dd hh:nn:ss', limdate);
  // Performing GC
  res:= sds_query('DELETE FROM `' + sess_table + '` WHERE modified < "' + sess_lim + '"');
  if sds_result_error(res) = '' then
    result:= true;
  sds_free_result(res);
end;


// Gets session data
function SessStart: string;
var
  sess_table: string;
  res: SDS_Result;
  row: SDS_Array;
  key, sid: string;
begin
  // Init
  result:= '';
  // Checking path
  sess_table:= GetWebConfigVar('session_path');
  if not FileExists(sess_table) then
  begin
    // Searching in system temp
    if FileExists({$IFDEF WIN32}GetEnvVar('WINDIR') + '\'{$ENDIF}{$IFDEF UNIX}'/tmp/'{$ENDIF}+ PWU_SESS_PATH) then
      sess_table:= {$IFDEF WIN32}GetEnvVar('WINDIR') + '\'{$ENDIF}{$IFDEF UNIX}'/tmp/'{$ENDIF}+ PWU_SESS_PATH
    else
      exit('');
  end;
  // Running garbage collector
  SessGC;
  // Is it registered
  if not IsCookie('PWUSESS') then
  begin
    session_registered:= false;
    SetRTI('SESSION_REGISTERED', 'FALSE');
    exit('');
  end;
  session_registered:= true;
  SetRTI('SESSION_REGISTERED', 'TRUE');
  key:= base64_decode(GetCookie('PWUSESS'));
  sid:= sds_escape(copy(key, 13, length(key) - 12));
  key:= sds_escape(copy(key, 1, 12));
  // Selecting
  res:= sds_query('SELECT data FROM `' + sess_table + '` WHERE id = ' + sid + ' AND key = "' + key + '"');
  if sds_result_rows(res) = 1 then
  begin
    row:= sds_fetch_row(res);
    result:= base64_decode(sds_fetch_column(row, 0));
    sds_free_row(row);
  end
    else
  begin
    result:= '';
    // Unset the cookie, it has timed out
    UnsetCookie('PWUSESS');
  end;
  sds_free_result(res);
end;


// Updates session table due to sess or registers a new session
function SessUpdate: boolean;
var
  sess_table, data: string;
  res: SDS_Result;
  id, i: longword;
  key, sid: string;
  registered: boolean;
begin
  if length(sess) = 0 then exit(true);
  // Init
  result:= false;
  sess_table:= GetWebConfigVar('session_path');
  if not FileExists(sess_table) then
  begin
    // Searching in system temp
    if FileExists({$IFDEF WIN32}GetEnvVar('WINDIR') + '\' {$ENDIF}
                  {$IFDEF UNIX}'/tmp/'{$ENDIF} + PWU_SESS_PATH)
                    or (sess_table = '') then
      sess_table:= {$IFDEF WIN32}GetEnvVar('WINDIR') + '\'{$ENDIF}{$IFDEF UNIX}'/tmp/'{$ENDIF}+ PWU_SESS_PATH;
  end;
  // Create session table if not exists
  if not FileExists(sess_table) then
  begin
    res:= sds_query('CREATE TABLE `' + sess_table + '` (id INT, key TEXT, data TEXT, modified DATETIME)');
    if sds_result_rows(res) < 1 then
      ThrowWebError('Could not create session table, SDS returned - ' + sds_result_error(res));
    sds_free_result(res);
  end;
  // Depending on whether session is registered
  if IsCookie('PWUSESS') then
  begin
    key:= base64_decode(GetCookie('PWUSESS'));
    sid:= sds_escape(copy(key, 13, length(key) - 12));
    key:= sds_escape(copy(key, 1, 12));
    // Selecting
    res:= sds_query('SELECT COUNT(*) FROM `' + sess_table + '` WHERE id = ' + sid + ' AND key = "' + key + '"');
    if sds_result_rows(res) = 1 then registered:= true else
    begin
      registered:= false;
      // Unset the cookie
      UnsetCookie('PWUSESS');
    end;
    sds_free_result(res);
  end else
    registered:= false;
  // Generate new data string
  data:= '';
  if length(sess) > 0 then
  for i:= 0 to length(sess) - 1 do
      data:= data + url_encode(sess[i].name) + '=' + url_encode(sess[i].value) + ';';
  // Strip tail ;
  data:= copy(data, 1, length(data) - 1);
  data:= base64_encode(data);
  if registered then
  begin
    // Updating
    res:= sds_query('UPDATE `' + sess_table +
                        '` SET data = "' + sds_escape(data) + '", ' +
                              'modified = NOW WHERE id = ' + sid +
                                              ' AND key = "' + key + '"');
    if sds_result_rows(res) = 1 then result:= true;
    sds_free_result(res);
  end
    else
  begin
    // Check headers
    if headers_sent then
    begin
      ThrowWebError('Can not register new session - headers are already sent');
      exit(false);
    end;
    // Creating new one
    key:= RandomStr(12);
    res:= sds_query('INSERT INTO `' + sess_table +
                          '` (key, data, modified) ' +
                     'VALUES ("' + key + '", "' +
                                   sds_escape(data) + '", ' +
                                   'NOW' +')');
    if sds_result_rows(res) = 1 then
    begin
      id:= sds_last_id(sess_table);
      str(id, sid);
      key:= base64_encode(key + sid);
      SetCookie('PWUSESS', key);
      SetRTI('SESSION_REGISTERED', 'TRUE');
    end else
      result:= false;
    sds_free_result(res);
  end;
end;


// Is responsible for getting GET, POST, COOKIE and SESSION data
function GetWebData: boolean;
var
  method,
  ctype,
  data: string;
  upl_max_size,
  cont_len,
  cnt: longword;
begin
  result:= false;
  // First getting method data
  method:= GetEnvVar('REQUEST_METHOD');
  if method = 'POST' then
  begin
    // Getting data from stdin
    data:= '';
    val(GetWebConfigVar('upload_max_size'), upl_max_size);
    upl_max_size:= upl_max_size * 1048576;
    val(GetEnvVar('CONTENT_LENGTH'), cont_len);
    if cont_len > upl_max_size then cont_len:= upl_max_size;
       SetLength(data, cont_len);
       for cnt:= 1 to cont_len do read(data[cnt]);
    // Depending on content type
    ctype:= GetEnvVar('CONTENT_TYPE');
    if substr_pos(lowercase(ctype), 'application/x-www-form-urlencoded') > 0 then
    begin
      PutCGIVars(data);
    end else
      if substr_pos(lowercase(ctype), 'multipart/form-data') > 0 then
      begin
        PutCGImpVars(@data, ctype);
      end;
  end else
    if method = 'GET' then
    begin
      data:= GetEnvVar('QUERY_STRING');
      PutCGIVars(data);
    end;

  // Get cookies
  if IsEnvVar('HTTP_COOKIE') then
  begin
    data:= GetEnvVar('HTTP_COOKIE');
    PutCookieVars(data);
  end;

  // Get session
  data:= SessStart;
  if data <> '' then PutSessVars(data);

  // Done
  result:= true;
end;


// Sets Run Time Information variable
procedure SetRTI(const name, value: string);
var
  i: longword;
begin
  if length(rti) > 0 then
    for i:= 0 to length(rti) - 1 do if rti[i].name = name then
    begin
      rti[i].value:= value;
      break;
    end;
  SetLength(rti, length(rti) + 1);
  rti[length(rti) - 1].name:= name;
  rti[length(rti) - 1].value:= value;
end;


{$IFDEF GZIP_ENABLED}
// Entire buffer compression. Slow, huge but efficient.
procedure DoWebCompress;
begin
  // Check if browser supports that
  if not substr_exists(GetEnvVar('HTTP_ACCEPT_ENCODING'), 'gzip') then exit;
  // Set encoding header
  // Compress the buffer
  ob:= gzip_gzbuffer(ob);
end;
{$ENDIF GZIP_ENABLED}


// END OF PRIVATE FUNCTIONS/PROCEDURES
{------------------------------------------------------------------------------}






{------------------------------------------------------------------------------}
{--- PUBLIC FUNCTIONS/PROCEDURES ----------------------------------------------}
{------------------------------------------------------------------------------}

// Returns number of elements in the cgi var list
function CountCGIVars: longword;
begin
  result:= length(cgi);
end;


// Returns number of configuration variables
function CountWebConfigVars: longword;
begin
  result:= length(conf);
end;


// Returns number of cookie variables
function CountCookies: longword;
begin
  result:= length(cook);
end;


// Returns number of elements in the environment
function CountEnvVars: longword;
begin
  result:= length(env);
end;


// Returns number of set headers
function CountWebHeaders: longword;
begin
  result:= length(hdr);
end;


// Returns number of Run-Time Information variables
function CountRTIVars: longword;
begin
  result:= length(rti);
end;


// Returns number of session variables
function CountSessVars: longword;
begin
  result:= length(sess);
end;


// Returns number of session variables
function CountUpFiles: longword;
begin
  result:= length(upf);
end;


// Returns number of all PWU variables
function CountWebVars: longword;
begin
  result:= length(vars);
end;


// Replaces special characters with their HTML equivalents
// If you are taking input on a guestook or forum for example, you will want to
// use FilterHTML
//
// Default security level: 2
function FilterHTML(const input: string): string;
begin
  result:= FilterHTML_S(input, 2);
end;


// Powers the FilterHTML function, here with ability to define security level
//
// Security level 1:
//   Replaces special characters with their HTML equivalents.
//   This one does not filter { or } or $ if you are for example working with
//   templates, because you need those characters.
//
// Security level 2:
//   Similar to level 1, but more filtering of malicious input variable
//   attempts. This filter replaces the special template characters, so if you
//   want your templates to come through use FilterHTML_1
function FilterHTML_S(const input: string; const SecureLevel: integer): string;
begin

  if SecureLevel = 1 then
  begin
    result:= substr_replace(input, '&', '&amp;');
    result:= substr_replace(result, '#', '&#35;');
    result:= substr_replace(result, '"', '&quot;');
    result:= substr_replace(result, '''', '&#39;');  //single quote
    result:= substr_replace(result, '<', '&lt;');
    result:= substr_replace(result, '>', '&gt;');
    result:= substr_replace(result, '|', '&#124;');  //pipe
    result:= substr_replace(result, '%', '&#37;');   //percent sign
//    result:= substr_replace(result,  #0, '');        //null character okay to trim?? MUST CONFIRM
  end;
  
  if SecureLevel = 2 then
  begin
    result:= substr_replace(input, '&', '&amp;');
    result:= substr_replace(result, '#', '&#35;');   //pound sign
    result:= substr_replace(result, '"', '&quot;');  //quote
    result:= substr_replace(result, '''', '&#39;');  //single quote
    result:= substr_replace(result, '<', '&lt;');    //less than
    result:= substr_replace(result, '>', '&gt;');    //greater than
    result:= substr_replace(result, '|', '&#124;');  //pipe
    result:= substr_replace(result, '%', '&#37;');   //percent sign
//    result:= substr_replace(result,  #0, '');        //null character okay to trim?? MUST CONFIRM
    result:= substr_replace(result, '(', '&#40;');   //open bracket
    result:= substr_replace(result, ')', '&#41;');   //closed bracket
    result:= substr_replace(result, '{', '&#123;');  //open parenthesis
    result:= substr_replace(result, '}', '&#125;');  //closed parenthesis
    result:= substr_replace(result, '$', '&#36;');   //dollar sign
    result:= substr_replace(result, '?', '&#63;');   //question mark
  end;
  
end;



// Indexed access to cgi variable
function FetchCGIVarName(index: longword): string;
begin
  if (index < longword(length(cgi))) and (length(cgi) > 0) then
    result:= cgi[index].name
  else
   result:= '';
end;


// Indexed access to cgi variable
function FetchCGIVarValue(index: longword): string;
begin
  if (index < longword(length(cgi))) and (length(cgi) > 0) then
    result:= cgi[index].value
  else
    result:= '';
end;


// Indexed access to configuration variable
function FetchWebConfigVarName(index: longword): string;
begin
  if (index < longword(length(conf))) and (length(conf) > 0) then
    result:= conf[index].name
  else
    result:= '';
end;


// Indexed access to configuration variable
function FetchWebConfigVarValue(index: longword): string;
begin
  if (index < longword(length(conf))) and (length(conf) > 0) then
    result:= conf[index].value
  else
    result:= '';
end;


// Indexed access to cookie variable
function FetchCookieName(index: longword): string;
begin
  if (index < longword(length(cook))) and (length(cook) > 0) then
    result:= cook[index].name
  else
    result:= '';
end;


// Indexed access to cookie variable
function FetchCookieValue(index: longword): string;
begin
  if (index < longword(length(cook))) and (length(cook) > 0) then
    result:= cook[index].value
  else
    result:= '';
end;


// Indexed access to env variable
function FetchEnvVarName(index: longword): string;
begin
   if (index < longword(length(env))) and (length(env) > 0) then
     result:= env[index].name
   else
     result:= '';
end;


// Indexed access to env variable
function FetchEnvVarValue(index: longword): string;
begin
  if (index < longword(length(env))) and (length(env) > 0) then
    result:= env[index].value
  else
    result:= '';
end;


// Indexed access to header
function FetchWebHeaderName(index: longword): string;
begin
  if (index < longword(length(hdr))) and (length(hdr) > 0) then
    result:= hdr[index].name
  else
    result:= '';
end;


// Indexed access to header
function FetchWebHeaderValue(index: longword): string;
begin
  if (index < longword(length(hdr))) and (length(hdr) > 0) then
    result:= hdr[index].value
  else
    result:= '';
end;


// Indexed access to RTI variable
function FetchRTIName(index: longword): string;
begin
  if (index < longword(length(rti))) and (length(rti) > 0) then
    result:= rti[index].name
  else
    result:= '';
end;


// Indexed access to RTI variable
function FetchRTIValue(index: longword): string;
begin
  if (index < longword(length(rti))) and (length(rti) > 0) then
    result:= rti[index].value
  else
    result:= '';
end;


// Indexed access to cgi variable
function FetchSessName(index: longword): string;
begin
  if (index < longword(length(sess))) and (length(sess) > 0) then
    result:= sess[index].name
  else
    result:= '';
end;


// Indexed access to cgi variable
function FetchSessValue(index: longword): string;
begin
  if (index < longword(length(sess))) and (length(sess) > 0) then
    result:= sess[index].value
  else
    result:= '';
end;


// Indexed access to uploaded file name
function FetchUpFileName(index: longword): string;
begin
  if (index < longword(length(upf))) and (length(upf) > 0) then
    result:= upf[index].name
  else
    result:= '';
end;


// Indexed access to PWU variable
function FetchWebVarName(index: longword): string;
begin
  if (index < longword(length(vars))) and (length(vars) > 0) then
    result:= vars[index].name
  else
    result:= '';
end;


// Indexed access to PWU variable
function FetchWebVarValue(index: longword): string;
begin
  if (index < longword(length(vars))) and (length(vars) > 0) then
    result:= vars[index].value
  else
    result:= '';
end;


// Formats a string replacing variables as if they were macros.
// i.e. if a string contains {$MyVariable} it will be replaced
// This function does not filter and replace malicious/html characters, but
// rather trims (discards) them
//
// Default security level: 2
function WebFormat(const str: string): string;
begin
  result:= WebFormat_SF(str, false, 0, 2);
  // Uses the following default security settings:
  //   Filter HTML input: NO, see WebFormatAndfilter
  //   Filter security: level 0, not applicable
  //   Trim security: level 2

end;


// Same as WebFormat, but filters and replaces HTML characters with safe ones,
// as opposed to trimming and discarding them like WebFormat does.
//
// Default security level: 2
function WebFormatAndFilter(const str: string): string;
begin
  result:= WebFormat_SF(str, true, 2, 0);
  // Uses the following default security settings:
  //
  //   Filter HTML: yes
  //   Filter security: level 2
  //   Trim security: level 0. Not applicable. We are filtering, not trimming
end;


// WebFormat_SF offers the ability to specify security levels and filter
// settings, and is also used internally to power the default WebFormat
// and WebFilterFormat functions. Those are the ones you use normally,
// this one is for special circumstances
//
// The _SF suffix means "with specifiable Security and Filter options"
//
// The Filter security is ignored and should be set at 0 when
// FilterHTMLOption = false, because there is no filter security setting
// that applies.
//
// The trim security is ignored and should set at 0 when
// FilterHTMLOption = true,  because we can't trim the special characters
// and then try to replace them after (they would already be trimmed).
// i.e. we have to use one or the other, either replace or trim your input.
//
function WebFormat_SF(const str: string;
                      const FilterHTMLOption: boolean;
                      const FilterSecureLevel,
                            TrimSecureLevel: integer): string;
const
  ID_CHARS = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_';
var
  i,
  len: longword;
  lex: string;
begin
  // Init
  i:= 1;
  len:= length(str);
  lex:= '';
  result:= '';
  // Parsing
  while i <= len do
  begin
    // Normal concat until chars of our attention
    while (i <= len) and (str[i] <> '$') and (str[i] <> '{') do
    begin
      SetLength(result, length(result) + 1);
      result[length(result)]:= str[i];
      inc(i);
    end;
    // If encountered an indication char
    if (i <= len) and (str[i] = '$') then
    begin
      // $varname?
      // Checking if escaped
      if ((i - 1) > 0) and (str[i - 1] = '\') then
      begin
        // Escaped, ignoring
        SetLength(result, length(result) + 1);
        result[length(result)]:= str[i];
        inc(i);
      end
        else
      begin
        // Getting var name
        inc(i);
        lex:= '';
        while (i <= len) and (substr_pos(ID_CHARS, str[i]) > 0) do
        begin
          SetLength(lex, length(lex) + 1);
          lex[length(lex)]:= str[i];
          inc(i);
        end;
        // Evaluating and adding
        if FilterHTMLOption = true then
        begin

          if FilterSecureLevel = 0 then
          begin
            lex:= GetWebVar_S(lex, 0) //must use GetWebVar security level 0 here since we are implementing our own security with filterHTML.
          end;

          if FilterSecureLevel = 1 then
          begin
            lex:= FilterHTML_S(GetWebVar_S(lex, 0), 1) //must use GetWebVar security level 0 here since we are implementing our own security with filterHTML.
          end;

          if FilterSecureLevel = 2 then
          begin
            lex:= FilterHTML_S(GetWebVar_S(lex, 0), 2) //must use GetWebVar security level 0 here since we are implementing our own security with filterHTML.
          end;
        end
          else
        begin

          if TrimSecureLevel = 0 then
          begin
            lex:= GetWebVar_S(lex, 0);
          end;

          if TrimSecureLevel = 1 then
          begin
//            writeln(lex+ ' BEFORE');  //DEBUG
            lex:= GetWebVar_S(lex, 1);
            //debug
//            writeln(lex + ' AFTER' ); //DEBUG
          end;

          if TrimSecureLevel = 2 then
          begin
//            writeln(lex + ' BEFORE' );//DEBUG
            lex:= GetWebVar_S(lex, 2);
//            writeln(lex + ' AFTER' ); //DEBUG
          end;

        end;

        result:= result + lex;
      end;
    end
      else
    if (i <= len) and (str[i] = '{') then
    begin
      // {$varname}?
      // Check if escaped or does match the pattern
      if ((i - 1) > 0) and (str[i - 1] = '\') then
      begin
        // Escaped, ignoring
        SetLength(result, length(result) + 1);
        result[length(result)]:= str[i];
        inc(i);
      end
        else
      if ((i + 1) < len) and (str[i + 1] <> '$') then
      begin
        // Does not match, ignoring
        SetLength(result, length(result) + 1);
        result[length(result)]:= str[i];
        inc(i);
      end
        else
      begin
        // There MUST be } or you should escape curly braces
        // Getting var name till }
        i:= i + 2;
        lex:= '';
        while (i <= len) and (str[i] <> '}') do
        begin
          SetLength(lex, length(lex) + 1);
          lex[length(lex)]:= str[i];
          inc(i);
        end;
        inc(i);
        // Evaluating and adding
        if FilterHTMLOption = true then
        begin

          if FilterSecureLevel = 0 then
          begin
            lex:= GetWebVar_S(lex, 0) //must use GetWebVar security level 0 here since we are implementing our own security with filterHTML.
          end;

          if FilterSecureLevel = 1 then
          begin
            lex:= FilterHTML_S(GetWebVar_S(lex, 0), 1) //must use GetWebVar security level 0 here since we are implementing our own security with filterHTML.
          end;

          if FilterSecureLevel = 2 then
          begin
            lex:= FilterHTML_S(GetWebVar_S(lex, 0), 2) //must use GetWebVar security level 0 here since we are implementing our own security with filterHTML.
          end;
        end
          else
        begin

          if TrimSecureLevel = 0 then
          begin
            lex:= GetWebVar_S(lex, 0);
          end;

          if TrimSecureLevel = 1 then
          begin
//            writeln(lex + ' BEFORE' );//DEBUG
            lex:= GetWebVar_S(lex, 1);
//            writeln(lex + ' AFTER' ); //DEBUG
          end;

          if TrimSecureLevel = 2 then
          begin
//            writeln(lex + ' BEFORE' );//DEBUG
            lex:= GetWebVar_S(lex, 2);
//            writeln(lex + ' AFTER' ); //DEBUG
          end;

        end;

        result:= result + lex;
      end;
    end;
  end;
end;




// Returns value of CGI (GET/POST) variable. This also means your URL variables.
//
// Default Security level is 2. Use the _S suffix function if you do not need
// high filtering security, or you wish to implment your own filters
function GetCGIVar(const name: string): string;
begin
  result:= GetCGIVar_S(name, 2);
end;


// Same as GetCGIVar, but the _S suffix means you can choose the security level
// User specified security level.
//
// Security 0: does not automatically trim. use this when you want to implement
//             your own filtering, such as when using FilterHTML
// Security 1: trims (deletes) special (malicious) characters
// Security 2: trims even more than level 1
//
function GetCGIVar_S(const name: string; const SecureLevel: integer): string;
var
  i: longword;
begin
  result:= '';
  if length(cgi) = 0 then exit;
  
  for i:= 0 to length(cgi) - 1 do if cgi[i].name = name then
  begin

    //perform a trim with security 2, output the result
    if SecureLevel = 2 then
    begin
      result:= TrimBadChars_S(cgi[i].value, 2);
      exit;
    end;
    
    //perform a trim with security 1, output the result
    if SecureLevel = 1 then
    begin
      result:= TrimBadChars_S(cgi[i].value, 1);
      exit;
    end;
    
    //perform NO trim, output the result
    if SecureLevel = 0 then
    begin
      result:= cgi[i].value;
      exit;
    end;
    
  end;
end;


// Returns value of CGI (GET/POST) variable as double precision float
// todo: implement security levels? is it needed?
function GetCGIVarAsFloat(const name: string): double;
var
  i: longword;
begin
  result:= 0.0;
  if length(cgi) = 0 then exit;
  for i:= 0 to length(cgi) - 1 do if cgi[i].name = name then
  begin
    val(cgi[i].value, result);
    break;
  end;
end;


// Returns value of CGI (GET/POST) variable as integer
// todo: implement security levels? is it needed?
function GetCGIVarAsInt(const name: string): longint;
var
  i: longword;
begin
  result:= 0;
  if length(cgi) = 0 then exit;
  for i:= 0 to length(cgi) - 1 do if cgi[i].name = name then
  begin
    val(cgi[i].value, result);
    break;
  end;
end;


// Returns value of configuration variable
// todo: implement security levels? is it needed?
function GetWebConfigVar(const name: string): string;
var
  i: longword;
begin
  result:= '';
  if length(conf) = 0 then exit;
  for i:= 0 to length(conf) - 1 do if conf[i].name = name then
  begin
    result:= conf[i].value;
    break;
  end;
end;


// Returns value of a cookie
// todo: implement security levels. it is needed!
function GetCookie(const name: string): string;
var
  i: longword;
begin
  result:= '';
  if length(cook) = 0 then exit;
  for i:= 0 to length(cook) - 1 do if cook[i].name = name then
  begin
    result:= cook[i].value;
    break;
  end;
end;


// Returns value of a cookie as double precision float
// todo: implement security levels? is it needed?
function GetCookieAsFloat(const name: string): double;
var
  i: longword;
begin
  result:= 0.0;
  if length(cook) = 0 then exit;
  for i:= 0 to length(cook) - 1 do if cook[i].name = name then
  begin
    val(cook[i].value, result);
    break;
  end;
end;


// Returns value of a cookie as integer
// todo: implement security levels? is it needed?
function GetCookieAsInt(const name: string): longint;
var
  i: longword;
begin
  result:= 0;
  if length(cook) = 0 then exit;
  for i:= 0 to length(cook) - 1 do if cook[i].name = name then
  begin
    val(cook[i].value, result);
    break;
  end;
end;


// Returns value of an environment variable
// todo: implement security levels. it is needed!
// i.e. say someone tries to pass a User Agent as <script java hack_up_Server>
function GetEnvVar(const name: string): string;
var
  i: longword;
begin
  result:= '';
  if length(env) = 0 then exit;
  for i:= 0 to length(env) - 1 do if upcase(env[i].name) = upcase(name) then
  begin
    result:= env[i].value;
    break;
  end;
end;


// Returns value part of already assigned HTTP header
// todo: implement security levels? is it needed?
function GetWebHeader(const name: string): string;
var
  i: longword;
begin
   result:= '';
   if length(hdr) = 0 then exit;
   for i:= 0 to length(hdr) - 1 do if upcase(hdr[i].name) = upcase(name) then
    begin
      result:= hdr[i].value;
      break;
    end;
end;


// Returns value of RTI (Run Time Information) variable
// todo: implement security levels? is it needed?
function GetRTI(const name: string): string;
var
  i: longword;
begin
  result:= '';
  if length(rti) = 0 then exit;
  for i:= 0 to length(rti) - 1 do if rti[i].name = name then
  begin
     result:= rti[i].value;
     break;
  end;
end;


// Returns value of RTI variable as double precision float
// todo: implement security levels? is it needed?
function GetRTIAsFloat(const name: string): double;
var
  i: longword;
begin
  result:= 0.0;
  if length(rti) = 0 then exit;
  for i:= 0 to length(rti) - 1 do if rti[i].name = name then
  begin
     val(rti[i].value, result);
     break;
  end;
end;


// Returns value of RTI variable as integer
// todo: implement security levels? is it needed?
function GetRTIAsInt(const name: string): longint;
var
  i: longword;
begin
  result:= 0;
  if length(rti) = 0 then exit;
  for i:= 0 to length(rti) - 1 do if rti[i].name = name then
  begin
    val(rti[i].value, result);
    break;
  end;
end;


// Returns value of session variable
// todo: implement security levels. it is needed!
function GetSess(const name: string): string;
var
  i: longword;
begin
  result:= '';
  if length(sess) = 0 then exit;
  for i:= 0 to length(sess) - 1 do if sess[i].name = name then
  begin
    result:= sess[i].value;
    break;
  end;
end;


// Returns value of session variable as double precision float
// todo: implement security levels? is it needed?
function GetSessAsFloat(const name: string): double;
var
  i: longword;
begin
   result:= 0.0;
   if length(sess) = 0 then exit;
   for i:= 0 to length(sess) - 1 do if sess[i].name = name then
  begin
    val(sess[i].value, result);
    break;
  end;
end;


// Returns value of session variable as integer
// todo: implement security levels? is it needed?
function GetSessAsInt(const name: string): longint;
var
  i: longword;
begin
  result:= 0;
  if length(sess) = 0 then exit;
  for i:= 0 to length(sess) - 1 do if sess[i].name = name then
  begin
    val(sess[i].value, result);
    break;
  end;
end;


// Returns original name of the uploaded file
// todo: implement security levels? is it needed?
function GetUpFileName(const name: string): string;
var
  i: longword;
begin
  result:= '';
  if length(upf) = 0 then exit;
  for i:= 0 to length(upf) - 1 do if upf[i].name = name then
  begin
    result:= upf[i].filename;
    break;
  end;
end;


// Returns size of the uploaded file
function GetUpFileSize(const name: string): longint;
var
  i: longword;
begin
  result:= 0;
  if length(upf) = 0 then exit;
  for i:= 0 to length(upf) - 1 do if upf[i].name = name then
  begin
    result:= upf[i].size;
    break;
  end;
end;


// Returns Content-Type of the uploaded file
// todo: implement security levels? is it needed?
function GetUpFileType(const name: string): string;
var
  i: longword;
begin
  result:= '';
  if length(upf) = 0 then exit;
  for i:= 0 to length(upf) - 1 do if upf[i].name = name then
  begin
    result:= upf[i].content_type;
    break;
  end;
end;


// Returns value of any PWU variable (cgi, session, cookie, vars)
// Default security level: 2
function GetWebVar(const name: string): string;
begin
  result:= GetWebVar_S(name, 2);
end;




// Powers the GetCGIVar_SafeHTML function. Use this function for special
// circumstances, with the ability to specify security level.
//
// Security level 0:
//   Doesn't filter special characters. Use this if you are filtering or
//   trimming yourself, or when you are using FilterHTML
//
// Security level 1:
//   Filters malicious characters from variable into safe html equivilents
//
// Security level 2:
//   Filters even more than security level 1, including { } $
//
function GetCGIVar_SF(const name: string; const SecureLevel: integer): string;
var
  i: longword;
begin
  result:= '';

  // look in cgi vars
  if length(cgi) > 0 then
  for i:= 0 to length(cgi) - 1 do if cgi[i].name = name then
  begin

    if SecureLevel = 0 then
    begin
      result:= cgi[i].value;
      exit;
    end;

    if SecureLevel = 1 then
    begin
      result:= FilterHTML_S(cgi[i].value, 1);
      exit;
    end;

    if SecureLevel = 2 then
    begin
      result:= FilterHTML_S(cgi[i].value, 2);
      exit;
    end;

  end;

end;

function GetCGIVar_SafeHTML(const name: string): string;
begin
  result:= GetCGIVar_SF(name, 2);
end;


// Powers the GetWebVar function. Use this function for special circumstances,
// with the ability to specify security level.
//
// Security level 0:
//   Doesn't trim special characters. Use this if you are trimming yourself,
//   or when you are using FilterHTML
//
// Security level 1:
//   Trims (deletes) malicious characters from variable
//
// Security level 2:
//   Trims even more than security level 1, including { } $
//
function GetWebVar_S(const name: string; const SecureLevel: integer): string;
var
  i: longword;
begin
  result:= '';
  // First look in vars
  if length(vars) > 0 then
  for i:= 0 to length(vars) - 1 do if vars[i].name = name then
  begin

    if SecureLevel = 0 then
    begin
      result:= vars[i].value;
      exit;
    end;
    
    if SecureLevel = 1 then
    begin
      result:= TrimBadChars_S(vars[i].value, 1);
      exit;
    end;

    if SecureLevel = 2 then
    begin
      result:= TrimBadChars_S(vars[i].value, 2);
      exit;
    end;

  end;

  // Then look in session vars
  if length(sess) > 0 then
  for i:= 0 to length(sess) - 1 do if sess[i].name = name then
  begin

    if SecureLevel = 0 then
    begin
      result:= sess[i].value;
      exit;
    end;
    
    if SecureLevel = 1 then
    begin
      result:= TrimBadChars_S(sess[i].value, 1);
      exit;
    end;
    
    if SecureLevel = 2 then
    begin
      result:= TrimBadChars_S(sess[i].value, 2);
      exit;
    end;

  end;

  // Then look in cookie vars
  if length(cook) > 0 then
  for i:= 0 to length(cook) - 1 do if cook[i].name = name then
  begin

    if SecureLevel = 0 then
    begin
      result:= cook[i].value;
      exit;
    end;

    if SecureLevel = 1 then
    begin
      result:= TrimBadChars_S(cook[i].value, 1);
      exit;
    end;

    if SecureLevel = 2 then
    begin
      result:= TrimBadChars_S(cook[i].value, 2);
      exit;
    end;

  end;

  // Then look in cgi vars
  if length(cgi) > 0 then
  for i:= 0 to length(cgi) - 1 do if cgi[i].name = name then
  begin

    if SecureLevel = 0 then
    begin
      result:= cgi[i].value;
      exit;
    end;

    if SecureLevel = 1 then
    begin
      result:= TrimBadChars_S(cgi[i].value, 1);
      exit;
    end;
    
    if SecureLevel = 2 then
    begin
      result:= TrimBadChars_S(cgi[i].value, 2);
      exit;
    end;

  end;
  
end;


// Returns value of PWU variable as float (double precision)
function GetWebVarAsFloat(const name: string): double;
begin
  val(GetWebVar(name), result);
end;


// Returns value of PWU variable as integer
function GetWebVarAsInt(const name: string): longint;
begin
  val(GetWebVar(name), result);
end;


// Tells whether a CGI (GET/POST) variable is assigned
function IsCGIVar(const name: string): boolean;
var
  i: longword;
begin
  result:= false;
  if length(cgi) = 0 then exit;
  for i:= 0 to length(cgi) - 1 do if cgi[i].name = name then
  begin
    result:= true;
    break;
  end;
end;


// Tells whether a configuration variable is assigned
function IsWebConfigVar(const name: string): boolean;
var
  i: longword;
begin
  result:= false;
  if length(conf) = 0 then exit;
  for i:= 0 to length(conf) - 1 do if conf[i].name = name then
  begin
    result:= true;
    break;
  end;
end;


// Tells whether a cookie is assigned
function IsCookie(const name: string): boolean;
var
  i: longword;
begin
  result:= false;
  if length(cook) = 0 then exit;
  for i:= 0 to length(cook) - 1 do if cook[i].name = name then
  begin
    result:= true;
    break;
  end;
end;


// Tells whether a environment variable is assigned
function IsEnvVar(const name: string): boolean;
var
  i: longword;
begin
  result:= false;
  if length(env) = 0 then exit;
  for i:= 0 to length(env) - 1 do if upcase(env[i].name) = upcase(name) then
  begin
    result:= true;
    break;
  end;
end;


// Tells whether a header is assigned
function IsWebHeader(const name: string): boolean;
var
  i: longword;
begin
  result:= false;
  if length(hdr) = 0 then exit;
  for i:= 0 to length(hdr) - 1 do if upcase(hdr[i].name) = upcase(name) then
  begin
    result:= true;
    break;
  end;
end;


// Tells whether a RTI variable exists
function IsRTI(const name: string): boolean;
var
  i: longword;
begin
  result:= false;
  if length(rti) = 0 then exit;
  for i:= 0 to length(rti) - 1 do if rti[i].name = name then
  begin
    result:= true;
    break;
  end;
end;


// Tells whether a session variable is assigned
function IsSess(const name: string): boolean;
var 
  i: longword;
begin
  result:= false;
  if length(sess) = 0 then exit;
  for i:= 0 to length(sess) - 1 do if sess[i].name = name then
  begin
    result:= true;
    break;
  end;
end;


// Tells whether file field is uploaded
function IsUpFile(const name: string): boolean;
var 
  i: longword;
begin
  result:= false;
  if length(upf) = 0 then exit;
  for i:= 0 to length(upf) - 1 do if upf[i].name = name then
  begin
    result:= true;
    break;
  end;
end;


// Tells whether a PWU variable exists
function IsWebVar(const name: string): byte;
var 
  i: longword;
begin
  result:= 0;
  // First looking in vars
  if length(vars) > 0 then
  for i:= 0 to length(vars) - 1 do if vars[i].name = name then
  begin
    result:= 1;
    exit;
  end;
  // Then looking in sess
  if length(sess) > 0 then
  for i:= 0 to length(sess) - 1 do if sess[i].name = name then
  begin
    result:= 2;
    exit;
  end;
  // Then looking in cook
  if length(cook) > 0 then
  for i:= 0 to length(cook) - 1 do if cook[i].name = name then
  begin
    result:= 3;
    exit;
  end;
  // Then looking in cgi
  if length(cgi) > 0 then
  for i:= 0 to length(cgi) - 1 do if cgi[i].name = name then
  begin
    result:= 4;
    exit;
  end;
end;


// Replaces all end-of-line chars with <br /> tags
function LineEndToBR(const str: string): string;
begin
  result:= str;
  if substr_exists(result, #13+#10) then
    result:= substr_replace(result, #13+#10, '<br />');
  if substr_exists(result, #10) then
    result:= substr_replace(result, #10, '<br />');
end;


// Plain output
procedure WebWrite(const str: string);
var
  i, j: longword;
begin
  {$IFDEF GZIP_ENABLED}
  if output_buffering then
  begin
     // Append str to buffer
     j:= length(ob);
     SetLength(ob, length(ob) + length(str));
     for i:= 1 to length(str) do ob[j + i]:= str[i];
  end
    else
  {$ENDIF GZIP_ENABLED}
  begin
     if not headers_sent then SendWebHeaders;
     write(str);
  end;
end;


// PWU-formatted output (plus trims malicious characters)
// As opposed to WebWriteFF, this function trims (deletes) malicious
// characters. It does not replace them with html equivilents.
// _F suffix stands for "formatted"
// Default security level: 2, trim
procedure WebWriteF(const str: string);
begin
  WebWriteF_Fi(str, false);
end;


// PWU-formatted output (plus filters and replaces malicious characters)
// As opposed to WebWriteF, this function fiters and does replace malicious
// characters with HTML equivilents, rather than deleting them
// _FF suffix stands for "format and filter"
// Default security level: 2, filter
procedure WebWriteFF(const str: string);
begin
  WebWriteF_Fi(str, true);
end;


// Powers the WebWriteF and WebWriteFF functions
// With specifiable filter option for malicious input. If FilterTheHTML is true
// then the malicious input and special characters are replaced with HTML
// equivilents. If false, the malious input is trimmed (deleted). If you don't
// want trimming or filtering at all, see WebFormat_SF and use it instead.
procedure WebWriteF_Fi(const str: string; const FilterTheHTML: boolean);
var
  i, j: longword;
  s: string;
begin
  {$IFDEF GZIP_ENABLED}
  if output_buffering then
  begin
     if FilterTheHTML = true then
       s:= WebFormatAndFilter(str)
     else
       s:= WebFormat(str);

     // Append str to buffer
     j:= length(ob);
     SetLength(ob, length(ob) + length(s));
     for i:= 1 to length(s) do ob[j + i]:= s[i];
  end
    else
  {$ENDIF GZIP_ENABLED}
  begin
     if not headers_sent then SendWebHeaders;

     if FilterTheHTML = true then
       write(WebFormatAndFilter(str))
     else
       write(WebFormat(str));

  end;
end;


// Plain output with line ending
procedure WebWriteLn(const str: string);
var
  i, j: longword;
begin
 {$IFDEF GZIP_ENABLED}
  if output_buffering then
  begin
    // Append str to buffer
    j:= length(ob);
    SetLength(ob, length(ob) + length(str));
    for i:= 1 to length(str) do ob[j + i]:= str[i];
    // Append end of line
    SetLength(ob, length(ob) + 2);
    ob[length(ob) - 1]:= #13;
    ob[length(ob)]:= #10;
  end                           
    else
 {$ENDIF GZIP_ENABLED}
  begin
     if not headers_sent then SendWebHeaders;
     writeln(str);
  end;
end;



// formatted writeln, outputs variables like macros. i.e. replaces {$MyVar}
// with an existing web variable. Trims malicious attempts by deleting special
// characters. F stands for "Formatted". If you don't want trimming, see
// WebWriteLnFF which filters instead.
procedure WebWriteLnF(const str: string);
begin
  WebWriteLnF_Fi(str, false);
end;

// formatted and filtered writeln, outputs variables like macros,
// i.e. replaces {$MyVar} with an existing web variable, plus filters
// malicious attempts by filtering HTML special characters.
// FF stands for "Formatted and Filtered"
procedure WebWriteLnFF(const str: string);
begin
  WebWriteLnF_Fi(str, true);
end;


// powers the WebWriteLnF and WebWriteLnFF functions, and is to be used for
// more control when webwritelnf and webwritelnff are too limiting.
// If FilterTheHTML option is on, then the template is filtered first and
// special characters are is replaced with HTML equivilents
// F stands for "Formatted", while _Fi stands for "Filter input" options
procedure WebWriteLnF_Fi(const str: string; const FilterTheHTML: boolean );
var
  i, j: longword;
  s: string;
begin
  {$IFDEF GZIP_ENABLED}
  if output_buffering then
  begin

    if FilterTheHTML = true then
      s:= WebFormatAndFilter(str)
    else
      s:= WebFormat(str);

    // Append str to buffer
    j:= length(ob);
    SetLength(ob, length(ob) + length(s));
    for i:= 1 to length(s) do ob[j + i]:= s[i];
    // Append end of line
    SetLength(ob, length(ob) + 2);
    ob[length(ob) - 1]:= #13;
    ob[length(ob)]:= #10;
  end
    else
  {$ENDIF GZIP_ENABLED}
  begin
    if not headers_sent then SendWebHeaders;

    if FilterTheHTML = true then
      writeln(WebFormatAndFilter(str))
    else
      writeln(WebFormat(str));
  end;
end;


// Plain file output
procedure WebFileOut(const fname: string);
var
  fh: text;
  s: string;
  i, j: longword;
begin
  if not FileExists(fname) then
  begin
    ThrowWebError('File does not exist - ' + fname);
    exit;
  end;
  if (not headers_sent) {$IFDEF GZIP_ENABLED} and (not output_buffering) {$ENDIF} then
    SendWebHeaders;
                
  assign(fh, fname);
  reset(fh);
  while not eof(fh) do
  begin
    readln(fh, s);
    {$IFDEF GZIP_ENABLED}
    if output_buffering then
    begin
      // Append str to buffer
      j:= length(ob);
      SetLength(ob, length(ob) + length(s));
      for i:= 1 to length(s) do ob[j + i]:= s[i];
      // Append end of line
      SetLength(ob, length(ob) + 2);
      ob[length(ob) - 1]:= #13;
      ob[length(ob)]:= #10;
    end else
    {$ENDIF GZIP_ENABLED}
      writeln(s);
  end;
   close(fh);
end;


// Plain binary file output
procedure WebResourceOut(const fname: string);
var
  fh: file of char;
  buff: array[1..4096] of char;
  i, j, len: longword;
begin
  if not FileExists(fname) then
  begin
    ThrowWebError('File does not exist - ' + fname);
    exit;
  end;
  if (not headers_sent){$IFDEF GZIP_ENABLED}
                        and (not output_buffering)
                       {$ENDIF}                   then
    SendWebHeaders;
  assign(fh, fname);
  reset(fh);
  while not eof(fh) do
  begin
    blockread(fh, buff, sizeof(buff), len);
   {$IFDEF GZIP_ENABLED}
    if output_buffering then
    begin
      // Append str to buffer
      j:= length(ob);
      SetLength(ob, longword(length(ob)) + len);
      for i:= 1 to len do ob[j + i]:= buff[i];
    end
      else
   {$ENDIF GZIP_ENABLED}
    begin
       for i:= 1 to len do write(buff[i]);
    end;
  end;
  close(fh);
end;


// Formatted file output (macro variables). If FilterHTMLOption is true, then
// any malicious characters are replaced with html equivalents. If false, then
// malicious characters are trimmed (deleted) without being replaced.
procedure WebTemplateOut(const fname: string; const FilterHTMLOption: boolean);
var
  fh: text;
  s: string;
  i, j: longword;
begin
  if not FileExists(fname) then
  begin
     ThrowWebError('File does not exist - ' + fname);
     exit;
  end;
  if (not headers_sent){$IFDEF GZIP_ENABLED}
                        and (not output_buffering)
                       {$ENDIF}                   then
    SendWebHeaders;
  assign(fh, fname);
  reset(fh);
  while not eof(fh) do
  begin
    readln(fh, s);
   {$IFDEF GZIP_ENABLED}
    if output_buffering then
    begin
      if FilterHTMLOption = true then
        s:= WebFormatAndFilter(s)
      else
        s:= WebFormat(s);

      // Append str to buffer
      j:= length(ob);
      SetLength(ob, length(ob) + length(s));
      for i:= 1 to length(s) do ob[j + i]:= s[i];
      // Append end of line
      SetLength(ob, length(ob) + 2);
      ob[length(ob) - 1]:= #13;
      ob[length(ob)]:= #10;
    end else
   {$ENDIF GZIP_ENABLED}
    if FilterHTMLOption = true then
      writeln(WebFormatAndFilter(s))
    else
      writeln(WebFormat(s));
    
  end;
  close(fh);
end;


// Sets HTTP header like 'Name: Value'
function PutWebHeader(const header: string): boolean;
var 
  i: longword;
    nv: StrArray;
begin
  result:= false;
  // Check headers
  if headers_sent then
  begin
    ThrowWebError('Can not set header - headers are already sent');
    exit(false);
  end;
  // Splitting into name=value pair
  nv:= substr_split(header, ':');
  if length(nv) <> 2 then exit;
  nv[0]:= str_trim(nv[0]);
  nv[1]:= str_trim(nv[1]);
  // Changing value if already set
  if length(hdr) > 0 then
  for i:= 0 to length(hdr) - 1 do if upcase(hdr[i].name) = upcase(nv[0]) then
  begin
    hdr[i].value:= nv[1];
    exit;
  end;
  // Or setting new header
  SetLength(hdr, length(hdr) + 1);
  hdr[length(hdr) - 1].name:= nv[0];
  hdr[length(hdr) - 1].value:= nv[1];
  result:= true;
end;


// Generates a random string of alphanumeric and '_' characters with specified length
function RandomStr(len: longint): string;
const
  PW_CHARS = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_';
var 
  i: longword;
begin
  SetLength(result, len);
  for i:= 1 to len do
  begin
      randomize;
      result[i]:= PW_CHARS[random(62) + 1];
  end;
end;


// Saves uploaded file to disk
function SaveUpFile(const name, fname: string): boolean;
var 
  i: longword;
  fh: text;
begin
  result:= false;
  if length(upf) = 0 then exit;
  {$I-}
  assign(fh, fname);
  rewrite(fh);
  if length(upf) > 0 then
  for i:= 0 to length(upf) - 1 do if upf[i].name = name then
  begin
    write(fh, upf[i].data);
    break;
  end;
  close(fh);
  {$I+}
  if ioresult = 0 then result:= true;
end;


// Destroys currently registered session
function SessDestroy: boolean;
var
  sess_table, key, sid: string;
  res: SDS_Result;
begin
  result:= false;
  // Checking
  sess_table:= GetWebConfigVar('session_path');
  if not FileExists(sess_table) then exit(false);
  // Checking if registered
  if not IsCookie('PWUSESS') then exit(false);
  // Extracting key and id
  key:= base64_decode(GetCookie('PWUSESS'));
  sid:= sds_escape(copy(key, 13, length(key) - 12));
  key:= sds_escape(copy(key, 1, 12));
  // Running query
  res:= sds_query('DELETE FROM `' + sess_table + '` WHERE id = ' + sid + ' AND key = "' + key + '"');
  if sds_result_rows(res) = 1 then result:= true;
  sds_free_result(res);
  // Unset sess
  SetLength(sess, 0);
end;


// Dynamically sets configuration variable name and value
function SetWebConfigVar(const name, value: string): boolean;
var 
  i: longword;
begin
  result:= false;
  // Specific behaviour
  if name = 'header_charset' then
  begin
    // Charset can be set before sending headers
    if headers_sent then ThrowWebError('Config error - header_charset can be configured before headers are set');
    exit(false);
  end
  else if name = 'error_reporting' then
  begin
    // Setting internal flag
    if lowercase(value) = 'on' then error_reporting:= true else error_reporting:= false;
  end
    else if name = 'error_halt' then
  begin
    // Setting internal flag
    if lowercase(value) = 'on' then error_halt:= true else error_halt:= false;
  end
  {$IFDEF GZIP_ENABLED}
    else if name = 'output_buffering' then
  begin
    // Setting internal flag and applying checks
    if lowercase(value) = 'on' then
    begin
      if not output_buffering then output_buffering:= true;
    end
    else
    begin
      if output_buffering then FlushBuffer;
      output_buffering:= false;
    end;
  end
    else if name = 'output_compression' then
  begin
    // Setting internal flag and applying checks
    if lowercase(value) = 'on' then
    begin
      if not output_buffering then
      begin
        ThrowWebError('Config error - output_compression can not be used without output_buffering');
        exit(false);
      end;
      if headers_sent then
      begin
        ThrowWebError('Config error - output_compression can be enabled before headers are sent');
        exit(false);
      end;
      output_compression:= true;
    end
      else
    begin
      if headers_sent then
      begin
        ThrowWebError('Config error - output_compression can be disabled before headers are sent');
        exit(false);
      end;
      if output_compression then
        UnsetWebHeader('Content-Encoding');
      output_compression:= false;
    end;
  end
  {$ENDIF GZIP_ENABLED}
  else
    if (name = 'session_path') or (name = 'session_life_time') then
    begin
      // Vain calls
      ThrowWebError('Config warning - runtime configuration of session_path and session_life_time has no effect');
      exit(false);
    end else
      if name = 'upload_max_size' then
      begin
        // Vain call
        ThrowWebError('Config warning - runtime configuration of upload_max_size has no effect');
        exit(false);
      end;
  // Changing value if already set
  if length(conf) > 0 then
  for i:= 0 to length(conf) - 1 do if conf[i].name = name then
  begin
    conf[i].value:= value;
    exit;
  end;

  // Setting a new config variable is possible
  SetLength(conf, length(conf) + 1);
  conf[length(conf) - 1].name:= name;
  conf[length(conf) - 1].value:= value;
  result:= true;
end;


// Sets a cookie
function SetCookie(const name, value: string): boolean;
var 
  i: longword;
begin
  result:= false;
  // Check headers
  if headers_sent then
  begin
    ThrowWebError('Can not set cookie - headers are already sent');
    exit(false);
  end;
  // Changing value if already set
  if length(cook) > 0 then
  for i:= 0 to length(cook) - 1 do if cook[i].name = name then
  begin
    cook[i].value:= value;
    exit;
  end;
  // Or setting new one
  SetLength(cook, length(cook) + 1);
  cook[length(cook) - 1].name:= name;
  cook[length(cook) - 1].value:= value;
  // Adding the header
  SetLength(hdr, length(hdr) + 1);
  hdr[length(hdr) - 1].name:= 'Set-Cookie';
  hdr[length(hdr) - 1].value:= url_encode(name) + '=' + url_encode(value) + ';path=/;expires=Mon, 01 Dec 2020 12:00:00 GMT';
  result:= true;
end;


// Sets a cookie as double precision float
function SetCookieAsFloat(const name: string; value: double): boolean;
var
  s: string;
begin
  str(value, s);
  result:= SetCookie(name, s);
end;


// Sets a cookie as integer
function SetCookieAsInt(const name: string; value: longint): boolean;
var
  s: string;
begin
  str(value, s);
  result:= SetCookie(name, s);
end;


// Sets an extended cookie
function SetCookieEx(const name, value, path, domain, expires: string): boolean;
var 
  i: longword;
begin
  result:= false;
  // Check headers
  if headers_sent then
  begin
    ThrowWebError('Can not set cookie - headers are already sent');
    exit(false);
  end;
  // Changing value if already set
  if length(cook) > 0 then
  for i:= 0 to length(cook) - 1 do if cook[i].name = name then
  begin
    cook[i].value:= value;
    exit;
  end;
  // Or setting new one
  SetLength(cook, length(cook) + 1);
  cook[length(cook) - 1].name:= name;
  cook[length(cook) - 1].value:= value;
  // Adding the header
  SetLength(hdr, length(hdr) + 1);
  hdr[length(hdr) - 1].name:= 'Set-Cookie';
  hdr[length(hdr) - 1].value:= url_encode(name) + '=' + url_encode(value) + ';path=' + path + ';domain=' + domain + ';expires=' + expires;
  result:= true;
end;


// Sets an extended cookie as double precision float
function SetCookieAsFloatEx(const name: string; value: double; const path, domain, expires: string): boolean;
var
  s: string;
begin
  str(value, s);
  result:= SetCookieEx(name, s, path, domain, expires);
end;


// Sets an extended cookie as integer
function SetCookieAsIntEx(const name: string; value: longint; const path, domain, expires: string): boolean;
var
  s: string;
begin
  str(value, s);
  result:= SetCookieEx(name, s, path, domain, expires);
end;


// Sets an environment variable
function SetEnvVar(const name, value: string): boolean;
var 
  i: longword;
begin
  result:= false;
  // Changing value if already set
  if length(env) > 0 then
  for i:= 0 to length(env) - 1 do if upcase(env[i].name) = upcase(name) then
  begin
      env[i].value:= value;
      exit;
  end;
  // Or setting new one
  SetLength(env, length(env) + 1);
  env[length(env) - 1].name:= name;
  env[length(env) - 1].value:= value;
  // Setting a real env var
  {$IFDEF WIN32}
  result:= SetEnvironmentVariable(pchar(name), pchar(value));
  {$ENDIF}
  {$IFDEF UNIX}
  if setenv(pchar(name), pchar(value), 1) = 0 then
    result:= true
  else
    result:= false;
  {$ENDIF}
end;


// Sets HTTP header
function SetWebHeader(const name, value: string): boolean;
var 
  i: longword;
begin
  result:= false;
  // Check headers
  if headers_sent then
  begin
    ThrowWebError('Can not set header - headers are already sent');
    exit(false);
  end;
  // Changing value if already set
  if length(hdr) > 0 then
  for i:= 0 to length(hdr) - 1 do
    if upcase(hdr[i].name) = upcase(name) then
    begin
      hdr[i].value:= value;
      exit;
    end;
  // Or setting new one
  SetLength(hdr, length(hdr) + 1);
  hdr[length(hdr) - 1].name:= name;
  hdr[length(hdr) - 1].value:= value;
  result:= true;
end;


// Sets session variable
function SetSess(const name, value: string): boolean;
var 
  i: longword;
begin
  result:= false;
  // Check
  if headers_sent then
  begin
    ThrowWebError('Modifying session data after headers are sent has no effect');
    exit(false);
  end;
  // Changing value if already set
  if length(sess) > 0 then
  for i:= 0 to length(sess) - 1 do
    if sess[i].name = name then
    begin
      sess[i].value:= value;
      exit;
    end;
  // Or, if setting a new one
  SetLength(sess, length(sess) + 1);
  sess[length(sess) - 1].name:= name;
  sess[length(sess) - 1].value:= value;
  result:= true;
end;


// Sets session variable as double precision float
function SetSessAsFloat(const name: string; value: double): boolean;
var
  s: string;
begin
  str(value, s);
  result:= SetSess(name, s);
end;


// Sets session variable as integer
function SetSessAsInt(const name: string; value: longint): boolean;
var
  s: string;
begin
  str(value, s);
  result:= SetSess(name, s);
end;


// Assigns PWU variable. i.e. macro variables in templates and formated output
// such as $myVar and {$myvar}
procedure SetWebVar(const name, value: string);
var 
  i: longword;
begin
  // Changing value if already set
  if length(vars) > 0 then
  for i:= 0 to length(vars) - 1 do if vars[i].name = name then
  begin
    vars[i].value:= value;
    exit;
  end;
  // Or setting new one
  SetLength(vars, length(vars) + 1);
  vars[length(vars) - 1].name:= name;
  vars[length(vars) - 1].value:= value;
end;


// Assigns PWU variable as double precision float
procedure SetWebVarAsFloat(const name: string; value: double);
var
  s: string;
begin
  str(value, s);
  SetWebVar(name, s);
end;


// Assigns PWU variable as integer
procedure SetWebVarAsInt(const name: string; value: longint);
var
  s: string;
begin
  str(value, s);
  SetWebVar(name, s);
end;


// Throws PWU error
function ThrowWebError(const message: string): boolean;
var
  i: longint;
  s: string;
begin
  // Init
  result:= false;
  // Icrease ERRORs RTI
  i:= GetRTIAsInt('ERRORS');
  inc(i);
  str(i, s);
  SetRTI('ERRORS', s);
  if not error_reporting then exit(true);
  // Disable content encoding
  if IsWebHeader('Content-Encoding') then
    UnsetWebHeader('Content-Encoding');
  // Send headers
  if not headers_sent then
    SendWebHeaders;
  // Flush the buffer
  FlushBuffer;
  // Adjusting error message
  WebWrite('<pre><b>PWU Error: <i>' + message + '</i></b></pre>');
  // Halt
  if error_halt then halt(0);
  // Done
  result:= true;
end;


// Trims (deletes) all bad, unsecure characters from a string.
// i.e. hackers sometimes use pipe characters | or ../../ to try to hack the
// server. Mainly useful for trimming URL variables for malicious attempts.
// Note: see also FilterHTML, which replaces characters with real output such
//       as &gt; &quot;
//
// Default security level: 2
function TrimBadChars(const input: string): string;
begin
  result:= TrimBadChars_S(input, 2);
end;


// Trims (deletes) all bad, unsecure characters from a string that is being used
// for filenames. For example, if you are opening a file, you will want the
// file name in a local directory, only characters like A-Z plus dots and
// brackets, but not things like pipe characters and quotations
function TrimBadChars_file(const input: string): string;
begin
//    result:= substr_replace(result,'.', '',[rfReplaceAll]);  // Dot is okay
//    result:= substr_replace(result,'~', '',[rfReplaceAll]);  // Squiggly ~ character is okay for filenames
    result:= substr_replace(input,'/', '');     // slashes NOT okay. safe means local directory only!
    result:= substr_replace(result,'\', '');    // slashes NOT okay. safe means local directory only!
    result:= substr_replace(result,'|', '');    // pipe character bad
    result:= substr_replace(result,'#', '');
    result:= substr_replace(result,'@', '');
    result:= substr_replace(result,'$', '');
    result:= substr_replace(result,'!', '');
    result:= substr_replace(result,'%', '');
    result:= substr_replace(result,'^', '');
    result:= substr_replace(result,'&', '');
    result:= substr_replace(result,'*', '');
    result:= substr_replace(result,'=', '');
    result:= substr_replace(result,'`', '');
    result:= substr_replace(result,'?', '');
    result:= substr_replace(result,'"', '');   // double quote
    result:= substr_replace(result,'''', '');  // single quote
    result:= substr_replace(result,'[', '');   // square bracket open
    result:= substr_replace(result,']', '');   // square bracket close
    result:= substr_replace(result,'>', '');   // greater than
    result:= substr_replace(result,'<', '');   // less than
    result:= substr_replace(result,',', '');   // comma
end;


// Trims (deletes) all bad, unsecure characters from a string that is being used
// for a directory. For example, if you are opening a directory or file and 
// directory, you will want only characters like A-Z, plus dots, slashes, and 
// brackets, but not things like pipe characters and quotations
function TrimBadChars_dir(const input: string): string;
begin
//    result:= substr_replace(result,'.', '');  // Dot is okay
//    result:= substr_replace(result,'~', '');  // Squiggly ~ character is okay for filenames
//    result:= substr_replace(result,'/', '',);   //slashes okay
//    result:= substr_replace(result,'\', '');  //slashes okay
    result:= substr_replace(input,'|', '');
    result:= substr_replace(result,'#', '');
    result:= substr_replace(result,'@', '');
    result:= substr_replace(result,'$', '');
    result:= substr_replace(result,'!', '');
    result:= substr_replace(result,'%', '');
    result:= substr_replace(result,'^', '');
    result:= substr_replace(result,'&', '');
    result:= substr_replace(result,'*', '');
    result:= substr_replace(result,'=', '');
    result:= substr_replace(result,'`', '');
    result:= substr_replace(result,'?', '');
    result:= substr_replace(result,'"', '');   // double quote
    result:= substr_replace(result,'''', '');  // single quote
    result:= substr_replace(result,'[', '');   // square bracket open
    result:= substr_replace(result,']', '');   // square bracket close
    result:= substr_replace(result,'>', '');   // greater than
    result:= substr_replace(result,'<', '');   // less than
    result:= substr_replace(result,',', '');   // comma
end;

// Powers the TrimBadChars function. Ability to define security level
//
// Security level 1: Trims bad (malicious) charachers
// Security level 2:  Even more are trimmed than in security level 1
function TrimBadChars_S(const input: string; const SecureLevel: integer): string;
begin

  if SecureLevel = 1 then
  begin
//  result:= substr_replace(result, #0, '');   //okay to trim null character or does this screw up ansistrings?
    result:= substr_replace(input, '/', '');   //slashes bad
    result:= substr_replace(result, '\', '');
    result:= substr_replace(result, '|', '');  //pipe character bad
    result:= substr_replace(result, '?', '');
    result:= substr_replace(result, '$', '');
    result:= substr_replace(result, '&', '');
    result:= substr_replace(result, '<', '');
    result:= substr_replace(result, '>', '');
    exit;
  end;

  if SecureLevel = 2 then
  begin
//  result:= substr_replace(result,  #0, '');  // okay to trim null character or does this screw up ansistrings????
    result:= substr_replace(input, '/', '');   // slashes bad
    result:= substr_replace(result, '\', '');
    result:= substr_replace(result, '|', '');  // pipe character bad
    result:= substr_replace(result, '?', '');
    result:= substr_replace(result, '$', '');
    result:= substr_replace(result, '<', '');
    result:= substr_replace(result, '>', '');
    result:= substr_replace(result, '#', '');
    result:= substr_replace(result, '@', '');
    result:= substr_replace(result, '!', '');
    result:= substr_replace(result, '%', '');
    result:= substr_replace(result, '^', '');
    result:= substr_replace(result, '&', '');
    result:= substr_replace(result, '*', '');
    result:= substr_replace(result, '=', '');
    result:= substr_replace(result, '~', '');
    result:= substr_replace(result, '{', '');
    result:= substr_replace(result, '}', '');
    result:= substr_replace(result, '(', '');
    result:= substr_replace(result, ')', '');
    result:= substr_replace(result, '[', '');    
    result:= substr_replace(result, ']', '');    
    result:= substr_replace(result, '`', '');   // backquote
    result:= substr_replace(result, '"', '');   // double quote    
    result:= substr_replace(result, '''', '');  // single quote        
    exit;
  end;
  
end;


// Unsets a cookie
function UnsetCookie(const name: string): boolean;
var
  tmp: Web_TVariables;
  i: longword;
begin
  result:= false;
  // Header check
  if headers_sent then
  begin
    ThrowWebError('Can not unset cookie - headers are already sent');
    exit(false);
  end;
  // First removing from the list
  SetLength(tmp, 0);
  if length(cook) > 0 then
  for i:= 0 to length(cook) - 1 do if cook[i].name <> name then
  begin
    SetLength(tmp, length(tmp) + 1);
    tmp[length(tmp) - 1]:= cook[i];
  end;
  // Swap
  cook:= tmp;
  // The setting a removing header
  SetLength(hdr, length(hdr) + 1);
  hdr[length(hdr) - 1].name:= 'Set-Cookie';
  hdr[length(hdr) - 1].value:= url_encode(name) + '=;path=/;expires=Fri, 26 Aug 2005 12:00:00 GMT';
  result:= true;
end;


// Unsets extended cookie
function UnsetCookieEx(const name, path, domain: string): boolean;
var
  tmp: Web_TVariables;
  i: longword;
begin
  result:= false;
  // Header check
  if headers_sent then
  begin
    ThrowWebError('Can not unset cookie - headers are already sent');
    exit(false);
  end;
  // First removing from the list
  SetLength(tmp, 0);
  if length(cook) > 0 then
  for i:= 0 to length(cook) - 1 do if cook[i].name <> name then
  begin
    SetLength(tmp, length(tmp) + 1);
    tmp[length(tmp) - 1]:= cook[i];
  end;
  // Swap
  cook:= tmp;
  // The setting a removing header
  SetLength(hdr, length(hdr) + 1);
  hdr[length(hdr) - 1].name:= 'Set-Cookie';
  hdr[length(hdr) - 1].value:= url_encode(name) + '=;path=' + path + ';domain=' + domain + ';expires=Fri, 26 Aug 2005 12:00:00 GMT';
  result:= true;
end;


// Removes HTTP header from the list
function UnsetWebHeader(const name: string): boolean;
var
  tmp: Web_TVariables;
  i: longword;
begin
  result:= false;
  // Check
  if headers_sent then
  begin
    ThrowWebError('Trying to unset headers when headers are already sent has no effect');
    exit(false);
  end;
  // First removing from the list
  SetLength(tmp, 0);
  if length(hdr) > 0 then
  for i:= 0 to length(hdr) - 1 do if upcase(hdr[i].name) <> upcase(name) then
  begin
    SetLength(tmp, length(tmp) + 1);
    tmp[length(tmp) - 1]:= hdr[i];
  end;
  // Swap
  hdr:= tmp;
  result:= true;
end;


// Unsets session variable
function UnsetSess(const name: string): boolean;
var
  tmp: Web_TVariables;
  i: longword;
begin
  result:= false;
  // Check
  if headers_sent then
  begin
    ThrowWebError('Modifying session data after headers are sent has no effect');
    exit(false);
  end;
  // First removing from the list
  SetLength(tmp, 0);
  if length(sess) > 0 then
  for i:= 0 to length(sess) - 1 do if sess[i].name <> name then
  begin
    SetLength(tmp, length(tmp) + 1);
    tmp[length(tmp) - 1]:= sess[i];
  end;
  // Swap
  sess:= tmp;
  result:= true;
end;


// Removes PWU variable from the list
procedure UnsetWebVar(const name: string);
var
  tmp: Web_TVariables;
  i: longword;
begin
  // Removing from the list
  SetLength(tmp, 0);
  if length(vars) > 0 then
  for i:= 0 to length(vars) - 1 do if vars[i].name <> name then
  begin
    SetLength(tmp, length(tmp) + 1);
    tmp[length(tmp) - 1]:= vars[i];
  end;
  // Swap
  sess:= tmp;
end;


// Bytewise XOR encryption
function XORCrypt(const str: string; key: byte): string;
var
  i,
  len: longword;
begin
  result:= '';
  len:= length(str);
  SetLength(result, len);
  for i:= 1 to len do
    result[i]:= chr(ord(str[i]) xor key);
end;


// END OF PUBLIC FUNCTIONS/PROCEDURES
{------------------------------------------------------------------------------}





{------------------------------------------------------------------------------}
{--- INITIALIZATION/FINALIZATION ----------------------------------------------}
{------------------------------------------------------------------------------}


var
 // temporary holder
 tmp: string;
    

initialization
//    assign(dh, 'debug.txt');                    // DEBUG
//    rewrite(dh);                                // DEBUG
//    writeln('Content-Type: text/plain');        // DEBUG
//    writeln;                                    // DEBUG

    // Init variables
    headers_sent:= false;

    // Init some defaults
    InitRTI;

    // Load up environment, print regular text error if not found
    if not LoadEnvVar then
    begin
      WriteLn('Content-Type: text/html'); //initialize a simple header
      WriteLn;
      Write('<pre><b>');
      WriteLn('PWU Error: <i>Could not get program environment.</i></b></pre>');
      halt;
    end;

    // Load up the config, print regular text error if not found
    if not ParseWebConfig then
    begin
      WriteLn('Content-Type: text/html'); //must have a header since not init yet
      WriteLn;
      Write('<pre><b>');
      WriteLn('PWU Error: <i>Could not find PWU configuration file.');
      WriteLn('           Please place ' + PWU_CONFIG_PATH + ' in your working directory.</i></b></pre>');
      halt;
    end;

    // Initialize the main headers since now that there aren't any above errors
    InitWebHeaders;

    // Load all available data
    if not GetWebData then
      ThrowWebError('Failed to recieve data from webserver');



finalization
   {$IFDEF GZIP_ENABLED}
    // Set content length if it can be set this way
    if (not headers_sent) and (length(ob) > 0) then
    begin

      if output_buffering and output_compression then
        DoWebCompress;

      str(length(ob), tmp);
      SetWebHeader('Content-Length', tmp);
    end;
   {$ENDIF}
     
    // Send headers and buffer if not sent already
    if not headers_sent then SendWebHeaders;

   {$IFDEF GZIP_ENABLED}
    if length(ob) <> 0 then FlushBuffer;
   {$ENDIF}
     
    //close(dh);    // DEBUG


end.

// END OF UNIT
{------------------------------------------------------------------------------}

