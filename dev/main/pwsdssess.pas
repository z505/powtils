{*******************************************************************************

                                POWTILS 

********************************************************************************

--------------------------------------------------------------------------------
 Simple Session Addon
--------------------------------------------------------------------------------
  SDS (text database) session unit. Use this as an example how to make a MySQL, 
  Firebird, or PostGre session extension for Powtils. 
  Plugin your own session units!
--------------------------------------------------------------------------------
  Authors/Credits:
--------------------------------------------------------------------------------
  L505 (Lars Olson), and code from Trustmaster (Vladimir Sibirov)
  This file is copyright to above authors. Legal: see the Artistic License.
********************************************************************************}


unit pwsdssess;

{$DEFINE EXTRA_SECURE}

{$I defines1.inc}

interface
uses
  pwmain, pwtypes;

const PWU_SESS_FILE = 'pwusess.sds'; // default session filename
      DEFAULT_SESS_TIME = '25';

function CountSessVars: longword;
function FetchSessName(idx: longword): string;
function FetchSessVal(idx: longword): string;
function GetSess(const name: string): string;
function GetSessAsFloat(const name: string): double;
function GetSessAsInt(const name: string): longint;
function IsSess(const name: string): boolean;
function SessDestroy: boolean;
function SetSess(const name, value: string): boolean;
function SetSessAsFloat(const name: string; value: double): boolean;
function SetSessAsInt(const name: string; value: longint): boolean;
function UnsetSess(const name: string): boolean;


implementation
uses 
  pwenvvar,  pwurlenc, pwsds, pwfileutil, pwbase64enc, sysutils;

var sess: TWebVars;   
    sess_initialized: boolean = false; 


function IsSessionFlag(s: string): boolean;
begin
  result:= false;
  if (Lcase(s) = 'session_life_time') or (Lcase(s) = 'session_path') then
    result:= true;
end;


procedure SetDefaultSessCfg;
begin {$IFDEF DBUG_ON} debugln('SetDefaultSessCfg begin');{$ENDIF}
  // local directory session file
  iAddWebCfgVar('session_path', PWU_SESS_FILE);
  iAddWebCfgVar('session_life_time', DEFAULT_SESS_TIME);
 {$IFDEF DBUG_ON}debugln('SetDefaultSessCfg end');{$ENDIF}
end;

// Dumps vars into sess
function PutSessVars(const data: string): boolean;
var
 i,
 len: longword;
 lex, name, value: string;

 procedure AddToLex;
 begin
   SetLength(lex, length(lex) + 1);
   lex[length(lex)]:= data[i];
   inc(i);
 end;

begin
 {$IFDEF DBUG_ON} debugln('PutSessVars begin');{$ENDIF}
 // Init
 result:= false;
 name:= ''; value:= '';
 i:= 1;
 len:= length(data);
 // Parse out
 while (i <= len) do
 begin
   // Getting name
   lex:= '';
   while (i <= len) and (data[i] <> '=') do AddToLex;
   name:= UrlDecode(lex);
   inc(i);
   // Getting value
   lex:= '';
   while (i <= len) and (data[i] <> ';') do AddToLex;
   value:= UrlDecode(lex);
   iAddWebVar(sess, name, value);
   inc(i);
 end;
 // Done
 result:= true;
 {$IFDEF DBUG_ON} debugln('PutSessVars end');{$ENDIF}
end;

// Session garbage collector
function SessGC: boolean;
  function tmpdir: astr;
  begin
   {$IFDEF WINDOWS}
    result:= GetEnvVar('WINDIR') + '\' + PWU_SESS_FILE;
   {$ENDIF}
   {$IFDEF UNIX}
    result:= '/tmp/';
   {$ENDIF}
  end;
var
 SessTblPath, tmppath, sess_lim: astr;
 res: SDS_Result;
 sess_time: longword;
 limdate: TDateTime;
begin
 result:= false;
 SessTblPath:= GetCfgVar('session_path');
 // cross platform slashes
 xpath(SessTblPath);
 if not FileExists(SessTblPath) then
 begin
   // Searching in system temp
   tmppath:= TmpDir();
   if FileExists(tmppath) then
     SessTblPath:= tmppath
   else
     exit; // false
 end;
 // Checking lifetime in minutes
 val(GetCfgVar('session_life_time'), sess_time);
 if sess_time = 0 then
 begin
   result:= true;
   exit;
 end;
 limdate:= now - (sess_time / 1440);
 sess_lim:= FormatDateTime('yyyy-mm-dd hh:nn:ss', limdate);
 // Perform GC
 res:= pwsds.query(
        'DELETE FROM `' + SessTblPath + '` WHERE modified < "' + sess_lim + '"'
       );
 if pwsds.ResultError(res) = '' then result:= true;
 pwsds.FreeResult(res);
end;

// Gets session data
function SessStart: string;
var SessTblPath: string;
    res: SDS_Result;
    row: SDS_Array;
    key, sid: string;

 function SysSessPath: string;
 begin
   result:= {$IFDEF WINDOWS}GetEnvVar('WINDIR') + '\'{$ENDIF}
            {$IFDEF UNIX}'/tmp/'{$ENDIF}
            + PWU_SESS_FILE;
 end;

begin
 {$IFDEF DBUG_ON} debugln('SessStart begin');{$ENDIF}
 // Init
 result:= '';
 // Checking path
 SessTblPath:= GetCfgVar('session_path');
 // Ensure correct cross platform slashes in path first
 xpath(SessTblPath);
 //...
 if not FileExists_readwrite(SessTblPath) then
 begin
   // try in system temp
   if FileExists_readwrite(SysSessPath) then SessTblPath:= SysSessPath
   else
   begin
     {$IFDEF DBUG_ON} debugln('SessStart exit, file not found');{$ENDIF}
     exit;
   end;
 end;
 // Run garbage collector
 SessGC;
 // Is it registered
 if not IsCookie('PWUSESS') then
 begin
    //    session_registered:= false;
   SetRTI('SESSION_REGISTERED', 'FALSE');
   exit;
 end;
 //  session_registered:= true;
 SetRTI('SESSION_REGISTERED', 'TRUE');
 key:= Base64Decode(GetCookie('PWUSESS'));
 sid:= pwsds.Escape(copy(key, 13, length(key) - 12));
 key:= pwsds.Escape(copy(key, 1, 12));
 // Selecting
 res:= pwsds.Query('SELECT data FROM `' + SessTblPath + '` WHERE id = ' + sid + 
                     ' AND key = "' + key + '"');
 if pwsds.ResultRows(res) = 1 then
 begin
   row:= pwsds.FetchRow(res);
   result:= Base64Decode(pwsds.FetchColumn(row, 0));
   pwsds.FreeRow(row);
 end
   else
 begin
   result:= '';
   // Unset, it has timed out
   UnsetCookie('PWUSESS');
 end;
 pwsds.FreeResult(res);
 {$IFDEF DBUG_ON} debugln('SessStart end');{$ENDIF}
end;

procedure InitSess;
var s: string;
begin
  // only once
  if sess_initialized then exit;
  s:= '';
  s:= SessStart;
  if s <> '' then PutSessVars(s);
  sess_initialized:= true;
end;

// Updates session table due to sess or registers a new session
function SessUpdate: boolean;
var
 SessTblPath, data: string;
 res: SDS_Result;
 id, i: longword;
 key, sid: string;
 registered: boolean;
 tmpstr: string;
begin
 {$IFDEF DBUG_ON}debugln('SessUpdate begin');{$ENDIF}

 result:= false;
 if length(sess) < 1 then exit;
 SessTblPath:= GetCfgVar('session_path');
 // cross platform slashes
 xpath(SessTblPath);
 if not FileExists(SessTblPath) then
 begin
   // try in system temp
   tmpstr:= {$IFDEF WINDOWS}GetEnvVar('WINDIR') + '\' {$ENDIF}
            {$IFDEF UNIX}'/tmp/'{$ENDIF} + PWU_SESS_FILE;
   if ( FileExists(tmpstr) ) or (SessTblPath = '') then SessTblPath:= tmpstr;
 end;
 // Create session table if not exists
 if not FileExists_plain(SessTblPath) then
 begin
   res:= pwsds.Query('CREATE TABLE `' + SessTblPath + '` (id INT, key TEXT, data TEXT, modified DATETIME)');
   if pwsds.ResultRows(res) < 1 then
     ThrowErr('Can''t create sess table, SDS result: ' + pwsds.ResultError(res));
   pwsds.FreeResult(res);
 end;
 // Depending on whether session is registered
 if IsCookie('PWUSESS') then
 begin
   key:= Base64Decode(GetCookie('PWUSESS'));
   sid:= pwsds.Escape(copy(key, 13, length(key) - 12));
   key:= pwsds.Escape(copy(key, 1, 12));
   // Selecting
   res:= pwsds.Query('SELECT COUNT(*) FROM `' + SessTblPath + '` WHERE id = ' + sid + ' AND key = "' + key + '"');
   if pwsds.ResultRows(res) = 1 then registered:= true else
   begin
     registered:= false;
     // Unset the cookie
     UnsetCookie('PWUSESS');
   end;
   pwsds.FreeResult(res);
 end else
   registered:= false;
 // Generate new data string
 data:= '';
 if length(sess) > 0 then
   for i:= 0 to length(sess) - 1 do
     data:= data + UrlEncode(sess[i].name) + '=' + UrlEncode(sess[i].value) + ';';
 // Strip tail ;
 data:= copy(data, 1, length(data) - 1);
 data:= Base64Encode(data);
 if registered then
 begin
   // Updating
   res:= pwsds.Query('UPDATE `' + SessTblPath +
                       '` SET data = "' + pwsds.Escape(data) + '", ' +
                             'modified = NOW WHERE id = ' + sid +
                                             ' AND key = "' + key + '"');
   if pwsds.ResultRows(res) = 1 then result:= true;
   pwsds.FreeResult(res);
 end
   else
 begin
   // Check headers
   if headers_sent then
   begin
     ThrowErr('Can''t set new session, headers already sent');
     result:= false;
     exit;
   end;
   // Creating new one
   key:= RandomStr(12);
   res:= pwsds.Query('INSERT INTO `' + SessTblPath +
                       '` (key, data, modified) ' +
                       'VALUES ("' + key + '", "' +
                                 pwsds.Escape(data) + '", ' +
                                 'NOW' +')');
   if pwsds.ResultRows(res) = 1 then
   begin
     id:= pwsds.LastID(SessTblPath);
     str(id, sid);
     key:= Base64Encode(key + sid);
     SetCookie('PWUSESS', key);
     SetRTI('SESSION_REGISTERED', 'TRUE');
   end else
     result:= false;
   pwsds.FreeResult(res);
 end;
 {$IFDEF DBUG_ON} debugln('SessUpdate end');{$ENDIF}
end;

{ Destroys currently registered session }
function SessDestroy: boolean;
var SessTblPath, key, sid: string;
    res: SDS_Result;
begin
  result:= false;
  // Checking
  SessTblPath:= GetCfgVar('session_path');
  xpath(SessTblPath);
  if not FileExists(SessTblPath) then exit;
  // Checking if registered
  if not IsCookie('PWUSESS') then exit;
  // Extracting key and id
  key:= Base64Decode(GetCookie('PWUSESS'));
  sid:= pwsds.Escape(copy(key, 13, length(key) - 12));
  key:= pwsds.Escape(copy(key, 1, 12));
  // Running query
  res:= pwsds.Query('DELETE FROM `' + SessTblPath + '` WHERE id = ' + sid + ' AND key = "' + key + '"');
  if pwsds.ResultRows(res) = 1 then result:= true;
  pwsds.FreeResult(res);
  // Unset sess
  SetLength(sess, 0);
end;

{PUBLIC}

{ Returns value of session variable
 todo: research if security levels can be implemented }
function GetSess(const name: string): string;
var i: longword;
begin
 result:= '';
 if length(sess) = 0 then exit;
 for i:= 0 to length(sess) - 1 do if sess[i].name = name then
 begin
   result:= sess[i].value;
   break;
 end;
end;

{ Returns value of session variable as double precision float
 todo: research if security levels can be implemented }
function GetSessAsFloat(const name: string): double;
var i: longword;
begin
  result:= 0.0;
  if length(sess) = 0 then exit;
  for i:= 0 to length(sess) - 1 do if sess[i].name = name then
  begin
    val(sess[i].value, result);
    break;
 end;
end;

{ Returns value of session variable as integer
 todo: research if security levels can be implemented }
function GetSessAsInt(const name: string): longint;
var i: longword;
begin
 result:= 0;
 if length(sess) = 0 then exit;
 for i:= 0 to length(sess) - 1 do if sess[i].name = name then
 begin
   val(sess[i].value, result);
   break;
 end;
end;

{ Sets session variable }
function SetSess(const name, value: string): boolean;
begin
 result:= false;
 // Check
 if headers_sent then
 begin
   ThrowErr('SetSess: no effect, headers sent already');
   exit;
 end;

 // Change value if name exist, or add if not exist
 if not iUpdateWebVar(sess, name, value, CASE_SENSITIVE) then
    iAddWebVar(sess, name, value);

 result:= true;
end;

{ Sets session variable as double precision float }
function SetSessAsFloat(const name: string; value: double): boolean;
var s: string;
begin
 str(value, s);
 result:= SetSess(name, s);
end;

{ Sets session variable as integer }
function SetSessAsInt(const name: string; value: longint): boolean;
var s: string;
begin
 str(value, s);
 result:= SetSess(name, s);
end;

{ Unsets session variable }
function UnsetSess(const name: string): boolean;
var
 tmp: TWebVars;
 i: longword;
begin
 result:= false;
 // Check
 if headers_sent then
 begin
   ThrowErr('UnsetSess: no effect, headers sent already');
   exit;
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

{ Tells whether a session variable is assigned }
function IsSess(const name: string): boolean;
var i: longword;
begin
 result:= false;
 if length(sess) = 0 then exit;
 for i:= 0 to length(sess) - 1 do if sess[i].name = name then
 begin
   result:= true;
   break;
 end;
end;

{ Returns number of session variables }
function CountSessVars: longword;
begin
 result:= length(sess);
end;

{ Indexed access to session name }
function FetchSessName(idx: longword): string;
begin
 if (idx < longword(length(sess))) and (length(sess) > 0) then
   result:= sess[idx].name
 else
   result:= '';
end;

{ Indexed access to session value }
function FetchSessVal(idx: longword): string;
begin
 if (idx < longword(length(sess))) and (length(sess) > 0) then
   result:= sess[idx].value
 else
   result:= '';
end;
{END PUBLIC}

procedure InitSessUnit;
begin
  // if not using pwu config file, set default configs
  if not(iCustomCfgUnitSet) then SetDefaultSessCfg;
  InitSess;
end;

procedure LocalInit;
begin
  // setup plugin functions
  CustomSessUpdate:= {$IFDEF FPC}@{$ENDIF}SessUpdate;
  CustomSessUnitInit:= {$IFDEF FPC}@{$ENDIF}InitSessUnit;
end;

initialization
//  writeln('DEBUG: INIT pwsdssess');

  LocalInit;

finalization

end.


