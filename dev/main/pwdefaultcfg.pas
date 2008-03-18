unit pwdefaultcfg;
{$I defines1.inc}

interface
uses
  pwtypes;


function GetCfgPath: astr;


implementation
uses
{$IFDEF UNIX}baseunix,{$ENDIF} {$IFDEF WINDOWS}windows,{$ENDIF}
 pwmain,
 pwenvvar,
 pwfileutil,
 strwrap1,
 pwsubstr,
 pwnative_out,
 pwerrors,
 pwdebugplugin;


const
 { System-wide configuration directory path on *NIX }

 { main configuration file name. }
 {$ifdef UNIX}    PWU_CFG_FILE     = 'pwu_unix.conf'; 
                  PWU_SYSCONF_PATH = '/etc/'; 
 {$endif}
 {$ifdef WINDOWS} PWU_CFG_FILE     = 'pwu_win.conf'; 
 {$endif}

{$IFDEF DBUG_ON}
 //var debugt: text;
 var debugt: longint;

 procedure debugln(s: string);
 begin
   pwdebugplugin.debugln(debugt, s);
 end;
{$ENDIF}


function GetCfgPath: astr;
var
  global_cfg_path1,
  global_cfg_path2,
  sys_cfg_path,
  tmpstr: string;
const
  GLOBAL_CFG_DIR = 'pwu/conf/'; 
begin 
 {$IFDEF PWUDEBUG}debugln('GetCfgPath begin'); {$ENDIF}
  result:= '';
 {$IFDEF WINDOWS}
  sys_cfg_path:= pwenvvar.GetEnvVar('WINDIR') +'/'+PWU_CFG_FILE;
 {$ENDIF}
 {$IFDEF UNIX}
  sys_cfg_path:= PWU_SYSCONF_PATH + PWU_CFG_FILE;
 {$ENDIF}
  xpath(sys_cfg_path); // cross platform slashes
  tmpstr:= GetEnvVar('DOCUMENT_ROOT');
  if tmpstr <> '' then 
  begin
    global_cfg_path1:=  tmpstr+'/../'+GLOBAL_CFG_DIR + PWU_CFG_FILE;
    global_cfg_path2:=  tmpstr+'/'+GLOBAL_CFG_DIR + PWU_CFG_FILE;
    xpath(global_cfg_path1);
    xpath(global_cfg_path2);
  end else
  begin
    global_cfg_path1:= '';
    global_cfg_path2:= '';
  end;
 {$IFDEF PWUDEBUG}
  debugln('search config path 1: ' + global_cfg_path1);
  debugln('search config path 2: ' + global_cfg_path2);
 {$ENDIF}  
  // First search for config file in current directory
  if FileExists_read(PWU_CFG_FILE) then
    result:= PWU_CFG_FILE
  else     // Try global DOCROOT/../pwu/conf (one back, hidden from public)
    if ( global_cfg_path1 <> '' ) and ( FileExists_read(global_cfg_path1) ) then
      result:= global_cfg_path1
    else   
      // Try global DOCROOT/pwu/conf/ location - http://example.com/pwu/conf/
      if ( global_cfg_path2 <> '' ) and ( FileExists_read(global_cfg_path2) ) then
        result:= global_cfg_path2
      else   // Try system-wide
        if FileExists_read(sys_cfg_path) then
          result:= sys_cfg_path;
 {$IFDEF PWUDEBUG}
  debugln('config path used: ' + result);          
  debugln('GetCfgPath end');   
 {$ENDIF}
end;


{ find config file and parse into conf[] var }
function ParseCfg: boolean;
var
 cfg_path,
 buff,
 name,
 value: astr;
 fh: text;
 i: integer;

label error1, error2;

begin
 {$IFDEF DBUG_ON} debugln('ParseCfg begin');{$ENDIF}
 result:= false;
 cfg_path:= GetCfgPath;
 // exit if cfg file not found
 if cfg_path = '' then goto error1;
 // open
 if not strwrap1.OpenFile(fh, cfg_path, 'r') then goto error1;
 // parse
 while not eof(fh) do
 begin
   readln(fh, buff);
   // skip empty lines and #comments
   if (buff = '') or (buff[1] = '#') then continue;
   i:= substrpos(buff, '=');
   // line must have equal sign if not a comment
   if i < 1 then goto error2;
   name:= copy(buff, 1, i - 1);
   value:= copy(buff, i + 1, length(buff) - i);
   name:= strtrim(name);
   value:= substrstrip(strtrim(value), '"');  // strip double quote
   value:= substrstrip(strtrim(value), ''''); // strip single quote
   if (name = '') or (value = '') then continue;
   iAddWebCfgVar(name, value);
 end;

{----EXIT OR HANDLE ERRORS----}

  close(fh);
  result:= true;
  {$IFDEF DBUG_ON} debugln('ParseCfg end');{$ENDIF}
  exit;

  error1: begin
            {$IFDEF DBUG_ON}debugln('ParseCfg: exit 1: cfg path error');{$ENDIF}
            exit; 
          end;

  error2: begin
            close(fh);
            {$IFDEF DBUG_ON}debugln('ParseCfg: invalid cfg file. Lines must start w/pound sign or have name=val pair');{$ENDIF}
            exit; 
          end;

end;

procedure InitCfg;
begin
  // can't find config file in global, local, or system path
  if not ParseCfg then ErrWithHeader(CANT_READ_CFG_FILE); 
end;

procedure UnitInit;
begin
 {$ifdef DBUG_ON} // init logging if enabled
  pwdebugplugin.DebugInit(debugt, 'pwdefaultcfg.debug.log');                                               
 {$endif}
  // setup this unit as a config plugin for pwmain
  pwmain.CustomCfgUnitInit:= {$ifdef FPC}@{$endif}InitCfg;
end;

procedure UnitFini;
begin
 {$IFDEF DBUG_ON}
  pwdebugplugin.DebugFini(debugt);
 {$ENDIF}
end;


initialization
  UnitInit;
finalization
  UnitFini;
end.

