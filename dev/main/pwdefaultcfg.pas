unit pwdefaultcfg;
{$I defines1.inc}

interface
uses pwtypes;

function GetCfgPath: astr;

implementation
uses 
 {$ifdef UNIX}baseunix{$endif} {$ifdef WINDOWS}windows{$endif},
  pwmain, pwenvvar, pwfileutil, strwrap1, pwsubstr, pwnative_out,  pwerrors,
  pwdebugplugin;

const
 { System-wide configuration directory path on *NIX }
 {$ifdef UNIX}   PWU_CFG_FILE     = 'pwu_unix.conf'; 
                 PWU_SYSCONF_PATH = '/etc/'; 
 {$endif}
 {$ifdef WINDOWS}PWU_CFG_FILE     = 'pwu_win.conf'; 
 {$endif}

{$ifdef DBUG_ON}
 // var debugt: text;
 var debugt: longint;

 procedure debugln(s: string);
 begin
   pwdebugplugin.debugln(debugt, s);
 end;
{$endif}


function GetCfgPath: astr;
var
  global_cfg_path1, global_cfg_path2, sys_cfg_path, tmpstr: astr;
const
  GLOBAL_CFG_DIR = 'pwu/conf/'; 
{b}                                                                             begin {$ifdef DBUG_ON}debugln('GetCfgPath begin');{$endif}
  result:= '';
 {$ifdef WINDOWS}sys_cfg_path:= pwenvvar.GetEnvVar('WINDIR') +'/'+PWU_CFG_FILE;{$endif}
 {$ifdef UNIX} sys_cfg_path:= PWU_SYSCONF_PATH + PWU_CFG_FILE;{$endif}
  xpath(sys_cfg_path); // cross platform slashes
  tmpstr:= GetEnvVar('DOCUMENT_ROOT');
  if tmpstr <> '' then begin
    global_cfg_path1:=  tmpstr+'/../'+GLOBAL_CFG_DIR + PWU_CFG_FILE;
    global_cfg_path2:=  tmpstr+'/'+GLOBAL_CFG_DIR + PWU_CFG_FILE;
    xpath(global_cfg_path1);
    xpath(global_cfg_path2);
  end else begin
    global_cfg_path1:= '';
    global_cfg_path2:= '';
  end;
 {$ifdef DBUG_ON}debugln('search config path 1: ' + global_cfg_path1); 
                 debugln('search config path 2: ' + global_cfg_path2);
 {$endif}  
  // First search for config file in current directory
  if FileThere(PWU_CFG_FILE, fmR) then
    result:= PWU_CFG_FILE
  else  // Try global DOCROOT/../pwu/conf (one back, hidden from public)
  if ( global_cfg_path1 <> '' ) and ( FileThere(global_cfg_path1, fmR) ) then
    result:= global_cfg_path1
  else  // Try global DOCROOT/pwu/conf/ location - http://example.com/pwu/conf/ 
  if ( global_cfg_path2 <> '' ) and ( FileThere(global_cfg_path2, fmR) ) then
    result:= global_cfg_path2
  else   // Try system-wide
  if FileThere(sys_cfg_path, fmR) then
    result:= sys_cfg_path;
{e}                                                                             {$ifdef DBUG_ON}debugln('config path used: ' + result); debugln('GetCfgPath end'); {$endif} end;


{ find config file and parse into conf[] var }
function ParseCfg: boolean;
var
  cfg_path, buff, name, value: astr;
  fh: text;
  i: integer;

 {$ifdef DBUG_ON}
  procedure Err1;
  begin 
    debugln('ParseCfg: cfg path error');
  end;

  procedure Err2;
  begin 
    debugln('ParseCfg: invalid cfg file. Lines must start w/pound sign or have name=val pair');
  end;
 {$endif}

{b}                                                                             begin {$ifdef DBUG_ON}ParseCfg_B;{$endif}
  result:= false;
  cfg_path:= GetCfgPath;
  // open
  if (cfg_path = '') or (not pwfileutil.OpenFile(fh, cfg_path, 'r')) then begin                      
    {$ifdef DBUG_ON}Err1;{$endif} 
    // exit if cfg file not found
    exit; 
  end;

  // parse
  while not eof(fh) do
  begin
    readln(fh, buff);
    // skip empty lines and #comments
    if (buff = '') or (buff[1] = '#') then continue;
    i:= substrpos(buff, '=');
    // line must have equal sign if not a comment
    if i < 1 then begin
      close(fh);
      {$ifdef DBUG_ON}Err2;{$endif} 
      exit; 
    end;
    name:= copy(buff, 1, i-1);
    value:= copy(buff, i+1, length(buff) - i);
    name:= strtrim(name);
    value:= substrstrip(strtrim(value), '"');  // strip double quote
    value:= substrstrip(strtrim(value), ''''); // strip single quote
    if (name = '') or (value = '') then continue;
    iAddWebCfgVar(name, value);
  end;

  close(fh);
  result:= true;
{e}                                                                             {$ifdef DBUG_ON}ParseCfg_E;{$endif} end;


procedure InitCfg;
begin
  // Halt if can't find config file in global, local, or system path
  if not ParseCfg then ErrWithHeader(CANT_READ_CFG_FILE); 
end;

{ setup this unit as config plugin for pwmain }
procedure UnitInit;
begin
 {$ifdef DBUG_ON} // init logging if enabled
  pwdebugplugin.DebugInit(debugt, 'pwdefaultcfg.debug.log');                                               
 {$endif}
  pwmain.CustomCfgUnitInit:= {$ifdef FPC}@{$endif}InitCfg;
end;

procedure UnitFini;
begin
 {$ifdef DBUG_ON}
  pwdebugplugin.DebugFini(debugt);
 {$endif}
end;


initialization
  UnitInit;
finalization
  UnitFini;
end.

