{ Modified March 2008 by Lars Olson. Aservia (web server). Based on nYume }

program aservia; 
{$ifdef fpc}{$mode objfpc}{$H+}{$UNITPATH ../../main/}{$endif} 
{$R+}

uses
  {$ifdef unix}cthreads, baseunix, unix,{$endif} 
  {$ifdef windows}windows,{$endif}
  zserver, cfgfile, pwfileutil, pwstrutil, pwtypes, shell;

{$include lang.inc}

// turn these on for more verbose reporting
// TODO: put in a config file option
const LOGGING_ON = false;
      MESSAGES_ON = false;   
      DEBUGGING_ON = false;   

// NOTE: MUST BE HTTP/1.0 - DOESN'T SUPPORT 1.1 CHUNK ENCODING, ETC.
const
      HTTP_VERSION = 'HTTP/1.0';
      ERR_404_MSG  = '404 File not found';
      ERR_403_MSG  = '403 Access denied';
      DOC_ROOT     = 'DOCUMENT_ROOT';
      SCRIPT_NAME  = 'SCRIPT_NAME';
      REQ_METHOD   = 'REQUEST_METHOD';
      QUERY_STRING = 'QUERY_STRING';
      SERV_SOFT    = 'SERVER_SOFTWARE';
      REMOTE_ADDR  = 'REMOTE_ADDR';
      THREAD_LOOP_SLEEP = 10;

var
  server   : TzServer;
  str1     : astr;
  critical : TRTLCriticalSection;
  fl       : text;
  mimecfg, vhostcfg: TCfgFile;  
  logflname: astr;
  filedir  : astr;
  ip       : astr;
  port     : word;
  newlog   : boo;
  idxpg: astr; // default index page
  error404 : astr;
  error403 : astr;
  blacklist: array of astr;
  needStopServer: boo;
  handle   : int32;

{ debugging to console }
procedure dbugln(s: astr); overload;   
begin if DEBUGGING_ON then writeln('DEBUG: ', s); 
end;

procedure dbugln(s1, s2: astr);        
begin dbugln(s1+s2); 
end;

procedure logln(var t: text; s: astr);  
begin if LOGGING_ON then writeln(t, s); 
end;

procedure logln(var t: text);  
begin logln(t, ''); 
end;


{ write a line to console }

procedure msgln(s1: astr);        
begin if MESSAGES_ON then writeln(s1); 
end;

procedure msgln(s1,s2: astr);        
begin if MESSAGES_ON then writeln(s1,s2); 
end;

procedure msgln(s1,s2,s3: astr);     
begin if MESSAGES_ON then writeln(s1,s2, s3); 
end;

procedure msgln(s1: astr; i: int32); 
begin if MESSAGES_ON then writeln(s1,i); 
end;

{ always write a note even if MESSAGES_ON is off }
procedure noteln(s1: astr);            begin  writeln(s1); end;
procedure noteln(s1,s2: astr);         begin  writeln(s1,s2); end;
procedure noteln(s1,s2,s3: astr);      begin  writeln(s1,s2,s3); end;
procedure noteln(s1,s2,s3,s4: astr);   begin  writeln(s1,s2,s3,s4); end;

procedure DeleteTrailingHttpVersion(var s: astr);
var found: int32;

  procedure removestr;
  begin
    if found > 0 then delete(s, found, (length(s)-found) +1);  
  end;

begin
  found:= pos(' HTTP/1.', s);
  removestr;
  found:= pos(' HTTP/2.', s);   // future proof 
  removestr;
  found:= pos(' HTTP/3.', s);   // future proof 
  removestr;
end;

{ user commands to stop server with keyboard at console }
function needStop(p: pointer): int32;
var s: astr;
begin
  s:= '';
  repeat
    readln(s);
    if (s = 'q') or (s = 'quit') then needStopServer:= true;
    sleep(800);
  until needStopServer;
  noteln(str_close);
  server.Stop;
  result:= 0;
end;
  
function parceRequest(req: astr): astr;
  { wrapper for pos() }
  function found(sub, instr: astr): bln;
  begin
    result:= false;
    if pos(sub, instr) <> 0 then result:= true;
  end;

var
  strings: array of astr; 
  postd: astr; 
  i: int32;
begin
  result := '';
  setLength(strings, 0);
  i:= pos(#13#10, req);
  while i <> 0 do begin
    setLength(strings, length(strings) + 1);
    strings[length(strings) - 1] := copy(req, 1, i - 1);
    delete(req, 1, i+1);
    i:= pos(#13#10, req);
  end;

  if length(strings) > 0 then
  for i:= 0 to length(strings) - 1 do
  begin
    if found('GET ', strings[i]) then begin
      result:= strings[i];
      // trim first 'GET ' part
      delete(result, 1, 4);
      addenv(REQ_METHOD, 'GET');
    end
    else if found('POST ', strings[i]) then
    begin
      result:= strings[i];
      // trim first 'POST ' part
      delete(result, 1, 5);
      filedir := '';
      postd := copy(req, pos(#13#10#13#10, req), length(req));
      postd := StringReplace(postd, '&', ';', [rfReplaceAll]);
      {$ifdef mswindows}postd := StringReplace(postd, '%', '%%', [rfReplaceAll]);{$endif}
      addenv(QUERY_STRING, postd);
      addenv(REQ_METHOD, 'POST');
    end else if found('User-Agent:', strings[i]) then begin
      delete(strings[i], 1, 12);
      addEnv('HTTP_USER_AGENT', strings[i]);
    end else if found('Host:', strings[i]) then begin
      delete(strings[i], 1, 6);
      if pos(':', strings[i]) <> 0 then
        strings[i] := copy(strings[i], 1, pos(':', strings[i]) - 1);
      addEnv('HTTP_HOST', strings[i]);
      filedir := strings[i];
    end else if found('Referer:', strings[i]) then begin
      delete(strings[i], 1, 9);
      addEnv('HTTP_REFERER', strings[i]);
    end;
  end;

  DeleteTrailingHttpVersion(result);
  if (result = '') or (result[length(Result)] = '/') then result += idxpg;
end;
  
function gettype(fname: astr): astr;
var ext  : astr;
    def  : astr;
begin
  def:= mimecfg.getOpt('default', 'application/force-download');;
  ext:= LowerCase(ExtractFileExt(fname));
  delete(ext, 1, 1);
  Result := mimecfg.getOpt(ext, def);
end;

function getFile(fname: astr; errcode: astr = '200 OK'; 
                 query: astr = ''): astr;
var
  fl: file;
  buf: astr;
  count: int32;
  filetype: astr;
  parameters: astr = '';
begin
  filetype := gettype(fname);
  if filetype <> 'execute/cgi' then
  begin
    setLength(buf, 4096);
    Result := '';
    assign(fl, fname);
    reset(fl, 1);
    result:= HTTP_VERSION+' ' + errcode + #13#10 + 
             str_server + #13#10 +
            'MIME-version: 1.0'#13#10 +
            'Allow: GET, POST'#13#10 +
            'Content-type: ' + filetype + #13#10 +
            'Content-length: ' + IntToStr(FileSize(fl)) + #13#10 + 
             #13#10;
    while not eof(fl) do begin
      blockRead(fl, pointer(buf)^, 4096, count);
      if count < 4096 then buf := copy(buf, 1, count);
      result += buf;
    end;  
    Close(fl);
  end else begin
    if query <> '' then begin
      count := pos('?', query);
      if count > 0 then parameters := copy(query, count + 1, length(query));
    end;
   {$ifdef mswindows}
    parameters := StringReplace(parameters, '%', '%%', [rfReplaceAll]);
   {$endif}
    parameters := StringReplace(parameters, '&', ';', [rfReplaceAll]);
    Result := HTTP_VERSION+' '+errcode+#13#10 
              + command(fname + ' "' + parameters + '"');
  end;
end;  

function request(p: pointer): int32;
var path: astr;

  function SlashDots: bln;
  begin
    result:= false;
    if pos('/../', ExtractRelativepath(filedir, path)) <> 0 then result:= true; 
  end;

var
  deny: boo;

  procedure Log;
  begin
    if not LOGGING_ON then exit;
   {$I-}
    Assign(fl, logflname); Append(fl); 
    if IOResult <> 0 then Rewrite(fl);
    logln(fl, str_requestfrom+ip+'; '{, DateTimeToStr(Date), ', ', TimeToStr(Time)});
    logln(fl, str1);
    if deny then logln(fl, str_denied);
    close(fl);
   {$I+}
  end;

var fname, name, ip: astr;
    i: int32;
    pInt: int32;
begin
  deny:= false;
  pInt:= int32(p^);
  enterCriticalSection(critical);
  try
    clearEnv;
    server.select(pInt);
    str1:= server.sRead;
    msgln(str_request{, DateTimeToStr(Date), ', ', TimeToStr(Time)});
    msgln(str1);
    ip:= server.getip(pInt);
    addEnv(REMOTE_ADDR, ip);
    addEnv(SERV_SOFT, str_server);
    i:= 0;
    while (not deny) and (i < length(blacklist)) do begin
      if ip = blacklist[i] then begin 
        deny:= true; 
        msgln(str_denied); 
      end;
      inc(i);
    end;
    dbugln('Checked blacklist');
    if deny then server.sWrite(getFile(error403, ERR_403_MSG)) 
    else
    if str1 <> '' then
    begin
      fname := parceRequest(str1);
      if (filedir = '') or (filedir = ip) then 
        filedir:= vhostcfg.getOpt('default', '')
      else begin
        filedir:= vhostcfg.getOpt(filedir, vhostcfg.getOpt('default', ''));
        filedir:= excludeTrailingPathDelimiter(filedir);
      end;
      addEnv(DOC_ROOT, filedir);
      name:= fname;
      if pos('?',fname) > 0 then name:= copy(fname,1,pos('?',fname) - 1);
      path:= filedir + name; 
      xpath(path);
      if (not fileThere(path)) and dirExists(path) then name:= name+'/'+idxpg;
      addEnv(SCRIPT_NAME, name);
      path:= filedir + name;
      xpath(path); // cross platform slashes
      if FileThere(path) then begin
        if SlashDots then begin
          server.sWrite(getFile(error403, ERR_403_MSG));
          deny:= true;
        end else 
          server.sWrite(getFile(path, '200 OK', fname))
      end else begin
        server.sWrite(getFile(error404, ERR_404_MSG));
      end;
    end;
    dbugln('Before log() procedure');
    log;

  finally
    dbugln('Disconnecting: '+i2s(pInt));
    server.disconnect(pInt);
    leaveCriticalSection(critical);
    dbugln('Left Critical Section');
  end;
  result:= 0;
  EndThread;
  dbugln('Ended thread');
end;

procedure setupLog;
begin
  if not LOGGING_ON then exit;
 {$I-}
  assign(fl, logflname);
  if not newlog then Append(fl);
  if newlog or (IOResult <> 0) then Rewrite(fl);
  logln(fl, str_server+#13#10);
  logln(fl, str_runserver{, DateTimeToStr(Date), ', ', TimeToStr(Time)});
  logln(fl, str_socket+ip+':'+i2s(port));
  logln(fl);  
  close(fl);
 {$I+}
end;

{ start thread to detect keyboard q/quit at console }
procedure beginNeedStopThread;
var id: TTHreadId = 0; 
    //err: dword = 0;
begin
  //id:= BeginThread(nil,DefaultStackSize,@needStop,nil,0,dummy);
   { UNIX and WINDOWS return different results (fpc bug) }
  id:= beginThread(@needStop);
  if id <> 0 then dbugln('(needStopThread) beginThread Result#: '+i2s(id));
//  err:= suspendThread(id);
//  if err <> 0 then dbugln('suspendThread Error#: ', err);
//  err:= 0;
//  err:= resumeThread(id);
//  if err > 0 then dbugln('resumeThread Error#: '+i2s(err));
end;

{ start connection threads }
procedure beginRequestThread;
var id: TTHreadId = 0; 
    //err: dword = 0;
begin
  //id:= BeginThread(nil,DefaultStackSize,@needStop,nil,0,dummy);
  id:= beginThread(@request, @handle);
  { UNIX and WINDOWS return different results (fpc bug) }
  if id <> 0 then dbugln('(requestThread) beginThread Result#: '+i2s(id));
//  err:= suspendThread(id);
//  if err <> 0 then dbugln('suspendThread Error#: ', err);
//  err:= 0;
//  err:= resumeThread(id);
//  if err > 0 then dbugln('resumeThread Error#: '+i2s(err));
end;

{ main server connections loop for all requests }
procedure runThreadLoop;
begin
  beginNeedStopThread;
  initCriticalSection(critical);
  repeat
    msgln(str_wait);
    handle:= server.connect;
    if (not needStopServer) and (handle >= 0) then begin
      msgln(str_connection);
      beginRequestThread;
    end else begin
      dbugln('runThreadLoop Handle: '+i2s(handle));
    end;
    sleep(THREAD_LOOP_SLEEP); 
  until needStopServer;
  doneCriticalSection(critical);
end;

procedure errLn(const msg: astr); 
begin writeln('E: ',msg);
end;

procedure errLn; 
begin errLn('');
end;

procedure CreateCfgAndServer;
  { retreive configuration file info, or use defaults if not avail }
  procedure GetMainCfg;
  var othercfg: TCfgFile;
  begin
    othercfg:= TCfgFile.create('config.cfg');
      ip        := othercfg.getOpt('ip',        '127.0.0.1');
      port      := othercfg.getOpt('port',      80);
      newlog    := othercfg.getOpt('deletelog', false);
      logflname := othercfg.getOpt('logfile',   'connections.log');
      idxpg     := othercfg.getOpt('index',     'index.html');
      error404  := othercfg.getOpt('error404',  'error404.html');
      error403  := othercfg.getOpt('error403',  'error403.html');
    othercfg.free; othercfg:= nil;

    othercfg:= TCfgFile.create('blacklist.cfg');
      blacklist:= othercfg.getAllOpts;
    othercfg.free; othercfg:= nil;
  end;

begin
  GetMainCfg;
  mimecfg:= TCfgFile.create('mime.cfg');
  vhostcfg:= TCfgFile.create('vhost.cfg');
  server:= TzServer.Create;
end;

{ cleanup }
procedure FreeCfgAndServer;
begin
  vhostcfg.Free; vhostcfg:= nil;
  mimecfg.Free; mimecfg:= nil;
  server.Free; server:= nil;
end;

procedure ErrCantConnect;
begin
  ErrLn;
  ErrLn('Can''t connect to address or port.');
  ErrLn('The ip:port you are using is '+ip+':'+inttostr(port));
  ErrLn('Tip: make sure another server is not running.');
  ErrLn('Error # '+ {$ifdef windows}inttostr(GetLastError){$endif}
                    {$ifdef unix}inttostr(fpGetErrNo){$endif}
  ); 
  { Ugly inline ifdef above due to FPC bug, can't wrap in another func with 
    fpc 2.2.0. See http://bugs.freepascal.org/view.php?id=10205 }

  // cleanup, then kill
  FreeCfgAndServer; Halt;
end;

{ initiates logs, runs thread loop }
procedure RunServer;
var inited: boo;
begin
  noteln(str_server);
  noteln(str_qcom, #13#10);
  noteln(str_runserver{, DateTimeToStr(Date), ', ', TimeToStr(Time)});
  createCfgAndServer;
  inited:= server.initConnection(ip, port);
  if inited then begin
    noteln(str_socket, ip, ':', i2s(port));
    setupLog;
    runThreadLoop;
  end else 
    errCantConnect;
  freeCfgAndServer;
end;


///////////////////////////////////////////////////////////////////////////////
begin
  needStopServer:= false;
  runServer;
end.
///////////////////////////////////////////////////////////////////////////////
