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
const LOG_ON = false;
      MESSAGES_ON = false;   
      DEBUG_ON = false;   

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

// globals prefixed with g
var
  gServer: TzServer;     // main server instance
  gPort: word;           // server port
  gIp: astr              //    ... ip
  gIdxPg,                // default index page  
  gError404, gError403,  // error strings
  gFileDir: astr;        // path for files to serve
  gLogFname: astr        // log file name
  gLog: text;            // log file record
  gHandle: int32;
  gMimeCfg, 
  gVhostCfg: TCfgFile;  
  gNewLog: boo;
  gCritical: TRTLCriticalSection;
  gBlacklist: array of astr;
  gNeedStopServer: boo;


{ debugging to console }
procedure dbugln(s: astr);   begin if DEBUG_ON then writeln('DEBUG: ', s); end;
procedure dbugln(s1,s2:astr);begin dbugln(s1+s2); end;

procedure logln(var t:text; s:astr); begin if LOG_ON then writeln(t, s); end;
procedure logln(var t: text); begin logln(t, ''); end;

{ write a line to console if messages on }
procedure msgln(s1: astr);          begin if MESSAGES_ON then writeln(s1); end;
procedure msgln(s1,s2: astr);       begin msgln(s1+s2); end;
procedure msgln(s1,s2,s3: astr);    begin msgln(s1+s2+s3); end;
procedure msgln(s1:astr; i:int32);  begin msgln(s1+i2s(i)); end;

{ write a note even if MESSAGES_ON is off }
procedure noteln(s1: astr);            begin  writeln(s1); end;
procedure noteln(s1,s2: astr);         begin  writeln(s1,s2); end;
procedure noteln(s1,s2,s3: astr);      begin  writeln(s1,s2,s3); end;
procedure noteln(s1,s2,s3,s4: astr);   begin  writeln(s1,s2,s3,s4); end;

procedure DeleteTrailingHttpVersion(var s: astr);
var found: int32;

  procedure removestr;
  begin if found > 0 then delete(s, found, (length(s)-found) +1);  
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
    if (s = 'q') or (s = 'quit') then gNeedStopServer:= true;
    sleep(800);
  until gNeedStopServer;
  noteln(str_close);
  gServer.stop;
  result:= 0;
end;
  
function parceRequest(req: astr): astr;
  { wrapper for pos() }
  function found(sub, instr: astr): boo;
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
      gFileDir := '';
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
      gFileDir := strings[i];
    end else if found('Referer:', strings[i]) then begin
      delete(strings[i], 1, 9);
      addEnv('HTTP_REFERER', strings[i]);
    end;
  end;

  DeleteTrailingHttpVersion(result);
  if (result = '') or (result[length(result)] = '/') then result += gIdxPg;
end;
  
function getType(fname: astr): astr;
var ext, def: astr;
begin
  def:= gMimeCfg.getOpt('default', 'application/force-download');;
  ext:= lowerCase(ExtractFileExt(fname));
  delete(ext, 1, 1);
  result := gMimeCfg.getOpt(ext, def);
end;

function getFile(fname:astr; errcode:astr = '200 OK'; query:astr = ''): astr;
var
  F: file;
  count: int32;
  buf, filetype: astr;
  parameters: astr = '';
begin
  filetype := gettype(fname);
  if filetype <> 'execute/cgi' then
  begin
    setLength(buf, 4096);
    result := '';
    assign(F, fname);
    reset(F, 1);
    result:= HTTP_VERSION+' ' + errcode + #13#10 + 
             str_server + #13#10 +
            'MIME-version: 1.0'#13#10 +
            'Allow: GET, POST'#13#10 +
            'Content-type: ' + filetype + #13#10 +
            'Content-length: ' + IntToStr(FileSize(F)) + #13#10 + 
             #13#10;
    while not eof(F) do begin
      blockRead(F, pointer(buf)^, 4096, count);
      if count < 4096 then buf := copy(buf, 1, count);
      result += buf;
    end;  
    Close(F);
  end else begin
    if query <> '' then begin
      count := pos('?', query);
      if count > 0 then parameters := copy(query, count + 1, length(query));
    end;
   {$ifdef mswindows}
    parameters := StringReplace(parameters, '%', '%%', [rfReplaceAll]);
   {$endif}
    parameters := StringReplace(parameters, '&', ';', [rfReplaceAll]);
    result := HTTP_VERSION+' '+errcode+#13#10 
              + command(fname + ' "' + parameters + '"');
  end;
end;  

function request(p: pointer): int32;
var path: astr;

  function SlashDots: boo;
  begin
    result:= false;
    if pos('/../', ExtractRelativepath(gFileDir, path)) <> 0 then result:= true; 
  end;

var
  deny: boo = false;
  str1: astr = '';

  procedure log;
  begin
    if not LOG_ON then exit;
   {$I-}
    Assign(gLog, gLogFname); Append(gLog); 
    if IOResult <> 0 then Rewrite(gLog);
    logln(gLog, str_requestfrom+gIp+'; '{, DateTimeToStr(Date), ', ', TimeToStr(Time)});
    logln(gLog, str1);
    if deny then logln(gLog, str_denied);
    close(gLog);
   {$I+}
  end;

var fname, name, gIp: astr;
    i, data: int32;
begin
  // incoming pointer converted to strong type
  data:= int32(p^);
  enterCriticalSection(gCritical);
  try
    clearEnv;
    gServer.select(data);
    str1:= gServer.sRead;
    msgln(str_request{, DateTimeToStr(Date), ', ', TimeToStr(Time)});
    msgln(str1);
    gIp:= gServer.getIp(data);
    addEnv(REMOTE_ADDR, gIp);
    addEnv(SERV_SOFT, str_server);
    i:= 0;
    while (not deny) and (i < length(gBlacklist)) do begin
      if gIp = gBlacklist[i] then begin 
        deny:= true; 
        msgln(str_denied); 
      end;
      inc(i);
    end;
    dbugln('Checked blacklist');
    if deny then gServer.sWrite(getFile(gError403, ERR_403_MSG)) 
    else
    if str1 <> '' then
    begin
      fname := parceRequest(str1);
      if (gFileDir = '') or (gFileDir = gIp) then 
        gFileDir:= gVhostCfg.getOpt('default', '')
      else begin
        gFileDir:= gVhostCfg.getOpt(gFileDir, gVhostCfg.getOpt('default', ''));
        gFileDir:= excludeTrailingPathDelimiter(gFileDir);
      end;
      addEnv(DOC_ROOT, gFileDir);
      name:= fname;
      if pos('?',fname) > 0 then name:= copy(fname,1,pos('?',fname) - 1);
      path:= gFileDir + name; 
      xpath(path);
      if (not fileThere(path)) and dirExists(path) then name:= name+'/'+gIdxPg;
      addEnv(SCRIPT_NAME, name);
      path:= gFileDir + name;
      xpath(path); // cross platform slashes
      if FileThere(path) then begin
        if SlashDots then begin
          gServer.sWrite(getFile(gError403, ERR_403_MSG));
          deny:= true;
        end else 
          gServer.sWrite(getFile(path, '200 OK', fname))
      end else begin
        gServer.sWrite(getFile(gError404, ERR_404_MSG));
      end;
    end;
    dbugln('Before log() procedure');
    log;

  finally
    dbugln('Disconnecting: '+i2s(data));
    gServer.disconnect(data);
    leaveCriticalSection(gCritical);
    dbugln('Left Critical Section');
  end;
  result:= 0;
  // IF ENDTHREAD IS NOT PLACED HERE, THEN LINUX KEEPS OPENING THREADS TO A 
  // MAXIMUM OF 380 AND CAUSES A CONTINUAL LEAKAGE
  endThread;
  dbugln('Ended thread');
end;

procedure setupLog;
begin
  if not LOG_ON then exit;
 {$I-}
  assign(gLog, gLogFname);
  if not gNewLog then append(gLog);
  if gNewLog or (IOResult <> 0) then rewrite(gLog);
  logln(gLog, str_server+#13#10);
  logln(gLog, str_runserver{, DateTimeToStr(Date), ', ', TimeToStr(Time)});
  logln(gLog, str_socket+gIp+':'+i2s(gPort));
  logln(gLog);  
  close(gLog);
 {$I+}
end;

{ start thread to detect keyboard q/quit at console }
procedure beginNeedStopThread;
var id: TThreadId = 0; 
begin
  //id:= BeginThread(nil,DefaultStackSize,@needStop,nil,0,dummy);
  id:= beginThread(@needStop);
  { UNIX and WINDOWS return different results (fpc bug) }
  if id <> 0 then dbugln('(needStopThread) beginThread Result#: '+i2s(id));
end;

{ start connection threads }
procedure beginRequestThread;
var id: TTHreadId = 0; 
begin
  //id:= BeginThread(nil,DefaultStackSize,@needStop,nil,0,dummy);
  id:= beginThread(@request, @gHandle);
  { UNIX and WINDOWS return different results (fpc bug) }
  if id <> 0 then dbugln('(requestThread) beginThread Result#: '+i2s(id));
end;

{ main server connections loop for all requests }
procedure runThreadLoop;
begin
  beginNeedStopThread;
  initCriticalSection(gCritical);
  repeat
    msgln(str_wait);
    gHandle:= gServer.connect;
    if (not gNeedStopServer) and (gHandle >= 0) then begin
      msgln(str_connection);
      beginRequestThread;
    end else begin
      dbugln('runThreadLoop Handle: '+i2s(gHandle));
    end;
    sleep(THREAD_LOOP_SLEEP); 
  until gNeedStopServer;
  doneCriticalSection(gCritical);
end;

procedure errLn(const msg: astr); 
begin writeln('E: ',msg);
end;

procedure errLn; 
begin errLn('');
end;

procedure createCfgAndServer;
  { retreive configuration file info, or use defaults if not avail }
  procedure GetMainCfg;
  var othercfg: TCfgFile;
  begin
    othercfg:= TCfgFile.create('config.cfg');
      gIp        := othercfg.getOpt('ip',        '127.0.0.1');
      gPort      := othercfg.getOpt('port',      80);
      gNewLog    := othercfg.getOpt('deletelog', false);
      gLogFname := othercfg.getOpt('logfile',   'connections.log');
      gIdxPg     := othercfg.getOpt('index',     'index.html');
      gError404  := othercfg.getOpt('error404',  'error404.html');
      gError403  := othercfg.getOpt('error403',  'error403.html');
    othercfg.free; othercfg:= nil;

    othercfg:= TCfgFile.create('blacklist.cfg');
      gBlacklist:= othercfg.getAllOpts;
    othercfg.free; othercfg:= nil;
  end;

begin
  getMainCfg;
  gMimeCfg:= TCfgFile.create('mime.cfg');
  gVhostCfg:= TCfgFile.create('vhost.cfg');
  gServer:= TzServer.Create;
end;

{ cleanup }
procedure freeCfgAndServer;
begin
  gVhostCfg.Free; gVhostCfg:= nil;
  gMimeCfg.Free; gMimeCfg:= nil;
  gServer.Free; gServer:= nil;
end;

procedure errCantConnect;
begin
  ErrLn;
  ErrLn('Can''t connect to address or port.');
  ErrLn('The ip:port you are using is '+gIp+':'+inttostr(gPort));
  ErrLn('Tip: make sure another server is not running.');
  ErrLn('Error # '+ {$ifdef windows}inttostr(GetLastError){$endif}
                    {$ifdef unix}inttostr(fpGetErrNo){$endif}
  ); 
  { Ugly inline ifdef above due to FPC bug, can't wrap in another func with 
    fpc 2.2.0. See http://bugs.freepascal.org/view.php?id=10205 }

  // cleanup, then kill
  freeCfgAndServer; Halt;
end;

{ initiates logs, runs thread loop }
procedure runServer;
var inited: boo;
begin
  noteln(str_server);
  noteln(str_qcom, #13#10);
  noteln(str_runserver{, DateTimeToStr(Date), ', ', TimeToStr(Time)});
  createCfgAndServer;
  inited:= gServer.initConnection(gIp, gPort);
  if inited then begin
    noteln(str_socket, gIp, ':', i2s(gPort));
    setupLog;
    runThreadLoop;
  end else 
    errCantConnect;
  freeCfgAndServer;
end;


///////////////////////////////////////////////////////////////////////////////
begin
  gNeedStopServer:= false;
  runServer;
end.
///////////////////////////////////////////////////////////////////////////////
