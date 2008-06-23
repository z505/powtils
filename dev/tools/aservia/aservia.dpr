
{ Modified March 2008 by Lars Olson. Aservia (web server). Based on nYume }
program aservia; 
{$ifdef fpc}{$mode objfpc}{$H+}{$endif} 
{$R+}

uses
  {$ifdef unix}cthreads, baseunix, unix,{$endif} 
  {$ifdef windows}windows,{$endif}
  zserver, cfgfile, pwfileutil, pwstrutil, pwtypes, strwrap1, shell;

{$include lang.inc}

// Turn these on for more verbose reporting
// TODO: put in a config file instead
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
  gIp: astr;             //    ... ip
  gIdxPg,                // default index page  
  gError404, gError403,  // error strings
  gFileDir: astr;        // path for files to serve
  gLogFname: astr;       // log file name
  gLog: text;            // log file record
  gHandle: int32;
  gMimeCfg, 
  gVhostCfg: TCfgFile;  
  gNewLog: boo;
  gCritical: TRTLCriticalSection;
  gBlacklist: array of astr;
  gNeedStopServer: boo;

{$ifdef dbug} { debugging to console }
 procedure Dbugln(s: astr);   begin if DEBUG_ON then writeln('DEBUG: ', s); end;
 procedure Dbugln(s1,s2:astr);begin dbugln(s1+s2); end;
{$endif}

procedure Logln(var t:text; s:astr); begin if LOG_ON then writeln(t, s); end;
procedure Logln(var t: text); begin logln(t, ''); end;

{ write a line to console if messages on }
procedure Msgln(s1: astr);          begin if MESSAGES_ON then writeln(s1); end;
procedure Msgln(s1,s2: astr);       begin msgln(s1+s2); end;
procedure Msgln(s1,s2,s3: astr);    begin msgln(s1+s2+s3); end;
procedure Msgln(s1:astr; i:int32);  begin msgln(s1+i2s(i)); end;

{ write a note even if MESSAGES_ON is off }
procedure Noteln(s1: astr);            begin  writeln(s1); end;
procedure Noteln(s1,s2: astr);         begin  writeln(s1,s2); end;
procedure Noteln(s1,s2,s3: astr);      begin  writeln(s1,s2,s3); end;
procedure Noteln(s1,s2,s3,s4: astr);   begin  writeln(s1,s2,s3,s4); end;

procedure DeleteTrailingHttpVersion(var s: astr);
var found: int32;

  procedure Removestr;
  begin if found > 0 then delete(s, found, (length(s)-found) +1);  
  end;

begin
  found:= pos(' HTTP/1.', s);
  Removestr;
  found:= pos(' HTTP/2.', s);   // future proof 
  Removestr;
  found:= pos(' HTTP/3.', s);   // future proof 
  Removestr;
end;

{ user commands to stop server with keyboard at console }
function NeedStop(p: pointer): int32;
var s: astr = '';
begin
  repeat
    Readln(s);
    if (s = 'q') or (s = 'quit') then gNeedStopServer:= true;
    Sleep(800);
  until gNeedStopServer;
  Noteln(str_close);
  gServer.Stop;
  result:= 0;
end;
  
function ParceRequest(req: astr): astr;
  { wrapper for pos() }
  function Found(sub, instr: astr): boo;
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
  SetLength(strings, 0);
  i:= pos(#13#10, req);
  while i <> 0 do begin
    SetLength(strings, length(strings) + 1);
    strings[length(strings) - 1] := Copy(req, 1, i - 1);
    Delete(req, 1, i+1);
    i:= Pos(#13#10, req);
  end;

  if length(strings) > 0 then
  for i:= 0 to length(strings) - 1 do
  begin
    if Found('GET ', strings[i]) then begin
      result:= strings[i];
      // trim first 'GET ' part
      Delete(result, 1, 4);
      Addenv(REQ_METHOD, 'GET');
    end
    else if Found('POST ', strings[i]) then
    begin
      result:= strings[i];
      // trim first 'POST ' part
      Delete(result, 1, 5);
      gFileDir := '';
      postd := Copy(req, pos(#13#10#13#10, req), Length(req));
      postd := StringReplace(postd, '&', ';', [rfReplaceAll]);
      {$ifdef mswindows}postd := StringReplace(postd, '%', '%%', [rfReplaceAll]);{$endif}
      Addenv(QUERY_STRING, postd);
      Addenv(REQ_METHOD, 'POST');
    end else if Found('User-Agent:', strings[i]) then begin
      Delete(strings[i], 1, 12);
      AddEnv('HTTP_USER_AGENT', strings[i]);
    end else if found('Host:', strings[i]) then begin
      Delete(strings[i], 1, 6);
      if pos(':', strings[i]) <> 0 then
        strings[i] := copy(strings[i], 1, pos(':', strings[i]) - 1);
      Addenv('HTTP_HOST', strings[i]);
      gFileDir := strings[i];
    end else if found('Referer:', strings[i]) then begin
      delete(strings[i], 1, 9);
      Addenv('HTTP_REFERER', strings[i]);
    end;
  end;

  DeleteTrailingHttpVersion(result);
  if (result = '') or (result[length(result)] = '/') then result += gIdxPg;
end;
  
function GetType(fname: astr): astr;
var ext, def: astr;
begin
  def:= gMimeCfg.getOpt('default', 'application/force-download');;
  ext:= lowerCase(ExtractFileExt(fname));
  delete(ext, 1, 1);
  result:= gMimeCfg.getOpt(ext, def);
end;

(* old nYume based code
function getFile(fname:astr; errcode:astr = '200 OK'; query:astr = ''): astr;
var F: file;
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
      if count < 4096 then buf:= copy(buf, 1, count);
      result += buf;
    end;  
    close(F);
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
*)

function GetFile(fname:astr; errcode:astr = '200 OK'; query:astr = ''): astr;
var count: int32;
    fstring, filetype: astr;
    fstringsz: int32;
    parameters: astr = '';
begin
  result := '';
  filetype := getType(fname);
  if filetype <> 'execute/cgi' then
  begin
    fstring:= File2str(fname);
    fstringsz:= Length(fstring);
    result:= HTTP_VERSION+' ' + errcode + #13#10 + 
             str_server + #13#10 +
            'MIME-version: 1.0'#13#10 +
            'Allow: GET, POST'#13#10 +
            'Content-type: ' + filetype + #13#10 +
            'Content-length: ' + i2s(fstringsz) + #13#10 + 
             #13#10;
    result:= result + fstring;
  end else 
  begin
    if query <> '' then begin
      count := pos('?', query);
      if count > 0 then parameters := copy(query, count + 1, length(query));
    end;
   {$ifdef mswindows}
    parameters := StringReplace(parameters, '%', '%%', [rfReplaceAll]);
   {$endif}
    parameters := StringReplace(parameters, '&', ';', [rfReplaceAll]);
    result := HTTP_VERSION+' '+errcode+#13#10 
              + Command(fname + ' "' + parameters + '"');
  end;
end;  

{ when server is accessed, a request is made }
function request(p: pointer): int32;
var path: astr;

  function slashDots: boo;
  begin
    result:= false;
    if pos('/../', ExtractRelativepath(gFileDir, path)) <> 0 then result:= true; 
  end;

var
  deny: boo = false;
  str1: astr = '';

  procedure Log;
  begin
    if not LOG_ON then exit;
   {$I-}
    Assign(gLog, gLogFname); Append(gLog); 
    if IOResult <> 0 then Rewrite(gLog);
    Logln(gLog, str_requestfrom+gIp+'; '{, DateTimeToStr(Date), ', ', TimeToStr(Time)});
    Logln(gLog, str1);
    if deny then Logln(gLog, str_denied);
    Close(gLog);
   {$I+}
  end;

var fname, name, ip: astr;
    i, data: int32;
begin
  // incoming pointer converted to strong type
  data:= int32(p^);
  enterCriticalSection(gCritical);
  try
    ClearEnv;
    gServer.Select(data);
    str1:= gServer.sRead;
    Msgln(str_request{, DateTimeToStr(Date), ', ', TimeToStr(Time)});
    Msgln(str1);
    ip:= gServer.GetIp(data);
    AddEnv(REMOTE_ADDR, ip);
    AddEnv(SERV_SOFT, str_server);
    i:= 0;
    while (not deny) and (i < Length(gBlacklist)) do begin
      if ip = gBlacklist[i] then begin 
        deny:= true; 
        Msgln(str_denied); 
      end;
      inc(i);
    end;
    {$ifdef dbug}Dbugln('Checked blacklist');{$endif}
    if deny then gServer.sWrite(GetFile(gError403, ERR_403_MSG)) 
    else
    if str1 <> '' then
    begin
      fname := ParceRequest(str1);
      if (gFileDir = '') or (gFileDir = ip) then 
        gFileDir:= gVhostCfg.GetOpt('default', '')
      else begin
        gFileDir:= gVhostCfg.GetOpt(gFileDir, gVhostCfg.getOpt('default', ''));
        gFileDir:= ExcludeTrailingPathDelimiter(gFileDir);
      end;
      AddEnv(DOC_ROOT, gFileDir);
      name:= fname;
      if pos('?',fname) > 0 then name:= copy(fname,1,pos('?',fname) - 1);
      path:= gFileDir + name; 
      Xpath(path);
      if (not FileThere(path)) and DirExists(path) then name:= name+'/'+gIdxPg;
      AddEnv(SCRIPT_NAME, name);
      path:= gFileDir + name;
      xpath(path); // cross platform slashes
      if FileThere(path) then begin
        if SlashDots then begin
          gServer.sWrite(GetFile(gError403, ERR_403_MSG));
          deny:= true;
        end else 
          gServer.sWrite(GetFile(path, '200 OK', fname))

      end else begin
        gServer.sWrite(GetFile(gError404, ERR_404_MSG));
      end;
    end;
    {$ifdef dbug}dbugln('Before log() procedure');{$endif}
    log;
  finally
    {$ifdef dbug}dbugln('Disconnecting: '+i2s(data));{$endif}
    gServer.Disconnect(data);
    LeaveCriticalSection(gCritical); 
    {$ifdef dbug}dbugln('Left Critical Section');{$endif}
  end;
  result:= 0;
  {$ifdef dbug}dbugln('Ending thread');{$endif}
  // IF ENDTHREAD IS NOT PLACED HERE, THEN LINUX KEEPS OPENING THREADS TO A 
  // MAXIMUM OF 380 AND CAUSES A CONTINUAL LEAKAGE
  EndThread;
end;

procedure setupLog;
begin
  if not LOG_ON then exit;
 {$I-}
  Assign(gLog, gLogFname);
  if not gNewLog then Append(gLog);
  if gNewLog or (IOResult <> 0) then Rewrite(gLog);
  Logln(gLog, str_server+#13#10);
  Logln(gLog, str_runserver{, DateTimeToStr(Date), ', ', TimeToStr(Time)});
  Logln(gLog, str_socket+gIp+':'+i2s(gPort));
  Logln(gLog);  
  Close(gLog);
 {$I+}
end;

{ start thread to detect keyboard q/quit at console }
procedure BeginNeedStopThread;
var id: TThreadId; 
begin 
  id:= BeginThread(@needStop); { UNIX and WIN return different results (fpc bug) }       // id:= BeginThread(nil,DefaultStackSize,@needStop,nil,0,dummy);
  {$ifdef dbug}Dbugln('(needStopThread) beginThread Result#: '+i2s(id));{$endif}               
end;

{ start connection threads }
procedure BeginRequestThread;
var id: TTHreadId; 
begin 
  id:= beginThread(@request, @gHandle);{ UNIX and WIN return different results (fpc bug)} // id:= BeginThread(,DefaultStackSize,@request,,0,dummy);
  {$ifdef dbug}dbugln('(requestThread) beginThread Result#: '+i2s(id));{$endif}
end;

{ main server connections loop for all requests }
procedure RunThreadLoop;
begin
  BeginNeedStopThread;
  InitCriticalSection(gCritical);
  repeat
    Msgln(str_wait);
    gHandle:= gServer.connect;
    if (not gNeedStopServer) and (gHandle >= 0) then begin
      Msgln(str_connection);
      BeginRequestThread;
    end else begin
      {$ifdef dbug}Dbugln('runThreadLoop Handle: '+i2s(gHandle));{$endif}
    end;
    Sleep(THREAD_LOOP_SLEEP); 
  until gNeedStopServer;
  DoneCriticalSection(gCritical);
end;

procedure ErrLn(const msg: astr); 
begin 
  Writeln('E: ',msg);
end;

procedure ErrLn; 
begin 
  ErrLn('');
end;

procedure CreateCfgAndServer;
  { retreive configuration file info, or use defaults if not avail }
  procedure GetMainCfg;
  var othercfg: TCfgFile;
  begin
    othercfg:= TCfgFile.Create('config.cfg');
      gIp      := othercfg.GetOpt('ip','127.0.0.1');
      gPort    := othercfg.GetOpt('port',80);
      gNewLog  := othercfg.GetOpt('deletelog',false);
      gLogFname:= othercfg.GetOpt('logfile','connections.log');
      gIdxPg   := othercfg.GetOpt('index','index.html');
      gError404:= othercfg.GetOpt('error404','error404.html');
      gError403 := othercfg.GetOpt('error403','error403.html');
    othercfg.free; othercfg:= nil;

    othercfg:= TCfgFile.Create('blacklist.cfg');
      gBlacklist:= othercfg.GetAllOpts;
    othercfg.free; othercfg:= nil;
  end;

begin
  GetMainCfg;
  gMimeCfg:= TCfgFile.Create('mime.cfg');
  gVhostCfg:= TCfgFile.Create('vhost.cfg');
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
  Errln;
  Errln('Can''t connect to address or port.');
  Errln('The ip:port you are using is '+gIp+':'+inttostr(gPort));
  Errln('Tip: make sure another server is not running.');
  Errln('Error # '+ {$ifdef windows}inttostr(GetLastError){$endif}
                    {$ifdef unix}inttostr(fpGetErrNo){$endif}
  ); 
  { Ugly inline ifdef above due to FPC bug, can't wrap in another func with 
    fpc 2.2.0. See http://bugs.freepascal.org/view.php?id=10205 }

  // cleanup, then kill server
  FreeCfgAndServer; HALT;
end;

{ initiates logs, runs thread loop }
procedure RunServer;
var inited: boo = false;
  // initialize connection to socket 
  procedure TryInit;
  var count: byte = 0; const MAX = 5; // amount of times 
  begin
    while not inited do 
    begin
      inited:= gServer.InitConnection(gIp, gPort);
      inc(count); if count > MAX then ErrCantConnect;
      // port may be busy for a few seconds if apache or other server closing
      if not inited then begin
        Noteln('Connection not initialized. Trying again in 5 seconds!');
        Noteln('Wait...(attempt '+i2s(count)+' of '+i2s(MAX)+')');
        Sleep(5000);
      end;
    end;
  end;

begin
  Noteln(str_server);
  Noteln(str_qcom, #13#10);
  Noteln(str_runserver{, DateTimeToStr(Date), ', ', TimeToStr(Time)});
  CreateCfgAndServer;
  TryInit;
  if inited then Noteln(str_connect_success);
  Noteln(str_socket, gIp, ':', i2s(gPort));
  SetupLog;
  RunThreadLoop;
  FreeCfgAndServer;
end;


///////////////////////////////////////////////////////////////////////////////
begin
  system.isMultiThread:= true;
  gNeedStopServer:= false;
  RunServer;
end.
///////////////////////////////////////////////////////////////////////////////
