{ Modified March 2008 by Lars Olson. Aservia (web server).
  Based on nYume Server }
program aservia; {$ifdef fpc}{$mode delphi}{$H+}{$endif}
uses
  {$ifdef unix}cthreads, baseunix, unix,{$endif} 
  {$ifdef windows}windows,{$endif}
  zserver, cfgfile, pwfileutil, pwstrutil, pwtypes, shell;

{$include lang.inc}

// NOTE: MUST BE 1.0 SINCE IT DOESN'T SUPPORT ALL 1.1 FEATURES YET
const HTTP_VERSION = 'HTTP/1.0';
      ERR_404_MSG  = '404 File not found';
      ERR_403_MSG  = '403 Access denied';
      DOC_ROOT     = 'DOCUMENT_ROOT';
      SCRIPT_NAME  = 'SCRIPT_NAME';
      REQ_METHOD   = 'REQUEST_METHOD';
      QUERY_STRING = 'QUERY_STRING';
      SERV_SOFT    = 'SERVER_SOFTWARE';
      REMOTE_ADDR  = 'REMOTE_ADDR';
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
  handle   : cardinal;
  newlog   : boolean;
  idxpg: astr; // default index page
  error404 : astr;
  error403 : astr;
  blacklist: array of astr;
  needStopServer: boolean = false;

{ debugging to console }
procedure dbugln(s: astr); overload;
begin
  writeln('DEBUG: ', s);
end;

procedure dbugln(s1, s2: astr); overload;
begin
  dbugln(s1+s2);
end;

procedure DeleteTrailingHttpVersion(var s: string);
var found: integer;

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
function needStop(p: pointer): longint;
var s: astr;
begin
  s:= '';
  repeat
    readln(s);
    if (s = 'q') or (s = 'quit') then needStopServer := true;
  until needStopServer;
  writeln(str_close);
  server.Stop;
  Result := 0;
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
  i: integer;
begin
  Result := '';
  SetLength(strings, 0);
  i:= pos(#13#10, req);
  while i <> 0 do begin
    SetLength(strings, Length(strings) + 1);
    strings[Length(strings) - 1] := copy(req, 1, i - 1);
    delete(req, 1, i+1);
    i:= pos(#13#10, req);
  end;

  if Length(strings) > 0 then
  for i:= 0 to Length(strings) - 1 do
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
  if (result = '') or (result[Length(Result)] = '/') then result += idxpg;
end;
  
function gettype(fname: astr): astr;
var ext  : astr;
    def  : astr;
begin
  def:= mimecfg.getOption('default', 'application/force-download');;
  ext:= LowerCase(ExtractFileExt(fname));
  delete(ext, 1, 1);
  Result := mimecfg.getOption(ext, def);
end;
  
function getfile(fname: astr; errcode: astr = '200 OK'; 
                 query: astr = ''): astr;
var
  fl   : file;
  buf  : astr;
  count: integer;
  filetype  : astr;
  parameters: astr = '';
begin
  filetype := gettype(fname);
  if filetype <> 'execute/cgi' then
  begin
    SetLength(buf, 4096);
    Result := '';
    Assign(fl, fname);
    Reset(fl, 1);
    Result := HTTP_VERSION+' ' + errcode + #13#10 + 
              str_server + #13#10 +
             'MIME-version: 1.0'#13#10 +
             'Allow: GET, POST'#13#10 +
             'Content-type: ' + filetype + #13#10 +
             'Content-length: ' + IntToStr(FileSize(fl)) + #13#10 + 
              #13#10;
    while not eof(fl) do begin
      BlockRead(fl, pointer(buf)^, 4096, count);
      if count < 4096 then buf := copy(buf, 1, count);
      Result += buf;
    end;  
    Close(fl);
  end
  else
  begin
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

function request(p: pointer): longint;
var path: astr;

  function SlashDots: bln;
  begin
    result:= false;
    if pos('/../', ExtractRelativepath(filedir, path)) <> 0 then result:= true; 
  end;

var
  deny: boolean;

  procedure Log;
  begin
   {$I-}
    Assign(fl, logflname); Append(fl);
    if IOResult <> 0 then Rewrite(fl);
    writeln(fl, str_requestfrom, ip, '; '{, DateTimeToStr(Date), ', ', TimeToStr(Time)});
    writeln(fl, str1);
    if deny then writeln(fl, str_denied);
    close(fl);
   {$I+}
  end;

var fname, name, ip: astr;
    i: integer;
begin
  deny:= false;
  EnterCriticalSection(critical);
  try
    clearEnv;
    server.select(longint(p^));
    str1 := server.sread;
    writeln(str_request{, DateTimeToStr(Date), ', ', TimeToStr(Time)});
    writeln(str1);
    ip:= server.getip(longint(p^));
    addEnv(REMOTE_ADDR, ip);
    addEnv(SERV_SOFT, str_server);
    i:= 0;
    while (not deny) and (i < Length(blacklist)) do begin
      if ip = blacklist[i] then begin deny := true; writeln(str_denied); end;
      inc(i);
    end;

    if deny then server.swrite(getfile(error403, ERR_403_MSG)) else
    if str1 <> '' then
    begin
      fname := parceRequest(str1);
      if (filedir = '') or (filedir = ip) then 
        filedir:= vhostcfg.getOption('default', '')
      else begin
        filedir:= vhostcfg.getOption(filedir, vhostcfg.getOption('default', ''));
        filedir:= ExcludeTrailingPathDelimiter(filedir);
      end;
      addEnv(DOC_ROOT, filedir);
      name:= fname;
      //      dbugln('name: ', name);
      if pos('?',fname) > 0 then name:= copy(fname,1,pos('?',fname) - 1);

      path:= filedir + name; 
      xpath(path);
      if (not FileThere(path)) and DirExists(path) then name:= name+'/'+idxpg;
      addEnv(SCRIPT_NAME, name);

      path:= filedir + name;
      xpath(path);
      //      dbugln('path: ', path);
      if FileThere(path) then begin
        if SlashDots then begin
          server.swrite(getfile(error403, ERR_403_MSG));
          deny:= true;
        end else 
          server.swrite(getfile(path, '200 OK', fname))
      end else begin
        server.swrite(getfile(error404, ERR_404_MSG));
      end;
    end;

    Log;

  finally
    server.Disconnect(longint(p^));
    LeaveCriticalSection(critical);
  end;

  result:= 0;
end;

procedure SetupLog;
begin
 {$I-}
  Assign(fl, logflname);
  if not newlog then Append(fl);
  if newlog or (IOResult <> 0) then Rewrite(fl);
  writeln(fl, str_server, #13#10);
  writeln(fl, str_runserver{, DateTimeToStr(Date), ', ', TimeToStr(Time)});
  writeln(fl, str_socket, ip, ':', port);
  writeln(fl);  
  close(fl);
 {$I+}
end;

procedure RunThreadLoop;
begin
  BeginThread(@needStop, nil);
  InitCriticalSection(critical);
  repeat
    writeln(str_wait);
    handle := server.Connect;
    if (not needStopServer) and (handle >= 0) then begin
      writeln(str_connection);
      BeginThread(@request, @handle);
      sleep(20);
    end;
   //  sleep(100); // this works as a default
   sleep(20);
  until needStopServer;
  DoneCriticalSection(critical);
end;

procedure Err(const msg: astr);
begin
  writeln(msg);
end;

procedure CreateCfgAndServer;

  procedure SetupMainCfg;
  var othercfg: TCfgFile;
  begin
    othercfg:= TCfgFile.create('config.cfg');
      ip        := othercfg.getOption('ip',        '127.0.0.1');
      port      := othercfg.getOption('port',      80);
      newlog    := othercfg.getOption('deletelog', false);
      logflname := othercfg.getOption('logfile',   'connections.log');
      idxpg     := othercfg.getOption('index',     'index.html');
      error404  := othercfg.getOption('error404',  'error404.html');
      error403  := othercfg.getOption('error403',  'error403.html');
    othercfg.free; othercfg:= nil;

    othercfg:= TCfgFile.create('blacklist.cfg');
      blacklist:= othercfg.getAllOptions;
    othercfg.free; othercfg:= nil;
  end;

begin
  SetupMainCfg;
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
  Err(LF+'Can''t connect to address or port.'+ LF+
      'The ip:port you are using is '+ip+':'+inttostr(port)+ LF+
      'Tip: make sure another server is not running.'+ LF+
      'Error # '+ {$ifdef windows}inttostr(GetLastError){$endif}
                   {$ifdef unix}inttostr(fpGetErrNo){$endif}
  ); 
  // Ugly inline ifdef above due to FPC bug, cannot wrap in another function 
  // with fpc 2.2.0. See http://bugs.freepascal.org/view.php?id=10205

  // cleanup, then kill
  FreeCfgAndServer; Halt;
end;

{ initiates logs, runs thread loop }
procedure RunServer;
var inited: boolean;
begin
  writeln(str_server);
  writeln(str_qcom, #13#10);
  writeln(str_runserver{, DateTimeToStr(Date), ', ', TimeToStr(Time)});
  CreateCfgAndServer;
  inited:= server.InitConnection(ip, port);
  if inited then begin
    writeln(str_socket, ip, ':', port);
    SetupLog;
    RunThreadLoop;
  end else 
    ErrCantConnect;
  FreeCfgAndServer;
end;


///////////////////////////////////////////////////////////////////////////////
begin
  RunServer;
end.
///////////////////////////////////////////////////////////////////////////////
