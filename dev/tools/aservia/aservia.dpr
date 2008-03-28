

Program aservia;
{$mode Delphi}
{$SMARTLINK ON}
{$LONGSTRINGS ON}

uses
  {$ifdef unix}cthreads,{$endif} {$ifdef windows}windows,{$endif}
  zserver, cfgfile, pwfileutil, pwstrutil, shell;

{$Include lang.inc}

var
  server   : TzServer;
  str1     : string;
  critical : TRTLCriticalSection;
  fl       : text;
  cfg      : TCfgFile; 
  vhost    : TCfgFile;  
  logflname: string;
  filedir  : string;
  ip       : string;
  port     : word;
  handle   : cardinal;
  newlog   : boolean;
  defaultfl: string;
  error404 : string;
  error403 : string;
  blacklist: array of string;
  needStopServer: boolean = false;

function needStop(p: pointer): longint;
var s: string;
begin
  repeat
    readln(s);
    if (s = 'q') or (s = 'quit') then needStopServer := true;
  until needStopServer;
  writeln(str_close);
  server.Stop;
  Result := 0;
end;
  
function parceRequest(req: string): string;
var
  strings: array of string;
  postd  : string;
  i      : integer;
begin
  Result := '';
  SetLength(strings, 0);

  i := pos(#13#10, req);
  while i <> 0 do begin
    SetLength(strings, Length(strings) + 1);
    strings[Length(strings) - 1] := copy(req, 1, i - 1);
    delete(req, 1, i + 1);
    i := pos(#13#10, req);
  end;

  if Length(strings) > 0 then
  for i := 0 to Length(strings) - 1 do
  begin
    if pos('GET ', strings[i]) <> 0 then begin
      Result := strings[i];
      delete(Result, 1, 4);
      addenv('REQUEST_METHOD', 'GET');
    end
    else if pos('POST ', strings[i]) <> 0 then
    begin
      Result := strings[i];
      delete(Result, 1, 5);
      filedir := '';
      postd := copy(req, pos(#13#10#13#10, req), length(req));
      postd := StringReplace(postd, '&', ';', [rfReplaceAll]);
      {$ifdef mswindows}postd := StringReplace(postd, '%', '%%', [rfReplaceAll]);{$endif}
      addenv('QUERY_STRING', postd);
      addenv('REQUEST_METHOD', 'POST');
    end
    else if pos('User-Agent:', strings[i]) <> 0 then begin
      delete(strings[i], 1, 12);
      addEnv('HTTP_USER_AGENT', strings[i]);
    end else if pos('Host:', strings[i]) <> 0 then begin
      delete(strings[i], 1, 6);
      if pos(':', strings[i]) <> 0 then
        strings[i] := copy(strings[i], 1, pos(':', strings[i]) - 1);
      addEnv('HTTP_HOST', strings[i]);
      filedir := strings[i];
    end else if pos('Referer:', strings[i]) <> 0 then begin
      delete(strings[i], 1, 9);
      addEnv('HTTP_REFERER', strings[i]);
    end;
  end;

  Result := StringReplace(Result, ' HTTP/1.1', '', [rfIgnoreCase]);
  if (Result = '') or (Result[Length(Result)] = '/') then Result += defaultfl;
end;
  
function gettype(filename: string): string;
var ext  : string;
    def  : string;
begin
  def := cfg.getOption('default', 'application/force-download');;
  ext := LowerCase(ExtractFileExt(filename));
  delete(ext, 1, 1);
  Result := cfg.getOption(ext, def);
end;
  
function getfile(filename: string; errcode: string = '200 OK'; query: string = ''): string;
var
  fl   : file;
  buf  : string;
  count: integer;
  filetype  : string;
  parameters: string = '';
begin
  filetype := gettype(filename);
  if filetype <> 'execute/cgi' then
  begin
    SetLength(buf, 4096);
    Result := '';
    Assign(fl, filename);
    Reset(fl, 1);
    Result := 'HTTP/1.1 ' + errcode + #13#10 + 
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
    Result := 'HTTP/1.1 ' + errcode + #13#10 
              + command(filename + ' "' + parameters + '"');
  end;
end;  

function request(p: pointer): longint;
var filename: string;
    name    : string;
    ip      : string;
    i       : integer;
    deny    : boolean = false;
begin
  EnterCriticalSection(critical);
  try
    clearEnv;
    server.select(longint(p^));
    str1 := server.sread;
    writeln(str_request{, DateTimeToStr(Date), ', ', TimeToStr(Time)});
    writeln(str1);
    ip := server.getip(longint(p^));
    addEnv('REMOTE_ADDR', ip);
    addEnv('SERVER_SOFTWARE', str_server);
    i := 0;
    while (not deny) and (i < Length(blacklist)) do begin
      if ip = blacklist[i] then begin deny := true; writeln(str_denied); end;
      inc(i);
    end;

    if deny then server.swrite(getfile(error403, '403 Access denied')) else
    if str1 <> '' then
    begin
      filename := parceRequest(str1);
      if (filedir = '') or (filedir = ip) then
        filedir := vhost.getOption('default', '')
      else
        filedir := vhost.getOption(filedir, vhost.getOption('default', ''));

      addEnv('DOCUMENT_ROOT', filedir);
      name := filename;
      if pos('?', filename) > 0 then
        name := copy(filename, 1, pos('?', filename) - 1);

      if (not FileThere(filedir + name, fmDefault)) 
         and DirectoryExists(filedir + name) 
      then name += '/' + defaultfl;

      addEnv('SCRIPT_NAME', name);

      if FileThere(filedir + name, fmDefault) 
         and (pos('/../', ExtractRelativepath(filedir, filedir + name)) <> 0) 
      then begin
        server.swrite(getfile(error403, '403 Access denied'));
        deny := true;
      end else begin 
        if FileThere(filedir + name, fmDefault) then
          server.swrite(getfile(filedir + name, '200 OK', filename))
        else
          server.swrite(getfile(error404, '404 File not found'));
      end;
    end;
   {$I-}
    Assign(fl, logflname); Append(fl);
    if IOResult <> 0 then Rewrite(fl);
    //writeln(fl, str_requestfrom, ip, '; ', DateTimeToStr(Date), ', ', TimeToStr(Time));
    writeln(fl, str1);
    if deny then writeln(fl, str_denied);
    close(fl);
   {$I+}
  finally
    server.Disconnect(longint(p^));
    LeaveCriticalSection(critical);
  end;

  Result := 0;
end;

procedure SetupCfg;
begin
  cfg := TCfgFile.create('config.cfg');
    ip        := cfg.getOption('ip',        '127.0.0.1');
    port      := cfg.getOption('port',      80);
    newlog    := cfg.getOption('deletelog', false);
    logflname := cfg.getOption('logfile',   'connections.log');
    defaultfl := cfg.getOption('index',     'index.html');
    error404  := cfg.getOption('error404',  'error404.html');
    error403  := cfg.getOption('error403',  'error403.html');
  cfg.free;

  cfg := TCfgFile.create('blacklist.cfg');
    blacklist := cfg.getAllOptions;
  cfg.free;
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

procedure RunServer;
begin
  writeln(str_server);
  writeln(str_qcom, #13#10);
  writeln(str_runserver{, DateTimeToStr(Date), ', ', TimeToStr(Time)});

  SetupCfg;
  cfg := TCfgFile.create('mime.cfg');
  vhost := TCfgFile.create('vhost.cfg');
  server := TzServer.Create(ip, port);
  writeln(str_socket, ip, ':', port);
  SetupLog;
  RunThreadLoop;
  server.Free;
  cfg.Free;
end;

///////////////////////////////////////////////////////////////////////////////
begin
  RunServer;
end.
///////////////////////////////////////////////////////////////////////////////