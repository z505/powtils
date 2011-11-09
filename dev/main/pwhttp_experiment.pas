{*******************************************************************************

                           Powtils HTTP Connections
                           
********************************************************************************

 HTTP get/post for connecting to external websites

 Authors/Credits: Trustmaster (Vladimir Sibirov), L505 (Lars)
 Copyright (c) 2003-2006 by PSP dev team. See Artistic License for legal info.
   
 Notes:

  Dec-12-2006 [L505]
   -fixed httpget, bug was in the copy routine, + 1 added

********************************************************************************}

unit pwhttp_experiment;
{$IFDEF FPC}{$MODE OBJFPC}{$H+}
  {$IFDEF EXTRA_SECURE}{$R+}{$Q+}{$CHECKPOINTER ON}{$ENDIF}
{$ENDIF}

{$IFDEF WIN32} {$DEFINE WINDOWS} {$ENDIF}
{$IFDEF WIN64} {$DEFINE WINDOWS} {$ENDIF}

interface

{============================= PUBLIC TYPES ===================================}

// data hiding: see httpconnection further below for implementation 
type HTTPConnection = pointer;

// http 1.1 must check for CHUNK and we don't do this so we use http 1.0
const HTTP_VERSION = 'HTTP/1.0' 

{===================== PUBLIC FUNCTIONS ========================}
procedure debugproc(s: string);
var debugln: procedure(s: string) = @debugproc; // user can change for custom debugging

procedure HttpClose(cp: HTTPConnection; var TIn, TOut: text);
function HttpConnect1(const address, agent: string; var TIn,TOut: text): HTTPConnection;
function HttpConnect(const address: string; var TIn,TOut: text): HTTPConnection;
function HttpCopy(const source, dest: string): boolean;
function HttpEof(var TIn: text): boolean;
function HttpGet1(const url, agent: string): string;
function HttpGet(const url: string): string;
function HttpGetHeader(cp: HTTPConnection; const name: string): string;
function HttpRead(var TIn: text): char;

function HttpReadLn(var TIn: text): string;
function HttpSendRequest(cp: HTTPConnection; const method, uri: string; var TIn, TOut: text): boolean;

function HttpResponseInfo(cp: HTTPConnection; var final_url, message: string): word;
procedure HttpSetHeader(cp: HTTPConnection; const name, value: string);
procedure HttpSetPostData(cp: HTTPConnection; const data: string);
procedure HttpPutHeader(cp: HTTPConnection; const header: string);


implementation

uses 
  hostname, 
 {$ifdef WINDOWS}
  sockets, // note: fpc 2.0.4 has sockets win32 I/O bug in fpc rtl
//  sockets,
 {$else}
  sockets,
 {$endif}   
  pwsubstr;

const DEFAULT_ACCEPT = 'text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5';
      DEFAULT_CHARSET = 'windows-1252, iso-8859-1;q=0.6, *;q=0.1';
      DEFAULT_USER_AGENT = 'PWU HTTP Module';

{============================= PRIVATE FUNCTIONS ==============================}

function int2str(i: integer): string;
begin
  system.str(i, result);
end;

function word2str(w: word): string;
begin
  system.str(w, result);
end;

// delete http:// at beginning of string -L505
procedure TrimHttpStr(var s: string);
begin        
  if length(s) < 1 then exit; 
  s:= substrireplace(s, 'http://', '');
end;

// check https:// can't do SSL or FTP yet -L505
function BadHttpFound(var s: string): boolean;
begin        
  result:= false;
  if length(s) < 1 then exit; 
  if pos('https://', s) = 1 then result:= true;
  if pos('ftp://', s) = 1 then result:= true;
end;

// add trailing url slash if needed -L505
procedure AddTrailSlash(var s: string);
begin
  if length(s) < 1 then exit; 
  if pos(s, '/') = 0 then s:= s + '/';
end;

// prepare http address: trim http:// and add trailing slash if needed -L505
procedure PrepHttpAddress(var s: string);
begin
  TrimHttpStr(s);
  AddTrailSlash(s);
end;

{============================= PRIVATE TYPES ==================================}
type
  THTTPHeader = record
    name, value: string;
  end;

  THTTPHeaders = array of THTTPHeader;

  THTTPConnection = record
    sock: longint; // Connected socket
    request, response: THTTPHeaders; // Request/Response headers
    code, uri: string; // Response code + message; final resolved uri (for redirect purpose)
    post: string; // Post data string
  end;

 // Pointer to THTTPConnection
  PHTTPConnection = ^THTTPConnection;

{========================= PUBLIC FUNCTIONS ===================================}

procedure debugproc(s: string); 
begin // default do nothing
end;

// Close HTTP connection
procedure HttpClose(cp: HTTPConnection; var TIn, TOut: text);
var 
  conn: PHTTPConnection;
begin
  conn := PHTTPConnection(cp);
  close(TIn);
  close(TOut);
  CloseSocket(conn^.sock);
  dispose(conn);
end;

// Connects to HTTP server specified by hostname:port or hostname
function HttpConnect1(const address, agent: string; var TIn, TOut: text): HTTPConnection;
var conn: PHTTPConnection;

  procedure AddReqHeader(const name, value: string);
  var ReqLen: integer;
  begin
    ReqLen:= length(conn^.request);
    SetLength(conn^.request, ReqLen + 1);
    conn^.request[ReqLen].name := name;
    conn^.request[ReqLen].value := value;
  end;

var addr: sockaddr_in;
    server: string;
    port: word;
    tmpurl: string;

  // allowed address syntax
  procedure ParseAddress;
  var p: longint;
  begin
    p := pos(':', tmpurl);
    if p > 0 then
    begin
      // Split by :
      server:= copy(tmpurl, 1, p - 1);
      val(copy(tmpurl, p + 1, length(tmpurl) - p), port);
      addr := InetResolve(server, port);
    end else begin 
      addr:= InetResolve(tmpurl, 80);
      server:= tmpurl;
    end;
//    writeln('DEBUG addr: ', addr.addr);
  end;

begin
  result:= nil;
  new(conn);
  tmpurl:= address;
  ParseAddress;
  // Checking address validity
  if addr.addr <= 0 then
  begin
    dispose(conn);
    exit;
  end;

  // open connection
  conn^.sock:= socket(AF_INET, SOCK_STREAM, 0);
  if not connect(conn^.sock, addr, TIn, TOut) then
  begin
    dispose(conn);
    exit;
  end;

  // Descriptors init
  reset(TIn);
  rewrite(TOut);

  // Setting some default request headers
  AddReqHeader('Accept', DEFAULT_ACCEPT);
  AddReqHeader('Accept-Charset', DEFAULT_CHARSET);
  AddReqHeader('Host', server);
  AddReqHeader('User-Agent', agent);
  AddReqHeader('Connection', 'close');
  result:= HttpConnection(conn);
end;

{ connect with default user agent }
function HttpConnect(const address: string; var TIn, TOut: text): HTTPConnection;
begin
  result:= HttpConnect1(address, DEFAULT_USER_AGENT, Tin, TOut);
end;

{ Copy remote file to local one. Source must be full HTTP URL (may even contain
  get params), example: www.server.com/path/script.php?cid=256&name=example
  Get prams must be URLEncoded, Dest is local file name accessible for writing}
function HttpCopy(const source, dest: string): boolean;
var fh: text;
    data: string;
begin
  result := false;
  data := HttpGet(source);
  if data = '' then exit;
  assign(fh, dest);         
  rewrite(fh);
  write(fh, data);
  close(fh);
  result := true;
end;

{ Checks if Response document is at enf of file }
function HttpEof(var TIn: text): boolean;
begin
  result := eof(Tin);
end;

{ Returns a string containing the file represented by URL
  URL must be full HTTP URL (may even contain get params), example:
  www.server.com/path/script.php?cid=256&name=example
  Get prams must be URLEncoded 
  
  ERROR CODES: in string format
  '-4 err' : tried to get HTTPS, not supported, only HTTP
  '-3 err' : 200, 301. 302, or 303 response not received
  '-2 err' : connect error
  '-1 err' : address from inet resolve not valid }
function HttpGet1(const url, agent: string): string;
var response: word;

  function ValidResponse: boolean;
  begin
    result:= false;
    if (response=200) or (response=301) or (response=302) or (response=303) then 
      result:= true;
  end;

var sIn, sOut: text;                                    
    sock: longint;

  // Close connection
  procedure CloseConnect;
  begin
    close(sIn);
    close(sout);
    CloseSocket(sock);      
  end;

var redir: boolean;
    loc, temp: string;

  procedure ProcessHeaders;
  begin
    redir:= false;
    repeat
      readln(sIn, temp);
      if upcase(copy(temp, 1, 8)) = 'LOCATION' then
      begin
        loc := substrireplace(temp, 'Location: ', '');
        redir := true;
      end;
    until temp = '';
  end;

var uri, tmpurl, host: string;
    port: word;
//    addr: TInetSockAddr;
    addr: sockaddr_in;
    p: longint;

  procedure ParseUrl;
  var slashpos: integer;
  begin
    slashpos:= pos('/', tmpurl);
    host := copy(tmpurl, 1, slashpos - 1);                     
    uri := copy(tmpurl, pos('/', tmpurl), length(tmpurl) - slashpos + 1); 
    if uri = '' then uri := '/';
    p := pos(':', host);
    if p > 0 then
    begin
      // Splitting by :
      val(copy(host, p + 1, length(host) - p), port);
      host:= copy(host, 1, p - 1);
      addr:= InetResolve(host, port);
    end else
      addr:= InetResolve(host, 80);
//    writeln('DEBUG addr: ', addr.addr);
  end;

  procedure SendRequest;
  begin
    writeln(sout, 'GET ' + uri + ' ' + HTTP_VERSION); // 1.1 must check for CHUNK and we don't do this
    writeln(sout, 'Accept: ' + DEFAULT_ACCEPT);
    writeln(sout, 'Host: ' + host);
    writeln(sout, 'User-Agent: ' + agent);
    writeln(sout, 'Connection: close');
    writeln(sout);  // must be empty line
    flush(sout);
  end;

var 
  c: char;
  readsize, // data bigger than 4GB may have issues, could use int64 but web files are not usually this big
  tmpbuflen: integer; 

const BUF_GROWBY = 512;  
      BUF_INITSIZE = 16384;
begin
  // Init
  result := '';

  tmpurl:= url;
  if BadHttpFound(tmpurl) then         
  begin 
    result:= '-4 err'; 
    exit; 
  end;      
  // get rid of http:// and localhost or site.com converted to localhost/ or site.com/ 
  PrepHttpAddress(tmpurl);     
  ParseUrl;
  // Check address validity
  if addr.addr <= 0 then begin result:= '-1 err'; exit; end;
  // open connection
  sock := socket(AF_INET, SOCK_STREAM, 0);
  if not connect(sock, addr, sIn, sout) then 
  begin 
    result:= '-2 err'; 
    exit; 
  end;
  // Descriptors init
  reset(sIn);
  rewrite(sout);

  SendRequest;
  // process first line
  readln(sIn, temp);
  val(copy(temp, 10, 3), response);
  if not ValidResponse then 
  begin
    result:= '-3 err: ' + temp;
    CloseConnect;
    exit;    
  end;  

  ProcessHeaders;

  if redir then
  begin
    // Redirected
    CloseConnect;
    result := HttpGet1(loc, agent);
  end else
  begin
    readsize:= 0;
    setlength(result, BUF_INITSIZE);  // set initial buffer to optimize 
    // Getting contents
    while not eof(sin) do
    begin
      read(sIn, c);
      inc(readsize);
      tmpbuflen:= length(result);
      // grow buffer only if needed
      if tmpbuflen < readsize then SetLength(result, tmpbuflen + BUF_GROWBY);
      result[readsize]:= c;
    end;
    setlength(result, readsize); // set string to proper total size read
    CloseConnect;
  end;
end;

{ get url with default user agent }
function HttpGet(const url: string): string;                    
begin
  result:= HttpGet1(url, DEFAULT_USER_AGENT);      
end;
                  
// Return value of server Response header
function HttpGetHeader(cp: HTTPConnection; const name: string): string;
var
  conn: PHTTPConnection;
  i: longword;
begin
  conn := PHTTPConnection(cp);
  result := '';
  if length(conn^.response) > 0 then
  for i := 0 to length(conn^.response) - 1 do 
    if upcase(conn^.response[i].name) = upcase(name) then
    begin
      result := conn^.response[i].value;
      break;
    end;
end;

// Reads single char from Response document
function HttpRead(var TIn: text): char;
begin
  result := #0;
  if not eof(TIn) then read(TIn, result);
end;

// Reads a line from Response document
function HttpReadLn(var TIn: text): string;
begin
  result := '';
  if not eof(TIn) then readln(TIn, result);
end;

// Sends HTTP request. Headers and POST data must be set before this call
function HttpSendRequest(cp: HTTPConnection; const method, uri: string; var TIn, TOut: text): boolean;
var conn: PHTTPConnection;

  procedure SendRequest;
  var i, reqlen: longword;
  begin
    // first line
    writeln(TOut, upcase(method) + ' ' + uri + ' ' +  HTTP_VERSION); 
    reqlen:= length(conn^.request);

    // then headers 
    if reqlen > 0 then  
      for i := low(conn^.request) to high(conn^.request) do begin 
        writeln(TOut, conn^.request[i].name + ': ' + conn^.request[i].value);
        flush(TOut);
      end;
    writeln(TOut); // must be empty line

    if upcase(method) = 'POST' then writeln(TOut, conn^.post);
    flush(TOut);
  end;

  procedure ReadInput;
  var resplen: integer;
      nv: StrArray;
      buff: string;
  begin
    // read message                        
    readln(TIn, conn^.code);
    // read headers
    repeat
      readln(TIn, buff);
      if buff <> '' then begin
        resplen:= length(conn^.response);
        SetLength(conn^.response, resplen + 1);
        nv := SubstrSplit(buff, ':');
        conn^.response[resplen].name := strtrim(nv[0]);
        conn^.response[resplen].value := strtrim(nv[1]);
      end;
    until (buff = '') or eof(TIn);
    if copy(conn^.code, 10, 3) = '200' then result := true;
//    writeln('DEBUG ' + conn^.code);
  end;

var i, rlen: longword;

begin
  result:= false;
  conn := PHTTPConnection(cp);
  SendRequest;
  ReadInput;
  rlen:= length(conn^.response);
  if rlen > 0 then begin
    for i := 0 to rlen - 1 do begin
      if upcase(conn^.response[i].name) = 'LOCATION' then begin
        conn^.uri := conn^.response[i].value;
        // user will have reconnect himself 
        exit;
      end;
    end;
  end;

end;

// Fetches response result info (exact document URL, response message and code as result)
function HttpResponseInfo(cp: HTTPConnection; var final_url, message: string): word;
var conn: PHTTPConnection;
begin
  conn := PHTTPConnection(cp);
  final_url := conn^.uri;
  message := conn^.code;
  val(copy(conn^.code, 1, 3), result);
end;

// Sets client Request header
procedure HttpSetHeader(cp: HTTPConnection; const name, value: string);
var conn: PHTTPConnection;
    reqlen, i: longint;
begin
  conn := PHTTPConnection(cp);
  reqlen:= length(conn^.request);
  // Changing value if already set
  if reqlen > 0 then
  for i := 0 to reqlen - 1 do 
    if upcase(conn^.request[i].name) = upcase(name) then
    begin
      conn^.request[i].value := value;
      exit;
    end;
  // Or set new header
  SetLength(conn^.request,  reqlen + 1);
  conn^.request[reqlen].name := name;
  conn^.request[reqlen].value := value;
end;

// Sets client Requst header from 'Name: Value' string
procedure HttpPutHeader(cp: HTTPConnection; const header: string);
var conn: PHTTPConnection;
    i: longword;
    nv: StrArray;
begin
  conn := PHTTPConnection(cp);
  // Splitting into name=value pair
  nv := substrsplit(header, ':');
  if length(nv) <> 2 then exit;
  nv[0] := strtrim(nv[0]);
  nv[1] := strtrim(nv[1]);
  // Changing value if already set
  if length(conn^.request) > 0 then
  for i := 0 to length(conn^.request) - 1 do if upcase(conn^.request[i].name) = upcase(nv[0]) then
  begin
    conn^.request[i].value := nv[1];
    exit;
  end;
  // Or setting new header
  SetLength(conn^.request, length(conn^.request) + 1);
  conn^.request[length(conn^.request) - 1].name := nv[0];
  conn^.request[length(conn^.request) - 1].value := nv[1];
end;

// Sets client Request POST data (for POST method)
// Variables must be URLEncoded
procedure HttpSetPostData(cp: HTTPConnection; const data: string);
var conn: PHTTPConnection;
    len: string;
    i: longword;
    reqlen: integer;
begin
  conn := PHTTPConnection(cp);
  conn^.post := data;
  str(length(data), len);
  reqlen:= length(conn^.request);
//  writeln('DEBUG reqlen: ', reqlen);
  // Changing value if already set
  if reqlen > 0 then begin
    for i := 0 to reqlen - 1 do 
      if upcase(conn^.request[i].name) = upcase('Content-Length') then begin
        conn^.request[i].value := len;
        exit;
      end;
  end;
  // Or set new header
  SetLength(conn^.request, reqlen + 1);
  conn^.request[reqlen].name := 'Content-Length';
  conn^.request[reqlen].value := len;
end;

end.
