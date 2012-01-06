{ fp web compiler remote program (client)
  Copyright Lars Olson 2008-2011 }

program fpw;
{$mode objfpc} {$H+}

uses pwhttp, pwhostname, pwurlenc, windows, sockets, compactsysutils, strwrap1,
     pwtypes, pwfileutil, pwstrutil;

const FPW_PARAM_COUNT = 3;
      FPC_PARAM_POS = FPW_PARAM_COUNT + 1;
      COMPILE_PROG = 'fpc';
      CONFIG_FILE = 'fpw.cfg';
      SERVER_SLASH = '/'; // todo: if Windows server in future, maybe use '\' 
      
var 
  FpcParams: array of string;
  FpwServPath: string = ''; Domain: string = '';  ServerSrcPath: string = '';
  SrcFileSearchStr: string = ''; // string to find in src path being compiled (ignore everything except after this string)
  SrcFileArrayIdx: int;
  OutExecutable: string = '';
  LauncherUrlPath: string = '';

function SrcFileParam: string; begin result:= FpcParams[SrcFileArrayIdx];end;
function ConfigFile: string;   begin result:= ExtractFilePath(paramstr(0)) + CONFIG_FILE; end;
procedure HaltCleanup;       begin halt; end;

// sends notification messages (currently just wraps to stdout)
procedure msgln(s: string);      begin writeln(s);    end;
procedure msgln(s1, s2: string); begin msgln(s1+s2);  end;
procedure msgln;               begin msgln('');     end;
procedure msg(s: string);        begin write(s);      end;
procedure msg(s1, s2: string);   begin msg(s1+s2);    end;
procedure msg;                 begin msg('');       end;

procedure ErrorHalt(const s: string);
begin 
  msgln('Problem: ');
  msgln(s);
  HaltCleanup;
end;

{ these are the fpc compiler params.. skip first to params since they are
  domain and cgi path of fpwserv }
procedure GetFpcParams(startpos: byte);
var i: int;
begin
  case startpos of
    // if no first == params specified, all fpc params start at paramstr(1)
    1:
    begin SetLength(FpcParams, ParamCount);
      for i:= 1 to ParamCount do FpcParams[i-1]:= ParamStr(i);
    end;
    // else == params specified
    FPC_PARAM_POS: 
    begin SetLength(FpcParams, ParamCount - FPW_PARAM_COUNT);
      for i:= FPC_PARAM_POS to ParamCount do 
        FpcParams[i - FPC_PARAM_POS]:= ParamStr(i);
    end;
  end;

  if length(FpcParams) < 1 then exit;
  // find source file parameter
  for i:= low(FpcParams) to high(FpcParams) do begin
    if FpcParams[i][1] <> '-' then SrcFileArrayIdx:= i;
  end;
end;

{ finds string we want to ignore in the source file sent as a param to be
  compiled. 
  i.e. if c:\web\cgi-bin\programs\ from c:\cgi-bin\programs\hello\hello.dpr 
       then we are left with: hello\hello.dpr }
function TrimSrcFileParam: string;
var found, afterfound, rightcnt: int;
begin
  result =SrcFileParam;
  if SrcFileSearchStr = '' then exit;
  found:= pos(SrcFileSearchStr, result);
  if found > 0 then begin
    afterfound:= length(SrcFileSearchStr) + found; // position after end of search string
    rightcnt:= (length(result) - afterfound) + 1;  // characters needed from right
    result:= rightstr(result, rightcnt);
    //msgln('debug: trimmed: ', result, ' afterfound:', afterfound);
  end;
end;

function MakeUrlPostVars: string;
var i: int;
  nameval: string = '';
begin
  result:= '';
  for i:= low(FpcParams) to high(FpcParams) do begin
    // src file appended to src path
    if i = SrcFileArrayIdx then 
      nameval:= 'param' + inttostr(i) + '=' 
                + urlencode(ServerSrcPath + TrimSrcFileParam) + '&' 
    else // default name=val pair
      nameval:= 'param' + inttostr(i) + '=' + urlencode(FpcParams[i]) + '&'; 
    result:= result + nameval;
  end;
end;

procedure WriteVersion; begin msgln('FPW (FPC Web Compiler Magick) by Z505'); end;

{ shows help and halts program cleanly }
procedure HelpHalt(s: string);
begin
  if s <> '' then begin
    msgln('Command problem: ');
    msgln('  ' + s);
    msgln;
  end;
  msgln('Program usage: ');
  msgln('  fpw ==site.com ==fpwservpath ==srcpath src.pas -fpcparams');
  msgln('Example:');
  msgln('  fpw ==mysite.com ==/cgi-bin/fpw123/fpwserv ==programs/ src.pas -CX -XX -Xs');
  msgln;
  msgln('The == prefix is used on the first two params.');
  msgln;
  msgln('You can skip the == params if you use an fpw.cfg');
  msgln('Optional fpw.cfg goes in fpw.exe folder (usually i386-win32/bin/ fpc folder).');
  HaltCleanup;
end;

procedure HelpHalt; overload;
begin HelpHalt('');
end;

function IncludePathDelim(const s: string): string;
var len: int;
begin
  result:='';
  len:= length(s);
  if len < 1 then exit;
  result:= s;
  if s[len] <> SERVER_SLASH then result:= s + SERVER_SLASH;
end;

function TidyServerSrcPath(s: string): string; 
begin result:= IncludePathDelim(s); 
end;

function CheckCfgFile: boolean;
type TLineRange = 1..5; // first four lines in config file to parse
var i: TLineRange;
    lines: array [TLineRange] of string = ('','','','', '');

  procedure CouldntParse(extra: string); 
  var msgstr: string;
  begin msgstr:= 'Couldn''t parse line '+i2s(i) + ' of fpw.cfg'; 
    HelpHalt(msgstr + extra);
  end;

  procedure CouldntParse; begin CouldntParse(''); end;

  procedure CouldntParseOrLineEmpty(extra: string); 
  begin CouldntParse(', or line empty'); 
  end;

  procedure CouldntParseOrLineEmpty; begin CouldntParseOrLineEmpty(''); end;

begin
  result:= false;

  // map a range if lines in a file to an array
  for i:= low(TLineRange) to high(TLineRange) do begin
    lines[i]:=GetLnN(i, ConfigFile);
    // ignore spaces at end of string on the line
    lines[i]:=TrimRight(lines[i]);
  end;

  i:= low(TLineRange); // 1 
  if (lines[i] = '-1NF') or (lines[i] = '') then 
    CouldntParseOrLineEmpty(', or fpw.cfg not found');
  Domain:= lines[i];

  i:= i+1; // 2
  if (lines[i] = '-1NF') or (lines[i] = '') then CouldntParseOrLineEmpty;
  FpwServPath:=lines[i];

  i:= i+1; // 3
  if (lines[i] = '-1NF') or (lines[i] = '') then CouldntParseOrLineEmpty;
  ServerSrcPath:=TidyServerSrcPath(lines[i]);

  i:= i+1; // 4
  if (lines[i] = '-1NF') then CouldntParse;
  SrcFileSearchStr:=lines[i];

  i:= i+1; // 5
  if (lines[i] = '-1NF') then CouldntParse;
  LauncherUrlPath:=lines[i];

  msgln('Using config file '+ CONFIG_FILE);
end;

procedure CheckMinParams; begin if ParamCount < 1 then HelpHalt; end;

function ParseParams: boolean;
var i: int; tmp: string = ''; found: byte = 0; cfgused: boolean = false;
begin
  CheckMinParams;
  result:=false;
  for i:= 1 to FPW_PARAM_COUNT do begin
    if i > ParamCount then break;  
    if pos('==', ParamStr(i)) = 1 then begin
      tmp:=StringReplace(ParamStr(i), '==', '', []);
      case i of
        1: Domain:= tmp;
        2: FpwServPath:= tmp;
        3: ServerSrcPath:= TidyServerSrcPath(tmp);
      end;      
      inc(found);
    end;
  end; 
  // must be 3 total == params
  if (found < FPW_PARAM_COUNT) and (found > 0) then 
    HelpHalt('must specify '+inttostr(FPW_PARAM_COUNT)+
             ' total == prefixed params or use fpw.cfg file.');
  if found = 0 then begin
    cfgused:=CheckCfgFile;
    if not cfgused then GetFpcParams(1);
  end else
    GetFpcParams(FPC_PARAM_POS)
end;

procedure WriteDetail1;
begin
  msg('Site:', Domain);
  msg(' Fpwserv Path:', FpwServPath);
  msgln(' Compiler:', COMPILE_PROG);
  //msgln('DEBUG: Url Params: ', allparts);
//  msgln('DEBUG: paramstr: ', paramstr(0));  
end;

procedure MakeFpwTmpFile;
var f: text; copyfrom, copyto: string; err: int;
const EXT  = '.exe'; PROG = 'fpwlaunch'+EXT;
begin
  assign(f, 'fpwlaunch.tmp');
  rewrite(f);
  writeln(f, 'http://', Domain, LauncherUrlPath, OutExecutable);
  close(f);
  copyfrom:= ExtractFilePath(paramstr(0))+PROG;
  copyto:= ExtractFilePath(SrcFileParam) + ExtractFileName(OutExecutable) + EXT;
  err:= CloneFile(copyfrom, copyto);
  if err < 0 then 
    msgln('Error copying file: copyfrom: ' + copyfrom + ' copyto: ' + copyto);
end;

procedure TalkToServer(verbose: boolean);
var h: HttpConnection; 
    sent: boolean; 
    hcode:int; 
    s, url, part2, allparts, tmpln: string;

    procedure WriteVerbose;
    begin //      if url <> '' then msgln('Redirect URL: ', url);
      msgln('Response msg: ', s);
    end;

    procedure WriteTips;
    begin case hcode of
        500: msgln('Tip: 500 error means check executable cgi permissions on server, or fpwserv is not working on your server');
        404: msgln('Tip: 404 means fpwserv program not found. Check the path: ' + FpwServPath);
        301, 302: msgln('Tip: 301/302 means a redirect occured');
      end;
    end;

const LINKING = 'Linking ';
begin
  part2:= 'compilepath='+COMPILE_PROG+'&cmd=compile';
  allparts:= MakeUrlPostVars + part2;
  // open connection
  h:= HttpConnect(Domain);
  if h = nil then ErrorHalt('CONNECTION FAILED TO DOMAIN: '+ Domain);
  HttpSetPostData(h, allparts);
  HttpSetHeader(h, 'Content-Type', 'application/x-www-form-urlencoded');
  sent:= HttpSendRequest(h, 'POST', FpwServPath);
  if not sent then begin 
    hcode:= httpresponseinfo(h, url, s);
    if verbose then WriteVerbose;
    WriteTips;
  end;
  // write compiler pipe feed
  if (hcode=200) or (hcode=0) then while not HttpEof(h) do begin
    tmpln:= httpreadln(h);
    // sniff output executable name
    if pos(LINKING, tmpln) = 1 then begin 
      OutExecutable:= StringReplace(tmpln, LINKING, '', []);
      MakeFpwTmpFile;
    end;
    msgln(tmpln);
  end;

  // free connection 
  HttpClose(h);
  // msgln('DEBUG: Sending: ', allparts);
end;

begin
  WriteVersion;
  ParseParams;
  WriteDetail1;
  TalkToServer(true);
end.

