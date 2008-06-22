{ Simple web based command line utility to control a web server that doesn't 
  have shell/ssh/telnet access available

  Notes:
    -tested on Linux, MS Windows

  Author: 
    Lars (L505
    http://z505.com
}
program webcmd1; 
{$ifdef fpc}{$mode objfpc} {$H+} {$UNITPATH ../../main} {$endif}

uses  
  {$ifdef unix}unix, baseunix,{$endif} 
  pwinit, pwmain, pwenvvar, pwsubstr, pwfileutil, pwtypes, htmout, pwstrutil;


procedure redirectStdErr;
begin
//  close(stderr); // needed? 
//  assign(stderr, 'stderr.txt'); 
//  rewrite(stderr); 
end;

procedure restoreStdErr;
begin
//  close(stderr); 
//  assign(stderr,); 
//  rewrite(stderr); 
end;

procedure err(const s: astr);
begin out('<br><b>Error:</b> ' + s);
end;

{$ifdef windows}
{ find program command (before first space) }
function getCmdPath(const cmd: astr): astr;
var i: integer;
begin
  result:= '';
  if cmd = '' then exit;
  for i:= 1 to length(cmd) do begin
    if cmd[i] = ' ' then exit;
    result:= result + cmd[i];
  end;
end;
 
 { find program arguments (after space) }
function getCmdArgs(const cmd: astr): astr;
var i, spacefound: integer;
begin
  result:= ''; spacefound:= 0;
  if cmd = '' then exit;
  for i:= 1 to length(cmd) do begin
    if spacefound > 1 then result:= result + cmd[i];
    if cmd[i] = ' ' then inc(spacefound);
  end;
end;
{$endif}

function execCmd(const cmd: astr): int32;
begin
 {$ifdef unix}   result:= fpSystem(cmd);{$endif} 
 {$ifdef windows}result:= executeProcess('cmd', '/c '+GetCmdPath(cmd)+' '+GetCmdArgs(cmd));{$endif}
end;

procedure showCmdStatus(err: int32);
begin
  out('WEBCMD NOTE: command exited with status: ' + i2s(err));
//  out('Status message: '+ fpgeterrno(errno));
end;

procedure runAndShowCmd(const cmd: astr);
var err: int32;
begin
  out('<hr style="border-style: solid; border-width: 1px;">');
  out('Output of command: <b>'+ cmd + '</b>');
  outln('<textarea style="width:100%; font-size:0.9em;" ROWS=40>');
  // execute command such as // ls/mv/cp/tar etc.
  err:= ExecCmd(cmd);
  outln(  '-------------------------------------------------------------------------');
  showCmdStatus(err);
  outln('</textarea>');
end;

function formPosted: boo;
begin
  result:= false;
  if isPostVar('form1posted') then result:= true;
end;

type THtmForm = record cmd: astr; end;
var HtmForm: THtmForm;

{ get incoming cmd and params }
procedure getPostedVars;
begin
  HtmForm.cmd:= getCgiVar_S('ed1', 0);
end;

{ process command, notify it was attempted }
procedure processCommand;
begin
//  redirectStdErr;
  runAndShowCmd(HtmForm.cmd);
  notify;
//  restoreStdErr;
end;

procedure setup;
  { server document root full path is useful as a special macro }
  procedure expandDocRootMacro(var s: astr);
  begin
    s:= SubstrReplace(s, '{$DOCROOT}', SERV.DocRoot() );
    s:= SubstrReplace(s, '$DOCROOT', SERV.DocRoot() );
  end;
begin
  getPostedVars;
  expandDocRootMacro(HtmForm.cmd);
  // setup $remembercmd macro var for later use with OutF or TemplateOut
  setVar('remembercmd', HtmForm.cmd);
end;

begin
  startPage;
  setup;
  jotForm;
  if formPosted then processCommand;
  endPage;
end.
