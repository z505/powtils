{ Simple web based command line utility to control a web server that doesn't 
  have shell/ssh/telnet access available

  Notes:
    -tested on linux, not on MS Windows

  Author: 
    Lars (L505
    http://z505.com
}
program webcmd1;
{$mode objfpc} {$H+}

uses  
  {$ifdef unix}unix, baseunix, compactsysutils,{$endif} 
  {$ifdef windows}sysutils,{$endif}
  pwinit,
  pwmain,
  pwenvvar,
  pwsubstr,
  pwfileutil,
  htmout;

procedure err(const s: string);
begin
  out('<br><b>Error:</b> ' + s);
end;

{$ifdef windows}
 { parse for program command (before first space) }
 function GetCmdPath(const cmd: string): string;
 var i: integer;
 begin
   result:= '';
   if cmd = '' then exit;
   for i:= 1 to length(cmd) do begin
     if cmd[i] = ' ' then exit;
     result:= result + cmd[i];
   end;
 end;
 
 { parse for command arguments (after space) }
 function GetCmdArgs(const cmd: string): string;
 var i, spacefound: integer;
 begin
   result:= '';
   spacefound:= 0;
   if cmd = '' then exit;
   for i:= 1 to length(cmd) do begin
     if spacefound > 1 then result:= result + cmd[i];
     if cmd[i] = ' ' then inc(spacefound);
   end;
 end;
{$endif}


procedure RunAndShowCmd(const cmd: string);
var err : Longint;
begin
  out('<hr style="border-style: solid;">');
  out('Output of command: <b>'+ cmd + '</b>');
  outln('<textarea style="width:100%; font-size:0.9em; font-family:courier new;" ROWS=80>');
  {$ifdef unix}   err:= fpSystem(cmd);{$endif} //ls -l *.pp
  {$ifdef windows}err:= executeprocess(GetCmdPath(cmd), GetCmdArgs(cmd));{$endif}
  outln(  '-------------------------------------------------------------------------');
  outln(  'WEBCMD NOTE: command exited with status: ' + inttostr(err));
  outln('</textarea>');
end;

function FormPosted: boolean;
begin
  result:= false;
  if IsCgiVar('form1posted') then result:= true;
end;

type
  THtmForm = record
    cmd: string;
  end;

var
  htmform: THtmForm;

{ get incoming cmd and params }
procedure GetPostedVars;
  { first handle macrovars, exe can be installed relative to document root }
  procedure FilterMacroVar(var s: string);
  begin
    s:= SubstrReplace(s, '{$DOCROOT}', CgiEnvVar.DocRoot() );
    s:= SubstrReplace(s, '$DOCROOT', CgiEnvVar.DocRoot() );
  end;

begin
  htmform.cmd:= GetCgiVar_S('ed1', 0);
  FilterMacroVar(htmform.cmd);
end;

{ process command, notify it was attempted }
procedure ProcessCommand;
begin
  RunAndShowCmd(htmform.cmd);
  Notify;
end;

procedure Setup;
begin
  GetPostedVars;
  // setup $remembercmd macro var for later use with OutF or TemplateOut
  setvar('remembercmd', htmform.cmd);
end;

begin
  StartPage;
  Setup;
  JotForm;
  if FormPosted then ProcessCommand;
  EndPage;
end.
