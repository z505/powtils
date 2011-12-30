(****************************************************************************** 
  Copyright Lars Olson  2008-2011
   
*******************************************************************************)

unit htmout;  {$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF} 

interface

//const PARAMS = 'params';

const
 // cross platform directory separator
 {$IFDEF windows}SLASH = '\';{$ENDIF}
 {$IFDEF unix}SLASH = '/';{$ENDIF}

procedure NoPage;
procedure Show;

implementation

uses pwinit, pwmain, baseunix, unix, pwurlenc, pwsubstrings, pwenvvar, 
     pwfileutil, strwrap1;

type TStrArray = array of string;
var ManyParams: TStrArray;       

procedure notice(const s: string);
begin
  outln('Note: ' + s);
  outln('');
end;

{ if no page specified, display simple error page }
procedure NoPage;
begin
  outln('No compilation file specified.');
  halt;
end;

procedure err(const s: string);
begin
  outln('');
  outln('Error: ' + s);
end;

{ compiler can be relative to document root with this macrovar trick}
procedure FilterMacroVar(var s: string);
begin
  s:= SubstrReplace(s, '{{DOCROOT}}', CgiEnvVar.DocRoot() );
end;

function CmdExists(const cmd: string): boolean;
begin
  if FileExists_plain(cmd) then result:= true else result:= false;
end;

procedure WriteCommandError;
begin
  outln('Command error');
end;

function GetParams(out rslt: TStrArray): boolean;
var i: integer;
    tmp: string;
    total: integer = 0;
begin
  result:= false;
  if CountCgiVars < 1 then exit;
  SetLength(rslt, CountCgiVars);
  for i:= 0 to CountCgiVars -1 do begin
    tmp:= FetchCgiVarName_S(i, 0);
    if pos('param', tmp) = 1 then begin
      rslt[total]:= urldecode(FetchCgiVarVal_S(i, 0));
      inc(total);
    end;
  end;
  // correct length
  if total > 0 then 
  begin 
    SetLength(rslt, total);
    result:= true;
  end else
    result:= false;
end;

function RunCommand(const cmd: string; const params: TStrArray): boolean;
var
  Si, So, Serr: Text;
  s: String;
begin
  //status('Command to run: <b>'+ cmd + '</b>');

  // try running command
  if AssignStream(Si, So, Serr, cmd, params) = -1 then
    outln('AssignStream failed !')
  else 
  begin
    if fpgeterrno <> 0 then
    begin
      outln('AssignStream failed !');
      exit;
    end;
    close (so);
    //status('Command result:');
    // read command result from STD IN 
    while not eof(Si) do
    begin
      readln(Si,s);
      outln(s);
    end;
    // read command result from STD ERR
    while not eof(Serr) do
    begin
      readln(Serr, s);
      outln(s);
    end;
    close(Si);
    result:= true;
  end;
end;

function CompileFile(cmd: string; const params: TStrArray): boolean;
begin
  result:= false;
  FilterMacroVar(cmd);
  if CmdExists(cmd) then 
    result:= RunCommand(cmd, params)
  else 
    err('Compiler path not found or cannot access: '#13#10 +'  '+ cmd + #13#10);
end;


function Compiling: boolean;
begin
  result:= false;
  if GetCgiVar('cmd') = 'compile' then result:= true;
end;

function CompilePath: string;
begin
  result:= GetCgiVar_S('compilepath', 0);
  result:= urldecode(compilepath);
end;

{ show compiler output  }
procedure Show;
 
  function Compiled: boolean;
  begin
    result:= false;
    if compilepath = '' then err('compilepath not specified') 
    else
      result:= CompileFile(compilepath, ManyParams);
  end;

begin
  // no parameters? param1= param2= etc.
  if GetParams(ManyParams) = false then NoPage;
  if (Compiling) and (Compiled) then begin
    //status('Tried to compile: ');
    //status(   '  ' + params);
  end;
  if (not Compiling) then WriteCommandError;
end;


end.
