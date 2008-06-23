{ Alexander N Zubakov  All Rights Reserved
  Modified March 2008 by Lars Olson. Aservia (web server). Based on nYume  
  License: see aservia license in docs/License.html }  

unit shell; {$mode objfpc}{$H+}

interface
uses pwtypes;

var PathToShell: astr = '/bin/sh';

procedure AddEnv(name, value: astr);
procedure ClearEnv;
function Cmd(command_: astr): integer;
function Command(command_: astr): astr;

{------------------------------------------------------------------------------}
                             implementation
{------------------------------------------------------------------------------}

uses pwfileutil, pwstrutil;

var
  LocalEnv  : astr  = {$ifndef mswindows}'env '{$else}''{$endif};
  changedenv: boo = false;
  
procedure addEnv(name, value: astr);
begin
  LocalEnv += {$ifndef mswindows}
    name + '="' + value + '" ';
  {$else}
    'set ' + name + '=' + value + #13#10;
  {$endif}
  
  changedenv := true;
end;

procedure ClearEnv;
begin
  LocalEnv  := {$ifndef mswindows}'env '{$else}''{$endif};
  changedenv:= false;
end;
  
function Cmd(command_: astr): integer;
var fname: astr = {$ifndef mswindows}'tmp_JKfjCGeh__cmd.sh'{$else}'tmp_JKfjCGeh__cmd.bat'{$endif};
    f: text;
begin
  Randomize;
  while FileThere(fname, fmR) do
    fname := '_' + IntToStr(random(10000)) + fname;

  Assign(f, fname);
    Rewrite(f);
    {$ifdef mswindows}Writeln(f, '@echo off');{$endif}
    if changedenv then Write(f, LocalEnv);
    Writeln(f, command_);
  Close(f);

  result := {$ifdef mswindows}ExecuteProcess('./' + fname, '')
            {$else}ExecuteProcess(PathToShell, fname){$endif};

  Erase(f);
end;

function Command(command_: astr): astr;
var f: file; 
    buf: astr; 
    count: int32;
    fname: astr = 'tmp_JKfjCGeh__pipe.txt';
begin
  randomize;
  while FileThere(fname, fmR) do
    fname += IntToStr(random(10000)) + '.txt';

  Cmd(command_ + {$ifdef mswindows}'>'{$else}' > '{$endif} + fname);
  Assign(f, fname);
  Reset(f, 1);

  result := '';
  setlength(buf, 4096);
  while not eof(f) do begin
    BlockRead(f, pointer(buf)^, 4096, count);
    if count < 4096 then buf := copy(buf, 1, count);
    result += buf;
  end;

  Close(f);
  Erase(f);
end;

{------------------------------------------------------------------------------}
                                     end.
{------------------------------------------------------------------------------}

