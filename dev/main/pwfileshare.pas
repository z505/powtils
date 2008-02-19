{*******************************************************************************

                           Powtils File sharing

********************************************************************************
  Functions for safe file sharing.

  Copyright (c) 2003-2006 by PSP devel team. See PSP License for information.

  Authors/Credits: Trustmaster (Vladimir Sibirov), L505 (Lars Olson)
********************************************************************************}

unit pwfileshare;

{$IFDEF FPC}{$MODE OBJFPC}{$H+}
    {$IFDEF EXTRA_SECURE}{$R+}{$Q+}{$CHECKPOINTER ON}{$ENDIF}
{$ENDIF}
{$IFDEF win32}{$DEFINE windows}{$ENDIF}

interface

function FileMarkRead(const fname: string; var key: word): boolean;
function FileMarkWrite(const fname: string): boolean;
function FileUnmarkRead(const fname: string; key: word): boolean;
function FileUnmarkWrite(const fname: string): boolean;

procedure debugproc(s: string); 
// custom debugging
var debugln: procedure(s: string) = {$IFDEF FPC}@{$ENDIF}debugproc;

implementation

uses
  pwfileutil,
  sysutils
 {$IFDEF windows} ,
  windows  // DWORD declaration, SLEEP
 {$endif}
 ;

const
  GC_TIMEOUT = 3.0; // If flag older than 3 minutes - it is garbage left after error
  WAIT_TIME = 100; // '10' worked on windows, '1' worked on unix, but using 100 just incase

{$IFDEF UNIX}
function sleep(seconds: longint): longint; cdecl; external 'c' name 'sleep';
{$ENDIF}

{ dummy debug, set DEBUGLN for custom debugging}
procedure debugproc(s: string);
begin
end;

{ Suspends the execution until writing operations are done }
function FileSuspendRead(const fname: string): boolean;
begin
  {$IFDEF DEBUGLN_ON}debugln('FileSuspendRead begin');{$ENDIF}
  // Garbage collection
  if FileExists_read(fname + '.lfw')
    and ((abs(Now - FileDateToDateTime(FileAge(fname + '.lfw'))) * 1440)
    >= GC_TIMEOUT)
  then
    DeleteFile(pchar(fname + '.lfw'));
  // Suspending
  while FileExists_read(fname + '.lfw') do sleep(WAIT_TIME);
  result:= true;
  {$IFDEF DEBUGLN_ON}debugln('FileSuspendRead end');{$ENDIF}
end;

// Suspends the execution until the file is unlocked
function FileSuspendWrite(const fname: string): boolean;
var
  sr: TSearchRec;
begin
  {$IFDEF DEBUGLN_ON}debugln('FileSuspendWrite begin');{$ENDIF}
  // First waiting for writing operations finish
  result := FileSuspendRead(fname);

  while sysutils.FindFirst(fname + '.lfr*', faAnyFile, sr) = 0 do
  begin
    // Garbage collection
    repeat
      if (abs(Now - FileDateToDateTime(sr.Time)) * 1440) >= GC_TIMEOUT then
      begin
        if pos('\', fname) > 0 then
          DeleteFile(pchar(ExtractFilePath(fname) + '\' + sr.Name))
        else if pos('/', fname) > 0 then
          DeleteFile(pchar(ExtractFilePath(fname) + '/' + sr.Name))
        else
          DeleteFile(pchar(sr.Name));
      end;
    until FindNext(sr) <> 0;
    sysutils.Findclose(sr);
    sleep(WAIT_TIME);
  end;
  sysutils.FindClose(sr); 
  result := true;
  {$IFDEF DEBUGLN_ON}debugln('FileSuspendWrite end');{$ENDIF}
end;

{ Creates unique file reading flag, returns false if IO error }
function FileMarkRead(const fname: string; var key: word): boolean;
var
  fh: file of byte;
  lex: string;
  randomw: word;
begin
  {$IFDEF DEBUGLN_ON}debugln('FileMarkRead begin');{$ENDIF}
  result:= false;
  lex := '';
  FileSuspendRead(fname);
  if not FileExists_read(fname) then exit; 

  repeat
    randomize;
    randomw:= word(random(65534-256)) + random(100) + random(100); // L505: increase chances of random number by adding two additional random numbers 
    key:= randomw;
    str(key, lex);
  until not FileExists_read(fname + '.lfr' + lex);

  assign(fh, fname + '.lfr' + lex);
  rewrite(fh);
  close(fh);
  if ioresult = 0 then result:= true;
 {$IFDEF DEBUGLN_ON}
  debugln('FileMarkRead: Last I/O ' + FileError);
  debugln('FileMarkRead end');
 {$ENDIF}
end;

{ Creates file writing flag }
function FileMarkWrite(const fname: string): boolean;
var fh: file of byte;
begin
  {$IFDEF DEBUGLN_ON}debugln('FileMarkWrite begin');{$ENDIF}
  result:= false;
  FileSuspendWrite(fname);
  if FileExists_read(fname) and (not FileExists_read(fname + '.lfw')) then
  begin
    assign(fh, fname + '.lfw');
    rewrite(fh);
    close(fh);
    result := true;
  end;
  {$IFDEF DEBUGLN_ON}debugln('FileMarkWrite end');{$ENDIF}
end;

// Removes unique file reading flag, returns false if IO error
function FileUnmarkRead(const fname: string; key: word): boolean;
var fh: file of byte;
    lex: string;
begin
  {$IFDEF DEBUGLN_ON}debugln('FileUnmarkRead begin');{$ENDIF}
  result:= false;
  lex := '';
  if not FileExists_read(fname) then exit;
  str(key, lex);
  if not FileExists_read(fname + '.lfr' + lex) then exit;
  assign(fh, fname + '.lfr' + lex);
  erase(fh);
  if ioresult = 0 then result:= true;
 {$IFDEF DEBUGLN_ON}
  debugln('FileUnmarkRead: Last I/O ' + FileError);
  debugln('FileUnmarkRead end');
 {$ENDIF}
end;

// Removes file writing flag
function FileUnmarkWrite(const fname: string): boolean;
var fh: file of byte;
begin
 {$IFDEF DEBUGLN_ON}debugln('FileUnmarkWrite begin');{$ENDIF}
  result:= false;
  if FileExists_read(fname + '.lfw') then
  begin                                               
    assign(fh, fname + '.lfw');
    erase(fh);
    if ioresult = 0 then result := true;
   {$IFDEF DEBUGLN_ON}debugln('FileUnmarkWrite: Last I/O ' + FileError); {$ENDIF}
  end;
 {$IFDEF DEBUGLN_ON}debugln('FileUnmarkWrite end');{$ENDIF}
end;

end.

