{*******************************************************************************
                 Powtils Debug Plugin Unit Capability
********************************************************************************
  Debugging plugin system, with a default available (text file log).
  This unit is only used in other units if DBUG_ON compiler define is on.
  i.e. normal web programs have no verbose debugging by defauly.

  Copyright (c) 2007-2008 by Powtils devel team. See NRCOL License.
  Authors/Credits: L505 (Lars Olson)
********************************************************************************}

unit pwdebugplugin; 
{$i defines1.inc}

interface
uses pwtypes, pwfileutil;

{$ifdef DBUG_ON}

 procedure DefaultDebugInit(var F: THandle; const fname: astr);
 procedure DefaultDebugLn(F: THandle; var s: astr);
 procedure DefaultDebugFini(F: THandle);

{ plugin debug system.. create your own functions that write logs anywhere. 
  Set below to your desired functions in your plugin debug unit. Ignore the 
  text handle parameter in your plugin unit if not using a Text log. I.e. if 
  using a database or other storage }
var
   debuginit: procedure(var F: THandle; const fname: astr) = {$ifdef FPC}@{$endif}defaultdebuginit;
   debugfini: procedure(F: THandle)                        = {$ifdef FPC}@{$endif}defaultdebugfini;
   debugln: procedure(F: THandle; var s: string)         = {$ifdef FPC}@{$endif}defaultdebugln;

(*
 var 
   debuginit: procedure(var t: text; const fname: astr) = {$ifdef FPC}@{$endif}defaultdebuginit;
   debugfini: procedure(var t: text)                    = {$ifdef FPC}@{$endif}defaultdebugfini;
   debugln: procedure(var t: text; var s: string)           = {$ifdef FPC}@{$endif}defaultdebugln;

 procedure DefaultDebugInit(var t: text; const fname: astr);
 procedure DefaultDebugFini(var t: text);
 procedure DefaultDebugln(var t: text; var s: string);
*)

{$endif}

implementation
uses
  pwnative_out,
  {$ifdef WINDOWS}windows{$ENDIF}
  {$ifdef UNIX}baseunix{$ENDIF}
  ;

function lasterr: integer;
begin
  {$ifdef WINDOWS}result:= getlasterror;{$endif}
  {$ifdef UNIX}result:= fpgeterrno;{$endif}
end;

{$ifdef DBUG_ON}
(*
 procedure DefaultDebugln(var t: text; var s: astr);
 begin
   writeln(t, s);    
 end;

 { creates a debug log file }
 procedure DefaultDebugInit(var t: text; const fname: astr);
 var oldfmode: integer;
 begin
   oldfmode:= FileMode;
   FileMode:= fmShareDenyNone;   // TODO: UNIX FILE LOCKING,  and delphi does not accept this... so use Sysutils FileCreate/FileOpen
   assign(t, fname);
   rewrite(t);
   DefaultDebugln(t, '----DEBUG LOG----');
   flush(t);
   FileMode:= oldfmode;
 end;

 procedure DefaultDebugFini(var t: text);
 begin
   close(t);
 end;
*)

 procedure DefaultDebugInit(var F: THandle; const fname: astr);
 var err: astr;
 begin 
   F:= FileCreate(fname, fmShareDenyNone);   // TODO: UNIX FILE LOCKING
   if F = -1 then begin
     str(lasterr, err);
     ErrWithHeader('problem creating debug file: '+fname+' lasterror: '+err);
   end;
 end;

 procedure DefaultDebugLn(F: THandle; var s: astr);
 begin
   s:= s + LineEnding;
   FileWrite(F, s[1], length(s));
 end;

 procedure DefaultDebugFini(F: THandle);
 begin
   FileClose(F);
 end;
{$endif}

end.