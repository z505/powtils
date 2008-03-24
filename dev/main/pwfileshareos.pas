{**************** Powtils ******************************************************
 Cross platform file sharing through OS calls
 Alternative to the fileshare.pas unit
 NOT DONE YET... DO NOT USE.
 
 Autrhors: Lars (L505) http://z505.com ,
 License: ~NRCOL public domain
*******************************************************************************} 

unit pwfileshareos;
{$i defines1.inc}
interface

function ResetFileShared(var f: TextFile{; const fname: shortstring}): integer;
procedure CloseFileShared(var f: text);
procedure RunTest1;

implementation

uses
 {$ifdef windows}windows,{$endif}
 {$ifdef unix}unix,{$endif}  
  sysutils;

{$ifdef fpc}
 const invalid_handle_value = -1;
{$endif}


procedure RunTest1;
var t: text;
begin
  Assign(t,  'test123.txt');
  ResetFileShared(t{, 'test123.txt'});
  {
  writeln('This is a test for file sharing');
  writeln('Run simultaneous processes at once to ensure this works.');
  writeln('This is a test for file sharing');  
  writeln('This is a test for file sharing');  
  writeln('This is a test for file sharing');  
  writeln('This is a test for file sharing');    
  writeln('This is a test for file sharing');    }
  CloseFileShared(t);
end;


{ Same as system.reset but implements file sharing returns nonzero if problem }
function ResetFileShared(var f: TextFile{; const fname: shortstring}): integer;
begin
 {$ifdef windows}
  TTextRec(f).handle:= FileCreate(TTextRec(f).name,fmShareDenyNone);
//  if dword(TTextRec(f).handle) = invalid_handle_value then result:= -1;
//  TTextRec(f).mode:= fmInput;
//  TTextRec(f).BufPos:= 0;
//  TTextRec(f).BufEnd:= 0;
 {$endif}  

  reset(f);
 {$ifdef unix}  
  result:= fpflock(f, LOCK_SH);
 {$endif}
end; 

procedure CloseFileShared(var f: text);
begin
  close(f);
 {$ifdef unix}  
  fpflock(f, LOCK_UN);
 {$endif}
end;

end.
