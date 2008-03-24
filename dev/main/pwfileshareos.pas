{**************** Powtils ******************************************************
 Cross platform file sharing through OS calls
 Alternative to the fileshare.pas unit
 
 Autrhors: Lars (L505) http://z505.com ,
 License: ~NRCOL public domain
*******************************************************************************} 

unit pwfileshareos;
{$i defines1.inc}
interface

function OpenFileShared(var f:Text): integer;
procedure CloseFileShared(var f: text);

implementation

uses
  {$ifdef windows}windows,{$endif}
  {$ifdef unix}unix,{$endif}  
  sysutils;

{$ifdef fpc}
 const invalid_handle_value = -1;
{$endif}

function OpenFileShared(var f:Text): integer;
begin
 {$ifdef windows}
  with TTextRec(f) do begin 
    handle:=FileOpen(Name,fmShareDenyNone);
    if dword(handle)= invalid_handle_value then begin
       result:= GetLastError;
    end;  
    mode:=fmInput;
    BufPos:=0;
    BufEnd:=0;
  end;  
 {$endif}  

 {$ifdef unix}  
  reset(f);
  fpflock(f, LOCK_SH);
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
