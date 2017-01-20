{
 This is part of the CompactUtils project

 Some functions are pulled right from the freepascal sources, so the license
 on this file is the same as freepascal sources.

 The functions in this unit require no sysutils bloat, yet they perform
 the exact functionality as sysutils functions. Sysutils contains initialization
 and finalization on which many functions do not rely!

 Since the original strutils unit hauls in SysUtils, this compact strutils
 unit below was created without any reliance on sysutils!

 See the compactutils unit file for description about this project.

 Regards,
   Lars (L505)
   http://z505.com
}
{$mode objfpc} {$H+}
unit CompactStrUtils;

interface

uses
  {$ifdef KOL_MCK}
   kol;
  {$else}
   {$ifndef SYSUTILS_ON}compactsysutils{$else}sysutils{$endif};
  {$endif}

function strpos(str1,str2 : pchar) : pchar;
function PosEx(const SubStr, S: string; Offset: Cardinal): Integer;
function AnsiReplaceText(const AText, AFromText, AToText: string): string;

{ todo:

    FindPart
    AnsiReplaceStr
    And many many more!
}

implementation

function strpos(str1,str2 : pchar) : pchar;
var
   p : pchar;
   lstr2 : integer;
begin
   strpos:=nil;
   p:= strscan(str1,str2^);
   if p=nil then
     exit;
   lstr2:=strlen(str2);
   while p<>nil do
     begin
        if strlcomp(p,str2,lstr2)=0 then
          begin
             strpos:=p;
             exit;
          end;
        inc(p);
        p:=strscan(p,str2^);
     end;
end;


function PosEx(const SubStr, S: string; Offset: Cardinal): Integer;
var
  i : pchar;
begin
  if (offset<1) or (offset>length(s)) then exit;
  i:= strpos(@s[offset],@substr[1]);
  if i=nil then
    PosEx:=0
  else
    PosEx:=succ(i-pchar(s));
end;


function AnsiReplaceText(const AText, AFromText, AToText: string): string;
var
  iFrom, iTo: longint;
begin
  iTo:= Pos(AFromText,AText);
  if iTo=0 then
    result:=AText
  else
    begin
     result:='';
     iFrom:=1;
     while (ito<>0) do
       begin
         result:=Result+Copy(AText,IFrom,Ito-IFrom+1)+AToText;
         ifrom:=ITo+Length(afromtext);
         ito:=Posex(Afromtext,atext,ifrom);
       end;
     if ifrom<=length(atext) then
      result:=result+copy(AText,ifrom, length(atext));
    end;
end;


end.
 
