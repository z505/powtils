{$MODE OBJFPC}{$H+} 
unit pwuDate;
{
 *******************************************************************************
                 -== Pascal Server Pages unit - Web_Date ==-
 *******************************************************************************
  Free Pascal Web Unit to output time and date in comfortable format
 *******************************************************************************
  See the Pascal Server Pages Documentation for more information.
 *******************************************************************************
    Written by Vladimir Sibirov a.k.a. Trustmaster
    http://www.psp.furtopia.org
    mailto:psp@furtopia.org
 *******************************************************************************
  Copyright (c) 2003-2004 by Pascal Server Pages development team.
  See the Pascal Server Pages License for more information.
 *******************************************************************************
}


interface


function FormatDate(FormatStr: string): string;


implementation

uses
  sysutils;


// Returns formatted date/time
function FormatDate(FormatStr: string): string;
var
  yr,mth,day,hour,mnt,scd,mscd: word;
  ys,mts,ds,hs,ms,ss,mss: string;
  ptr,len: integer;
  isItem: boolean;
  item: string;
  c: char;
begin
  DecodeDate(Date, yr, mth, day);
  DecodeTime(Time, hour, mnt, scd, mscd);
  str(yr,ys);
  str(mth,mts);
  if length(mts) = 1 then mts:='0'+mts;
  str(day,ds);
  if length(ds) = 1 then ds:='0'+ds;
  str(hour,hs);
  if length(hs) = 1 then hs:='0'+hs;
  str(mnt,ms);
  if length(ms) = 1 then ms:='0'+ms;
  str(scd,ss);
  if length(ss) = 1 then ss:='0'+ss;
  str(mscd,mss);
  if length(mss) = 1 then mss:= '00' + mss;
  if length(mss) = 2 then mss:= '0' + mss;
  len:= length(FormatStr);
  ptr:= 1;
  isItem:= false;
  while ptr <= len do
  begin
    item:= '';
    c:= FormatStr[ptr];
    if (c <> '[') and (c <> ']') and (not isItem) then
    begin
      result:= result + c;
      inc(ptr);
    end;
    if (c = '[') and (not isItem) then
    begin
      isItem:= true;
      inc(ptr);
    end;
    if (c = ']') and isItem then
    begin
      isItem:= false;
      inc(ptr);
    end;
    if (c <> '[') and (c <> ']') and isItem then
    begin
      repeat
        c:= FormatStr[ptr];
        item:= item + c;
        inc(ptr);
      until FormatStr[ptr] = ']';
      if item = 'year' then result:= result + ys;
      if item = 'month' then result:= result + mts;
      if item = 'day' then result:= result + ds;
      if item = 'hour' then result:= result + hs;
      if item = 'minute' then result:= result + ms;
      if item = 'second' then result:= result + ss;
      if item = 'millisecond' then result:= result + mss;
    end;
  end;
end;


end.
