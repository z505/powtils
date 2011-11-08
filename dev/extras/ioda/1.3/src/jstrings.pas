Unit jstrings;

(* Copyright (C) 1994-2004  jo@magnus.de
   This library is free software; you can redistribute it and/or modify it 
	under the terms of the GNU Lesser General Public License as published by
	the Free Software Foundation; either version 2.1 of the License, or 
	(at your option) any later version.

   This library is distributed in the hope that it will be useful, but 
	WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
	or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public 
	License for more details.

   You should have received a copy of the GNU Lesser General Public License 
	along with this library; if not, write to the Free Software Foundation, 
	Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA 
*)

{$H+}
INTERFACE
	function StrSubst(s:string; const oldCh,newCh:char; cis:boolean):string;
	function StrDel(s:string; const oldCh:char; cis:boolean):string;
	function StrStrSubst(s:string; const oldStr,newStr:string; cis:boolean):string;
	function Split(s,splitter:string; var a:array of String):integer;	
	function Hex2Int(s:string):cardinal;
	
	
IMPLEMENTATION
uses Sysutils;

const
	hexstring 	: 	string  = '0123456789ABCDEF';
	
		
	
function StrSubst(s:string; const oldCh,newCh:char; cis:boolean):string;
var
	i	:	integer;
begin
	for i:=1 to length(s) do
		if (s[i]=oldCh) or (cis and (Upcase(s[i])=Upcase(oldCh))) then s[i]:=newCh;
	result:=s
end;


function StrDel(s:string; const oldCh:char; cis:boolean):string;
var
	i	:	integer;
begin
	i:=1;
	while i<=length(s) do
	begin
		if (s[i]=oldCh) or (cis and (Upcase(s[i])=Upcase(oldCh))) then delete(s,i,1) else inc(i);
	end;
	result:=s
end;


function StrStrSubst(s:string; const oldStr,newStr:string; cis:boolean):string;
var
	p,last	:	integer;
	t,oldS	:	string;
begin
	if (oldStr='') or (oldStr=newStr) then EXIT;
	if cis then oldS:=AnsiUppercase(oldStr) else oldS:=oldStr;
	last:=1;
	repeat
		if cis then t:=AnsiUppercase(s) else t:=s;
		p:=pos(oldS,copy(t,last,255));
		if p>0 then
		begin
			inc(p,last-1);
			s:=copy(s,1,p-1)+newStr+copy(s,p+length(oldS),254);
			last:=p+length(newStr);
		end;
	until p=0;
	result:=s
end;



function Split(s,splitter:string; var a:array of string):integer;
var
	p,i,l	:	integer;
	t		:	string;
begin

	if s='' then
	begin
		result:=-1; EXIT
	end;
	i:=0; l:=length(splitter);
try			
	while (s<>'') and (i<=High(a)) do
	begin
		p:=pos(splitter,s);
		if p>0 then
		begin
			if (p>1) then 
			begin
				t:=copy(s,1,p-1);
				a[i]:=t;
			end else
				a[i]:='';

			delete(s,1,l+p-1);
			inc(i);
		end else
		begin
			a[i]:=s; inc(i);
			BREAK;
		end;
	end;
	result:=i-1;
except
	on E:Exception do 
	begin	
		writeln(E.message,' in JStrings.pas::Split()'); result:=0;
	end;
end;

end;


function Hex2Int(s:string):cardinal;
var
	w  	:	longint;
	i,j	: 	integer;

begin
	w:=0;
	s:=Upcase(s);
	for i:=1 to length(s) do
	begin
		j:=1;
		while (j<=16) and (s[i]<>hexstring[j]) do inc(j);
		if j>16 then
		begin
			Hex2Int:=0; EXIT;
		end;
		w:=w shl 4 + (j-1);
	end;
	result:=w;
end;


end.
