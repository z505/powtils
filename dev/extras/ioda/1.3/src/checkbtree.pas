program CheckBTree;
{$H+}
(* Copyright (C) 1994-2005  jo@magnus.de
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


uses
   Classes,SysUtils,IdList,BtreeFlex;

type
	TPrg	=	class
					constructor Create(const name:string; zeige:cardinal; var res:integer);
					destructor  Destroy; OVERRIDE;
					procedure	Run;
					
				PRIVATE
					baum	:	TBayBaum;
					loLimit,
					words,
					allwords	:	cardinal;
					
					procedure Count(const akt:Tval);
				
				PUBLIC
					property 	Worte:cardinal read words; 
					property 	AlleWorte:cardinal read allwords; 
				end;



constructor TPrg.Create(const name:string; zeige:cardinal; var res:integer);
begin
	inherited Create;
	words:=0; allwords:=0; loLimit:=zeige;
	baum:=TMemBayBaum.Create(name,0,0,true,res);
	if res<>0 then FAIL;
	with baum do
	begin
		writeln(' QUALITY: ',quality:10);
		writeln(' WORDS    ',WordCount:10);
		writeln(' ENTRIES: ',AllCount:10);
		writeln(' NODES:   ',NodeCount:10);		
	end;
end;


destructor TPrg.Destroy;
begin
	inherited Destroy;
	baum.Free
end;


procedure TPrg.Run;
begin
	baum.GetAll(@Count);
end;

	
procedure TPrg.Count(const akt:Tval);
begin
	if (akt.z>=loLimit) or (akt.s=paramstr(2)) then writeln(akt.z:12,akt.s:20);
	inc(words);
	inc(allwords,akt.z);
end;


var
	prg	:	TPrg;
	res	:	integer;
	zeige	:	cardinal;
begin
	zeige:=StrToIntDef(paramstr(2),1000000);
	prg:=TPrg.Create(paramstr(1),zeige,res);
	if res=0 then 
		prg.Run 
	else 
	begin
		writeln('RES=',res);
		HALT(1)
	end;
	
	writeln(' nachgezählt:');
	writeln(' WORDS:   ',prg.Worte:10);
	writeln(' ENTRIES: ',prg.AlleWorte:10);
	prg.Free;
end.


