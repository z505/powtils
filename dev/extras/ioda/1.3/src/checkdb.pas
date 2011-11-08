program CheckDB;
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

uses
   Classes,SysUtils,IdList,BTreeFlex,OccTable;

type
	TPrg	=	class
					constructor Create(const name:string; zeige:cardinal; var res:integer);
					destructor  Destroy; OVERRIDE;
					procedure	Run;
					
				PRIVATE
					baum	:	TBayBaum;
					occ	:	TClusterMaster;
					loLimit,
					words,
					occWords,
					allwords	:	cardinal;
					
					procedure Count(const akt:Tval);
				
				PUBLIC
					property 	Worte:cardinal read words;
					property 	OccWorte:cardinal read occWords;
					property 	AlleWorte:cardinal read allwords;
				end;

				
constructor TPrg.Create(const name:string; zeige:cardinal; var res:integer);
begin
	inherited Create;
	words:=0; allwords:=0; occWords:=0; loLimit:=zeige;
	baum:=TMemBayBaum.Create(name,0,0,true,res);   if res<>0 then FAIL;
	occ:=TClusterMaster.Open(name,0,0,0,true,false,res); if res<>0 then FAIL;
	with baum do
	begin
		writeln(' QUALITY:      ',quality:10);
		writeln(' UNIQUE WORDS: ',WordCount:10);
		writeln(' ALL WORDS:    ',AllCount:10);
		writeln(' NODES:        ',NodeCount:10);		
	end;
end;


destructor TPrg.Destroy;
begin
	inherited Destroy;
	occ.Free;
	baum.Free;
end;


procedure TPrg.Run;
begin
	baum.GetAll(@Count);
end;

	
procedure TPrg.Count(const akt:Tval);
var
	n	:	longint;
begin
	if (akt.z>loLimit) or (akt.s=paramstr(2)) then writeln(akt.z:12,akt.s:20);
	inc(words);
	allwords+=akt.z;
	n:=occ.GetItemCount(akt.dp);
	if n<0 then begin 
		writeln('FEHLER ',n,' bei "',akt.s,'": BTree=',akt.z,' DP=',akt.dp:12); 
		EXIT;
	end;
	occWords+=cardinal(n);
	if cardinal(n)<>akt.z then writeln('Abweichung bei "',akt.s,'": BTree=',akt.z,' OccList=',n);
end;


var
	prg	:	TPrg;
	res	:	integer;
	zeige	:	cardinal;
begin
	zeige:=StrToIntDef(paramstr(2),1000000);
	writeln('BTree stored values:');
	prg:=TPrg.Create(paramstr(1),zeige,res);
	if res=0 then begin
		writeln('List stored values:');
		write('[please wait]');
		prg.Run;
	end else  begin
		writeln('RES=',res);
		HALT(1)
	end;
	
	writeln(#13' UNIQUE WORDS: ',prg.Worte:10);
	writeln(' BTREE WORDS:  ',prg.AlleWorte:10);
	writeln(' LIST WORDS:   ',prg.OccWorte:10);
	writeln;

	if  (prg.OccWorte=prg.AlleWorte) then begin
		if (prg.Worte=prg.baum.WordCount) and (prg.AlleWorte=prg.baum.AllCount) then
			writeln('DATABASE IS O.K.'#10)
		else 
			writeln('DATABASE IS O.K. but Btree statistic is not up to date (jodad flush needed?)'#10)
	end else 
		writeln('DATABASE IS INKONSISTENT!'#10);

	prg.Free;
end.


