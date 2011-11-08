library jodafulltext;

(* Copyright (C) 1994-2005  Jochen Magnus <jo@magnus.de>
                            Oliver Graf <ograf@rz-online.net>
 
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

uses CMEM,Classes,SysUtils,Volltext,HitList;

const
	VTMAXDBS = 32;
	version = '3.4';
			 
type			
	EConfigError = class(Exception);
	PUV 			 = ^cardinal;
 
var
	vtdbs : array [1 .. VTMAXDBS] of TVolltext;
		  
		  

function jodaOpen(dbname : PChar; openmode : integer): integer; cdecl; export;
var
	vtidx, res : integer;
	db		   : string;
						   
begin
	res:=0;
	result:=0;
	vtidx:=1;
	while (vtidx<=VTMAXDBS) and (vtdbs[vtidx]<>nil) do inc(vtidx);
	if vtidx>VTMAXDBS then
		result:=-1
	else begin
		db:=ChangeFileExt(StrPas(dbname),'');
		vtdbs[vtidx]:=TVolltext.Create(db,TDbMode(openmode),res);
		if res<>0 then begin
			result:=-abs(res);
			try
				vtdbs[vtidx].Free
			except
				on E:Exception do ;
			end;
			vtdbs[vtidx]:=nil;
		end else begin
			result:=vtidx;
		end;
	end;
end; { jodaOpen }



function jodaClose(handle : integer): integer; cdecl; export;
begin  
	result:=-1;
	if (handle>0) and (handle<=VTMAXDBS) then begin
		if vtdbs[handle]=nil then
			result:=-2
		else begin
			try
				vtdbs[handle].Free;
				vtdbs[handle]:=nil;
				result:=0;
			except
				on E:Exception do result:=-4;
			end;
		end;
	end;
end; { jodaClose }


{ Deprecated! }
procedure jodaSortIssues(handle:integer; const issueParams:PChar); cdecl; export;
begin
	if ((handle>0) and (handle<=VTMAXDBS) and (vtdbs[handle]<>NIL)) then 
		vtdbs[handle].SetSortedResults(StrPas(issueParams));
end;


function jodaSortIssuesPP(handle:integer; const issuePattern, issuePreferred:PChar): integer; cdecl; export;
begin
	result:=-1;
	if (handle>0) and (handle<=VTMAXDBS) then begin
		if vtdbs[handle]=nil then
			result:=-2
		else begin
			vtdbs[handle].SetSortedResults(StrPas(issuePattern), StrPas(issuePreferred));
			result:=0;
		end;
	end;
end;


function jodaSortIssuesOLP(handle: integer; const offset, length: integer; issuePreferred:PChar): integer; cdecl; export;
begin
	result:=-1;
	if (handle>0) and (handle<=VTMAXDBS) then begin
		if vtdbs[handle]=nil then
			result:=-2
		else begin
			vtdbs[handle].SetSortedResults(offset, length, StrPas(issuePreferred));
			result:=0;
		end;
	end;
end;


function jodaSearch(handle : integer;
						 query, dstart, dend, fileFilter	: PChar;
						 maxHits, sortOrder, bitFilter	: integer;
					    var overflow:integer): integer; cdecl; export;
var
	q  : string;
	ov : boolean;
	   
begin
	result:=-1;
	if (handle>0) and (handle<=VTMAXDBS) then begin
		if vtdbs[handle]=nil then
			result:=-2
		else begin
			q:=StrPas(query);
			result:=vtdbs[handle].Suche(q,StrPas(dstart),StrPas(dend),StrPas(fileFilter),maxHits,sortOrder,bitFilter,ov);
			if ov then overflow:=1 else overflow:=0;
		end;
	end;
end; { jodaSearch }



function jodaVLSearch(	  handle						   : integer;
						  query, dstart, dend, fileFilter  : PChar;
						  bitFilter						   : PByte;
						  bitFilterNum, maxHits, sortOrder : integer;
					  var overflow						   : integer): integer; cdecl; export;
var
	q  : string;
	ov : boolean;
	   
begin
	result:=-1;
	if (handle>0) and (handle<=VTMAXDBS) then begin
		if vtdbs[handle]=nil then
			result:=-2
		else begin
			q:=StrPas(query);
			result:=vtdbs[handle].vlSuche(q,StrPas(dstart),StrPas(dend),StrPas(fileFilter),bitFilter,bitFilterNum,maxHits,sortOrder,ov);
			if ov then overflow:=1 else overflow:=0;
		end;
	end;
end; { jodaVLSearch }



function jodaGetOneHit(handle, hit, maxlen : integer;
					   	  buffer			       : PChar): integer; cdecl; export;
begin
	result:=-1;
	if (handle>0) and (handle<=VTMAXDBS) then begin
		if vtdbs[handle]=nil then
			result:=-2
		else begin
			if (hit<0) then
				result:=-3
			else begin
				StrPCopy(buffer,copy(vtdbs[handle][hit],1,maxlen-1));
				result:=0;
			end;
		end;
	end;
end; { jodaGetOneHit }
	


function jodaGetAllHits(handle, hits, maxlen : integer;
							   buffer				 	: PChar): integer; cdecl; export;
var
	res	: string;
	i	: integer;
	
begin
	result:=-1;
	if (handle>0) and (handle<=VTMAXDBS) then begin
		if vtdbs[handle]=nil then
			result:=-2
		else begin
			res:='';
			if hits>1 then
			begin
				for i:=0 to hits-2 do
				begin
					if vtdbs[handle][i]<>'' then
						res:=res+vtdbs[handle][i]+#10
					else
						BREAK;
				end;
			end;
			
			if hits>0 then
				res:=res+vtdbs[handle][hits-1];
			StrPCopy(buffer,copy(res,1,maxlen-1));
			result:=length(res);
		end;
	end;
end; { jodaGetAllHits }


function jodaGetQuery(handle : integer): PChar; cdecl; export;
begin
	if ((handle>0) and (handle<=VTMAXDBS) and (vtdbs[handle]<>NIL)) then 
		result:=PChar(vtdbs[handle].query)
	else
		result:=nil;
end;

function jodaGetHit(handle : integer; i: integer): PHit; cdecl; export;
begin
	if ((handle>0) and (handle<=VTMAXDBS) and (vtdbs[handle]<>NIL)) then 
		result:=vtdbs[handle].HitPtr[i]
	else
		result:=nil;
end;


function jodaGetHits(handle	: integer; var i:integer): PHitArray; cdecl; export;
begin
	if ((handle>0) and (handle<=VTMAXDBS) and (vtdbs[handle]<>NIL)) then begin
		result:=vtdbs[handle].HitArrayPtr;
		i:=vtdbs[handle].HitCount;
	end else begin
		result:=nil;
		i:=0;
	end;
end;


function jodaStore(handle : integer;
					    words, fileName, date : PChar;
					    info	       		     : integer;
				       var id		           : cardinal): integer; cdecl; export;
var
	wlist : TStringList;
begin
	result:=0;
	if (handle>0) and (handle<=VTMAXDBS) then begin
		if vtdbs[handle]=nil then
			result:=-2
		else begin
			wlist:=TStringList.Create;
			wlist.Sorted:=False;
			wlist.Duplicates:=dupAccept;
			wlist.SetText(words);
			if wlist.Count=0 then
				result:=-100
			else 
				result:=vtdbs[handle].InsertWords(wlist,StrPas(fileName),StrPas(date),'',info,id);
			wlist.Free;
		end;
	end;
end; { jodaStore }



function jodaInvalidateEntry(handle : integer; words: PChar; id : cardinal): integer; cdecl; export;
var
	wlist : TStringList;
begin
	result:=0;
	if (handle>0) and (handle<=VTMAXDBS) then begin
		if vtdbs[handle]=nil then
			result:=-2
		else begin
			wlist:=TStringList.Create;
			wlist.Sorted:=False;
			wlist.Duplicates:=dupAccept;
			wlist.SetText(words);
			if wlist.Count=0 then
				result:=-100
			else
				result:=vtdbs[handle].InvalidateEntry(wlist,id);
			wlist.Free;
		end;
	end;
end;


function jodaChainDuplicate(handle : integer; fname: PChar; lastID:cardinal; var id: cardinal): integer; cdecl; export;
begin
	result:=0;
	if (handle>0) and (handle<=VTMAXDBS) then begin
		if vtdbs[handle]=nil then
			result:=-2
		else begin
			result:=vtdbs[handle].ChainDuplicate(StrPas(fname),lastID,id);
		end;
	end;
end; { jodaChainDuplicate }


function jodaMergeDB(dst, src : integer; startDatum : PChar; wordCheck,fileCheck,destructive,verbose:integer): integer; cdecl; export;
begin
	result:=0;
	if (src<1) or (src>VTMAXDBS) then
		result:=-1
	else if (dst<1) or (dst>VTMAXDBS) then
		result:=-2
	else if (dst=src) then
		result:=-3
	else if (vtdbs[src]=nil) or (vtdbs[dst]=nil) then
		result:=-4
	else begin
		result:=vtdbs[dst].ExecMerge(vtdbs[src],startDatum,wordCheck<>0,fileCheck<>0,destructive<>0,verbose<>0);
	end;
end; { jodaMergeDB }


exports	
    jodaOpen, jodaClose,
    jodaSortIssuesPP, jodaSortIssuesOLP,
    jodaStore, jodaInvalidateEntry, jodaChainDuplicate,
    jodaSearch, jodaVLSearch,
    jodaGetQuery, jodaGetHit, jodaGetHits,
    jodaMergeDB,
    (* alternative Interface to the above line: *)
    jodaSortIssues, jodaGetOneHit, jodaGetAllHits;

var
	i	: integer;
begin
	try
		for i:=1 to VTMAXDBS do vtdbs[i]:=NIL;
	except
		on E:Exception do writeln('Cannot Initalize libjodafulltext!');
	end;
end.
