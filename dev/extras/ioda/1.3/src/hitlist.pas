unit HitList;
{
 Trefferliste
}

(* Copyright (C) 2004  Oliver Graf <ograf@oli-ver-ena.de>
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

uses classes, sysutils, RegExpr;

type
	{$PackRecords 2}
	THit	 = record
				   id	   : cardinal;
				   date	   : cardinal;
				   weight  : cardinal;
				   info	   : cardinal;
				   dup	   : cardinal; (* 0 == no duplicate *)
				   fileref : AnsiString; (* may be empty *)
				   title   : AnsiString; (* may be empty *)
				   sort	   : AnsiString; (* may be empty *)
			   end;
	PHit = ^THit;
	THitArray = Array of THit;
	PHitArray = ^THitArray;
	TDupSortList = class(TList)
				PUBLIC
				constructor Create(const pattern, preferred:string); VIRTUAL; overload;
				constructor Create(const offset, length: integer; preferred:string); VIRTUAL; overload;
				destructor  Destroy; OVERRIDE;
				procedure	ClearList();
				function	AddWithFileRef(fileref, title : string; id, date, weight, info: cardinal): integer;
				function	GetHit(i : integer): THit;
				procedure	CustomSort();
				public
				sortStr, prefStr: string;
				PROTECTED
				sortRe	 : TRegExpr;
				prefRe   : TRegExpr;
				offs, len: integer;
				PUBLIC
				property Hits[i	: integer]: THit read GetHit; default;
			end;
	THitList = class
				constructor	Create(const maxhits: integer);
				destructor	Destroy; OVERRIDE;
				procedure 	Clear;
				procedure	AddWithoutFileRef(id, date, weight, info: cardinal);
				procedure	AddWithFileRef(fileref, title : string; id, date, weight, info: cardinal);
				procedure	AddDuplicates(dups: TDupSortList);
				function	GetHit(i: integer): string;
				function	GetHitRaw(i: integer): THit;
				function	GetHitRawPtr(i: integer): PHit;
				function	GetHitListPtr(): PHitArray;
				PROTECTED
				max    : integer;
				cnt    : integer;
				hitarr : THitArray;
				PUBLIC
				property Hits[i:integer]:string read GetHit; default;
				property RawHits[i:integer]:THit read GetHitRaw;
				property Count:integer read cnt write cnt;
			end;				   

IMPLEMENTATION

(*
 *  THitList
 *
 *  Storage class for Joda Hits (i.e. result of a fulltext search)
 *
 *)
 
constructor THitList.Create(const maxhits : integer);
begin
	inherited Create;
	max := maxhits;
	count := 0;
	SetLength(hitarr, max);
end;


destructor THitList.Destroy;
begin
	SetLength(hitarr,0);
	inherited Destroy;
end;


procedure THitList.Clear;
begin
	count := 0;
	SetLength(hitarr,0);
	SetLength(hitarr,max);
end;


procedure THitList.AddWithoutFileRef(id, date, weight, info: cardinal);
begin
	if count>=max then begin
		max := max*2;
		SetLength(hitarr, max);
	end;
	hitarr[count].id      := id;
	hitarr[count].date    := date;
	hitarr[count].weight  := weight;
	hitarr[count].info    := info;
	hitarr[count].dup     := 0;
	hitarr[count].fileref := '';
	hitarr[count].title   := '';
	hitarr[count].sort    := '';
	cnt+=1
end;

procedure THitList.AddWithFileRef(fileref, title: string; id, date, weight, info: cardinal);
begin
	if count>=max then begin
		max := max*2;
		SetLength(hitarr, max);
	end;
	hitarr[count].id      := id;
	hitarr[count].date    := date;
	hitarr[count].weight  := weight;
	hitarr[count].info    := info;
	hitarr[count].dup     := 0;
	hitarr[count].fileref := fileref;
	hitarr[count].title   := title;
	hitarr[count].sort    := '';
	cnt+=1
end;

procedure THitList.AddDuplicates(dups: TDupSortList);
var
	i : integer;
begin								   
	while count+dups.count>=max do max := max*2;
	SetLength(hitarr, max);
	for i:=0 to dups.count-1 do begin
		hitarr[count].id      := dups[i].id;
		hitarr[count].date    := dups[i].date;
		hitarr[count].weight  := dups[i].weight;
		hitarr[count].info    := dups[i].info;
		hitarr[count].dup     := i;
		hitarr[count].fileref := dups[i].fileref;
		hitarr[count].title   := dups[i].title;
		hitarr[count].sort    := dups[i].sort;
		cnt+=1
	end;
end;

function THitList.GetHit(i: integer): string;
begin
	if (i<0) or (i>=count) then
		raise EListError.Create('hit index out of range');
	result:='';
	if hitarr[i].fileref<>'' then
		result:=result+hitarr[i].fileref+#9+hitarr[i].title+#9;
	result:=result+IntToStr(hitarr[i].id)+#9+IntToStr(hitarr[i].date)+#9+IntToStr(hitarr[i].weight)+#9+IntToStr(hitarr[i].info);
//	if hitarr[i].dup>0 then
	result:=result+#9+IntToStr(hitarr[i].dup);
end;

function THitList.GetHitRaw(i: integer): THit;
begin
	if (i<0) or (i>=count) then
		raise EListError.Create('hit index out of range');
	result:=hitarr[i]
end;

function THitList.GetHitRawPtr(i: integer): PHit;
begin
	if (i<0) or (i>=count) then
		result:=nil
	else
		result:=@hitarr[i];
end;

function THitList.GetHitListPtr(): PHitArray;
begin
	result:=@hitarr;
end;

(*
 *  TDupSortList
 *
 *  temporary sortable hitlist for ordered duplicate results
 *  (in case of multi issue matches)
 *
 *)
constructor TDupSortList.Create(const pattern, preferred: string); overload;
begin
	inherited Create;
	sortStr:=pattern;
	prefStr:=preferred;
	sortRe:=TRegExpr.Create;
	sortRe.Expression:=pattern;
	if preferred<>'' then begin
		prefRe:=TRegExpr.Create;
		prefRe.Expression:=preferred;
	end else
		prefRe:=nil;
	offs:=0;
	len:=0;
end;

constructor TDupSortList.Create(const offset, length: integer; preferred: string); overload;
begin
	inherited Create;
	sortStr:=IntToStr(offset)+','+IntToStr(length);
	prefStr:=preferred;
	sortRe:=nil;
	if preferred<>'' then begin
		prefRe:=TRegExpr.Create;
		prefRe.Expression:=preferred;
	end else
		prefRe:=nil;
	offs:=offset;
	len:=length;
end;

destructor TDupSortList.Destroy();
begin
	ClearList;
	sortRe.Free; sortRe:=nil;
	prefRe.Free; prefRe:=nil;
	inherited Destroy;
end;

procedure TDupSortList.ClearList();
begin
	while count>0 do begin
		dispose(PHit(items[0]));
		Delete(0);
	end;
end;

function TDupSortList.AddWithFileRef(fileref, title: string; id, date, weight, info: cardinal): integer;
var
	ahit   : PHit;
begin 
	new(ahit);
	ahit^.id      := id;
	ahit^.date    := date;
	ahit^.weight  := weight;
	ahit^.info    := info;
	ahit^.fileref := fileref;
	ahit^.title   := title;
	if sortRe<>nil then begin
		if sortRe.Exec(fileref) then
			ahit^.sort := copy(fileref,sortRe.MatchPos[0],sortRe.MatchLen[0])
		else
			ahit^.sort := fileref;
	end	else if len>0 then
		ahit^.sort := copy(fileref,offs,len)
	else
		ahit^.sort := fileref;
	result:=inherited Add(ahit);
end;

function TDupSortList.GetHit(i : integer): THit;
begin
	result:=PHit(items[i])^;
end;

function SortCompareFunc(p1, p2 : Pointer): integer;
begin 
	result:=0;
	if p1<>p2 then
		if PHit(p1)^.sort>PHit(p2)^.sort then
			result := 1
		else if PHit(p1)^.sort<PHit(p2)^.sort then
			result := -1;
end;

procedure TDupSortList.CustomSort();
var
	i, f : integer;
	h	 : PHit;
begin
	if count>1 then begin
		Sort(@SortCompareFunc);
		if prefStr<>'' then begin // one Issue should go to the top
			f := -1;
			for i:=0 to count-1 do
				if prefRe.Exec(PHit(items[i])^.fileref) then begin
					f := i;
					break;
				end;
			if (f>0) then begin // move to first pos
				h := PHit(items[f]);
				Delete(f);
				Insert(0,h);
			end;
		end;
	end;
end;

end.
