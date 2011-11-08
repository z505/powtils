unit DirScanner;
{$H+}

(*
	Stellt den TDirScanner zum rekursiven Datei scannen zur Verfügung.
	Dieser liefert mit 'Finde' eine Liste vom Type TFileList.
	
	TFileList ist ein Nachkomme von TStringlist, der als Default-Eigenschaft
	statt der üblichen Strings das TSearchRec seiner TSearchObj-Objekte liefert.
	
	Benutzt in die Klasse TRegExpr verpackte RegExMaschinen, die noch nicht
	ausgereift sind: ^ $ funktionieren nicht + und * nur teilweise.
*)

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

INTERFACE
uses
	Classes,SysUtils,RegExpr,JStrings{$IFDEF LINUX},BaseUnix,UnixUtil,Unix,Globs{$ENDIF};
		
type
	TSearchObj	=	class
							PUBLIC
							constructor Create(const srec:TSearchRec);
							destructor  Destroy; OVERRIDE;
							
							PROTECTED
							mySrec	:	TSearchRec;

							PUBLIC
							property		SearchRec:TSearchRec Read mySrec;
						end;
							
	TFileList	=	class(TStringList)
							PUBLIC
							constructor Create;
							destructor  Destroy; OVERRIDE;
							procedure   AddSearchRec(const name:string; const srec:TSearchRec);
							
							PROTECTED
							procedure   Clear; OVERRIDE;
							function		GetSearchRec(i:integer):TSearchRec;
							
							PUBLIC
							property		SearchRec[i:integer]:TSearchRec read GetSearchRec; DEFAULT;
						end;
						
	TDirScanner	=	class
							constructor	Create(const unfileName:string);
							destructor	Destroy; OVERRIDE;
							function 	Finde(path,searchPattern:string; tiefe:integer):integer;
							
							PROTECTED
							fileliste:	TFileList;
							unfiles	:	TStringList;
							suspendFile:string;
							pattern	:	{$IFDEF LINUX} string {$ELSE}	TRegExpr {$ENDIF};
							maxR		:	integer;

							procedure 	Travel(const path:string; r:integer);
							function 	NameOK(const s:string):boolean;
							function		GetSearchrec(i:integer):TSearchRec;
							function 	GetString(i:integer):string;
							function 	GetCount:integer;	
							
							PUBLIC
							property		SearchRec[i:integer]:TSearchRec read GetSearchRec; DEFAULT;
							property		Strings[i:integer]:string read GetString;
							property		Count:integer read GetCount;
							property		LockFile:string read suspendFile write suspendFile;
						end;
						
						
IMPLEMENTATION

// Speicher-Objekt für eine Datei

constructor TSearchObj.Create(const srec:TSearchRec);
begin
	inherited Create;
	mySrec:=srec
end;


destructor TSearchObj.Destroy; 
begin
	inherited Destroy
end;


// TStringList speziell für fileliste von TSearchRec

constructor TFileList.Create;
begin
	inherited Create; 
	sorted:=true; duplicates:=dupAccept;
end;


destructor TFileList.Destroy;
begin
	Clear;
	inherited Destroy
end;


procedure TFileList.AddSearchRec(const name:string; const srec:TSearchRec);
begin
	AddObject(name,TSearchObj.Create(srec));
end;


procedure TFileList.Clear;
var
	i	:	integer;
begin
	for i:=0 to count-1 do objects[i].Free;
	inherited Clear;
end;


function	TFileList.GetSearchRec(i:integer):TSearchRec;
begin
	result:=TSearchObj(objects[i]).SearchRec
end;


// Rekursiver Verzeichnisscanner

constructor TDirScanner.Create(const unfileName:string);
var
	i	:	integer;
begin
	inherited Create;
	fileliste:=TFilelist.Create;
	unfiles:=TStringlist.Create;
	unfiles.Sorted:=true; unfiles.Duplicates:=dupIgnore;
	if (unfileName<>'') and FileExists(unfileName) then begin
		unfiles.LoadFromFile(unfileName);
		for i:=0 to unfiles.Count-1 do 
			if (pos('REGEX=',unfiles[i])=1) or (pos('REGEX_',unfiles[i])=1) then begin
				unfiles.objects[i]:=TRegExpr.Create;
				TRegExpr(unfiles.objects[i]).Expression:=copy(unfiles[i],7,256);
			end;
	end;
end;


destructor TDirScanner.Destroy;
var
	i	:	integer;
begin
	for i:=0 to unfiles.Count-1 do unfiles.objects[i].Free;
	unfiles.Free;
	fileliste.Free;
	inherited Destroy
end;


function TDirScanner.NameOK(const s:string):boolean;
var
   i	:	integer;
begin
	result:=false;
	for i:=0 to unfiles.Count-1 do begin
		if unfiles.objects[i]=NIL then begin
			if pos(unfiles[i],s)>0 then EXIT
		end else if TRegExpr(unfiles.objects[i]).Exec(s) then 
			EXIT
	end;
	result:=true;
end;


procedure TDirScanner.Travel(const path:string; r:integer);
var
	srec		:	TSearchRec;
	e			:	integer;
	name,
	shortname:	string;
{$IFDEF LINUX}
	gNode,g	:	PGlob;
	info		:	Stat;
	fliste	:	TStringList;
	year,month,
	day,x		:	word;
	
begin
	gNode:=NIL;
	gNode:=Glob(path+'/*'); 
	if gNode=NIL then EXIT;
	fliste:=TStringList.Create; 
	g:=gNode;
	while g<>NIL do begin // es kann nur ein "GLOB" existieren => fliste als Zwischenspeicher
		fliste.Add(strPas(g^.name));
		g:=g^.next;	
	end;
	GlobFree(gNode);
  	for e:=0 to fliste.Count-1 do begin
		shortname:=fliste[e]; name:=path+'/'+shortname;
		if (shortname<>'.') and (shortname<>'..') and NameOK(name) then begin
			fpStat(name,info);
      	if fpS_ISDIR(info.mode) then begin
	    		if (r<maxR) and ((suspendFile='') or (not FileExists(name+'/'+suspendFile))) then Travel(name,r+1)
       	end else if fnmatch(pattern,shortname) then begin
				srec.name:=name;
				srec.attr:=info.mode;
				EpochToLocal(info.mtime,year,month,day,x,x,x);
				srec.time:=DateTimeToFileDate(EncodeDate(year,month,day));
			 	fileliste.AddSearchRec(shortname,srec);
			end;
		end;
		setlength(name,0); setlength(shortname,0);
	end;
	fliste.Free;
{$ELSE}
begin
	e:=FindFirst(path+'/*',255,srec);
  	while e=0 do begin
		shortname:=srec.name; name:=path+'/'+shortname; 
		if (srec.name[1]<>'.') and (NameOK(name)) then begin
      	if srec.attr and faDirectory<>0 then begin
	    		if (r<maxR) and ((suspendFile='') or (not FileExists(name+'/'+suspendFile))) then Travel(name,r+1)
       	end else if pattern.Exec(srec.name) then begin
				srec.name:=name;
			 	fileliste.AddSearchRec(shortname,srec);
			end;
		end;
	  	e:=FindNext(srec);
	end;
  	FindClose(srec);
{$ENDIF}
end;


function TDirScanner.Finde(path,searchPattern:string; tiefe:integer):integer;
begin
	if path[length(path)]='/' then path:=copy(path,1,length(path)-1);
	maxR:=tiefe; 
	fileliste.Clear;
{$IFDEF LINUX}
	pattern:=searchPattern;
	Travel(path,0);
{$ELSE}
	searchPattern:=StrStrSubst(searchPattern,'*','\w*',false);
	pattern:=TRegExpr.Create;
	pattern.Expression:=searchPattern;
	Travel(path,0);
  	pattern.Free;
{$ENDIF}
	result:=fileliste.Count;
end;


function TDirScanner.GetSearchRec(i:integer):TSearchRec;
begin
	result:=fileliste[i];
end;


function TDirScanner.GetString(i:integer):string;
begin
	result:=fileliste.strings[i];
end;


function TDirScanner.GetCount:integer;
begin
	result:=fileliste.Count;
end;


end.
