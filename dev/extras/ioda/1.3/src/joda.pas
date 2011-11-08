program joda;
{$H+}

(* Copyright (C) 1994-2005  jo@magnus.de
   This program is free software; you can redistribute it and/or modify it 
	under the terms of the GNU Lesser General Public License as published by
	the Free Software Foundation; either version 2.1 of the License, or 
	(at your option) any later version.

   This program is distributed in the hope that it will be useful, but 
	WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
	or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public 
	License for more details.

   You should have received a copy of the GNU Lesser General Public License 
	along with this library; if not, write to the Free Software Foundation, 
	Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA 
*)

uses
	CMEM,Classes,SysUtils,JStrings,ConfigReader,Volltext;

const
	version			=	'3.5';
type
	EConfigError	=	class(Exception);


procedure Help;
begin
	writeln('This is joda V. ',version);
	writeln('Archive incr.:           joda -a   database filepath filePattern [filedatum] {word list(s) from (many) file(s) via filter prog}');
	writeln('Erase words via filter:  joda -d   database delFile id {word list from file via filter prog}');
	writeln('Erase words via list:    joda -e   database wordListFile id {word list from file}');
	writeln('Merger/Optimizer:        joda -m   [-wordcheck] [-filecheck] [-verbose] database sourcedb [minimalDatum] ');
	writeln('Optimizer same db:       joda -opt [-wordcheck] [-filecheck] [-verbose] database [minimalDatum] ');
	writeln('Archive from pipe:       joda -p   database {word list(s) from (many) file(s) via pipe}');
	writeln('Query:                   joda -q   database searchExpression [from] [until] [fFilter] [maxTreffer] [sortOrder 1|2] [info]');
	writeln('Query extended:          joda -x   database searchExpression [from] [until] [fFilter] [bFilter] [maxTreffer] [sortOrder 1|2]');
	writeln('Store one file:          joda -s   database wordListFile (. for pipe) fileRef (. for no file reference) [filedatum] [infobitmap] [id]');
	HALT(1);
end;


var
	cfg														:	TConfigReader;
	vt,targetVT												:	TVolltext;
	db,db_,targetDB,result,inFile,wordListFile,
	such,fFilter,bFilter,von,bis 						:	string;
	vlBuf														:	array[0..255] of byte;
	id,info													:	cardinal;
	res,i,p,vlCount,sortOrder,maxTreffer			:	integer;
	overflow,doLock										:	boolean;
	openMode													:	TDbMode;

begin
	try
		if paramCount=0 then HELP;
		cfg:=TConfigReader.Create;
		if copy(paramstr(1),1,1)<>'-' then cfg.Add('-q');
		i:=cfg.ReadCommandLine+1;
		if cfg['openMode']<>'' then openMode:=TdbMode(cfg.Int['openMode']) else openMode:=undefinedOpenMode;
		db:=ChangeFileExt(paramstr(i),''); db_:=db;
		if (cfg.Bool['-opt']) then begin
			p:=pos(':',db_); 			// Sonderfall Optimize: Target ist temporäre DB, MergeDB benennt später in SourceDB um
			if p>0 then
				db:=copy(db_,1,p)	 	// Quelle ist bereits eine cloned DB: "echten" Datenbanknamen für temp. TargetDB weglassen
			else
				db:=db+':';				// Quelle besitzt eigene Config
		end;
		res:=0; doLock:=cfg.Bool['-lockdb'];
		vt:=TVolltext.Create(db,TDBMode(openMode),res);
		if res<>0 then raise EConfigError.Create('Fehler beim Öffnen der Datenbank "'+db+'" - Code='+IntToStr(res));

		if cfg.Bool['-x'] or cfg.Bool['-xu'] or cfg.Bool['-q'] or cfg.Bool['-qu'] then begin
			fFilter:=''; von:=''; bis:=''; maxTreffer:=100; sortOrder:=2; info:=0; bFilter:='';
			such:=paramstr(i+1);
			if (paramCount>=i+2) and (paramstr(i+2)<>'.') then von:=paramstr(i+2);
			if (paramCount>=i+3) and (paramstr(i+3)<>'.') then bis:=paramstr(i+3);
			if (paramCount>=i+4) and (paramstr(i+4)<>'.') then fFilter:=paramstr(i+4);
			
			if (cfg.Bool['-q']) or (cfg.Bool['-qu']) then begin		
				if (paramCount>=i+5) and (paramstr(i+5)<>'.') then maxTreffer:=StrToIntDef(paramstr(i+5),100);
				if cfg.Bool['-qu'] then maxTreffer:=-maxTreffer; // flag signalizes: utf-8 decoding required
				if (paramCount>=i+6) and (paramstr(i+6)<>'.') then sortOrder:=StrToIntDef(paramstr(i+6),2);
				if (paramCount>=i+7) and (paramstr(i+7)<>'.') then info:=StrToIntDef(paramstr(i+7),0);
				res:=vt.Suche(such,von,bis,fFilter,maxTreffer,sortOrder,info,overflow);
			end else	begin
				if (paramCount>=i+5) and (paramstr(i+5)<>'.') then bFilter:=paramstr(i+5);			
				if (paramCount>=i+6) and (paramstr(i+6)<>'.') then maxTreffer:=StrToIntDef(paramstr(i+6),100);
				if cfg.Bool['-xu'] then maxTreffer:=-maxTreffer; // flag signalizes: utf-8 decoding required
				if (paramCount>=i+7) and (paramstr(i+7)<>'.') then sortOrder:=StrToIntDef(paramstr(i+7),2);
				vlCount:=0; i:=1; 
					while ((i<length(bFilter)) and (vlCount<=255)) do begin
					vlBuf[vlCount]:=Hex2Int(copy(bFilter,i,2));
					i+=2;	inc(vlCount);
				end;
				res:=vt.vlSuche(such,von,bis,fFilter,@vlBuf,vlCount,maxTreffer,sortOrder,overflow);
			end;
			result:=such+#10+IntToStr(res);
			if overflow then result:=result+'+'+#10 else result:=result+#10;
			if (not cfg.Bool['-count']) then for i:=0 to res-1 do result:=result+vt[i]+#10;
			writeln(result);
		end else if cfg.Bool['-a'] then begin
			if (paramCount>=i+3) and (paramstr(i+3)<>'.') then von:=paramstr(i+3) else von:='';	// Datum
			if doLock then vt.SetLockFile(true);
			vt.InsertWordsFromFiles(paramstr(i+1),paramstr(i+2),von);
			if doLock then vt.SetLockFile(false);
			if cfg.Bool['-m'] then begin
				targetDB:=ChangeFileExt(paramstr(i+4),'');
				targetVT:=TVolltext.Create(targetDB,ReadWriteDB,res);
				if res<>0 then raise EConfigError.Create('Fehler beim Öffnen der Zieldatenbank "'+targetDB+'" - Code='+IntToStr(res));
				if doLock then targetVT.SetLockFile(true);
				targetVT.ExecMerge(vt,cfg['startDatum'],cfg.Bool['-wordcheck'],cfg.Bool['-filecheck'],cfg.Bool['-kill'],cfg.Bool['-verbose']);
				targetVT.Free;
			end;
		end else if cfg.Bool['-s'] then begin
			inFile:=''; wordListFile:=''; von:=''; info:=0; id:=0;
			if (paramCount>=i+1) and (paramstr(i+1)<>'.') then wordListFile:=paramstr(i+1);
			if (paramCount>=i+2) and (paramstr(i+2)<>'.') then inFile:=paramstr(i+2);
			if (paramCount>=i+3) and (paramstr(i+3)<>'.') then von:=paramstr(i+3);	// Datum
			if (paramCount>=i+4) and (paramstr(i+4)<>'.') then info:=StrToIntDef(paramstr(i+4),0);
			if (paramCount>=i+5) and (paramstr(i+5)<>'.') then id:=StrToIntDef(paramstr(i+5),0);
			if doLock then vt.SetLockFile(true);
			vt.InsertWordsFromFile(wordListFile,inFile,von,info,id);
		end else if cfg.Bool['-e'] then begin
			if (paramCount>=i+1) and (paramstr(i+1)<>'.') then wordListFile:=paramstr(i+1);
			if (paramCount>=i+2) and (paramstr(i+2)<>'.') then id:=StrToInt(paramstr(i+2));
			if doLock then vt.SetLockFile(true);
			vt.InvalidateEntryFromFile(wordListFile,id);
		end else	if cfg.Bool['-d'] then begin
			if (paramCount>=i+1) and (paramstr(i+1)<>'.') then inFile:=paramstr(i+1);
			if (paramCount>=i+2) and (paramstr(i+2)<>'.') then id:=StrToInt(paramstr(i+2));
			if doLock then vt.SetLockFile(true);
			vt.InvalidateEntryFromProgram(inFile,id);
		end else	if cfg.Bool['-p'] then begin
			if doLock then vt.SetLockFile(true);
			vt.InsertWordsFromPipe()
		end else if cfg.Bool['-m'] then begin
			if doLock then vt.SetLockFile(true);
			vt.MergeDB(paramstr(i+1),paramstr(i+2),cfg.Bool['-wordcheck'],cfg.Bool['-filecheck'],cfg.Bool['-verbose'])
		end else if cfg.Bool['-opt'] then begin
			if doLock then vt.SetLockFile(true);
			vt.MergeDB(db_,paramstr(i+1),cfg.Bool['-wordcheck'],cfg.Bool['-filecheck'],cfg.Bool['-verbose'])
		end else 
			Help;

	except
		on E:EConfigError do writeln(E.message)
	end;	
	try
		vt.Free;
		cfg.Free;
	except
		on E:Exception do ;
	end;
end.
