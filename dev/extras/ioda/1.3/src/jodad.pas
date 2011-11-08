Program JodaD;

{
	Volltext-Datenbank joda als TCP-Server by jo 2001-2002
	Mögliche Kommandos:
	
	c	:	close		  [datenbank]
	e  :  Erase      [datenbank wortListe id] 					Alle Worte und Datei-Eintrag als ungültig markieren
	m	:	Mischen    [datenbank zweiteDatenbank ('kill')]		'kill' für verschieben statt kopieren
	o	:	open		  [datenbank]
	q	:	Query      [datenbank query (vonDatum bisDatum Dateifilter maxTreffer sortOrder bitFilter)]
	x	:	eXt. Query [datenbank query (vonDatum bisDatum Dateifilter bitFilter maxTreffer sortOrder )]
	qu,xu: same as q,x - except that the query string is UTF-8 encoded
	S	:	SortIssues [datenbank sortIssues]						sortIssue  = issuePattern[,issuePreferred]
																				setzt ThirdLevelCheck auf Ausgabensortierung und ggf.
																				bevorzugte Ausgabe um bzw. bei Leerstring wieder zurück.
																				issuePreferred='': reine alphabetische Sortierung ohne bevorzugte Ausgabe.
	s	:	Einfügen   [datenbank wortliste (dateiname datum info id)] 	Dateiname ODER id muss vorhanden sein! (Wortliste in Datei bzw. Pipe)
	v	:	view status ([datenbank]) 												Status für alle oder eine bestimmte DB ("ready" oder "closed")
	
	Alle Parameter sind Strings. "wortliste" ist ein String durch \n getrennter Worte.
	Datumsfelder stets im Format dd.mm.yyyy
}

(* Copyright (C) 1994-2004  jo@magnus.de
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
	CMEM,Classes,SysUtils,BaseUnix,Sockets,JStrings,Volltext,ConfigReader,Logbook,IONet;
	
{$H+}

const
	version				=	'3.5';
	defaultLogbook		=	'/var/log/apps/jodad.log';
	defaultRcvLen		:	longint	=	$10000;		// aus Sicherheitsgründen: max. Transfervolumen für ankommende Jobs
	jodaDefaultSocket	: 	word		=	  3359;

	
type
	TJodaDaemon	=	class(TAppServer)
							constructor Create;
							destructor	Destroy; OVERRIDE;
						
							PROTECTED
							dbList	:	TStringList;
							appErrorMsg:string;
							ro			:	boolean;
							
							function 	Action(const msg:string):string; OVERRIDE;
							function 	FindDB(db:string; var i:integer):string;
							function 	LoadDB(const db:string):TVolltext;
							
							PUBLIC
							property		ErrorMsg:string read appErrorMsg;
						end;

const
	status		:	array[boolean] of string=('closed','ready');	

var
	log			:	TLogbook;	



procedure AlarmHandler(sig:longint; info: psiginfo; context:PSigContext); CDECL;
begin
	writeln('TIMEOUT - ABORT!'); HALT(10);
end;


constructor TJodaDaemon.Create;
var
	cfg				:	TConfigReader;
	vt					:	TVolltext;
	db,logbookFN	:	string;
	rcvLen			:	cardinal;
	i,j,res			:	integer;
	jodaSocket		: 	word;	
	
begin
	cfg:=TConfigReader.Create;
	j:=cfg.ReadCommandLine;
	logbookFN:=cfg['logbook']; if logbookFN='' then logbookFN:=defaultLogbook;
	log:=TLogbook.Create(logbookFN,1000,res);
	if (log=NIL) then begin 
		writeln('Cannot create or open log "',logbookFN,'" Error: ',res); HALT(1); 
	end;

	jodaSocket:=cfg.Int['socket']; if jodaSocket=0 then jodaSocket:=jodaDefaultSocket;
	rcvLen:=cfg.Int['rcvLen']; if rcvLen=0 then rcvLen:=defaultRcvLen;
	inherited Create(jodaSocket,rcvLen);
	ro:=not cfg.Bool['-rw'];								// ReadOnly ist Default!
	log.Add('jodad V. '+version+' is up, listening with maxRcvLen='+IntToStr(rcvLen)+' on socket '+IntToStr(jodaSocket)+', readOnly='+BoolToStr(ro),true);
	dbList:=TStringList.Create;
	
	for i:=j+1 to paramCount do begin
		db:=ChangeFileExt(paramstr(i),'');
		vt:=LoadDB(db);
		if vt<>NIL then dbList.AddObject(db,vt);
		cfg.Clear;
	end;
	cfg.Free;
	log.Add('... and running',true);
end;


destructor TJodaDaemon.Destroy;
var
	i	:	integer;
begin
	for i:=0 to dbList.Count-1 do 
		if dbList.objects[i]<>NIL then 
			with dbList.objects[i] as TVolltext do	Free;
	dbList.Free;
	inherited Destroy;
end; 


function TJodaDaemon.FindDB(db:string; var i:integer):string;
var
	j	:	integer;
begin
	if dbList.Find(db,i) then
		result:=dbList[i]											// exact match
	else begin
		db:=ExtractFilename(db);
		for j:=0 to dbList.Count-1 do
		 	if ExtractFilename(dbList[j])=db then begin
				result:=dbList[j]; i:=j; EXIT;				// match of file name w/o path
		end;
		i:=-1;
	end;
end;


function TJodaDaemon.LoadDB(const db:string):TVolltext;
var
	res		:	integer;
	openMode	:	TDbMode;
	
begin
	result:=NIL;
	try
		if ro then openMode:=readOnlyDB else openMode:=undefinedOpenMode;
		res:=1;	// Logbuch-Eintrag erzwingen
		result:=TVolltext.Create(db,openMode,res);
		if res<>0 then 
		begin
			EApp.Create('Cannot open '+db+'. ErrorCode='+IntToStr(res));
			writeln('Cannot open '+db+'. ErrorCode='+IntToStr(res));
			result.Free;
			result:=NIL;
		end else
			log.Add('Database '+db+' opened',true);
	except
		on E:EApp do 
		begin
			appError:=-1; appErrorMsg:=E.message; 
			log.Add('Error while opening database '+db+': '+E.Message,true);
			writeln('Error while opening database '+db+': '+E.Message);
		end;
	end;
end;



function TJodaDaemon.Action(const msg:string):string;
var
	vt,secunda												:	TVolltext;
	worte														:	TStringList;
	alarma													: 	TSigHandler;
	such,von,bis,fFilter,bFilter,inFile,datum,dbn:	string;
	vlBuf														:	array[0..255] of byte;
	a															:	array[0..9] of string;
	id,info													:	cardinal;
	i,n,vlCount,maxTreffer,sortOrder					:	integer;
	cmd														:	string[3];
	overflow													:	boolean;

begin
	try
		result:=''; vt:=NIL;
		for i:=0 to 9 do a[i]:='';
		n:=split(msg,#9,a);
		cmd:=copy(a[0],1,2);
		if cmd<>'v' then begin					// 'v'erbose auch ohne Parameter möglich (alle DB werden angezeigt)
			if n<1 then
			begin
				result:='Too few parameters in "'+msg+'"'; EXIT
			end;

			dbn:=FindDB(ExtractFileName(a[1]),i);
			if dbn<>'' then begin
				vt:=dbList.objects[i] as TVolltext;
				a[1]:=dbn;
			end else begin
				if cmd='o' then begin			//	'o'pen ist dann erstes open (db bei Programmstart nicht geöffnet)
					vt:=NIL; 
					dbList.AddObject(a[1],vt);	// Datenbank wird dann unten geöffnet
					FindDB(ExtractFileName(a[1]),i);
				end else	begin
					result:='Database "'+a[1]+'" is unknown here';
					EXIT;
				end;
			end;
		end else if (n>=1) then begin			// 'v'erbose für eine bestimmte db
			result:=status[FindDB(ExtractFileName(a[1]),i)<>''];
			EXIT;
		end;

		if (vt=NIL) and (cmd<>'o') and (cmd<>'v') then begin  // nur 'o'pen oder 'verbose' sind ohne db-Objekt möglich
			result:='DB is closed at the moment';
			EXIT;
		end;

		if (cmd[1]='x') or (cmd[1]='q') then begin
			if n<2 then
				result:='Too few parameters ('+IntToStr(n)+'/2) in "'+copy(msg,1,256)+'"'
			else begin
				fFilter:=''; von:=''; bis:=''; maxTreffer:=100; sortOrder:=2; info:=0; bFilter:='';
				such:=a[2];
				if (n>=3) and (a[3]<>'.') then von:=a[3];
				if (n>=4) and (a[4]<>'.') then bis:=a[4];
				if (n>=5) and (a[5]<>'.') then fFilter:=a[5];
				alarma:=TSigHandler.Create(SIGALRM,@AlarmHandler);
				fpAlarm(60);	// Laufzeitbegrenzer!

				if cmd[1]='q' then begin
					if (n>=6) and (a[6]<>'.') then maxTreffer:=StrToIntDef(a[6],100);
					if (length(cmd)=2) and (cmd[2]='u') then maxTreffer:=-maxTreffer; // flag signalizes: utf-8 decoding required
					if (n>=7) and (a[7]<>'.') then sortOrder:=StrToIntDef(a[7],2);
					if (n>=8) and (a[8]<>'.') then info:=StrToIntDef(a[8],0);
					n:=vt.Suche(such,von,bis,fFilter,maxTreffer,sortOrder,info,overflow);
				end else begin
					if (n>=6) and (a[6]<>'.') then bFilter:=a[6];			
					if (n>=7) and (a[7]<>'.') then maxTreffer:=StrToIntDef(a[7],100);
					if (length(cmd)=2) and (cmd[2]='u') then maxTreffer:=-maxTreffer; // flag signalizes: utf-8 decoding required
					if (n>=8) and (a[8]<>'.') then sortOrder:=StrToIntDef(a[8],2);
					vlCount:=0; i:=1; 
					while ((i<length(bFilter)) and (vlCount<=255)) do begin
						vlBuf[vlCount]:=Hex2Int(copy(bFilter,i,2));
						i+=2;	inc(vlCount);
					end;
					n:=vt.vlSuche(such,von,bis,fFilter,@vlBuf,vlCount,maxTreffer,sortOrder,overflow);
				end;
				fpAlarm(0);
				alarma.Free;
				result:=such;
				result:=result+#10+IntToStr(n);
				if overflow then result:=result+'+'+#10 else result:=result+#10;
				for i:=0 to n-1 do result:=result+vt[i]+#10;
			end;
		end else if (cmd='S') then begin
			if n<2 then
				result:='Too few parameters ('+IntToStr(n)+'/2) in "'+copy(msg,1,256)+'"'
			else begin
				vt.SortIssues:=a[2];
				result:=status[true]
			end;
	  	end else if (cmd='s') then begin
			if ro then 
				result:='DB is opened readOnly: ABORTED'
			else if n<6 then begin
				result:='Too few parameters ('+IntToStr(n)+'/6): ';
				for i:=0 to n do 	result:=result+IntToStr(i)+'=['+copy(a[i],1,250)+'] ';
			end else begin
				inFile:=''; datum:=''; info:=0; id:=0;
				if a[3]<>'.' then inFile:=a[3];
				if a[4]<>'.' then datum:=a[4];
				if a[5]<>'.' then info:=StrToIntDef(a[5],0);
				if a[6]<>'.' then id:=StrToIntDef(a[6],0);

				worte:=TStringlist.Create;
				worte.Sorted:=false; worte.Duplicates:=dupAccept;
				worte.SetText(@a[2][1]);
				n:=vt.InsertWords(worte,inFile,datum,'',info,id);
				if n>=0 then
					result:=IntToStr(n)
				else
					result:='Inserting failed with ErrorCode #'+IntToStr(n);
				worte.Free;
			end;
	  	end else if (cmd='e') then begin
			if ro then 
			  	result:='DB is opened readOnly: ABORTED'
			else if n<3 then begin
			  	result:='Too few parameters ('+IntToStr(n)+'/3): ';
			  	for i:=0 to n do 	result:=result+IntToStr(i)+'=['+copy(a[i],1,250)+'] ';
			end else begin
				id:=StrToIntDef(a[3],0);
			  	worte:=TStringlist.Create;
			  	worte.Sorted:=false; worte.Duplicates:=dupAccept;
			  	worte.SetText(@a[2][1]);
			  	n:=vt.InvalidateEntry(worte,id);
			  	if n<0 then result:='Invalidating Entry failed with ErrorCode #'+IntToStr(n) else result:=IntToStr(n);
			  	worte.Free;
		  end;
	  	end else if (cmd='m') then begin		// only for MEMORY (tempory) databases!
			if ro then 
				result:='DB is opened readOnly: ABORTED'
		  	else if FindDB(ExtractFileName(a[2]),i)<>'' then  begin
			  	secunda:=dbList.objects[i] as TVolltext;
			  	n:=vt.ExecMerge(secunda,'',false,false,a[3]='kill',false);
			  	if n<>0 then result:=IntToStr(n) else result:=status[true];
		  	end else
				result:='Source Database "'+a[i]+'" is unknown here';
	  	end else if (cmd='c') then begin
			vt.Free; 
			dbList.objects[i]:=NIL;
			result:=status[true]
	  	end else if (cmd='o') then begin
			if vt=NIL then	begin
				dbList.objects[i]:=LoadDB(a[1]);
			 	if dbList.objects[i]=NIL then 
					result:='Cannot open Database "'+dbn+'"'
			 	else
					result:=status[true]
		 	end else begin
				vt.ReOpenDB; result:=status[true]
		 	end;
	 	end else if (cmd='v') then begin
			result:='';
			for i:=0 to dbList.Count-1 do result:=result+dbList[i]+#9+status[dbList.objects[i]<>NIL]+#10
	 	end;

	except
		on E:Exception do 
		begin
			writeln('*** Nicht behandelter Fehler! '+E.Message+' bei cmd='+cmd);	// Fallback wg. Serverbetrieb
			result:='Exception: '+E.Message+' bei cmd='+cmd;
		end;
	end;
end;


var
	joda	:	TJodaDaemon;
begin
	if paramcount=0 then 
		writeln('jodad [database1 database2 ...]  i.e. jodad rz2003 rz2004')
	else
	begin
		DateSeparator:='.'; ShortDateFormat:='dd.mm.yyyy'; 
		joda:=TJodaDaemon.Create;
		if (joda.Error=0) and not joda.Run(true) then log.Add('jodad PANIC: too many childs or socket closed',true);
		log.Add('Jodad stopped: '+IntToStr(joda.Error),true);
		joda.Free;
		log.Free;
	end;
end.
