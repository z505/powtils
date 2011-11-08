unit Volltext;
{
	Das Unit Volltext verbindet die Module Bayerbaum (bayBaum), Vorkommensliste (occTable) 
	und Dateiliste (fileRef) zu einer Datenbank. Es nutzt einen Parser (syntaxParser), der
	Suchanfragen zerlegen kann, die die Teilausdrücke gewichtet und die Suche triggert.
	Die Datenbank selbst kann Wortlisten archivieren - Format: wort[,info]. 
	
	Sie kann auch einzelne Dateien hinzufügen oder ganze Verzeichnisbäume abscannen. In beiden 
	Fällen wird ein externen Filter zur Erzeugung der Wortliste benutzt, der in einer
	beliebigen Programmiersprache verfasst sein kann. Unter Linux erfolgt die Kommunikation per
	Pipe, alternativ auch per temporärer Datei.
		
	Mit der Methode MergeDB kann eine zweite Datenbank zur primären hinzugefügt werden. Mit dieser
	Funktion kann auch eine Datenbank optimiert werden, indem mit TVolltext eine neue DB
	angelegt und die bestehende hinzu "merged" wird. Dabei werden Cluster und fileRef-Dateien
	optimal neu aufgebaut, weil bei beiden Dateien zusammengehörige Blöcke zusammengefasst werden.

	Fehlernummern 600-699
	
	jo 7/2001    Portiert, komplett überarbeitet und verbessert aus dem Volltextarchiv von 1995 (Delphi 1)
	ograf 9/2002 Erweitert um vlSuche (sequentielle Untersuchung mehrere Infobyte-Bedingungen
	jo 10/2002   Suche als Interface zur vlSuche beibehalten, Suchfunktionen aufgeräumt und zusammengefasst 
	jo 11/2002   Kompatibilität zu Alt-(Win-)Archiven aufgehoben; alle Archive wurden inzwischen neu aufgebaut.
	jo 12/2002   Reguläre Ausdrücke beim FileFilter erlaubt
   jo 01/2003   Doublettenerkennung per MD5 (statt Dateinamen-Gleichheit) und Doublettensortierung (PreferredIssue) eingebaut
	jo 11/2003   Löschen von Einträgen möglich / tmpFile und execProg aus allen Funktionsparametern gestrichen (jetzt aus cfg)
	jo 01/2004   Doubletten jetzt dreistufig: 0 normal archivieren, 1: mit Erstobjekt verketten, 2: ignorieren
	jo 05/2004   Höhere Effizienz: occtable (und Schnittstelle dazu) komplett neu geschrieben, idList geringfügig erweitert
	jo 09/2004   Released under LGPL
	jo 01/2005   New BTree format: Flexible String length up to 243 Chars, smaller data, faster code.
					 joda is now UTF-8 compatible!
	jo 06/2005	 Substring and RegEx Search implemented. Optimization made for repeated similar search items.
}

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

{$H+}
INTERFACE
uses
	Classes,Dos{$IFDEF LINUX},BaseUnix,Unix{$ENDIF},SysUtils,
	JStrings,IdList,BtreeFlex,OccTable,fileRefs,SyntaxParser,DirScanner,
	Logbook,ConfigReader,RegExpr,MD5,Unicode,HitList;
	
const
	sortByWeight 		 = 1;
	sortByAge			 = 2;
	sortByInfoByWeight = 3;
	day95		 	 		 = 34700; 	// Differenz-Tage zwischen 1900 (TDateTime=0) und 1995 (occTable.date=0)
	btMaxHits:cardinal = $10000;	// max. Trefferanzahl im Bayerbaum
	maxHits : cardinal = $100000; // max. Trefferanzahl in der Zwischenliste bei der Suche
	
type
	TDbMode		=	(ReadWriteDB=0,ReadOnlyDB=1,ReadOnlyDBNoCache=2,undefinedOpenMode=255);

// Klassenfunktionen:
	TThirdLevelCheck=function (id,datum,gewicht,info:cardinal):boolean of object;
	TOptWordFunc=	function (	const wort:string; var key,optimized,checked:cardinal; 
										const idList:TDWList; minAge:word):integer of object;

// Volltext-Klasse:
	TVolltext =	class
						constructor Create(const db:string; mode:TDbMode; var res:integer);
						destructor 	Destroy; OVERRIDE;
// Suchmethoden
						function Suche(var such:string; const von,bis,fileFilter:string; maxTreffer,sortOrder:integer; bitFilter:byte; var overflow:boolean):integer;
									// byteFilter für Bittests kann, je nach Occtype, ein oder drei byte sein, weiteres siehe "BTest"
									// maxTreffer<0: Flag for UTF-8 decoding required, maxTreffer:=abs(maxTreffer)
						function vlSuche(var such : string; const von,bis,fileFilter:string; bitFilter: PByte; bitFilterNum,maxTreffer,sortOrder:integer; var overflow:boolean):integer;
									// mehrere Bittests möglich, siehe unter "BTest"
									// maxTreffer<0: Flag for UTF-8 decoding required, maxTreffer:=abs(maxTreffer)
// Einfüge-Methoden
                  function ChainDuplicate(fname:string; lastID:cardinal; var id:cardinal): integer;
						         // externe schnittstelle zum verketten von doubletten
						         // wird über libjodafulltext exportiert und vom python interface verwendet

                  function InsertWords(worte:TStringList; fname,datum,md5Sum:string; info: cardinal; var id:cardinal):integer; 
									// Insert-Schnittstelle zu aufrufenden *Pascal*-Programmen und intern benutzt
									// datum darf leer sein, dann wird das Tagesdatum eingesetzt
									
						function InsertWordsFromFile(const wordListFile,inFile,datum:string; info:cardinal; var id:cardinal):integer;
									// Insert-Schnittstelle zu bel. Programmen, fertige Worteliste (ein Wort/Zeile) via Datei oder Pipe
									// datum darf leer sein, dann wird das Tagesdatum eingesetzt
									// "inFile" darf leer sein, wenn keine Filereferenz benutzt wird, sonst der echte Dateinamen
									
						function InsertWordsFromProgram(inFile,datum:string; var id:cardinal):integer; 
									// Insert-Schnittstelle zu bel. Programmen, Datei (txt, html u.a.) wird über cfg['execProg'] geparst und indiziert
									// datum darf leer sein, dann wird das Tagesdatum eingesetzt

						function InsertWordsFromFiles(path,pattern,datum:string):integer; 
									// Insert-Schnittstelle zu bel. Programmen, wie vor, jedoch für ganze Verzeichnisbäume
									// kann Doubletten über MD5 erkennen und verketten (1) oder ignorieren (2)
									// datum darf leer sein, dann wird das Dateidatum eingesetzt

						function	InsertWordsFromPipe():integer;
									// Spezialfunktion von Oli G. zum Einfügen vieler Wortlisten aus versch. Dateien per Pipe (-> E-Paper Graz)
									
// Optimierungs- und DB-Misch-Methoden:
						function	MergeDB(const sourceDB,startDatum:string; wordCheck,fileCheck,verbose:boolean):integer; 
									// mischt sourceDB zu db. Kann auch zur Optimierung von Cluster (.occ) und fileRef (.ref) benutzt werden,
									// indem eine die Ziel-Instanz (db) leer angelegt und die Quelle (secondDB) die bestehende DB ist.

						function ExecMerge(source:TVolltext; const startDatum:string; wordCheck,fileCheck,destructive,verbose:boolean):integer;
									// nur für aufrufende Pascal-Programme:
									// kann statt MergeDB aufgerufen werden, wenn Source-VT-Instanz selbst verwaltet wird (z.B. Server "jodad")
									// destructive löscht die Source-DB nach erfolgreichem Merge

//	Service-Methoden für andere Pascal-Programme:
						function ReOpenDB:integer;
									// Datenbank wird neu initialisiert
									
						function	GetResultList(var list:TStringList):integer;
									// gibt alle Suchergebnisse in einer TStringList heraus (list darf NIL sein und wird dann angelegt)
									
						procedure Clear;
									// Reine Memory-Datenbanken werden gelöscht (für temporäre DB in jodad)

// Lösch-Methoden:									
						function	InvalidateEntry(worte:TStringList; id:cardinal):integer;
									// Lösch-Schnittstelle für Pascal-Programme
									// Ein Eintrag, ggf. mitsamt allen Doubletten, wird als ungültig markiert:
									// Occtable: Eintrag wird gelöscht, Platz kann wiederverwendet werden
									// FileRef:  Eintrag wird geleert, Speicherplatz bleibt belegt (=> MergeDB verwenden)
									
						function InvalidateEntryFromFile(const wordListFile:string; id:cardinal):integer;
									// Lösch-Schnittstelle zu bel. Programmen, fertige Worteliste (ein Wort/Zeile) via Datei oder Pipe

						function InvalidateEntryFromProgram(inFile:string; id:cardinal):integer;
									// Lösch-Schnittstelle zu bel. Programmen, Datei (txt, html u.a.) wird über cfg['execProg'] geparst und Worte gelöscht
									// tmpFile sollte bei Linux leer sein (=Pipe)
						// s=issuePattern[,issuePreferred]: setzt ThirdLevelCheck auf Ausgabensortierung 
						// und ggf. auf bevorzugte Ausgabe um bzw. bei Leerstring wieder zurück.
						// issuePreferred='': reine alphabetische Sortierung ohne bevorzugte Ausgabe.
						// BOTH are regexes!
						procedure   SetSortedResults(const s:string);	// jo 31.1.2003
						// added by ograf 28.10.2004
						//
						// better SetSortedResults interfaces without parsing
						// and added version without regex but
						// offset and length
						//
						procedure   SetSortedResults(const pattern, preferred: string);
						procedure   SetSortedResults(const offset, length: integer; preferred: string);
						procedure   SetLockFile(createFile:boolean);		// creates (true) or erases a lockFile (.LOCK)

						PROTECTED
						treffer			:	THitList;
						tmpdup			:	TDupSortList;
						unworte,
						flList,
						btResList		:	TStringList;
						btree				:	TBaybaum;
						occ				:	TClusterMaster;
						fileRef			:	TfileRef;
						secunda			:	TVolltext;
						log				:	TLogbook;
						reg				:	TRegExpr;
						ThirdLevelCheck:	TThirdLevelCheck;
						fileList			:	THash;
						utf8Decoder		:	TUTF8Decoder;
						issueOffset,
						issueLength,
						extSearch		:	integer;
						issuePattern,
						issuePreferred,
						dbCfg,
						dbName,
						basePathName,
						fFilter,
						tmpFile,
						execProg,
						lastQuery		: 	string;
						vlbFilterNum,
						rres,
						doubletten		:	integer;
						vlbFilter		:	PByte;
						maxFind,
						btreeCapacity,
						btreeGrowSize,
						occCapacity,
						occGrowSize,
						refCapacity,
						refGrowSize		:	longint;
						globalInfoBits	:	cardinal;
						dStart,
						dSTop				:	word;
						mapMode,
						vtError,
						maxNums,
						opeNed,
						useBigOccList_,
						occSize,
						useMemBtree,
						useFileRef,
						flPass	 		:	integer;
						mapFactor		:	real;
						ro,
						recycling,
						fFilterFilter,
						useLog,
						useWordFilter,
						followLinks,
						caseSensitive,
						tempDB,
						clonedDB,
						dbLocked			:	boolean;

						function    GetSortedResultMode:string;
						function		TellError:integer;
						function 	Age95(const datum:string):longint;
						function 	Wortzeichen(const s:string):boolean;
						procedure 	PrepareSearch(var parsEl:TParsEl);
						procedure   FirstLevelCheck(var parsEl:TParsEl; idList:TDWList; diff:longint; operation:TOp);
						procedure 	SecondLevelCheck(idL,ocL,curL:TDWList; diff:longint; operation:TOp);
						function 	ThirdLevelCheck_WithSortedFileRef(id,datum,gewicht,info:cardinal):boolean;
						function 	ThirdLevelCheck_WithFileRef(id,datum,gewicht,info:cardinal):boolean;
						function 	ThirdLevelCheck_WithoutFileRef(id,datum,gewicht,info:cardinal):boolean;
						function 	BTest(info:cardinal):boolean;
						function		GetTreffer(i:integer):string;
						function		GetTrefferPtr(i	: integer):PHit;
						function		GetAllTrefferPtr():PHitArray;
						function		GetTrefferCount():integer;
						function 	CountC(count,wcount:cardinal):cardinal;
						function 	InsertWord(s : string; fid,fpos,info,wcount:cardinal; age:word):integer; 
						function 	GetWordsFromProgram(inFile:string; var worte:TStringList):integer;

						function		ScanCluster_WithFileRef(const wort:string; var key,optimized,checked:cardinal; 
																	   const idList:TDWList; minAge:word):integer;
						function		ScanCluster_WithoutFileRef(const wort:string; var key,optimized,checked:cardinal; 
																			const idList:TDWList; minAge:word):integer;
						function 	InvalidateWord(s:string; fid:cardinal):integer;	// Löschen einzelner Worte
						
						PUBLIC
						property		Suchergebnis[i:integer]:string read GetTreffer; DEFAULT;
						property		HitPtr[i:integer]:PHit read GetTrefferPtr;
						property		HitArrayPtr	: PHitArray read GetAllTrefferPtr;
						property		HitCount:integer read getTrefferCount;
						property    SortIssues	: string read GetSortedResultMode write SetSortedResults;
						property		Error		   : integer read TellError;
						property		MaxNumsPerWord : integer read maxNums write maxNums;
						property		BasePath	   : string read basePathName write basePathName;
						property    DatabaseName: string read dbName;
						property    DatabaseConfigName: string read dbCfg;
						property		Query:string read lastQuery;
					end;						   


IMPLEMENTATION
const
	maxTitelLen	=	1024;
	nonos :	set of char	= ['.',#44,';','!','?','=','"',#39,'/',':','-','`','*','+','\','<','>','|','(',')','[',']','{','}','&','÷'];	
	zahlen:	set of char = ['0'..'9'];
	overStr: array[boolean] of string = ('',' OVERFLOW');
	defaultMaxNums =	10;


// Volltext-Klassenbester
constructor TVolltext.Create(const db:string; mode:TDbMode; var res:integer);
var
	cfg			:	TConfigReader;
	st				:	TSystemTime;
	p				:	integer;
	logbookFN	:	string;
	
begin
	inherited Create;
	tempDB:=false; dbLocked:=false;
	cfg:=TConfigReader.Create;
	p:=pos(':',db);
	if p>0 then begin
		dbCfg:=copy(db,1,p-1);
		if length(db)>p then
			dbName:=ExtractFilePath(dbCfg)+copy(db,p+1,255)
		else begin
			DateTimeToSystemTime(now,st);
			with st do dbName:=dbCfg+Format('_%0.2d%0.2d%0.2d%0.2d%0.2d%0.2d%0.3d',[year-2000,month,day,hour,minute,second,millisecond]);
			tempDB:=true;
		end;
		clonedDB:=true;
	end else begin
		dbName:=db;
		dbCfg:=db;
		clonedDB:=false;
	end;
	
	res:=cfg.ReadConfigFile(dbCfg);
	if res<=0 then begin  
		writeln('Cannot open ConfigFile '+dbCfg);	
		res:=600; cfg.Free; EXIT;
	end;

	if mode=undefinedOpenMode then mode:=TDBMode(cfg.Int['openMode']);
	useLog 			:= res<>0;				// Flag für "Logbuch-Eintrag"
	res				:= 0;
	vtError  		:= 0;
	ro					:=	mode<>ReadWriteDB; 
	maxNums			:=	defaultMaxNums;
	mapMode  		:= cfg.Int['occMapMode'];
	mapFactor		:= 0.5;					// Default: 50% der bisherigen Vorkommen eines Wortes reservieren
	useMemBtree		:= cfg.Int['useMemBtree'];
	extSearch		:= cfg.Int['extendedSearch'];
	btreeCapacity	:= cfg.Int['btreeCapacity'];
	btreeGrowSize	:= cfg.Int['btreeGrowSize'];
	useBigOccList_	:= cfg.Int['useBigOccList']; occSize:=abs(useBigOccList_);
	occCapacity		:= cfg.Int['occCapacity'];
	occGrowSize		:= cfg.Int['occGrowSize'];
	useFileRef		:= cfg.Int['useFileref'];
	refCapacity		:= cfg.Int['refCapacity'];
	refGrowSize		:= cfg.Int['refGrowSize'];
	recycling		:= cfg.Int['enableRecycling']=1;
	followLinks		:=	cfg.Int['followLinks']=1;
	caseSensitive	:=	cfg.Int['caseSensitive']=1;
	globalInfoBits := cardinal(not cfg.Int['localInfoBits']); // Default ist $FFFFFFFF (alle InfoBits wirken Synonym-global)
	doubletten		:= cfg.Int['doubletten'];
	opeNed			:=	0;
	
	if mode=ReadOnlyDBNoCache then begin
		mode:=ReadOnlyDB;						//  nur vorsichtshalber 
		useMemBtree:=0;
		useBigOccList_:=occSize;
		if useFileRef>0 then useFileRef:=1;
	end;

	fileRef:=NIL; occ:=NIL; btree:=NIL; treffer:=NIL; tmpdup:=NIL; fileList:=NIL;
	unworte:=NIL; log:=NIL; reg:=NIL; utf8Decoder:=NIL; rres:=0; flPass:=0;
	DateSeparator:='.'; ShortDateFormat:='dd.mm.yyyy'; 
	logbookFN:=cfg['logbook']; if logbookFN='' then logbookFN:=dbCfg+'.log';
	log:=TLogbook.Create(logbookFN,10000,res);

	if res<>0 then begin  
		res:=613; EXIT;
	end;
	if mapMode>0 then	mapFactor:=1/mapMode;	// andere Modi => InsertWords und InsertWordsFromFiles

	tmpFile:=cfg['tmpfile']; 
	execProg:=cfg['execprog'];
	basePathName:=cfg['basePath'];
	useWordFilter:=cfg.Int['useNoWordFilter']<>1;

	unworte:=TStringlist.Create;
	unworte.Sorted:=true; unworte.Duplicates:=dupIgnore;
	if FileExists(dbCfg+'.unworte.txt') then unworte.LoadFromFile(dbCfg+'.unworte.txt');

	issuePattern:=''; issuePreferred:='';
	issueOffset:=0; issueLength:=0;
	if useFileRef>0 then
		ThirdLevelCheck:=@ThirdLevelCheck_WithFileRef
	else
		ThirdLevelCheck:=@ThirdLevelCheck_WithoutFileRef;

	if cfg['charsettable']<>'' then begin
		try
			utf8Decoder:=T8BitUTF8Decoder.Create(cfg['charsettable']);
		except
			on E:Exception do begin utf8Decoder.Free; utf8Decoder:=NIL; end;
		end;
		if utf8Decoder=NIL then log.Add('Cannot load "'+cfg['charsettable']+'"',true);
	end;
	cfg.Free;
	flList:=TStringList.Create; flList.sorted:=true;
	btResList:=TStringList.Create; btResList.sorted:=true;
	res:=ReOpenDB;
end;


destructor TVolltext.Destroy;
begin
	utf8Decoder.Free;
	reg.Free;
	fileRef.Free;
	occ.Free;
	btree.Free;
	fileList.Free;
	treffer.Free;
	tmpdup.Free;
	unworte.Free;
	flList.Free;
	btResList.Free;
	if tempDB then begin
		DeleteFile(dbName+'.btf');
		DeleteFile(dbName+'.ocl');
		if useFileRef>0 then DeleteFile(dbName+'.ref');
		if useLog then log.Add('Temporary Database was deleted. Thank you for using joda!',true);		
	end else
		if useLog then log.Add('Database is now closed. Thank you for using joda!',true);
	log.Free;
	if dbLocked then SetLockFile(false);
	inherited Destroy;
end;


function TVolltext.ReOpenDB:integer;
begin
	fileRef.Free;
	occ.Free;
	btree.Free;
	inc(opeNed);
	result:=-1;
	try
		if useMemBtree<>0 then
			btree:=TMemBaybaum.Create(dbName,btreeCapacity,btreeGrowSize,ro,result)
		else
			btree:=TFileBaybaum.Create(dbName,ro,result);
		if result=0 then begin  
			occ:=TClusterMaster.Open(dbName,useBigOccList_,occCapacity,occGrowSize,ro,recycling,result);
			if (result=0) and (useFileRef>0) then begin  
				if useFileRef=2 then
					fileRef:=TMemFileRef.Create(dbName,refCapacity,refGrowSize,ro,result)
				else
					fileRef:=TDynamicFileRef.Create(dbName,ro,result);
			end;
		end;
	except
		on E:Exception do result:=-616
	end;
	
	if result=0 then begin  
		if useLog then log.Add('*** Database is now open ('+IntToStr(opeNed)+'. time) ***',true)
	end else
		log.Add('*** Database '+dbname+' FAILED to open - Error #'+IntToStr(result)+' ***',true);
end;


procedure TVolltext.Clear;
begin
	btree.Clear; occ.Clear;
	if useFileRef>0 then fileRef.Clear; 
end;


procedure TVolltext.SetSortedResults(const s:string);
var
	a : array[0..1] of string;
	n : integer;
begin
	if useFileRef=0 then EXIT;
	issuePattern:='';
	issuePreferred:='';
	issueOffset:=0;
	issueLength:=0;
	if s<>'' then begin
		n:=split(s,',',a);
		issuePattern:=a[0];
		if (n>0) then issuePreferred:=a[1];
		ThirdLevelCheck:=@ThirdLevelCheck_WithSortedFileRef;
	end else
		ThirdLevelCheck:=@ThirdLevelCheck_WithFileRef;
end;


procedure TVolltext.SetSortedResults(const pattern, preferred: string); overload;
begin
	if useFileRef=0 then EXIT;
	issuePattern:= '';
	issuePreferred:='';
	issueOffset:=0;
	issueLength:=0;
	if pattern<>'' then begin
		issuePattern := pattern;
		issuePreferred := preferred;
		ThirdLevelCheck:=@ThirdLevelCheck_WithSortedFileRef;
	end else
		ThirdLevelCheck:=@ThirdLevelCheck_WithFileRef;
end;


procedure TVolltext.SetSortedResults(const offset, length: integer; preferred: string); overload;
begin
	if useFileRef=0 then EXIT;
	issuePattern:= '';
	issuePreferred:='';
	issueOffset:=0;
	issueLength:=0;
	if (length>0) and (offset>0) then begin
		issueOffset:=offset;
		issueLength:=length;
		issuePreferred:=preferred;
		ThirdLevelCheck:=@ThirdLevelCheck_WithSortedFileRef;
	end else
		ThirdLevelCheck:=@ThirdLevelCheck_WithFileRef;
end;


function TVolltext.GetSortedResultMode:string;
begin
	result:=issuePattern+','+issuePreferred;
end;


function TVolltext.TellError:integer;
begin
	result:=vtError; vtError:=0;
end;


function TVolltext.Age95(const datum:string):longint;
var
	d	:	TDateTime;
begin
	if (datum='') or (datum='TODAY') then 
		result:=trunc(Date)-day95
	else if (datum='YESTERDAY') then
		result:=trunc(Date)-day95-1
	else if (datum='TOMORROW') then
		result:=trunc(Date)-day95+1
	else if (copy(datum,1,1)='I') then
		result:=StrToIntDef(copy(datum,2,length(datum)-1),-1)
	else
		try
			d:=StrToDate(datum);
			result:=trunc(d)-day95;		// Tage seit dem 1.1.1995
		except
			on EConvertError do result:=-1
		end;
end;


function TVolltext.Wortzeichen(const s:string):boolean;
var
	i,nums	:	integer;
begin
	result:=false; 
	if s='' then EXIT;
	nums:=0;

	if s[1]='ß' then EXIT;
	for i:=1 to length(s) do 
	begin
		if s[i] in nonos then EXIT;
		if (s[i]='%') and (i<length(s)) and (s[i+1] in zahlen) then EXIT;
		if (s[i] in zahlen) then begin  
			if nums>maxNums then EXIT;
			inc(nums); 
		end;
	end;
	
	for i:=1 to length(s) do
		case s[i] of
			'a'..'z',
			'A'..'Z',
			'0'..'9'	:	begin 
								result:=true; EXIT; 
							end;
		end;
end;


function TVolltext.BTest(info : cardinal) : boolean;
var
	fop,fval : PByte;
	vbyte	 : cardinal;
	i		 : integer;
	res		 : boolean;
begin
	fop    := vlbFilter;
	fval   := vlbFilter + 1;
	result := (fop^ AND $80) <> 0;
	for i := 1 to vlbFilterNum do begin
		vbyte := fval^;
		case (fop^ AND $3F) of
		  //
		  // Old Style VL Search OPs
		  // DEPRECATED AND NO LONGER SUPPORTED
		  //
		  // New Style VL Search Combinations
		  //
		  // info = val
		  4	 : res :=  (info AND $000000FF)         = vbyte;
		  5	 : res := ((info AND $0000FF00) shr 8)  = vbyte;
		  6	 : res := ((info AND $00FF0000) shr 16) = vbyte;
		  7	 : res := ((info AND $FF000000) shr 24) = vbyte;
		  // info < val
		  8	 : res :=  (info AND $000000FF)         < vbyte;
		  9	 : res := ((info AND $0000FF00) shr 8)  < vbyte;
		  10 : res := ((info AND $00FF0000) shr 16) < vbyte;
		  11 : res := ((info AND $FF000000) shr 24) < vbyte;
		  // info > val
		  12 : res :=  (info AND $000000FF)         > vbyte;
		  13 : res := ((info AND $0000FF00) shr 8)  > vbyte;
		  14 : res := ((info AND $00FF0000) shr 16) > vbyte;
		  15 : res := ((info AND $FF000000) shr 24) > vbyte;
		  // (info & val) = val (all val bits set)
		  16 : res :=  ((info AND $000000FF)         AND vbyte) = vbyte;
		  17 : res := (((info AND $0000FF00) shr 8)  AND vbyte) = vbyte;
		  18 : res := (((info AND $00FF0000) shr 16) AND vbyte) = vbyte;
		  19 : res := (((info AND $FF000000) shr 24) AND vbyte) = vbyte;
		  // (info & val) <> 0 (one val bit set)
		  20 : res :=  ((info AND $000000FF)         AND vbyte) <> 0;
		  21 : res := (((info AND $0000FF00) shr 8)  AND vbyte) <> 0;
		  22 : res := (((info AND $00FF0000) shr 16) AND vbyte) <> 0;
		  23 : res := (((info AND $FF000000) shr 24) AND vbyte) <> 0;
		  // info <> val
		  36 : res :=  (info AND $000000FF)         <> vbyte;
		  37 : res := ((info AND $0000FF00) shr 8)  <> vbyte;
		  38 : res := ((info AND $00FF0000) shr 16) <> vbyte;
		  39 : res := ((info AND $FF000000) shr 24) <> vbyte;
		  // info >= val
		  40 : res :=  (info AND $000000FF)         >= vbyte;
		  41 : res := ((info AND $0000FF00) shr 8)  >= vbyte;
		  42 : res := ((info AND $00FF0000) shr 16) >= vbyte;
		  43 : res := ((info AND $FF000000) shr 24) >= vbyte;
		  // info <= val
		  44 : res :=  (info AND $000000FF)         <= vbyte;
		  45 : res := ((info AND $0000FF00) shr 8)  <= vbyte;
		  46 : res := ((info AND $00FF0000) shr 16) <= vbyte;
		  47 : res := ((info AND $FF000000) shr 24) <= vbyte;
		  // (info & val) <> val (one val bit not set)
		  48 : res :=  ((info AND $000000FF)         AND vbyte) <> vbyte;
		  49 : res := (((info AND $0000FF00) shr 8)  AND vbyte) <> vbyte;
		  50 : res := (((info AND $00FF0000) shr 16) AND vbyte) <> vbyte;
		  51 : res := (((info AND $FF000000) shr 24) AND vbyte) <> vbyte;
		  // (info & val) = 0 (no val bits set)
		  52 : res :=  ((info AND $000000FF)         AND vbyte) = 0;
		  53 : res := (((info AND $0000FF00) shr 8)  AND vbyte) = 0;
		  54 : res := (((info AND $00FF0000) shr 16) AND vbyte) = 0;
		  55 : res := (((info AND $FF000000) shr 24) AND vbyte) = 0;
		  else begin 	// invalid operator
			inc(fop,2); inc(fval,2);
			CONTINUE;
		  end;
		end; { case }

		if (((fop^ AND $c0)=$c0) AND (result=false)) then break; { shortcut and }
		if (((fop^ AND $40)=$40) AND (result=true)) then break; { shortcut or  }
		
		if ((fop^ AND $80) <> 0) then
			result:=result AND res
		else
			result:=result OR res;
		  
		inc(fop,2); inc(fval,2);
	end;
	
end;	


procedure TVolltext.PrepareSearch(var parsEl:TParsEl);
var
	i	:	integer;
begin
	with parsEl do begin
		if not caseSensitive then begin
			value:=AnsiUpperCase(value);
			head:= AnsiUpperCase(head);
		end;
		
		if (flList.Find(value,i) and (PParsEl(flList.objects[i])^.strict=strict)) then begin	// Liste im Erstling anlegen
			if PParsEl(flList.objects[i])^.idList=NIL then PParsEl(flList.objects[i])^.idList:=TDWList.Create(maxHits);
			parent:=PParsEl(flList.objects[i]);
			idList:=parent^.idList;					// nur eine "geborgte" Liste (Syntaxparser.Clear berücksichtigt das)
		end;

		if strict<0 then begin
			if extSearch>0 then 
				btResList:=TResList.Create			// Hilfsliste für Substring- und RegEx-Multisuche anlegen
			else begin
				hits:=maxlongint; EXIT	        	// Flag für 'nicht weiter prüfen'
			end;
		end;

		if ((strict>0) and (unworte.Find(value,i))) then	
			hits:=maxlongint
		else begin
			if strict=0 then 
				hits:=btree.HowMany(head,strict)
			else
				hits:=btree.HowMany(value,strict);
			flList.AddObject(value,TObject(@parsEl));
		end;
	end;
end;


procedure TVolltext.FirstLevelCheck(var parsEl:TParsEl; idList:TDWList; diff:longint; operation:TOp);
var
	currentList,occList	:	TDWList;
	resList					:	TResList;
	i,res						:	integer;

begin
	if operation=UND then currentList:=TDWList.Create(maxHits) else currentList:=NIL; // Hilfsliste für UND-Op.
	resList:=NIL;
	
	if ((not parsEl.examined) and ((parsEl.parent=NIL) or (not parsEl.parent^.examined))) then begin 
		log.Add(parsEl.value+' hits='+IntToStr(parsEl.hits)+ ' strict='+IntToStr(parsEl.strict)+' has_parent='+BoolToStr(parsEl.parent<>NIL)+' extSearch='+IntToStr(extSearch),false);
		case parsEl.strict of 
		-3,-2,-1	:	begin
						if extSearch>1 then begin
							if flPass=0 then begin		// nur beim ersten Mal den Bayerbaum nach ALLEN sub/regex-Ausdrücken durchsuchen
								if extSearch=3 then btree.CacheOn;
								btree.MultipleSearch(flList,btMaxHits);
								if extSearch=3 then btree.CacheOff;							
								inc(flPass);
							end;
							resList:=parsEl.btResList;// bei weiteren Fällen sind die temp. Resultatlisten bereits gefüllt
						end else begin			//  konventionelle Lösung ohne Sammellisten ist bei hohen IO-Zeiten schlechter
							case parsEl.strict of 
								-3	:	resList:=btree.GetRegEx(parsEl.value,NIL,btMaxHits);
								-2	:	resList:=btree.GetSub(parsEl.value,NIL,btMaxHits);
								-1 : 	resList:=btree.GetTail(parsEl.value,NIL,btMaxHits);
							end;
						end;
					end;
			 0 : 	resList:=btree.GetAlikeRegEx(parsEl.value,parsEl.head,NIL,btMaxHits);
			 1 : 	resList:=btree.GetAlike(parsEl.value,NIL,btMaxHits);
			 2 : 	resList:=btree.GetEqual(parsEl.value,NIL,btMaxHits);
		end;

		for i:=0 to resList.Count-1 do begin
			if resList[i].dp<>0 then begin
				occList:=occ.GetItemList(resList[i].dp,NIL,false,res);
				SecondLevelCheck(idList,occList,currentList,diff,operation);
				if parsEl.idList<>NIL then parsEl.idList.Assign(occList,false);
				occList.Clear;
				if idlist.Overflow then BREAK;
			end;
		end;
		
		parsEl.examined:=true;
		if parsEl.parent<>NIL then parsEl.parent^.examined:=true;
	end else begin									// Suchbegriffsdoublette
		log.Add('Reuse previous results for "'+parsEl.value+'" hits='+IntToStr(parsEl.hits),false);
		SecondLevelCheck(idList,parsEl.idList,currentList,diff,operation);
	end;

	if resList<>NIL then resList.Clear;
	if currentList<>NIL then begin  
		idlist.AndMask(currentList,diff);	// Abgleich mit der Hilfsliste
		currentList.Free; 
	end;
end;


procedure TVolltext.SecondLevelCheck(idL,ocL,curL:TDWList; diff:longint; operation:TOp);
var
	i	:	integer;
	el	:	TDW;
begin
	for i:=0 to ocl.Count-1 do begin	// id=dw1, pos=dw2, datum=dw3, gewicht=dw4, info=dw5
		el:=ocL.Element[i];
		with el do begin
			if (dw3>=dStart) and (dw3<=dStop) and ((vlbFilterNum=0) or BTest(dw5)) then 
			case operation of
				NOP,
				ODER	:	idL.AddElem(el);
				NICHT	:	idL.DeleteIfFind([dw1,dw2,diff]);
				UND	:	if idL.Exists([dw1,dw2,diff]) then curL.AddElem(el);
			end;
		end;
		if idL.Overflow then BREAK;
	end;
end;



function TVolltext.ThirdLevelCheck_WithSortedFileRef(id,datum,gewicht,info:cardinal):boolean;
var
	name,titel	:	string;
	i,e			:	integer;
begin
	result:=false; 
	if rres>maxFind then EXIT;					// hier werden Doubletten NICHT mitgezählt!

	name:=fileRef[id]; titel:=fileRef.titel[id];
	e:=fileRef.Error; 
	if (e=0) and (name<>'') then begin  
		i:=0;
		while (e=0) and (name<>'') do begin
			if ((fFilter='') or 
				(((reg=NIL) and ((pos(fFilter,name)>0)<>fFilterFilter)) or ((reg<>NIL) and (reg.Exec(name)<>fFilterFilter)))) then begin
					tmpdup.AddWithFileRef(name,titel,id,datum,gewicht,info);
					inc(i); 
				end;
			name:=fileRef[0];						// ggf. Doubletten holen
			e:=fileRef.Error;
		end;
		if i>0 then inc(rres);
	end;
	tmpdup.CustomSort;
	treffer.AddDuplicates(tmpdup);
	tmpdup.ClearList;
	
	if e<>0 then vtError:=e else result:=true
end;


function TVolltext.ThirdLevelCheck_WithFileRef(id,datum,gewicht,info:cardinal):boolean;
var
	name,titel	:	string;
	i,e			:	integer;
begin
	result:=false;
	if rres>maxFind then EXIT;
	
	name:=fileRef[id]; titel:=fileRef.titel[id];
	e:=fileRef.Error;
	if (e=0) and (name<>'') then begin  
		i:=0;
		while (e=0) and (name<>'') do begin
			if ((fFilter='') or 
				(((reg=NIL) and ((pos(fFilter,name)>0)<>fFilterFilter)) or ((reg<>NIL) and (reg.Exec(name)<>fFilterFilter)))) then begin
				treffer.AddWithFileRef(name,titel,id,datum,gewicht,info);
				inc(i);
			end;
			name:=fileRef[0];		// ggf. Doubletten holen
			e:=fileRef.Error; 
		end;
		if i>0 then inc(rres); // count duplicates only one time. same as for sortedrefs
	end;

	if e<>0 then vtError:=e else result:=true
end;


function TVolltext.ThirdLevelCheck_WithoutFileRef(id,datum,gewicht,info:cardinal):boolean;
begin
	if treffer.Count>maxFind then 
		result:=false
	else
	begin
		inc(rres);
		treffer.AddWithOutFileRef(id,datum,gewicht,info);
		result:=true;
	end;
end;


function TVolltext.Suche(var such:string; const von,bis,fileFilter:string; maxTreffer,sortOrder:integer; bitFilter:byte; var overflow:boolean):integer;
// Altes Such-Interface. Nur aus Kompatibilitätsgründen beibehalten.
// Dabei bedeutet 0 (im Unterschied zur vlSuche), dass der Wert ignoriert wird (=true).
var
   vlbuf 		: 	array [0..5] of byte;
	bitFilterP	:	Pbyte;
	vlcount		:	integer;
begin
	if bitFilter<>0 then begin
		bitFilterP:=@bitFilter;
		vlbuf[0]:=4;
		vlbuf[1]:=bitFilterP^;
		vlcount:=2;
	end else
		vlcount:=0;

	Suche:=vlSuche(such,von,bis,fileFilter,@vlbuf,vlcount,maxTreffer,sortOrder,overflow)
end;


function TVolltext.vlSuche(var such	: string; const von,bis,fileFilter:string; bitFilter:PByte; bitFilterNum,maxTreffer,sortOrder:integer; var overflow:boolean):integer;
var
	parser	:	TParser;
	res,i	 	: 	integer;
begin
	if such='' then begin  
		result:=-1; EXIT;
	end;
	try
		if maxTreffer<0 then begin
			maxTreffer:=abs(maxTreffer);
			if utf8decoder<>NIL then
				such:=utf8Decoder.Decode(such)
			else 
				log.Add('UTF8-Decoder is *NOT* initialized!',true);
		end;
		result:=0; vtError:=0; rres:=0;
		overflow:=false; 
		maxFind:=maxTreffer;
		treffer.Free;
		treffer:=THitlist.Create(maxTreffer);
		tmpdup.Free; tmpdup:=NIL;
		flList.Clear;
		flPass:=0;
		if ThirdLevelCheck = @ThirdLevelCheck_WithSortedFileRef then
			if issueOffset>0 then
				tmpdup:=TDupSortList.Create(issueOffset,issueLength,issuePreferred)
			else
				tmpdup:=TDupSortList.Create(issuePattern,issuePreferred);

		parser:=TParser.Create(such,res);
		if res=0 then begin  
			if von='' then dStart:=0 	  else dStart:=word(Age95(von));
			if bis='' then dStop :=$FFFF else dStop :=word(Age95(bis));
			vlbFilterNum:=bitFilterNum shr 1;
			vlbFilter:=bitFilter;
			fFilter:=fileFilter;
			if (pos('!',fFilter)=1) then begin
				fFilterFilter:=true;
				fFilter:=copy(fFilter,2,length(fFilter)-1);
			end else 
				fFilterFilter:=false;
				
			if (pos('REGEX=',fFilter)=1) or (pos('REGEX_',fFilter)=1) then begin  
				fFilter:=copy(fFilter,7,length(fFilter)-6);
				reg:=TRegExpr.Create;
				reg.Expression:=ffilter;
			end;

			parser.Optimizer(@PrepareSearch);	 			 	// mit "Unworten" abgleichen, Häufigkeiten ermitteln und SuchBaum optimieren
			parser.Trigger(@FirstLevelCheck,sortOrder);	// Suche in zwei Ebenen ausführen (inkl. Datums- und Bytefilter anwenden)
			for i:=0 to parser.Count-1 do	ThirdLevelCheck(parser[i],parser.info1[i],parser.info2[i],parser.info3[i]); // Filefilter anwenden und Ergebnisse speichern
			overflow:=parser.Overflow;							// Überlauf bei einem Zwischenergebnis 
		end else 
			vtError:=res;
			
		reg.Free; reg:=NIL;
		tmpdup.Free; tmpdup:=NIL;
		parser.Free;
		flList.Clear;
		lastQuery:=UnhideRegExe(such);
		log.Add('Suche nach '+lastQuery+' von '+von+' bis '+bis+' fFilter='+fileFilter+
		        ' bFilterLen='+IntToStr(vlbFilterNum)+' hatte '+IntToStr(treffer.Count)+
				  ' ('+IntToStr(maxTreffer)+') Treffer.'+overStr[overflow],true);
		if vtError<>0 then log.Add('Fehler! Code='+IntToStr(vtError),false);
		result:=treffer.Count;
	except
		on E:Exception do log.Add('*** Nicht behandelter Fehler während der Suche ('+lastQuery+'): '+E.Message,true);	// Fallback wg. Serverbetrieb
	end;
end;


function	TVolltext.GetTreffer(i:integer):string;
begin
	if ((treffer<>NIL) and (i<treffer.Count)) then result:=treffer[i] else result:='';
end;


function	TVolltext.GetTrefferPtr(i:integer):PHit;
begin
	if ((treffer<>NIL) and (i<treffer.Count)) then
		result:=treffer.GetHitRawPtr(i)
	else
		result:=nil;
end;


function	TVolltext.GetAllTrefferPtr():PHitArray;
begin
	if (treffer<>NIL) then
		result:=treffer.getHitListPtr
	else
		result:=nil;
end;


function	TVolltext.GetTrefferCount():integer;
begin
	if (treffer<>NIL) then
		result:=treffer.count
	else
		result:=0;
end;


function	TVolltext.GetResultList(var list:TStringList):integer;
var
	i : integer;
begin
	if list=NIL then list:=TStringlist.Create;
	list.Sorted:=false; list.Duplicates:=dupIgnore;
	for i:=0 to treffer.count-1 do
		list.Add(treffer[i]);
	result:=list.Count;
end;


function TVolltext.CountC(count,wcount:cardinal):cardinal;
begin
	result:=cardinal(round((count+wcount)*mapFactor));	// Faktor wird je nach mapMode im Constructor, InsertWords oder InsertWordsFromFiles bestimmt
	if result<defCluster then result:=defCluster else if result>maxCluster then result:=maxCluster;
end;


function TVolltext.InsertWord(s:string; fid,fpos,info,wcount:cardinal; age:word):integer;
var	// hier kein UpperCase/Trim mehr!
	i				:	integer;
	c,count,key	:	cardinal;
	
begin
	result:=-1;
	if not Wortzeichen(s) or ((useWordFilter) and (unworte.Find(s,i))) then EXIT;
	inc(result);
	
	if not btree.SearchWord(s,count,key) then begin
		key:=0; count:=0;								// erstmaliges Vorkommen eines Wortes überhaupt
	end;

	c:=CountC(count,wcount);
	if result=0 then begin
		result:=occ.InsertRecord(key,c,fid,fpos,age,info);
		if result=0 then begin
			if not btree.Insert(s,key) then begin	// ggf. nur Wortzähler im Baybaum erhöhen
				log.Add('Btree-Error: '+IntToStr(btree.Error),true);
				result:=-614;
			end;
		end;
	end;
end;


function TVolltext.InsertWords(worte:TStringList; fname,datum,md5Sum:string; info:cardinal; var id:cardinal):integer;
var
	insList				:	TIdList;
	a						:	array[0..3] of string;
	s,titel				:	string;
	info_,inf			:	cardinal;
	age					:	longint;
	i,j,res,n,words,w	:	integer;
	
begin
	if ro then begin  
		result:=-601; EXIT
	end;
	result:=0; words:=0; titel:='';
	if basePathName<>'' then delete(fname,1,length(basePathName));
	if worte.Count=0 then begin  
		log.Add('File "'+fname+'" fehlen die Worte (missing words before testing)',true);
		EXIT
	end;
	age:=Age95(datum);
	if age<0 then begin  						// ungültige Datumsangabe (wohl durch Age95)
		log.Add('Invalid Date "'+datum+'" for file "'+fname+'""',true);
		result:=-608; EXIT
	end;

	if mapMode=-2 then begin  					// andere Modi => Constructor und InsertWordsFromFiles
		mapFactor:=age/365.25; 					// nach verstrichenen Tagen/Jahr für Jahresarchive
		try 
			mapFactor:=(1/(mapFactor-trunc(mapFactor)))-1;
			if mapFactor>3 then mapFactor:=3;
		except 
			on EDivByZero do mapFactor:=0.001;
		end;
	end;

	if id=0 then begin  
		if useFileRef>0 then 
			id:=fileRef.NextID 
		else
		begin
			log.Add('ID ist ungültig (0)',true);
			result:=-609; EXIT;
		end;
	end;

	insList:=TIdList.Create;
	insList.Sorted:=true; insList.Duplicates:=dupAccept;

	w:=worte.Count; if w<100 then w:=100;
	for i:=0 to worte.Count-1 do begin				// Vorbereitungslauf
		s:=worte[i]; if s='' then Continue;
		if copy(s,1,3)='*|*' then begin  
			titel:=copy(s,4,maxTitelLen);
			if md5Sum<>'' then titel:=titel+'*|*'+md5Sum;
			Continue;
		end;
		
		n:=split(s,',',a);
		if a[0]='' then Continue;
		if caseSensitive then s:=Trim(a[0]) else s:=AnsiUpperCase(Trim(a[0]));
		if n>0 then info_:=info OR StrToIntDef(a[n],0) else info_:=info; // kompatibel zu alten Filter mit Gewichtsangabe ("word,gewicht,info")
		n:=1;
		if insList.Find(s,j) then begin
			inf:=info_ and globalInfoBits;
			while ((j<insList.Count) and (insList[j]=s)) do begin
				if (insList.ids[j][1] and globalInfoBits)<>inf then begin
					info_:=info_ or (insList.ids[j][1] and globalInfoBits);
					insList.ids[j][1]:=insList.ids[j][1] or inf;
				end;
				inc(n); inc(j);
			end;
		end;
		insList.AddIds(s,[i,info_,n]);
	end;

	s:='';
	for i:=0 to insList.Count-1 do				// Einfügelauf
	try
		if insList[i]<>s then n:=insList.ids[i][2]; // n (Häufigkeit des Wortes in der Liste) stets dem ersten Eintrag der Gleichen entnehmen
		s:=insList[i];
      res:=InsertWord(s,id,insList.ids[i][0],insList.ids[i][1],n,word(age));
		if res<>-1 then begin  
			if res<0 then begin  
				insList.Free; 
				log.Add('Insertion Error: '+IntToStr(res),true);
				result:=res; EXIT
			end;
			inc(words); 
		end;
	except
		on EConvertError do;							// Eintrag überspringen
	end;
	insList.Free;

	if words>0 then begin  
		if useFileRef>0 then result:=fileRef.NewFilename(fname,titel,id) else result:=words;
		log.Add('File "'+fname+'" with '+IntToStr(words)+' words inserted. Res='+IntToStr(result)+' FileID='+IntToStr(id),true);
	end else
	begin
		log.Add('File "'+fname+'" fehlen die Worte (missing words after testing)',true);
		result:=0;
	end;
end;


function TVolltext.ChainDuplicate(fname:string; lastID:cardinal; var id:cardinal): integer;
var
	res	: integer;

begin
	if basePathName<>'' then delete(fname,1,length(basePathName));
	if lastID<>0 then begin  
		res:=fileRef.ChainFilename(fname,lastID,id);
		if res=0 then begin  
			log.Add('File '+fname+', ID='+IntToStr(id)+', chained with existing ID='+IntToStr(lastID),true);
			result:=0;
		end else
		begin
			log.Add('Chaining of File '+fname+' FAILED with Result='+IntToStr(res),true);
			result:=res;
		end
	end else
	begin
		log.Add('File '+fname+' skipped (lastID is invalid)',true);
		result:=-650;
	end;
end; { TVolltext.ChainDuplicate }


function TVolltext.GetWordsFromProgram(inFile:string; var worte:TStringList):integer;
{$IFDEF LINUX}	
var
	cmdFifo,f	:	text; 
	s				:	string;	
{$ENDIF }	
begin
	if ro then begin  
		result:=-601; EXIT
	end;
	result:=0;
	if execProg='' then execProg:=ExtractFilePath(paramstr(0))+'arcfilter';
	worte:=TStringList.Create;
	worte.Sorted:=false; worte.Duplicates:=dupAccept;

	if tmpFile='' then 
{$IFDEF LINUX}
	begin
		try
			if pos('NAMEDPIPE:',execProg)=1 then begin// named pipe eines laufenden(!) Programms nutzen
				if length(execProg)>10 then s:=copy(execProg,11,255) else s:='/tmp/';
				assign(cmdFIFO,s+'arcfiltercmd.fifo');
				rewrite(cmdFIFO);
				writeln(cmdFIFO,inFile);
				close(cmdFIFO);
				assign(f,s+'arcfilterres.fifo');
				reset(f);
			end else 
				popen(f,execProg+' '+inFile,'R');

			while not (eof(f)) do begin
				readln(f,s);
				worte.Add(s);
			end;
			close(f)
		except
			on E:Exception do result:=-603
		end;
	end
{$ELSE}
		result:=-602
{$ENDIF}
	else begin
		exec(execProg,infile+' '+tmpFile);
		if (doserror=0) and FileExists(tmpFile) then begin  
			worte.LoadFromFile(tmpFile);
			DeleteFile(tmpFile);
		end;
	end;
end;


function TVolltext.InsertWordsFromProgram(inFile,datum:string; var id:cardinal):integer;
var
	worte	:	TStringList;
begin
	result:=GetWordsFromProgram(inFile,worte);
	if result=0 then result:=InsertWords(worte,inFile,datum,'',0,id);
	worte.Free;
end;


function TVolltext.InsertWordsFromFile(const wordListFile,inFile,datum:string; info:cardinal; var id:cardinal):integer;
var
	worte	:	TStringList;
	s		:	THandleStream;
	
begin
	if ro then begin  
		log.Add('Database is readOnly! - Insert aborted',true);
		result:=-601; EXIT
	end;

	result:=0;
	if (inFile='') and (useFileRef>0) then begin  
		log.Add('Die Methode "InsertWordsFromFile" benötigt beim Betrieb mit Dateireferenzliste ("useFileRef") einen Dateinamen',true);
		result:=-607; EXIT;
	end;
	
	worte:=TStringList.Create;
	worte.Sorted:=false; worte.Duplicates:=dupAccept;

	if wordListFile<>'' then 
		worte.LoadFromFile(wordListFile)
	else
	begin
		s:=THandlestream.Create(0);				// 0 für STDIN
		worte.LoadFromStream(s);
		s.Free;
	end;
	
	result:=InsertWords(worte,inFile,datum,'',info,id);
	worte.Free;
end;


function TVolltext.InsertWordsFromPipe():integer;
var
	n, files	  : integer;
	a			  : array [0..3] of string;
	line		  : string;
	infile, datum : string;
	info, id	  : cardinal;
	worte		  : TStringList;
	
begin
	if ro then begin  
		log.Add('Database is readOnly! - Insert aborted',true);
		result:=-601; EXIT
	end;

	result:=0;
	files:=0;
	
	worte:=TStringList.Create;
	worte.Sorted:=false; worte.Duplicates:=dupAccept;
	infile:='';
	datum:='';
	info:=0;
	id:=0;

	while not eof(input) do begin
		readln(line);
		if copy(line,1,3)='@f@' then begin
			if infile<>'' then begin
				n:=InsertWords(worte,infile,datum,'',info,id);
				if n<1 then result:=n;
				worte.Clear;
				infile:='';
				inc(files);
			end;
			n:=split(copy(line,4,length(line)),'|',a);
			if n<1 then
				log.Add('need at least a fileref and a date',true)
			else begin
				infile:=a[0];
				datum:=a[1];
				if n>1 then info:=StrToIntDef(a[2],0) else info:=0;
				if n>2 then id:=StrToIntDef(a[3],0) else id:=0;
			end;
		end else
			worte.Add(line);
	end;
	if infile<>'' then begin
		if infile<>'' then begin
			n:=InsertWords(worte,infile,datum,'',info,id);
			if n<1 then result:=n;
		end;
		worte.Clear;
		infile:='';
		inc(files);
	end;

	log.Add('Inserted '+IntToStr(files)+' files from pipe',true);
	worte.Free;
end;


function TVolltext.InsertWordsFromFiles(path,pattern,datum:string):integer;
var
	worte,duplos										:	TStringList;
	doneList												:	TIdList;
	scanner												:	TDirScanner;
	datum_,fname,md5Sum,filename,duploFile		:	string;
	i,j,res,failed,a,d,skipped,n,wc				:	integer;
	id,lastID											:	cardinal;
{$IFDEF LINUX}		
	fInfo													:	stat;
{$ENDIF}	
		
begin
	if ro then begin  
		log.Add('Database is readOnly! - Insert aborted',true);
		result:=-601; EXIT
	end;
	if useFileRef=0 then begin  
		log.Add('Die Methode "InsertWordsFromFiles" unterstützt nicht den Betrieb ohne Dateireferenzliste ("useFileRef")',true);
		result:=-606; EXIT
	end;

	vtError:=0; datum_:=datum; doneList:=NIL; duplos:=NIL;
	a:=0; d:=0; skipped:=0; failed:=0; 
	doneList:=TIdList.Create; 
	if doubletten=3 then begin
		duplos:=TStringList.Create;
		duploFile:=dbCfg+'.doublettes';
		duplos.Sorted:=true; duplos.Duplicates:=dupIgnore;
		if FileExists(duploFile) then duplos.LoadFromFile(duploFile);
	end;
	log.Add('Scanning Files '+pattern+' in '+path,true);
	scanner:=TDirScanner.Create(dbCfg+'.unfiles.txt');
	scanner.LockFile:='NOARCHIVE';
	n:=scanner.Finde(path,pattern,16);	// von "scanner" geborgte Liste
	log.Add(IntToStr(n)+' Files to archive',true);
	for i:=0 to n-1 do
	begin
		if vtError<>0 then break;
		log.Add('File '+scanner[i].name+' to be archived',true);
{$IFDEF LINUX}		
		if not followLinks then begin
			if (fpStat(scanner[i].name,fInfo)>=0) then begin
				if (fpS_ISLNK(fInfo.mode)) then begin
					log.Add('File is a Link - skipped',true);
					inc(skipped); Continue;
				end;
			end else	begin
				log.Add('Cannot get file info',true);
				inc(skipped); Continue;
			end;
		end;
{$ENDIF}		
		try
			if datum='' then datum_:=DateToStr(FileDateToDateTime(scanner[i].time));
		except
			log.Add('File '+scanner[i].name+' has an invalid date: '+IntToStr(scanner[i].time),true);
			inc(skipped); Continue;
		end;
		worte:=NIL; wc:=0; md5Sum:='';
		res:=GetWordsFromProgram(scanner[i].name,worte);
		if (worte<>NIL) then wc:=worte.Count;
		if (res<>0) then begin  
			log.Add('Fehler (Code='+IntToStr(res)+') beim Verarbeiten der Datei '+scanner[i].name+ '(Anzahl Worte: '+IntToStr(wc)+')',true);
			inc(failed); worte.Free; Continue;
		end else if wc=0 then begin  
			log.Add('File '+scanner[i].name+' skipped (missing words)',true);
			inc(skipped); worte.Free; Continue;
		end;

		if (wc>9) then md5Sum:=GetMD5FromList(worte);
		fname:=scanner[i].name;
		if basePathName<>'' then delete(fname,1,length(basePathName));
		fileName:=ExtractFileName(fname); fileName:=ChangeFileExt(fileName,'');
		
		if (md5Sum<>'') and (((doubletten>0) and (doneList.Find(md5Sum,j))) or ((doubletten=3) and (duplos.Find(md5sum+filename,j)))) then begin  
			if doubletten=1 then begin
				lastID:=doneList.id[j];
				if lastID<>0 then begin  
					res:=fileRef.ChainFilename(fname,lastID,id);
					if res=0 then begin  
						log.Add('File '+fname+', ID='+IntToStr(id)+', chained with existing ID='+IntToStr(doneList.id[j]),true);
						doneList.id[j]:=id;			// Zeiger auf letzten Eintrag nachstellen
						inc(d);
					end else
					begin
						log.Add('Chaining of File '+fname+' FAILED with Result='+IntToStr(res),true);
						inc(failed)
					end
				end else
				begin
					log.Add('File '+fname+' skipped (missing words in Doublette)',true);
					inc(skipped);
				end;
			end else begin
				log.Add('Doublette'+fname+' skipped',true);
				inc(skipped);
			end;
		end else
		begin
			if mapMode=-1 then begin 				// andere Modi => Constructor und InsertWords
				try
					mapFactor:=(i+1)/n;		//	nach Anzahl Dateien/Gesamtanzahl pro Lauf
					mapFactor:=(1/(mapFactor-trunc(mapFactor)))-1;
					if mapFactor>3 then mapFactor:=3;
				except
					on EDivByZero do mapFactor:=0.001;
				end;
			end;
			
			id:=0;								// FileRef-Objekt vergibt ID
			res:=InsertWords(worte,scanner[i].name,datum_,md5Sum,0,id);
			if res<0 then begin  
				log.Add('Fehler (Code='+IntToStr(res)+') beim Verarbeiten der Datei '+scanner[i].name,true);
				inc(failed);
			end else
			begin
				if (doubletten>0) and (md5Sum<>'') then begin
					doneList.AddId(md5Sum,id);
					if doubletten=3 then begin
						duplos.Add(md5sum+filename); 
					end;
					
				end;
				if id>0 then inc(a) else inc(skipped);
			end;
		end;
		worte.Free;
	end;
	
	result:=n;
	log.Add('Archiving done: '+IntToStr(a)+' Files archived, '+IntToStr(d)+' doublettes stored. '+IntToStr(skipped)+' Files skipped and '+IntToStr(failed)+' Files failed.',true);
	scanner.Free;
	doneList.Free;
	if duplos<>NIL then begin
		duplos.SaveToFile(duploFile);
		duplos.Free;
	end;
end;


function TVolltext.ScanCluster_WithFileRef(const wort:string; var key,optimized,checked:cardinal; const idList:TDWList; minAge:word):integer;
var
	el						:	TDW;
	name,titel,av		:	string;
	i,id,lastID,k,rest:	cardinal;
	c,j,secErr,platz,lID	:	longint;
	
begin
	c:=0; result:=0; lID:=0; platz:=0;
	if secunda.occ.Count=0 then EXIT;
	for i:=0 to secunda.occ.Count-1 do begin	// Platz ermitteln, um optimale Auslastung zu erreichen
		if secunda.occ[i].dw1=lID then platz+=2 else	platz+=occSize;
		lID:=secunda.occ[i].dw1;
	end;
	lID:=0;

	for i:=0 to secunda.occ.Count-1 do begin	// id=dw1, pos=dw2, datum=dw3, gewicht=dw4 (hier unbenutzt), info=dw5
		inc(checked);
		el:=secunda.occ[i];
		if el.dw3<minAge then Continue;		// zu alten Eintrag überspringen

		if idList.Find([el.dw1],j) then
			el.dw1:=idList.value[j]				// alte ID schon verarbeitet => gegen neue tauschen
 		else begin
			name:=secunda.fileRef[el.dw1]; secErr:=secunda.TellError;
			if name='' then Continue; 			// gelöschten Eintrag überspringen

			if (fileList<>NIL) then begin 	// Datei noch vorhanden? 
				av:=fileList[name];
				if av='' then begin				// noch nicht getestet
					if FileExists(basePathName+name) then 
						av:='1' 
					else begin
						av:='0';
						log.Add('File "'+basePathName+name+'" not present - Entry skipped',true);						
					end;
					fileList[name]:=av;
				end;
				if av='0' then Continue;
			end;

			if secErr=0 then begin  
				titel:=secunda.fileRef.titel[el.dw1];
				secErr:=secunda.TellError;
			end;

			if secErr<>0 then begin  
				result:=-610; EXIT;
			end;

			result:=fileRef.NewFilename(name,titel,id);
			idList.Add([el.dw1,id]);			// Paar alte:neue Id merken
			el.dw1:=id; lastID:=id;
			while result=0 do
			begin
				name:=secunda.fileRef[0];
				secErr:=secunda.TellError;
				if secErr<>0 then begin  
					result:=-611; EXIT;
				end;
				if name='' then BREAK;
				result:=fileRef.ChainFilename(name,lastID,id);
				lastID:=id;
			end;
		end;

		if el.dw1=lID then platz-=2 else	platz-=occSize;
		lID:=el.dw1;
		rest:=(platz+occSize-1) div occSize; if rest>maxCluster then rest:=maxCluster;
		inc(optimized);
		k:=key;
		with el do result:=occ.InsertRecord(k,rest,dw1,dw2,dw3,dw5);
		if (result=0) and (key=0) then begin // nur ersten key merken!
			key:=k;
			if not btree.Insert(wort,key) then result:=-604; 
		end else
			inc(c);

		if result<>0 then EXIT
	end;

	if (c>0) and not btree.Update(wort,key,c,1) then result:=-612;
end;


function TVolltext.ScanCluster_WithoutFileRef(const wort:string; var key,optimized,checked:cardinal; const idList:TDWList; minAge:word):integer;
var
	el					:	TDW;
	i,k,rest			:	cardinal;
	c,platz,lID		:	longint;
	
begin
	c:=0; result:=0; lID:=0; platz:=0;
	if secunda.occ.Count=0 then EXIT;
	for i:=0 to secunda.occ.Count-1 do begin	// Platz ermitteln, um optimale Auslastung zu erreichen
		if secunda.occ[i].dw1=lID then platz+=2 else	platz+=occSize;
		lID:=secunda.occ[i].dw1;
	end;
	lID:=0;

	for i:=0 to secunda.occ.Count-1 do begin	// id=dw1, pos=dw2, datum=dw3, gewicht=dw4 (hier unbenutzt), info=dw5
		inc(checked);
		el:=secunda.occ[i];
		if el.dw3>=minAge then begin  
			if el.dw1=lID then platz-=2 else	platz-=occSize;
			lID:=el.dw1;
			rest:=(platz+occSize-1) div occSize; if rest>maxCluster then rest:=maxCluster;
			inc(optimized); 
			k:=key; 
			with el do result:=occ.InsertRecord(k,rest,dw1,dw2,dw3,dw5);
			if (result=0) and (key=0) then begin  	// nur ersten key merken!
				key:=k;
				if not btree.Insert(wort,key) then result:=-604; 
			end else
				inc(c);
		end;
		if result<>0 then EXIT
	end;

	if (c>0) and not btree.Update(wort,key,c,1) then result:=-612;
end;


function TVolltext.ExecMerge(source:TVolltext; const startDatum:string; wordCheck,fileCheck,destructive,verbose:boolean):integer;
var
	ScanCluster							:	TOptWordFunc;
	el										:	TVal;
	idList								:	TDWList;
	key,dummy,n,optimized,checked	:	cardinal;
	i,j,res								:	integer;
	minAge								:	word;
	fastMode								:	boolean;

begin
	if source<>NIL then secunda:=source;	//	für externe Aufrufer (z.B. jodad)
	if startDatum<>'' then minAge:=word(Age95(startDatum)) else minAge:=0;
// bei reiner Optimierung ist kein REF-Handling nötig - Zieldatei neu (leer) und keine Filter angegeben:
	fastMode:=(useFileRef>0) and (btree.WordCount=0) and (minAge=0) and (not wordCheck) and (not fileCheck) and (not destructive);
	if fastMode then begin
		if (not ro) and (useFileRef=2) then fileRef.Commit;
		log.Add('Using fastMode: REF file will be copied',true);
	end;
	if fileCheck then begin
		fileList:=THash.Create;
	end;		

	if (useFileRef=0) or fastMode then 
		ScanCluster:=@ScanCluster_WithoutFileRef
	else
		ScanCluster:=@ScanCluster_WithFileRef;
	
	idList:=TDWList.Create(0);
	secunda.btree.GetAll(NIL);
	secunda.btree.Unsort;
	n:=0; optimized:=0; checked:=0; vtError:=0;

	for i:=0 to secunda.btree.Count-1 do 
	begin
		if vtError<>0 then begin  
			result:=vtError; EXIT;
		end;
		
		el:=secunda.btree[i];
		if not wordCheck or (Wortzeichen(el.s) and not unworte.Find(el.s,j)) then begin  
			if (verbose) and (i mod 1000=0) then begin   
				write(#13,checked:10,' (',optimized:10,') von ',secunda.btree.AllCount,' = ',(checked/secunda.btree.AllCount)*100:5:1,'%');
			end;

			if not btree.SearchWord(el.s,dummy,key) then key:=0; // neuer Begriff in der primären DB
			n:=n+el.z;
			secunda.occ.GetItemList(el.dp,NIL,false,res);
			ScanCluster(el.s,key,optimized,checked,idList,minAge);
			if vtError<>0 then begin  	
				log.Add('Error '+IntToStr(vtError)+' '+IntToStr(el.z)+' '+el.s,true);
				vtError:=0;
			end;
		end else 
			checked+=el.z;
	end;

	if verbose then writeln(#13,checked:10,' (',optimized:10,') von ',secunda.btree.AllCount,' = ',(checked/secunda.btree.AllCount)*100:5:1,'%    ');
	idList.Free; idList:=NIL;
	fileList.Free; fileList:=NIL;
	log.Add('Merging of '+IntToStr(n)+' words done with result='+IntToStr(vtError),true);
	if destructive then 
		secunda.Clear
{$IFDEF LINUX}	
	else if fastMode then begin
		if secunda.tempDB then secunda.fileRef.Commit;
		shell('cp -pf '+secunda.dbName+'.ref '+dbName+'.ref')
	end
{$ENDIF}
	;
	result:=vtError; vtError:=0;
end;


function TVolltext.MergeDB(const sourceDB,startDatum:string; wordCheck,fileCheck,verbose:boolean):integer;
var
	p					:	integer;
	sourceDBName	:	string;
	
begin
	if ro then begin
		result:=-1; EXIT
	end;
	result:=0; vtError:=0;
	log.Add('Merging '+sourceDB+' into '+dbname+' (wordCheck='+IntToStr(ord(wordCheck))+', fileCheck='+IntToStr(ord(fileCheck))+')',true);
	secunda:=NIL;
	secunda:=TVolltext.Create(sourceDB,ReadOnlyDB,result);
	if result<>0 then	begin
		log.Add('Merger cannot open '+sourceDB+' ErrorCode='+IntToStr(result),true);
		secunda.Free; EXIT;
	end;

	result:=ExecMerge(NIL,startDatum,wordCheck,fileCheck,false,verbose);
	secunda.Free;
	if tempDB then begin
		p:=pos(':',sourceDB);
		if p>0 then sourceDBName:=copy(sourceDB,p+1,255) else sourceDBName:=sourceDB;
		DeleteFile(sourceDBName+'.btf'); 
		DeleteFile(sourceDBName+'.ocl');
		btree.Commit;
		occ.Commit;
		if useFileRef>0 then begin
			DeleteFile(sourceDBName+'.ref');
			fileRef.Commit;
			RenameFile(dbName+'.ref',sourceDBName+'.ref');
		end;
		RenameFile(dbName+'.btf',sourceDBName+'.btf');
		RenameFile(dbName+'.ocl',sourceDBName+'.ocl');
	end;
end;


function TVolltext.InvalidateWord(s:string; fid:cardinal):integer;	// Löschen von Einträgen
var	// hier kein Trim/Uppercase mehr!
	i				:	longint;
	count,key	:	cardinal;
begin
	result:=0;
	if not Wortzeichen(s) or unworte.Find(s,i) then EXIT;
	
	if btree.SearchWord(s,count,key) then begin 	// bereits bekanntes Wort
		if key=0 then EXIT;
		
		result:=occ.InvalidateRecord(key,fid);
		if result>0 then 
		begin
			if result>=integer(count) then key:=0;	// es gibt kein Vorkommen des Wortes mehr
			if not btree.Update(s,key,-result,1) then begin		//  nur den Wortzähler im Baybaum herabsetzen
				log.Add('Btree-Error: '+IntToStr(btree.Error),true);
				result:=-618;
			end;
		end;
	end; 
end;


function TVolltext.InvalidateEntry(worte:TStringList; id:cardinal):integer;
var
	tmpList			:	TStringList;
	a					:	array[0..3] of string;
	s,wort			:	string;
	i,j,res,words	:	integer;
	
begin
	if ro then begin  
		result:=-601; EXIT
	end;
	
	if id=0 then begin
		log.Add('FileID='+IntToStr(id)+' skipping InvalidateEntry',true);
		EXIT
	end;
	
	if worte.Count=0 then begin  
		log.Add('Invalidator FileID='+IntToStr(id)+' fehlen die Worte (missing words before testing)',true);
		EXIT
	end;

	tmpList:=TStringList.Create;
	tmpList.Sorted:=true; tmpList.Duplicates:=dupIgnore;
	result:=0; words:=0;
	for i:=0 to worte.Count-1 do try
		s:=worte[i]; 
		if (s='') or (copy(s,1,3)='*|*') then Continue;	//	 hier irrelevante Information
		split(s,',',a);
		if caseSensitive then wort:=Trim(a[0]) else wort:=AnsiUpperCase(Trim(a[0]));
		if (not tmpList.Find(wort,j)) then begin
			tmpList.Add(wort);						// Doubletten (aus unterschiedlicher Groß-/Kleinschr. und
			res:=InvalidateWord(wort,id);			// angehängten Flags beim Laden unerkannt) erst HIER ignorieren
			if res>0 then words+=res;
		end;
	except
		on EConvertError do ;						// Eintrag überspringen
	end;

	tmpList.Free;
	if (words>0) and (useFileRef>0) then result:=fileRef.InvalidateFilename(id);

	if result=0 then begin
		if words>0 then
			log.Add(IntToStr(words)+' words from FileID='+IntToStr(id)+' DELETED',true)
		else
			log.Add('FileID='+IntToStr(id)+' NOT FOUND for deletion. Res='+IntToStr(result),true);
		result:=words
	end else
	begin
		log.Add('FileID='+IntToStr(id)+' NOT DELETED - ERROR: '+IntTostr(result),true);
		result:=-result 
	end;
end;


function TVolltext.InvalidateEntryFromProgram(inFile:string; id:cardinal):integer;
var
	worte	:	TStringList;
begin
	result:=GetWordsFromProgram(inFile,worte);
	if result=0 then result:=InvalidateEntry(worte,id);
	worte.Free;
end;


function TVolltext.InvalidateEntryFromFile(const wordListFile:string; id:cardinal):integer;
var
	worte	:	TStringList;
	s		:	THandleStream;
	
begin
	if ro then begin
		log.Add('Database is readOnly! - Insert aborted',true);
		result:=-601; EXIT
	end;

	result:=0;
	worte:=TStringList.Create;
	worte.Sorted:=false; worte.Duplicates:=dupAccept;	// Doubletten werden wg. Schreibweise und angehängten Flags erst oben ausgefiltert 

	if wordListFile<>'' then 
		worte.LoadFromFile(wordListFile)
	else begin
		s:=THandlestream.Create(0);				// 0 für STDIN
		worte.LoadFromStream(s);
		s.Free;
	end;
	if result=0 then result:=InvalidateEntry(worte,id);
	worte.Free;
end;


procedure TVolltext.SetLockFile(createFile:boolean);
var
	h	:	longint;
begin
	if createFile then begin
		h:=FileCreate(dbName+'.LOCK');
		if h>0 then FileClose(h);
		dbLocked:=true;
	end else if dbLocked then begin
		DeleteFile(dbName+'.LOCK');
		dbLocked:=false;
	end;
end;

end.
