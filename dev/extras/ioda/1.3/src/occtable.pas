UNIT OccTable;
{ 
	stellt ein Hilfsobjekt für das Volltextobjekt zur Verfügung.
	Zusammen mit einem Bayerbaum eingesetzt, speichert es alle Vorkommen eines	Strings.
	
	Fehlernummern 300-399
	jo 4/95, portiert nach Linux-fp 6/01, völlig neu gefasst 5/04	
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

{$H-}
INTERFACE
uses
	classes,sysUtils,JStreams,JStrings,IDList;
	
const
	defCluster	=	2;    		               // min. Voll-Einträge/Cluster
	maxCluster	= 8192;                       // max. Voll-Einträge/Cluster

type
{ Bedeutung der Bits 0 und 1: 
		erstes Word (ID bzw. LEN)
			01: ID 
			11: ID mit full flag (wird nur in erster ID gesetzt) 
			10: len flag
		vorletztes Word: (Pos);
			10: last flag 

		Cluster können zwei Zustände haben:
			In VOLLEN Clustern ist das full flag (11 bei erster ID) und das last flag (letzte Pos) gesetzt,
			SONST ist das len-Flag (am Beginn des freien Platzes) gesetzt.
}

	occElType 	= (OID,OMOREDATA,ODATA,OLEN);
	TOccEl =	packed record
		case occElType of								
		   OID	:     (id		:  cardinal);  //  0-3FFFFFFh (1G)
		   OMOREDATA:  (datum,						//  0-FFFFh	  (64K)
		   			    moreInfo:  word);		//  0-FFFFh	  (64K)
		   ODATA :     (info,						//  0-FFFFh	  (64K)
							 pos		:	word);		//  0-3FFFh   (16K)
		   OLEN  :     (clen 	:  cardinal);	//  0-3FFFFFFh (1G)
	end;
	POccEl = ^TOccEl;
	
	TOccA = array[0..maxCluster-1] of TOccEl;
	POccA = ^TOcca;

	TOccFirstCluster = packed record
		last,
		next	:	cardinal;
		occs	:	ToccA					// dynamische Länge
	end;
	POccFirstCluster = ^TOccFirstCluster;
	
	TOccNextCluster =	packed record
		next	:	cardinal;
		occs	:	ToccA					// dynamische Länge
	end;
	POccNextCluster = ^TOccNextCluster;

	TTraverse 	=	procedure(elID,elPos,elDat,elWeight,elInf:cardinal) of object;

//	CClusterMaster	=	class of TClusterMaster;
	TClusterMaster	=	class
		class function CheckoutType(name:string; var size:longint):integer;
		class function Open(name:string; elSize,initSize,growSize:longint; readOnly,enableRecycling:boolean; var res:integer):TClusterMaster;
		constructor Create(const name:string; elSize:longint; readOnly,enableRecycling:boolean; var res:integer);
		destructor  Destroy; OVERRIDE;
		function    InsertRecord(var key:cardinal; size,elID,elPos,elDate,elInfo:cardinal):integer; VIRTUAL; ABSTRACT;
		function    GetItemList(key:cardinal; callback:TTraverse; countOnly:boolean; var res:integer):TDWList; VIRTUAL; ABSTRACT;
		function 	GetItemCount(key:cardinal):longint; VIRTUAL;
		function 	InvalidateRecord(key,fid:cardinal):longint; VIRTUAL; ABSTRACT;
		procedure   Commit; VIRTUAL; 
		procedure   Clear; VIRTUAL; 

		PROTECTED
		firstOcc		:	POccFirstCluster;
		occ,tmpOcc	:	POccNextCluster;
		firstOccSize,
		occSize,
		nextKey,
		occLen,
		fullClusterLen,
		def1stReadLen,
		defContReadLen,
		lenCount,
		dataSum		:	cardinal;
		occStream	:	TStream;
		resList		:	TDWList;
		myFileName	:	string;
		dirty,
		ro,
		recycling	:	boolean;

		function		GetResult(i:integer):TDW;
		function 	GetCount:integer;
		function		GetData(p:PByte; var r:cardinal; min:cardinal):boolean;
		function		GetItem(p:PByte; var last:boolean; var i,r,id,datum,info:cardinal; callback:TTraverse; resi:TDWList):boolean;
		procedure 	SetContReadLen(len:cardinal);
		procedure	WriteHeader;
//		procedure 	CallbackNOP(elID,elPos,elDat,elWeight,elInf:cardinal); // Beispiel eines Callbacks
		
		PUBLIC
		property		Results[i:integer]:TDW read GetResult; DEFAULT; // wenn "caller" der letzten GetItemlist-Operation NIL war
		property		Count:integer read GetCount;
		property		ResultList:TDWList read resList;
	end;

	TClusterMasterD	=	class(TClusterMaster)
		constructor Create(const name:string; elSize:longint; readOnly,enableRecycling:boolean; var res:integer);
		destructor  Destroy; OVERRIDE;
		function    InsertRecord(var key:cardinal; size,elID,elPos,elDate,elInfo:cardinal):integer; OVERRIDE;
		function    GetItemList(key:cardinal; callback:TTraverse; countOnly:boolean; var res:integer):TDWList; OVERRIDE;
		function 	InvalidateRecord(key,fid:cardinal):longint; OVERRIDE;
		
		PROTECTED
		function    NewCluster(size:cardinal; first:boolean):cardinal; VIRTUAL;
	end;

	TClusterMasterM	=	class(TClusterMasterD)
		constructor Create(const name:string; elSize,initSize,growSize:longint; readOnly,enableRecycling:boolean; var res:integer);
		destructor  Destroy; OVERRIDE;
		procedure   Commit; OVERRIDE;
		procedure   Clear;  OVERRIDE;
	end;


IMPLEMENTATION
type
	EFWrongFileType=	class(Exception);

const
	startContReadLen	= 512; 		   			// Anzahl Byte pro weiterem Lesezugriff (erster Zugriff siehe Constructor 'def1stReadLen')
	dwIsID				= $00000001;
	dwIsLen				= $00000002;
	dwIsFull				= $00000002;
	dwIsIDInFullCluster = $00000003;
	wIsID					= $0001;
	wIsLast				= $0002;
	wIsLen				= $0002;
	minKey				=	 512;
	cluDaLen	:	array[boolean] of cardinal = (4,8);
	escMask	:	array[boolean] of cardinal = (dwIsIDInFullCluster,dwIsID);
	cs			:	string[95] = ' (c) jo@magnus.de - OccTable is published under LGPL on http://ioda.sourceforge.net/ '#0;
// Klassenmethoden:

class function TClusterMaster.CheckoutType(name:string; var size:longint):integer;
var
	occStream_	:	TStream;
begin
	result:=0; size:=0;
	if FileExists(name+'.ocl') then begin
		try
			occStream_:=TFileStream.Create(name+'.ocl',fmOpenRead);
			size:=occStream_.ReadDWord;
			occStream_.Free;
		except
			on EFOpenError  do result:=303;
			on EStreamError do result:=304;
		end;
	end;
end;


class function TClusterMaster.Open(name:string; elSize,initSize,growSize:longint; readOnly,enableRecycling:boolean; var res:integer):TClusterMaster;
var
	size	:	longint;
begin
	res:=301; result:=NIL;
	if FileExists(name+'.ocl') then begin
		res:=CheckoutType(name,size);
		if res<>0 then EXIT;
	end else begin
		if readOnly then EXIT;		// es muss eine neue Datei angelegt werden
		size:=abs(elSize)
	end;
	if elSize>=0 then 
		result:=TClusterMasterD.Create(name,size,readOnly,enableRecycling,res)
	else 
		result:=TClusterMasterM.Create(name,size,initSize,growSize,readOnly,enableRecycling,res);
end;


// abstrakte Klasse:

constructor TClusterMaster.Create(const name:string; elSize:longint; readOnly,enableRecycling:boolean; var res:integer);
begin
	inherited Create;
	occStream:=NIL; resList:=NIL; firstOcc:=NIL; occ:=NIL; tmpOcc:=NIL;
	res:=0; myFileName:=name+'.ocl'; ro:=readOnly; dirty:=false;
	try
		if FileExists(myFileName) then begin
			if readOnly then 
				occStream:=TFileStream.Create(myFileName,fmOpenRead)
			else
				occStream:=TFileStream.Create(myFileName,fmOpenReadWrite);
			occLen:=occStream.ReadDWord; 
			nextKey:=occStream.Size; 
			if nextKey<minKey then nextKey:=minKey;
		end else	begin
			if readOnly then raise EFWrongFileType.Create('ReadOnly mode requestet - may not create file!');
			occLen:=abs(elSize);
			if (occLen<>8) and (occLen<>10) and (occLen<>12) then raise EFWrongFileType.Create('Invalid value of elSize (value of "useBigOccList" in config file may be [+/-] 8,10 or 12)');
			occStream:=TFileStream.Create(myFileName,fmCreate);
			WriteHeader;
		end;
		
	except
		on EFWrongFileType do res:=301;
		on EFCreateError do res:=302;
		on EFOpenError do res:=303;
		on EStreamError do res:=304;
	end;
	
	recycling:=enableRecycling; 
	fullClusterLen:=maxCluster*occLen;
	firstOccSize:=8+fullClusterLen;
	occSize:=4+fullClusterLen;
	def1stReadLen:=((512-8) div occLen)*occlen;
	defContReadLen:=startContReadLen;		//	wird im Lauf dynamisch angepasst
	dataSum:=0; lenCount:=0;
	try
		getmem(firstOcc,firstOccSize);
		getmem(occ,occSize);
		getmem(tmpOcc,occSize);
	except
		on EOutOfMemory do
		begin
			if firstOcc<>NIL then freemem(firstOcc,firstOccSize); firstOcc:=NIL;
			if occ<>NIL then freemem(occ,occSize); occ:=NIL;
			res:=-300;
		end;
	end;
end;


destructor TClusterMaster.Destroy;
begin
	freemem(tmpOcc,occSize);
	freemem(occ,occSize);
	freemem(firstOcc,firstOccSize);
	resList.Free;
	occStream.Free;
	inherited Destroy
end;


procedure TClusterMaster.WriteHeader;
var dummy: char;
begin
  dummy:=#0;
	occStream.Seek(0,soFromBeginning);
	occStream.WriteDWord(occLen);
	occStream.Write(cs[1],length(cs));
	occStream.Seek(minKey-1,soFromBeginning);
	occStream.Write(dummy,1);
	nextkey:=minKey;
end;

procedure TClusterMaster.Commit; 
begin end;


procedure TClusterMaster.Clear;
begin 
	with occStream do begin
		size:=0; position:=0;
	end;
	WriteHeader;
end;


function TClusterMaster.GetResult(i:integer):TDW;
begin
	if resList=NIL then
		fillDWord(result,sizeOf(TDW) shr 2,0)
	else	
		result:=resList.Element[i];
end;


function TClusterMaster.GetCount:integer;
begin
	if resList=NIL then result:=0 else result:=resList.Count;
end;


function TClusterMaster.GetData(p:PByte; var r:cardinal; min:cardinal):boolean;
var
	b,n	:	cardinal;
begin
	result:=false;
	if (p=NIL) or (r>=fullClusterLen) then EXIT;
	if r+defContReadLen>=fullClusterLen then b:=fullClusterLen-r else b:=defContReadLen;
	p+=r;
	n:=occStream.Read(p^,b);
	r+=n;
	result:=r>=min;
end;


function TClusterMaster.GetItem(p:PByte; var last:boolean; var i,r,id,datum,info:cardinal; callback:TTraverse; resi:TDWList):boolean;
var
	pos,gewicht,hiInfo	:	cardinal;
begin
	last:=true; result:=false;
	if id=0 then begin
		if (i+occLen>r) and (not GetData(p,r,i+occLen)) then EXIT;		// nicht genug Daten vorhanden
		id:=POccEl(p+i)^.id shr 2;				// flags ausfiltern
		if occLen>8 then begin					// datum & info werden nur beim ersten Wort gespeichert!
			datum:=POccEl(p+i+4)^.datum;
			if occLen>10 then 
				hiInfo:=POccEl(p+i+4)^.moreInfo shl 16 
			else
				hiInfo:=0;
		end else begin
			datum:=0;
			hiInfo:=0;
		end;
		i+=occLen-4;
		pos:=  POccEl(p+i)^.pos shr 2;
		last:= POccEl(p+i)^.pos and wIsLast<>0;
		info:= (POccEl(p+i)^.info) or hiInfo;
		i+=4;
	end else begin
		if (i+2>r) and (not GetData(p,r,i+2)) then EXIT;// nicht genug Daten vorhanden
		pos:=  PWord(p+i)^ shr 2;
		last:= PWord(p+i)^ and wIsLast<>0;
		i+=2;
	end;	

	if pos<500 then gewicht:=500-pos else gewicht:=0;
	if @callback<>NIL then 
		Callback(id,pos,datum,gewicht,info)
	else if resi<>NIL then 
		resi.Add([id,pos,datum,gewicht,info]);

	if i+4>r then last:=last or not GetData(p,r,i+4);
   result:=true
end;


// procedure TClusterMaster.CallbackNOP(elID,elPos,elDat,elWeight,elInf:cardinal); als Beispiel


function TClusterMaster.GetItemCount(key:cardinal):longint;
var
	c	:	longint;
begin
	resList.Free; resList:=NIL;
	GetItemList(key,NIL,true,c);
	result:=c;
end;


procedure TClusterMaster.SetContReadLen(len:cardinal);	// errechnet durchschnittliche Clustergröße
begin
	inc(lenCount); dataSum+=len; 
	if lenCount mod 100=0 then begin
		defContReadLen:=((dataSum div lenCount) shr 9) shl 9;	// in 512-Byte-Blöcken rechnen
		if defContReadLen<512 then 
			defContReadLen:=512 
		else if defContReadLen>8192 then
			defContReadLen:=8192;

		if dataSum>=$F0000000 then begin
			dataSum:=dataSum shr 8; lenCount:=lenCount shr 8
		end;
	end;
end;


// konkrete Klasse: Datenhaltung auf Disk

constructor TClusterMasterD.Create(const name:string; elSize:longint; readOnly,enableRecycling:boolean; var res:integer);
begin
	inherited Create(name,elSize,readOnly,enableRecycling,res);
	if res<>0 then FAIL;	
end;


destructor TClusterMasterD.Destroy;
begin
	inherited Destroy;
end;


function TClusterMasterD.NewCluster(size:cardinal; first:boolean):cardinal;
begin
	result:=nextKey;
	if first then 
		nextkey+=8+occLen*size
	 else
		nextkey+=4+occLen*size;
end;


function TClusterMasterD.InsertRecord(var key:cardinal; size,elID,elPos,elDate,elInfo:cardinal):integer;
var	 														// Result<0 für Fehler, 0 für o.k. 
	idEl,datEl,moreDatEl						:	TOccEl;
	i,j,len,r,lastI,aktKey,tmpKey,
	newKey,nextCluster,lastID,lastInfo	:	cardinal;
	p												:	PByte;
	w												:	word;
	isFull,fullWrite,repeater					:	boolean;

begin
	try
		result:=0; lastI:=0; lastID:=0; fullWrite:=false; dirty:=true;
		size:=size and $3FFF;						// size kann maximal 16383 sein
// Daten zunächst auf drei Record-Varianten (jeweils DWords) verteilen:
		idEl.id:=(elID shl 2) or dwIsID;		// ID-Flag setzen
		with moreDatEl do begin
			datum		:= word(elDate);
			moreInfo := elInfo shr 16;
		end;
		if elPos>$3FFF then elPos:=0;			// keine Wortpositionen > 16383 möglich
		with datEl do begin
			info  	:= elInfo and $FFFF;
			pos   	:= elPos shl 2;			// Platz für Flags schaffen
		end;

		if size>maxCluster then	size:=maxCluster else if size=0 then size:=defCluster;
		if key=0 then begin						// FALL 1: neues Wort, Erstcluster anlegen
			key:=NewCluster(size,true);
			if size=1 then begin
				idEl.id:=idEl.id or dwIsFull;// full flag setzen 
				datEl.pos:=datEl.pos or wIsLast;// last-Flag setzen
			end;
			fillWord(firstOcc^,(8+occLen*size) shr 1,0);
			with firstOcc^ do begin
				occs[0]:=idEL;
				if occLen>8 then occs[1]:=moreDatEl;
				p:=@occs; 
				POccEl(p+occLen-4)^:=datEl;
				if size>1 then PoccEl(p+occLen)^.clen:=(size shl 2) or dwIsLen;	// len mit len flag setzen
			end;
			occStream.Seek(key,soFromBeginning);
			occStream.Write(firstOcc^,8+occLen*size);	// neuen Cluster wegen Dateilänge unbedingt komplett abspeichern
			EXIT											// Fall 1 ist fertig
		end;
		occStream.Seek(key,soFromBeginning);
		occStream.Read(firstOcc^,8+def1stReadLen);// zunächst nur maximal 512 Byte lesen

		if firstOcc^.last=0 then with firstOcc^ do begin
			aktKey:=key;
		  	isFull:=occs[0].id and dwIsIDInFullCluster=dwIsIDInFullCluster; // ID *und* full flag müssen gesetzt sein
			nextCluster:=next;
			p:=@occs;
		end else with occ^ do begin
			aktKey:=firstOcc^.last;				// letzten Knoten laden
			occStream.Seek(aktKey,soFromBeginning);
			occStream.Read(occ^,4+def1stReadLen);
		  	isFull:=occs[0].id and dwIsIDInFullCluster=dwIsIDInFullCluster;
			nextCluster:=next;
			p:=@occs;
		end;
		
		if not isFull then begin
			len:=0; i:=0; r:=def1stReadLen;	// nach dem Ende des teilvollen Cluster suchen
			while ((len=0) and (i<fullClusterLen)) do begin // +4 ^^ muss vorhanden sein, weil Cluster hier nicht voll sein kann (not isFull!)
				if (i+4>r) and (not GetData(p,r,i+4)) then begin result:=-305; EXIT; end;	// sollte nicht vorkommen: zu wenig Daten vorhanden
				w:=PWord(p+i)^ and $0003;		// zum Bit-Test wird nur ein Word benutzt (Flags in lsb und lsb+1)
				if w=wIsID then begin			//	is ID
					lastID:=POccEl(p+i)^.id shr 2;// id merken (falls gleiche ID)
					lastI:=i+occLen-2;			// Zeiger auf letzte pos (Wordptr!) merken (für ev. last flag unten)
					if (i+occLen-4>r) and (not GetData(p,r,i+occLen-4)) then begin result:=-305; EXIT; end;	// sollte nicht vorkommen: zu wenig Daten vorhanden
					lastInfo:=POccEl(p+i+occLen-4)^.info; // info für Vergleich unten merken
					if occLen>10 then lastInfo:=lastInfo or (POccEl(p+i+4)^.moreInfo shl 16);
					i+=occLen						// um occLen (8,10 oder 12 Bytes) weitergehen				
				end else if w=wIsLen then begin// dwIsIDInFullCluster (id+full flag) kommt hier nicht vor (not isFull...)
					len:=POccEl(p+i)^.clen shr 2;// len flag gefunden
					BREAK
				end else begin	
					lastI:=i;						// wie oben letztes datEl für ev. last flag merken
					i+=2;
				end;
			end;
			if (len=0) or (len>maxCluster) or (i>=len*occLen) then begin result:=-306; EXIT; end;	// Fehler: Keine sinnvolle Längenangabe gefunden
			SetContReadLen(len);					// Leselänge per Statistik optimieren
			
			repeater:=(elID=lastID) and (elInfo=lastInfo);
			if repeater then j:=2 else j:=occLen;	// Wiederholung: nur Pos eintragen
			if i+j > len*occLen then begin
				PWord(p+lastI)^:=PWord(p+lastI)^ or wIsLast; // last flag in letzter Pos setzen
				POccEl(p)^.id:=POccEl(p)^.id or dwIsFull; // full flag in erster ID setzen
				POccEl(p+i)^.clen:=0;			// Längen-DWord löschen
				i+=4;
				isFull:=true;						// verbleibender Platz reicht nicht aus - Platz bleibt leer => neuen Cluster anlegen (s.u.)
				fullWrite:=true;						// Record muss komplett abgespeichert werden
			end else begin
				if not repeater then begin
					POccEl(p+i)^:=idEl; 			// Wort aus anderem Text und/oder mit anderer INFO: kompletten Record setzen
					if occLen>8 then POccEl(p+i+4)^:=moreDatEl;
					POccEl(p+i+occLen-4)^:=datEl;
					i+=occLen;
					lastI:=i-2;						// index für mögliches last flag merken					
				end else begin
					PWord(p+i)^:=datEl.pos;		// beim Wiederholer wird nur pos eingetragen (Word = 2 Byte)
					lastI:=i;						// dto.
					i+=2;
				end;

				if i+4<=len*occLen then begin // es verbleibt noch Platz für mindestens einen weiteren (Längen-) Eintrag
					POccEl(p+i)^.clen:=(len shl 2) or dwIsLen; // len flag im ersten noch unbenutzten Eintrag vermerken
					i+=4;								// um Länge der Länge (DWord = 4 Byte) zum Schreiben erhöhen
				end else begin						// sonst im ersten Eintrag das Flag "voll" setzen
					PWord(p+lastI)^:=PWord(p+lastI)^ or wIsLast; 	// last flag setzen
					POccEl(p)^.id:=POccEl(p)^.id or dwIsFull;	// full Flag in der ersten ID setzen. Zum Platz-Optimieren siehe unten ¹)
				end;
				occStream.Seek(aktKey+cluDaLen[aktKey=key],soFromBeginning); // FÄLLE 2+3: freien Platz im Erst- bzw. ContCluster gefunden
				occStream.Write(p^,i);			// Occs abspeichern
				EXIT									// und fertig
			end;
		end;

		if isFull then begin
			// Zum Platz-Optimieren siehe unten ¹)
			if (recycling) and (nextCluster<>0) then begin	// auch in nicht-letzten Clustern soll Platz aus gelöschten Einträgen recycled werden
				if fullWrite then begin					// ggf. zunächst die oben geänderten Daten (full+last flags) speichern
					occStream.Seek(aktKey+cluDaLen[aktKey=key],soFromBeginning);
					occStream.Write(p^,i);		// Records (hier ohne Header) soweit beschrieben abspeichern
					fullWrite:=false;
				end;
				
				while (isFull) and (nextCluster<>0) do begin
					tmpKey:=nextCluster;
					occStream.Seek(tmpKey,soFromBeginning);
					occStream.Read(tmpOcc^,4+occLen); // nur Header und ersten Record lesen
					if tmpOcc^.occs[0].id and dwIsIDInFullCluster<>dwIsIDInFullCluster then
						isFull:=false
					else
						nextCluster:=tmpOcc^.next
				end;
				if isFull then begin
					occ^:=tmpOcc^;					// für next-Verzeigerung unten den letzten Record aus der Kette benutzen!
					aktKey:=tmpKey;
				end else begin
					firstOcc^.last:=nextCluster;	// last-Zeiger auf den oben gefundenen, teil-freien Cluster setzen
					occStream.Seek(key,soFromBeginning);
					occStream.Write(firstOcc^,4); // nur den last-Zeiger speichern und dann Rekursion aufrufen
					result:=InsertRecord(key,size,elID,elPos,elDate,elInfo); // hier läuft es dann auf Fall 3 hinaus
					EXIT								// Ende nach Rekursion
				end;
			end;
			
			newKey:=NewCluster(size,false);
			if aktKey=key then 					// aktueller ist Erstcluster
				firstOcc^.next:=newKey			// also Vorwärtszeiger dort setzen
			else begin								// aktueller (occ @ aktKey) ist Fortsetzungscluster
				occ^.next:=newKey;				// Vorwärtszeiger im Vorgänger-Cluster setzen
				occStream.Seek(aktKey,soFromBeginning);
				if fullWrite then begin
					occStream.Write(occ^,4+i);	// Cluster komplett abspeichern (ggf. oben flags verändert) 
					fullWrite:=false;
				end else
					occStream.Write(occ^,4);	// nur dessen "next"-Feld abspeichern 
			end;

			firstOcc^.last:=newKey;				// neuen last-Zeiger im Erstcluster setzen
			occStream.Seek(key,soFromBeginning);
			if fullWrite then begin
				occStream.Write(firstOcc^,8+i);// Cluster komplett abspeichern (full flag s.o.)
				fullWrite:=false
			end else
				occStream.Write(firstOcc^,8);	// nur dessen "last" und "next"-Felder abspeichern

			if size=1 then begin
				idEl.id:=idEl.id or dwIsFull;// full flag setzen 
				datEl.pos:=datEl.pos or wIsLast;// last-Flag setzen
			end;
			fillWord(occ^,(4+occLen*size) shr 1,0);
			with occ^ do begin
				occs[0]:=idEL;
				if occLen>8 then occs[1]:=moreDatEl;
				p:=@occs; 
				POccEl(p+occLen-4)^:=datEl;
				if size>1 then POccEl(p+occLen)^.clen:=(size shl 2) or dwIsLen;	// len mit len flag setzen
			end;
			occStream.Seek(newKey,soFromBeginning);
			occStream.Write(occ^,4+occLen*size);// neuen Cluster wegen Dateilänge unbedingt komplett abspeichern
		end;												// Fall 4 ist fertig
	except
		on E:Exception do result:=-304;
	end;
end;


function TClusterMasterD.GetItemList(key:cardinal; callback:TTraverse; countOnly:boolean; var res:integer):TDWList;
var
	p												:	PByte;
	i,id,r,aktKey,firstKey,datum,info	:	cardinal;
	l												:	longint;
	n												:	integer;
	last,isFull									:	boolean;
	
begin
	try
		n:=0; firstKey:=key;
		if (@callback<>NIL) or (countOnly) then begin
			resList.Free; resList:=NIL;
		end else begin
			if resList=NIL then resList:=TDWList.Create(maxlongint) else resList.Clear;
		end;
		result:=resList;
		
		while key>0 do begin
			aktKey:=key;
			occStream.Seek(key,soFromBeginning);
			if key=firstKey then with firstocc^ do begin
				l:=occStream.Read(firstOcc^,8+def1stReadLen);
				if l=0 then begin
					res:=-315; EXIT
				end;
				key:=next;
				p:=@occs;
			end else with occ^ do begin
				l:=occStream.Read(occ^,4+def1stReadLen);
				if l=0 then begin
					res:=-315; EXIT
				end;
				key:=next;
				p:=@occs;
			end;
			if key=aktKey then begin
				res:=-316; EXIT
			end;

			i:=0; last:=false; r:=def1stReadLen;
			isFull:=POccel(p)^.id and dwIsIDInFullCluster=dwIsIDInFullCluster;
			while not last do begin
				if (POccel(p+i)^.id and dwIsID<>0) then begin	//	nicht bei len flag (dwIsLen)
					id:=0; 
					repeat
	//write(firstKey:10,id:10);
						if GetItem(p,last,i,r,id,datum,info,callback,resList) then inc(n); 
	//writeln(' => ',id:10,i:10,last:10,aktKey:10,key:10);					
					until (last) or (POccel(p+i)^.id and escMask[isFull]<>0) or (i>fullClusterLen);
					if i>fullClusterLen then begin
						res:=-314; EXIT;			// normales Abbruchkriterium nicht gefunden: Liste inkonsistent!
					end;
				end else 
					BREAK								// len flag ist - wie last flag - ein Abbruchkriterium
			end;
		end;
	except
		on EStreamError do res:=-304;
	end;
	if resList<>NIL then resList.Sort;
	res:=n;
end;


function TClusterMasterD.InvalidateRecord(key,fid:cardinal):longint;
const
	undefStart	=	$FFFFFFFF;
var
	p												:	PByte;
	firstKey,aktKey,id,i,m,datum,info,
	preI,startI,lastI,lastPI,len,r,z		:	cardinal;
	l												:	longint;
	n												:	integer;
	first,last,rpt,isFull,wasFull			:	boolean;

begin
	try
		n:=0; firstKey:=key; result:=0;
		resList.Free; resList:=NIL;				// GetItem speichert keine Ergebnisse

		while key>0 do begin
			aktKey:=key;
			occStream.Seek(key,soFromBeginning);
			if key=firstKey then with firstocc^ do begin
				l:=occStream.Read(firstOcc^,8+def1stReadLen);
				if l=0 then begin result:=-317; EXIT; end;
				key:=next;
				p:=@occs;
			end else with occ^ do begin
				l:=occStream.Read(occ^,4+def1stReadLen);
				if l=0 then begin	result:=-318; EXIT; end;
				key:=next;
				p:=@occs;
			end;
			if key=aktKey then begin
				result:=-319; EXIT
			end;

			z:=undefStart; r:=def1stReadLen; 
			wasFull:=POccel(p)^.id and dwIsIDInFullCluster=dwIsIDInFullCluster; 
			repeat
				i:=0; startI:=undefStart; lastI:=0; lastPI:=0; len:=0; last:=false; rpt:=false; 
				isFull:=POccel(p)^.id and dwIsIDInFullCluster=dwIsIDInFullCluster; 
				repeat			
					if POccel(p+i)^.id and dwIsID<>0 then begin	//	nicht bei len flag (dwIsIDInFullCluster)
						id:=0; 
						repeat
							first:=id=0; preI:=i;
							GetItem(p,last,i,r,id,datum,info,NIL,NIL);	
							if first then begin
								if id=fid then begin						
									if startI<>undefStart then begin	// ID-Wiederholung, dieser Fall kann nur nach							
										rpt:=true; 							// nachträglichem Verschieben im Cluster eintreten
										if lastI=0 then lastI:=preI;
									end else begin
										startI:=preI; inc(n);
									end
								end else if (startI<>undefStart) and (lastI=0) then 
									lastI:=preI;
							end else if (startI<>undefStart) and (lastI=0) then
								inc(n);

							if last then begin
								if not isFull then begin result:=-311; EXIT; end;	// Gewicht darf nicht in teil-leeren Cluster vorkommen
								lastPI:=i-2; 
								len:=(i+occLen-1) div occLen;
							end;
						until last or (POccel(p+i)^.id and escMask[isFull]<>0) or (i>fullClusterLen);
						if i>fullClusterLen then begin
							result:=-314; EXIT;		// kein Abbruchkriterium gefunden: Liste inkonsistent!
						end;
					end else begin
						if isFull then begin	result:=-313; EXIT; end;	// Länge darf nicht in vollem Cluster vorkommen
						len:=POccel(p+i)^.clen shr 2;
						last:=true						// len flag ist - wie last flag - ein Abbruchkriterium
					end;
				until last;

				if startI<>undefStart then begin
					if (len=0) or (len>maxCluster) then begin
						result:=-312; EXIT 				// keine (gültige) Länge gefunden bzw. errechnet
					end;
					if lastI=0 then lastI:=i; m:=0;
					if lastI>=i then						// zu löschende ID liegt am Ende des Clusters (rpt niemals true!)
						z:=startI
					else begin								// last flag löschen und Daten verschieben
						if lastPI<>0 then PWord(p+lastPI)^:=PWord(p+lastPI)^ and $FFFD; // last flag löschen
						m:=i-lastI;
						try
							move(POccel(p+lastI)^,POccel(p+startI)^,m);
						except
							result:=-321; EXIT;
						end;
						z:=startI+m;
					end;
					if z>0 then POccel(p)^.id:=POccel(p)^.id and $FFFFFFFD; // full flag (10b=2) löschen
					POccel(p+z)^.clen:=(len shl 2) or dwIsLen;	// neue Länge (als Endemarke) eintragen
				end;
			until not rpt;

			if z<>undefStart then begin
				occStream.Seek(aktKey+cluDaLen[aktKey=firstKey],soFromBeginning);
				occStream.Write(p^,z+4);
				if (recycling) and (wasFull) and (firstOcc^.last>aktKey) then begin // last-Zeiger des ersten Clusters auf akt. Cluster (zum späteren "Nachfüllen") setzen
					if aktKey=firstKey then	firstOcc^.last:=0 else firstOcc^.last:=aktKey;
					occStream.Seek(firstKey,soFromBeginning);
					occStream.Write(firstOcc^,4);
				end;
			end;
		end;
	except
		on E:Exception do begin writeln(' EX=',E.message,' in occtable.InvalidateEntry'); result:=-321; end;
		on EStreamError do result:=-309;
	end;
	dirty:=dirty or (n>0);
	result:=n;
end;


// Datenhaltung im Arbeitsspeicher (nach Programmschluss oder "Commit" auf Disk zurückgeschrieben)

constructor TClusterMasterM.Create(const name:string; elSize,initSize,growSize:longint; readOnly,enableRecycling:boolean; var res:integer);
begin
	inherited Create(name,elSize,readOnly,enableRecycling,res);
	if res<>0 then EXIT;
	occStream.Free;						// TFileStream freigeben
	occStream:=TLargeMemoryStream.Create(0,growSize);	// als TLargeMemoryStream neu anlegen
	with occStream as TLargeMemoryStream do LoadFromFile(myFileName); 
	if (initSize>occStream.Size) and (not ro) then with occStream as TLargeMemoryStream do SetSize(initSize); // erst hier sinnvoll, weil LoadFromFile die Capacity einstellt
end;


destructor TClusterMasterM.Destroy;
begin
	Commit;
	inherited Destroy;
end;


procedure TClusterMasterM.Clear;
begin
	with occStream as TLargeMemoryStream do Clear;
	inherited Clear;
	dirty:=true;
end;


procedure TClusterMasterM.Commit;
var
	store	:	TFileStream;
	n		:	cardinal;
begin
	if ro or (not dirty) or (occStream=NIL) then EXIT;
	occStream.Seek(0,soFromBeginning);
	store:=TFileStream.Create(myFileName,fmCreate);
	if occStream.Size < nextKey then n:=occStream.Size else n:=nextKey;
	store.CopyFrom(occStream,n);
	store.Free;
	dirty:=false;
end;

end.

//	¹) Nach dem Einfügen ist das Platz optimieren wg. nachträglicher Löschungen und damit verbundener 
// Verschiebungen nicht mehr erlaubt! Der Code war:
//	if (nextKey-(aktKey+cluDaLen[aktKey=key]+i) < occLen+4) then nextKey:=aktKey+cluDaLen[aktKey=key]+i; 
