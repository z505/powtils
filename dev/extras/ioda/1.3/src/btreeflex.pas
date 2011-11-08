(****************************************************************************)
(*                                                                          *)
(*                               Top-Down-B-Baum                            *)
(*                                                                          *)
(*						    				by jo@magnus.de                            *)
(*																									 *)
(*										  DOS-Version 11/92									 *)
(*                           Windows-Portierung 9/94                        *)
(*								    Linux-FP-Portierung 6/01								 *)
(*								  Dynamische Stringlängen 1/05							 *)
(*							   Wildcards und RegExpressions 6/05 						 *)
(*																									 *)
(*	     							  Fehlermeldungen 100-299		       				 *)
(*																									 *)
(****************************************************************************)

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

UNIT BTreeFlex;
{$H-}
INTERFACE
uses
	Classes,SysUtils,IdList;

const
	NODESIZE		=	2048;								// gewünschte Knotengröße
	MAXVALCOUNT	=	NODESIZE div 16;				// max. Anzahl Zeiger pro Knoten (je 16 Bytes)

type
	TIOBuf	=	packed array[0..NODESIZE-1-16] of byte; // Platz für Header abziehen
	TValArr	=	packed array [0..MAXVALCOUNT-1] of PVal;

	TNode		=  packed record
						key,
						counter,
						size,
						r		:	cardinal;			//   16 Bytes
						buf	:	TIOBuf;				// 8176 Bytes * Bis hier hin wird 1:1 auf Medium gespeichert *
						val	:	TValArr;				// Zeigern auf die Elemente in buf (MAXVALCOUNT*4 Bytes)
					end;
	PNode		=	^TNode;

	TCaller	=	procedure(const akt:TVal) of object;
	
	TBayBaum	=	class
							constructor Create;
							destructor  Destroy; OVERRIDE;
							
							// Inserts a new word
							function 	Insert(newStr:string; dataptr:cardinal):boolean;
							// Updates dataptr and/or counter for an existing word
							function 	Update(s:string; dataptr:cardinal; counter,counterMode:longint):boolean;
							// Searches a word and returns the words meta information
							function    SearchWord(s:string; var inf,dataptr:cardinal):boolean;
							// Counts occurencies of word(s) if strict>0. Returns a guess for strict=0 and a long guess for strict<0
							function    HowMany(s:string; strict:shortint):longint;

							// results are returned by callback (except MultipleSearch) OR internal result list
							// STRICT search i.e. foo
							function    GetEqual(s:string; Caller:TCaller; maxHits:cardinal):TResList;
							function    GetEqual(s:string; Caller:TCaller):TResList;
							// HEAD search: head of search item has to match, i.e. foo*, best performance
							function    GetAlike(s:string; Caller:TCaller; maxHits:cardinal):TResList;
							function    GetAlike(s:string; Caller:TCaller):TResList;
							// TAIL search: tail of search item has to match, i.e. *foo, high performance
							function 	GetTail(s:string; Caller:TCaller; maxHits:cardinal):TResList;
							function 	GetTail(s:string; Caller:TCaller):TResList;
							// SUBSTRING search: search item has to be at least a substring, i.e. *foo*, lower performance
							function    GetSub(s:string; Caller:TCaller; maxHits:cardinal):TResList;
							function    GetSub(s:string; Caller:TCaller):TResList;
							// REGEX AFTER HEAD search: uses a regex after a given literal head of the search item , i.e. /^foo.+/
							// performance depends on the length of the head: high to low
							function 	GetAlikeRegEx(s,preS:string; Caller:TCaller; maxHits:cardinal):TResList;
							function 	GetAlikeRegEx(s,preS:string; Caller:TCaller):TResList;
							// REGEX search, i.e.  /foo.+/, relativly the lowest performance of all search methods
							function    GetRegEx(s:string; Caller:TCaller; maxHits:cardinal):TResList;
							function    GetRegEx(s:string; Caller:TCaller):TResList;
							// ALL IN ONE PASS: Get results for some search items given in a TStringList
							// where strict < 0 (tail, substring or regex search. NOT used for regex after head search!)
							// PParsEl records are assigned to the lists string items
							function    MultipleSearch(sl:TStringList; maxHits:cardinal):TResList;
							function   	MultipleSearch(sl:TStringList):TResList;
							// Get all words (sorted output of the complete tree)
							function    GetAll(Caller:TCaller; maxHits:cardinal):TResList;
							function    GetAll(Caller:TCaller):TResList;

							// Helper for unsorting the internal result list (for a better [unsorted] building of a new btree)
							procedure 	Unsort;
							// Writes back memory stream if it is dirty
							procedure   Commit; VIRTUAL; ABSTRACT;
							// Clears ALL data
							procedure   Clear;  VIRTUAL; ABSTRACT;
							// Switches temporary memory stream on (meaningless for TMemBayBaum)
							// improves much better IO for following MultipleSearch or GetAll/GetSub/GetTail-Operations!
							procedure 	CacheOn;  VIRTUAL; ABSTRACT;
							// Switches temporary memory stream off if used (meaningless for TMemBayBaum)
							procedure 	CacheOff; VIRTUAL; ABSTRACT;

							PROTECTED
							root		:	TNode;
							resList	:	TResList;
							top,
							wordCounter,
							allCounter,
							nodeCounter:cardinal;
							bayError	:	integer;
							dirty,
							multiple,
							stopped	:	boolean;

							function    TellError:integer;
							function    TellQuality:integer;
							function		CreateNode(var el:PNode):boolean;
							procedure	CopyNode(const elSrc:TNode; var elTarget:TNode);
							procedure   ClearNodeContent(var el:TNode);
							procedure 	MoveNodeEl(var el:TNode; i,len:integer);
							function    bSearch(s:string; alike:boolean; var el:TNode; var i:integer):integer;
							function    Split(var prev,akt:TNode):boolean;
							function    Search(s:string; dataptr:cardinal; alike:boolean; var el:TNode; var n:integer):boolean;
							procedure 	AddResult(const val:TVal);
							function		GetResult(i:integer):TVal;
							function 	GetCount:integer;
							function    GetElem(var el:TNode; id:cardinal):boolean; VIRTUAL; ABSTRACT;
							function    SetElem(var el:TNode):boolean; VIRTUAL; ABSTRACT;

							PUBLIC
							property		Results[i:integer]:TVal read GetResult; DEFAULT;
							//				^^ wenn "caller" der letzten GetXX/TraverseXX-Operation NIL war
							property		Count:integer read GetCount;
							property		ResultList:TResList read resList;
							property    Error:integer read tellError;
							property		Stopp:boolean write stopped;
							property		WordCount:cardinal read wordCounter;
							property    AllCount:cardinal read allCounter;
							property	   NodeCount:cardinal read nodeCounter;
							property		Quality:integer read TellQuality;
					end;

	TFileBayBaum=class(TBayBaum)
							constructor Create(const name:string; readOnly:boolean; var res:integer);
							destructor  Destroy; OVERRIDE;
							procedure   Commit; OVERRIDE;
							procedure   Clear;  OVERRIDE;
							procedure 	CacheOn;  OVERRIDE;
							procedure 	CacheOff; OVERRIDE;
							
							PROTECTED
							elStream		:	TStream;
							myFileName	:	string;
							ro,roSave	:	boolean;
							
							function    GetElem(var el:TNode;id:cardinal):boolean; OVERRIDE;
							function    SetElem(var el:TNode):boolean; OVERRIDE;
							procedure	WriteHeader;
					end;
						
	TMemBayBaum=class(TFileBayBaum)
							constructor Create(const name:string; initSize,growSize:longint; readOnly:boolean; var res:integer);
							destructor  Destroy;  OVERRIDE;
							procedure   Commit;   OVERRIDE;
							procedure   Clear;    OVERRIDE;
							procedure 	CacheOn;  OVERRIDE;
							procedure 	CacheOff; OVERRIDE;
					end;
				
	
IMPLEMENTATION
uses
	JStreams,RegExpr;
	
const
	tNodeHeaderSize = 16;								// Größe des Headers von TNode (= TNode ohne Puffer und Zeigerarray)
	tNodeSize=  tNodeHeaderSize+sizeof(TIOBuf);	// effektive Knotengröße (ohne Zeigerarray) DIES WIRD GESPEICHERT & GELESEN!
	rootPos	=  512;
	cs		:	string[95] = ' (c) jo@magnus.de - BTreeFlex is published under LGPL on http://ioda.sourceforge.net/ '#0;



constructor TBayBaum.Create;
begin
	inherited Create;
	wordCounter:=0; nodeCounter:=1; allCounter:=0; dirty:=false;
	top:=rootPos; bayError:=0; multiple:=false; // "multipleItems" is not yet implemented!
	fillDWord(root,sizeOf(TNode) shr 2,0);
	resList:=TResList.Create;
end;


destructor TBayBaum.Destroy;
begin
	resList.Free;
	inherited Destroy;
end;


function TBayBaum.CreateNode(var el:PNode):boolean;
begin
	try	
		new(el); 
	except 
		on EOutOfMemory do begin 	
			el:=NIL; result:=false; EXIT
		end;
	end;
	with el^ do begin 
		key:=0; counter:=0; size:=0; r:=0; 
	end;
	result:=true
end;


procedure TBayBaum.CopyNode(const elSrc:TNode; var elTarget:TNode);
var
	i,delta	:	integer;
begin
	delta:=integer(@elTarget.buf-@elSrc.buf);	// Delta der Basisaddresse beider Datenblöcke (buf)
	if delta=0 then EXIT;							//	Ziel und Quelle sind gleich, fertig
	move(elSrc,elTarget,tNodeSize);
	for i:=0 to elSrc.counter-1 do       		// Zeiger in den Datenblock der Kopie neu zuweisen
      elTarget.val[i]:=PVal(PByte(elSrc.val[i])+delta);
end;


procedure TBayBaum.ClearNodeContent(var el:TNode);
begin
	with el do begin
		key:=0; counter:=0; size:=0; r:=0;
	end;
end;


procedure TBayBaum.MoveNodeEl(var el:TNode; i,len:integer);
var
	j,n:	integer;
begin
   n:=el.size-cardinal(PByte(el.val[i])-@el.buf);
 	move(el.val[i]^,PVal(PByte(el.val[i])+len)^,n);		 	 // Daten im Puffer (buf) verschieben
	move(el.val[i],el.val[i+1],(el.counter-cardinal(i))*4);// Zeiger um eins nach recht
	for j:=i+1 to el.counter do el.val[j]:=PVal(PByte(el.val[j])+len); // Zeiger darauf anpassen, Platz für neuen Zeiger schaffen:
end;			


function TBayBaum.bSearch(s:string; alike:boolean; var el:TNode; var i:integer):integer;
var
	lo,hi	:	integer;
	t		:	shortString;
	found	:	boolean;
	
begin
	result:=0;
	if el.counter=0 then begin
		i:=0; inc(result); EXIT;
	end;

	if s>el.val[el.counter-1]^.s then begin
		i:=el.counter; inc(result); EXIT;
	end;
	
	lo:=0; hi:=el.counter-1; found:=false;
	if alike then 
		while hi>=lo do begin
     		i:=(lo+hi) div 2;
			t:=copy(el.val[i]^.s,1,length(s));
			if s>t then begin
				if i+1<integer(el.counter) then
					lo:=i+1 
				else
					BREAK
			end else if s<t then 
				hi:=i-1 
     		else begin
				found:=true; BREAK
			end
		end
	else
		while hi>=lo do begin
     		i:=(lo+hi) div 2;
			t:=el.val[i]^.s;
			if s>t then begin
				if i+1<integer(el.counter) then
					lo:=i+1 
				else
					BREAK
			end else if s<t then 
				hi:=i-1 
     		else begin
				EXIT;										// MULTIPLE is not yet implemented
//				if not multiple then EXIT;
//				found:=true; BREAK
			end
		end;
	
	if found then 
		while i>0 do begin
			if alike then t:=copy(el.val[i-1]^.s,1,length(s)) else t:=el.val[i-1]^.s;
			if t<>s then EXIT;
			dec(i);
		end
	else begin
		if s>t then inc(i);
		dec(result);
	end;
end;


function TBayBaum.Split(var prev,akt:TNode):boolean;
var
	neu	 		:	PNode;
	l,len			:	cardinal;
  	c1,c2,i,j	:  integer;

begin
	result:=false;
	if not CreateNode(neu) then begin 
		bayError:=101; EXIT; 
	end;
	fillDWord(neu^,sizeof(TNode) shr 4,0);
	
	c1:=akt.counter div 2;
	if akt.counter mod 2=0 then c2:=c1-1 else c2:=c1;
	neu^.key:=top;
	top+=tNodeSize; inc(nodeCounter);
	len:=0;

	for i:=0 to c2-1 do begin						// neu ist der rechte (größere) Teilknoten 	
		j:=i+c1+1;
		l:=tValSizeN+length(akt.val[j]^.s);
      neu^.val[i]:=PVal(@neu^.buf+len);		// Zeiger für neues TVal ans Ende des Puffers (buf) legen
		move(akt.val[j]^,neu^.val[i]^,l);		// rechte Hälfte von AKT -> NEU umladen (nur l Bytes zuweisen)
		akt.val[j]:=NIL;
		len+=l;
	end;

	neu^.size:=len; 
	neu^.counter:=c2;
	neu^.r:=akt.r; 
	akt.size-=len;
	akt.counter:=c1;
	akt.r:=0;
	len:=tValSizeN+length(akt.val[c1]^.s);		// Länge des zu verschiebenden Elementes
	
	if akt.key=rootPos then begin					// =PREV=AKT=Sonderfall: root ist voll
		akt.key:=top;									// neue Adresse für alten root
		top+=tNodeSize; inc(nodeCounter);
		ClearNodeContent(prev);
		with prev do begin
			key:=rootPos;								// PREV wird neuer root
			r:=neu^.key; 
		end;
		i:=0;                            		// für Behandlung unten
	end else												// Normalfall: Mitte von AKT nach oben reichen
//		if (prev.size+len>sizeof(TIOBuf)) or (i>=MAXVALCOUNT) then ERROR
		case bSearch(akt.val[c1]^.s,false,prev,i) of
		  -1,0 :	begin                         // links oder drin einfügen
						prev.val[i]^.l:=neu^.key;	// neuen Knoten links anbinden
						MoveNodeEl(prev,i,len);		// Daten in buf und Zeiger darauf um eins nach rechts verschieben 
					end;
			1 :  	begin
						prev.r:=neu^.key;				// rechts anfügen
						prev.val[i]:=PVal(@prev.buf+prev.size);
					end;
		end;

	move(akt.val[c1]^,prev.val[i]^,len);		// keine direkte Zuweisung wg. voller Recordlänge des TVal!
	prev.val[i]^.l:=akt.key;
	prev.size+=len; 
	inc(prev.counter);
	akt.size-=len;										// akt.counter bereits oben korrekt gesetzt
	akt.r:=akt.val[c1]^.l;
	akt.val[c1]:=NIL;
	result:=SetElem(prev) and SetElem(akt) and SetElem(neu^);
	dispose(neu); 
end;


function TBayBaum.Insert(newStr:string; dataptr:cardinal):boolean;
{ INSERT sucht ein passendes Blatt des Baumes und sortiert dort das neue
  Element ein. Werden während der Suche volle Knoten bemerkt, so ruft sich
  INSERT nach der Aufspalteprozedur SPLIT rekursiv wieder auf (TOP-DOWN-BBAUM). }
var
	akt,prev	:	PNode;
	len		:	cardinal;
	i,zwi		:	integer;
	found,ok	:	boolean;

begin
	result:=false; 
	akt:=NIL; prev:=NIL;

	if not (CreateNode(akt) and CreateNode(prev)) then begin 
		if akt<>NIL then dispose(akt);
		bayError:=101; EXIT; 
	end;

	GetElem(akt^,rootPos);
	CopyNode(akt^,prev^);
	i:=0;	
	found:=false; ok:=true; 
	if length(newStr)+1>=sizeOf(TBayStr) then newStr[0]:=char(sizeOf(TBayStr)-1);
	len:=tValSizeN+length(newStr);
	
	repeat
		if (akt^.counter>=MAXVALCOUNT) or (akt^.size+tNodeHeaderSize+sizeof(TVal)>=tNodeSize) then begin
			ok:=Split(prev^,akt^);					// es muss immer Platz für ^^ einen vollständigen Eintrag bleiben!
			if not ok then bayError:=104;
			dispose(prev); dispose(akt);
			result:=ok and Insert(newStr,dataptr);
			EXIT;											// Rekursion nach SPLIT
		end;
		CopyNode(akt^,prev^);

		zwi:=bSearch(newStr,false,akt^,i);
		if (zwi=0) and (not multiple) then begin
			with akt^.val[i]^ do begin
				inc(z);
				if dp=0 then dp:=dataptr; 
			end;
			if SetElem(akt^) then begin
				inc(allCounter); result:=true;
			end;
			dispose(prev); dispose(akt); EXIT;
		end;
		
		if zwi<=0 then begin
			if akt^.val[i]^.l<>0 then begin
				ok:=GetElem(akt^,akt^.val[i]^.l);
			end else begin
				found:=true;							// Elemente im Blatt verschieben
				MoveNodeEl(akt^,i,len);
			end;
		end else begin
			if akt^.r<>0 then
				ok:=GetElem(akt^,akt^.r)
			else begin									// auch: leeres Blatt!
				found:=true;                     // neues Element wird letztes (größtes) im Blatt
				akt^.val[i]:=PVal(@akt^.buf+integer(akt^.size)); // neuem Zeiger Platz in buf zuweisen
			end;
		end;
	until found or not ok;

	if ok then begin
		with akt^.val[i]^ do begin
			l			:= 0;
			z			:= 1; 
			dp			:= dataptr; 
			move(newStr,s,length(newStr)+1);		// nicht den String zuweisen, sondern nur length+1!
		end;
		with akt^ do begin
			inc(counter);
			size+=len;
		end;

		if SetElem(akt^) then
		begin
			inc(wordCounter); inc(allCounter);
		end else begin
			ok:=false; if bayError=0 then bayError:=103;
		end;
	end;
	dispose(prev); dispose(akt);
	result:=ok;
end;



function TBayBaum.Search(s:string; dataptr:cardinal; alike:boolean; var el:TNode; var n:integer):boolean;
// Suche AB el^.key nach einem Element mit BESTIMMTEM dataptr, falls dataptr<>0
var
	found	:	boolean;


	procedure Visit(id:cardinal; r:integer);
	var
		akt	:	PNode;
		i		:	integer;

	begin
		if (id=0) or (r>1024) then EXIT;
		if not CreateNode(akt) then begin 
			bayError:=101; EXIT; 
		end;

		if GetElem(akt^,id) and (akt^.counter>0) then
			case bSearch(s,alike,akt^,i) of
			 	0 : if ((dataptr=0) or (akt^.val[i]^.dp=dataptr)) then begin
						CopyNode(akt^,el); n:=i; found:=true;
					 end;
			  -1:  Visit(akt^.val[i]^.l,r+1);
			   1:  Visit(akt^.r,r+1);
			end;
		dispose(akt);
	end;


begin
	found:=false;
	Visit(el.key,0);
	result:=found;
end;


function TBayBaum.SearchWord(s:string; var inf,dataptr:cardinal):boolean;
var
	el			:	PNode;
	i			:	integer;
begin
	result:=false;
	inf:=0; dataptr:=0;
	if not CreateNode(el) then begin 
		bayError:=101; EXIT; 
	end;

	if length(s)>=sizeOf(TBayStr)-1 then s[0]:=char(sizeOf(TBayStr)-1);
	el^.key:=rootPos;
	if Search(s,0,false,el^,i) then begin
		inf:=el^.val[i]^.z;
		dataptr:=el^.val[i]^.dp;
		result:=true;
	end;

   dispose(el);
end;


function TBayBaum.HowMany(s:string; strict:shortint):longint;
var
	found	:	longint;


	procedure Visit(id:longint);
	var
		akt	:	PNode;
		t		:	string;
      i     :  integer;
		n	   :	cardinal;
		ok		:	byte;

	begin
		if id=0 then EXIT;
		if not CreateNode(akt) then begin 
			bayError:=101; EXIT; 
		end;

		if GetElem(akt^,id) and (akt^.counter>0) then
			case bsearch(s,strict<2,akt^,i) of
			  0 : begin
						ok:=0; n:=i;
						repeat
							if strict<2 then begin
								t:=copy(akt^.val[i]^.s,1,length(s));
								if s<>t then inc(ok);
							end;
							inc(found,akt^.val[i]^.z);
							inc(i);
						until (strict=2) or (cardinal(i)>=akt^.counter) or (ok=2);

						if strict<2 then begin
							Visit(akt^.val[n]^.l);
							Visit(akt^.r);
						end;
				 end;
			 -1 : Visit(akt^.val[i]^.l);
			  1 : Visit(akt^.r);
			end;

		dispose(akt);
	end;


begin
	found:=0;
	if strict<0 then begin							// Werte dienen nur der relativen Gewichtung
		case strict of 
			-3 :	result:=wordCounter shr 3; 	// RegEx   ¹/8
			-2 :  result:=wordCounter shr 6;		// GetSub  ¹/64
			-1	:	result:=wordCounter shr 7; 	// GetTail ¹/128
		end;
	end else begin
		Visit(rootPos);
		if ((strict=0) and (length(s)<5)) then found:=found shl (5-length(s));
		result:=found;
	end;
end;


procedure TBayBaum.AddResult(const val:TVal);
begin
	resList.Add(val);
end;


function TBayBaum.GetResult(i:integer):TVal;
begin
	if i<resList.count then
		result:=resList[i]
	else begin
		bayError:=-106; result.s:='';
	end;
end;


function TBayBaum.GetCount:integer;
begin
	result:=resList.Count;
end;


procedure TBayBaum.Unsort;
begin
	resList.Unsort
end;


function TBayBaum.GetEqual(s:string; Caller:TCaller; maxHits:cardinal):TResList;
var
	c	:	cardinal;


	procedure Visit(id:cardinal);				{ Inorder-Traversierung }
	var
		akt	:	PNode;
		i		:	integer;

	begin
		if (id=0) or (c>maxHits) or stopped then EXIT;
		if not CreateNode(akt) then begin 
			bayError:=101; EXIT; 
		end;

		if GetElem(akt^,id) and (akt^.counter>0) then
			 case bSearch(s,false,akt^,i) of
			  0 	:	begin 
			  				Caller(akt^.val[i]^); inc(c); 
						end;
			 -1 	: 	Visit(akt^.val[i]^.l);
			  1	: 	Visit(akt^.r);
			end;

		dispose(akt);
	end;

begin
	stopped:=false; c:=0; resList.Clear;
	if @caller=NIL then caller:=@AddResult;
	Visit(rootPos);
	result:=resList;
end;


function TBayBaum.GetEqual(s:string; Caller:TCaller):TResList; overload;
begin
	result:=GetEqual(s,Caller,$100000)
end;


function TBayBaum.GetAlike(s:string; Caller:TCaller; maxHits:cardinal):TResList;
var
	c	:	cardinal;


	procedure Visit(id:cardinal);				{ Inorder-Traversierung }
	var
		akt	:	PNode;
		t		:	TBayStr;
		i  	:	integer;
		ok 	:	byte;

	begin
		if (id=0) or (c>maxHits) or stopped then EXIT;
		if not CreateNode(akt) then begin 
			bayError:=101; EXIT; 
		end;

		if GetElem(akt^,id) and (akt^.counter>0) then
			case bsearch(s,true,akt^,i) of
			 0 :	begin
					ok:=0; t:='';
					repeat
						Visit(akt^.val[i]^.l);
						if ok=0 then begin
							Caller(akt^.val[i]^); inc(c);
						end;
						inc(i);
						if cardinal(i)<akt^.counter then t:=copy(akt^.val[i]^.s,1,length(s));
						if s<>t then inc(ok);
					until (cardinal(i)>=akt^.counter) or (c>maxHits) or (ok=2);
					Visit(akt^.r);			{ wegen gleichen Einträgen infolge Split }
				end;
			 -1: Visit(akt^.val[i]^.l);
			  1: Visit(akt^.r);
			end;

		dispose(akt);
	end;

begin
	stopped:=false; c:=0; resList.Clear;
	if @caller=NIL then caller:=@AddResult;
	Visit(rootPos);
	result:=resList;
end;


function TBayBaum.GetAlike(s:string; Caller:TCaller):TResList; overload;
begin
	result:=GetAlike(s,Caller,$100000);
end;


function TBayBaum.GetSub(s:string; Caller:TCaller; maxHits:cardinal):TResList;
var
	c		:	cardinal;


	procedure Visit(id:cardinal);				// Inorder-Traversierung mit einfacher Substring-Suche
	var
		akt	:	PNode;
		i		:	integer;

	begin
		if (id=0) or (c>maxHits) or stopped then EXIT;
		if not CreateNode(akt) then begin 
			bayError:=101; EXIT; 
		end;

		if GetElem(akt^,id) and (akt^.counter>0) then begin
			for i:=0 to akt^.counter-1 do begin
				Visit(akt^.val[i]^.l);
				if system.pos(s,akt^.val[i]^.s)>0 then begin
					Caller(akt^.val[i]^);
					inc(c);
				end;
			end;
			Visit(akt^.r);
		end;
		dispose(akt);
	end;



begin
	stopped:=false; c:=0; resList.Clear;
	if @caller=NIL then caller:=@AddResult;
	Visit(rootPos);
	result:=resList;
end;


function TBayBaum.GetSub(s:string; Caller:TCaller):TResList; overload;
begin
	result:=GetSub(s,Caller,$100000);
end;


function TBayBaum.GetTail(s:string; Caller:TCaller; maxHits:cardinal):TResList;
var
	c		:	cardinal;
	len	:	integer;
	

	procedure Visit(id:cardinal);				// Inorder-Traversierung mit einfacher Substring-Suche
	var
		akt	:	PNode;
		i,p	:	integer;

	begin
		if (id=0) or (c>maxHits) or stopped then EXIT;
		if not CreateNode(akt) then begin 
			bayError:=101; EXIT; 
		end;

		if GetElem(akt^,id) and (akt^.counter>0) then begin
			for i:=0 to akt^.counter-1 do begin
				Visit(akt^.val[i]^.l);
				p:=system.pos(s,akt^.val[i]^.s);
				if ((p>0) and (p=length(akt^.val[i]^.s)-len)) then begin
					Caller(akt^.val[i]^);
					inc(c);
				end;
			end;
			Visit(akt^.r);
		end;
		dispose(akt);
	end;



begin
	stopped:=false; c:=0; resList.Clear;
	len:=length(s)-1;
	if @caller=NIL then caller:=@AddResult;
	Visit(rootPos);
	result:=resList;
end;


function TBayBaum.GetTail(s:string; Caller:TCaller):TResList; overload;
begin
	result:=GetTail(s,Caller,$100000);
end;


function TBayBaum.GetAlikeRegEx(s,preS:string; Caller:TCaller; maxHits:cardinal):TResList;
var
	c		:	cardinal;
	n		:	integer;
	regEx	:	TRegExpr;
	el		:	PNode;
	

	procedure Visit(id:cardinal; n:longint);				{ Inorder-Traversierung }
	var
		akt	:	PNode;
		t		:	TBayStr;
		i  	:	integer;
		ok 	:	byte;

	begin
		if (id=0) or (c>maxHits) or stopped then EXIT;
		if not CreateNode(akt) then begin 
			bayError:=101; EXIT; 
		end;

		if GetElem(akt^,id) and (akt^.counter>0) then
			if n<0 then 
				n:=bsearch(preS,true,akt^,i)
			else begin
				i:=n; n:=0;
			end;
			
			case n of
			 0 :	begin
					ok:=0; t:='';
					repeat
						Visit(akt^.val[i]^.l,-1);
						if ((ok=0) and (regEx.Exec(akt^.val[i]^.s))) then begin
							Caller(akt^.val[i]^); inc(c);
						end;
						inc(i);
						if cardinal(i)<akt^.counter then t:=copy(akt^.val[i]^.s,1,length(preS));
						if preS<>t then inc(ok);
					until (cardinal(i)>=akt^.counter) or (c>maxHits) or (ok=2);
					Visit(akt^.r,-1);			// wegen gleichen Einträgen infolge Split
				end;
			 -1: Visit(akt^.val[i]^.l,-1);
			  1: Visit(akt^.r,-1);
			end;

		dispose(akt);
	end;


begin
	stopped:=false; c:=0; resList.Clear;
	if @caller=NIL then caller:=@AddResult;
	regEx:=TRegExpr.Create;
	regEx.Expression:=s;
	if not CreateNode(el) then begin 
		bayError:=101; EXIT; 
	end;
			
	el^.key:=rootPos;
	if Search(preS,0,true,el^,n) then Visit(el^.key,n);
	dispose(el);
	regEx.Free;
	result:=resList;
end;


function TBayBaum.GetAlikeRegEx(s,preS:string; Caller:TCaller):TResList; overload;
begin
	result:=GetAlikeRegEx(s,preS,Caller,$100000);
end;


function TBayBaum.GetRegEx(s:string; Caller:TCaller; maxHits:cardinal):TResList;
var
	c		:	cardinal;
	regEx	:	TRegExpr;
	

	procedure Visit(id:cardinal);				// Inorder-Traversierung mit RegEx-Suche
	var
		akt	:	PNode;
		i		:	integer;

	begin
		if (id=0) or (c>maxHits) or stopped then EXIT;
		if not CreateNode(akt) then begin 
			bayError:=101; EXIT; 
		end;

		if GetElem(akt^,id) and (akt^.counter>0) then begin
			for i:=0 to akt^.counter-1 do begin
				Visit(akt^.val[i]^.l);
				if regEx.Exec(akt^.val[i]^.s) then begin
					Caller(akt^.val[i]^); inc(c);
				end;
			end;
			Visit(akt^.r);
		end;
		dispose(akt);
	end;


begin
	stopped:=false; c:=0; resList.Clear;
	if @caller=NIL then caller:=@AddResult;
	regEx:=TRegExpr.Create;
	regEx.Expression:=s;
	Visit(rootPos);
	regEx.Free;
	result:=resList;
end;


function TBayBaum.GetRegEx(s:string; Caller:TCaller):TResList; overload;
begin
	result:=GetRegEx(s,Caller,$100000);
end;


function TBayBaum.MultipleSearch(sl:TStringList; maxHits:cardinal):TResList;
var													// untersucht mehrere Suchausdrücke in einem Lauf (effizienter)
	c			:	cardinal;
	i			:	integer;
	regEx		:	TRegExpr;
	regExe	:	TList;


	procedure Visit(id:cardinal);				// Inorder-Traversierung mit einfacher Substring-Suche
	var
		akt	:	PNode;
		i,j,p	:	integer;

	begin
		if (id=0) or (c>maxHits) or stopped then EXIT;
		if not CreateNode(akt) then begin 
			bayError:=101; EXIT; 
		end;

		if GetElem(akt^,id) and (akt^.counter>0) then begin
			for i:=0 to akt^.counter-1 do begin
				Visit(akt^.val[i]^.l);
				for j:=0 to sl.Count-1 do begin
					if sl.objects[j]<>NIL then begin
						case TParsEl(pointer(sl.objects[j])^).strict of
							-3 :  if TRegExpr(regExe[j]).Exec(akt^.val[i]^.s) then begin
										if TParsEl(pointer(sl.objects[j])^).btResList<>NIL then TParsEl(pointer(sl.objects[j])^).btResList.Add(akt^.val[i]^) else AddResult(akt^.val[i]^);
										inc(c);
									end;
							-2	:	if system.pos(sl[j],akt^.val[i]^.s)>0 then begin		// beliebige Position im String
										if TParsEl(pointer(sl.objects[j])^).btResList<>NIL then TParsEl(pointer(sl.objects[j])^).btResList.Add(akt^.val[i]^) else AddResult(akt^.val[i]^);
										inc(c);
									end;
							-1	:	begin																	// am Ende des Strings
										p:=system.pos(sl[j],akt^.val[i]^.s);
										if ((p>0) and (p=length(akt^.val[i]^.s)-length(sl[j])+1)) then begin
											if TParsEl(pointer(sl.objects[j])^).btResList<>NIL then TParsEl(pointer(sl.objects[j])^).btResList.Add(akt^.val[i]^) else AddResult(akt^.val[i]^);
											inc(c);
										end;
									end;
						end;
					end;
				end;
			end;
			Visit(akt^.r);
		end;
		dispose(akt);
	end;

begin
	stopped:=false; c:=0; resList.Clear;
	regExe:=TList.Create;
	for i:=0 to sl.Count-1 do begin
		if ((sl.objects[i]<>NIL) and (TParsEl(pointer(sl.objects[i])^).strict=-3)) then begin
			regEx:=TRegExpr.Create;
			regEx.Expression:=sl[i];
			regExe.Add(regEx);
		end else
			regExe.Add(NIL);
	end;

	Visit(rootPos);

	for i:=0 to sl.Count-1 do begin
		if ((sl.objects[i]<>NIL) and (TParsEl(pointer(sl.objects[i])^).strict=-3)) then TRegExpr(regExe[i]).Free;
	end;
	regExe.Free;
	result:=resList;
end;


function TBayBaum.MultipleSearch(sl:TStringList):TResList; overload;
begin
	result:=MultipleSearch(sl,$100000);
end;


function TBayBaum.GetAll(Caller:TCaller; maxHits:cardinal):TResList;
var
	c	:	cardinal;


	procedure Visit(id:cardinal);				{ Inorder-Traversierung }
	var
		akt	:	PNode;
		i		:	integer;

	begin
		if (id=0) or (c>maxHits) or stopped then EXIT;
		if not CreateNode(akt) then begin 
			bayError:=101; EXIT; 
		end;

		if GetElem(akt^,id) and (akt^.counter>0) then begin
			for i:=0 to akt^.counter-1 do begin
				Visit(akt^.val[i]^.l);
				Caller(akt^.val[i]^);
				inc(c);
			end;
			Visit(akt^.r);
		end;
		dispose(akt);
	end;

begin
	stopped:=false; c:=0; resList.Clear;
	if @caller=NIL then caller:=@AddResult;
	Visit(rootPos);
	result:=resList;
end;


function TBayBaum.GetAll(Caller:TCaller):TResList; overload;
begin
	result:=GetAll(Caller,maxlongint);
end;


function TBayBaum.TellError:integer;
begin
	result:=bayError; 
	bayError:=0;
end;


function TBayBaum.TellQuality:integer;


	function LG(n:longint):shortint;	{ errechnet ganzzahligen Anteil von LOG100n }
	var
		i	:	shortint;
	begin
		i:=0;
		while n>0 do begin
			n:=n div 100; inc(i);
		end;
		LG:=i;
	end;


begin
	if wordCounter=0 then
		TellQuality:=0
	else
		TellQuality:=round((allCounter/wordCounter)*LG(wordCounter));
end;


function TBayBaum.Update(s:string; dataptr:cardinal; counter,counterMode:longint):boolean;
var
	akt	:	PNode;
	i		:	integer;
	
begin
	Update:=false;
	if not CreateNode(akt) then begin 
		bayError:=101; EXIT; 
	end;
	
	akt^.key:=rootPos;
	if Search(s,0,false,akt^,i) then begin
		if counter<>0 then begin
			if counterMode=0 then begin			// Counter ist absolute Anzahl
				allCounter+=cardinal(counter)-akt^.val[i]^.z;
				akt^.val[i]^.z:=counter;
			end else if ((counter>0) or (akt^.val[i]^.z>=cardinal(abs(counter)))) then begin
				inc(akt^.val[i]^.z,counter);
				inc(allCounter,counter);
			end;		
		end;
		akt^.val[i]^.dp:=dataptr;
		Update:=SetElem(akt^);
	end;
	dispose(akt);
end;



// Bayerbaum auf der Festplatte
constructor TFileBayBaum.Create(const name:string; readOnly:boolean; var res:integer);
begin
	inherited Create;
	res:=0; elStream:=NIL; myFileName:=name+'.btf'; 
	ro:=readOnly; roSave:=readOnly;

	try
		if FileExists(myFileName) then begin
			if readOnly then 
				elStream:=TFileStream.Create(myFileName,fmOpenRead)
			else
				elStream:=TFileStream.Create(myFileName,fmOpenReadWrite);
				
			if elStream.Size>rootPos then begin
				elStream.Read(wordCounter,sizeof(cardinal));
				elStream.Read(allCounter,sizeof(cardinal));
				elStream.Read(nodeCounter,sizeof(cardinal));
				GetElem(root,rootPos);
				top:=cardinal(elStream.Size);
			end else if readOnly then
				res:=202									//	nichts zu lesen
			else begin
				WriteHeader;
				top:=rootPos+tNodeSize;
			end;
		end else	if readOnly then 
				res:=202
		else begin
			elStream:=TFileStream.Create(myFileName,fmCreate);
			WriteHeader;
			top:=rootPos+tNodeSize;
		end;
	except
		on EOutOfMemory  do res:=201;
		on EFCreateError do res:=203;
		on EFOpenError do res:=204;
		on EStreamError do res:=205;
	end;
end;


destructor TFileBayBaum.Destroy;
begin
	if (elStream<>NIL) and (elStream is TFileStream) then Commit;	// sonst erledigt das der Nachfolger
	elStream.Free;
	inherited Destroy
end;


procedure TFileBayBaum.WriteHeader;
begin
	if ro or (elStream=NIL) then EXIT;
	try 
		elStream.Seek(0,soFromBeginning);
		elStream.Write(wordCounter,sizeof(cardinal));
		elStream.Write(allCounter,sizeof(cardinal));
		elStream.Write(nodeCounter,sizeof(cardinal));
		elStream.Write(cs[1],length(cs));
		root.key:=rootPos; 
		SetElem(root);
	except
		on EStreamError do bayError:=206;
	end;
end;


function TFileBayBaum.GetElem(var el:TNode; id:cardinal):boolean;
var
	i,j	:	integer;

begin
	result:=true; 
	if (id=rootPos) and (root.key=rootPos) then begin
		CopyNode(root,el);
		EXIT;
	end;
	if ((id-rootPos) mod tNodeSize<>0) or (id+tNodeSize>elStream.size) then begin
		result:=false; bayError:=209; EXIT;
	end;

	elStream.Seek(id,soFromBeginning);
	if (elStream.Read(el,tNodeSize)=tNodeSize) then begin
		j:=0;
      for i:=0 to el.counter-1 do begin		
			el.val[i]:=PVal(@el.buf[j]);		// Zeiger auf Einzelelemente (TVal) im Datenpuffer (buf) setzen
         j+=tValSizeN+el.buf[j+tValSizeN-1];
		end;
	end else 
		bayError:=206
end;


function TFileBayBaum.SetElem(var el:TNode):boolean;
begin
	if ((ro) or (el.key<rootPos) or ((el.key-rootPos) mod tNodeSize<>0) or (el.key>elStream.size+tNodeSize)) then begin
		result:=false;	bayError:=210; EXIT;
	end;
	if el.key=rootPos then CopyNode(el,root); 
	elStream.Seek(el.key,soFromBeginning);
	elStream.Write(el,tNodeSize);
	dirty:=true; result:=true;
end;


procedure TFileBayBaum.Commit;
begin 
	WriteHeader;
end;


procedure TFileBayBaum.Clear;
begin 
	if ro then EXIT;
	with elStream do begin
		size:=0; position:=0;
	end;
	wordCounter:=0; nodeCounter:=1; allCounter:=0; bayError:=0; 
	ClearNodeContent(root);
	root.key:=rootPos;
	SetElem(root);
	top:=rootPos+tNodeSize;
end;


procedure TFileBayBaum.CacheOn;
begin
	if elStream is TLargeMemoryStream then EXIT;
	Commit;
	elStream.Free;							// TFileStream freigeben
	elStream:=TLargeMemoryStream.Create(0,0);	// als TLargeMemorystream neu anlegen
	with elStream as TLargeMemoryStream do LoadFromFile(myFileName);
	ro:=true;
end;


procedure TFileBayBaum.CacheOff;
begin
	if elStream is TFileStream then EXIT;
	elStream.Free;							// TLargeMemoryStream freigeben
	ro:=roSave;
	if ro then 
		elStream:=TFileStream.Create(myFileName,fmOpenRead)
	else
		elStream:=TFileStream.Create(myFileName,fmOpenReadWrite);
end;


// Bayerbaum im Speicher

constructor TMemBayBaum.Create(const name:string; initSize,growSize:longint; readOnly:boolean; var res:integer);
begin
	inherited Create(name,readOnly,res);
	if res<>0 then EXIT;
	elStream.Free;							// TFileStream freigeben
	elStream:=TLargeMemoryStream.Create(0,growSize);	// als TLargeMemorystream neu anlegen
	with elStream as TLargeMemoryStream do LoadFromFile(myFileName);
	if (initSize>elStream.Size) and (not ro) then with elStream as TLargeMemoryStream do SetSize(initSize); // erst hier sinnvoll, weil LoadFromFile die Capacity einstellt
end;


destructor TMemBayBaum.Destroy;
begin
	Commit;
	inherited Destroy;
end;


procedure TMemBayBaum.Clear;
begin
	if ro or (elStream=NIL) then EXIT;
	with elStream as TLargeMemoryStream do Clear;
	inherited Clear;
end;


procedure TMemBayBaum.Commit;
var
	store	:	TFileStream;
begin
	if ro or (not dirty) or (elStream=NIL) then EXIT;
	inherited Commit;
	elStream.Seek(0,soFromBeginning);
	store:=TFileStream.Create(myFileName,fmCreate);
	store.CopyFrom(elStream,top);
	store.Free;
	dirty:=false;
end;


procedure TMemBayBaum.CacheOn; 
begin end;


procedure TMemBayBaum.CacheOff; 
begin end;


end.
