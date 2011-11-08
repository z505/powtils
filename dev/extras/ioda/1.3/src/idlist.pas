unit IDList;
{$H+}

(*
	Hilfsobjekte zur Volltextklasse.
	Stellt eine TStringlist zur Verfügung, die über ein angeschlossenes
	Objekt zur Speicherung von DWord-(Longint) Werten verfügt.
	Dieser liefert mit 'Finde' eine Liste vom Type TIdList.
	
	TList-Abkömmling TDWList mit vier DWords (für id,pos,age und gewicht)
	mit Spezialmethoden zum Ausfiltern mehrfacher IDs und Sortieren nach
	Alter oder Gewicht. Letztere wird beim Ausfiltern aufaddiert. Methoden
	zur AND-, NOT und OR-Verknüfung ("Assign") solcher Listen.
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
	Classes,SysUtils;
		
const
	tValSizeN = 12+1;				// Länge der DWord-Werte+Stringlänge in TVal (= TVal mit Leerstring)

type
	TStackItem=	record
						size	:	longint;
						ptr	:	pointer;
					end;
	PStackItem= ^TStackItem;
					
	TStack	=	class(TList)
						constructor Create;
						destructor	Destroy; OVERRIDE;
						
						function		Push(var elem; size:longint):boolean;
						function		Pop(var elem):boolean;
						procedure 	Clear; OVERRIDE;
					end;

	TPtrStack=	class(TList)
						constructor Create;
						destructor	Destroy; OVERRIDE;
						
						function		Push(p:pointer):boolean;
						function		Pop:pointer;
					end;


	TCardArr		=	array [0..7] of cardinal;
	PCardArr		=	^TCardArr;
	
	TIdList	=	class(TStringList)
						PUBLIC
						constructor Create;
						destructor  Destroy; OVERRIDE;
						procedure   AddId(const name:string; idVal:cardinal);
						procedure   AddIds(const name:string; idVals:array of cardinal);
						procedure 	InsertIds(index:integer; const name:string; idVals:array of cardinal);
						procedure   Clear; OVERRIDE;
													
						PROTECTED
						function		GetId(i:cardinal):cardinal;
						procedure	SetId(i,idVal:cardinal);

						function		GetIds(i:cardinal):TCardArr;
						procedure 	SetIds(i:cardinal; idVals:TCardArr);
						procedure	SetIds(i:cardinal; idVals:array of cardinal);
							
						PUBLIC
						property		id[i:cardinal]:cardinal read GetId write SetId; 
						property		ids[i:cardinal]:TCardArr read GetIds write SetIds;
					end;


	TStrContainer=class	
						constructor Create(const s:string);
						destructor  Destroy; OVERRIDE;

						PRIVATE
						myS	:	string;
						PUBLIC
						property Str:string read myS write myS;
					end;

						
	THash		=	class(TStringList)
						PUBLIC
						constructor Create;
						destructor  Destroy; OVERRIDE;
						procedure   AddStr(const key,sVal:string);
						procedure   Clear; OVERRIDE;
													
						PROTECTED
						function		GetStr(i:integer):string;
						procedure	SetStr(i:integer; const sval:string);
						function		GetHash(const key:string):string;
						procedure 	SetHash(const key,sVal:string);
						function 	StrExists(const key:string):boolean;						
							
						PUBLIC
						property		Str[i:integer]:string read GetStr write SetStr; 
						property    H[s:string]:string read GetHash write SetHash; DEFAULT;
						property    Exists[s:string]:boolean read StrExists;
					end;
					

	TDW		=	record
						dw1,dw2,dw3,dw4,dw5	:	longint;
					end;
	PDW		=  ^TDW;
						
	TDWList	=	class(TList)
						constructor Create(maximum:longint);
						constructor CreateSorted(maximum:longint; sortOrder:integer);
						destructor  Destroy; OVERRIDE;
						procedure 	Add(kv:array of longint);
						procedure 	AddElem(const elem:TDW);
						function 	Find(keys:array of longint; var i:integer):boolean;
						//				dw1 muss passen,[dw2 muss passen,[differenz zu dw2]]
						function		Exists(keys:array of longint):boolean; // = Find ohne index-Rückgabe
						procedure	Delete(i:integer);
						procedure	DeleteIfFind(keys:array of longint);
						procedure 	Assign(newlist:TDWList; clr:boolean);
						procedure	AndMask(andList:TDWList; diff:longint);
						procedure 	NotMask(andList:TDWList; diff:longint);
						procedure   Clear; OVERRIDE;
						procedure	Sort;
						procedure	Compact(sortOrder:integer);
							
						PROTECTED
						aktKeys	:	array[0..3] of longint;
						aktI,
						sortOrd	:	integer;
						max		:	longint;
						sorted,
						oberflau	:	boolean;			// overflow :-)
							
						function 	FindFirst(keys:array of longint; doSort:boolean):integer;
						function 	FindNext:integer;
						procedure 	OverflowFilter;
						function		GetDW1(i:integer):longint;
						function		GetDW2(i:integer):longint;
						function		GetInfo1(i:integer):longint;
						function		GetInfo2(i:integer):longint;
						function		GetInfo3(i:integer):longint;						
						procedure 	SetDW1(i:integer; l:longint);
						procedure 	SetDW2(i:integer; l:longint);
						procedure 	SetInfo1(i:integer; l:longint);
						procedure 	SetInfo2(i:integer; l:longint);
						procedure 	SetInfo3(i:integer; l:longint);
						function		GetElem(i:integer):TDW;
						
						PUBLIC
						property		Key[i:integer]:longint read GetDW1 write SetDW1; DEFAULT;
						property		Value[i:integer]:longint read GetDW2 write SetDW2;
						property		Info1[i:integer]:longint read GetInfo1 write SetInfo1;
						property		Info2[i:integer]:longint read GetInfo2 write SetInfo2;
						property		Info3[i:integer]:longint read GetInfo3 write SetInfo3;
						property		Element[i:integer]:TDW read GetElem;
						property		Overflow:boolean read oberflau;
					end;
						
// vormals im Unit Btreeflex deklariert:
	TBayStr	=	string[255-12];
	PBayStr	=	^TBayStr;

	TVal		=	packed record
						l,
						z,
						dp		:	cardinal;
						s		:	TBayStr;
					end;   								//  256 Bytes
	PVal		=  ^TVal;

	TResList	=	class(TList)
							PUBLIC
							constructor Create;
							destructor  Destroy; OVERRIDE;
							procedure   Add(const val:TVal);
							procedure 	Insert(i:cardinal; const val:TVal);
							procedure	Delete(i:cardinal);
							procedure 	Unsort;
							
							PROTECTED
							procedure   Clear; OVERRIDE;
							function		GetVal(i:cardinal):TVal;
							procedure	SetVal(i:cardinal; const val:TVal);
							
							PUBLIC
							property		v[i:cardinal]:TVal read GetVal write SetVal; DEFAULT;
					end;


// vormals im Unit Syntaxparser deklariert:
	TOp		=	(NOP,NICHT,UND,ODER,ARG);
	PParsEl	=	^TParsEl;
	TParsEl	=	record
						l,r,
						parent	:	PParsEl;
						idList	:	TDWList;
						hits		:	longint;
						case op	:	TOp of
						  arg		:  (btResList:TResList; value,head:TBayStr; strict:shortint; examined:boolean);
						  nop,
						  nicht,
						  und,
						  oder	:  (opArg:longint);
					end;

						
IMPLEMENTATION

// Callback-Funktionen für List-Sort

function DWordCompare1(p1,p2:pointer):integer;	// nach 1. ID, 2. POS aufsteigend
begin
	if TDW(p1^).dw1 < TDW(p2^).dw1 then
		result:=-1
	else if TDW(p1^).dw1 > TDW(p2^).dw1 then
		result:=1
	else begin
		if TDW(p1^).dw2 < TDW(p2^).dw2 then
			result:=-1
		else if TDW(p1^).dw2 > TDW(p2^).dw2 then
			result:=1
		else
			result:=0;
	end;
end;


function DWordCompare3(p1,p2:pointer):integer;	// nach 1. DATUM, 2. GEWICHT ABSTEIGEND
begin
	if TDW(p1^).dw3 > TDW(p2^).dw3 then
		result:=-1
	else if TDW(p1^).dw3 < TDW(p2^).dw3 then
		result:=1
	else begin
		if TDW(p1^).dw4 > TDW(p2^).dw4 then
			result:=-1
		else if TDW(p1^).dw4 < TDW(p2^).dw4 then
			result:=1
		else
			result:=0;
	end;
end;


function DWordCompare4(p1,p2:pointer):integer;	// nach 1. GEWICHT, 2. DATUM ABSTEIGEND
begin
	if TDW(p1^).dw4 > TDW(p2^).dw4 then
		result:=-1
	else if TDW(p1^).dw4 < TDW(p2^).dw4 then
		result:=1
	else begin
		if TDW(p1^).dw3 > TDW(p2^).dw3 then
			result:=-1
		else if TDW(p1^).dw3 < TDW(p2^).dw3 then
			result:=1
		else
			result:=0;
	end;	
end;


function DWordCompare5(p1,p2:pointer):integer;	// nach 1. INFO, 2. GEWICHT, 3. DATUM ABSTEIGEND
begin
	if TDW(p1^).dw5 > TDW(p2^).dw5 then
		result:=-1
	else if TDW(p1^).dw5 < TDW(p2^).dw5 then
		result:=1
	else begin
		if TDW(p1^).dw4 > TDW(p2^).dw4 then
			result:=-1
		else if TDW(p1^).dw4 < TDW(p2^).dw4 then
			result:=1
		else begin
			if TDW(p1^).dw3 > TDW(p2^).dw3 then
				result:=-1
			else if TDW(p1^).dw3 < TDW(p2^).dw3 then
				result:=1
			else
				result:=0;
		end;	
	end;	
end;


// allgemeiner Stack für untypisierte Daten

constructor TStack.Create;
begin
	inherited Create;
end;


destructor TStack.Destroy;
begin
	Clear;
	inherited Destroy;
end;


procedure TStack.Clear;
var
	i		:	integer;
	item	:	PStackItem;
begin
	for i:=0 to count-1 do 
	begin
		item:=items[i];
		freemem(item^.ptr,item^.size);
		dispose(item);
	end;
	inherited Clear;
end;


function TStack.Push(var elem; size:longint):boolean;
var
	item	:	PStackItem;
begin
	result:=false; 
	if size<=0 then EXIT;

	item:=NIL;
	try
		new(item);
		getmem(item^.ptr,size);
		item^.size:=size;
		System.Move(elem,item^.ptr^,size);
		Add(item);
		result:=true;
	except
		on EOutOfMemory do if item<>NIL then dispose(item);
	end
end;


function	TStack.Pop(var elem):boolean;
var
	item	:	PStackItem;
begin
	result:=false;
	if count=0 then EXIT;
	
	item:=items[count-1];
	System.move(item^.ptr^,elem,item^.size);
	freemem(item^.ptr,item^.size);
	dispose(item);
	Delete(count-1);
	result:=true
end;


// Stack speziell für Pointer

constructor TPtrStack.Create;
begin
	inherited Create;
end;


destructor TPtrStack.Destroy;
begin
	inherited Destroy;
end;


function TPtrStack.Push(p:pointer):boolean;
begin
	try
		Add(p);
		result:=true;
	except
		on EOutOfMemory do result:=false;
	end
end;


function	TPtrStack.Pop:pointer;
begin
	result:=NIL;
	if count=0 then EXIT;
	
	result:=items[count-1];
	Delete(count-1);
end;



// TStringList speziell für ID-Container

constructor TIdList.Create;
begin
	inherited Create;
	sorted:=true; duplicates:=dupIgnore;
end;


destructor TIdList.Destroy;
begin
	Clear;
	inherited Destroy
end;


procedure TIdList.AddId(const name:string; idVal:cardinal);
begin
	AddIds(name,[idVal]);
end;


procedure TIdList.AddIds(const name:string; idVals:array of cardinal);
var
	idA	:	PCardArr;
	len,j	:	integer;
begin
	try
		getmem(idA,8*4);
		len:=length(idVals);
		if len>8 then len:=8;
		for j:=0 to len-1 do idA^[j]:=idVals[j];
	except
		on E:Exception do writeln('Exception in idList.AdIds: '+E.Message);
	end;

	AddObject(name,TObject(idA));
end;


procedure TIdList.InsertIds(index:integer; const name:string; idVals:array of cardinal);
var
	idA	:	PCardArr;
	len,j	:	integer;
begin
	try
		getmem(idA,8*4);
		len:=length(idVals);
		if len>8 then len:=8;
		for j:=0 to len-1 do idA^[j]:=idVals[j];
	except
		on E:Exception do writeln('Exception in idList.AdIds: '+E.Message);
	end;

	InsertObject(index,name,TObject(idA));
end;


procedure TIdList.Clear;
var
	i			:	integer;
begin
	for i:=0 to count-1 do freemem(PCardArr(objects[i]),8*4);
	inherited Clear;		// ACHTUNG: Bug bei CMEM-Verwendung? [beseitigt]
end;


function	TIdList.GetId(i:cardinal):cardinal;
begin
	result:=PCardArr(objects[i])^[0]
end;


procedure TIdList.SetId(i,idVal:cardinal);
begin
	PCardArr(objects[i])^[0]:=idVal
end;


function	TIdList.GetIds(i:cardinal):TCardArr;
begin
	result:=PCardArr(objects[i])^
end;


procedure TIDList.SetIds(i:cardinal; idVals:TCardArr);
begin
	PCardArr(objects[i])^:=idVals
end;


procedure TIdList.SetIds(i:cardinal; idVals:array of cardinal); overload;
var
	len,j	:	integer;
begin
	len:=length(idVals); if len>8 then len:=8;
	for j:=0 to len-1 do PCardArr(objects[i])^[j]:=idVals[j]
end;


// Container für den String-Wert

constructor TStrContainer.Create(const s:string);
begin
	inherited Create;
	myS:=s;
end;


destructor TStrContainer.Destroy;
begin
	inherited Destroy
end;

// TStringList für String-Hash

constructor THash.Create;
begin
	inherited Create;
	sorted:=true; duplicates:=dupIgnore;
end;


destructor THash.Destroy;
begin
	Clear;
	inherited Destroy
end;


procedure THash.AddStr(const key,sVal:string);
begin
	AddObject(key,TStrContainer.Create(sVal));
end;


procedure THash.Clear;
var
	i	:	integer;
begin
	for i:=0 to count-1 do objects[i].Free;
	inherited Clear;		// ACHTUNG: Bug bei CMEM-Verwendung? [beseitigt]
end;


function	THash.GetStr(i:integer):string;
begin
	result:=TStrContainer(objects[i]).Str;
end;


procedure THash.SetStr(i:integer; const sVal:string);
begin
	TStrContainer(objects[i]).Str:=sVal;
end;


function THash.GetHash(const key:string):string;
var
	i	:	integer;
begin
	if Find(key,i) then result:=GetStr(i) else result:='';
end;


procedure THash.SetHash(const key,sVal:string);
var
	i	:	integer;
begin
	if Find(key,i) then SetStr(i,sVal) else AddStr(key,sVal);
end;


function THash.StrExists(const key:string):boolean;
var
	i	:	integer;
begin
	result:=Find(key,i)
end;


// Liste für fünf DWord-Einträge

constructor TDWList.Create(maximum:longint);
begin
	inherited Create;
	sorted:=false; oberflau:=false; sortOrd:=0;
	if maximum=0 then max:=maxlongint else max:=maximum;
end;


constructor TDWList.CreateSorted(maximum:longint; sortOrder:integer);
begin
	Create(maximum);
	sortOrd:=sortOrder;
end;


destructor TDWList.Destroy;
begin
	Clear;
	inherited Destroy
end;


procedure TDWList.Clear;
var
	i	:	integer;
	el	:	PDW;
begin
	for i:=0 to count-1 do 
	begin
	try
		el:=items[i];
		if el<>NIL then dispose(el);
		el:=NIL;
	except
		on E:Exception do begin writeln('*** Exception #11C *** '+E.Message,' ',i:10,count:10); exit; end;
	end;
	end;
	inherited Clear;
	sorted:=false; oberflau:=false;
end;


procedure TDWList.OverflowFilter;
var
	el		:	PDW;
	i,n	:	integer;

begin
	case sortOrd of
		1	:	inherited Sort(@DWordCompare4);
		2	:	inherited Sort(@DWordCompare3);
		3	:	inherited Sort(@DWordCompare5);
	end;

	n:=Count-(max div 4);
	for i:=Count-1 downto n do			// letztes Viertel rückwärts löschen
	begin
		el:=items[i];
		if el<>NIL then dispose(el);
		inherited Delete(i)
	end;
	oberflau:=true; sorted:=false;
end;


procedure TDWList.Add(kv:array of longint);
var
	el	:	PDW;
begin
	if High(kv)<0 then EXIT;
	
	if (Count>=max) and (sortOrd>0) then begin 
		writeln(count:10,max:10,' OVERFLOW => SORTING!');
		OverflowFilter; // laufende Sortierung
	end;
	try
		new(el);
	except 
		on EOutOfMemory do begin 
			writeln('OUT OF MEMORY IN "TDWList.Add"!');
			EXIT
		end;
	end;

	fillDWord(el^,sizeOf(TDW) shr 2,0);
	with el^ do
	begin
		case High(kv) of
			1	:	dw2:=kv[1];
			2	:	begin dw2:=kv[1]; dw3:=kv[2]; end;
			3	:	begin dw2:=kv[1]; dw3:=kv[2]; dw4:=kv[3]; end;
			4	:	begin dw2:=kv[1]; dw3:=kv[2]; dw4:=kv[3]; dw5:=kv[4]; end;
		end;
		dw1:=kv[0]; 
	end;
	inherited Add(el);
	sorted:=false;
end;


procedure TDWList.AddElem(const elem:TDW);
var
	el	:	PDW;
begin
	try
		new(el);
	except 
		on EOutOfMemory do EXIT
	end;
	el^:=elem;
	inherited Add(el);
	sorted:=false;
end;


procedure TDWList.Delete(i:integer);
var
	el	:	PDW;
begin
	el:=items[i];
	if el<>NIL then dispose(el);
	inherited Delete(i)
end;


procedure TDWList.DeleteIfFind(keys:array of longint);
var
	i	:	integer;
begin
	while Find(keys,i) do Delete(i);
end;


procedure TDWList.Assign(newlist:TDWList; clr:boolean);
var
	i	:	integer;
begin
	if clr then Clear;
	for i:=0 to newList.Count-1 do 
	begin
		inherited Add(newlist.items[i]);		// Zeiger in diese Liste eintragen
		newlist.items[i]:=NIL;					// Originalzeiger als gelöscht markieren
	end;
	if Count>max then OverflowFilter;
	sorted:=false;
end;


procedure TDWList.AndMask(andList:TDWList; diff:longint);
var
	el,tempEl		:	PDW;
	i,j,n				:	integer;
	aDiff,minDiff	:	longint;
	
begin
	i:=0; n:=Count;
	while i<n do
	begin
		minDiff:=maxlongint;
		el:=items[i];
		j:=andList.FindFirst([el^.dw1,el^.dw2,diff],true);
		if j>=0 then
		begin
			repeat
				tempEl:=andList.items[j];
				inc(el^.dw4,tempEl^.dw4); tempEl^.dw4:=0;	//	Gewicht nur einmal übernehmen
				aDiff:=abs(el^.dw2 - tempEl^.dw2);
				if aDiff<minDiff then minDiff:=aDiff;
				if minDiff=0 then minDiff:=1;
				j:=andList.FindNext;
			until j<0;
			inc(el^.dw4,24 div minDiff);
			inc(i);
		end else
		begin
			Delete(i); dec(n)
		end;
	end;
end;


procedure TDWList.NotMask(andList:TDWList; diff:longint);
var
	el	:	PDW;
	i	:	integer;
begin
	i:=0;
	while i<Count do
	begin
		el:=items[i];
		if andList.FindFirst([el^.dw1,el^.dw2,diff],true)>=0 then 
			Delete(i)
		else
			inc(i)
	end;
end;


procedure TDWList.Sort;
begin
	inherited Sort(@DWordCompare1);
	sorted:=true;
end;


procedure TDWList.Compact(sortOrder:integer);
var
	temp			:	TDWList;
	el,tempEl	:	PDW;
	i,j			:	integer;
	
begin
	Sort;	// wichtig: hier sortieren, um bei temp-Liste häufige Sortierung zu vermeiden!
	temp:=TDWList.Create(0);

	for i:=0 to Count-1 do 
	begin
		el:=items[i];
		j:=temp.FindFirst([el^.dw1],false);		//	temp erhält sortierte Eingabe s.o.
		if j>=0 then
		begin
			tempEl:=temp.items[j];
			inc(tempEl^.dw4,el^.dw4);
		end else
			temp.AddElem(el^);
	end;	
	Assign(temp,true);
	temp.Free;	
	case sortOrder of
		1	:	inherited Sort(@DWordCompare4);
		2	:	inherited Sort(@DWordCompare3);
		3	:	inherited Sort(@DWordCompare5);
	end;
end;


function TDWList.FindFirst(keys:array of longint; doSort:boolean):integer;
var 
	i,l,r	:	integer;
	diff	:	longint;
begin
	if doSort and not sorted then Sort;
	result:=-1;
	if (High(keys)<2) or (keys[2]=maxlongint) then	//	keine Differenz oder =maxlongint => ignorieren
		diff:=maxlongint
	else
		diff:=keys[2];

	aktKeys[0]:=keys[0]; aktKeys[2]:=diff;
	if High(keys)>0 then aktKeys[1]:=keys[1] else aktKeys[1]:=0;
	aktI:=-1;

	l:=0; r:=Count-1;
	while l<=r do
	begin
		i:=(l+r) div 2;
		if TDW(items[i]^).dw1<keys[0] then
     		l:=i+1
   	else if TDW(items[i]^).dw1>keys[0] then
     		r:=i-1
		else
		begin
  			// Anfang key[0]=Param1 suchen, ggf. dann Param2-key[2] Differenzvergleich
			while (i>0) and (TDW(items[i-1]^).dw1=keys[0]) do dec(i);
			if diff=maxlongint then 
			begin
				aktI:=i+1;
				result:=i; EXIT;	// Differenz unerheblich => true
			end;							
			
			while i<Count do
			begin
				if (TDW(items[i]^).dw1=keys[0]) and (abs(TDW(items[i]^).dw2-keys[1])<=diff) then
				begin
					aktI:=i+1;
					result:=i; EXIT
				end;
				inc(i);
			end;
			EXIT;
		end;
	end;
end;


function TDWList.FindNext:integer;
begin
	if sorted and (aktI>=0) and (aktI<Count) and	// not sorted bedeutet auch: Liste verändert
		(TDW(items[aktI]^).dw1=aktKeys[0]) and 
		(abs(TDW(items[aktI]^).dw2-aktKeys[1])<=aktKeys[2]) then 
	begin
		result:=aktI;
		inc(aktI);
	end else
		result:=-1;
end;


function TDWList.Find(keys:array of longint; var i:integer):boolean;
begin
	i:=FindFirst(keys,true);
	result:=i>=0
end;


function	TDWList.Exists(keys:array of longint):boolean;
begin
	result:=FindFirst(keys,true)>=0
end;


function	TDWList.GetDW1(i:integer):longint;
begin
	result:=TDW(items[i]^).dw1
end;


function	TDWList.GetDW2(i:integer):longint;
begin
	result:=TDW(items[i]^).dw2
end;


function	TDWList.GetInfo1(i:integer):longint;
begin
	result:=TDW(items[i]^).dw3
end;


function	TDWList.GetInfo2(i:integer):longint;
begin
	result:=TDW(items[i]^).dw4
end;


function	TDWList.GetInfo3(i:integer):longint;
begin
	result:=TDW(items[i]^).dw5
end;


procedure TDWList.SetDW1(i:integer; l:longint);
begin
	TDW(items[i]^).dw1:=l;
end;


procedure TDWList.SetDW2(i:integer; l:longint);
begin
	TDW(items[i]^).dw2:=l;
end;


procedure TDWList.SetInfo1(i:integer; l:longint);
begin
	TDW(items[i]^).dw3:=l;
end;


procedure TDWList.SetInfo2(i:integer; l:longint);
begin
	TDW(items[i]^).dw4:=l;
end;


procedure TDWList.SetInfo3(i:integer; l:longint);
begin
	TDW(items[i]^).dw5:=l;
end;


function	TDWList.GetElem(i:integer):TDW;
begin
	result:=TDW(items[i]^)
end;


// Liste für Resultate der Bayerbaum-Suche und -Callbacks:
constructor TResList.Create;
begin
	inherited Create;
end;


destructor TResList.Destroy;
begin
	Clear;
	inherited Destroy
end;


procedure TResList.Clear;
var
	el		:	PVal;
	i		:	integer;
	
begin
	for i:=0 to count-1 do begin
		el:=items[i];
		if el<>NIL then freemem(el,tValSizeN+length(el^.s));
	end;
	inherited Clear;
end;


procedure TResList.Add(const val:TVal);
var
	el	:	PVal;
	l	:	integer;
	
begin
	l:=tValSizeN+length(val.s);
	getmem(el,l); 
	system.move(val,el^,l);
	inherited Add(el);
end;


procedure TResList.Insert(i:cardinal; const val:TVal);
var
	el	:	PVal;
	l	:	integer;
begin
	l:=tValSizeN+length(val.s);
	getmem(el,l); 
	system.move(val,el^,l);
	inherited Insert(i,el);
end;


procedure TResList.Delete(i:cardinal);
var
	el	:	PVal;
begin
	el:=items[i];
	if el<>NIL then freemem(el,tValSizeN+length(el^.s));
	inherited Delete(i)
end;


procedure TResList.Unsort;
var
	i	:	cardinal;
begin
	if count<3 then EXIT;
	randomize;
	for i:=0 to 2*count do	// ungefähre Verwirrung
		Exchange(random(count-1),random(count-1));
end;


function	TResList.GetVal(i:cardinal):TVal;
begin
	result:=TVal(items[i]^);
end;


procedure TResList.SetVal(i:cardinal; const val:TVal);
var
	el		:	PVal;
	l1,l2	:	integer;
begin
	el:=items[i];
	l1:=tValSizeN+length(val.s);
	l2:=tValSizeN+length(el^.s);
	if l1<>l2 then begin
		freemem(el,l1);
		getmem(el,l2); 
	end;
	system.move(val,TVal(items[i]^),l2);
end;


end.
