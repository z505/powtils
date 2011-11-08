UNIT SyntaxParser;
{
	Baut aus einem Suchstring mit Klammern und logischen Operatoren einen binären Syntaxbaum auf, 
	der sich dann traversieren läßt. Auswertung ohne Klammern ist sequentiell.
	Arbeitet mit Delegates (Callbacks).
	(c) jo@magnus.de 1992, portiert nach Windows 1994, portiert nach Linux-fp 6/2001

	Fehlernummern 500-599
}

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


{$H+}
INTERFACE
uses
	Classes,SysUtils,Strings,JStrings,IdList,RegExpr;

const
	maxFind		=	$10000;				// maximale Treffer in einem Zwischenergebnis
	ANDSTR		=	'AND';
	ORSTR			=	'OR';
	NOTSTR		=	'NOT';
	NEARBYSTR1	=  'NEARBY';
	NEARBYSTR2	=  'NEAR';	
	ops			:	set of char		=	['|','\','&'];
	opsPara		:	set of char		=	['|','\','&','(',')'];

type
	TParserProc	=	procedure (var parsEl:TParsEl) of object;
	TTriggerProc=	procedure (var parsEl:TParsEl; idList:TDWList; diff:longint; operation:TOp) of object;

	TParser=	class
						constructor Create(var s:string; var res:integer);
						destructor  Destroy; OVERRIDE;
						procedure   Clear;
						procedure   NewARGel(var el:PParsEl; const newValue:string; link:TOp);
						procedure   PostOrder(callback:TParserProc);
						procedure   PreOrder(callback:TParserProc);
						procedure   InOrder(callback:TParserProc);
						procedure   Optimizer(callback:TParserProc);
						function  	Trigger(triggerCB:TTriggerProc; sortOrder:integer):integer;

						PROTECTED
						root		:	PParsEl;
						such		:	PChar;
						suchlen	:	integer;
						oberflau,
						dummynode:	boolean;

						function    Postfix:boolean;
						function    SyntaxBaum:boolean;
						function 	GetID(i:integer):longint;
						function 	GetPos(i:integer):longint;
						function 	GetInfo1(i:integer):longint;
						function 	GetInfo2(i:integer):longint;
						function 	GetInfo3(i:integer):longint;
						function		GetResultCount:integer;
						
						PUBLIC
						property		ID[i:integer]:longint read GetID; DEFAULT;
						property		Pos[i:integer]:longint read GetPos;
						property		Info1[i:integer]:longint read GetInfo1;
						property		Info2[i:integer]:longint read GetInfo2;
						property		Info3[i:integer]:longint read GetInfo3;
						property		Count:integer read GetResultCount;
						property		Overflow:boolean read oberflau;
					end;


		function HideRegExe(var s:string):boolean;
		function UnhideRegExe(s:string):string;


IMPLEMENTATION


function HideRegExe(var s:string):boolean;
var
	regEx		:	TRegExpr;
	t			:	string;
begin
	result:=false;
	if system.pos('/',s)=0 then EXIT;
	
	regEx:=TRegExpr.Create;
	regEx.Expression:='/(.+?)/';
	if regEx.Exec(s) then begin
		repeat
			if regEx.MatchPos[0]>=0 then begin
				t:=copy(s,regEx.MatchPos[0],regEx.MatchLen[0]);
				t:=StrSubst(t,'(',#1,false);
				t:=StrSubst(t,')',#2,false);
				t:=StrSubst(t,'|',#3,false);
				s:=copy(s,1,regEx.MatchPos[0]-1)+t+copy(s,regEx.MatchPos[0]+regEx.MatchLen[0],length(s));
				result:=true;
			end;
		until not regex.ExecNext;
	end;
	regEx.Free;
end;


function UnhideRegExe(s:string):string;
begin
	s:=StrSubst(s,#1,'(',false);
	s:=StrSubst(s,#2,')',false);
	s:=StrSubst(s,#3,'|',false);
	result:=s;
end;


constructor TParser.Create(var s:string; var res:integer);

	function OpVor(const s:string; i:integer):boolean;
	var
		j,c	:	integer;
	begin
		j:=i-1; c:=2;
		while (j>0) and (c>0) do begin
			case s[j] of 
				'&','\','|'	:	c:=0;
				'0'..'9'	:	if c=2 then dec(j) else break;
				'.'	 	:	if (c=2) then begin
									dec(j); dec(c);
								end else
									break;
				else			break;
			end;
		end;
		result:=c=0
	end;


var
	i,j,k,worte	:	integer;

begin
	inherited Create;
	root:=NIL; such:=NIL; suchlen:=0; oberflau:=false;

	if (s[1]='{') and (s[length(s)]='}') then 
		s:=copy(s,2,length(s)-2)
	else begin
		i:=1; j:=length(s);						// Mehrfach-Blanks komprimieren
		while i<j do begin
			if (s[i]=#32) and (s[i+1]=#32) then begin
				delete(s,i,1);
				dec(j);
			end else
				inc(i)
		end;
	
		HideRegExe(s);
		s:=StrStrSubst(s,' '+andStr+' ','&',true);
		s:=StrStrSubst(s,' '+andStr+'.','&.',true);
		s:=StrStrSubst(s,' '+orStr +' ','|',true);
		s:=StrStrSubst(s,' '+notStr+' ','\',true);
		s:=StrStrSubst(s,' '+notStr+'.','\.',true);
		s:=StrStrSubst(s,' '+nearByStr1,'&.50',true);
		s:=StrStrSubst(s,' '+nearByStr2,'&.50',true);
		s:=StrSubst(s,#39,'"',false);
		s:=StrSubst(s,':',' ',false);		
//		s:=StrSubst(s,',','&',false);			// jetzt RegEx-Kennung, jo 30.5.2005
//		s:=StrSubst(s,'/','|',false);			// ebenso
		s:=Trim(s);
		if (s='') or (s[1] in ops) then s:='*'+s;

		i:=system.pos('"',s);					// z.B.: "hallo wie geht es" wird durch (hallo&3.wie&3.geht&3.es) ersetzt
		while i>0 do begin
			s[i]:='(';
			j:=system.pos('"',s);
			if j>0 then begin
				s[j]:=')';
				worte:=0;
				for k:=i+1 to j-1 do 
					if (s[k]=#32) or (s[k] in opsPara) then inc(worte);
				if worte>0 then begin
					for k:=i+1 to j-1 do 
						if s[k]=#32 then begin
							s[k]:='&';
							s:=copy(s,1,k)+'.'+IntToStr(worte)+copy(s,k+1,length(s));
							inc(j);
						end;
				end else	begin						// ein "Wort" für Literalsuche z.B.: "AND"
					delete(s,j,1);
					delete(s,i,1);
				end;
			end;
			i:=system.pos('"',s);
		end;

		i:=system.pos('(',s);					// Klammer-Test
		while i>0 do begin
			s[i]:=#5;
			j:=system.pos(')',s);
			if j>i then begin
				s[j]:=#6;
				worte:=0;
				for k:=i+1 to j-1 do 
					if (s[k]=#32) or (s[k] in opsPara) then inc(worte);
				if worte=0 then begin			// ein (Wort) ist falsch: Klammern entfernen
					delete(s,j,1);
					delete(s,i,1);
				end;
			end else begin
				res:=-503; EXIT;
			end;
			i:=system.pos('(',s);
		end;
		s:=StrSubst(s,#5,'(',false);
		s:=StrSubst(s,#6,')',false);

		i:=2; j:=length(s);
		while i<=j do begin
			if (s[i]=#32) and not 
				((s[i-1] in ops) or (s[i-1]='-') or 
 				 (((s[i-1]>='0') and (s[i-1]<='9')) and OpVor(s,i)) or
				 (i=j) or (s[i+1]=#32) or (s[i+1] in opsPara)) then
					s[i]:='&';
			inc(i);
		end;
	end;

	worte:=0;
	for k:=1 to length(s) do 
		if s[k]='-' then inc(worte);
	// ^^ nur eine Näherung - ungenau bei mehreren (nicht-zusammenhängenden) Koppelworten!
	k:=system.pos('-',s);
	while (k>0) do begin
		i:=k-1; s[k]:='&'; system.insert('.'+IntToStr(worte),s,k+1);
		while i>0 do begin
			if (s[i]=#32) or (s[i] in opsPara) then begin
				if s[i+1]='.' then begin
					inc(i);
					while ((i<length(s)) and (s[i+1]>='0') and (s[i+1]<='9')) do inc(i);
				end; 
				break;
			end;
			dec(i);
		end;
		system.insert('(',s,i+1);

		i:=k+2;
		while i<=length(s) do begin
			if (s[i]=#32) or (s[i] in opsPara) then break;
			inc(i);
		end;
		system.insert(')',s,i);
		dec(worte);
		k:=system.pos('-',s);
	end;
	
	suchlen:=length(s);
	getmem(such,suchlen+1);
	strPCopy(such,s);
	s:='{'+s+'}';
	if Postfix then begin
		if SyntaxBaum then res:=0 else res:=-502;
	end else
		res:=-501;
end;



procedure TParser.Clear;

	procedure Clr(akt:PParsEl);			{ Postorder-Traversierung }
	begin
		if akt<>NIL then
			with akt^ do begin
				Clr(l);
				Clr(r);
				if ((idList<>NIL) and (parent=NIL)) then idList.Free;
				if ((op=ARG) and (btResList<>NIL)) then btResList.Free;
				dispose(akt);
			end;
	end;

begin
	Clr(root); root:=NIL;
	if such<>NIL then freemem(such,suchlen+1);
	such:=NIL; suchlen:=0;
end;


destructor TParser.Destroy;
begin
	Clear;
	inherited Destroy;
end;


function TParser.Postfix:boolean;
var
	v			:	PChar;
	oldlen	:	integer;
	i,j,k		:	word;		// Underflow-Effekt benutzt (=> $FFFF), daher keine 32-Bit-Integer verwenden!



	procedure rPars(r,t:word);
	var
		s	:	shortstring;
		ch	:	char;
		b	:	boolean;
	begin
		ch:=#0; s:='';
		while ((i<suchlen-1) or (i=$FFFF)) and
				((j<suchlen+255-1) or (j=$FFFF)) and (r<2) do begin
			b:=ch='(';
			inc(i);
			ch:=such[i];
			if (ch<>'(') and (ch<>')') and (not (ch in ops)) then begin
				inc(r);
				repeat
					inc(j); v[j]:=ch;
					inc(i); ch:=such[i];
				until (ch='(') or (ch=')') or (ch in ops) or (i>=suchlen);
			end;
			b:=b and (ch=')');

			if ch='(' then
				r:=0
			else if (ch=')') and (t>0) and not b then begin
				while (i<suchlen-1) and (such[i+1]=' ') do inc(i);
				EXIT;
			end;

			if (i<suchlen) and (ch in ops) then begin
				if (i+2<suchlen) and (such[i+1]='.') then begin
					inc(i); k:=i; inc(i);
					while (i<suchlen) and (such[i]>='0') and (such[i]<='9') do
						inc(i);
					if i-k<12 then begin
						move(such[k],s[1],i-k); s[0]:=char(i-k);
						if such[i]<>#32 then dec(i);
					end;
				end;

				if not (v[j] in ops) then begin	 { #255 überflüssig }
					inc(j); v[j]:=#255;
				end;

				rPars(r,t+1);

				inc(j); v[j]:=ch;
				if s<>'' then begin
					move(s[1],v[j+1],length(s)); inc(j,length(s));
					s:='';
				end;
				if r>0 then dec(r);
			end;
		end;
	end;

begin
	oldlen:=suchlen+256;
	getmem(v,oldlen);	 						{ 256 Byte ist (reichlich) Reserve für Blanks }
	fillchar(v^,oldlen,0);
	i:=$FFFF; j:=$FFFF;
	rPars(0,0);
	freemem(such,suchlen+1);
	suchlen:=integer(strLen(v));
	getmem(such,suchlen+1);					{ Speicherplatz aktualisieren }
	strCopy(such,v);		   				{ neues SUCH zuweisen }
	freemem(v,oldlen);
	Postfix:=true;
end;


function TParser.SyntaxBaum:boolean;
var
	cstack,stack	:	TPtrStack;
	el,l,r			:	PParsEl;
	s					:	TBayStr;
	i,j,k,p,q		:	integer;
	ok					:	boolean;
	ch					:	char;

begin
	stack:=TPtrStack.Create;
	cstack:=TPtrStack.Create;

	i:=0; ok:=true; 
	while (i<suchlen) and ok do begin
		new(el); fillchar(el^,sizeof(TParsEl),0);
		cstack.Push(el);			{ Kontrollstack }
		ch:=such[i];
		if ch in ops then begin
			r:=stack.Pop; l:=stack.Pop;
			if (r<>NIL) and (l<>NIL) then begin
				el^.r:=r; el^.l:=l;
				case ch of
				  '&' 		: el^.op:=UND;
				  '\' 		: el^.op:=NICHT;
				  '|' 		: el^.op:=ODER;
				end;
			end else
				ok:=false;

			inc(i);
			if (i+1<suchlen) and (such[i]='.') then begin	{ Argument folgt }
				inc(i);
				j:=i;
				while (i<suchlen) and (such[i]>='0') and (such[i]<='9') do
					inc(i);
				if i-j<12 then begin
					move(such[j],s[1],i-j); s[0]:=char(i-j);
					val(s,el^.opArg,p);
					if p<>0 then
						ok:=false;
				end else
					ok:=false;
			end else
				el^.opArg:=maxlongint;
				
			if such[i]=#255 then inc(i);		// kommt nur bei Ops mit Parametern vor
		end else	begin
			j:=i;
			while (i<suchlen) and (such[i]<>#255) and not (such[i] in ops) do inc(i);
			with el^ do begin
				op:=ARG;
				if i-j < sizeOf(TBayStr) then k:=i-j else k:=sizeOf(TBayStr)-1;
				move(such[j],value[1],k); value[0]:=char(k);
				value:=Trim(value);
				if ((length(value)=0) or (value='*') or (value='?')) then 
					ok:=false
				else if ((value[1]='/') and (value[length(value)]='/')) then begin
					if length(value)<5 then 
						ok:=false
					else begin
						value:=copy(value,2,length(value)-2);
						value:=UnhideRegExe(value);	// RegExe wieder aufdecken
						strict:=-3;					// ein echter regulärer Ausdruck
					end;
				end else begin
					p:=system.pos('*',value);
					q:=system.pos('?',value);
					if ((p=0) and (q=0)) then	// keine Wildcards
						strict:=2
					else if ((p=length(value)) and (q=0)) then begin
						if length(value)<3 then 
							ok:=false
						else begin
							dec(value[0]);
							strict:=1
						end;
					end else if ((p=1) and (q=0) and (system.pos('*',copy(value,2,length(value)-2))=0)) then begin	// nur * am Wortanfang (zus. * am Wortende ist erlaubt)
						value:=copy(value,2,length(value)-1);		//	ggf. Stern am Ende noch ^^ ignorieren
						if value[length(value)]='*' then begin
							dec(value[0]);	
							strict:=-2;		// => GetSub oder MultipleSearch.case -2
						end else
							strict:=-1;		// => GetTail oder MultipleSearch.case -1
						if length(value)<3 then ok:=false;
					end else begin
						if length(value)>=3 then begin
							value:=StrSubst(value,'?','.',false);
							while system.pos('**',value)>0 do value:=StrStrSubst(value,'**','*',false);
							value:=StrStrSubst(value,'*','.*',false);
							if value[1]='.' then begin
								while ((length(value)>0) and ((value[1]='.') or (value[1]='*'))) do value:=copy(value,2,length(value)-1);
							end else						
								value:='^'+value;

							if ((value[length(value)]='.') or (value[length(value)]='*')) then begin
								while ((length(value)>0) and ((value[length(value)]='.') or (value[length(value)]='*'))) do dec(value[0]);
							end else
								value:=value+'$';
							
							strict:=-3;			// => GetRegEx oder MultipleSearch.case -3
						end else
							ok:=false;
					end;
				end;
				
				if ((strict=-3) and (value[1]='^')) then begin
					p:=2;
					while p<=length(value) do 
						case value[p] of
							'a'..'z',
							'A'..'Z',
							'0'..'9'	:	inc(p);
							else			BREAK;
						end;

					if ((p>2) and (p<=length(value)) and 
					    ((value[p]='?') or (value[p]='*') or 
						 ((p<length(value)) and (value[p]=#123) and (value[p+1]='0')))) then dec(p);  {0,...}
						 
					if p>2 then begin 
						strict:=0; head:=copy(value,2,p-2);
					end;
				end;
			end;
			if such[i]=#255 then inc(i);
		end;
		stack.Push(el);
	end;
	el:=stack.Pop;

	if (el=NIL) or (stack.Count>0) then ok:=false;
	if not ok then begin    		{ ggf. fehlerhafte Verkettungen: per Kontrollstack aufräumen }
		el:=cstack.Pop;
		while el<>NIL do begin
			dispose(el);
			el:=cstack.Pop;			
		end;
	end;
	cstack.Free;
	stack.Free;

	{ jetzt noch Root mit rechtem NOP-Knoten(abschluß) anlegen für
	  Sonderfälle nur ein Argument oder nur NICHT-Verknüpfung }
	new(root); fillchar(root^,sizeof(TParsEl),0);
	root^.op:=UND; root^.l:=el;

	new(el); fillchar(el^,sizeof(TParsEl),0);
	el^.op:=NOP; el^.hits:=maxlongint;
	root^.r:=el;

	SyntaxBaum:=ok;
end;



procedure TParser.newARGel(var el:PParsEl; const newValue:string; link:TOp);
{ dupliziert das angegebene ARG-Element, wandelt das Original in LINK ^
  und verkettet die Kopie links damit. Rechts wird das neue ARG-Element
  angekettet. Zeiger EL wird auf den rechten (neuen) Nachfahren gesetzt.
}
var
	nel1,nel2	:	PParsEl;
begin
	if el^.op<>ARG then EXIT;
	new(nel1);
	nel1^:=el^;	{ übernimmt VALUE des Originals }

	new(nel2);
	nel2^:=el^; nel2^.value:=newValue;

	with el^ do begin
		op:=link; l:=nel1; r:=nel2; value:='';
	end;
	el:=nel2;
end;


procedure TParser.PostOrder(callback:TParserProc);


	procedure Visit(akt:PParsEl);
	begin
		if akt<>NIL then begin
			Visit(akt^.l);
			Visit(akt^.r);
			Callback(akt^);
		end;
	end;

begin
	Visit(root);
end;


procedure TParser.preOrder(callback:TParserProc);


	procedure Visit(akt:PParsEl);
	begin
		if akt<>NIL then begin
			Callback(akt^);
			Visit(akt^.l);
			Visit(akt^.r);
		end;
	end;

begin
	Visit(root);
end;


procedure TParser.inOrder(callback:TParserProc);


	procedure Visit(akt:PParsEl);
	begin
		if akt<>NIL then begin
			Visit(akt^.l);
			Callback(akt^);
			Visit(akt^.r);
		end;
	end;

begin
	Visit(root);
end;


procedure TParser.Optimizer(callback:TParserProc);	{ Postorder-Traversierung }
var
	l,r	:	PParsEl;
	stack:	TPtrStack;


	procedure Visit(akt:PParsEl);
	begin
		if akt<>NIL then begin
			Visit(akt^.l);
			Visit(akt^.r);

			if akt^.op=NOP then
				stack.Push(akt)
			else if akt^.op=ARG then begin
				Callback(akt^);
				stack.Push(akt);
			end else begin
				r:=stack.Pop; l:=stack.Pop;
				case akt^.op of
				  NICHT : akt^.hits:=l^.hits;			{ rechts ist die NOT-(Filter-)Komponente }

				  UND :	begin
								if l^.hits>r^.hits then begin
									stack.Push(akt^.r);
									akt^.r:=akt^.l;
									akt^.l:=stack.Pop;
									akt^.hits:=r^.hits 	{ zunächst nur besseren Zweig durchsuchen }
								end else
									akt^.hits:=l^.hits 	{ zunächst nur besseren Zweig durchsuchen }
							end;

				  ODER: 	begin
							if l^.hits>r^.hits then begin
								stack.Push(akt^.r);
								akt^.r:=akt^.l;
								akt^.l:=stack.Pop;
							end;
							akt^.hits:=l^.hits+r^.hits;{ beide Zweige müssen untersucht werden }
						end;
				end;
				stack.Push(akt);
			end;
		end;
	end;

begin
	stack:=TPtrStack.Create;
	Visit(root);
	stack.Free;
end;


function TParser.Trigger(triggerCB:TTriggerProc; sortOrder:integer):integer;


	procedure Visit(akt:PParsEl);
	begin
		with akt^ do begin
			if op=NOP then EXIT;
			if l^.op=ARG then begin
				idList:=TDWList.CreateSorted(maxFind,sortOrder);
				if (l^.hits>0) and (l^.hits<>maxlongint) then begin
//					write('Untersuche (l): ',l^.value);
					triggerCB(l^,idList,opArg,NOP);
//					writeln(', gefunden: ',idList.Count:4);
				end;
			end else begin
				Visit(l);
				idList:=l^.idList;
				l^.idList:=NIL;
			end;
				
			if r^.op=ARG then  begin
				if (r^.hits>0) and (r^.hits<>maxlongint) then begin
//					write('Untersuche (r): ',r^.value);				
					triggerCB(r^,idList,opArg,op);
//					writeln(', gefunden: ',idList.Count:4);					
				end;					
			end else begin
				Visit(r);
				case op of
					ODER	:	if (r^.hits>0) and (r^.hits<>maxlongint) then idList.Assign(r^.idList,false);
					UND	:	if (r^.hits<>maxlongint) then begin
//						write('Verknüpfe (r), ');
						idList.AndMask(r^.idList,opArg);
//						writeln('verbleiben: ',idList.Count:4);
					end;
					NICHT	:	if (r^.hits>0) and (r^.hits<>maxlongint) then idList.NotMask(r^.idList,opArg) { else idList.Clear}
				end;
				r^.idList.Free; r^.idList:=NIL;
			end;

			hits:=idList.Count;
		end;
	end;


begin
	oberflau:=false; result:=0;
	Visit(root);
	if (root^.idList<>NIL) and (root^.idList.Count>1) then begin
// 	writeln('VOR COMPACT:  ',root^.idList.Count:4);
		root^.idList.Compact(sortOrder);
// 	writeln('NACH COMPACT: ',root^.idList.Count:4);	
		result:=root^.idList.Count;
	end;
	oberflau:=root^.idList.OverFlow;
end;


function TParser.GetID(i:integer):longint;
begin
	result:=root^.idList[i];
end;


function TParser.GetPos(i:integer):longint;
begin
	result:=root^.idList.value[i];
end;


function TParser.GetInfo1(i:integer):longint;
begin
	result:=root^.idList.info1[i];
end;



function TParser.GetInfo2(i:integer):longint;
begin
	result:=root^.idList.info2[i];
end;


function TParser.GetInfo3(i:integer):longint;
begin
	result:=root^.idList.info3[i];
end;


function TParser.GetResultCount:integer;
begin
	if (root<>NIL) and (root^.idList<>NIL) then
		result:=root^.idList.Count
	else
		result:=0;
end;

end.
