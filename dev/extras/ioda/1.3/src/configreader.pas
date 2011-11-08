Unit ConfigReader;
{$H+}{$I-}

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
	Classes,SysUtils,JStrings;

type
	TStrContainer=class	
						constructor Create(const str:string);
						destructor  Destroy; OVERRIDE;

						PRIVATE
						myStr	:	string;
						
						PUBLIC
						property value:string read myStr write myStr;
					end;

	TConfigReader=class
						constructor Create;
						destructor	Destroy; OVERRIDE;
						procedure 	Clear;
						function		ReadConfigFile(fname:string):integer;
						function		ReadCommandLine:integer;
						function		Add(s:string):boolean;

						PROTECTED
						keyList	:	TStringList;
						cfg,cmd	:	integer;

						function 	Search(const name:string):string;
						function 	SearchBool(const name:string):boolean;
						function 	SearchNum(const name:string):longint;

						PUBLIC
						property		Str[name:string]:string read Search; DEFAULT;
						property		Bool[name:string]:boolean read SearchBool;
						property		Int[name:string]:longint read SearchNum;
						property		CmdCount:integer read cmd;
						property		CfgCount:integer read cfg;						
					end;


IMPLEMENTATION
type
	TBuf		=	array[0..4095] of char;
	PBuf		=	^TBuf;
	EIOerror	=	class(Exception);


// String-Container Hilfsklasse zum Config-Reader

constructor TStrContainer.Create(const str:string);
begin
	inherited Create;
	myStr:=str;
end;


destructor  TStrContainer.Destroy;
begin
	inherited Destroy
end;


// Config-Reader für Kommandozeile und Konfigurationsdateien

constructor TConfigReader.Create;
begin
	inherited Create;
	keyList:=TStringList.Create; keyList.sorted:=true;
	cfg:=0; cmd:=0;
end;


destructor TConfigReader.Destroy;
begin
	Clear;
	keyList.Free;
	inherited Destroy
end;


procedure TConfigReader.Clear;
var
	i	:	integer;
begin
	if keyList<>NIL then
	begin
		for i:=0 to keyList.count-1 do 
		begin
			keyList.objects[i].Free;
			keyList.objects[i]:=NIL;
		end;
	end;
	cfg:=0; cmd:=0;
end;


function TConfigReader.Add(s:string):boolean;
var 
	k,v	:	string;
	p		:	integer;
	
begin
	result:=false;
	s:=Trim(s);
	if pos('-',s)=1 then
	begin
		if pos('=',s)=0 then 
			s:=s+'=TRUE'							// Options like "-p"
		else
			delete(s,1,1)							// ansonsten "-" löschen
	end;
	p:=pos('=',s); if p=0 then EXIT;
	k:=Uppercase(Trim(copy(s,1,p-1)));
	v:=Trim(copy(s,p+1,length(s)-p));
	if Search(k)='' then
		keyList.AddObject(k,TStrContainer.Create(v));
	result:=true
end;



function TConfigReader.Search(const name:string):string;
var
	i	:	integer;
begin
	if keyList.Find(Uppercase(name),i) then 
		with keyList.objects[i] as TStrContainer do result:=value 
	else 
		result:=''
end;



function TConfigReader.SearchNum(const name:string):longint;
var
	s	:	string;
begin
	s:=Search(name);
	result:=StrToIntDef(s,0)
end;



function TConfigReader.SearchBool(const name:string):boolean;
var
	s	:	string;
begin
	s:=Search(name);
	result:=(CompareText(s,'TRUE')=0) or (CompareText(s,'YES')=0) or (CompareText(s,'JA')=0);
end;


function TConfigReader.ReadConfigFile(fname:string):integer;
var
	f	 		:	text;
	buf	 	:	PBuf;
	s			:	string;
	ok,fopen	:	boolean;
	
begin
	ok:=true; fopen:=false; buf:=NIL;
	if ExtractFileExt(fname)='' then fname:=fname+'.config';
	assign(f,fname);
	try
		reset(f); 
		if ioresult<>0 then raise EIOerror.Create('IOError');
		fOpen:=true;
		try 
			new(buf); 
			settextbuf(f,buf^);
		except
			on EOutOfMemory do ;		// Fehler kann ignoriert werden
		end;

		while not eof(f) do
		begin
			readLn(f,s);
			if (pos('#',s)<>1) and (pos(';',s)<>1) and (pos('//',s)<>1)  and (pos('=',s)>0) then 
			begin
				if Add(s) then inc(cfg);
			end;
		end;
	except
		on EIOerror do	ok:=false
	end;
	if fOpen then  close(f); 
	if buf<>NIL then dispose(buf); 
	if ok then 
		result:=cfg
	else
		result:=-1;
end;


function TConfigReader.ReadCommandLine:integer;
var
	i	:	integer;
begin
	i:=1;
	while i<=paramCount do begin;
		if Add(paramStr(i))	then
			inc(cmd)
		else
			break;
		inc(i);
	end;
	result:=cmd;
end;


end.
