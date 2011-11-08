UNIT LOGBOOK;
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
	classes,SysUtils{$IFDEF LINUX},BaseUnix{$ENDIF};

type
	TLogbook	=	class
						myLog		:	string;
						count,
						maxcount	:	longint;

						constructor Create(const logbuch:string; maxlines:longint; var res:integer);
						destructor  Destroy; OVERRIDE;
						procedure   Add(const zeile:string; header:boolean);
						procedure   CheckCounter;
					end;


IMPLEMENTATION
type
	TBuf	=	array[0..8191] of char;
	PBuf	=	^TBuf;
	EIOerror=class(Exception);


constructor TLogbook.Create(const logbuch:string; maxlines:longint; var res:integer);
var
	f	:	text;
begin
	inherited Create;
	res:=-1;
	if logbuch='' then FAIL;
	myLog:=logbuch; count:=0; 
	
	if not FileExists(logbuch) then
	begin
		assign(f,logbuch); rewrite(f); 
		res:=ioresult; if res<>0 then FAIL;
		close(f);	
		res:=ioresult; if res<>0 then FAIL;		
	end else
		res:=0;

	if maxLines>=100 then maxCount:=maxLines else maxCount:=100;
end;


destructor TLogbook.Destroy;
begin
	inherited Destroy
end;


procedure TLogbook.Add(const zeile:string; header:boolean);
var
	s,pid	:	string;
	io		:	integer;
	f		:	text;
	
begin
	assign(f,myLog); append(f); io:=ioresult;
	if io<>0 then EXIT;
	if header then	
	begin
		{$IFDEF LINUX}pid:=' ['+IntToStr(fpGetPID)+'] '{$ELSE}pid:=' '{$ENDIF};
		try
			DateTimeToString(s,'dd.mm.yyyy hh:nn:ss',Now);
		except
			on EConvertError do s:='??';
		end;
		s:=' '+s+pid 
	end else 
		s:=' ';
	writeln(f,s,zeile);
	close(f); io:=ioresult;
	if count>=maxCount div 2 then CheckCounter else inc(count);
end;


procedure TLogbook.CheckCounter;
var
	s,newLog		:	string;
	inBuf,outBuf:	PBuf;
	i,n			:	integer;
	f,fn			:	text;
	ok,
	inOpen,
	outOpen		:	boolean;
	E				:	Exception;

begin
	inbuf:=NIL; outbuf:=NIL; ok:=true; 
	inOpen:=false; outOpen:=false; 
	n:=0; count:=0;
	
	try
		assign(f,myLog); reset(f); 
		if ioresult<>0 then raise EIOerror.Create('IOError');
		inOpen:=true;
		try 
			new(inBuf); 
			settextbuf(f,inBuf^);
		except
			on EOutOfMemory do ;		// Fehler kann ignoriert werden
		end;
		
		while not eof(f) do
		begin
			readln(f,s); inc(n);
			if ioresult<>0 then raise EIOerror.Create('IOError');
		end;
		close(f);
		
		if n<maxcount then 
		begin
			if inbuf<>NIL then dispose(inbuf); EXIT;
		end;
		
		reset(f);
		if ioresult<>0 then raise EIOerror.Create('IOError');		
		newLog:=ChangeFileExt(myLog,'.LOGNEW');
		assign(fn,newLog); rewrite(fn);
		if ioresult<>0 then raise EIOerror.Create('IOError');
		outOpen:=true;
		try 
			new(outBuf); 
			settextbuf(fn,outBuf^);
		except
			on EOutOfMemory do ;		// Fehler kann ignoriert werden
		end;

		for i:=maxCount to n do	
		begin
			readLn(f,s); 
			if ioresult<>0 then raise EIOerror.Create('IOError');
		end;

		while not eof(f) do
		begin
			readLn(f,s); 
			if ioresult<>0 then raise EIOerror.Create('IOError');
			writeln(fn,s); 
			if ioresult<>0 then raise EIOerror.Create('IOError');
		end;

	except
		on E:Exception do 
		begin
			ok:=false;
			writeln('Exception during logbook truncation: ',E.message);
		end;
	end;

	if inOpen then  close(fn); 
	if outOpen then close(f);
	if inBuf<>NIL then dispose(inBuf); 
	if outBuf<>NIL then dispose(outBuf);
	
	ok:=ok and (ioresult=0) and DeleteFile(myLog) and RenameFile(newLog,myLog);

	if ok then
		Add('*** Logbook truncated ***',true)
	else
		Add('*** Logbook Truncation FAILURE ***',true);
end;


end.
