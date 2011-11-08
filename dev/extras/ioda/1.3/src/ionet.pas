Unit IONet;
{$H+}{$I-}

// Objekt-Verpackung um Signalhandler und einfache TCP-Streaming-Sockets
// Abstrakter AppServer																		 jo  7/2001
// Version 2 mit verbessertem Shutdown/Close-Verhalten und einfacher Quittung  10/2001
// auch Fork arbeitet jetzt einwandfrei (Vaterprozeß schließt jetzt newSocket) 12/2001

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
	SysUtils,UnixUtil,BaseUnix,Unix,Sockets,idList,JStrings;

type
//	TSigHandleProc	=	procedure(sig:longint); CDECL;
	TSigHandleProc	= SigActionHandler;
	TSigHandler	=	class
							constructor Create(sigNum:integer; sigHandleProc:TSigHandleProc);
							destructor  Destroy; OVERRIDE;
							
							PROTECTED
							handler,
							oldHandler:	BaseUnix.PSigActionRec;
							signal	:	integer;
						end;
						
	TInetServer	=	class
							constructor Create(myPort:word; var res:integer);
							destructor  Destroy; OVERRIDE;
							function 	ReceiveMessage(var partner,msg:string; maxBufLen:longint; var newSocke:longint):integer;
							function 	ReceiveFixMessage(var partner,msg:string; maxBufLen:longint; var newSocke:longint):integer;
							procedure 	SendMessage(socket:longint; const s:string);
							procedure 	SendFixMessage(socket:longint; const s:string);
							procedure   Reply(const s:string; useFix:boolean);

							PROTECTED
							inetAddr	:	TInetSockAddr;
							socke,
							replySocket,
							inetError:	longint;
							sIn,sOut : 	text;	
							meChild	:	boolean;
							
							PUBLIC
							property		isChild:boolean read meChild write meChild;
							property		Error:longint read inetError;
						end;

	TInetClient	=	class
							constructor Create(const toAddr:string; var res:integer);
							destructor  Destroy; OVERRIDE;
							procedure   SendMessage(const s:string);
							
							PROTECTED
							inetAddr	:	TInetSockAddr;
							socke,
							inetError:	longint;
							sIn,sOut : 	text;
							
							PUBLIC
							property		Error:longint read inetError;
						end;

	TAppServer	=	class
							constructor Create(myPort:word; myMaxReceiveLen:longint);
							destructor	Destroy; OVERRIDE;
							function		Run(fixLen:boolean):boolean; VIRTUAL;

							PROTECTED
							chldHandler,
							hupHandler,
							trmHandler	:	TSigHandler;
							server		:	TInetServer;
							maxRcvLen	:	longint;
							appError		:	integer;
							port			:	word;
							isChild		:	boolean;
							
							function 	Action(const msg:string):string; VIRTUAL; ABSTRACT;
							
							PUBLIC
							property		Error:integer read appError;
						end;
						
	EApp=class(Exception);


IMPLEMENTATION
const
	MSG_WAITALL = 256;
type
	TA4	=	array[0..3] of byte;
	PA4	=	^TA4;

	TLinger	=	record
		lOnOff,lTime	:	integer;
	end;

const
	maxApps								= 128;	// max. Anzahl child processes
	apps					:	integer	= 0;		// Notbremse gegen zu viele Prozesse
	// globale Variablen wg. SIG-Handler:
	terminated			:	boolean	= false;	// für SIGTERM
	exitStat				:	integer	= 0;		// für SIGCHILD	
	sockListRefCount	:	integer	= 0;		// Referenzzähler für sockList-Benutzung
	sockList				:	TDWList	= NIL;	// PID:Socket-Pärchen 
	sockOpt				:	integer	=	1;


procedure ChildHandler(sig:longint; info: psiginfo; context:PSigContext); CDECL;
var
	i,j,pid	:	integer;
begin
	pid:=fpWaitPID(0,@exitStat,WUNTRACED or WNOHANG);
	if (pid>0) and (sockList<>NIL) and (sockList.Find([pid],i)) then 
	begin
		j:=sockList.value[i];
		if j>0 then
		begin
			shutdown(j,2);	FileClose(j);	// Socket für Child im Vaterprozeß schließen!
			sockList.Delete(i);
		end;
	end;
	dec(apps);
end;


procedure TermHandler(sig:longint; info: psiginfo; context:PSigContext); CDECL;
begin
	terminated:=true;
	writeln('Signal #'+IntToStr(sig)+' received - terminating program.');	
end;


procedure InetAddr2Str(addr:TInetSockAddr; var ipStr:string);
var
	l	:	PA4;
	i	:	integer;
begin
	ipStr:='';
	l:=@addr.addr;
	for i:=0 to 2 do ipStr:=ipStr+IntToStr(l^[i])+'.'; 
	ipStr:=ipStr+IntToStr(l^[3])+':'+IntToStr(addr.port);
end;


// Signalhandler 

constructor TSigHandler.Create(sigNum:integer; sigHandleProc:TSigHandleProc);
begin
	new(oldHandler); new(handler);
	with handler^ do
	begin
		sa_handler:=sigHandleProc;
		fillchar(sa_mask,sizeOf(TSigSet),0);
		sa_flags:=SA_NOCLDSTOP or SA_RESTART or SA_NOMASK;
		sa_restorer:=NIL;
	end;
	signal:=sigNum;
	fpSigAction(signal,handler,oldHandler);
	if sockListRefCount=0 then sockList:=TDWList.Create(0);
	inc(sockListRefCount);
end;


destructor TSigHandler.Destroy;
begin
	fpSigAction(signal,oldHandler,handler);
	dec(sockListRefCount);
	if sockListRefCount=0 then sockList.Free; 
	inherited Destroy;
end;


// Einfache Serverklasse für Internet-Streaming Sockets

constructor TInetServer.Create(myPort:word; var res:integer);
begin
	res:=0; replySocket:=0;
	fillchar(inetAddr,sizeof(inetAddr),0);
	with inetAddr do 
	begin
		family:=AF_INET;
		port:=swap(myPort);
		addr:=0;
	end;

	socke:=Socket(AF_INET,SOCK_STREAM,0);
	if SocketError<>0 then 
	begin
		res:=-700; inetError:=socketError; EXIT
	end;
	
	fpFcntl(socke,F_SETFL,OPEN_NONBLOCK);
	SetSocketOptions(socke,SOL_SOCKET,SO_REUSEADDR,sockOpt,sizeof(sockOpt)); // Wert 1 (ein) für RE-USE ADDRESS

	if Bind(socke,inetAddr,sizeof(inetAddr)) then 
	begin
		if not Listen(socke,128) then 
		begin
			res:=-703; inetError:=socketError
		end;
	end else
	begin
		res:=-702; inetError:=socketError
	end;
end;


destructor TInetServer.Destroy;
begin	
	if not meChild then Shutdown(socke,2); 
	FileClose(socke); 
	inherited Destroy
end;


function TInetServer.ReceiveMessage(var partner,msg:string; maxBufLen:longint; var newSocke:longint):integer;
var
	addr		:	TInetSockAddr;
	len,l		:	longint;
	fds		:	TFDSet;
begin
	partner:=''; msg:=''; result:=0; newSocke:=0;
	try
		SetLength(msg,maxBufLen);
	except
		on EOutofMemory do 
		begin
			result:=-701; EXIT;
		end;
	end;
	len:=sizeof(TInetSockAddr);
	
	repeat
		fpFD_Zero(fds); fpFD_Set(socke,fds);
		if fpSelect(4,@fds,NIL,NIL,5000)>0 then 
		begin
			newSocke:=Accept(socke,addr,len);
			fpFcntl(newSocke,F_SETFL,OPEN_NONBLOCK);
		end;
	until (newSocke<>0) or terminated;
	
	if newSocke<0 then 
	begin
		inetError:=SocketError;
		result:=-704; EXIT;	
	end;
	
	if terminated then EXIT;

	replySocket:=newSocke;
	InetAddr2Str(addr,partner);
	len:=0; 
	repeat
		fpSelect(4,NIL,NIL,NIL,5);	// Select auf socket leider nicht möglich: abwarten
		l:=recv(newSocke,msg[len+1],maxBufLen-len,0);
		if l>0 then	inc(len,l) else BREAK;
	until len>=maxBufLen;
	
	if len<=0 then 
		result:=-705 
	else 
	begin
		SetLength(msg,len);
		result:=len;
	end;
end;


function TInetServer.ReceiveFixMessage(var partner,msg:string; maxBufLen:longint; var newSocke:longint):integer;
var
	addr			:	TInetSockAddr;
	len,msgLen	:	longint;
	fds			:	TFDSet;
begin
	partner:=''; msg:=''; result:=0; newSocke:=0;
	try
		SetLength(msg,maxBufLen);
	except
		on EOutofMemory do 
		begin
			result:=-701; EXIT;
		end;
	end;
	len:=sizeof(TInetSockAddr);
	
	repeat
		fpFD_Zero(fds); fpFD_Set(socke,fds);
		if fpSelect(4,@fds,NIL,NIL,5000)>0 then newSocke:=Accept(socke,addr,len);
	until (newSocke<>0) or terminated;
	
	if newSocke<0 then 
	begin
		inetError:=SocketError;
		result:=-704; EXIT;	
	end;
	
	if terminated then EXIT;

	replySocket:=newSocke;
	InetAddr2Str(addr,partner);

	len:=recv(newSocke,msgLen,4,MSG_WAITALL);
	if (len<>4) or (msgLen>maxBufLen) then
	begin
		result:=-706; EXIT;
	end;
	
	len:=recv(newSocke,msg[1],msgLen,MSG_WAITALL);
	if len<msgLen then 
		result:=-707 
	else 
	begin
		SetLength(msg,len);
		result:=len;
	end;
end;


procedure TInetServer.SendMessage(socket:longint; const s:string);
var
	buffer	:	PChar;
begin
	buffer:=@s[1];
	Send(socket,buffer^,length(s),0);
end;


procedure TInetServer.SendFixMessage(socket:longint; const s:string);
type
	TBuf		=	array[0..$7FFFF] of char;
	PBuf		=	^TBuf;
var
	buffer	:	PBuf;
	len		:	integer;
begin
	len:=length(s);
	getmem(buffer,len+4);
	move(len,buffer^,4);
	move(s[1],buffer^[4],len);
	Send(socket,buffer^,len+4,0);
	freemem(buffer,len+4);
end;


procedure TInetServer.Reply(const s:string; useFix:boolean);
begin
	if replySocket<>0 then 
	begin
		if (useFix) then SendFixMessage(replySocket,s) else SendMessage(replySocket,s)
	end;
end;


// Client-Klasse

constructor TInetClient.Create(const toAddr:string; var res:integer);
var
	a		:	array[0..3] of string;
	n		:	integer;
	myPort:	word;
	
begin
	socke:=Socket(AF_INET,SOCK_STREAM,0);
	if socketError<>0 then 
	begin
		res:=-710; inetError:=socketError; EXIT
	end;
	
	a[0]:=''; a[1]:=''; a[2]:=''; a[3]:='';		// eigentlich nicht notwenig, doch derzeit Compilerwarnung
	n:=split(toAddr,':',a);
	if n<1 then
	begin
		res:=-1; EXIT
	end;
	myPort:=StrToInt(a[1]);
	n:=split(a[0],'.',a);
	if n<3 then
	begin
		res:=-2; EXIT
	end;

	with inetAddr do 
	begin
		family:=AF_INET;
		port:=swap(myPort);
		addr:=(StrToInt(a[3]) shl 24) or (StrToInt(a[2]) shl 16) or
		 		(StrToInt(a[1]) shl  8) or (StrToInt(a[0]));
	end;

	if not Connect(socke,inetAddr,sizeOf(TInetSockAddr)) then 
	begin
		res:=-710; inetError:=socketError; 
	end else
		res:=0;
end;


destructor TInetClient.Destroy;
begin
	Shutdown(socke,2);
	FileClose(socke);
	inherited Destroy
end;


procedure TInetClient.SendMessage(const s:string);
var
	buffer	:	PChar;
begin
	buffer:=@s[1];
	Send(socke,buffer^,length(s),0);
end;


// Application Class

constructor TAppServer.Create(myPort:word; myMaxReceiveLen:longint);
var
	res	:	integer;
begin
	inherited Create;

	appError		:=	0;
	isChild		:= false;
	chldHandler	:=	NIL;
	hupHandler	:=	NIL;
	trmHandler	:=	NIL;
	server		:=	NIL;
	port			:=	myPort;
	maxRcvLen	:= myMaxReceiveLen;

	try
		chldHandler:=TSigHandler.Create(SIGCHLD,@ChildHandler);
		hupHandler:=TSigHandler.Create(SIGHUP,@TermHandler);
		trmHandler:=TSigHandler.Create(SIGTERM,@TermHandler);
		server:=TInetServer.Create(port,res);
		if res<>0 then raise EApp.Create('Cannot start server process. ErrorCode='+IntToStr(res));
	except
		on E:EApp do appError:=-720
	end;
end;


destructor TAppServer.Destroy;
begin
	if server<>NIL then server.Free;
	if chldHandler<>NIL then chldHandler.Free;
	if trmHandler<>NIL then	trmHandler.Free;	
	if hupHandler<>NIL then	hupHandler.Free;	
	inherited Destroy;
end;


function TAppServer.Run(fixLen:boolean):boolean;
var
	partner,msg	:	string;
	newSocke,res:	longint;
	pid			:	integer;
	prefix		:	char;
	fds			:	TFDSet;
	
begin
	repeat
		if (fixLen) then
			res:=server.ReceiveFixMessage(partner,msg,maxRcvLen,newSocke)
		else
			res:=server.ReceiveMessage(partner,msg,maxRcvLen,newSocke);
		
		if res<0 then
		begin
			if (res>=-707) and (res<=-705) then
			begin
				msg:='<COM ERROR=#'+intToStr(res)+'>';
				server.Reply(msg,fixLen); 
				shutdown(newSocke,2); FileClose(newSocke);
				CONTINUE;
			end else
			begin
				appError:=res; EXIT;
//				writeln('Socket Communication is broken: SocketError=#',socketError); 
			end;
		end;
		
		if msg<>'' then prefix:=msg[1] else prefix:=#0;
		if prefix='+'	then			// Spawn
		begin							
			delete(msg,1,1);
			pid:=fpFork;
			if pid=0 then				// 0 im Child, sonst PID des Childs
			begin
				isChild:=true; server.isChild:=true; terminated:=true; 
				msg:=Action(msg);
				server.Reply(msg,fixLen);
				shutdown(newSocke,1); // FileClose(newSocke)	jetzt im ChildHandler des Vaters, s.u.
				fpFD_Zero(fds); fpFD_Set(newSocke,fds);
				fpSelect(4,@fds,@fds,NIL,500); 	// warten, bis alles gesendet ist
			end else
			begin
				sockList.Add([pid,newSocke]);				// Vater schließt im Child-Handler den Socket!
				inc(apps); 
			end;
		end else if prefix='-' then
		begin								// Kommando im Vaterprozess ausführen
			delete(msg,1,1);
			if msg='SHUTDOWN' then
			begin
				if copy(partner,1,9)='127.0.0.1' then
				begin						// nur aus lokalem Netz ausführen 
					terminated:=true;	
					msg:='jodad is going down';
				end
			end else
				msg:=Action(msg);

			server.Reply(msg,fixLen);
			shutdown(newSocke,2); FileClose(newSocke);
		end;
	until	terminated or (apps>maxApps);
	result:=terminated
end;


end.
