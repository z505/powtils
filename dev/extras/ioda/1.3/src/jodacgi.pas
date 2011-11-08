{$H+}
program JodaCGI;
// This programm works as CGI. 
// THIS IS JUST A DEMO WITHOUT ANY FORMATTING!
// You can place formatting code starting at line 100
// For header and footer, a template is recommended.
// I.e.: Create a template with a header, an exec-cgi call of THIS PROGRAMM and a footer

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

uses
	Classes,ezCGI,SysUtils,JStrings,Volltext;

type
	EConfigError	=	class(Exception);

const
	useBigOccList	=	0;
	useFileRef		=  1;

var
	cgi											:	TEZcgi;
	vt												:	TVolltext;
	splitter										:	array[0..7] of string;
	dbPath,db,such,fFilter,von,bis,
	fname,titel,gewicht,age,wortCount,
	datum,serverName,rubrik,target		:	string;
	info											:	cardinal;
	res,err,i,j,k,sortOrder,maxTreffer	:	integer;
	overflow										:	boolean;

begin
	cgi:=TEZcgi.Create;
	try
		cgi.WriteContent('text/html'); 
		cgi.Run;
		db:=cgi.Values['source'];
		dbPath:=cgi.Values['dbpath'];
		such:=cgi.Values['search'];
		ffilter:=cgi.Values['issue'];
		von:=cgi.Values['from'];
		bis:=cgi.Values['until'];
		maxTreffer:=StrToIntDef(cgi.GetValue('maxtreffer','100'),100);
		sortOrder:=StrToIntDef(cgi.GetValue('sortorder','2'),2);
		info:=StrToIntDef(cgi.GetValue('bitfilter','0'),0);
		target:='target="'+cgi.GetValue('target','_top')+'" ';
		serverName:=cgi.Values['serverName'];
			
		res:=0;
		vt:=TVolltext.Create(dbpath+db,ReadOnlyDBNoCache,res);
		if res<>0 then raise EConfigError.Create('Fehler beim Öffnen der Datenbank "'+dbPath+db+'" - Code='+IntToStr(res));

		if FileExists(vt.DatabaseName+'.LOCK') then 
		begin
			writeln('Datenbank "'+vt.DatabaseName+'" ist locked (update in progress?)');
			raise EConfigError.Create('Datenbank "'+vt.DatabaseName+'" ist momentan geschlossen (Update aktiv)<br>Bitte versuchen Sie es zu einem späteren Zeitpunkt.');
		end;
//		vt.SortIssues:='/[A-Z]+/,'+cgi.Values['prefi'];
		res:=vt.Suche(such,von,bis,fFilter,maxTreffer,sortOrder,info,overflow);
		err:=vt.Error;

		writeln('<b>',res,'</b> Treffer</td></tr>');
		if err<>0 then writeln('<b>Error #',err,'</b>');
		if res=0 then begin
			writeln('No hits'); HALT(0);
		end;
		
		if (res>maxtreffer) or (overflow) then
		begin
			writeln('More hits available - please refine your query');
			if overflow then writeln(' (internal overflow)');
		end;
			
		for i:=0 to res-1 do 
		begin
			titel:='';
			Split(vt[i],#9,splitter);
			fname:=splitter[0];
			titel:=splitter[1];
			age:=splitter[3];
			gewicht:=splitter[4];

			if titel<>'' then
			begin
				Split(titel,'*|*',splitter);
				titel:=splitter[0];
				rubrik:=splitter[1];
				wortCount:=splitter[2];	
			end;

			if titel<>'' then
			begin
				k:=Split(titel,'|',splitter);
				for j:=0 to k do
				begin
					if copy(splitter[j],1,2)='T=' then
						splitter[j]:='<h3>'+copy(splitter[j],3,255)+'</h3>'
					else
						splitter[j]:='<h4>'+copy(splitter[j],3,255)+'</h4>';
				end;
			end else
			begin
				splitter[0]:='<h4>No Title</h4>'; gewicht:='?'; wortCount:='?';
			end;
// FORMATTING REQUIRED...			
			writeln('<a href="',serverName+fname,'" ',target,' onClick="return opendoc('#39+serverName+fname+#39',0)">');

			for j:=0 to k do write(splitter[j]);
     		writeln('</a>');

			datum:=DateTimeToStr(StrToIntDef(age,0)+day95);
			writeln(' [',datum+'] [rubrik]');
		end;
	except
		on E:EConfigError do writeln('<p style="color:red; font-weight:bold">'+E.message+'</p>');
		on E:ECGIException do writeln('<p style="color:red; font-weight:bold">'+E.message+'</p>');
	end;

	try
		vt.Free;
		cgi.Free;
	except
		on E:Exception do writeln(E.message,' during shutdown');
	end;
end.
