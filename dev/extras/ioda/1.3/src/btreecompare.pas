program Test;
{$H+}
uses
   Classes,SysUtils,BtreeFlex;

type
	TPrg	=	class
					constructor Create(const name1,name2:string; var res:integer);
					destructor  Destroy; OVERRIDE;
					procedure	Run;
					
				PRIVATE
					baum,vBaum	:	TBayBaum;
					wc				:	cardinal;

					procedure 	Vgl(var akt:Tval);

				PUBLIC
					property 	Differenz:cardinal read wc; 
				end;



constructor TPrg.Create(const name1,name2:string; var res:integer);
begin
	inherited Create;
	baum:=TMemBayBaum.Create(name1,0,0,true,res);
	if res<>0 then FAIL;
	vBaum:=TMemBayBaum.Create(name2,0,0,true,res);
	if res<>0 then FAIL;
	wc:=0;
end;


destructor TPrg.Destroy;
begin
	inherited Destroy;
	baum.Free;
	vBaum.Free;
end;


procedure TPrg.Run;
begin
	baum.GetAll(@Vgl);
end;

	
procedure TPrg.Vgl(var akt:Tval);
var 
	inf,dataptr	:	cardinal;
	
begin
	if not vBaum.SearchWord(akt.s,inf,dataptr) then begin
		writeln(akt.s);
		inc(wc);
	end;
end;


var
	prg	:	TPrg;
	res	:	integer;
begin
	prg:=TPrg.Create(paramstr(1),paramstr(2),res);
	if res=0 then 
		prg.Run 
	else begin
		writeln('RES=',res);
		HALT(1)
	end;
	writeln(prg.Differenz);
	prg.Free;
end.

