Unit Globs;

// Sollte eigentlich in Sysutils von Linux enthalten sein. 
// Ist es aber nicht in FPC Version 1.9.6   -   jo 1/2005

INTERFACE
uses
	BaseUnix,UnixUtil;

type
	pglob = ^tglob;
  	tglob = record
   	name : pchar;
    	next : pglob;
  	end;


Procedure Globfree(var p : pglob);
Function Glob(Const path:pathstr):pglob;


IMPLEMENTATION

Procedure Globfree(var p : pglob);
{
  Release memory occupied by pglob structure, and names in it.
  sets p to nil.
}
var
  temp : pglob;
begin
  while assigned(p) do
   begin
     temp:=p^.next;
     if assigned(p^.name) then
      freemem(p^.name);
     dispose(p);
     p:=temp;
   end;
end;


Function Glob(Const path:pathstr):pglob;
{
  Fills a tglob structure with entries matching path,
  and returns a pointer to it. Returns nil on error,
  linuxerror is set accordingly.
}
var
  temp,
  temp2   : string[255];
  thedir  : pdir;
  buffer  : pdirent;
  root,
  current : pglob;
begin
{ Get directory }
  temp:=dirname(path);
  if temp='' then
   temp:='.';
  temp:=temp+#0;
  thedir:=fpopendir(@temp[1]);
  if thedir=nil then
    exit(nil);
  temp:=basename(path,''); { get the pattern }
  if thedir^.dd_fd<0 then
     exit(nil);
{get the entries}
  root:=nil;
  current:=nil;
  repeat
    buffer:=fpreaddir(thedir^);
    if buffer=nil then
     break;
    temp2:=strpas(@(buffer^.d_name[0]));
    if fnmatch(temp,temp2) then
     begin
       if root=nil then
        begin
          new(root);
          current:=root;
        end
       else
        begin
          new(current^.next);
          current:=current^.next;
        end;
       if current=nil then
        begin
           fpseterrno(ESysENOMEM);
          globfree(root);
          break;
        end;
       current^.next:=nil;
       getmem(current^.name,length(temp2)+1);
       if current^.name=nil then
        begin
          fpseterrno(ESysENOMEM);
          globfree(root);
          break;
        end;
       move(buffer^.d_name[0],current^.name^,length(temp2)+1);
     end;
  until false;
  fpclosedir(thedir^);
  glob:=root;
end;

end.
