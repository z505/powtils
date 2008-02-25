{-- copy/pasted from sysutils.pp /unix ----------------------------------------}

{****************************************************************************
                              File Functions
****************************************************************************}



Type 
   PathStr = AnsiString;
   NameStr = AnsiString;
   ExtStr = AnsiString;
   DirStr = AnsiString;

{$I datetime.inc}   
{$I textrec.inc}
{$I filerec.inc}

Procedure FSplit(const Path:PathStr;Var Dir:DirStr;Var Name:NameStr;Var Ext:ExtStr);
Var
  DotPos,SlashPos,i : longint;
Begin
  SlashPos:=0;
  DotPos:=256;
  i:=Length(Path);
  While (i>0) and (SlashPos=0) Do
   Begin
     If (DotPos=256) and (Path[i]='.') Then
      begin
        DotPos:=i;
      end;
     If (Path[i]='/') Then
      SlashPos:=i;
     Dec(i);
   End;
  Ext:=Copy(Path,DotPos,255);
  Dir:=Copy(Path,1,SlashPos);
  Name:=Copy(Path,SlashPos + 1,DotPos - SlashPos - 1);
End;

Function FileOpen (Const FileName : string; Mode : Integer) : Longint;

Var LinuxFlags : longint;

BEGIN
  LinuxFlags:=0;
  Case (Mode and 3) of
    0 : LinuxFlags:=LinuxFlags or O_RdOnly;
    1 : LinuxFlags:=LinuxFlags or O_WrOnly;
    2 : LinuxFlags:=LinuxFlags or O_RdWr;
  end;
  FileOpen:=fpOpen (FileName,LinuxFlags);
  //!! We need to set locking based on Mode !!
end;


Function FileCreate (Const FileName : String) : Longint;

begin
  FileCreate:=fpOpen(FileName,O_RdWr or O_Creat or O_Trunc);
end;


Function FileCreate (Const FileName : String;Mode : Longint) : Longint;

Var LinuxFlags : longint;

BEGIN
  LinuxFlags:=0;
  Case (Mode and 3) of
    0 : LinuxFlags:=LinuxFlags or O_RdOnly;
    1 : LinuxFlags:=LinuxFlags or O_WrOnly;
    2 : LinuxFlags:=LinuxFlags or O_RdWr;
  end;
  FileCreate:=fpOpen(FileName,LinuxFlags or O_Creat or O_Trunc);
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;

begin
  FileRead:=fpRead (Handle,Buffer,Count);
end;


Function FileWrite (Handle : Longint; const Buffer; Count : Longint) : Longint;

begin
  FileWrite:=fpWrite (Handle,Buffer,Count);
end;


Function FileSeek (Handle,FOffset,Origin : Longint) : Longint;

begin
  FileSeek:=fplSeek (Handle,FOffset,Origin);
end;


Function FileSeek (Handle : Longint; FOffset,Origin : Int64) : Int64;

begin
  FileSeek:=fplSeek (Handle,FOffset,Origin);
end;


Procedure FileClose (Handle : Longint);

begin
  fpclose(Handle);
end;

Function FileTruncate (Handle,Size: Longint) : boolean;

begin
  FileTruncate:=fpftruncate(Handle,Size)>=0;
end;

Function UnixToWinAge(UnixAge : time_t): Longint;

Var
  Y,M,D,hh,mm,ss : word;

begin
  EpochToLocal(UnixAge,y,m,d,hh,mm,ss);
  Result:=DateTimeToFileDate(EncodeDate(y,m,d)+EncodeTime(hh,mm,ss,0));
end;


Function FileAge (Const FileName : String): Longint;

Var Info : Stat;

begin
  If  fpstat (FileName,Info)<0 then
    exit(-1)
  else
    Result:=UnixToWinAge(info.st_mtime);
end;


Function FileExists (Const FileName : String) : Boolean;

Var Info : Stat;

begin
  FileExists:=fpstat(filename,Info)>=0;
end;


Function DirectoryExists (Const Directory : String) : Boolean;

Var Info : Stat;

begin
  DirectoryExists:=(fpstat(Directory,Info)>=0) and fpS_ISDIR(Info.st_mode);
end;


Function LinuxToWinAttr (FN : Pchar; Const Info : Stat) : Longint;

begin
  Result:=faArchive;
  If fpS_ISDIR(Info.st_mode) then
    Result:=Result or faDirectory;
  If (FN[0]='.') and (not (FN[1] in [#0,'.']))  then
    Result:=Result or faHidden;
  If (Info.st_Mode and S_IWUSR)=0 Then
     Result:=Result or faReadOnly;
  If fpS_ISSOCK(Info.st_mode) or fpS_ISBLK(Info.st_mode) or fpS_ISCHR(Info.st_mode) or fpS_ISFIFO(Info.st_mode) Then
     Result:=Result or faSysFile;
end;

type

 pglob = ^tglob;
  tglob = record
    name : pchar;
    next : pglob;
  end;

Function Dirname(Const path:pathstr):pathstr;
{
  This function returns the directory part of a complete path.
  Unless the directory is root '/', The last character is not
  a slash.
}
var
  Dir  : PathStr;
  Name : NameStr;
  Ext  : ExtStr;
begin
  FSplit(Path,Dir,Name,Ext);
  if length(Dir)>1 then
   Delete(Dir,length(Dir),1);
  DirName:=Dir;
end;


Function Basename(Const path:pathstr;Const suf:pathstr):pathstr;
{
  This function returns the filename part of a complete path. If suf is
  supplied, it is cut off the filename.
}
var
  Dir  : PathStr;
  Name : NameStr;
  Ext  : ExtStr;
begin
  FSplit(Path,Dir,Name,Ext);
  if Suf<>Ext then
   Name:=Name+Ext;
  BaseName:=Name;
end;


Function FNMatch(const Pattern,Name:shortstring):Boolean;
Var
  LenPat,LenName : longint;

  Function DoFNMatch(i,j:longint):Boolean;
  Var
    Found : boolean;
  Begin
  Found:=true;
  While Found and (i<=LenPat) Do
   Begin
     Case Pattern[i] of
      '?' : Found:=(j<=LenName);
      '*' : Begin
            {find the next character in pattern, different of ? and *}
              while Found do
                begin
                inc(i);
                if i>LenPat then Break;
                case Pattern[i] of
                  '*' : ;
                  '?' : begin
                          if j>LenName then begin DoFNMatch:=false; Exit; end;
                          inc(j);
                        end;
                else
                  Found:=false;
                end;
               end;
              Assert((i>LenPat) or ( (Pattern[i]<>'*') and (Pattern[i]<>'?') ));
            {Now, find in name the character which i points to, if the * or ?
             wasn't the last character in the pattern, else, use up all the
             chars in name}
              Found:=false;
              if (i<=LenPat) then
              begin
                repeat
                  {find a letter (not only first !) which maches pattern[i]}
                  while (j<=LenName) and (name[j]<>pattern[i]) do
                    inc (j);
                  if (j<LenName) then
                  begin
                    if DoFnMatch(i+1,j+1) then
                    begin
                      i:=LenPat;
                      j:=LenName;{we can stop}
                      Found:=true;
                      Break;
                    end else
                      inc(j);{We didn't find one, need to look further}
                  end else
                  if j=LenName then
                  begin
                    Found:=true;
                    Break;
                  end;
                  { This 'until' condition must be j>LenName, not j>=LenName.
                    That's because when we 'need to look further' and
                    j = LenName then loop must not terminate. }
                until (j>LenName);
              end else
              begin
                j:=LenName;{we can stop}
                Found:=true;
              end;
            end;
     else {not a wildcard character in pattern}
       Found:=(j<=LenName) and (pattern[i]=name[j]);
     end;
     inc(i);
     inc(j);
   end;
  DoFnMatch:=Found and (j>LenName);
  end;

Begin {start FNMatch}
  LenPat:=Length(Pattern);
  LenName:=Length(Name);
  FNMatch:=DoFNMatch(1,1);
End;


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


{
 GlobToSearch takes a glob entry, stats the file.
 The glob entry is removed.
 If FileAttributes match, the entry is reused
}

Type
  TGlobSearchRec = Record
    Path       : shortString;
    GlobHandle : PGlob;
  end;
  PGlobSearchRec = ^TGlobSearchRec;

Function GlobToTSearchRec (Var Info : TSearchRec) : Boolean;

Var SInfo : Stat;
    p     : Pglob;
    GlobSearchRec : PGlobSearchrec;

begin
  GlobSearchRec:=Info.FindHandle;
  P:=GlobSearchRec^.GlobHandle;
  Result:=P<>Nil;
  If Result then
    begin
    GlobSearchRec^.GlobHandle:=P^.Next;
    Result:=Fpstat(GlobSearchRec^.Path+StrPas(p^.name),SInfo)>=0;
    If Result then
      begin
      Info.Attr:=LinuxToWinAttr(p^.name,SInfo);
      Result:=(Info.ExcludeAttr and Info.Attr)=0;
      If Result Then
         With Info do
           begin
           Attr:=Info.Attr;
           If P^.Name<>Nil then
           Name:=strpas(p^.name);
           Time:=UnixToWinAge(Sinfo.st_mtime);
           Size:=Sinfo.st_Size;
           Mode:=Sinfo.st_mode;
           end;
      end;
    P^.Next:=Nil;
    GlobFree(P);
    end;
end;

Function DoFind(Var Rslt : TSearchRec) : Longint;

Var
  GlobSearchRec : PGlobSearchRec;

begin
  Result:=-1;
  GlobSearchRec:=Rslt.FindHandle;
  If (GlobSearchRec^.GlobHandle<>Nil) then
    While (GlobSearchRec^.GlobHandle<>Nil) and not (Result=0) do
      If GlobToTSearchRec(Rslt) Then Result:=0;
end;



Function FindFirst (Const Path : String; Attr : Longint; out Rslt : TSearchRec) : Longint;

Var
  GlobSearchRec : PGlobSearchRec;

begin
  New(GlobSearchRec);
  GlobSearchRec^.Path:=ExpandFileName(ExtractFilePath(Path));
  GlobSearchRec^.GlobHandle:=Glob(Path);
  Rslt.ExcludeAttr:=Not Attr and (faHidden or faSysFile or faVolumeID or faDirectory); //!! Not correct !!
  Rslt.FindHandle:=GlobSearchRec;
  Result:=DoFind (Rslt);
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;

begin
  Result:=DoFind (Rslt);
end;


Procedure FindClose (Var F : TSearchrec);

Var
  GlobSearchRec : PGlobSearchRec;

begin
  GlobSearchRec:=F.FindHandle;
  GlobFree (GlobSearchRec^.GlobHandle);
  Dispose(GlobSearchRec);
end;


Function FileGetDate (Handle : Longint) : Longint;

Var Info : Stat;

begin
  If (fpFStat(Handle,Info))<0 then
    Result:=-1
  else
    Result:=Info.st_Mtime;
end;


Function FileSetDate (Handle,Age : Longint) : Longint;

begin
  // Impossible under Linux from FileHandle !!
  FileSetDate:=-1;
end;


Function FileGetAttr (Const FileName : String) : Longint;

Var Info : Stat;

begin
  If  FpStat (FileName,Info)<0 then
    Result:=-1
  Else
    Result:=LinuxToWinAttr(Pchar(FileName),Info);
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;

begin
  Result:=-1;
end;


Function DeleteFile (Const FileName : String) : Boolean;

begin
  Result:=fpUnLink (FileName)>=0;
end;


Function RenameFile (Const OldName, NewName : String) : Boolean;

begin
  RenameFile:=BaseUnix.FpRename(OldNAme,NewName)>=0;
end;

Function FileIsReadOnly(const FileName: String): Boolean;

begin
  Result := fpAccess(PChar(FileName),W_OK)<>0;
end;

Function FileSetDate (Const FileName : String;Age : Longint) : Longint;

var
  t: TUTimBuf;

begin
  Result := 0;
  t.actime := Age;
  t.modtime := Age;
  if fputime(PChar(FileName), @t) = -1 then
    Result := fpgeterrno;
end;

{------------------------------------------------------------------------------}
