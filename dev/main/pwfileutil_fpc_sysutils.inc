{ License of this include file:
  FPC RTL Modified GPL }

{$ifdef unix}

  procedure Sleep(milliseconds: Cardinal);
  var timeout,timeoutresult : TTimespec;
  begin
    timeout.tv_sec:=milliseconds div 1000;
    timeout.tv_nsec:=1000*1000*(milliseconds mod 1000);
    fpnanosleep(@timeout,@timeoutresult);
  end;

  { TODO: return error as OUT param }
  function ExecuteProcess(Const Path: astr; Const ComLine: astr):integer;
  var
    pid    : longint;
    CommandLine: astr;
    cmdline2 : ppchar;
  //  e      : EOSError;
  Begin
    { always surround the name of the application by quotes
      so that long filenames will always be accepted. But don't
      do it if there are already double quotes!
    }
     cmdline2:=nil;
     if Comline<>'' Then
       begin
         CommandLine:=ComLine;
         { Make an unique copy because stringtoppchar modifies the string }
         UniqueString(CommandLine);
         cmdline2:= StringtoPPChar(CommandLine,1);
         cmdline2^:= pchar(Path);
       end
     else
       begin
         getmem(cmdline2,2*sizeof(pchar));
         cmdline2^:=pchar(Path);
         cmdline2[1]:=nil;
       end;
  //  {$ifdef USE_VFORK}
  //  pid:=fpvFork;
  //  {$else USE_VFORK}
    pid:=fpFork;
  //  {$endif USE_VFORK}
    if pid=0 then
     begin
     {The child does the actual exec, and then exits}
  //    {$ifdef FPC_USE_FPEXEC}
        fpexecv(pchar(Path),Cmdline2);
  //    {$else}
  //      Execl(CommandLine);
  //    {$endif}
       { If the execve fails, we return an exitvalue of 127, to let it be known}
       fpExit(127);
     end
    else
     if pid=-1 then         {Fork failed}
     begin
  //      e:=EOSError.CreateFmt(SExecuteProcessFailed,[Path,-1]);
  //      e.ErrorCode:=-1;
  //      raise e;
          exit;
          // TODO: return error as OUT param 
     end;

    { We're in the parent, let's wait. }
    result:=WaitProcess(pid); // WaitPid and result-convert

  //  {$ifdef FPC_USE_FPEXEC}
    if Comline <> '' then freemem(cmdline2);
  //  {$endif}

    if (result<0) or (result=127) then begin
  //    E:=EOSError.CreateFmt(SExecuteProcessFailed,[Path,result]);
  //    E.ErrorCode:=result;
  //    Raise E;
        exit;
        // TODO: return error as OUT param
    end;
  End;


  function ExecuteProcess(Const Path: astr; Const ComLine: array Of astr):integer;
  var pid    : longint;
     //  e : EOSError;
  Begin
    pid:=fpFork;
    if pid=0 then begin
       {The child does the actual exec, and then exits}
        fpexecl(Path,Comline);
       { If the execve fails, we return an exitvalue of 127, to let it be known}
       fpExit(127);
    end else if pid=-1 then         {Fork failed}
    begin
      //e:=EOSError.CreateFmt(SExecuteProcessFailed,[Path,-1]);
      //e.ErrorCode:=-1;
      //raise e;
      exit;
    end;

    { We're in the parent, let's wait. }
    result:=WaitProcess(pid); // WaitPid and result-convert

    if (result<0) or (result=127) then begin
      //    E:=EOSError.CreateFmt(SExecuteProcessFailed,[Path,result]);
      //    E.ErrorCode:=result;
      //    raise E;
      exit;
    end;
  end;

{$endif unix}

{$ifdef windows}
{ todo: return OUT param for error code }
function ExecuteProcess(Const Path: astr; Const ComLine: astr):integer;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  Proc : THandle;
  l    : DWord;
  CommandLine : astr;
//  e : EOSError;
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb:=SizeOf(SI);
  SI.wShowWindow:=1;
  { always surround the name of the application by quotes
    so that long filenames will always be accepted. But don't
    do it if there are already double quotes, since Win32 does not
    like double quotes which are duplicated!
  }
  if pos('"',path)=0 then CommandLine:='"'+path+'"' else CommandLine:=path;
  if ComLine <> '' then 
    CommandLine:=Commandline+' '+ComLine+#0
  else CommandLine := CommandLine + #0;

  if not CreateProcess(nil, pchar(CommandLine), 
    Nil, Nil, False,$20, Nil, Nil, SI, PI) 
  then begin
// todo: return error code
//    e:=EOSError.CreateFmt(SExecuteProcessFailed,[CommandLine,GetLastError]);
//    e.ErrorCode:=GetLastError;
//    raise e;
    exit;
  end;
  Proc:= PI.hProcess;
  if WaitForSingleObject(Proc, dword($ffffffff)) <> $ffffffff then
    begin
      GetExitCodeProcess(Proc,l);
      CloseHandle(Proc);
      CloseHandle(PI.hThread);
      result:=l;
    end
  else
    begin
// todo: return error code
//      e:=EOSError.CreateFmt(SExecuteProcessFailed,[CommandLine,GetLastError]);
//      e.ErrorCode:=GetLastError;
      CloseHandle(Proc);
      CloseHandle(PI.hThread);
//      raise e;
    end;
end;

function ExecuteProcess(const path: astr; const ComLine: array of astr): integer;
var CommandLine: astr;
    i: Integer;
begin
  Commandline:='';
  For i:=0 to high(ComLine) Do Commandline:=CommandLine+' '+Comline[i];
  ExecuteProcess:=ExecuteProcess(Path,CommandLine);
end;
{$endif}

{ fpc rtl: todo, other platforms using include files :( }
function DirectoryExists(const dir: astr) : Boolean;
{$ifdef windows}
var Attr:Dword;
begin
  Attr:=GetFileAttributes(dir);
  if Attr <> $ffffffff then
    Result:= (Attr and FILE_ATTRIBUTE_DIRECTORY) > 0
  else
    Result:=False;
{$endif}
{$ifdef unix}
var Info : Stat;
begin
  DirectoryExists:=(fpstat(dir,Info)>=0) and fpS_ISDIR(Info.st_mode);
{$endif}
end;

const MaxDirs = 129;

function ExtractRelativepath (Const BaseName,DestName: astr): astr;
var Source, Dest: astr;  Sc,Dc,I,J: Longint;
    SD,DD: Array[1..MaxDirs] of PChar;
const OneLevelBack = '..' + PathDelim;
begin
  if Uppercase(ExtractFileDrive(BaseName))<>Uppercase(ExtractFileDrive(DestName)) Then
  begin
    Result:=DestName;
    exit;
  end;
  Source:=ExtractFilePath(BaseName);
  Dest:=ExtractFilePath(DestName);
  SC:=GetDirs (Source,SD);
  DC:=GetDirs (Dest,DD);
  I:=1;
  While (I<DC) and (I<SC) do
    begin
    If StrIcomp(DD[i],SD[i])=0 then
      Inc(i)
    else
      Break;
    end;
  Result:='';
  For J:=I to SC-1 do Result:=Result+OneLevelBack;
  For J:=I to DC-1 do Result:=Result+DD[J]+PathDelim;
  Result:=Result+ExtractFileName(DestNAme);
end;

Procedure DoDirSeparators (Var fname: astr);
var I : longint;
begin
  for I:=1 to Length(fname) do
    If fname[I] in DirSeparators then
      fname[i]:=PathDelim;
end;


function SetDirSeparators (Const fname: astr): astr;
begin
  Result:=fname;
  DoDirSeparators (Result);
end;

{
  DirName is split in a #0 separated list of directory names,
  Dirs is an array of pchars, pointing to these directory names.
  The function returns the number of directories found, or -1
  if none were found.
  DirName must contain only PathDelim as Directory separator chars.}
function getDirs (var DirName: astr; var Dirs: array of pchar) : Longint;
var i : Longint;
begin
  i:= 1;
  result:= -1;
  while I <= Length(DirName) do begin
    if DirName[i]=PathDelim then begin
      DirName[i]:=#0;
      Inc(Result);
      Dirs[Result]:=@DirName[I+1];
    end;
    inc(I);
  end;
  if result > -1 then inc(result);
end;

function IncludeTrailingPathDelimiter(const Path: astr): astr;
var L: integer;
begin
  result:=Path; L:= Length(result);
  if (L=0) or (Result[L] <> PathDelim) then result:= result+PathDelim;
end;

function IncludeTrailingBackslash(Const Path: astr): astr;
begin
  Result:=IncludeTrailingPathDelimiter(Path);
end;

function ExcludeTrailingBackslash(Const Path: astr): astr;
begin
  Result:=ExcludeTrailingPathDelimiter(Path);
end;

function ExcludeTrailingPathDelimiter(Const Path: astr): astr;
var L: integer;
begin
  L:= length(Path);
  if (L>0) and (Path[L]=PathDelim) then dec(L);
  result:= copy(Path,1,L);
end;

function IsPathDelimiter(Const Path: astr; Index: Integer): Boolean;
begin
  result:=(Index>0) and (Index<=Length(Path)) and (Path[Index]=PathDelim);
end;

function ChangeFileExt(const fname, Extension: astr): astr;
var i: longint;
begin
  i:= Length(fname);
  while (i > 0) and not(fname[i] in ['/', '.', '\', ':']) do dec(i);
  if (i = 0) or (fname[i] <> '.') then i:= Length(fname)+1;
  result:= copy(fname, 1, i-1) + Extension;
end;

function ExtractFilePath(const fname: astr): astr;
var i: longint;
begin
  i := Length(fname);
  while (i > 0) and not (fname[i] in ['/', '\', ':']) do dec(i);
  if i > 0 then result:= copy(fname, 1, i) else result:='';
end;

function ExtractFileDir(const fname: astr): astr;
var i: longint;
begin
  i:= Length(fname);
  while (i > 0) and not (fname[i] in ['/', '\', ':']) do Dec(i);
  if (i > 1) and (fname[I] in ['\', '/']) and
    not (fname[I - 1] in ['/', '\', ':']) then dec(i);
  result := Copy(fname, 1, I);
end;

function ExtractFileDrive(const fname: astr): astr;
var i: longint;
begin
  if (Length(fname) >= 3) and (fname[2] = ':') then
    result:= copy(fname, 1, 2)
  else if (Length(fname) >= 2) and (fname[1] in ['/', '\']) and
    (fname[2] in ['/', '\']) then
  begin
    i := 2;
    while (i < Length(fname)) do begin
      if fname[i+1] in ['/', '\'] then break;
      inc(i);
    end ;
    result:= Copy(fname, 1, i);
  end else result := '';
end;

function ExtractFileName(const fname: astr): astr;
var i: longint;
begin
  i:= Length(fname);
  while (I > 0) and not (fname[I] in ['/', '\', ':']) do dec(i);
  result:= Copy(fname, i+1, 255);
end;

function ExtractFileExt(const fname: astr): astr;
var i: longint;
begin
  i := Length(fname);
  while (I > 0) and not (fname[I] in ['.', '/', '\', ':']) do dec(I);
  if (i>0) and (fname[I] = '.') then result:= copy(fname,I,255) else result:= '';
end;
