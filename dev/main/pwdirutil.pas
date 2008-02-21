(*******************************************************************************

                              Directory Utilities

********************************************************************************

  Functions for dealing with directories and their contents. Find files,
  wildcard matches, subdirectories.

  Authors/Credits: L505 (Lars),

*******************************************************************************)

unit pwdirutil;

{$ifdef fpc}{$mode objfpc}{$H+}{$endif}
{$IFDEF WIN32}{$DEFINE WINDOWS}{$ENDIF}

interface

uses
  pwtypes,
  sysutils; // future: compactsysutils


type
  TDirContents = record
    Dirs: AstrArray;
    DirCount: integer;
    Files: AstrArray;
    FileCount: integer;
  end;

  TDirNames = record
    Dirs: AstrArray;
    Count: integer;
  end;
                                
  TFileNames = record
    Files: AstrArray;
    Count: integer;
  end;

  TPath = record
    path,
    fname: astr;
  end;

  PathArray = array of TPath;

function ForceDir(const path: astr): boolean;
function GetTrailDir(const s: astr): astr;
procedure ClearFileNames(var fn: TFileNames);
procedure GetDirContent(Dir: astr; const wildcard: astr; var res: TDirContents); overload;
procedure GetDirContent(Dir: astr; var res: TDirContents); overload;
procedure GetDirContent_nodots(Dir: astr; const wildcard: astr; var res: TDirContents); overload;
procedure GetDirContent_nodots(Dir: astr; var res: TDirContents); overload;
procedure GetSubDirs(Dir: astr; const wildcard: astr; var res: TDirNames); overload;
procedure GetSubDirs(Dir: astr; var res: TDirNames); overload;
procedure GetFiles(Dir: astr; const wildcard: astr; var res: TFileNames); overload;
procedure GetFiles(Dir: astr; var res: TFileNames); overload;
procedure GetDirFiles(const dir, mask: astr; var res: PathArray);
function GetCurDir: astr;

procedure Clear(var a: PathArray);
procedure Add(var a: PathArray; path, fname: astr);

function DelFiles(Dir: astr; const wildcard: astr): boolean;

procedure RunTests;

implementation

uses pwfileutil;

procedure GetTrailDir_TEST1;
begin
  writeln(GetTrailDir('..\fail\pass\'));
  writeln(GetTrailDir('../fail/pass/'));
end;

procedure GetTrailDir_TEST2;
begin
  writeln(GetTrailDir('..\fail\pass'));
  writeln(GetTrailDir('../fail/pass'));
end;

procedure GetTrailDir_TEST3;
begin
  writeln(GetTrailDir('\pass'));
  writeln(GetTrailDir('/pass'));
end;

procedure GetTrailDir_TEST4;                               
begin                                             
  writeln(GetTrailDir('pass/'));
  writeln(GetTrailDir('pass\'));
end;

{ verify it finds the trailing path in all situations }
procedure RunTests;
const
  deldir = 'delete12345\delete';
  delmask = '*.delete';
begin
  writeln('-------------------');
  GetTrailDir_TEST1;
  GetTrailDir_TEST2;
  GetTrailDir_TEST3;
  GetTrailDir_TEST4;
  writeln('Note: ', delmask, ' must exist in ', deldir);
  writeln('DelFiles result: ',  DelFiles(deldir, delmask));
  writeln('-------------------');
end;                     

{ forces to create a directory }
function ForceDir(const path: astr): boolean;

  function MakeForcedDir(const dir: astr): Boolean;

    function ForceDirs(const s: astr): Boolean;
    var path: astr;
    begin
      result:= true;
      path:= ExcludeTrailingPathDelimiter(s);
      if (path = '') then exit;
      if not DirectoryExists(path) then
      begin
        result:= ForceDirs(ExtractFilePath(path));
        if result then result:= MakeDir(path);
      end;
    end;
  
  var drive : astr;
  begin
    result:= false;
    drive:= ExtractFileDrive(dir);
    if (drive <> '') and (DirectoryExists(drive) = false) then exit;
    if dir = '' then exit;
    result:= ForceDirs(dir);
  end;

var newpath: astr;
    len: integer;
begin
  len:= length(path);
  // trim trailing slash
  case path[len] of
    '/', '\': newpath:= leftstr(path, len-1);
  else
    newpath:= path;
  end;
  // cross platform slashes
  xpath(newpath);
  result:= MakeForcedDir(newpath);
end;


{ return working directory }
function GetCurDir: astr;
begin
  getdir(0,  result);
end;

procedure Add(var a: PathArray; path, fname: astr);
var len: integer;
begin
  if path = '' then exit; if fname = '' then exit; len:= length(a);
  setlength(a, len + 1);
  a[len].path:= path;
  a[len].fname:= fname;
end;

procedure Clear(var a: PathArray);           
begin
  setlength(a, 0);  
end;

{ Get all files in sub directories one level deep 

  GetDirFiles('/somewhere/', '*.txt', result);

  /somewhere/dir/
  /somewhere/other/
  /somewhere/place/

  This would get all *.txt files from "dir/", "other/", and "place/"
 
  Note: result var is not cleared first, it is persistent    }
procedure GetDirFiles(const dir, mask: astr; var res: PathArray);
var dn: TDirNames;
    fn: TFileNames;
    i, i2: integer;
begin
  // get sub dirs first
  GetSubDirs(dir, dn);
  for i:= low(dn.dirs) to high(dn.dirs) do begin
    GetFiles(dn.dirs[i], mask, fn);
    for i2:= low(fn.files) to high(fn.files) do begin
      Add(res, dn.dirs[i], fn.files[i2]);
    end;
    ClearFileNames(fn);
  end;
end;

{ gets trailing directory name from a string containing /path/to/trailing/ 
  works with windows or unix slashes }
function GetTrailDir(const s: astr): astr;
var i: integer;
    slen: integer;
    slashpos: integer = 0;
    trailingslash: boolean = false;
begin
  result:= ''; slen:= length(s);
  if slen < 1 then exit;
  for i:= slen downto 1 do begin
    if s[i] in SLASHES then begin
      if i = slen then begin
        trailingslash:= true;
        continue; // skip last delimiting slash /dir/somedir/ <---
      end;
      slashpos:= i;
      break;
    end;
  end;

  for i:= slashpos+1 to slen do begin
    if (i = slen) and (trailingslash) then continue;
    result:= result + s[i];
  end;
end;


procedure ClearFileNames(var fn: TFileNames);
begin
  setlength(fn.files, 0); fn.count:= 0;
end;

{ find all files in a given directory, with wildcard match
  Appends to VAR result, if it has existing data
  READ-ONLY FILES are skipped }
procedure GetFiles(Dir: astr; const wildcard: astr; var res: TFileNames);
var Info : TSearchRec;
begin
  // must have trailing slash
  if dir[length(dir)] <> SLASH then dir:= dir + SLASH;
  //initialize count, appends if result has existing array contents
  if length(res.files) < 1 then 
    res.Count:= 0 
  else 
    res.Count:= length(res.files); 
  if FindFirst(dir + wildcard, faAnyFile and faDirectory, Info) = 0 then
  begin
    repeat
      // keep track of file names
      if (info.Attr and faDirectory) <> faDirectory then
      begin
        Inc(res.Count);
        SetLength(res.files, res.Count);
        res.files[res.Count - 1]:= info.Name;
      end;
    until FindNext(info) <> 0;
  end;
  FindClose(Info);
end;

{ find all files in a given directory
  READ-ONLY FILES are skipped }
procedure GetFiles(Dir: astr; var res: TFileNames);
begin
  GetFiles(dir, '*', res);
end;

{ delete files in given dir, return false if at least 1 delete didn't occur }
function DelFiles(Dir: astr; const wildcard: astr): boolean;
var fn: TFileNames;
    i, problem: integer;
    removed: boolean; 
begin
  removed:= false; problem:= 0;
  dir:= includetrailingpathdelimiter(dir);
  GetFiles(Dir, wildcard, fn);
  if fn.count < 1 then exit;
  for i:= 0 to fn.count-1 do begin
    removed:= DeleteFile(dir + fn.files[i]);
    if not removed then inc(problem);
  end;
  if problem > 0 then result:= false else result:= true;
end;

{ find all subdirectory names in a given directory, with wildcard
  Appends if VAR result has existing contents
  READ-ONLY DIRECTORIES are skipped, dotted directories skipped }
procedure GetSubDirs(Dir: astr; const wildcard: astr; var res: TDirNames);
var Info : TSearchRec;
begin
  // must have trailing slash
  if dir[length(dir)] <> SLASH then dir:= dir + SLASH;
  //initialize count, appends if result has existing array contents
  if length(res.dirs) < 1 then 
    res.Count:= 0 
  else 
    res.Count:= length(res.dirs); 

  if FindFirst(dir + wildcard, faAnyFile and faDirectory, Info) = 0 then
  begin
    repeat
      // keep track of directory names
      if (info.Attr and faDirectory) = faDirectory then                            
      begin
        // we want only subdirectories, not dots like ../ and ./
        if (info.name = '.') or (info.name = '..') then continue;
        Inc(res.Count);
        SetLength(res.dirs, res.Count);
        res.dirs[res.Count - 1]:= info.name;
      end;
    until FindNext(info) <> 0;
  end;
  FindClose(Info);
end;

{ find all subdirectory names in a given directory
   READ-ONLY DIRECTORIES are skipped }
procedure GetSubDirs(Dir: astr; var res: TDirNames);
begin
  GetSubDirs(Dir, '*', res);
end;

{ find contents of any directory with wildcard, but skip dots ../ ./
   READ-ONLY FILES are skipped }
procedure GetDirContent_nodots(Dir: astr; const wildcard: astr; var res: TDirContents);
var
  Info : TSearchRec;
begin
  // must have trailing slash
  if dir[length(dir)] <> SLASH then dir:= dir + SLASH;
  // initialize counts, appends if there are existing contents
  if length(res.dirs) < 1 then 
    res.dirCount:= 0 
  else 
    res.dirCount:= length(res.dirs); 
  if length(res.files) < 1 then 
    res.fileCount:= 0 
  else 
    res.fileCount:= length(res.files);       
  
  if FindFirst(dir + wildcard, faAnyFile and faDirectory, Info) = 0 then
  begin
    repeat
      // keep track of directory names
      if (info.Attr and faDirectory) = faDirectory then
      begin
        // we want only true directories, not dots like ../ and ./
        if (info.name = '.') or (info.name = '..') then continue;
        Inc(res.DirCount);
        SetLength(res.dirs, res.DirCount);
        res.dirs[res.DirCount - 1]:= info.name;
      end else
      begin
        Inc(res.FileCount);
        SetLength(res.files, res.FileCount);
        res.files[res.FileCount - 1]:= info.Name;
      end;
    until FindNext(info) <> 0;
  end;
  FindClose(Info);
end;

{ find contents of any directory but skip dots like ../ ./
  READ ONLY FILES are skipped }
procedure GetDirContent_nodots(Dir: astr; var res: TDirContents);
begin
  GetDirContent_nodots(dir, '*', res);
end;

{ find contents of any directory with a wildcard filter
  READ ONLY FILES are skipped }
procedure GetDirContent(Dir: astr; const wildcard: astr; var res: TDirContents);
var
  Info : TSearchRec;                              
begin
  // must have trailing slash
  if dir[length(dir)] <> SLASH then dir:= dir + SLASH;
  // initialize counts, appends if there are existing contents
  if length(res.dirs) < 1 then 
    res.dirCount:= 0 
  else 
    res.dirCount:= length(res.dirs); 
  if length(res.files) < 1 then 
    res.fileCount:= 0 
  else 
    res.fileCount:= length(res.files);         
  if FindFirst(dir + wildcard, faAnyFile and faDirectory, Info) = 0 then
  begin
    repeat
      // keep track of directory names
      if (info.Attr and faDirectory) = faDirectory then
      begin
        Inc(res.DirCount);
        SetLength(res.dirs, res.DirCount);
        res.dirs[res.DirCount - 1]:= info.name;
      end else
      begin
        Inc(res.FileCount);
        SetLength(res.files, res.FileCount);
        res.files[res.FileCount - 1]:= info.Name;
      end;
    until FindNext(info) <> 0;
  end;
  FindClose(Info);
end;

{ find contents of any directory
  READ ONLY FILES are skipped }
procedure GetDirContent(Dir: astr; var res: TDirContents);
begin
  GetDirContent(Dir, '*.*', res);
end;



end.

