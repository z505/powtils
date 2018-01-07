(*******************************************************************************

                              Directory Utilities

********************************************************************************

  Functions for dealing with directories and their contents. Find files,
  wildcard matches, subdirectories.

  NOTE: GetDirFiles changed to better name: GetSubdirFiles

  TODO: Unicode filenames functions. Currently this unit uses Ansistrings

  Authors/Credits: L505 (Lars),

*******************************************************************************)

unit pwdirutil;

{$ifdef fpc}{$mode objfpc}{$H+}
{$endif}

{$R+}

interface
uses
  pwtypes,
 {$ifdef fpc}
  pwstrutil,
 {$else}
  strutils,
 {$endif}
  sysutils; // future: compactsysutils

const DEFAULT_INIT = true;

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

  TPaths = record
    items: array of TPath;
    count: integer;
  end;

procedure Init(var rec: TPaths); overload;
procedure Init(var rec: TFileNames); overload;
procedure Init(var rec: TDirNames); overload;
procedure Init(var rec: TDirContents); overload;

function ForceDir(const path: astr): boo;
function GetTrailDir(const s: astr): astr;
procedure GetDirContent(dir: astr; const wildcard: astr; var res: TDirContents; initrec: boo); overload;
procedure GetDirContent(dir: astr; const wildcard: astr; var res: TDirContents); overload;
procedure GetDirContent(dir: astr; var res: TDirContents; initrec: boo); overload;
procedure GetDirContent(dir: astr; var res: TDirContents); overload;
procedure GetDirContent_nodots(dir: astr; const wildcard: astr; var res: TDirContents; initrec: boo); overload;
procedure GetDirContent_nodots(dir: astr; const wildcard: astr; var res: TDirContents); overload;
procedure GetDirContent_nodots(dir: astr; var res: TDirContents; initrec: boo); overload;
procedure GetDirContent_nodots(dir: astr; var res: TDirContents); overload;
procedure GetSubDirs(dir: astr; const wildcard: astr; var res: TDirNames; initrec: boo); overload;
procedure GetSubDirs(dir: astr; const wildcard: astr; var res: TDirNames); overload;
procedure GetSubDirs(dir: astr; var res: TDirNames; initrec: boo); overload;
procedure GetSubDirs(dir: astr; var res: TDirNames); overload;
procedure GetFiles(dir: astr; const wildcard: astr; var res: TFileNames; initrec: boo); overload;
procedure GetFiles(dir: astr; const wildcard: astr; var res: TFileNames); overload;
procedure GetFiles(dir: astr; var res: TFileNames; initrec: boo); overload;
procedure GetFiles(dir: astr; var res: TFileNames); overload;
procedure GetSubdirFiles(const dir, mask: astr; var res: TPaths; initrec: boo); overload;
procedure GetSubdirFiles(const dir, mask: astr; var res: TPaths); overload;
function GetSubdirFiles(const parentdir, mask: astr): TPaths; overload;
function GetCurDir: astr;

procedure Clear(var list: TPaths); overload;
procedure Clear(var fn: TFileNames); overload;

procedure Add(var list: TPaths; path, fname: astr);

function DelFiles(dir: astr; const wildcard: astr): boo;

// Todo:
//   function DelFiles(list: TPaths): boo;
//   function CloneFiles(fromlist: TPaths; todir: astr): boo; overload;

function CloneFiles(src, dest: astr; const wildcard: astr): boo;

procedure RunTests;

// deprecated
const
{$IFDEF FPC}
  ClearFileNames: procedure(var fn: TFileNames) = @Clear;
{$ELSE}
  ClearFileNames: procedure(var fn: TFileNames) = Clear;
{$ENDIF}

implementation

uses
  pwfileutil;

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

const ClearedTPaths: TPaths = ();

{ for local scope records which are not automatically initialized }
procedure Init(var rec: TPaths); overload;
begin rec:= ClearedTPaths;
end;

const ClearedTFileNames: TFileNames = ();

{ for local scope records which are not automatically initialized }
procedure Init(var rec: TFileNames); overload;
begin rec:= ClearedTFileNames;
end;

const ClearedTDirNames: TDirNames = ();

{ for local scope records which are not automatically initialized }
procedure Init(var rec: TDirNames); overload;
begin rec:= ClearedTDirNames;
end;

const ClearedTDirContents: TDirContents = ();

{ for local scope records which are not automatically initialized }
procedure Init(var rec: TDirContents); overload;
begin rec:= ClearedTDirContents;
end;

procedure Clear(var list: TPaths);
begin Init(list);
end;

procedure Clear(var fn: TFileNames);
begin Init(fn);
end;

{ forces to create a directory }
function ForceDir(const path: astr): boo;

  function MakeForcedDir(const dir: astr): boo;

    function ForceDirs(const s: astr): boo; var newpath: astr;
    begin
      result:= true;
      newpath:= ExcludeTrailingPathDelimiter(s);
      if (newpath = '') then exit;
      if not DirectoryExists(newpath) then begin
        result:= ForceDirs(ExtractFilePath(newpath));
        if result then result:= MakeDir(newpath);
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

var newpath: astr; len: integer;
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
begin getdir(0,  result);
end;

procedure Add(var list: TPaths; path, fname: astr);
var oldlen, newlen: integer;
begin
  if path = '' then exit; if fname = '' then exit; oldlen:= list.count;
  newlen:= oldlen + 1;
  setlength(list.items, newlen);
  list.items[oldlen].path:= path;
  list.items[oldlen].fname:= fname;
  list.count:= newlen;
end;


{ Get all files in sub directories one level deep 

  GetDirFiles('/somewhere/', '*.txt', result);

  /somewhere/dir/
  /somewhere/other/
  /somewhere/place/

  This would get all *.txt files from "dir/", "other/", and "place/"
 
  Note: result var is cleared first only if InitRec is true }
procedure GetSubdirFiles(const dir, mask: astr; var res: TPaths; initrec: boo);
var dn: TDirNames;
    fn: TFileNames;
    i, i2: integer;
begin
  if initrec then Init(res);
  // get sub dirs first
  GetSubDirs(dir, dn);
  for i:= low(dn.dirs) to high(dn.dirs) do begin
    GetFiles(dn.dirs[i], mask, fn);
    for i2:= low(fn.files) to high(fn.files) do begin
      Add(res, dn.dirs[i], fn.files[i2]);
    end;
    Clear(fn);
  end;
end;

{ same as above but record inited by default }
procedure GetSubdirFiles(const dir, mask: astr; var res: TPaths);
begin GetSubdirFiles(dir, mask, res, DEFAULT_INIT);
end;

{ same as above but returns as a function result }
function GetSubdirFiles(const parentdir, mask: astr): TPaths;
begin GetSubdirFiles(parentdir, mask, result);
end;

{ gets trailing directory name from a string containing /path/to/trailing/ 
  works with windows or unix slashes }
function GetTrailDir(const s: astr): astr;
var i, slen, slashpos: integer;
    trailingslash: boo;
begin
  result:= ''; slen:= length(s); slashpos:= 0; trailingslash:= false;
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

procedure UpdateDirNameCount(var dn: TDirNames);
begin dn.Count:= length(dn.dirs); 
end;

procedure UpdateFileNameCount(var fn: TFileNames);
begin fn.Count:= length(fn.files); 
end;

{ Gets files from a directory using wild card match 

  Note: result var is cleared first only if InitRec is true }
procedure GetFiles(dir: astr; const wildcard: astr; var res: TFileNames; initrec: boo); overload;
var Info : TSearchRec;
begin
  // must have trailing slash
  if dir[length(dir)] <> SLASH then dir:= dir + SLASH;
  //initialize count, appends if result has existing array contents
  if initrec then Init(res) else UpdateFileNameCount(res);

  if FindFirst(dir + wildcard, faAnyFile and faDirectory, Info) = 0 then begin
    repeat // keep track of file names
      if (info.Attr and faDirectory) <> faDirectory then begin
        inc(res.Count);
        SetLength(res.files, res.Count);
        res.files[res.Count - 1]:= info.Name; 
      end;
    until FindNext(info) <> 0;
  end;
  FindClose(Info);
end;

{ find all files in a given directory, with wildcard match
  Appends to VAR result, if it has existing data
  READ-ONLY FILES are skipped }
procedure GetFiles(dir: astr; const wildcard: astr; var res: TFileNames);
begin GetFiles(dir, wildcard, res, DEFAULT_INIT); 
end;

{  Note: result var is cleared first only if InitRec is true }
procedure GetFiles(dir: astr; var res: TFileNames; initrec: boo);
begin GetFiles(dir, res, initrec);
end;

{ find all files in a given directory
  READ-ONLY FILES are skipped }
procedure GetFiles(dir: astr; var res: TFileNames);
begin GetFiles(dir, '*', res);
end;

{ delete files in given dir, return false if at least 1 delete from gotten
  files didn't occur, return true if all deletes okay or if no deletes were
  required due to no files being found}
function DelFiles(dir: astr; const wildcard: astr): boo;
var fn: TFileNames;
    i, problem: integer;
    removed: boo; 
begin
  result:= false; removed:= false; problem:= 0;
  dir:= IncludeTrailingPathDelimiter(dir);
  GetFiles(Dir, wildcard, fn);
  if fn.count < 1 then begin result:= true; exit; end;
  for i:= 0 to fn.count-1 do begin
    removed:= DeleteFile(dir + fn.files[i]);
    if not removed then inc(problem);
  end;
  if problem > 0 then result:= false else result:= true;
end;

{ copies many files from src directory to dest, using wildcard match  }
function CloneFiles(src, dest: astr; const wildcard: astr): boo;
var fn: TFileNames;
    i, 
    problem,               // if issues occur during copying
    copyres: integer;      // result of each copy
begin
  copyres:= -1; problem:= 0;
  src:= includetrailingpathdelimiter(src);
  dest:= includetrailingpathdelimiter(dest);
  GetFiles(src, wildcard, fn);
  if fn.count < 1 then exit;
  for i:= 0 to fn.count-1 do begin
    copyres:= CloneFile(src + fn.files[i], dest + fn.files[i]);
    if copyres < 1 then inc(problem);
  end;
  if problem > 0 then result:= false else result:= true;
end;

{ TODO overloaded
function CloneFiles(list: TPaths; dest: astr): boo;
begin
  
end;
}

procedure GetSubDirs(dir: astr; const wildcard: astr; var res: TDirNames);
begin GetSubDirs(dir, wildcard, res, DEFAULT_INIT);
end;

{ find all subdirectory names in a given directory, with wildcard
  Appends if VAR result has existing contents
  READ-ONLY DIRECTORIES are skipped, dotted directories skipped 
  Note: result var is cleared first only if InitRec is true } 
procedure GetSubDirs(Dir: astr; const wildcard: astr; var res: TDirNames; initrec: boo);
var Info : TSearchRec;
begin
  // must have trailing slash
  if dir[length(dir)] <> SLASH then dir:= dir + SLASH;
  //initialize count, appends if result has existing array contents
  if initrec then Init(res) else UpdateDirNameCount(res);

  if FindFirst(dir + wildcard, faAnyFile and faDirectory, Info) = 0 then
  begin
    repeat
      // keep track of directory names
      if (info.Attr and faDirectory) = faDirectory then begin
        // we want only subdirectories, not dots like ../ and ./
        if (info.name = '.') or (info.name = '..') then continue;
        inc(res.Count);
        setlength(res.dirs, res.Count);
        res.dirs[res.Count - 1]:= info.name;
      end;
    until FindNext(info) <> 0;
  end;
  FindClose(Info);
end;

{  Note: result var is cleared first only if InitRec is true }
procedure GetSubDirs(dir: astr; var res: TDirNames; initrec: boo);
begin GetSubDirs(Dir, '*', res, initrec);
end;

{ find all subdirectory names in a given directory
   READ-ONLY DIRECTORIES are skipped }
procedure GetSubDirs(dir: astr; var res: TDirNames);
begin GetSubDirs(Dir, '*', res);
end;

procedure UpdateDirContentCounts(var dc: TDirContents);
begin 
  dc.dirCount:= length(dc.dirs); 
  dc.fileCount:= length(dc.files);       
end;

procedure GetDirContent_nodots(dir: astr; const wildcard: astr; var res: TDirContents);
begin GetDirContent_nodots(dir, wildcard, res, DEFAULT_INIT);  
end;

{ Find contents of any directory with wildcard, but skip dots ../ ./
  READ-ONLY FILES are skipped 
  Note: result var is cleared first only if InitRec is true }   
procedure GetDirContent_nodots(dir: astr; const wildcard: astr; var res: TDirContents; initrec: boo);
var Info : TSearchRec;
begin
  // must have trailing slash
  if dir[length(dir)] <> SLASH then dir:= dir + SLASH;
  // initialize counts, appends if there are existing contents
  if initrec then Init(res) else UpdateDirContentCounts(res);

  if FindFirst(dir + wildcard, faAnyFile and faDirectory, Info) = 0 then
  begin
    repeat
      // keep track of directory names
      if (info.Attr and faDirectory) = faDirectory then begin
        // we want only true directories, not dots like ../ and ./
        if (info.name = '.') or (info.name = '..') then continue;
        Inc(res.DirCount);
        SetLength(res.dirs, res.DirCount);
        res.dirs[res.DirCount - 1]:= info.name;
      end else begin
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
procedure GetDirContent_nodots(dir: astr; var res: TDirContents);
begin GetDirContent_nodots(dir, '*', res);
end;

{ Note: result var is cleared first only if InitRec is true }
procedure GetDirContent_nodots(dir: astr; var res: TDirContents; initrec: boo);
begin GetDirContent_nodots(dir, '*', res, initrec);
end;

procedure GetDirContent(dir: astr; const wildcard: astr; var res: TDirContents);
begin GetDirContent(dir, wildcard, res, DEFAULT_INIT);
end;

{ find contents of any directory with a wildcard filter
  READ ONLY FILES are skipped 
  Note: result var is cleared first only if InitRec is true }  
procedure GetDirContent(dir: astr; const wildcard: astr; var res: TDirContents; initrec: boo); overload;
var Info : TSearchRec;                              
begin
  // must have trailing slash
  if dir[length(dir)] <> SLASH then dir:= dir + SLASH;
  // initialize counts, appends if there are existing contents
  if initrec then Init(res) else UpdateDirContentCounts(res);  

  if FindFirst(dir + wildcard, faAnyFile and faDirectory, Info) = 0 then
  begin
    repeat
      // keep track of directory names
      if (info.Attr and faDirectory) = faDirectory then begin
        Inc(res.DirCount);
        SetLength(res.dirs, res.DirCount);
        res.dirs[res.DirCount - 1]:= info.name;
      end else begin
        inc(res.FileCount);
        setlength(res.files, res.FileCount);
        res.files[res.FileCount - 1]:= info.Name;
      end;
    until FindNext(info) <> 0;
  end;
  FindClose(Info);
end;

{ Note: result var is cleared first only if InitRec is true }
procedure GetDirContent(dir: astr; var res: TDirContents; initrec: boo); overload;
begin GetDirContent(dir, '*.*', res, initrec);
end;

{ find contents of any directory
  READ ONLY FILES are skipped }
procedure GetDirContent(dir: astr; var res: TDirContents); overload;
begin GetDirContent(Dir, '*.*', res, DEFAULT_INIT);
end;

end.

