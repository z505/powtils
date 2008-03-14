(*******************************************************************************

                              Directory Utilities

********************************************************************************

  Functions for dealing with directories and their contents. Find files,
  wildcard matches, subdirectories.

  Authors/Credits: L505 (Lars),

*******************************************************************************)

unit pwdirutil;

{$ifdef fpc}{$mode objfpc}{$H+}{$endif} {$R+}
{$IFDEF WIN32}{$DEFINE WINDOWS}{$ENDIF}

interface
uses
  pwtypes, pwstrutil,
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

function ForceDir(const path: astr): bln;
function GetTrailDir(const s: astr): astr;
procedure GetDirContent(dir: astr; const wildcard: astr; var res: TDirContents; initrec: bln); overload;
procedure GetDirContent(dir: astr; const wildcard: astr; var res: TDirContents); overload;
procedure GetDirContent(dir: astr; var res: TDirContents; initrec: bln); overload;
procedure GetDirContent(dir: astr; var res: TDirContents); overload;
procedure GetDirContent_nodots(dir: astr; const wildcard: astr; var res: TDirContents; initrec: bln);
procedure GetDirContent_nodots(dir: astr; const wildcard: astr; var res: TDirContents); overload;
procedure GetDirContent_nodots(dir: astr; var res: TDirContents; initrec: bln); overload;
procedure GetDirContent_nodots(dir: astr; var res: TDirContents); overload;
procedure GetSubDirs(dir: astr; const wildcard: astr; var res: TDirNames; initrec: bln); overload;
procedure GetSubDirs(dir: astr; const wildcard: astr; var res: TDirNames); overload;
procedure GetSubDirs(dir: astr; var res: TDirNames; initrec: bln); overload;
procedure GetSubDirs(dir: astr; var res: TDirNames); overload;
procedure GetFiles(dir: astr; const wildcard: astr; var res: TFileNames; initrec: bln); overload;
procedure GetFiles(dir: astr; const wildcard: astr; var res: TFileNames); overload;
procedure GetFiles(dir: astr; var res: TFileNames; initrec: bln); overload;
procedure GetFiles(dir: astr; var res: TFileNames); overload;
procedure GetDirFiles(const dir, mask: astr; var res: TPaths; initrec: bln); overload;
procedure GetDirFiles(const dir, mask: astr; var res: TPaths); overload;
function GetCurDir: astr;

procedure Clear(var a: TPaths); overload;
procedure Clear(var fn: TFileNames); overload;

procedure Add(var a: TPaths; path, fname: astr);

function DelFiles(dir: astr; const wildcard: astr): bln;

// Todo:
//   function DelFiles(paths: TPaths): bln;
//   function CloneFiles(frompaths: TPaths; todir: astr): bln; overload;

function CloneFiles(src, dest: astr; const wildcard: astr): bln;

procedure RunTests;


const // backwards compat:
  ClearFileNames: procedure(var fn: TFileNames) = @Clear;

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

{ especially for local scope records which are not automatically initialized } 
procedure Init(var rec: TPaths); overload;
begin
  SetLength(rec.items, 0);
  rec.count:= 0;
end;

{ especially for local scope records which are not automatically initialized } 
procedure Init(var rec: TFileNames); overload;
begin
  SetLength(rec.files, 0);
  rec.count:= 0;
end;

{ especially for local scope records which are not automatically initialized } 
procedure Init(var rec: TDirNames); overload;
begin
  SetLength(rec.dirs, 0);  
  rec.count:= 0;
end;

{ especially for local scope records which are not automatically initialized } 
procedure Init(var rec: TDirContents); overload;
begin
  SetLength(rec.dirs,  0);
  SetLength(rec.files,  0);
  rec.dircount:= 0;
  rec.filecount:= 0;
end;

procedure Clear(var a: TPaths);           
begin
  Init(a);
end;

procedure Clear(var fn: TFileNames);
begin
  Init(fn);
end;

{ forces to create a directory }
function ForceDir(const path: astr): bln;

  function MakeForcedDir(const dir: astr): bln;

    function ForceDirs(const s: astr): bln;
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

procedure Add(var a: TPaths; path, fname: astr);
var oldlen: integer;
    newlen: integer;
begin
  if path = '' then exit; if fname = '' then exit; oldlen:= a.count;
  newlen:= oldlen + 1;
  setlength(a.items, newlen);
  a.items[oldlen].path:= path;
  a.items[oldlen].fname:= fname;
  a.count:= newlen;
end;


{ Get all files in sub directories one level deep 

  GetDirFiles('/somewhere/', '*.txt', result);

  /somewhere/dir/
  /somewhere/other/
  /somewhere/place/

  This would get all *.txt files from "dir/", "other/", and "place/"
 
  Note: result var is not cleared first, it is persistent    }
procedure GetDirFiles(const dir, mask: astr; var res: TPaths; initrec: bln);
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

procedure GetDirFiles(const dir, mask: astr; var res: TPaths);
begin
  GetDirFiles(dir, mask, res, DEFAULT_INIT);
end;

{ gets trailing directory name from a string containing /path/to/trailing/ 
  works with windows or unix slashes }
function GetTrailDir(const s: astr): astr;
var i: integer;
    slen: integer;
    slashpos: integer;
    trailingslash: bln;
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
begin
  dn.Count:= length(dn.dirs); 
end;

procedure UpdateFileNameCount(var fn: TFileNames);
begin
  fn.Count:= length(fn.files); 
end;

procedure GetFiles(dir: astr; const wildcard: astr; var res: TFileNames; initrec: bln); overload;
var Info : TSearchRec;
begin
  // must have trailing slash
  if dir[length(dir)] <> SLASH then dir:= dir + SLASH;
  //initialize count, appends if result has existing array contents
  if initrec then Init(res)
  else 
    UpdateFileNameCount(res);

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

{ find all files in a given directory, with wildcard match
  Appends to VAR result, if it has existing data
  READ-ONLY FILES are skipped }
procedure GetFiles(dir: astr; const wildcard: astr; var res: TFileNames);
begin
  GetFiles(dir, wildcard, res, DEFAULT_INIT); 
end;

procedure GetFiles(dir: astr; var res: TFileNames; initrec: bln);
begin
  GetFiles(dir, res, initrec);
end;

{ find all files in a given directory
  READ-ONLY FILES are skipped }
procedure GetFiles(dir: astr; var res: TFileNames);
begin
  GetFiles(dir, '*', res);
end;

{ delete files in given dir, return false if at least 1 delete didn't occur }
function DelFiles(dir: astr; const wildcard: astr): bln;
var fn: TFileNames;
    i, problem: integer;
    removed: bln; 
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

function CloneFiles(src, dest: astr; const wildcard: astr): bln;
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

{ TODO oveloaded
function CopyFiles(Paths: TPaths; dest: astr): bln;
begin
  
end;
}

procedure GetSubDirs(dir: astr; const wildcard: astr; var res: TDirNames);
begin
  GetSubDirs(dir, wildcard, res, DEFAULT_INIT);
end;

{ find all subdirectory names in a given directory, with wildcard
  Appends if VAR result has existing contents
  READ-ONLY DIRECTORIES are skipped, dotted directories skipped }
procedure GetSubDirs(Dir: astr; const wildcard: astr; var res: TDirNames; initrec: bln);
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

procedure GetSubDirs(dir: astr; var res: TDirNames; initrec: bln);
begin
  GetSubDirs(Dir, '*', res, initrec);
end;

{ find all subdirectory names in a given directory
   READ-ONLY DIRECTORIES are skipped }
procedure GetSubDirs(dir: astr; var res: TDirNames);
begin
  GetSubDirs(Dir, '*', res);
end;

procedure UpdateDirContentCounts(var dc: TDirContents);
begin
  dc.dirCount:= length(dc.dirs); 
  dc.fileCount:= length(dc.files);       
end;



procedure GetDirContent_nodots(dir: astr; const wildcard: astr; var res: TDirContents);
begin
  GetDirContent_nodots(dir, wildcard, res, DEFAULT_INIT);  
end;

{ find contents of any directory with wildcard, but skip dots ../ ./
   READ-ONLY FILES are skipped }
procedure GetDirContent_nodots(dir: astr; const wildcard: astr; var res: TDirContents; initrec: bln);
var
  Info : TSearchRec;
begin
  // must have trailing slash
  if dir[length(dir)] <> SLASH then dir:= dir + SLASH;
  // initialize counts, appends if there are existing contents
  if initrec then Init(res) else UpdateDirContentCounts(res);

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
procedure GetDirContent_nodots(dir: astr; var res: TDirContents);
begin
  GetDirContent_nodots(dir, '*', res);
end;

procedure GetDirContent_nodots(dir: astr; var res: TDirContents; initrec: bln);
begin
  GetDirContent_nodots(dir, '*', res, initrec);
end;

procedure GetDirContent(dir: astr; const wildcard: astr; var res: TDirContents);
begin
  GetDirContent(dir, wildcard, res, DEFAULT_INIT);
end;

{ find contents of any directory with a wildcard filter
  READ ONLY FILES are skipped }
procedure GetDirContent(dir: astr; const wildcard: astr; var res: TDirContents; initrec: bln); overload;
var
  Info : TSearchRec;                              
begin
  // must have trailing slash
  if dir[length(dir)] <> SLASH then dir:= dir + SLASH;
  // initialize counts, appends if there are existing contents
  if initrec then Init(res) else UpdateDirContentCounts(res);  

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

procedure GetDirContent(dir: astr; var res: TDirContents; initrec: bln); overload;
begin
  GetDirContent(dir, '*.*', res, initrec);
end;

{ find contents of any directory
  READ ONLY FILES are skipped }
procedure GetDirContent(dir: astr; var res: TDirContents); overload;
begin
  GetDirContent(Dir, '*.*', res, DEFAULT_INIT);
end;



end.

