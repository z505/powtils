{ fpmake and fpcmake not serving my simple needs. These build utilities are 
  born, so that there are no ugly GNU make files (ugliest fucking cow shit I've 
  seen) for building projects automatically.

  Useful for automating and building demos, tests, units, demos, projects, etc.
 
  Authors: L505
           http://z505.com

}

unit pwbuildutil; {$mode objfpc} {$h+}

interface
uses
  pwdirutil;

type astr = ansistring;
     AstrArray = array of astr;
     bln = boolean;
     num  = integer;                                                   
//type TMoreEnum =  (DEFINES, UNITPATHS, INCPATHS);

  TFpcOptions = record
    Dir,              // working directory
    Crapdir,          // subdir for .o/.a/.ppu files (relative to working dir)
    ProgBinFile,      // program file name
    ProgBinDir: astr; // program bin    
    SmartStrip,       // -XX -CX -Xs
    Compile,
    Rebuild,
    CleanBeforeRebuild,
    IgnoreErr: bln;   // ignore compiler errors
    Extra: astr;
    FpcVersion: astr;
    intern: record    // private, not for interface user to worry about
      defines,                                                         
      incpaths, 
      unitpaths: AstrArray;
      Inited: bln;           // signals whether record has been initialized
      craptargetdir: astr;   // i.e. /.crap/i386-win32/
      progbintargetdir: astr;   // i.e. /bin/i386-win32/
    end;
  end;

procedure Init(out opts: TFpcOptions);

function Compile(const srcunit, opts, fpversion: astr; IgnoreErr: bln): num;
function Compile(const srcunit: astr; var opts: TFpcOptions): num;
function Compile(const srcunit, opts, fpversion: astr): num;
function Compile(const srcunit, opts: astr): num;

procedure CompileMany(paths: PathArray; var opts: TFpcOptions; ShowSeparator: bln);
procedure CompileMany(paths: PathArray; var opts: TFpcOptions);

procedure ShowOpts(var opts: TFpcOptions);

function Build(const srcunit, opts, fpversion: astr; IgnoreErr: bln): num;
function Build(const srcunit, opts, fpversion: astr): num;

function RunCmd(const path, comline: astr): num;
function RunCmd(const path: astr; const comline: array of astr): num;
procedure AddUnitPath(var opts: TFpcOptions; s: astr);
procedure AddIncPath(var opts: TFpcOptions; s: astr);
procedure AddDefine(var opts: TFpcOptions; s: astr);

procedure ResetUnitPaths(var opts: TFpcOptions; s: astr);
procedure ResetIncPaths(var opts: TFpcOptions);
procedure ResetDefines(var opts: TFpcOptions);

procedure WriteSeparator;

(* Todo: 
    -zip (tar, winzip, etc) functions
    -with blacklist of files/directories not needing to be compiled?
    -function to find all .pp/.pas files in a folder and compile all of them
*)

implementation

uses
  strutils, sysutils, pwfileutil, pwfputil, pwtypes;

procedure noteln(const s: string);
begin
  writeln('Note: ', s);
end;

{ halt program with polite message }
procedure HaltErr(s: astr);
begin
  noteln('quit early: ' + s);
  halt;
end;

procedure HaltErr(s: astr; i: num);
var tmp: astr = '';
begin
  str(i, tmp);
  noteln('quit early: ' + s + tmp);
  halt;
end;

procedure WriteSeparator;
begin
  writeln('-------------------------------------------------------------------------------');
  writeln('-------------------------------------------------------------------------------');
end;

procedure AstrArrayAdd(var a: AstrArray; s: string);
var len: num;
begin      
  if s = '' then exit;
  len:= length(a);
  setlength(a, len + 1);
  a[len]:= s;
end;

procedure AstrArrayReset(var a: AstrArray);
begin
  setlength(a, 0);
end;

procedure CheckIfOptsInited(const opts: TFpcOptions);
begin
  if not opts.intern.inited then 
    HaltErr('"opts" record must be inited before using');
end;


{ must call this to ensure Record is initialized }
procedure Init(out opts: TFpcOptions);
begin
  with opts do begin
    SmartStrip:= false; Rebuild:= false; IgnoreErr:= false;
    Extra:= ''; CrapDir:= '.crap';  ProgBinFile:= ''; ProgBinDir:= ''; Dir:= '';
    // default version is current compiler of this unit
    FpcVersion:= pwfputil.FpcVersion();
    CleanBeforeRebuild:= false;
    Compile:= true;
    with intern do begin
      // signal to other functions we are initialized
      intern.Inited:= true;
      setlength(defines, 0);
      setlength(incpaths, 0);
      setlength(unitpaths, 0);
      craptargetdir:= '';
      progbintargetdir:= '';
    end;
  end;
end;


procedure AddUnitPath(var opts: TFpcOptions; s: astr);
begin
  // must be initialized first 
  CheckIfOptsInited(opts);
  AstrArrayAdd(opts.intern.unitpaths, s);
end;


procedure ResetUnitPaths(var opts: TFpcOptions; s: astr);
begin
  // must be initialized first 
  CheckIfOptsInited(opts);
  AstrArrayReset(opts.intern.unitpaths);
end;

procedure AddIncPath(var opts: TFpcOptions; s: astr);
begin
  // must be initialized first 
  CheckIfOptsInited(opts);
  AstrArrayAdd(opts.intern.incpaths, s);
end;

procedure ResetIncPaths(var opts: TFpcOptions);
begin
  // must be initialized first 
  CheckIfOptsInited(opts);
  AstrArrayReset(opts.intern.incpaths);
end;

procedure AddDefine(var opts: TFpcOptions; s: astr);
begin
  // must be initialized first 
  CheckIfOptsInited(opts);
  AstrArrayAdd(opts.intern.defines, s);
end;

procedure ResetDefines(var opts: TFpcOptions);
begin
  // must be initialized first 
  CheckIfOptsInited(opts);
  AstrArrayReset(opts.intern.defines);
end;

procedure CleanUnitCrap(const path: astr);
const
  masks: array [1..5] of string[5]
    = ('*.ppu', '*.dcu','*.a', '*.res','*.o');
var
  i: integer;
begin
  noteln('Removing files from dir: ' + path);
  for i:= low(masks) to high(masks) do begin
    if not DelFiles(path, masks[i]) then
      HaltErr('did not delete at least 1 file in '+path);
  end;
end;

{ makes options Record into a string like '-Fu/path -oProg' }
function MakeOpts(var opts: TFpcOptions): string;
var allopts: astr = '';

  procedure AddSimpleOpts(const opt: astr);
  begin
    allopts:= allopts+' '+opt;
  end;

  procedure AddOpts(const prefix, opt: astr);
  begin
    if length(opt) > 0 then allopts:= allopts+' '+prefix+opt;
  end;

  procedure AddStrArrayOpts;

    procedure Add(prefix: astr; arr: AstrArray);
    var i: longword;
    begin
      if length(arr) > 0 then
        for i:= low(arr) to high(arr) do if length(arr[i]) > 0 then
          AddOpts(prefix, arr[i]);      
    end;

  begin
     Add('-d', opts.intern.defines);
     Add('-Fi', opts.intern.incpaths);
     Add('-Fu', opts.intern.unitpaths);
  end;

  procedure AddTrailSlashes;
  var len: num;
  begin
    len:= length(opts.dir);
    if len > 0 then
      case opts.dir[len] of
        '\',  '/': ;
      else // add trailing slash if none
        opts.dir:= opts.dir + SLASH;
      end;

    len:= length(opts.crapdir);
    if len > 0 then
      case opts.crapdir[len] of
        '\',  '/': ;
      else // add trailing slash if none
        opts.crapdir:= opts.crapdir + SLASH;
      end;
  end;                          

var targetdir: string;

begin
  // must be initialized first 
  CheckIfOptsInited(opts);
  AddTrailSlashes;
  AddStrArrayOpts;                            
  if opts.smartstrip then AddSimpleOpts('-XX -CX -Xs');
  if opts.rebuild then AddSimpleOpts('-B');

  if opts.crapdir <> '' then begin
    targetdir:= opts.dir + opts.crapdir + FpcTargetDir();
    if opts.CleanBeforeRebuild then begin
      CleanUnitCrap(targetdir);
    end;
    opts.intern.craptargetdir:= targetdir;
    ForceDir(targetdir);
  end;

  if opts.progbindir <> '' then begin
    targetdir:= opts.dir + opts.progbindir + FpcTargetDir();
    ForceDir(targetdir); 
    opts.intern.progbintargetdir:= targetdir;
  end;

  AddOpts('-FU', opts.intern.craptargetdir);
  AddOpts('-FE', opts.intern.progbintargetdir);
  AddOpts('-o', opts.progbinfile);
  AddSimpleOpts(opts.extra);
  result:= allopts;
end;

{ writes Record options to screen as a string }
procedure ShowOpts(var opts: TFpcOptions);
begin
  // must be initialized first 
  CheckIfOptsInited(opts);
  writeln(makeopts(opts));
end;

{ todo: windows ../units }
function GetFpcUnitBasePath(const fpversion: astr): astr;
begin
  result:= GetEnvironmentVariable('FPCDIR');
{$IFDEF UNIX}
  if result = '' then
  begin
    result:= '/usr/local/lib/fpc/'+fpversion;
    if not DirectoryExists(result) and
       DirectoryExists('/usr/lib/fpc/'+fpversion) 
    then
      result:= '/usr/lib/fpc/'+fpversion;
  end;
{$ENDIF}

end;

{ adds trailing slash if not already there 
  TODO: move function to pwdirutil/strwrap1/pwfileutil/}
procedure ForceTrailSlash(var path: astr);
var len: integer;
begin
  len:= length(path); 
  if len < 1 then exit;
  if path[len] <> SLASH then path:= path + SLASH;
end;


procedure GetFpcPath(var fpcpath: astr);
var found: astr;
begin
  if (fpcpath = '') then
  begin
    fpcpath:= 'fpc';
    found:= FileSearch(fpcpath, GetEnvironmentVariable('PATH'));
    if (found <> '') then fpcpath:= found;
  end;
end; 
                     
function GetFpcFullPath: astr;
begin
  {$IFDEF WINDOWS}
    // executeprocess() launches 'fpc' fine on MS Windows
    result:= 'fpc';
  {$ENDIF}
  {$IFDEF UNIX}
    // unix executeprocess() is not as path smart
    GetFpcPath(result);
  {$ENDIF}
end;

{ compile program with options string }
function Compile(const srcunit, opts, fpversion: astr; IgnoreErr: bln): num;
begin
  result:= ExecuteProcess(GetFpcFullPath(), SrcUnit+' '+opts);
  if (result<>0) and (not IgnoreErr) then
    HaltErr('compiler returned error: ', result);
end;

{ other }
function Compile(const srcunit, opts, fpversion: astr): num;
begin
  result:= Compile(srcunit, opts, fpversion, false);
end;

{ other }
function Compile(const srcunit, opts: astr): num;
begin
  result:= Compile(srcunit, opts, FpcVersion(), false);
end;

{ compile program with options in a record }
function Compile(const srcunit: astr; var opts: TFpcOptions): num;
var madeopts: astr;
begin
  madeopts:= makeopts(opts);
  if not opts.Compile then exit; // just clean or do other tasks
  result:= Compile(opts.dir+srcunit, madeopts, opts.FpcVersion, opts.IgnoreErr);
end;

procedure CompileMany(paths: PathArray; var opts: TFpcOptions; ShowSeparator: bln);
var i: integer;
begin
  if length(paths) < 1 then exit;
  for i:= low(paths) to high(paths) do begin
    opts.dir:= paths[i].path;
    Compile(paths[i].fname, opts);
    if ShowSeparator then WriteSeparator; // shows ----------- lines
  end;
end;

procedure CompileMany(paths: PathArray; var opts: TFpcOptions);
begin
  CompileMany(paths, opts, true);
end;

{ same as compile but forces fpc build -B }
function Build(const srcunit, opts, fpversion: astr; IgnoreErr: bln): num;
begin
  result:= Compile(srcunit, '-B ' + opts, fpversion, IgnoreErr);
end;

{ default }
function Build(const srcunit, opts, fpversion: astr): num;
begin
  result:= Build(srcunit, opts, fpversion, false);
end;

{ simple way to run a command }
function RunCmd(const path, comline: astr): num;
begin
  result:= ExecuteProcess(path, comline);
end;

{ overloaded with array }
function RunCmd(const path: string; const comline: array of astr): num;
begin
  result:= ExecuteProcess(path, comline);
end;

end.



{function compile(const srcunit: astr; const opts: TFpcOptions): integer;
var allopts: astr = '';

  procedure AddOpts(const opts: astr);
  begin
    allopts:= allopts+' '+opts;
  end;

  procedure AddStrArrayOpts;

    procedure Add(prefix: astr; arr: AstrArray);
    var i: longword;
    begin
    if length(arr) > 0 then
      for i:= low(arr) to high(arr) do 
        AddOpts(prefix + arr[i]);      
    end;

  begin
     Add('-d', opts.defines);
     Add('-Fi', opts.incpaths);
     Add('-Fu', opts.unitpaths);
  end;

begin
  if not opts.intern.inited then halterr('"opts" record must be inited before using');
  AddStrArrayOpts;
  if opts.smartstrip then AddOpts('-XX -CX -Xs');
  if opts.build then AddOpts('-B');
  AddOpts('-FU'+opts.OutCrapPath);
  AddOpts('-o'+opts.outprog);
  AddOpts(opts.extra);
  result:= compile('fpc', srcunit+' '+allopts);
end;}
