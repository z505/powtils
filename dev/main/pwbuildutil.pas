{ PowBuild - utilities for automating builds/makes/packages

  Useful for automating and building demos, projects, etc. 
  
  Fpmake and fpcmake are not serving my simple needs. These utilities are born 
  due to my hatred toward GNU Make files (ugliest f*cking cow sh*t ever seen by 
  man).

  Notes: 
    - df is an alias for "default" for quick command line typing
    - ".crap/" directory coined by Lars Olson for .ppu/.a files
    - CleanBeforeRebuild can be set to force

  NOTE: these build utils don't always take advantage of "reusing" compiled
  global PPU files. Other "Make" utilties do. More of this will be implemented 
  in the future. However, I've had "PPU Hell" and "PPU conflicts" when trying 
  to reuse global PPU files (and some projects require regenerating .PAS 
  sources with DEFINES and include file issues. Fpc has bugs too). Compilations 
  may be slower if local ppu copies are generated - but I've rarely found super 
  fast compilations to be that great compared to "CORRECT compilations" that 
  reduce headaches! 
  
  With time... these utils will contain more features.
 
  Authors: L505
           http://z505.com

  License: NRCOL (public domain)
}

unit pwbuildutil; {$mode objfpc} {$H+} {$R+}

interface
uses
  pwtypes,
  pwdirutil,
  arrayfuncs;

type 
  TGroup = record
    Name: str15;        // group name
    Dir,                // working directory
    Crapdir,            // subdir for .o/.a/.ppu files (relative to working dir)
    ProgBinFile,        // exe output file name
    ProgBinDir: astr;   // exe output directory
    SmartStrip,         // -XX -CX -Xs
    Compile,
    Rebuild,
    CleanBeforeRebuild,
    IgnoreErr: boo;     // ignore compiler errors
    Extra: astr;                 
    FpcVersion: astr;
    priv: record      // private, not for interface user to worry about
      defines,                                                          
      incpaths, 
      unitpaths: AstrRay;
      craptargetdir: astr;   // i.e. /.crap/i386-win32/
      progtargetdir: astr;   // i.e. /bin/i386-win32/
    end;
  end;
 
  // deprecated, backwards compatible
  TFpcOptions = TGroup;


procedure NoteLn(const s: string);
procedure HaltErr(const s: astr);

procedure Init(out g: TGroup);

function GetProgTargetDir(groupidx: int32): string;

function Compile(const srcunit, opts, fpversion: astr; IgnoreErr: boo): int32;
function Compile(const srcunit: astr; var g: TGroup): int32;
function Compile(const srcunit, opts, fpversion: astr): int32;
function Compile(const srcunit, opts: astr): int32;

procedure CompileMany(var paths: TPaths; var g: TGroup; ShowSeparator: boo);
procedure CompileMany(var paths: TPaths; var g: TGroup);

procedure ShowOpts(var g: TGroup);

function Build(const srcunit, opts, fpversion: astr; IgnoreErr: boo): int32;
function Build(const srcunit, opts, fpversion: astr): int32;

function RunCmd(const path, comline: astr): int32;
function RunCmd(const path: astr; const comline: array of astr): int32;

procedure AddUnitPath(var g: TGroup; s: astr);
procedure AddUnitPaths(var g: TGroup; a: array of astr);

procedure AddIncPath(var g: TGroup; s: astr);
procedure AddIncPaths(var g: TGroup; a: array of astr);

procedure AddDefine(var g: TGroup; s: astr);
procedure AddDefines(var g: TGroup; a: array of astr);

procedure AddExtraOpt(var g: TGroup; s: astr);
procedure AddExtraOpts(var g: TGroup; a: array of astr);

procedure ResetUnitPaths(var g: TGroup);
procedure ResetIncPaths(var g: TGroup);
procedure ResetDefines(var g: TGroup);
procedure ResetExtraOpts(var g: TGroup);

procedure WriteSeparator;
procedure WriteSeparator1;

function Group: str15;
function Rebuilding: boolean;
function DoingAll: boolean;
function Cleaning: boolean;
function doingdefault: boolean;


procedure CreateGroup(g: TGroup; paths: TPaths);
procedure CreateGroup(paths: TPaths; g: TGroup); // deprecated

procedure Run;

procedure SetVisibleGroups(const names: str15ray);

(* Todo: 
    -zip (tar, winzip, etc) functions
    -with blacklist of files/directories not needing to be compiled?
    -function to find all .pp/.pas files in a folder and compile all of them
*)

implementation
uses strutils, sysutils, pwfileutil, pwfputil;

// pail: contains group settings, and corresponding file paths to compile
type
  TPailItem = record
    paths: TPaths;
    group: TGroup;
  end;

  TPail = array of TPailItem;

var // all build groups 
  Pail: TPail = nil;
  VisibleGroups: str15ray = nil;

function GetProgTargetDir(groupidx: int32): astr;
begin
  result:= '';
  if length(Pail)-1 < groupidx then exit;
  result:= Pail[groupidx].group.priv.progtargetdir;
end;

procedure SetVisibleGroups(const names: str15ray);
begin
  if length(names) > 0 then VisibleGroups:= AssignArray(names);
end;

{ display simple message }
procedure NoteLn(const s: string);
begin writeln('Note: ', s);
end;

{ warn with polite message (no halt) }
procedure WarnLn(const s: string);
begin writeln('Warning: ', s);
end;

{ halt program with polite message }
procedure HaltErr(const s: astr);
begin noteln('quit early: ' + s); halt;
end;

{ overloaded }
procedure HaltErr(s: astr; i: int32);
var tmp: astr = '';
begin str(i, tmp); noteln('quit early: ' + s + tmp); halt;
end;

procedure WriteSeparator;
begin
  writeln('-------------------------------------------------------------------------------');
  writeln('-------------------------------------------------------------------------------');
end;

procedure WriteSeparator1;
begin
  writeln('-------------------------------------------------------------------------------');
end;                

procedure WriteDoneSeparator;
begin 
  writeln('------------FINISHED---------------FINISHED---------------FINISHED-------------');
end;                

{ returns build group }
function Group: str15;
begin
  result:= paramstr(1);
end;

// default groups to compile such as: all, default, df (alias for default)
type eDefGroups = (gDefault,  gDf,  gAll);

// default flags such as clean, rebuild, 
type eDefFlags = (fRebuild,  fClean);

const defgroups: array [eDefgroups] of str15 = ('default', 'df', 'all');
      defflags: array [eDefFlags] of str15 = ('rebuild', 'clean');

function Flag: astr;
begin result:= paramstr(2);
end;

{ checks if we are running a full "rebuild" }
function Rebuilding: boolean;
begin
  result:= false;                          
  if flag = defflags[fRebuild] then result:= true;
end;

{ checks if we are running an "all" build }
function DoingAll: boolean;
begin
  result:= false;
  if group = 'all' then result:= true;   
end;

{ checks if we are running a "clean" }
function Cleaning: boolean;
begin
  result:= false;
  if flag = defflags[fClean] then result:= true;
end;

{ checks if we are running "default" build }
function DoingDefault: boolean;
begin
  result:= false;
  if group = defgroups[gDefault] then result:= true;
  if group = defgroups[gDf] then result:= true;
end;

{ console help }      
procedure ShowHelp;

  procedure IndentedLn(s: string);
  begin writeln('    ', s);
  end;

  procedure ShowLns(a: array of str15);
  var i: int32;
  begin for i:= low(a) to high(a) do IndentedLn(a[i]);
  end;

begin
  writeln('PowBuild by Z505 --> Syntax: build <group> <optional flag>');
  writeln('Type in "build help" to receive extended help information');
  writeln;
  if VisibleGroups = nil then HaltErr('Build author didn''t set visible bulid groups.');
  writeln(' Main Build Groups:');
  ShowLns(Defgroups);
  writeln(' Custom Build Groups:');
  ShowLns(VisibleGroups);
  writeln(' Flags:');
  ShowLns(DefFlags);
  writeln;
  writeln('i.e: build default        (the usual build)');
  writeln('     build df             (df is an alias for "default")');
  writeln('     build df clean       (clean all .ppu/.a files)');
  writeln('     build df rebuild     (builds using -B, also cleans .ppu/.a)');
  writeln('     build all            (compile all build groups, may take long)');
  writeln('     build all clean      (clean .ppu/.a files of all build groups)');
  writeln('     build all rebuild    (cleans all build groups, and builds using -B');
  writeln('     build foobar         (user defined build)');
  writeln('     build foobar clean   (etc. etc. etc.)');
  Halt;
end;

procedure ShowMissingGroupHelp;
begin
  WriteSeparator1;
  writeln('HELP: group (target) you specified was not found in this build system');
  WriteSeparator1;
  Halt;
end;

procedure ShowExtendedHelp;
begin
  writeln('EXTENDED HELP:');
  writeln;
  writeln('  PowBuild is similar to FpMake or Ruby Rake (worse or better!)');
  writeln('  More powerful than GNU make, as build files are true fpc programs');
  writeln('  A "build file" is a fpc program with pwbuildutil in uses clause');
  writeln('  A build file can use any unit in uses (sysutils, pwdirutil, etc.)');
  writeln;
  writeln('  See an example "build.pp" file in any directory.');
  writeln('  Once a build.pp file is found, compile it: "fpc build".');
  writeln('  Then run "build" (help) or "build default" (builds default group)');
  writeln;
  writeln('  A build.pp file must set "visible groups" for the users.');
  writeln('  Build files are "domain specific fpc programs" since they fill a niche');
  writeln;
  writeln('  A "group" is what you may know as a "target" from make files');
  writeln('  To build a specific group, type "build yourgroup"');  
  writeln;
  writeln('...more <enter>');  
  readln;
  WriteSeparator1;
  ShowHelp;
end;

function AllGroupNames: str15ray;
var i: int32;
begin
  if length(pail) < 1 then exit;
  setlength(result, length(pail));
  for i:= low(pail) to high(pail) do result[i]:= pail[i].group.name;
end;

{ ensures targets are setup right }
procedure CheckGroups;
  
  procedure FindGroups;
  var i1: eDefgroups;
      i2: int32;
      found: int32;
  begin
    found:= 0;
    // find default (df), all
    for i1:= low(defgroups) to high(defgroups) do begin
      if group() = defgroups[i1] then inc(found);
    end;
    for i2:= low(pail) to high(pail) do begin
      if pail[i2].group.name = group() then inc(found);
    end;
    if found < 1 then ShowMissingGroupHelp;
  end;

begin
  if paramcount < 1 then ShowHelp;
  if (paramcount = 1) then if paramstr(1) = 'help' then begin
    ShowExtendedHelp;
    exit;
  end;
   // only find groups in array if any specified at commandline
  if paramcount > 0 then FindGroups;
end;


procedure Run;
var i: int32;
begin
  Checkgroups;
  if length(pail) < 1 then HaltErr('Zero groups in pail available!');
  for i:= low(pail) to high (pail) do begin
    CompileMany(pail[i].paths, pail[i].group);
  end;
end;

{ add a group of files to be compiled with options }
procedure CreateGroup(g: TGroup; paths: TPaths);
var oldlen: int32;
begin
  if g.Name = '' then HaltErr('Must specify a name for each group.');
  oldlen:= length(pail);
  setlength(pail, oldlen+1);
  pail[oldlen].paths:= paths;
  pail[oldlen].group:= g;
end;

{ old deprecated function with wrong order of params }
procedure CreateGroup(paths: TPaths; g: TGroup);
begin CreateGroup(g, paths);
end;

procedure AddItem(var a: AstrRay; s: string);
var len: int32;
begin      
  if s = '' then exit;
  len:= length(a);
  setlength(a, len+1);
  a[len]:= s;
end;

procedure clear(var a: AstrRay);
begin
  setlength(a, 0);
end;

{ must call this to ensure Record is initialized }
procedure Init(out g: TGroup);
const cleared: TGroup = ();
begin
  g:= cleared;
  with g do begin
    CrapDir:= '.crap';  
    // default version is current compiler of this unit
    FpcVersion:= pwfputil.FpcVersion();
    Rebuild:= rebuilding();
    // if rebuilding or cleaning then CleanBeforeRebuild
    CleanBeforeRebuild:= (rebuild) or (cleaning);
    // only compile if we are not cleaning
    Compile:= not cleaning;
  end;
end;

{ add an -Fu path }
procedure AddUnitPath(var g: TGroup; s: astr);
begin
  AddItem(g.priv.unitpaths, s);
end;

{ multiple at once! :-) }
procedure AddUnitPaths(var g: TGroup; a: array of astr); var i: int32;
begin
  for i:= low(a) to high(a) do AddUnitPath(g, a[i]);
end;

{ add an -Fi path }
procedure AddIncPath(var g: TGroup; s: astr);
begin
  AddItem(g.priv.incpaths, s);
end;

{ multiple at once! :-) }
procedure AddIncPaths(var g: TGroup; a: array of astr); var i: int32;
begin
  for i:= low(a) to high(a) do AddIncPath(g, a[i]);
end;

{ adds extra compiler option,  i.e. -Sd  -Whatever  -blah }
procedure AddExtraOpt(var g: TGroup; s: astr);
begin
  g.extra:= g.extra + ' ' + s;
end;

{ multiple at once! :-) }
procedure AddExtraOpts(var g: TGroup; a: array of astr); var i: int32;
begin
  for i:= low(a) to high(a) do AddExtraOpt(g, a[i]);
end;

{ adds -dSOMEDEFINE }
procedure AddDefine(var g: TGroup; s: astr);
begin
  AddItem(g.priv.defines, s);
end;

{ multiple at once! :-) }
procedure AddDefines(var g: TGroup; a: array of astr); var i: int32;
begin
  for i:= low(a) to high(a) do AddDefine(g, a[i]);
end;

{ ... reset settings in record ...............................................}
procedure ResetExtraOpts(var g: TGroup);
begin
  g.extra:= '';
end;

procedure ResetUnitPaths(var g: TGroup);
begin
  clear(g.priv.unitpaths);
end;

procedure ResetIncPaths(var g: TGroup);
begin
  clear(g.priv.incpaths);
end;

procedure ResetDefines(var g: TGroup);
begin
  clear(g.priv.defines);
end;
{ ............................................................................}


{ delete .PPU/.A files a.k.a. "unit crap" }
procedure CleanUnitCrap(const path: astr);
const 
  masks: array [1..5] of string[5] = ('*.ppu', '*.dcu','*.a', '*.res','*.o');
var i, problems: int32;
begin
  problems:= 0;
  noteln('Cleaning: removing files from dir: ' + path);
  for i:= low(masks) to high(masks) do begin
    { warn if deletion of all files unsucessful }
    if not DelFiles(path, masks[i]) then inc(problems);
  end;
  if problems > 0 then WarnLn('did not delete at least 1 file in '+path);
end;

{ makes options Record into a string like '-Fu/path -oProg' }
function MakeOpts(var g: TGroup): string;
var allopts: astr = '';

  procedure AddSimpleOpts(const opt: astr);
  begin allopts:= allopts+' '+opt;
  end;

  procedure AddOpts(const prefix, opt: astr);
  begin if length(opt) > 0 then allopts:= allopts+' '+prefix+opt;
  end;

  procedure AddStrArrayOpts;

    procedure Add(prefix: astr; a: AstrRay);
    var i: int32;
    begin
      if length(a) > 0 then
        for i:= low(a) to high(a) do if length(a[i]) > 0 then
          AddOpts(prefix, a[i]);      
    end;

  begin
     Add('-d', g.priv.defines);
     Add('-Fi', g.priv.incpaths);
     Add('-Fu', g.priv.unitpaths);
  end;

  procedure AddTrailSlash(var path: string);
  begin path:= IncludeTrailingPathDelimiter(path);
  end;

  procedure AddTrailSlashes;
  begin
    AddTrailSlash(g.Dir); 
    AddTrailSlash(g.CrapDir); 
    AddTrailSlash(g.ProgBinDir);
  end;

var targdir: string;

begin
  targdir:= '';
  AddTrailSlashes;
  AddStrArrayOpts;                            
  if g.smartstrip then AddSimpleOpts('-XX -CX -Xs');
  if g.rebuild then AddSimpleOpts('-B');

  if g.crapdir <> '' then begin
    targdir:= g.dir + g.crapdir + FpcTargetDir();
    if g.CleanBeforeRebuild then CleanUnitCrap(targdir);
    g.priv.craptargetdir:= targdir;
    ForceDir(targdir);
  end;

  targdir:= '';                                 
  if g.progbindir <> '' then begin
    targdir:= g.dir + g.progbindir + FpcTargetDir();
    g.priv.progtargetdir:= targdir;
    if not ForceDir(targdir) then HaltErr('error creating folder: '+targdir); 
  end;

  AddOpts('-FU', g.priv.craptargetdir);
  AddOpts('-FE', g.priv.progtargetdir);
  AddOpts('-o', g.progbinfile);
  AddSimpleOpts(g.extra);
  result:= allopts;
end;

{ writes Record options to screen as a string }
procedure ShowOpts(var g: TGroup);
begin
  writeln(makeopts(g));
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
var len: int32;
begin
  len:= length(path); 
  if len < 1 then exit;
  if path[len] <> SLASH then path:= path + SLASH;
end;

{ retrieve location of fpc path on drive }
procedure GetFpcPath(var fpcpath: astr);
var found: astr;
begin
  if (fpcpath = '') then begin
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
function Compile(const srcunit, opts, fpversion: astr; IgnoreErr: boo): int32;
const ERR1 = 'compiler returned error: ';
begin
  result:= ExecuteProcess(GetFpcFullPath(), SrcUnit+' '+opts);
  if (result<>0) and (not IgnoreErr) then HaltErr(ERR1, result);
end;

{ other }
function Compile(const srcunit, opts, fpversion: astr): int32;
begin
  result:= Compile(srcunit, opts, fpversion, false);
end;

{ other }
function Compile(const srcunit, opts: astr): int32;
begin
  result:= Compile(srcunit, opts, FpcVersion(), false);
end;

{ compile program with options in a record }
function Compile(const srcunit: astr; var g: TGroup): int32;
var madeopts: astr;
    path: astr;
begin
  madeopts:= makeopts(g);
  if not g.Compile then exit; // just clean or do other tasks
  path:= g.dir+srcunit;
  writeln('>>> COMPILING: ', path);
  result:= Compile(path, madeopts, g.FpcVersion, g.IgnoreErr);
end;

procedure CompileMany(var paths: TPaths; var g: TGroup; ShowSeparator: boo);
var i: int32;
begin
  if paths.count < 1 then exit;
  WriteSeparator1;
  writeln('----- PROCESSING GROUP: ', g.Name, ' -----');
  WriteSeparator1;
  for i:= low(paths.items) to high(paths.items) do begin
    g.dir:= paths.items[i].path;
    Compile(paths.items[i].fname, g);
    if ShowSeparator then WriteSeparator1; // shows ----------- lines
  end;
  WriteDoneSeparator; // finishing separator :o)
end;

procedure CompileMany(var paths: TPaths; var g: TGroup);
begin
  CompileMany(paths, g, true);
end;

{ same as compile but forces fpc build -B }
function Build(const srcunit, opts, fpversion: astr; IgnoreErr: boo): int32;
begin
  result:= Compile(srcunit, '-B ' + opts, fpversion, IgnoreErr);
end;

{ default }
function Build(const srcunit, opts, fpversion: astr): int32;
begin
  result:= Build(srcunit, opts, fpversion, false);
end;

{ simple way to run a command }
function RunCmd(const path, comline: astr): int32;
begin
  result:= ExecuteProcess(path, comline);
end;

{ overloaded with array }
function RunCmd(const path: string; const comline: array of astr): int32;
begin
  result:= ExecuteProcess(path, comline);
end;

end.

(*
function compile(const srcunit: astr; const g: TGroup): int32;
var allopts: astr = '';

  procedure AddOpts(const opts: astr);
  begin
    allopts:= allopts+' '+opts;
  end;

  procedure AddStrArrayOpts;

    procedure Add(prefix: astr; arr: AstrRay);
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
  if not opts.priv.inited then halterr('"opts" record must be inited before using');
  AddStrArrayOpts;
  if opts.smartstrip then AddOpts('-XX -CX -Xs');
  if opts.build then AddOpts('-B');
  AddOpts('-FU'+opts.OutCrapPath);
  AddOpts('-o'+opts.outprog);
  AddOpts(opts.extra);
  result:= compile('fpc', srcunit+' '+allopts);
end;


procedure CheckIfOptsInited(const g: TGroup);
begin
  if not opts.priv.inited then 
    HaltErr('"opts" record must be inited before using');
end;
*)
