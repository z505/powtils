{ makes (builds) all Powtils examples, currently for building with FPC only
  (dcc32 build system is possible in the future)

  By L505 (Lars)
  http://z505.com
}

program build; {$mode objfpc} {$H+} 

uses 
  pwtypes in '../main/pwtypes.pas',
  arrayfuncs in '../main/arrayfuncs.pas',
  pwfputil in '../main/pwfputil.pas',
  pwstrutil in '../main/pwstrutil.pas',
  pwsubstr in '../main/pwsubstr.pas',
  pwfileutil in '../main/pwfileutil.pas',
  pwdirutil in '../main/pwdirutil.pas',
  pwbuildutil in '../main/pwbuildutil.pas';


type
  eNames = (tGzipOn, tGzipSysutilsOn, tSysutilsOn);
const
  names: array [eNames] of str15 =
    ('gzipon', 'gzipsysutilson', 'sysutilson');


procedure BuildExamples(var Paths: TPaths);
var o: TFpcOptions;
    all: boolean;
    curgroup: astr;
begin
  Init(o);
  o.smartstrip:= true;
  AddUnitPath(o, '../main/');
  AddUnitPath(o, 'code-pastie/hiliter/');

  all:= doingall();

  if (all) or (doingdefault) then begin
    o.ProgBinDir:= 'bin';
    o.Name:= 'default';
    CreateGroup(paths, o);
  end;

  // now build other copies of the program with custom DEFINES
  curgroup:= names[tGzipOn];
  if (all) or (group = curgroup) then begin
    AddDefine(o, 'GZIP_ON'); 
    o.Name:= curgroup;
    o.ProgBinDir:= 'bin-'+curgroup; 
    o.Rebuild:= true;
    CreateGroup(Paths,  o);
  end;

  curgroup:= names[tGzipSysutilsOn];
  if (all) or (group = curgroup) then begin
    ResetDefines(o);
    AddDefine(o, 'GZIP_ON'); 
    AddDefine(o, 'SYSUTILS_ON');
    o.Name:= curgroup;
    o.ProgBinDir:= 'bin-'+curgroup; 
    o.Rebuild:= true;
    CreateGroup(Paths,  o);
  end;

  curgroup:= names[tSysutilsOn];
  if (all) or (group = curgroup) then begin
    ResetDefines(o);
    AddDefine(o, 'SYSUTILS_ON');
    o.ProgBinDir:= 'bin-'+curgroup; 
    o.Name:= curgroup;
    o.Rebuild:= true;
    CreateGroup(Paths,  o);
  end;

  Run();

end;

var Paths: TPaths;

begin
  // visible for HELP command
  SetVisibleGroups(names);
  // get all .DPR files to compile
  GetSubdirFiles('./', '*.dpr', Paths);
  if Paths.count < 0 then HaltErr('Path problem getting example *.DPR files');
  BuildExamples(Paths);
end.


