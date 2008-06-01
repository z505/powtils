{ makes (builds) all Powtils examples, currently for building with FPC only 
  (dcc32 build system is possible in the future)

  By L505 (Lars)
  http://z505.com
}

program build; {$mode objfpc} {$H+} 

uses 
  pwtypes in '../main/pwtypes.pas',
  pwdirutil in '../main/pwdirutil.pas',
  pwbuildutil in '../main/pwbuildutil.pas';


type
  eNames = (tGzipOn, tGzipSysutilsOn, tSysutilsOn);
const
  names: array [eNames] of str15 =
    ('gzipon', 'gzipsysutilson', 'sysutilson');


procedure BuildExamples(var Paths: TPaths);
var g: TGroup;
    all: boolean;
    curgroup: astr;
begin
  Init(g);
  g.smartstrip:= true;
  AddUnitPath(g, '../main/');
  AddUnitPath(g, 'code-pastie/hiliter/');

  all:= doingall();

  if (all) or (doingdefault) then begin
    g.ProgBinDir:= 'bin';
    g.Name:= 'default';
    CreateGroup(g, paths);
  end;

  // now build other copies of the program with custom DEFINES
  curgroup:= names[tGzipOn];
  if (all) or (group = curgroup) then begin
    AddDefine(g, 'GZIP_ON'); 
    g.Name:= curgroup;
    g.ProgBinDir:= 'bin-'+curgroup; 
    g.Rebuild:= true;
    CreateGroup(g, Paths);
  end;

  curgroup:= names[tGzipSysutilsOn];
  if (all) or (group = curgroup) then begin
    ResetDefines(g);
    AddDefine(g, 'GZIP_ON'); 
    AddDefine(g, 'SYSUTILS_ON');
    g.Name:= curgroup;
    g.ProgBinDir:= 'bin-'+curgroup; 
    g.Rebuild:= true;
    CreateGroup(g, Paths);
  end;

  curgroup:= names[tSysutilsOn];
  if (all) or (group = curgroup) then begin
    ResetDefines(g);
    AddDefine(g, 'SYSUTILS_ON');
    g.ProgBinDir:= 'bin-'+curgroup; 
    g.Name:= curgroup;
    g.Rebuild:= true;
    CreateGroup(Paths,  g);
  end;

  Run();

end;

var Paths: TPaths;

begin
  // visible for HELP command
  SetVisibleGroups(names);
  // get all .DPR files to compile
  GetDirFiles('./', '*.dpr', Paths);
  if Paths.count < 1 then HaltErr('Path problem getting example *.DPR files');
  BuildExamples(Paths);
end.


