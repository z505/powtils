{ makes (builds) all Powtils examples, By L505 }

program build; {$mode objfpc} {$H+} 

uses 
  pwtypes in '../main/pwtypes.pas',
  pwdirutil in '../main/pwdirutil.pas',
  pwbuildutil in '../main/pwbuildutil.pas';


type
  eNames = (tGzipOn, tGzipSysutilsOn, tSysutilsOn);
const
  names: array [eNames] of str15 =
    ('gzipon', 'gzipsysutilson', 'systutilson');


procedure BuildExamples(var Paths: TPaths);
var o: TFpcOptions;
    all: boolean;
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

  if (all) or (group = names[tGzipOn]) then begin
    AddDefine(o, 'GZIP_ON'); 
    o.Name:= group;
    o.ProgBinDir:= 'bin-'+names[tGzipOn]; 
    CreateGroup(Paths,  o);
  end;

  if (all) or (group = names[tGzipSysutilsOn]) then begin
    AddDefine(o, 'SYSUTILS_ON');
    o.Name:= group;
    o.ProgBinDir:= 'bin-'+names[tGzipSysutilsOn]; 
    CreateGroup(Paths,  o);
  end;

  if (all) or (group = names[tSysutilsOn]) then begin
    ResetDefines(o);
    AddDefine(o, 'SYSUTILS_ON');
    o.ProgBinDir:= 'bin-'+names[tSysutilsOn]; 
    o.Name:= group;
    CreateGroup(Paths,  o);
  end;

  Run();

end;

var Paths: TPaths;

begin
  // visible for HELP command
  SetVisibleGroups(names);
  // suck em up
  GetDirFiles('./', '*.dpr', Paths);
  if Paths.count < 0 then HaltErr('Path problem getting example dpr files');
  BuildExamples(Paths);
end.


