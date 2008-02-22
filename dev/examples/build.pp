{ makes (builds) all Powtils examples, By L505 }

program build; {$mode objfpc} {$H+} 

uses 
  pwtypes in '../main/pwtypes.pas',
  pwdirutil in '../main/pwdirutil.pas',
  pwbuildutil in '../main/pwbuildutil.pas';


type
  eNames = (tGzipOn, tGzipSysutilsOn, tSysutilsOn);
const
  names: array [eNames] of string20 =
    ('gzipon', 'gzipsysutilson', 'systutilson');


procedure BuildExamples(const Paths: PathArray);
var o: TFpcOptions;
    all: boolean;
begin
  Init(o);
  o.smartstrip:= true;
  AddUnitPath(o, '../main/');

  all:= doingall();

  if (all) or (doingdefault) then begin
    o.ProgBinDir:= 'bin';
    o.Name:= 'default';
    NewGroup(paths, o);
  end;

  if (all) or (group = names[tGzipOn]) then begin
    AddDefine(o, 'GZIP_ON'); 
    o.Name:= group;
    o.ProgBinDir:= 'bin-'+group; 
    NewGroup(Paths,  o);
  end;

  if (all) or (group = names[tGzipSysutilsOn]) then begin
    AddDefine(o, 'SYSUTILS_ON');
    o.Name:= group;
    o.ProgBinDir:= 'bin-'+group; 
    NewGroup(Paths,  o);
  end;

  if (all) or (group = names[tSysutilsOn]) then begin
    ResetDefines(o);
    AddDefine(o, 'SYSUTILS_ON');
    o.ProgBinDir:= 'bin-'+group; 
    o.Name:= group;
    NewGroup(Paths,  o);
  end;

  Run();

end;

var Paths: PathArray;

begin
  GetDirFiles('./', '*.dpr', Paths);
  BuildExamples(Paths);
end.


