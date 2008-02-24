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
    writeln('debug DEAFULT');
    o.ProgBinDir:= 'bin';
    o.Name:= 'default';
    CreateGroup(paths, o);
  end;

  curgroup:= names[tGzipOn];
  if (all) or (group = curgroup) then begin
    writeln('debug GZIP_ON');
    AddDefine(o, 'GZIP_ON'); 
    o.Name:= curgroup;
    o.ProgBinDir:= 'bin-'+curgroup; 
    CreateGroup(Paths,  o);
  end;

  curgroup:= names[tGzipSysutilsOn];
  if (all) or (group = curgroup) then begin
    writeln('debug GZIP_ON SYSUTILS_ON');
    ResetDefines(o);
    AddDefine(o, 'GZIP_ON'); 
    AddDefine(o, 'SYSUTILS_ON');
    o.Name:= curgroup;
    o.ProgBinDir:= 'bin-'+curgroup; 
    CreateGroup(Paths,  o);
  end;

  curgroup:= names[tSysutilsOn];
  if (all) or (group = curgroup) then begin
    writeln('debug SYUSTILS_ON');
    ResetDefines(o);
    AddDefine(o, 'SYSUTILS_ON');
    o.ProgBinDir:= 'bin-'+curgroup; 
    o.Name:= curgroup;
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


