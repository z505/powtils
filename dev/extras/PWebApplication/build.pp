{ makes (builds) all Jorges template examples, currently for building with FPC 
  only  (dcc32 build system is possible in the future)

  By L505 (Lars)
  http://z505.com
}

program build; {$mode objfpc} {$H+} 

uses 
  pwtypes in '../../main/pwtypes.pas',
  pwdirutil in '../../main/pwdirutil.pas',
  pwbuildutil in '../../main/pwbuildutil.pas';

type eNames = (tNone);
const names: array [eNames] of str15 = ('none');
var o: TFpcOptions;

procedure BuildExamples(var Paths: TPaths);
var all: boolean;
begin
  Init(o);
  o.smartstrip:= true;
  AddUnitPath(o, '../../main/');
  AddExtraOpt(o, '-Sd'); // mode delphi
  all:= doingall();
  if (all) or (doingdefault) then begin
    o.ProgBinDir:= 'bin';
    o.Name:= 'default';
    CreateGroup(paths, o);
  end;
  Run();
end;

var tmp: astr;

procedure CopyNeededFiles;
begin
  NoteLn('COPYING HTML FILES');
  if not CloneFiles('examples', GetProgTargetDir(0), '*.template.html') then 
    writeln('Error copying html files');
  if not CloneFiles('examples', GetProgTargetDir(0), 'pwu_*.conf') then 
    writeln('Error copying config files');
end;


procedure Build;
var Paths: TPaths;
begin
  // get all .DPR files to compile
  GetDirFiles('./', '*.dpr', Paths, true);
  if Paths.count < 1 then HaltErr('Path problem getting example *.DPR files');
  BuildExamples(Paths);
end;

begin
  // visible for HELP command
  SetVisibleGroups(names);
  Build;
  CopyNeededFiles;
end.


