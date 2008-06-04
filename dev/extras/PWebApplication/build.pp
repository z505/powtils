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

type NameCount = (tNone);
const names: array [NameCount] of str15 = ('none');
var g: TGroup;

procedure BuildExamples(var Paths: TPaths);
begin
  Init(g);
  g.smartstrip:= true;
  AddUnitPath(g, '../../main/');
  AddExtraOpt(g, '-Sd'); // mode delphi
  if (doingAll) or (doingDefault) then begin
    g.ProgBinDir:= 'bin';
    g.Name:= 'default';
    CreateGroup(g, paths);
  end;
  Run();
end;


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
  Paths:= GetSubdirFiles('./', '*.dpr');
  if Paths.count < 1 then HaltErr('Can''t find any *.DPR files');
  BuildExamples(Paths);
end;

begin
  // visible for HELP command
  SetVisibleGroups(names);
  Build;
  CopyNeededFiles;
end.


