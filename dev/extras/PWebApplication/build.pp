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


type
  eNames = (tNone);
const
  names: array [eNames] of str15 =
    ('none');


procedure BuildExamples(var Paths: TPaths);
var o: TFpcOptions;
    all: boolean;
begin
  Init(o);
  o.smartstrip:= true;
  AddUnitPath(o, '../../main/');

  all:= doingall();

  if (all) or (doingdefault) then begin
    o.ProgBinDir:= 'bin';
    o.Name:= 'default';
    CreateGroup(paths, o);
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


