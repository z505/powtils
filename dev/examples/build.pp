{ makes (builds) all Powtils examples, By L505 }

program build; {$mode objfpc} {$H+} 

uses 
  dirutils in '../main/pwdirutil',
  pwbuildutil in '../main/pwbuildutil';

procedure CompileExamples(const Paths: PathArray);
var Opts: TFpcOptions; 
begin
  Init(Opts);
  // o.Build:= true;
  AddUnitPath(Opts, '../main/');
  CompileMany(Paths,  Opts);

  Init(Opts);
  AddUnitPath(Opts, '../main/');
  AddDefine(Opts, 'GZIP_ON');
  CompileMany(Paths,  Opts);

  Init(Opts);
  AddUnitPath(Opts, '../main/');
  AddDefine(Opts, 'GZIP_ON');
  AddDefine(Opts, 'SYSUTILS_ON');
  CompileMany(Paths,  Opts);

  Init(Opts);
  AddUnitPath(Opts, '../main/');
  AddDefine(Opts, 'SYSUTILS_ON');
  CompileMany(Paths,  Opts);

end;

var Paths: PathArray;

begin
  GetDirFiles('./', '*.dpr', Paths);
  CompileExamples(Paths);
end.


