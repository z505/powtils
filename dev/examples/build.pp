{ makes (builds) all Powtils examples, By L505 }

program build; {$mode objfpc} {$H+} 

uses 
  dirutils in '../main/pwdirutil',
  pwbuildutil in '../main/pwbuildutil';

function checkdorebuild: boolean;
begin
  result:= false;
  if paramstr(2) = 'rebuild' then result:= true;
end;

function checkdoall: boolean;
begin
  result:= false;
  if paramstr(1) = 'all' then result:= true;
end;

procedure CompileExamples(const Paths: PathArray);
var opts: TFpcOptions;
    all: boolean;
begin
  Init(opts);
  opts.Build:= checkdorebuild();
  AddUnitPath(opts, '../main/');

  all:= checkdoall();

  if (all) or (paramstr(1) = '') then begin
    opts.ProgBinDir:= 'bin/';
    CompileMany(Paths,  opts);
  end;

  if (all) or (paramstr(1) = 'gzipon') then begin
    ResetDefines(opts);
    AddDefine(opts, 'GZIP_ON');
    opts.ProgBinDir:= 'bin-gzip_on/';
    CompileMany(Paths,  opts);
  end;

  if (all) or (paramstr(1) = 'gzipsysutilson') then begin
    ResetDefines(opts);
    AddDefine(opts, 'SYSUTILS_ON');
    AddDefine(opts, 'GZIP_ON');
    opts.ProgBinDir:= 'bin-sysutils_on-gzip_on/';
    CompileMany(Paths,  opts);
  end;

  if (all) or (paramstr(1) = 'sysutilson') then begin
    ResetDefines(opts);
    AddDefine(opts, 'SYSUTILS_ON');
    opts.ProgBinDir:= 'bin-sysutils_on/';
    CompileMany(Paths,  opts);
  end;

end;

var Paths: PathArray;

begin
  GetDirFiles('./', '*.dpr', Paths);
  CompileExamples(Paths);
end.


