unit pwfputil; {$mode objfpc} {$H+}

interface

function FpcTargetDir: string;
function FpcVersion: string;

implementation


function FpcTargetDir: string;
begin
  result:= {$I %FPCTARGETCPU%}+'-'+{$I %FPCTARGETOS%};
  result:= lowercase(result);
end;

function FpcVersion: string;
begin
  result:= {$I %FPCVERSION%};
end;

end.