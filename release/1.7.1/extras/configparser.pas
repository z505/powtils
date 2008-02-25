{ This unit was created so that the user can access any configuratoin
  file of his choice, not just the main configuration file.

 The idea and code is from psp 1.3
 
 L505

}

unit ConfigParser;

{$MODE OBJFPC} {$H+} 

interface

uses

const 
  fmOpenRead = $0000;   


function GetConfigValue(fileName, confName: string): string;

implementation

function GetConfigValue(fileName, confName: string): string;
var
  fh: text;
  buff: string;
  conf: array[1..2] of string;
  c: char;
  len,
  i,
  cnt: integer;
  spaces: boolean;
  OldFileMode: integer;
begin
  result := ''; //safety
  OldFileMode:= FileMode;
  FileMode:= fmOpenRead; //open read only. Web files do not always have write permissions. We don't need write access here.
  assign(fh, fileName);
  reset(fh);
  while not eof(fh) do
  begin
    readln(fh, buff);
    len := length(buff);
    conf[1] := '';
    conf[2] := '';
    spaces := false;
    cnt := 1;
    for i := 1 to len do
    begin
      c := buff[i];
      if (c <> '=') and (c <> '#') and (c <> ' ') and (c <> '"') then
        conf[cnt] := conf[cnt] + c;
      if c = '=' then cnt := 2;
      if (c = '"') then
      begin
        if spaces then
          spaces:= false
        else
          spaces:= true;
      end;
      if (c = ' ') and spaces then
        conf[cnt]:= conf[cnt] + c;
      if c = '#' then
        break;
    end;
    if conf[1] = confName then
      break;
  end;
  close(fh);
  //filemode should be set back to default
  FileMode:= OldFileMode; 
  result := conf[2];
end;

end.
