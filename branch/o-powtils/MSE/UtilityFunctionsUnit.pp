unit UtilityFunctionsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

procedure RemoveSubstrFromString (var Str: String; SubStr: String);

implementation

procedure RemoveSubstrFromString (var Str: String; SubStr: String);
var
  Index, Len: Integer;
  
begin
  Index:= Pos (SubStr, Str);
  Len:= Length (SubStr);
  
  while Index<> 0 do
  begin
    Delete (Str, Index, Len);
    Index:= Pos (SubStr, Str);

  end;
  
end;

end.

