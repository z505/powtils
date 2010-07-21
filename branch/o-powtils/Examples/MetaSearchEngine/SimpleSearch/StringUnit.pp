unit StringUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{
Encode the words in UTF-8, Windows-1256 and
}
function EncodeWords (const Words: TStringList; Encoding: Integer): TStringList; inline;


implementation

  {
  Encode the words in UTF-8, Windows-1256 and
  }
  function EncodeWords (const Words: TStringList; Encoding: Integer): TStringList;
  const
   Ye1= 'ی';
   Ye2= 'ﻱ';
   Ye3= 'ﺉ';
   YeHa: array [0..2] of String= (Ye1, Ye2, Ye3);

   Ke1= 'ک';
   Ke2= 'ﻙ';
   Ke3= 'ک';
   KeHa: array [0..2] of String= (Ke1, Ke2, Ke3);

   YeUTF8= #$DB#$8c;
   KeUTF8= #$DA#$A9;


  var
    i, j: Integer;
    c: Char;
    Pc: PChar;

  begin
    Result:= TStringList.Create;
    Result.Capacity:= Words.Count;

    for i:= 0 to Words.Count- 1 do
    begin
      j:= Pos (Words [i], YeUTF8);
      Result.Add (Words [i]);


      while j<> 0 do
      begin
        pc:= @(Result [i][j]);
        pc^:= YeHa [Encoding, 1];
        Inc (pc);
        pc^:= YeHa [Encoding, 2];

        j:= Pos (Result [i], YeUTF8);

      end;

      j:= Pos (Result [i], KeUTF8);

      while j<> 0 do
      begin
        pc:= @(Result [i][j]);
        pc^:= KeHa [Encoding, 1];
        Inc (pc);
        pc^:= KeHa [Encoding, 2];

        j:= Pos (Result [i], KeUTF8);

      end;

    end;

  end;

end.

