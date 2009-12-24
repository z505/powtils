unit JostarPageDispatcher;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, PageHandlerBaseUnit,AbstractHandlerUnit;

type

  { TJostarPageDispatcher }

  TJostarPageDispatcher= class (THTMLHandler)
  private

  public
    constructor Create;

    procedure MyDispatch; override;
    function CreateNewInstance: TAbstractHandler; override;

  end;

implementation
uses
  StringUnit;

{ TJostarPageDispatcher }

constructor TJostarPageDispatcher.Create;
begin
  inherited Create ('Index.psp', '/Jostar/Index.psp');

end;

const
  SpaceCharSet: set of char= [' ', #10, #13, #7];

procedure TJostarPageDispatcher.MyDispatch;

  function ParseQuery (const Query: AnsiString): TStringList;
  var
    i, l: Integer;
    CharPtr: PChar;
    ActiveWord: AnsiString;

  begin
    Result:= TStringList.Create;

    CharPtr:= @(Query [1]);
    i:= 1;
    l:= Length (Query);

    while i< l do
    begin

      while i< l do
      begin
        if not (CharPtr^ in SpaceCharSet) then
          Break;

        Inc (CharPtr);
        Inc (i);

      end;

      if CharPtr^= '"' then
      begin
        ActiveWord:= '"';

        while i< l do
        begin
          if CharPtr^<> '"' then
            Break;

          ActiveWord+= CharPtr^;
          Inc (CharPtr);
          Inc (i);

        end;

        ActiveWord+= '"';

        if ActiveWord<> '""' then
          Result.Add (ActiveWord);

      end
      else
      begin
        ActiveWord:= '';
        while i< l do
        begin
          if CharPtr^ in SpaceCharSet then
            Break;

          ActiveWord+= CharPtr^;
          Inc (CharPtr);
          Inc (i);

        end;

        if ActiveWord<> '' then
          Result.Add (ActiveWord);

      end;

    end;

  end;

var
  Query, Engines, Temp: AnsiString;
  Words: TStringList;
  WordsInEnc: array [0..3] of TStringList;
  Encoding: Integer;
  StartTime, EndTime: TTimeStamp;
  i: Integer;

begin
  StartTime:= DateTimeToTimeStamp (Time);

  Query:= Vars.CgiVarValueByName ['Q'];
  if Query= '' then
    Exit;

  Words:= ParseQuery (Query);
  Engines:= Vars.CgiVarValueByName ['Engines'];
  if Engines= '' then
    Engines:= 'GYM';

  Temp:= Parameter ['Enc'];
  try
    Encoding:= StrToInt (Temp);

  except
    on e: Exception do
      Encoding:= 7;

  end;

  FillChar (WordsInEnc, SizeOf (WordsInEnc), 0);

  for i:= 0 to 2 do
    if Encoding and (1 shl i)<> 0 then
      WordsInEnc [i]:= EncodeWords (Words, i);

  for i:= 1 to Length (Engines) do
    if Engines [i]= 'G' then
//      Goo


  Words.Free;

  EndTime:= DateTimeToTimeStamp (Time);

end;

function TJostarPageDispatcher.CreateNewInstance: TAbstractHandler;
begin
  Result:= TJostarPageDispatcher.Create;

end;

end.

