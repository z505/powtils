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
  Query, Engines: AnsiString;
  Words: TStringList;
  StartTime, EndTime: TTimeStamp;
  i: Integer;

begin
  StartTime:= DateTimeToTimeStamp (Time);

  Query:= Vars.CgiVarValueByName ['Q'];
  Engines:= Vars.CgiVarValueByName ['Engines'];

  if Query= '' then
    Exit;

  Words:= ParseQuery (Query);
  if Engines= '' then
    Engines:= 'GYM';

  for i:= 1 to Length (Engines) do



  Words.Free;

  EndTime:= DateTimeToTimeStamp (Time);

end;

function TJostarPageDispatcher.CreateNewInstance: TAbstractHandler;
begin
  Result:= TJostarPageDispatcher.Create;

end;

end.

