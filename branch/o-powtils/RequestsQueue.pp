unit RequestsQueue;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BlockingQueueUnit;
  
type
{
  This class is going to save a request parameters.
  It read the parameters from the Mainpipehandle which is passed to it.
}
  { TRequest }

  TRequest= class (TObject)
  private
    FOutputPipe: String;
    FPageName: String;
    FCookieStr: String;
    FVariables: String;
    
  public
    {Variables is a string which contains all the GET/POST variables
      and maybe some of environment variables which are seprated by & from each other.
    }
    property Variables: String read FVariables;
    {CookiStr is a string which contains all Cookie values
      which are seprated by & from each other.
    }
    property CookieStr: String read FCookieStr;

    {PageName is the name of the requested page.
    }
    property PageName: String read FPageName;
    {PageName is the name of the pipe in which the output should be written to.
    }

    property OutputPipe: String read FOutputPipe;
    
    constructor Create (const ParamsAndCookies: String);

    function ToString: String;
    
  end;

  TCircularRequestsQueue= specialize TBlockingCircularQueue<TRequest>;

implementation

{ TRequest }
{
  Loads the Parameters and Cookies from an string (ParamsAndCookies).
  A #255 used as disjointer between parameters and cookies which are
  written in a line by RequestCollector.
}
constructor TRequest.Create (const ParamsAndCookies: String);

  function CharPos (StartIndx: Integer; Ch: Char; const Source: String): Integer;
  var
    CharPtr: PChar;
    i: Integer;

  begin
    CharPtr:= @(Source [StartIndx]);

    for i:= StartIndx to Length (Source) do
    begin
      if CharPtr^= Ch then
        Exit (i);

      Inc (CharPtr);
    end;
    Result:= -1;

  end;

const
  Seprator: char= #$FF;
  
var
  Position1, Position2: Integer;

begin
  inherited Create;

  Position2:= 0;
  Position1:= CharPos (Position2+ 1, Seprator, ParamsAndCookies);
  FPageName:= Copy (ParamsAndCookies, Position2+ 1, Position1- 1);

  Position2:= CharPos (Position1+ 1, Seprator, ParamsAndCookies);
  FOutputPipe:= Copy (ParamsAndCookies, Position1+ 1, Position2- Position1- 1);

  Position1:= CharPos (Position2+ 1, Seprator, ParamsAndCookies);
  FVariables:= Copy (ParamsAndCookies, Position2+ 1, Position1- Position2- 1);

  Position2:= CharPos (Position1+ 1, Seprator, ParamsAndCookies);
  FCookieStr:= Copy (ParamsAndCookies, Position1+ 1, Position2- Position1- 1);

end;

function TRequest.ToString: String;
begin
  Result:= 'FPageName='+ FPageName+ ' OutputPipe= '+ FOutputPipe+
       'Variables= '+ FVariables+ ' CookieStr= '+ FCookieStr;


end;

end.

