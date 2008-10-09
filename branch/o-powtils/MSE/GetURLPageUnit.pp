unit GetURLPageUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ResidentPageBaseUnit;

type

  { TGetURLPage }

  TGetURLPage= class(TResidentPageBase)
  private
//    HTML
    function IsLoggedIn: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure MyDispatch; override;

  end;

implementation
uses
  ThisProjectGlobalUnit, WebUnit, SEThreadingUnit, SEHTTPSenderUnit;

{ TGetURLPage }

function TGetURLPage.IsLoggedIn: Boolean;
var
  LastAction: TDateTime;
  LastActionStr: String;
  Delta: TDateTime;

begin
  Result:= False;

  if Session.ValueByName ['UserName']<> '' then
  begin
    LastActionStr:= Session.ValueByName ['LastAction'];
    if LastActionStr<> '' then
    begin
      LastAction:= StrToDateTime (LastActionStr);
      Delta:= Now- LastAction;;
      Result:= Now- LastAction< 2;

    end;

  end;

end;

constructor TGetURLPage.Create;
begin
  inherited Create ('AdminMainPage');

end;

destructor TGetURLPage.Destroy;
begin

  inherited Destroy;
end;

procedure TGetURLPage.MyDispatch;
var
  ResultPage: TStringList;
  HTTPSender: THTTPSender;
  URL: String;
  UserAgentString: String;
    
begin
  if IsLoggedIn then
  begin
    URL:= CgiVars.CgiVarValueByName ['URL'];
    UserAgentString:= CgiVars.CgiVarValueByName ['UserAgnet'];
    ResultPage:= TStringList.Create;
    HTTPSender:= THTTPSender.Create (UserAgentString);
    WriteLn ('Before Get!');
    ResultPage.Text:= HTTPSender.Get (URL);
    WriteLn ('After Get!');

    WriteProcedure (ResultPage.Text);

    WriteProcedure (
      GlobalObjContainer.FileStringCollection.FileStringByName ['GetURLResultPage-1'].DataInFile.Text
      );
    WriteProcedure (
      GlobalObjContainer.FileStringCollection.FileStringByName ['GetURLResultPage-2'].DataInFile.Text
      );
      
    ResultPage.Free;
    HTTPSender.Free;

  end
  else
    Header.AddHeader (TWebHeader.Create ('Location', 'AdminLogin.psp'));

end;

end.

