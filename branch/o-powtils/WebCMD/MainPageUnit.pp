unit MainPageUnit;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, {CollectionBaseUnit, }ResidentPageBaseUnit,
    WebUnit, XMLNode;

type

  { TMainPage }

  TMainPage= class (TXMLResidentPageBase)
  private
    function SessionIsValid: Boolean;
    function GetInfo: TXMLNode;
  
  public
    constructor Create;

    procedure MyDispatch;override;

  end;

implementation
uses
  ThisProjectGlobalUnit, Unix, DateUtils;

{ TMainPage }

function TMainPage.SessionIsValid: Boolean;
var
  LastActionTime: TDateTime;

begin
  Result:= True;
  
  if Session.VariableExists ('LastActionTime') then
  begin
    LastActionTime:= StrToDateTime (Session.ValueByName ['LastActionTime']);
    if IncHour (LastActionTime, 1)< Now then
      Result:= False;

  end;
  
end;

function TMainPage.GetInfo: TXMLNode;
begin
  Result:= TXMLNode.Create ('ServerInfo');
  Result.AddAttribute ('ServerTime', TimeToStr (Now));
  Result.AddAttribute ('ServerDate', DateToStr (Now));
  
end;

constructor TMainPage.Create;
begin
  inherited Create ('../../WebCMD/MainPage.xsl', 'MainPage');

end;

procedure TMainPage.MyDispatch;

  function LoginPassed (UserName, Password: String): Boolean;
  begin
    Result:= (UserName= Password) and (UserName<> '');
    
  end;
  
var
  Action, UserName, Password: String;
  LoginNode: TXMLNode;
 
begin

  Action:= CgiVars.CgiVarValueByName ['Action'];
  if Action= '' then
    Header.AddHeader (TWebHeader.Create ('LOCATION', 'LoginPage?Retry=1'))
  else
  begin

    UserName:= CgiVars.CgiVarValueByName ['UserName'];
    Password:= CgiVars.CgiVarValueByName ['Password'];

    case Action [1] of
      'L':
        if LoginPassed (UserName, Password) then
        begin
          FXMLRoot.AddAttribute ('HostName', GetHostName);
          LoginNode:= TXMLNode.Create ('LoginInfo');
          LoginNode.AddAttribute ('UserName', UserName);
          
          Session.AddValue ('UserName', UserName);
          Session.AddValue ('LastActionTime', DateTimeToStr (Now));

          FXMLRoot.AddChild (LoginNode);

        end
        else
          Header.AddHeader (TWebHeader.Create ('LOCATION', 'LoginPage?Retry=1'));

      'R':
      ;
    end;
    
    if SessionIsValid then
      FXMLRoot.AddChild (GetInfo)
    else
      Header.AddHeader (TWebHeader.Create ('LOCATION', 'LoginPage?Retry=2'));

  end;
  
end;

end.

