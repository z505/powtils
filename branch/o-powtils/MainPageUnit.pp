unit MainPageUnit;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, {CollectionBaseUnit, }ResidentPageBaseUnit, WebUnit;

type

  { TMainPage }

  TMainPage= class (TXMLResidentPageBase)
  public
    constructor Create;

    procedure MyDispatch;override;

  end;

implementation
uses
  ThisProjectGlobalUnit, XMLNode, Unix;

{ TMainPage }

constructor TMainPage.Create;
begin
  inherited Create ('../../WebCMD/MainPage.xsl', 'MainPage');

end;

procedure TMainPage.MyDispatch;

  function LoginPassed (UserName, Password: String): Boolean;
  begin
    Result:= UserName= Password;
    
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
          
          Session.AddVariable ('UserName', UserName);
          Session.AddVariable ('LastAction', DateTimeToStr (Now));

          FXMLRoot.AddChild (LoginNode);

        end
        else
          Header.AddHeader (TWebHeader.Create ('LOCATION', 'LoginPage?Retry=1'));

      'R':
      ;
    end;
    
  end;
  
end;

end.

