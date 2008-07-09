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
  UserName, Password: String;
  LoginNode: TXMLNode;
 
begin

  UserName:= CgiVars.CgiVarValueByName ['UserName'];
  Password:= CgiVars.CgiVarValueByName ['Password'];

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
    
end;

end.

