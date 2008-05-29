unit MainPageUnit;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, {CollectionBaseUnit, }ResidentPageBaseUnit, WebUnit;

type

  { TMainPage }

  TMainPage= class (TResidentPageBase)
  public
    constructor Create;

    procedure MyDispatch;override;

  end;

implementation
uses
  ThisProjectGlobalUnit;

{ TMainPage }

constructor TMainPage.Create;
begin
  inherited Create ('MainPage', ctTextHTML, 'localhost', '/cgi-bin/WebCMD/MainPage');

end;

procedure TMainPage.MyDispatch;
  function LoginPassed (UserName, Password: String): Boolean;
  begin
    Result:= UserName= 'Amir';
    
  end;
  
var
 UserName, Password: String;
 
begin

  UserName:= CgiVars.CgiVarValueByName ['UserName'];
  Password:= CgiVars.CgiVarValueByName ['Password'];

  if LoginPassed (UserName, Password) then
    WriteProcedure (UserName+ ' : '+ Password)
  else
    Header.AddHeader (TWebHeader.Create ('LOCATION', 'LoginPage?Retry=1'));
    
end;

end.

