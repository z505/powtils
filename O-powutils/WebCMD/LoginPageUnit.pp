unit LoginPageUnit;

{$mode objfpc}{$H+}

interface
uses
  SysUtils, {CollectionBaseUnit, }ResidentPageBaseUnit, WebUnit;
  
type

  { TLoginPage }

  TLoginPage= class (TXMLResidentPageBase)
  public
    constructor Create;

    procedure MyDispatch;override;

  end;

implementation
uses
  ThisProjectGlobalUnit, XMLNode, AttributeUnit;
  
  { TLoginPage }

constructor TLoginPage.Create;
begin
  inherited Create ('../../WebCMD/LoginPage.xsl', 'LoginPage');

end;

procedure TLoginPage.MyDispatch;
begin
  if CgiVars.CgiVarValueByName ['Retry']<> '' then
    FXMLRoot.AddAttribute ('RetryMode', 'True');
    
end;

end.

