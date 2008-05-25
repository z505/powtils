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
  ThisProjectGlobalUnit;
  
  { TLoginPage }

constructor TLoginPage.Create;
begin
  inherited Create ('http://127.0.0.1/WebCMD/LoginPage.xsl', 'LoginPage');

end;

procedure TLoginPage.MyDispatch;
begin

end;

end.

