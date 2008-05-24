unit LoginPageUnit;

{$mode objfpc}{$H+}

interface
uses
  SysUtils, {CollectionBaseUnit, }ResidentPageBaseUnit, WebUnit;
  
type

  { TLoginPage }

  TLoginPage= class (TResidentPageBase)
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
  inherited Create ('LoginPage', ctTextHTML, 'localhost', '/cgi-bin/WebCMD/LoginPage');

end;

procedure TLoginPage.MyDispatch;
begin
  WriteProcedure (GlobalObjContainer.FileStringCollection.FileString [0].DataInFile.Text);

end;

end.

