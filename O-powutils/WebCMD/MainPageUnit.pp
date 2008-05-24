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
var
 UserName, Password: String;
 
begin
  UserName:= CgiVars.CgiVarByName ['UserName'].ToString;
  WriteLn (UserName);
  Password:= CgiVars.CgiVarByName ['Password'].ToString;
  WriteLn (Password);

  WriteProcedure (UserName+ ' : '+ Password);
  
end;

end.

