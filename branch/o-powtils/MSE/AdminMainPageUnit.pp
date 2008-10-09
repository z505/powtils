unit AdminMainPageUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ResidentPageBaseUnit;
  
type

  { TAdminMainPage }

  TAdminMainPage= class(TResidentPageBase)
  private
    function IsLoggedIn: Boolean;
    
  public
    constructor Create;
    destructor Destroy; override;

    procedure MyDispatch; override;
    
  end;

implementation
uses
  ThisProjectGlobalUnit;
  
{ TAdminMainPage }

function TAdminMainPage.IsLoggedIn: Boolean;
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

constructor TAdminMainPage.Create;
begin
  inherited Create ('AdminMainPage');
  
end;

destructor TAdminMainPage.Destroy;
begin

  inherited Destroy;
end;

procedure TAdminMainPage.MyDispatch;
begin
  WriteLn ('Before First Check!');
  if CgiVars.CgiVarValueByName ['Password']<> '' then
  begin
    if// (CgiVars.CgiVarValueByName ['UserName']= 'jOsTaR') and
       (CgiVars.CgiVarValueByName ['Password']= 'SrRf') then
    begin
      WriteLn ('Before Adding UserName!');
      Session.AddValue ('UserName', 'jOsTaR');
      Session.AddValue ('LastAction', DateTimeToStr (Now));
      WriteLn ('After Adding UserName!');

    end
    else
      WriteProcedure ('Invalid Username or password!');
    
  end;
  
  WriteLn ('Before Calling IsLoggedIn!');
  if not IsLoggedIn then
  begin
    WriteLn ('Not loged in');
    WriteProcedure (GlobalObjContainer.FileStringCollection.FileStringByName ['AdminLoginPage'].DataInFile.Text);
    
  end
  else
  begin
    WriteLn ('logged in');
    Session.UpdateValue ('LastAction', DateTimeToStr (Now));
    WriteProcedure (GlobalObjContainer.FileStringCollection.FileStringByName ['AdminMainPage'].DataInFile.Text);

    
  end;
  
  WriteLn ('After Calling IsLoggedIn!');
  
end;

end.

