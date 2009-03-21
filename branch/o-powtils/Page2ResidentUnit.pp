unit Page2ResidentUnit;

{$mode objfpc}{$H+}

interface
uses
  SysUtils, ResidentPageBaseUnit, WebUnit;

type

  { TMyWebPage2 }

  TMyWebPage2= class (TResidentPageBase)
  private

  published
    property PageName: String read FPageName write FPageName;
    
  public
    destructor Destroy; override;
    procedure Clear;

    procedure MyDispatch; override;

  end;
  
implementation
uses
  CookieUnit;

{ TMyWebPage2 }

destructor TMyWebPage2.Destroy;
begin
  inherited;
  
end;

procedure TMyWebPage2.Clear;
begin

  inherited;
end;

procedure TMyWebPage2.MyDispatch;
var
  i: Integer;

begin
//  Cookies.Add (TCookie.Create ('Test', 'TestValue', '05/29/2007', 'Temp', ''));
  Cookies.AddNameValue (TCookie.Create ('Test2', 'TestValue2'));

  for i:= 0 to CgiVars.size- 1 do
    Self.Write ('*'+ CgiVars.CgiVar [i].ToString);

end;

end.

