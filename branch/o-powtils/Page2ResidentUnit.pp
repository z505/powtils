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
    property ContentType: TContentType read FContentType write FContentType;
    property ForceToSendHeader: Boolean read FForceToSendHeader write FForceToSendHeader;
    
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure MyDispatch; override;

  end;
  
implementation

{ TMyWebPage2 }

constructor TMyWebPage2.Create;
begin
  inherited Create ('Page2', ctTextHTML, 'localhost', '/cgi-bin/Test/Page2');

end;

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
  Cookies.Add (TCookie.Create ('Test', 'TestValue', '05/29/2007', 'Temp', ''));
  Cookies.Add (TCookie.Create ('Test2', 'TestValue2'));

  for i:= 0 to CgiVars.size- 1 do
    Self.WriteProcedure ('*'+ CgiVars.CgiVar [i].ToString);

end;

end.

