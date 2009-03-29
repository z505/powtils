unit Page1ResidentUnit;

{$mode objfpc}{$H+}

interface
uses
  SysUtils, {CollectionBaseUnit, }ResidentPageBaseUnit, WebUnit;
  
type

  { TMyWebPage1 }

  TMyWebPage1= class (TResidentPageBase)
  published
  public
    constructor Create;
    
    destructor Destroy; override;
    procedure Clear;
    
    procedure MyDispatch;override;

  end;

implementation
uses
  WebHeaderUnit;

{ TMyWebPage1 }

constructor TMyWebPage1.Create ;
begin
  inherited Create ('Page1', ctTextHTML, 'localhost', '/cgi-bin/Test/Page1');

end;

destructor TMyWebPage1.Destroy;
begin
  inherited Destroy;
  
end;

procedure TMyWebPage1.Clear;
begin

end;

procedure TMyWebPage1.MyDispatch ;
var
  i: Integer;

begin

  for i:= 0 to 10 do
    Write (IntToStr (i));

  for i:= 0 to Vars.size- 1 do
    Write (Vars.CgiVar [i].ToString);

end;

end.

