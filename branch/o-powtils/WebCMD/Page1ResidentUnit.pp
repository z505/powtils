unit Page1ResidentUnit;

{$mode objfpc}{$H+}

interface
uses
  SysUtils, {CollectionBaseUnit, }ResidentPageBaseUnit, WebUnit;
  
type

  { TMyWebPage1 }

  TMyWebPage1= class (TResidentPageBase)
  private
    FContentType: TContentType;

  published
    property ContentType: TContentType read FContentType write FContentType;
    property ForceToSendHeader: Boolean read FForceToSendHeader write FForceToSendHeader;
    
  public
    constructor Create;
    
    destructor Destroy; override;
    procedure Clear;
    
    procedure MyDispatch;override;

  end;

implementation

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
    WriteProcedure (IntToStr (i));

  for i:= 0 to CgiVars.size- 1 do
    WriteProcedure (CgiVars.CgiVar [i].ToString);

end;

end.

