unit MyWebUnit;

{$mode objfpc}{$H+}

interface
uses
  SysUtils, WebClassUnit;
  
type

  { TMyWeb }

  TMyWeb= class (TWeb)
  private
  public
    constructor Create(ContType: TContentType;
                   ForceToSendHeader: Boolean= False);
    procedure Free;
    procedure Clear;
    
    procedure Dispach ;
  end;

implementation

{ TMyWeb }

constructor TMyWeb.Create (ContType: TContentType; ForceToSendHeader: Boolean);
begin
  inherited;
end;

procedure TMyWeb.Free;
begin
  inherited;
end;

procedure TMyWeb.Clear;
begin
  inherited;
end;

procedure TMyWeb.Dispach;
begin
  Self.webWrite ('Hello Word!');
end;

end.

