unit GlobalUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileStringsUnit, WebUnit, WebConfigurationUnit;
  
type
  { TGlobalObjectContainer }

  TGlobalObjectContainer= class (TObject)
  private
    FFileStringCollection: TFileStrings;
    FConfigurations: TWebConfigurationCollection;
    FServedRequestCount: Int64;
    FStartTime: TDateTime;
    function GetCurrentTime: TDateTime;
    
  public
    property FileStringCollection: TFileStrings read FFileStringCollection;
    property Configurations: TWebConfigurationCollection read FConfigurations;
    property StartTime: TDateTime read FStartTime;
    property CurrentTime: TDateTime read GetCurrentTime;
    property ServedRequestCount: Int64 read FServedRequestCount;
    
    constructor Create;
    procedure Free;

    procedure NewRequestServed;

  end;

implementation

{ TGlobalObjectContainer }

const
  WebConfigFile: String= 'PWU.conf';

function TGlobalObjectContainer.GetCurrentTime: TDateTime;
begin
  Result:= Now;

end;

constructor TGlobalObjectContainer.Create;
begin
  inherited;
  
  FConfigurations:= TWebConfigurationCollection.Create (WebConfigFile);
  FFileStringCollection:= TFileStrings.Create;
  FStartTime:= Now;
  FServedRequestCount:= 0;

end;

procedure TGlobalObjectContainer.Free;
begin
  FFileStringCollection.Free;
  Configurations.Free;

  inherited;
  
end;

procedure TGlobalObjectContainer.NewRequestServed;
begin
  Inc (FServedRequestCount);

end;

finalization

end.

