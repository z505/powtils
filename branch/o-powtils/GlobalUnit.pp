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
    
  public
    property FileStringCollection: TFileStrings read FFileStringCollection;
    property Configurations: TWebConfigurationCollection read FConfigurations;
    
    constructor Create;
    procedure Free;
    
  end;

implementation

{ TGlobalObjectContainer }

const
  WebConfigFile: String= 'PWU.conf';

constructor TGlobalObjectContainer.Create;
begin
  inherited;
  
  FConfigurations:= TWebConfigurationCollection.Create (WebConfigFile);
  FFileStringCollection:= TFileStrings.Create;

end;

procedure TGlobalObjectContainer.Free;
begin
  FFileStringCollection.Free;
  Configurations.Free;

  inherited;
  
end;

finalization

end.

