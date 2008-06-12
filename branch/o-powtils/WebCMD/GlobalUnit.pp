unit GlobalUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileStringsUnit, WebUnit;
  
type
  { TGlobalObjectContainer }

  TGlobalObjectContainer= class (TObject)
  private
    FFileStringCollection: TFileStrings;
    FWebConfiguration: TWebConfigurationCollection;
    
  public
    property FileStringCollection: TFileStrings read FFileStringCollection;
    property WebConfiguration: TWebConfigurationCollection read FWebConfiguration;
    
    constructor Create;
    procedure Free;
    
  end;

const
  WebConfigFile: String= 'PWU.conf';

implementation

{ TGlobalObjectContainer }

constructor TGlobalObjectContainer.Create;
begin
  inherited;
  
  FFileStringCollection:= TFileStrings.Create;
  FWebConfiguration:= TWebConfigurationCollection.Create;
  FWebConfiguration.ParseWebConfig (WebConfigFile);

end;

procedure TGlobalObjectContainer.Free;
begin
  
  FFileStringCollection.Free;
  
  inherited;
  
end;

initialization
  
finalization
  
end.

