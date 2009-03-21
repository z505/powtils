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
    
  public
    property FileStringCollection: TFileStrings read FFileStringCollection;
    
    constructor Create;
    procedure Free;
    
  end;

var
  WebConfiguration: TWebConfigurationCollection;

implementation

{ TGlobalObjectContainer }

constructor TGlobalObjectContainer.Create;
begin
  inherited;
  
  FFileStringCollection:= TFileStrings.Create;

end;

procedure TGlobalObjectContainer.Free;
begin
  
  FFileStringCollection.Free;
  
  inherited;
  
end;

const
  WebConfigFile: String= 'PWU.conf';

initialization
  WebConfiguration:= TWebConfigurationCollection.Create (WebConfigFile);

finalization
  WebConfiguration.Free;

end.

