unit WebConfigurationUnit;

{$mode objfpc}{$H+}

interface
uses
  CollectionUnit;

type
  { TWebConfiguration }

  TWebConfiguration= class (TNameStrValue)
  private
  public

  end;

  { TWebConfigurationCollection }
  {
    This class loads the web configuration data from PWU_CONFIG_PATH file.
  }
  TWebConfigurationCollection= class (TNameValueCollection)
  private
    function GetConfigurationByIndex (Index: Integer): TWebConfiguration;
    function GetConfigurationByName (ValueName: String): TWebConfiguration;
    function GetConfigurationValueByName (Name: String): String;

    function ParseWebConfig (const ConfigFilename: String): Boolean;
  public
    property ConfigurationValueByName [Name: String]: String read GetConfigurationValueByName;
    property ConfigurationByName [Name: String]: TWebConfiguration read GetConfigurationByName;
    property Configuration [Index: Integer]: TWebConfiguration read GetConfigurationByIndex;

    constructor Create (const CofigFilename: String);

  end;

implementation
uses
  ExceptionUnit;

{ TWebConfigurationCollection }

function TWebConfigurationCollection.GetConfigurationByIndex (Index: Integer): TWebConfiguration;
begin
  Result:= NameValue [Index] as TWebConfiguration;

end;

function TWebConfigurationCollection.GetConfigurationByName (ValueName: String): TWebConfiguration;
begin
  Result:= NameValueByName [ValueName] as TWebConfiguration;

end;

function TWebConfigurationCollection.GetConfigurationValueByName (Name: String): String;
begin
  Result:= ConfigurationByName [Name].Value;

end;

function TWebConfigurationCollection.ParseWebConfig (const ConfigFilename: String): Boolean;
begin
  raise ENotImplementedYet.Create ('TWebConfigurationCollection', 'ParseWebConfig');

end;

constructor TWebConfigurationCollection.Create (const CofigFilename: String);
begin
  inherited Create;

  ParseWebConfig (CofigFilename)

end;

end.

