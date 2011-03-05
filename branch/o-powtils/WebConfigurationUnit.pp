unit WebConfigurationUnit;

{$mode objfpc}{$H+}

interface
uses
  GenericCollectionUnit, MyTypes;

type
  TNameStrValue= specialize TPairForBuiltInData <String, TObject>;

  TNameValueCollection= specialize TGenericCollection<TNameStrValue>;

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

  protected
    property ConfigurationByName [Name: String]: TWebConfiguration read GetConfigurationByName;
    property Configuration [Index: Integer]: TWebConfiguration read GetConfigurationByIndex;

  public
    property ConfigurationValueByName [Name: String]: String read GetConfigurationValueByName;

    constructor Create (const CofigFilename: String);

  end;

implementation
uses
  SysUtils;

{ TWebConfigurationCollection }

function TWebConfigurationCollection.GetConfigurationByIndex (Index: Integer): TWebConfiguration;
begin
  Result:= Item [Index] as TWebConfiguration;

end;

function TWebConfigurationCollection.GetConfigurationByName (ValueName: String): TWebConfiguration;
begin
//  Result:= NameValueByName [ValueName] as TWebConfiguration;

end;

function TWebConfigurationCollection.GetConfigurationValueByName (Name: String): String;
begin
  try
//    Result:= ConfigurationByName [Name].Value;

  except
{    on e: ENameNotFound do
      Result:= '';
}
  end;

end;

function TWebConfigurationCollection.ParseWebConfig (const ConfigFilename: String): Boolean;
var
  InputFile: TextFile;
  S: String;

begin
  Result:= True;
  AssignFile (InputFile, ConfigFilename);
  Reset (InputFile);

  while not EoF (InputFile) do
  begin
    ReadLn (InputFile, S);
    S:= Trim (S);
    if S<> '' then
//      Self.Add (TWebConfiguration.Create (S));

  end;

  CloseFile (InputFile);

end;

constructor TWebConfigurationCollection.Create (const CofigFilename: String);
begin
  inherited Create;

  ParseWebConfig (CofigFilename)

end;

end.

