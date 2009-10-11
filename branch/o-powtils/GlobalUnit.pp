unit GlobalUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileStringsUnit, WebConfigurationUnit;
  
type
  EConfigFileNotFound= class (Exception);

  { TGlobalObjectContainer }

  TGlobalObjectContainer= class (TObject)
  private
    FFileStringCollection: TFileStrings;
    FConfigurations: TWebConfigurationCollection;
    FQueueIsFullCount: Int64;
    FServedRequestCount: Int64;
    FRegisteredRequestCount: Int64;

    CSForNewRequestServed,
    CSForNewRequestRegistered,
    CSQueueIsFullOccured: TRTLCriticalSection;

    FStartTime: TDateTime;
    function GetCurrentTime: TDateTime;
    
  public
    property FileStringCollection: TFileStrings read FFileStringCollection;
    property Configurations: TWebConfigurationCollection read FConfigurations;
    property StartTime: TDateTime read FStartTime;
    property CurrentTime: TDateTime read GetCurrentTime;
    property ServedRequestCount: Int64 read FServedRequestCount;
    property RegisteredRequestCount: Int64 read FRegisteredRequestCount;
    property QueueIsFullCount: Int64 read FQueueIsFullCount;

    constructor Create;
    destructor Destroy; override;

    procedure NewRequestServed;
    procedure NewRequestRegistered;
    procedure QueueIsFullOccured;

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

{
  The configfilename must be PSP.conf and be in the same path as the application.
}

  procedure ReadConfigFileInfo;
  const
  {Default values for Webconfiguration:
  }
    DefaultConfigurationValues: array [1..7] of array [1..2] of String=
           (
            ('Charset', 'UTF-8'),//Default Charset
            ('RestartInterval', '-1'),//Default RestartInterval
            ('SessionIDLen', '20'),// Default SessionIDLen
            ('SessionVarName', 'PSPSESS'),// Default SessionVarName
            ('SessionUseCookie', 'TRUE'),//Dafault SessionUseCookie
            ('TemproraryPipesFilenameLength', '12'),//Dafault Temprorary pipes filename length
            ('AdminPage', 'AdminPage')//Dafault path (relative path and filename for AdminPage)
            );
  var
    ConfigFileHandle: TextFile;
    TempInt: Integer;
    TempString: String;
    i: Integer;

  begin
    if not FileExists ('PSP.conf') then
      EConfigFileNotFound.Create ('Config File not found!');

    AssignFile (ConfigFileHandle, 'PSP.conf');
    Reset (ConfigFileHandle);

    while not Eof (ConfigFileHandle) do
    begin
      ReadLn (ConfigFileHandle, TempString);
      TempString:= Trim (TempString);

      if (TempString<> '') and
        ((Length (TempString)= 0) or (TempString [1]<> '#')) then
      begin
        TempInt:= Pos (':', TempString);
        Configurations.AddNameValue (TWebConfiguration.Create (
          Copy (TempString, 1, TempInt- 1),
          Copy (TempString, TempInt+ 1, Length (TempString)- TempInt)));

      end;

    end;

    for i:= Low (DefaultConfigurationValues) to High (DefaultConfigurationValues) do
      if Configurations.ConfigurationValueByName [DefaultConfigurationValues [i][1]]= '' then
        Configurations.Add (TWebConfiguration.Create (DefaultConfigurationValues [i][1],
                             DefaultConfigurationValues [i][2]));

    CloseFile (ConfigFileHandle);
    for i:= 1 to ParamCount do
    begin
      TempString:= Trim (ParamStr (i));

      if (TempString<> '') and
        ((Length (TempString)= 0) or (TempString [1]<> '#')) then
      begin
        TempInt:= Pos (':', TempString);
        Configurations.AddNameValue (TWebConfiguration.Create (
          Copy (TempString, 1, TempInt- 1),
          Copy (TempString, TempInt+ 1, Length (TempString)- TempInt)));

      end;

    end;


  end;

begin
  inherited;
  
  FConfigurations:= TWebConfigurationCollection.Create (WebConfigFile);
  ReadConfigFileInfo;

  FFileStringCollection:= TFileStrings.Create;
  FStartTime:= Now;
  FServedRequestCount:= 0;
  FRegisteredRequestCount:= 0;
  FQueueIsFullCount:= 0;

end;

destructor TGlobalObjectContainer.Destroy;
begin
  FFileStringCollection.Free;
  Configurations.Free;

  inherited;
  
end;

procedure TGlobalObjectContainer.NewRequestServed;
begin
  EnterCriticalsection (CSForNewRequestServed);

  Inc (FServedRequestCount);

  LeaveCriticalsection (CSForNewRequestServed);

end;

procedure TGlobalObjectContainer.NewRequestRegistered;
begin
  EnterCriticalsection (CSForNewRequestRegistered);

  Inc (FRegisteredRequestCount);

  LeaveCriticalsection (CSForNewRequestRegistered);

end;

procedure TGlobalObjectContainer.QueueIsFullOccured;
begin
  EnterCriticalsection (CSQueueIsFullOccured);

  Inc (FQueueIsFullCount);

  LeaveCriticalsection (csQueueIsFullOccured);
end;

finalization

end.

