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
    procedure Free;

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
begin
  inherited;
  
  FConfigurations:= TWebConfigurationCollection.Create (WebConfigFile);
  FFileStringCollection:= TFileStrings.Create;
  FStartTime:= Now;
  FServedRequestCount:= 0;
  FRegisteredRequestCount:= 0;
  FQueueIsFullCount:= 0;

end;

procedure TGlobalObjectContainer.Free;
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

