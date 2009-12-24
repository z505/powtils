unit QueryRequestUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ResultStorageUnit;

type

  { TQueryRequest }

  TQueryRequest= class (TObject)
  private
    Query: AnsiString;
    Requester: TThread;
    EngineIndex: Integer;
    ResultStorage: TResultStorage;

  public
    constructor Create (const Q: AnsiString; const Req: TThread;
        EngIndex: Integer; ResStorage: TResultStorage);

  end;

  { TQueryRequestQueue }

  TQueryRequestQueue= class (TObject)
  private
    Mutex: TRTLCriticalSection;
    Requests: TList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add (NewRequest: TQueryRequest);
    function Delete: TQueryRequest;

  end;


implementation

constructor TQueryRequestQueue.Create;
begin
  inherited Create;

  InitCriticalSection (Mutex);
  Requests:= TList.Create;

end;

destructor TQueryRequestQueue.Destroy;
begin
  Requests.Clear;
  Requests.Free;

  DoneCriticalsection (Mutex);

  inherited Destroy;
end;

procedure TQueryRequestQueue.Add (NewRequest: TQueryRequest);
begin
  EnterCriticalsection (Mutex);

  Requests.Add (NewRequest);

  LeaveCriticalsection (Mutex);

end;

function TQueryRequestQueue.Delete: TQueryRequest;
begin
  EnterCriticalsection (Mutex);

  Result:= TQueryRequest (Requests.Items [0]);
  Requests.Delete (0);

  LeaveCriticalsection (Mutex);

end;

{ TQueryRequest }

constructor TQueryRequest.Create(const Q: AnsiString; const Req: TThread;
  EngIndex: Integer; ResStorage: TResultStorage);
begin
  inherited Create;

  Query:= Q;
  Requester:= Req;
  EngineIndex:= EngIndex;
  ResultStorage:= ResStorage;

end;

end.

