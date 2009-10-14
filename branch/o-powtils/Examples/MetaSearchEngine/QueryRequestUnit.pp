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
//    Mutex:
    Requests: TList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add (NewRequest: TQueryRequest);

  end;


implementation

constructor TQueryRequestQueue.Create;
begin
  inherited Create;

  Requests:= TList.Create;

end;

destructor TQueryRequestQueue.Destroy;
begin
  Requests.Clear;
  Requests.Free;

  inherited Destroy;
end;

procedure TQueryRequestQueue.Add (NewRequest: TQueryRequest);
begin
  Requests.Add (NewRequest);

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

