unit SearchEnginesQueryManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit, StreamUnit,
  QueryRequestUnit, ResultStorageUnit;

type
  { TSearchEnginesRequst }

  TSearchEnginesRequst= class (TObject)
  private
    FWords: TStringList;
    FThread: TThread;

  public
    constructor Create (Words: TStringList; Theard: TThread);

  end;

  { TSearchEngineQueryThead }

  TSearchEngineQueryThead= class (TThread)
  private
    FBaseURL: AnsiString;
    FQueryQueue: TQueryRequestQueue;

  protected
    property BaseURL: AnsiString read FBaseURL;

  public
    constructor Create (Configuration: TStringList; RequestQueue: TQueryRequestQueue);

  end;
  { TSearchEngineQueryManager }

  TSearchEngineQueryManager= class (TBaseCollection)
  private
    function GetThread (Index: Integer): TThread;
    FRequestQueue: TQueryRequestQueue;

  protected
    property Thread [Index: Integer]: TThread read GetThread;
    property RequestQueue: TQueryRequestQueue read FRequestQueue;

  public
    constructor Create (NoOfThreads: Integer; AStream: TStream);
    destructor Destroy; override;

    procedure RegisterQuery (const Query: AnsiString; EngineIndex: Integer;
                 Requestor: TThread; ResultStorage: TResultStorage);

  end;

  { TSearchEnginesQueryManager }

  TSearchEnginesQueryManager= class (TObject)
  private
    Queue: TList;

  public
    constructor Create (AStream: TStream);

  end;

implementation

{ TSearchEnginesQueryManager }

constructor TSearchEnginesQueryManager.Create (AStream: TStream);
var
  TextStream: TMyTextStream;
  n: Integer;

begin
  inherited Create;

  TextStream:= TMyTextStream (AStream);
  n:= StrToInt (TextStream.ReadLine);



  TextStream.Free;

end;

{ TSearchEnginesRequst }

constructor TSearchEnginesRequst.Create(Words: TStringList; Theard: TThread);
begin
  inherited Create;

  FWords:= Words;
  FThread:= Theard;

end;

{ TSearchEngineQueryManager }

function TSearchEngineQueryManager.GetThread (Index: Integer): TThread;
begin
  Result:= Member [Index] as TThread;

end;

constructor TSearchEngineQueryManager.Create (NoOfThreads: Integer;
  AStream: TStream);
var
  i: Integer;
  TextStream: TMyTextStream;
  ThreadConfig: TStringList;

begin
  inherited Create;


  FRequestQueue:= TQueryRequestQueue.Create;
  Allocate (NoOfThreads);

  TextStream:= TMyTextStream.Create (AStream);

  ThreadConfig:= TStringList.Create;
  while TextStream.Position< TextStream.Size do
    ThreadConfig.Add (Trim (TextStream.ReadLine));
  TextStream.Free;

  for i:= 1 to NoOfThreads do
    FMembers [i- 1]:= TSearchEngineQueryThead.Create (ThreadConfig, FRequestQueue);

end;

destructor TSearchEngineQueryManager.Destroy;
begin

  inherited Destroy;
end;

procedure TSearchEngineQueryManager.RegisterQuery (const Query: AnsiString;
  EngineIndex: Integer; Requestor: TThread; ResultStorage: TResultStorage);
var
  NewRequest: TQueryRequest;

begin
  NewRequest:= TQueryRequest.Create (Query, Requestor, EngineIndex,
                 ResultStorage);

  RequestQueue.Add (NewRequest);
  Requestor.Suspend;

  NewRequest.Free;

end;

{ TSearchEngineQueryThead }

constructor TSearchEngineQueryThead.Create(Configuration: TStringList;
  RequestQueue: TQueryRequestQueue);
begin
  inherited Create (True);

  if Configuration [0]= 'URL' then
    FBaseURL:= Configuration [1];
  FQueryQueue:= RequestQueue;

end;

end.

