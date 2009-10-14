unit SearchResultUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit;

type
  TSearchEngineID= Integer;

  TSearchEngineInfo= record
    SearchEnginesID: TSearchEngineID;
    RankInSearchEngines: Double;

  end;

  { TSearchResult }

  TSearchResult= class (TObject)
  private
    FDescription: String;
    FLink: String;
    FTitle: String;
    FSearchEngineInfo: TSearchEngineInfo;

  public
    property Title: String read FTitle;
    property Link: String read FLink;
    property Description: String read FDescription;
    property SearchEngineInfo: TSearchEngineInfo read FSearchEngineInfo;

    constructor Create (var _Title, _Link, _Description: String);
    destructor Destroy; override;

    function ToString: String;

    procedure AddNewSearchEngineInfo (var _SearchEngineInfo: TSearchEngineInfo);

  end;

  { TSearchResultCollection }

  TSearchResultCollection= class (TBaseCollection)
  private
    function GetResult (Index: Integer): TSearchResult;
    procedure SetCount(const AValue: Int64);

  protected
    FCount: Int64;

  public
    property SearchResult [Index: Integer]: TSearchResult read GetResult;
    property Count: Int64 read FCount write SetCount;

    procedure AddResult (ASeachResult: TSearchResult);
    constructor Create (NoOfCount: Int64);

    function ToString: String;

  end;

  { TSearchResultPool }

  TSearchResultPool= class (TSearchResultCollection)
  private
    FTotalCount: Integer;
    Mutex: TRTLCriticalSection;

  public
    property TotalCount: Integer read FTotalCount write FTotalCount;

    constructor Create;
    destructor Destroy; override;

    procedure Merge (ASearchCollection: TSearchResultCollection;
            ThisSearchTotalCount: Int64);

  end;

implementation
uses
  MyTypes;

{ TResult }

constructor TSearchResult.Create (var _Title, _Link, _Description: String);
begin
  inherited Create;

  FTitle:= _Title;
  FLink:= _Link;
  FDescription:= _Description;
  FSearchEngineInfo.SearchEnginesID:= 0;
  FSearchEngineInfo.RankInSearchEngines:= 0;

end;

destructor TSearchResult.Destroy;
begin
  inherited Destroy;

end;

function TSearchResult.ToString: String;
begin
  Result:= 'Title= '+ FTitle+ #10+ '<BR/>'+
           'Link= '+ FLink+ #10+ '<BR/>'+
           'Summary= '+ FDescription+ #10+ '<BR/>';

end;

procedure TSearchResult.AddNewSearchEngineInfo (var _SearchEngineInfo: TSearchEngineInfo);
begin

end;

{ TResultCollection }

function TSearchResultCollection.GetResult (Index: Integer): TSearchResult;
begin
  Result:= FMembers [Index] as TSearchResult;

end;

procedure TSearchResultCollection.SetCount (const AValue: Int64);
begin
  if FCount< AValue then
    FCount:= AValue;

end;

procedure TSearchResultCollection.AddResult (ASeachResult: TSearchResult);
begin
  Add (ASeachResult);

end;

constructor TSearchResultCollection.Create (NoOfCount: Int64);
begin
  inherited Create;

  FCount:= NoOfCount;

end;

function TSearchResultCollection.ToString: String;
type
  PSearchResult= ^TSearchResult;

var
  i: Integer;
  Ptr: PObject;

begin
  Result:= '';

  Ptr:= GetPointerToFirst;
  for i:= 1 to Size do
  begin
    Result+= '<SPAN>'+ (Ptr^ as TSearchResult).ToString+ '</SPAN><P/>'#10;
    Inc (Ptr);

  end;

end;

{ TSearchResultPool }

constructor TSearchResultPool.Create;
begin
  inherited Create (0);

  InitCriticalSection (Mutex);

end;

destructor TSearchResultPool.Destroy;
begin
  DoneCriticalsection (Mutex);

  inherited;
end;

procedure TSearchResultPool.Merge (ASearchCollection: TSearchResultCollection;
  ThisSearchTotalCount: Int64);
begin

end;

end.

