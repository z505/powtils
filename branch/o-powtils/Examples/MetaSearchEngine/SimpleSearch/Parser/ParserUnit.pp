unit ParserUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SearchAutomataUnit, CollectionUnit, SearchResultUnit;

type
  { TEntryConfig }

  TEntryConfig= class (TObject)
  private
    NeedEncoding: Boolean;
    Start: TStringList;
    StartAutomatas: TBaseCollection;
    Fin: TStringList;
    FinAutomatas: TBaseCollection;

  public
    constructor Create (AnStream: TStream);
    destructor Destroy; override;

  end;

  TParserResult= record
    ResultCountInThisSearch: Integer;
    ResultAnnouncedInThisSearch: Integer;
  end;

  { TParser }

  TParser= class (TObject)
  private
    SearchEngineName: String;
    ResultCountConfig: TEntryConfig;
    StartingResult: TEntryConfig;
    LinkConfig: TEntryConfig;
    TitleConfig: TEntryConfig;
    SummaryConfig: TEntryConfig;

    AmpAutomata: TAutomata;

  public
    constructor Create (AnStream: TStream);
    destructor Destroy; override;

    procedure Parse (CharPtr: PChar; Len: Integer;
             out ResultPool: TSearchResultPool);

  end;

implementation
uses
  StreamUnit, MyTypes;

{ TEntryConfig }

constructor TEntryConfig.Create (AnStream: TStream);
const
  ToBeIgnoredCharSet: TCharSet= [' ', #9, #10, #13, '"', ''''];


var
  Str: String;
  i, n: Integer;
  MyStream: TMyTextStream;

begin
  inherited Create;

  MyStream:= TMyTextStream.Create (AnStream);

  Str:= MyStream.ReadLine;
  NeedEncoding:= Str= 'TRUE';

  n:= StrToInt (MyStream.ReadLine);
  Start:= TStringList.Create;

  while 0< n do
  begin
    Str:= MyStream.ReadLine;
    Start.Add (Str);
    Dec (n);

  end;

  n:= StrToInt (MyStream.ReadLine);
  Fin:= TStringList.Create;

  while 0< n do
  begin
    Str:= MyStream.ReadLine;
    Fin.Add (Str);
    Dec (n);

  end;

  StartAutomatas:= TBaseCollection.Create;
  for i:= 0 to Start.Count- 1 do
    StartAutomatas.Add (TAutomata.Create (Start.Strings [i], ToBeIgnoredCharSet));

  FinAutomatas:= TBaseCollection.Create;
  for i:= 0 to Fin.Count- 1 do
    FinAutomatas.Add (TAutomata.Create (Fin.Strings [i], ToBeIgnoredCharSet));

  MyStream.Free;

end;

destructor TEntryConfig.Destroy;
begin
  Start.Free;
  Fin.Free;
  StartAutomatas.Free;
  FinAutomatas.Free;

  inherited Destroy;

end;

{ TParser }

constructor TParser.Create (AnStream: TStream);
var
  MyStream: TMyTextStream;

begin
  inherited Create;

  MyStream:= TMyTextStream.Create (AnStream);

  SearchEngineName:= MyStream.ReadLine;
  ResultCountConfig:= TEntryConfig.Create (AnStream);
//  StartingResult:= TEntryConfig.Create (AnStream);
  LinkConfig:= TEntryConfig.Create (AnStream);
  TitleConfig:= TEntryConfig.Create (AnStream);
  SummaryConfig:= TEntryConfig.Create (AnStream);

  AmpAutomata:= TAutomata.Create ('&amp;', []);

  MyStream.Free;

end;

destructor TParser.Destroy;
begin
  ResultCountConfig.Free;
  LinkConfig.Free;
  TitleConfig.Free;
  SummaryConfig.Free;
  AmpAutomata.Free;

  inherited Destroy;
end;

procedure TParser.Parse (CharPtr: PChar; Len: Integer;
             out ResultPool: TSearchResultPool);
type
  PAutomata= ^TAutomata;

  function ParseEntry (AConfig: TEntryConfig; var StartCharPtr, EndCharPtr: PChar): Boolean;
  var
    i: Integer;
    Ptr: PObject;
    StartFindPositionInfo,
    FinFindPositionInfo: TFindResultInfo;
    Min, TempCharPtr: PChar;

  begin
    Result:= False;

    Ptr:= AConfig.StartAutomatas.GetPointerToFirst;
    StartFindPositionInfo.FinPtr:= CharPtr- 1;
    for i:= 1 to AConfig.StartAutomatas.Size do
    begin
      if not (Ptr^ as TAutomata).Find (CharPtr, Len, StartFindPositionInfo) then
        Exit (False);

      Dec (Len, StartFindPositionInfo.FinPtr- CharPtr+ 1);
      CharPtr:= StartFindPositionInfo.FinPtr+ 1;

      Inc (Ptr);

    end;
    StartCharPtr:= StartFindPositionInfo.FinPtr+ 1;

    Ptr:= AConfig.FinAutomatas.GetPointerToFirst;
    Min:= nil;
    if (Ptr^ as TAutomata).Find (CharPtr, Len, FinFindPositionInfo) then
      Min:= FinFindPositionInfo.StartPtr;

    Inc (Ptr);
    for i:= 2 to AConfig.FinAutomatas.Size do
    begin
      if (Ptr^ as TAutomata).Find (CharPtr, Len, FinFindPositionInfo) then
      begin
        TempCharPtr:= FinFindPositionInfo.StartPtr;
        if (Min= nil) or (TempCharPtr< Min) then
          Min:= TempCharPtr;

      end;

       Inc (Ptr);

    end;

    if Min= nil then
      Exit (False);

    EndCharPtr:= Min- 1;
    CharPtr:= Min;
    Result:= True;

  end;

  procedure CreateString (StartCharPtr, EndCharPtr: PChar; var S: String; NeecEncoding: Boolean);
  var
    Len: Integer;
    PositionInfo: TFindResultInfo;
    Index: Integer;

  begin
    Len:= EndCharPtr- StartCharPtr+ 1;
    SetLength (S, Len);

    MoveChar0 (StartCharPtr^, S [1], Len);

    while AmpAutomata.Find (@ S[1], Len, PositionInfo) do
    begin
      Index:= PositionInfo.StartIndex;
      Delete (S, Index+ 2, 4);//&amp;

    end;

  end;

  function ParseCountString (var S: String): Int64; inline;
  var
    PCh: PChar;
    i: Integer;

  begin
    PCh:= @S [1];
    Result:= 0;

    for i:= 1 to Length (S) do
    begin
      if ('0'<= PCh^) and (PCh^<= '9') then
        Result:= Result* 10+ Ord (PCh^)- 48;
      Inc (PCh);

    end;

  end;

var
  StartCharPtr, EndCharPtr: PChar;
  CountStr: String;
  Link, Title, Summary: String;
  ASearchResult: TSearchResult;
  ThisSearchResult: TSearchResultCollection;
  ResultCountInThisSearch,
  ResultAnnouncedInThisSearch: Int64;

begin
  if not ParseEntry (ResultCountConfig, StartCharPtr, EndCharPtr) then
    Exit;

  CreateString (StartCharPtr, EndCharPtr, CountStr, False);
  ResultAnnouncedInThisSearch:= ParseCountString (CountStr);
  ResultCountInThisSearch:= 0;
  ThisSearchResult:= TSearchResultCollection.Create (ResultAnnouncedInThisSearch);

  while True do
  begin
    if ParseEntry (LinkConfig, StartCharPtr, EndCharPtr) then
      CreateString (StartCharPtr, EndCharPtr, Link, LinkConfig.NeedEncoding)
    else
      Break;

    if ParseEntry (TitleConfig, StartCharPtr, EndCharPtr) then
      CreateString (StartCharPtr, EndCharPtr, Title, TitleConfig.NeedEncoding)
    else
      Break;
    if ParseEntry (SummaryConfig, StartCharPtr, EndCharPtr) then
      CreateString (StartCharPtr, EndCharPtr, Summary, SummaryConfig.NeedEncoding)
    else
      Break;

    ASearchResult:= TSearchResult.Create (Title, Link, Summary);
    ThisSearchResult.Add (ASearchResult);
    Inc (ResultCountInThisSearch);

  end;

  ResultPool.Merge (ThisSearchResult, ResultAnnouncedInThisSearch);
  ThisSearchResult.Clear;
  ThisSearchResult.Free;

end;

end.

