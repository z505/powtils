unit HTMLParserUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StringSearchUnit, CollectionUnit, SearchResultItemUnit;
  
type

  { TSearchTags }

  TSearchTags= class (TObject)
  private
    StartingTags: TStringList;// All of tags in StartingTags should be visited
    StartingTagSearcher: TBaseCollection;
    DecodeNeeded: Boolean;
    EndingTags: TStringList;// One of the tags in EndingTags should be visited
    EndingTagSearcher: TBaseCollection;

  public
    constructor Create (AnStream: TStream);
    destructor Destroy; override;
    
    function FindFirst (CharPtr: PChar; var Len: Integer): String;
   
  end;
  
  EInvalidConfigFile= class (Exception);
  
  { THMLParser }

  THMLParser= class (TObject)
  private
    LinkTags, SummaryTags, TitleTags,
    NumberOfResults, BoundaryTags: TSearchTags;
    SearchEngineNameStr: String;
    SearchEngineName: PChar;
    
  public
    constructor Create (AnStream: TStream);
    destructor Destroy; override;
    
    function Parse (var CharPtr: PChar; var Len: Integer): TSearchItemCollection;
    
  end;
  
implementation
uses
  MyTypes;
  
function ReadLine (AnStream: TStream): String;
var
  Ch: Char;
  
begin
  Result:= '';

  if AnStream.Position< AnStream.Size then
  begin
    AnStream.Read (Ch, 1);

    while Ch<> #10 do
    begin
      Result:= Result+ Ch;
      AnStream.Read (Ch, 1);
      if AnStream.Position= AnStream.Size then
        Break;

    end;

  end;
  
end;

{ TSearchTags }

constructor TSearchTags.Create (AnStream: TStream);
var
  S: String;
  n: Integer;
  
begin
  DecodeNeeded:= UpperCase (ReadLine (AnStream)) [1]= 'T';
  n:= StrToInt (ReadLine (AnStream));
  StartingTags:= TStringList.Create;
  StartingTagSearcher:= TBaseCollection.Create;
  
  while 0< n do
  begin
    S:= ReadLine (AnStream);
    StartingTagSearcher.Add (TFastStringSearchAlgorithm.Create (S));
    StartingTags.Add (S);
    Dec (n);
    
  end;

  n:= StrToInt (ReadLine (AnStream));
  EndingTags:= TStringList.Create;
  EndingTagSearcher:= TBaseCollection.Create;
  
  while 0< n do
  begin
    S:= ReadLine (AnStream);
    EndingTags.Add (S);
    EndingTagSearcher.Add (TFastStringSearchAlgorithm.Create (S));
    Dec (n);
    
  end;

end;

destructor TSearchTags.Destroy;
begin
  StartingTags.Free;
  EndingTags.Free;
  StartingTagSearcher.Free;
  EndingTagSearcher.Free;
  
  inherited Destroy;
  
end;

function TSearchTags.FindFirst (CharPtr: PChar; var Len: Integer): String;
var
  i: Integer;
  Ptr: PObject;
  StartCharPtr: PChar;
  StartPtr: PChar;
  
begin
  Result:= '';
  
  Ptr:= StartingTagSearcher.GetPointerToFirst;
  StartPtr:= CharPtr;
  for i:= 1 to StartingTagSearcher.Size do
  begin
    StartPtr:= (Ptr^ as TFastStringSearchAlgorithm).FindFirst (StartPtr, Len).Second;
    if StartPtr= nil then
      Exit;
    Inc (StartPtr);
    Dec (Len, StartPtr- CharPtr);
    
  end;
  
end;

{ THMLParser }

constructor THMLParser.Create (AnStream: TStream);
begin
  inherited Create;
  
  SearchEngineNameStr:= ReadLine (AnStream);
  SearchEngineName:= @SearchEngineNameStr [1];
  
  NumberOfResults:= TSearchTags.Create (AnStream);
  if UpperCase (ReadLine (AnStream))<> 'LINK' then
    raise EInvalidConfigFile.Create ('LINK');
  LinkTags:= TSearchTags.Create (AnStream);
  if UpperCase (ReadLine (AnStream))<> 'TITLE' then
    raise EInvalidConfigFile.Create ('TITLE');
  TitleTags:= TSearchTags.Create (AnStream);
  if UpperCase (ReadLine (AnStream))<> 'SUMMARY' then
    raise EInvalidConfigFile.Create ('SUMMARY');
  SummaryTags:= TSearchTags.Create (AnStream);

end;

destructor THMLParser.Destroy;
begin
  LinkTags.Free;
  SummaryTags.Free;
  TitleTags.Free;
  NumberOfResults.Free;
  
  inherited Destroy;
  
end;

function THMLParser.Parse(var CharPtr: PChar; var Len: Integer
  ): TSearchItemCollection;
begin

end;

end.

