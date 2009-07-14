unit MainPageUnit; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PageHandlerBaseUnit, FileStringsUnit, AbstractHandlerUnit;

type

  { TMainPageDispatcher }

  TMainPageDispatcher= class (TXMLHandlerPage)
  private

  public
    constructor Create;
    destructor Destroy; override;

    procedure MyDispatch; override;
    function CreateNewInstance: TAbstractHandler; override;

  end;

implementation
uses
  XMLNode, ThisProjectGlobalUnit, PersaDictionaryUnit;

{ TMainPageDispatcher }
constructor TMainPageDispatcher.Create;
begin
  inherited Create ('PersaDic', '/PersaDic/PersaDic.xslt');

end;

destructor TMainPageDispatcher.Destroy;
begin
  inherited Destroy;

end;

procedure TMainPageDispatcher.MyDispatch;
var
  QueryInfo: TXMLNode;
  Word: String;
  QueryResult: TQueryResult;
  i: Integer;
  Answers, Answer: TXMLNode;
  StartTime, EndTime: TTimeStamp;

begin
  StartTime:= DateTimeToTimeStamp (Time);

  QueryInfo:= TXMLNode.Create (XMLRoot, 'QueryInfo');
  Word:= Vars.CgiVarValueByName ['Q'];
  QueryInfo.AddAttribute ('Word', Word);
  System.WriteLn ('TMainPageDispatcher.MyDispatch word= ', Word);
  if Word= '' then
    Exit;

  QueryResult:= TQueryResult.Create (Word);
  GlobalObjContainer.PersaDic.FindMeaning (Word, QueryResult);

  Answers:= TXMLNode.Create (QueryInfo, 'Answers');
  for i:= 0 to QueryResult.Count- 1 do
  begin
    Answer:= TXMLNode.Create (Answers, 'Answer');
    Answer.AddAttribute ('Word', QueryResult.Word [i]);
    Answer.AddAttribute ('Meaning', QueryResult.Meaning [i]);

  end;

  EndTime:= DateTimeToTimeStamp (Time);

  QueryInfo.AddAttribute ('Time', IntToStr (EndTime.Time- StartTime.Time));

  QueryInfo.AddAttribute ('StartingTime', DateTimeToStr (GlobalObjContainer.StartTime));
  QueryInfo.AddAttribute ('AnsweredQuery', IntToStr (GlobalObjContainer.ServedRequestCount));

end;

function TMainPageDispatcher.CreateNewInstance: TAbstractHandler;
begin
  Result:= TMainPageDispatcher.Create;

end;

initialization

finalization

end.

