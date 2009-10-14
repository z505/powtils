unit PersaDictionaryUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DictionaryTreeUnit, StreamUnit, MeaningUnit,
  SpellCheckerUnit, CollectionUnit;

type

  { TMeaning }

  TMeaning= class (TAbstractDataInNode)
  private
    FMeanings: array of AnsiString;
    function GetMeaning (Index: Integer): AnsiString;
    function GetMeaningCount: Integer;

    procedure AddNewMeaning (const NewMeaning: AnsiString);

  public
    property Meaning [Index: Integer]: AnsiString read GetMeaning;
    property MeaningCount: Integer read GetMeaningCount;

    procedure LoadFromStream (ABinaryStream: TMyBinStream); override;
    procedure SaveToStream (ABinaryStream: TMyBinStream); override;

    function ToString: AnsiString; override;

    constructor Create (ABinaryStream: TMyBinStream); overload;
    constructor Create (AWord: AnsiString); overload;

  end;

  { TQueryResult }

  TQueryResult= class (TStringList)
  private
    FMisSpelled: Boolean;
    FQuery: AnsiString;

    function GetMeaning (Index: Integer): AnsiString; inline;
    function GetNode (Index: Integer): TAbstractDicTreeNode; inline;
    function GetWord(Index: Integer): AnsiString; inline;
    function FindStringForNode (const Node: TAbstractDicTreeNode): AnsiString; inline;


  protected
    property Node [Index: Integer]: TAbstractDicTreeNode read GetNode;

    procedure AddObject (const S: AnsiString; Data: TAbstractDicTreeNode);

  public
    property MisSpelled: Boolean read FMisSpelled;
    property Query: AnsiString read FQuery;
    property Word [Index: Integer]: AnsiString read GetWord;
    property Meaning [Index: Integer]: AnsiString read GetMeaning;

    constructor Create (const S: AnsiString);

    function ToXML: AnsiString;


  end;

  { TPersaDic }

  TPersaDic= class (TDictionaryTree)
  private
    SpellChecker: TSpellChecker;

  public
    procedure LoadFromTextFile (const Filename: String);
    procedure LoadFromTextFile1 (const Filename: String);
    procedure LoadFromBinaryFile (const Filename: String);

    constructor Create;
    destructor Destroy; override;

    procedure SaveAsBinaryFile (const Filename: String);
    procedure SaveAsTextFile (const Filename: String);

    //Checks if the Word already exists in the dictionary. If not, it adds it to dictionarytree.
    //Otherwise, it retrieve the associated meaning with Word and checks if the distance between
    // new Meaning and all the old meaning is greater than MinDistance of sum of their length or not.
    // If it is, then Meaning will be added as a new meaning for Word.
    // Returns True if it successfuly added and False otherwise.
    function AddWordMeaning (const WordStr: AnsiString; MeaningStr: AnsiString; MinDistance: Integer= 5): Boolean;

    function FindMeaning (const S: AnsiString; Answers: TQueryResult): Boolean;

    // FindWords assumes that the Word contains the lowercase letters.
    function FindWords (const Word: AnsiString; Answers: TQueryResult): Boolean;// '?' considered as wildchar

    procedure Prepare;

  end;

implementation
uses
  StringKeyUnit, Math;

{ TMeaning }

function TMeaning.GetMeaning (Index: Integer): AnsiString;
begin
  Result:= FMeanings [Index];

end;

function TMeaning.GetMeaningCount: Integer;
begin
  Result:= Length (FMeanings);

end;

procedure TMeaning.AddNewMeaning (const NewMeaning: AnsiString);
begin
  SetLength (FMeanings, Length (FMeanings)+ 1);
  FMeanings [High (FMeanings)]:= NewMeaning;

end;

procedure TMeaning.LoadFromStream (ABinaryStream: TMyBinStream);
var
  S: AnsiString;

begin
  SetLength (FMeanings, 1);

  S:= ABinaryStream.ReadStr;
  FMeanings [0]:= S;

end;

procedure TMeaning.SaveToStream (ABinaryStream: TMyBinStream);
var
  i:Integer;

begin
  ABinaryStream.WriteStr (FMeanings [0]);

  for i:= 1 to MeaningCount- 1 do
  begin
    ABinaryStream.WriteStr ('::@::');
    ABinaryStream.WriteStr (FMeanings [i]);

  end;

end;

function TMeaning.ToString: AnsiString;
var
  i: Integer;

begin
  Result:= FMeanings [0];

  for i:= 1 to MeaningCount- 1 do
  begin
    Result+= '::@::';
    Result+= FMeanings [i];

  end;

end;

constructor TMeaning.Create (ABinaryStream: TMyBinStream);
begin
  inherited Create;

  LoadFromStream (ABinaryStream);

end;

constructor TMeaning.Create (AWord: AnsiString);
begin
  inherited Create;

  SetLength (FMeanings, 1);
  FMeanings [0]:= AWord;

end;

function RemoveSpaces (const S: AnsiString): AnsiString;
var
  i, l: Integer;

begin
  l:= Length (S);
  i:= 0;

  Result:= '';
  while i< l do
  begin
    Inc (i);

    if S [i] in [' '] then
      Continue;
    Result+= S [i];

  end;
end;

{ TPersaDic }
procedure TPersaDic.LoadFromTextFile (const Filename: String);
var
  MyStream: TMyTextStream;
  Stream: TFileStream;
  WordAndMeaningStr: AnsiString;
  WordKey: TStringKey;

  procedure SeparateAndInsertWordAndMeaning (const S: AnsiString);
  var
    Node :TAbstractDicTreeNode;
    Meaning: TMeaning;
    MeaningStr, WordStr: AnsiString;
    TempStr: AnsiString;
    i, j: Integer;
    l: Integer;
    Flag: Boolean;

  begin

    i:= 1;
    WordStr:= '';
    MeaningStr:= '';

    l:= Length (S);
    while i<= l do
    begin
      if Copy (S, i, 4)= ' :: ' then
        Break;

      WordStr+= S [i];
      Inc (i);

    end;
    Inc (i, 4);

    while i<= l do
    begin
      MeaningStr+= S [i];
      Inc (i);

    end;

    WordStr:= Trim (WordStr);
    MeaningStr:= Trim (MeaningStr);

    if WordStr= '' then
      Exit;

    if (Pos ('(', WordStr)<> 0) and (WordStr [Length (WordStr)]= ')') then
    begin
      try
//        StrToInt (Copy (WordStr, Pos ('(', WordStr)+ 1, Length (WordStr)- 1- Pos ('(', WordStr)));
      except
        on e: EConvertError do
          Exit;

      end;
      Delete (WordStr, Pos ('(', WordStr), Length (WordStr));


    end;
    AddWordMeaning (WordStr, MeaningStr);

  end;

begin
  Stream:= TFileStream.Create (Filename, fmOpenRead);
  MyStream:= TMyTextStream.Create (Stream);

  MyStream.ReadCh;
  MyStream.ReadCh;
  MyStream.ReadCh;

  WordKey:= TStringKey.Create;

  while MyStream.Position< MyStream.Size do
  begin
    WordAndMeaningStr:= MyStream.ReadLine;
    SeparateAndInsertWordAndMeaning (WordAndMeaningStr);

  end;

  WordKey.Free;
  MyStream.Free;
  Stream.Free;

end;

procedure TPersaDic.LoadFromTextFile1 (const Filename: String);
var
  MyStream: TMyTextStream;
  Stream: TFileStream;
  WordAndMeaningStr: AnsiString;
  WordKey: TStringKey;

  procedure SeparateAndInsertWordAndMeaning (const S: AnsiString);
  var
    Meaning: TMeaning;
    MeaningStr, WordStr: AnsiString;
    i: Integer;
    l: Integer;

  begin

    i:= 1;
    WordStr:= '';
    MeaningStr:= '';

    l:= Length (S);
{    if S [l]<> '|' then
    begin
      WriteLN (S);
      Exit;

    end;
}
    while i<= l do
    begin
      if S [i]= '|' then
        Break;

      MeaningStr+= S [i];
      Inc (i);

    end;
    Inc (i);

    while i<= l- 1 do
    begin
      WordStr+= S [i];
      Inc (i);

    end;

    AddWordMeaning (WordStr, MeaningStr);

  end;

begin

  Stream:= TFileStream.Create (Filename, fmOpenRead);
  MyStream:= TMyTextStream.Create (Stream);

  WordKey:= TStringKey.Create;

  while MyStream.Position< MyStream.Size do
  begin
    WordAndMeaningStr:= Trim (MyStream.ReadLine);
    SeparateAndInsertWordAndMeaning (WordAndMeaningStr);

  end;

  WordKey.Free;
  MyStream.Free;
  Stream.Free;

end;

procedure TPersaDic.LoadFromBinaryFile (const Filename: String);
var
  MyStream: TMyTextStream;
  Stream: TFileStream;

begin
  raise Exception.Create ('Not Implemented Yet!');

  Stream:= TFileStream.Create (Filename, fmOpenRead);
  MyStream:= TMyTextStream.Create (Stream);

  MyStream.Free;
  Stream.Free;

  SpellChecker:= TSpellChecker.Create (Self);


end;

constructor TPersaDic.Create;
begin
  inherited;

  SpellChecker:= nil;

end;

destructor TPersaDic.Destroy;
begin
  SpellChecker.Free;

  inherited;

end;

procedure TPersaDic.SaveAsBinaryFile (const Filename: String);
var
  Stream: TFileStream;

begin
  inherited Create;

  Stream:= TFileStream.Create (Filename, fmCreate);

  inherited SaveToStream (Stream);

  Stream.Free;

end;

procedure TPersaDic.SaveAsTextFile (const Filename: String);
var
  MyStream: TMyTextStream;
  Stream: TFileStream;
  WordAndMeaningStr: AnsiString;
  WordKey: TStringKey;

  procedure SeparateAndInsertWordAndMeaning (const S: AnsiString);
  var
    Meaning: TMeaning;
    MeaningStr, WordStr: AnsiString;
    i: Integer;
    l: Integer;

  begin

    i:= 1;
    WordStr:= '';
    MeaningStr:= '';

    l:= Length (S);
    while i<= l do
    begin
      if Copy (S, i, 4)= ' :: ' then
        Break;

      WordStr+= S [i];
      Inc (i);

    end;
    Inc (i, 4);

    while i<= l do
    begin
      MeaningStr+= S [i];
      Inc (i);

    end;

    WordKey.SetKey (LowerCase (WordStr));
    Meaning:= TMeaning.Create (MeaningStr);

    Insert (WordKey, Meaning, False);

  end;

  function FindStringForNode (const Node: TAbstractDicTreeNode): AnsiString;
  var
    ActiveNode: TAbstractDicTreeNode;

  begin
    ActiveNode:= Node;
    Result:= '';

    while ActiveNode.ParentNode<> nil do
    begin
      Result:= Chr (ActiveNode.IndexInParent)+ Result;
      ActiveNode:= ActiveNode.ParentNode;

    end;

  end;

var
  Index: Integer;
  Node: TAbstractDicTreeNode;
  Meaning: TMeaning;
  WordStr: String;
  MeaninigStr: String;

begin
  Stream:= TFileStream.Create (Filename, fmCreate);
  MyStream:= TMyTextStream.Create (Stream);

  MyStream.WriteChar (' ');
  MyStream.WriteChar (' ');
  MyStream.WriteChar (' ');

  WordKey:= TStringKey.Create;
  Index:= 1;

  while Index<= NoOfDataInTree do
  begin
    Node:= NodeByIndex [Index];
    WordStr:= FindStringForNode (Node);
    Meaning:= Node.DataInNode as TMeaning;

    MyStream.WriteLine (WordStr+ ' :: '+ Meaning.ToString);

    Inc (Index);

  end;

  WordKey.Free;
  MyStream.Free;
  Stream.Free;

end;

function TPersaDic.AddWordMeaning (const WordStr: AnsiString; MeaningStr: AnsiString; MinDistance: Integer): Boolean;
  function Distance (const S1, S2: String): Integer;
  var
    dp: array of array of Integer;

    function Find (Len1, Len2: Integer): Integer;
    begin
      if Len1= 0 then
        Exit (Len2)
      else if Len2= 0 then
        Exit (Len1);

      if 0<= dp [Len1, Len2] then
        Exit (dp [Len1, Len2]);

      if S1 [Len1]= S2 [Len2] then
        Result:= Find (Len1- 1, Len2- 1)
      else
        Result:= Find (Len1- 1, Len2- 1)+ 1;//Replace.

      if Find (Len1- 1, Len2)+ 1< Result then
        Result:= Find (Len1- 1, Len2)+ 1;

      if Find (Len1, Len2- 1)+ 1< Result then
        Result:= Find (Len1, Len2- 1)+ 1;

    end;

  var
    i, j: Integer;

  begin
    SetLength (dp, Length (S1)+ 1);
    for i:= 0 to Length (S1) do
    begin
      SetLength (dp [i], Length (S2)+ 1);
      FillChar (dp [i][0], SizeOf (dp [i]), 255);

    end;

    Result:= Find (Length (S1), Length (S2));

    for i:= 0 to Length (S1) do
      SetLength (dp [i], 0);
    SetLength (dp, 0);

  end;

var
  WordKey: TStringKey;
  Meaning: TMeaning;
  Node: TAbstractDicTreeNode;
  Flag: Boolean;
  i: Integer;
  TempStr: AnsiString;

begin
  WordKey:= TStringKey.Create (LowerCase (WordStr));
  Meaning:= TMeaning.Create (MeaningStr);

  if Insert (WordKey, Meaning, False) then
  begin
    Node:= NodeByKey [WordKey];
    Flag:= False;

    for i:= 0 to (Node.DataInNode as TMeaning).MeaningCount- 1 do
    begin
      TempStr:= RemoveSpaces ((Node.DataInNode as TMeaning).Meaning [i]);
      if Distance (TempStr, RemoveSpaces (MeaningStr))<
        (Length (TempStr)+ Length (RemoveSpaces (MeaningStr)))/ MinDistance then
      begin
        Flag:= True;
        Break;

      end;

    end;

    if not Flag then
      (Node.DataInNode as TMeaning).AddNewMeaning (MeaningStr)

  end;

  WordKey.Free;


end;

function TPersaDic.FindMeaning (const S: AnsiString; Answers: TQueryResult): Boolean;

  function FindNode (const AWord: String): TAbstractDicTreeNode;
  var
    StrKey: TStringKey;

  begin
    StrKey:= TStringKey.Create (AWord);

    Result:= Self.GetNodeByKey (StrKey);

    StrKey.Free;

  end;

var
  i: Integer;
  Suggestions: TStringList;
  LowerCaseS: AnsiString;

begin
  LowerCaseS:= LowerCase (S);

  Answers.Clear;
  Suggestions:= TStringList.Create;

  SpellChecker.DoSpellChecking (LowerCaseS, Suggestions);

  Result:= False;

  if Suggestions.Count= 1 then
    if Suggestions [0]= LowerCaseS then
      Result:= True;

  if 0< Suggestions.Count then
  begin
    Suggestions.Sort;
    Answers.AddObject (Suggestions [0],
        TAbstractDicTreeNode (Suggestions.Objects [0]));

    for i:= 1 to Suggestions.Count- 1 do
      if Suggestions.Objects [i]<> Suggestions.Objects [i- 1] then
        Answers.AddObject (Suggestions [i],
            TAbstractDicTreeNode (Suggestions.Objects [i]));

  end;

  Suggestions.Free;

end;

function TPersaDic.FindWords(const Word: AnsiString; Answers: TQueryResult
  ): Boolean;
const
  MaxResult: Integer= 100;

var
  Key: TStringKeyWithWildChar;
  ResultCountTillNow: Integer;

  procedure DFS (Index: Integer; Node: TAbstractDicTreeNode);
  var
    i: Integer;

  begin
    if Index= Key.Length then
    begin
      if MaxResult<= ResultCountTillNow then
        Exit;

      if Node.DataInNode<> nil then
      begin
        Answers.AddObject (Node.DataInNode.ToString, Node);
        Inc (ResultCountTillNow);
        Exit;

      end;

      Exit;

    end;

    if MaxResult<= ResultCountTillNow then
      Exit;

    if Key.IsWildChar [Index] then
    begin
      for i:= Key.PossibleMinValue to Key.PossibleMaxValue do
        if Node.ChildByIndex [i]<> nil then
        begin
          DFS (Index+ 1, Node.ChildByIndex [i]);

          if MaxResult<= ResultCountTillNow then
            Exit;

        end;

    end
    else if Node.ChildByIndex [Key.ValueAt [Index]]<> nil then
      DFS (Index+ 1, Node.ChildByIndex [Key.ValueAt [Index]])
    else
      Exit;

  end;

begin
  Key:= TStringKeyWithWildChar.Create (Word);
  ResultCountTillNow:= 0;

  DFS (0, Root);
  Result:= 1<= ResultCountTillNow;

end;

procedure TPersaDic.Prepare;
begin
  SpellChecker:= TSpellChecker.Create (Self);

end;

{ TQueryResult }

function TQueryResult.GetNode (Index: Integer): TAbstractDicTreeNode;
begin
  Result:= Objects [Index] as TAbstractDicTreeNode;

end;

function TQueryResult.GetMeaning (Index: Integer): AnsiString;
begin
  Result:= Node [Index].DataInNode.ToString;

end;

function TQueryResult.FindStringForNode (const Node: TAbstractDicTreeNode): AnsiString;
var
  ActiveNode: TAbstractDicTreeNode;

begin
  ActiveNode:= Node;
  Result:= '';

  while ActiveNode.ParentNode<> nil do
  begin
    Result:= Chr (ActiveNode.IndexInParent)+ Result;
    ActiveNode:= ActiveNode.ParentNode;

  end;

end;

function TQueryResult.GetWord (Index: Integer): AnsiString;
begin
  Result:= FindStringForNode (Node [Index]);

end;

constructor TQueryResult.Create (const S: AnsiString);
begin
  inherited Create;

  FQuery:= S;

end;

procedure TQueryResult.AddObject (const S: AnsiString; Data: TAbstractDicTreeNode
  );
begin
  inherited AddObject (S, Data);

end;

function TQueryResult.ToXML: AnsiString;
var
  i: Integer;

begin
  Result:= '<Query Word= "'+ Query+ '">';
  Result+= '<Answers>';

  for i:= 0 to Count- 1 do
    Result+= '<Answer Word= "'+ FindStringForNode (Node [i])+
                   '" Meaninig= "'+ Node [i].DataInNode.ToString+ '" />';

  Result+= '</Answers>';
  Result+= '</Query>';

end;

end.

