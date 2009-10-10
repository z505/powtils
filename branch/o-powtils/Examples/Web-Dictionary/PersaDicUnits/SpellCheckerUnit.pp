unit SpellCheckerUnit; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DictionaryTreeUnit;

type

  { TSpellChecker }

  TSpellChecker= class (TObject)
  private
    FDicTree: TDictionaryTree;

  public
    constructor Create (DicTree: TDictionaryTree);
    destructor Destroy; override;

    { DoSpellChecking returns true if AWord exists in FDicTree and
        false otherwise.}
    function DoSpellChecking (const AWord: String;
                              Suggestions: TStrings): Boolean;

  end;

implementation
uses
  StringKeyUnit, MapUnit;

{ TSpellChecker }

constructor TSpellChecker.Create (DicTree: TDictionaryTree);
begin
  inherited Create;

  FDicTree:= DicTree;

end;

destructor TSpellChecker.Destroy;
begin
  inherited Destroy;

end;

type                                                       
  TSearchStatus= record
    NoOfDeletedChar: Byte;
    NoOfAddedChars: Byte;
    NoOfReplacements: Byte;
    Index: Byte;
    Node: TAbstractDicTreeNode;
    Cost: Byte;
    Text: AnsiString;

  end;

var
  IntToCharArray: array [0..255] of Char;

function CompareTwoSearchStatus (const a: TSearchStatus; const b: TSearchStatus): Boolean;//IsGreaterThan
begin
  if a.Index< b.Index then
    Exit (False)
  else if b.Index< a.Index then
    Exit (True)
  else//a.Index= b.Index
  begin
    if DWord (a.Node)<= DWord (b.Node) then
      Exit (False)
    else
      Exit (True);

  end;

end;

type
  TSearchStatusMap= specialize TMap<TSearchStatus>;

function TSpellChecker.DoSpellChecking (const AWord: String;
                      Suggestions: TStrings): Boolean;
const
  QueueSize= 10000;

var
  WordLen: Word;
  Queue: array [0..QueueSize] of TSearchStatus;
  SoQ, EoQ: Integer;
  MinCost: Integer;

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
  VisitedSearchStatus: TSearchStatusMap;

  procedure InsertToQueue (const _Index: Integer; const _DeleteChar: Byte;
                           const _AddChar: Byte;
                           const _ReplacementCount: Byte;
                           const _Cost: Integer;
                           const _Node: TAbstractDicTreeNode);
  var
    TempSearchStatus: TSearchStatus;
    FoundSearchStatus: TSearchStatusMap.TFindResult;

  begin
    if MinCost< _Cost then
      Exit;

    TempSearchStatus.Index:= _Index;
    TempSearchStatus.Node:= _Node;

    FoundSearchStatus:= VisitedSearchStatus.Find (TempSearchStatus);

    if FoundSearchStatus.IsValid then
      if FoundSearchStatus.Data.Cost< _Cost then
        Exit;

    if (EoQ+ 1) mod QueueSize= SoQ then
      Exit;

    VisitedSearchStatus.Insert (TempSearchStatus);

    with Queue [EoQ] do
    begin
      NoOfDeletedChar:= _DeleteChar;
      NoOfAddedChars:= _AddChar;
      NoOfReplacements:= _ReplacementCount;
      Index:= _Index;
      Node:= _Node;
      Cost:= _Cost;
//      Text:= FindStringForNode (Node);

(*$IFDEF DebugMode*)
      Text:= FindStringForNode (Node);
(*$Endif*)

    end;
    EoQ:= (EoQ+ 1) mod QueueSize;

  end;

  function QueueIsNotEmpty: Boolean;
  begin
    Result:= not (SoQ= EoQ);

  end;

  function DeleteFromQueue: TSearchStatus;
  begin
    Result:= Queue [SoQ];
    SoQ:= (SoQ+ 1) mod QueueSize;

  end;

  function ExistsInDic (const AWord: String): TAbstractDicTreeNode;
  var
    StrKey: TStringKey;

  begin
    StrKey:= TStringKey.Create ;
    StrKey.SetKey (AWord);

    Result:= FDicTree.NodeByKey [StrKey];

    StrKey.Free;

  end;

var
  ActiveEntry: TSearchStatus;
  Node: TAbstractDicTreeNode;
  i: Integer;
  StrKey: TStringKey;

begin
  Node:= ExistsInDic (AWord);

  if Node<> nil then
    if Node.DataInNode<> nil then
    begin
      Suggestions.Clear;
      Suggestions.AddObject (FindStringForNode (Node), Node);
      Exit (True);

    end;

  StrKey:= TStringKey.Create (AWord);
  Result:= False;

  WordLen:= Length (AWord);
  SoQ:= 0; EoQ:= 0;

  Suggestions.Clear;
  VisitedSearchStatus:= TSearchStatusMap.Create (@CompareTwoSearchStatus);

  InsertToQueue (0, 0, 0, 0, 0, FDicTree.Root);
  MinCost:= MaxInt;

  while QueueIsNotEmpty do
  begin
    ActiveEntry:= DeleteFromQueue;

    if ActiveEntry.Node.ChildByIndex [StrKey.ValueAt [ActiveEntry.Index]-
                                        StrKey.PossibleMinValue]<> nil then
      InsertToQueue (ActiveEntry.Index+ 1,
                     ActiveEntry.NoOfDeletedChar,
                     ActiveEntry.NoOfAddedChars,
                     ActiveEntry.NoOfReplacements,
                     ActiveEntry.Cost,
                     ActiveEntry.Node.
                          ChildByIndex [StrKey.ValueAt [ActiveEntry.Index]-
                                        StrKey.PossibleMinValue]
                     );//No change

    if ActiveEntry.Node.DataInNode<> nil then
      if ActiveEntry.Cost+ WordLen- ActiveEntry.Index< MinCost then
      begin
        MinCost:= ActiveEntry.Cost+ WordLen- ActiveEntry.Index;
        Suggestions.Clear;
        Suggestions.AddObject (FindStringForNode (ActiveEntry.Node),
                               ActiveEntry.Node);

      end
      else if (ActiveEntry.Cost+ Abs (WordLen- ActiveEntry.Index)= MinCost) then
        Suggestions.AddObject (FindStringForNode (ActiveEntry.Node), ActiveEntry.Node);

     if WordLen< 2* ActiveEntry.Cost then
       Continue;

     if ActiveEntry.Index< WordLen then
       InsertToQueue (ActiveEntry.Index+ 1,
                     ActiveEntry.NoOfDeletedChar+ 1,
                     ActiveEntry.NoOfAddedChars,
                     ActiveEntry.NoOfReplacements,
                     ActiveEntry.Cost+ 1,
                     ActiveEntry.Node);//Delete a Char

        for i:= 0 to ActiveEntry.Node.Size- 1 do
          if ActiveEntry.Node.ChildByIndex [i]<> nil then
            InsertToQueue (ActiveEntry.Index,
                           ActiveEntry.NoOfDeletedChar,
                           ActiveEntry.NoOfAddedChars+ 1,
                           ActiveEntry.NoOfReplacements,
                           ActiveEntry.Cost+ 1,
                           ActiveEntry.Node.ChildByIndex [i]);//Insert a Char

        for i:= 0 to ActiveEntry.Node.Size- 1 do
          if (ActiveEntry.Node.ChildByIndex [i]<> nil) and (AWord [ActiveEntry.Index]<> IntToCharArray [i]) then
          begin
            InsertToQueue (ActiveEntry.Index+ 1,
                           ActiveEntry.NoOfDeletedChar,
                           ActiveEntry.NoOfAddedChars,
                           ActiveEntry.NoOfReplacements+ 1,
                           ActiveEntry.Cost+ 1,
                           ActiveEntry.Node.ChildByIndex [i]);//Replace a Char

          end;

        if ActiveEntry.Index<= WordLen- 2 then
          if ActiveEntry.Node.ChildByIndex [StrKey.ValueAt [ActiveEntry.Index+ 1]-
                                            StrKey.PossibleMinValue]<> nil then
            if ActiveEntry.Node.ChildByIndex [StrKey.ValueAt [ActiveEntry.Index+ 1]-
                                            StrKey.PossibleMinValue].
                  ChildByIndex [StrKey.ValueAt [ActiveEntry.Index]-
                                            StrKey.PossibleMinValue]<> nil then
            begin
              InsertToQueue (ActiveEntry.Index+ 2,
                             ActiveEntry.NoOfDeletedChar,
                             ActiveEntry.NoOfAddedChars,
                             ActiveEntry.NoOfReplacements+ 1,
                             ActiveEntry.Cost+ 1,
                             ActiveEntry.Node.
                                  ChildByIndex [StrKey.ValueAt [ActiveEntry.Index+ 1]-
                                            StrKey.PossibleMinValue].
                                  ChildByIndex [StrKey.ValueAt [ActiveEntry.Index]-
                                            StrKey.PossibleMinValue]
                             );//Change position of two consecutive chars in AWord

            end;

      end;

  StrKey.Free;
  VisitedSearchStatus.Free;

end;

var
  i: Integer;

initialization
  for i:= 0 to High (IntToCharArray) do
   IntToCharArray [i]:= Chr (i);

end.

