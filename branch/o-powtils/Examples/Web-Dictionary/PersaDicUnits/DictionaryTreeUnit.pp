unit DictionaryTreeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MyTypes, StreamUnit;

type

  { TAbstractKey }

  TAbstractKey= class (TObject)
  protected
    function GetMaxValue: Integer; virtual; abstract;
    function GetMinValue: Integer; virtual; abstract;
    function GetLength: Integer; virtual; abstract;
    function GetValueAt (Index: Integer): Integer; virtual; abstract;

  public
    property PossibleMaxValue: Integer read GetMaxValue;
    property PossibleMinValue: Integer read GetMinValue;

    property ValueAt [Index: Integer]: Integer read GetValueAt;
    property Length: Integer read GetLength;

  end;

  TInsertionResultInformation= record
    NoOfNewNode: Integer;
    DataHasBeenSaved: Boolean;
    DataHasBeenOverwritten: Boolean;

  end;

  { TAbstractDataInNode }

  TAbstractDataInNode= class (TObject)
  private
  public
    procedure LoadFromStream (ABinaryStream: TMyBinStream); virtual; abstract;
    procedure SaveToStream (ABinaryStream: TMyBinStream); virtual; abstract;

    function ToString: AnsiString; virtual; abstract;

  end;

  { TAbstractDicTreeNode }

  TAbstractDicTreeNode= class (TObject)
  private
    function GetChildByIndex (const Index: Integer): TAbstractDicTreeNode; virtual; abstract;
    function GetDataInNode: TAbstractDataInNode; virtual;
    function GetSize: Integer; virtual; abstract;

    {Insert tries to insert Data with key in the tree and returns the number of
       nodes created in order to insert the Data in tree}
    function Insert (const Key: TAbstractKey; const Index: Integer;
                     const Data: TAbstractDataInNode; const Overwrite: Boolean):
                     TInsertionResultInformation; virtual; abstract;

    {GetDataByKey tries to find the node corresponding to key in the tree and returns
       the Data stored in that node, and nil otherwise}
    function GetDataByKey (const Key: TAbstractKey;
                           const Index: Integer): TObject; virtual; abstract;
    {GetNodeByKey tries to find the node corresponding to key in the tree and returns
       the node itself, and nil otherwise}
    function GetNodeByKey (const Key: TAbstractKey;
                           const Index: Integer): TAbstractDicTreeNode; virtual; abstract;

    function GetDataByIndex (Index: Integer): TAbstractDataInNode; virtual;
    function GetNodeWithDataByIndex (Index: Integer): TAbstractDicTreeNode; virtual;


    procedure LoadFromStream (ABinaryStream: TMyBinStream); virtual; abstract;
    procedure SaveToStream (ABinaryStream: TMyBinStream); virtual; abstract;

  private
    FParentNode: TAbstractDicTreeNode;
    FIndexInParent: Integer;
    FNoOfNodesInSubtree: Integer;
    FNoOfDataInSubtree: Integer;

  protected
    FDataInNode: TAbstractDataInNode;

  public
    property Size: Integer read GetSize;
    property ChildByIndex [const Index: Integer]: TAbstractDicTreeNode read GetChildByIndex;
    property DataInNode: TAbstractDataInNode read GetDataInNode;
    property ParentNode: TAbstractDicTreeNode read FParentNode;
    property IndexInParent: Integer read FIndexInParent;
    property NodeWithDataByIndex [Index: Integer]: TAbstractDicTreeNode read GetNodeWithDataByIndex;
    property DataByIndex [Index: Integer]: TAbstractDataInNode read GetDataByIndex;

    constructor Create (_ParentNode: TAbstractDicTreeNode;
                 _IndexInParent: Integer);
    destructor Destroy; override;

    function ToXML: AnsiString; virtual;

  end;

  { TNodeList }

  TDicTreeNodeList= class (TList)
  private
    function GetNode (Index: Integer): TAbstractDicTreeNode; inline;
    function GetSize: Integer; inline;

  public
    property Node [Index: Integer]: TAbstractDicTreeNode read GetNode;
    property Size: Integer read GetSize;

    constructor Create (_Size: Integer); overload;
    destructor Destroy; override;

  end;

  { TDicTreeNode }

  TDicTreeNode= class (TAbstractDicTreeNode)
  private
    FChildList: TDicTreeNodeList;

    function GetChildByIndex (const Index: Integer): TAbstractDicTreeNode; override;
    function GetSize: Integer; override;

    function Insert (const Key: TAbstractKey; const Index: Integer;
                      const Data: TAbstractDataInNode; const Overwrite: Boolean):
                      TInsertionResultInformation; override;

    function GetDataByKey (const Key: TAbstractKey;
                           const Index: Integer): TObject; override;
    function GetNodeByKey (const Key: TAbstractKey;
                           const Index: Integer): TAbstractDicTreeNode; override;

    procedure LoadFromStream (ABinaryStream: TMyBinStream); override;
    procedure SaveToStream (ABinaryStream: TMyBinStream); override;

//    constructor Create;

  public

    constructor Create (ChildCount: Integer; _ParentNode: TAbstractDicTreeNode;
                         _IndexInParent: Integer);
    destructor Destroy; override;

  end;

  { TNodeForLargeDomain }

  TNodeForLargeDomain= class (TAbstractDicTreeNode)
  private
    FChildList: TDicTreeNodeList;

    function GetChildByIndex (Index: Integer): TAbstractDicTreeNode;
    function GetSize: Integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Insert (var Data: TIntegerArray; Index: Integer;
      Obj: TObject);

  end;

  { TDictionaryTree }

  TDictionaryTree= class (TObject)
  private
    FRoot: TAbstractDicTreeNode;
    FNoOfNodesInTree: Integer;
    FNoOfDataInTree: Integer;

  protected
    function GetNodeByIndex (const Index: Integer): TAbstractDicTreeNode;
    function GetDataByIndex (const Index: Integer): TObject;
    function GetDataByKey (const Key: TAbstractKey): TObject;
    function GetNodeByKey (const Key: TAbstractKey): TAbstractDicTreeNode;

  public
    property Root: TAbstractDicTreeNode read FRoot;
    property NoOfNodesInTree: Integer read FNoOfNodesInTree;
    property NoOfDataInTree: Integer read FNoOfDataInTree;
    property DataByKey [Key: TAbstractKey]: TObject read GetDataByKey;
    property NodeByKey [Key: TAbstractKey]: TAbstractDicTreeNode read GetNodeByKey;
    property DataByIndex [Index: Integer]: TObject read GetDataByIndex;
    property NodeByIndex [Index: Integer]: TAbstractDicTreeNode read GetNodeByIndex;

    constructor Create;
    destructor Destroy; override;

    function Insert (const Key: TAbstractKey; const Data: TAbstractDataInNode;
                      Overwrite: Boolean): Boolean;

    procedure LoadFromStream (AnStream: TStream);
    procedure SaveToStream (AnStream: TStream);

  end;

  { EInvalidNodeType }

  EInvalidNodeType= class (Exception)
  public
    constructor Create (ANodeType: Integer);

  end;
implementation

{ TNode }

function TDicTreeNode.GetSize: Integer;
begin
  Result:= FChildList.Count;

end;

function TDicTreeNode.GetChildByIndex (const Index: Integer): TAbstractDicTreeNode;
begin
  Result:= FChildList.Node [Index];

end;

constructor TDicTreeNode.Create (ChildCount: Integer;
  _ParentNode: TAbstractDicTreeNode; _IndexInParent: Integer);
begin
  inherited Create (_ParentNode, _IndexInParent);

  FChildList:= TDicTreeNodeList.Create (ChildCount);

end;

destructor TDicTreeNode.Destroy;
begin
  FChildList.Free;

  inherited;

end;

function TDicTreeNode.Insert (const Key: TAbstractKey; const Index: Integer;
                        const Data: TAbstractDataInNode; const OverWrite: Boolean):
                         TInsertionResultInformation;
var
  NextNode: TAbstractDicTreeNode;
  ActiveValueInKey: Integer;

begin
  if Key.Length= Index then
  begin
    Result.NoOfNewNode:= 0;

    if FDataInNode= nil then
    begin
      FDataInNode:= Data;

      Result.DataHasBeenOverwritten:= False;
      Result.DataHasBeenSaved:= True;

    end
    else if OverWrite then
    begin
      FDataInNode:= Data;

      Result.DataHasBeenOverwritten:= True;
      Result.DataHasBeenSaved:= True;

    end
    else
      Result.DataHasBeenSaved:= False;

    if Result.DataHasBeenSaved and not Result.DataHasBeenOverwritten then
      Inc (FNoOfDataInSubtree);

    Exit;

  end;

  ActiveValueInKey:= Key.ValueAt [Index];
  ActiveValueInKey:= ActiveValueInKey- Key.PossibleMinValue;

  NextNode:= ChildByIndex [ActiveValueInKey];
  if NextNode= nil then
  begin
    NextNode:= TDicTreeNode.Create (Key.PossibleMaxValue- Key.PossibleMinValue+ 1,
             Self, ActiveValueInKey);
    FChildList [ActiveValueInKey]:= NextNode;
    Result:= NextNode.Insert (Key, Index+ 1, Data, OverWrite);
    Inc (Result.NoOfNewNode);

  end
  else
    Result:= NextNode.Insert (Key, Index+ 1, Data, OverWrite);

  if Result.DataHasBeenSaved and not Result.DataHasBeenOverwritten then
    Inc (FNoOfDataInSubtree);
  Inc (FNoOfNodesInSubtree, Result.NoOfNewNode);

end;

function TDicTreeNode.GetDataByKey (const Key: TAbstractKey; const Index: Integer
  ): TObject;
var
  NextNode: TAbstractDicTreeNode;
  ActiveValueInKey: Integer;

begin
  if Key.Length= Index then
  begin
    Result:= GetDataInNode;

    Exit;

  end;

  ActiveValueInKey:= Key.ValueAt [Index];
  ActiveValueInKey:= ActiveValueInKey- Key.PossibleMinValue;

  NextNode:= ChildByIndex [ActiveValueInKey];
  if NextNode= nil then
    Result:= nil
  else
    Result:= NextNode.GetDataByKey (Key, Index+ 1);

end;

function TDicTreeNode.GetNodeByKey(const Key: TAbstractKey; const Index: Integer
  ): TAbstractDicTreeNode;
var
  NextNode: TAbstractDicTreeNode;
  ActiveValueInKey: Integer;

begin
  if Key.Length= Index then
  begin
    Result:= Self;
    Exit;

  end;

  ActiveValueInKey:= Key.ValueAt [Index];
  ActiveValueInKey:= ActiveValueInKey- Key.PossibleMinValue;

  NextNode:= ChildByIndex [ActiveValueInKey];
  if NextNode= nil then
    Result:= nil
  else
    Result:= NextNode.GetNodeByKey (Key, Index+ 1);

end;

procedure TDicTreeNode.LoadFromStream (ABinaryStream: TMyBinStream);
begin

end;

procedure TDicTreeNode.SaveToStream (ABinaryStream: TMyBinStream);
begin
  ABinaryStream.WriteChar (#1);
  FDataInNode.SaveToStream (ABinaryStream);
  ABinaryStream.WriteInt (Size);

end;

{ TDictionaryTree }

constructor TDictionaryTree.Create;
begin
  inherited Create;

  FRoot:= nil;
  FNoOfDataInTree:= 0;
  FNoOfNodesInTree:= 0;

end;

destructor TDictionaryTree.Destroy;
begin
  FRoot.Free;

  inherited Destroy;

end;

function TDictionaryTree.Insert (const Key: TAbstractKey; const Data: TAbstractDataInNode;
                      Overwrite: Boolean): Boolean;
var
  InsertionResult: TInsertionResultInformation;

begin
  if Root= nil then
    FRoot:= TDicTreeNode.Create (Key.GetMaxValue- Key.GetMinValue+ 1, nil, -1);

  InsertionResult:= Root.Insert (Key, 0, Data, Overwrite);
  Inc (FNoOfNodesInTree, InsertionResult.NoOfNewNode);

  if InsertionResult.DataHasBeenSaved then
    Inc (FNoOfDataInTree);

  Result:= InsertionResult.DataHasBeenSaved;

end;

function TDictionaryTree.GetDataByKey (const Key: TAbstractKey): TObject;
begin
  if Root= nil then
    Result:= nil
  else
    Result:= Root.GetDataByKey (Key, 0);

end;

function TDictionaryTree.GetNodeByKey (const Key: TAbstractKey): TAbstractDicTreeNode;
begin
  if Root= nil then
    Result:= nil
  else
    Result:= Root.GetNodeByKey (Key, 0);

end;

function TDictionaryTree.GetNodeByIndex (const Index: Integer): TAbstractDicTreeNode;
begin
  if Root= nil then
    Result:= nil
  else
    Result:= Root.NodeWithDataByIndex [Index];

end;

function TDictionaryTree.GetDataByIndex (const Index: Integer): TObject;
begin
  if Root= nil then
    Result:= nil
  else
    Result:= Root.DataByIndex [Index];

end;

procedure TDictionaryTree.LoadFromStream (AnStream: TStream);
var
  RootNodeType: Char;
  MyStream: TMyBinStream;

begin

end;

procedure TDictionaryTree.SaveToStream (AnStream: TStream);
var
  BinaryStream: TMyBinStream;

begin
  BinaryStream:= TMyBinStream.Create (AnStream);

  if Root<> nil then
    Root.SaveToStream (BinaryStream);

  BinaryStream.Free;
end;

{ TNodeForLargeDomain }

function TNodeForLargeDomain.GetChildByIndex (Index: Integer): TAbstractDicTreeNode;
begin

end;

function TNodeForLargeDomain.GetSize: Integer;
begin

end;

constructor TNodeForLargeDomain.Create;
begin
  FChildList:= TDicTreeNodeList.Create;
  raise Exception.Create ('Not Implemented Yet!');

end;

destructor TNodeForLargeDomain.Destroy;
var
  i: Integer;

begin
  FChildList.Free;

  inherited;

end;

procedure TNodeForLargeDomain.Insert(var Data: TIntegerArray; Index: Integer;
  Obj: TObject);
begin

end;

{ TNodeList }

function TDicTreeNodeList.GetNode (Index: Integer): TAbstractDicTreeNode;
begin
  Result:= TObject (Items [Index]) as TAbstractDicTreeNode;

end;

function TDicTreeNodeList.GetSize: Integer;
begin
  Result:= Count;

end;

destructor TDicTreeNodeList.Destroy;
var
  i: Integer;

begin
  for i:= 0 to Count- 1 do
    Node [i].Free;

  inherited Destroy;

end;

constructor TDicTreeNodeList.Create (_Size: Integer);
begin
  inherited Create;

  SetCount (_Size);

end;


{ TAbstractDicTreeNode }

function TAbstractDicTreeNode.GetDataInNode: TAbstractDataInNode;
begin
  Result:= FDataInNode;

end;

function TAbstractDicTreeNode.GetDataByIndex (Index: Integer): TAbstractDataInNode;
begin
  Result:= NodeWithDataByIndex [Index].DataInNode;

end;

function TAbstractDicTreeNode.GetNodeWithDataByIndex (Index: Integer): TAbstractDicTreeNode;
var
  NextNode: TAbstractDicTreeNode;
  i: Integer;

begin
  if (Index= 1) and (DataInNode<> nil) then
    Exit (Self);


  if DataInNode<> nil then
    Dec (Index);

  NextNode:= nil;
  for i:= 0 to Size- 1 do
    if ChildByIndex [i]<> nil then
    begin

      if ChildByIndex [i].FNoOfDataInSubtree< Index then
        Dec (Index, ChildByIndex [i].FNoOfDataInSubtree)
      else
      begin
        NextNode:= ChildByIndex [i];
        Result:= NextNode.NodeWithDataByIndex [Index];
        Exit;

      end;

    end;

  raise Exception.Create ('There should be some problems in impementation: GetDataByIndex');

end;

constructor TAbstractDicTreeNode.Create (_ParentNode: TAbstractDicTreeNode;
  _IndexInParent: Integer);
begin
  inherited Create;

  FDataInNode:= nil;
  FParentNode:= _ParentNode;
  FIndexInParent:= _IndexInParent;

end;

destructor TAbstractDicTreeNode.Destroy;
begin
  FDataInNode.Free;

  inherited Destroy;
end;

function TAbstractDicTreeNode.ToXML: AnsiString;
var
  i: Integer;

begin
  Result:= '<AbstractNode Size= "'+ IntToStr (Size)+ '" >';
  for i:= 0 to Size- 1 do
    if GetChildByIndex (i)<> nil then
      Result+= GetChildByIndex (i).ToXML;

  if DataInNode<> nil then
    Result+= '<Data/>';

  Result+= '</AbstractNode>';

end;

{ EInvalidNodeType }

constructor EInvalidNodeType.Create (ANodeType: Integer);
begin
  inherited Create ('Invalid node type ='+ IntToStr (ANodeType));

end;

end.

