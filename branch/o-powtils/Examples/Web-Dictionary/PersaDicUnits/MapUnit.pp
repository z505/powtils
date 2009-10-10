unit MapUnit; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMap }

  generic TMap<T>= class (TObject)
  private
    FCount: Integer;

  type private
    PMapNode= ^TMapNode;

    TMapNode= record
      RightChild: PMapNode;
      LeftChild: PMapNode;
      Count: Integer;
      Data: T;

    end;

    TIsGreaterThanFunction= function (const a: T; const b: T): Boolean;

  var private
    FIsGreaterThanFunction: TIsGreaterThanFunction;
    FRoot: PMapNode;

  public
    property Count: Integer read FCount;

    constructor Create (IsGreaterThanFunction: TIsGreaterThanFunction);
    destructor Destroy; override;

    procedure Insert (NewData: T);
    function IsExists (Data: T): Boolean;
  private
    function NewMapNode (d: T): PMapNode;
    procedure DeleteAllMapNode (ANode: PMapNode);

  end;

implementation

{ TMap }

constructor TMap.Create (IsGreaterThanFunction: TIsGreaterThanFunction);
begin
  inherited Create;

  FIsGreaterThanFunction:= IsGreaterThanFunction;
  FCount:= 0;

end;


destructor TMap.Destroy;
begin
  DeleteAllMapNode (FRoot);

  inherited;

end;

procedure TMap.Insert (NewData: T);
var
  ActiveNode, PrevNode: PMapNode;

begin
  if FRoot= nil then
    FRoot:= NewMapNode (NewData)
  else
  begin
    PrevNode:= nil;
    ActiveNode:= FRoot;

    while ActiveNode<> nil do
    begin
      if FIsGreaterThanFunction (ActiveNode^.Data, NewData) then
      begin
        PrevNode:= ActiveNode;
        ActiveNode:= ActiveNode^.RightChild;

      end
      else if FIsGreaterThanFunction (NewData, ActiveNode^.Data) then
      begin
        PrevNode:= ActiveNode;
        ActiveNode:= ActiveNode^.LeftChild;

      end
      else
      begin
        Inc (ActiveNode^.Count);
        Exit;

      end;

    end;

    if FIsGreaterThanFunction (PrevNode^.Data, NewData) then
      PrevNode^.RightChild:= NewMapNode (NewData)
    else
      PrevNode^.LeftChild:= NewMapNode (NewData)

  end;

end;

function TMap.IsExists (Data: T): Boolean;
var
  ActiveNode: PMapNode;
  PrevNode: PMapNode;

begin
  if FRoot= nil then
    Exit (False)
  else
  begin
    PrevNode:= nil;
    ActiveNode:= FRoot;

    while ActiveNode<> nil do
    begin
      if FIsGreaterThanFunction (ActiveNode^.Data, Data) then
      begin
        PrevNode:= ActiveNode;
        ActiveNode:= ActiveNode^.RightChild;

      end
      else if FIsGreaterThanFunction (Data, ActiveNode^.Data) then
      begin
        PrevNode:= ActiveNode;
        ActiveNode:= ActiveNode^.LeftChild;

      end
      else
        Exit (0< ActiveNode^.Count);

    end;

    if FIsGreaterThanFunction (PrevNode^.Data, Data) then
      Exit (False);

    if FIsGreaterThanFunction (Data, PrevNode^.Data) then
      Exit (False);

    Result:= True;

  end;

end;

function TMap.NewMapNode (d: T): PMapNode;
begin
  New (Result);
  Result^.LeftChild:= nil;
  Result^.RightChild:= nil;
  Result^.Data:= d;
  Result^.Count:= 1;

end;

procedure TMap.DeleteAllMapNode (ANode: PMapNode);
begin
  if ANode<> nil then
  begin
    DeleteAllMapNode (ANode^.LeftChild);
    DeleteAllMapNode (ANode^.RightChild);
    Dispose (ANode);

  end;
end;

{
type
  TRec= record
    d: Integer;
  end;

type
  TMapRec= specialize TMap<TRec>;

function Compare (const a, b: TRec): Boolean;
begin
end;

var
 Map: TMapRec;

initialization
  Map:= TMapRec.Create (@Compare);
//  Map.IsExists (r);
}
end.

