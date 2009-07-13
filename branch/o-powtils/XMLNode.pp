unit XMLNode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit, AttributeUnit;

type
  ENodeNotFound= class (Exception);
  
  TXMLNodeCollection= class;

  { TXMLNode }

  TXMLNode= class
  private
    FAttributes: TAttributeCollection;
    FChilds: TXMLNodeCollection;
    FContent: String;
    FParent: TXMLNode;
    FTag: String;
    function GetChild (Index: Integer): TXMLNode;
    function GetNodeByPath (Path: String): TXMLNode;

  public
    property Parent: TXMLNode read FParent;
    property Child [Index: Integer]: TXMLNode read GetChild;
    property Childs: TXMLNodeCollection read FChilds;
    property Tag: String read FTag write FTag;
    property Attributes: TAttributeCollection read FAttributes;
    property NodeByPath [Path: String]: TXMLNode read GetNodeByPath;
    
    constructor Create;
    constructor Create (ParentNode: TXMLNode; TagName: String);
    constructor Create (TagName: String);
    destructor Destroy; override;
    
    procedure AddChild (NewNode: TXMLNode);
    function ToStringWithIndent (Space: String= ''): String;
    function ToStringWithOutIndent: String;
    procedure AddAttribute (NewAttribute: TAttribute);
    procedure AddAttribute (Name, Value: String);

    procedure SaveToFile (var OutputFile: TextFile);
    
  end;
  
  PXMLNode= ^TXMLNode;
  
  { TXMLNodeCollection }

  TXMLNodeCollection= class (TBaseCollection)
  private
    function GetNode (Index: Integer): TXMLNode; overload;
    function GetNode (ChildName: String): TXMLNode; overload;
  
  public
    property Node [Index: Integer]: TXMLNode read GetNode;
    property NodeByName [ChildName: String]: TXMLNode read GetNode;

    constructor Create;
    
    procedure Merge (AnotherCollection: TXMLNodeCollection);
    procedure AddNode (NewNode: TXMLNode);
    
  end;
  

implementation

{ TXMLNode }

function TXMLNode.GetChild (Index: Integer): TXMLNode;
begin
  Result:= FChilds.Node [Index];
  
end;

function TXMLNode.GetNodeByPath (Path: String): TXMLNode;
var
  S: String;
  
begin
  S:= Copy (Path, 1, Pos ('/', Path)- 1);
  Delete (Path, 1, Pos ('/', Path));
  
  if UpperCase (S)<> UpperCase (FTag) then
    raise ENodeNotFound.Create ('');

  if S= '' then
    Result:= Self
  else
  begin
    S:= Copy (Path, 1, Pos ('/', Path)- 1);
    Delete (Path, 1, Pos ('/', Path));
    Result:= FChilds.NodeByName [S].NodeByPath [Path];
    
  end;
  
end;

constructor TXMLNode.Create;
begin
  inherited Create;
  
  FTag:= '';
  FContent:= '';
  FAttributes:= TAttributeCollection.Create;
  FChilds:= TXMLNodeCollection.Create;

end;

constructor TXMLNode.Create (ParentNode: TXMLNode; TagName: String);
begin
  inherited Create;
  
  FTag:= TagName;
  FParent:= ParentNode;
  FContent:= '';
  FAttributes:= TAttributeCollection.Create;
  FChilds:= TXMLNodeCollection.Create;
  FParent.AddChild (Self);

end;

constructor TXMLNode.Create (TagName: String);
begin
  inherited Create;

  FTag:= TagName;
  FParent:= nil;
  FContent:= '';
  FAttributes:= TAttributeCollection.Create;
  FChilds:= TXMLNodeCollection.Create;

end;

destructor TXMLNode.Destroy;
begin
  FChilds.Free;
  FAttributes.Free;
  
  inherited;
  
end;

procedure TXMLNode.AddChild (NewNode: TXMLNode);
begin
  FChilds.AddNode (NewNode);
  
end;

function TXMLNode.ToStringWithIndent(Space: String): String;
var
  i: Integer;

begin
  Result:= Space+ '<'+ FTag+ ' '+ FAttributes.ToString;

  if FChilds.Size= 0 then
    Result:= Result+ '/>'
  else
  begin
    Result:= Result+ '>';
    for i:= 0 to FChilds.Size- 1 do
      Result:= Result+ #10+ Child [i].ToStringWithIndent (Space+ '  ');
    Result:= Result+ #10+ '</'+ FTag+ '>';
      
  end;

end;

function TXMLNode.ToStringWithOutIndent: String;
var
  i: Integer;

begin

  Result:= '<'+ FTag+ ' '+ FAttributes.ToString;

  if FChilds.Size= 0 then
    Result:= Result+ '/>'
  else
  begin
    Result:= Result+ '>';
    for i:= 0 to FChilds.Size- 1 do
      Result:= Result+ #10+ Child [i].ToStringWithOutIndent;
      
    Result:= Result+ #10'</'+ FTag+ '>';
    
  end;
      
end;

procedure TXMLNode.AddAttribute (NewAttribute: TAttribute);
begin
  FAttributes.Add (NewAttribute);
  
end;

procedure TXMLNode.AddAttribute (Name, Value: String);
begin
  FAttributes.Add (TAttribute.Create (Name, Value));
  
end;

procedure TXMLNode.SaveToFile (var OutputFile: TextFile);
begin
  WriteLn (OutputFile, Self.ToStringWithIndent (''));
  
end;

{ TXMLNodeCollection }

function TXMLNodeCollection.GetNode (Index: Integer): TXMLNode;
begin
  Result:= Member [Index] as TXMLNode;
  
end;

function TXMLNodeCollection.GetNode (ChildName: String): TXMLNode;
var
  i: Integer;
  
begin
  Result:= nil;
  
  ChildName:= UpperCase (ChildName);
  
  for i:= 0 to Size- 1 do
    if UpperCase (Node [i].Tag)= ChildName then
    begin
      Result:= Node [i];
      Exit;
      
    end;
  Result:= nil;
  
  raise ENodeNotFound.Create (ChildName);
    
end;

constructor TXMLNodeCollection.Create;
begin
  inherited Create;
  
end;

procedure TXMLNodeCollection.Merge (AnotherCollection: TXMLNodeCollection);
var
  i: Integer;
  Ptr: PXMLNode;
  
begin
  Ptr:= @FMembers [0];
  
  for i:= 0 to AnotherCollection.Size- 1 do
  begin
    Self.AddNode (Ptr^);
    Inc (Ptr);
    
  end;
  
end;

procedure TXMLNodeCollection.AddNode (NewNode: TXMLNode);
begin
  inherited Add (NewNode);
  
end;

end.

