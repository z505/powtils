unit AutomataUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
type

  { TAutomataState }

  TAutomataState= class (TObject)
  private
    FName: String;
    FNextStates: array [#0..#255] of TAutomataState;

    function GetNextState(Ch: Char): TAutomataState;
    
  public
    property Name: String read FName;
    property NextState [Ch: Char]: TAutomataState read GetNextState;
    
    constructor Create (StateName: String); overload;
    constructor Create (AnStream: TStream); overload;
    destructor Destroy; override;
    
  end;

implementation
uses
  StreamUnit;
  
type

{
  Grammar:
    The input should be in prefix notation:
      (= String) ==> The String should be visited.
      (# String1 String2) ==> The String2 should be visited after String1.
      (/ String1,String2,...,Stringn) ==> One of the String1, String2 or ... Stringn should be visited.
      (* String) ==> Any count of occurence of String.
      (n String) ==> n occurence of String.
      (+ String) ==> (= String)(* String)
      ('String') ==> A usual String
      (String) ==> Any of above.
      
}
  { TAbstractParseTreeNode }

  TAbstractParseTreeNode= class (TObject)
  private
    FChildren: array of TAbstractParseTreeNode;

  protected
    function GetChild (Index: Integer): TAbstractParseTreeNode;
    function GetSize: Integer;
    procedure SetChild (ANode: TAbstractParseTreeNode; Index: Integer);
    
  public
    property Child [Index: Integer]: TAbstractParseTreeNode read GetChild;
    property Size: Integer read GetSize;
    
    constructor Create (ChildSize: Integer);
    destructor Destroy; override;

    function ToString: String; virtual; abstract;
    function CreateDFA: TAutomataState;  virtual; abstract;
    
  end;
  
  TNodeArray= array of TAbstractParseTreeNode;
  
  { TConcatNode }

  TConcatNode= class (TAbstractParseTreeNode)
  public
    constructor Create (Child1, Child2: TAbstractParseTreeNode);
    destructor Destroy; override;
    
    function CreateDFA: TAutomataState;
    
  end;
  
  { TMatchAnyNode }

  TMatchAnyNode= class (TAbstractParseTreeNode)
  private
  public
    property ChildCount: Integer read GetSize;

    constructor Create (NodeArray: TNodeArray);
    destructor Destroy; override;

  end;
  
  { TStarNode }

  TStarNode= class (TAbstractParseTreeNode)
  protected
    function GetChild: TAbstractParseTreeNode;

  public
    property Child: TAbstractParseTreeNode read GetChild;
    
    constructor Create (ANode: TAbstractParseTreeNode);
    destructor Destroy; override;
    
  end;
  
  { TPlusNode }

  TPlusNode= class (TAbstractParseTreeNode)
  protected
    function GetChild: TAbstractParseTreeNode;

  public
    property Child: TAbstractParseTreeNode read GetChild;

    constructor Create (ANode: TAbstractParseTreeNode);
    destructor Destroy; override;

  end;
  
  { TCountNode }

  TCountNode= class (TAbstractParseTreeNode)
  private
    FCount: Integer;
    
  public
    property Count: Integer read FCount;
    
    constructor Create (n: Integer; ANode: TAbstractParseTreeNode);
    destructor Destroy; override;

  end;
  
  { TStringNode }

  TStringNode= class (TAbstractParseTreeNode)
  private
    FText: String;
    
  public
    property Text: String read FText;
    
    constructor Create (Str: String);
    destructor Destroy; override;
    
  end;
  
  { TEqualNode }

  TEqualNode= class (TAbstractParseTreeNode)
  private
    function GetChild: TAbstractParseTreeNode;
    
  public
    property Child: TAbstractParseTreeNode read GetChild;
    
    constructor Create (Anode: TAbstractParseTreeNode);
    destructor Destroy; override;
    
  end;

  { TPatternParser }

  TPatternParser= class (TObject)
  private
    FStream: TStream;

    function GetNextChar: Char;
    function GetNextToken: String;
    
  public
    constructor Create (AnStream: TStream);
    destructor Destroy; override;
    
    function Parse: TAbstractParseTreeNode;
  
  end;

  { EInvalidCharOrToken }

  EInvalidCharOrToken= class (Exception)
  private
    InvalidToken: String;
    
  public
    constructor Create (AChar: Char); overload;
    constructor Create (AToken: String); overload;

  end;

{ TConcatNode }

constructor TConcatNode.Create (Child1, Child2: TAbstractParseTreeNode);
begin
  inherited Create (2);
  
  SetChild (Child1, 0);
  SetChild (Child2, 1);

end;

destructor TConcatNode.Destroy;
begin
  inherited Destroy;
end;

function TConcatNode.CreateDFA: TAutomataState;
begin

end;

{ TMatchAnyNode }

constructor TMatchAnyNode.Create (NodeArray: TNodeArray);
var
  i: Integer;
  
begin
  inherited Create (Length (NodeArray));
  
  for i:= 0 to High (NodeArray) do
    SetChild (NodeArray [i], i);
    
end;

destructor TMatchAnyNode.Destroy;
begin
  inherited Destroy;
end;

{ TEqualNode }

function TEqualNode.GetChild: TAbstractParseTreeNode;
begin
  Result:= inherited GetChild (0);
  
end;

constructor TEqualNode.Create (ANode: TAbstractParseTreeNode);
begin
  inherited Create (1);
  
  SetChild (ANode, 0);

end;

destructor TEqualNode.Destroy;
begin
  inherited Destroy;
end;

{ EInvalidCharOrToken }

constructor EInvalidCharOrToken.Create (AChar: Char);
begin
  inherited Create ('Invalid Char: '+ AChar);

end;

constructor EInvalidCharOrToken.Create (AToken: String);
begin
  inherited Create ('Invalid Token: '+ AToken);

  InvalidToken:= AToken;
  
end;

{ TStringNode }

constructor TStringNode.Create (Str: String);
begin
  inherited Create (0);
  
  FText:= Str;
  
end;

destructor TStringNode.Destroy;
begin
  inherited Destroy;
end;

{ TCountNode }

constructor TCountNode.Create (n: Integer; ANode: TAbstractParseTreeNode);
begin
  inherited Create (1);
  
  FCount:= n;
  SetChild (ANode, 0);
  
end;

destructor TCountNode.Destroy;
begin
  inherited Destroy;
end;

{ TPlusNode }

function TPlusNode.GetChild: TAbstractParseTreeNode;
begin
  Result:= inherited GetChild (0);
  
end;

constructor TPlusNode.Create (ANode: TAbstractParseTreeNode);
begin
  inherited Create (1);
  
  SetChild (ANode, 0);
  
end;

destructor TPlusNode.Destroy;
begin
  inherited Destroy;
  
end;

function TAbstractParseTreeNode.GetChild (Index: Integer): TAbstractParseTreeNode;
begin
  Result:= FChildren [Index];
  
end;

function TAbstractParseTreeNode.GetSize: Integer;
begin
  Result:= Length (FChildren);
  
end;

procedure TAbstractParseTreeNode.SetChild (ANode: TAbstractParseTreeNode; Index: Integer);
begin
  FChildren [Index]:= ANode;
  
end;

constructor TAbstractParseTreeNode.Create (ChildSize: Integer);
begin
  inherited Create;
  
  SetLength (FChildren, ChildSize);

end;

destructor TAbstractParseTreeNode.Destroy;
var
  i: Integer;
  
begin
  for i:= 0 to Size- 1 do
    FChildren [i].Free;
    
  SetLength (FChildren, 0);
    
  inherited Destroy;
  
end;

{ TStarNode }

function TStarNode.GetChild: TAbstractParseTreeNode;
begin
  Result:= inherited GetChild (0);
  
end;

constructor TStarNode.Create (ANode: TAbstractParseTreeNode);
begin
  inherited Create (1);
  
  SetChild (ANode, 0);

end;

destructor TStarNode.Destroy;
begin
  inherited Destroy;
  
end;

{ TPatternParser }

function TPatternParser.GetNextChar: Char;
const
  SkipChars: set of char= [#10, #13, ' ', #7];
  
begin
  Result:= ReadCharFromStream (FStream);
  
  while Result in SkipChars do
    Result:= ReadCharFromStream (FStream);

end;

function TPatternParser.GetNextToken: String;
var
  Ch: Char;
  
begin
  Ch:= GetNextChar;

  if Ch in ['(', ')', '/', '*', '+', '=', '#'] then
    Result:= Ch
  else if Ch= '''' then
  begin
    Result:= Ch;
    
    Ch:= GetNextChar;
    while Ch<> '''' do
    begin
      Result:= Result+ Ch;
      Ch:= GetNextChar;
      
    end;
    
  end
  else if Ch in ['0'..'9'] then
  begin
    Result:= Ch;

    Ch:= GetNextChar;
    while Ch in ['0'..'9'] do
    begin
      Result:= Result+ Ch;
      Ch:= GetNextChar;

    end;
    
    FStream.Position:= FStream.Position- 1;

  end
  else
    raise EInvalidCharOrToken.Create (Ch);
    
end;

constructor TPatternParser.Create (AnStream: TStream);
begin
  inherited Create;
  
  FStream:= AnStream;
  
end;

destructor TPatternParser.Destroy;
begin
  inherited Destroy;
  
end;

function TPatternParser.Parse: TAbstractParseTreeNode;
var
  Token: String;
  AChild: TAbstractParseTreeNode;
  Children: TNodeArray;
  
begin
  Token:= GetNextToken;
  if Token<> '(' then
    raise EInvalidCharOrToken.Create (Token);
    
  Token:= GetNextToken;
  
  case Token [1] of
    '=':
    begin
      AChild:= Self.Parse;
      Result:= TEqualNode.Create (AChild);
      Token:= GetNextToken;
      if Token<> ')' then
        raise EInvalidCharOrToken.Create (Token);

    end;

    '+':
    begin
      AChild:= Self.Parse;
      Result:= TPlusNode.Create (AChild);
      Token:= GetNextToken;
      if Token<> ')' then
        raise EInvalidCharOrToken.Create (Token);

    end;

    '*':
    begin
      AChild:= Self.Parse;
      Result:= TStarNode.Create (AChild);
      Token:= GetNextToken;
      if Token<> ')' then
        raise EInvalidCharOrToken.Create (Token);

    end;

    '''':
    begin
      Result:= TStringNode.Create (Copy (Token, 2, Length (Token)- 1));
      Token:= GetNextToken;
      if Token<> ')' then
        raise EInvalidCharOrToken.Create (Token);

    end;

    '#':
    begin
      SetLength (Children, 2);
      Children [0]:= Self.Parse;
      Children [1]:= Self.Parse;
      Result:= TConcatNode.Create (Children [0], Children [1]);
      
      SetLength (Children, 0);
      Token:= GetNextToken;
      if Token<> ')' then
        raise EInvalidCharOrToken.Create (Token);

    end;

    '/':
    begin
      SetLength (Children, 0);
      
      while True do
        try
          SetLength (Children, Length (Children)+ 1);
          Children [High (Children)]:= Self.Parse;

        except
          on e: EInvalidCharOrToken do
            if e.InvalidToken= ')' then
              Break
            else
              raise e;
              
        end;

      SetLength (Children, Length (Children)- 1);
      Result:= TMatchAnyNode.Create (Children);
      SetLength (Children, 0);

    end;

    else
    begin
      try
        AChild:= Self.Parse;
        Result:= TCountNode.Create (StrToInt (Token), AChild);
        Token:= GetNextToken;
        if Token<> ')' then
          raise EInvalidCharOrToken.Create (Token);

      except
        on e: EConvertError do
      end;

    end;

  end;
  
end;
  
{ TAutomataState }

function TAutomataState.GetNextState (Ch: Char): TAutomataState;
begin
  Result:= FNextStates [Ch];
  
end;

constructor TAutomataState.Create (StateName: String);
begin
  inherited Create;
  
  FName:= StateName;

end;

constructor TAutomataState.Create (AnStream: TStream);
var
  Parser: TPatternParser;
  Node: TAbstractParseTreeNode;
  
begin
  Parser:= TPatternParser.Create (AnStream);
  Node:= Parser.Parse;
  Node.CreateDFA;
  Parser.Free;
  Node.Free;
  
end;

destructor TAutomataState.Destroy;
begin
  inherited Destroy;
  
end;

end.

