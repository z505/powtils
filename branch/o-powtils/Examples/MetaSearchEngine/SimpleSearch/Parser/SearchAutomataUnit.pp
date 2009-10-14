unit SearchAutomataUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TState }

  TState= class (TObject)
  private
    FNextState: array [#0..#255] of TState;
    FName: String;
    FID: Integer;

    function GetNextState (Ch: Char): TState; inline;
    procedure SetNextState (Ch: Char; AState: TState); inline;

  public
    property Name: String read FName;
    Property Next [Ch: Char]: TState read GetNextState;
    property ID: Integer read FID;

    constructor Create (StateStr: String; AnID: Integer);
    destructor Destroy; override;

  end;

  { TStateCollection }

  TStateCollection= class (TStringList)
  private
    procedure AddNewState (NewState: TState; Name: String);
    function GetState (Name: String): TState;
    function GetStateByIndex (Index: Integer): TState;

  public
    property State [Name: String]: TState read GetState;
    property StateByIndex [Index: Integer]: TState read GetStateByIndex;

    function GetStateOrCreateOne (AName: String): TState;

  end;

  TFindResultInfo= record
    StartIndex, FinIndex: Integer;
    StartPtr, FinPtr: PChar;

  end;

  TCharSet= set of char;

  { TAutomata }

  TAutomata= class (TObject)
  private
    LastCharsList: TList;
    StateCollection: TStateCollection;
    SpaceCharSet: TCharSet;
    PatternLen: Integer;

  public
    constructor Create (SearchPattern: String; IgnoreChars: TCharSet);
    destructor Destroy; override;

    function Find (PStr: PChar; Len: Integer; var PositionInfo: TFindResultInfo): Boolean;

    function ToString: String;

  end;

implementation

type

  { EAnStateWithSameNameExists }

  EAStateWithSameNameExists= class (Exception)
  public
    constructor Create (Name: String);

  end;

  { EStateNotFound }

  EStateNotFound= class (Exception)
  public
    constructor Create (Name: String);

  end;

{ EStateNotFound }

constructor EStateNotFound.Create (Name: String);
begin
  inherited Create ('There is no state with name= "'+ Name+ '" in the collection!');

end;

{ EAnStateWithSameNameExists }

constructor EAStateWithSameNameExists.Create (Name: String);
begin
  inherited Create ('There is a state with name= "'+ Name+ '" in the collection!');

end;

{ TState }

function TState.GetNextState (Ch: Char): TState;
begin
  Result:= FNextState [Ch];

end;

procedure TState.SetNextState (Ch: Char; AState: TState); inline;
begin
  FNextState [Ch]:= AState;

end;

constructor TState.Create (StateStr: String; AnID: Integer);
var
  C: Char;

begin
  inherited Create;

  FID:= AnID;;
  FName:= StateStr;
  for c:= #0 to #$FF do
    FNextState [c]:= nil;

end;

destructor TState.Destroy;
begin

  inherited Destroy;

end;

{ TAutomata }

constructor TAutomata.Create (SearchPattern: String; IgnoreChars: TCharSet);
var
  i, j: Integer;
  Ch: Char;
  NewPattern: String;
  ActiveStateName,
  NextStateName,
  NextStateNameSubString: String;
  ActiveState, NextState: TState;

begin
  inherited Create;

  SpaceCharSet:= IgnoreChars;
  NewPattern:= '';
  for i:= 1 to Length (SearchPattern) do
    if not (SearchPattern [i] in SpaceCharSet) then
    begin
      NewPattern+= SearchPattern [i];

    end;

 if SearchPattern<> NewPattern then
    WriteLn ('Search Pattern has been modified from "'+ SearchPattern+ '" to "'+ NewPattern+ '"');

  SearchPattern:= NewPattern;

  StateCollection:= TStateCollection.Create;
  LastCharsList:= TList.Create;
  PatternLen:= Length (SearchPattern);

  for i:= 1 to Length (SearchPattern) do
    LastCharsList.Add (nil);

  for i:= 0 to Length (SearchPattern) do
  begin
    ActiveStateName:= Copy (SearchPattern, 1, i);
    ActiveState:= StateCollection.GetStateOrCreateOne (ActiveStateName);

  end;

  StateCollection.Sort;

  for i:= 0 to StateCollection.Count- 1 do
  begin
    ActiveState:= StateCollection.StateByIndex [i];
    ActiveStateName:= ActiveState.Name;

    for Ch:= #0 to #255 do
    begin
      NextStateName:= ActiveStateName+ Ch;

      for j:= Length (NextStateName) downto 0 do
      begin
        NextStateNameSubString:= Copy (NextStateName, Length (NextStateName)- j+ 1, j);

        NextState:= StateCollection.State [NextStateNameSubString];
        if NextState<> nil then
        begin
          ActiveState.SetNextState (Ch, NextState);
          Break;

        end;

      end;

    end;

  end;

end;

destructor TAutomata.Destroy;
var
  i: Integer;

begin
  for i:= 0 to StateCollection.Count- 1 do
    StateCollection.Objects [i].Free;
  StateCollection.Free;
  LastCharsList.Free;

  inherited Destroy;

end;

function TAutomata.Find (PStr: PChar; Len: Integer;
            var PositionInfo: TFindResultInfo): Boolean;
var
  ListIndex: Integer;

  procedure AddToList (CharPtr: PChar);
  begin
    LastCharsList.Items [ListIndex]:= CharPtr;
    ListIndex:= (ListIndex+ 1) mod PatternLen;

  end;

  procedure CreateResult (var PositionInfo:TFindResultInfo);
  var
    FirstIndex, LastIndex: Integer;

  begin
    LastIndex:= (ListIndex+ PatternLen- 1) mod PatternLen;
    FirstIndex:= (LastIndex+ 1) mod PatternLen;

    PositionInfo.StartPtr:= LastCharsList.Items [FirstIndex];
    PositionInfo.FinPtr:= LastCharsList.Items [LastIndex];
    PositionInfo.StartIndex:= LastCharsList.Items [FirstIndex]- PStr;
    PositionInfo.FinIndex:= LastCharsList.Items [LastIndex]- PStr;

  end;

var
  PCh: PChar;
  Ch: Char;
  i: Integer;
  ActiveState,
  NextState,
  FinalState: TState;

begin
  ActiveState:= StateCollection.StateByIndex [0];
  FinalState:= StateCollection.StateByIndex [StateCollection.Count- 1];

  ListIndex:= 0;

  PCh:= PStr;
  for i:= 1 to Len do
  begin
    if not (PCh^ in SpaceCharSet) then
    begin
      NextState:= ActiveState.Next [PCh^];
      AddToList (PCh);
      if NextState= FinalState then
      begin
        CreateResult (PositionInfo);
        Exit (True);

      end;

      ActiveState:= NextState;

    end;
    Inc (PCh);

  end;

  Result:= False;

end;

function TAutomata.ToString: String;
var
  i: Integer;
  Ch: Char;
  ActiveState: TState;

begin
  Result:= '';

  for i:= 0 to StateCollection.Count- 1 do
  begin
    ActiveState:= StateCollection.StateByIndex [i];

    Result+= '('+ IntToStr (ActiveState.ID)+ ':"'+ ActiveState.Name+ '"';
    for Ch:= #0 to #$FF do
      if ActiveState.Next [Ch]<> StateCollection.StateByIndex [0] then
        Result+= '('+ Ch+ ':'+ IntToStr (ActiveState.Next [Ch].ID)+ ')';

    Result+= ')'#10;

  end;

end;

{ TStateCollection }

procedure TStateCollection.AddNewState (NewState: TState; Name: String);
var
  Index: Integer;

begin
  Index:= -1;
  if not Self.Find (Name, Index) then
    Self.AddObject (Name, NewState)
  else
    raise EAStateWithSameNameExists.Create (Name);

end;

function TStateCollection.GetState (Name: String): TState;
var
  Index: Integer;

begin
  Index:= -1;
  if Self.Find (Name, Index) then
    Result:= Self.Objects [Index] as TState
  else
    Result:= nil;

end;

function TStateCollection.GetStateByIndex (Index: Integer): TState;
begin
  Result:= Objects [Index] as TState;

end;

function TStateCollection.GetStateOrCreateOne (AName: String): TState;
begin
  Result:= State [AName];

  if Result= nil then
  begin
    Result:= TState.Create (AName, Count+ 1);
    Self.AddObject (AName, Result);

  end;

end;

end.

