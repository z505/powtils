unit CollectionUnit;

interface
uses
  Classes, SysUtils, MyTypes, GenericCollectionUnit;
  
type
  TCompareFunction= function (Obj1, Obj2: TObject): Boolean;

  TBaseCollection= specialize TGenericCollection<TObject>;

{  TInt64Collection= class (TBaseCollection)
  private
    function GetMember (Index: Integer): Int64;
    procedure SetMember(Index: Integer; const Value: Int64);

  public
    property MemberAt [Index: Integer]: Int64
      read GetMember write SetMember;

    constructor Create;
    destructor Destroy; override;

    procedure Add (Data: Int64);
    procedure Delete (Index: Integer);

    procedure FillWithZero (Length: Integer);
    function Min: Int64;
    function Max: Int64;
    function Avg: Int64;

  end;
}
  { TNameValue }

  TNameValue= class (TObject)
  protected
    FName: String;
    FNameInUpperCase: String;
    FValue: TObject;
    
  public
    property Name: String read FName;
    property Value: TObject read FValue;

    constructor Create (AName: String; AValue: TObject);
    destructor Destroy; override;
    
  end;

  { TNameValueCollection }

  TNameValueCollection= class (TBaseCollection)
  private
    function GetValueByName (AName: String): TObject;

  protected
    function GetNameValue (Index: Integer): TNameValue;
    function GetNameValueByName (AName: String): TNameValue;
    
    procedure RemoveValueByName (Name: String);
    function GetNameValueIndexByName (AName: String): Integer;
    
  public
    property NameValueByName [AName: String]: TNameValue read GetNameValueByName;
    property NameValue [Index: Integer]: TNameValue read GetNameValue;
    property ValueByName [AName: String]: TObject read GetValueByName;
    
    function IsExists (AName: String): Boolean;
    function UpdateValue (AName, AValue: String): Boolean;

  end;
  

  { EVariableNotFound }

  ENameNotFound= class (Exception)
  public
    constructor Create (AName: String);
    
  end;
  
implementation

uses
  Math;


{ TNameValueCollection }

function TNameValueCollection.GetValueByName (AName: String): TObject;
begin
  try
    Result:= NameValueByName [AName].Value;
    
  except
    on e: ENameNotFound do
      Result:= nil;
      
  end;
  
end;

function TNameValueCollection.GetNameValue(Index: Integer): TNameValue;
begin
  Result:= Item [Index] as TNameValue;
  
end;

function TNameValueCollection.GetNameValueByName (AName: String): TNameValue;
var
  i: Integer;
  Ptr: PObject;

begin
  AName:= UpperCase (AName);
  Ptr:= Self.First;
  
  for i:= 1 to Count do
  begin
    if (Ptr^ as TNameValue).FNameInUpperCase= AName then
    begin
      Result:= Ptr^ as TNameValue;
      Exit;
      
    end;
    
    Inc (Ptr);
    
  end;
  
  raise ENameNotFound.Create (AName);
  
end;

procedure TNameValueCollection.RemoveValueByName (Name: String);
var
  i: Integer;
  Ptr: PObject;
  
begin
  Ptr:= First;
  Name:= UpperCase (Name);
  
  for i:= 1 to Count do
  begin
    if (Ptr^ as TNameValue).FNameInUpperCase= Name then
    begin
      Delete (i);
      Break;
      
    end;
    Inc (Ptr);
    
  end;
  
end;

function TNameValueCollection.GetNameValueIndexByName (AName: String): Integer;
var
  i: Integer;
  Ptr: PObject;

begin
  Ptr:= First;
  Result:= -1;
  AName:= UpperCase (AName);

  for i:= 0 to Count- 1 do
  begin
    if (Ptr^ as TNameValue).FNameInUpperCase= AName then
    begin
      Result:= i;
      Break;

    end;
    Inc (Ptr);

  end;
  
  Result:= -1;

end;

function TNameValueCollection.IsExists (AName: String): Boolean;
var
  i: Integer;
  Ptr: PObject;
  
begin
  Ptr:= First;
  Result:= False;
  AName:= UpperCase (AName);
  
  for i:= 1 to Count do
  begin
    if (Ptr^ as TNameValue).FNameInUpperCase= AName then
    begin
      Result:= True;
      Break;

    end;
    Inc (Ptr);

  end;


end;

function TNameValueCollection.UpdateValue (AName, AValue: String): Boolean;
begin
  NameValueByName [AName].FValue:= TObject (@AValue);
  
end;

{ EVariableNotFound }

constructor ENameNotFound.Create(AName: String);
begin
  inherited Create (AName+ ' not found in collection!');
  
end;

{ TNameValue }

constructor TNameValue.Create (AName: String; AValue: TObject);
begin
  inherited Create;
  
  FName:= AName;
  FValue:= AValue;
  FNameInUpperCase:= UpperCase (FName);
  
end;

destructor TNameValue.Destroy;
begin

  FValue.Free;
  
  inherited Destroy;
  
end;

end.

