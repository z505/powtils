unit CollectionUnit;

interface
uses
  Classes, SysUtils;
  
type
  TCompareFunction= function (Obj1, Obj2: TObject): Boolean;
  
  TBaseCollection= class (TObject)
  protected
    FMembers: array of TObject;
    FSize: Integer;

  private
  
  protected
    function GetMember (Index: Integer): TObject;
    (*No range checking while writing in*)
    procedure SetMember (Index: Integer; const Value: TObject);


  public
    property Size: Integer read FSize;
    property Member [Index: Integer]: TObject read GetMember write SetMember;

    function GetPointerToFirst: Pointer;
    constructor Create;
    destructor Destroy; override;

    //Remove all elements in collection but not free them
    procedure Clear; virtual;

    procedure Add (Data: TObject); virtual;
    //Remove the index'th element and free it.
    procedure Delete (Index: Integer); virtual;
    procedure AddAnotherCollection (AnotherColleciton: TBaseCollection);

   // Sort the members based on compare function (which is accept two TObject objects) and return true if the first one is smaller than the second
    procedure Sort (CompareFunction: TCompareFunction);
    procedure Allocate (Size: Integer);

  end;

  TDoubleCollection= class (TObject)
  protected
    FMembers: array of Extended;
    FSize: Integer;
    
  private
    function GetMember (Index: Integer): Extended;
    function GetPointerToMembers: PExtended;
    function GetMinIndex: Integer;
    function GetMaxIndex: Integer;
    procedure SetMember (Index: Integer; const Value: Extended);

  public
    property Size: Integer read FSize;
    property Member [Index: Integer]: Extended read GetMember write SetMember;
    property PointerToMembers: PExtended read GetPointerToMembers;
    property MinIndex: Integer read GetMinIndex;
    property MaxIndex: Integer read GetMaxIndex;

    constructor Create;
    destructor Destroy; override;

    procedure Add (Data: Extended);
    procedure Delete (Index: Integer);
    procedure Allocate (NewSize: Integer);
    function GetPointerToFirst: PDouble;
    
  end;

  TLongWordCollection= class (TBaseCollection)
  private
    function GetMember (Index: Integer): LongWord;

  public
    property MemberAt [Index: Integer]: LongWord read GetMember;

    constructor Create;
    destructor Destroy; override;

    procedure Add (Data: LongWord); 
    procedure Delete (Index: Integer); override;

    procedure FillWithZero (Length: Integer);
    function Min: LongWord;
    function Max: LongWord;
    function Avg: LongWord;

  end;

  TInt64Collection= class (TBaseCollection)
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

  TIntegerCollection= class (TBaseCollection)
  private
    function GetMember (Index: Integer): Integer;
    procedure SetMember(Index: Integer; const Value: Integer);

  public
    property MemberAt [Index: Integer]: Integer read GetMember
      write SetMember;

    constructor Create;
    destructor Destroy; override;

    procedure Add (Data: Integer);
    procedure Delete (Index: Integer);

    procedure FillWithZero (Length: Integer);

  end;

  TByteCollection= class (TBaseCollection)
  private
    function GetByteAt(Index: Integer): Byte;
  public
    property ByteAr [Index: Integer]: Byte read GetByteAt;

    procedure AddByte (AByte: Byte);
    procedure Delete (Index: Integer);

    destructor Destroy; override;
    
  end;
  
  TBaseFixSizeCollection= TBaseCollection;
  TStringCollection= TStrings;
  
  { TNameValue }

  TNameValue= class (TObject)
  protected
    FName: String;
    FNameInUpperCase: String;
    FValue: TObject;
    
  public
    property Name: String read FName;
    property Value: TObject read FValue;

    constructor Create (Nam: String; Value: TObject);
    destructor Destroy; override;
    
  end;
  
  { TNameValueCollection }

  TNameValueCollection= class (TBaseCollection)
  protected
    function GetNameValue (Index: Integer): TNameValue;
    function GetNameValueByName (AName: String): TNameValue;
    
    procedure RemoveValueByName (Name: String);
    
  public
    property NameValueByName [AName: String]: TNameValue read GetNameValueByName;
    property NameValue [Index: Integer]: TNameValue read GetNameValue;
    
    function IsExists (Name: String): Boolean;

  end;
  

  { EVariableNotFound }

  ENameNotFound= class (Exception)
  public
    constructor Create (AName: String);
    
  end;
  
implementation

uses ExceptionUnit, Math, MyTypes;

{ TBaseCollection }

procedure TBaseCollection.Add (Data: TObject);
begin
  Inc (FSize);
  SetLength (FMembers, FSize);

  FMembers [FSize- 1]:= Data;  

end;

procedure TBaseCollection.AddAnotherCollection (
  AnotherColleciton: TBaseCollection);
var
  i: Integer;
  SrcPtr, TrgPtr: PObject;
  
begin
  i:= Size;
  Allocate (Size+ AnotherColleciton.Size);

  TrgPtr:= GetPointerToFirst;
  Inc (TrgPtr, i);
  SrcPtr:= AnotherColleciton.GetPointerToFirst;

  for i:= 1 to AnotherColleciton.Size do
  begin
    TrgPtr^:= SrcPtr^;
    Inc (TrgPtr);
    Inc (SrcPtr);
    
  end;

end;

procedure TBaseCollection.Allocate (Size: Integer);
var
  Ptr: PObject;
  i: Integer;
  
begin
  SetLength (FMembers, Size);
  FSize:= Size;
  
  Ptr:= GetPointerToFirst;
  for i:= 1 to Size do
  begin
    Ptr^:= nil;
    Inc (Ptr);
    
  end;
  
end;

procedure TBaseCollection.Clear;
begin
  FSize:= 0;
  SetLength (FMembers, 0);
  
end;

constructor TBaseCollection.Create;
begin
  inherited;

  FSize:= 0;
  
end;

procedure TBaseCollection.Delete (Index: Integer);
var
  i: Integer;
  PtrCurrent, PtrPrev: PObject;
  
begin
  if (Index< 0) or (FSize<= Index) then
    raise ERangeCheckError.Create ('TBaseCollection.Delete');

  FMembers [Index].Free;
  PtrCurrent:= @FMembers [Index+ 1];
  PtrPrev:= @FMembers [Index];
  for i:= Index+ 1 to FSize- 1 do
  begin
    PtrPrev^:= PtrCurrent^;
    Inc (PtrCurrent);
    Inc (PtrPrev);
//    FMembers [i- 1]:= FMembers [i];
    
  end;
    
  Dec (FSize);
  SetLength (FMembers, FSize);

end;

destructor TBaseCollection.Destroy;
var
  i: Integer;
  Ptr: PObject;
  
begin
  Ptr:= GetPointerToFirst;
  for i:= 1 to FSize do
  begin
    Ptr.Free;
    Inc (Ptr);
    
  end;

  SetLength (FMembers, 0);
  FSize:= 0;
  
  inherited Destroy;

end;

function TBaseCollection.GetMember (Index: Integer): TObject;
begin
  if (Index< 0) or (FSize<= Index) then
    raise ERangeCheckError.Create ('TBaseCollection.Delete');

  Result:= FMembers [Index];

end;

function TBaseCollection.GetPointerToFirst: Pointer;
begin
  Result:= Pointer (@FMembers [0]);

end;

procedure TBaseCollection.SetMember (Index: Integer; const Value: TObject);
begin
//  FMembers [Index].Free;
  FMembers [Index]:= Value;
    
end;

procedure TBaseCollection.Sort;
var
  Ptr1, Ptr2: PObject;
  Temp: TObject;
  i, j: Integer;

begin

  Ptr1:= GetPointerToFirst;
  for i:= 1 to Size do
  begin
    Ptr2:= Ptr1;
    Inc (Ptr2);

    for j:= i+ 1 to Size do
    begin
      if CompareFunction (Ptr1^, Ptr2^) then
      begin
        Temp:= Ptr1^;
        Ptr1^:= Ptr2^;
        Ptr2^:= Temp;

      end;
      Inc (Ptr2);
      
    end;
    
    Inc (Ptr1);

  end;

end;

{ TDoubleCollection }

procedure TDoubleCollection.Add (Data: Extended);
begin
  Inc (FSize);
  SetLength (FMembers, FSize);

  FMembers [FSize- 1]:= Data;  

end;

procedure TDoubleCollection.Allocate (NewSize: Integer);
begin
  SetLength (FMembers, NewSize);
  FSize:= NewSize;
  
end;

constructor TDoubleCollection.Create;
begin
  inherited;

  FSize:= 0;
  
end;

procedure TDoubleCollection.Delete(Index: Integer);
var
  i: Integer;

begin
  if (Index< 0) or (FSize<= Index) then
    raise ERangeCheckError.Create ('TBaseCollection.Delete');

  for i:= Index+ 1 to FSize- 1 do
    FMembers [i- 1]:= FMembers [i];
    
  Dec (FSize);
  SetLength (FMembers, FSize);
  
end;

destructor TDoubleCollection.Destroy;
begin
  SetLength (FMembers, 0);
  FSize:= 0;

  inherited;
end;

function TDoubleCollection.GetMaxIndex: Integer;
var
  i: Integer;
  MaxValue: Extended;
  PMember: PExtended;

begin
  Result:= 0;
  if Size= 0 then
    Result:= -1
  else
  begin
    MaxValue:= Member [0];
    PMember:= @FMembers [1];

    for i:= 1 to Size- 1 do
    begin
      if MaxValue< PMember^ then
      begin
        MaxValue:= PMember^;
        Result:= i;

      end;

      Inc (PMember);

    end;

  end;

end;

function TDoubleCollection.GetMember (Index: Integer): Extended;
begin
  if (Index< 0) or (FSize<= Index) then
    raise ERangeCheckError.Create ('TBaseCollection.Delete');

  Result:= FMembers [Index];

end;

function TDoubleCollection.GetMinIndex: Integer;
var
  i: Integer;
  MinValue: Extended;
  PMember: PExtended;

begin
  Result:= 0;
  if Size= 0 then
    Result:= -1
  else
  begin
    MinValue:= Member [0];
    PMember:= @FMembers [0];

    for i:= 1 to Size- 1 do
      if PMember^< MinValue then
      begin
        MinValue:= PMember^;
        Result:= i;
                
      end;

  end;

end;

function TDoubleCollection.GetPointerToFirst: PDouble;
begin
  Result:= @FMembers [0];

end;

function TDoubleCollection.GetPointerToMembers: PExtended;
begin
  if Size= 0 then
    raise ERangeCheckError.Create ('GetPointerToMembers');
    
  Result:= @FMembers [0];
  
end;

procedure TDoubleCollection.SetMember (Index: Integer;
  const Value: Extended);
begin
  FMembers [Index]:= Value;
    
end;

{ TLongWordCollection }

procedure TLongWordCollection.Add (Data: LongWord);
var
  NewPtr: PLongWord;

begin
  NewPtr:= New (PLongWord);
  NewPtr^:= Data;
  
  inherited Add (TObject (NewPtr));
  
end;

function TLongWordCollection.Avg: LongWord;
var
  i: Integer;
  Ptr: PObject;

begin
  Ptr:= GetPointerToFirst;
  Result:= 0;

  for i:= 1 to Size do
  begin
    Inc (Result, PLongWord (Ptr^)^);
    Inc (Ptr);
    
  end;

  if Size<> 0 then
    Result:= Result div Size;

end;

constructor TLongWordCollection.Create;
begin
  inherited;

end;

procedure TLongWordCollection.Delete (Index: Integer);
begin
  Dispose (PLongWord (Member [Index]));

  inherited;
  
end;

procedure TLongWordCollection.FillWithZero (Length: Integer);
var
  i: Integer;
  Ptr: PObject;

begin
  Allocate (Length);
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Length do
  begin
    Ptr^:= TObject (New (PLongWord));
    PLongWord (Ptr^)^:= 0;
    Inc (Ptr);

  end;

end;

destructor TLongWordCollection.Destroy;
var
  i: Integer;
  Ptr: ^TObject;
  
begin
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Size do
  begin
    Dispose (PLongWord (Ptr^));
    Inc (Ptr);
    
  end;

  Clear;
  inherited Destroy;

end;

function TLongWordCollection.GetMember (Index: Integer): LongWord;
begin
  Result:= PLongWord (Member [Index])^;
  
end;

function TLongWordCollection.Max: LongWord;
var
  Ptr: PObject;
  i: Integer;

begin
  Result:= 0;
  if 0< Size then
    Result:= MemberAt [0];
  Ptr:= GetPointerToFirst;

  for i:= 2 to Size do
  begin
    Inc (Ptr);
    if Result< PLongWord (Ptr^)^ then
      Result:= PLongWord (Ptr^)^

  end;

end;

function TLongWordCollection.Min: LongWord;
var
  Ptr: PObject;
  i: Integer;

begin
  Result:= 0;
  if 0< Size then
    Result:= MemberAt [0];
  Ptr:= GetPointerToFirst;

  for i:= 2 to Size do
  begin
    if PLongWord (Ptr^)^< Result then
      Result:= PLongWord (Ptr^)^;
    Inc (Ptr);
    
  end;
  
end;

{ TByteCollection }

procedure TByteCollection.AddByte (AByte: Byte);
var
  Ptr: PByte;

begin
  New (Ptr);
  Ptr^:= AByte;

  inherited Add (TObject (Ptr));
  
end;

procedure TByteCollection.Delete(Index: Integer);
begin
  Dispose (PByte (Member [Index]));

  inherited;

end;

destructor TByteCollection.Destroy;
var
  i: Integer;
  Ptr: PObject;
  
begin
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Size do
  begin
    Dispose (PByte (Ptr^));
    Inc (Ptr);
    
  end;

  Clear;
  inherited;
  
end;

function TByteCollection.GetByteAt(Index: Integer): Byte;
begin
  Result:= PByte (Member [Index])^;
  
end;

{ TIntegerCollection }

procedure TIntegerCollection.Add (Data: Integer);
var
  NewPtr: PInteger;

begin
  New (NewPtr);
  NewPtr^:= Data;
  
  inherited Add (TObject (NewPtr));
  
end;

constructor TIntegerCollection.Create;
begin
  inherited;
  
end;

procedure TIntegerCollection.Delete (Index: Integer);
begin
  if Member [Index]<> nil then
    Dispose (PInteger (Member [Index]));

  inherited;
  
end;

procedure TIntegerCollection.FillWithZero (Length: Integer);
var
  i: Integer;

begin
  for i:= 1 to Length do
    Self.Add (0);

end;

destructor TIntegerCollection.Destroy;
var
  i: Integer;
  Ptr: ^TObject;
  
begin
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Size do
  begin
    Dispose (PInteger (Ptr^));
    Inc (Ptr);
    
  end;

  Clear;
  inherited;

end;

function TIntegerCollection.GetMember(Index: Integer): Integer;
begin
  Result:= PInteger (Member [Index])^;

end;

procedure TIntegerCollection.SetMember (Index: Integer;
  const Value: Integer);
begin
  PInteger (Member [Index])^:= Value;
  
end;

{ TInt64Collection }

procedure TInt64Collection.Add (Data: Int64);
var
  NewPtr: PInt64;

begin
  NewPtr:= New (PInt64);
  NewPtr^:= Data;
  
  inherited Add (TObject (NewPtr));
  
end;

function TInt64Collection.Avg: Int64;
var
  i: Integer;
  Ptr: PObject;

begin
  Ptr:= GetPointerToFirst;
  Result:= 0;

  for i:= 1 to Size do
  begin
    Inc (Result, PInt64 (Ptr^)^);
    Inc (Ptr);
    
  end;

  if Size<> 0 then
    Result:= Result div Size;

end;

constructor TInt64Collection.Create;
begin
  inherited;

end;

procedure TInt64Collection.Delete (Index: Integer);
begin
  Dispose (PInt64 (Member [Index]));

  inherited;
  
end;

procedure TInt64Collection.FillWithZero (Length: Integer);
var
  i: Integer;
  Ptr: PObject;

begin
  Allocate (Length);
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Length do
  begin
    Ptr^:= TObject (New (PInt64));
    Inc (Ptr);

  end;

end;

destructor TInt64Collection.Destroy;
var
  i: Integer;
  Ptr: ^TObject;
  
begin
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Size do
  begin
    Dispose (PInt64 (Ptr^));
    Inc (Ptr);
    
  end;

  Clear;
  inherited Destroy;

end;

function TInt64Collection.GetMember (Index: Integer): Int64;
begin
  Result:= PInt64 (Member [Index])^;

end;

function TInt64Collection.Max: Int64;
var
  Ptr: PObject;
  i: Integer;

begin
  Result:= 0;
  if 0< Size then
    Result:= MemberAt [0];
  Ptr:= GetPointerToFirst;

  for i:= 2 to Size do
  begin
    Inc (Ptr);
    if Result< PInt64 (Ptr^)^ then
      Result:= PInt64 (Ptr^)^

  end;

end;

function TInt64Collection.Min: Int64;
var
  Ptr: PObject;
  i: Integer;

begin
  Result:= 0;
  if 0< Size then
    Result:= MemberAt [0];
  Ptr:= GetPointerToFirst;

  for i:= 2 to Size do
  begin
    if PInt64 (Ptr^)^< Result then
      Result:= PInt64 (Ptr^)^;
    Inc (Ptr);
    
  end;

end;

procedure TInt64Collection.SetMember(Index: Integer; const Value: Int64);
begin
  PInt64 (Member [Index])^:= Value;

end;

{ TNameValueCollection }

function TNameValueCollection.GetNameValue(Index: Integer): TNameValue;
begin
  Result:= Member [Index] as TNameValue;
  
end;

function TNameValueCollection.GetNameValueByName (AName: String): TNameValue;
var
  i: Integer;
  Ptr: PObject;

begin
  AName:= UpperCase (AName);
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Size do
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
  Ptr:= GetPointerToFirst;
  Name:= UpperCase (Name);
  
  for i:= 1 to Size do
  begin
    if (Ptr^ as TNameValue).FNameInUpperCase= Name then
    begin
      Delete (i);
      Break;
      
    end;
    Inc (Ptr);
    
  end;
  
end;

function TNameValueCollection.IsExists (Name: String): Boolean;
var
  i: Integer;
  Ptr: PObject;
  
begin
  Ptr:= GetPointerToFirst;
  Result:= False;
  Name:= UpperCase (Name);
  
  for i:= 1 to Size do
  begin
    if (Ptr^ as TNameValue).FNameInUpperCase= Name then
    begin
      Result:= True;
      Break;

    end;
    Inc (Ptr);

  end;


end;

{ EVariableNotFound }

constructor ENameNotFound.Create(AName: String);
begin
  inherited Create (AName+ ' not found in collection!');
  
end;

{ TNameValue }

constructor TNameValue.Create (Nam: String; Value: TObject);
begin
  inherited Create;
  
  FName:= Nam;
  FValue:= Value;
  FNameInUpperCase:= UpperCase (FName);
  
end;

destructor TNameValue.Destroy;
begin

  FValue.Free;
  
  inherited Destroy;
  
end;

end.

