unit SessionManagerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit;

type

  { EVariableNotFoundInSession }

  EVariableNotFoundInSession= class (Exception)
  public
    constructor Create;
    
  end;
  
  { ESessionNotFound }

  ESessionNotFound= class (Exception)
  public
    constructor Create;
    
  end;
  
  { TVariable }

  TVariable= class (TNameValue)
  private
    function GetStrValue: String;
    procedure UpdateValue (AVal: String);
    
  public
    property StrValue: String read GetStrValue;
    
    constructor Create (AName, AValue: String);
    destructor Destroy; override;

  end;

  TSessionID= TGuid;
  
  { TSession }

  TSession= class (TNameValueCollection)
  private
    FSessionID: TSessionID;
    
    function GetValueByName(VariableName: String): String;
    function GetVariable (Index: Integer): TVariable;

  public
    property SessionID: TSessionID read FSessionID;
    property Variable [Index: Integer]: TVariable read GetVariable;
    property ValueByName [VariableName: String]: String read GetValueByName;

    constructor Create (ThisSessionID: TSessionID);
    
    procedure DeleteVariable (VariableName: String);
    procedure AddValue (AName, AValue: String);
    
    function VariableExists (VariableName: String): Boolean;

  end;
  
  { TAbstractSessionManager }

  TAbstractSessionManager= class (TBaseCollection)
  private
    FSessionIDLen: Integer;
    FSessionIDVarName: String;
    FStoreSessionIDInCookie: Boolean;
    
    function GetSession (Index: Integer): TSession; virtual;
    function GetSessionBySessionID (SessionID: TSessionID): TSession; virtual;
    
    function FindSessionIndex (SessionID: TSessionID): Integer;

  public
    property SessionIDVarName: String read FSessionIDVarName;
    property Session [Index: Integer]: TSession read GetSession;
    property SessionBySessionID [SessionID: TSessionID]: TSession read GetSessionBySessionID;
    property StoreSessionIDInCookie: Boolean read FStoreSessionIDInCookie;

    constructor Create (SessionIDLen: Integer; SessIDVarName: String;
        StoreSessIDInCookie: Boolean);
    destructor Destroy; override;

    procedure AddSession (NewSession: TSession);
    procedure DeleteSession (SessionID: TSessionID);

    function GetNewSessionID: TSessionID; virtual;
    function CreateEmptySession: TSession;
    
    procedure Load; virtual; abstract;
    procedure Save; virtual; abstract;
    
  end;
  
  { TBasicSessionManager }

  TBasicSessionManager= class (TAbstractSessionManager)
  private
  public
    
    procedure Load; override;
    procedure Save; override;

  end;

const
  EmptySessionID: TGuid= '{5D6D6F68-69C9-11DD-B04C-EB0A56D89593}';

function GetSession (SessionID: TSessionID): TSession;

const
  DefualtSessionIDLen: Integer= 20;
  DefualtSessionVarName: String= 'PSPSESS';
  
var
  SessionManager: TAbstractSessionManager;// Will be created in TResident constructor
  
implementation
uses
  MyTypes;

function GetSession (SessionID: TSessionID): TSession;
begin
  if IsEqualGUID (SessionID, EmptySessionID) then
  begin
    Result:= SessionManager.CreateEmptySession;

  end
  else
    try
      Result:= SessionManager.SessionBySessionID [SessionID]
      
    except
      on e: ESessionNotFound do //Session has been expired
        Result:= SessionManager.CreateEmptySession

    end;
    
end;
  
{ TAbstractSessionManager }

function TAbstractSessionManager.GetSession (Index: Integer): TSession;
begin
  Result:= Member [Index] as TSession;
  
end;

function TAbstractSessionManager.GetSessionBySessionID (SessionID: TSessionID
  ): TSession;
var
  Ptr: PObject;
  i: Integer;
  
begin

  Ptr:= GetPointerToFirst;
  for i:= 1 to FSize do
  begin
    if IsEqualGUID ((TSession (Ptr^)).SessionID, SessionID) then
    begin
      Result:= TSession (Ptr^);
      Exit;

    end;

    Inc (Ptr);

  end;

  raise ESessionNotFound.Create;

end;

function TAbstractSessionManager.FindSessionIndex (SessionID: TSessionID): Integer;
var
  i: Integer;
  Ptr: PObject;
  
begin
  Ptr:= GetPointerToFirst;
  for i:= 1 to FSize do
  begin
    if IsEqualGUID ((TSession (Ptr^)).SessionID, SessionID) then
    begin
      Result:= i- 1;
      Exit;
      
    end;
    
    Inc (Ptr);
    
  end;
  
  Result:= -1;

end;

constructor TAbstractSessionManager.Create (SessionIDLen: Integer;
   SessIDVarName: String; StoreSessIDInCookie: Boolean);
begin
  inherited Create;
  
  Load;
  FSessionIDLen:= SessionIDLen;
  FSessionIDVarName:= SessIDVarName;
  FStoreSessionIDInCookie:= StoreSessIDInCookie;
  
end;

destructor TAbstractSessionManager.Destroy;
begin
  Save;

  inherited;
  
end;

procedure TAbstractSessionManager.AddSession (NewSession: TSession);
begin
  Add (NewSession);
  
end;

procedure TAbstractSessionManager.DeleteSession (SessionID: TSessionID);
var
  Index: Integer;
  
begin
  Index:= FindSessionIndex (SessionID);
  if Index< 0 then
    raise ESessionNotFound.Create
  else
    Delete (Index);
    
end;

function TAbstractSessionManager.GetNewSessionID: TSessionID;

  function GenerateSessionID: TSessionID;
  const
    Letters: String= 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890';
    
  var
    i: Integer;
    CharPtr: PChar;
    
  begin
    CreateGUID (Result);
{    Result:= Space (FSessionIDLen);
    CharPtr:= @Result [1];
    
    for i:= 1 to FSessionIDLen do
    begin
      CharPtr^:= Letters [Random (Length (Letters))] ;
      Inc (CharPtr);
      
    end;
}
  end;
  
begin
  Result:= GenerateSessionID;
  
  while FindSessionIndex (Result)<> -1 do
    Result:= GenerateSessionID;
    
end;

function TAbstractSessionManager.CreateEmptySession: TSession;
begin
  Result:= TSession.Create (GetNewSessionID);
  Self.AddSession (Result);
  
end;


{ TVariable }

function TVariable.GetStrValue: String;
begin
  Result:= String (FValue);
  
end;

procedure TVariable.UpdateValue (AVal: String);
begin
  FValue:= TObject (AVal);
  
end;

constructor TVariable.Create (AName, AValue: String);
begin
  inherited Create (AName, TObject (AValue));
  
end;

destructor TVariable.Destroy;
begin
  FValue:= nil;
  
  inherited Destroy;
  
end;

{ TSession }

function TSession.GetVariable (Index: Integer): TVariable;
begin
  Result:= NameValue [Index] as TVariable;
  
end;

function TSession.GetValueByName (VariableName: String): String;
begin
  Result:= (NameValueByName [VariableName] as TVariable).GetStrValue;
  
end;

constructor TSession.Create (ThisSessionID: TSessionID);
begin
  inherited Create;
  
  FSessionID:= ThisSessionID;
  
end;

{
procedure TSession.AddVariable (Name, Value: String);
begin
  Add (TVariable._Create (UpperCase (Name), Value));
  
end;

procedure TSession.UpdateValue (Name, Value: String);
var
  TempVar: TVariable;
  
begin
  TempVar:= VariableExists (Name);
  if TempVar= nil then
    raise EVariableNotFoundInSession.Create
  else
    TempVar.UpdateValue (Value);
  
end;
}

procedure TSession.DeleteVariable (VariableName: String);
var
  Index: Integer;
  
begin
  Index:= GetNameValueIndexByName (VariableName);
  if Index<> -1 then
    Delete (Index);
    
end;

procedure TSession.AddValue (AName, AValue: String);
var
  ANewVariable: TVariable;
  
begin
  ANewVariable:= TVariable.Create (AName, AValue);
  Self.Add (ANewVariable);
  
end;

function TSession.VariableExists (VariableName: String): Boolean;
begin
  Result:= GetNameValueIndexByName (VariableName)<> -1;

end;

{ EVariableNotFoundInSession }

constructor EVariableNotFoundInSession.Create;
begin
  inherited Create ('Variable Not Found In Session');
  
end;

{ ESessionNotFound }

constructor ESessionNotFound.Create;
begin
  inherited Create ('Session Not Found!');

end;

{ TBasicSessionManager }

procedure TBasicSessionManager.Load;
begin
  
end;

procedure TBasicSessionManager.Save;
begin
  
end;

end.

