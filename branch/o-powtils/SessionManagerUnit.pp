unit SessionManagerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils{, CollectionUnit};

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

  TVariable= class (TObject)
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

  TSession= class (TList)
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
    
  end;
  
  { TAbstractSessionManager }

  TAbstractSessionManager= class (TList)
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
    {This constructor reads the SessionIdLen, SessionIDVariableName and
      StoreSessionIdInCookie variables from Configuration in GlobalObjectContainer.
    }
    constructor Create;
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

  TCookiBasedSessionManager= class (TBasicSessionManager)
  end;


const
  EmptySessionID: TGuid= '{5D6D6F68-69C9-11DD-B04C-EB0A56D89593}';

function GetSession (SessionID: TSessionID): TSession;

var
  SessionManager: TAbstractSessionManager;// Will be created in TResident constructor
  
implementation
uses
  MyTypes, ThisProjectGlobalUnit;

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
  Result:= TObject (Items [Index]) as TSession;
  
end;

function TAbstractSessionManager.GetSessionBySessionID (SessionID: TSessionID
  ): TSession;
var
  Ptr: PObject;
  i: Integer;
  
begin

  Ptr:= First;
  for i:= 1 to Count do
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
  Ptr:= First;
  for i:= 1 to Count do
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

constructor TAbstractSessionManager.Create;
begin
{TODO: TAbstractSessionManager.Create;}
{
  inherited Create;

  Load;

  FSessionIDLen:= 20;
  try
    FSessionIDLen:= StrToInt (GlobalObjContainer.Configurations.ConfigurationValueByName ['SessionIDLen']);

  except
    on e: ENameNotFound do;
    on e: EConvertError do
      WriteLn ('Session id len is not an integer!');

  end;

  try
    FSessionIDVarName:= GlobalObjContainer.Configurations.ConfigurationValueByName ['SessionIDVarName'];
  except
    on e: ENameNotFound do
    begin
      FSessionIDVarName:= 'PSPSESSID';

    end;

  end;

{  if Copy (GlobalObjContainer.Configurations.ConfigurationValueByName ['InstallationDirectory'],
    1, 1)<> '/' then
    raise EInvalidArgument.Create ('InstallationDirectory should be absolute path!');

  end;
}

{  FSessionIDLen:= SessionIDLen;
  FSessionIDVarName:= SessIDVarName;
  FStoreSessionIDInCookie:= StoreSessIDInCookie;
}
}
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
{  const
    Letters: String= 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890';
}
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
{TODO:   TVariable.GetStrValue: String;}
//  Result:= String (FValue);
  
end;

procedure TVariable.UpdateValue (AVal: String);
begin
{TODO: TVariable.UpdateValue (AVal: String);}
//  FValue:= TObject (AVal);
  
end;

constructor TVariable.Create (AName, AValue: String);
begin
{TODO: TVariable.Create (AName, AValue: String);}
//  inherited Create (AName, TObject (AValue));
  
end;

destructor TVariable.Destroy;
begin

  inherited Destroy;
  
end;

{ TSession }

function TSession.GetVariable (Index: Integer): TVariable;
begin
//  Result:= NameValue [Index] as TVariable;
  
end;

function TSession.GetValueByName (VariableName: String): String;
begin
//  Result:= (NameValueByName [VariableName] as TVariable).GetStrValue;
  
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
{TODO: 16}
//  Index:= IndexOf (VariableName);
  if Index<> -1 then
    Delete (Index);
    
end;

procedure TSession.AddValue (AName, AValue: String);
var
  ANewVariable: TVariable;
  
begin
  ANewVariable:= TVariable.Create (AName, AValue);
  {TODO: 16}
//  Self.AddObject (AName, ANewVariable);
  
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
  raise Exception.Create ('TBasicSessionManager.Load: Not implemented Yet!');

end;

procedure TBasicSessionManager.Save;
begin
  raise Exception.Create ('TBasicSessionManager.Save: Not implemented Yet!');

end;

end.

