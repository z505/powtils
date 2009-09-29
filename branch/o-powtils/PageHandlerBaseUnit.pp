{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                    PSP 1.6.x PageHandlerBaseUnit

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

--------------------------------------------------------------------------------
 Collection Unit
--------------------------------------------------------------------------------

 PSP 1.6.x
 ---------

  [30/MAR/2006 - Amir]
   - This unit contains the classes, variables which is need for a Resident page.
   By resident page, we mean that these pages will be used on non-CGI mode (in which
   on every request all the allocation must be done)
}

unit PageHandlerBaseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebUnit, CollectionUnit, XMLNode, Unix,
    BaseUnix, SessionManagerUnit, WebHeaderUnit,
    AbstractHandlerUnit;
  
type

  { THandlerBase }

  THandlerBase= class (TAbstractHandler)
  private
    function GetSession: TSession;


  protected
    procedure Clear;

  public
    property Session: TSession read GetSession;

    constructor Create (ContType: TContentType; ThisPageName: Ansistring;
                                ThisPagePath: AnsiString);
    destructor Destroy; override;

//    function CreateNewInstance: TAbstractHandler; override;

  end;
  
  { THTMLHandlerPage }
  THTMLHandler= class (THandlerBase)
  public
    constructor Create (ThisPageName: AnsiString; ThisPagePath: AnsiString= '');
    procedure Flush; override;

  end;

  
  { TXMLHandlerPage }

  TXMLHandler= class (THandlerBase)
  private
    FXSLPath: String;
    FXMLRoot: TXMLNode;
    FIndent: Boolean;

  protected
    property XMLRoot: TXMLNode read FXMLRoot;

  public
    property XSLPath: String read FXSLPath;

    constructor Create (ThisPageName: String; XSLPage: String;
                RelativePagePath: String= '';
                Encoding: String= 'UTF-8';                                    
                Version: String= '1.0';
                Indent: Boolean= False);

    destructor Destroy; override;
    procedure Flush; override;

  end;

  { TAbstractHandlerCollection }

  TAbstractHandlerCollection= class (TStringList)
  private
    function GetPageHandler (Index: Integer): TAbstractHandler;
    function GetPageHandlerByName (const AName: String): TAbstractHandler;


  public
    property PageHandler [Index: Integer]: TAbstractHandler read GetPageHandler;
    property PageHandlerByName [const AName: String]: TAbstractHandler
            read GetPageHandlerByName;

    procedure AddPageHandler (const AName: String;
                      const APageHandler: TAbstractHandler);

    constructor Create;
    destructor Destroy; override;

  end;

  { EPageNotFound }

  EPageNotFound= class (Exception)
  public
    constructor Create (const Name: String);

  end;

implementation
uses
  ThisProjectGlobalUnit, DateUtils, GlobalUnit, ExceptionUnit;
  
{ THandlerBase }

function THandlerBase.GetSession: TSession;
(*$I+*)
const
  SessionIDVarName: String= '';
(*$I-*)
var
  SessionIDInCookie: String;
  
begin
  if SessionIDVarName= '' then
    SessionIDVarName:= GlobalObjContainer.Configurations.ConfigurationValueByName ['SessionIDVarName'];

  if IsEqualGUID (SessionID, EmptySessionID) then
  begin
    SessionIDInCookie:= Cookies.CookieValueByName [SessionIDVarName];
    try
      FSessionID:= StringToGUID (SessionIDInCookie);
      
    except
      on e: EConvertError do
        FSessionID:= EmptySessionID;
       
    end;
    
  end;

  Result:= SessionManagerUnit.GetSession (FSessionID);
  FSessionID:= Result.SessionID;
  Cookies.Add (SessionIDVarName, GUIDToString (FSessionID),
                    Now+ 1.0/ 24);
  
end;

constructor THandlerBase.Create(ContType: TContentType;
  ThisPageName: Ansistring; ThisPagePath: AnsiString);
begin
  inherited Create (ContType, ThisPageName, ThisPagePath, False);

  if GlobalObjContainer.Configurations.ConfigurationValueByName ['SessionEnabled']= 'YES' then
    FSessionID:= EmptySessionID;

end;

destructor THandlerBase.Destroy;
begin
  Clear;
  
  inherited;
  
end;

{
function THandlerBase.CreateNewInstance: TAbstractHandler;
begin
  raise EShouldNotBeCalled.Create ('THandlerBase', 'CreateNewInstance');

end;
}

procedure THandlerBase.Clear;
begin
  WriteHeaders;
  if Buffer.Count<> 0 then
    Write (Buffer.Text);

  FpClose (PipeHandle);
  PipeIsAssigned:= False;

end;

{ TXMLHandlerBase }

constructor TXMLHandler.Create (ThisPageName: String; XSLPage: String;
             RelativePagePath: String;
             Encoding: String; Version: String;
             Indent: Boolean= False);
begin
  inherited Create (ctTextXML, ThisPageName, RelativePagePath);

  FXSLPath:= XSLPage;
  FIndent:= Indent;

  FXMLRoot:= TXMLNode.Create (ThisPageName);

  Buffer.Add ('<?xml version="'+ Version+ '" encoding="'+ Encoding+ '" ?>');
  if XSLPath<> '' then
    Buffer.Add ('<?xml-stylesheet href="'+ XSLPath+ '" type= "text/xsl" ?>');

end;

procedure TXMLHandler.Flush;
begin
  if FIndent then
    Write (FXMLRoot.ToStringWithIndent)
  else
    Write (FXMLRoot.ToStringWithOutIndent);

end;

destructor TXMLHandler.Destroy;
begin
  Flush;
  FXMLRoot.Free;
  
  inherited;
  
end;

{ TAbstractHandlerCollection }

function TAbstractHandlerCollection.GetPageHandler (Index: Integer): TAbstractHandler;
begin
  Result:= Objects [Index] as TAbstractHandler;

end;

function TAbstractHandlerCollection.GetPageHandlerByName (const AName: String): TAbstractHandler;
var
  Index: Integer;

begin
  Index:= Self.IndexOf (AName);
  if 0<= Index then
    Result:= GetPageHandler (Index)
  else
    raise EPageNotFound.Create (AName);

end;

procedure TAbstractHandlerCollection.AddPageHandler (const AName: String;
  const APageHandler: TAbstractHandler);
begin
  AddObject (AName, APageHandler);
  Sort;

end;

constructor TAbstractHandlerCollection.Create;
begin
  inherited Create;

end;

destructor TAbstractHandlerCollection.Destroy;
var
  i: Integer;

begin
  for i:= 0 to Count- 1 do
    Objects [i].Free;

  Clear;

  inherited Destroy;
end;

{ EPageNotFound }

constructor EPageNotFound.Create (const Name: String);
begin
  inherited Create ('No page with name= '+ Name+ ' exists in collection!');

end;

{ THTMLHandler }

constructor THTMLHandler.Create (ThisPageName: AnsiString;
  ThisPagePath: AnsiString);
begin
  inherited Create (ctTextHTML, ThisPageName, ThisPagePath);

end;

procedure THTMLHandler.Flush;
begin
  Write (' ');

end;

end.

