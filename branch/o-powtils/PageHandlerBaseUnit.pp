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

  { THandlerPageBase }

  THandlerPageBase= class (TAbstractHandler)
  private
    function GetSession: TSession;

  protected
    procedure Clear;

  published

  public
    property Session: TSession read GetSession;

    constructor Create (ContType: TContentType);
    destructor Destroy; override;

    function CreateNewInstance: TAbstractHandler; override;

  end;
  
  { THTMLHandlerPage }
  THTMLHandlerPage= class (THandlerPageBase)
  public
    constructor Create;
    procedure Flush; override;

  end;

  
  { TXMLHandlerPage }

  TXMLHandlerPage= class (THandlerPageBase)
  private
    FXSLPath: String;
    FXMLRoot: TXMLNode;
    FIndent: Boolean;

  protected
    property XMLRoot: TXMLNode read FXMLRoot;

  public
    property XSLPath: String read FXSLPath;

    constructor Create (ThisPageName: String; XSLPage: String;
                Encoding: String= 'UTF-8';
                Version: String= '1.0'; PageHost: String= '';
                PagePath: String= ''; Indent: Boolean= False);

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
  
{ THandlerPageBase }

function THandlerPageBase.GetSession: TSession;
(*$I+*)
const
  SessionIDVarName: String= '';
(*$I-*)
var
  SessionIDInCookie: String;
  
begin
  if SessionIDVarName= '' then
    SessionIDVarName:= GlobalObjContainer.Configurations.ConfigurationValueByName ['SessionIDVarName'];

  if IsEqualGUID (FSessionID, EmptySessionID) then
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

constructor THandlerPageBase.Create (ContType: TContentType);
begin
  inherited Create (ContType, False);

  if GlobalObjContainer.Configurations.ConfigurationValueByName ['SessionEnabled']= 'YES' then
    FSessionID:= EmptySessionID;

end;

destructor THandlerPageBase.Destroy;
begin
  Clear;
  
  inherited;
  
end;

function THandlerPageBase.CreateNewInstance: TAbstractHandler;
begin
  raise EShouldNotBeCalled.Create ('THandlerPageBase', 'CreateNewInstance');

end;

procedure THandlerPageBase.Clear;
begin
  WriteHeaders;
  if Buffer.Count<> 0 then
    Write (Buffer.Text);

  FpClose (PipeHandle);
  PipeIsAssigned:= False;

end;

{ TXMLHandlerBase }

constructor TXMLHandlerPage.Create (ThisPageName: String; XSLPage: String;
             Encoding: String; Version: String;
             PageHost: String; PagePath: String;
             Indent: Boolean= False);
begin
  inherited Create (ctTextXML);

  FXSLPath:= XSLPage;
  FIndent:= Indent;
  
  FXMLRoot:= TXMLNode.Create (ThisPageName);

  Buffer.Add ('<?xml version="'+ Version+ '" encoding="'+ Encoding+ '" ?>');
  if XSLPath<> '' then
    Buffer.Add ('<?xml-stylesheet href="'+ XSLPath+ '" type= "text/xsl" ?>');

end;

procedure TXMLHandlerPage.Flush;
begin
  if FIndent then
    Write (FXMLRoot.ToStringWithIndent)
  else
    Write (FXMLRoot.ToStringWithOutIndent);

end;

destructor TXMLHandlerPage.Destroy;
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

{ THTMLHandlerPage }

constructor THTMLHandlerPage.Create;
begin
  inherited Create (ctTextHTML);

end;

procedure THTMLHandlerPage.Flush;
begin
  Write (' ');

end;

end.

