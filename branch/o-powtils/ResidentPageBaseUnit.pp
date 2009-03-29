{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                    PSP 1.6.x BasaClassUnit

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

unit ResidentPageBaseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebUnit, CollectionUnit, XMLNode, AttributeUnit, Unix,
    BaseUnix, SessionManagerUnit, ResidentApplicationUnit, WebHeaderUnit,
    AbstractDispatcherUnit, CookieUnit, CgiVariableUnit;
  
type

  { TResidentPageBase }

  TResidentPageBase= class (TAbstractDispatcher)
  private

    function GetSession: TSession;

  protected

    procedure Clear;

  published

  public
    property Session: TSession read GetSession;

    constructor Create (ThisPageName: String; ContType: TContentType;
                       PageHost: String= ''; PagePath: String= '');
    destructor Destroy; override;

    procedure MyDispatch; virtual; abstract;
    
  end;
  
  { THTMLResidentPageBase }
  THTMLResidentPageBase= TResidentPageBase;
  
  { TXMLResidentPageBase }

  TXMLResidentPageBase= class (TResidentPageBase)
  private
    FXSLPath: String;
    FIndent: Boolean;

  protected
    FXMLRoot: TXMLNode;

  public
    property XSLPath: String read FXSLPath;

    constructor Create (XSLPage: String;
                ThisPageName: String; Encoding: String= 'UTF-8';
                Version: String= '1.0'; PageHost: String= '';
                PagePath: String= ''; Indent: Boolean= False);

    destructor Destroy; override;
    procedure Flush;

  end;

  { TBasePageCollection }

  TBasePageCollection= class (TBaseCollection)
  private
    function GetPage(Index: Integer): TResidentPageBase;
  public
    property Page [Index: Integer]: TResidentPageBase read GetPage;
    
  end;

implementation
uses
  ThisProjectGlobalUnit, DateUtils, GlobalUnit;
  
{ TResidentPageBase }

function TResidentPageBase.GetSession: TSession;
(*$I+*)
const
  SessionIDVarName: String= '';
(*$I-*)
var
  SessionIDInCookie: String;
  
begin
  if SessionIDVarName= '' then
    SessionIDVarName:= WebConfiguration.ConfigurationValueByName ['SessionIDVarName'];

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

constructor TResidentPageBase.Create (ThisPageName: String;
            ContType: TContentType; PageHost: String; PagePath: String);
begin
  inherited Create (ThisPageName, False);

  if WebConfiguration.ConfigurationByName ['SessionEnabled'].Value= 'YES' then
    FSessionID:= EmptySessionID;

end;

destructor TResidentPageBase.Destroy;
begin
  Clear;
  
  inherited;
  
end;

procedure TResidentPageBase.Clear;
begin
  if PipeIsAssigned then
  begin
    WriteHeaders;
    if Buffer.Count<> 0 then
      Write (Buffer.Text);

    FpClose (FPipeHandle);
    PipeIsAssigned:= False;
    
  end;

end;

{
procedure TResidentPageBase.Dispatch (CgiVar: Pointer);
begin
  MyDispatch (TCgiVariableCollection (CgiVar));
  
  
end;
}


{ TBasePageCollection }

function TBasePageCollection.GetPage (Index: Integer): TResidentPageBase;
begin
  Result:= Member [Index] as TResidentPageBase;
  
end;

{ TXMLResidentPageBase }

constructor TXMLResidentPageBase.Create ( XSLPage: String;
  ThisPageName: String; Encoding: String; Version: String;
  PageHost: String; PagePath: String; Indent: Boolean= False);
begin
  inherited Create (ThisPageName, ctTextXML,
                       PageHost, PagePath);

  FXSLPath:= XSLPage;
  FIndent:= Indent;
  
  FXMLRoot:= TXMLNode.Create (ThisPageName);
{  Header.AddHeader (TWebHeader.Create ('', '<?xml version="'+ Version+ '" encoding="'+ Encoding+ '" ?>'));
  Header.AddHeader (TWebHeader.Create ('', '<?xml-stylesheet href="'+ XSLPath+ '" type= "text/xsl" ?>'));
}
  Buffer.Add ('<?xml version="'+ Version+ '" encoding="'+ Encoding+ '" ?>');
  Buffer.Add ('<?xml-stylesheet href="'+ XSLPath+ '" type= "text/xsl" ?>');

end;

procedure TXMLResidentPageBase.Flush;
begin
  if FIndent then
    Write (FXMLRoot.ToStringWithIndent)
  else
    Write (FXMLRoot.ToStringWithOutIndent);

  inherited;

end;

destructor TXMLResidentPageBase.Destroy;
begin
  Flush;
  FXMLRoot.Free;
  
  inherited;
  
end;

end.

