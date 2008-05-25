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
    BaseUnix;
  
type

  { TResidentPageBase }

  TResidentPageBase= class (TWeb)
  protected
//    FPersistance: Boolean;
    FMainPipeFileName: String;
    FPageName: String;
    FPipeHandle: cInt;
    FTempPipeFilePath: String;
    PipeIsAssigned: Boolean;
    FRequestURI: String;
    FHostName: String;
    FForceToSendHeader: Boolean;

    procedure SetPipeFileName (const Filename: String);
    procedure WriteDirectlyToOutput (const S: String);
    procedure WriteToBuffer (const S: String);
    procedure WriteToPipe (const S: String);
    procedure WriteHeaders;

    procedure Clear;

  published
    property PipeFileName: String Write SetPipeFileName;

  public
    constructor Create (ThisPageName: String; ContType: TContentType= ctTextHTML;
                       PageHost: String= ''; PagePath: String= ''); virtual;
    destructor Destroy; override;

    procedure MyDispatch; virtual; abstract;
    procedure Flush;
    
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

    procedure Free; virtual;
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
  ThisProjectGlobalUnit;
  
{ TResidentPageBase }

procedure TResidentPageBase.SetPipeFileName (const Filename: String);
begin
  FPipeHandle:= FpOpen (Filename, O_WRONLY);

  WriteProcedure:= @WriteToPipe;
  PipeIsAssigned:= True;

end;

procedure TResidentPageBase.WriteDirectlyToOutput (const S: String);
  
begin
  if not HeaderCanBeSent then
  begin
{    WriteLn (FPipeHandle, Header.Text);
    WriteLn (Cookie.Text);
}

  end;
  
  WriteLn (FPipeHandle, S);
  HeaderCanBeSent:= False;

end;

procedure TResidentPageBase.WriteToBuffer (const S: String);
begin
  Buffer.Add (S);

end;

procedure TResidentPageBase.WriteToPipe (const S: String);
const
  NewLine: String= #10#10;

begin
  if HeaderCanBeSent then
    WriteHeaders;

  if 0< Self.Buffer.Count then
    WriteBuffer;

  if (S<> '') and HeaderCanBeSent then
  begin
    HeaderCanBeSent:= False;
    FpWrite (FPipeHandle, NewLine [1], 2);

  end;
  
  FpWrite (FPipeHandle, S [1], Length (S));

end;

procedure TResidentPageBase.WriteHeaders;
var
  S: String;

begin
  HeaderCanBeSent:= False;
  if Header.Size<> 0 then
  begin
    S:= Header.Text;
    Header.Clear;
    WriteProcedure (S);
    
  end;
  
  if Cookies.Size<> 0 then
  begin
    S:= Cookies.Text;
    Cookies.Clear;
    WriteProcedure (S);

  end;

  WriteProcedure (#10);
  WriteProcedure (#10);
  HeaderCanBeSent:= False;

end;

constructor TResidentPageBase.Create (ThisPageName: String;
            ContType: TContentType; PageHost: String; PagePath: String);
begin
  inherited CreateWithOutGetWebData (PageHost, PagePath, ThisPageName,
     GlobalObjContainer.WebConfiguration, ContType);

  PipeIsAssigned:= False;
  
end;

{
constructor TResidentPageBase.CreateXML (ContType: TContentType; XSLPage: String;
                ThisPageName: String; PageHost: String;
                PagePath: String);
begin
  inherited CreateWithOutGetWebData (PageHost, PagePath, ThisPageName, ContType);
  Header.Add ('<?xml-stylesheet href="'+ XSLPage+ '" type= "text/xsl?>');

  PipeIsAssigned:= False;


end;
}

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
      WriteProcedure (Buffer.Text);

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

procedure TResidentPageBase.Flush;
begin
//  fpFlush (FPipeHandle);??!!
  
end;

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
  Buffer.Add ('<?xml version="'+ Version+ '" encoding="'+ Encoding+ '" ?>');
  Buffer.Add ('<?xml-stylesheet href="'+ XSLPath+ '" type= "text/xsl" ?>');

end;

procedure TXMLResidentPageBase.Flush;
begin
  if FIndent then
    WriteProcedure (FXMLRoot.ToStringWithIndent)
  else
    WriteProcedure (FXMLRoot.ToStringWithOutIndent);

  inherited;

end;

procedure TXMLResidentPageBase.Free;
begin
  Flush;
  FXMLRoot.Free;
  
  inherited Free;
  
end;

end.
