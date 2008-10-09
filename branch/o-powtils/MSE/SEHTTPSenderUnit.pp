unit SEHTTPSenderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StreamUnit, IdHTTP;
  
type

  { THTTPSender }

  THTTPSender= class (TIDHTTP)
  private
  public
    constructor Create (UserAgentString: String); overload;
    constructor Create; overload;

    function Get (AURL: String): String;
    
  end;

  { TSEHTTPSender }

  TSEHTTPSender= class (THTTPSender)
  private
    FIDHTTP: TIdHTTP;
    UserAgentString: String;
    FQueryURL: String;
    FImageURL: String;
    FSearchEngineName: String;
    FSERank: Integer;
    AndOperator: String;
    OrOperator: String;
    ExactOperator: String;
    SiteOperator: String;
    IntitleOperator: String;
    InURLOperator: String;

  public
    constructor Create (AnStream: TStream);
    destructor Destroy; override;
    
    function Get (var AURL: String): String;
    
  end;
  
implementation
uses
  IdException;
  
{ TSEHTTPSender }

constructor TSEHTTPSender.Create (AnStream: TStream);
begin
  inherited Create;

  FSearchEngineName:= ReadLineFromStream (AnStream);
  UserAgentString:= ReadLineFromStream (AnStream);
  FQueryURL:= ReadLineFromStream (AnStream);
  FImageURL:= ReadLineFromStream (AnStream);
{  AndOperator:= ReadLineFromStream (AnStream);
  OrOperator:= ReadLineFromStream (AnStream);
  ExactOperator:= ReadLineFromStream (AnStream);
  SiteOperator:= ReadLineFromStream (AnStream);
  IntitleOperator:= ReadLineFromStream (AnStream);
  InURLOperator:= ReadLineFromStream (AnStream);
}
  FIDHTTP:= TIdHTTP.Create;
  FIDHTTP.Request.UserAgent:= UserAgentString;

end;

destructor TSEHTTPSender.Destroy;
begin
  FIDHTTP.Free;
  
  inherited Destroy;
  
end;

function TSEHTTPSender.Get (var AURL: String): String;
begin
  Result:= FIDHTTP.Get (AURL);

end;

{ THTTPSender }

constructor THTTPSender.Create (UserAgentString: String);
begin
  inherited Create;
  
  Self.Request.UserAgent:= UserAgentString;
  
end;

constructor THTTPSender.Create;
begin
  inherited Create;
  
end;

function THTTPSender.Get (AURL: String): String;
begin
  try
    Result:= inherited;
    
  except
    on e: EIdException do
      Result:= e.Message;
      
  end;
  
end;

end.

