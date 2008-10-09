unit SEThreadingUnit;

{$mode objfpc}{$H+}

interface

uses
  ThreadingUnit, Classes, SysUtils, IdHTTP, IdCookieManager, CollectionUnit,
    StreamUnit, SEResultParserUnit, SEHTTPSenderUnit;

type

  { TGetResultFromSEThread }

  TGetResultFromSEThread= class (TThread)
  private
    FResponseString: TStrings;
    FSearchEngineName: String;
    FURL: String;
    Parser: TSEParser;
    Sender: TSEHTTPSender;
    
   protected
     property SearchEngineName: String read FSearchEngineName;

     procedure Execute; override;

   public
     property URL: String read FURL write FURL;
     property ResponseString: TStrings read FResponseString write FResponseString;

    constructor Create (AnStream: TStream);
    destructor Destroy; override;

  end;

  { TThreadCollection }

  THTTPSenderThreadCollection= class (TBaseCollection)
  private
    function GetThread(Index: Integer): TThread;
    
  public
    property Thread [Index: Integer]: TThread read GetThread;
    constructor Create (n: Integer);

  end;
  
implementation
uses
  MyTypes;

{ THTTPSenderThreadCollection }

function THTTPSenderThreadCollection.GetThread (Index: Integer): TThread;
begin
  Result:= Member [Index] as TThread;
  
end;

constructor THTTPSenderThreadCollection.Create (n: Integer);
var
  Ptr: PObject;
  
begin
  inherited Create;
  
  Allocate (n);
end;

{ TGetResultFromSEThread }

procedure TGetResultFromSEThread.Execute;
begin
//  while True do
  begin
    ResponseString.Text:= Sender.Get (FURL);
    WriteLn ('Before Suspend!');
    Terminate;
    
  end;
  
end;

constructor TGetResultFromSEThread.Create (AnStream: TStream);
begin
  inherited Create (True);
  
  FSearchEngineName:= ReadLineFromStream (AnStream);
  Parser:= TSEParser.Create (AnStream);
  Sender:= TSEHTTPSender.Create (AnStream);
  
end;

destructor TGetResultFromSEThread.Destroy;
begin
  Parser.Free;
  Sender.Free;
  
  inherited Destroy;

end;

end.
