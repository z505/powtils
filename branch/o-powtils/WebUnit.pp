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
   - This unit is rewritten from web.pp (PSP 1.4.x). All the effort in this version
     was on made an object oriented design. There is nothing fixed till now.
     This unit for now, only works on the Linux. In Next step, me or someone else
     should add support on Windows.

  [14/Mar/2009- Amir]
    - There are some major changes in class structures.
  [07/Sep/2010- Amir]
    - Polishing the code

}

unit WebUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CookieUnit,
  WebHeaderUnit, CgiVariableUnit, SessionUnit,
  WebRunTimeInformationUnit, WebUploadedFileUnit;
  
type
  {
    There is no support for uploading file in O-PSP. This class was implemented but not tested!.
  }

  { TMPLine }
  // Line type for Multipart/Form-Data handling functions
  {
  TMPLine= class (TBaseFixSizeCollection)
  private
    function GetStrings (Index: Integer): String;
  public
    property Strings [Index: Integer]: String read GetStrings;
    procedure Insert (Index: Integer; Line: String);
    
    constructor Create;
    procedure Free;
  end;
  
  // Multipart/Form-Data form type
  TMPForm= TStringCollection;

  PMPForm= ^TMPForm;
}

  
  { TWeb }
{
  This class is collection of procedures/functions and variables in the web unit.
  This class is used as a parent class.
}

  TWeb= class (TObject)
  private
    FHostName: AnsiString;
    FPageName: AnsiString;
    FRequestURI: AnsiString;
    
    FCgi: TCgiVariableCollection;  // Both CGI GET/POST data
    FSession: TSessionCollection; // Session data
    FCookies: TCookieManager; // Cookie data
    FRti: TWebRunTimeInformationCollection;  // Run Time Information
    FHeaders: THeaderCollection;  // Headers

    FUploadedFile: TWebUpFileCollection;    // Uploaded files storage
    FHeaderCanBeSent, // True: iff the newline after headers is not sent
    FSessionIsRegistered: Boolean; // Session registered flag
    ErrorReporting, HaltOnError: Boolean;

//   {$IFDEF GZIP_ENABLED}
//      OutputBuffering, OutputCompression, // Output buffering and compression flags
//   {$ENDIF} Not implemented yet!

    FContentType: TContentType;
    Buffers: TStringList;

    procedure WriteToBuffer (const S: AnsiString);

  protected
    procedure WriteBuffer;
    procedure FlushBuffer;
{
    constructor CreateWithOutGetWebData (PageHostName, PagePath, ThisPageName: String;
                ContType: TContentType);

}
 published
    property PageName: AnsiString read FPageName write FPageName;
    property HostName: AnsiString read FHostName write FHostName;
    property RequestURI: AnsiString read FRequestURI write FRequestURI;
    property HeaderCanBeSent: Boolean read FHeaderCanBeSent write FHeaderCanBeSent; // Headers sent flag

 public
    property CgiVars: TCgiVariableCollection read FCgi;  // CGI GET/POST data
    property Cookies: TCookieManager read FCookies; // Cookie data
    property Header: THeaderCollection read FHeaders;  // Headers
    property Rti: TWebRunTimeInformationCollection read FRti;  //  Run Time Information
    property Session: TSessionCollection read FSession;// Session data
    property UploadedFile: TWebUpFileCollection read FUploadedFile;    // Uploaded files storage
    property Buffer: TStringList read Buffers;   // Output buffer
    property SessionIsRegistered: Boolean read FsessionIsRegistered; // Session registered flag
    property ContentType: TContentType read FContentType;

    procedure MyDispatch; virtual; abstract;

    destructor Destroy; override;
    
    procedure Write (const S: AnsiString); virtual;
    function ThrowWebError (const Message: AnsiString): Boolean;
    
  end;

implementation
uses
  GlobalUnit, ExceptionUnit,
  ThisProjectGlobalUnit;

{ TWeb }

// write all the data in buffer
procedure TWeb.WriteBuffer;
var
  S: AnsiString;
  
begin
  S:= Buffer.Text;
  Buffers.Clear;

  if S<> '' then
  begin
    Write (#10#10);
    FHeaderCanBeSent:= False;
    
  end;

  Write (S);


end;

// Flush the buffer's content
procedure TWeb.FlushBuffer;
begin
  WriteBuffer;

end;

// Doestn't print the S, but only add it to Buffer.
procedure TWeb.WriteToBuffer (const S: AnsiString);
begin
  Buffer.Add (S);
  
end;

// The same as TWeb.WriteDirectlyToOutput
procedure TWeb.Write (const S: AnsiString);
begin
  if not HeaderCanBeSent then
    WriteBuffer;
    
  if Self.Buffers.Count<> 0 then
    WriteBuffer;

  Write (S);
  
end;

function TWeb.ThrowWebError (const Message: AnsiString): Boolean;
//var
//  i: Integer;
//  s: AnsiString;

begin
  Result:= True;
  raise ENotImplementedYet.Create ('TWeb', 'ThrowWebError'+ Message);

  // Init
  //Result:= False;
  // Icrease ERRORs RTI
{   i:= GetRTIAsInt ('ERRORS');
  Inc (i);
  Str (i, s);

  SetRTI ('ERRORS', S);
  
  if not error_reporting then
    Exit (True);
    
  // Disable content encoding
  if IsWebHeader ('Content-Encoding') then
    UnsetWebHeader('Content-Encoding');
    
  // Send headers
  if not HeaderIsSent then
    FlushBuffer;
    
  // Adjusting error message
  WebWrite('<pre><b>PWU Error: <i>' + Message + '</i></b></pre>');
  // Halt
  
  if HaltOnError then
    Halt (0);
  // Done
  Result:= True;
  }
end;

// Free the web variables
destructor TWeb.Destroy;
begin
  Buffers.Free;

  FRti.Free;
  FCgi.Free;

  FCookies.Free;
  FHeaders.Free;
  FSession.Free;
  FUploadedFile.Free;
  
  inherited;
  
end;

{ TMPLine }
{
function TMPLine.GetStrings (Index: Integer): String;
begin
  Result:= String (((Self as TBaseFixSizeCollection).Member [Index]));
end;

procedure TMPLine.Insert (Index: Integer; Line: String);
begin
//  (Self as TBaseFixSizeCollection).Insert (Index, TObject (Line));

end;

constructor TMPLine.Create;
begin
  inherited create (6);
end;

procedure TMPLine.Free;
begin
  inherited;
end;
}

end.

