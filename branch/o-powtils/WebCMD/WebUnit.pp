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
}

unit WebUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit, WebStringUnit, sds;
  
const
  // Sets PWU configuration file name.
  PWU_CONFIG_PATH = 'PWU.conf';

  // Sets PSP configuration file name.
  PSP_CONFIG_PATH = 'PSP.conf';

  // Sets session file name
  PWU_SESS_PATH = 'PWUsess.sds';

  // System-wide configuration directory path on *NIX
  PWU_SYSCONF_PATH = '/etc/';

  // Supply PWU version
  PWU_VERSION = '1.0d';


var
  environ: ppchar; cvar; external;
  
type
  // Exceptions
  ENotImplementedYet= class (Exception);

  { ECookieNotFound }

  ECookieNotFound= class (Exception)
  public
    constructor Create;
    constructor Create (Msg: String);

  end;

  {
    TContentType which is now an enumeration. It can be String, too.
  }
  TContentType= (ctStart, ctTextHTML, ctTextXML, ctNode);
  
  { TEnvironment }

  TEnvironment= class (TNameValue)
  private
    function GetValue: String;
    
  public
    property Name: String read FName;
    property Value: String read GetValue;
    
    constructor Create (Nam, Val: String);
    destructor Destroy; override;
    
  end;
  
  { TWebEnvironmentCollection }
  {
    This class has some problem, it doesn't works on both linux and windows. Something
     should be done about the declaration of the class
  }
  TWebEnvironmentCollection= class (TNameValueCollection)
  private
    function GetEnvironmentByIndex(Index: Integer): TEnvironment;
    function GetEnvironmentByName(ValueName: String): TEnvironment;
    function GetEnvironmentValueByName(ValueName: String): String;
    
    {------------------------------------------------------------------------------}
    {--- SYSAPI FUNCTIONS ---------------------------------------------------------}
    {------------------------------------------------------------------------------}
    {$IFDEF WIN32}
      function GetEnvironmentStrings: pchar; stdcall; external 'kernel32' name 'GetEnvironmentStringsA';
      function FreeEnvironmentStrings(p: pchar) : longbool; stdcall; external 'kernel32' name 'FreeEnvironmentStringsA';
      function SetEnvironmentVariable(const lpszName, lpszValue: pchar): boolean; stdcall; external 'kernel32' name 'SetEnvironmentVariableA';
    {$ENDIF}

    {$IFDEF UNIX}
      function GetEnvironmentVariable (const Name: PChar): PChar; cdecl;
      function SetEnvironmentVariable (const Name, Value: PChar; Replace: Integer): Integer; cdecl;
      function UnsetEnvironmentVariable (const name: PChar): Integer; cdecl;
    {$ENDIF}


  public
    property EnvironmentValueByName [ValueName: String]: String read GetEnvironmentValueByName;
    property EnvironmentByName [ValueName: String]: TEnvironment read GetEnvironmentByName;
    property EnvironmentValue [Index: Integer]: TEnvironment read GetEnvironmentByIndex;

    function LoadEnvVar: Boolean;
    constructor Create;
    
    function ToString: String;
    
  end;

  { TWebConfiguration }

  TWebConfiguration= class (TNameValue)
  private
    function GetValue: String;
    
  public
    property Name: String read FName;
    property Value: String read GetValue;

    constructor Create (Nam, Val: String);
    destructor Destroy; override;


  end;

  { TWebConfigurationCollection }
  {
    This class loads the web configuration data from PWU_CONFIG_PATH file.
  }
  TWebConfigurationCollection= class (TNameValueCollection)
  private
    function GetConfigurationByIndex (Index: Integer): TWebConfiguration;
    function GetNameByName (ValueName: String): TWebConfiguration;
    function GetValueByName (Name: String): String;

  public
    property ConfigurationValueByName [Name: String]: String read GetValueByName;
    property ConfigurationByName [Name: String]: TWebConfiguration read GetNameByName;
    property Configuration [Index: Integer]: TWebConfiguration read GetConfigurationByIndex;

    function ParseWebConfig (CofigFilename: String): Boolean;
    procedure AddWebConfiguration (NewWebConfiguration: TWebConfiguration);
    
  end;

  { TWebRunTimeInformation }

  TWebRunTimeInformation= class (TNameValue)
  private
  
  public
    constructor Create (Nam, Val: String);
    destructor Destroy; override;
    
  end;
  { TWebRunTimeInformationCollection }
  {
    This class loads and hold the run time information.
  }

  TWebRunTimeInformationCollection= class (TNameValueCollection)
  private
    function GetRunTimeInformationByIndex(Index: Integer): TWebRunTimeInformation;
    function GetRunTimeInformationByName(Name: String): TWebRunTimeInformation;
  public
    property RunTimeInformation [Index: Integer]: TWebRunTimeInformation
             read GetRunTimeInformationByIndex;
    property RunTimeInformationByName [Name: String]: TWebRunTimeInformation
             read GetRunTimeInformationByName;

    procedure AddRunTimeInformation (NewRunTimeInformation: TWebRunTimeInformation);
    procedure Init (IsHeaderSent: Boolean);
    
  end;

  { TWebHeader }

  TWebHeader= class (TNameValue)
  private
    function GetName: String;
    function GetValue: String;

  public
    property Name: String read GetName;
    property Value: String read GetValue;

    constructor Create (Nam, Val: String);
    destructor Destroy; override;

    function ToString: String;

  end;

  { TWebHeaderCollection }
  {
    This class loads and hold the Header information.
  }
  TWebHeaderCollection= class (TNameValueCollection)
  private
    function GetText: String;
    function GetWebHeader (Index: Integer): TWebHeader;
    
  public
    property WebHeader [Index: Integer]: TWebHeader read GetWebHeader;

    property Text: String read GetText;

    procedure Init (Env: TWebEnvironmentCollection; Conf:
                         TWebConfigurationCollection; PageContentType: TContentType);

    procedure Clear;
    procedure AddHeader (NewHeader: TWebHeader); overload;

  end;

  { TCgiVar }

  TCgiVar= class (TNameValue)
  private
    function GetName: String;
    function GetValue: String;

  public
    property Name: String read GetName;
    property Value: String read GetValue;

    constructor Create (Nam, Val: String);
    destructor Destroy; override;

    function ToString: String;

  end;

  {
    This class loads and hold the cgi variables information.
  }

  { TCgiVariableCollection }

  TCgiVariableCollection= class (TNameValueCollection)
  private
    function GetCgiVar (Index: Integer): TCgiVar;
    function GetCgiVarByName (VarName: String): TCgiVar;
    function GetCgiVarValueByName (VarName: String): String;
    function GetText: String;

  public
    property CgiVar [Index: Integer]: TCgiVar read GetCgiVar;
    property CgiVarByName [VarName: String]: TCgiVar read GetCgiVarByName;
    property CgiVarValueByName [VarName: String]: String read GetCgiVarValueByName;

    property Text: String read GetText;

    procedure Clear (FreeObj: Boolean= True);
    procedure AddCgiVar (NewCgiVar: TCgiVar);

    procedure LoadFromString (Str: String);
    
  end;

  {
    Currently, there is no support for cookies in O-PSP (but there is full support in PSP.
  }

  { TCookie }

 TCookie= class (TNameValue)
 private
    PVal: PString;
    function GetValue: String;

  protected
    FDomain: String;
    FExpires: TDateTime;
    FPath: String;
    
  public
    property Domain: String read FDomain;
    property Path: String read FPath;
    property Name: String read FName;
    property Value: String read GetValue;
    property Expires: TDateTime read FExpires;
    
    constructor Create (CookieName, CookieValue: String;
        CookieExpireTime: TDateTime= 0; CookiePath: String= ''; CookieDomain: String= '');
    destructor Destroy; override;
    
    function ToString (Index: Integer= 0): String;
    
  end;

  { TCookieCollection }

  TCookieCollection= class (TNameValueCollection)
  private
    FHostName: String;
    FPageURI: String;
    FWebHeaderCollection: TWebHeaderCollection;
    FIsHeaderSent: PBoolean;
    function GetCookie (Index: Integer): TCookie;
    function GetCookieByName (Name: String): TCookie;
    function GetCookieValueByName(Name: String): String;
    function GetText: String;
    
  public
    property Cookie [Index: Integer]: TCookie read GetCookie;
    property CookieByName [Name: String]: TCookie read GetCookieByName;
    property CookieValueByName [Name: String]: String read GetCookieValueByName;
    property HostName: String read FHostName;
    property PageURI: String read FPageURI;
    property Text: String read GetText;
    
    constructor Create (HeaderCollection: TWebHeaderCollection; IsHeaderSent: PBoolean;
       ThisPageHostName, ThisPageURI: String);
    
    destructor Destroy; override;

    procedure Load (EnvironmentCollection: TWebEnvironmentCollection);
    procedure LoadFromString (CookieString: String);
    procedure WriteCookie;
    
    procedure Add (NewCookie: TCookie); overload;
    procedure Add (CookieName, CookieValue: String;
        CookieExpireTime: TDateTime= ''; CookiePath: String= ''; CookieDomain: String= ''); overload;
    function IsExists (Name: String): Boolean;
    procedure RemoveCookieByName (Name: String);
    
  end;
  
  { TSessionCollection }

  {
    This class loads and hold and save the session information.
  }
  TSessionCollection= class (TNameValueCollection)
  private
    FCookieCollection: TCookieCollection;
    FConfigurationCollection: TWebConfigurationCollection;
    FRunTimeInformation: TWebRunTimeInformationCollection;
    SdsEngine: TSDSWrapper;
    FSessionIsRegistered: Boolean;

    
    function SessionGarbageCollector: Boolean;
  public
    property SessionIsRegistered: Boolean read FSessionIsRegistered write FSessionIsRegistered;
    constructor Create (CookieCollection: TCookieCollection; ConfigurationCollection: TWebConfigurationCollection);
    destructor Destroy; override;
    function SessionStart: String;
  end;
  
  StrArray= array of string;


  {
    There is no suppory for uploading file in O-PSP. This class was implemented but not tested!.
  }

  { TWebUpFile }

  TUploadedFile= class (TObject)
  private
    FName, FFileName, FData, FContent_Type: string;
    Fsize: Integer;
    
  public
    property Size: Integer read FSize;
    property Name: String read FName;
    property FileName: String read FFileName;
    property Data: String read FData;
    property Content_Type: String read FContent_Type;
    
    constructor Create (_Size: Integer; _Name, _FileName, _Data, _Content_Type: String);
    
  end;

  { TWebUpFileCollection }

  TWebUpFileCollection= class (TBaseCollection)
  private
    function GetFiles(Index: Integer): TUploadedFile;
    
  public
    property Files [Index: Integer]: TUploadedFile read GetFiles;
    procedure Add (FileInfo: TUploadedFile);
    
  end;

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
{

}
  TWebWriteProcedure= procedure (const S: String) of Object;

  
  { TWeb }
{
  This class is collection of procedures/functions and variables in the web unit.
  This class is used as parent class.
}

  TWeb= class (TObject)
  private
    FHostName: String;
    FPageName: String;
    FRequestURI: String;
    
    FCgi: TCgiVariableCollection;  // Both CGI GET/POST data
    FEnvironment: TWebEnvironmentCollection;  // Enironment data
    FConfiguration: TWebConfigurationCollection; // Configuration data
    FSession: TSessionCollection; // Session data
    FCookies: TCookieCollection; // Cookie data
    FRti: TWebRunTimeInformationCollection;  // Run Time Information
    FHeaders: TWebHeaderCollection;  // Headers

    FUploadedFile: TWebUpFileCollection;    // Uploaded files storage
    FHeaderCanBeSent, // True: iff the newline after headers is not sent
    FSessionIsRegistered: Boolean; // Session registered flag
    ErrorReporting, HaltOnError: Boolean;

//   {$IFDEF GZIP_ENABLED}
//      OutputBuffering, OutputCompression, // Output buffering and compression flags
//   {$ENDIF} Not implemented yet!

    FContentType: TContentType;
    Buffers: TStringList;

    function GetWebData: Boolean;
    procedure WriteDirectlyToOutput (const S: String);
    procedure WriteToBuffer (const S: String);

  protected
    WriteProcedure: TWebWriteProcedure;

    procedure WriteBuffer;
    procedure FlushBuffer;

 published
    property PageName: String read FPageName write FPageName;
    property HostName: String read FHostName write FHostName;
    property RequestURI: String read FRequestURI write FRequestURI;
    property HeaderCanBeSent: Boolean read FHeaderCanBeSent write FHeaderCanBeSent; // Headers sent flag

 public
    property CgiVars: TCgiVariableCollection read FCgi;  // CGI GET/POST data
    property Conf: TWebConfigurationCollection read FConfiguration; // Configuration data
    property Cookies: TCookieCollection read FCookies; // Cookie data
    property Environment: TWebEnvironmentCollection read FEnvironment;  // Enironment data
    property Header: TWebHeaderCollection read FHeaders;  // Headers
    property Rti: TWebRunTimeInformationCollection read FRti;  //  Run Time Information
    property Session: TSessionCollection read FSession;// Session data
    property UploadedFile: TWebUpFileCollection read FUploadedFile;    // Uploaded files storage
    property Buffer: TStringList read Buffers;   // Output buffer
    property SessionIsRegistered: Boolean read FsessionIsRegistered; // Session registered flag
    property ContentType: TContentType read FContentType;

    procedure Dispatch; virtual; abstract;

    constructor Create (ContType: TContentType= ctTextHTML;
                  ForceToSendHeader: Boolean= False);
                  
    constructor CreateWithOutGetWebData (PageHostName, PagePath, ThisPageName: String;
                WebConf: TWebConfigurationCollection; ContType: TContentType= ctTextHTML);
    
    destructor Destroy; override;
    procedure Clear;
    
    procedure Write (const S: String);
    procedure SetCookies (CookieCollection: TCookieCollection);
    function ThrowWebError (const Message: String): Boolean;
    
  end;

var
  WebConfiguration: TWebConfigurationCollection;
  
implementation
uses
  SubStrings, Base64_Enc, URLEnc, MyTypes;

procedure WebWriteDirectlyToOutput1 (const S: String);
begin
end;

{ TWeb }

// write all the data in buffer
procedure TWeb.WriteBuffer;
var
  S: String;
  
begin
  S:= Buffer.Text;
  Buffers.Clear;

  if S<> '' then
  begin
    WriteProcedure (#10#10);
    FHeaderCanBeSent:= False;
    
  end;

  WriteProcedure (S);


end;

// Flush the buffer's content
procedure TWeb.FlushBuffer;
begin
  WriteBuffer;

end;

// It is responsible for getting GET, POST, COOKIE and SESSION data
function TWeb.GetWebData: Boolean;
var
  Method, Ctype,
  Data: String;
  
  Upl_Max_Size,
  Cont_Len, cnt: LongWord;
  
begin
  Result:= False;
  // First getting method data
  Method:= FEnvironment.EnvironmentValueByName ['REQUEST_METHOD'];
  
  if Method= 'POST' then
  begin
    // Getting data from stdin
    Data:= '';
    
    Val (FConfiguration.ConfigurationValueByName ['upload_max_size'], Upl_Max_Size);
    Upl_Max_Size:= Upl_Max_Size* 1048576;
    
    Val (FEnvironment.EnvironmentValueByName ['CONTENT_LENGTH'], Cont_Len);
    if Cont_Len> Upl_Max_Size then
      Cont_Len:= Upl_Max_Size;
      
    SetLength (Data, Cont_Len);
    
    for cnt:= 1 to Cont_Len do
      Read (Data [cnt]);
      
    // Depending on content type
    Ctype:= FEnvironment.EnvironmentValueByName ['CONTENT_TYPE'];

    if substr_pos (LowerCase (Ctype), 'application/x-www-form-urlencoded')> 0 then
//      FCgi.Add (Data)
    else if substr_pos (LowerCase (Ctype), 'multipart/form-data')> 0 then
    begin
//      PutCGImpVars(@data, ctype);
      raise ENotImplementedYet.Create ('multipart/form-data Support!');
    end;

  end
  else if Method= 'GET' then
  begin
    Data:= FEnvironment.EnvironmentValueByName ['QUERY_STRING'];
//    FCgi.Add (Data)

  end;

  // Get cookies
  if FEnvironment.IsExists ('HTTP_COOKIE') then
  begin//To Be Completed
    //Data:= FEnvironment.GetValueByName ('HTTP_COOKIE').ToString;
    //
  end;

  // Get session
  Data:= FSession.SessionStart;
  if Data<> '' then
//    FSession.Add (Data);
    
  // Done
  Result:= True;
end;

// Print the string S directly into output.
// If the buffer is not sent yet, it write the content of buffer
procedure TWeb.WriteDirectlyToOutput (const S: String);
begin
  if not HeaderCanBeSent then
    WriteBuffer;
    
  if Self.Buffers.Count<> 0 then
    WriteBuffer;

  Write (S);
  
end;

// Doestn't print the S, but only add it to Buffer.
procedure TWeb.WriteToBuffer (const S: String);
begin
  Buffer.Add (S);
  
end;

// The same as TWeb.WriteDirectlyToOutput
procedure TWeb.Write (const S: String);
begin
  if not HeaderCanBeSent then
    WriteBuffer;
    
  if Self.Buffers.Count<> 0 then
    WriteBuffer;

  Write (S);
  
end;

procedure TWeb.SetCookies (CookieCollection: TCookieCollection);
begin
  if FCookies<> nil then
    FCookies.Free;
    
  FCookies:= CookieCollection;
  
end;

function TWeb.ThrowWebError (const Message: String): Boolean;
//var
//  i: Integer;
//  s: String;

begin

  // Init
  Result:= False;
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

constructor TWeb.Create (ContType: TContentType;
                 ForceToSendHeader: Boolean);

begin
  inherited Create;
  
  raise Exception.Create ('This constructor should not be called');

  FContentType:= ContType;
  
  if ForceToSendHeader then
  begin
    WriteBuffer;
    WriteProcedure:= @WriteDirectlyToOutput;
    
  end
  else
    WriteProcedure:= @WriteToBuffer;

  FHeaderCanBeSent:= True;// May be used for writing cookies

{--Creating all the variable--
  This can be done on the first use of each variable
}
  FRti:= TWebRunTimeInformationCollection.Create;
  FCgi:= TCgiVariableCollection.Create;
  FConfiguration:= TWebConfigurationCollection.Create;
  FEnvironment:= TWebEnvironmentCollection.Create;
  FHeaders:= TWebHeaderCollection.Create;
  FCookies:= TCookieCollection.Create (FHeaders, @FHeaderCanBeSent, PageName,
               PageName);
               
  FSession:= TSessionCollection.Create (FCookies, FConfiguration);
  
//  FUploadedFile:= TWebUpFileCollection.Create;//??!!

  FRti.Init (HeaderCanBeSent);
  
  if not FEnvironment.LoadEnvVar then
  begin
    Write ('<pre><b>');
    Write ('PWU Error: <i>Could not get program environment.</i></b></pre>');
    FlushBuffer;
    Halt;
    
  end;
  
  if LowerCase (FConfiguration.ConfigurationValueByName ['error_reporting'])= 'on' then
    ErrorReporting:= True
  else
    ErrorReporting:= False;
  
  if LowerCase (FConfiguration.ConfigurationValueByName ['error_halt']) = 'on' then
    HaltOnError:= True
  else
    HaltOnError:= False;

  // Initialize the main headers since now that there aren't any above errors
  FHeaders.Init (FEnvironment, FConfiguration, FContentType);

end;

//This constructor is called by a ResientPageBaseUnit
//which doesn't need to read the parameters from standard input
constructor TWeb.CreateWithOutGetWebData (PageHostName, PagePath, ThisPageName: String;
   WebConf: TWebConfigurationCollection; ContType: TContentType);
   
begin
  inherited Create;

  FContentType:= ContType;
  Buffers:= TStringList.Create;
  FHeaders:= TWebHeaderCollection.Create;
  FHeaderCanBeSent:= True;

{--Creating all the variable--
  This can be done on the first use of each variable
}
  FRti:= TWebRunTimeInformationCollection.Create;
  FConfiguration:= TWebConfigurationCollection.Create;
  FCookies:= TCookieCollection.Create (Header, @FHeaderCanBeSent, PageHostName, PagePath);
  FCgi:= TCgiVariableCollection.Create;
  
  FEnvironment:= TWebEnvironmentCollection.Create;
  FSession:= TSessionCollection.Create (FCookies, FConfiguration);
//  FUploadedFile:= TWebUpFileCollection.Create;//??!!

  FRti.Init (HeaderCanBeSent);

  if not FEnvironment.LoadEnvVar then
  begin
    WriteProcedure ('<pre><b>');
    WriteProcedure ('PWU Error: <i>Could not get program environment.</i></b></pre>');
    FlushBuffer;
    Halt;
    
  end;

  FConfiguration:= WebConf;

  if LowerCase (FConfiguration.ConfigurationValueByName ['error_reporting'])= 'on' then
    ErrorReporting:= True
  else
    ErrorReporting:= False;

  if LowerCase (FConfiguration.ConfigurationValueByName ['error_halt']) = 'on' then
    HaltOnError:= True
  else
    HaltOnError:= False;

  // Initialize the main headers since now that there aren't any above errors
    FHeaders.Init (FEnvironment, FConfiguration, FContentType);
  
end;

// Free the web variables
destructor TWeb.Destroy;
begin
  Buffers.Free;

  FRti.Free;
  FCgi.Free;

//  FConfiguration.Free;
  FCookies.Free;
  FEnvironment.Free;
  FHeaders.Free;
  FSession.Free;
//  FUploadedFile.Free;
  
  inherited;
  
end;

procedure TWeb.Clear;
begin


end;

{ TUploadedFile }

constructor TUploadedFile.Create(_Size: Integer; _Name, _FileName, _Data,
  _Content_Type: String);
begin
  inherited Create;
  
  Fsize:= _Size;
  FName:= _Name;
  FFileName:= _FileName;
  FData:= _Data;
  FContent_Type:= _Content_Type;
  
end;

{ Web_TUpFiles }

function TWebUpFileCollection.GetFiles (Index: Integer): TUploadedFile;
begin
  Result:= Member [Index] as TUploadedFile;
  
end;

procedure TWebUpFileCollection.Add (FileInfo: TUploadedFile);
begin
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
{ TWebEnvironmentCollection }

{$IFDEF UNIX}
//  TWebEnvironmentCollection.environ cvar; external;
function TWebEnvironmentCollection.GetEnvironmentVariable (const Name: PChar): PChar; cdecl;external 'c' name 'getenv';
function TWebEnvironmentCollection.SetEnvironmentVariable (const Name, Value: PChar; Replace: Integer): Integer; cdecl; external 'c' name 'setenv';
function TWebEnvironmentCollection.UnsetEnvironmentVariable (const name: PChar): Integer; cdecl; external 'c' name 'unsetenv';
{$ENDIF}

{$IFDEF WIN32}
function TWebEnvironmentCollection.GetEnvironmentStrings: pchar; stdcall; external 'kernel32' name 'GetEnvironmentStringsA';
function TWebEnvironmentCollection.FreeEnvironmentStrings (p: pchar) : longbool; stdcall; external 'kernel32' name 'FreeEnvironmentStringsA';
function TWebEnvironmentCollection.SetEnvironmentVariable (const lpszName, lpszValue: pchar): boolean; stdcall; external 'kernel32' name 'SetEnvironmentVariableA';
{$ENDIF}

//{$ENDIF}

function TWebEnvironmentCollection.GetEnvironmentByIndex(Index: Integer
  ): TEnvironment;
begin
  Result:= NameValue [Index] as TEnvironment;
  
end;

function TWebEnvironmentCollection.GetEnvironmentByName(ValueName: String
  ): TEnvironment;
begin
  Result:= NameValueByName [ValueName] as TEnvironment;
  
end;

function TWebEnvironmentCollection.GetEnvironmentValueByName(ValueName: String
  ): String;
begin
  try
    Result:= EnvironmentByName [ValueName].Value;
    
  except
    on e: ENameNotFound do
      Result:= 'Not Found!';
      
  end;
end;

// Loads up system environment
function TWebEnvironmentCollection.LoadEnvVar: Boolean;
var
{$IFDEF WIN32}
    p, hp: pchar;
{$ENDIF}
{$IFDEF UNIX}
    p: ppchar;
{$ENDIF}
    s: string;
    i: longint;
    NewVar: TEnvironment;
    
begin
  Result:= True;

  {$IFDEF WIN32}
  p:= GetEnvironmentStrings;
  hp:= p;
  while hp^ <> #0 do
  {$ENDIF}
  
  {$IFDEF UNIX}
  p:= Environ;
  while (p^ <> nil) and (p^ <> #0) do
  {$ENDIF}
  
  begin
    {$IFDEF WIN32}s:= AnsiString(hp);{$ENDIF}
    {$IFDEF UNIX}s:= AnsiString(p^);{$ENDIF}
    i:= substr_pos(s, '=');
    NewVar:= TEnvironment.Create (UpCase (Copy (s, 1, i - 1)), Copy (s, i+ 1, Length (s)- i));
    Self.Add (NewVar);
    
    // Next entry
    {$IFDEF WIN32}hp:= hp + strlen(hp) + 1;{$ENDIF}
    {$IFDEF UNIX}inc(p);{$ENDIF}
  end;
  {$IFDEF WIN32}
  FreeEnvironmentStrings (p);
  {$ENDIF}
  
  Result:= True;
  
end;

constructor TWebEnvironmentCollection.Create;
begin
  inherited Create;
  
end;

function TWebEnvironmentCollection.ToString: String;
begin
  Result:= '';

  raise ENotImplementedYet.Create ('TWebEnvironmentCollection.ToString');
  
end;

{ TWebConfigurationCollection }

function TWebConfigurationCollection.GetConfigurationByIndex (Index: Integer):
                                                             TWebConfiguration;
begin
  Result:= NameValue [Index] as TWebConfiguration;
  
end;

function TWebConfigurationCollection.GetNameByName (ValueName: String): TWebConfiguration;
begin
  Result:= NameValueByName [ValueName] as TWebConfiguration;
  
end;

function TWebConfigurationCollection.GetValueByName (Name: String): String;
begin
  try
    Result:= (PString(NameValueByName [Name].Value ))^;
    
  except
    on e: ENameNotFound do
      Result:= 'Not Found!';
    
  end;
  
end;

// Finds the configuration file and parses it
function TWebConfigurationCollection.ParseWebConfig (CofigFilename: String): Boolean;
var
  CONFIG_PATH, SYS_PATH,
  Buff, Name, Value: string;
  FileHandle: TextFile;
  i: Integer;
  NewVariable: TWebConfiguration;
  
begin
  Result:= False;
  
  if not FileExists (CofigFilename) then
    Exit;
    
  Result:= True;
  
  // Opening
  AssignFile (FileHandle, CofigFilename);
  Reset (FileHandle);

  // Parsing
  while not EOF (FileHandle) do
  begin
    Readln (FileHandle, Buff);
    // Emtpy lines are ignored
    if Buff= '' then
      Continue;
      
    // All comment lines start with # only
    if Buff [1]= '#' then
      Continue;
      
    i:= SubStr_Pos (Buff, '=');
    Name:= Copy (Buff, 1, i- 1);
    Value:= Copy (Buff, i+ 1, Length (Buff)- i);
    Name:= Str_Trim (Name);
    value:= SubStr_Strip (Str_Trim (Value), '"');
    
    if (Name= '') or (Value= '') then
      Continue;
      
    NewVariable:= TWebConfiguration.Create (Name, Value);
    Self.AddWebConfiguration (NewVariable);
    
  end;
  CloseFile (FileHandle);

  // Setting program flags

  {$IFDEF GZIP_ENABLED}
  if LowerCase (GetWebConfigVar ('output_buffering')) = 'on' then
    output_buffering:= True
  else
    output_buffering:= False;
    
  if LowerCase (GetWebConfigVar ('output_compression')) = 'on' then
    output_compression:= True
  else
    output_compression:= False;
  {$ENDIF}

  Result:= True;
  
end;

procedure TWebConfigurationCollection.AddWebConfiguration(
  NewWebConfiguration: TWebConfiguration);
begin
  inherited Add (NewWebConfiguration);
  
end;


{ TWebHeaderCollection }

function TWebHeaderCollection.GetText: String;
var
  HeaderPtr: ^TWebHeader;
  i: Integer;
  
begin
  Result:= '';
  HeaderPtr:= GetPointerToFirst;
  
  if 0< FSize then
    Result:= HeaderPtr^.ToString;
    
  for i:= 1 to FSize- 1 do
  begin
    Inc (HeaderPtr);
    Result:= Result+ #10+ HeaderPtr^.ToString;
    
  end;
    
end;

function TWebHeaderCollection.GetWebHeader (Index: Integer): TWebHeader;
begin
  Result:= NameValue [Index] as TWebHeader;
  
end;

procedure TWebHeaderCollection.Init (Env: TWebEnvironmentCollection;
                                            Conf: TWebConfigurationCollection;
                                            PageContentType: TContentType);
var
  HeaderVar: TWebHeader;
  ContentTypeValue: String;
  
begin
  HeaderVar:= TWebHeader.Create ('<Powered-By', 'OPWU/' + PWU_VERSION+ '>');
  Self.AddHeader (HeaderVar);

  ContentTypeValue:= ' text/';
  case PageContentType of
    ctTextHTML: ContentTypeValue:= ContentTypeValue+ 'html;';
    ctTextXML: ContentTypeValue:= ContentTypeValue+ 'xml;';
    
  end;
  
  ContentTypeValue:= ContentTypeValue+ ' charset=' + Conf.ConfigurationValueByName ['Header_Charset'];
  HeaderVar:= TWebHeader.Create ('Content-Type', ContentTypeValue);
  Self.AddHeader (HeaderVar);

{
 {$IFDEF GZIP_ENABLED}
  if output_buffering
    and output_compression
      and substr_exists(GetEnvVar('HTTP_ACCEPT_ENCODING'), 'gzip') then
  begin
    SetLength(hdr, 3);
    hdr[2].name:= 'Content-Encoding';
    hdr[2].value:= 'gzip';
  end;
 {$ENDIF GZIP_ENABLED}
}

end;

procedure TWebHeaderCollection.Clear;
var
  WebHeaderPtr: ^TWebHeader;
  i: Integer;
  
begin
  WebHeaderPtr:= GetPointerToFirst;
  
  for i:= 0 to Size- 1 do
  begin
    WebHeaderPtr^.Free;
    Inc (WebHeaderPtr);
    
  end;
  
  inherited Clear;
  
end;

procedure TWebHeaderCollection.AddHeader (NewHeader: TWebHeader);
begin
  inherited Add (NewHeader);
  
end;

{==============================================================================}
{================================   Types   ===================================}
{==============================================================================}
type
  SDS_Result = Pointer;
  SDS_Array = Pointer;
  SDS_ColumnInfo = Pointer;
  
{ TSessionCollection }

// Session garbage collector
function TSessionCollection.SessionGarbageCollector: Boolean;
var
  SessionTable,
  SessionLimit: String;
  Res: SDS_Result;
  SessionTime: LongWord;
  LimitDate: TDateTime;
  
begin
  Result:= False;
  
  // Checking
  SessionTable:= FConfigurationCollection.ConfigurationByName ['session_path'].Value;
  
  if not FileExists (SessionTable) then
  begin
    // Searching in system temp
    if FileExists({$IFDEF WIN32}GetEnvVar('WINDIR') + '\'{$ENDIF}{$IFDEF UNIX}'/tmp/'{$ENDIF}+ PWU_SESS_PATH) then
      SessionTable:= {$IFDEF WIN32}GetEnvVar('WINDIR') + '\'{$ENDIF}{$IFDEF UNIX}'/tmp/'{$ENDIF}+ PWU_SESS_PATH
    else
      Exit (False);
  end;
  
  // Checking lifetime in minutes
//  SessionTime:= FConfigurationCollection.NameValueByName ['session_life_time'].Value.ToInt;
  if SessionTime= 0 then
    Exit (True);
    
  LimitDate:= Now- (SessionTime div 1440);
  
  SessionLimit:= FormatDateTime ('yyyy-mm-dd hh:nn:ss', LimitDate);

  // Performing GC
  Res:= SdsEngine.Query ('DELETE FROM `'+ SessionTable+ '` WHERE modified < "'+ SessionLimit+ '"');
  
  if SdsEngine.Result_Error (Res)= '' then
    Result:= True;

  SdsEngine.Free_Result (Res);

end;

constructor TSessionCollection.Create (CookieCollection: TCookieCollection;
  ConfigurationCollection: TWebConfigurationCollection);

begin
  inherited Create;

  FCookieCollection:= CookieCollection;
  FConfigurationCollection:= ConfigurationCollection;
  SdsEngine:= TSDSWrapper.Create;
  
end;

destructor TSessionCollection.Destroy;
begin
  SdsEngine.Free;

  inherited;
  
end;

function TSessionCollection.SessionStart: String;
var
  SessionTable: String;
  Res: SDS_Result;
  Row: SDS_Array;
  Key, SessionID: string;
begin

  // Init
  Result:= '';
  
  // Checking path
  SessionTable:= FConfigurationCollection.ConfigurationByName ['session_path'].Value;
  
  if not FileExists (SessionTable) then
  begin
    // Searching in system temp
    if FileExists ({$IFDEF WIN32}GetEnvVar('WINDIR') + '\'{$ENDIF}{$IFDEF UNIX}'/tmp/'{$ENDIF}+ PWU_SESS_PATH) then
      SessionTable:= {$IFDEF WIN32}GetEnvVar('WINDIR') + '\'{$ENDIF}{$IFDEF UNIX}'/tmp/'{$ENDIF}+ PWU_SESS_PATH
    else
      Exit ('');
  end;
  
  // Running garbage collector
  SessionGarbageCollector;

  // Is it registered
  if not FCookieCollection.IsExists ('PWUSESS') then
  begin
    FSessionIsRegistered:= False;
    FRunTimeInformation.Add (TWebRunTimeInformation.Create ('SESSION_REGISTERED', 'FALSE'));
    Exit ('');
  end;
  FSessionIsRegistered:= True;
  
  FRunTimeInformation.AddRunTimeInformation (TWebRunTimeInformation.Create ('SESSION_REGISTERED', 'TRUE'));
  
  Key:= base64_decode (FCookieCollection.GetCookieByName ('PWUSESS').Value);
  SessionID:= sds_escape (Copy (key, 13, Length (Key)- 12));
  Key:= sds_escape (Copy (Key, 1, 12));
  
  // Selecting
  Res:= SdsEngine.Query ('SELECT data FROM `'+ SessionTable+ '` WHERE id = '+ SessionID+ ' AND key = "'+ Key+ '"');
  
  if sds_result_rows(Res) = 1 then
  begin
    row:= sds_fetch_row (Res);
    Result:= base64_decode (sds_fetch_column (Row, 0));
    SdsEngine.Free_Row (Row);
  end
  else
  begin
    Result:= '';
    // Unset the cookie, it has timed out
    FCookieCollection.RemoveCookieByName ('PWUSESS');
  end;
  sds_free_result (Res);
end;

{ TWebRunTimeInformationCollection }

function TWebRunTimeInformationCollection.GetRunTimeInformationByIndex(
  Index: Integer): TWebRunTimeInformation;
  
begin
  Result:= NameValue [Index] as TWebRunTimeInformation;
  
end;

function TWebRunTimeInformationCollection.GetRunTimeInformationByName(
  Name: String): TWebRunTimeInformation;
  
begin
  Result:= NameValueByName [Name] as TWebRunTimeInformation;
end;

procedure TWebRunTimeInformationCollection.AddRunTimeInformation(
  NewRunTimeInformation: TWebRunTimeInformation);
begin
  inherited;
  
end;

procedure TWebRunTimeInformationCollection.Init (IsHeaderSent: Boolean);
var
  NewRTIVar: TWebRunTimeInformation;
  
begin
  NewRTIVar:= TWebRunTimeInformation.Create ('HEADERS_SENT', BoolToStr (IsHeaderSent));
  Self.Add (NewRTIVar);
  NewRTIVar:= TWebRunTimeInformation.Create ('ERRORS', '0');
  Self.Add (NewRTIVar);
  
end;

{ TCookieCollection }

function TCookieCollection.GetCookie (Index: Integer): TCookie;
begin
  Result:= Member [Index] as TCookie;
  
end;

function TCookieCollection.GetText: String;
var
  i: Integer;
  Ptr: PObject;
  
begin
  Result:= '';
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to FSize do
  begin
    Result:= Result+ 'Set-Cookie:'+ TCookie (Ptr^).ToString (1)+ #10;
    Inc (Ptr);
    
  end;
    
end;

constructor TCookieCollection.Create (HeaderCollection: TWebHeaderCollection; IsHeaderSent: PBoolean;
       ThisPageHostName, ThisPageURI: String);
begin
  inherited Create;
  
  FIsHeaderSent:= IsHeaderSent;
  FWebHeaderCollection:= HeaderCollection;
  FHostName:= ThisPageHostName;
  FPageURI:= ThisPageURI;
  
end;

destructor TCookieCollection.Destroy;
begin
  FIsHeaderSent:= nil;
  
  inherited;
  
end;

procedure TCookieCollection.Load (EnvironmentCollection: TWebEnvironmentCollection);
var
  HTTPCOOKIENameValue: TNameValue;
  DataStr: String;

begin
  raise ENotImplementedYet.Create ('TCookieCollection.Load');
  
  HTTPCOOKIENameValue:= EnvironmentCollection.NameValueByName ['HTTP_COOKIE'];
  
  if HTTPCOOKIENameValue<> nil then
  begin
//    DataStr:= HTTPCOOKIENameValue.Value.ToString;
    WriteLn (DataStr);
    // To be Worked on!!??!!
    {
    // Init
    i:= 1;
    cnt:= 0;
    len:= length(data);
    if data[1] = '\' then inc(i);
    // Parse out
    while (i <= len) do
    begin
      // New item
      SetLength(cook, cnt + 1);
      // Getting name
      lex:= '';
      while (i <= len) and (data[i] <> '=') do
      begin
        SetLength(lex, length(lex) + 1);
        lex[length(lex)]:= data[i];
        inc(i);
      end;
      cook[cnt].name:= url_decode(lex);
      inc(i);
      // Getting value
      lex:= '';
      while (i <= len) and (data[i] <> ';') do
      begin
        SetLength(lex, length(lex) + 1);
        lex[length(lex)]:= data[i];
        inc(i);
      end;
      cook[cnt].value:= url_decode(lex);
      inc(i);
      // Ignoring spaces
      while (i <= len) and (data[i] = ' ') do
        inc(i);
      // Increasing counter
      inc(cnt);
    end;
    // Done
    result:= true;
     }
  end;
end;

procedure TCookieCollection.LoadFromString (CookieString: String);
var
  LastNamePos, LastValuePos, Len: Integer;
  Name, Value: String;
  CurrentCookie: TCookie;
  
begin
  Len:= Length (CookieString);
  if Len= 0 then
    Exit;
  if CookieString [Len]<> ';' then
    CookieString:= CookieString+ ';';
    
  LastNamePos:= 0;
  LastValuePos:= 0;
  if CookieString [1]= '\' then
    Inc (LastValuePos);
  // Parse out
  
  while True do
  begin

    // Getting name
    LastNamePos:= Pos ('=', CookieString);
    if LastNamePos= 0 then
      Break;
      
    Name:= Copy (CookieString, 1, LastNamePos- 1);
    System.Delete (CookieString, 1, LastNamePos);

    // Getting value
    LastValuePos:= Pos (';', CookieString);
    Value:= Copy (CookieString, 1, LastValuePos- 1);
    System.Delete (CookieString, 1, LastValuePos);
    
    CurrentCookie:= TCookie.Create (Name, Value);
    
    Self.Add (CurrentCookie);
    
  end;
  
end;

procedure TCookieCollection.WriteCookie;
var
  i: Integer;

begin
  for i:= 0 to FSize- 1 do
    FWebHeaderCollection.Add (TWebHeader.Create ('Set-Cookie',
    URLEncode (Cookie [i].Name)+ '='+ URLEncode (Cookie [i].Value)+
     ';path='+ Cookie [i].Path+ ';domain='+ Cookie [i].Domain+
     ';expires=' + DateTimeToStr (Cookie [i].Expires)));

end;

procedure TCookieCollection.Add (NewCookie: TCookie);
begin
  inherited Add (NewCookie);
  
end;

procedure TCookieCollection.Add (CookieName, CookieValue: String;
  CookieExpireTime: TDateTime; CookiePath: String; CookieDomain: String);
var
  Ptr: PObject;
  i: Integer;
  NewCookie: TCookie;
  
begin
  Ptr:= GetPointerToFirst;
  NewCookie:= nil;
  
  for i:= 1 to Size do
  begin
    if TCookie (Ptr^).Name= CookieName then
    begin
      NewCookie:= TCookie (Ptr^);
      NewCookie.PVal^:= CookieValue;
      NewCookie.FExpires:= CookieExpireTime;
      
    end;
    
    Inc (Ptr);
    
  end;
  
  if NewCookie= nil then
  begin
    NewCookie:= TCookie.Create (CookieName, CookieValue,
      CookieExpireTime,
       CookiePath, CookieDomain);
    Add (NewCookie);
    
  end;
  
end;

function TCookieCollection.IsExists (Name: String): Boolean;
var
  i: Integer;
  Ptr: PObject;
  
begin
  Ptr:= GetPointerToFirst;
  Result:= True;

  for i:= 1 to Size do
  begin
    if (TCookie (Ptr^)).Name= Name then
      Exit;
    Inc (Ptr);

  end;

  Result:= False;
  
end;

function TCookieCollection.GetCookieByName (Name: String): TCookie;
var
  i: Integer;
  Ptr: PObject;

begin
  Ptr:= GetPointerToFirst;

  for i:= 1 to Size do
  begin
    if (TCookie (Ptr^)).Name= Name then
    begin
      Result:= TCookie (Ptr^);
      Exit;
      
    end;
    
    Inc (Ptr);

  end;

  raise ECookieNotFound.Create (Name);

end;

function TCookieCollection.GetCookieValueByName (Name: String): String;
var
  i: Integer;
  Ptr: PObject;

begin
  Ptr:= GetPointerToFirst;

  for i:= 1 to Size do
  begin
    if (TCookie (Ptr^)).Name= Name then
    begin
      Result:= TCookie (Ptr^).Value;
      Exit;

    end;

    Inc (Ptr);

  end;

  Result:= '';


end;

procedure TCookieCollection.RemoveCookieByName(Name: String);
var
  i: Integer;

begin
  if not Self.IsExists (Name) then
    raise ECookieNotFound.Create (Name+ 'does not exist');

  for i:= 0 to Size- 1 do
    if Cookie [i].Name= Name then
    begin
      Cookie [i].Free;
      Delete (i);
      Break;
    end;

end;


{ TCookie }

function TCookie.GetValue: String;
begin
  Result:= PVal^;

end;

constructor TCookie.Create(CookieName, CookieValue: String;
  CookieExpireTime: TDateTime; CookiePath: String; CookieDomain: String);
begin
  New (PVal);
  PVal^:= CookieValue;

  inherited Create (CookieName, TObject (PVal));

  FName:= CookieName;
  FValue:= TWebString.Create (CookieValue);
  FDomain:= CookieDomain;
  FExpires:= CookieExpireTime;
  FPath:= CookiePath;
  
end;

destructor TCookie.Destroy;
begin
  FValue.Free;

  inherited Free;
  
end;

function TCookie.ToString (Index: Integer): String;
begin
  case Index of
    0:
      Result:= FDomain+ ':'+ FormatDateTime ('ddd,dd-mmm-yyyy hh:nn:ss', FExpires)+ ' PDT:'+ FName+ ':'+ Value+ ':'+ FPath;
    1:
      Result:=  URLEncode (FName)+ '='+ Value+ ';path='+ FPath+ ';expires='
      + FormatDateTime ('ddd,dd-mmm-yyyy hh:nn:ss', FExpires)+ ' PDT';
      
  end;
  
end;


{ TWebHeader }

function TWebHeader.GetName: String;
begin
  Result:= FName;
  
end;

function TWebHeader.GetValue: String;
begin
  Result:= (PString (PByte (FValue)))^;

end;

constructor TWebHeader.Create (Nam, Val: String);
var
  PVal: PString;

begin
  New (PVal);
  PVal^:= Val;

  inherited Create (Nam, TObject (PVal));

end;

destructor TWebHeader.Destroy;
begin
  Dispose (PString (FValue));
  FValue:= nil;
  
  inherited;
  
end;

function TWebHeader.ToString: String;
begin
  Result:= Name+ ':'+ Value;
  
end;

{ TWebRunTimeInformation }

constructor TWebRunTimeInformation.Create (Nam, Val: String);
var
  PVal: PString;

begin
  New (PVal);
  PVal^:= Val;

  inherited Create (Nam, TObject (PVal));

end;

destructor TWebRunTimeInformation.Destroy;
begin
  Dispose (PString (FValue));
  FValue:= nil;
  
  inherited;
  
end;

{ TEnvironment }

function TEnvironment.GetValue: String;
begin
  Result:= (PString (PByte (FValue)))^;
  
end;

constructor TEnvironment.Create (Nam, Val: String);
var
  PVal: PString;
  
begin
  New (PVal);
  PVal^:= Val;
  
  inherited Create (Nam, TObject (PVal));
  
end;

destructor TEnvironment.Destroy;
begin
  dispose (PString (FValue));
  FValue:= nil;
  
  inherited;
  
end;

{ TWebConfiguration }

function TWebConfiguration.GetValue: String;
begin
  Result:= (PString (PByte (FValue)))^;
  
end;

constructor TWebConfiguration.Create (Nam, Val: String);
var
  PVal: PString;

begin
  New (PVal);
  PVal^:= Val;

  inherited Create (Nam, TObject (PVal));

end;

destructor TWebConfiguration.Destroy;
begin
  Dispose (PString (FValue));
  FValue:= nil;
  
  inherited Free;
  
end;

{ TCgiVariableCollection }

function TCgiVariableCollection.GetCgiVar (Index: Integer): TCgiVar;
begin
  Result:= NameValue [Index] as TCgiVar;
  
end;

function TCgiVariableCollection.GetCgiVarByName (VarName: String): TCgiVar;
begin
  Result:= NameValueByName [VarName] as TCgiVar;
  
end;

function TCgiVariableCollection.GetCgiVarValueByName (VarName: String): String;
begin
  try
    Result:= (NameValueByName [VarName] as TCgiVar).Value;
    
  except
    on e: ENameNotFound do
      Result:= '';
      
  end;
  
end;

function TCgiVariableCollection.GetText: String;
var
  i: Integer;
  
begin
  Result:= '';
  for i:= 0 to Size- 1 do
    Result:= Result+ CgiVar [i].ToString+ ';';
    
end;

procedure TCgiVariableCollection.Clear (FreeObj: Boolean);
var
  i: Integer;
  
begin
  if FreeObj then
    for i:= 0 to Size- 1 do
      CgiVar [i].Free;
      
  inherited Clear;
  
end;

procedure TCgiVariableCollection.AddCgiVar (NewCgiVar: TCgiVar);
begin
  try
    Add (NewCgiVar as TNameValue);
    
  except
    on e: ENameNotFound do
    begin
      RemoveValueByName (NewCgiVar.Name);
      AddCgiVar (NewCgiVar);
      
    end;
    
  end;
  
end;

procedure TCgiVariableCollection.LoadFromString (Str: String);
var
  VarName, VarValue: String;
  PCh, PEndChar: PChar;

begin
  if Length (Str)= 0 then
    Exit;

  PCh:= @Str [1];
  Dec (PCh);
  PEndChar:= @Str [Length (Str)];
  
  while PCh<> PEndChar do
  begin
    VarName:= '';
    VarValue:= '';
    
    while PCh<> PEndChar do
    begin
      Inc (PCh);
      if PCh^= '=' then
        Break
      else if PCh^= '\' then
      begin
        VarName:= VarName+ PCh^;
        Inc (PCh);
        VarName:= VarName+ PCh^;

      end
      else
        VarName:= VarName+ PCh^;


    end;

    while PCh<> PEndChar do
    begin
      Inc (PCh);
      
      if PCh^= '&' then
        Break
      else if PCh^= '\' then
      begin
        VarValue:= VarValue+ PCh^;
        Inc (PCh);
        VarValue:= VarValue+ PCh^;

      end
      else
        VarValue:= VarValue+ PCh^;

    end;

    Self.AddCgiVar (TCgiVar.Create ( URLDecode (VarName),
                                   URLDecode (VarValue)));
    
  end;
  
end;

{ TCgiVar }

function TCgiVar.GetName: String;
begin
  Result:= FName;
  
end;

function TCgiVar.GetValue: String;
begin
  Result:= (PString (PByte (FValue)))^;

end;

constructor TCgiVar.Create (Nam, Val: String);
var
  PVal: PString;

begin
  New (PVal);
  PVal^:= Val;

  inherited Create (Nam, TObject (PVal));
  
end;

destructor TCgiVar.Destroy;
begin
  Dispose (PString (PByte (FValue)));
  FValue:= nil;

  inherited;
  
end;

function TCgiVar.ToString: String;
begin
  Result:= FName+ ':'+ Value;
  
end;

{ ECookieNotFound }

constructor ECookieNotFound.Create;
begin
  inherited Create ('Cookie Not Found!');
  
end;

constructor ECookieNotFound.Create(Msg: String);
begin
  inherited Create (Msg);
  
end;

end.

