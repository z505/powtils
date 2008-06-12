  { TWeb }

  TWeb= class (TObject)
  private
    FCgi,  // CGI GET/POST data
    FConf, // Configuration data
    FCookie, // Cookie data
    FEnv,  // Enironment data
    FHdr,  // Headers
    FRti,  // Run Time Information
    FSess, // Session data
    Fvars: TWebVariableCollection; // PWU assign data
    UploadedFile: TWebUpFileCollection;    // Uploaded files storage
    ObjectBuffer: string;   // Output buffer
    HeaderIsSent, // Headers sent flag
    IsSessionRegistered: Boolean; // Session registered flag

   {$IFDEF GZIP_ENABLED}
      OutputBuffering, OutputCompression, // Output buffering and compression flags
   {$ENDIF}
    ErrorReporting, ErrorHalt: boolean; // Error reporting flags

    //dh: text; // Debug output

    FContentType: TContentType;
    Buffers: TStringList;
    RTI: TWebVariableCollection;

    procedure WriteBuffer;
    procedure FlushBuffer;

    function GetRTIAsInt (const Name: String): Integer;
  public
    property ContentType: TContentType read FContentType;

    procedure Dispach (GetParameter: TParemeterCollection;
                 PostParameter: TParemeterCollection); virtual; abstract;

    constructor Create (ContType: TContentType= ctTextHTML;
                  ForceToSendHeader: Boolean= False);
    procedure Free;
    procedure Clear;

    procedure WebWrite (const S: String);
    function ThrowWebError (const Message: String): Boolean;
  end;

