{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                    OPSP 1.1 ResidentApplicationUnit

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

--------------------------------------------------------------------------------
 Resident Application Unit
--------------------------------------------------------------------------------

 OPSP 1.2
 ---------

  [30/JAN/2006 - Amir]
   - This unit contains the classes, variables which is need for a Resident application.
   , we mean that this application will be used on non-CGI mode (in which
   on every request all the allocation must be done). This unit doesn't need to be changed
   when one wants to develop an application. The only change which must be done is in the
   "ThisApplicationPagesUnit" unit.

  [16/MAR/2007 - Amir]
   - A big change in TResident.ExecuteInThread:
      In the previous version, I tried to use AssignFile and Reset to handle the
      reading from pipe. But, there was a risk of missing a request. Now, it uses
      the FpOpen and FpRead.

  [28/APR/2008 - Amir]
   - A big change in TResident.ExecuteInThread:
      In the previous version, I tried to use AssignFile and Reset to handle the
      reading from pipe. But, there was a risk of missing a request. Now, it uses
      the FpOpen and FpRead.

}

unit ResidentApplicationUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit, ThreadingUnit,
  CustApp, WebUnit, RequestsQueue,
  BaseUnix, SessionManagerUnit;

type
  EConfigFileNotFound= class (Exception);

  { EMainPipeNotFound }

  EMainPipeNotFound= class (Exception)
    constructor Create (Name: String);
    
  end;
  
  TOnNewRequestArrived= procedure (Sender: TObject) of object;

  { EInvalidArgument }

  EInvalidArgument= class (Exception)
    constructor Create (ConfigName: String; Value: Integer); overload;
    constructor Create (ConfigName: String; Value: String); overload;

  end;
  
  { TResident }
  
  TResident= class (TCustomApplication)
  private
    FBeforeRestart: TNotifyEvent;
    FNumberOfActiveThread: Integer;
    FRequestQueueSize: Integer;
    FRestartInterval: Integer;
    FMainPipeFileName: String;
    FOnNewRequest: TOnNewRequestArrived;
    FTempPipeFilePath: String;
    FRemainedToRestart: Integer;
    FUsingSessionManager: Boolean;
    MainPipeFileHandle: cInt;
    FRequestQueue: TCircularRequestsQueue;
    FDispatchedRequestCount: Integer;

    ThreadPool: TThreadPool;

    procedure SetMainPipeFileName (const AValue: String);
    procedure SetNumberOfActiveThread(const AValue: Integer);
    procedure SetRestartInterval (const AValue: Integer);
    
  published
    property MainPipeFileName: String read FMainPipeFileName write SetMainPipeFileName ;
    property TempPipeFilePath: String read FTempPipeFilePath write FTempPipeFilePath;
    property OnNewPageRequest: TOnNewRequestArrived read FOnNewRequest write FOnNewRequest;
    property BeforeRestart: TNotifyEvent read FBeforeRestart write FBeforeRestart;
//    property RestartInterval: Integer read FRestartInterval write SetRestartInterval;
    property RemainedToRestart: Integer read FRemainedToRestart;
    property NumberOfActiveThread: Integer read FNumberOfActiveThread write SetNumberOfActiveThread;
    property RequestQueueSize: Integer read FRequestQueueSize;
    property UsingSessionManager: Boolean read FUsingSessionManager;

    function RegisterRequest (const ARequestString: String): Boolean;

  public
  
    procedure Execute;
    procedure ExecuteInThread;
  
    constructor Create (AOwner : TComponent);
    destructor Destroy; override;

//    procedure RegisterADisptacher ();

  end;

  { TParameter }

  TParameter= class (TStringList)
  private
    function GetArgument (Index: Integer): String;
    
  public
    property Argument [Index: Integer]: String read GetArgument;

    constructor Create (Str: String); overload;

  end;

var
  Resident: TResident;
  

implementation
uses
  ThisApplicationPagesUnit, ExceptionUnit, URLEnc, WebConfigurationUnit,
  GlobalUnit;
  
{ TParameter }

function TParameter.GetArgument (Index: Integer): String;
begin
  Result:= Strings [Index];
  
end;

constructor TParameter.Create (Str: String);
const
  ParameterSeprator: String= #$FF;
  
var
  i, Position: Integer;
  Ptr: PString;
  
begin
  inherited Create;
  
  Str:= TrimLeft (Str)+ ' ';

  while Str<> '' do
  begin
    Position:= Pos (ParameterSeprator, Str);
    
    if Position<> 0 then
    begin
      Self.Add (Copy (Str, 1, Position- 1));
      System.Delete (Str, 1, Position);
      Str:= TrimLeft (Str);
      
    end
    else
    begin
      Self.Add (Str);
      Break;
      
    end;
    
  end;

end;

{ TResident }

procedure TResident.SetMainPipeFileName (const AValue: String);
begin
  FMainPipeFileName:= AValue;
  
end;

procedure TResident.SetNumberOfActiveThread (const AValue: Integer);
begin
  if AValue<= 0 then
    raise EInvalidArgument.Create ('Maximum of Active threads', AValue);

  FNumberOfActiveThread:= AValue;

end;

procedure TResident.SetRestartInterval (const AValue: Integer);
begin
  FRestartInterval:= AValue;
  FRemainedToRestart:= AValue;
  
end;

function TResident.RegisterRequest (const ARequestString: String): Boolean;
var
  NewRequest: TRequest;

begin
  NewRequest:= TRequest.Create (ARequestString);
  FRequestQueue.Insert (NewRequest);
  Inc (FDispatchedRequestCount);

(*$IFDEF DEBUGMODE*)
  WriteLn (FDispatchedRequestCount, ':NewRequest.Parameters=', NewRequest.Parameters);
(*$ENDIF*)

end;

procedure TResident.Execute;
{
var
  MessageStr: String;
  MsgParamenter: TParameter;
  CookieString: String;
  CgiVar: TCgiVariableCollection;
  CurrentPage: TResidentPageBase;
  ReqCon: Integer;
  SegmentedString: String;
  StringIsComplete: Boolean;
}
begin
  raise ENotImplementedYet.Create ('It is not complete');
{
  MainPipeFileHandle:= FpOpen (FMainPipeFileName, O_RDONLY);
  ReqCon:= 0;
  SegmentedString:= '';
  StringIsComplete:= False;

  while True do
  begin

    ReadLn (MainPipeFileHandle, MessageStr);
(*$IFDEF DEBUG_MODE*)
    WriteLn ('MessageStr=', MessageStr);
(*$ENDIF*)

    if MessageStr= '' then
    begin
      while MessageStr= '' do
      begin
        Reset (MainPipeFileHandle);
        ReadLn (MainPipeFileHandle, MessageStr);

      end;

    end;

    ReadLn (MainPipeFileHandle, CookieString);

(*$IFDEF DEBUG_MODE*)
    WriteLn ('CookieString=', CookieString);
(*$ENDIF*)

    MsgParamenter:= TParameter.Create (MessageStr);

    if MsgParamenter.ArgumentCount<= 1 then// Invalid parameters
    begin
      MsgParamenter.Free;
      Continue;//raise ...

    end
    else // request is in correct form
    begin

// Get a new instance of PageName Handler
// This procedure is defined in ThisApplicationPagesUnit.pas
      CurrentPage:= GetAppropriatePageByPageName (UpperCase (MsgParamenter.Argument [0]));

// Request paramters must be (PageName) (OutputPipeName) (Get/Put Variable)
      if MsgParamenter.ArgumentCount= 3 then
        CurrentPage.CgiVars.LoadFromString (MsgParamenter.Argument [2]);

//Loading Cookies Parameter.
      CurrentPage.Cookies.LoadFromString (CookieString);

// Set the pipename in which the current page should write its output
      CurrentPage.PipeFileName:= MsgParamenter.Argument [1];

// Excecure OnNewRequest event
      if Assigned (FOnNewRequest) then
        FOnNewRequest (Self, CurrentPage);

      CurrentPage.MyDispatch;

// Free the objects memory
      CurrentPage.Free;

      MsgParamenter.Free;
      CgiVar.Free;

// check to see if it is time to restart the application
      Dec (FRemainedToRestart);
      if FRemainedToRestart= 0 then
        Break;

    end;

  end;

  CloseFile (MainPipeFileHandle);
}

end;

{
}
procedure TResident.ExecuteInThread;
const
  NumOfCharsReadInEachTurn= 1000;
  EndOfRequestChar= #$FD;
  
var
  NewRequest: TRequest;
  i, EndOfCurrentRequest,
  BufferLen: Integer;
  Buffer: AnsiString;
  BufferPtr: PChar;
  StringIsComplete: Boolean;
  SegmentedString: String;

begin
  MainPipeFileHandle:= FpOpen (FMainPipeFileName, O_RDONLY);

  SegmentedString:= '';
  StringIsComplete:= False;
  
  Buffer:= '';
  for i:= 1 to NumOfCharsReadInEachTurn+ 1 do
    Buffer:= Buffer+ ' ';
  BufferLen:= 0;

  while True do
  begin
    try

      if BufferLen= 0 then
      begin
        BufferLen:= FpRead (MainPipeFileHandle, Buffer [1], NumOfCharsReadInEachTurn);

        if BufferLen<= 0 then
        begin
          WriteLn ('Closing the pipe!');

          FpClose (MainPipeFileHandle);
          MainPipeFileHandle:= FpOpen (FMainPipeFileName, O_RDONLY);
          BufferLen:= 0;
          
          Continue;

        end;

        BufferPtr:= @Buffer [1];

      end;

      EndOfCurrentRequest:= Pos (EndOfRequestChar, Buffer);
      if EndOfCurrentRequest<> 0 then
      begin
        for i:= 1 to EndOfCurrentRequest- 1 do
        begin
          SegmentedString:= SegmentedString+ BufferPtr^;
          Inc (BufferPtr);

        end;
        Inc (BufferPtr);// To Ignore #FD
        Dec (BufferLen, EndOfCurrentRequest);
        StringIsComplete:= True;

      end
      else
      begin
        SegmentedString:= SegmentedString+ Buffer;
        Buffer:= '';
        BufferLen:= 0;
        
      end;
      
      if StringIsComplete then
      begin
        WriteLn ('Completed Request:', SegmentedString);

        RegisterRequest (SegmentedString);

        StringIsComplete:= False;
        SegmentedString:= '';

      end;


    except
      on e: EQueueIsFull do
        WriteLn ('Queue is Full Happened!');

    end;

  end;
  
  FpClose (MainPipeFileHandle);
  FRequestQueue.Free;
  ThreadPool.Free;

end;

{
  Create the application. ReadConfigFileInfo reads the config file parameters
  such as MainPipename, NumberofActiveThread and QueueSize.
  
  Note that RestartInterval is not implemented yet. My goal was to restart the
   application after a number of specific executaion, which is a great help specially
   about memory leak. But till now, I can't find any way to do this.
}
constructor TResident.Create (AOwner: TComponent);

{
  The configfilename must be PSP.conf and be in the same path as the application.
}

  procedure ReadConfigFileInfo;
  var
    ConfigFileHandle: TextFile;
    TempInt: Integer;
    TempString: String;
    SessionIDLen: Integer;
    SessionVarName: String;
    SessionUseCookie: Boolean;
    
  begin
    if not FileExists ('PSP.conf') then
      EConfigFileNotFound.Create ('Config File not found!');

    AssignFile (ConfigFileHandle, 'PSP.conf');
    Reset (ConfigFileHandle);

    while not Eof (ConfigFileHandle) do
    begin
      ReadLn (ConfigFileHandle, TempString);
      TempString:= Trim (TempString);
      
      if (TempString<> '') and
        ((Length (TempString)= 0) or (TempString [1]<> '#')) then
      begin
        TempInt:= Pos (':', TempString);
        WebConfiguration.AddNameValue (TWebConfiguration.Create (
          Copy (TempString, 1, TempInt- 1),
          Copy (TempString, TempInt+ 1, Length (TempString)- TempInt)));
          
      end;
      
    end;
    CloseFile (ConfigFileHandle);
    
    MainPipeFileName:= WebConfiguration.ConfigurationByName ['MainPipeFileName'].Value;
    if not FileExists (FMainPipeFileName) then
      raise EMainPipeNotFound.Create (FMainPipeFileName);
      
    TempPipeFilePath:= WebConfiguration.ConfigurationByName ['TemproraryPipesPath'].Value;
    NumberOfActiveThread:= StrToInt (WebConfiguration.ConfigurationByName ['MaximumNumberofActiveThreads'].Value);
    FRequestQueueSize:= StrToInt (WebConfiguration.ConfigurationByName ['MaximumSizeofRequestQueue'].Value);
    
    FRestartInterval:= -1;
    FUsingSessionManager:= False;
    try
      FRestartInterval:= StrToInt (WebConfiguration.ConfigurationByName ['RestartInterval'].Value);
      
    except
    
    end;

    try
      FUsingSessionManager:= UpperCase (WebConfiguration.ConfigurationByName ['SessionManager'].Value)= 'TRUE';

    except
      on e: ENameNotFound do
        FUsingSessionManager:= False;

    end;

    if FUsingSessionManager then
    begin
      try
        SessionIDLen:= DefualtSessionIDLen;
        SessionIDLen:= StrToInt (WebConfiguration.ConfigurationByName ['SessionIDLen'].Value);

      except
        on e: ENameNotFound do;
        on e: EConvertError do
        begin
          WriteLn ('Session id len is not an integer!');

        end;

      end;

      try
        SessionVarName:= WebConfiguration.ConfigurationByName ['SessionIDVarName'].Value;

      except
        on e: ENameNotFound do
        begin
          SessionVarName:= DefualtSessionVarName;

        end;

      end;


      try
        SessionUseCookie:=
          UpperCase (WebConfiguration.ConfigurationByName ['SessionUseCookie'].Value)
            = 'TRUE';

      except
        on e: ENameNotFound do
        begin
          SessionUseCookie:= True;

        end;

      end;

      SessionManagerUnit.SessionManager:= TBasicSessionManager.Create (
        SessionIDLen, SessionVarName, SessionUseCookie);
        
    end;

  end;

begin
  inherited Create (AOwner);
  
  ReadConfigFileInfo;

  FRequestQueue:= TCircularRequestsQueue.Create (FRequestQueueSize, NumberOfActiveThread);

  ThreadPool:= TThreadPool.Create (FRequestQueue, NumberOfActiveThread);
  ThreadPool.Execute;

  FDispatchedRequestCount:= 0;


end;

destructor TResident.Destroy;
var
  i: Integer;

begin

  while not FRequestQueue.IsEmpty do
  begin
    FRequestQueue.TryToMakeItEmpty;

{$ifdef DebugMode}
    Writeln ('Resident[Destroy]: Queue is not empty, yet!');
{$endif}
    Sleep (100);

  end;

  {$ifdef DebugMode}
  Writeln ('Resident[Destroy]: At last, Queue became empty!');
  Writeln ('Resident[Destroy]:');
  Writeln ('Resident[Destroy]:');
  {$endif}

  ThreadPool.Free;

{$ifdef DebugMode}
  WriteLn ('Resident[Destroy]: At the end');
{$endif}

  FRequestQueue.Free;

  inherited Destroy;

end;

{ EMainPipeNotFound }

constructor EMainPipeNotFound.Create(Name: String);
begin
  inherited Create ('No pipe with Name= '+ Name+ ' Found!');
  
end;

{ EInvalidArgument }

constructor EInvalidArgument.Create(ConfigName: String; Value: Integer);
begin
  inherited Create (ConfigName+ ' can not be '+ IntToStr (Value));

end;

constructor EInvalidArgument.Create(ConfigName: String; Value: String);
begin
  inherited Create (ConfigName+ ' can not be '+ Value);

end;

end.

