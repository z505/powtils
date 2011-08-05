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

  [12/APR/2009 - Amir]
   - A big change in TResident:
     Now, every dispatcher should register itself in TResident. By this way, there
     is no need to have ThisApplicationPagesUnit.

  [01/AUG/2009 - Amir]
   - I have done a depth debug and remove some minor bugs.

  [10/SEP/2009 - Amir]
   - Install function have been implemented in TResident class. This function tries
   to install the application. In the current implementation, it creates a directory
   where the Temproray pipefile should be stored-in along with the "Mainpipe". It also
   tries to create and compile the "RequestCollector" pages in.

  [10/OCT/2009 - Amir]
   - Add a resource file (Page.res) which contains the template for RequestCollector pages.


}

unit ResidentApplicationUnit;

{$mode objfpc}{$H+}{$M+}
{$DEFINE DebugMode}

interface

uses
  Classes, SysUtils, ThreadingUnit, QueueUnit,
  CustApp, RequestsQueue, AbstractHandlerUnit,
  BaseUnix, SessionManagerUnit, PageHandlerBaseUnit;

type
  { EMainPipeNotFound }

  EMainPipeNotFound= class (Exception)
    constructor Create (Name: AnsiString);
    
  end;
  
  TOnNewRequestArrived= procedure (Sender: TObject) of object;

  { EInvalidArgument }

  EInvalidArgument= class (Exception)
    constructor Create (ConfigName: AnsiString; Value: Integer); overload;
    constructor Create (ConfigName: AnsiString; Value: String); overload;
    constructor Create (Reason: AnsiString);

  end;
{$STATIC ON}
  { TAbstractResident }
  
  TAbstractResident= class (TCustomApplication)
  private
    FBeforeRestart: TNotifyEvent;
    FNumberOfActiveThread: Integer;
    FRequestQueueSize: Integer;
    FRestartInterval: Integer;
    FMainPipeFileName: String;
    FOnNewRequest: TOnNewRequestArrived;
    FAdminPagePath: AnsiString;
    FTempPipeFilesPath: String;
    FRemainedToRestart: Integer;
    FUsingSessionManager: Boolean;
    MainPipeFileHandle: cInt;
    FRequestQueue: TRequestBlockingQueue;
    FDispatchedRequestCount: Integer;

    FAllHandlers: TAbstractHandlerCollection;
    ThreadPool: TThreadPool;

    procedure SetMainPipeFileName (const AValue: String);
    procedure SetNumberOfActiveThread(const AValue: Integer);
    procedure SetRestartInterval (const AValue: Integer);

  private
    property AllHandlers: TAbstractHandlerCollection read FAllHandlers;
    property RequestQueue: TRequestBlockingQueue read FRequestQueue;
    procedure SetAdminPagePath (const AValue: AnsiString);
    procedure SetUsingSessionManager (const AValue: Boolean);

    {
      If the install parameter is passed to program, either through the
        command line arguments or through PSP.conf/PWU.conf, it tries to
        create the main pipe and the temp directory.
    }
    procedure Install;


  published
    property MainPipeFileName: String read FMainPipeFileName write SetMainPipeFileName ;
    property TempPipeFilesPath: String read FTempPipeFilesPath write FTempPipeFilesPath;
    property OnNewPageRequest: TOnNewRequestArrived read FOnNewRequest write FOnNewRequest;
    property BeforeRestart: TNotifyEvent read FBeforeRestart write FBeforeRestart;
//    property RestartInterval: Integer read FRestartInterval write SetRestartInterval;
    property RemainedToRestart: Integer read FRemainedToRestart;
    property NumberOfActiveThread: Integer read FNumberOfActiveThread write SetNumberOfActiveThread;
    property RequestQueueSize: Integer read FRequestQueueSize;
    property UsingSessionManager: Boolean read FUsingSessionManager write SetUsingSessionManager;
    property AdminPagePath: AnsiString read FAdminPagePath write SetAdminPagePath;

    function RegisterRequest (const ARequestString: String): Boolean;

  public
  
    procedure ExecuteInThread;
  
    constructor Create (AnOwner: TComponent); override;
    destructor Destroy; override;

    {
      Add a new page handler to the system. It also installs a request collector
      in appropriate directory if the install is passed to application.
    }
    procedure RegisterPageHandlerHandler (AHandler: TAbstractHandler);
    function GetPageHandler (const AName: String): TAbstractHandler;

  end;

  { TGeneralResident }

  TGeneralResident= class (TAbstractResident)
  private
  public
    constructor Create (AnOwner: TComponent); override;
    destructor Destroy; override;

  end;

  { TResidentWithFixedQueueSize }

  TResidentWithFixedQueueSize= class (TAbstractResident)
  private
  public
    constructor Create (AnOwner: TComponent; QueueSize: Integer);
    destructor Destroy; override;

  end;

  { TParameter }
(*
  TParameter= class (TStringList)
  private
    function GetArgument (Index: Integer): String;
    
  public
    property Argument [Index: Integer]: String read GetArgument;

    constructor Create (Str: String); overload;

  end;
*)
var
  Resident: TAbstractResident;
  

implementation

uses
  AdminPageDispatcherUnit,
  ThisProjectGlobalUnit, lresources, Process, BlockingQueueUnit;
  
{ TParameter }
(*
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
*)
{ TResident }

procedure TAbstractResident.SetMainPipeFileName (const AValue: String);
begin
  FMainPipeFileName:= AValue;
  
end;

procedure TAbstractResident.SetNumberOfActiveThread (const AValue: Integer);
begin
  if AValue<= 0 then
    raise EInvalidArgument.Create ('Maximum of Active threads', AValue);

  FNumberOfActiveThread:= AValue;

end;

procedure TAbstractResident.SetRestartInterval (const AValue: Integer);
begin
  FRestartInterval:= AValue;
  FRemainedToRestart:= AValue;
  
end;

procedure TAbstractResident.SetAdminPagePath (const AValue: AnsiString);
begin
  FAdminPagePath:= AValue;
  RegisterPageHandlerHandler (TAdminPageHandler.Create (AValue));

end;

procedure TAbstractResident.SetUsingSessionManager (const AValue: Boolean);
begin
  FUsingSessionManager:= AValue;

if UsingSessionManager then
  if UpperCase (GlobalObjContainer.Configurations.ConfigurationValueByName ['SessionUseCookie'])= 'TRUE' then
    SessionManagerUnit.SessionManager:= TBasicSessionManager.Create
  else
    raise Exception.Create ('Currently, the only working session manager is TCookiBasedSessionManager!');


end;

function TAbstractResident.RegisterRequest (const ARequestString: String): Boolean;
var
  NewRequest: TRequest;

begin
  Result:= True;

  NewRequest:= TRequest.Create (ARequestString);
  try
//    FRequestQueue.Insert (NewRequest);

  except
    on e: EQueueIsFull do
    begin
      Result:= False;
      NewRequest.Free;
      raise;

    end;

  end;

  GlobalObjContainer.NewRequestRegistered;

(*$IFDEF DebugMode*)
  WriteLn (FDispatchedRequestCount, ':NewRequest.Parameters=', NewRequest.ToString);
(*$ENDIF*)

end;

procedure TAbstractResident.Install;

  function DeleteDir (const DirPath: AnsiString): Boolean;
  var
    Path: String;
    SR: TSearchRec;
    Filename: AnsiString;

  begin
    Path:= DirPath;
    if Length (Path)<> 0 then
      if Path [Length (Path)]<> '/' then
        Path+= '/';

    if FindFirst (Path+ '*', faAnyFile, SR)= 0 then
    begin
      repeat
        if (SR.Name<> '.') and (SR.Name<> '..') then
        begin
          Filename:= Path+ SR.Name;
          if not DeleteFile (Filename) then
            Exit (False);

        end;

      until 0<> FindNext (SR);

    end;

    RemoveDir (DirPath);

  end;

  function CreateDir (const DirPath: AnsiString): Boolean;
  var
    Path: String;
    i: Integer;
    S: String;

  begin
    Path:= DirPath;
    if Length (Path)<> 0 then
    begin
      if Path [Length (Path)]<> '/' then
        Path+= '/';

    end;

    i:= 1;
    S:= '/';
    while i< Length (Path) do
    begin
      while i< Length (Path) do
      begin
        Inc (i);
        if Path [i]= '/' then
          Break;
        S+= Path [i];

      end;

      if not DirectoryExists (S) then
      begin
        if FpMkdir (S, $01F8)<> 0 then;//770 1 1110 1000
             Exit (False);
      end;
      S+= '/';

    end;

    Result:= True;

  end;

  procedure CheckForCorrectness;
  var
    i: Integer;

  begin
    if not FileExists (MainPipeFileName) then
      raise EInvalidArgument.Create ('MainPipe ("'+ MainPipeFileName+ '"'+
           ' does not exist');

    if not DirectoryExists (TempPipeFilesPath) then
      raise EInvalidArgument.Create ('TempPipe directory ("'+ TempPipeFilesPath+ '"'+
           ' does not exist');

    for i:= 0 to AllHandlers.Count- 1 do
      if not FileExists (GlobalObjContainer.Configurations.
             ConfigurationValueByName ['InstallationDirectory']+
              AllHandlers.PageHandler [i].PageName) then
        raise EInvalidArgument.Create ('RequestCollector "'+ AllHandlers.PageHandler [i].PageName+ '"'+
           ' does not exist');

  end;

var
  i: Integer;

begin
  if FileExists (MainPipeFileName) then
    DeleteFile (MainPipeFileName);
  if DirectoryExists (TempPipeFilesPath) then
    DeleteDir (TempPipeFilesPath);

  if not CreateDir (ExtractFileDir (MainPipeFileName)) then
    WriteLn ('Error while createing "', ExtractFileDir (MainPipeFileName), '"');
  FpMkfifo (MainPipeFileName, $1B0);//660.//110110000

  CreateDir (TempPipeFilesPath);

  for i:= 0 to AllHandlers.Count- 1 do
//    AllHandlers.PageHandler [i].ins
      if not FileExists (GlobalObjContainer.Configurations.
             ConfigurationValueByName ['InstallationDirectory']+
              AllHandlers.PageHandler [i].PageName) then
        raise EInvalidArgument.Create ('RequestCollector "'+ AllHandlers.PageHandler [i].PageName+ '"'+
           ' does not exist');

  CheckForCorrectness;

end;

{
}
procedure TAbstractResident.ExecuteInThread;
const
  NumOfCharsReadInEachTurn= 1000;
  EndOfRequestChar= #$FD;

  procedure SendQueueIsFullResponse (const RequestStr: AnsiString);
  const
    Headers: array [0..1] of AnsiString=
       ('X-Powered-By: Powtils',
        'Content-Type: text/html');
    Response: array [0..0] of AnsiString=
       ('Sever is too busy!!');

  var
    Request: TRequest;
    OutputPipeHanlde: cInt;
    i: Integer;

  begin
    Request:= TRequest.Create (RequestStr);

    OutputPipeHanlde:= FpOpen (Request.OutputPipe, O_WRONLY);

    for i:= 0 to High (Headers) do
      FpWrite (OutputPipeHanlde, Headers [i][1], Length (Headers [i]));
    FpWrite (OutputPipeHanlde, #10#10, 2);

    for i:= 0 to High (Response) do
      FpWrite (OutputPipeHanlde, Response [i][1], Length (Response [i]));

    FpClose (OutputPipeHanlde);

    GlobalObjContainer.QueueIsFullOccured;

    Request.Free;

  end;
  
var
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
  
  Buffer:= StringOfChar (' ', NumOfCharsReadInEachTurn+ 1);
  BufferLen:= 0;

  while True do
  begin
    try

      if BufferLen= 0 then
      begin
        BufferLen:= FpRead (MainPipeFileHandle, Buffer [1], NumOfCharsReadInEachTurn);

        if BufferLen<= 0 then
        begin
(*$IFDEF DebugMode*)
          WriteLn ('Execute In Thread: BufferLen< 0. Closing the pipe!');
(*$ENDIF*)

          FpClose (MainPipeFileHandle);
          MainPipeFileHandle:= FpOpen (FMainPipeFileName, O_RDONLY);

(*$IFDEF DebugMode*)
          WriteLn ('Execute In Thread: BufferLen was 0. Openning the pipe!');
(*$ENDIF*)

          BufferLen:= 0;
          Continue;

        end
        else
        begin
(*$IFDEF DebugMode*)
          Write ('Read String:');
          for i:= 1 to BufferLen do
            Write (Buffer [i]);
          WriteLn ('.');
(*$ENDIF*)

        end;

        BufferPtr:= @Buffer [1];

      end;

      EndOfCurrentRequest:= Pos (EndOfRequestChar, BufferPtr);
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
(*$IFDEF DebugMode*)
        WriteLn ('EndOfRequestChar has not been found in BufferPtr!');
        WriteLn ('Active Buffer= ', BufferPtr, '.');
(*$ENDIF*)

        for i:= BufferLen downto 1 do
        begin
          SegmentedString:= SegmentedString+ BufferPtr^;
          Inc (BufferPtr);

        end;
        BufferLen:= 0;

(*$IFDEF DebugMode*)
        WriteLn ('New SegmentedString is: ', SegmentedString, '.');
(*$ENDIF*)

      end;
      
      if StringIsComplete then
      begin
(*$IFDEF DebugMode*)
        WriteLn ('Execute In Thread: Completed Request:', SegmentedString);
(*$ENDIF*)

        RegisterRequest (SegmentedString);

        StringIsComplete:= False;
        SegmentedString:= '';

      end;


    except
      on e: EQueueIsFull do
      begin
        SendQueueIsFullResponse (SegmentedString);
        StringIsComplete:= False;
        SegmentedString:= '';


(*$IFDEF DebugMode*)
        WriteLn ('Queue is Full Happened!');
(*$ENDIF*)

      end;

    end;

  end;
  
  FpClose (MainPipeFileHandle);
//  FRequestQueue.Free;
  ThreadPool.Free;

end;

{
  Create the application. ReadConfigFileInfo reads the config file parameters
  such as MainPipename, NumberofActiveThread and QueueSize.
  
  Note that RestartInterval is not implemented yet. My goal was to restart the
   application after a number of specific executaion, which is a great help specially
   about memory leak. But till now, I can't find any way to do this.
}
constructor TAbstractResident.Create (AnOwner: TComponent);
begin
  inherited Create (AnOwner);

  MainPipeFileName:= GlobalObjContainer.Configurations.ConfigurationValueByName ['MainPipeFileName'];

  TempPipeFilesPath:= GlobalObjContainer.Configurations.ConfigurationValueByName ['TemproraryPipesPath'];
  NumberOfActiveThread:= StrToInt (GlobalObjContainer.Configurations.ConfigurationValueByName ['MaximumNumberofActiveThreads']);
  FRequestQueueSize:= StrToInt (GlobalObjContainer.Configurations.ConfigurationValueByName ['MaximumSizeofRequestQueue']);

  FRestartInterval:= -1;
  try
    FRestartInterval:= StrToInt (GlobalObjContainer.Configurations.ConfigurationValueByName ['RestartInterval']);

  except

  end;

  UsingSessionManager:= UpperCase (GlobalObjContainer.Configurations.ConfigurationValueByName ['SessionManager'])= 'TRUE';

  FDispatchedRequestCount:= 0;

  FAllHandlers:= TAbstractHandlerCollection.Create;

  RegisterPageHandlerHandler (TAdminPageHandler.Create (AdminPagePath));

  try
    if UpperCase (GlobalObjContainer.Configurations.ConfigurationValueByName ['Install'])= 'TRUE' then
    begin
      Install;

    end;

  except
    on e: Exception do
    begin
      WriteLn ('Installation was not successful. Exiting!');
      WriteLn (e.Message);

    end;

  end;

end;

destructor TAbstractResident.Destroy;
begin

//  while not FRequestQueue.IsEmpty do
  begin
(*$IFDEF DebugMode*)
    Writeln ('Resident[Destroy]: Queue is not empty, yet!');
(*$ENDIF*)

    Sleep (100);

  end;

(*$IFDEF DebugMode*)
  Writeln ('Resident[Destroy]: Finally, Queue became empty!');
  Writeln ('Resident[Destroy]:');

  Writeln ('Resident[Destroy]: Before ThreadPool.Free' );
(*$ENDIF*)

  ThreadPool.Free;

(*$IFDEF DebugMode*)
  Writeln ('Resident[Destroy]: After ThreadPool.Free' );
  WriteLn ('Resident[Destroy]: At the end');
(*$ENDIF*)

//  FRequestQueue.Free;
  AllHandlers.Free;

  inherited Destroy;

end;

procedure TAbstractResident.RegisterPageHandlerHandler ( AHandler: TAbstractHandler);

  procedure InstallPageHandler (PageHandler: TAbstractHandler);
  const
    SingleQuote: char= '''';

  var
    TargetFilename: AnsiString;
    FileString: TStringList;
    AStream: TStringStream;

    CompileProc: TProcess;
    MoveProc: TProcess;
    RemoveFileProc: TProcess;

    i: Integer;
    RandomFileName: String;

  begin

    AStream:= TStringStream.Create (
             LazarusResources.Find ('RequestCollectorPage').Value);
    FileString:= TStringList.Create;
    FileString.LoadFromStream (AStream);

    RandomFileName:= '';
    for i:= 1 to 15 do
      RandomFileName:= RandomFileName+ Chr (65+ Random (26));

    FileString [0]:= 'program '+ RandomFileName+ ';';
    FileString [19]:= '      '+   SingleQuote+ GlobalObjContainer.Configurations.ConfigurationValueByName ['TemproraryPipesPath']+ SingleQuote+ ';';
    FileString [21]:= '      '+SingleQuote+ GlobalObjContainer.Configurations.ConfigurationValueByName ['MainPipeFilename']+ SingleQuote+ ';';
    FileString [25]:= '  '+ SingleQuote+ PageHandler.PageName+ SingleQuote+ ';';

    FileString.SaveToFile (GlobalObjContainer.Configurations.ConfigurationValueByName ['InstallationDirectory']+ PageHandler.PageName+ '.pp');


    AStream.Free;
    FileString.Free;

    try
      CompileProc:= TProcess.Create (nil);
      CompileProc.CommandLine:= 'fpc "'+ GlobalObjContainer.Configurations.ConfigurationValueByName ['InstallationDirectory']+ PageHandler.PageName+ '.pp"';

      CompileProc.Options := CompileProc.Options + [poWaitOnExit];
      CompileProc.Execute;

    finally
      CompileProc.Free;

    end;

    if PageHandler.RelativePath<> PageHandler.PageName then
      try
        if GlobalObjContainer.Configurations.ConfigurationValueByName ['InstallationDirectory']<> '' then
        begin
          if GlobalObjContainer.Configurations.ConfigurationValueByName ['InstallationDirectory']
            [Length (GlobalObjContainer.Configurations.ConfigurationValueByName ['InstallationDirectory'])]= '/' then
            TargetFilename:= GlobalObjContainer.Configurations.ConfigurationValueByName ['InstallationDirectory']+
                                 PageHandler.RelativePath
          else
            TargetFilename:= GlobalObjContainer.Configurations.ConfigurationValueByName ['InstallationDirectory']+
                                 '/'+ PageHandler.RelativePath;

        end;

        MoveProc:= TProcess.Create (nil);
        MoveProc.CommandLine:= 'mv "'+ GlobalObjContainer.Configurations.ConfigurationValueByName ['InstallationDirectory']+ PageHandler.PageName+ '" "'+ TargetFilename+ '"';
        WriteLn ('---------------------');
        WriteLn (MoveProc.CommandLine);
        WriteLn ('---------------------');

        MoveProc.Options := CompileProc.Options + [poWaitOnExit];
        MoveProc.Execute;

      finally
        MoveProc.Free;

      end;

    try
      RemoveFileProc:= TProcess.Create (nil);
      RemoveFileProc.CommandLine:= 'rm "'+ GlobalObjContainer.Configurations.ConfigurationValueByName ['InstallationDirectory']+ PageHandler.PageName+ '.pp"';

      RemoveFileProc.Options := [poWaitOnExit];
      RemoveFileProc.Execute;

      RemoveFileProc.CommandLine:= 'rm "'+ GlobalObjContainer.Configurations.ConfigurationValueByName ['InstallationDirectory']+ PageHandler.PageName+ '.o"';

      RemoveFileProc.Execute;

    finally
      RemoveFileProc.Free;

    end;

  end;

begin
  AllHandlers.AddPageHandler (UpperCase (AHandler.PageName), AHandler);

  try
    if UpperCase (GlobalObjContainer.Configurations.ConfigurationValueByName ['Install'])= 'TRUE' then
    begin
      InstallPageHandler (AHandler);

    end;

  except
    on e: Exception do
    begin
      WriteLn ('Installation was not successful. Exiting!');
      WriteLn (e.Message);

    end;

  end;

end;

function TAbstractResident.GetPageHandler (const AName: String): TAbstractHandler;
begin
  Result:= AllHandlers.PageHandlerByName [AName].CreateNewInstance;

end;

{ EMainPipeNotFound }

constructor EMainPipeNotFound.Create (Name: AnsiString);
begin
  inherited Create ('No pipe with Name= '+ Name+ ' Found!');
  
end;

{ EInvalidArgument }

constructor EInvalidArgument.Create(ConfigName: AnsiString; Value: Integer);
begin
  inherited Create (ConfigName+ ' can not be '+ IntToStr (Value));

end;

constructor EInvalidArgument.Create(ConfigName: AnsiString; Value: AnsiString);
begin
  inherited Create (ConfigName+ ' can not be '+ Value);

end;

constructor EInvalidArgument.Create (Reason: Ansistring);
begin
  inherited Create (Reason);

end;

{ TGeneralResident }

constructor TGeneralResident.Create(AnOwner: TComponent);
begin
  inherited Create (AnOwner);
  FRequestQueue:= TRequestBlockingQueue.Create;

  ThreadPool:= TThreadPool.Create (FRequestQueue
  , NumberOfActiveThread);

  ThreadPool.Execute;


end;

destructor TGeneralResident.Destroy;
begin
  inherited Destroy;
end;

function CreateResidentApplicationInstance: TAbstractResident;
begin
  if GlobalObjContainer.Configurations.ConfigurationValueByName ['MaximumSizeofRequestQueue']= '-1' then
    Result:= TGeneralResident.Create (nil)
  else
    Result:= TResidentWithFixedQueueSize.Create (nil,
        StrToInt (GlobalObjContainer.Configurations.ConfigurationValueByName ['MaximumSizeofRequestQueue'])
          )

end;

{ TResidentWithFixedQueueSize }

constructor TResidentWithFixedQueueSize.Create (AnOwner: TComponent;
  QueueSize: Integer);
begin
  inherited Create (AnOwner);
end;

destructor TResidentWithFixedQueueSize.Destroy;
begin
  inherited Destroy;
end;

initialization
  {$I Page.lrs}

  CreateResidentApplicationInstance;

end.

