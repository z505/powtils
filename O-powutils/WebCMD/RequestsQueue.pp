unit RequestsQueue;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit;
  
type
{
  This class is going to save a request parameters.
  It read the parameters from the Mainpipehandle which is passed to it.
}
  { TRequest }

  TRequest= class (TObject)
  private
    FParameters: String;
    FCookieStr: String;
    
  public
    {Parameters is a string which contains all the GET/POST variables
      and some of environment variables which are seprated by & from each other.
    }
    property Parameters: String read FParameters;
    
    {CookiStr is a string which contains all Cookie values
      which are seprated by & from each other.
    }
    property CookieStr: String read FCookieStr;
    
    constructor Create (var ParamsAndCookies: String);
    
  end;
  
  { EQueueIsFull }

  EQueueIsFull= class (Exception)
    constructor Create;

  end;

  { EQueueIsEmpty }

  EQueueIsEmpty= class (Exception)
    constructor Create;

  end;


  { TCircularRequestsQueue }

  TCircularRequestsQueue= class (TBaseCollection)
  private
    FThreadsWaitOnDelete,
    FThreadsWaitOnInsert: TBaseCollection;
    SoQ, EoQ: Integer;
    CS: TRTLCriticalSection;
    ActiveMemberCount: Integer;

  public
    constructor Create (QueueSize: Integer);
    destructor Destroy; override;

    procedure Insert (Request: TRequest);
    function Delete: TRequest;
    procedure AddOnInsert (Thread: TThread);
    procedure AddOnDelete (Thread: TThread);

  end;

implementation
uses
  URLEnc;
  
{ TRequest }
{
  Loads the Parameters and Cookies from MainPipe.
  A #255#255 used as disjointer between parameters and cookies which are
  written in a line by RequestCollector.
}
{
constructor TRequest.Create (var MainPipeHanlde: TextFile);
const
  ParametersCookiesSeprator: String= #$FE#$FE;
var
  ParamsAndCookies: String;
  Position: Integer;
  
begin
  inherited Create;
  
  ReadLn (MainPipeHanlde, ParamsAndCookies);
  Position:= Pos (ParametersCookiesSeprator, ParamsAndCookies);
  
  FParameters:= Copy (ParamsAndCookies, 1, Position- 1);
  Delete (ParamsAndCookies, 1, Position+ 1);
  FCookieStr:= URLDecode (ParamsAndCookies);

  
(*$IFDEF DEBUG_MODE*)
  WriteLn ('FParameters=', FParameters);
  WriteLn ('FCookieStr=', FCookieStr);
(*$ENDIF*)

end;
}

{
  Loads the Parameters and Cookies from an string (ParamsAndCookies).
  A #255 used as disjointer between parameters and cookies which are
  written in a line by RequestCollector.
}
constructor TRequest.Create (var ParamsAndCookies: String);
const
  ParametersCookiesSeprator: String= #$FE;
  
var
  Position: Integer;

begin
  inherited Create;

  Position:= Pos (ParametersCookiesSeprator, ParamsAndCookies);

  FParameters:= Copy (ParamsAndCookies, 1, Position- 1);
  Delete (ParamsAndCookies, 1, Position+ 1);
  FCookieStr:= URLDecode (ParamsAndCookies);

(*$IFDEF DEBUG_MODE*)
  WriteLn ('FParameters=', FParameters);
  WriteLn ('FCookieStr=', FCookieStr);
  
(*$ENDIF*)

end;

{ EQueueIsFull }

constructor EQueueIsFull.Create;
begin
  inherited Create ('Queue Is Full!');
  
end;

{ EQueueIsEmpty }

constructor EQueueIsEmpty.Create;
begin
  inherited Create ('Queue Is Empty!');
  
end;


{ TCircularRequestsQueue }

constructor TCircularRequestsQueue.Create (QueueSize: Integer);
begin
  inherited Create;

  InitCriticalSection (CS);

  Allocate (QueueSize);
  SoQ:= 0;
  EoQ:= 0;
  FSize:= Size;
  ActiveMemberCount:= 0;

  FThreadsWaitOnDelete:= TBaseCollection.Create;
  FThreadsWaitOnInsert:= TBaseCollection.Create;
  
end;

destructor TCircularRequestsQueue.Destroy;
var
  i: Integer;

begin
  DoneCriticalsection (CS);
  
  FThreadsWaitOnInsert.Clear;
  FThreadsWaitOnDelete.Clear;

  FThreadsWaitOnInsert.Free;
  FThreadsWaitOnDelete.Free;
  
  inherited;

end;

procedure TCircularRequestsQueue.Insert (Request: TRequest);
var
  SuspendedThread: TThread;

begin
  EnterCriticalsection (CS);

  if FSize<= ActiveMemberCount then
  begin
    LeaveCriticalsection (CS);
    raise EQueueIsFull.Create;

  end;

  FMembers [EoQ]:= Request;
  if EoQ= FSize- 1 then
    EOq:= FSize- 1;

  EoQ:= (EoQ+ 1) mod FSize;
  Inc (ActiveMemberCount);
  
  if FThreadsWaitOnDelete.Size<> 0 then
  begin
    SuspendedThread:= FThreadsWaitOnDelete.Member [FThreadsWaitOnDelete.Size- 1] as TThread;
    FThreadsWaitOnDelete.Member [FThreadsWaitOnDelete.Size- 1]:= nil;
    FThreadsWaitOnDelete.Delete (FThreadsWaitOnDelete.Size- 1);
    SuspendedThread.Resume;

  end;

  LeaveCriticalsection (CS);

end;

function TCircularRequestsQueue.Delete: TRequest;
begin
  EnterCriticalsection (CS);
  
  if ActiveMemberCount= 0 then
  begin
    LeaveCriticalsection (CS);
    raise EQueueIsEmpty.Create;
    
  end;

  Result:= FMembers [SoQ] as TRequest;
  SoQ:= (SoQ+ 1) mod FSize;
  Dec (ActiveMemberCount);
  
  LeaveCriticalsection (CS);

end;

procedure TCircularRequestsQueue.AddOnInsert (Thread: TThread);
begin
  EnterCriticalsection (CS);

  FThreadsWaitOnInsert.Add (Thread);

  LeaveCriticalsection (CS);

end;

procedure TCircularRequestsQueue.AddOnDelete (Thread: TThread);
begin
  EnterCriticalsection (CS);

  FThreadsWaitOnDelete.Add (Thread);

  LeaveCriticalsection (CS);

end;

end.

