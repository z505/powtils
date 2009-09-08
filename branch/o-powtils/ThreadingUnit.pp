unit ThreadingUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit, RequestsQueue, BaseUnix;
  
type
  { TDispactherThread }

  TDispactherThread= class (TThread)
  private
    FOutputPipeHandle: cint;
    FOutputPipeName: String;
    FRequestQueue: TCircularRequestsQueue;

    procedure SetOutputPipeName (const AValue: String);

   protected
     {
       Name and location of the pipe in which the output should be written.
     }
     property OutputPipeName: String read FOutputPipeName write SetOutputPipeName;

     procedure Execute; override;

   public
     property OutputPipeHandle: cint read FOutputPipeHandle;

    constructor Create (ReqQueue: TCircularRequestsQueue); overload;
    destructor Destroy; override;

  end;

  { TThreadCollection }

  TThreadCollection= class (TBaseCollection)
  private
    function GetThread (Index: Integer): TDispactherThread;

  public
    property Thread [Index: Integer]: TDispactherThread
         read GetThread;

    constructor Create;
    destructor Destroy; override;
    
  end;

  { TThreadPool }

  TThreadPool= class (TObject)
  private
    FThreadCollection: TThreadCollection;

  public

    constructor Create (RequestPool: TCircularRequestsQueue;
                       n: Integer);
    destructor Destroy; override;

    procedure Execute;

    {This procedure can be implemented in a better way -- using message passing}
    procedure WaitUntilEndOfAllActiveThreads;
    
  end;
  
implementation

uses
  ResidentApplicationUnit, MyTypes, AbstractHandlerUnit;


{ TDispactherThread }

procedure TDispactherThread.SetOutputPipeName (const AValue: String);
begin
  FOutputPipeName:= AValue;

  FOutputPipeHandle:= FpOpen (FOutputPipeName, O_WRONLY);

end;

procedure TDispactherThread.Execute;
var
  NewRequest: TRequest;
  PageInstance: TAbstractHandler;
  
begin

  while True do
  begin

    try
      NewRequest:= FRequestQueue.Delete;

    except
      on e: EQueueIsEmpty do
      begin
(*$IFDEF DebugMode*)
        WriteLn ('TDispactherThread.Execute: RequestQueue is empty!');
(*$ENDIF*)

        FRequestQueue.AddToSuspendedThreads (Self);

(*$IFDEF DebugMode*)
        WriteLn ('TDispactherThread.Execute: Before suspend!', ThreadID);
(*$ENDIF*)

        Self.Suspend;

(*$IFDEF DebugMode*)
        WriteLn ('TDispactherThread.Execute: After suspend!', ThreadID);
(*$ENDIF*)

        Continue;

      end;
      
    end;
    
(*$IFDEF DebugMode*)
    WriteLn ('TDispactherThread.Execute: Request to be Served is (', NewRequest.ToString, ')');
(*$ENDIF*)

    PageInstance:= Resident.GetPageHandler (UpperCase (NewRequest.PageName));
    OutputPipeName:= NewRequest.OutputPipe;

    try
(*$IFDEF DebugMode*)
      WriteLn ('TDispactherThread.Execute: Before Dispatch');
(*$ENDIF*)

      PageInstance.Dispatch (NewRequest, Self);

(*$IFDEF DebugMode*)
      WriteLn ('TDispactherThread.Execute: After Dispatch');
(*$ENDIF*)

    except
      on e: Exception do
        WriteLn (e.Message);

    end;

(*$IFDEF DebugMode*)
    WriteLn ('TDispactherThread.Execute: Before Freeing PageInstance');
(*$ENDIF*)

    PageInstance.Free;

(*$IFDEF DebugMode*)
    WriteLn ('TDispactherThread.Execute: After Freeing PageInstance');
(*$ENDIF*)

  end;

end;

constructor TDispactherThread.Create (ReqQueue: TCircularRequestsQueue);
begin
  inherited Create (True);

  FreeOnTerminate:= False;
  FRequestQueue:= ReqQueue;

end;

destructor TDispactherThread.Destroy;
begin
  FRequestQueue:= nil;

  inherited;

end;

{ TThreadPool }

constructor TThreadPool.Create (RequestPool: TCircularRequestsQueue;
            n: Integer);
var
  i: Integer;
  Ptr: PObject;

begin
  inherited Create;

  FThreadCollection:= TThreadCollection.Create;
  FThreadCollection.Allocate (n);
  Ptr:= FThreadCollection.GetPointerToFirst;
  
  for i:= 0 to n- 1 do
  begin
    Ptr^:= TDispactherThread.Create (RequestPool);
    Inc (Ptr);
    
  end;

end;

procedure TThreadPool.Execute;
var
  i: Integer;

begin
  for i:= 0 to FThreadCollection.Size- 1 do
    FThreadCollection.Thread [i].Resume;

end;

destructor TThreadPool.Destroy;
begin

  FThreadCollection.Free;

  inherited;

end;

procedure TThreadPool.WaitUntilEndOfAllActiveThreads;
var
  i: Integer;
  Ptr: PObject;
  
begin
  i:= 0;
  Ptr:= FThreadCollection.GetPointerToFirst;
  
  while i< FThreadCollection.Size do
  begin
    if (Ptr^ as TThread).Suspended then
    begin
      Inc (Ptr);
      Inc (i);
      
    end
    else
      Sleep (100);
      
  end;

end;

{ TThreadCollection }

function TThreadCollection.GetThread (Index: Integer): TDispactherThread;
begin
  Result:= Member [Index] as TDispactherThread;
  
end;

constructor TThreadCollection.Create;
begin
  inherited;
  
end;

destructor TThreadCollection.Destroy;
var
  i: Integer;
  
begin
  for i:= 0 to Size- 1 do
  begin
    if Thread [i].Suspended then
      Thread [i].Resume;

    WaitForThreadTerminate (Thread [i].ThreadID, 0);
    Thread [i].Free;

  end;

  inherited;
  
end;

end.

