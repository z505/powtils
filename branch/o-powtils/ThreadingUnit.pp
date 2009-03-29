unit ThreadingUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit, RequestsQueue, AbstractDispatcherUnit;
  
type
  { TResidentPageExcecuteThread }

  TResidentPageExcecuteThread= class (TThread)
  private
    FRequestQueue: TCircularRequestsQueue;

   protected
     procedure Execute; override;

   public
    constructor Create (ReqQueue: TCircularRequestsQueue); overload;
    destructor Destroy; override;

  end;

  { TThreadCollection }

  TThreadCollection= class (TBaseCollection)
  private
    function GetResidentThread (Index: Integer): TResidentPageExcecuteThread;

  public
    property ResidentThread [Index: Integer]: TResidentPageExcecuteThread
         read GetResidentThread;

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
  ResidentApplicationUnit, ResidentPageBaseUnit,
  ThisApplicationPagesUnit, MyTypes, SessionManagerUnit, ExceptionUnit;

{ TResidentPageExcecuteThread }

procedure TResidentPageExcecuteThread.Execute;
var
  NewRequest: TRequest;
  MsgParameter: TParameter;
  PageInstance: TResidentPageBase;
  
begin

  while True do
  begin
    
    try
      NewRequest:= FRequestQueue.Delete;
      
    except
      on e: EQueueIsEmpty do
      begin
        FRequestQueue.AddToSuspendedThreads (Self);
        Self.Suspend;
        Continue;

      end;
      
    end;
    
    MsgParameter:= TParameter.Create (NewRequest.Parameters);

    if MsgParameter.Count<= 1 then// Invalid parameters
    begin
      Writeln ('Error', NewRequest.Parameters, ' ', NewRequest.CookieStr);
      MsgParameter.Free;
      Continue;

    end
    else // request is in correct form
    begin
  // MsgParamters must be (PageName) (OutputPipeName) (Get/Put Variable)
      PageInstance:= GetAppropriatePageByPageName (UpperCase (MsgParameter.Argument [0]));

      (*$IFDEF DEBUGMODE*)
      WriteLn ('MsgParameter.Count= ', MsgParameter.Count);
      (*$ENDIF*)

      if 2< MsgParameter.Count then
        PageInstance.Vars.LoadFromString (MsgParameter.Argument [2]);

      if NewRequest.CookieStr<> '' then
        PageInstance.Cookies.LoadFromString (NewRequest.CookieStr);

  // Set the pipename in which the current page should write its output
       raise ENotImplementedYet.Create ('PageInstance', 'SetPipeName');

//      PageInstance.PipeFileName:= MsgParameter.Argument [1];
      try
        PageInstance.MyDispatch;
        
      except
        on e: Exception do
          WriteLn (e.Message);
          
      end;

      PageInstance.Free;

    end;

  end;

end;

constructor TResidentPageExcecuteThread.Create (ReqQueue: TCircularRequestsQueue);
begin
  inherited Create (True);

  FreeOnTerminate:= False;
  FRequestQueue:= ReqQueue;

end;

destructor TResidentPageExcecuteThread.Destroy;
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
    Ptr^:= TResidentPageExcecuteThread.Create (RequestPool);
    Inc (Ptr);
    
  end;

end;

procedure TThreadPool.Execute;
var
  i: Integer;

begin
  for i:= 0 to FThreadCollection.Size- 1 do
    FThreadCollection.ResidentThread [i].Resume;

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

function TThreadCollection.GetResidentThread (Index: Integer
  ): TResidentPageExcecuteThread;
begin
  Result:= Member [Index] as TResidentPageExcecuteThread;
  
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
    if ResidentThread [i].Suspended then
      ResidentThread [i].Resume;

    WaitForThreadTerminate (ResidentThread [i].ThreadID, 0);
    ResidentThread [i].Free;

  end;

  inherited;
  
end;

end.

