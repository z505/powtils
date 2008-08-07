unit ThreadingUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit, RequestsQueue;
  
type
  { TResidentPageExcecuteThread }

  TResidentPageExcecuteThread= class (TThread)
  private
    FRequestQueue: TCircularRequestsQueue;

   protected
     procedure Execute; override;

   public

    constructor Create (ReqQueue: TCircularRequestsQueue); overload;
    procedure Free;

  end;

  { TThreadCollection }

  TThreadCollection= class (TBaseCollection)
  private
    function GetResidentThread (Index: Integer): TResidentPageExcecuteThread;

  public
    property ResidentThread [Index: Integer]: TResidentPageExcecuteThread
         read GetResidentThread;

    constructor Create;
    procedure Free;
    
  end;

  { TThreadPool }

  TThreadPool= class (TObject)
  private
    FThreadCollection: TThreadCollection;

  public

    constructor Create (RequestPool: TCircularRequestsQueue;
                       n: Integer);
    procedure Execute;
    procedure Free;

    {This procedure can be implemented in a better way -- using message passing}
    procedure WaitUntilEndOfAllActiveThreads;
    
  end;
  
implementation

uses
  ResidentApplicationUnit, ResidentPageBaseUnit,
  ThisApplicationPagesUnit, MyTypes, SessionManagerUnit;

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
        FRequestQueue.AddOnDelete (Self);
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

      WriteLn ('MsgParameter.Count= ', MsgParameter.Count);
      if 2< MsgParameter.Count then
      begin
        WriteLn (MsgParameter.Argument [2]);
        PageInstance.CgiVars.LoadFromString (MsgParameter.Argument [2]);
        
      end;

      if NewRequest.CookieStr<> '' then
        PageInstance.Cookies.LoadFromString (NewRequest.CookieStr);

  // Set the pipename in which the current page should write its output
      PageInstance.PipeFileName:= MsgParameter.Argument [1];
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

procedure TResidentPageExcecuteThread.Free;
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

procedure TThreadPool.Free;
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

procedure TThreadCollection.Free;
var
  i: Integer;
  
begin
  for i:= 0 to Size- 1 do
    ResidentThread [i].Free;

  inherited;
  
end;

end.

