unit AbstractHandlerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Unix, RequestsQueue, CookieUnit, WebHeaderUnit, CgiVariableUnit,
    SessionManagerUnit, ThreadingUnit;

type

  { TAbstractHandler }

  TAbstractHandler= class (TObject)
  private
    FCookies: TCookieCollection;
    FBuffer: TStringList;
    FHeaderCanBeSent: Boolean;
    FHeaders: THeaderCollection;
    FPipeHandle: cInt;
    FVars: TCgiVariableCollection;
    FPageName: String;
    FShouldBeFreedManually: Boolean;
    FContentType: TContentType;

    FThread: TDispactherThread;
    procedure SetPipeHandle (const AValue: cInt);

  protected
    property ShouldBeFreedManually: Boolean read FShouldBeFreedManually;
    property Buffer: TStringList read FBuffer;
    property PipeHandle: cInt read FPipeHandle write SetPipeHandle;

    FTempPipeFilePath: String;
    PipeIsAssigned: Boolean;
    FRequestURI: String;
    FHostName: String;
    FForceToSendHeader: Boolean;
    FSessionID: TSessionID;

    procedure WriteToBuffer (const S: String);
    procedure Write (const S: String);
    procedure WriteLn (const S: String);
    procedure WriteHeaders;

  public
    property Cookies: TCookieCollection read FCookies;
    property Headers: THeaderCollection read FHeaders;
    property Vars: TCgiVariableCollection read FVars;
    property HeaderCanBeSent: Boolean read FHeaderCanBeSent;

    constructor Create (ContType: TContentType;_ShouldBeFreedManually: Boolean);
    destructor Destroy; override;

    function CreateNewInstance: TAbstractHandler; virtual; abstract;

    procedure Dispatch (RequestInfo: TRequest);
    procedure RegisterThread (AThread: TDispactherThread);

    procedure MyDispatch; virtual; abstract;

  end;

implementation
uses
  ExceptionUnit, BaseUnix, GlobalUnit;

{ TAbstractHandler }

procedure TAbstractHandler.SetPipeHandle (const AValue: cInt);
begin
  FPipeHandle:= AValue;
  PipeIsAssigned:= True;

end;

procedure TAbstractHandler.WriteToBuffer (const S: String);
begin
  FBuffer.Add (S);

end;

procedure TAbstractHandler.Write (const S: String);
const
  NewLine: String= #10#10;
(*$I+*)
  BufText: String= '';
(*$I-*)

begin
  if HeaderCanBeSent then
  begin
    BufText:= Headers.Text+ #10;
    Headers.Clear;
    FpWrite (FPipeHandle, BufText [1], Length (BufText));

    BufText:= Cookies.Text;
    Cookies.Clear;
    FpWrite (FPipeHandle, BufText [1], Length (BufText));

    FpWrite (FPipeHandle, NewLine [1], 1);

  end;

  if FBuffer.Count<> 0 then
  begin
    BufText:= FBuffer.Text;
    FBuffer.Clear;

    FpWrite (FPipeHandle, BufText [1], Length (BufText));

  end;

  FpWrite (FPipeHandle, S [1], Length (S));
  FHeaderCanBeSent:= False;

end;

procedure TAbstractHandler.WriteLn (const S: String);
const
  NewLine: String= #10;

begin
  Write (S+ NewLine);

end;

procedure TAbstractHandler.WriteHeaders;
var
  S: String;

begin
  if Headers.Size<> 0 then
  begin
    S:= Headers.Text;
    Headers.Clear;
    FpWrite (FPipeHandle, S [1], Length (S));

  end;

  if Cookies.Size<> 0 then
  begin
    S:= Cookies.Text;
    Cookies.Clear;
    FpWrite (FPipeHandle, S [1], Length (S));

  end;

  FpWrite (FPipeHandle, S [1], Length (S));
  FHeaderCanBeSent:= False;

end;

constructor TAbstractHandler.Create (ContType: TContentType;
                   _ShouldBeFreedManually: Boolean);
begin
  inherited Create;

  FShouldBeFreedManually:= _ShouldBeFreedManually;

  FBuffer:= TStringList.Create;
  FVars:= TCgiVariableCollection.Create;
  FHeaders:= THeaderCollection.Create;

  FCookies:= TCookieCollection.Create (FHeaders, @FHeaderCanBeSent, '', '');
  FContentType:= ContType;

  Headers.AddHeader (THeader.Create ('X-Powered-By', 'Powtils'));
  Headers.AddHeader (THeader.Create ('Content-Type',
                'Powtils'+ ';charset= '+ WebConfiguration.ConfigurationValueByName ['CHARSET']
             ));
  FHeaderCanBeSent:= True;

end;

destructor TAbstractHandler.Destroy;
begin
  FBuffer.Free;
  FCookies.Free;
  FVars.Free;
  FHeaders.Free;

  inherited Destroy;

end;

procedure TAbstractHandler.Dispatch (RequestInfo: TRequest);
var
  VarString, CookieString: String;

  function LoadVariables (const VariablesStr: AnsiString): TCgiVariableCollection;
  var
    StartIndicator, CharPtr: PChar;
    CgiVar: TCgiVar;
    VarName, VarValue: String;
    ReminderLen: Integer;

  begin
    Result:= TCgiVariableCollection.Create;

    CharPtr:= @VariablesStr [1];
    ReminderLen:= Length (VariablesStr);

    while 0< ReminderLen do
    begin
      StartIndicator:= CharPtr;

      while CharPtr^<> '=' do
      begin
        Inc (CharPtr);
        Dec (ReminderLen);

      end;
      Inc (CharPtr);
      Dec (ReminderLen);

      SetString (VarName, StartIndicator, CharPtr- StartIndicator);

      StartIndicator:= CharPtr;

      while CharPtr^<> '&' do
      begin
        Inc (CharPtr);
        Dec (ReminderLen);

      end;

      Inc (CharPtr);
      Dec (ReminderLen);

      SetString (VarValue, StartIndicator, CharPtr- StartIndicator);
      CgiVar:= TCgiVar.Create (VarName, VarValue);

      Result.Add (CgiVar);

    end;

  end;

  procedure LoadCookies (var CharPtr: PChar);
  begin

  end;

begin
  FVars.Free;
//  FCookies.Free;

  FVars:= LoadVariables (RequestInfo.Variables);
//  LoadCookies (ArgumentPtr);

  MyDispatch;

end;

procedure TAbstractHandler.RegisterThread (AThread: TDispactherThread);
begin
  FThread:= AThread;
  PipeHandle:= AThread.OutputPipeHandle;

end;

end.

