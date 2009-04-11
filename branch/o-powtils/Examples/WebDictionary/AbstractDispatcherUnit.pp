unit AbstractDispatcherUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Unix, RequestsQueue, CookieUnit, WebHeaderUnit, CgiVariableUnit,
    SessionManagerUnit;

type

  { TAbstractDispatcher }

  TAbstractDispatcher= class (TObject)
  private
    FCookies: TCookieCollection;
    FBuffer: TStringList;
    FHeaderCanBeSent: Boolean;
    FHeaders: THeaderCollection;
    FVars: TCgiVariableCollection;
    FPageName: String;
    FShouldBeFreedManually: Boolean;

  protected
    property ShouldBeFreedManually: Boolean read FShouldBeFreedManually;
    property Buffer: TStringList read FBuffer;

    FPipeHandle: cInt;
    FTempPipeFilePath: String;
    PipeIsAssigned: Boolean;
    FRequestURI: String;
    FHostName: String;
    FForceToSendHeader: Boolean;
    FSessionID: TSessionID;

    procedure SetPipeFileName (const Filename: String);

    procedure WriteToBuffer (const S: String);
    procedure Write (const S: String);
    procedure WriteLn (const S: String);
    procedure WriteHeaders;

  public
    property PageName: String read FPageName;
    property Cookies: TCookieCollection read FCookies;
    property Headers: THeaderCollection read FHeaders;
    property Vars: TCgiVariableCollection read FVars;
    property HeaderCanBeSent: Boolean read FHeaderCanBeSent;

    constructor Create (ThisPageName: String; _ShouldBeFreedManually: Boolean);
    destructor Destroy; override;

    function CreateNewInstance: TAbstractDispatcher; virtual; abstract;

    procedure Dispatch (var ArgumentPtr: PChar);

    procedure MyDispatch; virtual; abstract;

  end;

implementation
uses
  ExceptionUnit, BaseUnix;

{ TAbstractDispatcher }

procedure TAbstractDispatcher.SetPipeFileName (const Filename: String);
begin
  raise ENotImplementedYet.Create ('TAbstractDispatcher', 'SetPipeFileName');

end;

procedure TAbstractDispatcher.WriteToBuffer (const S: String);
begin
  FBuffer.Add (S);

end;

procedure TAbstractDispatcher.Write (const S: String);
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
    FHeaderCanBeSent:= False;

  end;

  if HeaderCanBeSent then
  begin
    FpWrite (FPipeHandle, NewLine [1], 2);
    FHeaderCanBeSent:= False;

  end;

  if FBuffer.Count<> 0 then
  begin
    BufText:= FBuffer.Text;
    FBuffer.Clear;

    FpWrite (FPipeHandle, BufText [1], Length (BufText));

  end;

  FpWrite (FPipeHandle, S [1], Length (S));

end;

procedure TAbstractDispatcher.WriteLn(const S: String);
const
  NewLine: String= #10;

begin
  Write (S+ NewLine);

end;

procedure TAbstractDispatcher.WriteHeaders;
var
  S: String;

begin
  FHeaderCanBeSent:= False;
  if Headers.Size<> 0 then
  begin
    S:= Headers.Text;
    Headers.Clear;
    Write (S);

  end;

  if Cookies.Size<> 0 then
  begin
    S:= Cookies.Text;
    Cookies.Clear;
    Write (S);

  end;

  Write (#10#10);
  FHeaderCanBeSent:= False;

end;

constructor TAbstractDispatcher.Create (ThisPageName: String;
          _ShouldBeFreedManually: Boolean);
begin
  inherited Create;

  FShouldBeFreedManually:= _ShouldBeFreedManually;
  FPageName:= ThisPageName;

  FBuffer:= TStringList.Create;
  FVars:= TCgiVariableCollection.Create;
  FHeaders:= THeaderCollection.Create;

  FCookies:= TCookieCollection.Create (FHeaders, @FHeaderCanBeSent, '', '');

end;

destructor TAbstractDispatcher.Destroy;
begin
  FBuffer.Free;
  FCookies.Free;
  FVars.Free;
  FHeaders.Free;

  inherited Destroy;

end;

procedure TAbstractDispatcher.Dispatch (var ArgumentPtr: PChar);
const
  SeparatorChar: char= #$FF;
var
  VarString, CookieString: String;

  procedure LoadVariables (var CharPtr: PChar);
  var
    StartIndicator: PChar;
    CgiVar: TCgiVar;
    VarName, VarValue: String;

  begin

    while CharPtr^<> SeparatorChar do
    begin
      StartIndicator:= CharPtr;

      while CharPtr^<> '=' do
        Inc (CharPtr);
      Inc (CharPtr);
      SetString (VarName, StartIndicator, CharPtr- StartIndicator);

      StartIndicator:= CharPtr;

      while CharPtr^<> '&' do
        Inc (CharPtr);
      Inc (CharPtr);
      SetString (VarValue, StartIndicator, CharPtr- StartIndicator);
      CgiVar:= TCgiVar.Create (VarName, VarValue);

    end;

  end;

  procedure LoadCookies (var CharPtr: PChar);
  begin

  end;

begin
  FVars.Free;
  FVars:= TCgiVariableCollection.Create;
//  FCookies.Free;
//  FCookies:= TCookieCollection.Create;

  LoadVariables (ArgumentPtr);
//  LoadCookies (ArgumentPtr);

  MyDispatch;

end;

end.

