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
    FPageName: AnsiString;
    FShouldBeFreedManually: Boolean;
    FContentType: TContentType;
    FThread: TDispactherThread;
    FRelativePath: AnsiString;
    FTempPipeFilePath: AnsiString;
    FRequestURI: AnsiString;
    FHostName: AnsiString;
    FForceToSendHeader: Boolean;

  protected
    PipeIsAssigned: Boolean;
    FSessionID: TSessionID;

  private
    function GetParameter (const ParamName: AnsiString): AnsiString;
    procedure SetPipeHandle (const AValue: cInt);

  published
    property PageName: AnsiString read FPageName write FPageName;
    property RelativePath: AnsiString read FRelativePath write FRelativePath;
    property ShouldBeFreedManually: Boolean read FShouldBeFreedManually;
    property Buffer: TStringList read FBuffer;
    property PipeHandle: cInt read FPipeHandle write SetPipeHandle;

  protected
    procedure WriteToBuffer (const S: AnsiString);
    procedure Write (const S: AnsiString);
    procedure WriteLn (const S: AnsiString);
    procedure WriteHeaders;

  protected
    property SessionID: TSessionID read FSessionID;

  public
    property Cookies: TCookieCollection read FCookies;
    property Headers: THeaderCollection read FHeaders;
    property Vars: TCgiVariableCollection read FVars;
    // It is the union of Name-value pairs in Vars and Cookies.
    property Parameter [const ParamName: AnsiString]: AnsiString read
      GetParameter;
    property HeaderCanBeSent: Boolean read FHeaderCanBeSent;

    constructor Create (ContType: TContentType; ThisPageName, ThisPageRelativePath: AnsiString;
                             _ShouldBeFreedManually: Boolean);
    destructor Destroy; override;

    function CreateNewInstance: TAbstractHandler; virtual; abstract;
    procedure Flush; virtual; abstract;

    procedure Dispatch (RequestInfo: TRequest; AThread: TDispactherThread);

    procedure MyDispatch; virtual; abstract;

  end;

implementation
uses
  BaseUnix, GlobalUnit, ConstantsUnit, ThisProjectGlobalUnit;

{ TAbstractHandler }

function TAbstractHandler.GetParameter (const ParamName: AnsiString): AnsiString;
begin
  try
    Result:= Vars.CgiVarByName [ParamName].Value;

  except
    try
      Result:= Cookies.CookieByName [ParamName].StrValue;
    except
      Result:= '';

    end;

  end;

end;

procedure TAbstractHandler.SetPipeHandle (const AValue: cInt);
begin
  FPipeHandle:= AValue;
  PipeIsAssigned:= True;

end;

procedure TAbstractHandler.WriteToBuffer (const S: AnsiString);
begin
  FBuffer.Add (S);

end;

procedure TAbstractHandler.Write (const S: AnsiString);
const
(*$I+*)
  BufText: AnsiString= '';
(*$I-*)

begin
  if HeaderCanBeSent then
  begin
    if Headers.Size<> 0 then
    begin
      BufText:= Headers.Text;
      Headers.Clear;
      FpWrite (FPipeHandle, BufText [1], Length (BufText));

    end;

    if Cookies.Size<> 0 then
    begin
      BufText:= Cookies.Text;
      Cookies.Clear;
      FpWrite (FPipeHandle, BufText [1], Length (BufText));

    end;

    FpWrite (FPipeHandle, NewLine [1], 1);
    FpWrite (FPipeHandle, NewLine [1], 1);
    FHeaderCanBeSent:= False;

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

procedure TAbstractHandler.WriteLn (const S: AnsiString);
begin
  Write (S+ NewLine);

end;

procedure TAbstractHandler.WriteHeaders;
var
  S: AnsiString;

begin
  if Headers.Size<> 0 then
  begin
    S:= Headers.Text;
    Headers.Free;
    FHeaders:= nil;
    FpWrite (FPipeHandle, S [1], Length (S));

  end;

  if Cookies.Size<> 0 then
  begin
    S:= Cookies.Text;
    Cookies.Clear;
    FpWrite (FPipeHandle, S [1], Length (S));

  end;

  FHeaderCanBeSent:= False;

end;

constructor TAbstractHandler.Create(ContType: TContentType; ThisPAgeName,
  ThisPageRelativePath: AnsiString; _ShouldBeFreedManually: Boolean);
const
  ContentTypeString: array [ctStart..ctNone] of String=
    ('', 'text/html', 'text/xml', '');

begin
  inherited Create;

  FShouldBeFreedManually:= _ShouldBeFreedManually;
  FPageName:= ThisPAgeName;
  FRelativePath:= ThisPageRelativePath;
  FContentType:= ContType;

  FBuffer:= TStringList.Create;
  FVars:= nil;
  FCookies:= nil;

  FHeaders:= THeaderCollection.Create;
  Headers.AddHeader (THeader.Create ('X-Powered-By', 'Powtils'));
  Headers.AddHeader (THeader.Create ('Content-Type',
               ContentTypeString [ContType] + ';charset='+ GlobalObjContainer.Configurations.ConfigurationValueByName ['CHARSET']
             ));

  FHeaderCanBeSent:= True;

end;

destructor TAbstractHandler.Destroy;
begin
  FBuffer.Free;
  FHeaders.Free;
  FCookies.Free;

  inherited Destroy;

end;

procedure TAbstractHandler.Dispatch (RequestInfo: TRequest; AThread: TDispactherThread);

  function LoadVariables (const VariablesStr: AnsiString): TCgiVariableCollection;
  var
    VariablesStrLen: Integer;

    function LoadAVariable (var CharPtr: PChar; var Index: Integer): TCgiVar;

      function URLDecode (const Str: AnsiString): AnsiString;

        function FindValue (ch: Char): Integer; inline;
        begin
          if ch in ['0'..'9'] then
            Result:= Ord (ch)- 48
          else if ch in ['A'..'Z'] then
            Result:= Ord (ch)- 55
          else if ch in ['a'..'z'] then
            Result:= Ord (ch)- 87;

        end;

      var
        CharPtr: PChar;
        i, Len: Integer;
        S: Integer;

      begin
        CharPtr:= @Str [1];
        Len:= Length (Str);
        Result:= '';
        i:= 1;

        while i<= Len do
        begin
          case CharPtr^ of
            '+':
              Result+= ' ';

            '%':
            begin
              if Len< i+ 2 then
                Exit;
              S:= FindValue ((CharPtr+ 1)^)* 16+ FindValue ((CharPtr+ 2)^);

              Inc (CharPtr, 2);
              Inc (i, 2);
              Result+= Chr (S);

            end
            else
              Result+= CharPtr^;

          end;

          Inc (CharPtr);
          Inc (i);

        end;

      end;

    var
      i: Integer;
      StartingPosition: PChar;
      Flag: Boolean;
      VarName, VarValue: AnsiString;

    begin
      VarName:= '';
      StartingPosition:= CharPtr;
      Flag:= False;

      for i:= Index to VariablesStrLen do
      begin
        if CharPtr^= '=' then
        begin
          SetString (VarName, StartingPosition, i- Index);
          Index:= i+ 1;
          Flag:= True;
          break;

        end;
        Inc (CharPtr);

      end;

      if not Flag then
        Exit (nil);

      VarValue:= '';
      Inc (CharPtr);
      StartingPosition:= CharPtr;
      Flag:= False;

      for i:= Index to VariablesStrLen do
      begin
        if CharPtr^= '&' then
        begin
          SetString (VarValue, StartingPosition, i- Index);
          Index:= i+ 1;
          Inc (CharPtr);
          Flag:= True;
          break;

        end;
        Inc (CharPtr);

      end;

      if not Flag then
      begin
        SetString (VarValue, StartingPosition, VariablesStrLen- Index+ 1);
        Index:= VariablesStrLen;

      end;

      Result:= TCgiVar.Create (URLDecode (VarName), URLDecode (VarValue));

    end;

  var
    CharPtr: PChar;
    ActiveIndex: Integer;
    AVar: TCgiVar;

  begin
    Result:= TCgiVariableCollection.Create;

    CharPtr:= @VariablesStr [1];
    ActiveIndex:= 1;
    VariablesStrLen:= Length (VariablesStr);

    while ActiveIndex< VariablesStrLen do
    begin
      AVar:= LoadAVariable (CharPtr, ActiveIndex);
      if AVar<> nil then
        Result.Add (AVar)
      else
        Break;

    end;

  end;

  function LoadCookies (const CookieString: Ansistring): TCookieCollection;
  begin
    Result:= TCookieCollection.Create (Headers, nil, RelativePath);

  end;

begin
(*$IFDEF DEBUGMODE*)
  System.WriteLn ('In Dispatch: Before LoadVariables');
(*$ENDIF*)

  FVars:= LoadVariables (RequestInfo.Variables);
  FCookies:= LoadCookies (RequestInfo.CookieStr);

(*$IFDEF DEBUGMODE*)
  System.WriteLn ('In Dispatch: After LoadVariables');
(*$ENDIF*)

  FThread:= AThread;
  PipeHandle:= AThread.OutputPipeHandle;

(*$IFDEF DEBUGMODE*)
  System.WriteLn ('In Dispatch: Before Calling MyDispatch');
(*$ENDIF*)

  MyDispatch;

(*$IFDEF DEBUGMODE*)
  System.WriteLn ('In Dispatch: After Calling MyDispatch');
(*$ENDIF*)

  FVars.Free;

(*$IFDEF DEBUGMODE*)
  System.WriteLn ('In Dispatch: After Freeing FVars');
(*$ENDIF*)


end;

end.

