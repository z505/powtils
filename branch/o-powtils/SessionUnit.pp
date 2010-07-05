unit SessionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit, CookieUnit, WebConfigurationUnit,
    WebRunTimeInformationUnit;

type
  { TSessionCollection }

  {
    This class loads and hold and save the session information.
  }
  TSessionCollection= class (TNameValueCollection)
  private
    FCookieCollection: TCookieManager;
    FRunTimeInformation: TWebRunTimeInformationCollection;
//    SdsEngine: TSDSWrapper;
    FSessionIsRegistered: Boolean;


    function SessionGarbageCollector: Boolean;
  public
    property SessionIsRegistered: Boolean read FSessionIsRegistered write FSessionIsRegistered;

    constructor Create (CookieManager: TCookieManager);
    destructor Destroy; override;

    function SessionStart: String;

  end;



implementation
uses
  ExceptionUnit;
{ TSessionCollection }

function TSessionCollection.SessionGarbageCollector: Boolean;
(*
var
  SessionTable,
  SessionLimit: String;
  Res: SDS_Result;
  SessionTime: LongWord;
  LimitDate: TDateTime;
*)
begin
  raise ENotImplementedYet.Create ('TSessionCollection', 'SessionGarbageCollector');
(*  Result:= False;

  // Checking
  SessionTable:= FConfigurationCollection.ConfigurationByName ['session_path'].Value;

  if not FileExists (SessionTable) then
  begin
    // Searching in system temp
    if FileExists({$IFDEF WIN32}GetEnvVar('WINDIR') + '\'{$ENDIF}{$IFDEF UNIX}'/tmp/'{$ENDIF}+ PWU_SESS_PATH) then
      SessionTable:= {$IFDEF WIN32}GetEnvVar('WINDIR') + '\'{$ENDIF}{$IFDEF UNIX}'/tmp/'{$ENDIF}+ PWU_SESS_PATH
    else
      Exit (False);
  end;

  // Checking lifetime in minutes
//  SessionTime:= FConfigurationCollection.NameValueByName ['session_life_time'].Value.ToInt;
  if SessionTime= 0 then
    Exit (True);

  LimitDate:= Now- (SessionTime div 1440);

  SessionLimit:= FormatDateTime ('yyyy-mm-dd hh:nn:ss', LimitDate);

  // Performing GC
  Res:= SdsEngine.Query ('DELETE FROM `'+ SessionTable+ '` WHERE modified < "'+ SessionLimit+ '"');

  if SdsEngine.Result_Error (Res)= '' then
    Result:= True;

  SdsEngine.Free_Result (Res);
*)
end;

constructor TSessionCollection.Create (CookieManager: TCookieManager);

begin
  inherited Create;
  raise ENotImplementedYet.Create ('TSessionCollection', 'Create');

(*
  FCookieCollection:= CookieCollection;
  FConfigurationCollection:= ConfigurationCollection;
  SdsEngine:= TSDSWrapper.Create;
*)
end;

destructor TSessionCollection.Destroy;
begin
//  SdsEngine.Free;
  raise ENotImplementedYet.Create ('TSessionCollection', 'Destroy');

  inherited;

end;

function TSessionCollection.SessionStart: String;
(*
var
  SessionTable: String;
  Res: SDS_Result;
  Row: SDS_Array;
  Key, SessionID: string;
  *)
begin
  raise ENotImplementedYet.Create ('TSessionCollection', 'SessionStart');
(*
  // Init
  Result:= '';

  // Checking path
  SessionTable:= FConfigurationCollection.ConfigurationByName ['session_path'].Value;

  if not FileExists (SessionTable) then
  begin
    // Searching in system temp
    if FileExists ({$IFDEF WIN32}GetEnvVar('WINDIR') + '\'{$ENDIF}{$IFDEF UNIX}'/tmp/'{$ENDIF}+ PWU_SESS_PATH) then
      SessionTable:= {$IFDEF WIN32}GetEnvVar('WINDIR') + '\'{$ENDIF}{$IFDEF UNIX}'/tmp/'{$ENDIF}+ PWU_SESS_PATH
    else
      Exit ('');
  end;

  // Running garbage collector
  SessionGarbageCollector;

  // Is it registered
  if not FCookieCollection.IsExists ('PWUSESS') then
  begin
    FSessionIsRegistered:= False;
    FRunTimeInformation.Add (TWebRunTimeInformation.Create ('SESSION_REGISTERED', 'FALSE'));
    Exit ('');
  end;
  FSessionIsRegistered:= True;

  FRunTimeInformation.AddRunTimeInformation (TWebRunTimeInformation.Create ('SESSION_REGISTERED', 'TRUE'));

  Key:= base64_decode (FCookieCollection.GetCookieByName ('PWUSESS').Value);
  SessionID:= sds_escape (Copy (key, 13, Length (Key)- 12));
  Key:= sds_escape (Copy (Key, 1, 12));

  // Selecting
  Res:= SdsEngine.Query ('SELECT data FROM `'+ SessionTable+ '` WHERE id = '+ SessionID+ ' AND key = "'+ Key+ '"');

  if sds_result_rows(Res) = 1 then
  begin
    row:= sds_fetch_row (Res);
    Result:= base64_decode (sds_fetch_column (Row, 0));
    SdsEngine.Free_Row (Row);
  end
  else
  begin
    Result:= '';
    // Unset the cookie, it has timed out
    FCookieCollection.RemoveCookieByName ('PWUSESS');
  end;
  sds_free_result (Res);
  *)

end;

end.

