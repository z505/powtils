program WebCMD;

{$mode objfpc}{$H+}

uses
{$ifdef unix}
   cthreads, BaseUnix,
 {$endif}
  Classes, SysUtils, LResources
  {$IFDEF DEBUGMODE}
  , heaptrc
  {$ENDIF}
  { add your units here },
  WebUnit, ThreadingUnit, PipeWrapperUnit, URLEnc,
  SessionManagerUnit, WebStringUnit, RequestsQueue, ThisProjectGlobalUnit,
  ExceptionUnit, AbstractHandlerUnit, CookieUnit, WebHeaderUnit,
  WebConfigurationUnit, SessionUnit, CgiVariableUnit, WebRunTimeInformationUnit,
  WebUploadedFileUnit, ResidentApplicationUnit,
  GenericCollectionUnit, GenericNameValueCollectionUnit;

begin
//  Resident.RegisterPageHandlerHandler ();

  Resident.ExecuteInThread;

  Resident.Free;

end.

