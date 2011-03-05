program PSP;

{$mode objfpc}{$H+}

uses
{$ifdef unix}
   cthreads, BaseUnix,
 {$endif}
  Classes, SysUtils
  {$IFDEF DEBUGMODE}
  , heaptrc
  {$ENDIF}
  { add your units here },
  {CollectionUnit, }WebUnit, ThreadingUnit, PipeWrapperUnit, URLEnc,
  SessionManagerUnit, WebStringUnit, RequestsQueue, ThisProjectGlobalUnit,
  ExceptionUnit, AbstractHandlerUnit, CookieUnit, WebHeaderUnit,
  WebConfigurationUnit, SessionUnit, CgiVariableUnit, WebRunTimeInformationUnit,
  WebUploadedFileUnit, LResources, ResidentApplicationUnit,
  GenericCollectionUnit, GenericNameValueCollectionUnit;

begin
//  Resident.RegisterPageHandlerHandler ();

  Resident.ExecuteInThread;

  Resident.Free;

end.

