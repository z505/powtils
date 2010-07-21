program Logbook;

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
  CollectionUnit, WebUnit, ThreadingUnit,
  PipeWrapperUnit, URLEnc, SessionManagerUnit,
  WebStringUnit, RequestsQueue, ThisProjectGlobalUnit,
  ExceptionUnit, AbstractHandlerUnit, CookieUnit, WebHeaderUnit,
  WebConfigurationUnit, SessionUnit, CgiVariableUnit,
  WebRunTimeInformationUnit, WebUploadedFileUnit, LResources,
  ResidentApplicationUnit, WelcomePageHandlerUnit;

begin
  Resident:= TResident.Create (nil);

  Resident.RegisterPageHandlerHandler (TWelcomePageHandler.Create);

  Resident.Install;
//  Resident.ExecuteInThread;

  Resident.Free;

end.

