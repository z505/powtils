program SimpleSearch;

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
  CollectionUnit, WebUnit, ThreadingUnit, PipeWrapperUnit, URLEnc,
  SessionManagerUnit, WebStringUnit, RequestsQueue, ThisProjectGlobalUnit,
  ExceptionUnit, AbstractHandlerUnit, CookieUnit, WebHeaderUnit,
  WebConfigurationUnit, SessionUnit, CgiVariableUnit, WebRunTimeInformationUnit,
  WebUploadedFileUnit, LResources, ResidentApplicationUnit,
  JostarPageDispatcher, StreamUnit, ParserUnit, SpellCheckerUnit,
  PersaDictionaryUnit, SearchEnginesQueryManager, QueryRequestUnit,
ResultStorageUnit, StringUnit;

begin
  Resident:= TResident.Create (nil);

//  Resident.RegisterPageHandlerHandler ();

  Resident.ExecuteInThread;

  Resident.Free;

end.

