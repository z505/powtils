program PersaDic;

{$mode objfpc}{$H+}
//{$Define DebugMode}
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
  WebUploadedFileUnit, MainPageUnit, PuzzlePageUnit, MyTypes, LResources,
  StreamUnit, ResidentApplicationUnit, QueueUnit, SemaphoreUnit,
  BlockingQueueUnit;

begin

  Resident:= TResident.Create (nil);

  Resident.RegisterPageHandlerHandler (TMainPageDispatcher.Create);
//  Resident.RegisterPageHandlerHandler (TPuzzlePageDispatcher.Create);

  Resident.ExecuteInThread;

  Resident.Free;

end.

