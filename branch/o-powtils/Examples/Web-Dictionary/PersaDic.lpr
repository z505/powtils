program PersaDic;

{$mode objfpc}{$H+}
{$Define DebugMode}
uses
{$ifdef unix}
   cthreads, BaseUnix,
 {$endif}
  Classes, heaptrc
  { add your units here }, ResidentApplicationUnit,
  CollectionUnit, WebUnit, ThreadingUnit,
  PipeWrapperUnit, URLEnc, SessionManagerUnit,
  WebStringUnit, RequestsQueue, XMLNode, AttributeUnit, ThisProjectGlobalUnit,
  ExceptionUnit, AbstractHandlerUnit, CookieUnit, WebHeaderUnit,
  WebConfigurationUnit, SessionUnit, CgiVariableUnit,
  WebRunTimeInformationUnit, WebUploadedFileUnit, MainPageUnit, ConstantsUnit,
  MeaningUnit;
  
begin

  Resident:= TResident.Create;
  Resident.RegisterPageHandlerHandler ('PersaDic.psp', TMainPageDispatcher.Create);

  Resident.ExecuteInThread;
//  Resident.Execute;

  Resident.Free;

end.

