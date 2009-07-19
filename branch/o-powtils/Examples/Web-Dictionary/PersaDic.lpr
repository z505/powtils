program PersaDic;

{$mode objfpc}{$H+}
{$Define DebugMode}
uses
{$ifdef unix}
   cthreads, BaseUnix,
 {$endif}
  Classes
  { add your units here }, ResidentApplicationUnit,
  CollectionUnit, WebUnit, ThreadingUnit,
  PipeWrapperUnit, URLEnc, SessionManagerUnit,
  WebStringUnit, RequestsQueue, XMLNode, AttributeUnit, ThisProjectGlobalUnit,
  ExceptionUnit, AbstractHandlerUnit, CookieUnit, WebHeaderUnit,
  WebConfigurationUnit, SessionUnit, CgiVariableUnit,
  WebRunTimeInformationUnit, WebUploadedFileUnit, MainPageUnit, ConstantsUnit,
  PersaDictionaryUnit, DictionaryTreeUnit, StreamUnit,
  SpellCheckerUnit, GlobalUnit, PuzzlePageUnit;
  
begin

  Resident:= TResident.Create;
  Resident.RegisterPageHandlerHandler ('PersaDic.psp', TMainPageDispatcher.Create);
  Resident.RegisterPageHandlerHandler ('PersaPuzzle.psp', TPuzzlePageDispatcher.Create);

  Resident.ExecuteInThread;
//  Resident.Execute;

  Resident.Free;

end.

