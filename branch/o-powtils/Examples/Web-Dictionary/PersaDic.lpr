program PersaDic;

{$mode objfpc}{$H+}
//{$Define DebugMode}
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
  SpellCheckerUnit, GlobalUnit, PuzzlePageUnit, StatisticsDispatcherUnit;

var
  PageHandler: TAbstractHandler;

begin

  Resident:= TResident.Create (nil);
  PageHandler:= TMainPageDispatcher.Create;
//  PageHandler.
  Resident.RegisterPageHandlerHandler ('PersaDic.psp', TMainPageDispatcher.Create);
  Resident.RegisterPageHandlerHandler ('PersaPuzzle.psp', TPuzzlePageDispatcher.Create);

  if Resident.Install then
  begin
    WriteLn ('Installation was successful');
    Resident.ExecuteInThread;

  end
  else
    WriteLn ('Installation was not successful. Exiting!');

  Resident.Free;

end.

