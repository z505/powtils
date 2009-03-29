program PSP;

{$mode objfpc}{$H+}

uses
{$ifdef unix}
   cthreads, BaseUnix,
 {$endif}
  Classes, heaptrc
  { add your units here }, ResidentApplicationUnit,
  Page1ResidentUnit, CollectionUnit, WebUnit, Page2ResidentUnit,
  ThisApplicationPagesUnit, PipeWrapperUnit, URLEnc, SessionManagerUnit,
  WebStringUnit, RequestsQueue, XMLNode, AttributeUnit, ThisProjectGlobalUnit,
  ExceptionUnit, AbstractDispatcherUnit, CookieUnit, WebHeaderUnit,
  WebConfigurationUnit, SessionUnit, CgiVariableUnit,
WebRunTimeInformationUnit, WebUploadedFileUnit;
  
var
  Resident: TResident;
  
begin

  Resident:= TResident.Create (nil);

  Resident.ExecuteInThread;
//  Resident.Execute;

  Resident.Free;

end.

