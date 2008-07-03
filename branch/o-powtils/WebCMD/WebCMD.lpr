program WebCMD;

{$mode objfpc}{$H+}

uses
{$ifdef unix}
   cthreads, BaseUnix,
 {$endif}
  Classes, heaptrc
  { add your units here }, ResidentApplicationUnit,
  CollectionUnit, WebUnit,
  ThisApplicationPagesUnit, PipeWrapperUnit, URLEnc, SessionManagerUnit,
  WebStringUnit, RequestsQueue, ThisProjectGlobalUnit,
  ExceptionUnit, ThreadingUnit, XMLNode, AttributeUnit,
  LoginPageUnit, MainPageUnit;
  
begin

  Resident:= TResident.Create (nil);

  Resident.ExecuteInThread;
//  Resident.Execute;

  Resident.Free;

end.

