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
  WebStringUnit, RequestsQueue, XMLNode, AttributeUnit, ThisProjectGlobalUnit,
  ExceptionUnit, ThreadingUnit,

  LoginPageUnit, MainPageUnit;
  
var
  Resident: TResident;
  
begin

  Resident:= TResident.Create (nil);

  Resident.ExecuteInThread;
//  Resident.Execute;

  Resident.Free;

end.

