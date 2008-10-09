program Jostar;

{$mode objfpc}{$H+}

uses
{$ifdef unix}
   cthreads, BaseUnix,
 {$endif}
  Classes//, heaptrc
  { add your units here }, ResidentApplicationUnit, GetURLPageUnit,
  SEThreadingUnit, indylaz, SEResultParserUnit, SEHTTPSenderUnit;
//  LoginPageUnit, MainPageUnit;
  
begin

  Resident:= TResident.Create (nil);

  Resident.ExecuteInThread;
//  Resident.Execute;

  Resident.Free;

end.

