{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                    PSP 1.6.x ThisApplicationPagesUnit

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

--------------------------------------------------------------------------------
 This Application Pages Unit
--------------------------------------------------------------------------------

 PSP 1.6.x
 ---------

  [30/MAR/2006 - Amir]
   - This unit is only contain the GetAppropriatePageByPageName which must be
   dynamically created (But not in this version. This unit is vary from one
   application to another.
}

unit ThisApplicationPagesUnit;

{$mode objfpc}{$H+}

interface
uses
  ResidentPageBaseUnit, SysUtils;

type
{ EPageNotFound }

  EPageNotFound= class (Exception)
    constructor Create (PageName: String);
    
  end;

function GetAppropriatePageByPageName (PageName: String): TResidentPageBase;

implementation

uses
  AdminTopPageUnit, AdminMainPageUnit, GetURLPageUnit;


function GetAppropriatePageByPageName (PageName: String): TResidentPageBase;
begin
  PageName:= UpperCase (PageName);

  if PageName= 'ADMINTOPPAGE' then
    Result:= TAdminTopPage.Create
  else if PageName= 'ADMINLOGINPAGE.PSP' then
    Result:= TAdminMainPage.Create
  else if PageName= 'ADMINMAINPAGE.PSP' then
    Result:= TAdminMainPage.Create
  else if PageName= 'ADMINGETURL.PSP' then
    Result:= TGetURLPage.Create
  else if PageName= 'ADMINTESTSE.PSP' then
    Result:= TAdminTestSE1.Create
  else
  begin
    WriteLn ('Page Not Found: ', PageName);
    raise EpageNotFound.Create (PageName);
  end;
  
end;

{ EPageNotFound }

constructor EPageNotFound.Create (PageName: String);
begin
  inherited Create ('Page '+ PageName+ ' not Found!');

end;

end.

