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
  Page1ResidentUnit;


function GetAppropriatePageByPageName (PageName: String): TResidentPageBase;
begin

  if PageName= 'INDEXPAGE.PSP' then
    Result:= TMyWebPage1.Create
  else
    raise EpageNotFound.Create (PageName);
  
end;

{ EPageNotFound }

constructor EPageNotFound.Create (PageName: String);
begin
  inherited Create ('Page '+ PageName+ ' not Found!');

end;

end.

