unit AdminTopPageUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ResidentPageBaseUnit;

type

  { TAdminTopPage }

  TAdminTopPage= class (TResidentPageBase)
  private
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure MyDispatch; override;
    
  end;
  
implementation
uses
  ThisProjectGlobalUnit;
  
{ TAdminTopPage }

constructor TAdminTopPage.Create;
begin
  inherited Create ('AdminTopPage');
  
end;

destructor TAdminTopPage.Destroy;
begin

  inherited Destroy;
end;

procedure TAdminTopPage.MyDispatch;
begin
  WriteToPipe (GlobalObjContainer.FileStringCollection.
             FileStringByName ['AdminTopPage1'].DataInFile.Text);
  WriteToPipe (DateTimeToStr (Now));
  WriteToPipe (GlobalObjContainer.FileStringCollection.
             FileStringByName ['AdminTopPage2'].DataInFile.Text);

end;

end.

