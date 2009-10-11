unit AdminPageDispatcherUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PageHandlerBaseUnit, AbstractHandlerUnit;

type

  { TAdminPageHandler }

  TAdminPageHandler= class (THTMLHandler)
  private
    FAdminPagePath: AnsiString;

  public
    constructor Create (PagePath: AnsiString);

    function CreateNewInstance: TAbstractHandler; override;
    procedure MyDispatch; override;



  end;

implementation

{ TAdminPageHandler }

constructor TAdminPageHandler.Create (PagePath: AnsiString);
begin
  FAdminPagePath:= PagePath;

  inherited Create ('AdminPage.psp', PagePath+ 'AdminPage.psp');

end;

function TAdminPageHandler.CreateNewInstance: TAbstractHandler;
begin
  Result:= TAdminPageHandler.Create (FAdminPagePath);

end;

procedure TAdminPageHandler.MyDispatch;
begin
  WriteLn ('You are in admin page!');

end;

end.

