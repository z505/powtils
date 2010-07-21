unit WelcomePageHandlerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PageHandlerBaseUnit;

type

  { TWelcomePageHandler }

  TWelcomePageHandler= class (TXMLHandler)
  private

  public
    constructor Create;
    destructor Destroy; override;

  end;

implementation

{ TWelcomePageHandler }

constructor TWelcomePageHandler.Create;
begin
  inherited Create ('WelcomePage', '/WelcomePage.xslt', 'WelcomePage.psp');

end;

destructor TWelcomePageHandler.Destroy;
begin
  inherited Destroy;
end;

end.

