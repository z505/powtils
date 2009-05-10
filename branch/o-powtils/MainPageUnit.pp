unit MainPageUnit; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PageHandlerBaseUnit, FileStringsUnit, AbstractHandlerUnit;

type

  { TMainPageDispatcher }

  TMainPageDispatcher= class (THTMLHandlerPageBase)
  private
    MainPage_HTMLData: TFileString;

  public
    constructor Create;
    destructor Destroy; override;

    procedure MyDispatch; override;
    function CreateNewInstance: TAbstractHandler; override;

  end;

implementation


{ TMainPageDispatcher }
constructor TMainPageDispatcher.Create;
begin
  inherited Create;

  MainPage_HTMLData:= TFileString.Create ('HTMLPages/MainPage.html', 'MainPage');

end;

destructor TMainPageDispatcher.Destroy;
begin
  MainPage_HTMLData.Free;

  inherited Destroy;
end;

procedure TMainPageDispatcher.MyDispatch;
begin
  WriteLn (MainPage_HTMLData.DataInFile);

end;

function TMainPageDispatcher.CreateNewInstance: TAbstractHandler;
begin
  Result:= TMainPageDispatcher.Create;

end;

initialization

finalization

end.

