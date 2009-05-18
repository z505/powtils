unit MainPageUnit; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PageHandlerBaseUnit, FileStringsUnit, AbstractHandlerUnit;

type

  { TMainPageDispatcher }

  TMainPageDispatcher= class (TXMLHandlerPage)
  private

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
  inherited Create ('PersaDic', 'Persadic.xsl');

end;

destructor TMainPageDispatcher.Destroy;
begin
  inherited Destroy;

end;

procedure TMainPageDispatcher.MyDispatch;
begin
  WriteLn (Vars.Text);

end;

function TMainPageDispatcher.CreateNewInstance: TAbstractHandler;
begin
  Result:= TMainPageDispatcher.Create;

end;

initialization

finalization

end.

