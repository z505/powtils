unit MainPageUnit; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PageHandlerBaseUnit, FileStringsUnit, AbstractHandlerUnit;

type

  { TMainPageDispatcher }

  TMainPageDispatcher= class (TXMLHandler)
  private

  public
    constructor Create;
    destructor Destroy; override;

    procedure MyDispatch; override;
    function CreateNewInstance: TAbstractHandler; override;

  end;

implementation
uses
  XMLNode;

{ TMainPageDispatcher }
constructor TMainPageDispatcher.Create;
begin
  inherited Create ('PersaDic', 'http://localhost/PersaDic/PersaDic.xsl');

end;

destructor TMainPageDispatcher.Destroy;
begin
  inherited Destroy;

end;

procedure TMainPageDispatcher.MyDispatch;
var
  QueryInfo: TXMLNode;

begin
  QueryInfo:= TXMLNode.Create ('QueryInfo');
  QueryInfo.AddAttribute ('Word', Vars.CgiVarValueByName ['Name']);

  XMLRoot.AddChild (QueryInfo);

end;

function TMainPageDispatcher.CreateNewInstance: TAbstractHandler;
begin
  Result:= TMainPageDispatcher.Create;

end;

initialization

finalization

end.

