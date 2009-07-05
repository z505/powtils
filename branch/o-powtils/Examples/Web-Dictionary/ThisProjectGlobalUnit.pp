unit ThisProjectGlobalUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GlobalUnit, PersaDictionaryUnit;
  
type

  { TMyGlobalObjectContainers }

  TMyGlobalObjectContainers= class (TGlobalObjectContainer)
  private
    FAnsweredQuery: DWord;
    FPersaDic: TPersaDic;

  public
    property PersaDic: TPersaDic read FPersaDic;
    property AnsweredQuery: DWord read FAnsweredQuery;

    procedure IncreaseAnsweredQuery; inline;

    constructor Create;
    destructor Destroy; override;

  end;
  
var
  GlobalObjContainer: TMyGlobalObjectContainers;
  
implementation
uses
  FileStringsUnit;
  
{ TMyGlobalObjectContainers }

procedure TMyGlobalObjectContainers.IncreaseAnsweredQuery; inline;
begin
  Inc (FAnsweredQuery);

end;

constructor TMyGlobalObjectContainers.Create;
begin
  inherited Create;

  FPersaDic:= TPersaDic.CreateFromTextFile (
        Configurations.ConfigurationValueByName ['DicFileName']);
  FAnsweredQuery:= 0;

end;

destructor TMyGlobalObjectContainers.Destroy;
begin
  PersaDic.Free;

  inherited Destroy;
end;

initialization
  GlobalObjContainer:= TMyGlobalObjectContainers.Create;
  
finalization
  GlobalObjContainer.Free;
  
end.

