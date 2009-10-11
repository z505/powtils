unit ThisProjectGlobalUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GlobalUnit, PersaDictionaryUnit;
  
type

  { TMyGlobalObjectContainers }

  TMyGlobalObjectContainers= class (TGlobalObjectContainer)
  private
    FPersaDic: TPersaDic;

  public
    property PersaDic: TPersaDic read FPersaDic;

    constructor Create;
    destructor Destroy; override;

  end;
  
var
  GlobalObjContainer: TMyGlobalObjectContainers;
  
implementation
uses
  FileStringsUnit;
  
{ TMyGlobalObjectContainers }

constructor TMyGlobalObjectContainers.Create;
begin
  inherited Create;

  FPersaDic:= TPersaDic.Create;
  PersaDic.LoadFromTextFile ('MergedDic.txt');

  PersaDic.Prepare;

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

