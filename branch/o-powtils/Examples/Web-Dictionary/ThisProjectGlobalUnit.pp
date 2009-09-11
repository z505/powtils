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
{
  FPersaDic.LoadFromTextFile1 ('NewDic.txt');
  FPersaDic.LoadFromTextFile ('NDic.txt');
  FPersaDic.SaveAsTextFile ('MergedDic.txt');
}
  FPersaDic.Prepare;

  FPersaDic.SaveAsTextFile ('MergedDic.txt');

end;

destructor TMyGlobalObjectContainers.Destroy;
begin
  PersaDic.Free;

  inherited Destroy;
end;

initialization
  GlobalObjContainer:= TMyGlobalObjectContainers.Create;
  halt;
  
finalization
  GlobalObjContainer.Free;
  
end.

