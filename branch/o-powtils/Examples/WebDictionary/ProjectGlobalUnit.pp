unit GlobalUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MeaningUnit;
type

  { TGlobalObjectContainer }

  TGlobalObjectContainer= class (TObject)
  private
    FFileManager: TFileManager;
    FWMIndex: TWMIndex;
  public
    property WMIndex: TWMIndex read FWMIndex;
    property FileManager: TFileManager read FFileManager;
    
    constructor Create;
    procedure Free;
    
  end;
var
  Globals: TGlobalObjectContainer;
  
implementation

{ TGlobalObjectContainer }

constructor TGlobalObjectContainer.Create;
begin
  inherited;
  
  FWMIndex:= TWMIndex.Create;
  FWMIndex.LoadFromFile ('IndexDic2-1.out');
//  FWMIndex.LoadFromFile ('TempIndex.out');
  FFileManager:= TFileManager.Create (5, 'Dic2-2.out');
  WriteLn ('FWMIndex is Created!');

end;

procedure TGlobalObjectContainer.Free;
begin
  FWMIndex.Free;
  FFileManager.Free;
  
  inherited;
  
end;

initialization
  Globals:= TGlobalObjectContainer.Create;
  
finalization
  Globals.Free;
  
end.

