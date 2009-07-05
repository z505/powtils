unit ThisProjectGlobalUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GlobalUnit, PersaDictionaryUnit;
  
type

  { TMyGlobalObjectContainers }

  TMyGlobalObjectContainers= class (TGlobalObjectContainer)
  private
    PersaDic: TPersaDic;

  public
    property PersaDic: TPersaDic read FPersaDic;

    constructor Create;

  end;
  
var
  GlobalObjContainer: TMyGlobalObjectContainers;
  
implementation
uses
  FileStringsUnit;
  
{ TMyGlobalObjectContainers }

constructor TMyGlobalObjectContainers.Create;
var
  FileString: TFileString;
  
begin
  inherited Create;
  
{
  FileString:= TFileString.Create ('Files/WelComePage.html', 'WelComePage');//
  FileStringCollection.AddFileString (FileString);
}

end;

initialization
  GlobalObjContainer:= TMyGlobalObjectContainers.Create;
  
finalization
  GlobalObjContainer.Free;
  
end.

