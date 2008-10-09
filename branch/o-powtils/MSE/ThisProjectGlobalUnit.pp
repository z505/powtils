unit ThisProjectGlobalUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GlobalUnit;
  
type

  { TMyGlobalObjectContainers }

  TMyGlobalObjectContainers= class (TGlobalObjectContainer)
  private
    
  public
    
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
  
  FileString:= TFileString.Create ('HTML Templates/Admin/TopPage1.html', 'AdminTopPage1');
  FileStringCollection.AddFileString (FileString);
  FileString:= TFileString.Create ('HTML Templates/Admin/TopPage2.html', 'AdminTopPage2');
  FileStringCollection.AddFileString (FileString);
  FileString:= TFileString.Create ('HTML Templates/Admin/LoginPage.html', 'AdminLoginPage');
  FileStringCollection.AddFileString (FileString);
  FileString:= TFileString.Create ('HTML Templates/Admin/MainPage.html', 'AdminMainPage');
  FileStringCollection.AddFileString (FileString);
  FileString:= TFileString.Create ('HTML Templates/Admin/GetURL.html', 'GetURLPage');
  FileStringCollection.AddFileString (FileString);
  FileString:= TFileString.Create ('HTML Templates/Admin/GetURLResultPage-1.html', 'GetURLResultPage-1');
  FileStringCollection.AddFileString (FileString);
  FileString:= TFileString.Create ('HTML Templates/Admin/GetURLResultPage-2.html', 'GetURLResultPage-2');
  FileStringCollection.AddFileString (FileString);

end;

initialization
  GlobalObjContainer:= TMyGlobalObjectContainers.Create;
  
finalization
  GlobalObjContainer.Free;
  
end.

