unit WebRunTimeInformationUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils{, CollectionUnit};

type
  { TWebRunTimeInformation }

{TODO: 14}
//  TWebRunTimeInformation= class (TNameStrValue)
  TWebRunTimeInformation= class (TObject)
  private
  public
  end;

  { TWebRunTimeInformationCollection }
  {
    This class loads and hold the run time information.
  }

  TWebRunTimeInformationCollection= class (TStringList)
  private
    function GetRunTimeInformationByIndex (Index: Integer): TWebRunTimeInformation;
    function GetRunTimeInformationByName (Name: String): TWebRunTimeInformation;
    function GetRunTimeInformationValueByName(Name: String): String;
  public
    property RunTimeInformation [Index: Integer]: TWebRunTimeInformation
             read GetRunTimeInformationByIndex;
    property RunTimeInformationByName [Name: String]: TWebRunTimeInformation
             read GetRunTimeInformationByName;
    property RunTimeInformationValueByName [Name: String]: String
             read GetRunTimeInformationValueByName;

  end;


implementation

{ TWebRunTimeInformationCollection }

function TWebRunTimeInformationCollection.GetRunTimeInformationByIndex (Index: Integer): TWebRunTimeInformation;
begin
{TODO: 16}
//  Result:= NameValue [Index] as TWebRunTimeInformation;

end;

function TWebRunTimeInformationCollection.GetRunTimeInformationByName (Name: String): TWebRunTimeInformation;
begin
{TODO: 17}
//  Result:= NameValueByName [Name] as TWebRunTimeInformation;

end;

function TWebRunTimeInformationCollection.GetRunTimeInformationValueByName (Name: String): String;
begin
{TODO: 16}
//  Result:= RunTimeInformationByName [Name].Value;

end;

end.

