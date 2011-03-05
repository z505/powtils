{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                    o-PSP WebHeaderUnit

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

--------------------------------------------------------------------------------
 WebHeader Unit
--------------------------------------------------------------------------------

 O-PSP
 ---------

  [14/Mar/2009- Amir]
    - TWebHeader is simply a name-value class. It is used to store the headers
    in the respone.
    - TWebHeaderCollection is a collection of TWebHeaders.

}

unit WebHeaderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {CollectionUnit, }ExceptionUnit, WebConfigurationUnit;

type
  { TWebHeader }

  THeader = class(TNameStrValue)
  public
    function ToString: string;

  end;

  {
    TContentType which is now an enumeration. It can be String, too.
  }
  TContentType = (ctStart, ctTextHTML, ctTextXML, ctNone);

  { THeaderCollection }
  {
    This class loads and holds the Header information.
  }
  THeaderCollection = class(TNameValueCollection)
  private
    function GetText: string;
    function GetHeader(Index: integer): THeader;

  public
    property Header[Index: integer]: THeader Read GetHeader;

    property Text: string Read GetText;

    procedure Init(PageContentType: TContentType);

    procedure AddHeader(NewHeader: THeader); overload;

  end;


implementation

uses
  ConstantsUnit;

{ TWebHeader }

function THeader.ToString: string;
begin
//  Result := FName + ':' + Value;

end;

{ TWebHeaderCollection }

function THeaderCollection.GetText: ansistring;
var
  i: integer;

begin
  if Count <> 0 then
    Result := Header[0].ToString
  else
    Result := '';

  for i := 1 to Count - 1 do
  begin
    Result := Result + NewLine;
    Result := Result + Header[i].ToString;

  end;

end;

function THeaderCollection.GetHeader(Index: integer): THeader;
begin
//  Result := NameValue[Index] as THeader;

end;

procedure THeaderCollection.Init(PageContentType: TContentType);
begin
  raise ENotImplementedYet.Create('TWebConfigurationCollection', 'Init');

end;

procedure THeaderCollection.AddHeader(NewHeader: THeader);
begin
//  AddNameValue(NewHeader);

end;

end.

