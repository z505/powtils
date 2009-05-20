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
  Classes, SysUtils, CollectionUnit, ExceptionUnit, WebConfigurationUnit;

type
  { TWebHeader }

  THeader= class (TNameStrValue)
  public
    function ToString: String;

  end;

  {
    TContentType which is now an enumeration. It can be String, too.
  }
  TContentType= (ctStart, ctTextHTML, ctTextXML, ctNode);

  { THeaderCollection }
  {
    This class loads and holds the Header information.
  }
  THeaderCollection= class (TNameValueCollection)
  private
    function GetText: String;
    function GetHeader (Index: Integer): THeader;

  public
    property Header [Index: Integer]: THeader read GetHeader;

    property Text: String read GetText;

    procedure Init (Conf: TWebConfigurationCollection;
               PageContentType: TContentType);

    procedure AddHeader (NewHeader: THeader); overload;

  end;


implementation
uses
  ConstantsUnit;

{ TWebHeader }

function THeader.ToString: String;
begin
  Result:= FName+ ':'+ Value;

end;

{ TWebHeaderCollection }

function THeaderCollection.GetText: String;
var
  i: Integer;

begin
  if Count<> 0 then
    Result:= Header [0].ToString
  else
    Result:= '';

  for i:= 1 to Count- 1 do
  begin
    Result+= NewLine;
    Result+= Header [i].ToString;

  end;

end;

function THeaderCollection.GetHeader (Index: Integer): THeader;
begin
  Result:= NameValue [Index] as THeader;

end;

procedure THeaderCollection.Init (Conf: TWebConfigurationCollection;
  PageContentType: TContentType);
begin
  raise ENotImplementedYet.Create ('TWebConfigurationCollection', 'Init');

end;

procedure THeaderCollection.AddHeader (NewHeader: THeader);
begin
  AddNameValue (NewHeader);

end;

end.

