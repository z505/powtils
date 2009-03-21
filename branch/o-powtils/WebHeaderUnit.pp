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

  TWebHeader= class (TNameStrValue)
  public
    function ToString: String;

  end;

  {
    TContentType which is now an enumeration. It can be String, too.
  }
  TContentType= (ctStart, ctTextHTML, ctTextXML, ctNode);

  { TWebHeaderCollection }
  {
    This class loads and holds the Header information.
  }
  TWebHeaderCollection= class (TNameValueCollection)
  private
    function GetText: String;
    function GetWebHeader (Index: Integer): TWebHeader;

  public
    property WebHeader [Index: Integer]: TWebHeader read GetWebHeader;

    property Text: String read GetText;

    procedure Init (Conf: TWebConfigurationCollection;
               PageContentType: TContentType);

    procedure Clear;
    procedure AddHeader (NewHeader: TWebHeader); overload;

  end;


implementation

{ TWebHeader }

function TWebHeader.ToString: String;
begin
  Result:= FName+ ':'+ Value;

end;

{ TWebHeaderCollection }

function TWebHeaderCollection.GetText: String;
begin
  raise ENotImplementedYet.Create ('TWebHeaderCollection', 'GetText');

end;

function TWebHeaderCollection.GetWebHeader (Index: Integer): TWebHeader;
begin
  Result:= NameValue [Index] as TWebHeader;

end;

procedure TWebHeaderCollection.Init(Conf: TWebConfigurationCollection;
  PageContentType: TContentType);
begin
  raise ENotImplementedYet.Create ('TWebConfigurationCollection', 'Init');

end;

procedure TWebHeaderCollection.Clear;
begin
  Clear;

end;

procedure TWebHeaderCollection.AddHeader (NewHeader: TWebHeader);
begin
  AddNameValue (NewHeader);

end;

end.

