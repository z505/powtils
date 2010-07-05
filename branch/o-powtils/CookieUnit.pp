{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                    o-PSP WebHeaderUnit

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

--------------------------------------------------------------------------------
 Cookie Unit
--------------------------------------------------------------------------------

 O-PSP
 ---------

  [14/Mar/2009- Amir]
    - TCookie is used to store a cookie information in the respone.

  [23/DEC/2009- Amir]
    - I Changed the implementation of TCookie. There is no change its interface.

}

unit CookieUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit, WebHeaderUnit;

type
  { ECookieNotFound }

  ECookieNotFound= class (Exception)
  public
    constructor Create;
    constructor Create (Msg: String);

  end;

  { TCookie }

  TCookie= class (TNameValue)
  private
    function GetStrValue: String;

  protected
    FDomain: String;
    FExpires: TDateTime;
    FPath: String;

  public
    property Domain: String read FDomain;
    property Path: String read FPath;
    property Expires: TDateTime read FExpires;
    property StrValue: String read GetStrValue;

    constructor Create (CookieName, CookieValue: String;
        CookieExpireTime: TDateTime= 0; CookiePath: String= ''; CookieDomain: String= '');
    destructor Destroy; override;

    function ToString (Mode: Integer= 0): String;

  end;

  { TCookieManager }

  TCookieManager= class (TNameValueCollection)
  private
    FHostName: String;
    FPageURI: String;
    FWebHeaderCollection: THeaderCollection;
    FIsHeaderSent: PBoolean;

    function GetCookie (Index: Integer): TCookie;
    function GetCookieByName (Name: String): TCookie;
    function GetCookieValueByName (Name: String): String;
    function GetText: String;

  public
    property Cookie [Index: Integer]: TCookie read GetCookie;
    property CookieByName [Name: String]: TCookie read GetCookieByName;
    property CookieValueByName [Name: String]: String read GetCookieValueByName;
    property HostName: String read FHostName;
    property PageURI: String read FPageURI;
    property Text: String read GetText;

    constructor Create (HeaderCollection: THeaderCollection; IsHeaderSent: PBoolean;
       ThisPageURI: String);

    destructor Destroy; override;

    procedure LoadFromString (CookieString: String);
    procedure WriteCookie;

    procedure Add (NewCookie: TCookie); overload;
    procedure Add (CookieName, CookieValue: String;
        CookieExpireTime: TDateTime= ''; CookiePath: String= ''; CookieDomain: String= ''); overload;
    procedure RemoveCookieByName (Name: String);

  end;


implementation
uses
  WebStringUnit, ExceptionUnit, MyTypes, ThisProjectGlobalUnit;

{ TCookie }

function TCookie.GetStrValue: String;
begin
  Result:= (Value as TWebString).ToString;


end;

constructor TCookie.Create (CookieName, CookieValue: String;
  CookieExpireTime: TDateTime; CookiePath: String; CookieDomain: String);
var
  PValue: PString;

begin
  PValue:= new (PString);
  PValue^:= CookieValue;
  inherited Create (CookieName, TObject (PValue));

  FDomain:= CookieDomain;
  FExpires:= CookieExpireTime;
  FPath:= CookiePath;

end;

destructor TCookie.Destroy;
var
  PValue: PString;

begin
  PValue:= PString (Value);
  Dispose (PValue);

  inherited;

end;

function TCookie.ToString (Mode: Integer): String;
begin
  case Mode of
    0:
      Result:= FDomain+ ':'+ FormatDateTime ('ddd,dd-mmm-yyyy hh:nn:ss', FExpires)+
          ' GMT:'+ FName+ ':'+ StrValue+ ':'+ FPath;
    1:
      raise ENotImplementedYet.Create ('TCookie', 'ToString (1)');

//      Result:=  URLEncode (FName)+ '='+ StrValue+ ';path='+ FPath+ ';expires='
//      + FormatDateTime ('ddd,dd-mmm-yyyy hh:nn:ss', FExpires)+ ' PDT';

  end;

end;

{ ECookieNotFound }

constructor ECookieNotFound.Create;
begin
  inherited Create ('Cookie Not Found!');

end;

constructor ECookieNotFound.Create(Msg: String);
begin
  inherited Create (Msg);

end;

{ TCookieManager }

function TCookieManager.GetCookie (Index: Integer): TCookie;
begin
{TODO: 4}
  //Result:= Objects [Index] as TCookie;

end;

function TCookieManager.GetText: String;
var
  i: Integer;
  Ptr: PObject;

begin
  Result:= '';
{
TODO:5
  for i:= 1 to Count do
  begin
    Result:= Result+ 'Set-Cookie:'+ TCookie (Ptr^).ToString (1)+ #10;
    Inc (Ptr);

  end;
}

end;

constructor TCookieManager.Create (HeaderCollection: THeaderCollection; IsHeaderSent: PBoolean;
       ThisPageURI: String);
begin
  inherited Create;

  FIsHeaderSent:= IsHeaderSent;
  FWebHeaderCollection:= HeaderCollection;
  FHostName:= GlobalObjContainer.Configurations.ConfigurationValueByName ['HostName'];
  FPageURI:= ThisPageURI;

end;

destructor TCookieManager.Destroy;
begin
  FIsHeaderSent:= nil;

  inherited;

end;

procedure TCookieManager.LoadFromString (CookieString: String);
var
  LastNamePos, LastValuePos, Len: Integer;
  Name, Value: String;
  CurrentCookie: TCookie;

begin
  Len:= length (CookieString);
  if Len= 0 then
    Exit;
  if CookieString [Len]<> ';' then
    CookieString:= CookieString+ ';';

  LastNamePos:= 0;
  LastValuePos:= 0;
  if CookieString [1]= '\' then
    Inc (LastValuePos);
  // Parse out

  while True do
  begin

    // Getting name
    LastNamePos:= Pos ('=', CookieString);
    if LastNamePos= 0 then
      Break;

    Name:= Copy (CookieString, 1, LastNamePos- 1);
    System.Delete (CookieString, 1, LastNamePos);

    // Getting value
    LastValuePos:= Pos (';', CookieString);
    Value:= Copy (CookieString, 1, LastValuePos- 1);
            System.Delete (CookieString, 1, LastValuePos);

    CurrentCookie:= TCookie.Create (Name, Value);

    Self.Add (CurrentCookie);

    if LastValuePos= 0 then
      Break;

  end;

end;

procedure TCookieManager.WriteCookie;
var
  i: Integer;

begin
  raise ENotImplementedYet.Create ('TCookieManager', 'WriteCookie');
{
  for i:= 0 to Count- 1 do
    FWebHeaderCollection.Add (TWebHeader.Create ('Set-Cookie',
    URLEncode (Cookie [i].Name)+ '='+ URLEncode (Cookie [i].Value)+
     ';path='+ Cookie [i].Path+ ';domain='+ Cookie [i].Domain+
     ';expires=' + DateTimeToStr (Cookie [i].Expires)));
}

end;

procedure TCookieManager.Add (NewCookie: TCookie);
begin
{TODO: 6}
//  inherited AddNameValue (NewCookie);

end;

procedure TCookieManager.Add (CookieName, CookieValue: String;
  CookieExpireTime: TDateTime; CookiePath: String; CookieDomain: String);
var
  Ptr: PObject;
  i: Integer;
  NewCookie: TCookie;

begin
  raise ENotImplementedYet.Create ('TCookieManager', 'Add');
{
  Ptr:= GetPointerToFirst;
  NewCookie:= nil;

  for i:= 1 to Size do
  begin
    if TCookie (Ptr^).Name= CookieName then
    begin
      NewCookie:= TCookie (Ptr^);
      NewCookie.PVal^:= CookieValue;
      NewCookie.FExpires:= CookieExpireTime;

    end;

    Inc (Ptr);

  end;

  if NewCookie= nil then
  begin
    NewCookie:= TCookie.Create (CookieName, CookieValue,
      CookieExpireTime,
       CookiePath, CookieDomain);
    Add (NewCookie);

  end;
}

end;

function TCookieManager.GetCookieByName (Name: String): TCookie;
var
  Index: Integer;

begin
{TODO: 7  Index:= IndexOf (Name);
  if 0<= Index then
    Result:= Cookie [Index]
   else
    raise ECookieNotFound.Create (Name);
}
end;

function TCookieManager.GetCookieValueByName (Name: String): String;
var
  Index: Integer;

begin
  raise ENotImplementedYet.Create ('TCookieManager', 'GetCookieValueByName');
  {
  Index:= IndexOf (Name);
  if 0<= Index then
    Result:= Cookie [Index]
   else
    raise ECookieNotFound.Create (Name);
}
end;

procedure TCookieManager.RemoveCookieByName(Name: String);
var
  i: Integer;

begin
  raise ENotImplementedYet.Create ('TCookieManager', 'RemoveCookieByName');
{
  if not Self.IsExists (Name) then
    raise ECookieNotFound.Create (Name+ 'does not exist');

  for i:= 0 to Size- 1 do
    if Cookie [i].Name= Name then
    begin
      Cookie [i].Free;
      Delete (i);
      Break;
    end;
}
end;

end.

