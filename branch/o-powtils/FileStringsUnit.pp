unit FileStringsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit;
  
type

  { EFileNotFound }

  EFileNotFound= class (Exception)
  public
    constructor Create (const Filename: AnsiString);
    
  end;
  
  { TFileString }

  TFileString= class (TObject)
  private
    FFileName: AnsiString;
    FFileID: AnsiString;
    FDataInFile: AnsiString;
    
  public
    property FileID: AnsiString read FFileID;
    property DataInFile: AnsiString read FDataInFile;
    
    constructor Create (FileName, FileIdentifier: AnsiString);
    
  end;
  
  { TFileStrings }

  TFileStrings= class (TBaseCollection)
  private
    function GetFileString (Index: Integer): TFileString;
    function GetFileString (const Index: AnsiString): TFileString;
    
  public
    property FileString [Index: Integer]: TFileString read GetFileString;
    property FileStringByName [Index: AnsiString]: TFileString read GetFileString;

    constructor Create;

{Add a AFileString to the collectio. The new FileString will be freed by the
  collection in the TFileString destructor.}
    procedure AddFileString (AFileString: TFileString);

  end;

  { EIDNotFound }

  EIDNotFound= class (Exception)
  public
    constructor Create (const ID: String);
    
  end;
  
implementation

{ TFileStrings }

function TFileStrings.GetFileString (Index: Integer): TFileString;
begin
  Result:= Item [Index] as TFileString;
  
end;

function TFileStrings.GetFileString (const Index: String): TFileString;
var
  i: Integer;
  
begin
  for i:= 0 to Count- 1 do
  begin
    Result:= FileString [i];
    
    if Result.FileID= Index then
      Exit;
      
  end;
  
  raise EIDNotFound.Create (Index);
  
end;

constructor TFileStrings.Create;
begin
  inherited;
  
end;

procedure TFileStrings.AddFileString(AFileString: TFileString);
begin
  inherited Add (AFileString);
  
end;

{ TFileString }

constructor TFileString.Create (FileName, FileIdentifier: String);
var
  InputString: TStringList;
  
begin
  inherited Create;
  
  FFileName:= FileName;
  FFileID:= FileIdentifier;

  if FileExists (FileName) then
  begin
    InputString:= TStringList.Create;
    InputString.LoadFromFile (FileName);
    FDataInFile:= InputString.Text;
    InputString.Free;

  end
  else
    raise EFileNotFound.Create (FileName);
  
end;

{ EIDNotFound }

constructor EIDNotFound.Create (const ID: String);
begin
  inherited Create ('ID= '+ ID+ ' not found in collection!');
  
end;

{ EFileNotFound }

constructor EFileNotFound.Create (const Filename: AnsiString);
begin
  inherited Create ('File with name= '+ Filename+ ' is not exist or is unaccessible!');
end;

end.

