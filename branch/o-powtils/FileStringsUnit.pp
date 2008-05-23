unit FileStringsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit;
  
type

  { EFileNotFound }

  EFileNotFound= class (Exception)
  public
    constructor Create (Filename: String);
    
  end;
  
  { TFileString }

  TFileString= class (TObject)
  private
    FFileName: String;
    FFileID: String;
    FDataInFile: String;
    
  public
    property FileID: String read FFileID;
    property DataInFile: String read FDataInFile;
    
    constructor Create (FileName, FileIdentifier: String);
    procedure Free;
    
  end;
  
  { TFileStrings }

  TFileStrings= class (TBaseCollection)
  private
    function GetFileString(Index: Integer): TFileString;
    function GetFileString(Index: String): TFileString;
    
  public
    property FileString [Index: Integer]: TFileString read GetFileString;
    property FileStringByName [Index: String]: TFileString read GetFileString;

    constructor Create;
    procedure Free (FreeObj: Boolean= True);
    
    procedure AddFileString (AFileString: TFileString);

  end;

  { EIDNotFound }

  EIDNotFound= class (Exception)
  public
    constructor Create (ID: String);
    
  end;
  
implementation

{ TFileStrings }

function TFileStrings.GetFileString (Index: Integer): TFileString;
begin
  Result:= Member [Index] as TFileString;
  
end;

function TFileStrings.GetFileString (Index: String): TFileString;
var
  i: Integer;
  
begin
  for i:= 0 to Size- 1 do
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

procedure TFileStrings.Free (FreeObj: Boolean= True);
var
  i: Integer;
  
begin
  if FreeObj then
    for i:= 0 to Size- 1 do
      FileString [i].Free;

  inherited Free;
  
end;

procedure TFileStrings.AddFileString(AFileString: TFileString);
begin
  inherited Add (AFileString);
  
end;

{ TFileString }

constructor TFileString.Create (FileName, FileIdentifier: String);
var
  InputFile: TextFile;
  Str: String;
  
begin
  inherited Create;
  
  FFileName:= FileName;
  FFileID:= FileIdentifier;
  FDataInFile:= '';

  if FileExists (FileName) then
  begin
    AssignFile (InputFile, FileName);
    Reset (InputFile);


    while not Eof (InputFile) do
    begin
      ReadLn (InputFile, Str);
      FDataInFile:= FDataInFile+ Str+ #10;

    end;

    CloseFile (InputFile);
    
  end
  else
    raise EFileNotFound.Create (FileName);
    
  
end;

procedure TFileString.Free;
begin
  FDataInFile:= '';
  
  inherited;
  
end;

{ EIDNotFound }

constructor EIDNotFound.Create(ID: String);
begin
  inherited Create ('ID= '+ ID+ ' not found in collection!');
  
end;

{ EFileNotFound }

constructor EFileNotFound.Create(Filename: String);
begin
  inherited Create ('File with name= '+ Filename+ ' is not exist or is unaccessible!');
end;

end.

