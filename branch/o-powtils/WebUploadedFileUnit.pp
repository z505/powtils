unit WebUploadedFileUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit;

type
  { TWebUpFile }

  { TUploadedFile }

  TUploadedFile= class (TObject)
  private
    FName, FFileName, FData, FContent_Type: string;
    Fsize: Integer;

  public
    property Size: Integer read FSize;
    property Name: String read FName;
    property FileName: String read FFileName;
    property Data: String read FData;
    property Content_Type: String read FContent_Type;

    constructor Create (_Size: Integer; _Name, _FileName, _Data, _Content_Type: String);

  end;

  { TWebUpFileCollection }

  TWebUpFileCollection= class (TBaseCollection)
  private
    function GetFiles(Index: Integer): TUploadedFile;

  public
    property Files [Index: Integer]: TUploadedFile read GetFiles;

  end;


implementation

{ TUploadedFile }

constructor TUploadedFile.Create (_Size: Integer; _Name, _FileName, _Data,
  _Content_Type: String);
begin
  inherited Create;

  Fsize:= _Size;
  FName:= _Name;
  FFileName:= _FileName;
  FData:= _Data;
  FContent_Type:= _Content_Type;

end;

{ Web_TUpFiles }

function TWebUpFileCollection.GetFiles (Index: Integer): TUploadedFile;
begin
  Result:= Member [Index] as TUploadedFile;

end;

end.

