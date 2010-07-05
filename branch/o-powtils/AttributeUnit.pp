unit AttributeUnit; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit;
  
type

  { ECharNotFoundInFile }

  ECharNotFoundInFile= class (Exception)
  public
    constructor Create (Ch: Char);

  end;

  { EAttributeNotFound }

  EAttributeNotFound= class (Exception)
  public
    constructor Create (Msg: String);
    
  end;
  
  TCharFile= file of char;
  { TAttribute }

  TAttribute= class (TObject)
  private
    FName: String;
    FValue: String;
    
  public
    property Name: String read FName write FName;
    property Value: String read FValue write FValue;
    
    constructor Create;
    constructor Create (AttName, AttValue: String);
    
    function ToString: String;
    
  end;
  
  { TAttributeCollection }

  TAttributeCollection= class (TBaseCollection)
  private
    function GetAttribute (Index: Integer): TAttribute;
    function GetAttributeByName (AttrName: String): TAttribute;
  public
    property Attribute [Index: Integer]: TAttribute read GetAttribute;
    property AttribByName [AttrName: String]: TAttribute read GetAttributeByName;

    constructor Create;
    function ToString: String;
    
    procedure Add (NewAttribute: TAttribute);
    
    procedure SaveToFile (var OutputFile: TextFile);

  end;

var
  LastChar: Char;
  
implementation

procedure Expect (Ch: Char; var InputFile: TCharFile);
var
  TempCh: Char;

begin
  TempCh:= LastChar;
    
  while TempCh= ' ' do
    Read (InputFile, TempCh);

  if TempCh<> Ch then
    raise ECharNotFoundInFile.Create (Ch);

  LastChar:= ' ';
  
end;

function ExtractValue (var Value: String): String;
var
  i: Integer;
  Flag: Byte;
  Ch: Char;

begin
  Value:= Trim (Value);
  Result:= '';

  if Length (Value)<> 0 then
  begin
    if Value [1]= '"' then
    begin
      Delete (Value, 1, 1);

      Flag:= 0;
      for i:= 1 to Length (Value)- 1 do
      begin
        Ch:= Value [i];

        if Ch= '\' then
        begin
          Inc (Flag);
          if Flag= 2 then
            Flag:= 0;

        end
        else
          Flag:= 0;

        if Ch= '"' then
        begin
          Delete (Value, 1, i);
          Break;

        end;

        if Flag<> 1 then
          Result:= Result+ Ch;

      end;

    end
    else
    begin
      Flag:= 0;
      for i:= 1 to Length (Value)- 1 do
      begin
        Ch:= Value [i];

        if Ch= '\' then
        begin
          Inc (Flag);
          if Flag= 2 then
            Flag:= 0;

        end
        else
          Flag:= 0;

        if Ch= ' ' then
        begin
          Delete (Value, 1, i);
          Break;

        end;

        Result:= Result+ Ch;

      end;

    end;

  end;

end;

function ExtractValue (var InputFile: TCharFile): String;
var
  Flag: Byte;
  Ch: Char;

begin
  Result:= '';

  Ch:= LastChar;;
  while Ch= ' ' do
    Read (InputFile, Ch);

  if Ch= '"' then
  begin
    Flag:= 0;
    
    while True do
    begin
      Read (InputFile, Ch);

      if Ch= '\' then
      begin
        Inc (Flag);
        if Flag= 2 then
          Flag:= 0;

      end
      else
        Flag:= 0;

      if Ch= '"' then
      begin
        LastChar:= ' ';
        Break;
        
      end;

      if Flag<> 1 then
        Result:= Result+ Ch;

    end;

  end
  else
  begin
    Flag:= 0;
    while True do
    begin
      Read (InputFile, Ch);

      if Ch= '\' then
      begin
        Inc (Flag);
        if Flag= 2 then
          Flag:= 0;

      end
      else
        Flag:= 0;

      if Ch= ' ' then
      begin
        LastChar:= ' ';
        Break;

      end;

      Result:= Result+ Ch;

    end;

  end;

end;

function ExtractName (var InputFile: TCharFile): String;
var
  Flag: Byte;
  Ch: Char;

begin
  Ch:= LastChar;;

  while Ch= ' ' do
    Read (InputFile, Ch);
  Result:= Ch;
    
  Flag:= 0;

  while True do
  begin
    Read (InputFile, Ch);

    if Ch= '\' then
    begin
      Inc (Flag);
      if Flag= 2 then
        Flag:= 0;

    end
    else
      Flag:= 0;

    if Ch= '=' then
    begin
      LastChar:= '=';
      Break;
      
    end
    else if Ch= ' ' then
    begin
      LastChar:= ' ';
      Break;
      
    end;

    if Flag<> 1 then
      Result:= Result+ Ch;

  end;

end;

function ReadNextToken (var InputFile: TCharFile): String;
var
  Ch: Char;

begin
  Ch:= LastChar;
  Result:= '';
  while Ch= ' ' do
    Read (InputFile, Ch);

  while Ch<> ' ' do
  begin
    Result:= Result+ Ch;
    Read (InputFile, Ch);

  end;
  LastChar:= ' ';
  
end;

{ TAttribute }

constructor TAttribute.Create;
begin
  inherited Create;
  
end;

constructor TAttribute.Create (AttName, AttValue: String);
begin
  inherited Create;
  
  FName:= AttName;
  FValue:= AttValue;
  
  if Length (FValue)<> 0 then
    if (FValue [1]= '"') and (FValue [Length (FValue)]= '"') then
    begin
      Delete (FValue, 1, 1);
      Delete (FValue, Length (FValue), 1);
      
    end;
    
end;

function TAttribute.ToString: String;
begin
  Result:= FName+ '="'+ FValue+ '"';
  
  //Remove invalid characters
  
end;

{ TAttributeCollection }

function TAttributeCollection.GetAttribute(Index: Integer): TAttribute;
begin
  Result:= Item [Index] as TAttribute;
  
end;

function TAttributeCollection.GetAttributeByName (AttrName: String): TAttribute;
var
  i: Integer;
  
begin
  AttrName:= UpperCase (AttrName);
  
  for i:= 0 to Count- 1 do
    if UpperCase (Attribute [i].Name)= AttrName then
    begin
      Result:= Attribute [i];
      Exit;
      
    end;
    
  raise EAttributeNotFound.Create (AttrName+ Self.ToString);
  
end;

constructor TAttributeCollection.Create;
begin
  inherited;
  
end;

function TAttributeCollection.ToString: String;
var
  i: Integer;
  Ptr: ^TAttribute;
  
begin
  Result:= '';
  
  if 0< Count then
  begin
    Ptr:= First;
    for i:= 0 to Count- 1 do
    begin
      Result:= Result+ Ptr^.ToString+ ' ';
      Inc (Ptr);
      
    end;
    
  end;
  
end;

procedure TAttributeCollection.Add (NewAttribute: TAttribute);
begin
  inherited;
  
end;

procedure TAttributeCollection.SaveToFile (var OutputFile: TextFile);
var
  i: Integer;
  
begin
  for i:= 1 to Count- 1 do
    Write (OutputFile, Attribute [i].ToString, ' ');


end;

{ EAttributeNotFound }

constructor EAttributeNotFound.Create (Msg: String);
begin
  inherited Create ('Attribute '+ Msg+ ' not found!');
  
end;

{ ECharNotFound }

constructor ECharNotFoundInFile.Create(Ch: Char);
begin
  inherited Create ('Char '+ Ch+ ' not found in File!');

end;

end.

