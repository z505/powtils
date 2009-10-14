unit StreamUnit;

interface
uses
  Classes;
const
  SpaceChars: set of char= [' '];

type

  { TMyTextStream }

  TMyTextStream= class (TObject)
  private
    FTargerStream: TStream;
    function GetPosition: Integer;
    function GetSize: Integer;
    procedure SetPosition (Pos: Integer);

  public
    property Size: Integer read GetSize;
    property Position: Integer read GetPosition write SetPosition;

    function ReadCh: Char;
    function ReadLine: AnsiString;
    function ReadInteger: Integer;

    procedure WriteLine (const S: String);
    procedure WriteChar (Ch: Char);
    procedure WriteStr (S: String);

    constructor Create (AnStream: TStream);

  end;

  { TMyBinStream }

  TMyBinStream= class (TObject)
  private
    FTargerStream: TStream;

  public
    function ReadCh: Char;
    function ReadInt: Integer;
    function ReadStr: AnsiString;

    procedure WriteChar (const Ch: Char);
    procedure WriteInt (const n: Integer);
    procedure WriteStr (const S: AnsiString);

    constructor Create (AnStream: TStream);

  end;



  {
function ReadCharFromStream (AnStream: TStream): Char;
function ReadLineFromStream (AnStream: TStream): String;

procedure WriteLineToStream (AnStream: TStream; S: String);
procedure WriteCharToStream (AnStream: TStream; Ch: Char);
procedure WriteCharToStream (AnStream: TStream; Ch: Char);
procedure WriteStrToStream (AnStream: TStream; S: String);
  }

implementation
uses
  SysUtils;

{
function ReadCharFromStream (AnStream: TStream): Char;
begin
  AnStream.Read (Result, 1);
  
end;

function ReadLineFromStream (AnStream: TStream): String;
var
  Ch: Char;

begin
  Result:= '';

  while AnStream.Position< AnStream.Size do
  begin
    Ch:= ReadCharFromStream (AnStream);
    if (Ch= #10) then
      Break;
    Result:= Result+ Ch;

  end;

  if Result= '' then
    Exit;

  if Result [Length (Result)]= #13 then
    Delete (Result, Length (Result), 1);

end;

procedure WriteLineToStream (AnStream: TStream; S: String);
begin
  AnStream.Write (Pointer (@S[1])^, Length (S));

(*$ifdef LINUX*)
  WriteCharToStream (AnStream, #10);

(*$else*)
  WriteCharToStream (AnStream, #13);
  WriteCharToStream (AnStream, #10);
(*$endif*)

end;

procedure WriteCharToStream (AnStream: TStream; Ch: Char);
begin
  AnStream.Write (Ch, 1);

end;

procedure WriteStrToStream (AnStream: TStream; S: String);
begin
  AnStream.Write (Pointer (@S[1])^, Length (S));

end;
}
{ TMyStream }

function TMyTextStream.GetPosition: Integer;
begin
  Result:= FTargerStream.Position;

end;

function TMyTextStream.GetSize: Integer;
begin
  Result:= FTargerStream.Size;

end;

procedure TMyTextStream.SetPosition (Pos: Integer);
begin
  FTargerStream.Position:= Pos;

end;

function TMyTextStream.ReadCh: Char;
begin
  FTargerStream.Read (Result, 1);
  
end;

function TMyTextStream.ReadLine: AnsiString;
var
  Ch: Char;

begin
  Result:= '';
  
  while Position< Size do
  begin
    Ch:= ReadCh;
    if (Ch= #10) then
      Break;
    Result:= Result+ Ch;

  end;

  if Result= '' then
    Exit;

  if Result [Length (Result)]= #13 then
    Delete (Result, Length (Result), 1);


end;

function TMyTextStream.ReadInteger: Integer;
const
  IntDigitChar: set of Char= ['0'..'9'];

var
  Ch: Char;

begin
  Result:= 0;

  Ch:= ReadCh;
  while (Ch in SpaceChars) and (Position< Size) do
    Ch:= ReadCh;
  if Ch in SpaceChars then
    Exit;

  while (Ch in IntDigitChar) and (Position< Size) do
  begin
    Result:= 10* Result+ Ord (Ch)- 48;
    Ch:= ReadCh;

  end;

  if not (Ch in IntDigitChar) then
    SetPosition (Position- 1);

end;

procedure TMyTextStream.WriteStr (S: String);
begin
  FTargerStream.Write (Pointer (@S[1])^, Length (S));

end;

constructor TMyTextStream.Create (AnStream: TStream);
begin
  inherited Create;

  FTargerStream:= AnStream;

end;

procedure TMyTextStream.WriteChar (Ch: Char);
begin
  FTargerStream.Write (Ch, 1);
  
end;

procedure TMyTextStream.WriteLine (const S: String);
begin
  FTargerStream.Write (Pointer (@S[1])^, Length (S));
    
(*$ifdef LINUX*)
  WriteChar (#10);
   
(*$else*)
  WriteChar (#13);
  WriteChar (#10);
(*$endif*)

end;

{ TMyBinStream }

function TMyBinStream.ReadCh: Char;
begin
  FTargerStream.Read (Result, 1);

end;

function TMyBinStream.ReadInt: Integer;
begin
  FTargerStream.Read (Result, 4);

end;

function TMyBinStream.ReadStr: AnsiString;
begin
  raise Exception.Create ('Not Implemented Yet!');

end;

procedure TMyBinStream.WriteChar (const Ch: Char);
begin
  FTargerStream.Write (Ch, 1);

end;

procedure TMyBinStream.WriteInt (const n: Integer);
begin
  FTargerStream.Write (n, 4);


end;

procedure TMyBinStream.WriteStr (const S: AnsiString);
begin
  raise Exception.Create ('Not Implemented Yet!');

end;

constructor TMyBinStream.Create (AnStream: TStream);
begin
  inherited Create;

  FTargerStream:= AnStream;

end;

end.
