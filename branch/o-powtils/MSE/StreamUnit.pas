unit StreamUnit;

interface
uses
  Classes;

type
  TMyStream= class (TStream)
  private
  public
    function ReadCh: Char;
    function ReadLine: String;

    procedure WriteLine (S: String);
    procedure WriteChar (Ch: Char);
    procedure WriteStr (S: String);
    
  end;
  
function ReadCharFromStream (AnStream: TStream): Char;
function ReadLineFromStream (AnStream: TStream): String;

procedure WriteLineToStream (AnStream: TStream; S: String);
procedure WriteCharToStream (AnStream: TStream; Ch: Char);
procedure WriteStrToStream (AnStream: TStream; S: String);

implementation

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

{ TMyStream }

function TMyStream.ReadCh: Char;
begin
  Read (Result, 1);
  
end;

function TMyStream.ReadLine: String;
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

procedure TMyStream.WriteStr (S: String);
begin
  Write (Pointer (@S[1])^, Length (S)); 
{  for i:= 1 to Length (S) do
    WriteChar (S [i]);
}
end;

procedure TMyStream.WriteChar (Ch: Char);
begin
  Write (Ch, 1);
  
end;

procedure TMyStream.WriteLine (S: String);
begin
{  for i:= 1 to Length (S) do
    WriteChar (S [i]);
}
  Write (Pointer (@S[1])^, Length (S));
    
(*$ifdef LINUX*)
  WriteChar (#10);
   
(*$else*)
  WriteChar (#13);
  WriteChar (#10);
(*$endif*)

end;

end.
