unit Unicode;
{$H+}

{
	 Converts ISO-8859-xx charsets to UTF-8 and vice versa.
	 
	 This unit offers a subset of the functions and classes from the unit "unicode.pp":
   	 Free Pascal Unicode support
   	 Copyright (C) 2000 by Sebastian Guenther, sg@freepascal.org
	 which is published under LGPL

	 The remaining part was modified in September 2004 by jo@magnus.de: 
	 	- 8BitTables are now loadable at runtime, so no more compile time includes are necessary
		- Pure AnsiStrings (no more PChars) are used in the interface
}

INTERFACE
uses SysUtils;
type

  UCChar 			= LongWord;					 // Unicode char type (32 bit)
  T8BitTable 		= array[Char] of UCChar; // Table for translating 8 bit characters
  P8BitTable 		= ^T8BitTable;
  ENoTableError	=	class(Exception);
// -------------------------------------------------------------------
//   Encoder & decoder base classes
// -------------------------------------------------------------------

  TUTF8Encoder = class
  public
    function Encode(const Source: String): string; virtual; abstract;
  end;

  TUTF8Decoder = class
  public
    function Decode(const Source: string): String; virtual; abstract;
  end;


// -------------------------------------------------------------------
//   Ansi (ISO8859-1) character set
//     This is a special case because the first 256 characters of
//     Unicode are the ISO8859-1 characters
// -------------------------------------------------------------------

  TAnsiUTF8Encoder = class(TUTF8Encoder)
  public
    function Encode(const Source: String): string; override;
  end;

  TAnsiUTF8Decoder = class(TUTF8Decoder)
  public
    function Decode(const Source: string): String; override;
  end;


// -------------------------------------------------------------------
//   Simple 8-bit character sets
// -------------------------------------------------------------------

  T8BitUTF8Encoder = class(TUTF8Encoder)
  private
    FTable: T8BitTable;
  public
    constructor Create(const ATablename:string);
    function Encode(const Source: String): string; override;
  end;


  PL3RevTable = ^TL3RevTable;
  TL3RevTable = array[0..63] of Char;
  PL2RevTable = ^TL2RevTable;
  TL2RevTable = array[0..63] of PL3RevTable;

  T8BitUTF8Decoder = class(TUTF8Decoder)
  protected
    FL1RevTable: array[0..15] of PL2RevTable;
    FL2RevTable: array[0..31] of PL3RevTable;
    FL3RevTable: array[0..127] of Char;
  public
    constructor Create(const ATablename:string);
    function Decode(const Source: string): String; override;
  end;



IMPLEMENTATION

// -------------------------------------------------------------------
//   Helper functions
// -------------------------------------------------------------------

function UnicodeToUTF8(c: UCChar): string;
begin
  if c <= $007f then
    Result := Chr(c)
  else if c <= $07ff then
    Result := Chr($c0 or (c shr 6)) + Chr($80 or (c and $3f))
  else if c <= $ffff then
    Result := Chr($e0 or (c shr 12)) + Chr($80 or ((c shr 6) and $3f)) +
      Chr($80 or (c and $3f));
  // !!!: How are Unicode chars >$ffff handled? (new in Unicode 3)
end;


function Read8BitTable(const tableName:string; var table:T8BitTable):boolean;
var
	ft		:	file of T8BitTable;
begin
	if not FileExists(tableName) then begin
		raise ENoTableError.Create('Error reading "'+tableName+'"');
		result:=false
	end;
	assign(ft,tableName); 
	reset(ft);
	read(ft,table);
	close(ft);
	result:=true
end;


// -------------------------------------------------------------------
//   Ansi (ISO8859-1) character set
// -------------------------------------------------------------------

function TAnsiUTF8Encoder.Encode(const Source: String): string;
var
  i: Integer;
begin
  SetLength(Result, 0);
  for i := 1 to Length(Source) do
    Result := Result + UnicodeToUTF8(UCChar(Ord(Source[i])));
end;

function TAnsiUTF8Decoder.Decode(const Source: string): String;
var
  p: PChar;
begin
  SetLength(Result, 0);
  p:=@Source[1];
  while p[0] <> #0 do
  begin
    if ShortInt(Ord(p[0])) > 0 then	// = "if Ord(p[0]) < $80 then"
    begin
      Result := Result + p[0];
      Inc(p);
    end else if Ord(p[0]) < $e0 then
    begin
      if p[0] <= #$c3 then
        Result := Result + Chr((Ord(p[1]) and $3f) or (Ord(p[0]) shl 6))
      else
        Result := Result + '?';
      Inc(p, 2);
    end else
    begin
      // Encoding which need 3 UTF-8 characters are certainly not contained
      // in the Ansi character set
      Result := Result + '?';
      Inc(p, 3);
    end;
  end;
end;


// -------------------------------------------------------------------
//   Simple 8-bit character sets
// -------------------------------------------------------------------



constructor T8BitUTF8Encoder.Create(const ATablename:string);
begin
	if not Read8BitTable(ATablename,FTable) then FAIL
end;

function T8BitUTF8Encoder.Encode(const Source: String): string;
var
  i: Integer;
begin
  SetLength(Result, 0);
  for i := 1 to Length(Source) do
    Result := Result + UnicodeToUTF8(FTable[Source[i]]);
end;


constructor T8BitUTF8Decoder.Create(const ATablename:string);
var
  i: Char;
  c: UCChar;
  Index: Integer;
  L1Table: PL2RevTable;
  ATable : T8BitTable;
begin
  if not Read8BitTable(ATablename,ATable) then FAIL;
  FillChar(FL3RevTable, SizeOf(FL3RevTable), '?');
  for i := Low(Char) to High(Char) do
  begin
    c := ATable[i];
    if c <= $007f then
      FL3RevTable[Ord(c)] := i
    else if c <= $07ff then
    begin
      Index := Ord(c) shr 6;
      if not Assigned(FL2RevTable[Index]) then
      begin
        New(FL2RevTable[Index]);
	FillChar(FL2RevTable[Index]^, SizeOf(TL3RevTable), '?');
      end;
      FL2RevTable[Index]^[Ord(c) and $3f] := i;
    end else if c <= $ffff then
    begin
      Index := Ord(c) shr 12;
      if not Assigned(FL1RevTable[Index]) then
      begin
        New(FL1RevTable[Index]);
	FillChar(FL1RevTable[Index]^, SizeOf(TL2RevTable), #0);
      end;
      L1Table := FL1RevTable[Index];
      Index := (Ord(c) shr 6) and $3f;
      if not Assigned(L1Table^[Index]) then
      begin
        New(L1Table^[Index]);
	FillChar(L1Table^[Index]^, SizeOf(TL3RevTable), '?');
      end;
      L1Table^[Index]^[Ord(c) and $3f] := i;
    end;
    // !!!: Cannot deal with unicode chars >$ffff
  end;
end;

{  PL3RevTable = ^TL3RevTable;
  TL3RevTable = array[0..255] of Char;
  PL2RevTable = ^TL2RevTable;
  TL2RevTable = array[0..255] of PL3RevTable;

  T8BitUTF8Decoder = class(TUTF8Decoder)
  protected
    FL1RevTable: array[0..15] of PL2RevTable;
    FL2RevTable: array[0..31] of PL3RevTable;
    FL3RevTable: array[0..127] of Char;
end;}

function T8BitUTF8Decoder.Decode(const Source: string): String;
var
  p: PChar;
  Index: Integer;
  L1Table: PL2RevTable;
begin
  SetLength(Result, 0);
  p:=@Source[1];
  while p[0] <> #0 do
  begin
    if ShortInt(Ord(p[0])) > 0 then	// = "if Ord(p[0]) < $80 then"
    begin
      // 1-byte encoding
      Result := Result + FL3RevTable[Ord(p[0])];
      Inc(p);
    end else if Ord(p[0]) < $e0 then
    begin
      // 2-byte encoding
      Index := Ord(p[0]) and $1f;
      if Assigned(FL2RevTable[Index]) then
        Result := Result + FL2RevTable[Index]^[Ord(p[1]) and $3f]
      else
        Result := Result + '?';
      Inc(p, 2);
    end else
    begin
      // 3-byte encoding
      Index := Ord(p[0]) and $0f;
      L1Table := FL1RevTable[Index];
      if Assigned(L1Table) then
      begin
        Index := Ord(p[1]) and $3f;
	if Assigned(L1Table^[Index]) then
	  Result := Result + L1Table^[Index]^[Ord(p[3]) and $3f]
	else
	  Result := Result + '?';
      end else
        Result := Result + '?';
      Inc(p, 3);
    end;
  end;
end;


end.
