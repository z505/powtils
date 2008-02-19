{*******************************************************************************

                         Powtils

********************************************************************************

  This unit contains the main functions and procedures and additions for
  buffering output.

--------------------------------------------------------------------------------
 Authors/Credits:
--------------------------------------------------------------------------------
  L505 (Lars Olson)
  TonyH (Anthony Henry)

********************************************************************************}


unit pwbuffer;

{$mode objfpc}{$H+}

interface


type
// to give us "self" capabilities we must declare this record separately so we 
// can access it recursively in TSmartBuffer, if we send it as a parameter
  TSmartBufferData = record   
    CurrentSize: LongWord;    
    Available: LongWord;
    Used: LongWord;
    text: string; // buffer string (data)
  end;
  
  TSmartBuffer = record
    Data: TSmartBufferData; // now "self" is available since already declared above
    Grow: procedure (StorageBuffData: TSmartBufferData); // send storagebuffdata as a parameter, now we have acccess to a "SELF".data 
    Chop: procedure(StorageBuffData: TSmartBufferData);
    Update: procedure(StorageBuffData: TSmartBufferData);
    Write: procedure (StorageBuffData: TSmartBufferData);
    AppendStr: procedure(StorageBuffData: TSmartBufferData; s: string); 
    AppendBuff: procedure(StorageBuffData: TSmartBufferData; BuffAppend : Pointer; BuffSize : LongWord);
    AppendLineFeed: procedure(StorageBuffData: TSmartBufferData);                  
  end;
   
  psmartbuffer = ^Tsmartbuffer;
  
procedure WriteBuff(StorageBuffData: TSmartBufferData);
procedure GrowBuff(StorageBuffData: TSmartBufferData);
procedure AppendStr(StorageBuffData: TSmartBufferData; s: string);
procedure AppendBuffer(StorageBuffData: TSmartBufferData; BuffAppend : Pointer; BuffSize : LongWord);
procedure AppendLineFeed(StorageBuffData: TSmartBufferData);
procedure ChopBuff(StorageBuffData: TSmartBufferData);
procedure UpdateBuff(StorageBuffData: TSmartBufferData);

const
  CGI_CRLF = #13#10;

implementation

uses native_out;


procedure WriteBuff(StorageBuffData: TSmartBufferData);
begin
  with StorageBuffData{self} do
    NativeWrite(PChar(Text), Used);     // Using overloaded version.
end;


procedure GrowBuff(StorageBuffData: TSmartBufferData);
CONST
  BUFINCREMENT = 128000;
begin
  with StorageBuffData{self} do
  begin
    Inc(CurrentSize, BUFINCREMENT);
    Inc(Available, BUFINCREMENT);
    SetLength(Text, CurrentSize);
  end;
end;

{ add a string to end of current buffer }
procedure AppendStr(StorageBuffData: TSmartBufferData; s: string);
var
  j: longword;
begin
  j:= length(s);
  If J > 0 then with StorageBuffData{self} do             // Move will give memory access error with 0 length string
  begin
     While Available < j do GrowBuff(StorageBuffData);       // Don't need IfThen While falls through
                                            // If we have enough space already
     Move(s[1], Text[Used + 1], j);
     Dec(Available, J);
     Inc(Used, J);
  end;
end;


// Append any arbitrary binary buffer to StorageBuff 
procedure AppendBuffer(StorageBuffData: TSmartBufferData; BuffAppend : Pointer; BuffSize : LongWord);
begin
  If BuffSize > 0 then with StorageBuffData{self} do
  begin
     While Available < BuffSize do GrowBuff(StorageBuffData);
     Move(BuffAppend^, Text[Used + 1], BuffSize);
     Dec(Available, BuffSize);
     Inc(Used, BuffSize);
  end;
end;


// Append CGI line feed
procedure AppendLineFeed(StorageBuffData: TSmartBufferData);
begin
   AppendStr(StorageBuffData{self}, CGI_CRLF);
end;


procedure ChopBuff(StorageBuffData: TSmartBufferData);
begin
  With StorageBuffData{self} DO
  begin
    Inc(Used);
    SetLength(Text, Used);
    Text[Used] := #0;      // Pchar compatibility
    Available := 0;
    Dec(Used);             // In case we append again we don't want to do it
    SetLength(Text,Used);  // After the null. 
    CurrentSize := Used;
  end;
end;


procedure UpdateBuff(StorageBuffData: TSmartBufferData);
VAR
  BuffLength,
  Idx : LongWord;

begin
  with StorageBuffData{self} do
  begin
    BuffLength := Length(Text);
    Idx := BuffLength + 1;
    While (Text[Idx] = #0) and (Idx > 0) DO Dec(Idx);
    Used := Idx;
    Available := BuffLength - Used;
    CurrentSize := BuffLength;    
  end;
end;



end.



