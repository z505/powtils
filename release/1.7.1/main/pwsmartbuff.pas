{*******************************************************************************

                                Powtils smart buffer

********************************************************************************

  This unit contains the main functions and procedures and additions for
  buffering output.

--------------------------------------------------------------------------------
 Authors/Credits:
--------------------------------------------------------------------------------
  L505 (Lars Olson)
  TonyH (Anthony Henry)



********************************************************************************}

{$mode objfpc}{$H+}
unit pwsmartbuff;

interface


type
  TSmartBuffer = RECORD  
        CurrentSize: LongWord;    
          Increment: LongWord;
          Available: LongWord;
               Used: LongWord;
               Data: Pointer;
             CurPos: Pointer;
             Holder: Pointer;
              DoGrow: boolean;
          DoCompress: boolean;   // Ignored at this time.....
                end;

   PSmartBuffer = ^TSmartBuffer;
  
const
  CGI_CRLF = #13#10;
  


function NewFixedBuffer(BuffSize: LongWord) :       PSmartBuffer;
function NewDynamicBuffer(BuffIncrement: LongWord): PSmartBuffer;    
procedure DisposeBuffer(PBuff: PSmartBuffer);

procedure Str2Buff   (S : AnsiString;        PBuff: PSmartBuffer);
procedure StrZ2Buff  (PStr : PChar;          PBuff: PSmartBuffer);
procedure AppendBuff (P : Pointer; 
                      SizeData : LongWord;   PBuff: PSmartBuffer);

procedure AppendLF        (PBuff: PSmartBuffer);
procedure growBuffer      (PBuff: PSmartBuffer);
procedure flushBuffer     (PBuff: PSmartBuffer);
procedure GzipBuffer      (PBuff: PSmartBuffer); 


implementation

uses 
     zlib_zutil,
     zlib_zbase,
     zlib_gzcrc,
     zlib_gzio,
     zlib_zdeflate,
     zlib_zcompres,
     zlib_zuncompr,
     native_out; 

function NewFixedBuffer(BuffSize: LongWord) : PSmartBuffer;

begin
  Result := NIL;
  Result := GetMem(SizeOf(TSmartBuffer));
  IF Result <> NIL then with Result^ DO
     begin
       DoGrow := FALSE;
    Increment := 0;
         Used := 0;
         Data := NIL;
         Data := GetMem(BuffSize);
         If Data <> NIL THEN
           begin
             CurPos := Data;
             Available := BuffSize;
            CurrentSize:= BuffSize;
           end
         else
           begin
             CurrentSize := 0;
               Available := 0;
                  CurPos := NIL;
           end;
     end;
end;        

function NewDynamicBuffer(BuffIncrement: LongWord): PSmartBuffer;
begin
  Result := NIL;
  Result := NewFixedBuffer(BuffIncrement);
  If Result <> NIL then with Result^ DO
    begin
      Increment:= Available;
         DoGrow:= TRUE;
    end;
end;

procedure DisposeBuffer(PBuff: PSmartBuffer);
begin
  If PBuff <> NIL then with PBuff^ DO
    begin
     If Data <> NIL then Freemem(Data);
     FreeMem(PBuff);
    end;
end; 

procedure flushBuffer (PBuff: PSmartBuffer);
begin
  With PBuff^ DO
    begin
      NativeWrite(Data, Used);
        CurPos := Data;
           Used:= 0;
      Available:= CurrentSize;   
    end;
end;

procedure GrowBuffer(PBuff: PSmartBuffer);
begin
  with PBuff^ do
  begin
    Inc(CurrentSize, Increment);
    Inc(Available,   Increment);
    ReAllocMem(Data, CurrentSize);
    CurPos := Data;
    Inc(CurPos, Used);
  end;
end;

// add a string to end of current buffer
procedure Str2Buff(s: Ansistring; PBuff: PSmartBuffer);
var
  SLen: longword;
begin
  SLen:= Length(S);
  With PBuff^ DO
    begin
      If DoGrow THEN While Available < SLen DO GrowBuffer(PBuff);
      If Available >= SLen THEN
        begin
         Move(s[1], CurPos^, SLen);
         Inc(CurPos, SLen);
         Inc(Used, SLen);
         Dec(Available, SLen);
        end 
      else
         begin
           Move(s[1], CurPos^, Available);
           Delete(S, 1, Available - 1);
           FlushBuffer(PBuff);
           Str2Buff(S, PBuff);
         end;
    end; {with Pbuff}  
 end;

procedure StrZ2Buff(PStr : PChar; PBuff: PSmartBuffer);
var
 DLength: longword;
begin
 DLength := 0;
 If PStr <> NIL then DLength := strlen(PStr);
 If DLength > 0 then with PBuff^ do
    begin
      If DoGrow then While Available < DLength do GrowBuffer(PBuff);
      If Available >= DLength THEN 
       begin
        Move(PStr^, CurPos^, DLength);
        Dec(Available, DLength);
        Inc(Used, DLength);
        Inc(CurPos, DLength);
       end
      else
       AppendBuff(PStr, DLength, PBuff); 
    end;
end;

 
// Append any arbitrary binary buffer to StorageBuff 
procedure AppendBuff(P : Pointer; SizeData : LongWord; PBuff: PSmartBuffer);
var
 buffleft: LongWord;

begin
  If (SizeData > 0) and (P <> NIL) then with PBuff^ do
  begin
    If DoGrow then While Available < SizeData do GrowBuffer(PBuff);
    If Available >= SizeData THEN 
      begin
        Move(P^, CurPos^,  SizeData);
        Dec(Available, SizeData);
        Inc(Used, SizeData);       
        Inc(CurPos, SizeData);
      end  
    else 
      begin  
       Move(P^, CurPos^, Available);
       Inc(Used, Available);
       buffleft := SizeData - Available;
       Inc(P, Available);
       FlushBuffer(PBuff);
       AppendBuff(P, buffleft, PBuff);
      end; 
  end;
end;


// Append CGI line feed
procedure AppendLF(PBuff : PSmartBuffer);
begin
  Str2Buff(CGI_CRLF, PBuff);
end;

      
procedure GzipBuffer(PBuff : PSmartBuffer);
Type
  gzheader = packed array[0..9] of byte;

var oldsize, i, crc, dlen, slen, x: longword;
    err : longint;
    s: z_stream;

   procedure RestorePBuff;
    // Restore the old values
    // In case of error condition.. 
     begin
      With PBuff^ DO
        begin
          FreeMem(Data);
          Data := Holder;
          CurPos := Data;
          Inc(CurPos, Used);
          CurrentSize := oldsize;
          Available := CurrentSize - Used;
        end;
     end;   

begin
    // Init
    Char(PBuff^.CurPos^) := #0;
    PBuff^.Holder := PBuff^.Data;
    oldsize := PBuff^.CurrentSize;
    PBuff^.Data := NIL;
    slen := PBuff^.Used;
    dlen := slen + (slen div 100) + 12; // That should be enough
    PBuff^.CurrentSize := dlen + SizeOf(gzheader) + 9;
    PBuff^.Data := getmem(PBuff^.CurrentSize);
    PBuff^.CurPos := PBuff^.Data;
    Inc(PBuff^.CurPos, SizeOf(gzheader));
    // Initializing stream
    s.zalloc := nil;
    s.zfree := nil;
    s.opaque := nil; 
    s.next_in := PBuff^.Holder;
    s.avail_in := uInt(slen);
    s.next_out := PBuff^.CurPos;
    s.avail_out := uInt(dlen);
    // Put the header in the buffer....
    gzheader(PBuff^.Data^)[0] := $1F;
    gzheader(PBuff^.Data^)[1] := $8B;
    gzheader(PBuff^.Data^)[2] := Z_DEFLATED;
    gzheader(PBuff^.Data^)[3] := 0;
    gzheader(PBuff^.Data^)[4] := 0;
    gzheader(PBuff^.Data^)[5] := 0;
    gzheader(PBuff^.Data^)[6] := 0;
    gzheader(PBuff^.Data^)[7] := 0;
    gzheader(PBuff^.Data^)[8] := 0;
    gzheader(PBuff^.Data^)[9] := {$IFDEF WIN32}0{$ELSE}3{$ENDIF};
    // Entire deflation
    err := deflateInit2(s, Z_DEFAULT_COMPRESSION, Z_DEFLATED, -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY);
    if err <> 0 then 
      begin
       restorePBuff;
       exit;
      end; 
    err := deflate(s, Z_FINISH);
    if (err <> Z_STREAM_END) or (s.total_in <> slen) then 
      begin
       restorePBuff;
       exit;
      end; 
    err := deflateEnd(s);
    // Get CRC32
    crc := crc32(0, Z_NULL, 0);
    crc := crc32(crc, Pointer(PBuff^.Holder), slen);
    x := crc;
    // GZ header is already ahead of result stream
    With PBuff^ DO
      begin
       Used := s.total_out + SizeOf(gzHeader) + 8;
       Available := CurrentSize;
       Dec(Available, Used);       
       Inc(CurPos, s.total_out);
       // Write CRC32 and ISIZE in LSB order
       for i := 0 to 3 do
         begin
           byte(CurPos^) := x and $FF;
           x := x shr 8;
           Inc(CurPos);
         end;
       x := slen;
       for i := 0 to 3 do
         begin
           byte(CurPos^) := x and $FF;
           Inc(CurPos);
           x := x shr 8;
         end;
       // Free the old memory
       freemem(Holder);
     end;     
end;
          

end.



