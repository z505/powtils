{*******************************************************************************

                                Powtils

********************************************************************************

--------------------------------------------------------------------------------
 Buffering
--------------------------------------------------------------------------------

  This unit contains the main functions and procedures and additions for
  buffering output. Derived from old pwuBuff unit

  Authors: TonyH (Anthony Henry)



********************************************************************************}

{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}
unit pwobjbuff;

interface

type
  TSmartBuffer = OBJECT
        CurrentSize: LongWord;
          Increment: LongWord;
          Available: LongWord;
               Used: LongWord;
               Data: Pointer;
             CurPos: Pointer;
             Holder: Pointer;
             DoGrow: boolean;
         DoCompress: boolean;   // Ignored at this time.....
        Constructor FixedBuffer(BuffSize : LongWord);
        Constructor DynamicBuffer(BuffIncrement : LongWord);
        Destructor  Destroy;
        Procedure  AppendStr(S : AnsiString);
        Procedure  AppendStrZ(PStr : PChar);
        Procedure  AppendBuffer(P : Pointer; SizeData : LongWord);
        Procedure  AppendLineFeed;
        Procedure  Grow;
        Procedure  Flush;
        Procedure  Gzip;
                end;
   PSmartBuffer = ^TSmartBuffer;

const
  CGI_CRLF = #13#10;

implementation

uses
     {$IFNDEF FPC}sysutils, windows,{$ENDIF}
     zlib_zutil,
     zlib_zbase,
     zlib_gzcrc,
     zlib_gzio,
     zlib_zdeflate,
     zlib_zcompres,
     zlib_zuncompr,
     pwnative_out; 

constructor TSmartBuffer.FixedBuffer(BuffSize: LongWord);
begin
  With Self DO
     begin
       DoGrow := FALSE;
    Increment := 0;
         Used := 0;
         Data := NIL;
         Data := {$IFDEF FPC}GetMem(BuffSize);{$ENDIF} {$IFNDEF FPC}GetMemory(BuffSize);{$ENDIF}
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

constructor TSmartBuffer.DynamicBuffer(BuffIncrement: LongWord);
begin
  With Self DO
    begin
      FixedBuffer(BuffIncrement);
      Increment:= Available;
         DoGrow:= TRUE;
    end;
end;

destructor TSmartBuffer.Destroy;
begin
  With Self DO
    begin
      If Used > 0 then Flush;
      If Data <> NIL Then FreeMem(Data);
    end;
end;

procedure TSmartBuffer.flush;
begin
  With Self DO
    begin
      NativeWrite(Data, Used);
      CurPos:= Data;
      Used:= 0;
      Available:= CurrentSize;
    end;
end;

procedure TSmartBuffer.Grow;
begin
  with Self do
  begin
    Inc(CurrentSize, Increment);
    Inc(Available,   Increment);
    ReAllocMem(Data, CurrentSize);
    CurPos:= Data;
    Inc(pchar(CurPos), Used);
  end;
end;

// add a string to end of current buffer
procedure TSmartBuffer.AppendStr(S : Ansistring);
var
  SLen: longword;
begin
  SLen:= Length(S);
  With Self DO
    begin
      If DoGrow THEN While Available < SLen DO Grow;
      If Available >= SLen THEN
        begin
         Move(s[1], CurPos^, SLen);
         Inc(pchar(CurPos), SLen);
         Inc(Used, SLen);
         Dec(Available, SLen);
        end 
      else
         begin
           Move(s[1], CurPos^, Available);
           Delete(S, 1, Available - 1);
           Flush;
           AppendStr(S);
         end;
    end;   
 end;

procedure TSmartBuffer.AppendStrZ(PStr : PChar);
var
 DLength: longword;
begin
 DLength := 0;
 If PStr <> NIL then DLength := strlen(PStr);
 If DLength > 0 then with Self do
    begin
      If DoGrow then While Available < DLength do Grow;
      If Available >= DLength THEN 
       begin
        Move(PStr^, CurPos^, DLength);
        Dec(Available, DLength);
        Inc(Used, DLength);
        Inc(pchar(CurPos), DLength);
       end
      else
       AppendBuffer(PStr, DLength); 
    end;
end;

 
// Append any arbitrary binary buffer to StorageBuff 
procedure TSmartBuffer.AppendBuffer(P : Pointer; SizeData : LongWord);
var
 buffleft: LongWord;

begin
  If (SizeData > 0) and (P <> NIL) then with Self do
  begin
    If DoGrow then While Available < SizeData do Grow;
    If Available >= SizeData THEN 
      begin
        Move(P^, CurPos^,  SizeData);
        Dec(Available, SizeData);
        Inc(Used, SizeData);
        Inc(pchar(CurPos), SizeData);
      end  
    else 
      begin  
       Move(P^, CurPos^, Available);
       Inc(Used, Available);
       buffleft := SizeData - Available;
       Inc(pchar(P), Available);
       Flush;
       AppendBuffer(P, buffleft);
      end; 
  end;
end;


// Append CGI line feed
procedure TSmartBuffer.AppendLineFeed;
begin
  Self.AppendStr(CGI_CRLF);
end;

      
procedure TSmartBuffer.Gzip;
Type
  gzheader = packed array[0..9] of byte;

var oldsize, i, crc, dlen, slen, x: longword;
    err : longint;
    s: z_stream;

   procedure RestoreBuffer;
    // Restore the old values
    // In case of error condition.. 
     begin
      With Self DO
        begin
          FreeMem(Data);
          Data := Holder;
          CurPos := Data;
          Inc(pchar(CurPos), Used);
          CurrentSize := oldsize;
          Available := CurrentSize - Used;
        end;
     end;   

begin
    // Init
    Char(Self.CurPos^) := #0;
    Self.Holder := Self.Data;
    oldsize := Self.CurrentSize;
    Self.Data := NIL;
    slen := Self.Used;
    dlen := slen + (slen div 100) + 12; // That should be enough
    Self.CurrentSize := dlen + SizeOf(gzheader) + 9;
    Self.Data := {$IFDEF FPC}getmem(Self.CurrentSize);{$ENDIF} {$IFNDEF FPC}getmemory(Self.CurrentSize);{$ENDIF}
    Self.CurPos := Self.Data;
    Inc(pchar(Self.CurPos), SizeOf(gzheader));
    // Initializing stream
    s.zalloc := nil;
    s.zfree := nil;
    s.opaque := nil; 
    s.next_in := Self.Holder;
    s.avail_in := uInt(slen);
    s.next_out := Self.CurPos;
    s.avail_out := uInt(dlen);
    // Put the header in the buffer....
    gzheader(Self.Data^)[0] := $1F;
    gzheader(Self.Data^)[1] := $8B;
    gzheader(Self.Data^)[2] := Z_DEFLATED;
    gzheader(Self.Data^)[3] := 0;
    gzheader(Self.Data^)[4] := 0;
    gzheader(Self.Data^)[5] := 0;
    gzheader(Self.Data^)[6] := 0;
    gzheader(Self.Data^)[7] := 0;
    gzheader(Self.Data^)[8] := 0;
    gzheader(Self.Data^)[9] := {$IFDEF WIN32}0{$ELSE}3{$ENDIF};
    // Entire deflation
    err := deflateInit2(s, Z_DEFAULT_COMPRESSION, Z_DEFLATED, -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY);
    if err <> 0 then 
      begin
       restoreBuffer;
       exit;
      end; 
    err := deflate(s, Z_FINISH);
    if (err <> Z_STREAM_END) or (s.total_in <> slen) then 
      begin
       restoreBuffer;
       exit;
      end; 
    err := deflateEnd(s);
    // Get CRC32
    crc := crc32(0, Z_NULL, 0);
    crc := crc32(crc, Pointer(Self.Holder), slen);
    x := crc;
    // GZ header is already ahead of result stream
    With Self DO
      begin
       Used := s.total_out + SizeOf(gzHeader) + 8;
       Available := CurrentSize;
       Dec(Available, Used);       
       Inc(pchar(CurPos), s.total_out);
       // Write CRC32 and ISIZE in LSB order
       for i := 0 to 3 do
         begin
           byte(CurPos^) := x and $FF;
           x := x shr 8;
           Inc(pchar(CurPos));
         end;
       x := slen;
       for i := 0 to 3 do
         begin
           byte(CurPos^) := x and $FF;
           Inc(pchar(CurPos));
           x := x shr 8;
         end;
       // Free the old memory
       freemem(Holder);
     end;     
end;
          

end.



