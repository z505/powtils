{*******************************************************************************

                                Powtils

********************************************************************************

--------------------------------------------------------------------------------
  Base64Enc Unit
--------------------------------------------------------------------------------
  This unit contains base64 functions.
  
--------------------------------------------------------------------------------
  Authors/Credits:
--------------------------------------------------------------------------------
  Trustmaster (Vladimir Sibirov), L505 (Lars Olson)
********************************************************************************}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}{$IFDEF EXTRA_SECURE}{$R+}{$Q+}{$CHECKPOINTER ON}{$ENDIF}
{$ENDIF}

unit pwbase64enc;

interface

function Base64Decode(const s: string): string;
function Base64Encode(const s: string): string;

implementation

type
  TAByte = array[0 .. MaxInt - 1] of byte;
  TPAByte = ^TAByte;

// Base64 string encoding
function Base64Encode(const s: string): string;
const
  b64: array[0..63] of char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  i, len: integer;
  rp, sp: TPAByte;
  c: longword;
begin
  result:='';
  len := length(s);
  if len = 0 then exit;
  SetLength(result, ((len + 2) div 3) * 4);
  sp := pointer(s);
  rp := pointer(result);
  for i := 1 to len div 3 do
  begin
    c := sp^[0] shl 16 + sp^[1] shl 8 + sp^[2];
    rp^[0] := byte(b64[(c shr 18) and $3f]);
    rp^[1] := byte(b64[(c shr 12) and $3f]);
    rp^[2] := byte(b64[(c shr 6) and $3f]);
    rp^[3] := byte(b64[c and $3f]);

   {$ifdef fpc}
    Inc(PtrUInt(rp), 4); // PtrUInt always same size as Pointer type
    Inc(PtrUInt(sp), 3);
   {$else}
    Inc(longword(rp), 4);
    Inc(longword(sp), 3);
   {$endif}

  end;
  case len mod 3 of
    1:
    begin
      c := sp^[0] shl 16;
      rp^[0] := byte(b64[(c shr 18) and $3f]);
      rp^[1] := byte(b64[(c shr 12) and $3f]);
      rp^[2] := byte('=');
      rp^[3] := byte('=');
    end;
    2:
    begin
      c := sp^[0] shl 16 + sp^[1] shl 8;
      rp^[0] := byte(b64[(c shr 18) and $3f]);
      rp^[1] := byte(b64[(c shr 12) and $3f]);
      rp^[2] := byte(b64[(c shr 6) and $3f]);
      rp^[3] := byte('=');
    end;
  end;  
end;



function Base64Decode(const s: string): string;
var i, j, len: longint; 
    sp, rp: TPAByte; 
    ch: char; 
    c: longword; 
begin 
  result:= '';
  len := length(s);
  if (len = 0) or (len mod 4 <> 0) then exit;
  len := len shr 2; 
  SetLength(result, len * 3); 
  sp := pointer(s); 
  rp := pointer(result); 
  for i := 1 to len do
  begin 
    c := 0; 
    j := 0; 
    while true do
    begin
      ch := char(sp^[j]);
      case ch of
        'A'..'Z' : c := c or (longword(ch) - byte('A'));
        'a'..'z' : c := c or (longword(ch) - byte('a') + 26);
        '0'..'9' : c := c or (longword(ch) - byte('0') + 52);
        '+' : c := c or 62;
        '/' : c := c or 63;
        else
        begin
          if j = 3 then
          begin
            rp^[0] := c shr 16;
            rp^[1] := byte(c shr 8);
            SetLength(result, length(result) - 1);
          end
            else
          begin
            rp^[0] := c shr 10;
            SetLength(result, length(result) - 2);
          end;
          exit;
        end;
      end;
      if j = 3 then break; 
      inc(j); 
      c := c shl 6; 
    end;
    rp^[0] := c shr 16; 
    rp^[1] := byte(c shr 8); 
    rp^[2] := byte(c); 

   {$ifdef fpc}
    Inc(PtrUInt(sp), 4); // PtrUInt always same size as Pointer type
    Inc(PtrUInt(rp), 3);
   {$else}
    Inc(longword(sp), 4);
    Inc(longword(rp), 3);
   {$endif}

  end;  
end;




end.
