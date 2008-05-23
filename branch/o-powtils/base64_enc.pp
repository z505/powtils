{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                       Pascal Web Unit project (PWU)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 ------------------------------------------------------------------------------
  SendMail Unit
 ------------------------------------------------------------------------------

  This unit contains base64 functions , derived from the Pascal Server Pages
  project.

 ------------------------------------------------------------------------------
  Developer Notes
 ------------------------------------------------------------------------------

  [ 11/OCT/2005 -L505 ]

   -modified unit slightly

 ------------------------------------------------------------------------------
  Developer Todo
 ------------------------------------------------------------------------------

  -pascal syntax and function naming

 ------------------------------------------------------------------------------
  Credits:
 ------------------------------------------------------------------------------
   -PSP
   -Vladimir Sibirov

   This unit was taken from the Pascal server pages project
 ------------------------------------------------------------------------------}


{$IFDEF FPC}{$H+}{$MODE OBJFPC}
    {$IFDEF EXTRA_SECURE}
     {$R+}{$Q+}{$CHECKPOINTER ON}
    {$ENDIF}
{$ENDIF}

unit Base64_Enc;

interface


function base64_decode(const s: string): string;

function base64_encode(const s: string): string;



implementation


type TAByte = array[0 .. MaxInt - 1] of byte; 
    TPAByte = ^TAByte; 


// Base64 string encoding
function base64_encode(const s: string): string;
const
  b64: array[0..63] of char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  i, len: integer;
  rp, sp: TPAByte;
  c: longword;
  
begin 
  len := length(s); 
  if len = 0 then exit('');
  SetLength(result, ((len + 2) div 3) * 4); 

  sp := @s;
  rp := @result;
  
  for i := 1 to len div 3 do
  begin 
    c := sp^[0] shl 16 + sp^[1] shl 8 + sp^[2]; 
    rp^[0] := byte(b64[(c shr 18) and $3f]); 
    rp^[1] := byte(b64[(c shr 12) and $3f]); 
    rp^[2] := byte(b64[(c shr 6) and $3f]); 
    rp^[3] := byte(b64[c and $3f]); 
    inc(longword(rp), 4); 
    inc(longword(sp), 3); 
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



function base64_decode(const s: string): string;  
var i, j, len: longint; 
    sp, rp: TPAByte; 
    ch: char; 
    c: longword; 
begin 
  len := length(s); 
  if (len = 0) or (len mod 4 <> 0) then exit('');
  len := len shr 2; 
  SetLength(result, len * 3); 
  sp := @s;
  rp := @result;
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
    inc(longword(sp), 4); 
    inc(longword(rp), 3); 
  end;  
end;




end.
