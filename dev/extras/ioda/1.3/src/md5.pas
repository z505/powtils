Unit MD5;
{$H+}

(*
 *  MD5 (Message Digest), algorithm by Ron Rivest
 *  This program is Copyright (c) 1998 by Michael Neumann.
 *
 *  This One-Way-Hashfunction produces a 128 bit hashvalue.
 * 
 *  03.03.1999
 *)

INTERFACE
uses Classes,SysUtils;

	function GetMD5(const s:string):string;
	function GetMD5FromList(const sl:TStringList):string;	
	

IMPLEMENTATION


TYPE ULONG = cardinal;

CONST MD5_BITBLOCK      = 512;
      MD5_BYTEBLOCK     = MD5_BITBLOCK DIV 8;
      MD5_HASHVALUE_LEN = SizeOf(ULONG)*4;

(* To initialise A,B,C,D *)
CONST magic : ARRAY[1..4] OF ULONG =
      ($01234567, $89abcdef, $fedcba98, $76543210);


(* rotates x n-bits to left *)
FUNCTION ROTATE_LEFT(x : ULONG; n : Integer) : ULONG;
BEGIN
     ROTATE_LEFT := ((x SHL n) OR (x SHR (32-n)));
END;


(* four non-linear functions *)
FUNCTION F(X,Y,Z : ULONG) : ULONG;
BEGIN
     F := ((X AND Y) OR (NOT X AND Z));
END;

FUNCTION G(X,Y,Z : ULONG) : ULONG;
BEGIN
     G := ((X AND Y) OR (Y AND NOT Z));
END;

FUNCTION H(X,Y,Z : ULONG) : ULONG;
BEGIN
     H := (X XOR Y XOR Z);
END;

FUNCTION I(X,Y,Z : ULONG) : ULONG;
BEGIN
     I := (Y XOR (X OR NOT Z));
END;

PROCEDURE FF(VAR a : ULONG; b, c, d, M : ULONG; s : Integer; t : ULONG);
BEGIN
     a := b + ROTATE_LEFT(a + F(b,c,d) + M + t, s);
END;

PROCEDURE GG(VAR a : ULONG; b, c, d, M : ULONG; s : Integer; t : ULONG);
BEGIN
     a := b + ROTATE_LEFT(a + G(b,c,d) + M + t, s);
END;

PROCEDURE HH(VAR a : ULONG; b, c, d, M : ULONG; s : Integer; t : ULONG);
BEGIN
     a := b + ROTATE_LEFT(a + H(b,c,d) + M + t, s);
END;

PROCEDURE II(VAR a : ULONG; b, c, d, M : ULONG; s : Integer; t : ULONG);
BEGIN
     a := b + ROTATE_LEFT(a + I(b,c,d) + M + t, s);
END;



(*
 *  In ABCD are A,B,C,D which contains at the end the hashvalue.
 *  In M must be a 512 bit block to calculate by 'md5_hashblock'
 *)

TYPE md5_struc = RECORD
     ABCD : ARRAY[1..4] OF ULONG;
     M    : ARRAY[1..16] OF ULONG;
     END;

 
(* ROT_xy: x is the round *)
CONST ROT_11=7; ROT_12=12; ROT_13=17; ROT_14=22;
      ROT_21=5; ROT_22= 9; ROT_23=14; ROT_24=20;
      ROT_31=4; ROT_32=11; ROT_33=16; ROT_34=23;
      ROT_41=6; ROT_42=10; ROT_43=15; ROT_44=21;


(*
 *  This function calculates the 512 bit block given in M[16]
 *  and adds it to the ABCD variables.
 *  At the end, the hash value of the whole message is in ABCD,
 *  it's a 128 bit value.
 *  ==> This function had to be called for every 512 bit block
 *      of the message.
 *)
PROCEDURE md5_hashblock(VAR m : md5_struc);
VAR a,b,c,d : ULONG;
BEGIN
     a  := m.ABCD[1];
     b  := m.ABCD[2];
     c  := m.ABCD[3];
     d  := m.ABCD[4];

    (* Round 1 *)
    FF(a, b, c, d, m.M[1],  ROT_11, $d76aa478);
    FF(d, a, b, c, m.M[2],  ROT_12, $e8c7b756);
    FF(c, d, a, b, m.M[3],  ROT_13, $242070db);
    FF(b, c, d, a, m.M[4],  ROT_14, $c1bdceee);
    FF(a, b, c, d, m.M[5],  ROT_11, $f57c0faf);
    FF(d, a, b, c, m.M[6],  ROT_12, $4787c62a);
    FF(c, d, a, b, m.M[7],  ROT_13, $a8304613);
    FF(b, c, d, a, m.M[8],  ROT_14, $fd469501);
    FF(a, b, c, d, m.M[9],  ROT_11, $698098d8);
    FF(d, a, b, c, m.M[10], ROT_12, $8b44f7af);
    FF(c, d, a, b, m.M[11], ROT_13, $ffff5bb1);
    FF(b, c, d, a, m.M[12], ROT_14, $895cd7be);
    FF(a, b, c, d, m.M[13], ROT_11, $6b901122);
    FF(d, a, b, c, m.M[14], ROT_12, $fd987193);
    FF(c, d, a, b, m.M[15], ROT_13, $a679438e);
    FF(b, c, d, a, m.M[16], ROT_14, $49b40821);

    (* Round 2 *)
    GG(a, b, c, d, m.M[2],  ROT_21, $f61e2562);
    GG(d, a, b, c, m.M[7],  ROT_22, $c040b340);
    GG(c, d, a, b, m.M[12], ROT_23, $265e5a51);
    GG(b, c, d, a, m.M[1],  ROT_24, $e9b6c7aa);
    GG(a, b, c, d, m.M[6],  ROT_21, $d62f105d);
    GG(d, a, b, c, m.M[11], ROT_22, $02441453);
    GG(c, d, a, b, m.M[16], ROT_23, $d8a1e681);
    GG(b, c, d, a, m.M[5],  ROT_24, $e7d3fbc8);
    GG(a, b, c, d, m.M[10], ROT_21, $21e1cde6);
    GG(d, a, b, c, m.M[15], ROT_22, $c33707d6);
    GG(c, d, a, b, m.M[4],  ROT_23, $f4d50d87);
    GG(b, c, d, a, m.M[9],  ROT_24, $455a14ed);
    GG(a, b, c, d, m.M[14], ROT_21, $a9e3e905);
    GG(d, a, b, c, m.M[3],  ROT_22, $fcefa3f8);
    GG(c, d, a, b, m.M[8],  ROT_23, $676f02d9);
    GG(b, c, d, a, m.M[13], ROT_24, $8d2a4c8a);

    (* Round 3 *)
    HH(a, b, c, d, m.M[6],  ROT_31, $fffa3942);
    HH(d, a, b, c, m.M[7],  ROT_32, $8771f681);
    HH(c, d, a, b, m.M[12], ROT_33, $6d9d6122);
    HH(b, c, d, a, m.M[15], ROT_34, $fde5380c);
    HH(a, b, c, d, m.M[2],  ROT_31, $a4beea44);
    HH(d, a, b, c, m.M[5],  ROT_32, $4bdecfa9);
    HH(c, d, a, b, m.M[8],  ROT_33, $f6bb4b60);
    HH(b, c, d, a, m.M[11], ROT_34, $bebfbc70);
    HH(a, b, c, d, m.M[14], ROT_31, $289b7ec6);
    HH(d, a, b, c, m.M[1],  ROT_32, $eaa127fa);
    HH(c, d, a, b, m.M[4],  ROT_33, $d4ef3085);
    HH(b, c, d, a, m.M[7],  ROT_34, $04881d05);
    HH(a, b, c, d, m.M[10], ROT_31, $d9d4d039);
    HH(d, a, b, c, m.M[13], ROT_32, $e6db99e5);
    HH(c, d, a, b, m.M[16], ROT_33, $1fa27cf8);
    HH(b, c, d, a, m.M[3],  ROT_34, $c4ac5665);

    (* Round 4 *)
    II(a, b, c, d, m.M[1],  ROT_41, $f4292244);
    II(d, a, b, c, m.M[8],  ROT_42, $432aff97);
    II(c, d, a, b, m.M[15], ROT_43, $ab9423a7);
    II(b, c, d, a, m.M[6],  ROT_44, $fc93a039);
    II(a, b, c, d, m.M[13], ROT_41, $655b59c3);
    II(d, a, b, c, m.M[4],  ROT_42, $8f0ccc92);
    II(c, d, a, b, m.M[11], ROT_43, $ffeff47d);
    II(b, c, d, a, m.M[2],  ROT_44, $85845dd1);
    II(a, b, c, d, m.M[9],  ROT_41, $6fa87e4f);
    II(d, a, b, c, m.M[16], ROT_42, $fe2ce6e0);
    II(c, d, a, b, m.M[7],  ROT_43, $a3014314);
    II(b, c, d, a, m.M[14], ROT_44, $4e0811a1);
    II(a, b, c, d, m.M[5],  ROT_41, $f7537e82);
    II(d, a, b, c, m.M[12], ROT_42, $bd3af235);
    II(c, d, a, b, m.M[3],  ROT_43, $2ad7d2bb);
    II(b, c, d, a, m.M[10], ROT_44, $eb86d391);

    m.ABCD[1] := m.ABCD[1] + a;
    m.ABCD[2] := m.ABCD[2] + b;
    m.ABCD[3] := m.ABCD[3] + c;
    m.ABCD[4] := m.ABCD[4] + d;
END;

PROCEDURE MemCopy(dst,src : PChar; len : LongInt);
BEGIN
     WHILE len > 0 DO
     BEGIN
          dst^ := src^;
          inc(dst);
          inc(src);
          dec(len);
     END;
END;
PROCEDURE MemSet(dst : PChar; value, len : LongInt);
BEGIN
     WHILE len > 0 DO
     BEGIN
          dst^ := Chr(value);
          inc(dst);
          dec(len);
     END;
END;

(*
 *  This function calculates the hashvalue of the message 'mem' which
 *  is 'length' bytes long, into 'hashvalue' which had to be 16 bytes
 *  long (128 bits).
 *)

PROCEDURE md5_hash_mem(mem : PChar; laenge : LongInt; hashvalue : Pointer);
VAR   m     : md5_struc;
      len,i : LongInt;
      block : PChar;

BEGIN
     len := laenge;
     MemCopy(Addr(m.ABCD),Addr(magic),MD5_HASHVALUE_LEN);

     (* this hashs all available 512 bit blocks *)
     WHILE len >= MD5_BYTEBLOCK DO        (* 64 = 512 bits divided by 8 *)
     BEGIN
          MemCopy(Addr(m.M),mem,MD5_BYTEBLOCK);
          md5_hashblock(m);
          len := len - MD5_BYTEBLOCK;
          mem := mem + MD5_BYTEBLOCK;
     END;

    (*
     * message-len to hash is not dividable through 64 (512 bits)
     * without a rest
     *)
    IF len<> 0 THEN
    BEGIN
         block := Addr(m.M);
         MemCopy(block,mem,len);  (*  the 'restliche' bytes *)
         i := len;
         (block+i)^ := #128;      (*  2**7 *)
         inc(i);

         (* 8 byte laenge-counter + 1 byte does not fit in this 512-bit block *)
         IF len > MD5_BYTEBLOCK-8 THEN
         BEGIN
              WHILE i<MD5_BYTEBLOCK DO
              BEGIN
                   (block+i)^ := #0;
                   inc(i);
              END;

              md5_hashblock(m);

              (*
               * calculate the next 512-bit block
               * 64-8 bytes are set to zero, the last 8 bytes
               * are the laenge of the message
               *)
              MemSet(block,0,MD5_BYTEBLOCK-8);
         END
         ELSE BEGIN (* the laenge fits into this 512-bit block *)
              WHILE i<MD5_BYTEBLOCK-8 DO
              BEGIN
                   (block+i)^ := #0;
                   inc(i);
              END;
         END;

         m.M[15] := 0;
         m.M[16] := laenge;
         md5_hashblock(m);
    END;

    (* copies the calculated hashvalue ABCD into the array hashvalue,
     * which must be 16 bytes long or 128 bits.
     *)
    MemCopy(hashvalue,Addr(m.ABCD),MD5_HASHVALUE_LEN);
END;


function GetMD5(const s:string):string;
var
	hashvalue : array[1..4] of ulong;
begin
   md5_hash_mem(PChar(s),length(s), @hashvalue);
	result:=IntToHex(hashvalue[1],8)+IntToHex(hashvalue[2],8)+IntToHex(hashvalue[3],8)+IntToHex(hashvalue[4],8);
end;


function GetMD5FromList(const sl:TStringList):string;
var
	hashvalue 	: 	array[1..4] of ulong;
	s				:	string;
	i				:	integer;
begin
	s:='';
	for i:=0 to sl.Count-1 do s:=s+' '+sl[i];
   md5_hash_mem(PChar(s),length(s), @hashvalue);
	result:=IntToHex(hashvalue[1],8)+IntToHex(hashvalue[2],8)+IntToHex(hashvalue[3],8)+IntToHex(hashvalue[4],8);
end;


end.
