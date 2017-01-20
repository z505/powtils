{
 Reason for unit: modular, smartlinking, standalone functions in separate unit
 Authors/Credits: FPC Team RTL, Lars (L505)
 License: Freepascal RTL Modified GPL
}
unit pwstrutil;

{$I defines1.inc}

interface
uses pwtypes;

type
  TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);

  PString = ^astr;

  { For FloatToText }
  TFloatFormat = (ffGeneral, ffExponent, ffFixed, ffNumber, ffCurrency);
  TFloatValue = (fvExtended, fvCurrency, fvSingle, fvReal, fvDouble, fvComp);

  TFloatRec = record
    Exponent: integer;
    Negative: Boolean;
    Digits: array[0..18] of Char;
  end;

const
  { Character that comes between integer and fractional part of a number }
  DecimalSeparator : Char = '.';

  { Character that is put every 3 numbers in a currency }
  ThousandSeparator : Char = ',';

  { Number of decimals to use when formatting a currency.  }
  CurrencyDecimals : Byte = 2;

  { Format to use when formatting currency :
    0 = $1
    1 = 1$
    2 = $ 1
    3 = 1 $
    4 = Currency string replaces decimal indicator. e.g. 1$50
   }
  CurrencyFormat : Byte = 1;

  { Same as above, only for negative currencies:
    0 = ($1)
    1 = -$1
    2 = $-1
    3 = $1-
    4 = (1$)
    5 = -1$
    6 = 1-$
    7 = 1$-
    8 = -1 $
    9 = -$ 1
    10 = $ 1-
   }
  NegCurrFormat : Byte = 5;

  { Currency notation. Default is $ for dollars. }
  CurrencyString : String[7] = '$';

type
  TSysLocale = record
    case byte of
      { win32 names }
      1 : (FarEast: boolean; MiddleEast: Boolean);
      { real meaning }
      2 : (MBCS : boolean; RightToLeft: Boolean);
  end;

var
  SysLocale : TSysLocale;

function Lcase(const s: astr): astr;
function Ucase(const s: astr): astr;

function BoolToStr(B: Boolean): astr;
function CurrToStr(Value: Currency): astr;
procedure FloatToDecimal(var result: TFloatRec; Value: Extended; Precision, Decimals : integer);
function CompareText(const S1, S2: astr): integer;
function SameText(const s1,s2:astr):Boolean;
function StrCopy(Dest, Source:PChar): PChar;
function StrECopy(Dest, Source: PChar): PChar;
function StrLCopy(Dest,Source: PChar; MaxLen: SizeInt): PChar;
function StrEnd(P: PChar): PChar;
function StrComp(p1, p2 : PChar): SizeInt;
function StrLComp(p1, p2 : PChar; L: SizeInt): SizeInt;
function StrIComp(p1, p2 : PChar): SizeInt;
function StrLIComp(p1, p2 : PChar; L: SizeInt): SizeInt;
function StrScan(P: PChar; C: Char): PChar;
function StrRScan(P: PChar; C: Char): PChar;
function StrUpper(P: PChar): PChar;
function StrLower(P: PChar): PChar;
function NewStr(const S: astr): PString;
procedure DisposeStr(S: PString);
procedure AssignStr(var P: PString; const S: astr);
procedure AppendStr(var Dest: astr; const S: astr);
function UpperCase(Const S : astr) : astr;
function Lowercase(Const S : astr) : astr;
function CompareMemRange(P1, P2: Pointer; Length: cardinal): integer;
function CompareStr(const S1, S2: astr): integer;
function LeftStr(const S: astr; Count: integer): astr;
function RightStr(const S: astr; Count: integer): astr;

function StringReplace(const S, OldPattern, NewPattern: astr;  Flags: TReplaceFlags): astr;
function IsDelimiter(const Delimiters, S: astr; Index: integer): Boolean;

type
  TSysCharSet = Set of char;

function WrapText(const Line, BreakStr: astr; const BreakChars: TSysCharSet;  MaxCol: integer): astr; overload;
function WrapText(const Line: astr; MaxCol: integer): astr; overload;

function strcat(dest,source : pchar) : pchar;
function strlcat(dest,source : pchar;l : SizeInt) : pchar;
function strmove(dest,source : pchar;l : SizeInt) : pchar;
function strpos(p1,p2 : pchar) : pchar;
function StrPas(p: PChar): astr;
function StrAlloc(Size: cardinal): PChar;
function strnew(p : pchar) : pchar;
function StrPCopy(Dest: PChar; Source: astr): PChar;
function StrPLCopy(Dest: PChar; Source: astr; MaxLen: SizeUInt): PChar;
procedure StrDispose(p: PChar);
function StrBufSize(p: PChar): SizeUInt;

function Trim(const S: astr): astr;
function TrimLeft(const S: astr): astr;
function TrimRight(const S: astr): astr;
function QuotedStr(const S: astr): astr;
function AnsiQuotedStr(const S: astr; Quote: char): astr;
function AnsiExtractQuotedStr(var  Src: PChar; Quote: Char): astr;
function AdjustLineBreaks(const S: astr): astr; overload;

{$ifndef fpc} // delphi
type TTextLineBreakStyle = (tlbsLF, tlbsCRLF, tlbsCR);
{$endif}

function AdjustLineBreaks(const S: astr; Style: TTextLineBreakStyle): astr; overload;
function IsValidIdent(const Ident: astr): boolean;

function s2i(const s: astr): integer;
function strToInt(const s: astr): integer;

function i2s(value: integer): astr; overload;
function i2s(value: int64): astr; overload;
function intToStr(value: integer): astr; overload;
function intToStr(value: int64): astr; overload;
{$IFDEF FPC}
  function intToStr(value: QWord): astr; overload;
{$ENDIF}

implementation

function StrToInt(const s: astr): integer;
var
 {$ifdef fpc}
  error: word;
 {$else}
  error: integer;
 {$endif}
begin
  val(S, result, Error);
  if error <> 0 then result:= 0;
end;

{ alias }
function s2i(const s: astr): integer;
begin
  result:= strToInt(s);
end;

{$IFNDEF FPC}
function strlen(p: pchar): longint;
var i : longint;
begin
  i:= 0; while p[i]<>#0 do inc(i); result:= i; exit;
end;
{$ENDIF}

function FloatToStrF(Value: Extended; format: TFloatFormat; Precision, Digits: integer): astr;
var
  P: integer;
  Negative, TooSmall, TooLarge: Boolean;
begin
  p:= 0;
  TooLarge:= false;
  Case format Of

    ffGeneral:

      begin
        if (Precision = -1) Or (Precision > 15) then Precision := 15;
        TooSmall := (Abs(Value) < 0.00001) and (Value>0.0);
        if not TooSmall then begin
          Str(Value:digits:precision, result);
          P := Pos('.', result);
          if P<>0 then result[P] := DecimalSeparator;
          TooLarge := P > Precision + 1;
        end;

        if TooSmall Or TooLarge then
        begin
          result := FloatToStrF(Value, ffExponent, Precision, Digits);
          // Strip unneeded zeroes.
          P:=Pos('E',result)-1;
          if P<>-1 then
             while (P>1) and (result[P]='0') do
             begin
               system.Delete(result,P,1);
               dec(P);
             end;
        end else if (P<>0) then // we have a decimalseparator
        begin
          P := Length(result);
          while (P>0) and (result[P] = '0') do dec(P);
          if (P>0) and (result[P]=DecimalSeparator) then dec(P);
          SetLength(result, P);
        end;
      end;

    ffExponent:

      begin
        if (Precision = -1) Or (Precision > 15) then Precision := 15;
        Str(Value:Precision + 8, result);
        result[3] := DecimalSeparator;
        P:=4;
        while (P>0) and (Digits < P) And (result[Precision + 5] = '0') do
        begin
          if P<>1 then
            system.Delete(result, Precision + 5, 1)
          else
            system.Delete(result, Precision + 3, 3);
          dec(P);
        end;
        if result[1] = ' ' then System.Delete(result, 1, 1);
      end;

    ffFixed:

      begin
        if Digits = -1 then Digits := 2
        else if Digits > 18 then Digits := 18;
        Str(Value:0:Digits, result);
        if result[1] = ' ' then System.Delete(result, 1, 1);
        P := Pos('.', result);
        if P <> 0 then result[P] := DecimalSeparator;
      end;

    ffNumber:

      begin
        if Digits = -1 then Digits := 2
        else if Digits > 15 then Digits := 15;
        Str(Value:0:Digits, result);
        if result[1] = ' ' then System.Delete(result, 1, 1);
        P := Pos('.', result);
        if P <> 0 then
          result[P] := DecimalSeparator
        else
          P := Length(result)+1;
        dec(P, 3);
        while (P > 1) Do
        begin
          if result[P - 1] <> '-' then Insert(ThousandSeparator, result, P);
          dec(P, 3);
        end;
      end;

    ffCurrency:

      begin
        if Value < 0 then
        begin
          Negative := True;
          Value := -Value;
        end
        else Negative := False;

        if Digits = -1 then Digits := CurrencyDecimals
        else if Digits > 18 then Digits := 18;
        Str(Value:0:Digits, result);
        if result[1] = ' ' then System.Delete(result, 1, 1);
        P := Pos('.', result);
        if P <> 0 then result[P] := DecimalSeparator;
        dec(P, 3);
        while (P > 1) Do
        begin
          Insert(ThousandSeparator, result, P);
          dec(P, 3);
        end;

        if Not Negative then
        begin
          Case CurrencyFormat Of
            0: result := CurrencyString + result;
            1: result := result + CurrencyString;
            2: result := CurrencyString + ' ' + result;
            3: result := result + ' ' + CurrencyString;
          end
        end
        else
        begin
          Case NegCurrFormat Of
            0: result := '(' + CurrencyString + result + ')';
            1: result := '-' + CurrencyString + result;
            2: result := CurrencyString + '-' + result;
            3: result := CurrencyString + result + '-';
            4: result := '(' + result + CurrencyString + ')';
            5: result := '-' + result + CurrencyString;
            6: result := result + '-' + CurrencyString;
            7: result := result + CurrencyString + '-';
            8: result := '-' + result + ' ' + CurrencyString;
            9: result := '-' + CurrencyString + ' ' + result;
            10: result := CurrencyString + ' ' + result + '-';
          end;
        end;
      end;
  end;
end;


function CurrToStr(Value: Currency): astr;
begin
  result:=FloatToStrF(Value,ffNumber,15,2);
end;

function TextToFloat(Buffer: PChar; var Value: Extended): Boolean; overload;
var E, P: integer; S: astr;
begin
  S:=StrPas(Buffer);
  P:=Pos(DecimalSeparator,S);
  if (P<>0) then
    S[P] := '.';
  Val(trim(S),Value,E);
  result:=(E=0);
end;


procedure FloatToDecimal(var result: TFloatRec; Value: Extended; Precision, Decimals : integer);
var
  Buffer: String[24];
  Error, N: integer;
begin
  Str(Value:23, Buffer);
  result.Negative := (Buffer[1] = '-');
  Val(Copy(Buffer, 19, 5), result.Exponent, Error);
  inc(result. Exponent);
  result.Digits[0] := Buffer[2];
  Move(Buffer[4], result.Digits[1], 14);
  if Decimals + result.Exponent < Precision then
    N := Decimals + result.Exponent
  else
    N := Precision;
  if N > 15 then
    N := 15;
  if N = 0 then
  begin
    if result.Digits[0] >= '5' then
    begin
      result.Digits[0] := '1';
      result.Digits[1] := #0;
      inc(result.Exponent);
    end else
      result.Digits[0] := #0;
  end else if N > 0 then
  begin
    if result.Digits[N] >= '5' then
    begin
      repeat
        result.Digits[N] := #0;
        dec(N);
        inc(result.Digits[N]);
      until (N = 0) Or (result.Digits[N] < ':');
      if result.Digits[0] = ':' then
        begin
        result.Digits[0] := '1';
        inc(result.Exponent);
        end;
    end else
    begin
      result.Digits[N] := '0';
      while (result.Digits[N] = '0') And (N > -1) Do
        begin
        result.Digits[N] := #0;
        dec(N);
        end;
    end;
  end else 
    result.Digits[0] := #0;

  if result.Digits[0] = #0 then
  begin
    result.Exponent := 0;
    result.Negative := False;
  end;
end;

function CompareText(const S1, S2: astr): integer;
var i, cnt, cnt1, cnt2: integer; Chr1, Chr2: byte;
begin
  result:= 0;
  cnt1:= Length(S1);
  cnt2:= Length(S2);
  if (cnt1 > cnt2) then cnt:= cnt2 else cnt:= cnt1;
  i:= 0;
  while (result = 0) and (i < cnt) do
  begin
    inc(i);
    Chr1:= byte(s1[i]);
    Chr2:= byte(s2[i]);
    if Chr1 in [97..122] then dec(Chr1,32);
    if Chr2 in [97..122] then dec(Chr2,32);
    result:= Chr1 - Chr2;
  end;
  if (result = 0) then result:= cnt1 - cnt2;
end;

function SameText(const s1,s2:astr):Boolean;
begin
  result:=CompareText(S1,S2)=0;
end;

{ returns a string representing integer Value  }
function IntToStr(value: integer): astr; overload;
begin
  system.Str(value, result);
end ;

function IntToStr(value: int64): astr; overload;
begin
  system.Str(value, result);
end ;

{ elegant alias }
function i2s(value: integer): astr; overload;
begin
  result:= IntToStr(value);
end;

function i2s(value: int64): astr; overload;
begin
  result:= IntToStr(value);
end;

{$IFDEF FPC}
 function IntToStr(value: QWord): astr;
 begin
   System.Str(value, result);
 end;

 function i2s(value: QWord): astr;
 begin
   result:= IntToStr(value);
 end;
{$ENDIF}


{ elegant wrapper for fpc/delphi lowercase function }
function Lcase(const s: astr): astr;
begin
 {$IFDEF FPC}result:= system.lowercase(s);{$ELSE}result:= lowercase(s);{$ENDIF}
end;

{ elegant wrapper for fpc/delphi uppercase function }
function Ucase(const s: astr): astr;
begin
 {$IFDEF FPC}result:= system.upcase(s);{$ELSE}result:= uppercase(s);{$ENDIF}
end;

function BoolToStr(b: boolean): astr;
begin
  if b then result:= 'TRUE' else result:= 'FALSE';
end;

{   Trim returns a copy of S with blanks characters on the left and right stripped off   }
const WhiteSpace = [' ',#10,#13,#9];

function Trim(const S: astr): astr;
var Ofs, Len: integer;
begin
  len := Length(S);
  while (Len>0) and (S[Len] in WhiteSpace) do dec(Len);
  Ofs := 1;
  while (Ofs<=Len) and (S[Ofs] in WhiteSpace) do inc(Ofs);
  result := Copy(S, Ofs, 1 + Len - Ofs);
end ;

{   TrimLeft returns a copy of S with all blank characters on the left stripped off  }
function TrimLeft(const S: astr): astr;
var i,l:integer;
begin
  L:= length(s);
  i := 1;
  while (i<=L) and (s[i] in whitespace) do inc(i);
  result:= copy(s, i, L);
end ;

{   TrimRight returns a copy of S with all blank characters on the right stripped off  }
function TrimRight(const S: astr): astr;
var l:integer;
begin
  l:= length(s);
  while (l>0) and (s[l] in whitespace) do dec(l);
  result := copy(s,1,l);
end ;

{   QuotedStr returns S quoted left and right and every single quote in S
    replaced by two quotes   }
function QuotedStr(const S: astr): astr;
begin
  result := AnsiQuotedStr(s, '''');
end ;

{   AnsiQuotedStr returns S quoted left and right by Quote,
    and every single occurance of Quote replaced by two   }
function AnsiQuotedStr(const S: astr; Quote: char): astr;
var i, j, cnt: integer;
begin
  result:= '' + Quote;
  cnt:= length(s);
  i:= 0;
  j:= 0;
  while i < cnt do begin
    i:= i+1;
    if S[i] = Quote then begin
      result := result + copy(S, 1 + j, i - j) + Quote;
      j := i;
    end ;
  end ;
  if i <> j then result := result + copy(S, 1 + j, i - j);
  result:= result + Quote;
end ;

{   AnsiExtractQuotedStr returns a copy of Src with quote characters
    deleted to the left and right and double occurances
    of Quote replaced by a single Quote   }
function AnsiExtractQuotedStr(var  Src: PChar; Quote: Char): astr;
var
  P,Q,R: PChar;
begin
 P := Src;
 Q := StrEnd(P);
 result:='';
 if P=Q then exit;
 if P^<>quote then exit;
 inc(p);

 setlength(result,(Q-P)+1);
 R:=@result[1];
 while P <> Q do
 begin
     R^:=P^;
     inc(R);
     if (P^ = Quote) then
     begin
       P := P + 1;
       if (p^ <> Quote) then begin
         dec(R);
         break;
       end;
     end;
     P := P + 1;
 end ;
 src:=p;
 SetLength(result, (R-pchar(@result[1])));
end ;


{   AdjustLineBreaks returns S with all CR characters not followed by LF
    replaced with CR/LF  }
//  under Linux all CR characters or CR/LF combinations should be replaced with LF

function AdjustLineBreaks(const S: astr): astr;
begin
 {$IFDEF FPC}
  result := AdjustLineBreaks(S, DefaultTextLineBreakStyle);
 {$ELSE}
  result := AdjustLineBreaks(S, tlbsCRLF);
 {$ENDIF}
end;

function AdjustLineBreaks(const S: astr; Style: TTextLineBreakStyle): astr;
var Source,Dest: PChar;
    DestLen: integer;
    I,J,L: Longint;
begin
  Source:=Pointer(S);
  L:=Length(S);
  DestLen:=L;
  I:=1;
  while (I<=L) do
    begin
    case S[i] of
      #10: if (Style=tlbsCRLF) then
               inc(DestLen);
      #13: if (Style=tlbsCRLF) then
             if (I<L) and (S[i+1]=#10) then
               inc(I)
             else
               inc(DestLen)
             else if (I<L) and (S[I+1]=#10) then
               dec(DestLen);
    end;
    inc(I);
    end;
  if (DestLen=L) then
    result:=S
  else
    begin
    SetLength(result, DestLen);
    FillChar(result[1],DestLen,0);
    Dest := Pointer(result);
    J:=0;
    I:=0;
    while I<L do
      case Source[I] of
        #10: begin
             if Style=tlbsCRLF then
               begin
               Dest[j]:=#13;
               inc(J);
              end;
             Dest[J] := #10;
             inc(J);
             inc(I);
             end;
        #13: begin
               if Style=tlbsCRLF then begin
                 Dest[j] := #13;
                 inc(J);
               end;
               Dest[j]:=#10;
               inc(J);
               inc(I);
               if Source[I]=#10 then inc(I);
             end;
      else
        Dest[j]:=Source[i];
        inc(J);
        inc(I);
      end;
    end;
end;


{   IsValidIdent returns true if the first character of Ident is in:
    'A' to 'Z', 'a' to 'z' or '_' and the following characters are
    on of: 'A' to 'Z', 'a' to 'z', '0'..'9' or '_'    }
function IsValidIdent(const Ident: astr): boolean;
var i, len: integer;
begin
result := false;
len := length(Ident);
if len <> 0 then begin
   result := Ident[1] in ['A'..'Z', 'a'..'z', '_'];
   i := 1;
   while (result) and (i < len) do begin
      i := i + 1;
      result := result and (Ident[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
      end ;
   end ;
end ;


function strcat(dest,source : pchar) : pchar;
begin
  strcopy(strend(dest),source);
  strcat:=dest;
end;


function strlcat(dest,source : pchar;l : SizeInt) : pchar;
var destend : pchar;
begin
  destend:=strend(dest);
  dec(l,destend-dest);
  if l>0 then
    strlcopy(destend,source,l);
  strlcat:=dest;
end;

function strmove(dest,source : pchar;l : SizeInt) : pchar;
begin
  move(source^,dest^,l);
  strmove:=dest;
end;


function strpos(p1,p2 : pchar) : pchar;
var
  p: pchar;
  lstr2 : SizeInt;
begin
  strpos:=nil;
  p:= strscan(p1,p2^);
  if p = nil then exit;
  lstr2:=strlen(p2);
  while p <> nil do
  begin
    if strlcomp(p, p2, lstr2) = 0 then
    begin
      strpos:= p;
      exit;
    end;
    inc(p);
    p:=strscan(p, p2^);
  end;
end;


type
   pbyte = ^byte;
   CharArray = array[0..0] of char;


{-- copy/pasted DIRECTLY from freepascal sources ------------------------------}
{ ansistrings! if using shortstrings use strings.pp unit included with fpc }
function StrPas(p: PChar): astr;
begin
  result:=p;
end ;

{-- copy/pasted DIRECTLY from freepascal sources ------------------------------}
{ ansistrings! if using shortstrings use strings.pp unit included with fpc }
function StrAlloc(Size: cardinal): PChar;
begin
  inc(size,sizeof(cardinal));
  getmem(result,size);
  cardinal(pointer(result)^):=size;
  inc(result,sizeof(cardinal));
end;


{-- copy/pasted DIRECTLY from freepascal sources ------------------------------}
{ ansistrings! if using shortstrings use strings.pp unit included with fpc }
function strnew(p : pchar) : pchar;
var
  len : longint;
begin
  result:=nil;
  if (p=nil) or (p^=#0) then
    exit;
  len:=strlen(p)+1;
  result:=StrAlloc(Len);
  if result<>nil then
    strmove(result,p,len);
end;


{-- copy/pasted DIRECTLY from freepascal sources ------------------------------}
{ ansistrings! if using shortstrings use strings.pp unit included with fpc }
function StrPCopy(Dest: PChar; Source: astr): PChar;
begin
  result := StrMove(Dest, PChar(Source), length(Source)+1);
end ;

{-- copy/pasted DIRECTLY from freepascal sources ------------------------------}
{ ansistrings! if using shortstrings use strings.pp unit included with fpc }
function StrPLCopy(Dest: PChar; Source: astr; MaxLen: SizeUInt): PChar;
var cnt: SizeUInt;
begin
  result := Dest;
  if (result <> Nil) and (MaxLen <> 0) then
  begin
    cnt := Length(Source);
    if cnt > MaxLen then cnt := MaxLen;
    StrMove(Dest, PChar(Source), cnt);
    CharArray(result^)[cnt] := #0;  { terminate ! }
  end ;
end ;


{-- copy/pasted DIRECTLY from freepascal sources ------------------------------}
{ ansistrings! if using shortstrings use strings.pp unit included with fpc }
procedure StrDispose(p: PChar);
begin
  if (p <> Nil) then
  begin
    dec(p, sizeof(cardinal));
    Freemem(p, cardinal(pointer(p)^));
  end;
end;

{-- copy/pasted DIRECTLY from freepascal sources ------------------------------}
{ ansistrings! if using shortstrings use strings.pp unit included with fpc }
function StrBufSize(p: PChar): SizeUInt;
begin
  if p <> Nil then
    result := SizeUInt(pointer(p - SizeOf(SizeUInt))^)-sizeof(SizeUInt)
  else
    result := 0;
end ;

function WrapText(const Line, BreakStr: string; const BreakChars: TSysCharSet;  MaxCol: integer): string;
const
  Quotes = ['''', '"'];
var
  L : String;
  C,LQ,BC : Char;
  P,BLen,Len : integer;
  HB,IBC : Boolean;
begin
  result:='';
  L:=Line;
  Blen:=Length(BreakStr);
  if (BLen>0) then
    BC:=BreakStr[1]
  else
    BC:=#0;
  Len:=Length(L);
  while (Len>0) do
    begin
    P:=1;
    LQ:=#0;
    HB:=False;
    IBC:=False;
    while ((P<=Len) and ((P<=MaxCol) or not IBC)) and ((LQ<>#0) or Not HB) do
      begin
      C:=L[P];
      if (C=LQ) then
        LQ:=#0
      else if (C in Quotes) then
        LQ:=C;
      if (LQ<>#0) then
        inc(P)
      else
        begin
        HB:=((C=BC) and (BreakStr=Copy(L,P,BLen)));
        if HB then
          inc(P,Blen)
        else
          begin
          if (P>MaxCol) then
            IBC:=C in BreakChars;
          inc(P);
          end;
        end;
//      Writeln('"',C,'" : IBC : ',IBC,' HB  : ',HB,' LQ  : ',LQ,' P>MaxCol : ',P>MaxCol);
      end;
    result:=result+Copy(L,1,P-1);
    if Not HB then
      result:=result+BreakStr;
    Delete(L,1,P-1);
    Len:=Length(L);
    end;
end;

{$IFNDEF FPC} // delphi
const sLineBreak = #13#10;
{$ENDIF}

function WrapText(const Line: string; MaxCol: integer): string;
begin
  result := WrapText(Line, sLineBreak, [' ', '-', #9], MaxCol);
end;

function StringReplace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string;
var Srch,OldP,RemS: string; // Srch and Oldp can contain uppercase versions of S,OldPattern
    P: integer;
begin
  Srch:=S;
  OldP:=OldPattern;
  if rfIgnoreCase in Flags then begin
    Srch:=UpperCase(Srch);
    OldP:=UpperCase(OldP);
  end;
  RemS:=S;
  result:='';
  while (Length(Srch)<>0) do
  begin
    P:=Pos(OldP, Srch);
    if P=0 then begin
      result:=result+RemS;
      Srch:='';
    end else
    begin
      result:=result+Copy(RemS,1,P-1)+NewPattern;
      P:=P+Length(OldP);
      RemS:=Copy(RemS,P,Length(RemS)-P+1);
      if not (rfReplaceAll in Flags) then begin
        result:=result+RemS;
        Srch:='';
      end else
        Srch:=Copy(Srch,P,Length(Srch)-P+1);
    end;
  end;
end;

function IsDelimiter(const Delimiters, S: string; Index: integer): Boolean;
begin
  result:= False;
  if (Index>0) and (Index<=Length(S)) then
    result:=Pos(S[Index],Delimiters)<>0; // Note we don't do MBCS yet
end;

{   LeftStr returns Count left-most characters from S }
function LeftStr(const S: string; Count: integer): string;
begin
  result:= Copy(S, 1, Count);
end ;

{ RightStr returns Count right-most characters from S }
function RightStr(const S: string; Count: integer): string;
begin
  if Count>Length(S) then Count:= Length(S);
  result:= Copy(S, 1 + Length(S) - Count, Count);
end;

{   NewStr creates a new PString and assigns S to it
    if length(s) = 0 NewStr returns Nil   }
function NewStr(const S: string): PString;
begin
  if (S='') then
    result:= nil
  else begin
    new(result);
    if (result <> nil) then result^:= s;
  end;
end;

{   DisposeStr frees the memory occupied by S   }
procedure DisposeStr(S: PString);
begin
  if S <> Nil then begin
    dispose(s);
    S:=nil;
  end;
end;

{   AssignStr assigns S to P^   }
procedure AssignStr(var P: PString; const S: string);
begin
  P^ := s;
end ;

{   AppendStr appends S to Dest   }
procedure AppendStr(var Dest: String; const S: string);
begin
  Dest := Dest + S;
end ;

{   UpperCase returns a copy of S where all lowercase characters ( from a to z )
    have been converted to uppercase   }
function UpperCase(Const S : String) : String;
var i: integer;
    P: PChar;
begin
  result := S;
  UniqueString(result);
  P:=Pchar(result);
  for i := 1 to Length(result) do begin
    if (P^ in ['a'..'z']) then P^ := char(byte(p^) - 32);
    inc(P);
  end;
end;

{   LowerCase returns a copy of S where all uppercase characters ( from A to Z )
    have been converted to lowercase  }
function Lowercase(Const S : String) : String;
var i: integer;
    P: PChar;
begin
  result := S;
  UniqueString(result);
  P:=Pchar(result);
  for i:= 1 to Length(result) do begin
    if (P^ in ['A'..'Z']) then P^ := char(byte(p^) + 32);
    inc(P);                                                                            
  end;
end;


function CompareMemRange(P1, P2: Pointer; Length: cardinal): integer;
var i: cardinal;
begin
  i:= 0;
  result:= 0;
  while (result=0) and (I<length) do begin
    result:= byte(P1^)-byte(P2^);
    P1:= pchar(P1)+1;            // VP compat.
    P2:= pchar(P2)+1;
    i:= i + 1;
  end ;
end ;

{   CompareStr compares S1 and S2, the result is the based on
    substraction of the ascii values of the characters in S1 and S2
    case     result
    S1 < S2  < 0
    S1 > S2  > 0
    S1 = S2  = 0     }
function CompareStr(const S1, S2: string): integer;
var cnt, cnt1, cnt2: integer;
begin
  result:= 0;
  cnt1:= Length(S1);
  cnt2:= Length(S2);
  if cnt1 > cnt2 then cnt:= cnt2 else cnt:= cnt1;
  result:= CompareMemRange(Pointer(S1),Pointer(S2), cnt);
  if result = 0 then result:= cnt1-cnt2;
end;


function StrCopy(Dest, Source:PChar): PChar;
var cnt : SizeInt;
begin
  cnt:= 0;
  while Source[cnt] <> #0 do begin
    Dest[cnt] := char(Source[cnt]);
    inc(cnt);
  end;
  { terminate the string }
  Dest[cnt] := #0;
  StrCopy := Dest;
end;

function StrECopy(Dest, Source: PChar): PChar;
{ Equivalent to the following:                                          }
{  strcopy(Dest,Source);                                                }
{  StrECopy := StrEnd(Dest);                                            }
var cnt : SizeInt;
begin
  cnt := 0;
  while Source[cnt] <> #0 do begin
    Dest[cnt] := char(Source[cnt]);
    inc(cnt);
  end;
  { terminate the string }
  Dest[cnt] := #0;
 {$IFDEF FPC}
  StrECopy := @(Dest[cnt]);
 {$ELSE}
  StrECopy := pchar(Dest[cnt]);
 {$ENDIF}
end;

function StrLCopy(Dest,Source: PChar; MaxLen: SizeInt): PChar;
var cnt: SizeInt;
begin
 cnt := 0;
 { To be compatible with BP, on a null string, put two nulls }
 if Source[0] = #0 then begin
   Dest[0]:=Source[0];
   inc(cnt);
 end;
 while (Source[cnt] <> #0)  and (cnt < MaxLen) do begin
   Dest[cnt] := char(Source[cnt]);
   inc(cnt);
 end;
 { terminate the string }
 Dest[cnt] := #0;
 StrLCopy := Dest;
end;

function StrEnd(P: PChar): PChar;
var cnt: SizeInt;
begin
  cnt := 0; while P[cnt] <> #0 do inc(cnt);
 {$IFDEF FPC}
  StrEnd := @(P[cnt]);
 {$ELSE}
  StrEnd := pchar(P[cnt]);
 {$ENDIF}
end;

function StrComp(p1, p2 : PChar): SizeInt;
var cnt: SizeInt;
begin
 cnt:= 0;
 while p1[cnt] = p2[cnt] do begin
   if (p2[cnt] = #0) or (p1[cnt] = #0) then break;
   inc(cnt);
 end;
 StrComp := ord(p1[cnt]) - ord(p2[cnt]);
end;

function StrLComp(p1, p2 : PChar; L: SizeInt): SizeInt;
var cnt: SizeInt;
    c1, c2: char;
begin
  cnt := 0;
 if L = 0 then begin
   StrLComp := 0;
   exit;
 end;
 repeat
   c1 := p1[cnt];
   c2 := p2[cnt];
   if (c1 = #0) or (c2 = #0) then break;
   inc(cnt);
 until (c1 <> c2) or (cnt >= L);
 StrLComp := ord(c1) - ord(c2);
end; 

function StrIComp(p1, p2 : PChar): SizeInt;
var
cnt: SizeInt;
c1, c2: char;
begin
  cnt := 0;
  c1 := upcase(p1[cnt]);
  c2 := upcase(p2[cnt]);
 while c1 = c2 do
 begin
   if (c1 = #0) or (c2 = #0) then break;
   inc(cnt);
   c1 := upcase(p1[cnt]);
   c2 := upcase(p2[cnt]);
end;
 StrIComp := ord(c1) - ord(c2);
end; 

function StrLIComp(p1, p2: PChar; L: SizeInt): SizeInt;
var cnt: SizeInt;
    c1, c2: char;
begin
 cnt := 0;
 if L = 0 then begin
   StrLIComp := 0;
   exit; 
 end;
 repeat
   c1 := upcase(p1[cnt]);
   c2 := upcase(p2[cnt]);
   if (c1 = #0) or (c2 = #0) then break;
   inc(cnt);
 until (c1 <> c2) or (cnt >= L);
 StrLIComp := ord(c1) - ord(c2);
end; 

function StrScan(P: pchar; C: char): pchar;
var cnt: sizeint;
begin
 cnt := 0;
 { As in Borland Pascal , if looking for NULL return null }
 if C = #0 then begin
  {$IFDEF FPC}
   StrScan := @(P[StrLen(P)]);
  {$ELSE}
   StrScan := pchar(P[StrLen(P)]);
  {$ENDIF}
   exit;
 end;
 { Find first matching character of Ch in Str }
 while P[cnt] <> #0 do begin
   if C = P[cnt] then begin
    {$IFDEF FPC}
     StrScan := @(P[cnt]);
    {$ELSE}
     StrScan := pchar(P[cnt]);
    {$ENDIF}
     exit;
   end;
   inc(cnt);
 end;
 { nothing found. }
 StrScan := nil;
end; 

function StrRScan(p: pchar; c: char): pchar;
var
  cnt: SizeInt;
  index: SizeInt;
begin
 cnt := Strlen(P);
 { As in Borland Pascal , if looking for NULL return null }
 if C = #0 then begin
  {$IFDEF FPC}
   StrRScan := @(p[cnt]);
  {$ELSE}
   StrRScan := pchar(p[cnt]);
  {$ENDIF}
   exit;
 end;

 dec(cnt);

 for index := cnt downto 0 do begin
   if C = p[index] then begin
    {$IFDEF FPC}
     StrRScan := @(p[index]);
    {$ELSE}
     StrRScan := pchar(p[index]);
    {$ENDIF}
     exit;
   end;
 end;
 { nothing found. }
 StrRScan := nil;
end;

function StrUpper(p: pchar): pchar;
var cnt: SizeInt;
begin
  cnt := 0;
  while (P[cnt] <> #0) do begin
    if P[cnt] in [#97..#122,#128..#255] then P[cnt] := Upcase(P[cnt]);
    inc(cnt);
  end;
  StrUpper := P;
end;

function StrLower(p: pchar): pchar;
var cnt: SizeInt;
begin
  cnt := 0;
  while (p[cnt] <> #0) do begin
    if p[cnt] in [#65..#90] then p[cnt] := chr(ord(p[cnt]) + 32);
    inc(cnt);
  end;
  StrLower:= p;
end;


end.
