{
 Reason for unit: modular, smartlinking, standalone functions in separate unit
 Authors/Credits: FPC Team RTL, Lars (L505)
 License: Freepascal RTL Modified GPL
}
unit pwstrutil; {$I defines1.inc}

interface

uses
  pwtypes;

type
  TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);

  PString = ^String;

  { For FloatToText }
  TFloatFormat = (ffGeneral, ffExponent, ffFixed, ffNumber, ffCurrency);
  TFloatValue = (fvExtended, fvCurrency, fvSingle, fvReal, fvDouble, fvComp);

  TFloatRec = record
    Exponent: Integer;
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

function BoolToStr(B: Boolean): string;
function CurrToStr(Value: Currency): string;
procedure FloatToDecimal(var Result: TFloatRec; Value: Extended; Precision, Decimals : integer);
function CompareText(const S1, S2: string): integer;
function SameText(const s1,s2:String):Boolean;
function StrCopy(Dest, Source:PChar): PChar;
function StrECopy(Dest, Source: PChar): PChar;
function StrLCopy(Dest,Source: PChar; MaxLen: SizeInt): PChar;
function StrEnd(P: PChar): PChar;
function StrComp(Str1, Str2 : PChar): SizeInt;
function StrLComp(Str1, Str2 : PChar; L: SizeInt): SizeInt;
function StrIComp(Str1, Str2 : PChar): SizeInt;
function StrLIComp(Str1, Str2 : PChar; L: SizeInt): SizeInt;
function StrScan(P: PChar; C: Char): PChar;
function StrRScan(P: PChar; C: Char): PChar;
function StrUpper(P: PChar): PChar;
function StrLower(P: PChar): PChar;
function NewStr(const S: string): PString;
procedure DisposeStr(S: PString);
procedure AssignStr(var P: PString; const S: string);
procedure AppendStr(var Dest: String; const S: string);
function UpperCase(Const S : String) : String;
function Lowercase(Const S : String) : String;
function CompareMemRange(P1, P2: Pointer; Length: cardinal): integer;
function CompareStr(const S1, S2: string): Integer;
function LeftStr(const S: string; Count: integer): string;
function RightStr(const S: string; Count: integer): string;

function StringReplace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string;
function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean;

type
  TSysCharSet = Set of char;

function WrapText(const Line, BreakStr: string; const BreakChars: TSysCharSet;  MaxCol: Integer): string; overload;
function WrapText(const Line: string; MaxCol: Integer): string; overload;

function strcat(dest,source : pchar) : pchar;
function strlcat(dest,source : pchar;l : SizeInt) : pchar;
function strmove(dest,source : pchar;l : SizeInt) : pchar;
function strpos(str1,str2 : pchar) : pchar;
function StrPas(Str: PChar): string;
function StrAlloc(Size: cardinal): PChar;
function strnew(p : pchar) : pchar;
function StrPCopy(Dest: PChar; Source: string): PChar;
function StrPLCopy(Dest: PChar; Source: string; MaxLen: SizeUInt): PChar;
procedure StrDispose(Str: PChar);
function StrBufSize(Str: PChar): SizeUInt;

function Trim(const S: string): string;
function TrimLeft(const S: string): string;
function TrimRight(const S: string): string;
function QuotedStr(const S: string): string;
function AnsiQuotedStr(const S: string; Quote: char): string;
function AnsiExtractQuotedStr(var  Src: PChar; Quote: Char): string;
function AdjustLineBreaks(const S: string): string; overload;
function AdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string; overload;
function IsValidIdent(const Ident: string): boolean;

function i2s(value: integer): string; overload;
function i2s(value: int64): string; overload;

function IntToStr(value: integer): string; overload;
function IntToStr(value: int64): string; overload;
{$IFDEF FPC}
  function IntToStr(value: QWord): string; overload;
{$ENDIF}

implementation


{$IFNDEF FPC}
function strlen(p: pchar): longint;
var i : longint;
begin
  i:= 0; while p[i]<>#0 do inc(i); result:= i; exit;
end;
{$ENDIF}

function FloatToStrF(Value: Extended; format: TFloatFormat; Precision, Digits: Integer): String;
Var
  P: Integer;
  Negative, TooSmall, TooLarge: Boolean;
begin
  p:= 0;
  TooLarge:= false;
  Case format Of

    ffGeneral:

      begin
        If (Precision = -1) Or (Precision > 15) then Precision := 15;
        TooSmall := (Abs(Value) < 0.00001) and (Value>0.0);
        If Not TooSmall then
        begin
          Str(Value:digits:precision, Result);
          P := Pos('.', Result);
          if P<>0 then
            Result[P] := DecimalSeparator;
          TooLarge := P > Precision + 1;
        end;

        If TooSmall Or TooLarge then
          begin
          Result := FloatToStrF(Value, ffExponent, Precision, Digits);
          // Strip unneeded zeroes.
          P:=Pos('E',result)-1;
          If P<>-1 then
             while (P>1) and (Result[P]='0') do
               begin
               system.Delete(Result,P,1);
               Dec(P);
               end;
          end
        else if (P<>0) then // we have a decimalseparator
          begin
          P := Length(Result);
          while (P>0) and (Result[P] = '0') Do
            Dec(P);
          If (P>0) and (Result[P]=DecimalSeparator) then
            Dec(P);
          SetLength(Result, P);
          end;
      end;

    ffExponent:

      begin
        If (Precision = -1) Or (Precision > 15) then Precision := 15;
        Str(Value:Precision + 8, Result);
        Result[3] := DecimalSeparator;
        P:=4;
        while (P>0) and (Digits < P) And (Result[Precision + 5] = '0') do
          begin
          If P<>1 then
            system.Delete(Result, Precision + 5, 1)
          else
            system.Delete(Result, Precision + 3, 3);
          Dec(P);
          end;
        If Result[1] = ' ' then
          System.Delete(Result, 1, 1);
      end;

    ffFixed:

      begin
        If Digits = -1 then Digits := 2
        Else If Digits > 18 then Digits := 18;
        Str(Value:0:Digits, Result);
        If Result[1] = ' ' then
          System.Delete(Result, 1, 1);
        P := Pos('.', Result);
        If P <> 0 then Result[P] := DecimalSeparator;
      end;

    ffNumber:

      begin
        If Digits = -1 then Digits := 2
        Else If Digits > 15 then Digits := 15;
        Str(Value:0:Digits, Result);
        If Result[1] = ' ' then System.Delete(Result, 1, 1);
        P := Pos('.', Result);
        If P <> 0 then
          Result[P] := DecimalSeparator
        else
          P := Length(Result)+1;
        Dec(P, 3);
        while (P > 1) Do
        begin
          If Result[P - 1] <> '-' then Insert(ThousandSeparator, Result, P);
          Dec(P, 3);
        end;
      end;

    ffCurrency:

      begin
        If Value < 0 then
        begin
          Negative := True;
          Value := -Value;
        End
        Else Negative := False;

        If Digits = -1 then Digits := CurrencyDecimals
        Else If Digits > 18 then Digits := 18;
        Str(Value:0:Digits, Result);
        If Result[1] = ' ' then System.Delete(Result, 1, 1);
        P := Pos('.', Result);
        If P <> 0 then Result[P] := DecimalSeparator;
        Dec(P, 3);
        while (P > 1) Do
        begin
          Insert(ThousandSeparator, Result, P);
          Dec(P, 3);
        end;

        If Not Negative then
        begin
          Case CurrencyFormat Of
            0: Result := CurrencyString + Result;
            1: Result := Result + CurrencyString;
            2: Result := CurrencyString + ' ' + Result;
            3: Result := Result + ' ' + CurrencyString;
          End
        End
        Else
        begin
          Case NegCurrFormat Of
            0: Result := '(' + CurrencyString + Result + ')';
            1: Result := '-' + CurrencyString + Result;
            2: Result := CurrencyString + '-' + Result;
            3: Result := CurrencyString + Result + '-';
            4: Result := '(' + Result + CurrencyString + ')';
            5: Result := '-' + Result + CurrencyString;
            6: Result := Result + '-' + CurrencyString;
            7: Result := Result + CurrencyString + '-';
            8: Result := '-' + Result + ' ' + CurrencyString;
            9: Result := '-' + CurrencyString + ' ' + Result;
            10: Result := CurrencyString + ' ' + Result + '-';
          end;
        end;
      end;
  end;
end;


function CurrToStr(Value: Currency): string;
begin
  Result:=FloatToStrF(Value,ffNumber,15,2);
end;

function TextToFloat(Buffer: PChar; Var Value: Extended): Boolean; overload;
Var
  E,P : Integer;
  S : String;
begin
  S:=StrPas(Buffer);
  P:=Pos(DecimalSeparator,S);
  If (P<>0) then
    S[P] := '.';
  Val(trim(S),Value,E);
  Result:=(E=0);
end;


procedure FloatToDecimal(Var Result: TFloatRec; Value: Extended; Precision, Decimals : integer);
Var
  Buffer: String[24];
  Error, N: Integer;
begin
  Str(Value:23, Buffer);
  Result.Negative := (Buffer[1] = '-');
  Val(Copy(Buffer, 19, 5), Result.Exponent, Error);
  Inc(Result. Exponent);
  Result.Digits[0] := Buffer[2];
  Move(Buffer[4], Result.Digits[1], 14);
  If Decimals + Result.Exponent < Precision then
    N := Decimals + Result.Exponent
  Else
    N := Precision;
  If N > 15 then
    N := 15;
  If N = 0 then
  begin
    If Result.Digits[0] >= '5' then
    begin
      Result.Digits[0] := '1';
      Result.Digits[1] := #0;
      Inc(Result.Exponent);
    end else
      Result.Digits[0] := #0;
  end else if N > 0 then
  begin
    If Result.Digits[N] >= '5' then
    begin
      repeat
        Result.Digits[N] := #0;
        Dec(N);
        Inc(Result.Digits[N]);
      until (N = 0) Or (Result.Digits[N] < ':');
      If Result.Digits[0] = ':' then
        begin
        Result.Digits[0] := '1';
        Inc(Result.Exponent);
        end;
    end else
    begin
      Result.Digits[N] := '0';
      while (Result.Digits[N] = '0') And (N > -1) Do
        begin
        Result.Digits[N] := #0;
        Dec(N);
        end;
    end;
  end else 
    Result.Digits[0] := #0;

  if Result.Digits[0] = #0 then
  begin
    Result.Exponent := 0;
    Result.Negative := False;
  end;
end;

function CompareText(const S1, S2: string): integer;
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

function SameText(const s1,s2:String):Boolean;
begin
  Result:=CompareText(S1,S2)=0;
end;

{ returns a string representing integer Value  }
function IntToStr(value: integer): string; overload;
begin
  System.Str(value, result);
end ;

function IntToStr(value: int64): string; overload;
begin
  System.Str(value, result);
end ;

{ elegant alias }
function i2s(value: integer): string; overload;
begin
  result:= IntToStr(value);
end ;

function i2s(value: int64): string; overload;
begin
  result:= IntToStr(value);
end ;

{$IFDEF FPC}
 function IntToStr(value: QWord): string;
 begin
   System.Str(value, result);
 end ;

 function i2s(value: QWord): string;
 begin
   result:= IntToStr(value);
 end ;
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

function BoolToStr(b: boolean): string;
begin
  if b then result:='TRUE' else result:='FALSE';
end;

{   Trim returns a copy of S with blanks characters on the left and right stripped off   }
const WhiteSpace = [' ',#10,#13,#9];

function Trim(const S: string): string;
var Ofs, Len: integer;
begin
  len := Length(S);
  while (Len>0) and (S[Len] in WhiteSpace) do dec(Len);
  Ofs := 1;
  while (Ofs<=Len) and (S[Ofs] in WhiteSpace) do Inc(Ofs);
  result := Copy(S, Ofs, 1 + Len - Ofs);
end ;

{   TrimLeft returns a copy of S with all blank characters on the left stripped off  }
function TrimLeft(const S: string): string;
var i,l:integer;
begin
  L:= length(s);
  i := 1;
  while (i<=L) and (s[i] in whitespace) do inc(i);
  result:= copy(s, i, L);
end ;

{   TrimRight returns a copy of S with all blank characters on the right stripped off  }
function TrimRight(const S: string): string;
var l:integer;
begin
  l := length(s);
  while (l>0) and (s[l] in whitespace) do dec(l);
  result := copy(s,1,l);
end ;

{   QuotedStr returns S quoted left and right and every single quote in S
    replaced by two quotes   }
function QuotedStr(const S: string): string;
begin
  result := AnsiQuotedStr(s, '''');
end ;

{   AnsiQuotedStr returns S quoted left and right by Quote,
    and every single occurance of Quote replaced by two   }
function AnsiQuotedStr(const S: string; Quote: char): string;
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
function AnsiExtractQuotedStr(var  Src: PChar; Quote: Char): string;
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
 R:=@Result[1];
 while P <> Q do
   begin
     R^:=P^;
     inc(R);
     if (P^ = Quote) then
       begin
         P := P + 1;
         if (p^ <> Quote) then
          begin
            dec(R);
            break;
          end;
       end;
     P := P + 1;
   end ;
 src:=p;
 SetLength(result, (R-pchar(@Result[1])));
end ;


{   AdjustLineBreaks returns S with all CR characters not followed by LF
    replaced with CR/LF  }
//  under Linux all CR characters or CR/LF combinations should be replaced with LF

function AdjustLineBreaks(const S: string): string;
begin
  Result:=AdjustLineBreaks(S,DefaultTextLineBreakStyle);
end;

function AdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string;
var
  Source,Dest: PChar;
  DestLen: Integer;
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
               Inc(DestLen);
      #13: if (Style=tlbsCRLF) then
             if (I<L) and (S[i+1]=#10) then
               Inc(I)
             else
               Inc(DestLen)
             else if (I<L) and (S[I+1]=#10) then
               Dec(DestLen);
    end;
    Inc(I);
    end;
  if (DestLen=L) then
    Result:=S
  else
    begin
    SetLength(Result, DestLen);
    FillChar(Result[1],DestLen,0);
    Dest := Pointer(Result);
    J:=0;
    I:=0;
    while I<L do
      case Source[I] of
        #10: begin
             if Style=tlbsCRLF then
               begin
               Dest[j]:=#13;
               Inc(J);
              end;
             Dest[J] := #10;
             Inc(J);
             Inc(I);
             end;
        #13: begin
             if Style=tlbsCRLF then
               begin
               Dest[j] := #13;
               Inc(J);
               end;
             Dest[j]:=#10;
             Inc(J);
             Inc(I);
             if Source[I]=#10 then
               Inc(I);
             end;
      else
        Dest[j]:=Source[i];
        Inc(J);
        Inc(I);
      end;
    end;
end;


{   IsValidIdent returns true if the first character of Ident is in:
    'A' to 'Z', 'a' to 'z' or '_' and the following characters are
    on of: 'A' to 'Z', 'a' to 'z', '0'..'9' or '_'    }
function IsValidIdent(const Ident: string): boolean;
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


function strpos(str1,str2 : pchar) : pchar;
var
  p: pchar;
  lstr2 : SizeInt;
begin
  strpos:=nil;
  p:= strscan(str1,str2^);
  if p = nil then exit;
  lstr2:=strlen(str2);
  while p <> nil do
  begin
    if strlcomp(p, str2, lstr2) = 0 then
    begin
      strpos:= p;
      exit;
    end;
    inc(p);
    p:=strscan(p, str2^);
  end;
end;


type
   pbyte = ^byte;
   CharArray = array[0..0] of char;


{-- copy/pasted DIRECTLY from freepascal sources ------------------------------}
{ ansistrings! if using shortstrings use strings.pp unit included with fpc }
function StrPas(Str: PChar): string;
begin
  Result:=Str;
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
  Result:=nil;
  if (p=nil) or (p^=#0) then
    exit;
  len:=strlen(p)+1;
  Result:=StrAlloc(Len);
  if Result<>nil then
    strmove(Result,p,len);
end;


{-- copy/pasted DIRECTLY from freepascal sources ------------------------------}
{ ansistrings! if using shortstrings use strings.pp unit included with fpc }
function StrPCopy(Dest: PChar; Source: string): PChar;
begin
  result := StrMove(Dest, PChar(Source), length(Source)+1);
end ;

{-- copy/pasted DIRECTLY from freepascal sources ------------------------------}
{ ansistrings! if using shortstrings use strings.pp unit included with fpc }
function StrPLCopy(Dest: PChar; Source: string; MaxLen: SizeUInt): PChar;
var cnt: SizeUInt;
begin
  result := Dest;
  if (Result <> Nil) and (MaxLen <> 0) then
  begin
    cnt := Length(Source);
    if cnt > MaxLen then cnt := MaxLen;
    StrMove(Dest, PChar(Source), cnt);
    CharArray(result^)[cnt] := #0;  { terminate ! }
  end ;
end ;


{-- copy/pasted DIRECTLY from freepascal sources ------------------------------}
{ ansistrings! if using shortstrings use strings.pp unit included with fpc }
procedure StrDispose(Str: PChar);
begin
  if (Str <> Nil) then
  begin
    dec(Str,sizeof(cardinal));
    Freemem(str,cardinal(pointer(str)^));
  end;
end;

{-- copy/pasted DIRECTLY from freepascal sources ------------------------------}
{ ansistrings! if using shortstrings use strings.pp unit included with fpc }
function StrBufSize(Str: PChar): SizeUInt;
begin
  if Str <> Nil then
    result := SizeUInt(pointer(Str - SizeOf(SizeUInt))^)-sizeof(SizeUInt)
  else
    result := 0;
end ;

function WrapText(const Line, BreakStr: string; const BreakChars: TSysCharSet;  MaxCol: Integer): string;
const
  Quotes = ['''', '"'];
Var
  L : String;
  C,LQ,BC : Char;
  P,BLen,Len : Integer;
  HB,IBC : Boolean;
begin
  Result:='';
  L:=Line;
  Blen:=Length(BreakStr);
  If (BLen>0) then
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
      If (C=LQ) then
        LQ:=#0
      else If (C in Quotes) then
        LQ:=C;
      If (LQ<>#0) then
        Inc(P)
      else
        begin
        HB:=((C=BC) and (BreakStr=Copy(L,P,BLen)));
        If HB then
          Inc(P,Blen)
        else
          begin
          If (P>MaxCol) then
            IBC:=C in BreakChars;
          Inc(P);
          end;
        end;
//      Writeln('"',C,'" : IBC : ',IBC,' HB  : ',HB,' LQ  : ',LQ,' P>MaxCol : ',P>MaxCol);
      end;
    Result:=Result+Copy(L,1,P-1);
    If Not HB then
      Result:=Result+BreakStr;
    Delete(L,1,P-1);
    Len:=Length(L);
    end;
end;

function WrapText(const Line: string; MaxCol: Integer): string;
begin
  Result:=WrapText(Line,sLineBreak, [' ', '-', #9], MaxCol);
end;

function StringReplace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string;
var
  Srch,OldP,RemS: string; // Srch and Oldp can contain uppercase versions of S,OldPattern
  P : Integer;
begin
  Srch:=S;
  OldP:=OldPattern;
  if rfIgnoreCase in Flags then
    begin
    Srch:=UpperCase(Srch);
    OldP:=UpperCase(OldP);
    end;
  RemS:=S;
  Result:='';
  while (Length(Srch)<>0) do
    begin
    P:=Pos(OldP, Srch);
    if P=0 then
      begin
      Result:=Result+RemS;
      Srch:='';
      end
    else
      begin
      Result:=Result+Copy(RemS,1,P-1)+NewPattern;
      P:=P+Length(OldP);
      RemS:=Copy(RemS,P,Length(RemS)-P+1);
      if not (rfReplaceAll in Flags) then
        begin
        Result:=Result+RemS;
        Srch:='';
        end
      else
         Srch:=Copy(Srch,P,Length(Srch)-P+1);
      end;
    end;
end;

function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean;
begin
  Result:=False;
  If (Index>0) and (Index<=Length(S)) then
    Result:=Pos(S[Index],Delimiters)<>0; // Note we don't do MBCS yet
end;

{   LeftStr returns Count left-most characters from S }
function LeftStr(const S: string; Count: integer): string;
begin
  result := Copy(S, 1, Count);
end ;

{ RightStr returns Count right-most characters from S }
function RightStr(const S: string; Count: integer): string;
begin
   If Count>Length(S) then
     Count:=Length(S);
   result := Copy(S, 1 + Length(S) - Count, Count);
end;

{   NewStr creates a new PString and assigns S to it
    if length(s) = 0 NewStr returns Nil   }
function NewStr(const S: string): PString;
begin
  if (S='') then
   Result:=nil
  else
   begin
     new(result);
     if (Result<>nil) then
       Result^:=s;
   end;
end;

{   DisposeStr frees the memory occupied by S   }
procedure DisposeStr(S: PString);
begin
  if S <> Nil then
   begin
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
Var
  i : Integer;
  P : PChar;
begin
  Result := S;
  UniqueString(Result);
  P:=Pchar(Result);
  for i := 1 to Length(Result) do
    begin
    if (P^ in ['a'..'z']) then P^ := char(byte(p^) - 32);
      Inc(P);
    end;
end;

{   LowerCase returns a copy of S where all uppercase characters ( from A to Z )
    have been converted to lowercase  }
function Lowercase(Const S : String) : String;
Var
  i : Integer;
  P : PChar;
begin
  Result := S;
  UniqueString(Result);
  P:=Pchar(Result);
  for i := 1 to Length(Result) do
    begin
    if (P^ in ['A'..'Z']) then P^ := char(byte(p^) + 32);
      Inc(P);
    end;
end;


function CompareMemRange(P1, P2: Pointer; Length: cardinal): integer;
var i: cardinal;
begin
  i := 0;
  result := 0;
  while (result=0) and (I<length) do
    begin
    result:=byte(P1^)-byte(P2^);
    P1:=pchar(P1)+1;            // VP compat.
    P2:=pchar(P2)+1;
    i := i + 1;
   end ;
end ;

{   CompareStr compares S1 and S2, the result is the based on
    substraction of the ascii values of the characters in S1 and S2
    case     result
    S1 < S2  < 0
    S1 > S2  > 0
    S1 = S2  = 0     }
function CompareStr(const S1, S2: string): Integer;
var cnt, cnt1, cnt2: integer;
begin
  result := 0;
  cnt1 := Length(S1);
  cnt2 := Length(S2);
  if cnt1>cnt2 then cnt:= cnt2 else cnt:= cnt1;
  result := CompareMemRange(Pointer(S1),Pointer(S2), cnt);
  if result=0 then result:= cnt1-cnt2;
end;


function StrCopy(Dest, Source:PChar): PChar;
var cnt : SizeInt;
begin
 cnt := 0;
 while Source[cnt] <> #0 do
 begin
   Dest[cnt] := char(Source[cnt]);
   Inc(cnt);
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
   Inc(cnt);
 end;
 { terminate the string }
 Dest[cnt] := #0;
 StrECopy:=@(Dest[cnt]);
end;

function StrLCopy(Dest,Source: PChar; MaxLen: SizeInt): PChar;
var cnt: SizeInt;
begin
 cnt := 0;
 { To be compatible with BP, on a null string, put two nulls }
 If Source[0] = #0 then
 begin
   Dest[0]:=Source[0];
   Inc(cnt);
 end;
 while (Source[cnt] <> #0)  and (cnt < MaxLen) do begin
    Dest[cnt] := char(Source[cnt]);
    Inc(cnt);
 end;
 { terminate the string }
 Dest[cnt] := #0;
 StrLCopy := Dest;
end;

function StrEnd(P: PChar): PChar;
var cnt: SizeInt;
begin
  cnt := 0; while P[cnt] <> #0 do inc(cnt); StrEnd := @(P[cnt]);
end;

function StrComp(Str1, Str2 : PChar): SizeInt;
var cnt: SizeInt;
begin
 cnt:= 0;
 while str1[cnt] = str2[cnt] do begin
   if (str2[cnt] = #0) or (str1[cnt] = #0) then break;
   inc(cnt);
 end;
 StrComp := ord(str1[cnt]) - ord(str2[cnt]);
end;

function StrLComp(Str1, Str2 : PChar; L: SizeInt): SizeInt;
var cnt: SizeInt;
    c1, c2: char;
begin
  cnt := 0;
 if L = 0 then begin
   StrLComp := 0;
   exit;
 end;
 repeat
   c1 := str1[cnt];
   c2 := str2[cnt];
   if (c1 = #0) or (c2 = #0) then break;
   Inc(cnt);
 until (c1 <> c2) or (cnt >= L);
 StrLComp := ord(c1) - ord(c2);
end; 

function StrIComp(Str1, Str2 : PChar): SizeInt;
var
cnt: SizeInt;
c1, c2: char;
begin
  cnt := 0;
  c1 := upcase(str1[cnt]);
  c2 := upcase(str2[cnt]);
 while c1 = c2 do
 begin
   if (c1 = #0) or (c2 = #0) then break;
   Inc(cnt);
   c1 := upcase(str1[cnt]);
   c2 := upcase(str2[cnt]);
end;
 StrIComp := ord(c1) - ord(c2);
end; 

function StrLIComp(Str1, Str2 : PChar; L: SizeInt): SizeInt;
var
cnt: SizeInt;
c1, c2: char;
begin
  cnt := 0;
 if L = 0 then
 begin
   StrLIComp := 0;
   exit;
 end;
 repeat
   c1 := upcase(str1[cnt]);
   c2 := upcase(str2[cnt]);
   if (c1 = #0) or (c2 = #0) then break;
   Inc(cnt);
 until (c1 <> c2) or (cnt >= L);
 StrLIComp := ord(c1) - ord(c2);
end; 

function StrScan(P: PChar; C: Char): PChar;
 Var
   count: SizeInt;
begin

 count := 0;
 { As in Borland Pascal , if looking for NULL return null }
 if C = #0 then
 begin
   StrScan := @(P[StrLen(P)]);
   exit;
 end;
 { Find first matching character of Ch in Str }
 while P[count] <> #0 do
 begin
   if C = P[count] then
    begin
        StrScan := @(P[count]);
        exit;
    end;
   Inc(count);
 end;
 { nothing found. }
 StrScan := nil;
end; 

function StrRScan(P: PChar; C: Char): PChar;
Var
count: SizeInt;
index: SizeInt;
begin
 count := Strlen(P);
 { As in Borland Pascal , if looking for NULL return null }
 if C = #0 then
 begin
   StrRScan := @(P[count]);
   exit;
 end;
 Dec(count);
 for index := count downto 0 do
 begin
   if C = P[index] then
    begin
        StrRScan := @(P[index]);
        exit;
    end;
 end;
 { nothing found. }
 StrRScan := nil;
end; 

function StrUpper(P: PChar): PChar;
var
cnt: SizeInt;
begin
 cnt := 0;
 while (P[cnt] <> #0) do
 begin
   if P[cnt] in [#97..#122,#128..#255] then
      P[cnt] := Upcase(P[cnt]);
   Inc(cnt);
 end;
 StrUpper := P;
end; 

function StrLower(P: PChar): PChar;
var
cnt: SizeInt;
begin
 cnt := 0;
 while (P[cnt] <> #0) do
 begin
   if P[cnt] in [#65..#90] then
      P[cnt] := chr(ord(P[cnt]) + 32);
   Inc(cnt);
 end;
 StrLower := P;
end; 


end.
