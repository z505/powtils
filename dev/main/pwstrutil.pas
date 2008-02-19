{
  Reason for unit: smartlinking better, standalone functions in separate units
  Authors/Credits: taken from FPC RTL. 
  License: Freepascal RTL Modified GPL
}
unit pwstrutil;

{$IFDEF FPC}{$MODE OBJFPC}{$H+}
  {$IFDEF EXTRA_SECURE}{$R+}{$Q+}{$CHECKPOINTER ON}{$ENDIF}
{$ENDIF}

interface

type 
  TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);

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

Type
  TSysCharSet = Set of char;


function WrapText(const Line, BreakStr: string; const BreakChars: TSysCharSet;  MaxCol: Integer): string;
function WrapText(const Line: string; MaxCol: Integer): string;


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
function AdjustLineBreaks(const S: string): string;
function AdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string;
function IsValidIdent(const Ident: string): boolean;


implementation


{   Trim returns a copy of S with blanks characters on the left and right stripped off   }

Const WhiteSpace = [' ',#10,#13,#9];

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
  l := length(s);
  i := 1;
  while (i<=l) and (s[i] in whitespace) do inc(i);
  Result := copy(s, i, l);
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
var i, j, count: integer;
begin
result := '' + Quote;
count := length(s);
i := 0;
j := 0;
while i < count do begin
   i := i + 1;
   if S[i] = Quote then begin
      result := result + copy(S, 1 + j, i - j) + Quote;
      j := i;
      end ;
   end ;
if i <> j then
   result := result + copy(S, 1 + j, i - j);
result := result + Quote;
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
    While I<L do
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
var Count: SizeUInt;
begin
  result := Dest;
  if (Result <> Nil) and (MaxLen <> 0) then
  begin
    Count := Length(Source);
    if Count > MaxLen then
      Count := MaxLen;
    StrMove(Dest, PChar(Source), Count);
    CharArray(result^)[Count] := #0;  { terminate ! }
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
  While (Len>0) do
    begin
    P:=1;
    LQ:=#0;
    HB:=False;
    IBC:=False;
    While ((P<=Len) and ((P<=MaxCol) or not IBC)) and ((LQ<>#0) or Not HB) do
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


Function StringReplace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string;
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

Function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean;
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


Function UpperCase(Const S : String) : String;
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

Function Lowercase(Const S : String) : String;
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
var count, count1, count2: integer;
begin
  result := 0;
  Count1 := Length(S1);
  Count2 := Length(S2);
  if Count1>Count2 then
    Count:=Count2
  else
    Count:=Count1;
  result := CompareMemRange(Pointer(S1),Pointer(S2), Count);
  if result=0 then
    result:=Count1-Count2;
end;


function StrCopy(Dest, Source:PChar): PChar;
var
 counter : SizeInt;
Begin
 counter := 0;
 while Source[counter] <> #0 do
 begin
   Dest[counter] := char(Source[counter]);
   Inc(counter);
 end;
 { terminate the string }
 Dest[counter] := #0;
 StrCopy := Dest;
end; 

function StrECopy(Dest, Source: PChar): PChar;
{ Equivalent to the following:                                          }
{  strcopy(Dest,Source);                                                }
{  StrECopy := StrEnd(Dest);                                            }
var
 counter : SizeInt;
Begin
 counter := 0;
 while Source[counter] <> #0 do
 begin
   Dest[counter] := char(Source[counter]);
   Inc(counter);
 end;
 { terminate the string }
 Dest[counter] := #0;
 StrECopy:=@(Dest[counter]);
end; 

function StrLCopy(Dest,Source: PChar; MaxLen: SizeInt): PChar;
var
 counter: SizeInt;
Begin
 counter := 0;
 { To be compatible with BP, on a null string, put two nulls }
 If Source[0] = #0 then
 Begin
   Dest[0]:=Source[0];
   Inc(counter);
 end;
 while (Source[counter] <> #0)  and (counter < MaxLen) do
 Begin
    Dest[counter] := char(Source[counter]);
    Inc(counter);
 end;
 { terminate the string }
 Dest[counter] := #0;
 StrLCopy := Dest;
end; 

Function StrEnd(P: PChar): PChar;
var
counter: SizeInt;
begin
 counter := 0;
 while P[counter] <> #0 do
    Inc(counter);
 StrEnd := @(P[Counter]);
end; 

function StrComp(Str1, Str2 : PChar): SizeInt;
var
counter: SizeInt;
Begin
  counter := 0;
 While str1[counter] = str2[counter] do
 Begin
   if (str2[counter] = #0) or (str1[counter] = #0) then
      break;
   Inc(counter);
 end;
 StrComp := ord(str1[counter]) - ord(str2[counter]);
end; 

function StrLComp(Str1, Str2 : PChar; L: SizeInt): SizeInt;
var
counter: SizeInt;
c1, c2: char;
Begin
  counter := 0;
 if L = 0 then
 begin
   StrLComp := 0;
   exit;
 end;
 Repeat
   c1 := str1[counter];
   c2 := str2[counter];
   if (c1 = #0) or (c2 = #0) then break;
   Inc(counter);
Until (c1 <> c2) or (counter >= L);
 StrLComp := ord(c1) - ord(c2);
end; 

function StrIComp(Str1, Str2 : PChar): SizeInt;
var
counter: SizeInt;
c1, c2: char;
Begin
  counter := 0;
  c1 := upcase(str1[counter]);
  c2 := upcase(str2[counter]);
 While c1 = c2 do
 Begin
   if (c1 = #0) or (c2 = #0) then break;
   Inc(counter);
   c1 := upcase(str1[counter]);
   c2 := upcase(str2[counter]);
end;
 StrIComp := ord(c1) - ord(c2);
end; 

function StrLIComp(Str1, Str2 : PChar; L: SizeInt): SizeInt;
var
counter: SizeInt;
c1, c2: char;
Begin
  counter := 0;
 if L = 0 then
 begin
   StrLIComp := 0;
   exit;
 end;
 Repeat
   c1 := upcase(str1[counter]);
   c2 := upcase(str2[counter]);
   if (c1 = #0) or (c2 = #0) then break;
   Inc(counter);
Until (c1 <> c2) or (counter >= L);
 StrLIComp := ord(c1) - ord(c2);
end; 

function StrScan(P: PChar; C: Char): PChar;
 Var
   count: SizeInt;
Begin

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
Begin
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
counter: SizeInt;
begin
 counter := 0;
 while (P[counter] <> #0) do
 begin
   if P[Counter] in [#97..#122,#128..#255] then
      P[counter] := Upcase(P[counter]);
   Inc(counter);
 end;
 StrUpper := P;
end; 

function StrLower(P: PChar): PChar;
var
counter: SizeInt;
begin
 counter := 0;
 while (P[counter] <> #0) do
 begin
   if P[counter] in [#65..#90] then
      P[Counter] := chr(ord(P[Counter]) + 32);
   Inc(counter);
 end;
 StrLower := P;
end; 


end.
