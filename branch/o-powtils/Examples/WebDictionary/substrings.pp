{$IFDEF FPC}{$H+}{$MODE OBJFPC}
   {$IFDEF EXTRA_SECURE}
    {$R+}{$Q+}{$CHECKPOINTER ON}
   {$ENDIF}
{$ENDIF}
unit SubStrings;
{
 *******************************************************************************
 *                -== Pascal Server Pages unit - Substrings ==-                *
 *******************************************************************************
 * Some advanced code for work with AnsiStrings                                *
 *******************************************************************************
 * See the Pascal Server Pages Documentation for more information.             *
 *******************************************************************************
 *   Written by Vladimir Sibirov a.k.a. Trustmaster                            *
 *   http://www.psp.furtopia.org                                               *
 *   mailto:psp@furtopia.org                                                   *
 *******************************************************************************
 * Copyright (c) 2003-2005 by Pascal Server Pages development team.            *
 * See the Pascal Server Pages License for more information.                   *
 *******************************************************************************
 * [PSP 1.4.0 - 19.06.05 - Trustmaster]:                                       *
 * - memory optimization (exact types);                                        *
 * - string comparison functions: str_comp, str_icomp, str_ncomp, str_incomp,  *
 *   str_is_float, str_is_int, str_conv_float, str_conv_int.                   *
 * [PSP 1.4.0 - 27.05.05 - Trustmaster]:                                       *
 * - PSP2-comliant function names with str_ and substr_ prefixes;              *
 * - fixed bugs in substr_count and str_reverse;                               *
 * - added case insensitive variants of several functions (substr_iexists,     *
 *   substr_icount, substr_ireplace, substr_istrip, substr_isplit);            *
 * - added new functions (substr_pos, substr_ipos, substr_rpos, substr_irpos). *
 *******************************************************************************
}

{==============================================================================}
{================================== INTERFACE =================================}
{==============================================================================}

interface

{==============================================================================}
{=================================== Types ====================================}
{==============================================================================}

type StrArray = array of string;
// Just the array of string

{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}

function SubStr_exists(const str, sub: string): boolean;
// Finds if sub exists in str

function SubStr_iexists(const str, sub: string): boolean;
// Case insensitive SubStr_exists

function SubStr_Pos (const Str, Sub: string): LongInt;
// Returns position of first occurance of sub in str

function SubStr_ipos(const str, sub: string): longint;
// Case insensitive SubStr_pos

function SubStr_rpos(const str, sub: string): longint;
// Returns position of last occurance of sub in str

function SubStr_irpos(const str, sub: string): longint;
// Case insensitive SubStr_rpos

function SubStr_count(const str, sub: string): longint;
// Returns number of occurances of sub in str

function SubStr_icount(const str, sub: string): longint;
// Case insensitive SubStr_count

function SubStr_Replace(const str, sub, repl: string): string;
// Replaces all the sub SubStrings in str with repl SubStrings

function SubStr_ireplace(const str, sub, repl: string): string;
// Case insensitive SubStr_replace

function SubStr_Strip (const Str, Sub: string): string;
// Removes all occurances of sub in the string

function SubStr_istrip(const str, sub: string): string;
// Case insensitive SubStr_strip

function SubStr_split(const str, delimiter: string): StrArray;
// Splits str into array by string delimiter

function SubStr_isplit(const str, delimiter: string): StrArray;
// Case insensitive str_split

function str_comp(const str1, str2: string): shortint;
// String comparsion

function str_icomp(const str1, str2: string): shortint;
// String comparsion, case insensitive

function str_ncomp(const str1, str2: string): shortint;
// String comparsion, natural algoritm

function str_incomp(const str1, str2: string): shortint;
// String comparsion, natural algoritm, case insensitive

function str_conv_int(const str: string): longint;
// Converts a string into int if it is a real int

function str_conv_float(const str: string): double;
// Converts a string into float it is a real float

function str_is_int(const str: string): boolean;
// Whether it is a string representation of integer type

function str_is_float(const str: string): boolean;
// Whether it is a string representation of float type

function str_reverse(const str: string): string;
// Returns reversed string

function str_trim_left(const str: string): string;
// Removes whitespaces and tab chars from the beginning of the line

function str_trim_right(const str: string): string;
// Removes whitespaces and tab chars from the end of the line

function Str_Trim (const Str: string): string;
// Removes whitespaces and tab chars from beginning and end of the line

{==============================================================================}
{================================ IMPLEMENTAION ===============================}
{==============================================================================}

implementation

{==============================================================================}
{================================ Constants ===================================}
{==============================================================================}

const STR_CONST_DIGITS = '1234567890';
    STR_CONST_INT = '-1234567890';

{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}

{--[ SubStr_exists ]-----------------------------------------------------------}
// Finds if sub exists in str

function SubStr_exists(const str, sub: string): boolean;
begin
    if SubStr_pos(str, sub) > 0 then result := true
    else result := false;
end;

{------------------------------------------------------------------------------}

{--[ SubStr_iexists ]----------------------------------------------------------}
// Case insensitive SubStr_exists

function SubStr_iexists(const str, sub: string): boolean;
begin
    if SubStr_pos(lowercase(str), lowercase(sub)) > 0 then result := true
    else result := false;
end;

{------------------------------------------------------------------------------}

{--[ SubStr_pos ]--------------------------------------------------------------}
// Returns position of first occurance of sub in str
// I know, pos() exists, but this is done to see the logic

function SubStr_pos(const str, sub: string): longint;
var spos, len, sublen: longint;
begin
    result := 0;
    spos := 1;
    len := length(str);
    sublen := length(sub);
    while (spos + sublen - 1) <= len do
        if copy(str, spos, sublen) = sub then
            begin
                result := spos;
                break;
            end
        else inc(spos);
end;

{------------------------------------------------------------------------------}

{--[ SubStr_ipos ]-------------------------------------------------------------}
// Case insensitive SubStr_pos

function SubStr_ipos(const str, sub: string): longint;
begin
    result := SubStr_pos(lowercase(str), lowercase(sub));
end;

{------------------------------------------------------------------------------}

{--[ SubStr_rpos ]-------------------------------------------------------------}
// Returns position of last occurance of sub in str
// Ha-ha, you won't find it in system unit :)

function SubStr_rpos(const str, sub: string): longint;
var spos, len, sublen: longint;
begin
    result := 0;
    len := length(str);
    sublen := length(sub);
    spos := len - sublen + 1;
    while spos > 0 do
        if copy(str, spos, sublen) = sub then
            begin
                result := spos;
                break;
            end
        else dec(spos);
end;

{------------------------------------------------------------------------------}

{--[ SubStr_irpos ]------------------------------------------------------------}
// Case insensitive SubStr_rpos

function SubStr_irpos(const str, sub: string): longint;
begin
    result := SubStr_rpos(lowercase(str), lowercase(sub));
end;

{------------------------------------------------------------------------------}

{--[ SubStr_count ]------------------------------------------------------------}
// Returns number of occurances of sub in str

function SubStr_count(const str, sub: string): longint;
var temp: string;
    sublen: longint;
begin
    result := 0;
    temp := str;
    sublen := length(sub);
    while pos(sub, temp) > 0 do
        begin
            inc(result);
            delete(temp, pos(sub, temp), sublen);
        end;
end;

{------------------------------------------------------------------------------}

{--[ SubStr_icount ]-----------------------------------------------------------}
// Case insensitive SubStr_count

function SubStr_icount(const str, sub: string): longint;
begin
    result := SubStr_count(lowercase(str), lowercase(sub));
end;

{------------------------------------------------------------------------------}

{--[ SubStr_replace ]----------------------------------------------------------}
// Replaces all the sub SubStrings in str with repl SubStrings

function SubStr_Replace (const str, sub, repl: string): string;
var posn, sublen, len, replen: longint;
begin
    result := str;
    posn := 1;
    sublen := length(sub);
    replen := length(repl);
    repeat
        if copy(result, posn, sublen) = sub then
            begin
                delete(result, posn, sublen);
                insert(repl, result, posn);
                posn := posn + replen;
            end
        else inc(posn);
        len := length(result);
    until posn > len;
end;

{------------------------------------------------------------------------------}

{--[ SubStr_ireplace ]---------------------------------------------------------}
// Case insensitive SubStr_replace

function SubStr_ireplace(const str, sub, repl: string): string;
var posn, sublen, len, replen: longint;
    lsub: string;
begin
    result := str;
    posn := 1;
    sublen := length(sub);
    replen := length(repl);
    lsub := lowercase(sub);
    repeat
        if lowercase(copy(result, posn, sublen)) = lsub then
            begin
                delete(result, posn, sublen);
                insert(repl, result, posn);
                posn := posn + replen;
            end
        else inc(posn);
        len := length(result);
    until posn > len;
end;

{------------------------------------------------------------------------------}

{--[ SubStr_strip ]------------------------------------------------------------}
// Removes all occurances of sub in the string

function SubStr_strip(const str, sub: string): string;
var len: longint;
begin
    result := str;
    len := length(sub);
    while pos(sub, result) > 0 do delete(result, pos(sub, result), len);
end;

{------------------------------------------------------------------------------}

{--[ SubStr_istrip ]-----------------------------------------------------------}
// Case insensitive SubStr_strip

function SubStr_istrip(const str, sub: string): string;
var len, posn: longint;
begin
    result := str;
    len := length(sub);
    repeat
        posn := pos(lowercase(sub), lowercase(result));
        if posn > 0 then delete(result, posn, len);
    until posn <= 0;
end;

{------------------------------------------------------------------------------}

{--[ SubStr_split ]------------------------------------------------------------}
// Splits str into array by string delimiter

function SubStr_split(const str, delimiter: string): StrArray;
var temp: string;
    i, len: longint;
begin
    SetLength(result, 0);
    temp := str;
    len := length(delimiter);
    i := 1;
    // Splitting while delemiter presents in temp
    while pos(delimiter, temp) > 0 do
        begin
            i := pos(delimiter, temp);
            SetLength(result, length(result) + 1);
            result[length(result) - 1] := copy(temp, 1, i - 1);
            delete(temp, 1, (i - 1) + len);
        end;
    // Just copying the last part
    SetLength(result, length(result) + 1);
    result[length(result) - 1] := temp;
end;

{------------------------------------------------------------------------------}

{--[ SubStr_isplit ]-----------------------------------------------------------}
// Case insensitive str_split

function SubStr_isplit(const str, delimiter: string): StrArray;
var temp: string;
    i, len: longint;
begin
    SetLength(result, 0);
    temp := str;
    len := length(delimiter);
    i := 1;
    // Splitting while delemiter presents in temp
    while pos(lowercase(delimiter), lowercase(temp)) > 0 do
        begin
            i := pos(lowercase(delimiter), lowercase(temp));
            SetLength(result, length(result) + 1);
            result[length(result) - 1] := copy(temp, 1, i - 1);
            delete(temp, 1, (i - 1) + len);
        end;
    // Just copying the last part
    SetLength(result, length(result) + 1);
    result[length(result) - 1] := temp;
end;

{------------------------------------------------------------------------------}

{--[ str_comp ]----------------------------------------------------------------}
// String comparsion

function str_comp(const str1, str2: string): shortint;
var i, lim: longint;
begin
    result := 0;
    if length(str1) > length(str2) then lim := length(str1)
    else lim := length(str2);
    for i := 1 to lim do
        begin
            if ord(str1[i]) > ord(str2[i]) then
                begin
                    result := 1;
                    break;
                end;
            if ord(str1[i]) < ord(str2[i]) then
                begin
                    result := -1;
                    break;
                end;
        end;
    if (result = 0) and (length(str1) > length(str2)) then result := 1;
    if (result = 0) and (length(str1) < length(str2)) then result := -1;
end;

{------------------------------------------------------------------------------}

{--[ str_icomp ]---------------------------------------------------------------}
// String comparsion, case insensitive

function str_icomp(const str1, str2: string): shortint;
begin
    result := str_comp(lowercase(str1), lowercase(str2));
end;

{------------------------------------------------------------------------------}

{--[ str_ncomp ]---------------------------------------------------------------}
// String comparsion, natural algoritm 

function str_ncomp(const str1, str2: string): shortint;
var i, j, len1, len2, num1, num2: longint;
    lex1, lex2, lex3, lex4: string;
    sub: shortint;
begin
    result := 0;
    i := 1;
    j := 1;
    len1 := length(str1);
    len2 := length(str2);
    while (i <= len1) and (j <= len2) do
        begin
            if (pos(str1[i], STR_CONST_DIGITS) > 0) and (pos(str2[j], STR_CONST_DIGITS) > 0) then
                begin
                    // Natural number comparsion
                    lex1 := '';
                    while (i <= len1) and (pos(str1[i], STR_CONST_DIGITS) > 0) do
                        begin
                            SetLength(lex1, length(lex1) + 1);
                            lex1[length(lex1)] := str1[i];
                            inc(i);
                        end;
                    val(lex1, num1);
                    lex1 := '';
                    while (j <= len2) and (pos(str2[j], STR_CONST_DIGITS) > 0) do
                        begin
                            SetLength(lex1, length(lex1) + 1);
                            lex1[length(lex1)] := str2[j];
                            inc(j);
                        end;
                    val(lex1, num2);
                    if num1 > num2 then
                        begin
                            result := 1;
                            break;
                        end;
                    if num1 < num2 then
                        begin
                            result := -1;
                            break;
                        end;
                end
            else
                begin
                    // Alpha order comparsion
                    lex1 := '';
                    // Getting numeric part if exists
                    while (i <= len1) and (pos(str1[i], STR_CONST_DIGITS) > 0) do
                        begin
                            SetLength(lex1, length(lex1) + 1);
                            lex1[length(lex1)] := str1[i];
                            inc(i);
                        end;
                    lex3 := '';
                    // Then getting the string part
                    while (i <= len1) and (pos(str1[i], STR_CONST_DIGITS) <= 0) do
                        begin
                            SetLength(lex3, length(lex3) + 1);
                            lex3[length(lex3)] := str1[i];
                            inc(i);
                        end;
                    lex2 := '';
                    // Getting numeric part if exists
                    while (j <= len2) and (pos(str2[j], STR_CONST_DIGITS) > 0) do
                        begin
                            SetLength(lex2, length(lex2) + 1);
                            lex2[length(lex2)] := str2[j];
                            inc(j);
                        end;
                    lex4 := '';
                    // Then getting the string part
                    while (j <= len2) and (pos(str2[j], STR_CONST_DIGITS) <= 0) do
                        begin
                            SetLength(lex4, length(lex4) + 1);
                            lex4[length(lex4)] := str2[j];
                            inc(j);
                        end;
                    if lex3 = lex4 then
                        begin
                            // Natural hint
                            val(lex1, num1);
                            val(lex2, num2);
                            if num1 > num2 then
                                begin
                                    result := 1;
                                    break;
                                end;
                            if num1 < num2 then
                                begin
                                    result := -1;
                                    break;
                                end;
                            if num1 = num2 then
                                begin
                                    result := 0;
                                    continue;
                                end;
                        end;
                    sub := str_comp(lex1, lex2);
                    if sub > 0 then
                        begin
                            result := 1;
                            break;
                        end;
                    if sub < 0 then
                        begin
                            result := -1;
                            break;
                        end;
                end;
        end;
end;

{------------------------------------------------------------------------------}

{--[ str_incomp ]--------------------------------------------------------------}
// String comparsion, natural algoritm, case insensitive

function str_incomp(const str1, str2: string): shortint;
begin
    result := str_ncomp(lowercase(str1), lowercase(str2));
end;

{------------------------------------------------------------------------------}

{--[ str_conv_int ]------------------------------------------------------------}
// Converts a string into int if it is a real int

function str_conv_int(const str: string): longint;
begin
    result := 0;
    if str_is_int(str) then val(str, result);
end;

{-----------------------------------------------------------------------------}

{--[ str_conv_float ]---------------------------------------------------------}
// Converts a string into float it is a real float

function str_conv_float(const str: string): double;
begin
    result := 0.0;
    if str_is_float(str) then val(str, result);
end;

{------------------------------------------------------------------------------}

{--[ str_is_int ]--------------------------------------------------------------}
// Whether it is a string representation of integer type

function str_is_int(const str: string): boolean;
var i: longint;
begin
    result := true;
    for i := 1 to length(str) do
        if pos(str[i], STR_CONST_INT) = 0 then
            begin
                result := false;
                break;
            end;
end;

{------------------------------------------------------------------------------}

{--[ str_is_float ]------------------------------------------------------------}
// Whether it is a string representation of float type

function str_is_float(const str: string): boolean;
var i, p: longint;
    tmp: string;
begin
    p := pos('.', str);
    if p <= 0 then exit(false);
    result := true;
    tmp := copy(str, 1, p - 1);
    for i := 1 to length(tmp) do
        if pos(tmp[i], STR_CONST_DIGITS) <= 0 then
            begin
                result := false;
                break;
            end;
    tmp := copy(str, p + 1, length(str) - p);
    for i := 1 to length(tmp) do
        if pos(tmp[i], STR_CONST_DIGITS) <= 0 then
            begin
                result := false;
                break;
            end;
end;

{------------------------------------------------------------------------------}

{--[ str_reverse ]-------------------------------------------------------------}
// Returns reversed string

function str_reverse(const str: string): string;
var i, len: longint;
begin
    len := length(str);
    SetLength(result, len);
    for i := 1 to len do result[i] := str[len - i + 1];
end;

{------------------------------------------------------------------------------}

{--[ str_trim_left ]-----------------------------------------------------------}
// Removes whitespaces and tab chars from the beginning of the line

function str_trim_left(const str: string): string;
var i: longint;
begin
    i := 0;
    while (str[i + 1] = #32) or (str[i + 1] = #9) do inc(i);
    result := copy(str, i + 1, length(str) - i);
end;

{------------------------------------------------------------------------------}

{--[ str_trim_right ]----------------------------------------------------------}
// Removes whitespaces and tab chars from the end of the line

function str_trim_right(const str: string): string;
var i, len: longint;
begin
    len := length(str);
    i := 0;
    while (str[len - i] = #32) or (str[len - i] = #9) do inc(i);
    result := copy(str, 1, len - i);
end;

{------------------------------------------------------------------------------}

{--[ str_trim ]----------------------------------------------------------------}
// Removes whitespaces and tab chars from beginning and end of the line

function str_trim(const str: string): string;
begin
    result := str_trim_right(str_trim_left(str));
end;

{==============================================================================}
{===================================== END ====================================}
{==============================================================================}

end.
