{$IFDEF FPC}{$MODE OBJFPC}{$H+}{$R+}{$Q+}{$CHECKPOINTER ON}{$ENDIF}
unit pwregexp;
{*******************************************************************************

                           PSP/PWU Regular Expressions

********************************************************************************

--------------------------------------------------------------------------------
  Perl Compatible Regular Expressions unit, Pascal implementation.
 Simplified functional oriented API for TRegExpr class written by Andrey V.
 Sorokin.
--------------------------------------------------------------------------------
  Copyright (c) 2003-2006 by Pascal Server Pages development team.
  See the Pascal Server Pages License for more information.

  Partial Copyright (c) 1999-2004 Andrey V. Sorokin.
--------------------------------------------------------------------------------
  Authors/Credits:
--------------------------------------------------------------------------------
  Trustmaster (Vladimir Sibirov), Anso (Andrey Sorokin)

********************************************************************************}

{==============================================================================}
{================================== INTERFACE =================================}
{==============================================================================}

interface

uses pwregexp_tregexpr, pwsubstr;

{==============================================================================}
{================================   Types   ===================================}
{==============================================================================}

type PRegexpEntry = Pointer;
    // RegExp entry struct pointer

    PRegexpResult = Pointer;
    // Global match result pointer

    TStrArray = array of string;

{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}

function RegExpCheck(const pattern, str: string): boolean;
// Checks if str matches RegExp pattern

function RegExpCount(const pattern, str: string): longword;
// Returns number of full-mask RegExp entries in str

function RegExpCountAll(rr: PRegexpResult): longword;
// Returns number of entries dumped by RegExpMatchAll

function RegExpEntry(rr: PRegexpResult; index: longword): PRegexpEntry;
// Fetches single global match result entry

function RegExpEntryCount(re: PRegexpEntry): longint;
// Returns number of submasks in the entry

function RegExpEntryItem(re: PRegexpEntry; index: longword): string;
// Returns RegExp entry item (submask)

function RegExpEntryLength(re: PRegexpEntry; index: longword): longword;
// Returns length of RegExp entry item (submask)

function RegExpEntryPos(re: PRegexpEntry; index: longword): longword;
// Returns position of RegExp entry item (submask) in source string

function RegExpError(re: PRegexpEntry): string;
// Returns error message caused by RegExpMatch

function RegExpErrorAll(rr: PRegexpResult): string;
// Returns error message caused by RegExpMatchAll

procedure RegExpFree(re: PRegexpEntry);
// Frees memory occupied by RegExp entry

procedure RegExpFreeAll(rr: PRegexpResult);
// Frees memory occupied by RegExp global match result

function RegExpMatch(const pattern, str: string; var matches: PRegexpEntry): boolean;
// Custom regular expression match

function RegExpMatchAll(const pattern, str: string; var matches: PRegexpResult): longword;
// Performs global regular expression match, number of full-mask entries is returned

function RegExpReplace(const pattern, replacement, str: string): string;
// Replaces data in string by RegExp pattern.

procedure RegExpSplit(const pattern, str: string; var pieces: TStrArray);
// Splits a string into array by delimiter defined by RegExp pattern.


{==============================================================================}
{================================ IMPLEMENTAION ===============================}
{==============================================================================}

implementation

{==============================================================================}
{================================   Types   ===================================}
{==============================================================================}

type SizeArray = array of longword;
    // Dynamic array of longword.

    TRegexpEntry = record
    // Struct that implements single regular expression entry
        count: longint; // Number of subentries
        items: StrArray; // Array containing all the entries
        posns: SizeArray; // Array containing positions for each subentry
        lents: SizeArray; // Array containing lengths for each subentry
        error: string; // Last error message or empty string
    end;

    PTRegexpEntry = ^TRegexpEntry;
    // Pointer to RegexpEntry

    TRegexpResult = record
    // Global match result
        count: longword; // Number of entries
        entries: array of PTRegexpEntry; // Entries
        error: string; // Error message
    end;

    PTRegexpResult = ^TRegexpResult;
    // Pointer to RegexpResult

{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}

{--[ RegExpCheck ]------------------------------------------------------------}
// Checks if str matches RegExp pattern

function RegExpCheck(const pattern, str: string): boolean;
var rh: TRegExpr;
    lim_rpos: longword;
    pat_lim: string;
begin
    // Init
    result := false;
    // Determine pattern boundaries
    pat_lim := copy(pattern, 1, 1);
    lim_rpos := substrrpos(pattern, pat_lim);
    // Building
    rh := TRegExpr.Create;
    // Split pattern and modifiers
    rh.Expression := copy(pattern, 2, lim_rpos - 2);
    rh.ModifierStr := copy(pattern, lim_rpos + 1, longword(length(pattern)) - lim_rpos);
    // Execution
    result := rh.Exec(str);
    // Finalization
    rh.Free;
end;

{------------------------------------------------------------------------------}

{--[ RegExpCount ]------------------------------------------------------------}
// Returns number of full-mask RegExp entries in str

function RegExpCount(const pattern, str: string): longword;
var rh: TRegExpr;
    lim_rpos: longword;
    pat_lim: string;
begin
    // Init
    result := 0;
    // Determine pattern boundaries
    pat_lim := copy(pattern, 1, 1);
    lim_rpos := substrrpos(pattern, pat_lim);
    // Building
    rh := TRegExpr.Create;
    // Split pattern and modifiers
    rh.Expression := copy(pattern, 2, lim_rpos - 2);
    rh.ModifierStr := copy(pattern, lim_rpos + 1, longword(length(pattern)) - lim_rpos);
    // Execution
    if rh.Exec(str) then
        repeat
            inc(result);
        until not rh.ExecNext;
    // Finalization
    rh.Free;
end;

{------------------------------------------------------------------------------}

{--[ RegExpCountAll ]--------------------------------------------------------}
// Returns number of entries dumped by RegExpMatchAll

function RegExpCountAll(rr: PRegexpResult): longword;
var rh: PTRegexpResult;
begin
    rh := PTRegexpResult(rr);
    result := rh^.count;
end;

{------------------------------------------------------------------------------}


{--[ RegExpEntry ]------------------------------------------------------------}
// Fetches global match result entry

function RegExpEntry(rr: PRegexpResult; index: longword): PRegexpEntry;
var rh: PTRegexpResult;
begin
    rh := PTRegexpResult(rr);
    result := PRegexpEntry(rh^.entries[index]);
end;

{------------------------------------------------------------------------------}

{--[ RegExpEntryCount ]------------------------------------------------------}
// Returns number of submasks in the entry

function RegExpEntryCount(re: PRegexpEntry): longint;
var mh: PTRegexpEntry;
begin
    mh := PTRegexpEntry(re);
    result := mh^.count;
end;

{------------------------------------------------------------------------------}

{--[ RegExpEntryItem ]-------------------------------------------------------}
// Returns RegExp entry item (submask)

function RegExpEntryItem(re: PRegexpEntry; index: longword): string;
var mh: PTRegexpEntry;
begin
    mh := PTRegexpEntry(re);
    result := mh^.items[index];
end;

{------------------------------------------------------------------------------}

{--[ RegExpEntryLength ]-----------------------------------------------------}
// Returns length of RegExp entry item (submask)

function RegExpEntryLength(re: PRegexpEntry; index: longword): longword;
var mh: PTRegexpEntry;
begin
    mh := PTRegexpEntry(re);
    result := mh^.lents[index];
end;

{------------------------------------------------------------------------------}

{--[ RegExpEntryPos ]--------------------------------------------------------}
// Returns position of RegExp entry item (submask) in source string

function RegExpEntryPos(re: PRegexpEntry; index: longword): longword;
var mh: PTRegexpEntry;
begin
    mh := PTRegexpEntry(re);
    result := mh^.posns[index];
end;

{------------------------------------------------------------------------------}

{--[ RegExpError ]------------------------------------------------------------}
// Returns error message caused by RegExpMatch

function RegExpError(re: PRegexpEntry): string;
var mh: PTRegexpEntry;
begin
    mh := PTRegexpEntry(re);
    result := mh^.error;
end;

{------------------------------------------------------------------------------}

{--[ RegExpErrorAll ]--------------------------------------------------------}
// Returns error message caused by RegExpMatchAll

function RegExpErrorAll(rr: PRegexpResult): string;
var rh: PTRegexpResult;
begin
    rh := PTRegexpResult(rr);
    result := rh^.error;
end;

{------------------------------------------------------------------------------}

{--[ RegExpFree ]-------------------------------------------------------------}
// Frees memory occupied by RegExp entry

procedure RegExpFree(re: PRegexpEntry);
var mh: PTRegexpEntry;
begin
    mh := PTRegexpEntry(re);
    dispose(mh);
end;

{------------------------------------------------------------------------------}

{--[ RegExpFreeAll ]---------------------------------------------------------}
// Frees memory occupied by RegExp global match result

procedure RegExpFreeAll(rr: PRegexpResult);
var rh: PTRegexpResult;
    i: longword;
begin
    rh := PTRegexpResult(rr);
    if rh^.count > 0 then for i := 0 to rh^.count - 1 do dispose(rh^.entries[i]);
    dispose(rh);
end;

{------------------------------------------------------------------------------}

{--[ RegExpMatch ]------------------------------------------------------------}
// Custom regular expression match

function RegExpMatch(const pattern, str: string; var matches: PRegexpEntry): boolean;
var rh: TRegExpr;
    pat_lim: string;
    lim_rpos, i: longword;
    mh: PTRegexpEntry;
begin
    // Init
    result := false;
    new(mh);
    // Determine pattern boundaries
    pat_lim := copy(pattern, 1, 1);
    lim_rpos := substrrpos(pattern, pat_lim);
    // Building
    rh := TRegExpr.Create;
    // Split pattern and modifiers
    rh.Expression := copy(pattern, 2, lim_rpos - 2);
    rh.ModifierStr := copy(pattern, lim_rpos + 1, longword(length(pattern)) - lim_rpos);
    // Execution
    result := rh.Exec(str);
    // Dumping match
    if result then
        begin
            SetLength(mh^.items, rh.SubExprMatchCount + 1);
            SetLength(mh^.posns, rh.SubExprMatchCount + 1);
            SetLength(mh^.lents, rh.SubExprMatchCount + 1);
            mh^.count := rh.SubExprMatchCount;
            for i := 0 to mh^.count do
                begin
                    mh^.items[i] := rh.Match[i];
                    mh^.posns[i] := rh.MatchPos[i];
                    mh^.lents[i] := rh.MatchLen[i];
                end;
        end
    else
        begin
            mh^.count := -1;
            mh^.error := rh.ErrorMsg(rh.LastError);
        end;
    // Finalization
    rh.Free;
    // Dump result
    matches := PRegexpEntry(mh);
end;

{------------------------------------------------------------------------------}

{--[ RegExpMatchAll ]--------------------------------------------------------}
// Performs global regular expression match, number of full-mask entries is returned

function RegExpMatchAll(const pattern, str: string; var matches: PRegexpResult): longword;
var rh: TRegExpr;
    rp: PTRegexpResult;
    pat_lim: string;
    lim_rpos, i: longword;
begin
    // Initialization
    result := 0;
    new(rp);
    rp^.count := 0;
    // Determine pattern boundaries
    pat_lim := copy(pattern, 1, 1);
    lim_rpos := substrrpos(pattern, pat_lim);
    // Building
    rh := TRegExpr.Create;
    // Split pattern and modifiers
    rh.Expression := copy(pattern, 2, lim_rpos - 2);
    rh.ModifierStr := copy(pattern, lim_rpos + 1, longword(length(pattern)) - lim_rpos);
    // Execution
    if rh.Exec(str) then
        repeat
            inc(result);
            inc(rp^.count);
            SetLength(rp^.entries, rp^.count);
            new(rp^.entries[rp^.count - 1]);
            SetLength(rp^.entries[rp^.count - 1]^.items, rh.SubExprMatchCount + 1);
            SetLength(rp^.entries[rp^.count - 1]^.posns, rh.SubExprMatchCount + 1);
            SetLength(rp^.entries[rp^.count - 1]^.lents, rh.SubExprMatchCount + 1);
            rp^.entries[rp^.count - 1]^.count := rh.SubExprMatchCount;
            for i := 0 to rp^.entries[rp^.count - 1]^.count do
                begin
                    rp^.entries[rp^.count - 1]^.items[i] := rh.Match[i];
                    rp^.entries[rp^.count - 1]^.posns[i] := rh.MatchPos[i];
                    rp^.entries[rp^.count - 1]^.lents[i] := rh.MatchLen[i];
                end;
        until not rh.ExecNext
    else rp^.error := rh.ErrorMsg(rh.LastError);
    // Finalization
    rh.Free;
    // Dump result
    matches := PRegexpResult(rp);
end;

{------------------------------------------------------------------------------}

{--[ RegExpReplace ]----------------------------------------------------------}
// Replaces data in string by RegExp pattern

function RegExpReplace(const pattern, replacement, str: string): string;
var rh: TRegExpr;
    pat_lim: string;
    lim_rpos: longword;
begin
    // Initialization
    result := str;
    // Determine pattern boundaries
    pat_lim := copy(pattern, 1, 1);
    lim_rpos := substrrpos(pattern, pat_lim);
    // Building
    rh := TRegExpr.Create;
    // Split pattern and modifiers
    rh.Expression := copy(pattern, 2, lim_rpos - 2);
    rh.ModifierStr := copy(pattern, lim_rpos + 1, longword(length(pattern)) - lim_rpos);
    // Execution
    result := rh.Replace(str, replacement, true);
    // Finalization
    rh.Free;
end;

{------------------------------------------------------------------------------}

{--[ RegExpSplit ]------------------------------------------------------------}
// Splits a string into array by delimiter defined by RegExp pattern

procedure RegExpSplit(const pattern, str: string; var pieces: TStrArray);
var rh: TRegExpr;
    pat_lim: string;
    lim_rpos: longword;
begin
    // Determine pattern boundaries
    pat_lim := copy(pattern, 1, 1);
    lim_rpos := substrrpos(pattern, pat_lim);
    // Building
    rh := TRegExpr.Create;
    // Split pattern and modifiers
    rh.Expression := copy(pattern, 2, lim_rpos - 2);
    rh.ModifierStr := copy(pattern, lim_rpos + 1, longword(length(pattern)) - lim_rpos);
    // Execution
    rh.Split(str, pieces);
    // Finalization
    rh.Free;
end;

{------------------------------------------------------------------------------}


{==============================================================================}
{===================================== END ====================================}
{==============================================================================}

end.
