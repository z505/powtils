{$IFDEF FPC}{$H+}{$MODE OBJFPC}
    {$IFDEF EXTRA_SECURE}
     {$R+}{$Q+}{$CHECKPOINTER ON}
    {$ENDIF}
{$ENDIF}
    
unit FileShareUnit;
{
 *******************************************************************************
 *                -== Pascal Server Pages Unit - FileShare ==-                 *
 *******************************************************************************
 * Some functions for safe file sharing.                                       *           
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
 * [PSP 1.4.0 - 15.08.05 - Trustmaster]:                                       *
 * - changed locking system, so .lfw flag is created on writing and unique     *
 * flags .lfrNNNNN are created for each reading thread. But it still doesn't   *
 * give 100% I/O error protection.                                             *
 * [PSP 1.4.0 - 27.05.05 - Trustmaster]:                                       *
 * - PSP2-compliancy changes (function names);                                 *
 * - file_error function added;                                                *
 * - some small fixes with returned results.                                   *
 *******************************************************************************
}

{==============================================================================}
{================================== INTERFACE =================================}
{==============================================================================}

interface

{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}
 
function file_exists(const fname: string): boolean;
// Checks if file exists

function file_error: string;
// Returns last I/O error message

function file_mark_read(const fname: string; var key: word): boolean;
// Creates file reading flag

function file_mark_write(const fname: string): boolean;
// Creates file writing flag

function file_unmark_read(const fname: string; key: word): boolean;
// Removes file reading flag

function file_unmark_write(const fname: string): boolean;
// Removes file writing flag

{==============================================================================}
{================================ IMPLEMENTAION ===============================}
{==============================================================================}

implementation
uses sysutils;

{==============================================================================}
{================================ Constants ===================================}
{==============================================================================}

const GC_TIMEOUT = 3.0; // If flag is older than 3 minutes - it is garbage left after error

{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}

{$IFDEF WIN32}
procedure sleep(dwMilliseconds:DWORD); stdcall; external 'kernel32' name 'Sleep';
{$ENDIF}
{$IFDEF UNIX}
function sleep(seconds: longint): longint; cdecl; external 'c' name 'sleep';
{$ENDIF}

{--[ file_exists ]-------------------------------------------------------------}
// Checks if file exists

function file_exists(const fname: string): boolean;
var fh: file of byte;
begin
    assign(fh, fname);
    {$i-}
    reset(fh);
    {$i+}
    if ioresult = 0 then
        begin
            result := true;
            close(fh);
        end
    else result := false;
end;

{------------------------------------------------------------------------------}

{--[ file_error ]--------------------------------------------------------------}
// Returns last I/O error message

function file_error: string;
begin
    case ioresult of
        2: result := 'File not found';
        3: result := 'Path not found';
        4: result := 'Too many open files';
        5: result := 'Access denied';
        6: result := 'Invalid file handle';
        12: result := 'Invalid file-access mode';
        15: result := 'Invalid disk number';
        16: result := 'Cannot remove current directory';
        17: result := 'Cannot rename across volumes';
        100: result := 'Error when reading from disk';
        101: result := 'Error when writing to disk';
        102: result := 'File not assigned';
        103: result := 'File not open';
        104: result := 'File not opened for input';
        105: result := 'File not opened for output';
        106: result := 'Invalid number';
        150: result := 'Disk is write protected';
        151: result := 'Unknown device';
        152: result := 'Drive not ready';
        153: result := 'Unknown command';
        154: result := 'CRC check failed';
        155: result := 'Invalid drive specified';
        156: result := 'Seek error on disk';
        157: result := 'Invalid media type';
        158: result := 'Sector not found';
        159: result := 'Printer out of paper';
        160: result := 'Error when writing to device';
        161: result := 'Error when reading from device';
        162: result := 'Hardware failure';
    else
        // Emtpy line - OK
        result := '';
    end;
end;

{------------------------------------------------------------------------------}

{--[ file_suspend_read ]-------------------------------------------------------}
// Suspends the execution until writing operations are done

function file_suspend_read(const fname: string): boolean;
begin
    // Garbage collection
    if file_exists(fname + '.lfw') and ((abs(Now - FileDateToDateTime(FileAge(fname + '.lfw'))) * 1440) >= GC_TIMEOUT) then DeleteFile(fname + '.lfw');
    // Suspending
    while file_exists(fname + '.lfw') do
        begin
            {$IFDEF WIN32}
            sleep(10);
            {$ENDIF}
            {$IFDEF UNIX}
            sleep(1);
            {$ENDIF}
        end;
    result := true;
end;

{------------------------------------------------------------------------------}

{--[ file_suspend_write ]------------------------------------------------------}
// Suspends the execution until the file is unlocked

function file_suspend_write(const fname: string): boolean;
var sr: TSearchRec;
begin
    // First waiting for writing operations finish
    result := file_suspend_read(fname);
    while FindFirst(fname + '.lfr*', faAnyFile, sr) = 0 do
        begin
            // Garbage collection
            repeat
                if (abs(Now - FileDateToDateTime(sr.Time)) * 1440) >= GC_TIMEOUT then
                    begin
                        if pos('\', fname) > 0 then
                            DeleteFile(ExtractFilePath(fname) + '\' + sr.Name)
                        else if pos('/', fname) > 0 then
                            DeleteFile(ExtractFilePath(fname) + '/' + sr.Name)
                        else
                            DeleteFile(sr.Name);
                    end;
            until FindNext(sr) <> 0;
            FindClose(sr);
            {$IFDEF WIN32}
            sleep(10);
            {$ENDIF}
            {$IFDEF UNIX}
            sleep(1);
            {$ENDIF}
        end;
    FindClose(sr);
    result := true;
end;

{------------------------------------------------------------------------------}

{--[ file_mark_read ]----------------------------------------------------------}
// Creates unique file reading flag for this thread
 
function file_mark_read(const fname: string; var key: word): boolean;
var fh: file of byte;
    lex: string;
begin
    lex := '';
    file_suspend_read(fname);
    if not file_exists(fname) then exit(false);
    repeat
        randomize;
        key := word(random(65534));
        str(key, lex);
    until not file_exists(fname + '.lfr' + lex);
    assign(fh, fname + '.lfr' + lex);
    rewrite(fh);
    close(fh);
    if ioresult = 0 then result := true else result := false;
end;

{------------------------------------------------------------------------------}

{--[ file_mark_write ]---------------------------------------------------------}
// Creates file writing flag
 
function file_mark_write(const fname: string): boolean;
var fh: file of byte;
begin
    file_suspend_write(fname);
    if file_exists(fname) and (not file_exists(fname + '.lfw')) then
        begin
            assign(fh, fname + '.lfw');
            rewrite(fh);
            close(fh);
            result := true;
        end
    else result := false;
end;

{------------------------------------------------------------------------------}

{--[ file_unmark_read ]--------------------------------------------------------}
// Removes unique file reading flag

function file_unmark_read(const fname: string; key: word): boolean;
var fh: file of byte;
    lex: string;
begin
    lex := '';
    if not file_exists(fname) then exit(false);
    str(key, lex);
    if not file_exists(fname + '.lfr' + lex) then exit(false);
    assign(fh, fname + '.lfr' + lex);
    erase(fh);
    if ioresult = 0 then result := true else result := false;
end;

{------------------------------------------------------------------------------}

{--[ file_unmark_write ]-------------------------------------------------------}
// Removes file writing flag

function file_unmark_write(const fname: string): boolean;
var fh: file of byte;
begin
    if file_exists(fname + '.lfw') then
        begin
            assign(fh, fname + '.lfw');
            erase(fh);
            if ioresult = 0 then result := true;
        end
    else result := false;
end;

{------------------------------------------------------------------------------}
 
{==============================================================================}
{===================================== END ====================================}
{==============================================================================}
 
end.
