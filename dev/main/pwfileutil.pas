{ File utilities. Produces compact binaries. No sysutils/classes bloat.
  Some functions are based on StrWrap1 unit which Lars and Jeff wrote.

  Authors: L505 (Lars Olson), JKP (Jeff Pohlmeyer), Vladimir Sibirov

  License:
   Any FPC include file modules are under modified FPC RTL GPL, the rest is GFY
}

unit pwfileutil;

{$IFDEF FPC}{$MODE OBJFPC}{$H+}
  {$IFDEF EXTRA_SECURE}{$R+}{$Q+}{$CHECKPOINTER ON}{$ENDIF}
{$ENDIF}

{$I DelphiDefines.inc}

interface
uses
 {$ifdef windows}windows,{$endif} {$ifdef unix}baseunix, unix,{$endif}
 {$ifndef fpc}sysutils,
  // IFDEF delphi 5 ? not needed in later delphi
  {FileCtrl,}
  //
 {$endif} // delphi
  pwtypes;


// many tests, feel free to add more
procedure RunTests;

// default, read, readwrite,
type TFmode = (fmDefault, fmR, fmRW);
     TFileOfChar = file of ansichar;

const
  DEFAULT_CHUNK_SIZE  = 8192;  // for functions that BlockRead in chunks
  { File open modes }
  fmOpenRead       = $0000;
  fmOpenWrite      = $0001;
  fmOpenReadWrite  = $0002;
  { Share modes}
  fmShareCompat    = $0000;
  fmShareExclusive = $0010;
  fmShareDenyWrite = $0020;
  fmShareDenyRead  = $0030;
  fmShareDenyNone  = $0040;

procedure Xpath(var path: astr);

function NewFile(const fname: astr): bln;
function CloneFile(src, dest: astr): integer;
function FileError: astr;

function FileThere(const fpath: astr; fm: TFmode): bln; overload;
function FileThere(const fpath: astr): bln; overload;

function File2Bytes(const fname: astr; out buf: TByteRay): int32;  overload;
function File2Bytes(const fname: astr; chunksz: int32; out buf:TByteRay): int32; overload;
function File2Str(const fname:astr; out err:int32): astr; overload;
function File2Str(const fname:astr): astr; overload;

function ExtractFilePart(fpath: astr): astr;
function ExtractFname(const fpath: astr; ext: bln): astr;
{$IFDEF FPC}
 function ExtractFilePath(const fname: astr): astr;
 function ExtractFileDir(const fname: astr): astr;
 function ExtractFileDrive(const fname: astr): astr;
 function ExtractFileName(const fname: astr): astr;
 function ExtractFileExt(const fname: astr): astr;
 function ExtractRelativepath (Const BaseName,DestNAme : astr): astr;

 function ChangeFileExt(const fname, Extension: astr): astr;

 function IncludeTrailingPathDelimiter(Const Path : astr) : astr;
 function IncludeTrailingBackslash(Const Path : astr) : astr;
 function ExcludeTrailingBackslash(Const Path: astr): astr;
 function ExcludeTrailingPathDelimiter(Const Path: astr): astr;
 function IsPathDelimiter(const path: astr; index: integer): boo;
 procedure DoDirSeparators (Var fname : astr);
 function SetDirSeparators (Const fname : astr) : astr;
 function GetDirs (Var DirName : astr; Var Dirs : Array of pansichar): longint;
{$ENDIF}

function OpenFileRead(var F:file; const fname: astr; recsize: integer): boo;
function OpenFileReWrite(var F:file; const fname:astr; recsize:integer): boo;
function OpenFile(var F: text; const fname: astr; mode: ansichar): boo; overload;
function OpenFile(var F: file; const fname: astr; recsize:integer; mode: byte): boo; overload;
function OpenFile(var F: file; const fname: astr; mode: ansichar): boo; overload;
function OpenFile(var F: TFileOfChar; const fname: astr; mode: ansichar): boo; overload;

function MakeDir(s: astr): boo;

{$ifdef fpc}
 function DirectoryExists(const dir: astr): boo;
 function DirExists(const dir: astr): boo;
{$endif}

{$ifdef windows}
function GetFileAttributes(Dir: astr): dword;
{$endif}

procedure Sleep(milliseconds: Cardinal);

{$IFDEF FPC}
 function ExecuteProcess(const path: astr; const ComLine: astr):integer; overload;
 function ExecuteProcess(const path: astr; const ComLine: array of astr): integer; overload;
{$ENDIF}

function GetFileSize(const fname: astr): int32;
function GetLargeFileSize(const fname: astr): int64;

// backwards compatibility for older Powtils programs (use FileThere)
function FileExists_plain(const fname: astr): bln;
function FileExists_read(const fname: astr): bln;
function FileExists_readwrite(const fname: astr): bln;

{ TODO str2file
  function str2file(const s, fname:astr): errcode; overload;
  function str2file(const s, fname:astr; chunksz: int32): errcode; overload;
}

const DirSeparators : set of ansichar = ['/','\'];

implementation 
uses
  pwsubstr,
{$IFDEF FPC}
  pwstrutil;
{$ELSE}
  strutils;
{$ENDIF}


{ Put tests in here }
procedure RunTests;
begin
  writeln('ExtractFilePart results: ', ExtractFilePart('c:\tmp\blah.cool\success.txt'));
  writeln('                         ', ExtractFilePart('c:\tmp\blah.cool\success'));
  writeln('                         ', ExtractFilePart('/path/success.txt'));  
  writeln('ExtractFname results: ', ExtractFname('c:\tmp\blah.cool\success.txt', false));
  writeln('                      ', ExtractFname('c:\tmp\blah.cool\success-with-ext.txt', true));
  writeln('                      ', ExtractFname('/path/special.success.txt', false));
  writeln('                      ', ExtractFname('/path/success', true));
  writeln('                      ', ExtractFname('/path/success', false));
  writeln('CloneFile result (test.txt must exist): ', clonefile('test.txt', 'newfile-success.txt') );
  readln;
end;

{$IFDEF WINDOWS}
{ Get the size of any file, doesn't matter whether it's a text or binary file.
  JEFF: return -1 if the file can't be found. }
function GetFileSize(const fname: astr): int32;
var
  FileInfo: WIN32_FIND_DATAA ;
  hInfo: THANDLE;
begin
  hInfo:= FindFirstFileA(pansiChar(fname),
          {$ifdef fpc} @FileInfo
          {$else}       FileInfo {$endif});
  if (hInfo <> INVALID_HANDLE_VALUE)
  then begin
    // Actually should return an Int64 ( for file size > 2GB )
    // Should be:
    //  result:=( (FileInfo.nFileSizeHigh * (MAXDWORD+1)) + FileInfo.nFileSizeLow );
    result:= FileInfo.nFileSizeLow;
    windows.findClose(hInfo);
  end else result:= -1
end;

{ gets file size, up to about 2GB, returns -1 on error }
function GetLargeFileSize(const fname: astr): int64;
var
  fileInfo:WIN32_FIND_DATAA;
  hInfo: THANDLE;
begin
  hInfo:= findFirstFileA(pansiChar(fname),
          {$ifdef fpc} @FileInfo
          {$else}       FileInfo {$endif});
  if (hInfo <> INVALID_HANDLE_VALUE) then begin
    result:= int64(fileInfo.nFileSizeHigh) shl int64(32) +    
             int64(fileInfo.nFileSizeLow);
    windows.findClose(hInfo);
  end else result:= -1
end;

{$ELSE} // Unix versions of above
function GetLargeFileSize(const fname: astr): int64;
var FileInfo:TStat;
begin
  if (fpStat(fname,FileInfo) = 0) then result:= FileInfo.st_size
    else result:= -1;
end;

function GetFileSize(const fname: astr): int32;
var fsize: int64;
begin
  fsize:= GetLargeFileSize(fname);
  // error if file too big
  if fsize > high(int32) then result:= -1 else result:= fsize;
end;
{$ENDIF}

{$ifdef windows}
procedure Sleep(milliseconds: Cardinal);
begin
  windows.sleep(milliseconds);
end;

function GetFileAttributes(dir: astr): dword;
begin
  result:=GetFileAttributesA(PAnsiChar(dir));  // L505: change pchar to pansichar
end;
{$endif}

{ returns -1 if problem, else total bytes of file and a buffer in OUT param } 
function File2Bytes(const fname:astr; chunksz:int32; out buf:TByteRay): int32; 
var F: file;
    curRead: int32; // currently read
    totalread: int32;
begin
  result:= -1; 
  totalRead:= 0;
  setlength(buf, 0);
  // open file with a record size of 1
  if OpenFile(F, fname, 1, fmOpenRead) = false then EXIT;
  curRead:= 1;
  // the file will be read as one big chunk
  while curRead > 0 do begin
    setlength(buf, length(buf)+chunksz);
    curRead:= 0;
    blockread(F, buf[totalread], chunksz, curRead);
    totalread:= totalRead + curRead;
  end;
  setlength(buf, totalRead);
  result:= totalread;
  closefile(F);
end;

{ overloaded with default chunk size }
function File2bytes(const fname: astr; out buf: TByteRay): int32; 
begin
  result:= file2bytes(fname, DEFAULT_CHUNK_SIZE, buf); 
end;

{ loads file into a string returns -1 in OUT param if error }
function File2str(const fname: astr; chunksz: int32; out err: int32): astr; overload;
var buf: TByteRay; len: int32;
begin
  result:= '';
  err:= file2bytes(fname, chunksz, buf);
  if err > 0 then begin
    len:= length(buf);
    setlength(result, len);
    uniquestring(result);
    move(pansichar(buf)[0], pansichar(result)[0], len);
  end;
end;

{ overloaded with default chunk size used }
function File2str(const fname: astr; out err: int32): astr; 
begin
  result:= file2str(fname, DEFAULT_CHUNK_SIZE, err); 
end;

{ loads file into a string, returns empty if file not accessible }
function File2str(const fname: astr): astr; 
var err: int32;
begin
  result:= file2str(fname, err);
  if err < 0 then result:= '';
end;

function OpenFile(var F: TFileOfChar; const fname: astr; mode: ansichar): boo;
begin
  result:= OpenFile(f, fname, mode);
end;

{ Try to open a text file, return true on success.
  The MODE argument must be one of [R]=read, [W]=write, or [A]=append. }
function OpenFile(var F: text; const fname: astr; mode: ansichar): boo;
var oldFM: byte;
begin
  if ( mode in ['A', 'R', 'W'] ) then inc(mode, 32); // "mode" to lowercase
  if ( mode in ['a', 'r', 'w'] ) then
  begin
    oldFM:= filemode;
    if ( mode= 'r') then filemode:= 0 else filemode:= 1;
    {$I-} // I/O checks off for now
     ioresult; // Clears any previous error.
     assign(F, fname);
     case mode of
       'a':append(F);
       'r':reset(F);
       'w':rewrite(F);
     end;
     result:= (ioresult = 0);
    {$I+} // restore I/O checking
    filemode:= oldFM;
  end else
    result:= FALSE; // invalid MODE argument
end;

function OpenFileRead(var F: file; const fname: astr; recsize: integer): boo;
var oldFM: byte;       
begin
  oldFM:= filemode;
  filemode:= fmOpenRead;
 {$I-} // disable I/O checks
  ioresult; // Clears any previous error.
  assign(F, fname);
  reset(F, recsize);
  result:= (ioresult = 0);
 {$I+} // restore I/O checks
  filemode:= oldFM;
end;

function OpenFile(var F: file; const fname: astr; recsize: integer; mode: byte): boo;
var oldFM: byte;
begin
  oldFM:= filemode;
  filemode:= mode; 
 {$I-} // I/O checks off for now
  ioresult; // clear any previous error
  assign(F, fname);
  reset(F, recsize);
  result:= (ioresult = 0);
 {$I+} // restore I/O checks
  filemode:= oldFM;
end;

{ tries to open file, forces file to be created if it does not exist }
function OpenFileReWrite(var F: file; const fname: astr; recsize: integer): boo;
var oldFM: byte;
begin
  oldFM:= filemode;
  filemode:= fmOpenReadWrite; 
 {$I-} // disable I/O checks 
  ioresult; // clears any previous error
  assign(F, fname);
  rewrite(F, recsize);
  result:= (ioresult = 0);
 {$I+} // restore I/O checks
  filemode:= oldFM;
end;

{ Try to open a file (doesn't have to be text) return false if unsuccessful }
function OpenFile(var F: file; const fname: astr; mode: ansichar): boo;
var oldFM: byte;
begin
  if ( mode in ['R', 'W'] ) then
    inc(mode, 32); // convert "mode" to lowercase
  if ( mode in ['r', 'w'] ) then
  begin
    oldFM:= filemode;
    if ( mode= 'r') then filemode:= 0 else filemode:= 1;
    {$I-} // I/O checks off for now
     ioresult; // Clears any previous error.
     assign(F, fname);
     case mode of
       'r':reset(F);
       'w':rewrite(F);
     end;
     result:= (ioresult = 0);
    {$I+} // restore I/O checking
    filemode:= oldFM;
  end else
    result:= FALSE; // invalid MODE argument
end;

{ Copies file from one path to another. Returns less than 1 if a problem }
function CloneFile(src, dest: astr): int32;

  function SaveBuf(const fname: astr; var buf: pointer; sz: integer): integer;
  var F: file;
  begin
    result:= -1; //init
    if OpenFileRewrite(F, fname, 1) = false then EXIT; // open in write mode
    BlockWrite(F, Buf^, sz, result);
    CloseFile(F);
  end;
   
  function LoadBuf(const fname: astr; var buf: TByteRay): integer;
  var F: file; FSize: integer; 
  begin
    result:= -1; // init               
    if OpenFile(F, fname, 1, fmOpenRead) = false then exit;
    FSize:= GetFileSize(fname);
    SetLength(buf, FSize);
    BlockRead(F, pointer(buf)^, FSize, result);
    CloseFile(F);
  end;

var buf: TByteRay;
begin
  result:= LoadBuf(src, buf);
  if result < 0 then exit;
  result:= SaveBuf(dest, pointer(buf), result);
end;

{ grab "file" from "file.ext" }
function ExtractFilePart(fpath: astr): astr;
var i: int32;
    dotcnt: byte;
begin
  result:= '';
  if fpath = '' then exit;
  fpath:= ExtractFileName(fpath);
  if fpath = '' then exit;
  dotcnt:= 0;
  for i:= length(fpath) downto 1 do begin
    if dotcnt > 0 then result:= fpath[i] + result;
    if fpath[i] = '.' then inc(dotcnt);
  end;
  if dotcnt < 1 then result:= fpath;
end;

{ if ext is set to false, only grab "file" from "file.ext" }
function ExtractFname(const fpath: astr; ext: bln): astr;
begin
  result:= '';
  if fpath = '' then exit;
  case ext of 
    true:  result:= ExtractFileName(fpath);
    false: result:= ExtractFilePart(fpath);
  end;
end;

{ creates a directory (not forced) } 
function MakeDir(s: astr): bln;
begin
  result:= false;
 {$I-} // temporarily shut of io checking
  MkDir(s);
 {$I+}
  if IOResult <> 0 then result:= false else result:= true;
end;

{ Cross platform file path slashes normalized: note if one has windows slashes
  stored in a unix path they will be converted... which is not good if unix
  people use slashes in there folder names (but they shouldn't when writing
  portable code, and powtils will not tolerate it.) }
procedure Xpath(var path: astr);
begin
 {$IFDEF WINDOWS}path:= substrreplace(path, '/', '\');{$ENDIF}
 {$IFDEF UNIX}path:= substrreplace(path, '\', '/');{$ENDIF}
end;

{ creates new file, returns true if success  }
function NewFile(const fname: astr): bln;
var fh: file of byte;
    oldfmode: byte;
begin
  result := false;
  oldfmode:= filemode;
  filemode:= fmOpenReadWrite; //  we need write access
 {$I-} // temporarily turn off io checking
  assign(fh, fname);
  rewrite(fh);
 {$I+}
  if ioresult = 0 then begin 
    result:= true; 
    close(fh); 
  end;
  // debugln('FileExists_readwrite: ' + fname);
  filemode:= oldfmode;
end;

{ Alternative to fileexists, not requiring sysutils and uses Assign() and I/O
  to check if file exists 
  By L505 (NRCOL) }  
function FileThere(const fpath: astr; fm: TFmode): bln; overload;
begin
  result:= false;
  if fpath = '' then exit;
  case fm of
    fmDefault: result:= fileexists_plain(fpath);
    fmR:       result:= fileexists_read(fpath);
    fmRW:      result:= fileexists_readwrite(fpath);
  end;
end;

{ Overloaded, default mode }  
function FileThere(const fpath: astr): bln; overload;
begin
  result:= FileThere(fpath, fmDefault);
end;

{ Checks if file exists, returns true if success }
function FileExists_plain(const fname: astr): bln;
var fh: file of byte;
begin
  result := false;
 {$I-} // temporarily turn off io checks
  assign(fh, fname);
  reset(fh);
 {$I+}
  if ioresult = 0 then begin 
    result:= true; 
    close(fh); 
  end;
end;

{ Checks if file exists read only, returns true if success }
function FileExists_read(const fname: astr): bln;
var fh: file of byte;
    oldfmode: byte;
begin
  result := false;
  oldfmode:= filemode;
  filemode:= fmOpenRead; // all we need is read access
 {$I-} // temporarily turn off io checks
  assign(fh, fname);
  reset(fh);
 {$I+}
  if ioresult = 0 then begin 
    result:= true; 
    close(fh); 
  end;
  filemode:= oldfmode;
end;

{ Checks if file exists with write access, returns true if success }
function FileExists_readwrite(const fname: astr): bln;
var fh: file of byte;
    oldfmode: byte;
begin
  result := false;
  oldfmode:= filemode;
  filemode:= fmOpenReadWrite; //  we need write access
 {$I-} // temporarily turn off io checks
  assign(fh, fname);
  reset(fh);
 {$I+}
  if ioresult = 0 then begin 
    result:= true; 
    close(fh); 
  end;
  // debugln('FileExists_readwrite: ' + fname);
  filemode:= oldfmode;
end;


{ Returns last I/O error message }
function FileError: astr;
begin
  case ioresult of
    2:   result:= 'File not found';
    3:   result:= 'Path not found';
    4:   result:= 'Too many open files';
    5:   result:= 'Access denied';
    6:   result:= 'Invalid file handle';
    12:  result:= 'Invalid file-access mode';
    15:  result:= 'Invalid disk number';
    16:  result:= 'Cannot remove current directory';
    17:  result:= 'Cannot rename across volumes';
    100: result:= 'Error when reading from disk';
    101: result:= 'Error when writing to disk';
    102: result:= 'File not assigned';
    103: result:= 'File not open';
    104: result:= 'File not opened for input';
    105: result:= 'File not opened for output';
    106: result:= 'Invalid number';
    150: result:= 'Disk is write protected';
    151: result:= 'Unknown device';
    152: result:= 'Drive not ready';
    153: result:= 'Unknown command';
    154: result:= 'CRC check failed';
    155: result:= 'Invalid drive specified';
    156: result:= 'Seek error on disk';
    157: result:= 'Invalid media type';
    158: result:= 'Sector not found';
    159: result:= 'Printer out of paper';
    160: result:= 'Error when writing to device';
    161: result:= 'Error when reading from device';
    162: result:= 'Hardware failure';
  else
    // Emtpy line - OK
    result := '';
  end;
end;

{ nicer alias }
function DirExists(const dir: astr): boo;
begin
  result:= DirectoryExists(dir);
end;

{ Above is under NRCOL license. Below file is modified Fpc Rtl GPL }

{$IFDEF FPC}
  {$I pwfileutil_fpc_sysutils.inc }
{$ENDIF}


end.
