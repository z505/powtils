{
  Authors: FPC RTL, Trustmaster (Vladimir Sibirov), JKP (Jeff Pohlmeyer), 
           L505 (Lars Olson),
  License: 
   FPC RTL Modified GPL. 
   However, any functions stamped with "Vladimir" are Artistic License. Any 
   functions stamped with "Lars" are NRCOL license (I hate GPL). Any functions 
   by both Lars & Vladimir are Artistic License. Functions are therefore free
   software,  similar to dual licensed code with some full public domain  }

unit pwfileutil;

{$IFDEF FPC}{$MODE OBJFPC}{$H+}
  {$IFDEF EXTRA_SECURE}{$R+}{$Q+}{$CHECKPOINTER ON}{$ENDIF}
{$ENDIF}

interface
uses 
  {$ifdef windows}windows,{$endif}
  {$ifdef unix}baseunix, unix,{$endif}
  pwtypes;


// many tests, feel free to run them, add to them, verify them, etc
procedure RunTests;

// default, read, readwrite, 
type TFmode = (fmDefault, fmR, fmRW);
     TFileOfChar = file of char;

const
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

function ExtractFilePart(fpath: astr): astr;
function ExtractFname(const fpath: astr; ext: bln): astr;

function FileThere(const fname: astr; fm: TFmode): bln;

function ExtractFilePath(const FileName: string): string;
function ExtractFileDir(const FileName: string): string;
function ExtractFileDrive(const FileName: string): string;
function ExtractFileName(const FileName: string): string;
function ExtractFileExt(const FileName: string): string;

function ChangeFileExt(const FileName, Extension: string): string;

function ExtractRelativepath (Const BaseName,DestNAme : String): String;
function IncludeTrailingPathDelimiter(Const Path : String) : String;
function IncludeTrailingBackslash(Const Path : String) : String;
function ExcludeTrailingBackslash(Const Path: string): string;
function ExcludeTrailingPathDelimiter(Const Path: string): string;
function IsPathDelimiter(Const Path: string; Index: Integer): Boolean;
Procedure DoDirSeparators (Var FileName : String);
Function SetDirSeparators (Const FileName : String) : String;
function GetDirs (Var DirName : String; Var Dirs : Array of pchar) : Longint;

function OpenFileRead(var F:file; const fname: string; recsize: integer): boolean;
function OpenFileReWrite(var F:file; const fname:string; recsize:integer): boolean;
function OpenFile(var F: text; const fname: string; mode: char): boolean; overload;
function OpenFile(var F: file; const fname: string; recsize:integer; mode: byte): boolean; overload;
function OpenFile(var F: file; const fname: string; mode: char): boolean; overload;
function OpenFile(var F: TFileOfChar; const fname: string; mode: char): boolean; overload;  
function MakeDir(s: string): boolean;

function DirectoryExists(const directory: string): boolean;

{$ifdef windows}
function GetFileAttributes(Directory: string): dword;
{$endif}

function ExecuteProcess(Const Path: AnsiString; Const ComLine: AnsiString):integer;
function ExecuteProcess(Const Path: AnsiString; Const ComLine: Array of AnsiString):integer;

// backwards compatibility for older Powtils programs (use FileThere)
function FileExists_plain(const fname: astr): bln;
function FileExists_read(const fname: astr): bln;
function FileExists_readwrite(const fname: astr): bln;

procedure Sleep(milliseconds: Cardinal);


Const
  DirSeparators : set of char = ['/','\'];

implementation

uses
  pwsubstr, pwstrutil;

{ Put tests in here to verify this unit works in as many situations as 
  possible. }
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

{$ifdef unix}

  { fpc rtl }
  procedure Sleep(milliseconds: Cardinal);
  var timeout,timeoutresult : TTimespec;
  begin
    timeout.tv_sec:=milliseconds div 1000;
    timeout.tv_nsec:=1000*1000*(milliseconds mod 1000);
    fpnanosleep(@timeout,@timeoutresult);
  end;


  { fpc rtl 
    TODO: return error as OUT param }
  function ExecuteProcess(Const Path: AnsiString; Const ComLine: AnsiString):integer;
  var
    pid    : longint;
    CommandLine: AnsiString;
    cmdline2 : ppchar;
  //  e      : EOSError;
  Begin
    { always surround the name of the application by quotes
      so that long filenames will always be accepted. But don't
      do it if there are already double quotes!
    }
     cmdline2:=nil;
     if Comline<>'' Then
       begin
         CommandLine:=ComLine;
         { Make an unique copy because stringtoppchar modifies the
           string }
         UniqueString(CommandLine);
         cmdline2:=StringtoPPChar(CommandLine,1);
         cmdline2^:=pchar(Path);
       end
     else
       begin
         getmem(cmdline2,2*sizeof(pchar));
         cmdline2^:=pchar(Path);
         cmdline2[1]:=nil;
       end;
  //  {$ifdef USE_VFORK}
  //  pid:=fpvFork;
  //  {$else USE_VFORK}
    pid:=fpFork;
  //  {$endif USE_VFORK}
    if pid=0 then
     begin
     {The child does the actual exec, and then exits}
  //    {$ifdef FPC_USE_FPEXEC}
        fpexecv(pchar(Path),Cmdline2);
  //    {$else}
  //      Execl(CommandLine);
  //    {$endif}
       { If the execve fails, we return an exitvalue of 127, to let it be known}
       fpExit(127);
     end
    else
     if pid=-1 then         {Fork failed}
     begin
  //      e:=EOSError.CreateFmt(SExecuteProcessFailed,[Path,-1]);
  //      e.ErrorCode:=-1;
  //      raise e;
          exit;
          // TODO: return error as OUT param
     end;

    { We're in the parent, let's wait. }
    result:=WaitProcess(pid); // WaitPid and result-convert

  //  {$ifdef FPC_USE_FPEXEC}
    if Comline<>'' Then
      freemem(cmdline2);
  //  {$endif}

    if (result<0) or (result=127) then begin
  //    E:=EOSError.CreateFmt(SExecuteProcessFailed,[Path,result]);
  //    E.ErrorCode:=result;
  //    Raise E;
        exit;
        // TODO: return error as OUT param
    end;
  End;

  { fpc rtl }
  function ExecuteProcess(Const Path: AnsiString; Const ComLine: Array Of AnsiString):integer;
  var pid    : longint;
  //  e : EOSError;
  Begin
    pid:=fpFork;
    if pid=0 then begin
       {The child does the actual exec, and then exits}
        fpexecl(Path,Comline);
       { If the execve fails, we return an exitvalue of 127, to let it be known}
       fpExit(127);
    end else if pid=-1 then         {Fork failed}
    begin
  //      e:=EOSError.CreateFmt(SExecuteProcessFailed,[Path,-1]);
  //      e.ErrorCode:=-1;
  //      raise e;
          exit;
    end;

    { We're in the parent, let's wait. }
    result:=WaitProcess(pid); // WaitPid and result-convert

    if (result<0) or (result=127) then begin
  //    E:=EOSError.CreateFmt(SExecuteProcessFailed,[Path,result]);
  //    E.ErrorCode:=result;
  //    raise E;
        exit;
    end;
  end;

{$endif unix}

{$ifdef windows}

procedure Sleep(milliseconds: Cardinal);
begin
  windows.sleep(milliseconds);
end;

function GetFileAttributes(Directory: string): dword;
begin
  result:=GetFileAttributesA(PChar(Directory));
end;

{ from fpc rtl 
  todo: return OUT param for error code }
function ExecuteProcess(Const Path: AnsiString; Const ComLine: AnsiString):integer;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  Proc : THandle;
  l    : DWord;
  CommandLine : ansistring;
//  e : EOSError;
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb:=SizeOf(SI);
  SI.wShowWindow:=1;
  { always surround the name of the application by quotes
    so that long filenames will always be accepted. But don't
    do it if there are already double quotes, since Win32 does not
    like double quotes which are duplicated!
  }
  if pos('"',path)=0 then CommandLine:='"'+path+'"' else CommandLine:=path;
  if ComLine <> '' then 
    CommandLine:=Commandline+' '+ComLine+#0
  else CommandLine := CommandLine + #0;

  if not CreateProcess(nil, pchar(CommandLine), 
    Nil, Nil, False,$20, Nil, Nil, SI, PI) 
  then begin
// todo: return error code
//    e:=EOSError.CreateFmt(SExecuteProcessFailed,[CommandLine,GetLastError]);
//    e.ErrorCode:=GetLastError;
//    raise e;
    exit;
  end;
  Proc:=PI.hProcess;
  if WaitForSingleObject(Proc, dword($ffffffff)) <> $ffffffff then
    begin
      GetExitCodeProcess(Proc,l);
      CloseHandle(Proc);
      CloseHandle(PI.hThread);
      result:=l;
    end
  else
    begin
// todo: return error code
//      e:=EOSError.CreateFmt(SExecuteProcessFailed,[CommandLine,GetLastError]);
//      e.ErrorCode:=GetLastError;
      CloseHandle(Proc);
      CloseHandle(PI.hThread);
//      raise e;
    end;
end;

function ExecuteProcess(Const Path: AnsiString; Const ComLine: Array of AnsiString):integer;
var CommandLine: AnsiString;
    i: Integer;
begin
  Commandline:='';
  For i:=0 to high(ComLine) Do Commandline:=CommandLine+' '+Comline[i];
  ExecuteProcess:=ExecuteProcess(Path,CommandLine);
end;
{$endif}

{ fpc rtl: todo, other platforms using include files :( }
function DirectoryExists (Const Directory : String) : Boolean;
{$ifdef windows}
var Attr:Dword;
begin
  Attr:=GetFileAttributes(Directory);
  if Attr <> $ffffffff then
    Result:= (Attr and FILE_ATTRIBUTE_DIRECTORY) > 0
  else
    Result:=False;
{$endif}
{$ifdef unix}
Var Info : Stat;
begin
  DirectoryExists:=(fpstat(Directory,Info)>=0) and fpS_ISDIR(Info.st_mode);
{$endif}
end;


{  By JKP and L505 (license: public domain) }
function OpenFile(var F: TFileOfChar; const fname: string; mode: char): boolean;
begin
  result:= OpenFile(f, fname, mode);
end;

{ Try to open a text file, return true on success.
  The MODE argument must be one of [R]=read, [W]=write, or [A]=append. 
  By JKP and L505 (license: public domain) }
function OpenFile(var F: text; const fname: string; mode: char): boolean;
var
  oldFM: byte;
begin
  if ( mode in ['A', 'R', 'W'] ) then
    inc(mode, 32); // convert "mode" to lowercase
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
     Result:= (ioresult = 0);
    {$I+} // restore I/O checking
    filemode:= oldFM;
  end else
    Result:= FALSE; // invalid MODE argument
end;

{  By JKP and L505 (license: public domain) }
function OpenFileRead(var F: file; const fname: string; recsize: integer): boolean;
var
  oldFM: byte;       
begin
  oldFM:= filemode;
  filemode:= fmOpenRead;
{$I-} // I/O checks off for now
  ioresult; // Clears any previous error.
  assign(F, fname);
  reset(F, recsize);
  Result:= (ioresult = 0);
{$I+} // restore I/O checking
  filemode:= oldFM;
end;

{  By JKP and L505 (license: public domain) }
function OpenFile(var F: file; const fname: string; recsize: integer; mode: byte): boolean;
var                                                                                        
  oldFM: byte;
begin
  oldFM:= filemode;
  filemode:= mode; 
{$I-} // I/O checks off for now
  ioresult; // Clears any previous error.
  assign(F, fname);
  reset(F, recsize);
  Result:= (ioresult = 0);
{$I+} // restore I/O checking
  filemode:= oldFM;
end;

{ tries to open file, forces file to be created if it does not exist 
  By JKP and L505 (license: public domain) }
function OpenFileReWrite(var F: file; const fname: string; recsize: integer): boolean;
var                                                                                        
  oldFM: byte;
begin
  oldFM:= filemode;
  filemode:= fmOpenReadWrite; 
{$I-} // I/O checks off for now
  ioresult; // Clears any previous error.
  assign(F, fname);
  rewrite(F, recsize);
  Result:= (ioresult = 0);
{$I+} // restore I/O checking
  filemode:= oldFM;
end;


{ Try to open a file (doesn't have to be text) return false if unsuccessful 
  By JKP and L505 (license: public domain) }
function OpenFile(var F: file; const fname: string; mode: char): boolean;
var
  oldFM: byte;
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
     Result:= (ioresult = 0);
    {$I+} // restore I/O checking
    filemode:= oldFM;
  end else
    Result:= FALSE; // invalid MODE argument
end;


{ Copies file from one path to another. Returns less than 1 if a problem 
  By Lars (NRCOL) }
function CloneFile(src, dest: astr): integer;
type bytearray = array of byte;

 function SaveBuf(const fname: astr; var buf: pointer; sz: integer): integer;
 var F: file;
 begin
   result:= -1; //init
   if OpenFileRewrite(F, fname, 1) = false then EXIT; // open in write mode
   BlockWrite(F, Buf^, sz, result);
   CloseFile(F);
 end;
 
 function LoadBuf(const fname: string; var buf: bytearray): integer;
 var F: file; FSize: Integer; 
 begin
   result:= -1; // init               
   if OpenFile(F, fname, 1, fmOpenReadWrite) = false then exit;
   FSize:= FileSize(F);
   SetLength(buf, FSize);
   BlockRead(F, pointer(Buf)^, FSize, result);
   CloseFile(F);
 end;

var buf: bytearray;
begin
  result:= LoadBuf(src, buf);
  if result < 0 then exit;
  result:=  SaveBuf(dest, pointer(buf), result);
end;

{ BEGIN: FROM FPC SYSUTILS (GPL CRAP,  ruins our entire source file virally) }

const MaxDirs = 129;

function ExtractRelativepath (Const BaseName,DestName : String): String;
var Source, Dest: String;  Sc,Dc,I,J: Longint;
    SD,DD: Array[1..MaxDirs] of PChar;
const OneLevelBack = '..' + PathDelim;
begin
  if Uppercase(ExtractFileDrive(BaseName))<>Uppercase(ExtractFileDrive(DestName)) Then
  begin
    Result:=DestName;
    exit;
  end;
  Source:=ExtractFilePath(BaseName);
  Dest:=ExtractFilePath(DestName);
  SC:=GetDirs (Source,SD);
  DC:=GetDirs (Dest,DD);
  I:=1;
  While (I<DC) and (I<SC) do
    begin
    If StrIcomp(DD[i],SD[i])=0 then
      Inc(i)
    else
      Break;
    end;
  Result:='';
  For J:=I to SC-1 do Result:=Result+OneLevelBack;
  For J:=I to DC-1 do Result:=Result+DD[J]+PathDelim;
  Result:=Result+ExtractFileName(DestNAme);
end;

Procedure DoDirSeparators (Var FileName : String);
var I : longint;
begin
  for I:=1 to Length(FileName) do
    If FileName[I] in DirSeparators then
      FileName[i]:=PathDelim;
end;


function SetDirSeparators (Const FileName : string) : String;
begin
  Result:=FileName;
  DoDirSeparators (Result);
end;

{
  DirName is split in a #0 separated list of directory names,
  Dirs is an array of pchars, pointing to these directory names.
  The function returns the number of directories found, or -1
  if none were found.
  DirName must contain only PathDelim as Directory separator chars.
}

Function GetDirs (Var DirName : String; Var Dirs : Array of pchar) : Longint;
var I : Longint;
begin
  I:=1;
  Result:=-1;
  While I<=Length(DirName) do
    begin
    If DirName[i]=PathDelim then
      begin
      DirName[i]:=#0;
      Inc(Result);
      Dirs[Result]:=@DirName[I+1];
      end;
    Inc(I);
    end;
  If Result>-1 then inc(Result);
end;

function IncludeTrailingPathDelimiter(Const Path : String) : String;
var
  l : Integer;
begin
  Result:=Path;
  l:=Length(Result);
  If (L=0) or (Result[l]<>PathDelim) then
    Result:=Result+PathDelim;
end;

function IncludeTrailingBackslash(Const Path : String) : String;
begin
  Result:=IncludeTrailingPathDelimiter(Path);
end;

function ExcludeTrailingBackslash(Const Path: string): string;
begin
  Result:=ExcludeTrailingPathDelimiter(Path);
end;

function ExcludeTrailingPathDelimiter(Const Path: string): string;
var L: Integer;
begin
  L:=Length(Path);
  If (L>0) and (Path[L]=PathDelim) then
    Dec(L);
  Result:=Copy(Path,1,L);
end;

function IsPathDelimiter(Const Path: string; Index: Integer): Boolean;
begin
  Result:=(Index>0) and (Index<=Length(Path)) and (Path[Index]=PathDelim);
end;

function ChangeFileExt(const FileName, Extension: string): string;
var i: longint;
begin
  I := Length(FileName);
  while (I > 0) and not(FileName[I] in ['/', '.', '\', ':']) do
    Dec(I);
  if (I = 0) or (FileName[I] <> '.') then
    I := Length(FileName)+1;
  Result := Copy(FileName, 1, I - 1) + Extension;
end;


function ExtractFilePath(const FileName: string): string;
var i: longint;
begin
  i := Length(FileName);
  while (i > 0) and not (FileName[i] in ['/', '\', ':']) do Dec(i);
  If I>0 then
    Result := Copy(FileName, 1, i)
  else
    Result:='';
end;

function ExtractFileDir(const FileName: string): string;
var i: longint;
begin
  I := Length(FileName);
  while (I > 0) and not (FileName[I] in ['/', '\', ':']) do Dec(I);
  if (I > 1) and (FileName[I] in ['\', '/']) and
    not (FileName[I - 1] in ['/', '\', ':']) then Dec(I);
  Result := Copy(FileName, 1, I);
end;

function ExtractFileDrive(const FileName: string): string;
var i: longint;
begin
  if (Length(FileName) >= 3) and (FileName[2] = ':') then
    result := Copy(FileName, 1, 2)
  else if (Length(FileName) >= 2) and (FileName[1] in ['/', '\']) and
    (FileName[2] in ['/', '\']) then
  begin
    i := 2;
    While (i < Length(Filename)) do
    begin
           if Filename[i + 1] in ['/', '\'] then break;
      inc(i);
    end ;
    Result := Copy(FileName, 1, i);
  end else Result := '';
end;

function ExtractFileName(const FileName: string): string;
var i: longint;
begin
  I := Length(FileName);
  while (I > 0) and not (FileName[I] in ['/', '\', ':']) do Dec(I);
  Result := Copy(FileName, I + 1, 255);
end;

function ExtractFileExt(const FileName: string): string;
var i: longint;
begin
  I := Length(FileName);
  while (I > 0) and not (FileName[I] in ['.', '/', '\', ':']) do Dec(I);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, 255)
  else Result := '';
end;

{ END: FROM FPC SYSUTILS}

{ this grabs "file" from "file.ext" 
  By L505 (license: public domain) }
function ExtractFilePart(fpath: astr): astr;
var i: integer;
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

{ if ext is set to false, only grab "file" from "file.ext" 
  By L505 (license: public domain) }
function ExtractFname(const fpath: astr; ext: bln): astr;
begin
  result:= '';
  if fpath = '' then exit;
  case ext of 
    true:  result:= ExtractFileName(fpath);
    false: result:= ExtractFilePart(fpath);
  end;
end;


{ creates a directory (not forced) 
  By Lars (NRCOL) } 
function MakeDir(s: astr): bln;
begin
  result:= false;
 {$I-} // temporarily shut of io checking
  MkDir(s);
 {$I+}
  if IOResult <> 0 then
    result:= false
  else
    result:= true;
end;


{ cross platform file path slashes normalized
  By Lars (NRCOL) }
procedure Xpath(var path: astr);
begin
 {$IFDEF WINDOWS}path:= substrreplace(path, '/', '\');{$ENDIF}
 {$IFDEF UNIX}path:= substrreplace(path, '\', '/');{$ENDIF}
end;

{ creates new file, returns true if success  
  By Lars (NRCOL)}
function NewFile(const fname: astr): bln;
var
  fh: file of byte;
  oldfmode: byte;
begin
  result := false;
  oldfmode:= filemode;
  filemode:= fmOpenReadWrite; //  we need write access
 {$I-} // temporarily turn off io checking
  assign(fh, fname);
  rewrite(fh);
 {$I+}
  if ioresult = 0 then
  begin
    result := true;
    close(fh);
  end;
  //    debugln('FileExists_readwrite: ' + fname);
  filemode:= oldfmode;
end;

{ Alternative to fileexists, not requiring sysutils and uses Assign() and I/O
  to check if file exists 
  By L505 (license: public domain) }  
function FileThere(const fname: astr; fm: TFmode): bln;
begin
  if fname = '' then exit;
  case fm of
    fmDefault: result:= fileexists_plain(fname);
    fmR:       result:= fileexists_read(fname);
    fmRW:      result:= fileexists_readwrite(fname);
  end;
end;

{ Checks if file exists, returns true if success 
  By Vladimir/Lars (Artistic) }
function FileExists_plain(const fname: astr): bln;
var
  fh: file of byte;
begin
  result := false;
 {$I-} // temporarily turn off io checking
  assign(fh, fname);
  reset(fh);
 {$I+}
  if ioresult = 0 then
  begin
    result := true;
    close(fh);
  end;
end;

{ Checks if file exists read only, returns true if success 
  By Vladimir/Lars (Artistic) }
function FileExists_read(const fname: astr): bln;
var
  fh: file of byte;
  oldfmode: byte;
begin
  result := false;
  oldfmode:= filemode;
  filemode:= fmOpenRead; // all we need is read access
 {$I-} // temporarily turn off io checking
  assign(fh, fname);
  reset(fh);
 {$I+}
  if ioresult = 0 then
  begin
    result := true;
    close(fh);
  end;
  filemode:= oldfmode;
end;

{ Checks if file exists with write access, returns true if success
  By Vladimir/Lars (artistic) }
function FileExists_readwrite(const fname: astr): bln;
var
  fh: file of byte;
  oldfmode: byte;
begin
  result := false;
  oldfmode:= filemode;
  filemode:= fmOpenReadWrite; //  we need write access
 {$I-} // temporarily turn off io checking
  assign(fh, fname);
  reset(fh);
 {$I+}
  if ioresult = 0 then
  begin
    result := true;
    close(fh);
  end;
  //    debugln('FileExists_readwrite: ' + fname);
  filemode:= oldfmode;
end;


{ Returns last I/O error message
  By Vladimir (artistic) }
function FileError: astr;
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

end.
