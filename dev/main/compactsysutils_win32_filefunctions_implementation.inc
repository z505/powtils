{-- copy/pasted from sysutils.pp /win32 ---------------------------------------}

{****************************************************************************
                              File Functions
****************************************************************************}

Function FileOpen (Const FileName : string; Mode : Integer) : Longint;
const
  AccessMode: array[0..2] of Cardinal  = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of Integer = (
               0,
               0,
               FILE_SHARE_READ,
               FILE_SHARE_WRITE,
               FILE_SHARE_READ or FILE_SHARE_WRITE);
Var
  FN : string;
begin
  FN:=FileName+#0;
  result := CreateFile(@FN[1], dword(AccessMode[Mode and 3]),
                       dword(ShareMode[(Mode and $F0) shr 4]), nil, OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL, 0);
end;


Function FileCreate (Const FileName : String) : Longint;
Var
  FN : string;
begin
  FN:=FileName+#0;
  Result := CreateFile(@FN[1], GENERIC_READ or GENERIC_WRITE,
                       0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;


Function FileCreate (Const FileName : String; Mode:longint) : SizeInt;
begin
  FileCreate:=FileCreate(FileName);
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;
Var
  res : dword;
begin
  if ReadFile(Handle, Buffer, Count, res, nil) then
   FileRead:=Res
  else
   FileRead:=-1;
end;


Function FileWrite (Handle : Longint; const Buffer; Count : Longint) : Longint;
Var
  Res : dword;
begin
  if WriteFile(Handle, Buffer, Count, Res, nil) then
   FileWrite:=Res
  else
   FileWrite:=-1;
end;


Function FileSeek (Handle,FOffset,Origin : Longint) : Longint;
begin
  Result := longint(SetFilePointer(Handle, FOffset, nil, Origin));
end;


Function FileSeek (Handle : Longint; FOffset,Origin : Int64) : Int64;
begin
  {$warning need to add 64bit call }
  Result := longint(SetFilePointer(Handle, FOffset, nil, Origin));
end;


Procedure FileClose (Handle : Longint);
begin
  if Handle<=4 then
   exit;
  CloseHandle(Handle);
end;


Function FileTruncate (Handle,Size: Longint) : boolean;
begin
  Result:=longint(SetFilePointer(handle,Size,nil,FILE_BEGIN))<>-1;
  If Result then
    Result:=SetEndOfFile(handle);
end;

Function DosToWinTime (DTime:longint;Var Wtime : TFileTime):longbool;
var
  lft : TFileTime;
begin
  DosToWinTime:=DosDateTimeToFileTime(longrec(dtime).hi,longrec(dtime).lo,@lft) and
                LocalFileTimeToFileTime(lft,Wtime);
end;


Function WinToDosTime (Var Wtime : TFileTime;var DTime:longint):longbool;
var
  lft : TFileTime;
begin
  WinToDosTime:=FileTimeToLocalFileTime(WTime,lft) and
                FileTimeToDosDateTime(lft,Longrec(Dtime).Hi,LongRec(DTIME).lo);
end;


Function FileAge (Const FileName : String): Longint;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Handle := FindFirstFile(Pchar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
        If WinToDosTime(FindData.ftLastWriteTime,Result) then
          exit;
    end;
  Result := -1;
end;


Function FileExists (Const FileName : String) : Boolean;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Handle := FindFirstFile(Pchar(FileName), FindData);
  Result:=Handle <> INVALID_HANDLE_VALUE;
  If Result then
    Windows.FindClose(Handle);
end;


Function DirectoryExists (Const Directory : String) : Boolean;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Result:=False;
  Handle := FindFirstFile(Pchar(Directory), FindData);
  If (Handle <> INVALID_HANDLE_VALUE) then
    begin
    Result:=((FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY);
    Windows.FindClose(Handle);
    end;
end;


Function FindMatch(var f: TSearchRec) : Longint;
begin
  { Find file with correct attribute }
  While (F.FindData.dwFileAttributes and cardinal(F.ExcludeAttr))<>0 do
   begin
     if not FindNextFile (F.FindHandle,F.FindData) then
      begin
        Result:=GetLastError;
        exit;
      end;
   end;
  { Convert some attributes back }
  WinToDosTime(F.FindData.ftLastWriteTime,F.Time);
  f.size:=F.FindData.NFileSizeLow;
  f.attr:=F.FindData.dwFileAttributes;
  f.Name:=StrPas(@F.FindData.cFileName);
  Result:=0;
end;


Function FindFirst (Const Path : String; Attr : Longint; out Rslt : TSearchRec) : Longint;
begin
  Rslt.Name:=Path;
  Rslt.Attr:=attr;
  Rslt.ExcludeAttr:=(not Attr) and ($1e);
                 { $1e = faHidden or faSysFile or faVolumeID or faDirectory }
  { FindFirstFile is a Win32 Call }
  Rslt.FindHandle:=FindFirstFile (PChar(Path),Rslt.FindData);
  If Rslt.FindHandle=Invalid_Handle_value then
   begin
     Result:=GetLastError;
     exit;
   end;
  { Find file with correct attribute }
  Result:=FindMatch(Rslt);
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;
begin
  if FindNextFile(Rslt.FindHandle, Rslt.FindData) then
    Result := FindMatch(Rslt)
  else
    Result := GetLastError;
end;


Procedure FindClose (Var F : TSearchrec);
begin
   if F.FindHandle <> INVALID_HANDLE_VALUE then
    Windows.FindClose(F.FindHandle);
end;


Function FileGetDate (Handle : Longint) : Longint;
Var
  FT : TFileTime;
begin
  If GetFileTime(Handle,nil,nil,@ft) and
     WinToDosTime(FT,Result) then
    exit;
  Result:=-1;
end;


Function FileSetDate (Handle,Age : Longint) : Longint;
Var
  FT: TFileTime;
begin
  Result := 0;
  if DosToWinTime(Age,FT) and
    SetFileTime(Handle, ft, ft, FT) then
    Exit;
  Result := GetLastError;
end;


Function FileGetAttr (Const FileName : String) : Longint;
begin
  Result:=GetFileAttributes(PChar(FileName));
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;
begin
  if not SetFileAttributes(PChar(FileName), Attr) then
    Result := GetLastError
  else
    Result:=0;
end;


Function DeleteFile (Const FileName : String) : Boolean;
begin
  DeleteFile:=Windows.DeleteFile(Pchar(FileName));
end;


Function RenameFile (Const OldName, NewName : String) : Boolean;
begin
  Result := MoveFile(PChar(OldName), PChar(NewName));
end;


{------------------------------------------------------------------------------}
