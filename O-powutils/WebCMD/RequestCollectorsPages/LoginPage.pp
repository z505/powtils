program Page;

{$mode objfpc}{$H+}

uses
  SysUtils, BaseUnix, Unix;

type
  // Associative variable
  Web_TVariable = record
    name, value: string;

  end;

  // Variables associative array
  Web_TVariables = array of Web_TVariable;

const
  PipesPath: String= '/var/www/cgi-bin/WebCMD/Pipes/';//Apache's user should be able to read and write on this dir. (The last char must be an slash)
  MainPipeName: String= '/var/www/cgi-bin/WebCMD/MainPipe4WebCMD';
  TempPipeLen: Integer= 10;
  PageName: String= 'LOGINPAGE.PSP';

var
  S: String;
  Env  // Enironment data
  : Web_TVariables;

{$IFDEF UNIX}
var environ: ppchar; cvar; external;
function getenv(const name: PChar): PChar; cdecl; external 'c' name 'getenv';
function setenv(const name, value: pchar; replace: longint): longint; cdecl; external 'c' name 'setenv';
function unsetenv(const name: pchar): longint; cdecl; external 'c' name 'unsetenv';
{$ENDIF}

function GetEnvVar (const Name: String): String;
var
  i: LongWord;

begin

  Result:= '';

  for i:= 0 to High (Env) do
  begin
    if UpCase (Env [i].Name)= UpCase (Name) then
    begin
      Result:= Env[i].Value;
      Break;

    end;

  end;

end;

// Loads up system environment
function LoadEnvVar: Boolean;
var
{$IFDEF WIN32}
    p, hp: pchar;
{$ENDIF}
{$IFDEF UNIX}
    p: ppchar;
{$ENDIF}
    s: string;
    i: longint;
begin
  result:= false;
  {$IFDEF WIN32}
  p:= GetEnvironmentStrings;
  hp:= p;
  while hp^ <> #0 do
  {$ENDIF}
  {$IFDEF UNIX}
  p:= environ;

  while (p^ <> nil) and (p^ <> #0) do
  {$ENDIF}
  begin
    {$IFDEF WIN32}s:= AnsiString(hp);{$ENDIF}
    {$IFDEF UNIX}s:= AnsiString(p^);{$ENDIF}
    i:= Pos ('=', s);

    SetLength (env, length(env) + 1);
    // Parsing as name=value
    Env [Length (Env)- 1].name:= UpCase (Copy (S, 1, i- 1));
    Env [Length (Env)- 1].value:= Copy (S, i+ 1, Length (s)- i);

    // Next entry
    {$IFDEF WIN32}hp:= hp + strlen(hp) + 1;{$ENDIF}
    {$IFDEF UNIX}inc(p);{$ENDIF}
  end;
  {$IFDEF WIN32}
  FreeEnvironmentStrings(p);
  {$ENDIF}
  result:= true;
end;

function GetParameters: String;
var
  Method,
  TempStr: string;
  ContLen,
  i: LongWord;

begin
  Result:= '';

  // First getting method data
  Method:= GetEnvVar ('REQUEST_METHOD');

  if Method= 'POST' then
  begin

    val (GetEnvVar ('CONTENT_LENGTH'), ContLen);

     for i:= 1 to ContLen do
     begin
       Read (TempStr);
       Result:= Result+ TempStr+ ';';

     end;

     if ContLen<> 0 then
       Delete (Result, Length (Result), 1);

  end
  else if Method= 'GET' then
    Result:= GetEnvVar('QUERY_STRING');

end;

function GenerateNewPipe: String;
const
  Letters: String= 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890';

var
  FileName: String;
  ChangeIndex: Integer;
  i: Integer;

begin
  FileName:= '';

  for i:= 1 to TempPipeLen do
    FileName:= FileName+ Letters [1+ Random (62)];

  while FileExists (PipesPath+ FileName) do
  begin
    ChangeIndex:= Random (TempPipeLen)+ 1;
    FileName [ChangeIndex]:= Letters [1+ Random (62)];

  end;

  FpMkfifo (PipesPath+ FileName, $1B4);//664 1 1011 0100

  Result:= PipesPath+ FileName;

end;

// Tells whether a environment variable is assigned
function IsEnvVar (const Name: String): Boolean;
var
  i: LongWord;

begin
  Result:= False;
  if Length (Env)= 0 then
    Exit;

  for i:= 0 to Length (Env) - 1 do
    if UpCase (Env [i].Name)= UpCase (Name) then
    begin
      Result:= True;
      Break;
    end;
end;

var
  OutputPipeHandle: cInt;
  InputPipeHandle: TextFile;

  NewFifoName,
  CookieString,
  ParametersString,
  AllParameters: String;

begin
  LoadEnvVar;

  ParametersString:= GetParameters;

  OutputPipeHandle:= FpOpen (MainPipeName, O_WRONLY);
  fpFlock (OutputPipeHandle, LOCK_EX);

  NewFifoName:= GenerateNewPipe;

  // Load Cookies
  // If your project doesn't need to Cookie, you can comment these lines to achieve a better performance.
  if IsEnvVar ('HTTP_COOKIE') then
    CookieString:= GetEnvVar ('HTTP_COOKIE')
  else
    CookieString:= '';

  AllParameters:= PageName+ #$FF+ NewFifoName+ #$FF+ ParametersString+ #$FE+ CookieString+ #$FD;
  fpWrite (OutputPipeHandle, AllParameters [1], Length (AllParameters));

  fpFlock (OutputPipeHandle, LOCK_UN);
  FpClose (OutputPipeHandle);

  AssignFile (InputPipeHandle, NewFifoName);
  Reset (InputPipeHandle);

  ReadLn (InputPipeHandle, S);
  WriteLn (S);
  while not Eof (InputPipeHandle) do
  begin
    ReadLn (InputPipeHandle, S);
    WriteLn (S);

  end;

  CloseFile (InputPipeHandle);

  if not DeleteFile (NewFifoName) then
    WriteLn ('Error!');

end.
