Program LightWebServer;

{$mode objfpc}{$H+}
{$define usecthreads}
{$apptype gui}

Uses
  {$IFDEF UNIX}
  {$IFDEF UseCThreads}
  CThreads,
  {$ENDIF}
  {$ENDIF}
  SysUtils,
  Classes,
  HttpServ,
  Synautil,
  BreakTokens,
  DaemonApp;

Const
  ConfigFileName : String = 'lightwebserver.cfg';

Type

  { TLightWebServerDaemon }
  TLightWebServerDaemon = Class(TCustomDaemon)
  Private
    fThread : THTTPServerThread;
    Procedure ThreadStopped(Sender : TObject);
  Public
    Function Start : Boolean; Override;
    Function Stop : Boolean; Override;
    Function Pause : Boolean; Override;
    Function Continue : Boolean; Override;
    Function Execute : Boolean; Override;
    Function ShutDown : Boolean; Override;
    Function Install : Boolean; Override;
    Function UnInstall: boolean; Override;
  End;

Var
  { Basic config }
  DocRoot       : String;
  IP            : String;
  Port          : String;
  Timeout       : LongWord;
  ThreadTimeout : Int64;
  MaxThreads    : LongWord;


Procedure AWriteLn(Msg : String; B : Boolean);
Begin
  Application.Logger.Info(Msg + ' ' + BoolToStr(B));
End;

Procedure AWriteLn(Msg : String);
Begin
  Application.Logger.Info(Msg);
End;

{ TLightWebServerDaemon }

Procedure TLightWebServerDaemon.ThreadStopped(Sender: TObject);
Begin
  FreeAndNil(FThread);
End;

Function TLightWebServerDaemon.Start: Boolean;
Var
  ConfigFile : TStringList;
Begin
  Result := Inherited Start;
  AWriteLn('LightWebServer Start', Result);
  AWriteLn('Trying to load config');
  Try
    ConfigFile := TStringList.Create;
    ConfigFile.LoadFromFile(ConfigFileName);
    If ConfigFile.IndexOfName('DocRoot') > -1 Then
      DocRoot := UnQuote(ConfigFile.Values['DocRoot']);
    If ConfigFile.IndexOfName('IP') > -1 Then
      IP := UnQuote(ConfigFile.Values['IP']);
    If ConfigFile.IndexOfName('Port') > -1 Then
      Port := UnQuote(ConfigFile.Values['Port']);
    If ConfigFile.IndexOfName('Timeout') > -1 Then
      Timeout := StrToInt(UnQuote(ConfigFile.Values['Timeout']));
    If ConfigFile.IndexOfName('ThreadTimeout') > -1 Then
      ThreadTimeout := StrToInt(UnQuote(ConfigFile.Values['ThreadTimeout']));
    If ConfigFile.IndexOfName('MaxThreads') > -1 Then
      MaxThreads := StrToInt(UnQuote(ConfigFile.Values['MaxThreads']));
    AWriteLn('Config file loaded');
  Finally
    ConfigFile.Free;
  End;
  AWriteLn('Creating server daemon');
  fThread := THTTPServerThread.Create(DocRoot, IP, Port, TimeOut, ThreadTimeout, MaxThreads);
  AWriteLn('Server is running');
  fThread.OnTerminate := @ThreadStopped;
  fThread.FreeOnTerminate := False;
  fThread.Resume;
End;

Function TLightWebServerDaemon.Stop: Boolean;
Begin
  Result := Inherited Stop;
  AWriteLn('Daemon Stop: ', Result);
  AWriteLn('No incomming connections will be accepted now.');
  fThread.WaitForFinish;
  AWriteLn(
  'Waiting for pending threads: ' +
  IntToStr(fThread.RunningChilds) +
  ' threads pending - ');
  Repeat Until fThread.RunningChilds = 0;
  AWriteLn('Finished waiting for threads');
  fThread.Terminate;
End;

Function TLightWebServerDaemon.Pause: Boolean;
Begin
  Result := Inherited Pause;
  AWriteLn('Daemon pause: ', Result);
  fThread.Suspend;
End;

Function TLightWebServerDaemon.Continue: Boolean;
Begin
  Result := Inherited Continue;
  AWriteLn('Daemon continue: ', Result);
  fThread.Resume;
End;

Function TLightWebServerDaemon.Execute: Boolean;
Begin
  Result := Inherited Execute;
  AWriteLn('Daemon execute: ', Result);
End;

Function TLightWebServerDaemon.ShutDown: Boolean;
Begin
  Result := Inherited ShutDown;
  AWriteLn('Daemon Shutdown: ', Result);
  FThread.Terminate;
End;

Function TLightWebServerDaemon.Install: Boolean;
Begin
  Result := Inherited Install;
  AWriteLn('Daemon Install: ', Result);
End;

Function TLightWebServerDaemon.UnInstall: Boolean;
Begin
  Result := Inherited UnInstall;
  AWriteLn('Daemon UnInstall: ', Result);
End;

Type

  { TLightWebServerDaemonMapper }

  TLightWebServerDaemonMapper = Class(TCustomDaemonMapper)
    Constructor Create(AOwner : TComponent); Override;
  End;

{ TLightWebServerDaemonMapper }

Constructor TLightWebServerDaemonMapper.Create(AOwner: TComponent);

Var
  D : TDaemonDef;

Begin
  Inherited Create(AOwner);
  D                         := DaemonDefs.Add As TDaemonDef;
  D.DisplayName             := 'Standalone LightWebServer';
  D.Name                    := 'LightWebServer';
  D.DaemonClassName         := 'TLightWebServerDaemon';
  D.WinBindings.ServiceType := stWin32;
End;

Begin
  RegisterDaemonClass(TLightWebServerDaemon);
  RegisterDaemonMapper(TLightWebServerDaemonMapper);
  Application.Title := 'Standalone LightWebServer';
  Application.Run;
End.

