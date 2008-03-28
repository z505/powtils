{$MODE OBJFPC}
{$SMARTLINK ON}
{$LONGSTRINGS ON}
unit shell;
interface
  var
    //Путь к интерпретатору скриптов командной строки
    PathToShell: string = '/bin/sh';

  //Модифицирует окружение путем добавления переменной
  //с именем name и значением value
  //Изменения касаются ТОЛЬКО исполняемой команды
  procedure addEnv(name, value: string);

  //Очищает все переменные окружения, установленные
  //процедурой addEnv
  procedure clearEnv;
  
  //Выполняет процесс/программу/команду оболочки command_
  //Возвращает код завершения процесса
  function cmd(command_: string): integer;
  
  //Выполняет процесс/программу/команду оболочки command_ и
  //возвращает весь консольный вывод выполняемой программы
  function command(command_: string): string;

implementation
  uses
    pwfileutil, pwstrutil;

  var
    LocalEnv  : string  = {$ifndef mswindows}'env '{$else}''{$endif};
    changedEnv: boolean = false;
    
  procedure addEnv(name, value: string);
  begin
    LocalEnv += {$ifndef mswindows}
      name + '="' + value + '" ';
    {$else}
      'set ' + name + '=' + value + #13#10;
    {$endif}
    
    changedEnv := true;
  end;

  procedure clearEnv;
  begin
    LocalEnv  := {$ifndef mswindows}'env '{$else}''{$endif};
    changedEnv:= false;
  end;
    
  function cmd(command_: string): integer;
  var
    fname: string = {$ifndef mswindows}'tmp_JKfjCGeh__cmd.sh'{$else}'tmp_JKfjCGeh__cmd.bat'{$endif};
    f    : text;
  begin
    randomize;
    while FileThere(fname, fmR) do
      fname := '_' + IntToStr(random(10000)) + fname;

    Assign(f, fname);
      Rewrite(f);
      {$ifdef mswindows}Writeln(f, '@echo off');{$endif}
      if changedEnv then Write(f, LocalEnv);
      Writeln(f, command_);
    Close(f);

    Result := {$ifdef mswindows}ExecuteProcess('./' + fname, '')
              {$else}ExecuteProcess(PathToShell, fname){$endif};

    Erase(f);
  end;

  function command(command_: string): string;
  var
    f    : file;
    buf  : string;
    count: integer;
    fname: string = 'tmp_JKfjCGeh__pipe.txt';
  begin
    randomize;
    while FileThere(fname, fmR) do
      fname += IntToStr(random(10000)) + '.txt';

    cmd(command_ + {$ifdef mswindows}'>'{$else}' > '{$endif} + fname);
    Assign(f, fname);
    Reset(f, 1);

    Result := '';
    SetLength(buf, 4096);
    while not eof(f) do
    begin
      BlockRead(f, pointer(buf)^, 4096, count);
      if count < 4096 then buf := copy(buf, 1, count);
      Result += buf;
    end;

    Close(f);
    Erase(f);
  end;
end.
