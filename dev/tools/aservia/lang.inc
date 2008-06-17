const
  VERSION = 'Aservia 1.0.1';
  AUTHORS = '/ Alexander N Zubakov, 2006; Lars Olson, 2008';
{$define lang_en}
{$ifdef lang_en}
  str_server = VERSION+' (en) '+AUTHORS;
  str_connect_success = '## Successful Connection!';
  str_qcom = 'Type q or quit to shutdown server';
  str_close = '## Shutdown Server';
  str_wait = '## Wait for connection... ';
  str_connection = '## Connection!';
  str_socket = '## Socket: ';
  str_runserver = '## Run server: ';
  str_requestfrom = '## Request from ';
  str_denied = '## Denied!'#13#10;
  str_request = '## Request: ';
{$else}
  str_server = VERSION+' (ru) '+AUTHORS;
  str_connect_success = '## Successful Connection!';
  str_qcom = 'Введите q или quit чтобы остановить сервер';
  str_close = '## Завершение работы сервера';
  str_wait = '## Ожидание соединения...';
  str_connection = '#####Соединение!';
  str_socket = '## Сокет: ';
  str_runserver = '## Запуск сервера: ';
  str_requestfrom = '## Запрос от ';
  str_denied = '## Доступ запрещен!'#13#10;
  str_request = '## Запрос: ';
{$endif}
