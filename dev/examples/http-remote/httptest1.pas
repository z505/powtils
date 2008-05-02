{ Simple http connection demo

  Author: Lars (L505)
          http://z505.com }
program httptest1; {$mode objfpc} {$H+}

uses pwhttp, pwhostname  ;

var h: HttpConnection;
    b: boolean;
    code: integer;
    url, s: string;
begin
  // open connection
  h:= HttpConnect('z505.com');

  // you could set these below on some site that needed them
  //  httpsetheader(h, 'Cookie', 'foo=bar; text=test');
  //  httpsetpostdata(h, 'variable1=value1&variable2=some_value');

  b:= HttpSendRequest(h, 'GET', '/');

  if not b then begin
    code := httpresponseinfo(h, url, s);
    case code of
      500: writeln('Internal server error');
      404: writeln('Page not found');
      301, 302: writeln('Redirect.. must do it yourself or see HttpGet()');
    end;
    writeln('response code: ', code);
    writeln('Redirect url: ', url);
    writeln('Response message: ', s);
  end;
  writeln(b);
  // write the page content
  while not HttpEOF(h) do writeln(httpreadln(h));

  // free connection 
  HttpClose(h);

end.
