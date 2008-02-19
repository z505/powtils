program CapstrDemo;
{$mode objfpc} {$H+}

uses
  capstr;

var
  buf: TCapstr;
begin

  resetbuf(@buf); // always reset before using capstring, if this was a built in type into the language this could be done automatically.
  addstr('test123', @buf); // add some data to the string
  addstr(' and testabc', @buf); // concat more data
  endupdate(@buf); // always clean up when done adding data
  writeln(buf.data); // buf.data can be accessed as read only if we have called EndUpdate()

  readln;

end.
