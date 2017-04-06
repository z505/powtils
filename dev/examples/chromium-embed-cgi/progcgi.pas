// This program's output will be loaded into chromium embedded, similar to how
// a cgi program is loaded into a web server
// Todo: for now this just outputs html. Need to add all cgi features such as
//       url variables, post/get, etc.

program progcgi;
{$mode objfpc} {$H+}
uses
  Sysutils;

procedure DisplaySVG;
var w, h, color: string;
begin
  w := GetEnvironmentVariable('SVGWIDTH');
  h := GetEnvironmentVariable('SVGHEIGHT');
  color := GetEnvironmentVariable('SVGCOLOR');
  writeln(
   '<svg width="'+w+'" height="'+h+'">'+
     '<circle cx="50" cy="50" r="40" stroke="green" stroke-width="4" fill="'+color+'" />' +
   '</svg>'
  );
end;


begin
  writeln('<html><head></head>');
  writeln('<body>');
  writeln(  'testing <b>simple</b> html program');
  DisplaySVG;
  writeln('</body>');
  writeln('</html>');
end.

