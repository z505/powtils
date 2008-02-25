unit htmout; {$mode objfpc} {$H+}

interface

procedure StartPage;
procedure EndPage;
procedure JotForm;
procedure Notify;

implementation

uses
  pwmain;

procedure StartPage;
begin
  webwrite(
    '<html>' +
      '<body>');
end;

{ show input form by default }
procedure JotForm;
begin
  outf(
        '<FORM METHOD=POST ACTION="">' +
            '<b>Web Command</b> (eg. ls, pwd, mv, cp, zip, unzip): <br>' +
            '<INPUT TYPE=text NAME=ed1 value="$remembercmd" style="width:100%;"><br>' +
            '<INPUT TYPE=hidden name=form1posted value=yes>' +
            '<INPUT TYPE=submit VALUE=run>' +
        '</FORM>');
end;

procedure Notify;
begin
  webwrite('<br>Note: command attempted! Results of your command should be above.');
end;

procedure EndPage;
begin
  webwrite(
      '</body>' +
    '</html>');
end;

end.
