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
      '<body style="font-family: verdana, arial, sans-serif;">');
end;

{ show input form, with any special chars formated/filtered to html entities }
procedure JotForm;
begin
  Out(
        '<FORM METHOD=POST ACTION="">' +
            '<b>Web Command</b> (i.e. ls, pwd, mv, cp, zip, tar) ' +
            '<small><i>Tip: {$DOCROOT} or $DOCROOT refers to DOCUMENT_ROOT</i></small><br />');
  OutF(     '<INPUT TYPE=text NAME=ed1 value="$remembercmd" style="width:100%;"><br>', @FilterHtml);
  Out(      '<INPUT TYPE=hidden name=form1posted value=yes>' +
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
