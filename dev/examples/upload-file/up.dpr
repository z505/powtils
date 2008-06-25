{ Demo showing upload file abilities

  NOTE: In CGI, you cannot download a file from within cgi-bin, you can only
  save it there. This demo therefore shows you how to sace a file to public
  html so you can download it too.

  Regards, Lars
  http://z505.com }

program up; {$ifdef fpc}{$mode objfpc}{$H+}{$endif} {$apptype console} 
uses pwinit, pwmain, pwtypes, pwenvvar, pwstrutil;

const WIDGET = 'widget1';

procedure OutBr(s: string); 
begin 
  Out(s); Out('<br />'); 
end;

procedure ShowForm;
begin
  Out('<FORM METHOD=POST ACTION="" enctype="multipart/form-data">' +
         '<input name='+WIDGET+' type=file size=20> <br /><br />'+
         '<input type=submit>' +
       '</FORM>');
  Out('<p>Click to upload file and get info</p> ');
end;

procedure ProcessUpfile;
var fname, path: astr;
begin
  fname:= '/test123456-'+GetUpFileName(WIDGET);
  // absolute path to save file in public html (document root)
  path:= SERV.DocRoot() + fname;
  OutBr('Upfile name: '+ GetUpFileName(WIDGET)); 
  OutBr('Upfile type: '+ GetUpFileType(WIDGET)); 
  OutBr('Upfile size: '+ i2s(GetUpFileSize(WIDGET))); 
  if SaveUpFile(WIDGET, path) then
    Out('Saved to: <a href="'+fname+'">'+fname+'</a> <br />' +
        'Absolute path: '+ path)
  else
    Out('Error saving file. example: directory may not exist <br />' +
        'Tried to save to: '+ path);
end;

begin
  // checks if user submitted file from form
  if IsUpFile(WIDGET) then ProcessUpFile else ShowForm;
end.
