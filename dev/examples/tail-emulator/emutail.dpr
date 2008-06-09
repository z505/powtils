{ Extremely simple Tail emulator (log file or text file monitoring program for 
  the web browser 

  Author: Lars (L505)
  http://z505.com                   } 


program EmuTail; {$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF} {$APPTYPE CONSOLE}


uses
  pwinitall,
  pwmain,
  pwsdssess,
  strwrap1,
  sysutils;



// get the size of any file, doesn't matter whether it's a text or binary file.
function GetFileSize(FileName: string): int64;
var
  FileInfo:TSearchRec;
begin
  result:= 0;
  FindFirst(FileName, faAnyFile, FileInfo);
  result:= FileInfo.Size ;
end;


const
  WatchFile = 'test.txt';
var
  CurrentFileSize,
  OldFileSize: longint;
  FileSizeChanged: boolean; //for the infinite loop, until we want to kill program
  
var
  CurrentTime: string;
  hh, mm, ss, ms: word; //hours, minutes, seconds, milliseconds
  showlines: string;
  LastLines: string;

begin

  // if this is the first session
  if GetSess('OldFileSize') = '' then
  begin
    OldFileSize:= GetFileSize(WatchFile)
  end else
  // if we have already been here before via a session
  begin
    OldFileSize:= StrToInt(GetSess('OldFileSize'));
  end;

  SetSess('OldFileSize', IntToStr(OldFileSize));


  // obtain the target file size on the server
  CurrentFileSize:= GetFileSize(WatchFile);
  // determine if the file size has changed. If so, notify user
  if CurrentFileSize <> OldFileSize then
    FileSizeChanged:= true;
  OldFileSize:= GetFileSize(WatchFile);

  // record the file size
  setSess('OldFileSize', IntToStr(OldFileSize));

  fileOut('htm/header.htm');
  out('<br />');

  showlines:= GetCGIVar('showlines');
  if showlines = '' then
    showlines:= '10'; //default 10 lines



  DecodeTime(Time, hh, mm, ss, ms);
  CurrentTime:= format('<b>Time:</b> %d:%d:%d',[hh,mm,ss,ms]);
  out('<b>Date:</b> ' + DateToStr(Date) + ' &nbsp; &nbsp;' + CurrentTime );
  out(' &nbsp; &nbsp; <i>Showing last <b>' + showlines + '</b> lines of file</i>' );
  out('<hr>');
  if FileExists(WatchFile) = false then begin
    out('File does not exist: ' + WatchFile);
    halt;
  end;

{ //if the user wants to see all the lines
  if ShowLines = 'all' then
  begin
     outln('<pre>');
     webfileout(WatchFile);
     outln('</pre>');
  end else
}

  if (FileSizeChanged = true) then
  begin
    outln('Notices: <font color=red>File changed!</font> <br /><br />');
    outln('<table bgcolor=#B4B4B4 width=100% cellpadding=4>');
    outln(  '<tr>');
    outln(    '<td><font face="courier new" size=2>');
    LastLines:= GetLastLns(WatchFile, strtoint(showlines) );
    //convert carriage returns into HTML breaks
    LastLines:= LineEndToBR(LastLines);
    outln(LastLines);
    outln(    '</font></td>');
    outln(  '</tr>');
    outln('</table>');
  end else
  begin
    outln('Notices: (none)<br /><br />');
    outln('<table bgcolor=#B4B4B4 width=100% cellpadding=4>');
    outln(  '<tr>');
    outln(    '<td><font face="courier new" size=2>');
    LastLines:= GetLastLns(WatchFile, strtoint(showlines) );
    LastLines:= LineEndToBR(LastLines);
    outln(LastLines);
    outln(    '</font></td>');
    outln(  '</tr>');
    outln('</table>');
  end;



  outln('<br />');
  outln('<hr>');



  fileOut('htm/footer.htm');

end.

