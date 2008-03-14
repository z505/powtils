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
  FirstTimeLoading: boolean;
  LastLines: string;

begin

  FirstTimeLoading:= false;
  // if this is the first session
  if GetSess('OldFileSize') = '' then
  begin
    FirstTimeLoading:= true;
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
  SetSess('OldFileSize', IntToStr(OldFileSize));

  WebFileOut('htm/header.htm');
  WebWrite('<br />');

  showlines:= GetCGIVar('showlines');
  if showlines = '' then
    showlines:= '10'; //default 10 lines



  DecodeTime(Time, hh, mm, ss, ms);
  CurrentTime:= format('<b>Time:</b> %d:%d:%d',[hh,mm,ss,ms]);
  WebWrite('<b>Date:</b> ' + DateToStr(Date) + ' &nbsp; &nbsp;' + CurrentTime );
  WebWrite(' &nbsp; &nbsp; <i>Showing last <b>' + showlines + '</b> lines of file</i>' );
  webwriteln('<hr>');
  if FileExists(WatchFile) = false then
  begin
    webwrite('File does not exist: ' + WatchFile);
    halt;
  end;

{ //if the user wants to see all the lines
  if ShowLines = 'all' then
  begin
     webwriteln('<pre>');
     webfileout(WatchFile);
     webwriteln('</pre>');
  end else
}

  if (FileSizeChanged = true) then
  begin
    webwriteln('Notices: <font color=red>File changed!</font> <br /><br />');
    webwriteln('<table bgcolor=#B4B4B4 width=100% cellpadding=4>');
    webwriteln(  '<tr>');
    webwriteln(    '<td><font face="courier new" size=2>');
    LastLines:= GetLastLns(WatchFile, strtoint(showlines) );
    //convert carriage returns into HTML breaks
    LastLines:= LineEndToBR(LastLines);
    webwriteln(LastLines);
    webwriteln(    '</font></td>');
    webwriteln(  '</tr>');
    webwriteln('</table>');
  end else
  begin
    webwriteln('Notices: (none)<br /><br />');
    webwriteln('<table bgcolor=#B4B4B4 width=100% cellpadding=4>');
    webwriteln(  '<tr>');
    webwriteln(    '<td><font face="courier new" size=2>');
    LastLines:= GetLastLns(WatchFile, strtoint(showlines) );
    LastLines:= LineEndToBR(LastLines);
    webwriteln(LastLines);
    webwriteln(    '</font></td>');
    webwriteln(  '</tr>');
    webwriteln('</table>');
  end;



  WebWriteln('<br />');
  WebWriteln('<hr>');



  WebFileOut('htm/footer.htm');

end.

