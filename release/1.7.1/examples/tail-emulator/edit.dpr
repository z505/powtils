{ Extremely simple edit program, edits a text file in web browser

  Author: Lars (L505)
  http://z505.com                   } 

program edit; {$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF} {$APPTYPE CONSOLE}
uses
  pwinit in '..\..\main\pwinit.pas',
  pwmain in '..\..\main\pwmain.pas',
  strwrap1 in '..\..\main\strwrap1.pas',
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
  CurrentTime: string;
  hh, mm, ss, ms: word; //hours, minutes, seconds, milliseconds
  showlines: string;
  FirstTimeLoading: boolean;
  LastLines: string;
  StrArray: array of string;
  Edit1_Lines: string;


begin
  // must be edit text updates
  if GetCGIVar('update') = 'yes' then
  begin
    Edit1_Lines:= GetCGIVar_SafeHTML('Edit1');
    StrSaveFile(WatchFile, Edit1_Lines);
    webwriteln('File has been saved. Use your back button to edit again.');
    halt;
  end;

  //no edit text updates, must be just loading the page with plans to edit
  if IsCgiVar('update') = false then
    SetWebVar('Edit1_Text', StrLoadFile(WatchFile));

  WebFileOut('htm/EditPage_header.htm');
  WebWrite('<br />');

  DecodeTime(Time, hh, mm, ss, ms);
  CurrentTime:= format('<b>Time:</b> %d:%d:%d',[hh,mm,ss,ms]);
  WebWrite('<b>Date:</b> ' + DateToStr(Date) + ' &nbsp; &nbsp;' + CurrentTime );
  WebWrite(' &nbsp; &nbsp; <i>You can edit the test file being watched by EmuTail</i>' );
  webwriteln('<hr>');
  if FileExists(WatchFile) = false then
  begin
    webwrite('File does not exist: ' + WatchFile);
    halt;
  end;


  WebTemplateOut('htm/EditPage.htm', false);
  WebFileOut('htm/footer.htm');

end.

