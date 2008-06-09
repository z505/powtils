{ Extremely simple edit program, edits a text file in web browser

  Author: Lars (L505)
  http://z505.com                   } 

program edit; {$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF} {$APPTYPE CONSOLE}
uses
  pwinit, pwmain, strwrap1, sysutils;

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
  Edit1_Lines: string;


begin
  // must be edit text updates
  if GetCGIVar('update') = 'yes' then
  begin
    Edit1_Lines:= GetCGIVar_SafeHTML('Edit1');
    StrSaveFile(WatchFile, Edit1_Lines);
    outln('File has been saved. Use your back button to edit again.');
    halt;
  end;

  //no edit text updates, must be just loading the page with plans to edit
  if ispostvar('update') = false then
    setVar('Edit1_Text', StrLoadFile(WatchFile));

  fileOut('htm/EditPage_header.htm');
  out('<br />');

  DecodeTime(Time, hh, mm, ss, ms);
  CurrentTime:= format('<b>Time:</b> %d:%d:%d',[hh,mm,ss,ms]);
  out('<b>Date:</b> ' + DateToStr(Date) + ' &nbsp; &nbsp;' + CurrentTime );
  out(' &nbsp; &nbsp; <i>You can edit the test file being watched by EmuTail</i>' );
  outln('<hr>');
  if FileExists(WatchFile) = false then
  begin
    out('File does not exist: ' + WatchFile);
    halt;
  end;


  templateOut('htm/EditPage.htm', false);
  fileOut('htm/footer.htm');

end.

