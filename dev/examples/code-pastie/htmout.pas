(****************************************************************************** 
   Code Pastie
 ******************************************************************************
  UNIT WAS CREATED IN A RUSH, YEARS AGO. NOT SUPER GOOD CODE.

   Author: Lars aka L505
           http://z505.com   
           See LICENSE.TXT (NRCOL public domain)
*******************************************************************************)

unit htmout; {$ifdef FPC}{$mode objfpc}{$H+}{$endif}

interface

uses
  pwtypes;

const
  PWRD = 'pastie'; // password .. change this and encrypt it if you want more security
  PASTIE_EXT = '.htm';
  PASTIE_DIR = 'pastes/';
  BLACK_BODY = '<body bgcolor=black text=gray>';

procedure NoPastiePage;
procedure ShowPastie;
procedure WriteTopHeader;
procedure WriteFooter;
procedure ShowPastieCssHtm;

type 
  THtmEd = record 
    text: astr;
  end;

var
 Edit1: THtmEd;           //gotten text from http post
 F: Longint;              //general file handle
 GotPw,                   //gotten password
 FileA: astr;


implementation

uses
  pwmain, pwurlenc, sysutils, pwenvvar, filefuncs, pwfileshare, shownew, 
  PasHiliter, pwfileutil;

var GotPg: astr;           //gotten pagename

// if no page was specified, display default simple error
procedure SharedFileOut(fname: astr);
var 
  k: word; // file sharing unique key
begin
  FileMarkRead(fname, k);
  out('<pre>');
  out(PasFileToHtmStr(fname));
  FileUnMarkRead(fname, k);
  out('</pre>');
end;


procedure SrcOut(fname: astr);
var k: word; // file sharing unique key
begin
  FileMarkRead(fname, k);
  out(PasFileToHtmStr(fname));
  FileUnMarkRead(fname, k);
end;

// if no page was specified, display default simple error
procedure NoPastiePage;
begin
  // note: could change to WebTemplateOut
  out(  '<title>Code Pastie</title>');
  out('</head>');
  out( BLACK_BODY);
  out(  '<FONT face=verdana SIZE="5"><b>Code Pastie</b></font>');
  out(  '<hr><font face=verdana>');
  out(  'No page was specified.');
  out(  'See the <a href="' + SERV.ScriptName() + '?p=main">Main Page</a>');
  out(  '<hr>');
  out('</body>');
  out('</html>');
  halt;
end;


procedure CheckPageNames;
begin
  if length(GotPg) > 4 then // pastie ID's can only be 4 characters long
  begin
    webwriteln('Page name too long, or too many pasties in our database.' + ' Please make page name shorter or ask administrator to delete some pasties.');
    halt;
  end;

end;

procedure CheckIfEditable;

  procedure NoEditAllowed;
  begin
    webwrite('<p>');
    webwrite('You can''t edit the "main" page, and you can''t edit the "show new pasties" page.' + ' Sorry! Please edit pasties with a specific ID.');
    halt;
  end;

begin
  if (lowercase(GotPg)) = 'main' then // can't edit main page
    NoEditAllowed;

  if (lowercase(GotPg)) = 'new' then // can't edit main page
    NoEditAllowed;

end;

function IsShowNew: boolean;
begin
  result:= false;
  if (lowercase(GotPg)) = 'new' then 
  begin
    ShowNewPasties;
    result:= true;
  end;
end;

{ makes path to pastes in database (actually just plain files) }
function MakePgFname: astr;
begin
  // windows/unix cross platform path
  result:= PASTIE_DIR + GotPg + PASTIE_EXT;  xpath(result);
end;

procedure WriteFooter;
var path: astr;
begin
  // windows/unix cross platform path
  path:= 'htminc/footer1.htm'; xpath(path);
  templateout(path, true);
end;

procedure WritePassBox;
begin
  out('<p>Passcode: <INPUT TYPE=password NAME="pw" ROWS=1 COLS=30 >');
end;

procedure StartPostForm;
begin
  out('<FORM action="'+ SERV.ScriptName()+'?p='+GotPg+'" method="post">'); 
end;

procedure EndForm;
begin
  out('</FORM>');
end;

{ display an edit box with page content inside }
procedure WriteEdPage;

  procedure CorrectPgName;
  begin
    GotPg:= StringReplace(GotPg,' ', '-',[rfReplaceAll]); //first replace any spaces with dashes, so the correct dashed html file is loaded
    GotPg:= UrlDecode(GotPg); //decode incase any percent signs, special encoded characters like %20
    GotPg:= lowercase(gotpg);
  end;

  procedure WriteFormWidgets;
  begin
    out('<TEXTAREA NAME="ed1" ROWS=25 COLS=85>');
    out(    FileA);//gotten text from file
    out('</TEXTAREA><br>');
    WritePassBox;
    out('<INPUT NAME="ed" VALUE=update TYPE=hidden>');
    // in newer versions of Powtils, this hack below isn't needed, version 1.6.0.2 contained a small issue we workaround by using HIDDEN form inputs
    //out('<INPUT NAME="p" VALUE="' + GotPg + '" TYPE=hidden>');
    outln('&nbsp;&nbsp;&nbsp;<input type="submit" name="button" value="Submit">');
  end;

begin
  if GetCgiVar('ed') = 'yes' then // the signal to edit a page
  begin
    // only pages that are editable, i.e. main page not 
    CheckIfEditable;
   { get the page content from the file... }
    CorrectPgName;
    FileA:= FileToString(MakePgFname);
    StartPostForm;
    WriteFormWidgets;
    EndForm;
    webwrite('<hr>');
  end;
end;

procedure WriteTopHeader;
begin
  webwriteln('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">');
  webwrite('<html>');
  webwrite('<head>');
end;

procedure VerifyPass;
begin
  GotPw:= GetCgiVar('pw');
  // verify posted password
  if GotPw <> PWRD then 
  begin
    webwriteln('Brian Kernigoon says: password wrong! Try again or I''ll slap you with a C!');
    webwriteln('<br>*applause*');
    halt; {wrong password, exit}
  end;
end;

procedure TryCreateFile(const fname: astr);
var k: word; // file sharing unique key
begin
  FileMarkRead(fname, k);
  F:= FileCreate(fname); //create PageName.htm since a new page was requested. This ensures a file can be created, and checking that it can be created is important since a stringlist.savetofile wouldn't offer this check.
  FileUnMarkRead(fname, k);
  // if there was an error creating file...
  if F = -1 then
  begin
    outln('Err 2: error creating file'); 
    Halt; 
  end;
  FileClose(F);  
end;

{ output the file if it exists, create it otherwise }
procedure WriteFileContent;

  procedure NewPageFile;

    procedure AskUser;
    begin
      out('<p>Pastie does not exist.');
      out('<p>Do you want to create new pastie entry with above ID?');
      StartPostForm;
      WritePassBox;
      out('&nbsp;&nbsp;');
      // in newer versions of Powtils, this hack below isn't needed, version 1.6.0.2 contained a small issue we workaround by using HIDDEN form inputs
      //out('<INPUT NAME="p" VALUE="' + GotPg + '" TYPE=hidden>');
      out('<INPUT TYPE="submit"> <INPUT TYPE="hidden" NAME="command" VALUE="newpage" >');
      EndForm;
    end;

  begin
    if  GetCgiVar('command') = 'newpage' then 
    begin
      VerifyPass;
      TryCreateFile(MakePgFname);
      GotPg:= StringReplace(GotPg,'-',' ',[rfReplaceAll]); //first replace any dashes from the safe page name to a space
      FileA:= FileA + 'This is a newly created pastie: ' + GotPg + ' ...Enter code here';
      GotPg:= StringReplace(GotPg,' ','-',[rfReplaceAll]); //replace spaces with dashes now, in order to save file as dashed
      StringToFile(MakePgFname, FileA);
      SharedFileOut(MakePgFname); //output the htm file
     
    end else
      // only ask user if this isn't the "show new pasties" page
      if not IsShowNew then AskUser;  
  end;

begin
  GotPg:= UrlDecode(GotPg); //first decode %20 and special characters into normal characters
  GotPg:= StringReplace(GotPg,' ','-',[rfReplaceAll]); //replace spaces with dashes now, in order to save file as dashed
  GotPg:= lowercase(GotPg); // case sensitivity sucks
  if FileExists(MakePgFname) then
    SharedFileOut(MakePgFname) //output the htm file
  else // create file if doesn't exist
    NewPageFile;
end;

procedure stylesheetout;
begin
  { your choice for stylesheet type..}

  // a file...
  //  out('<LINK rel="stylesheet" href="/cgi-htm/code-pastie/style.css">');

  // or embedded
  fileout('styleembed.inc');
end;

procedure HaltErr(s: astr);
begin
  outln('ERR: ' + s);
  halt;
end;



function curpastiefile: astr;
begin
  result:= getcgivar('viewcss');
  if result = '' then HaltErr('no pastie ID specified');
  result:= PASTIE_DIR + result + PASTIE_EXT; xpath(result);
end;


procedure ShowPastieCssHtm;
begin
  out('<html>');
  out(BLACK_BODY);

  out('<b>CSS CONFIG:</b>');
  out('<br />');  
  out('<textarea ROWS=15 style="width:95%">');
  fileout('styleembed.inc');
  out('</textarea>');

  out('<br />');
  out('<br />');

  out('<b>HIGHLIGHTED HTML:</b>');
  out('<br />');
  out('<textarea ROWS=25 style="width:95%">');
  outln( '<pre>');
  srcout(   curpastiefile);
  outln( '</pre>');
  out('</textarea>');
  out('</body>');
  out('</html>');
end;

procedure ShowPastie;

  { corrects the page name to contain no spaces and trims bad characters, makes case insensitive }
  procedure CorrectPageName(var s: astr);
  begin
    s:= StringReplace(s, '-', ' ',[rfReplaceAll]); //first replace any dashes with spaces, so html page shows spaces version
    s:= StringReplace(s, #0, '000',[rfReplaceAll]); //null character bad, 000 makes it easy to spot hackers
    s:= lowercase(s);
  end;
  
  { check password first }
  procedure CheckPass;
  begin
    if GetCgiVar('ed') = 'update' then
    begin
      if IsCgiVar('pw') then
        VerifyPass
      else //password not found, maybe user trying to edit page maliciously
      begin
        outln('Err 1: password not found'); 
        halt(1);
      end;
    end;
  end;

  procedure UpdatePageContent;
    procedure CorrectPgToSave(var pgname: astr);
    begin
      pgname:= StringReplace(pgname,' ', '-',[rfReplaceAll]); //first replace spaces with dashes
      pgname:= lowercase(pgname); // UNIX case sensitivity sucks
    end;
  begin
    if IsCgiVar('ed') then
    begin
      // check signal for page update
      if GetCgiVar('ed') = 'update' then 
      begin
        {get the page content from edit box that was updated...}
        edit1.text:= GetCgiVar_S('ed1', 0); // unsecure, security set to zero because this is a private code pastie allowing any java script injections or null characters or other bad, well, shit
        FileA:= edit1.Text;
        CorrectPgToSave(GotPg);
        StringToFile(MakePgFname, FileA);
      end;
    end;
  end;


begin
  GotPg:= GetCgiVar_S('p', 0);  // retrieve p variable unfiltered unsecure
  GotPg:= TrimBadChars_file(GotPg); //replace any BAD CHARACTERS from the file name (local directory safety ../ )

  // incase a bad page
  if GotPg = '' then NoPastiePage;

  CorrectPageName(GotPg);
  out('<title>Code Pastie : '+ GotPg +'</title>');
  stylesheetout;
  out('</head>');

  SetVar('_GotPg', GotPg);
  TemplateOut('htminc/header1.htm', true);
  CheckPass;  
  CheckPageNames;
  UpdatePageContent;
  WriteEdPage;
  WriteFileContent;
end;




end.
