(******************************************************************************
   Simple content management system
 ******************************************************************************
   See LICENSE.TXT for this demo

   Author: Lars aka L505
           http://z505.com   
*******************************************************************************)

unit htmout;  {$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF} 

interface

uses
  pwinit, pwmain, pwurlenc, pwsubstr, pwenvvar, cmsfilefuncs, pwfileutil,
  pwfileshare;

const
  PWRD = 'simplecms'; 
  // password.. change & encrypt it if you want security
  
  // cross platform directory separator
  {$IFDEF windows}
   SLASH = '\';
  {$ENDIF}
  {$IFDEF unix}
   SLASH = '/';
  {$ENDIF}

procedure NoCmsPage;
procedure ShowCms;
procedure WriteTopHeader;
procedure WriteFooter;

type 
  THtmEd = record 
    text: string;
  end;

var
 Edit1: THtmEd;           //gotten text from http post

 GotPw,                   //gotten password
 FileA: string;


implementation

var
  GotPg: string;   //gotten pagename

{ creates file, protecting with file sharing for multiple users }
procedure SharedFileCreate(fname: string);
var
  created: boolean;
begin
  FileMarkWrite(fname);
  created:= NewFile(fname);
  // if there was an error creating file...
  if not created then
  begin
    outln('Err 2: error creating file');
    Halt(2); //stop program
  end;
  FileUnmarkWrite(fname);
end;

procedure SharedFileOut(fname: string);
var
  k: word; // file sharing unique key
begin
  FileMarkRead(fname, k);
  FileOut(fname);
  FileUnMarkRead(fname, k);
end;

// if no page was specified, display default simple error
procedure NoCmsPage;
begin
  // note: could change to TemplateOut()
  out(  '<title>Simple CMS</title>');
  out('</head>');
  out('<body>');
  out(  '<FONT face=verdana SIZE="5"><b>Simple CMS</b></font>');
  out(  '<hr><font face=verdana>');
  out(  'No page was specified.');
  out(  'See the <a href="' + CGIEnvVar.ScriptName() + '?p=Main-Page">Main Page</a>');
  out(  '<hr>');
  out('</body>');
  out('</html>');
  halt;
end;

procedure SpacesToDashes(var s: string);
begin
  s:= SubStrReplace(s,' ', '-');
end;

procedure DashesToSpaces(var s: string);
begin
  s:= SubStrReplace(s,'-',' ');
end;

procedure CheckPageNames;
begin
   if length(GotPg)> 50 then //security measures for file name length
   begin
     outln('Page name too long. Please make page name shorter.');
     halt;
   end;
end;

{ makes path to content file name }
function MakePgFname: string;
begin
  result:= 'content' + SLASH + GotPg + '.htm'
end;

procedure WriteFooter;
begin
  TemplateOut('htminc/footer1.htm', true);
end;

procedure WritePassBox;
begin
  out('<p>Passcode: <INPUT NAME="pw" ROWS=1 COLS=30 >');
end;

procedure StartPostForm;
begin
  outln('<FORM action="' + CGIEnvVar.ScriptName()  + '?p=' + GotPg +'" method="post">'); //post editbox text to cgi app, signal an edit update with  ed=update
end;

procedure EndForm;
begin
  out('</FORM>');
end;

{ display an edit box with page content inside }
procedure WriteEdPage;

  procedure CorrectPgName;
  begin
    SpacesToDashes(GotPg); //first replace any spaces with dashes, so the correct dashed html file is loaded
    GotPg:= UrlDecode(GotPg); //decode incase any percent signs, special encoded characters like %20
    GotPg:= lowercase(gotpg);
  end;

  procedure WriteFormWidgets;
  begin
    // note: could be changed to TemplateOut()
    out('<TEXTAREA NAME="ed1" ROWS=20 COLS=85 >');
    out(FileA);//gotten text from file
    out('</TEXTAREA><br>');
    WritePassBox;
    out('</INPUT>');
    out('<INPUT NAME="ed" VALUE=update TYPE=hidden></input>');
    // in newer versions of Powtils, this hack below isn't needed, version 1.6.0.2 contained a small issue we workaround by using HIDDEN form inputs
    out('<INPUT NAME="p" VALUE="' + GotPg + '" TYPE=hidden></input>');
    out('&nbsp;&nbsp;&nbsp;<input type="submit" name="button" value="Submit"></input>');
  end;

begin
  if GetCgiVar('ed') = 'yes' then // the signal to edit a page
  begin
   { get the page content from the file... }
    CorrectPgName;
    FileA:= FileToString(MakePgFname);
    StartPostForm;
    WriteFormWidgets;
    EndForm;
    out('<hr>');
  end;
end;

procedure WriteTopHeader;
begin
  out('<html>');
  out('<head>');
end;

procedure VerifyPass;
begin
  GotPw:= GetCgiVar('pw');
  // verify posted password
  if GotPw <> PWRD then
  begin
    outln('Mom says: password wrong! Try again darling.');
    halt;
  end;
end;

{ output the file if it exists, create it otherwise }
procedure WriteFileContent;

  procedure NewPageFile;

    procedure AskUser;
    begin
      out('<p>Page does not exist.');
      out('<p>Do you want to create the new page?');
      StartPostForm;
      WritePassBox;
      out('&nbsp;&nbsp;');
      // could be changed to webtemplateout
      out('<INPUT TYPE="submit"> <INPUT TYPE="hidden" NAME="command" VALUE="newpage" >');
      // in newer versions of Powtils, this hack isn't needed, version 1.6.0.2 contained a small issue we workaround by using HIDDEN form inputs      webwrite('<INPUT NAME="p" VALUE="' + GotPg + '" TYPE=hidden></input>');
      EndForm;
    end;

  begin
    // if hidden form input signals this then make new page
    if  GetCgiVar('command') = 'newpage' then
    begin
      VerifyPass;
      SharedFileCreate(MakePgFname); //create PageName.htm since a new page was requested. This ensures a file can be created, and checking that it can be created is important since a stringlist.savetofile wouldn't offer this check.
      // friendly page title without dashes
      DashesToSpaces(GotPg);
      FileA:= FileA + '<pre>'#13#10'This is a newly created page: ' + GotPg + #13#10 + ' <i>Enter text here</i>'+ #13#10 +'</pre>';
      // but save file as dashed
      SpacesToDashes(GotPg);
      StringToFile(MakePgFname, FileA);
      SharedFileOut(MakePgFname); //output the htm file
    end else
      AskUser;
  end;

begin
  GotPg:= UrlDecode(GotPg); //first decode %20 and special characters
  SpacesToDashes(GotPg);
  GotPg:= lowercase(GotPg); // case sensitivity sucks
  if FileExists_read(MakePgFname) then
    SharedFileOut(MakePgFname) //output htm file
  else // create file if doesn't exist
    NewPageFile;
end;


procedure ShowCms;

  { corrects page name to contain spaces insteaed of dashes and trims bad characters, makes case insensitive }
  procedure CorrectPageName(var s: string);
  begin
    DashesToSpaces(s); //friendly page name without dashes
    s:= SubstrReplace(s, #0, '000'); //null character bad, 000 makes it easy to spot hackers
    s:= lowercase(s);
  end;

  { check password first }
  procedure CheckPass;
  begin
    if GetCgiVar('ed') = 'update' then
    begin
      if IsWebVar('pw') > 0 then
        VerifyPass
      else //password not found, maybe user trying to edit page maliciously
      begin
        outln('Err 1: password not found');
        halt(1);
      end;
    end;
  end;

  procedure UpdatePageContent;

    procedure CorrectPgToSave(var pgname: string);
    begin
      SpacesToDashes(pgname); //first replace spaces with dashes
      pgname:= lowercase(pgname); // UNIX case sensitivity sucks
    end;

  begin
    if IsWebVar('ed') > 0 then
    begin
      // check signal for page update
      if GetCgiVar('ed') = 'update' then
      begin
        // get page content from edit box that was edited
        edit1.text:= GetCgiVar_S('ed1', 0); // unsecure, security set to zero because this is a private CMS allowing any java script injections or null characters or other bad crap
        FileA:= edit1.Text;
        CorrectPgToSave(GotPg);
        StringToFile(MakePgFname, FileA);
      end;
    end;
  end;

var
  GotPgDashed: string;
begin
  GotPg:= GetCgiVar_S('p', 0);  // retrieve p variable unfiltered unsecure
  GotPg:= TrimBadChars_file(GotPg); //replace any BAD CHARACTERS from the file name (local directory safety ../ )

  if GotPg = '' then  // display error
    NoCmsPage;
  CorrectPageName(GotPg);
  GotPgDashed := GotPg;
  SpacesToDashes(GotPgDashed);
  SetWebVar('_GotPg', GotPg);
  SetWebVar('_GotPgDashed', GotPgDashed);
  TemplateOut('htminc/header1.htm', true);
  CheckPass;
  CheckPageNames;
  UpdatePageContent;
  WriteEdPage;
  WriteFileContent;
end;


end.
