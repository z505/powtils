(******************************************************************************
   Static site generator
 ******************************************************************************
   Released under bsd or mit license (take your pick)

   Author: Lars aka L505
           http://z505.com   
*******************************************************************************)

unit htmout;  {$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF} 

{$I ..\..\main\DelphiDefines.inc}

interface

uses
  pwinit, pwmain, pwurlenc, pwsubstr, pwenvvar, cmsfilefuncs, pwfileutil,
  pwfileshare {$IFNDEF FPC}, sysutils{$ENDIF};

const
  // cross platform directory separator (retarded operating system incompatibilites can go fuck themselves)
  {$IFDEF windows}SLASH = '\';{$ENDIF}
  {$IFDEF unix}   SLASH = '/';{$ENDIF}

  OUTPUT_DIR_CFG_FILE = 'output-dir.cfg';
  HTM_HEADER_FILE = 'htminc'+SLASH+'htmheader1.htm';
  HEADER_FILE = 'htminc'+SLASH+'header1.htm';
  TITLE_FILE = 'htminc'+SLASH+'title.htm';
  FOOTER_FILE = 'htminc'+SLASH+'footer1.htm';

  HTM_EXT = '.htm'; // html file extension

  // password.. change & encrypt it if you want secure site from hacker faggots
  PWRD = 'static-site';


procedure InitMacroVars;
procedure NoCmsPage;
procedure WriteTitle;
procedure ShowCms;
procedure WriteTopHeader;
procedure WriteFooter;

type
  THtmEd = record 
    text: string;
  end;

var
 Edit1: THtmEd;    //retrieved text from http post

 GotPw,            //retrieved password from http post
 Article: string;


implementation

uses
  compactutils, strwrap1; // PStrList

var
  GotPg: string;   // pagename from p=page in url variable
  GotPgDashed: string; // page name with dashes

procedure outbr(s: string);
begin
  outln(s);
  outln('<br />');
end;

procedure OutErr(s: string);
begin
  outbr('<b>Error: '+s+'</b>');
end;

{ creates file, protecting with file sharing for multiple users
  TODO: fix this: use new fpc file sharing/locking mechanism. Below is unreliable }
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

{ TODO: fix this: use new fpc file sharing/locking mechanism. Below is unreliable }
procedure SharedFileOut(fname: string);
var
  k: word; // file sharing unique key
begin
  FileMarkRead(fname, k);
  FileOut(fname);
  FileUnMarkRead(fname, k);
end;

procedure SpacesToDashes(var s: string);
begin
  s:= SubStrReplace(s,' ', '-');
end;

procedure DashesToSpaces(var s: string);
begin
  s:= SubStrReplace(s,'-',' ');
end;

procedure InitMacroVars;
begin
  GotPg:= GetPostVar_S('p', 0);  // retrieve p variable unfiltered unsecure
  GotPg := SubstrReplace(GotPg, #0, '000'); //null character bad, 000 makes it easy to spot hackers
  GotPg:= TrimBadFile(GotPg); //replace any BAD CHARACTERS from the file name (local directory safety ../ )
  GotPg := lowercase(GotPg);
  GotPg := UrlDecode(GotPg); //decode incase any percent signs, special encoded characters like %20

  DashesToSpaces({var}GotPg);
  // keep on hand a variable that has spaces converted to dashes
  GotPgDashed := GotPg;
  SpacesToDashes({var}GotPgDashed);

  SetVar('_GotPg', GotPg);
  SetVar('_GotPgDashed', GotPgDashed);
  SetVar('_Prog', SERV.ScriptName());
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
  out(  'See the <a href="' + SERV.ScriptName() + '?p=Main-Page">Main Page</a>');
  out(  '<hr>');
  out('</body>');
  out('</html>');
  halt;
end;

procedure CheckPageNames;
begin
   if length(GotPg) > 50 then //security measures for file name length, fuck hacker faggots
   begin
     outln('Page name too long. Please make page name shorter.');
     halt;
   end;
end;

{ makes path to content file name }
function MakePgFname: string;
begin
  result:= 'content' + SLASH + GotPgDashed + HTM_EXT;
end;

procedure WriteFooter;
begin
  TemplateOut(FOOTER_FILE, true);
end;

procedure WritePassBox;
begin
  out('<p>Passcode: <INPUT NAME="pw" ROWS=1 COLS=30 >');
end;

procedure StartPostForm;
begin
  outln('<FORM action="' + SERV.ScriptName()  + '?p=' + GotPgDashed +'" method="post">'); //post editbox text to cgi app, signal an edit update with  ed=update
end;

procedure EndForm;
begin
  out('</FORM>');
end;

{ display an edit box with page content inside }
procedure WriteEdPage;

  procedure WriteFormWidgets;
  begin
    // note: could be changed to TemplateOut()
    out('<TEXTAREA NAME="ed1" ROWS=20 COLS=85 >');
    out(Article); // retrieved content from file
    out('</TEXTAREA><br>');
    WritePassBox;
    out('</INPUT>');
    out('<INPUT NAME="ed" VALUE=update TYPE=hidden></input>');
    // in newer versions of Powtils, this hack below isn't needed, version 1.6.0.2 contained a small issue we workaround by using HIDDEN form inputs
    out('<INPUT NAME="p" VALUE="' + GotPgDashed + '" TYPE=hidden></input>');
    out('&nbsp;&nbsp;&nbsp;<input type="submit" name="button" value="Submit"></input>');
  end;

begin
  if GetPostVar('ed') = 'yes' then // the signal to edit a page
  begin
   { get the page content from the file... }
    Article := FileToString(MakePgFname);
    StartPostForm;
    WriteFormWidgets;
    EndForm;
    out('<hr>');
  end;
end;

procedure WriteTopHeader;
begin
  FileOut(HTM_HEADER_FILE);
end;

procedure WriteTitle;
begin
  TemplateOut(TITLE_FILE);
end;

procedure VerifyPass;
begin
  GotPw:= GetPostVar('pw');
  // verify posted password
  if GotPw <> PWRD then
  begin
    outln('Mom says: password wrong! Try again darling.');
    halt;
  end;
end;

// Takes a string of html (content) and then builds an html file with
// headers/footers and other includes
// Returns false if problem saving file
function MakeHtmlFile(fname: string): boolean;
var
  htmhead, title, head, foot, outfile: PStrList;
  filefound: boolean;

  procedure LoadFile(filename: string; sl: PStrList);
  begin
    if FileExists_read(filename) then begin
      sl^.LoadFromFile(filename);
      filefound := true;
    end else begin
      filefound := false;
      OutErr('Could not find file: '+ filename);
    end;
  end;

var
  saved: boolean;
begin
  result := false;
  filefound := false;
  htmhead := NewStrList;
  title := NewStrList;
  head := NewStrList;
  foot := NewStrList;
  outfile := NewStrList;

  LoadFile(HTM_HEADER_FILE, htmhead);

  if filefound then LoadFile(TITLE_FILE, title);
  if filefound then LoadFile(HEADER_FILE, head);
  if filefound then LoadFile(FOOTER_FILE, foot);

  if filefound then begin
    // build html file in a PStrList
    outfile^.AddStrings(htmhead);
    outfile^.AddStrings(title);
    outfile^.AddStrings(head);
    outfile^.Add(Article);
    outfile^.AddStrings(foot);
    // expand $macrovars
    outfile^.Text := Fmt(outfile^.Text);
    result := outfile^.SaveToFile(fname);
  end else begin
    OutErr('Could not find one or more files in htminc/ directory or the main content file');
  end;

  htmhead^.free ;
  title^.free;
  head^.free;
  foot^.free;
  outfile^.free;
end;

// returns err as true if problem, returns string of file path to html output folder
function GetPublicHtmlOutputFolder(var err: boolean): string;
begin
  result := '';
  err := true;
  // get fourth line of config file
  result := GetLnN(4, OUTPUT_DIR_CFG_FILE);
  if result = FILE_ERR then begin
    OutErr('problem opening file: '+OUTPUT_DIR_CFG_FILE);
  end else begin
    err := false;
  end;
end;

{$IFDEF windows}
// correct slashes "/" to "\" on windows
function CorrectDirSep(fpath: string): string;
begin
  result := SubstrReplace(fpath, '/', '\');
end;
{$ENDIF}

// returns err as false if problem
function HtmOutputDir(var err: boolean): string;
begin
  result := SERV.DocRoot();
  result := IncludeTrailingPathDelimiter(result);
  result := result + GetPublicHtmlOutputFolder(err);
 {$IFDEF windows}
  result := CorrectDirSep(result);
 {$ENDIF}
  result := IncludeTrailingPathDelimiter(result);
end;

function CreatingNewPage: boolean;
begin
  result := false;
  if GetPostVar('command') = 'newpage' then result := true
end;

procedure DisplayHtmContentLink;
var
  dummy: boolean;
  htmpath, link: string;
begin
  // make a url to the static html page
  htmpath := GetPublicHtmlOutputFolder(dummy);
  if htmpath[length(htmpath)] = '\' then htmpath[length(htmpath)] := '/';
  if htmpath[length(htmpath)] <> '/' then htmpath := htmpath + '/';
  htmpath := htmpath + GotPgDashed + HTM_EXT;
  link := '<a href="/' + htmpath + '">'+htmpath+'</a>';
  outbr('');
  outbr('Your static html page (for end users to view) will be accessible here: ' + link);
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

  var
    savefname: string;
    err: boolean;
  begin
    // if hidden form input signals this then make new page
    if CreatingNewPage then begin
      err := true;
      VerifyPass;
      SharedFileCreate(MakePgFname); //create PageName.htm since a new page was requested. This ensures a file can be created, and checking that it can be created is important since a stringlist.savetofile wouldn't offer this check.
      // todo: make lower case page name become Page Name
      Article := Article
                 +'<pre>'#13#10'This is a newly created page: '
                 +   GotPg + #13#10 + ' <i>Enter text here</i>'+ #13#10
                 +   'The content is stored in a cgi-bin content folder, ' + #13#10
                 +   'and now a static html file will also be generated in the ' + #13#10
                 +   'public html output folder in your config file.'
                 +'</pre>';
      savefname := HtmOutputDir(err);
      if err then begin
        OutErr('Could not save to an html output directory, check your config file: ' + OUTPUT_DIR_CFG_FILE);
      end else begin
        savefname :=  savefname + GotPgDashed + HTM_EXT;
        // save the static html file to public html directory
        if MakeHtmlFile(savefname) then begin
          outbr('');
          outbr('Public html file saved:' + savefname);
        end else begin
          OutErr('Could not save: ' + savefname + '  Check if html output directory exists in your public html. ' +'The path must be configured in the config file: ' +OUTPUT_DIR_CFG_FILE);
        end;
      end;
      // save cgi htm content
      StringToFile(MakePgFname, Article);
      SharedFileOut(MakePgFname); // output the htm file to stdout
      DisplayHtmContentLink;
    end else
      AskUser;
  end;

begin
  if FileExists_read(MakePgFname) then begin
    SharedFileOut(MakePgFname); // output htm file
    DisplayHtmContentLink;
  end else begin // create file if doesn't exist
    NewPageFile;
  end;
end;


procedure ShowCms;
  { check password first }
  procedure CheckPass;
  begin
    if GetPostVar('ed') = 'update' then
    begin
      if IsPostVar('pw') then
        VerifyPass
      else //password not found, maybe user trying to edit page maliciously
      begin
        outln('Err 1: password not found');
        halt(1);
      end;
    end;
  end;

  procedure UpdatePageContent;
  begin
    if IsPostVar('ed') then
    begin
      // check signal for page update
      if GetPostVar('ed') = 'update' then
      begin
        // get page content from edit box that was edited
        edit1.text:= GetPostVar_S('ed1', 0); // unsecure, security set to zero because this is a private CMS allowing any java script injections or null characters or other bad crap. fuck hacker faggots
        Article := edit1.Text;
        if not StringToFile(MakePgFname, Article) then begin
          OutErr('Error saving file: '+ MakePgFname);
        end;
      end;
    end;
  end;

begin
  if GotPg = '' then  // display error
    NoCmsPage;
  TemplateOut(HEADER_FILE, true);
  CheckPass;
  CheckPageNames;
  UpdatePageContent;
  WriteEdPage;
  WriteFileContent;
end;



end.
