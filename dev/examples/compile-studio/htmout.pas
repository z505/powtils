(****************************************************************************** 
  Compile Studio html output unit
  Copyright Lars Olson 2008-2011 

*******************************************************************************)

unit htmout;  {$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF} 

interface

const
 // cross platform directory separator
 {$IFDEF windows}DIRSEP = '\';{$ENDIF}
 {$IFDEF unix}DIRSEP = '/';{$ENDIF}

procedure NoCsPage;
procedure ShowCs;
procedure WriteTopHeader;
procedure WriteFooter;

type 
  THtmEd = record 
    text: string;
  end;
                      
implementation

uses
  {$IFDEF UNIX}baseunix, unix,{$ENDIF}
  pwinit, pwmain, pwurlenc, pwsubstr, pwenvvar, csfilefuncs, pwfileutil,
  pwfileshare, strwrap1;

const
  PWRD = 'cs'; // password.. change & encrypt it if you want security

var
 AskingToCreateNewFile: boolean = false; //flag
 Edit1: THtmEd;           // gotten text from http post
 GotPw,                   // gotten password
 FileA,                   // loaded file
 GotPg: string;           // gotten pagename

procedure WriteTopHeader;
begin
  outln('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">');
  out('<html>');
  out('<head>');
end;

procedure notice(const s: string);
begin
  outln('<font color=yellow>Note: '+s+'</font>');
  outln('');
end;

{ creates file, protecting with file sharing for multiple users }
procedure SharedFileCreate(const fname: string);
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

procedure SharedFileOut(const fname: string);
var 
  k: word; // file sharing unique key
  filestr: string;
begin
  FileMarkRead(fname, k);
  filestr:= StrLoadFile(fname);
  if filestr = '-1NF' then 
    out('SharedFileOut: file not found: ' + fname)
  else
  begin
    filestr:= FilterHtml(filestr);
    out(filestr);
  end;
  FileUnMarkRead(fname, k);
end;

{ if no page specified, display simple error page }
procedure NoCsPage;
begin
  // note: could change to WebTemplateOut
  out(  '<title>Compiler Studio</title>');
  out('</head>');
  out('<body>');
  out(  '<FONT face=verdana SIZE="5"><b>Compiler Studio</b></font>');
  out(  '<hr><font face=verdana>');
  out(  'No page was specified.');
  out(  'See the <a href="' + CGIEnvVar.ScriptName() + '?p=main">Main Page</a>');
  out(  '<hr>');
  out('</body>');
  out('</html>');
  halt;
end;


procedure CheckPageNames;
begin
   if length(GotPg)> 50 then //security measures for file name length
   begin
     out('<br>Page name too long. Please make page name shorter.<br>');
     halt;
   end;
end;

{ makes path to source file }
function MakePgFname: string;
begin
  result:= 'pasfiles' + DIRSEP + GotPg + '.pas'
end;

procedure WritePassBox;
begin
  out('<p>Passcode: <INPUT TYPE=password NAME=pw ROWS=1 COLS=30 >');
end;

procedure StartPostForm;
var url: string;
begin
  url:= SERV.ScriptName() + '?p=' + GotPg;
  outln('<FORM action="'+url+'" method="post">'); 
end;

procedure EndForm;
begin
  out('</FORM>');
end;

{ display an edit box with page content inside }
procedure WriteEditBox;

  procedure CorrectPgName;
  begin
    GotPg:= UrlDecode(GotPg); //decode incase any percent signs, special encoded characters like %20
    GotPg:= lowercase(gotpg);
  end;

  // could use TemplatOut in future instead
  procedure WriteFormWidgets;
  begin
    out('<TEXTAREA NAME="ed1" ROWS=20 COLS=85 >');
    out(FileA);//gotten text from file
    out('</TEXTAREA><br>');
    WritePassBox;
    out('</INPUT>');
    out('<INPUT NAME="ed" VALUE=update TYPE=hidden></input>');
    // in newer versions of Powtils, this hack below isn't needed, version 1.6.0.2 contained a small issue we workaround by using HIDDEN form inputs
    out('<INPUT NAME="p" VALUE="' + GotPg + '" TYPE=hidden></input>');
    outln('&nbsp;&nbsp;&nbsp;<input type="submit" name="button" value="SAVE FILE"></input>');
  end;

begin
 { get the page content from the file... }
  CorrectPgName;
  FileA:= FileToString(MakePgFname);
  StartPostForm;
  WriteFormWidgets;
  EndForm;
  out('<hr>');
end;

procedure VerifyPass;
begin
  GotPw:= GetCgiVar('pw');
  // verify posted password
  if GotPw <> PWRD then 
  begin
    outln('Mom says: password wrong! Try again darling.');
    halt; {wrong password, exit}
  end;
end;

{ output the file if it exists, create it otherwise }
procedure WriteFileContent;

  procedure NewPageFile;

    // Could use TemplateOut in future instead
    procedure AskUser;
    begin
      AskingToCreateNewFile:= true; // set flag
      out('<p>Source file does not exist.');
      out('<p>Do you want to create the new source file?');
      StartPostForm;
      WritePassBox;
      out('&nbsp;&nbsp;');
      out('<INPUT TYPE="submit"> <INPUT TYPE="hidden" NAME="command" VALUE="newpage" >');
      // in newer versions of Powtils, this hack below isn't needed, version 1.6.0.2 contained a small issue we workaround by using HIDDEN form inputs
      out('<INPUT NAME="p" VALUE="' + GotPg + '" TYPE=hidden></input>');
      EndForm;
    end;
  var
    ferr: boolean;
  begin
    if  GetCgiVar('command') = 'newpage' then 
    begin
      VerifyPass;
      SharedFileCreate(MakePgFname); //create PageName.pas since a new page was requested. This ensures a file can be created, and checking that it can be created is important since a stringlist.savetofile wouldn't offer this check.

      //GotPg:= StringReplace(GotPg,'-',' ',[rfReplaceAll]); 
      FileA:= FileA + #13#10 +
              'program ' + GotPg + ';'#13#10 + 
              '  // enter source here'#13#10 +
              'begin'#13#10 +
              'end.';

      //GotPg:= StringReplace(GotPg,' ','-',[rfReplaceAll]); 
      StringToFile(MakePgFname, FileA);
      notice('File saved: ');
      outln('');
      SharedFileOut(MakePgFname); //output the htm file
    end else
      AskUser;  
  end;

begin
  GotPg:= UrlDecode(GotPg); //first decode %20 and special characters into normal characters
  GotPg:= lowercase(GotPg); // case sensitivity sucks
  if FileExists_read(MakePgFname) then
    SharedFileOut(MakePgFname) //output the htm file
  else // create file if doesn't exist
    NewPageFile;
end;


procedure err(const s: string);
begin
  outln('');
  outln('<b>Error:</b> ' + s);
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

procedure UpdateSourceFile;
begin
  { get the text content from edit box that was updated...}
  edit1.text:= GetCgiVar_S('ed1', 0); // unsecure, security set to zero because this is a private program allowing any characters in
  FileA:= edit1.Text;
  GotPg:= lowercase(GotPg); // UNIX case sensitivity sucks
  StringToFile(MakePgFname, FileA);
  notice('file updated');
end;

{ compiler can be relative to document root with this macrovar trick}
procedure FilterMacroVar(var s: string);
begin
  s:= SubstrReplace(s, '{{DOCROOT}}', CgiEnvVar.DocRoot() );
end;

function CmdExists(const cmd: string): boolean;
begin
  if FileExists_plain(cmd) then result:= true else result:= false;
end;

{ show compiler studio }
procedure ShowCs;

  { corrects the page name, case insensitive }
  procedure CorrectPageName(var s: string);
  begin
    // null character bad, 000 makes it easy to spot malicous attempts, although this program isn't secure anyway since it is a compiler! It is open to any sort of attack since it executes commands... it is meant to be password protected program that only you have access to
    s:= SubstrReplace(s, #0, '000'); 
    s:= lowercase(s);
  end;
  
  function Compiled: boolean;
    
    procedure CompileFile(cmd: string; const param1: string);

      procedure RunCommand(const cmd, p1: string);
      var
        Si, So, Serr: Text;
        s: String;
        i: longint;
      begin
        outln('Command to run: <b>'+ cmd + '</b>');

        if not CmdExists(cmd) then 
        begin
          err('the given command path doesn''t exist, or system can''t access it!');
          Compiled:= false;
          exit;
        end;
        // try running command
        if AssignStream(Si, So, Serr, cmd, [p1]) = -1 then
          outln('AssignStream failed !')
        else 
        begin
          if fpgeterrno <> 0 then
          begin
            outln('AssignStream failed !');
            Compiled:= false;
            exit;
          end;
          close (so);
          outln('Command result:');
          // read command result from STD IN 
          while not eof(si) do
          begin
            readln(si,s);
            outln(s);
          end;
          // read command result from STD ERR
          while not eof(serr) do
          begin
            readln(serr, s);
            outln(s);
          end;
          close(Si);
          compiled:= true;
        end;
      end;

    begin
      FilterMacroVar(cmd);
      if CmdExists(cmd) then 
        RunCommand(cmd, param1)
      else 
      begin
        err('compiler path not found or cannot access: '#13#10 +'  '+ cmd + #13#10);
        Compiled:= false;
      end;
    end;

  var
    compilepath: string;
  begin
    result:= false;
    compilepath:= GetCgiVar_S('compilepath', 0);
    // check signal for compilation
    if (compilepath <> '') then 
      CompileFile(compilepath, 'pasfiles' + DIRSEP + GotPg);
  end;
var 
  Editing: boolean = false;
  Updating: boolean = false;
  Compiling: boolean = false;
begin
  GotPg:= GetCgiVar_S('p', 0);  // retrieve p variable unfiltered unsecure
  GotPg:= TrimBadChars_file(GotPg); //replace any BAD CHARACTERS from the file name (local directory safety ../ )
  if GotPg = '' then  //no page to display
    NoCsPage;

  if GetCgiVar('ed') = 'yes' then Editing:= true;
  if GetCgiVar('ed') = 'update' then Updating:= true;
  if GetCgiVar('cmd') = 'compile' then Compiling:= true;

  CorrectPageName(GotPg);
  outln('<title>Compiler Studio: '+ GotPg +'</title>');
  out('</head>');
  // set template variable
  SetWebVar('_GotPg', GotPg);
  WebTemplateOut('htminc/header1.htm', true);
  CheckPass;  
  CheckPageNames;
  
  if Editing then WriteEditBox;

  if (Compiling) and (Compiled) then 
    outln(#13#10'Tried to compile: <a href="pasfiles/' + GotPg + '">'+ GotPg + '</a>');
    
  if Updating then UpdateSourceFile;

  if (Updating) or ((not Editing) and (not Compiling)) then
    WriteFileContent;

end;


procedure WriteFooter;
var 
  Editing: boolean = false;
begin
  if GetCgiVar('ed') = 'yes' then Editing:= true;

  if (Editing) or (AskingToCreateNewFile) then 
    WebTemplateOut('htminc/normal-footer.htm', true)
  else
    WebTemplateOut('htminc/set-me-up-footer.htm', true);

  // set me up footer contains an important path that you need to setup and it
  // contains the compile button that is only shown when we are done editing 
  // or when we are viewing a file
end;

end.