(****************************************************************************** 
  Compile Studio html output unit
  Copyright Lars Olson 2008-2011 

*******************************************************************************)

unit htmout;  {$ifdef fpc}{$mode objfpc}{$h+}{$endif} 

interface

const
 // cross platform directory separator
 {$ifdef windows}dirsep = '\';{$endif}
 {$ifdef unix}dirsep = '/';{$endif}

procedure nocspage;
procedure showcs;
procedure writetopheader;
procedure writefooter;

type 
  thtmed = record 
    text: string;
  end;
                      
implementation

uses
  {$ifdef unix}baseunix, unix,{$endif}
  pwinit, pwmain, pwurlenc, pwsubstr, pwenvvar, csfilefuncs, pwfileutil,
  pwfileshare, strwrap1;

const
  pwrd = 'cs'; // password.. change & encrypt it if you want security

var
  askingtocreatenewfile: boolean = false; //flag
  edit1: thtmed;           // gotten text from http post
  gotpw,                   // gotten password
  filea,                   // loaded file
  gotpg: string;           // gotten pagename

procedure writetopheader;
begin
  outln('<!doctype html public "-//w3c//dtd html 4.01 transitional//en">');
  out('<html>');
  out('<head>');
end;

procedure notice(const s: string);
begin
  outln('<font color=yellow>note: '+s+'</font>');
  outln('');
end;

{ creates file, protecting with file sharing for multiple users }
procedure sharedfilecreate(const fname: string);
var
  created: boolean;
begin
  filemarkwrite(fname);
  created:= newfile(fname);
  // if there was an error creating file...
  if not created then begin
    outln('Error: sharedfilecreate(): couldn''t create file'); 
    halt(2); //stop program
  end;
  fileunmarkwrite(fname);
end;

procedure sharedfileout(const fname: string);
var 
  k: word; // file sharing unique key
  filestr: string;
begin
  filemarkread(fname, k);
  filestr:= strloadfile(fname);
  if filestr = '-1nf' then 
    out('Error: sharedfileout(): file not found: ' + fname)
  else
  begin
    filestr:= filterhtml(filestr);
    out(filestr);
  end;
  fileunmarkread(fname, k);
end;

{ if no page specified, display simple error page }
procedure nocspage;
begin
  // note: could change to templateout
  out(  '<title>compiler studio</title>');
  out('</head>');
  out('<body>');
  out(  '<font face=verdana size="5"><b>compiler studio</b></font>');
  out(  '<hr><font face=verdana>');
  out(  'no page was specified.');
  out(  'see the <a href="' + cgienvvar.scriptname() + '?p=main">main page</a>');
  out(  '<hr>');
  out('</body>');
  out('</html>');
  halt;
end;


procedure checkpagenames;
begin
  if length(gotpg)> 50 then begin //security measures for file name length
    out('<br>Page name too long. Please make it shorter.<br>');
    halt;
  end;
end;

{ makes path to source file }
function makepgfname: string;
begin
  result:= 'pasfiles' + dirsep + gotpg + '.pas';
end;

procedure writepassbox;
begin
  out('<p>passcode: <input type=password name=pw rows=1 cols=30 >');
end;

procedure startpostform;
var url: string;
begin
  url:= serv.scriptname() + '?p=' + gotpg;
  outln('<form action="'+url+'" method="post">'); 
end;

procedure endform;
begin
  out('</form>');
end;

{ display an edit box with page content inside }
procedure writeeditbox;

  procedure correctpgname;
  begin
    gotpg:= urldecode(gotpg); //decode incase any percent signs, special encoded characters like %20
    gotpg:= lowercase(gotpg);
  end;

  // could use templatout in future instead
  procedure writeformwidgets;
  begin
    out('<textarea name="ed1" rows=20 cols=85 >');
    out(filea);//gotten text from file
    out('</textarea><br>');
    writepassbox;
    out('</input>');
    out('<input name="ed" value=update type=hidden></input>');
    // in newer versions of powtils, this hack below isn't needed, version 1.6.0.2 contained a small issue we workaround by using hidden form inputs
    out('<input name="p" value="' + gotpg + '" type=hidden></input>');
    outln('&nbsp;&nbsp;&nbsp;<input type="submit" name="button" value="save file"></input>');
  end;

begin
 { get the page content from the file... }
  correctpgname;
  filea:= filetostring(makepgfname);
  startpostform;
  writeformwidgets;
  endform;
  out('<hr>');
end;

procedure verifypass;
begin
  gotpw:= getpostvar('pw');
  // verify posted password
  if gotpw <> pwrd then begin
    outln('Error: password wrong');
    halt; {wrong password, exit}
  end;
end;

{ output the file if it exists, create it otherwise }
procedure writefilecontent;

  procedure newpagefile;

    // could use templateout in future instead
    procedure askuser;
    begin
      askingtocreatenewfile:= true; // set flag
      out('<p>source file does not exist.');
      out('<p>do you want to create the new source file?');
      startpostform;
      writepassbox;
      out('&nbsp;&nbsp;');
      out('<input type="submit"> <input type="hidden" name="command" value="newpage" >');
      // in newer versions of powtils, this hack below isn't needed, version 1.6.0.2 contained a small issue we workaround by using hidden form inputs
      out('<input name="p" value="' + gotpg + '" type=hidden></input>');
      endform;
    end;
  var
    ferr: boolean;
  begin
    if  getpostvar('command') = 'newpage' then 
    begin
      verifypass;
      sharedfilecreate(makepgfname); //create pagename.pas since a new page was requested. this ensures a file can be created, and checking that it can be created is important since a stringlist.savetofile wouldn't offer this check.

      //gotpg:= stringreplace(gotpg,'-',' ',[rfreplaceall]); 
      filea:= filea + #13#10 +
              'program ' + gotpg + ';'#13#10 + 
              '// uses  '#13#10 +
              ' '#13#10 +
              'begin'#13#10 +
              'end.';

      //gotpg:= stringreplace(gotpg,' ','-',[rfreplaceall]); 
      stringtofile(makepgfname, filea);
      notice('file saved: ');
      outln('');
      sharedfileout(makepgfname); //output the htm file
    end else
      askuser;  
  end;

begin
  gotpg:= urldecode(gotpg); //first decode %20 and special characters into normal characters
  gotpg:= lowercase(gotpg); // case sensitivity sucks
  if fileexists_read(makepgfname) then
    sharedfileout(makepgfname) //output the htm file
  else // create file if doesn't exist
    newpagefile;
end;


procedure err(const s: string);
begin
  outln('');
  outln('<b>error:</b> ' + s);
end;


{ check password first }
procedure checkpass;
begin
  if getpostvar('ed') = 'update' then begin
    if ispostvar('pw') then
      verifypass
    else begin//password not found, maybe user trying to edit page maliciously
      outln('error: password not found'); 
      halt(1);
    end;
  end;
end;

procedure updatesourcefile;
begin
  { get the text content from edit box that was updated...}
  edit1.text:= getpostvar_s('ed1', 0); // unsecure, security set to zero because this is a private program allowing any characters in
  filea:= edit1.text;
  gotpg:= lowercase(gotpg); // unix case sensitivity sucks
  stringtofile(makepgfname, filea);
  notice('file updated');
end;

{ compiler can be relative to document root with this macrovar trick}
procedure filtermacrovar(var s: string);
begin
  s:= substrreplace(s, '{{DOCROOT}}', cgienvvar.docroot() );
end;

function cmdexists(const cmd: string): boolean;
begin
  if fileexists_plain(cmd) then result:= true else result:= false;
end;

{ show compiler studio }
procedure showcs;

  { corrects the page name, case insensitive }
  procedure correctpagename(var s: string);
  begin
    // null character bad, 000 makes it easy to spot malicous attempts, although this program isn't secure anyway since it is a compiler! it is open to any sort of attack since it executes commands... it is meant to be password protected program that only you have access to
    s:= substrreplace(s, #0, '000'); 
    s:= lowercase(s);
  end;
  
  function compiled: boolean;
    
    procedure compilefile(cmd: string; const param1: string);

      procedure runcommand(const cmd, p1: string);
      var
        si, so, serr: text;
        s: string;
        i: longint;
      begin
        outln('command to run: <b>'+ cmd + '</b>');

        if not cmdexists(cmd) then 
        begin
          err('the given command path doesn''t exist, or system can''t access it!');
          compiled:= false;
          exit;
        end;
        // try running command
        if assignstream(si, so, serr, cmd, [p1]) = -1 then
          outln('assignstream failed !')
        else 
        begin
          if fpgeterrno <> 0 then
          begin
            outln('assignstream failed !');
            compiled:= false;
            exit;
          end;
          close (so);
          outln('command result:');
          // read command result from std in 
          while not eof(si) do
          begin
            readln(si,s);
            outln(s);
          end;
          // read command result from std err
          while not eof(serr) do
          begin
            readln(serr, s);
            outln(s);
          end;
          close(si);
          compiled:= true;
        end;
      end;

    begin
      filtermacrovar(cmd);
      if cmdexists(cmd) then 
        runcommand(cmd, param1)
      else 
      begin
        err('compiler path not found or cannot access: '#13#10 +'  '+ cmd + #13#10);
        compiled:= false;
      end;
    end;

  var
    compilepath: string;
  begin
    result:= false;
    compilepath:= getpostvar_s('compilepath', 0);
    // check signal for compilation
    if (compilepath <> '') then 
      compilefile(compilepath, 'pasfiles' + dirsep + gotpg);
  end;
var 
  editing: boolean = false;
  updating: boolean = false;
  compiling: boolean = false;
begin
  gotpg:= getpostvar_s('p', 0);  // retrieve p variable unfiltered unsecure
  gotpg:= trimbadfile(gotpg); //replace any bad characters from the file name (local directory safety ../ )
  if gotpg = '' then  //no page to display
    nocspage;

  if getpostvar('ed') = 'yes' then editing:= true;
  if getpostvar('ed') = 'update' then updating:= true;
  if getpostvar('cmd') = 'compile' then compiling:= true;

  correctpagename(gotpg);
  outln('<title>Compiler Studio: '+ gotpg +'</title>');
  out('</head>');
  // set template variable
  setvar('_gotpg', gotpg);
  templateout('htminc/header1.htm', true);
  checkpass;  
  checkpagenames;
  
  if editing then writeeditbox;

  if (compiling) and (compiled) then 
    outln(#13#10'Tried to compile: <a href="pasfiles/' + gotpg + '">'+ gotpg + '</a>');
    
  if updating then updatesourcefile;

  if (updating) or ((not editing) and (not compiling)) then
    writefilecontent;

end;


procedure writefooter;
var 
  editing: boolean = false;
begin
  if getpostvar('ed') = 'yes' then editing:= true;

  if (editing) or (askingtocreatenewfile) then 
    templateout('htminc/normal-footer.htm', true)
  else
    templateout('htminc/set-me-up-footer.htm', true);

  // set me up footer contains an important path that you need to setup and it
  // contains the compile button that is only shown when we are done editing 
  // or when we are viewing a file
end;

end.