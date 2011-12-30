unit main;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
interface
uses
 mseglob,mseguiglob,mseguiintf,mseapplication,msestat,msemenus,msegui,
 msegraphics,msegraphutils,mseevent,mseclasses,mseforms,msesimplewidgets,
 msewidgets,msedataedits,mseedit,mseifiglob,msestrings,msetypes,mseeditglob,
 msegrids,msetextedit,msewidgetgrid;

type
 tmainfo = class(tmainform)
   tbutton1: tbutton;
   tbutton2: tbutton;
   memo1: tmemoedit;
   tbutton3: tbutton;
   twidgetgrid1: twidgetgrid;
   texted: ttextedit;
   tbutton4: tbutton;
   procedure BtnLoadWikTest(const sender: TObject);
   procedure widgetgridonlayoutchange(const sender: tcustomgrid);
   procedure parseandsaveonexec(const sender: TObject);
   procedure parseandsave2onexec(const sender: TObject);
 end;
var
 mainfo: tmainfo;
implementation
uses
 main_mfm, msestream, wikitohtm2, strwrap1, wikiparserb;

procedure tmainfo.BtnLoadWikTest(const sender: TObject);
begin
  //memo1.loadfromfile('bigtest.txt');
  texted.loadfromfile('bigtest.txt');
  //memo1.value:= readfiledatastring('bigtest.txt');  
end;

procedure tmainfo.widgetgridonlayoutchange(const sender: tcustomgrid);
begin
//  memo1.value:='layout changed ';
//  texted.anchors:=[];
//  texted.anchors:=[an_top];  
end;


procedure debugproc(const s: string);
begin
  mainfo.memo1.value:= mainfo.memo1.value + s + #13#10;
end;


procedure tmainfo.parseandsaveonexec(const sender: TObject);
begin
  wikiparserb.debugln:= @debugproc;
  AssignFile(wikitohtm2.textfile, 'test.htm');
  ReWrite(wikitohtm2.textfile);
  ParseWikiTxt(StrLoadFile('bigtest.txt'));
  CloseFile(wikitohtm2.textfile);
end;

procedure tmainfo.parseandsave2onexec(const sender: TObject);
begin
  wikiparserb.debugln:= @debugproc;
  AssignFile(wikitohtm2.textfile, 'test2.htm');
  ReWrite(wikitohtm2.textfile);
  ParseWikiTxt(StrLoadFile('bigtest2.txt'));
  CloseFile(wikitohtm2.textfile);  
end;

end.
